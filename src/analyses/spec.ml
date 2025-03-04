(** Analysis by specification file. *)

open Prelude.Ana
open Analyses

module SC = SpecCore

module Spec =
struct
  include Analyses.DefaultSpec

  let name()  = "spec"
  module D = SpecDomain.Dom
  module C = SpecDomain.Dom

  (* special variables *)
  let return_var    = Goblintutil.create_var @@ Cil.makeVarinfo false "@return"    Cil.voidType, `NoOffset
  let global_var    = Goblintutil.create_var @@ Cil.makeVarinfo false "@global"    Cil.voidType, `NoOffset

  (* spec data *)
  let nodes = ref []
  let edges = ref []

  let load_specfile () =
    let specfile = GobConfig.get_string "ana.spec.file" in
    if String.length specfile < 1 then failwith "You need to specify a specification file using --set ana.spec.file path/to/file.spec when using the spec analysis!";
    if not (Sys.file_exists specfile) then failwith @@ "The given spec-file ("^specfile^") doesn't exist (CWD is "^Sys.getcwd ()^").";
    let _nodes, _edges = SpecUtil.parseFile specfile in
    nodes := _nodes; edges := _edges (* don't change -> no need to save them in domain *)

  (* module for encapsulating general spec checking functions used in multiple transfer functions (assign, special) *)
  (*
  .spec-format:
  - The file contains two types of definitions: nodes and edges. The labels of nodes are output. The labels of edges are the constraints.
  - The given nodes are warnings, which have an implicit back edge to the previous node if used as a target.
  - Alternatively warnings can be specified like this: "node1 -w1,w2,w3> node2 ...1" (w1, w2 and w3 will be output when the transition is taken).
  - The start node of the first transition is the start node of the automaton.
  - End nodes are specified by "node -> end _".
  - "_end" is the local warning for nodes that are not in an end state, _END is the warning at return ($ is the list of keys).
  - An edge with '_' matches everything.
  - Edges with "->>" (or "-w1,w2>>" etc.) are forwarding edges, which will continue matching the same statement for the target node.
  *)
  module SpecCheck =
  struct
    (* custom goto (D.goto is just for modifying) that checks if the target state is a warning and acts accordingly *)
    let goto ?may:(may=false) ?change_state:(change_state=true) key state m ws =
      let loc = !Tracing.current_loc::(D.callstack m) in
      let warn key m msg =
        Str.global_replace (Str.regexp_string "$") (D.string_of_key key) msg
        |> D.warn ~may:(D.is_may key m || D.is_unknown key m)
      in
      (* do transition warnings *)
      List.iter (fun state -> match SC.warning state !nodes with Some msg -> warn key m msg | _ -> ()) ws;
      match SC.warning state !nodes with
      | Some msg ->
        warn key m msg;
        m (* no goto == implicit back edge *)
      | None ->
        M.debug "GOTO %s: %s -> %s" (D.string_of_key key) (D.string_of_state key m) state;
        if not change_state then m
        else if may then D.may_goto key loc state m else D.goto key loc state m

    let equal_exp ctx spec_exp cil_exp = match spec_exp, cil_exp with
      (* TODO match constants right away to avoid queries? *)
      | `String a, Const(CStr (b,_)) -> M.debug "EQUAL String Const: %s = %s" a b; a=b
      (* | `String a, Const(CWStr xs as c) -> failwith "not implemented" *)
      (* CWStr is done in base.ml, query only returns `Str if it's safe *)
      | `String a, e -> (match ctx.ask (Queries.EvalStr e) with
          | `Lifted b -> M.debug "EQUAL String Query: %s = %s" a b; a=b
          | _      -> M.debug "EQUAL String Query: no result!"; false
        )
      | `Regex a, e -> (match ctx.ask (Queries.EvalStr e) with
          | `Lifted b -> M.debug "EQUAL Regex String Query: %s = %s" a b; Str.string_match (Str.regexp a) b 0
          | _      -> M.debug "EQUAL Regex String Query: no result!"; false
        )
      | `Bool a, e -> (match ctx.ask (Queries.EvalInt e) with
          | b -> (match Queries.ID.to_bool b with Some b -> a=b | None -> false)
        )
      | `Int a, e  -> (match ctx.ask (Queries.EvalInt e) with
          | b -> (match Queries.ID.to_int b with Some b -> (Int64.of_int a)=(IntOps.BigIntOps.to_int64 b) | None -> false)
        )
      | `Float a, Const(CReal (b, fkind, str_opt)) -> a=b
      | `Float a, _ -> M.warn "EQUAL Float: unsupported!"; false
      (* arg is a key. currently there can only be one key per constraint, so we already used it for lookup. TODO multiple keys? *)
      | `Var a, b  -> true
      (* arg is a identifier we use for matching constraints. TODO save in domain *)
      | `Ident a, b -> true
      | `Error s, b -> failwith @@ "Spec error: "^s
      (* wildcard matches anything *)
      | `Free, b    -> true
      | a,b -> M.warn "EQUAL? Unmatched case - assume true..."; true

    let check_constraint ctx get_key matches m new_a old_key (a,ws,fwd,b,c as edge) =
      (* If we have come to a wildcard, we match it instantly, but since there is no way of determining a key
         this only makes sense if fwd is true (TODO wildcard for global. TODO use old_key). We pass a state replacement as 'new_a',
         which will be applied in the following checks.
         Multiple forwarding wildcards are not allowed, i.e. new_a must be None, otherwise we end up in a loop. *)
      if SC.is_wildcard c && fwd && new_a=None then Some (m,fwd,Some (b,a),old_key) (* replace b with a in the following checks *)
      else
        (* save original start state of the constraint (needed to detect reflexive edges) *)
        let old_a = a in
        (* Assume new_a  *)
        let a = match new_a with
          | Some (x,y) when a=x -> y
          | _ -> a
        in
        (* if we forward, we have to replace the starting state for the following constraints *)
        let new_a = if fwd then Some (b,a) else None in
        (* TODO how to detect the key?? use "$foo" as key, "foo" as var in constraint and "_" for anything we're not interested in.
            What to do for multiple keys (e.g. $foo, $bar)? -> Only allow one key & one map per spec-file (e.g. only $ as a key) or implement multiple maps? *)
        (* look inside the constraint if there is a key and if yes, return what it corresponds to *)
        (* if we can't find a matching key, we use the global key *)
        let key = get_key c |? Cil.var (fst global_var) in
        (* ignore(printf "KEY: %a\n" d_plainlval key); *)
        (* get possible keys that &lval may point to *)
        let keys = D.keys_from_lval key (Analyses.ask_of_ctx ctx) in (* does MayPointTo query *)
        let check_key (m,n) var =
          (* M.debug @@ "check_key: "^f.vname^"(...): "^D.string_of_entry var m; *)
          let wildcard = SC.is_wildcard c && fwd && b<>"end" in
          (* skip transitions we can't take b/c we're not in the right state *)
          (* i.e. if not in map, we must be at the start node or otherwise we must be in one of the possible saved states *)
          if not (D.mem var m) && a<>SC.startnode !edges || D.mem var m && not (D.may_in_state var a m) then (
            (* ignore(printf "SKIP %s: state: %s, a: %s at %i\n" f.vname (D.string_of_state var m) a (!Tracing.current_loc.line)); *)
            (m,n) (* not in map -> initial state. TODO save initial state? *)
          )
          (* edge must match the current state or be a wildcard transition (except those for end) *)
          else if not (matches edge) && not wildcard then (m,n)
          (* everything matches the constraint -> go to new state and increase counter *)
          else
            (* TODO if #Queries.MayPointTo > 1: each result is May, but all combined are Must *)
            let may = (List.compare_length_with keys 1 > 0) in
            (* do not change state for reflexive edges where the key is not assigned to (e.g. *$p = _) *)
            let change_state = not (old_a=b && SC.get_lval c <> Some `Var) in
            M.debug "GOTO ~may:%B ~change_state:%B. %s -> %s: %s" may change_state a b (SC.stmt_to_string c);
            let new_m = goto ~may:may ~change_state:change_state var b m ws in
            (new_m,n+1)
        in
        (* do check for each varinfo and return the resulting domain if there has been at least one matching constraint *)
        let new_m,n = List.fold_left check_key (m,0) keys in (* start with original domain and #transitions=0 *)
        if n==0 then None (* no constraint matched the current state *)
        else Some (new_m,fwd,new_a,Some key) (* return new domain and forwarding info *)

    let check ctx get_key matches =
      let m = ctx.local in
      (* go through constraints and return resulting domain for the first match *)
      (* if no constraint matches, the unchanged domain is returned *)
      (* repeat for target node if it is a forwarding edge *)
      (* TODO what should be done if multiple constraints would match? *)
      (* TODO ^^ for May-Sets multiple constraints could match and should be taken! *)
      try
        let rec check_fwd_loop m new_a old_key = (* TODO cycle detection? *)
          let new_m,fwd,new_a,key = List.find_map (check_constraint ctx get_key matches m new_a old_key) !edges in
          (* List.iter (fun x -> M.debug (x^"\n")) (D.string_of_map new_m); *)
          if fwd then M.debug "FWD: %B, new_a: %s, old_key: %s" fwd (dump new_a) (dump old_key);
          if fwd then check_fwd_loop new_m new_a key else new_m,key
        in
        (* now we get the new domain and the latest key that was used *)
        let new_m,key = check_fwd_loop m None None in
        (* List.iter (fun x -> M.debug (x^"\n")) (D.string_of_map new_m); *)
        (* next we have to check if there is a branch() transition we could take *)
        let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.is_branch c) !edges in
        (* just for the compiler: key is initialized with None, but changes once some constaint matches. If none match, we wouldn't be here but at catch Not_found. *)
        match key with
        | Some key ->
          (* we need to pass the key to the branch function. There is no scheme for getting the key from the constraint, but we should have been forwarded and can use the old key. *)
          let check_branch branches var =
            (* only keep those branch_edges for which our key might be in the right state *)
            let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> D.may_in_state var a new_m) branch_edges in
            (* M.debug @@ D.string_of_entry var new_m^" -> branch_edges: "^String.concat "\n " @@ List.map (fun x -> SC.def_to_string (SC.Edge x)) branch_edges; *)
            (* count should be a multiple of 2 (true/false), otherwise the spec is malformed *)
            if List.length branch_edges mod 2 <> 0 then failwith "Spec is malformed: branch-transitions always need a true and a false case!" else
              (* if nothing matches, just return new_m without branching *)
              (* if List.is_empty branch_edges then Set.of_list new_m else *)
            if List.is_empty branch_edges then Set.of_list ([new_m, Cil.integer 1, true]) else (* XX *)
              (* unique set of (dom,exp,tv) used in branch *)
              let do_branch branches (a,ws,fwd,b,c) =
                let c_str = match SC.branch_exp c with Some (exp,tv) -> SC.exp_to_string exp | _ -> "" in
                let c_str = Str.global_replace (Str.regexp_string "$key") "%e:key" c_str in (* TODO what should be used to specify the key? *)
                (* TODO this somehow also prints the expression!? why?? *)
                let c_exp = Formatcil.cExp c_str [("key", Fe (D.K.to_exp var))] in (* use Fl for Lval instead? *)
                (* TODO encode key in exp somehow *)
                (* ignore(printf "BRANCH %a\n" d_plainexp c_exp); *)
                ctx.split new_m [Events.SplitBranch (c_exp, true)];
                Set.add (new_m,c_exp,true) (Set.add (new_m,c_exp,false) branches)
              in
              List.fold_left do_branch branches branch_edges
          in
          let keys = D.keys_from_lval key (Analyses.ask_of_ctx ctx) in
          let new_set = List.fold_left check_branch Set.empty keys in ignore(new_set); (* TODO refactor *)
          (* List.of_enum (Set.enum new_set) *)
          new_m (* XX *)
        | None -> new_m
      with Not_found -> m (* nothing matched -> no change *)
  end

  (* queries *)
  let query ctx (type a) (q: a Queries.t) =
    match q with
    | _ -> Queries.Result.top q

  let query_lv ask exp =
    match ask (Queries.MayPointTo exp) with
    | l when not (Queries.LS.is_top l) ->
      Queries.LS.elements l
    | _ -> []

  let eval_fv ask exp: varinfo option =
    match query_lv ask exp with
    | [(v,_)] -> Some v
    | _ -> None


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let get_key c = match SC.get_key_variant c with
      | `Lval s ->
        M.debug "Key variant assign `Lval %s; %s" s (SC.stmt_to_string c);
        (match SC.get_lval c, lval with
         | Some `Var, _ -> Some lval
         | Some `Ptr, (Mem Lval x, o) -> Some x (* TODO offset? *)
         | _ -> None)
      | _ -> None
    in
    let matches (a,ws,fwd,b,c) =
      SC.equal_form (Some lval) c &&
      (* check for constraints *p = _ where p is the key *)
      match lval, SC.get_lval c with
      | (Mem Lval x, o), Some `Ptr  when SpecCheck.equal_exp ctx (SC.get_rval c) rval ->
        let keys = D.keys_from_lval x (Analyses.ask_of_ctx ctx) in
        if List.compare_length_with keys 1 <> 0 then failwith "not implemented"
        else true
      | _ -> false (* nothing to do *)
    in
    let m = SpecCheck.check ctx get_key matches in
    let key_from_exp = function
      | Lval (Var v,o) -> Some (v, Lval.CilLval.of_ciloffs o)
      | _ -> None
    in
    match key_from_exp (Lval lval), key_from_exp (stripCasts rval) with (* TODO for now we just care about Lval assignments -> should use Queries.MayPointTo *)
    | Some k1, Some k2 when k1=k2 -> m (* do nothing on self-assignment *)
    | Some k1, Some k2 when D.mem k1 m && D.mem k2 m -> (* both in D *)
      M.debug "assign (both in D): %s = %s" (D.string_of_key k1) (D.string_of_key k2);
      (* saveOpened k1 *) m |> D.remove' k1 |> D.alias k1 k2
    | Some k1, Some k2 when D.mem k1 m -> (* only k1 in D *)
      M.debug "assign (only k1 in D): %s = %s" (D.string_of_key k1) (D.string_of_key k2);
      (* saveOpened k1 *) m |> D.remove' k1
    | Some k1, Some k2 when D.mem k2 m -> (* only k2 in D *)
      M.debug "assign (only k2 in D): %s = %s" (D.string_of_key k1) (D.string_of_key k2);
      let m = D.alias k1 k2 m in (* point k1 to k2 *)
      if Lval.CilLval.class_tag k2 = `Temp (* check if k2 is a temporary Lval introduced by CIL *)
      then D.remove' k2 m (* if yes we need to remove it from our map *)
      else m (* otherwise no change *)
    | Some k1, _ when D.mem k1 m -> (* k1 in D and assign something unknown *)
      M.debug "assign (only k1 in D): %s = %a" (D.string_of_key k1) d_exp rval;
      D.warn @@ "changed pointer "^D.string_of_key k1^" (no longer safe)";
      (* saveOpened ~unknown:true k1 *) m |> D.unknown k1
    | _ -> (* no change in D for other things *)
      M.debug "assign (none in D): %a = %a [%a]" d_lval lval d_exp rval d_plainexp rval;
      m

  (*
  - branch-transitions in the spec-file come in pairs: e.g. true-branch goes to node a, false-branch to node b
  - branch is called for both possibilities
  - TODO query the exp and take/don't take the transition
  - in case of `Top we take the transition
  - both branches get joined after (e.g. for fopen: May [open; error])
  - if there is a branch in the code, branch is also called
    -> get the key from exp and backtrack to the corresponding branch-transitions
    -> reevaluate with current exp and meet domain with result
  *)
  (*
  - get key from exp
  - ask EvalInt
  - if result is `Top and we are in a state that is the starting node of a branch edge, we have to:
    - go to target node and modify the state in specDomain
    - find out which value of key makes exp equal to tv
    - save this value and answer queries for EvalInt with it
  - if not, compare it with tv and take the corresponding branch
  *)
  let branch ctx (exp:exp) (tv:bool) : D.t =
    let m = ctx.local in
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    (* try to evaluate the expression using query
       -> if the result is the same as tv, do the corresponding transition, otherwise remove the entry from the domain
       for pointers this won't help since it always returns `Top *)
    ( let i = ctx.ask (Queries.EvalInt exp) in (* when (Queries.ID.is_bool i) *)
      (match Queries.ID.to_bool i with
      | Some b when b<>tv -> M.debug "EvalInt: `Int bool" (* D.remove k m TODO where to get the key?? *)
      | _ -> M.debug "EvalInt: `Int no bool")
    );
    let check a b tv =
      (* ignore(printf "check: %a = %a\n" d_plainexp a d_plainexp b); *)
      match a, b with
      | Const (CInt(i, kind, str)), Lval lval
      | Lval lval, Const (CInt(i, kind, str)) ->
        (* let binop = BinOp (Eq, a, b, Cil.intType) in *)
        (* standardize the format of the expression to 'lval==i'. -> spec needs to follow that format, the code is mapped to it. *)
        let binop = BinOp (Eq, Lval lval, Const (CInt(i, kind, str)), Cil.intType) in
        let key = D.key_from_lval lval in
        let value = D.find key m in
        if Cilint.is_zero_cilint i && tv then (
          M.debug "error-branch";
          (* D.remove key m *)
        )else(
          M.debug "success-branch";
          (* m *)
        );
        (* there should always be an entry in our domain for key *)
        if not (D.mem key m) then m else
          (* TODO for now we just assume that a Binop is used and Lval is the key *)
          (* get the state(s) that key is/might be in *)
          let states = D.get_states key m in
          (* compare SC.exp with Cil.exp and tv *)
          let branch_exp_eq c exp tv =
            (* let c_str = match SC.branch_exp c with Some (exp,tv) -> SC.exp_to_string exp | _ -> "" in
               let c_str = Str.global_replace (Str.regexp_string "$key") "%e:key" c_str in
               let c_exp = Formatcil.cExp c_str [("key", Fe (D.K.to_exp key))] in *)
            (* c_exp=exp *) (* leads to Out_of_memory *)
            match SC.branch_exp c with
            | Some (c_exp,c_tv) ->
              (* let exp_str = sprint d_exp exp in *) (* contains too many casts, so that matching fails *)
              let exp_str = sprint d_exp binop in
              let c_str = SC.exp_to_string c_exp in
              let c_str = Str.global_replace (Str.regexp_string "$key") (D.string_of_key key) c_str in
              (* ignore(printf "branch_exp_eq: '%s' '%s' -> %B\n" c_str exp_str (c_str=exp_str)); *)
              c_str=exp_str && c_tv=tv
            | _ -> false
          in
          (* filter those edges that are branches, start with a state from states and have the same branch expression and the same tv *)
          let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.is_branch c && List.mem a states && branch_exp_eq c exp tv) !edges in
          (* there should be only one such edge or none *)
          if List.compare_length_with branch_edges 1 <> 0 then ( (* call of branch for an actual branch *)
            M.debug "branch: branch_edges length is not 1! -> actual branch";
            M.debug "%s -> branch_edges1: %a" (D.string_of_entry key m) (Pretty.d_list "\n " (fun () x -> Pretty.text (SC.def_to_string (SC.Edge x)))) branch_edges;
            (* filter those edges that are branches, end with a state from states have the same branch expression and the same tv *)
            (* TODO they should end with any predecessor of the current state, not only the direct predecessor *)
            let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.is_branch c && List.mem b states && branch_exp_eq c exp tv) !edges in
            M.debug "%s -> branch_edges2: %a" (D.string_of_entry key m) (Pretty.d_list "\n " (fun () x -> Pretty.text (SC.def_to_string (SC.Edge x)))) branch_edges;
            if List.compare_length_with branch_edges 1 <> 0 then m else
              (* meet current value with the target state. this is tricky: we can not simply take the target state, since there might have been more than one element already before the branching.
                 -> find out what the alternative branch target was and remove it *)
              let (a,ws,fwd,b,c) = List.hd branch_edges in
              (* the alternative branch has the same start node, the same branch expression and the negated tv *)
              let (a,ws,fwd,b,c) = List.find (fun (a2,ws,fwd,b,c) -> SC.is_branch c && a2=a && branch_exp_eq c exp (not tv)) !edges in
              (* now b is the state the alternative branch goes to -> remove it *)
              (* TODO may etc. *)
              (* being explicit: check how many records there are. if the value is Must b, then we're sure that it is so and we don't remove anything. *)
              if D.V.length value = (1,1) then m else (* XX *)
                (* there are multiple possible states -> remove b *)
                let v2 = D.V.remove_state b value in
                (* M.debug @@ "branch: changed state from "^D.V.string_of value^" to "^D.V.string_of v2; *)
                D.add key v2 m
          ) else (* call of branch directly after splitting *)
            let (a,ws,fwd,b,c) = List.hd branch_edges in
            (* TODO may etc. *)
            let v2 = D.V.set_state b value in
            (* M.debug @@ "branch: changed state from "^D.V.string_of value^" to "^D.V.string_of v2; *)
            D.add key v2 m
      | _ -> M.debug "nothing matched the given BinOp: %a = %a" d_plainexp a d_plainexp b; m
    in
    match stripCasts (constFold true exp) with
    (* somehow there are a lot of casts inside the BinOp which stripCasts only removes when called on the subparts
       -> matching as in flagMode didn't work *)
    | BinOp (Eq, a, b, _) -> check (stripCasts a) (stripCasts b) tv
    | BinOp (Ne, a, b, _) -> check (stripCasts a) (stripCasts b) (not tv)
    | UnOp (LNot, a, _)   -> check (stripCasts a) (integer 0)    tv
    (* TODO makes 2 tests fail. probably check changes something it shouldn't *)
    (* | Lval _ as a         -> check (stripCasts a) (integer 0)    (not tv) *)
    | e -> M.debug "branch: nothing matched the given exp: %a" d_plainexp e; m

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let m = ctx.local in
    (* M.debug @@ "return: ctx.local="^D.short 50 m^D.string_of_callstack m; *)
    (* if f.svar.vname <> "main" && BatList.is_empty (D.callstack m) then M.debug @@ "\n\t!!! call stack is empty for function "^f.svar.vname^" !!!"; *)
    if f.svar.vname = "main" then (
      let warn_main msg_loc msg_end = (* there is an end warning for local, return or both *)
        (* find edges that have 'end' as a target *)
        (* we ignore the constraint, TODO maybe find a better syntax for declaring end states *)
        let end_states = BatList.filter_map (fun (a,ws,fwd,b,c) -> if b="end" then Some a else None) !edges in
        let must_not, may_not = D.filter_values (fun r -> not @@ List.exists (fun end_state -> D.V.in_state end_state r) end_states) m in
        let may_not = Set.diff may_not must_not in
        (match msg_loc with (* local warnings for entries that must/may not be in an end state *)
         | Some msg ->
           Set.iter (fun r -> D.warn           ~loc:(D.V.loc r) msg) must_not;
           Set.iter (fun r -> D.warn ~may:true ~loc:(D.V.loc r) msg) may_not
         | None -> ());
        (match msg_end with
         | Some msg -> (* warnings at return for entries that must/may not be in an end state *)
           let f msg rs = Str.global_replace (Str.regexp_string "$") (D.string_of_keys rs) msg in
           if Set.cardinal must_not > 0 then D.warn           (f msg must_not);
           if Set.cardinal may_not  > 0 then D.warn ~may:true (f msg may_not)
         | _ -> ())
      in
      (* check if there is a warning for entries that are not in an end state *)
      match SC.warning "_end" !nodes, SC.warning "_END" !nodes with
      | None, None -> () (* nothing to do here *)
      | msg_loc,msg_end -> warn_main msg_loc msg_end
    );
    (* take care of return value *)
    let au = match exp with
      | Some(Lval lval) when D.mem (D.key_from_lval lval) m -> (* we return a var in D *)
        let k = D.key_from_lval lval in
        let varinfo,offset = k in
        if varinfo.vglob then
          D.alias return_var k m (* if var is global, we alias it *)
        else
          D.add return_var (D.find' k m) m (* if var is local, we make a copy *)
      | _ -> m
    in
    (* remove formals and locals *)
    (* TODO only keep globals like in fileUse *)
    List.fold_left (fun m var -> D.remove' (var, `NoOffset) m) au (f.sformals @ f.slocals)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* M.debug @@ "entering function "^f.vname^D.string_of_callstack ctx.local; *)
    if f.svar.vname = "main" then load_specfile ();
    let m = if f.svar.vname <> "main" then
        D.edit_callstack (BatList.cons !Tracing.current_loc) ctx.local
      else ctx.local in [m, m]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    (* M.debug @@ "leaving function "^f.vname^D.string_of_callstack au; *)
    let au = D.edit_callstack List.tl au in
    let return_val = D.find_option return_var au in
    match lval, return_val with
    | Some lval, Some v ->
      let k = D.key_from_lval lval in
      (* remove special return var and handle potential overwrites *)
      let au = D.remove' return_var au (* |> check_overwrite_open k *) in
      (* if v.key is still in D, then it must be a global and we need to alias instead of rebind *)
      (* TODO what if there is a local with the same name as the global? *)
      if D.V.is_top v then (* returned a local that was top -> just add k as top *)
        D.add' k v au
      else (* v is now a local which is not top or a global which is aliased *)
        let vvar = D.V.get_alias v in (* this is also ok if v is not an alias since it chooses an element from the May-Set which is never empty (global top gets aliased) *)
        if D.mem vvar au then (* returned variable was a global TODO what if local had the same name? -> seems to work *)
          (* let _ = M.debug @@ vvar.vname^" was a global -> alias" in *)
          D.alias k vvar au
        else (* returned variable was a local *)
          let v = D.V.set_key k v in (* adjust var-field to lval *)
          (* M.debug @@ vvar.vname^" was a local -> rebind"; *)
          D.add' k v au
    | _ -> au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* let _ = GobConfig.set_bool "dbg.debug" false in *)
    let arglist = List.map (Cil.stripCasts) arglist in (* remove casts, TODO safe? *)
    let get_key c = match SC.get_key_variant c with
      | `Lval s ->
        M.debug "Key variant special `Lval %s; %s" s (SC.stmt_to_string c);
        lval
      | `Arg(s, i) ->
        M.debug "Key variant special `Arg(%s, %d). %s" s i (SC.stmt_to_string c);
        (try
           let arg = List.at arglist i in
           match arg with
           | Lval x -> Some x (* TODO enough to just assume the arg is already there as a Lval? *)
           | AddrOf x -> Some x
           | _      -> None
         with Invalid_argument s ->
           M.debug "Key out of bounds! Msg: %s" s; (* TODO what to do if spec says that there should be more args... *)
           None
        )
      | _ -> None (* `Rval or `None *)
    in
    let matches (a,ws,fwd,b,c) =
      let equal_args spec_args cil_args =
        if List.compare_length_with spec_args 1 = 0 && List.hd spec_args = `Free then
          true (* wildcard as an argument matches everything *)
        else if List.compare_lengths arglist spec_args <> 0 then (
          M.debug "SKIP the number of arguments doesn't match the specification!";
          false
        )else
          List.for_all2 (SpecCheck.equal_exp ctx) spec_args cil_args (* TODO Cil.constFold true arg. Test: Spec and c-file: 1+1 *)
      in
      (* function name must fit the constraint *)
      SC.fname_is f.vname c &&
      (* right form (assignment or not) *)
      SC.equal_form lval c &&
      (* function arguments match those of the constraint *)
      equal_args (SC.get_fun_args c) arglist
    in
    SpecCheck.check ctx get_key matches


  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
