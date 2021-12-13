
open Prelude.Ana
open Analyses

(** An analysis that tracks the origin of a value.
    It only considers definite values of local variables.
    We do not pass information interprocedurally. *)
module Spec : Analyses.MCPSpec =
struct
  let name () = "origin"

  module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

  module I = IntDomain.Flattened

  module Origin = Lattice.Prod (I) (PL)
  module OriginSet = SetDomain.ToppedSet (Origin) (struct let topname = "All" end)

  (* Map of (local int) variables to flat integers *)
  (*module D = MapDomain.MapBot (Basetype.Variables) (Origin)*)
  module D = struct
    include MapDomain.MapBot (Basetype.Variables) (OriginSet)
  end
  (* No information about globals*)
  module G = Lattice.Unit
  (* No contexts*)
  module C = Lattice.Unit

  include Analyses.IdentitySpec
  let context _ _ = ()

  let is_pointer_or_integer_var (v: varinfo) =
    match v.vtype with
    | TInt _ -> true
    | TPtr _ -> true
    | _ -> false

  let get_local = function
    | Var v, NoOffset when is_pointer_or_integer_var v && not v.vglob -> Some v (* local integer or pointer variable whose address is maybe taken *)
    | _, _ -> None

  (** Evaluates expressions *)
  (* let rec eval (state : D.t) (e: exp) (node: PL.t) =
      let int_val = match e with
      | Const c -> (match c with
        | CInt64 (i,_,_) -> I.of_int i
        | _ -> I.top ()
        )
      | Lval lv -> (match get_local lv with
        | Some v -> fst (D.find v state)
        | _ -> I.top ()
        )
      | BinOp (PlusA, e1, e2, t) -> (
        let v1 = fst (eval state e1 node) in
        let v2 = fst (eval state e2 node) in
        I.add v1 v2
      )
      | AddrOf (Var v, _) -> fst (D.find v state)
      | _ -> I.top ()
      in (int_val, node) *)

  (* Taken from arinc *)
  let mayPointTo ctx exp =
    match ctx.ask (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && Queries.LS.cardinal a > 0 ->
      let top_elt = (dummyFunDec.svar, `NoOffset) in
      let a' = if Queries.LS.mem top_elt a then (
          M.debug "mayPointTo: query result for %a contains TOP!" d_exp exp; (* UNSOUND *)
          Queries.LS.remove top_elt a
        ) else a
      in
      Queries.LS.elements a'
    | v ->
      M.debug "mayPointTo: query result for %a is %a" d_exp exp Queries.LS.pretty v;
      []

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let node = match !MyCFG.current_node with
      | Some n -> `Lifted n
      | _ -> PL.top ()
    in
    match get_local lval with
    (* | Some loc -> D.add loc (eval ctx.local rval node) ctx.local *)
    | Some loc -> 
      let curr_set = D.find loc ctx.local in
      (* let value = IntDomain.IntDomTuple.tag (ctx.ask (Queries.EvalInt rval)) in *)
      let values = mayPointTo ctx rval in
      let new_set = List.fold_left (fun s x -> OriginSet.add (`Lifted (Int64.of_int (Lval.CilLval.hash x)), node) s) curr_set values in 
      (*let new_set = values OriginSet.add (value, node) curr_set in*)
      D.add loc new_set ctx.local
    | None -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t = ctx.local
  (*let node = match !MyCFG.current_node with
    | Some n -> (`Lifted n)
    | _ -> PL.top ()
    in
    let v = eval ctx.local exp node in
    match I.to_bool (fst v) with
    | Some b when b <> tv -> raise Deadcode (* if the expression evaluates to not tv, the tv branch is not reachable *)
    | _ -> ctx.local *)

  let body ctx (f:fundec) : D.t =
    (* Initialize locals to top *)
    List.fold (fun m l -> D.add l (OriginSet.empty ()) m) ctx.local f.slocals

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Do nothing, as we are not interested in return values for now. *)
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Set the formal int arguments to top *)
    let callee_state = List.fold (fun m l -> D.add l (OriginSet.empty ()) m) (D.bot ()) f.sformals in
    [(ctx.local, callee_state)]

  let set_local_int_lval_top (state: D.t) (lval: lval option) =
    match lval with
    | Some lv ->
      (match get_local lv with
       | Some local -> D.add local (OriginSet.empty ()) state
       | _ -> state
      )
    |_ -> state

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    (* If we have a function call with assignment
        x = f (e1, ... , ek)
        with a local int variable x on the left, we set it to top *)
    set_local_int_lval_top ctx.local lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* When calling a special function, and assign the result to some local int variable, we also set it to top. *)
    set_local_int_lval_top ctx.local lval

  let startstate v = D.bot ()
  let exitstate v = D.top () (* TODO: why is this different from startstate? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
