open Cil
module E = Errormsg
module GU = Goblintutil

let init () =
  initCIL ();
(*  lineDirectiveStyle := None;*)
  Rmtmps.keepUnused := true;
  print_CIL_Input := true

let currentStatement = ref dummyStmt
let ugglyImperativeHack = ref dummyFile
let showtemps = ref false
    
let parse fileName = 
  Frontc.parse fileName ()

let print (fileAST: file) = 
  dumpFile defaultCilPrinter stdout "stdout" fileAST
    
let printDebug fileAST = 
  dumpFile Printer.debugCilPrinter stdout "stdout" fileAST

let rmTemps fileAST = 
  Rmtmps.removeUnusedTemps fileAST

class allBBVisitor = object
  inherit nopCilVisitor 
  method vstmt s =
    match s.skind with
      | Instr(il) ->
          let list_of_stmts = 
            List.map (fun one_inst -> mkStmtOneInstr one_inst) il in
          let block = mkBlock list_of_stmts in
            ChangeDoChildrenPost(s, (fun _ -> s.skind <- Block(block); s))
      | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end 

let end_basic_blocks f =
  let thisVisitor = new allBBVisitor in
  visitCilFileSameGlobals thisVisitor f  

let createCFG (fileAST: file) =
  end_basic_blocks fileAST; 
  (* Partial.calls_end_basic_blocks fileAST; *)
  Partial.globally_unique_vids fileAST; 
  iterGlobals fileAST (fun glob -> 
    match glob with
      | GFun(fd,_) -> 
          prepareCFG fd; 
          computeCFGInfo fd true
      | _ -> ()
  )

let partial fileAST =
  Partial.partial fileAST "main" []

let simplify fileAST =
  iterGlobals fileAST Simplify.doGlobal

let oneret fileAST =
  iterGlobals fileAST (fun glob -> 
    match glob with
      | GFun(fd,_) -> Oneret.oneret fd; 
      | _ -> ()
  )

let getAST fileName = 
  let fileAST = parse fileName in
    (*  rmTemps fileAST; *)
    (*  oneret fileAST;*)
    (*  simplify fileAST;*)
    fileAST

let getMergedAST fileASTs = 
  let merged = Mergecil.merge fileASTs "stdout" in
    if !E.hadErrors then
      E.s (E.error "There were errors during merging\n");
    merged

exception Found of fundec
let getFun fun_name = 
  try 
    iterGlobals !ugglyImperativeHack (fun glob ->
      match glob with 
        | GFun({svar={vname=vn}} as def,_) when vn = fun_name -> raise (Found def)
        | _ -> ()
    );
    failwith ("Function "^ fun_name ^ " not found!")
  with
    | Found def -> def

let in_section check attr_list = 
  let f attr = match attr with
    | Attr ("section", [AStr str]) -> check str
    | _ -> false
  in List.exists f attr_list

let is_init = in_section (fun s -> s = ".init.text")
let is_initptr = in_section (fun s -> s = ".initcall6.init")
let is_exit = in_section (fun s -> s = ".exit.text")

let rec get_varinfo exp: varinfo = 
  (* ignore (Pretty.printf "expression: %a\n" (printExp plainCilPrinter) exp); *)
  match exp with
    | AddrOf (Var v, _) -> v
    | CastE (_,e) -> get_varinfo e
    | _ -> failwith "Unimplemented: searching for variable in more complicated expression"

exception MyException of varinfo
let find_module_init funs fileAST = 
  try iterGlobals fileAST (
    function 
      | GVar ({vattr=attr}, {init=Some (SingleInit exp) }, _) when is_initptr attr -> 
          raise (MyException (get_varinfo exp))
      | _ -> ()
    ); 
    (funs, [])
  with MyException var -> 
    let f (s:fundec) = s.svar.vname = var.vname in
      List.partition f funs

(*brutal osek hack*)
  let is_task f =  (String.length f >= 12 && String.sub f 0 12 = GU.taskprefix)

type startfuns = fundec list * fundec list * fundec list

let getFuns fileAST : startfuns =
  let add_main f (m,e,o) = (f::m,e,o) in
  let add_exit f (m,e,o) = (m,f::e,o) in
  let add_other f (m,e,o) = (m,e,f::o) in
  let add_funname fn lst = lst := fn :: !lst in
  let f acc glob =
    match glob with 
      | GFun({svar={vname=mn}} as def,_) when List.mem mn !GU.mainfuns -> add_main def acc
      | GFun({svar={vname=mn}} as def,_) when List.mem mn !GU.exitfuns -> add_exit def acc
      | GFun({svar={vname=mn; vattr=attr}} as def, _) when !GU.kernel && is_init attr -> 
          Printf.printf "Start function: %s\n" mn; add_funname mn GU.mainfuns; add_main def acc
      | GFun({svar={vname=mn; vattr=attr}} as def, _) when !GU.kernel && is_exit attr -> 
          Printf.printf "Cleanup function: %s\n" mn; add_funname mn GU.exitfuns; add_exit def acc
      | GFun ({svar={vstorage=NoStorage}} as def, _) when !GU.nonstatic -> add_other def acc
      | GFun (def, _) when (!GU.allfuns) ->  add_other def  acc
      | GFun (def, _) when !GU.oil && is_task def.svar.vname -> add_other def acc
      | _ -> acc
  in
  foldGlobals fileAST f ([],[],[])

let dec_table_ok = ref false
let dec_table = Hashtbl.create 111
let dec_make () : unit =
  dec_table_ok := true ;
  Hashtbl.clear dec_table;
  iterGlobals !ugglyImperativeHack (fun glob ->
    match glob with 
      | GFun({svar={vid=vid}} as def,_) -> Hashtbl.add dec_table vid def
      | _ -> ()
  )
  
let rec getdec fv = 
  if !dec_table_ok then
    Hashtbl.find dec_table fv.vid
  else begin
    dec_make ();
    getdec fv
  end

let getFirstStmt fd = List.hd fd.sbody.bstmts

let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

let p_expr exp = Pretty.printf "%a\n" (printExp defaultCilPrinter) exp
let d_expr exp = Pretty.printf "%a\n" (printExp plainCilPrinter) exp

let rec typeOf (e: exp) : typ = 
  match e with
  | Const(CInt64 (_, ik, _)) -> TInt(ik, [])

    (* Character constants have type int.  ISO/IEC 9899:1999 (E),
     * section 6.4.4.4 [Character constants], paragraph 10, if you
     * don't believe me. *)
  | Const(CChr _) -> intType

    (* The type of a string is a pointer to characters ! The only case when 
     * you would want it to be an array is as an argument to sizeof, but we 
     * have SizeOfStr for that *)
  | Const(CStr s) -> charPtrType

  | Const(CWStr s) -> TPtr(!wcharType,[])

  | Const(CReal (_, fk, _)) -> TFloat(fk, [])

  | Const(CEnum(tag, _, ei)) -> typeOf tag

  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> !typeOfSizeOf
  | AlignOf _ | AlignOfE _ -> !typeOfSizeOf
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, a) -> TPtr(t, a)
     | _ -> raise Not_found
  end

and typeOfInit (i: init) : typ = 
  match i with 
    SingleInit e -> typeOf e
  | CompoundInit (t, _) -> t

and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match unrollType (typeOf addr) with
        TPtr (t, _) -> typeOffset t off
      | _ -> raise Not_found
  end

and typeOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) = 
      partitionAttributes ~default:(AttrName false) baseAttrs in
    typeAddAttributes contageous
  in
  function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, baseAttrs) ->
	  let elementType = typeOffset t o in
	  blendAttributes baseAttrs elementType
      | t -> raise Not_found
  end 
  | Field (fi, o) ->
      match unrollType basetyp with
        TComp (_, baseAttrs) ->
	  let fieldType = typeOffset fi.ftype o in
	  blendAttributes baseAttrs fieldType
      | _ -> raise Not_found
