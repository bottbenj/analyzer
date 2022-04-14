open Prelude.Ana
open GobConfig
open Analyses

let ( let=? ) (c, r) f = match c with None -> r | Some x -> f x

type constr = Read | ReadWrite | Write

let check_constraint c =
  match String.find c "=" with
  | _ -> Write
  | exception Not_found -> 
    match String.find c "+" with 
    | _ -> ReadWrite 
    | exception Not_found -> Read

let handle ~(discard_state : ('a, 'b, 'c, 'd) ctx -> 'a)
    ?(discard_expression : (CilType.Lval.t -> ('a, 'b, 'c, 'd) ctx -> 'a) option)
    ?(read_expression : CilType.Exp.t -> ('a, 'b, 'c, 'd) ctx -> 'a = fun _ ctx -> ctx.local)
    ?(discard_globals : (('a, 'b, 'c, 'd) ctx -> 'a) option)
    (ctx : ('a, 'b, 'c, 'd) ctx) =
  let (MyCFG.ASM (_, asm)) = ctx.edge [@@warning "-8"] in
  let discard_state = if get_bool "sem.asm.basic-preserve-globals" then (fun ctx -> ctx.local) else discard_state in
  let apply f ctx = {ctx with local= f ctx} in
  let state = ctx in
  (* basic asm has no information we can use so discard all state to be sound *)
  let=? outs, ins, clobber = (asm, (Option.default discard_state discard_globals) state) in
  (* handle reads *)
  let state =
    List.fold_left
      (fun ctx (_, _, exp) -> apply (read_expression exp) ctx)
      state ins
  in
  (* handle writes *)
  let=? discard_expression = (discard_expression, discard_state state) in
  let state =
    List.fold_left
      (fun ctx (_, c, lval) ->
         match check_constraint c with
         | Read ->
           warn "" |> ignore ;
           ctx
         | Write ->
           apply (discard_expression lval) ctx
         | ReadWrite ->
           ctx
           |> apply (read_expression (Cil.Lval lval))
           |> apply (discard_expression lval) )
      state outs
  in
  (* handle memory clobbers *)
  let state =
    if List.mem "memory" clobber && not (get_bool "sem.asm.memory-preserve-globals") then
      let=? discard_globals = (discard_globals, apply discard_state state) in
      apply discard_globals state
    else state
  in
  state.local


(*
todo: check making an invalidate event to notify the other analysis
*)