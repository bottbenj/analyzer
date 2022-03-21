open Prelude.Ana
open GobConfig
open Analyses

open struct
  let ( let= ) (c, r) f = if c then r else f ()

  let ( let=? ) (c, r) f = match c with None -> r | Some x -> f x
end

let handle ~(discard_state : ('a, 'b, 'c, 'd) ctx -> 'a)
    ?(discard_expression : (CilType.Lval.t -> ('a, 'b, 'c, 'd) ctx -> 'a) option)
    ?(read_expression : CilType.Exp.t -> ('a, 'b, 'c, 'd) ctx -> 'a = fun _ ctx -> ctx.local)
    (ctx : ('a, 'b, 'c, 'd) ctx) =
  let (MyCFG.ASM (_, outs, ins)) = ctx.edge [@@warning "-8"] in
  let apply f ctx = {ctx with local= f ctx} in
  let state = ctx in
  (* basic asm has no information we can use so discard all state to be sound *)
  (* TODO: properly track what asm are basic *)
  let is_basic = outs = [] && ins = [] in
  let= _ = (is_basic, discard_state state) in
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
      (fun ctx (_, _, lval) -> apply (discard_expression lval) ctx)
      state outs
  in
  state.local