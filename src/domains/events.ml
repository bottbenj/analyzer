open Prelude.Ana

type t =
  | Lock of LockDomain.Addr.t  (** This is only emitted if the mutex was not previously held *)
  | Unlock of LockDomain.Addr.t
  | Escape of EscapeDomain.EscapedVars.t
  | EnterMultiThreaded
  | SplitBranch of exp * bool (** Used to simulate old branch-based split. *)
  | AssignSpawnedThread of lval * ThreadIdDomain.Thread.t (** Assign spawned thread's ID to lval. *)
  | Access of {var_opt: CilType.Varinfo.t option; write: bool} (** Access varinfo (unknown if None). *)
  | Assign of {lval: CilType.Lval.t; exp: CilType.Exp.t} (** Used to simulate old [ctx.assign]. *)
  | Invalidate of {lvals: CilType.Lval.t list} (** Used to simulate old [ctx.assign]. *)

let pretty () = function
  | Lock m -> dprintf "Lock %a" LockDomain.Addr.pretty m
  | Unlock m -> dprintf "Unock %a" LockDomain.Addr.pretty m
  | Escape escaped -> dprintf "Escape %a" EscapeDomain.EscapedVars.pretty escaped
  | EnterMultiThreaded -> text "EnterMultiThreaded"
  | SplitBranch (exp, tv) -> dprintf "SplitBranch (%a, %B)" d_exp exp tv
  | AssignSpawnedThread (lval, tid) -> dprintf "AssignSpawnedThread (%a, %a)" d_lval lval ThreadIdDomain.Thread.pretty tid
  | Access {var_opt; write} -> dprintf "Access {var_opt=%a, write=%B}" (docOpt (CilType.Varinfo.pretty ())) var_opt write
  | Assign {lval; exp} -> dprintf "Assugn {lval=%a, exp=%a}" CilType.Lval.pretty lval CilType.Exp.pretty exp
  | Invalidate{lvals} -> dprintf "Invalidate {lvals=%a}" (docList (CilType.Lval.pretty ())) lvals
