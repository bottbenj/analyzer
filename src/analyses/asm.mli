val handle :
  discard_state:(('a, 'b, 'c, 'd) Analyses.ctx -> 'a)
  -> ?discard_expression:(Cil.lval -> ('a, 'b, 'c, 'd) Analyses.ctx -> 'a)
  -> ?read_expression:(Cil.exp -> ('a, 'b, 'c, 'd) Analyses.ctx -> 'a)
  -> ?discard_globals:(('a, 'b, 'c, 'd) Analyses.ctx -> 'a)
  -> ('a, 'b, 'c, 'd) Analyses.ctx
  -> 'a
