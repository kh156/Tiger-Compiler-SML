type venv = Env.enventry Symbol.table
type tenv = ty Symbol.table

structure Translate = struct type exp = unit end

type expty = {exp: Translate.exp, ty: Types.ty}

transVar: venv * tenv * Absyn.var -> expty
transExp: venv * tenv * Absyn.exp -> expty
transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
transTy:  		 tenv * Absyn.ty -> Types.ty

