signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv: ty Symbol.table            (*predefined types*)
  val base_venv: enventry Symbol.table      (*predefined functions*)
end

structure Envir :> ENV
struct

	type access = unit
 	type ty = Types.ty
 	structure T = Types
 	datatype enventry = VarEntry of {ty: ty}
	              	  | FunEntry of {formals: ty list, result: ty}

	val base_tenv = foldr (fn ((name,ty), table) => Symbol.enter(table, Symbol.symbol name, ty)) 
                      Symbol.empty
                      [
                      ("int", T.INT),
                      ("string", T.STRING)
                      ]

  val base_venv = foldr (fn ((name, enventry), table) => Symbol.enter(table, Symbol.symbol name, enventry))
  	   			Symbol.empty
    				[
    				("print", enventry.FunEntry {formals=[T.SRING], result=T.UNIT}),
    				("flush", enventry.FunEntry {formals=[], result=T.UNIT}),
    				("getchar", enventry.FunEntry {formals=[], result=T.STRING}),
    				("ord", enventry.FunEntry {formals=[T.STRING], result=T.INT}),
    				("chr", enventry.FunEntry {formals=[T.INT], result=T.STRING}),
    				("size", enventry.FunEntry {formals=[T.STRING], result=T.INT}),
    				("substring", enventry.FunEntry {formals=[T.STRING, T.INT, T.INT], result=T.STRING}),
    				("concat", enventry.FunEntry {formals=[T.STRING, T.STRING], result=T.STRING}),
    				("not", enventry.FunEntry {formals=[T.INT], result=T.INT}),
    				("exit", enventry.FunEntry {formals=[T.INT], result=T.UNIT})
    				]

end