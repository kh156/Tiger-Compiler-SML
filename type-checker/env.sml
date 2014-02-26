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


<<<<<<< HEAD
  val base_venv = foldr (fn ((name, enventry), table) => Symbol.enter(table, Symbol.symbol name, enventry))
  	   			Symbol.empty
=======
    val base_venv = foldr (fn ((name, enventry), table) => Symbol.enter(table, Symbol.symbol name, enventry))
    				Symbol.empty
>>>>>>> 6b2345571d477fbf2324129ad7b6d9ae5beba317
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
<<<<<<< HEAD
    				("exit", enventry.FunEntry {formals=[T.INT], result=T.UNIT})
=======
    				("exit", enventry.FunEntry {formals=[T.INT], result=T.UNIT}),
>>>>>>> 6b2345571d477fbf2324129ad7b6d9ae5beba317
    				]

end