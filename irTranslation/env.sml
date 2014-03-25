signature ENV =
sig
  type access
  datatype enventry = VarEntry of {access: Translate.access, ty: T.ty}
                    | FunEntry of {level: Translate.level, label: Te.Label, formals: T.ty list, result: T.ty}
  val base_tenv: Types.ty Symbol.table            (*predefined types*)
  val base_venv: enventry Symbol.table      (*predefined functions*)
end

structure Envir :> ENV = 
struct
  structure T = Types
  structure Te = Temp
	type access = unit
 	datatype enventry = VarEntry of {access: Translate.access, ty: T.ty}
	              	  | FunEntry of {level: Translate.level, label: Te.Label, formals: T.ty list, result: T.ty}

	val base_tenv = foldr (fn ((name,ty), table) => Symbol.enter(table, Symbol.symbol name, ty)) 
                      Symbol.empty
                      [
                      ("int", T.INT),
                      ("string", T.STRING)
                      ]

  val base_venv = foldr (fn ((name, enventry), table) => Symbol.enter(table, Symbol.symbol name, enventry))
  	   			Symbol.empty
    				[
    				("print", FunEntry {formals=[T.STRING], result=T.UNIT}),
    				("flush", FunEntry {formals=[], result=T.UNIT}),
    				("getchar", FunEntry {formals=[], result=T.STRING}),
    				("ord", FunEntry {formals=[T.STRING], result=T.INT}),
    				("chr", FunEntry {formals=[T.INT], result=T.STRING}),
    				("size", FunEntry {formals=[T.STRING], result=T.INT}),
    				("substring", FunEntry {formals=[T.STRING, T.INT, T.INT], result=T.STRING}),
    				("concat", FunEntry {formals=[T.STRING, T.STRING], result=T.STRING}),
    				("not", FunEntry {formals=[T.INT], result=T.INT}),
    				("exit", FunEntry {formals=[T.INT], result=T.UNIT})
    				]

end