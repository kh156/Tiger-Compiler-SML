signature ENV =
sig
  type access
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result: Types.ty}
  val base_tenv: Types.ty Symbol.table            (*predefined types*)
  val base_venv: enventry Symbol.table      (*predefined functions*)
end

structure Envir :> ENV = 
struct
  structure T = Types
  structure Te = Temp
  structure Trans = Translate
	type access = unit
 	datatype enventry = VarEntry of {access: Trans.access, ty: T.ty}
	              	  | FunEntry of {level: Trans.level, label: Te.label, formals: T.ty list, result: T.ty}

	val base_tenv = foldr (fn ((name,ty), table) => Symbol.enter(table, Symbol.symbol name, ty)) 
                      Symbol.empty
                      [
                      ("int", T.INT),
                      ("string", T.STRING)
                      ]

  val base_venv = foldr (fn ((name, enventry), table) => Symbol.enter(table, Symbol.symbol name, enventry))
  	   			Symbol.empty
    				[
    				("print", FunEntry {level=Trans.ROOT, label=Te.namedlabel "print", formals=[T.STRING], result=T.UNIT}),
    				("flush", FunEntry {level=Trans.ROOT, label=Te.namedlabel "flush", formals=[], result=T.UNIT}),
    				("getchar", FunEntry {level=Trans.ROOT, label=Te.namedlabel "getchar", formals=[], result=T.STRING}),
    				("ord", FunEntry {level=Trans.ROOT, label=Te.namedlabel "ord", formals=[T.STRING], result=T.INT}),
    				("chr", FunEntry {level=Trans.ROOT, label=Te.namedlabel "chr", formals=[T.INT], result=T.STRING}),
    				("size", FunEntry {level=Trans.ROOT, label=Te.namedlabel "size", formals=[T.STRING], result=T.INT}),
    				("substring", FunEntry {level=Trans.ROOT, label=Te.namedlabel "substring", formals=[T.STRING, T.INT, T.INT], result=T.STRING}),
    				("concat", FunEntry {level=Trans.ROOT, label=Te.namedlabel "concat", formals=[T.STRING, T.STRING], result=T.STRING}),
    				("not", FunEntry {level=Trans.ROOT, label=Te.namedlabel "not", formals=[T.INT], result=T.INT}),
    				("exit", FunEntry {level=Trans.ROOT, label=Te.namedlabel "exit", formals=[T.INT], result=T.UNIT})
    				]

end