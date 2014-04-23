signature ENV =
sig
  type access
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result: Types.ty}
  val base_tenv: Types.ty Symbol.table            (*predefined types*)
  val base_venv: enventry Symbol.table      (*predefined functions*)

  val stdlibLevel : Translate.level
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

  val stdlibLabel = Temp.newlabel()
  val stdlibLevel = Trans.newLevel {parent = Trans.ROOT, name = stdlibLabel, formals = []}
  (*I don't think these standard library fns should be in the Trans.ROOT level...*)
  val base_venv = foldr (fn ((name, enventry), table) => Symbol.enter(table, Symbol.symbol name, enventry))
  	   			Symbol.empty
    				[
    				("print", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_print", formals=[T.STRING], result=T.UNIT}),
    				("flush", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_flush", formals=[], result=T.UNIT}),
    				("getchar", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_getchar", formals=[], result=T.STRING}),
    				("ord", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_ord", formals=[T.STRING], result=T.INT}),
    				("chr", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_chr", formals=[T.INT], result=T.STRING}),
    				("size", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_size", formals=[T.STRING], result=T.INT}),
    				("substring", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_substring", formals=[T.STRING, T.INT, T.INT], result=T.STRING}),
    				("concat", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_concat", formals=[T.STRING, T.STRING], result=T.STRING}),
    				("not", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_not", formals=[T.INT], result=T.INT}),
    				("exit", FunEntry {level=stdlibLevel, label=Te.namedlabel "tig_exit", formals=[T.INT], result=T.UNIT})
    				]

end