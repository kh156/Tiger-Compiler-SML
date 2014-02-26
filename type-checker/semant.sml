signature sem = 
sig

val transVar: venv * tenv * Absyn.var -> expty
val transExp: venv * tenv * Absyn.exp -> expty
val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
val transTy:  		 tenv * Absyn.ty -> Types.ty
val transProg: 			 	Absyn.exp -> unit

end

strcture semant :> sem = 
struct

(*define return type of trans* functions*)
structure Translate = struct type exp = unit end
type expty = {exp: Translate.exp, ty: T.ty}

(*make code easier to read*)
structure A = Absyn
structure T = Types
structure E = Envir
structure S = Symbol

(*more meaningful/easier to read types*)
type venvTable = E.enventry S.table
type tenvTable = T.ty S.table


fun transProg(programCode : A.exp) = 
    let 
    	val venv = E.base_venv
		val tenv = E.base_tenv
    in 
		transExp(venv, tenv, programCode)
    end
end

fun transExp(venv, tenv, ...) = ... (*to be implemented*)

fun isSameType(t1: ty, t2: ty) = ... (*to be implemented*)


fun transTy(tenv: tenvTable, typeFromAbsyn: A.ty) = ...(* interpret type from Absyn.ty *)

fun transDec(venv, tenv, A.VarDec{name: A.symbol,
		     					  escape: bool ref,
		     					  typ: (A.symbol * A.pos) option,
		     					  init: A.exp,
		     					  pos: A.pos}) = 
	let
		val {exp, tyinit} = transExp(venv, tenv, init)
	in
		case typ of
			NONE => case tyinit=T.NIL of 
				false => {tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})}
				true => (ErrorMsg.error pos "variable is initialized to NIL type!";
						{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})})
			| SOME(s, p) => (
				case of S.look(tenv, s) of
					NONE => (ErrorMsg.error pos "declared type for variable does not exist!";
							{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})})
					SOME(t) => case of isSameType(tyinit, t)
							false => (ErrorMsg.error pos "declared type for variable doesn't match the type of initial expression!";
									{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})})
							true  => {tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})}
			)
	end

	| transDec(venv, tenv, A.TypeDec[{name: A.symbol, ty: A.ty, pos: A.pos}]) = 
		{tenv = S.enter(tenv, name, transTy(tenv, ty)), venv = venv}

	| transDec(venv, tenv, A.FunctionDec[]) = ...(*to be implemented*)

end