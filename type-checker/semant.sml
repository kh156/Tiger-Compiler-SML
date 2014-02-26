signature sem = 
sig

val transVar: venv * tenv * Absyn.var -> expty
val transExp: venv * tenv * Absyn.exp -> expty
val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
val transTy:   	     tenv * Absyn.ty -> Types.ty
val transProg:        		Absyn.exp -> unit

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

(*main entry point for type-checking a program*)
fun transProg(programCode : A.exp) = 
    let 
    	val venv = E.base_venv
    	val tenv = E.base_tenv
    in 
    	transExp(venv, tenv, programCode)
    end


fun transExp(venv, tenv, ...) = ... (*to be implemented*)

fun isSameType(t1: T.ty, t2: T.ty) = ... (*to be implemented*)

fun actual_ty(ty: T.ty, pos: A.pos) =
	case ty of
		T.NAME(s, tref) => (case !tref of
							     SOME(t) => actual_t (t,pos)
							     | NONE => (ErrorMsg.error pos ("Undefined type with name: "^(S.name s)); T.ERROR))
		| _ => ty


fun t(tenv, A.NameTy(s:A.symbol, pos:A.pos)) =
	case S.lookup(tenv, s) of NONE => (ErrorMsg.error pos "Undefined type with name"^(S.name s); T.ERROR)
							| SOME(t) => t

| transTy(tenv, A.RecordTy recordTypeList) =
	let 
		val fields=
	    let
	    	fun checkSingleField ({name: A.symbol, escape: bool ref, typ: A.symbol, pos: A.pos}) =
		     	case S.look(tenv, typ) of NONE => (ErrorMsg.error pos ("Undefined type: "^(S.name typ)^" when declaring an array type!");
		     										(name, T.ERROR))
					   					| SOME(t) => (name, t)
	     in
		 	map checkSingleField recordTypeList
	     end
    in
		T.RECORD(fields, ref ())
    end
	

| transTy(tenv, A.ArrayTy (s:A.symbol, pos:A.pos)) =
	case S.lookup(tenv, s) of NONE => (ErrorMsg.error pos "Undefined type "^(S.name s)^" when declaring an array type!";
										T.ARRAY(T.ERROR, ref())) (*how to use ref()?*)
							| SOME(t) => T.ARRAY((actual_ty (t,pos)), ref())


(*book says type NIL must be constrained by a RECORD type???*)
fun transDec(venv, tenv, A.VarDec{name: A.symbol,
                                  escape: bool ref,
                                  typ: (A.symbol * A.pos) option,
                                  init: A.exp,
                                  pos: A.pos}) = 
	let
		val {exp, tyinit} = transExp(venv, tenv, init)
	in
		case typ of
			NONE => (case tyinit=T.NIL of 
				false => {tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})}
				| true => (ErrorMsg.error pos "variable is initialized to NIL type!";
				    	{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})}))
			| SOME(s, p) => (
				case of S.look(tenv, s) of
					NONE => (ErrorMsg.error pos "declared type for variable does not exist!";
							{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})})
					| SOME(t) => (case of isSameType(tyinit, t)
							false => (ErrorMsg.error pos "declared type for variable doesn't match the type of initial expression!";
							    	{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})})
							| true  => {tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})}))
	end

| transDec(venv, tenv, A.TypeDec typeDecList) = 
	let
		fun addSingleType({name: A.symbol, ty: A.ty, pos: A.pos}, tenvCurr) =
			S.enter(tenvCurr, name, transTy(tenvCurr, ty))
	in
		{tenv = foldr addSingleType tenv typeDecList, venv = venv}
	end

| transDec(venv, tenv, A.FunctionDec funcs) =
	let 
		fun secondPass (venv,[]) = ()
	 	| 	secondPass (venv, {name, params, body, pos, result}::func) =
			let 
				val retoption = case result of 
					SOME(rt,p) => S.look(tenv,rt)
				| 	NONE => NONE
				val tyret = case retoption of 
					SOME(t) => t
				|	NONE => T.UNIT 

				fun enterparam ({name, escape, typ, pos}, venvCurr) = 
					let 
						var ty = case S.look(tenv, typ) of
					    	SOME t => t
					    | 	NONE => T.ERROR
					in
						S.enter (venvCurr, name, E.VarEntry {ty = ty})
					end
				val venv' = foldr enterparam venv params
				val {exp = _, ty = tybody} = transExp (venv', tenv, body)
			in (
				case comparetype(tybody,tyret) of
					true => ()
			    | 	false => ErrorMsg.error pos "Function body type and return type do not mactch!";
				secondPass (venv, func)
			)
			end

		fun firstPass ({name, params, body, pos, result}, venvCurr) =
			let 
				val retoption = case result of 
					SOME(rt,p) => S.look(tenv,rt)
				| 	NONE => NONE
				val tyret = case retoption of 
					SOME(t) => t
				|	NONE => T.UNIT 
				fun transparam {name, escape, typ, pos} = 
				    case S.look(tenv, typ) of
				    	SOME t => t
				    | 	NONE => (ErrorMsg.error pos "Unknown params type in function declaration"; T.ERROR)
				val params' = map transparam params
			in 
				S.enter(venvCurr, name, E.FunEntry {formals = params', result = tyret})
			end

		var venv' = foldr firstPass venv funcs;
	in 
		secondPass venv' funcs;
		{venv = venv', tenv = tenv}
	end


fun transVar(venv, tenv, A.SimpleVar (s: A.symbol, pos: A.pos)) = 
	case of S.look(venv, s) of
		SOME(E.VarEntry {ty}) => {exp = (), ty = actual_ty (ty, pos)}
		| SOME(_)			=> (ErrorMsg.error pos "Var with name "^(S.name s)^" is a function, not a simple variable!"; {exp = (), ty = T.ERROR})
		| NONE()		    => (ErrorMsg.error pos "Undefined variable "^(S.name s); {exp = (), ty = T.ERROR})

| transVar(venv, tenv, A.FieldVar (var: A.var, s: A.symbol, pos: A.pos)) = 
	let
		var {exp = _, ty = parentType} = transVar(venv, tenv, var)
		fun findMatchField(oneField::rest, symToLook, unique) =
			case oneField of
				(name, t) => (if name=symToLook then {exp = (), ty = t} else findMatchField(rest, symToLook, unique))
				[]	=> (ErrorMsg.error pos "Did not find field "^(S.name s)^" from parent record!"; {exp = (), ty = T.ERROR})
				_	=> (ErrorMsg.error pos "Error trying to access field "^(S.name s)^" of parent record!"; {exp = (), ty = T.ERROR})
	in
		(case parentType of T.RECORD(fields, unique) => findMatchField(fields, s, unique)
							| _ => (ErrorMsg.error pos "Trying to access field "^(S.name s)^" whose parent is not a record type!";
									{exp = (), ty = T.ERROR}))
	end

| transVar(venv, tenv, A.SubscriptVar (var: A.var, exp: A.exp, pos: A.pos)) = 
	let 
		val {exp = _, ty = varType} = transVar(venv, tenv, var)
		val {exp = _, ty = expType} = transExp(venv, tenv, exp) 
    in 
		case expType of T.INT => (
				case varType of T.ARRAY (eleType, unique) => {exp = (), ty = ty}
		   	  		 | _ => (ErrorMsg.error pos "Var must be an array type to access an indexed element!";
		      			     {exp = (), ty = T.ERROR}))
		| _ => (ErrorMsg.error pos "Exp to access an element in an array should be an integer!";
				{exp = (), ty = T.ERROR})
    end

end