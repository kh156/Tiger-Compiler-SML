(*make code easier to read*)
structure A = Absyn
structure T = Types
structure E = Envir
structure S = Symbol
(*define return type of trans* functions*)
structure Translate = struct type exp = unit end

signature SEM = 
sig
	(*more meaningful/easier to read types*)
	type venvTable
	type tenvTable
	type expty
	type ty

	val transVar: venvTable * tenvTable * Absyn.var -> expty
	val transExp: venvTable * tenvTable * Absyn.exp -> expty
	val transDec: venvTable * tenvTable * Absyn.dec -> {venv: venvTable, tenv: tenvTable}
	val transTy:   	     	  tenvTable * Absyn.ty -> ty
	val transProg:        			      Absyn.exp -> unit
end

structure Semant :> SEM = 
struct

(*more meaningful/easier to read types*)
type venvTable = E.enventry S.table
type tenvTable = T.ty S.table
type expty = {exp: Translate.exp, ty: T.ty}
type ty = T.ty
val error = ErrorMsg.error
val nestDepth = ref 0

fun incNestDepth () = nestDepth := !nestDepth + 1
fun decNestDepth () = nestDepth := !nestDepth - 1

fun checkInt ({exp,ty},pos) = 
  case ty of
	T.Int => ()
  | _ => error pos "integer required"

fun checkUnit ({exp, ty}, pos) =
  case ty of
    T.UNIT => ()
  | _ => error pos "unit required"

fun checkString ({exp, ty}, pos) =
  case ty of
    T.STRING => ()
  | _ => error pos "string required"

fun lookupType (tenv typ pos) = 
  case S.look (tenv, typ) of
    SOME ty => ty
  | NONE => (err pos ("type is not defined: " ^ S.name n) ; T.UNIT))

fun compareTypes (tenv, [], ) = 
	(tenv, ty1::lst1, ty2::lst2) =
		if comparetype(ty1,ty2) then compareTypes(tenv,lst1,lst2)
		else (err pos ("types do not match") ; T.UNIT))

fun transExp(venv, tenv) =
	    transExp (venv, tenv, A.NilExp) = {exp=(), T.NIL}
	  | transExp (venv, tenv, A.IntExp i) = {exp=(), T.INT}
	  | transExp (venv, tenv, A.VarExp v) = trvar v
	  | transExp (venv, tenv, A.StringExp (s, pos)) = (exp=(), T.STRING)
	  | transExp (venv, tenv, A.SeqExp []) = {exp=(), ty=T.UNIT}
	  | transExp (venv, tenv, A.SeqExp exps) = 
	  | transExp (venv, tenv, A.OpExp{left,oper,right,pos}) =
  		if (oper=A.PlusOp orelse oper=A.MinusOp 
  		orelse oper=A.TimesOp orelse oper=A.DivideOp)
  	    then (checkInt(transExp left, pos);
		 	  checkInt(transExp right, pos);
		 	  {exp=(),ty=T.INT})
		else if (oper=A.EqOp orelse oper=A.NeqOp 
		orelse oper=A.LtOp orelse oper=A.LeOp
		orelse oper=A.GtOp orelse oper=A.LtOp)
		then
			case #ty transExp(left) of
				T.INT => (checkInt(transExp right, pos);
	 	  				   {exp=(),ty=T.INT})
			  | T.STRING => (checkString(transExp right, pos);
	 	  				      {exp=(),ty=T.STRING})
			  | _ => (error pos "Can't perform an operation on this type");
			  		  {exp=(),ty=T.INT}
		else
			(error pos "Error");
			{exp=(),ty=T.INT}

	  | transExp (venv, tenv, A.RecordExp{fields,typ,exp}) = 

	  | transExp (venv, tenv, A.AssignExp{var,exp,pos}) =
	  	if #ty trvar(var) = #ty transExp(exp)
	  	then {exp=(),ty=T.UNIT}
	  	else 
	  		(error pos "Types of variable and expression do not match");
			{exp=(),ty=T.UNIT}

	  | transExp (venv, tenv, A.LetExp{decs,body,pos}) =
	  	let val {venv=venv',tenv=tenv'} =
	  			   transDecs(venv,tenv,decs)
	  	 in transExp(venv',tenv') body
	  	end
	  | transExp (venv, tenv, A.CallExp{func, args, pos}) =
(*	  	case S.look(venv, func) of
	  		SOME (FunEntry of {formals, result}) =>
	  			if length(args) <> length(formals) then
	  				(error pos "Number of arguments incorrect: " ^ length(args); {exp=(),ty=T.UNIT})
	  			else if*)



(*	  		_ => (error pos "This function does not exist" ^ Symbol.name(func); {exp=(),ty=T.UNIT})*)

	  | transExp (venv, tenv, A.IfExp {test, then', else', pos}) =
	  	(case else' of
         NONE => (* if-then *)
         	let 
         		val t = transExp(venv, tenv) test
         	in
         		(checkInt(t);
         		checkUnit('then);
         		{ exp=(), ty=T.UNIT })
         	end
         SOME else' => (* if-then-else *)
         	let 
         		val t = transExp(venv, tenv) test
         		val thenType = transExp(venv, tenv) then'
         		val elseType = transExp(venv, tenv) else'
         	in
         		checkInt(t);
         		if #ty thenType = #ty elseType then
         			{ exp=(), ty=#ty thenType }
         		else
         			(error pos "Types of Then and Else statements do not match";
         			 { exp=(), ty=T.UNIT })
         	end
	  | transExp (venv, tenv, A.ForExp {var, escape, lo, hi, body, pos}) =
	  	(checkInt(lo);
	  	checkInt(hi);
	  	checkUnit(body);
	  	S.enter (venv, var, Env.VarEntry {access=access, ty=Types.INT});
	  	{ exp=(), ty=T.UNIT })
	  | transExp (venv, tenv, A.WhileExp {test, body, pos}) =
	    let
	    	val _ = incNestDepth()
	  		val t = transExp(venv, tenv) test
	  		val b = transExp(venv, tenv) body
	  		val _ = decNestDepth()
	  	in
	  		(checkInt(t);
	  		checkUnit(b);
	  		{ exp=(), ty=T.UNIT })
	  	end
	  | transExp (venv, tenv, A.BreakExp pos) =
	  	if !nestDepth > 0 then { exp=(), ty=T.UNIT }
	  	else (error pos "Invalid nesting depth for a Break";
	  		  { exp=(), ty=T.UNIT })
	  | transExp (A.ArrayExp {typ, size, init, pos}) = { exp=(), ty=T.UNIT }

(*main entry point for type-checking a program*)
fun transProg(programCode : A.exp) = 
    let 
    	val venv = E.base_venv
    	val tenv = E.base_tenv
    in 
    	transExp(venv, tenv, programCode)
    end


(*fun transExp(venv, tenv, ...) = ... (*to be implemented*)*)

(*fun isSameType(t1: T.ty, t2: T.ty) = ... (*to be implemented*)*)

fun actual_ty(ty: T.ty, pos: A.pos) =
	case ty of
		T.NAME(s, tref) => (case !tref of
							     SOME(t) => actual_ty (t,pos)
							     | NONE => (ErrorMsg.error pos ("Undefined type with name: "^(S.name s)); T.ERROR))
		| _ => ty

fun transTy(tenv, A.NameTy(s:A.symbol, pos:A.pos)) =
	(case S.look(tenv, s) of NONE => (ErrorMsg.error pos ("Undefined type with name"^(S.name s)); T.ERROR)
							| SOME(t) => t)

| transTy(tenv, A.RecordTy recordTypeList) =
	let 
		val fields=
	    let
	    	fun checkSingleField ({name: A.symbol, escape: bool ref, typ: A.symbol, pos: A.pos}) =
		     	(case S.look(tenv, typ) of NONE => (ErrorMsg.error pos (("Undefined type: "^(S.name typ)^" when declaring an array type!"));
		     										(name, T.ERROR))
					   					| SOME(t) => (name, t))
	     in
		 	map checkSingleField recordTypeList
	     end
    in
		T.RECORD(fields, ref ())
    end
	

| transTy(tenv, A.ArrayTy (s:A.symbol, pos:A.pos)) =
	(case S.look(tenv, s) of NONE => (ErrorMsg.error pos ("Undefined type "^(S.name s)^" when declaring an array type!");
										T.ARRAY(T.ERROR, ref())) (*how to use ref()?*)
							| SOME(t) => T.ARRAY((actual_ty (t,pos)), ref()))


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
				case S.look(tenv, s) of
					NONE => (ErrorMsg.error pos "declared type for variable does not exist!";
							{tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})})
					| SOME(t) => (case isSameType(tyinit, t) of
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
	in (
		secondPass venv' funcs;
		{venv = venv', tenv = tenv}
	)
	end
	

fun transVar(venv, tenv, A.SimpleVar (s: A.symbol, pos: A.pos)) = 
	(case S.look(venv, s) of
		SOME(E.VarEntry {varType}) => {exp = (), ty = actual_ty (varType, pos)}
		| SOME(_)			=> (ErrorMsg.error pos ("Var with name "^(S.name s)^" is a function, not a simple variable!"); {exp = (), ty = T.ERROR})
		| NONE  		    => (ErrorMsg.error pos ("Undefined variable "^(S.name s)); {exp = (), ty = T.ERROR}))

| transVar(venv, tenv, A.FieldVar (var: A.var, s: A.symbol, pos: A.pos)) = 
	(let
		val {exp = _, ty = parentType} = transVar(venv, tenv, var)
		fun findMatchField(oneField::rest, symToLook, unique) =
			(case oneField of
				(name:A.symbol, t:T.ty) => (if name=symToLook then {exp = (), ty = t} else findMatchField(rest, symToLook, unique))
				| _	    => (ErrorMsg.error pos ("Error trying to access field "^(S.name s)^" of parent record!"); {exp = (), ty = T.ERROR}))
	in
		(case parentType of T.RECORD(fields, unique) => findMatchField(fields, s, unique)
							| _ => (ErrorMsg.error pos ("Trying to access field "^(S.name s)^" whose parent is not a record type!");
									{exp = (), ty = T.ERROR}))
	end)

| transVar(venv, tenv, A.SubscriptVar (var: A.var, exp: A.exp, pos: A.pos)) = 
	(let 
		val {exp = _, ty = varType} = transVar(venv, tenv, var)
		val {exp = _, ty = expType} = transExp(venv, tenv, exp) 
    in 
		case expType of T.INT => (
				case varType of T.ARRAY (eleType, unique) => {exp = (), ty = eleType}
		   	  		 | _ => (ErrorMsg.error pos "Var must be an array type to access an indexed element!";
		      			     {exp = (), ty = T.ERROR}))
		| _ => (ErrorMsg.error pos "Exp to access an element in an array should be an integer!";
				{exp = (), ty = T.ERROR})
    end)
end