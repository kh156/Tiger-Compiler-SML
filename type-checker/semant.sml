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

fun checkInt ({exp=exp,ty=ty},pos) = 
  (case ty of
	T.INT => ()
  | _ => error pos "integer required")

fun checkUnit ({exp=exp, ty=ty}, pos) =
  (case ty of
    T.UNIT => ()
  | _ => error pos "unit required")

fun checkString ({exp=exp, ty=ty}, pos) =
  (case ty of
    T.STRING => ()
  | _ => error pos "string required")

fun lookupType (tenv, typSymbol, pos) = 
  (case S.look (tenv, typSymbol) of
    SOME ty => ty
  | NONE => (error pos ("type is not defined: "^(S.name typSymbol)); T.UNIT))


(*fun isSameType(t1: T.ty, t2: T.ty) = *)


fun actual_ty(ty: T.ty, pos: A.pos) =
	(case ty of
		T.NAME(s, tref) => (case !tref of
						SOME(t) => actual_ty (t,pos)
					   | NONE => (ErrorMsg.error pos ("Undefined type with name: "^(S.name s)); T.ERROR))
		| _ => ty)

fun compareType (type1: T.ty, type2: T.ty, pos1: A.pos, pos2: A.pos) = (* Returns true if ty1 is a subtype of ty2 *)
	let
		val trueType1 = actual_ty(type1, pos1)
		val trueType2 = actual_ty(type2, pos2)
	in
		case trueType2 of
		  	T.UNIT => true (* All units are subtype of UNIT *)
			| T.RECORD(l,u) => 
				if trueType1 = T.NIL
					then true
					else trueType1 = trueType2
			| _ =>
				if trueType1 = T.NIL andalso trueType2 = T.NIL 
					then false 
					else trueType1 = trueType2
	end

fun changeRefToRealType(tenv, name: A.symbol, realTy: T.ty, pos: A.pos) =
	(case S.look(tenv, name) of 
		SOME(T.NAME(s, tref)) => (let val temp = (tref := SOME(realTy)) in tenv end)
		| _ => (ErrorMsg.error pos "Error processing mutually recursive types!"; tenv))

fun transExp (venv, tenv, A.NilExp) = {exp=(), ty=T.NIL}
	  | transExp (venv, tenv, A.IntExp i) = {exp=(), ty=T.INT}
	  | transExp (venv, tenv, A.VarExp v) = transVar(venv, tenv, v)
	  | transExp (venv, tenv, A.StringExp (s, pos)) = {exp=(), ty=T.STRING}
	  | transExp (venv, tenv, A.SeqExp exps) =
		let
			fun parseExps([]) = {exp = (), ty = T.UNIT}
			|	parseExps((e, p)::[]) = transExp(venv, tenv, e)
			|	parseExps((e, p)::l) = (
					transExp(venv, tenv, e);
					parseExps(l)
				)
		in
			parseExps(exps)
		end

	  | transExp (venv, tenv, A.OpExp{left=left,oper=oper,right=right,pos=pos}) =
  		if (oper=A.PlusOp orelse oper=A.MinusOp 
  		orelse oper=A.TimesOp orelse oper=A.DivideOp)
  	    then (checkInt(transExp(venv, tenv, left), pos);
		 	  checkInt(transExp(venv, tenv, right), pos);
		 	  {exp=(),ty=T.INT})
		else if (oper=A.EqOp orelse oper=A.NeqOp 
		orelse oper=A.LtOp orelse oper=A.LeOp
		orelse oper=A.GtOp orelse oper=A.GeOp)
		then
			let
				val {exp=exp, ty=leftType} = transExp(venv, tenv, left)
				val {exp=exp, ty=rightType} = transExp(venv, tenv, right)
			in
				if (compareType(leftType, rightType, pos, pos) orelse compareType(rightType, leftType, pos, pos))
			  	   	  then {exp=(), ty=T.INT}
			  		  else ((ErrorMsg.error pos "Logical comparison on two different types!");
			  		  		{exp=(),ty=T.ERROR})
			end
		else
			((error pos "Error identifying the operator used!");
			{exp=(),ty=T.ERROR})

	  | transExp (venv, tenv, A.RecordExp {fields=fields, typ=typ, pos=pos}) = 
	  	let
	  		val T.RECORD (symbolTypeList,unique) = (case S.look(tenv, typ) of 
	  							          SOME(v) => actual_ty (v,pos)
	  							          | NONE => (ErrorMsg.error pos "Expression with undefined record type!";
	  							           T.RECORD([], ref())))
	  		(*fields is a (symbol * exp * pos) list*)
	  		(*symbolTypeList is (Symbol.symbol * ty) list*)
	  		fun checkRecord((symbol, exp, pos)::otherFields, (tySymbol, ty)::otherTypes) =
	  			(case (ty)=(#ty (transExp(venv, tenv, exp))) of 
	  				true => (case (S.name symbol)=(S.name tySymbol) of 
	  						true => checkRecord(otherFields, otherTypes)
	  						| false => false)
	  				| false => false)
	  		| checkRecord([], []) = true
	  		| checkRecord(_, _) = false
	  	in
	  		if checkRecord(fields, symbolTypeList)
	  		then {exp=(), ty=T.RECORD (symbolTypeList, unique)}
	  		else {exp=(), ty=T.ERROR}
	  	end

	  | transExp (venv, tenv, A.AssignExp{var=var,exp=exp,pos=pos}) =
	  	if (#ty (transVar(venv, tenv, var))) = (#ty (transExp(venv, tenv, exp)))
	  	then {exp=(),ty=T.UNIT}
	  	else 
	  		(error pos "Types of variable and assigned expression do not match";
			{exp=(),ty=T.ERROR})

	  | transExp (venv, tenv, A.LetExp {decs=decs,body=body,pos=pos}) =
	  	let 
	  		fun transOneDec(oneDec, {venv=venv, tenv=tenv}) = 
	  			transDec(venv, tenv, oneDec)
	  		val {venv=venv',tenv=tenv'} = foldr transOneDec {venv=venv, tenv=tenv} decs
	  	 in transExp(venv',tenv', body)
	  	end

	  | transExp (venv, tenv, A.CallExp{func=func, args=args, pos=pos}) =
	   (case S.look(venv, func) of
	  		SOME (E.FunEntry {formals=formals, result=result}) =>
	  		  ( let 
	  		  		fun transExpHere(oneExp) = transExp(venv, tenv, oneExp)
	  				val argTypes = map transExpHere args
	  				fun compareTypes (ty1::lst1, ty2::lst2, pos) =
							if compareType(ty1,ty2, pos, pos) 
								then compareTypes(lst1,lst2, pos)
								else false
					|	compareTypes ([], [], pos) = true
	  				|	compareTypes (_, [], pos) = false
	  				|	compareTypes ([], _, pos) = false
	  			in
		  			if length(argTypes) <> length(formals) then
		  				(error pos ("Number of arguments incorrect: "^Int.toString(length(args))); {exp=(),ty=actual_ty(result, pos)})
	            	else (
	            		if compareTypes (formals, map (#ty) argTypes, pos) 
	            			then ()
	            			else (error pos ("Params do not match with function: "^S.name(func)));
			            {exp=(),ty=actual_ty(result, pos)}
			        )
				end
			  )
	  		| NONE => (error pos ("This function does not exist " ^ S.name(func)); {exp=(),ty=T.ERROR})
	  	)

	  | transExp (venv, tenv, A.IfExp {test=test, then'=thenExp, else'=elseExp, pos=pos}) =
	  	(case elseExp of
           NONE => (* if-then *)
         	(let 
         		val t = transExp(venv, tenv, test)
         	in
         		(checkInt(t, pos);
         		checkUnit(transExp(venv, tenv, thenExp), pos);
         		{ exp=(), ty=T.UNIT })
         	end)
         | SOME elseExp => (* if-then-else *)
         	(let 
         		val t = transExp(venv, tenv, test)
         		val thenType = transExp(venv, tenv, thenExp)
         		val elseType = transExp(venv, tenv, elseExp)
         	in
         		(checkInt(t, pos);
         		if (#ty thenType) = (#ty elseType) then
         			{ exp=(), ty= (#ty thenType) }
         		else
         			(error pos "Types of Then and Else statements do not match!";
         			 { exp=(), ty=T.ERROR})
         		)
         	end)
         )

	  | transExp (venv, tenv, A.ForExp {var=var, escape=escape, lo=lo, hi=hi, body=body, pos=pos}) =
	  	let 
	  		val venvNew = S.enter (venv, var, E.VarEntry {ty=Types.INT})
	  		val bodyType = transExp(venvNew, tenv, body)
	  	in
		  	(checkInt(transExp(venvNew, tenv, lo), pos);
		  	checkInt(transExp(venvNew, tenv, hi), pos);
		  	checkUnit(bodyType, pos);
		  	{ exp=(), ty=T.UNIT })
	    end

	  | transExp (venv, tenv, A.WhileExp {test=test, body=body, pos=pos}) =
	    let
	    	val _ = incNestDepth()
	  		val t = transExp(venv, tenv, test)
	  		val b = transExp(venv, tenv, body)
	  		val _ = decNestDepth()
	  	in
	  		(checkInt(t, pos);
	  		checkUnit(b, pos);
	  		{ exp=(), ty=T.UNIT })
	  	end

	  | transExp (venv, tenv, A.BreakExp pos) =
	  	if !nestDepth > 0 then { exp=(), ty=T.UNIT }
	  	else (error pos "Invalid nesting depth for a Break";
	  		  { exp=(), ty=T.ERROR })

	  | transExp (venv, tenv, A.ArrayExp {typ=typ, size=size, init=init, pos=pos}) = 
	  	(case (#ty (transExp(venv, tenv, size))) of 
	  		  T.INT => (case S.look(tenv, typ) of
	  		  	    	 SOME(t) => (case actual_ty (t,pos) of
	  		  	    	 	   			T.ARRAY(eleType, unique) => if (actual_ty (eleType,pos)) = (#ty (transExp(venv, tenv, init)))
	  		  	    	    							then {exp=(), ty=T.ARRAY(actual_ty (eleType,pos), ref())}
	  		  	    	    							else (ErrorMsg.error pos "Type mismatch during array creation!"; {exp=(), ty=T.ERROR})
	  		  	    	    			| _ => (ErrorMsg.error pos "Type ID used to create array is not an array type!"; {exp=(), ty=T.ERROR}))
	  		  	    	 | NONE => (ErrorMsg.error pos "Undefined type during array creation!"; {exp=(), ty=T.ERROR}))
	  		  | _ => (ErrorMsg.error pos "Array size must be an integer!"; {exp = (), ty = T.ERROR}))


and transTy(tenv, A.NameTy(s:A.symbol, pos:A.pos)) =
	(case S.look(tenv, s) of NONE => (ErrorMsg.error pos ("Undefined type with name "^(S.name s)); T.ERROR)
							| SOME(t) => t)

| transTy(tenv, A.RecordTy fieldList) =
	let 
		val fields=
	    let
	    	fun checkSingleField ({name: A.symbol, escape: bool ref, typ: A.symbol, pos: A.pos}) =
		     	(case S.look(tenv, typ) of NONE => (ErrorMsg.error pos ("Undefined type: "^(S.name typ)^" when declaring an record type!");
		     										(name, T.ERROR))
					   					| SOME(t) => (name, t))
	     in
		 	map checkSingleField fieldList
	     end
    in
		T.RECORD(fields, ref ())
    end
	

| transTy(tenv, A.ArrayTy (s:A.symbol, pos:A.pos)) =
	(case S.look(tenv, s) of NONE => (ErrorMsg.error pos ("Undefined type "^(S.name s)^" when declaring an array type!");
										T.ARRAY(T.ERROR, ref())) (*how to use ref()?*)
							| SOME(t) => T.ARRAY(t, ref()))


(*book says type NIL must be constrained by a RECORD type???*)
and transDec(venv, tenv, A.VarDec{name: A.symbol,
                                  escape: bool ref,
                                  typ: (A.symbol * A.pos) option,
                                  init: A.exp,
                                  pos: A.pos}) = 
	let
		val {exp=exp, ty=tyinit} = transExp(venv, tenv, init)
	in
		case typ of
			NONE => (case tyinit=T.NIL of 
				false => {tenv = tenv, venv = S.enter(venv, name, E.VarEntry {ty = tyinit})}
				| true => (ErrorMsg.error pos "variable is initialized to NIL type!";
				    	{venv = S.enter(venv, name, E.VarEntry {ty = tyinit}), tenv = tenv}))
			| SOME(s, p) => (
				case S.look(tenv, s) of
					NONE => (ErrorMsg.error pos "declared type for variable does not exist!";
							{venv = S.enter(venv, name, E.VarEntry {ty = tyinit}), tenv = tenv})
					| SOME(t) => (case compareType(tyinit, t, pos, p) of
							false => (ErrorMsg.error pos "declared type for variable doesn't match the type of initial expression!";
							    	{venv = S.enter(venv, name, E.VarEntry {ty = tyinit}), tenv = tenv})
							| true  => {venv = S.enter(venv, name, E.VarEntry {ty = tyinit}), tenv = tenv}))
	end

| transDec(venv, tenv, A.TypeDec typeDecList) = 
	let
		fun addNameType({name: A.symbol, ty: A.ty, pos: A.pos}, tenvCurr) =
			S.enter(tenvCurr, name, T.NAME(name, ref NONE))

		val newtenv = foldr addNameType tenv typeDecList

		fun figureOutRealType({name: A.symbol, ty: A.ty, pos: A.pos}, tenvCurr)=
			let
				val realType = transTy(tenvCurr, ty)
			in
				changeRefToRealType(tenvCurr, name, realType, pos)
			end
	in
		{venv = venv, tenv = foldr figureOutRealType newtenv typeDecList}
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
						val ty = case S.look(tenv, typ) of
					    	SOME t => t
					    | 	NONE => T.ERROR
					in
						S.enter (venvCurr, name, E.VarEntry {ty = ty})
					end
				val venv' = foldr enterparam venv params
				val {exp = _, ty = tybody} = transExp (venv', tenv, body)
			in (
				case compareType(tybody, tyret, pos, pos) of
					true => ()
			    | 	false => ErrorMsg.error pos "Function body type and return type do not mactch!";
				secondPass (venv, func)
			)
			end

		fun firstPass ({name=name, params=params, body=body, pos=pos, result=result}, venvCurr) =
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

		val venv' = foldr firstPass venv funcs;
	in (
		secondPass(venv', funcs);
		{venv = venv', tenv = tenv}
	)
	end

| transDec(venv, tenv, A.StartOfDecList ()) = {venv=venv, tenv=tenv}
	

and transVar(venv, tenv, A.SimpleVar (s: A.symbol, pos: A.pos)) = 
	(case S.look(venv, s) of
		SOME(E.VarEntry {ty=varType}) => {exp = (), ty = actual_ty (varType, pos)}
		| SOME(_)			=> (ErrorMsg.error pos ("Var with name "^(S.name s)^" is a function, not a simple variable!"); {exp = (), ty = T.ERROR})
		| NONE  		    => (ErrorMsg.error pos ("Undefined variable "^(S.name s)); {exp = (), ty = T.ERROR}))

| transVar(venv, tenv, A.FieldVar (var: A.var, s: A.symbol, pos: A.pos)) = 
	(let
		val {exp = _, ty = parentType} = transVar(venv, tenv, var)
		fun findMatchField((sym, t)::rest, symToLook, unique, pos) =
			    (if (S.name sym)=(S.name symToLook)
			    	then {exp = (), ty = actual_ty (t,pos)}
			    	else findMatchField(rest, symToLook, unique, pos))
		| findMatchField([], symToLook, unique, pos) = (ErrorMsg.error pos ("Did not find matched field "^(S.name symToLook)^" in record type!");{exp = (), ty = T.ERROR})
	in
		(case parentType of T.RECORD(fields, unique) => findMatchField(fields, s, unique, pos)
							| _ => (ErrorMsg.error pos ("Trying to access field "^(S.name s)^" whose parent is not a record type!");
									{exp = (), ty = T.ERROR}))
	end)

| transVar(venv, tenv, A.SubscriptVar (var: A.var, exp: A.exp, pos: A.pos)) = 
	(let 
		val {exp = _, ty = varType} = transVar(venv, tenv, var)
		val {exp = _, ty = expType} = transExp(venv, tenv, exp)
    in 
		case expType of T.INT => (
				case varType of T.ARRAY (eleType, unique) => {exp = (), ty = actual_ty (eleType,pos)}
		   	  		 | _ => (ErrorMsg.error pos "Var must be an array type to access an indexed element!";
		      			     {exp = (), ty = T.ERROR}))
		| _ => (ErrorMsg.error pos "Exp to access an element in an array should be an integer!";
				{exp = (), ty = T.ERROR})
    end)

(*main entry point for type-checking a program*)
fun transProg(programCode : A.exp) = 
    let 
    	val venv = E.base_venv
    	val tenv = E.base_tenv
    	val temp = transExp(venv, tenv, programCode)
    in 
    	()
    end

end
