(*make code easier to read*)
structure A = Absyn
structure T = Types
structure E = Envir
structure S = Symbol
structure Trans = Translate

signature SEM = 
sig
	(*more meaningful/easier to read types*)
	type venvTable
	type tenvTable
	type expty
	type ty

	val transVar: venvTable * tenvTable * Absyn.var * Temp.label * Trans.level -> expty
	val transExp: venvTable * tenvTable * Absyn.exp * Temp.label * Trans.level -> expty
	val transDec: venvTable * tenvTable * Absyn.dec * Temp.label * Trans.level * Trans.exp list-> {venv: venvTable, tenv: tenvTable, trExpList: Trans.exp list}
	val transTy:   	     	  tenvTable * Absyn.ty -> ty
	val transProg:        			      Absyn.exp -> Trans.frag list
end

(*Notes on IR translation: semant.sml should never contain direct reference to Tree or Frame module!!!*)
structure Semant :> SEM = 
struct

(*more meaningful/easier to read types*)
type venvTable = E.enventry S.table
type tenvTable = T.ty S.table
type expty = {exp: Trans.exp, ty: T.ty}
type ty = T.ty
val error = ErrorMsg.error
val nestDepth = ref 0
val numBreaks = ref 0

val refCount = ref 0

fun incNumBreaks () = numBreaks := !numBreaks + 1
fun decNumBreaks () = numBreaks := !numBreaks - 1
fun incNestDepth () = nestDepth := !nestDepth + 1
fun decNestDepth () = nestDepth := !nestDepth - 1

fun lookupType (tenv, typSymbol, pos) = 
  (case S.look (tenv, typSymbol) of
    SOME ty => ty
  | NONE => (error pos ("type is not defined: "^(S.name typSymbol)); T.UNIT))


structure MySet = ListSetFn (struct
  type ord_key = A.symbol
  val compare = (fn (_,_) => General.LESS)
end)

fun noRepeatName(decList) = 
	let
		fun enterOneDec({name=name, ty=typ, pos=pos}, setSoFar)= MySet.add(setSoFar, name)
	in
		if MySet.numItems(foldr enterOneDec MySet.empty decList) = List.length(decList)
		then true
		else (ErrorMsg.error 0 "Mutually recursive type declarations have repeated names!";false)
	end

fun noRepeatNameFunction(decList) = 
	let
		fun enterOneDec({name=name, params=params, result=result, body=body, pos=pos}, setSoFar) = MySet.add(setSoFar, name)
	in
		if MySet.numItems(foldr enterOneDec MySet.empty decList) = List.length(decList)
		then true
		else (ErrorMsg.error 0 "Mutually recursive function declarations have repeated names!";false)
	end

fun actual_ty(ty: T.ty, pos: A.pos) =
	(case ty of
		T.NAME(s, tref) => (case !tref of
						SOME(t) => actual_ty (t,pos)
					   | NONE => (ErrorMsg.error pos ("Undefined type with name: "^(S.name s)); T.ERROR))
		| _ => ty)

fun checkHasConcreteType(originalName: A.symbol, ty: T.ty, pos: A.pos, firstTime: int) = 
		(case ty of
		T.NAME(s, tref) => if (originalName=s andalso firstTime=0)
						then (ErrorMsg.error pos ("Cyclic type declaration detected: "^(S.name s)); false)
						else (case !tref of
						SOME(t) => (checkHasConcreteType (originalName,t,pos,0))
					   | NONE => (ErrorMsg.error pos ("Undefined type with name: "^(S.name s)); false))
		| _ => true)

fun compareType (type1: T.ty, type2: T.ty, pos1: A.pos, pos2: A.pos) = (* Returns true if ty1 is a subtype of ty2 *)
	let
		val trueType1 = actual_ty(type1, pos1)
		val trueType2 = actual_ty(type2, pos2)
	in 
		if trueType1=T.ERROR orelse trueType2=T.ERROR
			then true 
			else if trueType2 = T.UNIT 
				then true
				else if trueType1 = T.NIL
					then (case trueType2 of
						T.NIL => (error pos1 "Error compairing two nils!"; false)
						| T.RECORD(l,u) => true
						| _ => trueType1=trueType2)
					else trueType1=trueType2
	end

fun checkInt ({exp=exp,ty=ty},pos) = 
	if compareType(ty, T.INT, pos, pos)
		then ()
  		else error pos "integer required"

fun checkUnit ({exp=exp, ty=ty}, pos) =
	if compareType(ty, T.UNIT, pos, pos)
		then ()
  		else error pos "unit required"

fun checkString ({exp=exp, ty=ty}, pos) =
	if compareType(ty, T.STRING, pos, pos)
		then ()
  		else error pos "string required"

fun changeRefToRealType(tenv, name: A.symbol, realTy: T.ty, pos: A.pos) =
	(case S.look(tenv, name) of 
		SOME(T.NAME(s, tref)) => (let val temp = (tref := SOME(realTy)) in tenv end)
		| _ => (ErrorMsg.error pos "Error processing mutually recursive types!"; tenv))

fun transExp (venv, tenv, A.NilExp, doneLabel, level) = {exp=Trans.nilExp(), ty=T.NIL}
	  | transExp (venv, tenv, A.IntExp i, doneLabel, level) = {exp=Trans.intExp(i), ty=T.INT}
	  | transExp (venv, tenv, A.VarExp v, doneLabel, level) = transVar(venv, tenv, v, doneLabel, level)
	  | transExp (venv, tenv, A.StringExp (s, pos), doneLabel, level) = {exp=Trans.strExp(s), ty=T.STRING}
	  | transExp (venv, tenv, A.SeqExp exps, doneLabel, level) =
		let
			fun parseExps([], translatedExpList) = {exp = Trans.seqExp(translatedExpList), ty = T.UNIT}
			|	parseExps((e, p)::l, translatedExpList) =
				let
					val {exp = translatedExp, ty = ty} = transExp(venv, tenv, e, doneLabel, level);
				in
					parseExps(l, translatedExp::translatedExpList)
				end
		in
			parseExps(exps, [])
		end

	  | transExp (venv, tenv, A.OpExp{left=left,oper=oper,right=right,pos=pos}, doneLabel, level) =
  		if (oper=A.PlusOp orelse oper=A.MinusOp orelse
  			oper=A.TimesOp orelse oper=A.DivideOp orelse
			oper=A.LtOp orelse oper=A.LeOp orelse
  			oper=A.GtOp orelse oper=A.GeOp)
  	    then 
  	    	let
  	    		val leftResult = transExp(venv, tenv, left, doneLabel, level)
  	    		val rightResult = transExp(venv, tenv, right, doneLabel, level)
  	    	in
  	    	  (checkInt(leftResult, pos);
		 	  checkInt(rightResult, pos);
		 	  {exp=Trans.intOpExp(oper, (#exp leftResult), (#exp rightResult)), ty=T.INT})
		 	end

		else if (oper=A.EqOp orelse oper=A.NeqOp)
		then
			let
				val {exp=expLeft, ty=leftType} = transExp(venv, tenv, left, doneLabel, level)
				val {exp=expRight, ty=rightType} = transExp(venv, tenv, right, doneLabel, level)
			in
				case leftType of
					T.STRING =>
						if (compareType(leftType, rightType, pos, pos) orelse compareType(rightType, leftType, pos, pos))
			  			then {exp=Trans.stringOpExp(oper, expLeft, expRight), ty=T.STRING}
			  			else ((ErrorMsg.error pos "Logical comparison on two different types!");
			  		  	{exp=Trans.stringOpExp(oper, expLeft, expRight), ty=T.ERROR})
					| _ => 
						if (compareType(leftType, rightType, pos, pos) orelse compareType(rightType, leftType, pos, pos))
			  			then {exp=Trans.intOpExp(oper, expLeft, expRight), ty=T.INT}
			  			else ((ErrorMsg.error pos "Logical comparison on two different types!");
			  		  	{exp=Trans.intOpExp(oper, expLeft, expRight), ty=T.ERROR})
			end
		else
			((error pos "Error identifying the operator used!");
			{exp=Trans.intExp(0), ty=T.ERROR})

	  | transExp (venv, tenv, A.RecordExp {fields=fields, typ=typ, pos=pos}, doneLabel, level) = 
	  	let
	  		val T.RECORD (symbolTypeList,unique) = case S.look(tenv, typ) of 
	  							          SOME(v) => actual_ty (v,pos)
	  							          | NONE => (ErrorMsg.error pos "Expression with undefined record type!";
	  							           T.RECORD([], ref 0))
	  		(*fields is a (symbol * exp * pos) list*)
	  		(*symbolTypeList is (Symbol.symbol * ty) list*)
	  		fun checkRecord((symbol, exp, pos)::otherFields, (tySymbol, ty)::otherTypes) =
	  			(case (S.name symbol)=(S.name tySymbol) of 
	  				true => (case (actual_ty (ty,pos))=(#ty (transExp(venv, tenv, exp, doneLabel, level)))	 of 
	  						true => checkRecord(otherFields, otherTypes)
	  						| false => (ErrorMsg.error pos "Field type does not match record type during record creation!";
	  									false))
	  				| false => (ErrorMsg.error pos "Field name does not match record type during record creation!";
	  									false))
	  			| checkRecord([], []) = true
	  			| checkRecord(_, _) = false

	  		fun translateEachExp((symbol, exp, pos)::otherFields, translated, size) = 
	  			let
	  				val fieldResult = transExp(venv, tenv, exp, doneLabel, level)
	  			in
	  				translateEachExp(otherFields, translated @ [(#exp fieldResult)], size+1)
	  			end
	  			| translateEachExp([], translated, size) = {translated=translated, size=size}
	  	in
	  		if checkRecord(fields, symbolTypeList)
	  		then {exp=Trans.recordExp(translateEachExp(fields, [], 0)), ty=T.RECORD (symbolTypeList, unique)}
	  		else {exp=Trans.recordExp(translateEachExp(fields, [], 0)), ty=T.ERROR}
	  	end

	  | transExp (venv, tenv, A.AssignExp{var=var,exp=exp,pos=pos}, doneLabel, level) =
	  	let
	  		val {exp=leftExp, ty=leftTy} = transVar(venv, tenv, var, doneLabel, level)
	  		val {exp=rightExp, ty=rightTy} = transExp(venv, tenv, exp, doneLabel, level)
	  	in
		  	if (leftTy = rightTy)
		  	then {exp=Trans.assignExp(leftExp, rightExp), ty=T.UNIT}
		  	else 
		  		(error pos "Types of variable and assigned expression do not match";
				{exp=Trans.assignExp(leftExp, rightExp), ty=T.ERROR})
		end

	  | transExp (venv, tenv, A.LetExp {decs=decs,body=body,pos=pos}, doneLabel, level) =
	  	let 
	  		fun transOneDec(oneDec, {venv=venv, tenv=tenv, trExpList=initExpList}) = 
	  			transDec(venv, tenv, oneDec, doneLabel, level, initExpList)
	  		val {venv=venv',tenv=tenv', trExpList=trExpList'} = foldr transOneDec {venv=venv, tenv=tenv, trExpList=[]} decs
	  		val {exp=expBody, ty=tyBody} = transExp(venv',tenv', body, doneLabel, level)
	  	 in
	  	 	{exp=Trans.addExpListBefore(trExpList', expBody), ty=tyBody}
	  	end

	  | transExp (venv, tenv, A.CallExp{func=func, args=args, pos=pos}, doneLabel, level) =
	   (case S.look(venv, func) of
	  		SOME (E.FunEntry {level=funLevel, label=label, formals=formals, result=result}) =>
	  		  ( let 
	  		  		fun transExpHere(oneExp) = transExp(venv, tenv, oneExp, doneLabel, level)
	  				val argResults = map transExpHere args
	  				fun compareTypes (ty1::lst1, ty2::lst2, pos) =
							if compareType(ty1,ty2, pos, pos) 
								then compareTypes(lst1,lst2, pos)
								else false
					|	compareTypes ([], [], pos) = true
	  				|	compareTypes (_, [], pos) = false
	  				|	compareTypes ([], _, pos) = false
	  			in
		  			if length(argResults) <> length(formals) then
		  				(error pos ("Number of arguments incorrect: "^Int.toString(length(args)));
		  					{exp=Trans.callExp(level, label, map (#exp) argResults), ty=actual_ty(result, pos)})
	            	else (
	            		if compareTypes (formals, map (#ty) argResults, pos) 
	            			then ()
	            			else (error pos ("Params do not match with function: "^S.name(func)));
			            {exp=Trans.callExp(level, label, map (#exp) argResults), ty=actual_ty(result, pos)}
			        )
				end
			  )
	  		| _ => (error pos ("This function does not exist: " ^ S.name(func)); {exp=Trans.intExp(0), ty=T.ERROR}))

	  | transExp (venv, tenv, A.IfExp {test=test, then'=thenExp, else'=elseExp, pos=pos}, doneLabel, level) =
	  	(case elseExp of
           NONE => (* if-then *)
         	(let 
         		val t = transExp(venv, tenv, test, doneLabel, level)
         		val thenResult = transExp(venv, tenv, test, doneLabel, level)
         	in
         		(checkInt(t, pos);
         		checkUnit(transExp(venv, tenv, thenExp, doneLabel, level), pos);
         		{ exp=Trans.ifThenExp(#exp t, #exp thenResult), ty=T.UNIT })
         	end)
         | SOME elseExp => (* if-then-else *)
         	(let 
         		val t = transExp(venv, tenv, test, doneLabel, level)
         		val thenResult = transExp(venv, tenv, thenExp, doneLabel, level)
         		val elseResult = transExp(venv, tenv, elseExp, doneLabel, level)
         	in
         		(checkInt(t, pos);
         		if (#ty thenResult) = (#ty elseResult) then
         			{ exp=Trans.ifThenElseExp(#exp t, #exp thenResult, #exp elseResult), ty= (#ty thenResult) }
         		else
         			(error pos "Types of Then and Else statements do not match!";
         			 { exp=Trans.ifThenElseExp(#exp t, #exp thenResult, #exp elseResult), ty=T.ERROR})
         		)
         	end)
         )

	  | transExp (venv, tenv, A.ForExp {var=var, escape=escape, lo=lo, hi=hi, body=body, pos=pos}, doneLabel, level) =
	  	let 
	  		val done = Temp.newlabel()

	  		val _ = incNestDepth()
	  		val iAccess =  Trans.allocLocal level (!escape)
	  		val venvNew = S.enter (venv, var, E.VarEntry {access=iAccess, ty=Types.INT})
	  		val bodyResult = transExp(venvNew, tenv, body, done, level)
	  		val _ = decNestDepth()
	  		val _ = numBreaks := 0
	  		val loResult = transExp(venvNew, tenv, lo, done, level)
	  		val hiResult = transExp(venvNew, tenv, hi, done, level)
	  	in
		  	(checkInt(loResult, pos);
		  	checkInt(hiResult, pos);
		  	checkUnit(bodyResult, pos);
		  	{ exp=Trans.forExp(iAccess, (#exp loResult), (#exp hiResult), (#exp bodyResult), done, level), ty=T.UNIT })
	    end

	  | transExp (venv, tenv, A.WhileExp {test=test, body=body, pos=pos}, doneLabel, level) =
	    let
	    	val done = Temp.newlabel()

	    	val _ = incNestDepth()
	  		val t = transExp(venv, tenv, test, done, level)
	  		val b = transExp(venv, tenv, body, done, level)
	  		val _ = decNestDepth()
	  		val _ = numBreaks := 0
	  	in
	  		(checkInt(t, pos);
	  		checkUnit(b, pos);
	  		{ exp=Trans.whileExp(#exp t, #exp b, done), ty=T.UNIT })
	  	end

	  | transExp (venv, tenv, A.BreakExp pos, doneLabel, level) =
	 	let
	 		val _ = incNumBreaks()
	 	in
		  	if !nestDepth = 0 then 
		  		(error pos "Invalid nesting depth for a Break";
		  		{ exp=Trans.breakExp(doneLabel), ty=T.ERROR })
		    else if !numBreaks > 1 then 
		    	(error pos "Excessive breaks!";
		    	{ exp=Trans.breakExp(doneLabel), ty=T.ERROR })
		  	else
		  		{ exp=Trans.breakExp(doneLabel), ty=T.UNIT }
		end

	  | transExp (venv, tenv, A.ArrayExp {typ=typ, size=size, init=init, pos=pos}, doneLabel, level) = 
	  	let
	  		val initResult = transExp(venv, tenv, init, doneLabel, level)
	  		val sizeResult = transExp(venv, tenv, size, doneLabel, level)
	  	in
	  	(case (#ty sizeResult) of 
	  		  T.INT => (case S.look(tenv, typ) of
	  		  	    	 SOME(t) => (case actual_ty (t,pos) of
	  		  	    	 	   		T.ARRAY(eleType, unique) => if (actual_ty (eleType,pos)) = (#ty initResult)
	  		  	    	    							then {exp=Trans.arrayExp(#exp initResult, #exp sizeResult), ty=actual_ty(t,pos)}
	  		  	    	    							else (ErrorMsg.error pos "Type mismatch during array creation!"; {exp=Trans.arrayExp(#exp initResult, #exp sizeResult), ty=T.ERROR})
	  		  	    	    		| _ => (ErrorMsg.error pos "Type ID used to create array is not an array type!"; {exp=Trans.arrayExp(#exp initResult, #exp sizeResult), ty=T.ERROR}))
	  		  	    	 | NONE => (ErrorMsg.error pos "Undefined type during array creation!"; {exp=Trans.arrayExp(#exp initResult, #exp sizeResult), ty=T.ERROR}))
	  		  | _ => (ErrorMsg.error pos "Array size must be an integer!"; {exp = Trans.arrayExp(#exp initResult, #exp sizeResult), ty = T.ERROR}))
		end

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
    in (
		refCount := !refCount + 1;
		T.RECORD(fields, ref (!refCount+1))
	)
    end
	

| transTy(tenv, A.ArrayTy (s:A.symbol, pos:A.pos)) =
	case S.look(tenv, s) of NONE => (ErrorMsg.error pos ("Undefined type "^(S.name s)^" when declaring an array type!");
										T.ERROR)
							| SOME(t) => (
								refCount := !refCount + 1;
								T.ARRAY(t, ref (!refCount + 1))
							)


(*book says type NIL must be constrained by a RECORD type???*)
and transDec(venv, tenv, A.VarDec{name: A.symbol,
                                  escape: bool ref,
                                  typ: (A.symbol * A.pos) option,
                                  init: A.exp,
                                  pos: A.pos}, doneLabel, level, initExpList) = 
	let
		val {exp=exp, ty=tyinit} = transExp(venv, tenv, init, doneLabel, level)
		val access = Trans.allocLocal level (!escape)
		val updatedExpList = Trans.assignExp(Trans.simpleVar(access, level), exp)::initExpList
	in
		case typ of
			NONE => (case tyinit=T.NIL of 
				false => {tenv = tenv, venv = S.enter(venv, name, E.VarEntry {access = access, ty = tyinit}), trExpList = updatedExpList}
				| true => (ErrorMsg.error pos "variable is initialized to NIL type!";
				    	{venv = S.enter(venv, name, E.VarEntry {access = access, ty = tyinit}), tenv = tenv, trExpList = updatedExpList}))
			| SOME(s, p) => (
				case S.look(tenv, s) of
					NONE => (ErrorMsg.error pos "declared type for variable does not exist!";
							{venv = S.enter(venv, name, E.VarEntry {access = access, ty = tyinit}), tenv = tenv, trExpList = updatedExpList})
					| SOME(t) => (case compareType(tyinit, t, pos, p) of
							false => (ErrorMsg.error pos "declared type for variable doesn't match the type of initial expression!";
							    	{venv = S.enter(venv, name, E.VarEntry {access = access, ty = tyinit}), tenv = tenv, trExpList = updatedExpList})
							| true  => {venv = S.enter(venv, name, E.VarEntry {access = access, ty = tyinit}), tenv = tenv, trExpList = updatedExpList}))
	end

| transDec(venv, tenv, A.TypeDec typeDecList, doneLabel, level, initExpList) = 
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

		val settledtenv = foldr figureOutRealType newtenv typeDecList

		fun checkTypeCycle({name: A.symbol, ty: A.ty, pos: A.pos}::otherDecs) = 
			(case S.look(settledtenv, name) of 
				SOME(t) => if checkHasConcreteType(name, t, pos, 1) then checkTypeCycle(otherDecs) else false
				| NONE => (ErrorMsg.error pos "Could not find the type that was just defined!"; false))
		| checkTypeCycle ([]) = true
	in
		if checkTypeCycle(typeDecList)
		then (if noRepeatName(typeDecList) then {venv = venv, tenv = settledtenv, trExpList = initExpList} else {venv= venv, tenv = tenv, trExpList = initExpList})
		else {venv= venv, tenv = tenv, trExpList = initExpList}
	end

| transDec(venv, tenv, A.FunctionDec funcs, doneLabel, level, initExpList) =
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

				val SOME(E.FunEntry entryRecord) = S.look(venv, name)
				val formalAccesses = (case Trans.formals (#level entryRecord) of
								 sl::rest => rest
								| _ => ErrorMsg.impossible "Formals of a function is an empty list!")

				fun enterparam ({name=name, escape=escape, typ=typ, pos=pos}, (venvCurr, access::rest)) = 
					let 
						val ty = case S.look(tenv, typ) of
					    	SOME t => t
					    | 	NONE => T.ERROR
					in
						(S.enter (venvCurr, name, E.VarEntry {access = access, ty = ty}), rest)
					end
					| enterparam ({name=name, escape=escape, typ=typ, pos=pos}, (venvCurr, [])) = 
					let 
						val ty = case S.look(tenv, typ) of
					    	SOME t => t
					    | 	NONE => T.ERROR
					in
						(ErrorMsg.impossible "No spaces on the stack allocated for the formals of a function...";
						(S.enter (venvCurr, name, E.VarEntry {access = Trans.allocLocal (#level entryRecord) (!escape), ty = ty}), []))
					end

				val (venv', _) = foldr enterparam (venv, formalAccesses) params
				val {exp = bodyExp, ty = tybody} = transExp (venv', tenv, body, doneLabel, (#level entryRecord))

				val entryRecord = case S.look(venv, name) of 
									SOME(E.FunEntry entry) => entry
									| _ => ErrorMsg.impossible "Function processing errors...not found..."
				val unitResult = Trans.procEntryExit({level = (#level entryRecord), body = bodyExp})

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

				(*ir translation stuff*)
				fun extractEscape({name=name, escape=escape, typ=typ, pos=pos}) = !escape
				val newLabel = Temp.newlabel()
				val newLevel = Trans.newLevel {parent = level, name = newLabel, formals = (map extractEscape params)}
			in 
				S.enter(venvCurr, name, E.FunEntry {level = newLevel, label = newLabel, formals = params', result = tyret})
			end

		val venv' = foldr firstPass venv funcs;

	in (
		secondPass(venv', funcs);
		noRepeatNameFunction(funcs);
		{venv = venv', tenv = tenv, trExpList = initExpList}
	)
	end

| transDec(venv, tenv, A.StartOfDecList (), doneLabel, level, initExpList) = {venv=venv, tenv=tenv, trExpList = initExpList}
	
(*val level in transVar is the level of the function in which variables are used!!*)
and transVar(venv, tenv, A.SimpleVar (s: A.symbol, pos: A.pos), doneLabel, level) = 
	(case S.look(venv, s) of
		SOME(E.VarEntry {access=access, ty=varType}) => {exp = Trans.simpleVar(access, level), ty = actual_ty (varType, pos)}
		| SOME(_)			=> (ErrorMsg.error pos ("Var with name "^(S.name s)^" is a function, not a simple variable!");
								{exp = Trans.intExp(0), ty = T.ERROR})
		| NONE  		    => (ErrorMsg.error pos ("Undefined variable "^(S.name s));
								{exp = Trans.intExp(0), ty = T.ERROR}))

| transVar(venv, tenv, A.FieldVar (var: A.var, s: A.symbol, pos: A.pos), doneLabel, level) = 
	(let
		val {exp = varExp, ty = parentType} = transVar(venv, tenv, var, doneLabel, level)
		fun findMatchField((sym, t)::rest, symToLook, unique, pos, index) =
			    	(if (S.name sym)=(S.name symToLook)
			    	then {exp = Trans.fieldVar(varExp, index), ty = actual_ty (t,pos)}
			    	else findMatchField(rest, symToLook, unique, pos, index+1))
		| findMatchField([], symToLook, unique, pos, index) = (ErrorMsg.error pos ("Did not find matched field "^(S.name symToLook)^" in record type!");{exp = Trans.fieldVar(varExp, index), ty = T.ERROR})
	in
		(case parentType of T.RECORD(fields, unique) => findMatchField(fields, s, unique, pos, 0)
							| _ => (ErrorMsg.error pos ("Trying to access field "^(S.name s)^" whose parent is not a record type!");
									{exp = Trans.intExp(0), ty = T.ERROR}))
	end)

| transVar(venv, tenv, A.SubscriptVar (var: A.var, exp: A.exp, pos: A.pos), doneLabel, level) = 
	(let 
		val {exp = varExp, ty = varType} = transVar(venv, tenv, var, doneLabel, level)
		val {exp = indexExp, ty = expType} = transExp(venv, tenv, exp, doneLabel, level)
    in 
		case expType of T.INT => (
				case varType of T.ARRAY (eleType, unique) => {exp = Trans.subscriptVar(varExp, indexExp), ty = actual_ty (eleType,pos)}
		   	  		 | _ => (ErrorMsg.error pos "Var must be an array type to access an indexed element!";
		      			     {exp = Trans.subscriptVar(varExp, indexExp), ty = T.ERROR}))
		| _ => (ErrorMsg.error pos "Exp to access an element in an array should be an integer!";
				{exp = Trans.subscriptVar(varExp, indexExp), ty = T.ERROR})
    end)

(*main entry point for type-checking a program*)
fun transProg(programCode : A.exp) = 
    let 
    	val unitResult = Trans.resetFragList()
    	val venv = E.base_venv
    	val tenv = E.base_tenv
		val startOfProgLabel = Temp.newlabel()
		val firstLevel = Trans.newLevel {parent = E.stdlibLevel, name = startOfProgLabel, formals = []}
    	val endOfProgLabel = Temp.newlabel()
    	val progResult = transExp(venv, tenv, programCode, endOfProgLabel, firstLevel)
    in 
    	(Trans.procEntryExit({level = firstLevel, body = #exp progResult}); 
    	 Temp.resetTempCount(); Trans.getResult())
    end

end
