structure Semant = 
struct

	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table

	structure Ty = Types
	structure A = Absyn
	structure Translate = struct type exp = unit end
	structure Tr = Translate
	val error = ErrorMsg.error

	type expty = {exp: Tr.exp, ty: Ty.ty}

	transVar: venv * tenv * A.var -> expty
	transExp: venv * tenv * A.exp -> expty
	transDec: venv * tenv * A.dec -> {venv: venv, tenv: tenv}
	transTy:  		 tenv * A.ty -> Types.ty

	fun checkInt ({exp,ty},pos) = 
	  case ty of
		Ty.Int => ()
	  | _ => error pos "integer required"

	fun checkUnit ({exp, ty}, pos) =
      case ty of
        Ty.UNIT => ()
      | _ => error pos "unit required"

    fun checkString ({exp, ty}, pos) =
      case ty of
        Ty.STRING => ()
      | _ => error pos "string required"

	fun transExp(venv, tenv) =
		    trexp (A.NilExp) = {exp=(), Ty.NIL}
		  | trexp (A.IntExp i) = {exp=(), Ty.INT}
		  | trexp (A.VarExp v) = trvar v
		  | trexp (A.StringExp (s, pos)) = (exp=(), Ty.STRING)
		  | trexp (A.SeqExp exps) = {exp=(), ty=Ty.UNIT}
		  | trexp (A.OpExp{left,oper,right,pos}) =
	  		if (oper=A.PlusOp orelse oper=A.MinusOp 
	  		orelse oper=A.TimesOp orelse oper=A.DivideOp)
	  	    then (checkInt(trexp left, pos);
			 	  checkInt(trexp right, pos);
			 	  {exp=(),ty=Ty.INT})
			else if (oper=A.EqOp orelse oper=A.NeqOp 
			orelse oper=A.LtOp orelse oper=A.LeOp
			orelse oper=A.GtOp orelse oper=A.LtOp)
			then
				case #ty trexp(left) of
					Ty.INT => (checkInt(trexp right, pos);
		 	  				   {exp=(),ty=Ty.INT})
				  | Ty.STRING => (checkString(trexp right, pos);
		 	  				      {exp=(),ty=Ty.STRING})
				  | _ => (error pos "Can't perform an operation on this type");
				  		  {exp=(),ty=Ty.INT}
			else
				(error pos "Error");
				{exp=(),ty=Ty.INT}

		  | trexp (A.RecordExp{fields,typ,exp}) =

		  | trexp (A.AssignExp{var,exp,pos}) =
		  	if #ty trvar(var) = #ty trexp(exp)
		  	then {exp=(),ty=Ty.UNIT}
		  	else 
		  		(error pos "Types of variable and expression do not match");
				{exp=(),ty=Ty.UNIT}

		  | trexp (A.LetExp{decs,body,pos}) =
		  	let val {venv=venv',tenv=tenv'} =
		  			   transDecs(venv,tenv,decs)
		  	 in transExp(venv',tenv') body
		  	end
		  | trexp (A.CallExp{func, args, pos}) =
		  | trexp (A.IfExp {test, then', else', pos}) =
		  | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
		  | trexp (A.WhileExp {test, body, pos}) =
		  | trexp (A.BreakExp pos) =
		  | trexp (A.LetExp {decs, body, pos}) =
		  | trexp (A.ArrayExp {typ, size, init, pos}) =

end