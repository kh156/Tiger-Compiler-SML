signature CODEGEN = 
sig
	structure F : FRAME
	val codegen : F.frame -> Tree.stm -> Assem.instr list
end


structure Mips:CODEGEN = 
struct
	structure F = MipsFrame
	structure T = Tree
	structure A = Assem
	structure S = Symbol

    fun getBinopString T.PLUS = "add"
	| 	getBinopString T.MINUS = "sub"
	| 	getBinopString T.MUL = "mult"
	| 	getBinopString T.DIV = "div"
	| 	getBinopString T.AND = "and"
	| 	getBinopString T.OR = "or"


fun codegen (frame) (stm: Tree.stm) : A.instrlist = 
	let 
		val ilist = ref (nil: A.instr list)
		fun emit x = ilist := x :: !ilist
		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

		fun munchStm


		and 
			(* lw *)
		 	munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
		 		result(fn r => emit(A.OPER {assem = "lw 'd0, " ^ Int.toString i ^ "('s0)\n",
		 		 							src = [munchExp e1], 
		 		 							dst = [r], 
		 		 							jump = NONE}))
		| 	munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
		 		result(fn r => emit(A.OPER {assem = "lw 'd0, " ^ Int.toString i ^ "('s0)\n",
		 									src = [munchExp e1], 
		 									dst = [r], 
		 									jump = NONE}))

			(* addi *)
		|	munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
          		result(fn r => emit(A.OPER {assem = "addi 'd0, 's0, " ^ Int.toString i ^ "\n",
            								src = [munchExp e1], 
            								dst = [r], 
            								jump = NONE}))
      	| 	munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
          		result(fn r => emit(A.OPER {assem = "addi 'd0, 's0, " ^ Int.toString i ^ "\n",
           									src = [munchExp e1],
           									dst = [r],
           									jump = NONE}))
       		(* subi *)
       	| 	munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
				result(fn r => emit(A.OPER {assem = "addi 'd0, 's0, " ^ Int.toString (~i) ^ "\n",
            								src = [munchExp e1], 
            								dst = [r], 
            								jump = NONE}))
			(* andi *)
       	| 	munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
          		result(fn r => emit(A.OPER {assem="andi 'd0, 's0, " ^ Int.toString i ^ "\n",
        									src=[munchExp e1], 
        									dst=[r], 
        									jump=NONE}))
      	| 	munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
          		result(fn r => emit(A.OPER {assem="andi 'd0, 's0, " ^ Int.toString i ^ "\n",
            								src=[munchExp e1], 
            								dst=[r], 
            								jump=NONE}))
        	(* ori *)
        | 	munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
          		result(fn r => emit(A.OPER {assem="ori 'd0, 's0, " ^ Int.toString i ^ "\n",
            								src=[munchExp e1], 
            								dst=[r], 
            								jump=NONE}))
      	| 	munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
         		result(fn r => emit(A.OPER {assem="ori 'd0, 's0, " ^ Int.toString i ^ "\n",
            								src=[munchExp e1], 
            								dst=[r], 
            								jump=NONE}))
        	(* rest of binops *)
      	| 	munchExp(T.BINOP(binop, e1, e2)) =
        		result(fn r => emit(A.OPER {assem = getBinopString(binop) ^ " 'd0, 's0, 's1\n",
        									src = [munchExp e1, munchExp e2], 
        									dst=[r], 
        									jump=NONE}))
        	(* T.MEM *)
		| 	munchExp (T.MEM e1) = 
		   		result(fn r => emit(A.OPER {assem = "lw 'd0, 0('s0)\n",
		   									src=[munchExp e1], 
		   									dst=[r], 
		   									jump = NONE}))
		   	(* T.TEMP *)
		| 	munchExp (T.TEMP t) = t
			(* T.NAME *)
		|	munchExp (T.NAME l) =
        		result(fn r => emit(A.OPER {assem=("la 'd0, " ^ S.name(label) ^ "\n"),
            								src=[], 
            								dst=[r], 
            								jump=NONE}))
			(* T.CONST *)
		|	munchExp (T.CONST c) = 
				result(fn r => emit(A.OPER {assem = "li 'd0, " ^ Int.toString c ^ "\n",
											src = [],
											dst = [],
											jump = NONE}))
			(* T.CALL *)
		| 	munchExp (T.CALL (e, args)) = 
				result(fn r => emit(A.OPER {
					assem = "jal " ^ S.name(label) ^ "\n",
					src = munchArgs(0, args),	(************* IS IT CORRECT???? *************)
					dst = [F.RV],	(************* IS IT CORRECT???? *************)
					jump = NONE
				}))

		fun munchArgs (i, []) = []
	  	| 	munchArgs(i, a::l) = 
		  	let
		  		val argReg = T.TEMP(Temp.newtemp());
		  	in
		  		(munchStm(T.MOVE(argReg, a));
			  	argReg::munchArgs(i+1,l))
		  	end


	in 
		munchStm stm; rev(!ilist)
	end