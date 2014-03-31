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


fun codegen (frame) (stm: Tree.stm) : A.instrlist = 
	let 
		val ilist = ref (nil: A.instr list)
		fun emit x = ilist := x :: !ilist
		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

		fun munchStm


		and 
			(* add imm *)
			munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
          		result(fn r => emit(A.OPER {assem = "addi 'd0, 's0, " ^ int i ^ "\n",
            								src = [munchExp e1], 
            								dst = [r], 
            								jump = NONE}))
      	| 	munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
          		result(fn r => emit(A.OPER {assem = "addi 'd0, 's0, " ^ int i ^ "\n",
           									src = [munchExp e1],
           									dst = [r],
           									jump = NONE}))
       		(* subt imm *)
       	| 	munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
				result(fn r => emit(A.OPER {assem = "addi 'd0, 's0, " ^ int (~i) ^ "\n",
            								src = [munchExp e1], 
            								dst = [r], 
            								jump = NONE}))
			(* and immediate *)
       	| 	munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
          		result(fn r => emit(A.OPER {assem="andi 'd0, 's0, " ^ int i ^ "\n",
        									src=[munchExp e1], 
        									dst=[r], 
        									jump=NONE}))
      	| 	munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
          		result(fn r => emit(A.OPER {assem="andi 'd0, 's0, " ^ int i ^ "\n",
            								src=[munchExp e1], 
            								dst=[r], 
            								jump=NONE}))
        	(* or immediate *)
        | 	munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
          		result(fn r => emit(A.OPER {assem="ori 'd0, 's0, " ^ int i ^ "\n",
            								src=[munchExp e1], 
            								dst=[r], 
            								jump=NONE}))
      	| 	munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
         		result(fn r => emit(A.OPER {assem="ori 'd0, 's0, " ^ int i ^ "\n",
            								src=[munchExp e1], 
            								dst=[r], 
            								jump=NONE}))


	in 
		munchStm stm; rev(!ilist)
	end