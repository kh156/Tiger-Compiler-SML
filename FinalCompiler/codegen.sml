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

    fun intToString i = if i >= 0
                        then Int.toString i
                        else "-" ^ Int.toString(~i)

    fun getBinopString T.PLUS = "add"
	| 	getBinopString T.MINUS = "sub"
	| 	getBinopString T.MUL = "mul"
	| 	getBinopString T.DIV = "div"
	| 	getBinopString T.AND = "and"
	| 	getBinopString T.OR = "or"
    |   getBinopString T.LSHIFT = "lshift"
    |   getBinopString T.RSHIFT = "rshift"
    |   getBinopString T.ARSHIFT = "arshift"
    |   getBinopString T.XOR = "xor"

fun codegen (frame) (stm: Tree.stm) : A.instr list = 
	let 
		val ilist = ref [] : A.instr list ref
		fun emit x = ilist := x :: !ilist
		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

    fun emitMoveInstr(srcTemp, dstTemp) =  emit(A.MOVE {assem = "move `d0, `s0\n",
                      src = srcTemp,
                      dst = dstTemp})

    fun firstBr(relop) = 
      (case relop of 
        T.EQ => "beq"
        | T.NE => "bne"
        | T.LT => "blt"
        | T.GT => "bgt"
        | T.LE => "ble"
        | T.GE => "bge"
        | T.ULT => "bltu"
        | T.ULE => "bgtu"
        | T.UGT => "bleu"
        | T.UGE => "bgeu")

    fun secondBr(relop) = 
      (case relop of 
        T.EQ => "bne"
        | T.NE => "beq"
        | T.LT => "bge"
        | T.GT => "ble"
        | T.LE => "bgt"
        | T.GE => "blt"
        | T.ULT => "bgeu"
        | T.ULE => "bgtu"
        | T.UGT => "bleu"
        | T.UGE => "bltu")

    fun emitBranchInstr(relop, result0, result1, tlabel, flabel)
                 = emit(A.OPER {assem = firstBr(relop) ^ " `s0, `s1, `j0\n\t" ^ secondBr(relop) ^ " `s0, `s1, `j1\n",
                                src = [result0, result1],
                                dst = [],
                                jump = SOME([tlabel, flabel])})

	fun munchStm(T.SEQ(stm1, stm2)) = 
        (munchStm(stm1); munchStm(stm2))

        | munchStm(T.LABEL(label)) =
          emit(A.LABEL {assem = (Symbol.name label ^ ":\n"),
                      lab = label})

        | munchStm(T.JUMP(T.TEMP ra, _)) = 
          emit(A.OPER {assem = "jr `s0\n",
                       src = [ra],
                       dst = [],
                       jump = NONE})

        | munchStm(T.JUMP(T.NAME labelName, l::rest)) = 
          emit(A.OPER {assem = "j `j0\n", 
                      src = [],
                      dst = [],
                      jump = SOME(l::rest)})

        | munchStm(T.JUMP(_, _)) = ErrorMsg.impossible "Tree.JUMP doesn't jump to a single label...(only case I know of)..."

        | munchStm(T.CJUMP(relop, exp1, exp2, tlabel, flabel)) = 
            emitBranchInstr(relop, munchExp(exp1), munchExp(exp2), tlabel, flabel)

        (*dst and src are both registers*)
        | munchStm(T.MOVE(T.TEMP r1, T.TEMP r2)) = emitMoveInstr(r2, r1)

        (*li instructions*)
        | munchStm(T.MOVE(T.TEMP r, T.CONST i)) = emit(A.OPER {assem = "li `d0, " ^ intToString i ^ "\n",
                                                        src = [],
                                                        dst = [r],
                                                        jump = NONE})
        (*dst is a register*)
        | munchStm(T.MOVE(T.TEMP r , exp)) = emitMoveInstr(munchExp(exp), r)

        | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), exp)) =
            emit(A.OPER {assem = "sw `s0, " ^ intToString i ^ "(`s1)\n",
                        src = [munchExp exp, munchExp e1],
                        dst = [],
                        jump = NONE})
            
        | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), exp)) =
            emit(A.OPER {assem = "sw `s0, " ^ intToString i ^ "(`s1)\n",
                        src = [munchExp exp, munchExp e1],
                        dst = [],
                        jump = NONE})

        | munchStm(T.MOVE(T.MEM(exp1), exp2)) =
            emit(A.OPER {assem = "sw `s0, 0(`s1)\n",
                        src = [munchExp exp2, munchExp exp1],
                        dst = [],
                        jump = NONE})

        (*neither of src and dst is register*)
        | munchStm(T.MOVE(exp1, exp2)) = emitMoveInstr(munchExp(exp2), munchExp(exp1))

        | munchStm(T.EXP exp) = (munchExp(exp);())

	and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
		 		result(fn r => emit(A.OPER {assem = "lw `d0, " ^ intToString i ^ "(`s0)\n",
		 		 							src = [munchExp e1], 
		 		 							dst = [r], 
		 		 							jump = NONE}))
		| 	munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
		 		result(fn r => emit(A.OPER {assem = "lw `d0, " ^ intToString i ^ "(`s0)\n",
		 									src = [munchExp e1], 
		 									dst = [r], 
		 									jump = NONE}))

			(* addi *)

        |   munchExp(T.BINOP(T.PLUS, T.CONST 0, T.CONST i)) =
            result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
                                            src = [F.ZERO], 
                                            dst = [r], 
                                            jump = NONE}))

        |   munchExp(T.BINOP(T.PLUS, T.CONST i, T.CONST 0)) =
            result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
                                            src = [F.ZERO], 
                                            dst = [r], 
                                            jump = NONE}))

        |   munchExp(T.BINOP(T.MINUS, T.CONST 0, T.CONST i)) =
            result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString (~i) ^ "\n",
                                            src = [F.ZERO], 
                                            dst = [r], 
                                            jump = NONE}))

		|	munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
            result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
            								src = [munchExp e1], 
            								dst = [r], 
            								jump = NONE}))
        | 	munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
           	result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
               									src = [munchExp e1],
               									dst = [r],
               									jump = NONE}))
        (* subi *)
        | 	munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
    			  result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString (~i) ^ "\n",
                								src = [munchExp e1], 
                								dst = [r], 
                								jump = NONE}))
    		(* andi *)
        | 	munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
            result(fn r => emit(A.OPER {assem="andi `d0, `s0, " ^ intToString i ^ "\n",
            									src=[munchExp e1], 
            									dst=[r], 
            									jump=NONE}))
        | 	munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
        		result(fn r => emit(A.OPER {assem="andi `d0, `s0, " ^ intToString i ^ "\n",
                								src=[munchExp e1], 
                								dst=[r], 
                								jump=NONE}))
        (* ori *)
        | 	munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
         		result(fn r => emit(A.OPER {assem="ori `d0, `s0, " ^ intToString i ^ "\n",
                								src=[munchExp e1], 
                								dst=[r], 
                								jump=NONE}))
        | 	munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
            result(fn r => emit(A.OPER {assem="ori `d0, `s0, " ^ intToString i ^ "\n",
                								src=[munchExp e1], 
                								dst=[r], 
                								jump=NONE}))
        (* rest of binops *)
        | 	munchExp(T.BINOP(binop, e1, e2)) =
            result(fn r => emit(A.OPER {assem = getBinopString(binop) ^ " `d0, `s0, `s1\n",
            									src = [munchExp e1, munchExp e2], 
            									dst=[r], 
            									jump=NONE}))
        (* T.MEM *)
		| 	munchExp (T.MEM e1) = 
		   	result(fn r => emit(A.OPER {assem = "lw `d0, 0(`s0)\n",
		   									src=[munchExp e1], 
		   									dst=[r], 
		   									jump = NONE}))
		(* T.TEMP *)
		| 	munchExp (T.TEMP t) = t
		(* T.NAME *)
		|	munchExp (T.NAME l) =
        		result(fn r => emit(A.OPER {assem=("la `d0, " ^ S.name(l) ^ "\n"),
            								src=[], 
            								dst=[r], 
            								jump=NONE}))
		(* T.CONST *)
		|	munchExp (T.CONST c) = 
				result(fn r => emit(A.OPER {assem = "li `d0, " ^ intToString c ^ "\n",
											src = [],
											dst = [r],
											jump = NONE}))
		(* T.CALL *)
		| 	munchExp (T.CALL (lexp, args)) = 
                let
                    val label= (case lexp of
                                T.NAME l => l
                                | _ => ErrorMsg.impossible "Call expression with non-label...(not function call??)...")
                    val argTemps = munchArgs(0, args, 0)

                in
    				(emit(A.OPER {
    					assem = "jal " ^ S.name(label) ^ "\n",
    					src = argTemps,	(*should be all the $a registers used in this fn call*)
    					dst = F.RV::F.callersaves,	(*RV, and all the $t registers because they could get altered*)
    					jump = NONE});
 
                    F.RV)
                end
        |   munchExp (T.ESEQ(_, _)) = ErrorMsg.impossible "Tree.ESEQ shouldn't appear in instruction selection phase..."


	and munchArgs (i, [], currSPOffset) = []
	  | munchArgs(i, a::l, currSPOffset) = 
		  let
		  	val argDst = if i<4 
                         then T.TEMP (List.nth(F.argregs,i)) 
                         else T.MEM(T.BINOP(T.PLUS, T.TEMP F.SP, T.CONST currSPOffset))
		  in
		  	(munchStm(T.MOVE(argDst, a));
		  	case argDst of
            T.TEMP t => t::munchArgs(i+1,l, currSPOffset)
            | _ => munchArgs(i+1,l, currSPOffset+4))
		  end


	in 
		munchStm stm; rev(!ilist)
	end
end
