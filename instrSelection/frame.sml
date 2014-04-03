signature FRAME = 
sig 
	val wordSize: int
	type frame
  datatype access = InFrame of int
                  | InReg of Temp.temp

	val newFrame : {name: Temp.label,
					formals: bool list} -> frame
	val name : frame -> Temp.label
	val formals : frame -> access list
	val allocLocal : frame -> bool -> access

  val FP : Temp.temp
  val exp : access -> Tree.exp -> Tree.exp

  val RV : Temp.temp
  val procEntryExit1 : frame * Tree.stm -> Tree.stm

  val externalCall: string * Tree.exp list -> Tree.exp

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end


structure MipsFrame :> FRAME = 
struct

  datatype access = InFrame of int
                  | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, spOffset: int ref}
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  (*sp is the offset for the stack pointer of the current frame from the fp*)

  structure Te = Temp
  structure Tr = Tree
  structure S = Symbol
  structure A = Assem

  val wordSize = 4

  val FP = Te.newtemp() (*FP should only be changed during Fn calls--> shifts into the level of the Fn*)
  val RV = Te.newtemp()
  val RA = Te.newtemp()
  val SP = Te.newtemp()
  val ZERO = Te.newtemp()

  val v0 = Temp.newtemp()   (* Return Vals*)
  val v1 = Temp.newtemp()   
 
  val a0 = Temp.newtemp()   (* Arguments *)
  val a1 = Temp.newtemp()   
  val a2 = Temp.newtemp()   
  val a3 = Temp.newtemp()   

  val t0 = Temp.newtemp()   (* Temps*)
  val t1 = Temp.newtemp()   
  val t2 = Temp.newtemp()   
  val t3 = Temp.newtemp()   
  val t4 = Temp.newtemp()   
  val t5 = Temp.newtemp()   
  val t6 = Temp.newtemp()   
  val t7 = Temp.newtemp()   
  val t8 = Temp.newtemp()   
  val t9 = Temp.newtemp()   
                            
  val s0 = Temp.newtemp()   (* Saved Temps *)
  val s1 = Temp.newtemp()   
  val s2 = Temp.newtemp()   
  val s3 = Temp.newtemp()   
  val s4 = Temp.newtemp()   
  val s5 = Temp.newtemp()   
  val s6 = Temp.newtemp()   
  val s7 = Temp.newtemp()   

  specialregs =  [FP, RV, RA, SP, ZERO, v0, v1] 
  argregs = [a0,a1,a2,a3] (* $a0-$a3 *)
  calleesaves = [s0,s2,s2,s3,s4,s5,s6,s7] (* $s0-$s7 *)
  callersaves = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9]  (* $t0-$t9 *)

  fun name({name = name, formals = _ , spOffset = _}) = name
  fun formals({name = _, formals = formals , spOffset = _}) = formals

  (*03/24/2014, currently assuming all formals are true--i.e. all parameters escape*)
  fun newFrame({name = name, formals = formals}) = 
    let val currOffset = ref 0
        fun allocFormals([]) = []
          | allocFormals(escape::rest) = (if escape
                                    then (currOffset := !currOffset-wordSize; InFrame(!currOffset)::allocFormals(rest))
                                    else InReg(Te.newtemp())::allocFormals(rest))
    in
      {name = name, formals = allocFormals(formals), spOffset = currOffset}
    end

  fun allocLocal({name = _, formals = _ , spOffset = spOffset}) = 
    let 
      val currOffset = spOffset
      fun allocL(escape) = (if escape
                            then (currOffset := !currOffset-wordSize; InFrame(!currOffset))
                            else InReg(Te.newtemp()))
    in
      allocL
    end

  fun exp(InFrame offset) = 
    let 
      fun addFP(fp: Tr.exp) = Tr.MEM(Tr.BINOP(Tr.PLUS, fp, Tr.CONST(offset)))
    in
      addFP
    end
  | exp(InReg t) = fn _ => Tr.TEMP(t)

  fun move(reg, var) = Tr.MOVE(Tr.TEMP(reg), Tr.TEMP(var))

  fun seq (stm::[]) = stm
    | seq (stm::rest) = Tr.SEQ(stm, seq(rest))
    | seq ([]) = Tr.EXP (Tr.CONST 0)

  fun moveArg (arg, access) =
      Tr.MOVE (exp access (Tr.TEMP(FP)), Tr.TEMP(arg))

  fun procEntryExit1 (frame, stm) =
    let
      val saved = RA :: calleesaves
      val tempRegs = map (fn temp => Temp.newtemp ()) saved
      val moveRegs = ListPair.mapEq move
      val saveRegs = seq ( moveRegs (tempRegs, saved))
      val restoreRegs = seq ( moveRegs (saved, tempRegs))
      (*This is all I know how to do*)
  in
      (*no idea what to do here*)
  end

  fun procEntryExit2 (frame,body) =
      body @ 
      [A.OPER{assem="",
              src=specialregs @ calleesaves,
              dst=[],jump=SOME[]}]

  fun procEntryExit3 ({name=name, formals=formals, locals=locals}:frame, 
                      body : Assem.instr list) =
      {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
       body = body,
       epilog = "END " ^ Symbol.name name ^ "\n"}

  fun externalCall(s, args) =
    Tr.CALL(Tr.NAME(Te.namedlabel s), args)

end