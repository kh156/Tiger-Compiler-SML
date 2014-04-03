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

  val FP = Te.newtemp() (*FP should only be changed during Fn calls--> shifts into the level of the Fn*)
  val RV = Te.newtemp()
  val RA = Te.newtemp()
  val SP = Te.newtemp()
  val ZERO = Te.newtemp()
  val wordSize = 4

  specialregs =  [FP, RV, RA, SP, ZERO] 
  argregs = List.tabulate (4, (fn i => Temp.newtemp ())) (* $a0-$a3 *)
  calleesaves = List.tabulate (8, (fn i => Temp.newtemp ())) (* $s0-$s7 *)
  callersaves = List.tabulate (10, (fn i => Temp.newtemp ())) (* $t0-$t9 *)

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

  fun procEntryExit1(frame, body) = 
    let
      (*steps 4, 5, 8 on page 168*)
      val savedRegs = RA :: calleesaves

    in
      body
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