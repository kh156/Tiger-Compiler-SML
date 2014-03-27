signature FRAME = 
sig 
	val wordSize: int

	type frame
	type access
	val newFrame : {name: Temp.label,
					formals: bool list} -> frame
	val name : frame -> Temp.label
	val formals : frame -> access list
	val allocLocal : frame -> bool -> access

  val FP : Temp.temp
  val wordSize : int
  val exp : access -> Tree.exp -> Tree.exp

  val RV : Temp.temp
  val procEntryExit1 : frame * Tree.stm -> Tree.stm

  val externalCall: string * Tree.exp list -> Tree.exp

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end


structure MipsFrame: FRAME = 
struct

  datatype access = InFrame of int
                  | InReg of Temp.temp

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  (*sp is the offset for the stack pointer of the current frame from the fp*)
  type frame = {name: Temp.label, formals: access list, spOffset: int ref}

  structure Te = Temp
  structure Tr = Tree
  structure S = Symbol

  val FP = Te.newtemp()
  val wordSize = 4
  val RV = Te.newtemp()

  fun name(frame) = (#name frame)
  fun formals(frame) = (#formals frame)

  (*03/24/2014, currently assuming all formals are true--i.e. all parameters escape*)
  fun newFrame(name, formals) = 
    let val currOffset = ref 0
        fun allocFormals([]) = []
          | allocFormals(escape::rest) = (if escape
                                    then (currOffset := !currOffset-wordSize; InFrame(!currOffset)::allocFormals(rest))
                                    else InReg(Te.newTemp()::allocFormals(rest)))
    in
      {name = name, formals = allocFormals(formals), spOffset = currOffset}
    end

  fun allocLocal(frame) = 
    let 
      val currOffset = (#spOffset frame)
      fun allocL(escape) = (if escape
                            then (currOffset := !currOffset-wordSize; InFrame(!currOffset))
                            else InReg(Te.newTemp()))
    in
      allocL
    end

  fun exp(InFrame offset) = 
    let 
      fun addFP(fp: Tr.exp) = Tr.MEM(Tr.BINOP(PLUS, fp, Tr.CONST(offset)))
    in
      addFP
    end
  | exp(InReg t) = fn _ => Tr.TEMP(t)


  fun procEntryExit1(frame, body) = 
    let
      (*steps 4, 5, 8 on page 168*)
    in
      body
    end

  fun externalCall(s, args) =
    Tr.CALL(Tr.NAME(Te.namedlabel s), args)

end