signature FRAME = 
sig 
	val wordSize: int
  datatype access = InFrame of int
                  | InReg of Temp.temp
	type frame = {name: Temp.label, formals: access list, spOffset: int ref, moveArgStms: Tree.stm}
  datatype register = Reg of string

	val newFrame : {name: Temp.label,
					formals: bool list} -> frame
	val name : frame -> Temp.label
	val formals : frame -> access list
	val allocLocal : frame -> bool -> access

  val colorable : Temp.temp list
  val colorableRegs : register list

  val FP : Temp.temp
  val exp : access -> Tree.exp -> Tree.exp

  val SP : Temp.temp
  val RA : Temp.temp
  val ZERO : Temp.temp
  val RV : Temp.temp
  val a0 : Temp.temp
  val procEntryExit1 : frame * Tree.exp -> Tree.stm
  val procEntryExit3 : Assem.instr list -> {body: Assem.instr list, prolog: string, epilog: string}

  val stringFrag : Temp.label * string -> string

  val specialregs : Temp.temp list
  val argregs : Temp.temp list
  val calleesaves : Temp.temp list
  val callersaves : Temp.temp list

  val externalCall: string * Tree.exp list -> Tree.exp

  val tempMap: register Temp.table
  val setTempMap: register Temp.table -> unit

  val getTempName: Temp.temp -> string

  val getReg : Temp.temp -> register

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end


structure MipsFrame :> FRAME = 
struct

  datatype access = InFrame of int
                  | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, spOffset: int ref, moveArgStms: Tree.stm}

  datatype register = Reg of string
  
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

  val v1 = Te.newtemp()   
 
  val a0 = Te.newtemp()   (* Arguments *)
  val a1 = Te.newtemp()   
  val a2 = Te.newtemp()   
  val a3 = Te.newtemp()   

  val t0 = Te.newtemp()   (* Temps*)
  val t1 = Te.newtemp()   
  val t2 = Te.newtemp()   
  val t3 = Te.newtemp()   
  val t4 = Te.newtemp()   
  val t5 = Te.newtemp()   
  val t6 = Te.newtemp()   
  val t7 = Te.newtemp()   
  val t8 = Te.newtemp()   
  val t9 = Te.newtemp()   
                            
  val s0 = Te.newtemp()   (* Saved Temps *)
  val s1 = Te.newtemp()   
  val s2 = Te.newtemp()   
  val s3 = Te.newtemp()   
  val s4 = Te.newtemp()   
  val s5 = Te.newtemp()   
  val s6 = Te.newtemp()   
  val s7 = Te.newtemp()   

  val tempMap = foldr (fn ((temp, regEntry), table) => Te.enter(table, temp, regEntry))
            Te.empty
            [(FP, Reg "$fp"),
             (RV, Reg "$v0"),
             (RA, Reg "$ra"),
             (SP, Reg "$sp"),
             (ZERO, Reg "$0"),
             (v1, Reg "$v1"),
             (a0, Reg "$a0"),
             (a1, Reg "$a1"),
             (a2, Reg "$a2"),
             (a3, Reg "$a3"),
             (t0, Reg "$t0"),
             (t1, Reg "$t1"),
             (t2, Reg "$t2"),
             (t3, Reg "$t3"),
             (t4, Reg "$t4"),
             (t5, Reg "$t5"),
             (t6, Reg "$t6"),
             (t7, Reg "$t7"),
             (t8, Reg "$t8"),
             (t9, Reg "$t9"),
             (s0, Reg "$s0"),
             (s1, Reg "$s1"),
             (s2, Reg "$s2"),
             (s3, Reg "$s3"),
             (s4, Reg "$s4"),
             (s5, Reg "$s5"),
             (s6, Reg "$s6"),
             (s7, Reg "$s7")]

  val myTempMap = ref tempMap

  val specialregs =  [FP, RV, RA, SP, ZERO, v1] 
  val argregs = [a0,a1,a2,a3] (* $a0-$a3 *)
  val calleesaves = [s0,s1,s2,s3,s4,s5,s6,s7] (* $s0-$s7 *)
  val callersaves = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9]  (* $t0-$t9 *)

  val colorable = calleesaves @ callersaves (* We can use s and t regs to load vars*)

  fun name({name = name, formals = _ , spOffset = _, moveArgStms = _ }) = name
  fun formals({name = _, formals = formals , spOffset = _, moveArgStms = _ }) = formals

  fun seq (stm::[]) = stm
    | seq (stm::rest) = Tr.SEQ(stm, seq(rest))
    | seq ([]) = Tr.EXP (Tr.CONST 0)

  fun exp(InFrame offset) = 
    let 
      fun addFP(fp: Tr.exp) = Tr.MEM(Tr.BINOP(Tr.PLUS, fp, Tr.CONST(offset)))
    in
      addFP
    end
  | exp(InReg t) = fn _ => Tr.TEMP(t)

  val argList = ref argregs
  val argPtr = ref 0
  fun resetArgList() = (argList := argregs; argPtr := 0)
  fun nextFreeArg() = (case (!argList) of 
                  oneArgReg::others => (argList := others; Tr.TEMP oneArgReg)
                  | [] => (argPtr := !argPtr + 4; (exp (InFrame (!argPtr)) (Tr.TEMP FP))))

  (*03/24/2014, currently assuming all formals are true--i.e. all parameters escape*)
  fun newFrame({name = name, formals = formals}) = 
    let 
        val _ = resetArgList()
        val currOffset = ref ~40
        val moveArgList = ref [] : Tr.stm list ref

        fun allocFormals([]) = []
          | allocFormals(escape::rest) = 
            let
              val temp = Te.newtemp()
            in
              (if escape
               then (currOffset := !currOffset-wordSize;
               moveArgList := (!moveArgList) @ [Tr.MOVE(exp (InFrame (!currOffset)) (Tr.TEMP FP), nextFreeArg())];
               InFrame(!currOffset)::allocFormals(rest))
               else (moveArgList := (!moveArgList) @ [Tr.MOVE(Tr.TEMP temp, nextFreeArg())];
               InReg(temp)::allocFormals(rest)))
            end

        val allocatedSL = (moveArgList := (!moveArgList) @ [Tr.MOVE(exp (InFrame (0)) (Tr.TEMP FP), nextFreeArg())];
                          InFrame(!currOffset))
        val allocatedFormals = allocFormals (tl formals)
        val _ = resetArgList()
    in
      {name = name, formals = allocatedSL::allocatedFormals, spOffset = currOffset, moveArgStms = seq(!moveArgList)}
    end

  fun allocLocal({name = _, formals = _ , spOffset = spOffset, moveArgStms = _}) = 
    let 
      val currOffset = spOffset
      fun allocL(escape) = (if escape
                            then (currOffset := !currOffset-wordSize; InFrame(!currOffset))
                            else InReg(Te.newtemp()))
    in
      allocL
    end

  fun seq (stm::[]) = stm
    | seq (stm::rest) = Tr.SEQ(stm, seq(rest))
    | seq ([]) = Tr.EXP (Tr.CONST 0)


  fun procEntryExit1 (frame: frame, bodyExp) =
    let
      val lab = Tr.LABEL (#name frame)
      val addedMoveArgs = Tr.ESEQ(#moveArgStms frame, bodyExp)
      val frameSize = !(#spOffset frame)
      val moveSPDown = Tr.MOVE(Tr.TEMP SP, Tr.BINOP(Tr.PLUS, Tr.TEMP SP, Tr.CONST (frameSize+(~4))))
      val moveSPUp = Tr.MOVE(Tr.TEMP SP, Tr.BINOP(Tr.MINUS, Tr.TEMP SP, Tr.CONST (frameSize+(~4))))
      val moveStm = Tr.MOVE((Tr.TEMP RV), addedMoveArgs)
  in
      seq [lab, moveSPDown, moveStm, moveSPUp]
  end

  (*what does this do??... 04/18 by Ang*)
(*  fun procEntryExit2 (frame,body) =
      body @ 
      [A.OPER{assem="",
              src=specialregs @ calleesaves,
              dst=[],jump=SOME[]}]*)


  fun procEntryExit3 (body : Assem.instr list) =
    let
      val t = List.last body
      val (h, b) = (case body of 
                  hd::body => (hd, List.filter (fn e => e <> t) body)
                  | _ => ErrorMsg.impossible "Error : body of instr list has no elements")

(*      val allocspace = Assem.OPER{assem="addi $sp, $sp, -32\n",
                                                dst=[],
                                                src=[], 
                                                jump=NONE}*)      
      val saveFP = Assem.OPER{assem="sw $fp, -12($sp)\n",
                                                dst=[],
                                                src=[FP], 
                                                jump=NONE}       

      val moveFP = Assem.OPER{assem="addi $fp, $sp, -4\n",
                                                dst=[FP],
                                                src=[SP], 
                                                jump=NONE}

      val saveRA = Assem.OPER{assem="sw $ra, -4($fp)\n",
                                                dst=[],
                                                src=[RA], 
                                                jump=NONE}
                             
      fun append1(level) = 
        if level <= 7
          then append1(level + 1) @ [Assem.OPER{assem="sw $s" ^ Int.toString level ^ ", -" ^ (Int.toString (level*4+12)) ^ "($fp)\n",
                                                dst=[], 
                                                src=[List.nth(calleesaves, level)], 
                                                jump=NONE}]
          else []
      fun append2(level)= 
        if level <= 7 
          then [Assem.OPER {assem="lw $s" ^ Int.toString level ^ ", -" ^ (Int.toString (level * 4 + 12)) ^ "($fp)\n", 
                            dst=[List.nth(calleesaves, level)], 
                            src=[FP], 
                            jump=NONE}] @ append2(level+1)
          else []
(*      val freespace = Assem.OPER{assem="addi $sp, $sp, 32\n",
                                                dst=[],
                                                src=[], 
                                                jump=NONE}*)
      val loadRA = Assem.OPER{assem="lw $ra, -4($fp)\n",
                                                dst=[RA],
                                                src=[FP], 
                                                jump=NONE}
      val loadFP = Assem.OPER{assem="lw $fp, -12($sp)\n",
                                                dst=[FP],
                                                src=[SP], 
                                                jump=NONE}   
      val body' = h :: ([saveFP, moveFP, saveRA] @ append1(0) @ b @ append2(0)) @ [loadRA, loadFP, t]
    in
      {prolog = "PROCEDURE",
       body = body',
       epilog = "END"}
    end

  fun externalCall(s, args) =
    Tr.CALL(Tr.NAME(Te.namedlabel s), args)


  fun getTempName(temp) =
    let 
      val name = Te.Table.look(!myTempMap, temp)
    in
      case name of NONE => Te.makestring(temp)
                 | SOME(Reg regstr) => regstr
    end

  fun getReg (temp) =
    let 
      val name = Te.Table.look(!myTempMap, temp)
    in
      case name of NONE => Reg "Undefined"
                 | SOME(regstr) => regstr
    end

  fun setTempMap (newTempMap) =
    myTempMap := newTempMap
  (*To my understanding, are these all the pre-colored regs???*)  
  val colorableRegs = map getReg colorable

  fun stringFrag(lab, str) = 
    let
      val l = String.size str
      fun countSlash(oneChar::rest, currCount) = 
        (case oneChar of 
          #"\\" => (case rest of
                    nextChar::rest2 => countSlash(rest2, currCount+1)
                    | _ => ErrorMsg.impossible "Escape string error: unclosed string because ending with \\ char!")
          | _ => countSlash(rest, currCount))
        | countSlash([], currCount) = currCount
      val numOfSlash = countSlash ((explode str), 0)
    in
      Symbol.name lab ^ ":\n\t.word " ^ (Int.toString (l-numOfSlash)) ^ "\n\t.ascii \"" ^ str ^ "\"\n"
    end

end