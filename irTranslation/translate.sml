signature TRANSLATE = 
sig
	type level 
	type access (*not same as Frame.access*)
  type exp

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access
	
  val procEntryExit : {level: level, body: exp} -> unit
  val getResult : unit -> FRAME.frag list

  val simpleVar : access * level -> exp
end


structure Translate : TRANSLATE = 

  structure Te = Temp
  structure Tr = Tree
  structure F = Frame
  structure A = Absyn
  val err = ErrorMsg.error
  exception ErrMsg

  type frag = FRAME.frag

  datatype level = ROOT
                  | CHILD of {parent: level, frame: F.frame}
  type access = level * F.access

  val outermost = ROOT

  val funFragList = ref []

  fun newLevel {parent: level, name: Te.label, formals: bool list} = 
    let 
      val formals' = true::formals
      val newframe = F.newFrame(name, formals')
    in
      CHILD(parent, newframe)
    end

  fun formals(level) = 
    let
      val theFrame = (#frame level)
      val theFormals = F.formals(theFrame)
    in
      case theFormals 
        of a::rest => rest
    end

  fun allocLocal(level) = 
    let 
      fun allocL escape:bool = 
        let
          val theFrame = (#frame level)
          val frameAccess = F.allocLocal theFrame escape
        in
          (level, frameAccess)
        end
    in
      allocL
    end


  datatype exp = Ex of Tr.exp
               | Nx of Tr.stm
               | Cx of Te.label * Te.label

  fun seq stm:[] = stm
    | seq [stm::rest] = Tr.SEQ(stm, seq(rest))

  fun unEx (Ex e) = e
    | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)
    | unEx (Cx genstm) = 
      let
        val r = Te.newtemp()
        val t = Te.newlabel() and f = Te.newlable()
      in
        Tr.ESEQ(seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1),
                    genstm(t, f),
                    Tr.LABEL f,
                    Tr.MOVE(Tr.TEMP r, Tr.CONST 0),
                    Tr.LABEL t],
                Tr.TEMP r)
      end

  fun unNx (Ex e) = Tr.EXP(e)
    | unNx (Nx s) = s
    | unNx (Cx genstm) = Tr.EXP(unEx(genstm)) (*evaluate the exp for side-effects and discard the result*)

  fun unCx (Cx genstm) = genstm
    | unCx (Nx _) = ErrorMsg.impossible "Error: cannot unCx(Nx stm)!"
    | unCx (Tr.CONST 0) = fn (t, f) => Tr.JUMP(Tr.NAME(f), [f])
    | unCx (Tr.CONST 1) = fn (t, f) => Tr.JUMP(Tr.NAME(t), [t])
    | unCx (Ex e) = fn (t, f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f)

  (*how is the current level useful here? I guess static links come into play here...*)
  fun simpleVar((varL: level, fa: F.access), l: level) =
    let
      f = (#frame varL) (*this is the frame the variable was declared*)
    in
      Ex (f.exp(fa) Tr.TEMP(f.FP))
    end

  fun procEntryExit(level, body) = 
    let
      val funFrame = (#frame level)
      val addedSteps = F.procEntryExit1(funFrame, unNx(body))
      val moveStm = Tr.MOVE((Tr.TEMP funFrame.RV), body)
      val addedMove = Tr.SEQ(addedSteps, moveStm)
    in
      funFragList := (F.PROC {body = addedMove, frame = funFrame})::(!funFragList)
    end

  fun getResult() = !funFragList

  fun nilExp () = Ex (Tr.CONST (0))
  fun intExp (i) = Ex (Tr.CONST (i))

  fun strExp str =
  let
    val label = Temp.newlabel()
  in
    frags := Frame.STRING (label, str) :: !frags;
    Ex (Tr.NAME label)
  end


fun seq [] = Tr.EXP (T.CONST 0)
  | seq [s]  = s
  | seq(h::t) = Tr.SEQ(h,seq(t))

fun seqExp [] = Ex (Tr.CONST 0)
    | seqExp [exp] = exp
    | seqExp (exp :: exps) =
        Ex (Tr.ESEQ (unNx exp, unEx (seqExp exps)))


  fun binOpExp (oper, l, r) = 
    let 
      val unexL = unEx l
      val unexR = unEx r
    in
      Ex (Tr.BINOP(oper, unexL, unexR))
    end

  fun compExp (oper, l, r) =
    Cx ( fn(t,f) => Tr.CJUMP(oper, l, r, t, f) )

  fun intOpExp (A.PlusOp, l, r) = binOpExp(Tr.PLUS, l, r)
    | intOpExp (A.MinusOp, l, r) = binOpExp(Tr.MINUS, l, r)
    | intOpExp (A.TimesOp, l, r) = binOpExp(Tr.MUL, l, r)
    | intOpExp (A.DivideOp, l, r) = binOpExp(Tr.DIV, l, r)
    | intOpExp (A.LtOp, l, r) = compExp(Tr.LT, l, r)
    | intOpExp (A.GtOp, l, r) = compExp(Tr.GT, l, r)
    | intOpExp (A.GeOp, l, r) = compExp(Tr.GE, l, r)
    | intOpExp (A.LeOp, l, r) = compExp(Tr.LE, l, r)
    | intOpExp (A.EqOp, l, r) = compExp(Tr.EQ, l, r)
    | intOpExp (A.NeOp, l, r) = compExp(Tr.NE, l, r)


  fun stringOpExp

  fun recordExp

  fun assignExp(leftExp ,rightExp) = Nx (Tr.MOVE (unEx leftexp, unEx rightexp))

  fun callExp (l:level, label, exps : exp list) = Ex(Tr.CALL(Tr.NAME(label),map(unEx, exps)))

  fun letExp ([], body) = body
        | letExp (decs, body) = Ex (T.ESEQ (seq (map unNx decs), unEx body))


  (*all kinds of transformations*)
  


end
