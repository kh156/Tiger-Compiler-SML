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
  val getResult : unit -> MipsFrame.frag list

  val simpleVar : access * level -> exp
end


structure Translate :> TRANSLATE = 
struct

  structure Te = Temp
  structure Tr = Tree
  structure F = MipsFrame
  structure A = Absyn
  val err = ErrorMsg.error
  exception ErrMsg

  type frag = F.frag

  datatype level = ROOT
                  | CHILD of {parent: level, frame: F.frame}
  type access = level * F.access

  val outermost = ROOT

  val fragList = ref [] : frag list ref

  fun newLevel {parent: level, name: Te.label, formals: bool list} = 
    let 
      val formals' = true::formals
      val newframe = F.newFrame({name=name, formals=formals'})
    in
      CHILD({parent=parent, frame=newframe})
    end

  fun formals({parent=parent, frame=frame}) = 
    let
      val theFormals = F.formals(frame)
    in
      case theFormals 
        of a::rest => rest
    end

  fun allocLocal({parent=parent, frame=theFrame}) = 
    let 
      fun allocL (escape:bool) = 
        let
          val frameAccess = F.allocLocal theFrame escape
        in
          ({parent=parent, frame=theFrame}, frameAccess)
        end
    in
      allocL
    end


  datatype exp = Ex of Tr.exp
               | Nx of Tr.stm
               | Cx of Te.label * Te.label -> Tr.stm

  fun seq (stm::[]) = stm
    | seq (stm::rest) = Tr.SEQ(stm, seq(rest))

  fun unEx (Ex e) = e
    | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)
    | unEx (Cx genstm) = 
      let
        val r = Te.newtemp()
        val t = Te.newlabel() and f = Te.newlabel()
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
    | unNx (Cx genstm) = Tr.EXP(unEx(Cx genstm)) (*evaluate the exp for side-effects and discard the result*)

  fun unCx (Cx genstm) = genstm
    | unCx (Nx _) = (ErrorMsg.impossible "Error: cannot unCx(Nx stm)!"; (fn (t, f) => Tr.JUMP(Tr.NAME(f), [f])))
    | unCx (Ex (Tr.CONST 0)) = (fn (t, f) => Tr.JUMP(Tr.NAME(f), [f]))
    | unCx (Ex (Tr.CONST 1)) = (fn (t, f) => Tr.JUMP(Tr.NAME(t), [t]))
    | unCx (Ex e) = (fn (t, f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f))

  fun seqExp [] = Ex (Tr.CONST 0)
    | seqExp [exp] = exp
    | seqExp (exp :: exps) =
        Ex (Tr.ESEQ (unNx exp, unEx (seqExp exps)))

  (*how is the current level useful here? I guess static links come into play here...*)
  fun simpleVar((varL: {parent: level, frame: F.frame}, fa: F.access), l: level) =
    let
      val f = (#frame varL) (*this is the frame the variable was declared*)
    in
      Ex (F.exp fa Tr.TEMP(F.FP)) (*F.FP? a single global FP???*)
    end

  fun fieldVar(varExp, index) = Ex (Tr.MEM(Tr.BINOP(Tr.PLUS, varExp, Tr.CONST (index*F.wordSize))))

  fun subscriptVar(varExp, Tr.CONST index) = Ex (Tr.MEM(Tr.BINOP(Tr.PLUS, varExp, Tr.CONST (index*F.wordSize))))

  fun procEntryExit(level: {parent:level, frame: F.frame}, body) = 
    let
      val funFrame = (#frame level)
      val addedSteps = F.procEntryExit1(funFrame, unNx(body))
      val moveStm = Tr.MOVE((Tr.TEMP F.RV), unEx body)
      val addedMove = Tr.SEQ(addedSteps, moveStm)
    in
      fragList := (F.PROC {body = addedMove, frame = funFrame})::(!fragList)
    end

  fun getResult() = !fragList

  fun nilExp () = Ex (Tr.CONST (0))
  fun intExp (i) = Ex (Tr.CONST (i))

  (*all kinds of transformations*)

  fun strExp str =
  let
    val label = Te.newlabel()
  in
    (fragList := F.STRING (label, str) :: !fragList;
    Ex (Tr.NAME label)) (*does string uses Tr.NAME???*)
  end

  fun binOpExp (oper, l, r) = 
    let 
      val unexL = unEx l
      val unexR = unEx r
    in
      Ex (Tr.BINOP(oper, unexL, unexR))
    end

  fun compExp (oper, l, r) =
    Cx ( fn(t,f) => Tr.CJUMP(oper, unEx l, unEx r, t, f) )

  fun intOpExp (A.PlusOp, l, r) = binOpExp(Tr.PLUS, l, r)
    | intOpExp (A.MinusOp, l, r) = binOpExp(Tr.MINUS, l, r)
    | intOpExp (A.TimesOp, l, r) = binOpExp(Tr.MUL, l, r)
    | intOpExp (A.DivideOp, l, r) = binOpExp(Tr.DIV, l, r)
    | intOpExp (A.LtOp, l, r) = compExp(Tr.LT, l, r)
    | intOpExp (A.GtOp, l, r) = compExp(Tr.GT, l, r)
    | intOpExp (A.GeOp, l, r) = compExp(Tr.GE, l, r)
    | intOpExp (A.LeOp, l, r) = compExp(Tr.LE, l, r)
    | intOpExp (A.EqOp, l, r) = compExp(Tr.EQ, l, r)
    | intOpExp (A.NeqOp, l, r) = compExp(Tr.NE, l, r)


  fun intExp(i) = Ex (Tr.CONST i)

  fun stringOpExp (A.EqOp, l, r) = Ex (F.externalCall("stringEqual", [unEx l, unEx r]))
    | stringOpExp (A.NeqOp, l, r) = 
      let 
        val result = unEx (stringOpExp(A.EqOp, l, r))
      in
        Ex (Tr.BINOP(Tr.XOR, result, Tr.CONST(1)))
      end

  fun recordExp({translated=fieldExps, size=size}) = 
    let
      val r = Te.newtemp()
    in
      Tr.ESEQ(seq[Tr.MOVE(Tr.TEMP r, F.externalCall("malloc", [Tr.CONST (size * F.wordSize)])),
                  seq(initRecordFields(fieldExps, [], 0, r))],
              Tr.TEMP r)
    end
  and initRecordFields(oneField::rest, result, curIndex, labelR):Tr.stm list = initRecordFields(rest,
    (Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP labelR, Tr.CONST (curIndex*F.wordSize))), oneField)::result), curIndex+1, labelR)
    | initRecordFields([], result, curIndex, labelR) = result

  fun assignExp(leftExp ,rightExp) = Nx (Tr.MOVE (unEx leftExp, unEx rightExp))

  fun callExp (l:level, label, exps) = Ex(Tr.CALL(Tr.NAME(label), map unEx exps)) (*need to calculate static links!!*)

  fun letExp ([], body) = body
        | letExp (decs, body) = Ex (Tr.ESEQ (seq (map unNx decs), unEx body))

  fun ifThenExp(testExp, thenExp) = 
    let
      val r = Te.newtemp()
      val t = Te.newlabel()
      val f = Te.newlabel()
    in
      Nx (seq[unCx (testExp) (t, f),
                    Tr.LABEL t,
                    (unNx thenExp),
                    Tr.LABEL f])
    end

  fun ifThenElseExp(testExp, thenExp, elseExp) = 
    let
      val r = Te.newtemp()
      val t = Te.newlabel()
      val f = Te.newlabel()
      val join = Te.newlabel()
    in
      Ex (Tr.ESEQ(seq[unCx (testExp) (t, f),
                    Tr.LABEL t,
                    Tr.MOVE(Tr.TEMP r, unEx thenExp),
                    Tr.JUMP(Tr.NAME(join), [join]),
                    Tr.LABEL f,
                    Tr.MOVE(Tr.TEMP r, unEx elseExp),
                    Tr.LABEL join],
                Tr.TEMP r))
    end

  fun whileExp(testExp, bodyExp, doneLabel) =
    let
      val l = Te.newlabel()
    in
      Nx (seq[(unCx testExp (l, doneLabel)),
                  Tr.LABEL l,
                  unNx bodyExp,
                  (unCx testExp) (l, doneLabel),
                  Tr.LABEL doneLabel])
    end

  fun forExp(iAccess, loExp, hiExp, bodyExp, doneLabel, level) = 
    let
      val l = Te.newlabel()
    in
      Nx (seq[unNx (assignExp(simpleVar(iAccess, level), loExp)),
                  Tr.LABEL l,
                  unNx bodyExp,
                  (compExp(Tr.LE, unEx (simpleVar(iAccess, level)), hiExp) (l, doneLabel))
                  Tr.LABEL doneLabel])
    end

  fun breakExp(doneLabel) = Nx (Tr.JUMP(Tr.NAME(doneLabel), [doneLabel]))

  fun arrayExp (size,init) =
    let
      val return = Tr.TEMP(Te.newtemp())
    in
      Ex (Tr.ESEQ(Tr.MOVE(return, F.externalCall("initArray", [unEx size, unEx init])), return))
    end
    
  fun addExpListBefore(listToAdd, letBody) = 
    Ex (Tr.ESEQ(seq(map unNx listToAdd), unEx letBody))

end
