signature TRANSLATE = 
sig
  datatype level = ROOT
                  | CHILD of {parent: level, frame: MipsFrame.frame, unique: unit ref}
	type access (*not same as Frame.access*)
  type exp
  type frag

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access
	
  val procEntryExit : {level: level, body: exp} -> unit
  val getResult : unit -> MipsFrame.frag list

  val simpleVar : access * level -> exp
  val getStaticLink : level * Tree.exp -> Tree.exp

  val resetFragList : unit -> unit

end


structure Translate = 
struct

  structure Te = Temp
  structure Tr = Tree
  structure F = MipsFrame
  structure A = Absyn
  val err = ErrorMsg.error
  exception ErrMsg

  type frag = F.frag

  datatype level = ROOT
                  | CHILD of {parent: level, frame: F.frame, unique: unit ref}

  type access = level * F.access

  val outermost = ROOT

  val fragList = ref [] : frag list ref

  fun resetFragList() = (fragList := []; ())

  fun crossWithLevel([], level) = []
    | crossWithLevel(formal::rest, level) = (level, formal)::crossWithLevel(rest, level)

  fun newLevel {parent: level, name: Te.label, formals: bool list} = 
    let 
      val formals' = true::formals
      val newframe = F.newFrame({name=name, formals=formals'})
    in
      CHILD({parent=parent, frame=newframe, unique = ref ()})
    end


  (*I made formals return the whole list--including the first element which is the static link.*)
  fun formals(CHILD {parent=parent, frame=frame, unique=unique}) = 
    let
      val theFormals = F.formals(frame)
    in
      crossWithLevel(theFormals, CHILD {parent=parent, frame=frame, unique=unique})
    end
    | formals(ROOT) = (ErrorMsg.impossible "Error: no formals can be found at the ROOT level!")


  fun allocLocal(CHILD {parent=parent, frame=frame, unique=unique}) = 
    let 
      fun allocL (escape:bool) = 
        let
          val frameAccess = F.allocLocal frame escape
        in
          (CHILD {parent=parent, frame=frame, unique=unique}, frameAccess)
        end
    in
      allocL
    end
    | allocLocal(ROOT) = (ErrorMsg.impossible "Error: cannot allocal local variable in ROOT level!")

  fun getStaticLink(CHILD {parent=parent, frame=frame, unique=unique}, newFP) =
    let
      val formalAccesses = formals (CHILD {parent=parent, frame=frame, unique=unique})
    in
      case formalAccesses of
        (l, fa)::rest => F.exp fa newFP
        | _ => ErrorMsg.impossible "Error: cannot find static link in a level..."
    end
    | getStaticLink(ROOT, newFP) = ErrorMsg.impossible "Error: cannot find static link in ROOT level..."

  datatype exp = Ex of Tr.exp
               | Nx of Tr.stm
               | Cx of Te.label * Te.label -> Tr.stm

  fun seq (stm::[]) = stm
    | seq (stm::rest) = Tr.SEQ(stm, seq(rest))
    | seq ([]) = Tr.EXP (Tr.CONST 0)

  fun unEx (Ex e) = e
    | unEx (Nx s) = (case s of 
                    Tr.EXP e => e 
                    | _ => Tr.ESEQ(s, Tr.CONST 0))
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
  fun simpleVar((CHILD {parent=parent, frame=f, unique=uniqueRef}, fa: F.access), l: level) =
    let
      fun fpOfDesLevel(CHILD {parent=desParent, frame=desFrame, unique=desUnique},
                            CHILD {parent=curParent, frame=curFrame, unique=curUnique}, newFP) = 
        if (curUnique = desUnique)
        then newFP
        else 
          let
            val slExpOfCurLevel = getStaticLink(CHILD {parent=curParent, frame=curFrame, unique=curUnique}, newFP)
          in
            fpOfDesLevel(CHILD {parent=desParent, frame=desFrame, unique=desUnique}, curParent, slExpOfCurLevel)
          end
      | fpOfDesLevel(_, _, _) = ErrorMsg.impossible "Tracing static link reaches the ROOT level..."
    in
      Ex (F.exp fa (fpOfDesLevel(CHILD {parent=parent, frame=f, unique=uniqueRef}, l, Tr.TEMP F.FP)))
      (*F.FP? a single global FP? only move when in Function calls?*)
    end
    | simpleVar ((ROOT, fa: F.access), l:level) = ErrorMsg.impossible "Reached ROOT level while tracing static links..."

  fun fieldVar(varExp, index) = Ex (Tr.MEM(Tr.BINOP(Tr.PLUS, unEx varExp, Tr.CONST (index*F.wordSize))))

  fun subscriptVar(varExp, index) =
    let
      val zeroBasedIndex = Tr.BINOP(Tr.MINUS, unEx index, Tr.CONST 1)
    in
      Ex (Tr.MEM(Tr.BINOP(Tr.PLUS, unEx varExp, Tr.BINOP(Tr.MUL, zeroBasedIndex, Tr.CONST (F.wordSize)))))
    end

  fun procEntryExit({level=CHILD {frame=frame, parent=parent, unique=unique}, body=body}) = 
    let
      val funFrame = frame
      val addedSteps = F.procEntryExit1(funFrame, unNx(body))
      val moveStm = Tr.MOVE((Tr.TEMP F.RV), unEx (Nx addedSteps))
    in
      fragList := (F.PROC {body = moveStm, frame = funFrame})::(!fragList)
    end
    | procEntryExit({level=ROOT, body=body}) = (ErrorMsg.impossible "Error: no function should be at the ROOT level!")

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
    | stringOpExp (_, l, r) = Ex (Tr.CONST 0)

  fun recordExp({translated=fieldExps, size=size}) = 
    let
      val r = Te.newtemp()
    in
      Ex (Tr.ESEQ(seq[Tr.MOVE(Tr.TEMP r, F.externalCall("malloc", [Tr.CONST (size * F.wordSize)])),
                  seq(initRecordFields(fieldExps, [], 0, r))],
              Tr.TEMP r))
    end
  and initRecordFields(oneField::rest, result, curIndex, labelR):Tr.stm list = initRecordFields(rest,
    result @ [(Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP labelR, Tr.CONST (curIndex*F.wordSize))), unEx oneField))], curIndex+1, labelR)
    | initRecordFields([], result, curIndex, labelR) = result

  fun assignExp(leftExp ,rightExp) = Nx (Tr.MOVE (unEx leftExp, unEx rightExp))

  fun callExp (l:level, label, exps) = Ex (Tr.CALL(Tr.NAME(label), map unEx exps))

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
      val l2 = Te.newlabel()
    in
      Nx (seq[unNx (assignExp(simpleVar(iAccess, level), loExp)),
                  Tr.LABEL l,
                  unNx bodyExp,
                  (unCx (compExp(Tr.EQ, (simpleVar(iAccess, level)), hiExp)) (doneLabel, l2)),
                  Tr.LABEL l2,
                  Tr.EXP (Tr.BINOP(Tr.PLUS, unEx (simpleVar(iAccess, level)), Tr.CONST 1)),
                  Tr.LABEL doneLabel])
    end

  fun breakExp(doneLabel) = Nx (Tr.JUMP(Tr.NAME(doneLabel), [doneLabel]))

  fun arrayExp (init,size) =
    let
      val return = Tr.TEMP(Te.newtemp())
    in
      Ex (Tr.ESEQ(seq[Tr.MOVE(return, F.externalCall("malloc", [Tr.BINOP(Tr.MUL, (unEx size), Tr.CONST F.wordSize)])),
                      Tr.EXP (F.externalCall("initArray", [unEx size, unEx init]))
                      ], return))
    end
    
  fun addExpListBefore(listToAdd, letBody) = 
    Ex (Tr.ESEQ(seq(map unNx listToAdd), unEx letBody))

end
