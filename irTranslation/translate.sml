signature TRANSLATE = 
sig
  type access (*not the same as Frame.access*)
  type level 
  type frag
  type breakpoint
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm

  val frags : frag list ref
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access
  val assign : exp * exp -> exp
  
  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
  val seq : Tree.stm list ->Tree.stm
  val newbreakpoint : unit -> breakpoint
  val assignExp : exp * exp -> exp
  val breakExp : Tree.label -> exp
  val intExp : int -> exp
  val nilExp : unit -> exp
  val ifThenExp : exp * exp -> exp
  val ifThenElseExp : exp * exp * exp -> exp
  val intOpExp : Absyn.oper * exp * exp -> exp
  val letExp : exp list * exp -> exp
  val seqExp : exp list -> exp
  val stringExp : string -> exp
  val stringOpExp : Absyn.oper * exp * exp -> exp
  val whileExp : exp * exp * Tree.label -> exp
  val forExp : exp * Tree.label * exp * exp * exp -> exp
  val callExp : level * Tree.label * exp list  -> exp
  val recordExp : exp list  -> exp
  
  val simpleVar : access * level -> exp
  val subscriptExp : exp * exp -> exp
  val fieldVar : exp * exp -> exp
  val empty : exp

  val procEntryExit: {level: level, body: exp} -> unit
  val getResult : unit -> frag list
  (*val breakExp : exp -> Temp.label*)
  
end


structure Translate : TRANSLATE = struct
  structure Frame : FRAME = MipsFrame
  structure A = Absyn
  structure T = Tree
  type breakpoint = Tree.label
  val err = ErrorMsg.error
  exception ErrMsg

  fun nilExp () = Ex (T.CONST (0))
  fun intExp (i) = Ex (T.CONST (i))

  fun stringExp str =
  let
    val label = Temp.newlabel()
  in
    frags := Frame.STRING (label, str) :: !frags;
    Ex (T.NAME label)
  end

  fun seqExp [] = Ex (T.CONST 0)
      | seqExp [exp] = exp
      | seqExp (exp :: exps) =
          Ex (T.ESEQ (unNx exp, unEx (seqExp exps)))


fun binOpExp (oper, l, r) = 
  let 
    val unexL = unEx l
    val unexR = unEx r
  in
    Ex (T.BINOP(oper, unexL, unexR))
  end

fun intOpExp (A.PlusOp, l, r) = binOpExp(T.PLUS, l, r)
  | intOpExp (A.MinusOp, l, r) = binOpExp(T.MINUS, l, r)
  | intOpExp (A.TimesOp, l, r) = binOpExp(T.MUL, l, r)
  | intOpExp (A.DivideOp, l, r) = binOpExp(T.DIV, l, r)
  | intOpExp (A.LtOp, l, r) = binOpExp(T.LT, l, r)
  | intOpExp (A.GtOp, l, r) = binOpExp(T.GT, l, r)
  | intOpExp (A.GeOp, l, r) = binOpExp(T.GE, l, r)
  | intOpExp (A.LeOp, l, r) = binOpExp(T.LE, l, r)
  | intOpExp (A.EqOp, l, r) = binOpExp(T.EQ, l, r)
  | intOpExp (A.NeOp, l, r) = binOpExp(T.NE, l, r)



fun stringOpExp
fun recordExp


fun assignExp (var, exp) =
  let
    val unxVar = unEx var
    val unxExp = unEx exp
  in
    Nx (T.MOVE(var, exp))
  end

fun callExp (l:level, label, exps : exp list) = Ex(T.CALL(T.NAME(label),sl::exps))



end