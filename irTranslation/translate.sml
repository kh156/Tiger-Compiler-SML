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
  
  structure F = Frame
  structure Te = Temp
  structure Tr = Tree

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
    | unNx (Cx genstm) = Tr.EXP(unEx(genstm))(*not sure about this...transform a conditional exp into a stm???*)

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
      f.exp(fa) Tr.TEMP(f.FP)
    end

  fun procEntryExit(level, body) = (funFragList := (!funFragList)::F.PROC({body = unNx(body), frame = (#frame level)}))

  fun getResult() = !funFragList


end
