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
end


structure MipsFrame: FRAME = 
struct

  datatype access = InFrame of int
                  | InReg of Temp.temp

  (*sp is the offset for the stack pointer of the current frame from the fp*)
  type frame = {name: Temp.label, formals: access list, spOffset: int ref}

  structure Te = Temp
  structure Tr = Tree
  structure S = Symbol

  val wordSize = 4

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
      {name, allocFormals(formals), currOffset}
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

end