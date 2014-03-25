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
<<<<<<< HEAD

  datatype access = InFrame of int
                  | InReg of Temp.temp

  (*sp is the offset for the stack pointer of the current frame from the fp*)
  type frame = {name: Temp.label, formals: access list, sp: int ref}

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
      val currOffset = (#sp frame)
      fun allocL(escape) = (if escape
                            then (currOffset := !currOffset-wordSize; InFrame(!currOffset))
                            else InReg(Te.newTemp()))
    in
      allocL
    end

=======
	val wordSize = 4

	datatype access = InFrame of int 
					| InReg of Temp.temp

	type frame = {name: Temp.label, formals: access list, offset: int ref} 

	fun newFrame {name: Temp.label, formals: bool list} = 
		let val start = ref 0
			fun newAccess [] = []
			| newAccess [b::rest] = 
				if b
					then (start := !start - wordSize; InFrame(!start)::newAccess(rest)) 
						(* subtract before InFrame????????????? *)

					else InReg(T.newtemp())::newAccess(rest)
		in	
			frame(name, newAccess(formals), start)
		end

	fun name (f:frame) = #name f
	fun formals (f:frame) = #formals f
	fun allocLocal ({ _, _, offset}) =
		let 
			fun ret b:bool = 
				if b 
					then (offset := !offset - wordSize; InFrame(!offset) )
						(* subtract before InFrame????????????? *)

					else (InReg(T.newtemp()))
		in
			ret
		end
>>>>>>> 610c7dba9aa406be87ec110393d7f166845522a1
end