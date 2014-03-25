structure MipsFrame: FRAME = 
struct
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
	fun allocLocal ({ _, _, offset}) =  (* ????????????? *)
		let 
			fun ret b:bool = 
				if b 
					then (offset := !offset - wordSize; InFrame(!offset) )
						(* subtract before InFrame????????????? *)

					else (InReg(T.newtemp()))
		in
			ret
		end
end