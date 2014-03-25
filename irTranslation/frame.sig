signature FRAME = 
sig 
	val wordSize: int

	type frame
	type access
	val newFrame : {name: Temp.label,
					formals: bool list} -> frame
	val name : frame -> Temp.label
	val formals : frame -> access list
	val allocLocal : frame -> bool -> access (* what's this ????????? *)
end