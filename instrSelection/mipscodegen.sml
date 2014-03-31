signature CODEGEN = 
sig
	structure F : FRAME
	val codegen : F.frame -> Tree.stm -> Assem.instr list
end


structure Mips:CODEGEN = 
struct
	structure F = MipsFrame
	structure T = Tree
	structure A = Assem
	structure S = Symbol


fun codegen (frame) (stm: Tree.stm) : A.instrlist = 
	let 
		val ilist = ref (nil: A.instr list)
		fun emit x = ilist := x :: !ilist
		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

		fun munchStm
			

		and munchExp

	in 
		munchStm stm; rev(!ilist)
	end