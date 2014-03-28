structure Main =
struct

	fun test(progName) =
		let
			val parsedAbsyn = Parse.parse(progName)
			val fragList = Semant.transProg(parsedAbsyn)
			fun printFrag([]) = ()
				| printFrag((MipsFrame.PROC {body=bodyStm, frame=frame})::rest) = 
					(Printtree.printtree(TextIO.stdOut, bodyStm); printFrag(rest))
				| printFrag((MipsFrame.STRING strFrag)::rest) = printFrag(rest)
		in
			printFrag(fragList)
		end

end