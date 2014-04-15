structure Main =
struct

	fun test(progName) =
		let
			val parsedAbsyn = Parse.parse(progName)
			val _ = FindEscape.findEscape parsedAbsyn
			val fragList = Semant.transProg(parsedAbsyn)
			fun printFrag([]) = ()
				| printFrag((MipsFrame.PROC {body=bodyStm, frame=frame})::rest) = 
					(Printtree.printtree(TextIO.stdOut, bodyStm); printFrag(rest))
				| printFrag((MipsFrame.STRING strFrag)::rest) = printFrag(rest)
		in
			printFrag(fragList)
		end

   structure Tr = Translate
   structure F = MipsFrame
   structure Te = Temp
   (*structure R = RegAlloc*)

   fun getsome (SOME x) = x
    | getsome (_) = ErrorMsg.impossible "Error during getSome in MainGiven..."

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")
(*         val _ = Printtree.printtree(out,body); *)
	       val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	       val instrs =   List.concat(map (Mips.codegen frame) stms')
         val format0 = Assem.format(F.getTempName)
      in  
         app (fn i => TextIO.output(out,format0 i)) instrs
     end
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out, (Symbol.name lab)^": "^s^"\n")

   fun withOpenFile fname f = 
       let 
          val out = TextIO.openOut fname
       in (f out before TextIO.closeOut out) 
	        handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
            withOpenFile (filename ^ ".s") 
	     (fn out => (app (emitproc out) frags))
       end


	fun emitprocDataFlow (F.PROC{body,frame}) =
     let 
          (*val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")*)
(*         val _ = Printtree.printtree(out,body); *)
	     val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	       val instrs =  List.concat(map (Mips.codegen frame) stms')

         val (flowGraph, nodeList) = MakeGraph.instrs2graph(instrs)
         fun tempToStr(temp, currStr) = currStr ^ " " ^ Int.toString(temp)
         fun boolToStr(b) = if b = true then "true" else "false"
         fun printNode(nodeID, (assem, defList, useList, isMove)) = 
         	"NodeID:" ^ Symbol.name nodeID ^ " assem:" ^ String.substring(assem, 0, String.size(assem)-1) ^ " defs:" ^ (foldr tempToStr "" defList)
         	^ " uses:" ^ (foldr tempToStr "" useList) ^ " isMove:" ^ boolToStr(isMove)
      in  
         Flow.Graph.printGraph printNode flowGraph
     end
    | emitprocDataFlow (F.STRING(lab,s)) = ()

   fun printDataFlowGraph filename = 
		let val absyn = Parse.parse filename
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
	        map emitprocDataFlow frags
        end


  fun emitprocIGraph (F.PROC{body,frame}) =
     let 
          (*val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")*)
(*         val _ = Printtree.printtree(out,body); *)
       val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
         val instrs =  List.concat(map (Mips.codegen frame) stms')

         val (flowGraph, nodeList) = MakeGraph.instrs2graph(instrs)
         val iGraph = Liveness.interferenceGraph (flowGraph, nodeList)
      in  

         (*(print("\n hahahah--->" ^ Int.toString(List.length nodeList));*)
         Liveness.show(TextIO.stdOut, iGraph)
     end
    | emitprocIGraph (F.STRING(lab,s)) = ()

   fun printIGraph filename = 
    let val absyn = Parse.parse filename
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
          map emitprocIGraph frags
        end
end
