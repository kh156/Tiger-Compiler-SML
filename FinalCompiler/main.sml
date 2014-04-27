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

  fun printRunTimeFiles (out) = 
    let
      val runTime = TextIO.openIn "runtimele.s"
      val sysSpim = TextIO.openIn "sysspim.s"
      fun process(inStream) = (case TextIO.inputLine inStream of
                              SOME(l) => (TextIO.output(out, l); process(inStream))
                              | NONE => ())
    in
      (TextIO.output(out, "\n");
      process(runTime);
      process(sysSpim))
    end

   fun getsome (SOME x) = x
    | getsome (_) = ErrorMsg.impossible "Error during getSome in MainGiven..."

   val firstStr = ref true

   fun process(body, frame) = 
      let
         (*val _ = Printtree.printtree(TextIO.stdOut,body); *)
         val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
         val instrsPre =   List.concat(map (Mips.codegen frame) stms')

         val {body=instrs, prolog=_, epilog=_} = F.procEntryExit3(instrsPre)

         val (flowGraph, nodeList) = MakeGraph.instrs2graph(instrs)
         val iGraph = Liveness.interferenceGraph (flowGraph, nodeList)
         val (regTable, noSpill, spillTempList) = Color.color({interference=iGraph, initial=F.tempMap, 
            spillCost=(fn n=> 1 div IGraph.outDegree(n)), registers=F.colorableRegs})

         val accessList = if (not noSpill)
                          then foldl (fn (e, l) => l @ [F.allocLocal frame true]) [] spillTempList
                          else []
         val memExpList = foldl (fn (access, l) => l @ [F.exp access (Tree.TEMP F.FP)]) [] accessList
       in
          if noSpill
          then (instrs, regTable)
          else (process(Translate.substituteSpillTemp (body, map Tree.TEMP spillTempList, memExpList), frame))
       end

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")

         val (instrs, regTable) = process(body, frame)

         val _ = F.setTempMap regTable
         
         val format0 = Assem.format(F.getTempName)
         
         (*val _ = Liveness.show(TextIO.stdOut, iGraph)*)
      in  
         app (fn i => TextIO.output(out,format0 i)) instrs
     end
    | emitproc out (F.STRING(lab,s)) = (if (!firstStr) then (TextIO.output(out, "\n.data\n.align 4\n"); firstStr := false) else ();
                                       TextIO.output(out, F.stringFrag(lab, s)))

   fun withOpenFile fname f = 
       let 
          val out = TextIO.openOut fname
       in (f out before TextIO.closeOut out) 
	        handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val absyn = Parse.parse filename
           val _ = firstStr := true
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
            withOpenFile (filename ^ ".s") 
	     (fn out => (printRunTimeFiles out; (app (emitproc out) frags)))
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
