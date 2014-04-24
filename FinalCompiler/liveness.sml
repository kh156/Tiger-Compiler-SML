
structure IGraph = FuncGraph(Temp.TempOrd)

signature LIVENESS =
sig

  type igraphNode = int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (IGraph.nodeID * IGraph.nodeID) list}
  
  val interferenceGraph : Flow.flowgraph * Flow.flowNodeInfo Flow.Graph.node list -> igraph 
  (** do we need the (Flow.flowNodeInfo -> Temp.temp list)? Isn't that intermediate data used to generate the igraph?*)
  
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness: LIVENESS =
struct

  structure IG = IGraph
  structure FG = Flow.Graph
  structure TS = Temp.Set

  (* IG.getNodeID is gtemp. I guess we use int to designate the color assigned to this variable/node ??*)
  type igraphNode = int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (IG.nodeID * IG.nodeID) list}

  val changed = ref false
  val moveList = ref [] : (IGraph.nodeID * IGraph.nodeID) list ref

  fun emptySetTable(orderedNodeList) = 
  	foldr (fn (node, table) => FG.addNode(table, FG.getNodeID(node), TS.empty)) FG.empty orderedNodeList

  fun dealWithOneNode(flowGraphNode, (flowGraph, liveInTable, liveOutTable)) = 
  	let
  		val currNodeID = FG.getNodeID(flowGraphNode)
  		val succsIDs = map FG.getNodeID (FG.succs' flowGraph flowGraphNode)
      (*val _ = print(Symbol.name currNodeID ^ "succs ID list has " ^ Int.toString(List.length succsIDs) ^ " elements\n")*)
  		val predsIDs = map FG.getNodeID (FG.preds' flowGraph flowGraphNode)
      (*val _ = print("preds ID list has " ^ Int.toString(List.length predsIDs) ^ " elements\n")*)

  		val succsInSets = foldr (fn (succID, currList) => currList @ [FG.nodeInfo (FG.getNode(liveInTable, succID))]) [] succsIDs
  		val newOutSet = foldr (fn (oneSet, currSet) => TS.union(oneSet, currSet)) TS.empty succsInSets

  		val (_, defList, useList, isMove) = FG.nodeInfo(flowGraphNode)
  		val newInSet = TS.union(TS.addList(TS.empty, useList), TS.difference(newOutSet, TS.addList(TS.empty, defList)))

  		val oldIn = FG.nodeInfo (FG.getNode(liveInTable, currNodeID))
  		val oldOut = FG.nodeInfo (FG.getNode(liveOutTable, currNodeID))

  	in
  		((if (TS.numItems(oldIn) = TS.numItems(newInSet) andalso TS.numItems(oldOut) = TS.numItems(newOutSet))
  		then ()
  		else changed := true);
  		(flowGraph, FG.addNode(liveInTable, currNodeID, newInSet), FG.addNode(liveOutTable, currNodeID, newOutSet)))
  	end

  fun livenessAnalyze(flowGraph, orderedNodeList, oldIn, oldOut) =
	foldr dealWithOneNode (flowGraph, oldIn, oldOut) orderedNodeList

  fun doUntilNoChange(flowGraph, orderedNodeList, oldIn, oldOut) = 
  	let
  		val (g, inT, outT) = livenessAnalyze(flowGraph, orderedNodeList, oldIn, oldOut)
  	in
	  	if !changed = true
	  	then (changed := false; doUntilNoChange(flowGraph, orderedNodeList, inT, outT))
	  	else (inT, outT)
	end

  fun extractAllTemps(orderedNodeList) = 
  	let
  		fun tempsOneNode(oneNode, currSet) = 
  			let
  				val (_, defList, useList, isMove) = FG.nodeInfo(oneNode)
  			in
  				TS.union(currSet, TS.addList(TS.addList(TS.empty, useList), defList))
  			end
  	in
  		foldr tempsOneNode TS.empty orderedNodeList
  	end

  fun addIntefereEdges(ig, orderedNodeList, liveOutTable) =
  	let 
  		fun doubleEdge(aNode, otherNodes, igLocal) = 
  			foldr (fn (bNode, g) => IG.doubleEdge(g, aNode, bNode)) igLocal otherNodes
  		
  		fun addEdges(aTemp::rest, igLocal) = 
  			addEdges(rest, doubleEdge(aTemp, rest, igLocal))

		  | addEdges([], igLocal) = igLocal

  		fun oneNode(node, igLocal) = 
  			let 
  				val liveOuts' = FG.nodeInfo (FG.getNode(liveOutTable, FG.getNodeID(node)))
          val (_, defList, useList, isMove) = FG.nodeInfo node
          val liveOuts = TS.union(liveOuts', TS.addList(TS.empty, defList))

  			in
          (if isMove
           then moveList := (hd defList, hd useList)::(!moveList)
           else ();
  				addEdges(TS.listItems liveOuts, igLocal))
  			end
  	in
  		foldr oneNode ig orderedNodeList
  	end

  (* backwards union analysis method: Liveness analysis *)
  fun interferenceGraph(flowGraph: Flow.flowgraph, orderedNodeList: Flow.flowNodeInfo Flow.Graph.node list) = 
	let
    val resetMoves = (moveList := [])
    (*val _ = print("node list has " ^ Int.toString(List.length orderedNodeList) ^ " elements\n")*)
		val (liveInTable, liveOutTable) = 
			doUntilNoChange(flowGraph, orderedNodeList, emptySetTable(orderedNodeList), emptySetTable(orderedNodeList))
		val allTemps = TS.listItems (extractAllTemps(orderedNodeList))
		val defaultNodeColor = 0

    (*debugging*)
    (*fun lookSet temp = FG.nodeInfo (FG.getNode (liveOutTable, temp))
    val sets = map lookSet (map FG.getNodeID orderedNodeList)
    fun printSet set = print("a set has " ^ Int.toString(TS.numItems set) ^ " elements\n")
    val _ = map printSet sets*)
    (*debugging*)

		val iiGraph = foldr (fn (oneTemp, g) => IG.addNode(g, oneTemp, defaultNodeColor)) IG.empty allTemps
		val resultG = addIntefereEdges(iiGraph, orderedNodeList, liveOutTable)
	in
		IGRAPH {graph = resultG, moves = !moveList}
	end

  fun show(outStream, IGRAPH {graph=igraph, moves=moves}) = 
  	let
  		fun say s =  TextIO.output(outStream,s)

		fun printNode(nodeID, color) = 
         	" NodeID: " ^ (Int.toString nodeID) ^ " Color: " ^ (Int.toString color)

	in
		IG.printGraph printNode igraph
	end

end
