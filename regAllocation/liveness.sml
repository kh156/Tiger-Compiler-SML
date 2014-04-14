
structure IGraph = FuncGraph(Temp.TempOrd)

signature LIVENESS =
sig

  type igraphNode = int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (igraphNode * igraphNode) list}
  
  val interferenceGraph : Flow.flowgraph * Flow.flowNodeInfo Flow.Graph.node list -> igraph 
  (** do we need the (Flow.flowNodeInfo -> Temp.temp list)? Isn't that intermediate data used to generate the igraph?*)
  
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness: LIVENESS =
struct

  structure IG = IGraph
  structure FG = Flow.Graph
  structure TS = Temp.Set

  (* FG.getNodeID is gtemp. I guess we use int to designate the color assigned to this variable/node ??*)
  type igraphNode = int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (igraphNode * igraphNode) list}

  val changed = ref false

  fun emptySetTable(orderedNodeList) = 
  	foldr (fn (node, table) => FG.addNode(table, FG.getNodeID(node), TS.empty)) FG.empty orderedNodeList

  fun dealWithOneNode(flowGraphNode, (liveInTable, liveOutTable)) = 
  	let
  		val currNodeID = FG.getNodeID(flowGraphNode)
  		val succsIDs = FG.succs(flowGraphNode)
  		val predsIDs = FG.preds(flowGraphNode)

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
  		(FG.addNode(liveInTable, currNodeID, newInSet), FG.addNode(liveOutTable, currNodeID, newOutSet)))
  	end

  fun livenessAnalyze(orderedNodeList) =
	foldr dealWithOneNode (emptySetTable(orderedNodeList), emptySetTable(orderedNodeList)) orderedNodeList

  fun doUntilNoChange(orderedNodeList) = 
  	let
  		val tables = livenessAnalyze(orderedNodeList)
  	in
	  	if !changed = true
	  	then (changed := false; doUntilNoChange(orderedNodeList))
	  	else tables
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
  				val liveOuts = TS.listItems (FG.nodeInfo (FG.getNode(liveOutTable, FG.getNodeID(node))))
  			in
  				addEdges(liveOuts, igLocal)
  			end
  	in
  		foldr oneNode ig orderedNodeList
  	end

  (* backwards union analysis method: Liveness analysis *)
  fun interferenceGraph(flowGraph: Flow.flowgraph, orderedNodeList: Flow.flowNodeInfo Flow.Graph.node list) = 
	let
		val (liveInTable, liveOutTable) = doUntilNoChange(orderedNodeList)
		val allTemps = TS.listItems (extractAllTemps(orderedNodeList))
		val defaultNodeColor = 0
		val iiGraph = foldr (fn (oneTemp, g) => IG.addNode(g, oneTemp, defaultNodeColor)) IG.empty allTemps
		val resultG = addIntefereEdges(iiGraph, orderedNodeList, liveOutTable)
	in
		IGRAPH {graph = resultG, moves = []}
	end

  fun show(outStream, IGRAPH {graph=igraph, moves=moves}) = 
  	let
  		fun say s =  TextIO.output(outStream,s)

		fun printNode(nodeID, color) = 
         	"NodeID: " ^ (Int.toString nodeID) ^ " Color: " ^ (Int.toString color) ^ "\n"

	in
		IG.printGraph printNode igraph
	end

end
