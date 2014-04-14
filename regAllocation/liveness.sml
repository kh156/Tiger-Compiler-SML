
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
  structure TT = Temp.Table

  (* FG.getNodeID is gtemp. I guess we use int to designate the color assigned to this variable/node ??*)
  type igraphNode = int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (igraphNode * igraphNode) list}

  val changed = ref false

  fun emptySetTable(orderedNodeList) = 
  	foldr (fn (node, table) => TT.enter(table, FG.getNodeID(node), TS.empty)) TT.empty orderedNodeList

  fun dealWithOneNode(flowGraphNode, (liveInTable, liveOutTable)) = 
  	let
  		val currNodeID = FG.getNodeID(flowGraphNode)
  		val succsIDs = FG.succs(flowGraphNode)
  		val predsIDs = FG.preds(flowGraphNode)

  		val succsInSets = foldr (fn (succID, currList) => currList @ [TT.look(liveInTable, succID)]) [] succsIDs
  		val newOutSet = foldr (fn (oneSet, currSet) => TS.union(oneSet, currSet)) TS.empty succsInSets

  		val (_, defList, useList, isMove) = FG.nodeInfo(flowGraphNode)
  		val newInSet = TS.union(TS.addList(TS.empty, useList), TS.difference(newOutSet, TS.addList(TS.empty, defList)))

  		val oldIn = TT.look(liveInTable, currNodeID)
  		val oldOut = TT.look(liveOutTable, currNodeID)

  	in
  		((if (TS.numItems(oldIn) = TS.numItems(newInSet) andalso TS.numItems(oldOut) = TS.numItems(newOutSet))
  		then ()
  		else changed := true);
  		(TT.enter(liveInTable, currNodeID, newInSet), TT.enter(liveOutTable, currNodeID, newOutSet)))
  	end

  fun doUntilNoChange(orderedNodeList) = 
  	let
  		val tables = livenessAnalyze(orderedNodeList)
  	in
	  	if !changed = true
	  	then (changed := false; doUntilNoChange(orderedNodeList))
	  	else tables
	end

  fun livenessAnalyze(orderedNodeList) =
	foldr dealWithOneNode (emptySetTable(orderedNodeList), emptySetTable(orderedNodeList)) orderedNodeList

  fun extractAllTemps(orderedNodeList) = 
  	let
  		fun tempsOneNode(oneNode, currSet) = 
  			let
  				val (_, defList, useList, isMove) = FG.nodeInfo(oneNode)
  			in
  				TS.union(currSet, TS.addList(TS.addList(TS.empty, TS.useList), defList))
  			end
  	in
  		foldr tempsOneNode TS.empty orderedNodeList
  	end

  fun addIntefereEdges(ig, orderedNodeList, liveOutTable) =
  	let 
  		fun addEdges(aTemp::rest, igLocal) = 
  			
		| addEdges([], igLocal) = igLocal
		
  		fun oneNode(node, igLocal) = 
  			let 
  				val liveOuts = TS.listItems TT.look(liveOutTable, getNodeID(node))
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
		val allTemps = TS.listItems extractAllTemps(orderedNodeList)
		val defaultNodeColor = 0
		val iiGraph = foldr (fn (oneTemp, g) => IG.addNode(g, oneTemp, defaultNodeColor)) IG.empty allTemps
		val resultG = addIntefereEdges(iiGraph, orderedNodeList, liveOutTable)
	in
		IGRAPH {graph = resultG, moves = []}
	end

  fun show(outStream, iGraph) = 
  	()


end