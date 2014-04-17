structure InterferenceGraph = FuncGraph(Temp.TempOrd)

signature COLOR = 
sig
	structure Frame : FRAME

	type allocation = Frame.register Temp.table 

	(*initial is pre-colored nodes due to calling conventions*)
	(*spillCost evaluates how much it costs to spill a variable, naive approach returns 1 for every temp*)
	(*registers are all the registers available in a machine*)
	(*Temp.temp list in the output is a spill list*)
	val color: {interference: Liveness.igraph,
				initial: allocation,
				spillCost: Liveness.igraphNode InterferenceGraph.node -> int,
				registers: Frame.register list}
				-> allocation * Temp.temp list
				}
end

structure Color :> COLOR =
struct
	structure IG = InterferenceGraph
	structure F = MipsFrame
	structure Te = Temp
	structure Table = Te.Table
	structure L = Liveness
  	structure Set = Te.Set

	structure Stack = 
	struct
		type 'a stack = 'a list
		val empty = []
		val push = op ::
		fun pop [] = (NONE, [])
		  | pop (tos::rest) = (SOME tos, rest)
	end

	val spilled = ref false


	fun removeFromList (elem, myList) = List.filter (fn x => x <> elem) myList;

	fun color ({L.IGRAPH {graph=interference, moves=moves}, initial, spillCost, registers}) =
		let
		    val coloredNodes = ref Set.empty (* The nodes we have colored so far*)
		  	val regColorMap = ref initial : allocation ref
		    val okColors = ref palette
		    val movePairs = ref moves

		    val numRegs = 27 (*should get this information from registers parameter that's passed in. E.g. List.length registers*)

		    val nodes = IG.nodes(interference) (* List of nodes*)
		    fun addNodeId(node,lst) = IG.getNodeID(node)::lst (*nods don't come in any particular order*)
		    val nodeIDList = foldl addNodeId [] nodes (* List of node ids. All node arguments reference this list, so they are id's*)


		    fun allMoveNodeIDs() = 
		    	let
		    		fun oneMovePair((oneNode, otherNode), currSet) =
		    			Set.add(Set.add(currSet, IG.getNodeID oneNode), IG.getNodeID otherNode)
		    	in
		    		foldl oneMovePair Set.empty !movePairs
		    	end

		    fun lookForSimpliable =
		    	let
		    		fun addNodeToList(currentNodeID, (simplifyWorkList, freezeWorkList, nonSimplifiable)) =
		    			let
		    				val currentNode = IG.getNode(currentNodeID)
		    				val preColored = Table.look(initial, currentNodeID)
		    				val moveNodeSet = allMoveNodeIDs()
		    			in
		    				case preColored of 
		    					SOME(color) => (simplifyWorkList, freezeWorkList, nonSimplifiable)
		    				  | NONE => if Set.member(moveNodeSet, currentNodeID)
		    				  			then (simplifyWorkList, currentNode::freezeWorkList, nonSimplifiable)
		    						    else (if IG.outdegree(currentNode)< numRegs
		    						    	  then (currentNode::simplifyWorkList, freezeWorkList, nonSimplifiable)
		    						    	  else (simplifyWorkList, freezeWorkList, currentNode::nonSimplifiable))
		    			end
		    	in
		    		foldl addNodeToList ([], [], []) nodeIDList
		    	end

		    (* simplify all nodes in the simplifyWorkList, returns the updated stack, lists and graph*)
		    fun simplify(selectStack, ig, simList) = 
		    	let 
		    		fun simplifyOneNode (nodeID, (stack, g)) =
		    			(Stack.push(nodeID, stack), IG.removeNode'(g, nodeID))
		    	in
		    		foldr simplifyOneNode (selectStack, ig) simList
		    	end

		    (*need to make this loop until no more changes*)
		    val (simplifyWorkList, freezeWorkList, nonSimplifiable) = lookForSimpliable()
		    val (updatedStack, updatedInteference) = simplify(Stack.empty, interference, simplifyWorkList)

		    (*look for possible coalescing and perform it, returns the new graph*)
		    fun coalesceAndReturnNewGraph(ig, movePairs) = 
		    	let
		    		fun briggs((node1ID, node2ID), g) = 
		    			let
		    				val node1 = IG.getNode(g, node1ID)
		    				val node2 = IG.getNode(g, node2ID)

		    				(*num of neighbors with significant degree after merge*)
		    				fun significantDegree(n) = 
		    					let
		    						val neighbors = IG.succs'(g, n)
		    						val succNodes = 
		    							List.filter (fn nn => (not (IG.getNodeID nn == IG.getNodeID node1))
		    											andalso (not (IG.getNodeID nn == IG.getNodeID node2)))
		    										neighbors

		    						fun isSignificant(neighborNode) =
		    							let
		    								val adjToBoth = 
		    									if IG.isAdjacent(neighborNode, node1) andalso IG.isAdjacent(neighborNode, node2)
		    									then 1
		    									else 0
		    							in
		    								IG.outDegree(neighborNode) - adjToBoth
		    							end
		    					in
		    						List.length(List.filter (fn b => b) (map significantDegree succNodes))
		    					end

		    				val totalDegree = significantDegree(node1) + significantDegree(node2)
		    			in
		    				if totalDegree < numRegs
		    				then (movePairs := removeFromList((node1ID, node2ID), !movePairs);
		    					  mergeNodes(g, node1, node2))
		    				else g
		    			end

		    	in
		    		foldr briggs ig movePairs
		    	end

		    (*helper function for Briggs Coalescing*)
		    fun mergeNodes(ig, n1, n2) = 
		    	let
		    		val node1Succs = IG.succs(n1)
		    		fun addEdge(succID, g) =
		    			if succID == IG.getNodeID n2
		    			then g
		    			else IG.doubleEdge(g, IG.getNodeID n2, succID)
		    		val addedG = foldl addEdge ig node1Succs
		    	in
		    		IG.remove(addedG, n1)
		    	end

		    (*subroutine for unfreeze procedure*)
		    fun bestMoveNodeToFreeze(ig) = 
		    	let
		    		val moveNodeIDs = allMoveNodeIDs()
		    		fun compareDegree(currNodeID, bestNodeID) = 
		    			if IG.outDegree(IG.getNode(ig, currNodeID)) < IG.outDegree(IG.getNode(ig, bestNodeID))
		    			then currNodeID
		    			else bestNodeID
		    	in
		    		foldr compareDegree (hd moveNodeIDs) moveNodeIDs
		    	end

		    (*turn move edges associated with this node into normal edges, should restart from simplify after this step*)
		    fun unfreezeMove(moveNodeID) =
		    	let 
		    		fun noHaveThisNode((n1, n2)) =
		    			if (IG.getNodeID n1 == moveNodeID orelse IG.getNodeID n2 == moveNodeID)
		    			then true
		    			else false
		    	in
		    		movePairs := List.filter noHaveThisNode !movePairs 
		    	end


		    (*we should use 1/IG.outDegree(node) as our spillCost function, so that we pick the highest degree node to spill*)
		    fun selectBestSpillNode(nodeID, (g, bestSoFarID)) = 
		    		if spillCost(IG.getNode nodeID)<spillCost(IG.getNode bestSoFarID)
		    		then (g, node)
		    		else (g, bestSoFar)

		    fun selectSpill(ig, simplifyWorkList, spillWorkList) =
		    	let 
		    		val head = hd spillWorkList
		    		val spillNodeID = foldl selectBestSpillNode (ig, head) spillWorkList 
		    		val spillWorkList' = removeFromList(spillNodeID, spillWorkList)
		    		val simplifyWorkList' = spillNodeID::simplifyWorkList
		    		(*i think we remove this spill node from the igraph and restart all over from simplify*)
		    		val ig' = IG.removeNode'(ig, spillNodeID)
		    	in 
		    		(ig', simplifyWorkList', spillWorkList')
		    	end	

		    fun filterColors(node, initial) =
				let
					val color = Table.look(initial, IG.getNodeID(node))
				in
					remove(color, initial)
				end					

		 	fun assignColors(selectStack, okColors) = 
		 		let 
			 		fun assignColor(node, (initial, coloredNodes)) =
			 			let
			 				val okColors = List.tabulate(numRegs, fn x => x); (* Generates list [0,1,2,..numRegs]*)
	 						val adjacent = IG.adj node
	 						val okColors' = foldl filterColors okColors adjacent
	 						val color = hd okColors' (* Just take the first available color to color our node *)
	 						val coloredNodes' = node::coloredNodes
	 						val colorTable = Table.enter(initial, node, color)
		 				in
		 					(colorTable, coloredNodes')
		 				end
			 	in 
			 		foldl assignColor (initial, []) selectStack
		 		end

		 	fun runRegAlloc()
		in
			body
		end

end