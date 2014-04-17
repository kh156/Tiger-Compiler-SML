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

	structure Stack = 
	struct
		type 'a stack = 'a list
		val empty = []
		val push = op ::
		fun pop [] = (NONE, [])
		  | pop (tos::rest) = (SOME tos, rest)
	end

	val spilled = ref false

  	structure Set = Te.Set

	fun color ({L.IGRAPH {graph=interference, moves=moves}, initial, spillCost, registers}) =
		let
		    val coloredNodes = ref Set.empty (* The nodes we have colored so far*)
		  	val regColorMap = ref initial : allocation ref
		    val okColors = ref palette

		    val nodes = IG.nodes(interference) (* List of nodes*)
		    fun addNodeId(node,lst) = IG.getNodeID(node)::lst (*nods don't come in any particular order*)
		    val nodeIDList = foldl addNodeId [] nodes (* List of node ids. All node arguments reference this list, so they are id's*)

		    val allMoveNodes = 
		    	let
		    		fun oneMovePair((oneNode, otherNode), currSet) =
		    			Set.add(Set.add(currSet, IG.getNodeID oneNode), IG.getNodeID otherNode)
		    	in
		    		foldl oneMovePair Set.empty moves
		    	end

		    val numRegs = 27 (*should get this information from registers parameter that's passed in. E.g. List.length registers*)

			fun remove (elem, myList) = List.filter (fn x => x <> elem) myList;

			fun moveRelated(nodeID) = Set.member(allMoveNodes, nodeID)

		    fun lookForSimpliable =
		    	let
		    		fun addNodeToList(currentNodeID, (simplifyWorkList, freezeWorkList, nonSimplifiable)) =
		    			let
		    				val currentNode = IG.getNode(currentNodeID)
		    				val preColored = Table.look(initial, currentNodeID) (*Check if node has been precolored*)
		    			in
		    				case preColored of 
		    					SOME(color) => (simplifyWorkList, freezeWorkList) (*Do not color if node has been precolored*)
		    				  | NONE => if moveRelated(currentNodeID) 
		    				  			then (simplifyWorkList, currentNode::freezeWorkList, nonSimplifiable)
		    						    else (if IG.outdegree(currentNode)< numRegs
		    						    	  then (currentNode::simplifyWorkList, freezeWorkList, nonSimplifiable)
		    						    	  else (simplifyWorkList, freezeWorkList, currentNode::nonSimplifiable))
		    			end
		    	in
		    		foldl addNodeToList ([], [], []) nodeIDList
		    	end

		    val (simplifyWorkList, freezeWorkList, nonSimplifiable) = lookForSimpliable()

		    (* simplify all nodes in the simplifyWorkList, returns the updated stack, lists and graph*)
		    fun simplify(selectStack, ig, simList) = 
		    	let 
		    		fun simplifyOneNode (nodeID, (stack, g)) =
		    			(Stack.push(nodeID, stack), IG.removeNode'(g, nodeID))
		    	in
		    		foldr simplifyOneNode (selectStack, ig) simList
		    	end

		    (*need to make this loop until no more changes*)
		    val (updatedStack, updatedInteference) = simplify(Stack.empty, interference, simplifyWorkList)

		    fun coalesceAndReturnNewGraph(ig, movePairs) = 
		    	let
		    		fun briggs((node1ID, node2ID), g) = 
		    			let
		    				val node1 = IG.getNode(g, node1ID)
		    				val node2 = IG.getNode(g, node2ID)
		    				val totalDegree = IG.outDegree(node1) + IG.outDegree(node2) - 2 (*minus the edge between them*)
		    			in
		    				if totalDegree < numRegs
		    				then mergeNodes(g, node1, node2)
		    				else g
		    			end

		    	in
		    		foldr briggs ig movePairs
		    	end

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

		    fun enableMoves(nodes, activeMoves, workListMoves) =
		    	let 
		    		fun enable node (activeMoves, workListMoves) =
		    			let
		    				val moves = nodeMoves(node)
		    				val result = foldl processMove (activeMoves, workListMoves) moves
		    			in
		    				(#1 result, #2 result)
		    			end
		    	in
		    		foldl enable (activeMoves, workListMoves) nodes
		    	end

		    fun processMove(move, (activeMoves, workListMoves)) =
		    	if SET.member(activeMoves, move)
		    	then (SET.add(activeMoves, move), SET.add(workListMoves, move))

		    fun selectBestNode(node, bestSoFar) = 
		    		if (spillCost(node)<spillCost(bestSoFar)) (* Heuristic for selecting best node to spill *)
		    		then node
		    		else bestSoFar

		    fun selectSpill(simplifyWorkList, spillWorkList) =
		    	let 
		    		val head = hd spillWorkList
		    		val spillNode = foldl selectBestNode head spillWorkList 
		    		val spillWorkList' = remove(spillNode, spillWorkList)
		    		val simplifyWorkList' = spillNode::simplifyWorkList
		    		(*i think we remove this spill node from the igraph and restart all over from the beginning*)
		    	in 
		    		(simplifyWorkList', spillWorkList')
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
		in
			body
		end

end