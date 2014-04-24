signature COLOR = 
sig

	type allocation = MipsFrame.register Temp.table 

	(*initial is pre-colored nodes due to calling conventions*)
	(*spillCost evaluates how much it costs to spill a variable, naive approach returns 1 for every temp*)
	(*registers are all the registers available in a machine*)
	(*Temp.temp list in the output is a spill list*)
	val color: {interference: Liveness.igraph,
				initial: allocation,
				spillCost: Liveness.igraphNode IGraph.node -> int,
				registers: MipsFrame.register list}
				-> allocation * Temp.temp list
end

structure Color :> COLOR =
struct
	structure IG = IGraph
	structure Te = Temp
	structure Table = Te.Table
	structure L = Liveness
  	structure Set = Te.Set

	type allocation = MipsFrame.register Te.table 

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

	fun isInList(elem, myList) = List.length(List.filter (fn x => x = elem) myList) > 0

	fun inPairList(n, myList) = List.length(List.filter (fn (n1, n2) => n1=n orelse n2=n ) myList) > 0

	fun color ({interference=L.IGRAPH {graph=interference, moves=moves}, initial=initial, spillCost=spillCost, registers=registers}) =
		let
			fun checkInitialInMoves (node1, node2) = 
				let val color1 = Table.look(initial, node1)
					val color2 = Table.look(initial, node2)
					val node1Colored = case color1 of
						SOME(color) => true
						| NONE => false
					val node2Colored = case color2 of
						SOME(color) => true
						| NONE => false
				in
					not(node1Colored orelse node2Colored)
				end
			val movesMinusInitial = List.filter checkInitialInMoves moves
		    val movePairs = ref movesMinusInitial
		    
		    (*val mergedPairs = ref [] : (IG.nodeID * IG.nodeID) list ref*)
		 	val coalesceSuccess = ref false
		 	val unfreezeSuccess = ref false

		 	fun addAllTemps(nodes, t) =
		 		foldr (fn (n, currT) => Table.enter(currT, n, Set.singleton n)) t nodes

		 	fun getSet(id, table) = case Table.look(table, id) of
		 							SOME(r) => r
		 							| NONE => ErrorMsg.impossible "During mergeTable operations... Set with ID not found..."

		 	val mergeTable = ref (addAllTemps(map IG.getNodeID (IG.nodes(interference)), Table.empty))

		 	fun mergeSets(n1ID, n2ID, table) = 
		 		Table.enter(table, n2ID, Set.union(getSet(n1ID, table), getSet(n2ID, table)))

		    val numRegs = List.length(registers)

		    fun allMoveNodeIDs() = 
		    	let
		    		fun oneMovePair((oneNode, otherNode), currSet) =
		    			Set.add(Set.add(currSet, oneNode), otherNode)
		    	in
		    		foldl oneMovePair Set.empty (!movePairs)
		    	end

		    fun lookForSimpliable(ig, nodeIDList) =
		    	let
		    		fun addNodeToList(currentNodeID, (simplifyWorkList, freezeWorkList, nonSimplifiable, graph)) =
		    			let
		    				val currentNode = IG.getNode(graph, currentNodeID)
		    				val preColored = Table.look(initial, currentNodeID)
		    				val moveNodeSet = allMoveNodeIDs()
		    				(*val test = print("processing " ^ Int.toString currentNodeID ^ "\n");*)
		    			in
		    				case preColored of 
		    					SOME(color) => (simplifyWorkList, freezeWorkList, nonSimplifiable, IG.remove(graph, currentNode))
		    				  | NONE => if Set.member(moveNodeSet, currentNodeID)
		    				  			then (simplifyWorkList, currentNodeID::freezeWorkList, nonSimplifiable, graph)
		    						    else (if IG.outDegree(currentNode)< numRegs
		    						    	  then (currentNodeID::simplifyWorkList, freezeWorkList, nonSimplifiable, graph)
		    						    	  else (simplifyWorkList, freezeWorkList, currentNodeID::nonSimplifiable, graph))
		    			end
		    	in
		    		foldl addNodeToList ([], [], [], ig) nodeIDList
		    	end

		    (* simplify all nodes in the simplifyWorkList, returns the updated stack, lists and graph*)
		    fun simplify(selectStack, ig, simList) = 
		    	let 
		    		fun simplifyOneNode (nodeID, (stack, g)) =
		    			(Stack.push(nodeID, stack), IG.removeNode'(g, nodeID))
		    	in
		    		foldr simplifyOneNode (selectStack, ig) simList
		    	end

		    (*look for possible coalescing and perform it, returns the new graph*)
		    fun coalesceAndReturnNewGraph(ig) = 
		    	let
		    		fun briggs((node1ID, node2ID)::rest, g) = 
		    			let
		    				(*val test = print("0 " ^ Int.toString node1ID ^ "\n");*)
		    				val node1 = IG.getNode(g, node1ID)
		    				(*val test = print("1 " ^ Int.toString node2ID ^ "\n");*)
		    				val node2 = IG.getNode(g, node2ID)
							(*val test = print("2\n")*)

		    				(*num of neighbors with significant degree after merge*)
		    				fun significantDegree(n) = 
		    					let
		    						val neighbors = IG.succs' g n
		    						val succNodes = 
		    							List.filter (fn nn => (not (IG.getNodeID nn = IG.getNodeID node1))
		    											andalso (not (IG.getNodeID nn = IG.getNodeID node2)))
		    										neighbors

		    						fun isSignificant(neighborNode) =
		    							let
		    								val adjToBoth = 
		    									if IG.isAdjacent(neighborNode, node1) andalso IG.isAdjacent(neighborNode, node2)
		    									then 1
		    									else 0
		    							in
		    								if IG.outDegree(neighborNode)-adjToBoth < numRegs
		    								then false
		    								else true
		    							end
		    					in
		    						List.length(List.filter (fn b => b) (map isSignificant succNodes))
		    					end

		    				val totalDegree = significantDegree(node1) + significantDegree(node2)

		    				val updatedIG = if totalDegree < numRegs
						    				then (mergeTable := mergeSets(node1ID, node2ID, !mergeTable);
						    					  coalesceSuccess := true;
						    					  mergeNodes(g, node1, node2))
						    				else g
		    			in
		    				briggs(!movePairs, updatedIG)
		    			end

		    		| briggs([], g) = g

		    		val newIG = briggs((!movePairs), ig)
		    	in
		    		(!coalesceSuccess, newIG)
		    	end

		    (*helper function for Briggs Coalescing. Result: n1 is merged into n2. Only n2 exists afterwards*)
		    and mergeNodes(ig, n1, n2) = 
		    	let
		    		val node1ID	= IG.getNodeID n1
		    		val node2ID = IG.getNodeID n2
		    		val node1Succs = IG.succs(n1)
		    		val node2Succs = IG.succs(n2)
		    		fun addEdge(succID, g) =
		    			if succID = IG.getNodeID n2
		    			then g
		    			else IG.doubleEdge(g, IG.getNodeID n2, succID)
		    		val addedG = foldl addEdge ig node1Succs


					fun noHaveNode1((nn1, nn2)) =
				    			if (nn1 = node1ID orelse nn2 = node1ID)
				    			then false
				    			else true

		    		fun isTrueEdge(n1ID, n2ID, g) = 
		    			(case IG.isAdjacent(IG.getNode (g, n1ID), IG.getNode (g, n2ID)) of
		    				true => (if isInList((n1ID, n2ID), !movePairs)
		    						 then false
		    						 else true)
		    				| false => false)

		    		fun removeConstraintMoves(selfNode, otherNode, moveNeighbors, g) = 
		    			map (fn neighbor => if isTrueEdge(otherNode, neighbor, g)
		    								then (movePairs := removeFromList((selfNode, neighbor), !movePairs);
		    									 movePairs := removeFromList((neighbor, selfNode), !movePairs))
		    								else ()) moveNeighbors

					val node1MoveNeighbors = 
		    			List.filter (fn neighbor => isInList((node1ID, neighbor), !movePairs) orelse isInList((neighbor, node1ID), !movePairs)) node1Succs
		    		val node2MoveNeighbors = 
		    			List.filter (fn neighbor => isInList((node2ID, neighbor), !movePairs) orelse isInList((neighbor, node2ID), !movePairs)) node2Succs

		    		val _ = removeConstraintMoves(node1ID, node2ID, node1MoveNeighbors, ig)
		    		val _ = removeConstraintMoves(node2ID, node1ID, node2MoveNeighbors, ig)
		    		val _ = movePairs := List.filter noHaveNode1 (!movePairs)

		    	in
		    		IG.remove(addedG, n1)
		    	end

		    (*subroutine for unfreeze procedure*)
		    fun bestMoveNodeToFreeze(ig) = 
		    	let
		    		val moveNodeIDs = Set.listItems(allMoveNodeIDs())
		    		fun compareDegree(currNodeID, bestNodeID) = 
		    			if IG.outDegree(IG.getNode(ig, currNodeID)) < IG.outDegree(IG.getNode(ig, bestNodeID))
		    			then currNodeID
		    			else bestNodeID
		    	in
		    		(unfreezeSuccess := List.length(moveNodeIDs) > 0;
		    		foldr compareDegree (hd moveNodeIDs) moveNodeIDs)
		    	end

		    (*turn move edges associated with this node into normal edges, should restart from simplify after this step*)
		    fun unfreezeMove(moveNodeID) =
		    	let 
		    		fun noHaveThisNode((n1, n2)) =
		    			if (n1 = moveNodeID orelse n2 = moveNodeID)
		    			then false
		    			else true
		    	in
		    		if !unfreezeSuccess = true
		    		then (movePairs := List.filter noHaveThisNode (!movePairs); true)
		    		else false
		    	end


		    (*we should use 1/IG.outDegree(node) as our spillCost function, so that we pick the highest degree node to spill*)
		    fun selectBestSpillNode(nodeID, (g, bestSoFarID)) = 
		    		if spillCost(IG.getNode (g,nodeID))<spillCost(IG.getNode (g, bestSoFarID))
		    		then (g, nodeID)
		    		else (g, bestSoFarID)

		    fun selectSpill(ig, selectStack, spillWorkList) =
		    	let 
		    		val head = hd spillWorkList
		    		val (_, spillNodeID) = foldl selectBestSpillNode (ig, head) spillWorkList
		    		val selectStack' = Stack.push(spillNodeID, selectStack)
		    		val ig' = IG.removeNode'(ig, spillNodeID)
		    	in 
		    		(if List.length(spillWorkList) <= 0
		 					 then ErrorMsg.impossible "No more node to select for potential spill...but algorithm doesn't end???"
		 					 else ();
		    		(ig', selectStack'))
		    	end	

		    fun filterColors(nodeID, (alloc, avaiRegs)) =
				let
					val register = Table.look(alloc, nodeID)
				in
					case register of
						SOME(r) => (alloc, removeFromList(r, avaiRegs))
						| NONE => (alloc, avaiRegs)
				end					

			fun findExtraNeighbors(node::rest, graph) = (IG.adj (IG.getNode (graph, node))) @ findExtraNeighbors(rest, graph)
				| findExtraNeighbors([], graph) = []

			fun enterIfNotPresent(t, oneID::rest, color) = 
				(case Table.look(t, oneID) of 
					SOME(c) => enterIfNotPresent(t, rest, color)
					| NONE => enterIfNotPresent(Table.enter(t, oneID, color), rest, color))

				| enterIfNotPresent(t, [], color) = t

			(*this method uses a coulpe global variables: initial, registers*)
		 	fun assignColors(oldIG, selectStack) = 
		 		let 
		 			(*val _ = app (fn e => print("Node on the stack: " ^ Int.toString e ^ "\n")) selectStack*)
			 		fun assignColor(nodeID, alloc) =
			 			let
			 				val okColors = registers
	 						val adjacent = IG.adj (IG.getNode (oldIG, nodeID))
	 						val (_, okColors') = foldl filterColors (alloc, okColors) adjacent
	 						val mergedNodeIDs = Set.listItems (getSet(nodeID, !mergeTable))
	 						val (_, avaiColors) = foldl filterColors (alloc, okColors') (findExtraNeighbors(mergedNodeIDs, oldIG))

	 						val color = case avaiColors of (* Just take the first available color to color our node *)
	 									h::rest => h
	 									| _ => ErrorMsg.impossible "Not enough color... needs actual spilling!"
	 						val addedAlloc = Table.enter(alloc, nodeID, color)
	 						val completeAlloc = enterIfNotPresent(addedAlloc, mergedNodeIDs, color)
		 				in
		 					(if List.length(okColors') <= 0
		 					 then ErrorMsg.impossible "No color assignable for a temp node, actual spill occurs..."
		 					 else ();
		 					 completeAlloc)
		 				end

		 			val newAlloc = foldl assignColor initial selectStack
			 	in 
			 		newAlloc
		 		end

		 	(*main loop*)
		 	fun runRegAlloc(ig, selectStack) = 
		 		let
				    val nodes = IG.nodes(ig) (* List of nodes*)
				    val nodeIDList = map IG.getNodeID nodes (* List of node ids *)
				    val (simplifyWorkList, freezeWorkList, nonSimplifiable, graphNoReserve) = lookForSimpliable(ig, nodeIDList)
				    val graphEmpty = if List.length(freezeWorkList)+List.length(nonSimplifiable) = 0 then true else false
				    val simplifyDidWork = List.length(simplifyWorkList) > 0
				    val (updatedStack, updatedIG) = simplify(selectStack, graphNoReserve, simplifyWorkList)
				in
					case simplifyDidWork of 
						true => runRegAlloc(updatedIG, updatedStack)
						| false =>
							(case graphEmpty of 
							true => ( (*print("I'm done\n");*)
				    				(*Liveness.show(TextIO.stdOut, Liveness.IGRAPH {graph=interference, moves=[]});*)
									assignColors(interference, updatedStack))
							| false => (case coalesceAndReturnNewGraph(updatedIG) of 
										(true, newIG) => (coalesceSuccess := false;
															(*print("Coalesced\n");*)
														  runRegAlloc(newIG, updatedStack))
										| (false, newIG) => (case unfreezeMove(bestMoveNodeToFreeze(newIG)) of 
															 true => (unfreezeSuccess := false;
															 			(*print("Unfreezed\n");*)
															 		  runRegAlloc(newIG, updatedStack))
															 | false => (case selectSpill(newIG, updatedStack, nonSimplifiable) of 
															 			(ig', stack') => runRegAlloc(ig', stack')
															 			)
															 )
										)
							)
				end
		in
			(runRegAlloc(interference, Stack.empty), [])
		end
end
