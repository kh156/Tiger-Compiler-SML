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
		fun pop [] = NONE
		  | pop (tos::rest) = SOME tos
	end

	val spilled = ref false

  	structure Set = Temp.Set

	fun color ({L.IGRAPH {graph=interference, moves=moves}, initial, spillCost, registers}) =
		let
		    val coloredNodes = ref Set.empty (* The nodes we have colored so far*)
		  	val regColorMap = ref initial : allocation ref
		    val validColors = ref palette
		    val nodeList = IG.nodes(interference)
		    val numRegs = 27 (*should get this information from registers parameter that's passed in*)

		    fun getDegree node = IG.outDegree(node)

		    fun remove (x, []) = [] (* Removes an item from a list *)
			  | remove (x, (y::ys)) = 
			      if x = y then remove (x, ys) 
				  else (y:: remove (x, ys));

		    fun makeWorkList =
		    	let
		    		fun addNodeToList(currentNode, (simplifyWorkList, spillWorkList)) =
		    			let
		    				val preColored = Table.look(initial, IG.getNodeID(currentNode)) (*Check if node has been precolored*)
		    			in
		    				case preColored of 
		    					SOME(color) => (simplifyWorkList, spillWorkList) (*Do not color if node has been precolored*)
		    				  | NONE => if getDegree(currentNode) > numRegs then (simplifyWorkList, currentNode::spillWorkList)
		    						   else (currentNode::simplifyWorkList, spillWorkList)
		    			end
		    	in
		    		foldl addNodeToList ([],[]) nodeList
		    	end

		    val (simplifyWorkList, spillWorkList) = makeWorkList()


		    fun simplify(node, selectStack, simplifyWorkList, spillWorkList, interference) = 
		    	let 
		    		val adjacentNodes = IG.adj(interference', node) (*i think these nodes will retain the original degree...hmm*)
		    		val selectStack' = Stack.push(node, selectStack)
		    		val simplifyWorkList' = remove(node, simplifyWorkList)
		    		val interference' = IG.remove(interference, node) (* Remove the node from the graph*)
		    		val updateNeighbors = foldl decrementDegree (simplifyWorkList', spillWorkList) adjacentNodes
		    	in
		    		(selectStack, #1 updateNeighbors, #2 updateNeighbors, interference')
		    	end


		    fun decrementDegree(node, (simplifyWorkList, spillWorkList)) =
		    	let
		    		val d = IG.degree(node)
		    		(* No need to decrement node degree *)
		    	in
		    		if d < numRegs 
		    		then (node::simplifyWorkList, remove(node, spillWorkList)) (*Move node from spill list to simplify list*)
		    		else (simplifyWorkList, spillWorkList)
		    	end

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

		 	fun assignColors node = 
		 		let 
		 			val adjacent = IG.adj node

		 		in

		 		end


		in
			body
		end

end