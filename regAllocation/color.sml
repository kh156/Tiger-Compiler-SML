structure IGraph = FuncGraph(Temp.TempOrd)

signature COLOR = 
sig
	structure Frame : Frame

	type allocation = Frame.register Temp.Table.table 

	val color: {interference: Liveness.igraph,
	initial: allocation,
	spillCost: Graph.node -> int,
	registers: Frame.register list}
	-> allocation * Temp.temp list
	}
end

structure Color :> COLOR =
struct
	structure IG = IGraph
	structure F = FRAME
	structure T = Temp
	structure Table = T.Table

	structure Stack = 
	struct
		type 'a stack = 'a list
		val empty = []
		val push = op ::
		fun pop [] = NONE
		  | pop (tos::rest) = SOME tos
	end

	val spilled = ref false
  	structure Set = ListSetFn(struct
                              type ord_key = Temp.temp
                              val compare  = Int.compare
                              end)

	fun color ({interference, initial, spillCost, registers}) =
		let
		    val coloredNodes = ref Set.empty (* The nodes we have colored so far*)
		  	val regColorMap = ref initial : allocation ref
		    val validColors = ref palette
		    val nodeList = IG.nodes(interference)
		    val numRegs = 27

		    fun getDegree node = IG.degree(node)

		    fun remove (x, []) = [] (* Removes an item from a list *)
			  | remove (x, (y::ys)) = 
			      if x = y then remove (x, ys) 
				  else (y:: remove (x, ys));

		    fun makeWorkList =
		    	let
		    		fun addNodeToList(node, (simplifyWorkList, spillWorkList)) =
		    			let
		    				val currentNode = node
		    				val preColored = Table.look(initial,gtemp(node)) (*Check if node has been precolored*)
		    			in
		    				case preColored of 
		    					SOME(color) => (simplifyWorkList, spillWorkList) (*Do not color if node has been precolored*)
		    				  | NONE => if IG.degree(currentNode) > numRegs then (simplifyWorkList, currentNode::spillWorkList)
		    						   else (currentNode::simplifyWorkList, spillWorkList)
		    			end
		    	in
		    		foldl addNodeToList ([],[]) nodeList
		    	end

		    val lists = makeWorkList()
		    val simplifyWorkList = #1 lists 
		    val spillWorkList = #2 lists 

		    fun simplify(node, selectStack, simplifyWorkList, spillWorkList, interference) = 
		    	let 
		    		val adjacent = IG.adj(node)
		    		val selectStack' = Stack.push(node, selectStack)
		    		val simplifyWorkList' = remove(node, simplifyWorkList)
		    		val interference' = IG.remove(interference, node) (* Remove the node from the graph*)
		    		val updateNeighbors = foldl DecrementDegree (simplifyWorkList', spillWorkList) adjacent
		    	in
		    		(selectStack, #1 updateNeighbors, #2 updateNeighbors, interference')
		    	end


		    fun decrementDegree(node, (simplifyWorkList, spillWorkList)) =
		    	let
		    		val d = IG.degree(node)
		    		(* No need to decrement node degree *)
		    	in
		    		if d = numRegs 
		    		then (node::simplifyWorkList, remove(node, spillWorkList)) (*Move node from spill list to simplify list*)
		    	end

		    fun selectBestNode(node, alt) = if (spillCost(node)>spillCost(alt)) (* Heuristic for selecting best node to spill *)
		    								then n 
		    								else alt

		    fun selectSpill(simplifyWorkList, spillWorkList) =
		    	let 
		    		val head = hd spillWorkList
		    		val spillNode = foldl selectBestNode head spillWorkList 
		    		val spillWorkList' = remove(spillNode, spillWorkList)
		    		val simplifyWorkList' = spillNode::simplifyWorkList 
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






end