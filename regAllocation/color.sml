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
	structure G = Liveness.G
	structure FG = FUNCGRAPH
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

	fun color {interference, initial, spillCost, registers} =
		let	
			val Liveness.IGRAPH {graph, tnode, gtemp, moves} = interference
(*		    val nodeSet = Set.addList(Set.empty, (map gtemp (nodes)))
			val spillSet = Set.addList(Set.empty, (map gtemp (nodes)))*)
		    val palette = Set.addList(Set.empty, F.colorable)
		    val coloredNodes = ref Set.empty (* The nodes we have colored so far*)
		  	val regColorMap = ref initial : allocation ref
		    val validColors = ref palette
		    val nodeList = FG.nodes()
		    val numRegs = 27

		    fun getDegree node = FG.degree node

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
		    				  | NONE => if FG.degree(currentNode) > numRegs then (simplifyWorkList, currentNode::spillWorkList)
		    						   else (currentNode::simplifyWorkList, spillWorkList)
		    			end
		    	in
		    		foldl addNodeToList ([],[]) nodeList
		    	end

		    val lists = makeWorkList()
		    val simplifyWorkList = #1 lists 
		    val spillWorkList = #2 lists 

		    fun simplify(node, selectStack, simplifyWorkList, spillWorkList) = 
		    	let 
		    		val adjacent = FG.adj(node)
		    		val selectStack' = Stack.push(node, selectStack)
		    		val simplifyWorkList' = remove(node, simplifyWorkList)
		    		val updateNeighbors = foldl DecrementDegree (simplifyWorkList', spillWorkList) adjacent
		    	in
		    		(selectStack, #1 updateNeighbors, #2 updateNeighbors)
		    	end


		    fun decrementDegree(node, (simplifyWorkList, spillWorkList)) =
		    	let
		    		val d = FG.degree(node)
		    		(* Somehow update the degree of the node to degree-1. *)
		    	in
		    		if d = numRegs 
		    		then (node::simplifyWorkList, remove(node, spillWorkList)) (*Move node from spill list to simplify list*)
		    	end

		 	fun assignColors node = 
		 		let 
		 			val adjacent = FG.adj node

		 		in

		 		end




		in
			body
		end


	end






end