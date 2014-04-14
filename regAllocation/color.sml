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
	val spilled = ref false
  	structure Set = ListSetFn(struct
                              type ord_key = Temp.temp
                              val compare  = Int.compare
                              end)

	fun color {interference, initial, spillCost, registers} =
		let
			val nodes = FG.nodes()
			val Liveness.IGRAPH {graph, tnode, gtemp, moves} = interference
		    val nodeSet = Set.addList(Set.empty, (map gtemp (nodes)))
			val spillSet = Set.addList(Set.empty, (map gtemp (nodes)))
		    val palette = Set.addList(Set.empty, F.colorable)
		    val colorTable = ref Temp.Table.empty (* map nodes to colors *)
		    val coloredNodes = ref Set.empty (* The nodes we have colored so far*)
		  	val regColorMap = ref initial : allocation ref
		    val validColors = ref palette

		    fun getDegree node = FG.degree node

		 	fun colorNode node = 
		 		let 
		 			val neighbors = FG.adj node

		 		in

		 		end




		in
			body
		end


	end






end