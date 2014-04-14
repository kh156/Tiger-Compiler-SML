
structure IGraph = FuncGraph(Temp.TempOrd)

signature LIVENESS =
sig

  (*First Temp.temp is gtemp. tnode is not needed...*)
  (*I guess use int to designate the color assigned to this variable/node ??*)
  type igraphNode = Temp.temp * int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (igraphNode * igraphNode) list}
  
  val interferenceGraph : Flow.flowgraph -> igraph 
  (** do we need the (Flow.flowNodeInfo -> Temp.temp list)? Isn't that intermediate data used to generate the igraph?*)
  
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness: LIVENESS =
struct

  structure IG = IGraph
  (*First Temp.temp is gtemp. I guess use int to designate the color assigned to this variable/node ??*)
  type igraphNode = Temp.temp * int 

  datatype igraph = IGRAPH of {graph: igraphNode IGraph.graph,
                               moves: (igraphNode * igraphNode) list}

  fun interferenceGraph(flowGraph: Flow.flowgraph) = 
	let
		val name = ()
	in
		IGRAPH {graph = IG.empty, moves = []}
	end

  fun show(outStream, iGraph) = 
  	()


end