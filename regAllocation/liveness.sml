signature LIVENESS =
sig
  structure G : GRAPH
  datatype igraph = IGRAPH of {graph: G.graph, 
                               tnode: Temp.temp -> G.node,
                               gtemp: G.node -> Temp.temp,
                               moves: (G.node * G.node) list}
  
  val interferenceGraph : Flow.flowgraph * G.node list -> 
                            igraph * (G.node -> Temp.temp list)
  
  val show : TextIO.outstream * igraph -> unit
end