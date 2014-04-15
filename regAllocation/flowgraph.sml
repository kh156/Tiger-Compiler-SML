structure Flow =
struct

    structure Graph = FuncGraph(Symbol.OrdKey)

    (*using Drew's funcgraph, all the information is stored in the a' node of the a' graph*)
    type flowNodeInfo = string * Temp.temp list * Temp.temp list * bool (*assemStr * def list * use list * ismove*)
    type flowgraph = flowNodeInfo Graph.graph

end

structure MakeGraph:
sig
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.flowNodeInfo Flow.Graph.node list
end
=
struct

  structure Fl = Flow
  structure G = Flow.Graph
  structure A = Assem
  structure Te = Temp

  fun getFirstEle(a::rest) = a
    | getFirstEle([]) = ErrorMsg.impossible "List of orderedLabels is shorter than the list of instructions... Go debug that..."

  fun getRest(a::rest) = rest
    | getRest([]) = ErrorMsg.impossible "List of orderedLabels is shorter than the list of instructions... Go debug that..."

  fun getNextEle(a::b::rest) = SOME(b)
    | getNextEle(_) = NONE

  fun instrs2graph(instrList) = 
    let

      val orderedLabels = ref [] : Te.label list ref

      fun dealWithOneInstr(A.OPER {assem=assem, dst=dst, src=src, jump=jump}, g) = 
        let
          val nodeInfo = (assem, dst, src, false)
          val l = Te.newlabel()
          val (newG, newNode) = G.addNode'(g, l, nodeInfo)
        in
          orderedLabels := !orderedLabels @ [l];
          newG
        end

      | dealWithOneInstr(A.LABEL {assem=assem, lab=lab}, g) = 
        let
          val nodeInfo = (assem, [], [], false)
          val (newG, newNode) = G.addNode'(g, lab, nodeInfo)
        in
          orderedLabels := !orderedLabels @ [lab]; 
          newG
        end

      | dealWithOneInstr(A.MOVE {assem=assem, dst=dst, src=src}, g) = 
        let
          val nodeInfo = (assem, [dst], [src], true)
          val l = Te.newlabel()
          val (newG, newNode) = G.addNode'(g, l, nodeInfo)
        in
          orderedLabels := !orderedLabels @ [l];
          newG
        end

      fun addEdges(A.OPER {assem=_, dst=_, src=_, jump=jump}, (g, labelList)) =
        let
          fun addOneEdge(label, g) = G.addEdge(g, {from = getFirstEle(labelList), to = label})

          val edgeOption = case getNextEle(labelList) of
                         SOME(e) => SOME({from = getFirstEle(labelList), to = e})
                        | NONE => NONE

          val gWithJumps = case jump of 
                         SOME(jumpList) => foldr addOneEdge g jumpList
                        | NONE => g
        in
          case edgeOption of 
            SOME(edge) => (print("edge from "^Symbol.name(#from edge)^" to "^Symbol.name(#to edge));
                          (G.addEdge(gWithJumps, edge), getRest(labelList)))
            | NONE => (gWithJumps, getRest(labelList))
        end

      | addEdges(A.LABEL _, (g, labelList)) = 
        let
          val edgeOption = case getNextEle(labelList) of
                       SOME(e) => SOME({from = getFirstEle(labelList), to = e})
                      | NONE => NONE
        in
          case edgeOption of 
            SOME(edge) => (print("edge from "^Symbol.name(#from edge)^" to "^Symbol.name(#to edge));
                          (G.addEdge(g, edge), getRest(labelList)))
            | NONE => (g, getRest(labelList))
        end

      | addEdges(A.MOVE _, (g, labelList)) =
        let
          val edgeOption = case getNextEle(labelList) of
                       SOME(e) => SOME({from = getFirstEle(labelList), to = e})
                      | NONE => NONE
        in
          case edgeOption of 
            SOME(edge) => (print("edge from "^Symbol.name(#from edge)^" to "^Symbol.name(#to edge));
                          (G.addEdge(g, edge), getRest(labelList)))
            | NONE => (g, getRest(labelList))
        end

      val gNoEdge = foldl dealWithOneInstr G.empty instrList
      val (g, l) = foldl addEdges (gNoEdge, !orderedLabels) instrList
      val nodes = map (fn (l) => G.getNode(g, l)) (!orderedLabels)
    in
      (g, nodes)
    end
end