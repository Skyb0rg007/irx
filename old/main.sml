use "graph.sml";

val g : (string, int) Graph.t = Graph.empty
val (v0, g) = Graph.vertex ("zero", g)
val (v1, g) = Graph.vertex ("one", g)
val (v2, g) = Graph.vertex ("two", g)

val g = Graph.edge (Graph.E { from = v0, to = v1, label = 01 }, g)
val g = Graph.edge (Graph.E { from = v1, to = v2, label = 12 }, g)
val g = Graph.edge (Graph.E { from = v0, to = v0, label = 99 }, g)

fun dot (g : (string, string) Graph.t): string =
    let fun nub xs =
            let fun go ([] : string Graph.edge list, _) = []
                  | go (y::ys, xs) =
                      if Option.isSome (List.find (fn x => Graph.edgeEqual (op =, x, y)) xs)
                      then go (ys, xs)
                      else y :: go (ys, y::xs)
            in  go (xs, [])
            end
        fun go (nodes, edges, g) =
            case Graph.matchAny g of
                 NONE => (nodes, edges)
               | SOME (Graph.C { vertex, succs, preds }, g) =>
                       go (vertex :: nodes, nub (succs @ preds) @ edges, g)
        val (nodes, edges) = go ([], [], g)
        fun showNode (Graph.V { index, label }) = "  v" ^ Int.toString (Graph.indexHash index) ^ " [label=\"" ^ label ^ "\"];\n"
        val nodesS = String.concatWith "" (List.map showNode nodes)
        fun showEdge (Graph.E { from, to, label }) =
            "  v" ^ Int.toString (Graph.indexHash from) ^
            " -> v" ^ Int.toString (Graph.indexHash to) ^ " [label=\"" ^ label ^ "\"];\n"
        val edgesS = String.concatWith "" (List.map showEdge edges)
    in  "digraph G { \n" ^ nodesS ^ "\n" ^ edgesS ^ "\n}"
    end

(* val () = print (Graph.showTree (Graph.mapEdges Int.toString g)) *)
val () = print (dot (Graph.mapEdges Int.toString g) ^ "\n")

