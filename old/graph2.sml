use "intmap.sml";

signature GRAPH =
sig
    type ('a, 'b) t

    structure Index : sig
        type t
        val equal : t * t -> bool
        val compare : t * t -> order
        val hash : t -> word
    end

    (* A node's context: predecessors, label, successors *)
    type ('a, 'b) context = (Index.t * 'b) list * 'a * (Index.t * 'b) list

    exception MissingIndex
    exception EmptyGraph

    (* Construction *)
    val empty : ('a, 'b) t
    val merge : (Index.t -> ('a, 'b) context) -> ('a, 'b) t -> ('a, 'b) t

    (* Query *)
    val null : ('a, 'b) t -> bool
    (* May raise MissingIndex *)
    val context : Index.t * ('a, 'b) t -> ('a, 'b) context
    (* May raise MissingIndex or EmptyGraph *)
    val match : Index.t * ('a, 'b) t -> ('a, 'b) context * ('a, 'b) t
    val maxIndex : ('a, 'b) t -> Index.t option
    val numNodes : ('a, 'b) t -> int
    val numEdges : ('a, 'b) t -> int
end

structure Graph : GRAPH =
struct
    structure Index = struct
        type t = int
        val equal : t * t -> bool = op =
        val compare = Int.compare
        val hash = Word.fromInt
    end

    type ('a, 'b) context = (Index.t * 'b) list * 'a * (Index.t * 'b) list
    type ('a, 'b) t = ('b IntMap.t * 'a * 'b IntMap.t) IntMap.t

    exception MissingIndex
    exception EmptyGraph

    val empty = IntMap.empty
    val null = IntMap.null
    fun maxIndex g = Option.map #1 (IntMap.max g)
    val numNodes = IntMap.size
    fun numEdges g = 0 (* TODO *)

    fun merge f g =
        let val index = Option.getOpt (maxIndex g, ~1) + 1
            val (p, l, s) = f index
            val preds = foldr (fn ((k, v), m) => IntMap.insert (k, v, m)) IntMap.empty p
            val succs = foldr (fn ((k, v), m) => IntMap.insert (k, v, m)) IntMap.empty s
            val g = IntMap.insert (index, (preds, l, succs), g)
            val g = 
        in  raise Fail ""
        end
    fun match _ = raise MissingIndex
    fun context _ = raise MissingIndex
end
