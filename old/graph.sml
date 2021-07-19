(* Inductive graph implementation *)
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

signature INDUCTIVE_GRAPH =
sig
    type ('a, 'b) t

    type index
    datatype 'b edge = E of { from: index, to: index, label: 'b }
    datatype 'a vertex = V of { index: index, label: 'a }
    datatype ('a, 'b) context = C of { preds: 'b edge list, succs: 'b edge list, vertex: 'a vertex }

    exception NoIndex of index

    val edgeEqual : ('a * 'a -> bool) * 'a edge * 'a edge -> bool

    val indexEqual : index * index -> bool
    val indexCompare : index * index -> order
    val indexHash : index -> int

    val empty : ('a, 'b) t
    val vertex : 'a * ('a, 'b) t -> index * ('a, 'b) t
    val edge : 'b edge * ('a, 'b) t -> ('a, 'b) t
    val match : index * ('a, 'b) t -> (('a, 'b) context * ('a, 'b) t) option
    val matchAny : ('a, 'b) t -> (('a, 'b) context * ('a, 'b) t) option
    val maxIndex : ('a, 'b) t -> index option
    val mapEdges : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

    val showTree : (string, string) t -> string
end

structure Graph : INDUCTIVE_GRAPH =
struct
    type index = int
    type ('a, 'b) ctx = 'b IntMap.t * index * 'a * 'b IntMap.t
    type ('a, 'b) t = ('a, 'b) ctx IntMap.t
    datatype 'b edge = E of { from: index, to: index, label: 'b }
    datatype 'a vertex = V of { index: index, label: 'a }
    datatype ('a, 'b) context = C of { preds: 'b edge list, succs: 'b edge list, vertex: 'a vertex }

    exception NoIndex of index

    fun edgeEqual (eq, E { from, to, label }, E { from = f2, to = t2, label = l2 }) =
        from = f2 andalso to = t2 andalso eq (label, l2)

    val indexEqual : index * index -> bool = op =
    val indexCompare = Int.compare
    fun indexHash x = x

    val empty = IntMap.empty


    fun match (idx, g) =
        case IntMap.lookup (idx, g) of
             NONE => NONE
           | SOME (p, idx, v, s) =>
               let fun removeSucc (pp, vert, lbl, ss) = (pp, vert, lbl, IntMap.delete (idx, ss))
                   fun removePred (pp, vert, lbl, ss) = (IntMap.delete (idx, pp), vert, lbl, ss)
                   val g = List.foldr (fn (x, acc) => IntMap.adjust (removeSucc, x, acc)) g (IntMap.keys p)
                   val g = List.foldr (fn (x, acc) => IntMap.adjust (removePred, x, acc)) g (IntMap.keys s)
                   val g = IntMap.delete (idx, g)
                   val pp = IntMap.foldrWithKey (fn (k, v, acc) => E { from = k, to = idx, label = v } :: acc) [] p
                   val ss = IntMap.foldrWithKey (fn (k, v, acc) => E { from = idx, to = k, label = v } :: acc) [] s
               in  SOME (C { preds = pp, succs = ss, vertex = V { index = idx, label = v } }, g)
               end
    fun maxIndex g = Option.map #1 (IntMap.max g)
    fun matchAny g = case maxIndex g of NONE => NONE | SOME idx => match (idx, g)

    fun vertex (lbl, g) =
        let val index = Option.getOpt (maxIndex g, ~1) + 1
            val ctx = (IntMap.empty, index, lbl, IntMap.empty)
        in  (index, IntMap.insert (index, ctx, g))
        end

    fun edge (E { from, to, label }, g) =
        let val (sp, sv, sl, ss) = Option.valOf (IntMap.lookup (from, g))
                handle Option => raise NoIndex from
            val sctx = (sp, sv, sl, IntMap.insert (to, label, ss))
            val g = IntMap.insert (from, sctx, g)
            val (dp, dv, dl, ds) = Option.valOf (IntMap.lookup (to, g))
                handle Option => raise NoIndex to
            val dctx = (IntMap.insert (from, label, dp), dv, dl, ds)
            val g = IntMap.insert (to, dctx, g)
        in  g
        end

    fun mapEdges f g =
        let fun mapCtx (p, v, l, s) = (IntMap.map (f, p), v, l, IntMap.map (f, s))
        in  IntMap.map (mapCtx, g)
        end

    fun showTree g =
    let fun go (p, v, l, s) =
        let val preds = IntMap.showTree p
            val succs = IntMap.showTree s
            val index = Int.toString v
            val label = l
        in "{ preds = \n" ^ preds ^ ", index = " ^ index ^ ", label = " ^ label ^ ", succs = \n" ^ succs ^ " }"
        end
    in  IntMap.showTree (IntMap.map (go, g))
    end
end

