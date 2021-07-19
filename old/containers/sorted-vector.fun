(* vim: set ft=sml: *)

signature SORTED_VECTOR_ELEM =
sig
    type t
    val compare : t * t -> order
end

signature SORTED_VECTOR =
sig
    type t
    type elem

    val empty : t
    val singleton : elem -> t
    val insert : elem * t -> t
    val lookup : elem * t -> elem option
    val union : t * t -> t
    val toList : t -> elem list
    val fromList : elem list -> t
    val compare : t * t -> order
    val equals : t * t -> bool
    val null : t -> bool
    val unions : t list -> t
end

functor SortedVectorFn(Elem : SORTED_VECTOR_ELEM) :> SORTED_VECTOR where type elem = Elem.t =
struct
    type elem = Elem.t
    type t = elem Vector.vector

    datatype search_result = Found of int | NotFound of int

    fun bin_search (x, v) =
        let fun midpoint (a, b) = (a + b) div 2
            fun go (lo, hi) =
                if lo >= hi then NotFound lo
                else let val k = midpoint (lo, hi)
                     in  case Elem.compare (Vector.sub (v, k), x) of
                              LESS => go (k + 1, hi)
                            | GREATER => go (lo, k)
                            | EQUAL => Found k
                     end
        in  go (0, Vector.length v)
        end

    fun compare (v1, v2) = Vector.collate Elem.compare (v1, v2)

    fun equals (v1, v2) = compare (v1, v2) = EQUAL

    val empty : t = Vector.fromList []

    fun singleton x = Vector.fromList [x]
    
    fun lookup (x, v) =
        case bin_search (x, v) of
             Found idx => SOME (Vector.sub (v, idx))
           | NotFound _ => NONE

    fun insert (x, v) =
        let val len = Vector.length v
        in  case bin_search (x, v) of
                Found idx => Vector.tabulate (len, fn i =>
                      if i = idx then x else Vector.sub (v, i))
              | NotFound idx => Vector.tabulate (len + 1, fn i =>
                      case Int.compare (i, idx) of
                           LESS => Vector.sub (v, i)
                         | EQUAL => x
                         | GREATER => Vector.sub (v, i - 1))
        end

    fun merge ([], ys) = ys
      | merge (xs, []) = xs
      | merge (x :: xs, y :: ys) =
          case Elem.compare (x, y) of
               LESS    => x :: merge (xs, y :: ys)
             | GREATER => y :: merge (x :: xs, ys)
             | EQUAL   => x :: merge (xs, ys)

    fun split [] = ([], [])
      | split [x] = ([x], [])
      | split (x1 :: x2 :: xs) =
          case split xs of
               (ys, zs) => (x1 :: ys, x2 :: zs)

    fun merge_sort [] = []
      | merge_sort [x] = [x]
      | merge_sort xs =
          let val (ys, zs) = split xs
          in  merge (merge_sort ys, merge_sort zs)
          end

    fun union (v1, v2) = Vector.fromList (merge (Vector.toList v1, Vector.toList v2))

    val toList = Vector.toList

    fun fromList xs = Vector.fromList (merge_sort xs)

    fun null v = Vector.length v = 0

    fun unions xs = List.foldr union empty xs
end
