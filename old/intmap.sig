
signature INTMAP =
sig
    type 'a t

    (** Construction **)
    (* O(1). The empty map. *)
    val empty : 'a t
    (* O(1). A map of one element. *)
    val singleton : int * 'a -> 'a t
    (* O(n * min(n, W)). Create a map from a list of key/value pairs. *)
    val fromList : (int * 'a) list -> 'a t
    (* O(n * min(n, W)). Create a map from a list of key/value pairs, with a combining function on equal keys. *)
    val fromListWith : ('a * 'a -> 'a) -> (int * 'a) list -> 'a t
    val fromListWithKey : (int * 'a * 'a -> 'a) -> (int * 'a) list -> 'a t
    (* O(n). Build a map from a list of key/value pairs where the keys are in ascending order. *)
    val fromAscList : (int * 'a) list -> 'a t
    (* O(n). Build a map from a list of key/value pairs where the keys are in ascending order, with a combining function on equal keys. *)
    val fromAscListWith : ('a * 'a -> 'a) -> (int * 'a) list -> 'a t (* O(n) *)
    val fromAscListWithKey : (int * 'a * 'a -> 'a) -> (int * 'a) list -> 'a t (* O(n) *)
    (* O(n). Build a map from a list of key/value pairs where the keys are in ascending order and all distinct. *)
    val fromDistinctAscList : (int * 'a) list -> 'a t

    (* Insertion *)
    (* O(min(n, W)). Insert a new key/value pair into the map, replacing the value if the key existed. *)
    val insert : int * 'a * 'a t -> 'a t
    (* O(min(n, W)). Insert a with a combining function called if the key existed. *)
    val insertWith : ('a * 'a -> 'a) -> int * 'a * 'a t -> 'a t
    val insertWithKey : (int * 'a * 'a -> 'a) -> int * 'a * 'a t -> 'a t
    (* O(min(n, W)). Returns a pair containing the results of [lookup (k, m)] and [insertWith f (k, x, m)] *)
    val insertLookupWith : ('a * 'a -> 'a) -> int * 'a * 'a t -> 'a option * 'a t
    val insertLookupWithKey : (int * 'a * 'a -> 'a) -> int * 'a * 'a t -> 'a option * 'a t

    (** Delete/Update **)
    (* O(min(n, W)). Delete a key and its value. Does nothing if key does not exist. *)
    val delete : int * 'a t -> 'a t
    (* O(min(n, W)). Adjust a value at a specific key. Does nothing if key does not exist. *)
    val adjust : ('a -> 'a) -> int * 'a t -> 'a t
    (* O(min(n, W)). Updates the value at a specific key, deleting it if the function returns NONE. *)
    val update : ('a -> 'a option) -> int * 'a t -> 'a t
    (* O(min(n, W)). Lookup and update. Returns the original value and the updated map. *)
    val updateLookup : ('a -> 'a option) -> int * 'a t -> 'a option * 'a t
    (* O(min(n, W)). Alters the value at the key. Can insert, delete, or update. *)
    val alter : ('a option -> 'a option) -> int * 'a t -> 'a t

    (** Lookup **)
    (* O(min(n, W)). Lookup the value at a key. *)
    val lookup : int * 'a t -> 'a option
    (* O(min(n, W)). Lookup with a default. *)
    val lookupWithDefault : 'a * int * 'a t -> 'a
    (* O(log(n)). Find largest key smaller than the given one. *)
    val lookupLT : int * 'a t -> (int * 'a) option
    (* O(log(n)). Find smallest key greater than the given one. *)
    val lookupGT : int * 'a t -> (int * 'a) option
    (* O(log(n)). Find largest key smaller than or equal to the given one. *)
    val lookupLE : int * 'a t -> (int * 'a) option
    (* O(log(n)). Find smallest key greater than or equal to the given one. *)
    val lookupGE : int * 'a t -> (int * 'a) option

    (** Size **)
    (* O(1). Is the map empty? *)
    val null : 'a t -> bool
    (* O(n). The number of elements in the map. *)
    val size : 'a t -> int

    (** Combine **)
    (* O(n + m). The (left-biased) union of two maps. *)
    val union : 'a t * 'a t -> 'a t
    (* O(n + m). The union with a combining function. *)
    val unionWith : ('a * 'a -> 'a) -> 'a t * 'a t -> 'a t
    val unionWithKey : (int * 'a * 'a -> 'a) -> 'a t * 'a t -> 'a t
    (* O(n + m). Difference between two maps. *)
    val difference : 'a t * 'b t -> 'a t
    (* O(n + m). Difference with a combining function. *)
    val differenceWith : ('a * 'b -> 'a option) -> 'a t * 'b t -> 'a t
    val differenceWithKey : (int * 'a * 'b -> 'a option) -> 'a t * 'b t -> 'a t
    (* O(n + m). The (left-biased) intersection of two maps. *)
    val intersection : 'a t * 'b t -> 'a t
    (* O(n + m). The intersection with a combining function. *)
    val intersectionWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
    val intersectionWithKey : (int * 'a * 'b -> 'c) -> 'a t * 'b t -> 'c t

    (** Disjoint **)
    (* O(n + m). Check if the key sets of the maps are disjoint. *)
    val disjoint : 'a t * 'a t -> bool

    (** Compose **)
    (* O(n * min(m, W)). Relate the keys of one map to the values of the other,
     * using the values of the former as keys for lookups in the latter. *)
    val compose : 'a t * int t -> 'a t

    (** Universal combining function **)
    (* O(n + m). Combine two maps, such that
     * Any key present in both maps is passed through `combine`. Kept with SOME, deleted with NONE.
     * Any nonempty subtree only in the first map is passed through `only1`.
     * Any nonempty subtree only in the second map is passed through `only2`. *)
    val mergeWithKey :
        { combine: int * 'a * 'b -> 'c option
        , only1: 'a t -> 'c t
        , only2: 'b t -> 'c t
        } -> 'a t * 'b t -> 'c t

    (** Traversal **)
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapWithKey : (int * 'a -> 'b) -> 'a t -> 'b t
    val mapAccum : ('a * 'b -> 'c * 'b) -> 'b -> 'a t -> 'b * 'c t
    val mapAccumWithKey : (int * 'a * 'b -> 'c * 'b) -> 'b -> 'a t -> 'b * 'c t
    val mapAccumR : ('a * 'b -> 'c * 'b) -> 'b -> 'a t -> 'b * 'c t
    val mapAccumRWithKey : (int * 'a * 'b -> 'c * 'b) -> 'b -> 'a t -> 'b * 'c t
    val mapKeys : (int -> int) -> 'a t -> 'a t
    val mapKeysWith : ('a * 'a -> 'a) -> (int -> int) -> 'a t -> 'a t
    val mapKeysMonotonic : (int -> int) -> 'a t -> 'a t

    (** Folds **)
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldrWithKey : (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldlWithKey : (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b

    (** Conversion **)
    val elems : 'a t -> 'a list
    val keys : 'a t -> int list
    val toList : 'a t -> (int * 'a) list
    val toDescList : 'a t -> (int * 'a) list

    (** Filter **)
    val filter : ('a -> bool) -> 'a t -> 'a t
    val filterWithKey : (int * 'a -> bool) -> 'a t -> 'a t
    val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
    val partitionWithKey : (int * 'a -> bool) -> 'a t -> 'a t * 'a t
    val filterMap : ('a -> 'b option) -> 'a t -> 'b t
    val filterMapWithKey : (int * 'a -> 'b option) -> 'a t -> 'b t

    datatype ('a, 'b) either = Left of 'a | Right of 'b
    val partitionMap : ('a -> ('b, 'c) either) -> 'a t -> 'b t * 'c t
    val partitionMapWithKey : (int * 'a -> ('b, 'c) either) -> 'a t -> 'b t * 'c t
    val split : int * 'a t -> 'a t * 'a t
    val splitLookup : int * 'a t -> 'a t * 'a option * 'a t

    (** Submap **)
    val isSubmapOf : ''a t * ''a t -> bool
    val isSubmapOfBy : ('a * 'b -> bool) -> 'a t * 'b t -> bool
    val isProperSubmapOf : ''a t * ''a t -> bool
    val isProperSubmapOfBy : ('a * 'b -> bool) -> 'a t * 'b t -> bool

    (** Min/Max **)
    val lookupMin : 'a t -> (int * 'a) option
    val lookupMax : 'a t -> (int * 'a) option
    val deleteMin : 'a t -> 'a t
    val deleteMax : 'a t -> 'a t
    val minView : 'a t -> ('a * 'a t) option
    val maxView : 'a t -> ('a * 'a t) option
    val minViewWithKey : 'a t -> ((int * 'a) * 'a t) option
    val maxViewWithKey : 'a t -> ((int * 'a) * 'a t) option

    (** Comparisons **)
    val equal : ''a t * ''a t -> bool
    val equalBy : ('a * 'b -> bool) -> 'a t * 'b t -> bool
    val compareBy : ('a * 'b -> order) -> 'a t * 'b t -> order
end

