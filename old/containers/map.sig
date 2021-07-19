signature MAP =
sig
    type key
    type 'a t

    val empty : 'a t
    val insert : key * 'a * 'a t -> 'a t
    val lookup : key * 'a t -> 'a option
    val delete : key * 'a t -> 'a t
    val size : 'a t -> int

    val fromList : (key * 'a) list -> 'a t
    val toList : 'a t -> (key * 'a) list
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldri : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
end
