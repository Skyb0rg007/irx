
signature SHOWS =
sig
    type t

    val empty : t
    val str : string -> t
    val chr : char -> t
    val int : int -> t
    val word : word -> t
    val ++ : t * t -> t
    val toString : t -> string
end

structure ShowS : SHOWS =
struct
    type t = string -> string

    fun empty s = s
    fun str s s' = s ^ s'
    fun chr c s = String.str c ^ s
    fun int n s = Int.toString n ^ s
    fun word w s = Word.toString w ^ s
    val op ++ = op o
    fun toString f = f ""
end
