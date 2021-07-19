
functor NameFn(val prefix : string) :>
sig
    eqtype t
    val new : string -> t
    val toString : t -> string
end =
struct
    val counter = ref 0
    type t = string * int
    fun new s = (s, !counter) before counter := !counter + 1
    fun toString (s, n) = prefix ^ s ^ "_" ^ Int.toString n
end

structure Name =
struct
    val counter = ref 0

    type t = string * int
    fun new s = (s, !counter) before counter := !counter + 1
    fun toString (s, n) = s ^ "#" ^ Int.toString n
end
structure Val =
struct
end
structure Exp =
struct
    datatype t =
        ConApp of Name.t * Name.t list
      | Const of Int64.int
      | Var of Name.t
end
structure Stmt =
struct
    datatype t = Let of Name.t * Exp.t 
end
structure Transfer =
struct
    datatype t =
        Call of Name.t * Name.t list
      | Case of { test: Name.t, default: Name.t option, cases: (Name.t * Name.t) list }
      | Goto of Name.t * Name.t list
      | Raise of Name.t list
      | Return of Name.t list
end
structure Block =
struct
    type t = {
        args: Name.t list,
        label: Name.t,
        body: Stmt.t list,
        transfer: Transfer.t
    }
end
