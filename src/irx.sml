signature NAME =
sig
    eqtype t
    val new : unit -> t
    val toString : t -> string
    val toLLVM : t -> string
end

functor NameFn(val prefix : string) :> NAME =
struct
    type t = IntInf.int
    val counter = ref (IntInf.fromInt 0)
    fun new () = !counter before counter := !counter + IntInf.fromInt 1
    fun toString n = "x" ^ IntInf.toString n
    fun toLLVM n = prefix ^ toString n
end

structure Irx =
struct
    structure Var = NameFn(val prefix = "%")
    structure FName = NameFn(val prefix = "@")

    structure Const =
    struct
        datatype t =
            Int of Int64.int
          | Bool of bool

        fun toString (Int n) = Int64.toString n
          | toString (Bool b) = Bool.toString b

        fun toLLVM (Int n) = "i64 " ^ Int64.toString n
          | toLLVM (Bool b) = "i1 " ^ Bool.toString b
    end

    structure Stmt =
    struct
        datatype t =
            Call of FName.t * Var.t list
          | Tail of FName.t * Var.t list
    end
end
