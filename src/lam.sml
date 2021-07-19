
structure Class =
struct
    datatype t = First | Second

    fun compare (First, First) = EQUAL
      | compare (Second, Second) = EQUAL
      | compare (First, Second) = LESS
      | compare (Second, First) = GREATER

    fun toString First = "¹"
      | toString Second = "²"

    fun max (First, First) = First
      | max _ = Second
end

structure Var :
sig
    eqtype t
    val new : Class.t -> t
    val class : t -> Class.t
    val toString : t -> string
    val compare : t * t -> order
end =
struct
    type t = Class.t * int

    val counter = ref 0

    fun new (c) = (c, !counter) before counter := !counter + 1

    fun class (c, _) = c

    fun toString (c, n) = String.concat ["%", Int.toString n, Class.toString c]

    fun compare ((c1, n1), (c2, n2)) =
        case Class.compare (c1, c2) of
             EQUAL => Int.compare (n1, n2)
           | order => order
end

structure Type =
struct
    datatype node =
        Bool
      | Arrow of t * t option
      | Pair of t * t
    withtype t = node * Class.t

    fun pp Bool = "bool"
      | pp (Arrow (t1, NONE)) = "(" ^ toString t1 ^ " → bot)"
      | pp (Arrow (t1, SOME t2)) = "(" ^ toString t1 ^ " → " ^ toString t2 ^ ")"
      | pp (Pair (t1, t2)) = "(" ^ toString t1 ^ " * " ^ toString t2 ^ ")"
    and toString (t, c) = pp t ^ Class.toString c
end

structure Exp =
struct
    datatype t =
        Bool of bool
      | Var of Var.t
      | Lam of Var.t * t
      | App of t * t
      | Fst of t
      | Snd of t
      | If of t * t * t
      | Let of Var.t * t * t
      | C of t

    fun toString e =
        let fun newline d = "\n" ^ CharVector.tabulate (d, fn _ => #" ")
            fun go (d, Bool b) = Bool.toString b
              | go (d, Var v) = Var.toString v
              | go (d, Lam (x, e)) = "(lambda (" ^ Var.toString x ^ ")" ^ newline (d+2) ^ go (d+2, e) ^ ")"
              | go (d, App (e1, e2)) = "(" ^ go (d, e1) ^ " " ^ go (d+2, e2) ^ ")"
              | go (d, Fst e) = "(fst " ^ go (d+4, e) ^ ")"
              | go (d, Snd e) = "(snd " ^ go (d+4, e) ^ ")"
              | go (d, If (cond, t, f)) =
                    "(if " ^ go (d+4, cond)
                    ^ newline (d+2) ^ go (d+2, t)
                    ^ newline (d+2) ^ go (d+2, f) ^ ")"
              | go (d, Let (x, e1, e2)) =
                    let val x = Var.toString x
                    in  "(let ((" ^ x ^ " " ^ go (d+String.size x+8, e1) ^ "))"
                        ^ newline (d+2) ^ go (d+2, e2) ^ ")"
                    end
              | go (d, C e) = "(C " ^ go (d+2, e) ^ ")"
        in  go (0, e)
        end
end

fun test () =
    let val t = Exp.Bool true
        val f = Exp.Bool false
        val x = Exp.Var (Var.new (Class.First))
        val y = Exp.Var (Var.new (Class.First))
        val e = Exp.App (x, y)
        val e = Exp.If (y, e, f)
        val e = Exp.Let (Var.new Class.First, e, e)
    in  TextIO.print (Exp.toString e ^ "\n")
    end

