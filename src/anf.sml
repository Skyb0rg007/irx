
structure Anf =
struct
    structure Var =
    struct
        type t = int
        val counter = ref 0
        fun new () = !counter before counter := !counter + 1
        fun toString n = "%" ^ Int.toString n
    end

    structure Const =
    struct
        datatype t =
            I32 of Int32.int
          | I64 of Int64.int
          | I1 of bool
        fun toString (I32 n) = Int32.toString n
          | toString (I64 n) = Int64.toString n
          | toString (I1 b) = Bool.toString b
    end

    datatype value =
        Var of Var.t
      | Const of Const.t
      | Prim of prim
      | Lam of Var.t * exp
    and exp =
        Val of value
      | LetVal of Var.t * value * exp
      | LetApp of Var.t * value * value * exp
      | If of value * exp * exp
    and prim =
        Add of value * value
      | Sub of value * value
      | Cmp of cmp * value * value
    and cmp = SLT | SGT | SLE | SGE | ULT | UGT | ULE | UGE

    fun valToString (_, Var v) = Var.toString v
      | valToString (_, Const k) = Const.toString k
      | valToString (_, Prim p) = primToString p
      | valToString (d, Lam (v, e)) =
          let val pre = "λ" ^ Var.toString v ^ "."
          in  pre ^ expToString (d + String.size pre, e)
          end
    and expToString (d, Val v) = valToString (d, v)
      | expToString (d, LetVal (x, v, e)) =
          let fun indent n = "\n" ^ CharVector.tabulate (n, fn _ => #" ")
          in  "let " ^ Var.toString x ^ "="
              ^ indent (d + 2) ^ valToString (d + 2, v)
              ^ indent d ^ expToString (d, e)
          end
      | expToString (d, LetApp (x, v1, v2, e)) =
          let fun indent n = "\n" ^ CharVector.tabulate (n, fn _ => #" ")
              val pre = "let " ^ Var.toString x ^ "="
              val v1s = indent (d + 2) ^ valToString (d + 2, v1)
              val v2s = indent (d + 2) ^ valToString (d + 2, v2)
              fun multi () = pre ^ indent (d + 2) ^ "(" ^ v1s ^ ") (" ^ v2s ^ ")"
              fun single () = pre ^ " " ^ v1s ^ " " ^ v2s
          in  case (v1, v2) of
                   (Lam _, _) => multi ()
                 | (_, Lam _) => multi ()
                 | _ => single ()
          end
    and primToString _ = "prim"

    structure Val =
    struct
        datatype t = datatype value
        val toString = valToString
    end

    structure Exp =
    struct
        datatype t = datatype exp
        val toString = expToString
    end
end

local open Anf in
(*
 * fun factorial n =
 *     let fun go (n, acc) =
 *             if n <= 1 then acc
 *             else go (n - 1, n * acc)
 *     in  go (n, 0)
 *     end
 *)
val v1 = Var.new ()
val go = Val.Lam (v1,
         Exp.If (Val.Prim (Cmp (SLE, Val.Var v1, Val.Const (Const.I64 1))),
                 Exp.Val (Val.Var v1)))
end
