
structure IRX =
struct
    structure Name : sig
        eqtype t
        val toString : t -> string
        val new : string -> t
    end =
    struct
        type t = string * int
        val counter = ref 0
        fun new s = (s, !counter) before counter := !counter + 1
        fun toString (s, _) = String.toString s
    end

    structure Type =
    struct
        structure Simple =
        struct
            datatype t =
                Int64
              | Word64
              | Float
              | Bool
              | Unit
              | Location of int list
              | UnspecifiedLocation
              | Dead
              | String
              | Char
            fun toString Int64 = "Int64"
              | toString Word64 = "Word64"
              | toString Float = "Float"
              | toString Bool = "Bool"
              | toString Unit = "Unit"
              | toString (Location locs) = "Location"
              | toString UnspecifiedLocation = "UnspecifiedLocation"
              | toString Dead = "Dead"
              | toString String = "String"
              | toString Char = "Char"
        end
        datatype t =
            Con of Name.t * t list
          | Var of Name.t
          | Simple of Simple.t

        fun toString (Con (c, args)) = Name.toString c ^ "(" ^ String.concatWith ", " (map toString args) ^ ")"
          | toString (Var v) = Name.toString v
          | toString (Simple v) = Simple.toString v
    end

    structure Literal =
    struct
        datatype t =
            Int64 of Int64.int
          | Word64 of Word64.word
          | Float of Real.real
          | Bool of bool
          | String of string
          | Char of char

        fun toString (Int64 n) = Int64.toString n
          | toString (Word64 w) = Word64.toString w
          | toString (Float r) = Real.toString r
          | toString (Bool b) = Bool.toString b
          | toString (String s) = String.toString s
          | toString (Char c) = Char.toString c
    end

    structure Tag =
    struct
        datatype t = C of Name.t | F of Name.t | P of int * Name.t

        fun toString (C nm) = "C" ^ Name.toString nm
          | toString (F nm) = "F" ^ Name.toString nm
          | toString (P (n, nm)) = "P" ^ Int.toString n ^ Name.toString nm
    end

    structure Val =
    struct
        datatype t =
            Tag of Tag.t * t list
          | Unit
          | Lit of Literal.t
          | Var of Name.t
          | Undefined of Type.t

        fun toString (Tag (t, args)) = Tag.toString t ^ "(" ^ String.concatWith ", " (map toString args) ^ ")"
          | toString Unit = "Unit"
          | toString (Lit lit) = Literal.toString lit
          | toString (Var v) = Name.toString v
          | toString (Undefined ty) = "(undefined : " ^ Type.toString ty ^ ")"
    end

    structure LPat = Val

    structure CPat =
    struct
        datatype t =
            Node of Tag.t * Name.t list
          | Lit of Literal.t

        fun toString (Node (t, args)) = Tag.toString t ^ "(" ^ String.concatWith ", " (map Name.toString args) ^ ")"
          | toString (Lit lit) = Literal.toString lit
    end

    structure Exp =
    struct
        datatype t =
            Let of LPat.t * t * t
          | Case of { exp: Val.t, default: t, cases: (CPat.t * t) list }
          | App of Name.t * t list
          | Return of Val.t
          | Store of Val.t
          | Fetch of Name.t
          | Update of Name.t * Val.t

        fun toString exp =
            let fun indent d = CharVector.tabulate (d, fn _ => #" ")
                fun go (d, Let (p, e1, e2)) = String.concat [
                      "let ", LPat.toString p, " = ", go (d + 2, e1), ";\n", indent d, go (d, e2)
                    ]
                  | go (d, Case {exp, default, cases}) = String.concat [
                      "case ", Val.toString exp, " of\n",
                      String.concat (map (fn (p, e) => String.concat [indent (d+2), CPat.toString p, " => ", go (d+4, e), "\n"]) cases),
                      indent (d+2), "_ => ", go (d + 4, default)
                    ]
                  | go (d, App (f, args)) = Name.toString f ^ "(" ^ String.concatWith ", " (map toString args) ^ ")"
                  | go (d, Return v) = "return " ^ Val.toString v
                  | go (d, _) = ""
            in  go (0, exp)
            end
    end

end
