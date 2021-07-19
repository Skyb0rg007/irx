
structure Word16 = Word32

structure LLVM =
struct
    structure Type =
    struct
        datatype t =
            Void
          | Integer of { bits: Word32.word }
          | Pointer of { referent: t, addrspace: Word32.word }
          | Half | Float | Double | FP128 | X86_FP80 | PPC_FP128
          | Function of { result: t, args: t list, vararg: bool }
          | Vector of { size: Word32.word, typ: t }
          | Structure of { packed: bool, elems: t list }
          | Array of { size: Word32.word, typ: t }

        fun toString Void = "void"
          | toString (Integer { bits }) = "i" ^ Word32.toString bits
          | toString (Pointer { referent, addrspace = 0w0 }) = toString referent ^ "*"
          | toString (Pointer { referent, addrspace }) = toString referent ^ " addrspace(" ^ Word32.toString addrspace ^ ") *"
          | toString Half = "half"
          | toString Float = "float"
          | toString Double = "double"
          | toString FP128 = "fp128"
          | toString X86_FP80 = "x86_fp80"
          | toString PPC_FP128 = "ppc_fp128"
          | toString (Function { result, args, vararg }) =
              toString result ^ " (" ^ String.concatWith ", " (List.map toString args) ^ (if vararg then ", ...)" else ")")
          | toString (Vector { size, typ }) = "<" ^ Word32.toString size ^ " x " ^ toString typ ^ ">"
          | toString (Array { size, typ }) = "[" ^ Word32.toString size ^ " x " ^ toString typ ^ "]"
          | toString (Structure { packed, elems }) =
              let val (l, r) = if packed then ("<{", "}>") else ("{", "}")
              in  l ^ String.concatWith ", " (List.map toString elems) ^ r
              end

        fun aggregate (Array _) = true
          | aggregate (Structure _) = true
          | aggregate _ = false

        fun floating Half = true
          | floating Float = true
          | floating Double = true
          | floating FP128 = true
          | floating X86_FP80 = true
          | floating PPC_FP128 = true
          | floating _ = false

        fun singleValue (Integer _) = true
          | singleValue Half = true
          | singleValue Float = true
          | singleValue Double = true
          | singleValue FP128 = true
          | singleValue X86_FP80 = true
          | singleValue PPC_FP128 = true
          | singleValue (Pointer _) = true
          | singleValue (Vector _) = true
          | singleValue _ = false

        val i1 = Integer { bits = 0w1 }
        val i8 = Integer { bits = 0w8 }
        val i16 = Integer { bits = 0w16 }
        val i32 = Integer { bits = 0w32 }
        val i64 = Integer { bits = 0w64 }
        val i128 = Integer { bits = 0w128 }
        fun ptr t = Pointer { referent = t, addrspace = 0w0 }
    end

    structure Constant =
    struct
        structure Float =
        struct
            datatype t =
                Half of Word16.word
              | Single of Word32.word
              | Double of Word64.word
              | Quadruple of Word64.word * Word64.word
              | X86_FP80 of Word16.word * Word64.word
              | PPC_FP128 of Word64.word * Word64.word

            val equal : t * t -> bool = op =

            fun typeOf (Half _) = Type.Half
              | typeOf (Single _) = Type.Float
              | typeOf (Double _) = Type.Double
              | typeOf (Quadruple _) = Type.FP128
              | typeOf (X86_FP80 _) = Type.X86_FP80
              | typeOf (PPC_FP128 _) = Type.PPC_FP128

            fun pad req str = CharVector.tabulate (req - String.size str, fn _ => #"0") ^ str

            fun toString (Half w) = "half 0xH" ^ pad 4 (Word16.fmt StringCvt.HEX w)
              | toString (Single w) = "float 0x" ^ pad 16 (Word32.fmt StringCvt.HEX w)
              | toString (Double w) = "double 0x" ^ pad 16 (Word64.fmt StringCvt.HEX w)
              | toString (Quadruple (w1, w2)) = "fp128 0xL" ^ pad 16 (Word64.fmt StringCvt.HEX w1) ^ pad 16 (Word64.fmt StringCvt.HEX w2)
              | toString (X86_FP80 (w1, w2)) = "x86_fp80 0xK" ^ pad 4 (Word16.fmt StringCvt.HEX w1) ^ pad 16 (Word64.fmt StringCvt.HEX w2)
              | toString (PPC_FP128 (w1, w2)) = "ppc_fp128 0xM" ^ pad 16 (Word64.fmt StringCvt.HEX w1) ^ pad 16 (Word64.fmt StringCvt.HEX w2)
        end
    end
end
