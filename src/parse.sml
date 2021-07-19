
structure Parse =
struct
    structure P = SparseFn(StringInput)
    structure P = SparseExtrasFn(P)
    structure C = SparseCharExtrasFn(structure P = P fun toChar c = c fun fromChar c = c)
    structure L = SparseLexFn(P)
    open C
    open L
end
