use "intmap.sig";

structure IntMap : INTMAP =
struct
    datatype 'a t = Empty
                  | Leaf of word * 'a
                  | Branch of word * word * 'a t * 'a t

    fun lowestBit x = Word.andb (x, 0w0 - x)
    fun max (x, y) = if x < y then x else y
    fun highestBit (x, m) =
        let val x' = Word.andb (x, Word.notb (m - 0w1))
            fun highb (x, m) =
                if x = m then m else highb (Word.andb (x, Word.notb m), m + m)
        in  highb (x', m)
        end
    fun branchingBit (m, p0, p1) = highestBit (Word.xorb (p0, p1), m)
    fun mask (k, m) = Word.orb (k, m - 0w1 + m) - m
    fun zeroBit (k, m) = Word.andb (k, m) = 0w0
    fun matchPrefix (k, p, m) = mask (k, m) = p
    fun swap (x, y) = (y, x)

    val empty = Empty

    fun singleton (k, v) = Leaf (Word.fromInt k, v)

    fun null Empty = true
      | null _ = false

    fun lookup (k, t) =
        let val k = Word.fromInt k
            fun go Empty = NONE
              | go (Leaf (k', x)) = if k = k' then SOME x else NONE
              | go (Branch (p, m, l, r)) = if k <= p then go l else go r
        in  go t
        end

    fun join (m, p0, t0, p1, t1) =
        let val m = branchingBit (m, p0, p1)
        in  if p0 < p1
            then Branch (mask (p0, m), m, t0, t1)
            else Branch (mask (p0, m), m, t1, t0)
        end

    fun insertw c (w, x, t) =
        let fun go Empty = Leaf (w, x)
              | go (t as Leaf (j, y)) =
                  if j = w
                  then Leaf (w, c (x, y))
                  else join (0w1, w, Leaf (w, x), j, t)
              | go (t as Branch (p, m, l, r)) =
                  if matchPrefix (w, p, m)
                  then if w <= p
                       then Branch (p, m, go l, r)
                       else Branch (p, m, l, go r)
                  else join (m + m, w, Leaf (w, x), p, t)
        in  go t
        end

    fun insert (k, x, t) = insertw #1 (Word.fromInt k, x, t)

    fun merge c s t =
        let fun go (s as Branch (p, m, s0, s1), t as Branch (q, n, t0, t1)) =
                  if m < n
                  then if matchPrefix (p, q, n)
                       then if p <= q
                            then Branch (q, n, go (s, t0), t1)
                            else Branch (q, n, t0, go (s, t1))
                       else join (n + n, p, s, q, t)
                  else if m > n
                       then if matchPrefix (q, p, m)
                            then if q <= p
                                 then Branch (p, m, go (s0, t), s1)
                                 else Branch (p, m, s0, go (s1, t))
                            else join (m + m, p, s, q, t)
                       else if p = q
                            then Branch (p, m, go (s0, t0), go (s1, t1))
                            else join (m + m, p, s, q, t)
              | go (t as Branch _, Leaf (w, x)) = insertw (c o swap) (w, x, t)
              | go (t as Branch _, Empty) = t
              | go (Leaf (w, x), t) = insertw c (w, x, t)
              | go (Empty, t) = t
        in  go (s, t)
        end

    fun fold f acc t =
    let fun go (acc, Empty) = acc
          | go (acc, Leaf (w, e)) = f (Word.toInt w, e, acc)
          | go (acc, Branch (_, _, l, r)) = go (go (acc, l), r)
    in  go (acc, t)
    end

    fun delete (k, t) =
        let val w = Word.fromInt k
            fun branch (_, _, l, Empty) = l
              | branch (_, _, Empty, r) = r
              | branch x = Branch x
            fun go Empty = Empty
              | go (t as Leaf (ky, _)) = if w = ky then Empty else t
              | go (t as Branch (p, m, l, r)) =
                  if matchPrefix (w, p, m)
                  then if w <= p
                       then branch (p, m, go l, r)
                       else branch (p, m, l, go r)
                  else t
        in  go t
        end
end
