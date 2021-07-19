use "shows.sml";

structure IntMap :
sig
    type 'a t

    (* Query *)
    val null : 'a t -> bool
    val size : 'a t -> int
    val lookup : int * 'a t -> 'a option

    (* Construction *)
    val empty : 'a t
    val singleton : int * 'a -> 'a t

    (* Insertion *)
    val insert : int * 'a * 'a t -> 'a t
    val insertWith : (int * 'a * 'a -> 'a) * int * 'a * 'a t -> 'a t

    (* Delete/Update *)
    val delete : int * 'a t -> 'a t
    val adjust : ('a -> 'a) * int * 'a t -> 'a t
    val update : ('a -> 'a option) * int * 'a t -> 'a t
    val alter : ('a option -> 'a option) * int * 'a t -> 'a t

    (* Map *)
    val map : ('a -> 'b) * 'a t -> 'b t
    val mapWithKey : (int * 'a -> 'b) * 'a t -> 'b t

    (* Folds *)
    val foldrWithKey : (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val differenceWithKey : (int * 'a * 'b -> 'a) -> 'a t * 'b t -> 'a t

    (* Conversion *)
    val keys : 'a t -> int list

    (* Min/Max *)
    val max : 'a t -> (int * 'a) option

    (* Debugging *)
    val showTree : string t -> string
end =
struct
    datatype 'a t = Bin of int * int * 'a t * 'a t
                  | Tip of int * 'a
                  | Nil

    (* Helpers *)
    fun maskW (i, m) = Word.toInt (Word.andb (i, Word.xorb (Word.~ m, m)))
    fun mask (i, m) = maskW (Word.fromInt i, Word.fromInt m)
    fun nomatch (i, p, m) = mask (i, m) <> p
    fun zero (i, m) = Word.andb (Word.fromInt i, Word.fromInt m) = 0w0
    fun highestBitMask x =
        let val x = Word.orb (x, Word.>> (x, 0w1))
            val x = Word.orb (x, Word.>> (x, 0w2))
            val x = Word.orb (x, Word.>> (x, 0w4))
            val x = Word.orb (x, Word.>> (x, 0w8))
            val x = Word.orb (x, Word.>> (x, 0w16))
            val x = Word.orb (x, Word.>> (x, 0w32))
        in  Word.xorb (x, Word.>> (x, 0w1))
        end
    fun branchMask (p1, p2) = Word.toInt (highestBitMask (Word.xorb (Word.fromInt p1, Word.fromInt p2)))
    fun linkWithMask (m, p1, t1, t2) =
        let val p = mask (p1, m)
        in  if zero (p1, m) then Bin (p, m, t1, t2) else Bin (p, m, t2, t1)
        end
    fun link (p1, t1, p2, t2) = linkWithMask (branchMask (p1, p2), p1, t1, t2)
    fun binCheckLeft (p, m, Nil, r) = r
      | binCheckLeft (p, m, l, r) = Bin (p, m, l, r)
    fun binCheckRight (p, m, l, Nil) = l
      | binCheckRight (p, m, l, r) = Bin (p, m, l, r)


    (* Query *)
    fun null Nil = true
      | null _ = false

    fun size t = 
        let fun go (acc, Bin (_, _, l, r)) = go (go (acc, l), r)
          | go (acc, Tip _) = 1 + acc
          | go (acc, Nil) = acc
        in  go (0, t)
        end

    fun lookup (k, t) =
        let fun go (Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then NONE
                  else if zero (k, m) then go l
                  else go r
              | go (Tip (kx, x)) =
                  if k = kx then SOME x else NONE
              | go Nil = NONE
        in  go t
        end

    (* Construction *)
    val empty = Nil

    val singleton = Tip

    fun insert (k, x, t) =
        let fun go (t as Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then link (k, Tip (k, x), p, t)
                  else if zero (k, m) then Bin (p, m, go l, r)
                  else Bin (p, m, l, go r)
              | go (t as Tip (ky, _)) =
                  if k = ky then Tip (k, x)
                  else link (k, Tip (k, x), ky, t)
              | go Nil = Tip (k, x)
        in  go t
        end

    fun insertWith (f, k, x, t) =
        let fun go (t as Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then link (k, Tip (k, x), p, t)
                  else if zero (k, m) then Bin (p, m, go l, r)
                  else Bin (p, m, l, go r)
              | go (t as Tip (ky, y)) =
                  if k = ky then Tip (k, f (k, x, y))
                  else link (k, Tip (k, x), ky, t)
              | go Nil = Tip (k, x)
        in  go t
        end

    fun delete (k, t) =
        let fun go (t as Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then t
                  else if zero (k, m) then binCheckLeft (p, m, go l, r)
                  else binCheckRight (p, m, l, go r)
              | go (t as Tip (ky, _)) =
                  if k = ky then Nil else t
              | go Nil = Nil
        in  go t
        end

    fun adjust (f, k, t) =
        let fun go (t as Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then t
                  else if zero (k, m) then Bin (p, m, go l, r)
                  else Bin (p, m, l, go r)
              | go (t as Tip (ky, y)) =
                  if k = ky then Tip (ky, f y) else t
              | go Nil = Nil
        in  go t
        end

    fun update (f, k, t) =
        let fun go (t as Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then t
                  else if zero (k, m) then binCheckLeft (p, m, go l, r)
                  else binCheckRight (p, m, l, go r)
              | go (t as Tip (ky, y)) =
                  if k = ky then case f y of NONE => Nil | SOME y' => Tip (ky, y')
                  else t
              | go Nil = Nil
        in  go t
        end

    fun alter (f, k, t) =
        let fun go (t as Bin (p, m, l, r)) =
                  if nomatch (k, p, m) then case f NONE of NONE => t | SOME x => link (k, Tip (k, x), p, t)
                  else if zero (k, m) then binCheckLeft (p, m, go l, r)
                  else binCheckRight (p, m, l, go r)
              | go (t as Tip (ky, y)) =
                  if k = ky then case f (SOME y) of NONE => Nil | SOME y' => Tip (ky, y')
                  else (case f NONE of NONE => Tip (ky, y) | SOME x => link (k, Tip (k, x), ky, t))
              | go Nil =
                  case f NONE of NONE => Nil | SOME x => Tip (k, x)
        in  go t
        end

    fun map (f, t) =
        let fun go (Bin (p, m, l, r)) = Bin (p, m, go l, go r)
              | go (Tip (k, x)) = Tip (k, f x)
              | go Nil = Nil
        in  go t
        end

    fun mapWithKey (f, t) =
        let fun go (Bin (p, m, l, r)) = Bin (p, m, go l, go r)
              | go (Tip (k, x)) = Tip (k, f (k, x))
              | go Nil = Nil
        in  go t
        end

    fun foldrWithKey f z t =
        let fun go (z, Bin (_, _, l, r)) = go (go (z, r), l)
              | go (z, Tip (kx, x)) = f (kx, x, z)
              | go (z, Nil) = z
        in case t of
                Bin (_, m, l, r) =>
                  if m < 0 then go (go (z, l), r) else go (go (z, r), l)
              | _ => go (z, t)
        end

    fun keys t = foldrWithKey (fn (k, _, ks) => k::ks) [] t

    fun min (Bin (_, m, l, r)) =
          let fun go (Bin (_, _, l, _)) = go l
                | go (Tip (k, v)) = SOME (k, v)
                | go Nil = NONE
          in  if m < 0 then go r else go l
          end
      | min (Tip (k, v)) = SOME (k, v)
      | min Nil = NONE

    fun max (Bin (_, m, l, r)) =
          let fun go (Bin (_, _, _, r)) = go r
                | go (Tip (k, v)) = SOME (k, v)
                | go Nil = NONE
          in  if m < 0 then go l else go r
          end
      | max (Tip (k, v)) = SOME (k, v)
      | max Nil = NONE

    fun mergeWithKey' bin f g1 g2 =
        let fun maybeLink (_, Nil, _, t2) = t2
              | maybeLink (_, t1, _, Nil) = t1
              | maybeLink (p1, t1, p2, t2) = link (p1, t1, p2, t2)
            fun shorter (m1, m2) = Word.fromInt m1 > Word.fromInt m2
            fun go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
                  if shorter (m1, m2)
                  then if nomatch (p2, p1, m1) then maybeLink (p1, g1 t1, p2, g2 t2)
                       else if zero (p2, m1) then bin (p1, m1, go (l1, t2), g1 r1)
                       else bin (p1, m1, g1 l1, go (r1, r2))
                   else if nomatch (p1, p2, m2) then maybeLink (p1, g1 t1, p2, g2 t2)
                       else if zero (p1, m2) then bin (p2, m2, go (t1, l2), g2 r2)
                       else bin (p2, m2, g2 l2, go (t1, r2))
              | go (t1' as Bin _, t2' as Tip (k2', _)) =
                  let fun merge0 _ = 0
                  in  merge0 (t2', k2', t1')
                  end
              | go _ = raise Fail ""
        in  go
        end

    fun mergeWithKey f g1 g2 (m1, m2) =
        let fun bin (_, _, l, Nil) = l
              | bin (_, _, Nil, r) = r
              | bin (p, m, l, r) = Bin (p, m, l, r)
            fun combine (Tip (k1, x1), Tip (_, x2)) =
                (case f (k1, x1, x2) of
                      NONE => Nil
                    | SOME x => Tip (k1, x))
              | combine _ = Fail "Can't happen"
        in  mergeWithKey' bin combine g1 g2 (m1, m2)
        end

    fun differenceWithKey f (m1, m2) = mergeWithKey f (fn x => x) (fn _ => Nil) (m1, m2)

    local
        val ++ = ShowS.++
        infixr 9 ++

        fun showBars [] = ShowS.empty
          | showBars (_::bars) = ShowS.str (String.concat (List.rev bars)) ++ ShowS.str "+--"
        fun go bars (Bin (p, m, l, r)) =
              showBars bars ++ ShowS.str "*\n" ++
              go ("|  "::bars) l ++
              go ("   "::bars) r
          | go bars (Tip (k, x)) =
              showBars bars ++
              ShowS.str " " ++ ShowS.int k ++ ShowS.str " := " ++ ShowS.str x ++ ShowS.str "\n"
          | go bars Nil = showBars bars ++ ShowS.str "|\n"
    in
        val showTree = ShowS.toString o go []
    end
end
