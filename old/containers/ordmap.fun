(* vim: set ft=sml: *)
use "./src/containers/map.sig";

signature ORDMAP_KEY =
sig
    type t
    val compare : t * t -> order
end

(* O(log n) lookup, insert, delete
 * O(m * log(n/m + 1)), m <= n union
 *)

functor OrdMapFn(Key : ORDMAP_KEY) : MAP where type key = Key.t =
struct
    type key = Key.t
    datatype 'a t = Bin of int * key * 'a * 'a t * 'a t
                  | Tip

    val delta = 3
    val ratio = 2

    fun size Tip = 0
      | size (Bin (sz, _, _, _, _)) = sz

    fun balanceL (k, x, Tip, Tip) =
          Bin (1, k, x, Tip, Tip)
      | balanceL (k, x, l as Bin (_, _, _, Tip, Tip), Tip) =
          Bin (2, k, x, l, Tip)
      | balanceL (k, x, l as Bin (_, lk, lx, Tip, Bin (_, lrk, lrx, _, _)), Tip) =
          Bin (3, lrk, lrx, Bin (1, lk, lx, Tip, Tip), Bin (1, k, x, Tip, Tip))
      | balanceL (k, x, l as Bin (_, lk, lx, ll as Bin _, Tip), Tip) =
          Bin (3, lk, lx, ll, Bin (1, k, x, Tip, Tip))
      | balanceL (k, x, l as Bin (ls, lk, lx, ll as Bin (lls, _, _, _, _), lr as Bin (lrs, lrk, lrx, lrl, lrr)), Tip) =
          if lrs < ratio * lls
          then Bin (1 + ls, lk, lx, ll, Bin (1 + lrs, k, x, lr, Tip))
          else Bin (1 + ls, lrk, lrx, Bin (1 + lls + size lrl, lk, lx, ll, lrl), Bin (1 + size lrr, k, x, lrr, Tip))
      | balanceL (k, x, Tip, r as Bin (rs, _, _, _, _)) =
          Bin (1 + rs, k, x, Tip, r)
      | balanceL (k, x, l as Bin (ls, lk, lx, ll, lr), r as Bin (rs, _, _, _, _)) =
          if ls > delta * rs
          then case (ll, lr) of
                    (Bin (lls, _, _, _, _), Bin (lrs, lrk, lrx, lrl, lrr)) =>
                        if lrs < ratio * lls then Bin (1 + ls + rs, lk, lx, ll, Bin (1 + rs + lrs, k, x, lr, r))
                        else Bin (1 + ls + rs, lrk, lrx, Bin (1 + lls + size lrl, lk, lx, ll, lrl), Bin (1 + rs + size lrr, k, x, lrr, r))
                  | _ => raise Fail "balanceL: Shouldn't happen"
          else Bin (1 + ls + rs, k, x, l, r)

    fun balanceR (k, x, Tip, Tip) =
          Bin (1, k, x, Tip, Tip)
      | balanceR (k, x, Tip, r as Bin (_, _, _, Tip, Tip)) =
          Bin (2, k, x, Tip, r)
      | balanceR (k, x, Tip, r as Bin (_, rk, rx, Tip, rr as Bin _)) =
          Bin (3, rk, rx, Bin (1, k, x, Tip, Tip), rr)
      | balanceR (k, x, Tip, Bin (_, rk, rx, Bin (_, rlk, rlx, _, _), Tip)) =
          Bin (3, rlk, rlx, Bin (1, k, x, Tip, Tip), Bin (1, rk, rx, Tip, Tip))
      | balanceR (k, x, Tip, r as Bin (rs, rk, rx, rl as Bin (rls, rlk, rlx, rll, rlr), rr as Bin (rrs, _, _, _, _))) =
          if rls < ratio * rrs
          then Bin (1 + rs, rk, rx, Bin (1 + rls, k, x, Tip, rl), rr)
          else Bin (1 + rs, rlk, rlx, Bin (1 + size rll, k, x, Tip, rll), Bin (1 + rrs + size rlr, rk, rx, rlr, rr))
      | balanceR (k, x, l as Bin (ls, _, _, _, _), Tip) =
          Bin (1 + ls, k, x, l, Tip)
      | balanceR (k, x, l as Bin (ls, _, _, _, _), r as Bin (rs, rk, rx, rl, rr)) =
          if rs > delta * ls
          then case (rl, rr) of
                    (Bin (rls, rlk, rlx, rll, rlr), Bin (rrs, _, _, _, _)) =>
                        if rls < ratio * rrs
                        then Bin (1 + ls + rs, rk, rx, Bin (1 + ls + rls, k, x, l, rl), rr)
                        else Bin (1 + ls + rs, rlk, rlx, Bin (1 + ls + size rll, k, x, l, rll), Bin (1 + rrs + size rlr, rk, rx, rlr, rr))
                  | _ => raise Fail "balanceR: Shouldn't happen"
          else Bin (1 + ls + rs, k, x, l, r)

    fun maxViewSure (k, x, l, Tip) = { key = k, value = x, submap = l }
      | maxViewSure (k, x, l, Bin (_, kr, xr, rl, rr)) =
          case maxViewSure (kr, xr, rl, rr) of
               { key, value, submap } => { key = key, value = value, submap = balanceL (k, x, l, submap) }

    fun minViewSure (k, x, Tip, r) = { key = k, value = x, submap = r }
      | minViewSure (k, x, Bin (_, kl, xl, ll, lr), r) =
          case minViewSure (kl, xl, ll, lr) of
               { key, value, submap } => { key = key, value = value, submap = balanceR (k, x, submap, r) }

    fun glue (Tip, r) = Tip
      | glue (l, Tip) = l
      | glue (l as Bin (sl, kl, xl, ll, lr), r as Bin (sr, kr, xr, rl, rr)) =
          if sl > sr
          then let val { key, value, submap } = maxViewSure (kl, xl, ll, lr) in balanceR (key, value, submap, r) end
          else let val { key, value, submap } = minViewSure (kr, xr, rl, rl) in balanceR (key, value, l, submap) end


    val empty = Tip

    fun singleton (k, x) = Bin (1, k, x, Tip, Tip)

    fun lookup (_, Tip) = NONE
      | lookup (k, Bin (_, kx, x, l, r)) =
          case Key.compare (k, kx) of
               LESS => lookup (k, l)
             | GREATER => lookup (k, r)
             | EQUAL => SOME x

    fun insert (k, x, Tip) = singleton (k, x)
      | insert (k, x, Bin (sz, ky, y, l, r)) =
          case Key.compare (k, ky) of
               LESS => balanceL (ky, y, insert (k, x, l), r)
             | GREATER => balanceR (ky, y, l, insert (k, x, r))
             | EQUAL => Bin (sz, k, x, l, r)

    fun delete (_, Tip) = Tip
      | delete (k, t as Bin (_, kx, x, l, r)) =
          case Key.compare (k, kx) of
               LESS => balanceR (kx, x, delete (k, l), r)
             | GREATER => balanceL (kx, x, l, delete (k, r))
             | EQUAL => glue (l, r)

    fun foldri _ acc Tip = acc
      | foldri f acc (Bin (_, kx, x, l, r)) = foldri f (f (kx, x, foldri f acc r)) l

    fun foldli _ acc Tip = acc
      | foldli f acc (Bin (_, kx, x, l, r)) = foldli f (f (kx, x, foldli f acc l)) r

    fun foldr _ acc Tip = acc
      | foldr f acc (Bin (_, kx, x, l, r)) = foldr f (f (x, foldr f acc r)) l

    fun foldl _ acc Tip = acc
      | foldl f acc (Bin (_, kx, x, l, r)) = foldl f (f (x, foldl f acc l)) r

    fun fromList xs = List.foldl (fn ((kx, x), acc) => insert (kx, x, acc)) empty xs

    fun toList m = foldli (fn (k, x, xs) => (k, x) :: xs) [] m
end

