(*
** CAS CS525, Spring 2011
** Instructor: Hongwei Xi
*)

(* ****** ****** *)
//
// Given a natural number n, permute(n) lists all the permutations
// of (1, 2, ..., n) in the lexicographic order.
//
(* ****** ****** *)

staload "prelude/DATS/list.dats"
staload "prelude/DATS/list_vt.dats"
staload "prelude/DATS/list0.dats"

(* ****** ****** *)

fun range {m,n:nat}
  (m:int m, n:int n): list (int, max (n-m+1, 0)) =
  if m <= n then list_cons (m, range (m+1, n)) else list_nil
// end of [range]

fun permute {n:nat}
  (n: int n): List (list (int, n)) = let
  fun{a:t@ype}
  rotout {n:int} {i:nat | i < n}
    (xs: list (a, n), i: int i): (a, list (a, n-1)) = let
    val (xfs, xbs) = list_split_at<a> (xs, i)
    val+ list_cons (x, xbs) = xbs
    val xs = list_append1_vt (xfs, xbs)
  in
    (x, xs)
  end // end of [rotout]
  fun{a:t@ype} perm {n:nat}
    (xs: list (a, n), n: int n): List (list (a, n)) = let
    typedef res_t = List (list (a, n))
  in
    case+ xs of
    | list_cons (x, xs1) => let
        fun loop {i:nat | i < n} (
          xs1: list (a, n-1), i: int i, res: res_t
        ) :<cloref1> res_t =
        if i > 0 then let
          val (x1, xs2) = rotout (xs1, i-1)
          val xs = list_cons (x1, list_cons (x, xs2))
        in
          loop (xs1, i-1, list_cons (xs, res))
        end else
          list_cons (xs, res)
        // end of [if]
        val xss = loop (xs1, n-1, list_nil)
        val f = lam (
          xs: list (a, n)
        ) : res_t =<cloref1> let
          val+ list_cons (x, xs1) = xs
          val xss1 = perm (xs1, n-1)
          val xss =
            list_map_cloref<list(a,n-1)><list(a,n)>
            (xss1, lam (xs1) =<cloref1> list_cons (x, xs1))
          // end of [val]
        in
          list_of_list_vt (xss)
        end // end of [f]
        val xsss = list_map_cloref (xss, f)
        val xsss = list_of_list_vt (xsss)
      in
        list_of_list_vt (list_concat (xsss))
      end // end of [list_cons]
    | list_nil () => list_cons (list_nil, list_nil)
  end // end of [perm]
in
  perm (range (1, n), n)
end // end of [permute]

(* ****** ****** *)

staload "contrib/testing/SATS/fprint.sats"
staload _(*anon*) = "contrib/testing/DATS/fprint.dats"

implement fprint_elt<int> (out, x) = fprint (out, x)

(* ****** ****** *)

implement
main () = () where {
  val xss = permute (5)
  fun f (xs: List (int)): void = let
    val () = list_fprint_elt (stdout_ref, xs, " >> ")
  in
    print_newline ()
  end // end of [val]
  val () = list_foreach_fun (xss, f)
} // end of [main]

(* ****** ****** *)

(* end of [permute.dats] *)
