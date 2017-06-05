(*
//
// Author: Hongwei Xi (September, 2010)
//
*)

(* ****** ****** *)

staload "libats/SATS/rarray.sats"
staload _(*anon*) = "libats/DATS/rarray.dats"

(* ****** ****** *)

fun{a:viewt@ype}
array_ptr_rforeach_clo {v:view} {n:nat} {l:addr} .<>. (
  pf: !v | A: &(@[a][n]), f: &(!v | &a) -<clo> void, n: size_t n
) :<> void = let
  fun loop {n:nat} {l:addr} .<n>. (
    pf: !v, pfarr: !rarray_v (a, n, l) | p: ptr l, n: size_t n, f: &(!v | &a) -<clo> void
  ) :<> void =
    if n > 0 then let
      prval @(pf1arr, pf1at) = rarray_v_uncons (pfarr)
      val p1 = p - sizeof<a>
      val () = f (pf | !p1)
      val () = loop (pf, pf1arr | p1, n-1, f)
      prval () = pfarr := rarray_v_cons {a} (pf1arr, pf1at)
    in
      // nothing
    end else () // end of [if]
  // end of [loop]
  prval pfarr = view@ (A)
  val (pfmul | ofs) = mul2_size1_size1 (n, sizeof<a>)
  prval pfarr = rarray_v_of_array_v {a} (pfmul, pfarr)
  val () = loop (pf, pfarr | &A+ofs, n, f)
  prval () = view@(A) := array_v_of_rarray_v {a} (pfmul, pfarr)
in
  // nothing
end // end of [array_ptr_rforeach_clo]

(* ****** ****** *)

implement
main () = () where {
  var !p_arr = @[int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  prval pfu = unit_v
  val () = array_ptr_rforeach_clo<int>
    (pfu | !p_arr, !p_clo, 10) where {
    var !p_clo = @lam (pf: !unit_v | x: &int): void =<clo> $effmask_all (print x)
  } // end of [val]
  val () = print_newline ()
  prval unit_v () = pfu
} // end of [main]

(* ****** ****** *)

(* end of [libats_rarray.dats] *)
