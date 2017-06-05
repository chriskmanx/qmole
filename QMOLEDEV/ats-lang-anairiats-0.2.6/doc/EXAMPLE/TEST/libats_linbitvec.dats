(*
** some testing code for functions declared in
** libats/SATS/linbitvec.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: the 1st of August, 2010
//

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

staload "libats/SATS/linbitvec.sats"

(* ****** ****** *)

fun bitvec_make_rand
  {n:nat} (
  n: size_t n
) : [l:addr] (
  free_gc_v l, BITVEC n @ l | ptr l
) = let
  val (pf_gc, pf_vec | p_vec) = bitvec_make (n)
  val () = loop (!p_vec, n, 0) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (vec: &BITVEC n, n: size_t n, i: size_t i):<!ref> void =
      if i < n then let
        val b = $RAND.randint (2)
        val () = bitvec_set_at (vec, i, b)
        val b1 = bitvec_get_at (vec, i)
(*
        val () = begin
          prerr "bitvec_make_rand: i = "; prerr i; prerr_newline ()
        end
        val () = begin
          prerr "bitvec_make_rand: b = "; prerr b; prerr_newline ()
        end
        val () = begin
          prerr "bitvec_make_rand: b1 = "; prerr b1; prerr_newline ()
        end
*)
        val () = $effmask_exn (assert (b = b1))
      in
        loop (vec, n, i+1)
      end // end of [if]
    // end of [loop]
  } // end of [val]
in
  (pf_gc, pf_vec | p_vec)
end // end of [bitvec_make_rand]

(* ****** ****** *)

fun print_bitvec {n:nat}
  (v: &BITVEC n, n: size_t n)
  : void = loop (v, n, 0) where {
  fun loop {i:nat | i <= n} .<n-i>.
    (v: &BITVEC n, n: size_t n, i: size_t i): void =
    if i < n then let
      val () = if i > 0 then print ","
      val () = print (bitvec_get_at (v, i))
    in
      loop (v, n, i+1)
    end // end of [if]
} // end of [print_bitvec]

(* ****** ****** *)

#define N 41
#define N1 0
#define N2 32
//
val (pf0_gc, pf0_vec | p0_vec) = bitvec_make_rand (N)
val () = print "vec0 = "
val () = print_bitvec (!p0_vec, N)
val () = print_newline ()
//
val (pf1_gc, pf1_vec | p1_vec) = bitvec_make (N)
val () = bitvec_copy (!p1_vec, !p0_vec, N)
val () = bitvec_xor (!p1_vec, !p0_vec, N)
val () = print "vec1 = "
val () = print_bitvec (!p1_vec, N)
val () = print_newline ()
val () = assert (bitvec_is_nil (!p1_vec, N))
//
val () = bitvec_neg (!p1_vec, N)
val () = print "vec1 = "
val () = print_bitvec (!p1_vec, N)
val () = print_newline ()
val () = assert (bitvec_is_all (!p1_vec, N))
//
val (pf1_gc, pf1_vec | p1_vec) = bitvec_make (N)
val () = bitvec_copy (!p1_vec, !p0_vec, N)
val () = bitvec_neg (!p1_vec, N)
val () = bitvec_and (!p1_vec, !p0_vec, N)
val () = print "vec1 = "
val () = print_bitvec (!p1_vec, N)
val () = print_newline ()
val () = assert (bitvec_is_nil (!p1_vec, N))
//
val (pf1_gc, pf1_vec | p1_vec) = bitvec_make (N)
val () = bitvec_copy (!p1_vec, !p0_vec, N)
val () = bitvec_neg (!p1_vec, N)
val () = bitvec_or (!p1_vec, !p0_vec, N)
val () = print "vec1 = "
val () = print_bitvec (!p1_vec, N)
val () = print_newline ()
val () = assert (bitvec_is_all (!p1_vec, N))
//
val (pf2_gc, pf2_vec | p2_vec) = bitvec_make_rand (N)
val () = print "vec2 = "
val () = print_bitvec (!p2_vec, N)
val () = print_newline ()
//
val (pf01_gc, pf01_vec | p01_vec) = bitvec_make_rand (N)
val () = bitvec_copy (!p01_vec, !p0_vec, N)
val () = print "vec01 = "
val () = print_bitvec (!p01_vec, N)
val () = print_newline ()
//
val () = bitvec_diff (!p01_vec, !p2_vec, N)
//
val (pf21_gc, pf21_vec | p21_vec) = bitvec_make_rand (N)
val () = bitvec_copy (!p21_vec, !p2_vec, N)
val () = print "vec21 = "
val () = print_bitvec (!p21_vec, N)
val () = print_newline ()
//
val () = bitvec_diff (!p21_vec, !p0_vec, N)
//
val () = bitvec_or (!p21_vec, !p01_vec, N)
//
val () = bitvec_xor (!p0_vec, !p2_vec, N)
val () = assert (bitvec_equal (!p0_vec, !p21_vec, N))

(* ****** ****** *)

implement main () = () where {
  val () = print "[prelude_linbitvec.dats] testing passes!\n"
} // end of [main]

(* ****** ****** *)

(* end of [libats_linbitvec.dats] *)
