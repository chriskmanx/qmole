(*
** some testing code for functions declared in
** libats/SATS/genarrays.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"

staload _(*anonymous*) = "libats/DATS/fmatrix.dats"
staload _(*anonymous*) = "libats/DATS/genarrays.dats"

(* ****** ****** *)

extern
fun{a:t@ype} fprint_elt (out: FILEref, x: a): void

fn{a:t@ype}
GEMAT_fprint {ord:order} {m,n:nat} {ld:pos} (
    out: FILEref
  , ord: ORDER ord
  , M: &GEMAT (a, m, n, ord, ld), m: size_t m, n: size_t n, ld: size_t ld
  ) : void = () where {
  var !p_clo = @lam (
    pf: !unit_v | i: size_t, j: size_t, x: &a
  ) : void =<clo> $effmask_all let
    val () = if j > 0 then fprint (out, ", ")
    val () = if (i > 0 andalso j = 0) then print "\n" 
    val () = fprint_elt<a> (out, x)
  in
    // empty
  end // end of [val]
  prval pf = unit_v ()
  val () = GEMAT_ptr_iforeach_vclo<a>
    (pf | ord, M, !p_clo, ORDERrow, m, n, ld) where {
  } // end of [val]
  prval unit_v () = pf
} // end of [GEMAT_fprint]

(* ****** ****** *)

dynload "libats/DATS/genarrays.dats"
dynload "libats/DATS/fmatrix.dats"

(* ****** ****** *)

typedef T = int
implement fprint_elt<T> (out, x) = fprintf (out, "%2i", @(x))

#define sz2i int1_of_size1

implement main () = let
  #define X 10
  val (pf1_mat | p1, fp1) = GEMAT_row_ptr_allocfree<T> (X, X)
//
  val () = GEMAT_ptr_initialize_fun<T> (
    ORDERrow, !p1, X, X, X, lam (i, j, x) => x := sz2i i - sz2i j
  ) // end of [val]
//
  val () = print ("M1 =\n")
  val () = GEMAT_fprint (stdout_ref, ORDERrow, !p1, X, X, X)
  val () = print_newline ()
//
  val (pf2_mat | p2, fp2) = GEMAT_row_ptr_allocfree<T> (X, X)
//
  // for testing [GEMAT_ptr_copy]
  val () = GEMAT_ptr_copy<T> (ORDERrow, !p1, !p2, X, X, X, X)
  val () = print ("M2(=M1) =\n")
  val () = GEMAT_fprint (stdout_ref, ORDERrow, !p2, X, X, X)
  val () = print_newline ()
//
  val () = GEMAT_ptr_initialize_elt<T> (ORDERrow, !p2, X, X, X, 10)
  val () = print ("M2 =\n")
  val () = GEMAT_fprint (stdout_ref, ORDERrow, !p2, X, X, X)
  val () = print_newline ()
//
  val ul = UPLOupper and dg = DIAGnonunit
  prval (pf1_trm, fpf1_mat) = TRMAT_v_of_GEMAT_v (pf1_mat, ul, dg)
  prval (pf2_trm, fpf2_mat) = TRMAT_v_of_GEMAT_v (pf2_mat, ul, dg)
  val () = TRMAT_ptr_copy<T> (ORDERrow, ul, dg, !p1, !p2, X, X, X)
  prval () = pf1_mat := fpf1_mat (pf1_trm)
  prval () = pf2_mat := fpf2_mat (pf2_trm)
//
  val ul = UPLOlower and dg = DIAGunit
  prval (pf1_trm, fpf1_mat) = TRMAT_v_of_GEMAT_v (pf1_mat, ul, dg)
  prval (pf2_trm, fpf2_mat) = TRMAT_v_of_GEMAT_v (pf2_mat, ul, dg)
  val () = TRMAT_ptr_copy<T> (ORDERrow, ul, dg, !p1, !p2, X, X, X)
  prval () = pf1_mat := fpf1_mat (pf1_trm)
  prval () = pf2_mat := fpf2_mat (pf2_trm)
//
  val () = print ("M2(=M1) =\n")
  val () = GEMAT_fprint (stdout_ref, ORDERrow, !p2, X, X, X)
  val () = print_newline ()
//
  val () = fp1 (pf1_mat | p1) // freeing M1
  val () = fp2 (pf2_mat | p2) // freeing M2
//
in
  print "[libats_genarrays.dats] testing passes!\n"
end // end of [main]

(* ****** ****** *)

(* end of [libats_genarrays.dats] *)
