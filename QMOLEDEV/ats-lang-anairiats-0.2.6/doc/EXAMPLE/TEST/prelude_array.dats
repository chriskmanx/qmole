(*
** some testing code for functions declared in
** prelude/SATS/array.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009
//

(* ****** ****** *)

// staload "prelude/SATS/array.sats"

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"
staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

implement
main (argc, argv) = let
  val () = () where {
    #define asz 10
    val A = array_make_arrsz
      {int} ($arrsz (0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    // end of [val]
// testing [array_iforeach_fun]
    val () = print "A (0-9) = "
    val () = array_iforeach_fun (A, f, asz) where {
      fn f (
        i: sizeLt asz, x: &int
      ) :<> void = $effmask_all let
        val () = if i > 0 then print ", " in print x
      end // end of [f]
    } // end of [val]
    val () = print_newline ()
// testing [array_iforeach_vclo]
    prval pf = unit_v ()
    val () = print "A (0-9) = "
    val () = array_iforeach_vclo {unit_v} (pf | A, !p_f, asz) where {
      var !p_f = @lam
        (pf: !unit_v | i: sizeLt asz, x: &int): void =<clo>
        $effmask_all (
          let val () = if i > 0 then print ", " in print x end
        ) // end of [$effmask_all]
    } // end of [val]
    val () = print_newline ()
    prval unit_v () = pf
  } // end of [val]
//
  val () = () where {
//
// HX: testing array_v_group and array_v_ungroup
//
    typedef T = int
    #define M 2; #define N 5; #define MN %(M*N)
    prval pfmn = mul_make {M,N} ()
    var !parr with pfarr = @[T][MN](0)
    prval pfarr2 = array_v_group {T} (pfmn, pfarr)
//
    val [l0:addr] (pf, fpf | p0) = array2_ptr_takeout<T> (pfarr2 | parr, 0, N)
    prval pfout = $UN.vtakeout {array_v (T, N, l0)} (pf)
    prval (pf0, fpf0) = $UN.viewout_decode (pfout)
    prval () = pfarr2 := fpf (pf)
    val [l1:addr] (pf, fpf | p1) = array2_ptr_takeout<T> (pfarr2 | parr, 1, N)
    prval pfout = $UN.vtakeout {array_v (T, N, l1)} (pf)
    prval (pf1, fpf1) = $UN.viewout_decode (pfout)
    prval () = pfarr2 := fpf (pf)
//
    val () = array_ptr_iforeach_fun<T> (!p0, f, N) where {
      val f = lam (i: sizeLt N, x: &T): void =<fun> x := int1_of_size1 (i)
    } // end of [val]
    prval () = fpf0 (pf0)
    val () = array_ptr_iforeach_fun<T> (!p1, f, N) where {
      val f = lam (i: sizeLt N, x: &T): void =<fun> x := int1_of_size1 (i+N)
    } // end of [val]
    prval () = fpf1 (pf1)
//
    prval () = pfarr := array_v_ungroup {T} (pfmn, pfarr2)
//
    val () = array_ptr_iforeach_fun<T> (!parr, f, MN) where {
      val f = lam (i: sizeLt MN, x: &T): void =<fun> $effmask_all (if i > 0 then print ","; print x)
    } // end of [val]
    val () = print_newline ()
  } // end of [val]
//
in
  print "The run of [prelude_array.dats] is done successfully!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_array.dats] *)
