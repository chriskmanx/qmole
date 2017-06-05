(*
** some testing code for functions declared in
** libats/smlbas/SATS/array.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "libats/smlbas/SATS/array2.sats"

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list0.dats"

staload _(*anonymous*) = "libats/smlbas/DATS/array2.dats"

(* ****** ****** *)

dynload "libats/smlbas/DATS/array2.dats"

(* ****** ****** *)

#define :: list0_cons
#define cons list0_cons
#define nil list0_nil

(* ****** ****** *)

implement main () = () where {
//
// Let the test begin!
//
//
  val M = fromList xss where {
    val xs1 = 0 :: 1 :: 2 :: nil ()
    val xs2 = 3 :: 4 :: 5 :: nil ()  
    val xs3 = 6 :: 7 :: 8 :: nil ()  
    val xss = xs1 :: xs2 :: xs3 :: nil ()
  } // end of [val]
  val () = app<int> (RowMajor, lam x => print x, M)
  val () = print_newline ()
  val () = app<int> (ColMajor, lam x => print x, M)
  val () = print_newline ()
//
  val () = modify<int> (RowMajor, lam x => x + 1, M)
  val () = app<int> (RowMajor, lam x => print x, M)
  val () = print_newline ()
  val () = app<int> (ColMajor, lam x => print x, M)
  val () = print_newline ()
//
  val M_r = tabulate<int> (RowMajor, 3, 3, f) where {
    val f = lam
      (i: size_t, j: size_t): int =<cloref1> let
      val i = int_of_size i and j = int_of_size j
      val () = printf ("i = %i and j = %i\n", @(i, j)) 
    in
      i + j
    end // end of [val]
  } (* end of [val] *)
//  
  val M_c = tabulate<int> (ColMajor, 3, 3, f) where {
    val f = lam
      (i: size_t, j: size_t): int =<cloref1> let
      val i = int_of_size i and j = int_of_size j
      val () = printf ("i = %i and j = %i\n", @(i, j)) 
    in
      i + j
    end // end of [val]
  } (* end of [val] *)
//  
} // end of [main]

(* ****** ****** *)

(* end of [libats_smlbas_array.dats] *)
