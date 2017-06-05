(*
** some testing code for functions declared in
** libats/smlbas/SATS/array.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "libats/smlbas/SATS/array.sats"

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/array0.dats"
staload _(*anonymous*) = "prelude/DATS/list.dats"

staload _(*anonymous*) = "libats/smlbas/DATS/array.dats"

(* ****** ****** *)

dynload "libats/smlbas/DATS/array.dats"

(* ****** ****** *)

#define :: list0_cons
#define cons list0_cons
#define nil list0_nil

(* ****** ****** *)

implement main () = () where {
//
// Let the test begin!
(*
  val () = begin // [maxLen] is undefined!
    print "maxLen = "; print (maxLen ()); print_newline ()
  end // end of [val]  
*)
  val A0 = array<int> (10(*asz*), 5)
  val () = app<int> (lam (x) => (print x), A0)
  val () = print_newline ()
//
  val A1 = fromList<int> (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: nil)
  val () = app<int> (lam (x) => (print x), A1)
  val () = print_newline ()
//
  val () = copy (A1, A0, 0)
  val () = app<int> (lam (x) => (print x), A0)
  val () = print_newline ()
//
  val () = modify<int> (lam (x) => 9 - x, A1) 
  val () = app<int> (lam (x) => (print x), A1)
  val () = print_newline ()
//
  val () = modifyi<int> (lam (i, x) => x + int_of_size i, A1) 
  val () = app<int> (lam (x) => (print x), A1)
  val () = print_newline ()
//
  val A2 = tabulate<int> (10, lam (i) => int_of_size (i * i))
  val () = appi (f, A2) where {
    macdef _0 = size_of_int1 (0)
    fn f (i: size_t, x: int):<cloref1> void = let
      val () = if i > _0 then print ", " in print x
    end (* end of [f] *)
  } // end of [val]
  val () = print_newline ()
//
  val () = assert (collate<int> (lam (x, y) => compare (x, y), A2, A2) = 0)
  val A3 = fromList<int> (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: nil)
  val () = assert (collate<int> (lam (x, y) => compare (x, y), A2, A3) > 0)
  val () = assert (collate<int> (lam (x, y) => compare (x, y), A3, A2) < 0)
//  
} // end of [main]

(* ****** ****** *)

(* end of [libats_smlbas_array.dats] *)
