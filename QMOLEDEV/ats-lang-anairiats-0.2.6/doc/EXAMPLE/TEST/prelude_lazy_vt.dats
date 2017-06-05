(*
// some testing code for functions declared in
// prelude/SATS/lazy_vt.sats
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/lazy_vt.dats"

(* ****** ****** *)

staload "contrib/testing/SATS/randgen.sats"
staload _(*anon*) = "contrib/testing/DATS/randgen.dats"
staload "contrib/testing/SATS/fprint.sats"
staload _(*anon*) = "contrib/testing/DATS/fprint.dats"

(* ****** ****** *)

typedef T = int

(* ****** ****** *)

#define N 10
implement randgen<T> () = $RAND.randint (N)
implement fprint_elt<T> (out, x) = fprint (out, x)

(* ****** ****** *)

implement
main () = () where {
//
  val () = $RAND.srand48_with_time ()
//
  #define L1 2
  val xs1 = list_vt_randgen<T> (L1)
  val () = (
    print "xs1 = "; list_vt_fprint_elt (stdout_ref, xs1, " -> "); print_newline ()
  ) // end of [val]
//
  #define L2 3
  val xs2 = list_vt_randgen<T> (L2)
  val () = (
    print "xs2 = "; list_vt_fprint_elt (stdout_ref, xs2, " -> "); print_newline ()
  ) // end of [val]
//
  typedef T2 = (T, T)
  implement fprint_elt<T2> (out, xx) = fprintf (out, "(%i, %i)", @(xx.0, xx.1))
//
  val xxs = stream_vt_of_lstlstprod<T,T> (xs1, xs2)
//
  #define L3 2
  val xs3 = list_vt_randgen<T> (L3)
  val () = (
    print "xs3 = "; list_vt_fprint_elt (stdout_ref, xs3, " -> "); print_newline ()
  ) // end of [val]
//
  typedef T3 = (T2, T)
  implement fprint_elt<T3> (out, xxx) = fprintf (out, "(%i, %i, %i)", @((xxx.0).0, (xxx.0).1, xxx.1))
//
  val xxxs = stream_vt_of_strmlstprod<T2,T> (xxs, xs3)
//
  val (n, xxxs) = list_vt_of_stream_vt (xxxs)
  val () = assertloc (n = L1*L2*L3)
  val () = (
    print "xxxs = "; list_vt_fprint_elt (stdout_ref, xxxs, " -> "); print_newline ()
  ) // end of [val]
//
  val () = list_vt_free (xxxs)
//
} // end of [val]

(* ****** ****** *)

(* end of [prelude_lazy_vt.dats] *)
