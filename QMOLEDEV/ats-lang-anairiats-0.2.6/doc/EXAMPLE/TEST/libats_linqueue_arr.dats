(*
//
// Author: Hongwei Xi (March, 2010)
//
*)

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

staload Q = "libats/SATS/linqueue_arr.sats"
stadef QUEUE = $Q.QUEUE
stadef QUEUE0 = $Q.QUEUE0

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"
staload _(*anon*) = "libats/DATS/linqueue_arr.dats"
staload _(*anon*) = "libats/ngc/DATS/deque_arr.dats"

(* ****** ****** *)

#define CAP 2000000
#define N1 1500000; #define N2 1000000

(* ****** ****** *)

implement
main () = () where {
  typedef itm = int
  var q: QUEUE0 (itm)
//
  val () = $Q.queue_initialize<itm> (q, CAP)
//
  val () = loop (q, N1, 0) where {
    fun loop {i,j:nat | i+j <= CAP} .<i>.
      (q: &QUEUE (itm, CAP, j) >> QUEUE (itm, CAP, i+j), i: int i, j: int j): void =
      if i > 0 then let
        val () = $Q.queue_insert<itm> (q, j) in loop (q, i-1, j+1)
      end // end of [val]
    // end of [loop]
  } // end of [val]
//
  val () = loop (q, 0) where {
    fun loop {i:nat | i <= N2} .<N2-i>.
      (q: &QUEUE (itm, CAP, N1-i) >> QUEUE (itm, CAP, N1-N2), i: int i): void =
      if i < N2 then let
        val x = $Q.queue_remove<itm> (q)
        val () = assert_errmsg (x = i, #LOCATION)
      in
        loop (q, i+1)
      end // end of [val]
    // end of [loop]
  } // end of [val]
//
  val () = loop (q, N2, N1) where {
    fun loop {i,j:nat | i <= N2} .<i>.
      (q: &QUEUE (itm, CAP, N1-i) >> QUEUE (itm, CAP, N1), i: int i, j: int j): void =
      if i > 0 then let
        val () = $Q.queue_insert<itm> (q, j) in loop (q, i-1, j+1)
      end // end of [val]
    // end of [loop]
  } // end of [val]
//
  #define CAP2 N1
  val () = $Q.queue_update_capacity<itm> (q, CAP2)
//
  val () = loop (q, 0) where {
    fun loop {i:nat | i <= N1} .<N1-i>.
      (q: &QUEUE (itm, CAP2, N1-i) >> QUEUE (itm, CAP2, 0), i: int i): void =
      if i < N1 then let
        val x = $Q.queue_remove<itm> (q)
        val () = assert_errmsg (x = N2+i, #LOCATION)
      in
        loop (q, i+1)
      end // end of [val]
    // end of [loop]
  } // end of [val]
//
  val () = $Q.queue_uninitialize_vt {itm} (q)
//
  val () = print "[libats_ngc_queue_arr.dats] testing passes!\n"
//
} // end of [main]

(* ****** ****** *)

(* end of [libats_ngc_queue_arr.dats] *)
