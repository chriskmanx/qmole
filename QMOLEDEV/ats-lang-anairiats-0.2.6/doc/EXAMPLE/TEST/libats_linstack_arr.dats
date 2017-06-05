(*
//
// Author: Hongwei Xi (March, 2010)
//
*)

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

staload S = "libats/SATS/linstack_arr.sats"
stadef STACK = $S.STACK
stadef STACK0 = $S.STACK0

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"
staload _(*anon*) = "prelude/DATS/reference.dats"
staload _(*anon*) = "libats/DATS/linstack_arr.dats"
staload _(*anon*) = "libats/ngc/DATS/deque_arr.dats"

(* ****** ****** *)

// dynload "libats/DATS/linstack_arr.dats" // not needed as [ATS_DYNLOADFLAG = 0]

(* ****** ****** *)

#define N 2000000

(* ****** ****** *)

implement main () = let
//
  typedef itm = int
  var S: STACK0 (itm)
  val () = $S.stack_initialize<itm> (S, N)
//
  val cap = $S.stack_cap (S)
  val () = assert_errmsg (cap = N, #LOCATION)
  val size = $S.stack_size (S)
  val () = assert_errmsg (size = 0, #LOCATION)
//
  val () = loop (S, N, 0) where {
    fun loop {i,j:nat | i+j <= N} .<i>.
      (S: &STACK (itm, N, j) >> STACK (itm, N, i+j), i: int i, j: int j): void =
      if i > 0 then let
        val () = $S.stack_insert<itm> (S, j) in loop (S, i-1, j+1)
      end // end of [val]
    // end of [loop]
  } // end of [val]
//
  val () = loop (S, 0) where {
    fun loop {i:nat | i <= N} .<N-i>.
      (S: &STACK (itm, N, N-i) >> STACK (itm, N, 0), i: int i): void =
      if i < N then let
        val x = $S.stack_remove<itm> (S)
        val () = assert_errmsg (x = N-1-i, #LOCATION)
      in
        loop (S, i+1)
      end // end of [val]
    // end of [loop]
  } // end of [val]
//
  val () = $S.stack_uninitialize_vt {itm} (S)
//
in
  print "[libats_linstack_arr.dats] testing passes!\n"
end // end of [main]

(* ****** ****** *)

(* end of [libats_linstack_arr.dats] *)
