(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload "libc/sys/SATS/time.sats" // for gettimeofday
staload "libc/sys/SATS/types.sats" // for several lint_of* functions

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

implement
srand48_with_gettimeofday () = let
  var tv: timeval?
  val err = gettimeofday (tv)
  val () = if err = 0 then let
    prval () = opt_unsome {timeval} (tv)
    val seed = (lint_of)tv.tv_sec * 1000000L + (lint_of)tv.tv_usec
    val () = srand48 (seed)
  in
    // nothing
  end else let
    prval () = opt_unnone {timeval} (tv)
  in
    // nothing
  end // end of [if]
in
  err (* 0/-1 : succ/fail *)
end // end of [srand48_with_gettimeofday]

(* ****** ****** *)

implement
randint {n} (n) = let
  val d01 = drand48 ()
  val r = int_of (d01 * n)
  val [r:int] r = int1_of (r)
  prval () = __assert () where {
    extern prfun __assert (): [0 <= r; r <= n] void
  } // end of [prval]
in
  if r < n then r else 0
end // end of [randint]

local

staload UN = "prelude/SATS/unsafe.sats"

in // in of [local]

implement
randsize {n} (n) = let
  val d01 = drand48 ()
  val r = ullint_of_double (d01 * (double_of_size)n)
  val r = $UN.cast2size (r)
  val [r:int] r = size1_of_size (r)
  prval () = __assert () where {
    extern prfun __assert (): [0 <= r; r <= n] void
  } // end of [prval]
in
  if r < n then r else (size1_of_int1)0
end // end of [randsize]

end // end of [local]

(* ****** ****** *)

implement
randperm {n} (n) = let
  typedef elt = natLt (n)
  val asz = size1_of_int1 (n)
  val (pfgc, pfarr | p) = array_ptr_alloc<int> (asz)
  val () = loop (pfarr | n, p, 0) where {
    fun loop {i:nat | i <= n} {l:addr} .<n-i>. (
      pfarr: !array_v (int?, n-i, l) >> array_v (elt, n-i, l)
    | n: int n, p: ptr l, i: int i
    ) :<> void =
      if i < n then let
        prval (pfat1, pfarr2) = array_v_uncons {int?} (pfarr)
        val () = !p := i
        val () = loop (pfarr2 | n, p + sizeof<int>, i+1)
        prval () = pfarr := array_v_cons {elt} (pfat1, pfarr2)
      in
        // nothing
      end else let
        prval () = array_v_unnil {int?} (pfarr)
        prval () = pfarr := array_v_nil {elt} ()
      in
        // nothing
      end // end of [if]
    // end of [loop]
  } // end of [val]
  val () = loop (pfarr | n, p, 0) where {
    fun loop {i:nat | i <= n} {l:addr} .<n-i>. (
      pfarr: !array_v (elt, n-i, l) | n: int n, p: ptr l, i: int i
    ) :<!ref> void =
      if n - i >= 2 then let
        val k = randint (n-i)
        val () = array_ptr_exch__intsz (!p, 0, k)
        prval (pfat1, pfarr2) = array_v_uncons {elt} (pfarr)
        val () = loop (pfarr2 | n, p+sizeof<int>, i+1)
        prval () = pfarr := array_v_cons {elt} (pfat1, pfarr2)
      in
        // nothing
      end // end of [if]
    // end of [loop]
  } // end of [val]
in
  (pfgc, pfarr | p)
end // end of [randperm]

(* ****** ****** *)

implement
randint_r {n}
  (buf, n, res) = let
  var d01: double
  val _0 = drand48_r (buf, d01)
  val r = int_of (d01 * n)
  val [r:int] r = int1_of (r)
  prval () = __assert () where {
    extern prfun __assert (): [0 <= r; r <= n] void
  } // end of [prval]
in
  if r < n then res := r else res := 0
end // end of [randint_r]

local

staload UN = "prelude/SATS/unsafe.sats"

in // in of [local]

implement
randsize_r {n}
  (buf, n, res) = let
  var d01: double
  val _0 = drand48_r (buf, d01)
  val r = ullint_of (d01 * (double_of_size)n)
  val r = $UN.cast2size (r)
  val [r:int] r = size1_of_size (r)
  prval () = __assert () where {
    extern prfun __assert (): [0 <= r; r <= n] void
  } // end of [prval]
in
  if r < n then res := r else res := (size1_of_int1)0
end // end of [randint_r]

end // end of [local]

(* ****** ****** *)

(* end of [random.dats] *)
