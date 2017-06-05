(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [memalign.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(*
//
// HX-2011-09-20: do we need this?
//
stadef
align_int_int_bool
  (sz:int, n:int) = n >= 0
stacst alignof_viewt0ype_int : viewt@ype -> int
stadef alignof = alignof_viewt0ype_int
praxi lemma_alignof_sizeof
  {a:viewt@ype} (): [n:pos] MUL (n, alignof(a), sizeof(a))
// end of [lemma_alignof_sizeof]
*)

(* ****** ****** *)
//
// HX: [a] is aligned at [l]
// this needs support from the underlying C compiler
//
absprop ISALIGNED (a:t@ype+, l:addr)
//
(* ****** ****** *)

praxi
atview_isaligned
  {a:viewt@ype} {l:addr} (pf: !a @ l): ISALIGNED (a?, l)
// end of [atview_isaligned]

(* ****** ****** *)

praxi
isaligned_sizeof {a:t@ype} {i:int} // HX: [i] is a constant
  {l:addr} (pf: ISALIGNED (a, l)): ISALIGNED (a, l+i*sizeof(a))
// end of [isaligned_sizeof]

(* ****** ****** *)

(*
** MB: C99 (section 7.20.3.3)
*)

praxi
freebyte_gc_isaligned
  {a:viewt@ype} {n:int} {l:addr} (
  pf: !freebyte_gc_v (n, l)
) : ISALIGNED (a?, l) // end of [freebyte_gc_isaligned]

praxi
freebyte_ngc_isaligned
  {a:viewt@ype} {n:int} {l:addr} (
  pf: !freebyte_ngc_v (n, l)
) : ISALIGNED (a?, l) // end of [freebyte_ngc_isaligned]

(* ****** ****** *)

(*
** MB: C99 (section 6.2.5-6)
*)

praxi
memalign_signed_unsigned_int {l:addr}
  (pf: ISALIGNED (int?, l)): ISALIGNED (uint, l)
praxi
memalign_signed_unsigned_int8 {l:addr}
  (pf: ISALIGNED (int8?, l)): ISALIGNED (uint8, l)
praxi
memalign_signed_unsigned_int16 {l:addr}
  (pf: ISALIGNED (int16?, l)): ISALIGNED (uint16, l)
praxi
memalign_signed_unsigned_int32 {l:addr}
  (pf: ISALIGNED (int32?, l)): ISALIGNED (uint32, l)
praxi
memalign_signed_unsigned_int64 {l:addr}
  (pf: ISALIGNED (int64?, l)): ISALIGNED (uint64, l)

(* ****** ****** *)

praxi
memalign_signed_unsigned_uint {l:addr}
  (pf: ISALIGNED (uint?, l)): ISALIGNED (int, l)
praxi
memalign_signed_unsigned_uint8 {l:addr}
  (pf: ISALIGNED (uint8?, l)): ISALIGNED (int8, l)
praxi
memalign_signed_unsigned_uint16 {l:addr}
  (pf: ISALIGNED (uint16?, l)): ISALIGNED (int16, l)
praxi
memalign_signed_unsigned_uint32 {l:addr}
  (pf: ISALIGNED (uint32?, l)): ISALIGNED (int32, l)
praxi
memalign_signed_unsigned_uint64 {l:addr}
  (pf: ISALIGNED (uint64?, l)): ISALIGNED (int64, l)

(* ****** ****** *)

praxi ptr_of_b0ytes_v
  {a:viewt@ype} {l:addr}
  (pf1: ISALIGNED (a?, l), pf2: b0ytes (sizeof a) @ l):<prf> a? @ l
// end of [ptr_of_b0ytes_v]

(* ****** ****** *)

prfun ptr_view_conversion
  {a1,a2:viewt@ype | sizeof a1 == sizeof a2} {l:addr}
  (pf1: ISALIGNED (a2?, l), pf2: !a1? @ l >> a2? @ l):<prf> void
// end of [ptr_view_conversion]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [memalign.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [memalign.sats] *)
