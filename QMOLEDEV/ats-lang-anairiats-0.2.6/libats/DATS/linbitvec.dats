(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

staload "libats/SATS/linbitvec.sats"

(* ****** ****** *)

implement bitvec_isnot_nil (vec, n) = ~bitvec_is_nil (vec, n)
implement bitvec_isnot_all (vec, n) = ~bitvec_is_all (vec, n)

(* ****** ****** *)

implement bitvec_notequal (vec1, vec2, n) = ~bitvec_equal (vec1, vec2, n)

(* ****** ****** *)

%{^
//
// declared in [string.h]
//
#ifndef memset
extern void *memset (void *buf, int chr, size_t n) ;
#endif
//
ats_ptr_type
atslib_linbitvec_bitvec_make
  (ats_size_type nbit) {
  uintptr_t *p_vec ; size_t nwrd ;
  nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  p_vec = ATS_CALLOC (nwrd, NBYTE_PER_WORD) ; // initialized to zero
  return p_vec ;
} // end of [atslib_linbitvec_bitvec_make]
//
ats_ptr_type // HX: same as [atslib_linbitvec_bitvec_make] for now
atslib_linbitvec_bitvec_make_nil
  (ats_size_type nbit) {
  uintptr_t *p_vec ; size_t nwrd ;
  nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  p_vec = ATS_CALLOC (nwrd, NBYTE_PER_WORD) ; // initialized to zero
  return p_vec ;
} // end of [atslib_linbitvec_bitvec_make_nil]
//
ats_ptr_type
atslib_linbitvec_bitvec_make_all
  (ats_size_type nbit) {
  uintptr_t *p_vec, zc; size_t nwrd ; int next ;
  nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  next = (nwrd << NBIT_PER_WORD_LOG) - nbit ; // extra bits
  p_vec = ATS_CALLOC (nwrd, NBYTE_PER_WORD) ; // initialized to zero
  memset (p_vec, 0xFF, nwrd * NBYTE_PER_WORD) ;
/*
** extra bits, which are in the front, must be set to zero!!!
*/
  if (nwrd > 0) { zc = ~0; p_vec[nwrd-1] &= (zc >> next) ; }
  return p_vec ;
} // end of [atslib_linbitvec_bitvec_make_all]
//
ats_void_type
atslib_linbitvec_bitvec_free
  (ats_ptr_type p_vec) { ATS_FREE (p_vec) ; return ; }
// end of [atslib_linbitvec_bitvec_free]
//
%} // end of [%{^]

(* ****** ****** *)

%{^
//
ats_bool_type
atslib_linbitvec_bitvec_is_nil (
  ats_ptr_type p0, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p = p0 ;
  if (!nwrd) return ats_true_bool ;
  if (*p != 0) return ats_false_bool ;
  while (--nwrd > 0) { if (*++p != 0) return ats_false_bool ; }
  return ats_true_bool ;
} // end of [atslib_linbitvec_bitvec_is_nil]
//
ats_bool_type
atslib_linbitvec_bitvec_is_all (
  ats_ptr_type p0, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  int next = (nwrd << NBIT_PER_WORD_LOG) - nbit ; // extra bits
  uintptr_t *p = p0, zc = ~0 ;
  while (nwrd > 1) {
    if (*p != zc) return ats_false_bool ; --nwrd ; ++p ;
  } ;
/*
** extra bits, which are in the front, must be zero!!!
*/
  if (nwrd) { if (*p != (zc >> next)) return ats_false_bool ; } ;
  return ats_true_bool ;  
} // end of [atslib_linbitvec_bitvec_is_all]
//
%} // end of [%{^]

(* ****** ****** *)

%{^
//
ats_bool_type
atslib_linbitvec_bitvec_equal (
  ats_ptr_type p10, ats_ptr_type p20, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p1 = p10, *p2 = p20 ;
  if (!nwrd) return ats_true_bool ;
  if (*p1 != *p2) return ats_false_bool ;
  while (--nwrd > 0) { if (*++p1 != *++p2) return ats_false_bool ; } ;
  return ats_true_bool ;  
} // end of [atslib_linbitvec_bitvec_copy]
//
%} // end of [%{^]

(* ****** ****** *)

%{^
//
ats_void_type
atslib_linbitvec_bitvec_copy (
  ats_ptr_type p10, ats_ptr_type p20, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p1 = p10, *p2 = p20 ;
  if (!nwrd) return ;
  *p1 = *p2 ; while (--nwrd > 0) { *(++p1) = *(++p2) ; }
  return ;  
} // end of [atslib_linbitvec_bitvec_copy]
//
%} // end of [%{^]

(* ****** ****** *)

%{^
//
ats_void_type
atslib_linbitvec_bitvec_neg (
  ats_ptr_type p0, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  int next = (nwrd << NBIT_PER_WORD_LOG) - nbit ; // extra bits
  uintptr_t *p = p0, zc = ~0 ;
  while (nwrd > 1) {
    *p = ~(*p) ; --nwrd ; ++p ;
  }
/*
** extra bits, which are in the front, must be set to zero!!!
*/
  if (nwrd > 0) { *p = ~(*p) ; *p &= (zc >> next) ; }
  return ;  
} // end of [atslib_linbitvec_bitvec_neg]
//
ats_void_type
atslib_linbitvec_bitvec_or (
  ats_ptr_type p10, ats_ptr_type p20, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p1 = p10, *p2 = p20 ;
  if (!nwrd) return ;
  *p1 |= *p2 ; while (--nwrd > 0) { *(++p1) |= *(++p2) ; }
  return ;  
} // end of [atslib_linbitvec_bitvec_or]
//
ats_void_type
atslib_linbitvec_bitvec_and (
  ats_ptr_type p10, ats_ptr_type p20, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p1 = p10, *p2 = p20 ;
  if (!nwrd) return ;
  *p1 &= *p2 ; while (--nwrd > 0) { *(++p1) &= *(++p2) ; }
  return ;  
} // end of [atslib_linbitvec_bitvec_and]
//
ats_void_type
atslib_linbitvec_bitvec_xor ( // symmetric difference
  ats_ptr_type p10, ats_ptr_type p20, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p1 = p10, *p2 = p20 ;
  if (!nwrd) return ;
  *p1 ^= *p2 ; while (--nwrd > 0) { *(++p1) ^= *(++p2) ; }
  return ;  
} // end of [atslib_linbitvec_bitvec_xor]
//
ats_void_type
atslib_linbitvec_bitvec_diff ( // difference
  ats_ptr_type p10, ats_ptr_type p20, ats_size_type nbit
) {
  int nwrd = (nbit + NBIT_PER_WORD - 1) >> NBIT_PER_WORD_LOG ;
  uintptr_t *p1 = p10, *p2 = p20 ;
  if (!nwrd) return ;
  *p1 &= ~(*p2) ; while (--nwrd > 0) { *(++p1) &= ~(*(++p2)) ; }
  return ;  
} // end of [atslib_linbitvec_bitvec_diff]
//
%} // end of [%{^]

(* ****** ****** *)

(* end of [linbitvec.dats] *)
