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

// HX: note that there is also a [printf.sats] in $ATSHOME/prelude/SATS

(* ****** ****** *)

%{#
#include "libc/CATS/printf.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// HX: [printf] and [fprintf] are
// implemented in $ATSHOME/libc/DATS/printf.dats
// based on [vprintf] and [vfprintf]
//

fun printf
  {ts:types} (fmt: printf_c ts, arg: ts): int
// end of [prinf]

fun fprintf
  {m:file_mode} {ts:types} (
    pf_mod: file_mode_lte (m, w)
  | out: &FILE m, fmt: printf_c ts, arg: ts
  ) : int // HX: [fprintf] is implemented in ATS
// end of [vfprinf]

(* ****** ****** *)

fun vprintf {ts:types}
  (fmt: printf_c ts, arg: &va_list (ts) >> va_list): int
  = "atslib_vprintf" // function!
// end of [vprintf]

fun vfprintf
  {m:file_mode} {ts:types} (
    pf_mod: file_mode_lte (m, w)
  | out: &FILE m, fmt: printf_c ts, arg: &va_list (ts) >> va_list
  ) : int = "atslib_vfprintf" // function!
// end of [vfprinf]

(* ****** ****** *)

(*
//
// HX: it is correct but too detailed!
//
fun snprintf {ts:types} {m1,m2:nat | m2 <= m1} {l:addr} (
    pf: &(b0ytes m1 @ l) >> strbuf (m1, n1) @ l
  | p: ptr l, m2: size_t m2, fmt: printf_c ts, arg: ts)
  : #[n1,n2:nat | (m2 > n2 && n1 == n2) || (n2 >= m2 && n1+1 == m2)] int n2
  = "atspre_snprintf"
*)
//
// HX: implemented in $ATSHOME/libc/DATS/printf.dats
//
fun snprintf {ts:types}
  {m1,m2:nat | m2 <= m1} {l:addr} (
    pf: !array_v(byte?, m1, l) >> strbuf (m1, n1) @ l
  | p: ptr l, m2: size_t m2, fmt: printf_c ts, arg: ts
  ) :<> #[n1:nat | n1 < m2] [n2:nat] int n2
  = "atslib_snprintf"
// end of [snprintf]

(* ****** ****** *)
//
// HX: implemented in $ATSHOME/libc/CATS/printf.cats
//
fun vsnprintf {ts:types}
  {m1,m2:nat | m2 <= m1} {l:addr} (
    pf: !array_v (byte?, m1, l) >> strbuf (m1, n1) @ l
  | p: ptr l, m2: size_t m2
  , fmt: printf_c ts, arg: &va_list (ts) >> va_list
  ) :<> #[n1:nat | n1 < m2] [n2:nat] int n2
  = "atslib_vsnprintf"
// end of [vsnprintf]

(* ****** ****** *)

(* end of [printf.sats] *)
