(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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

(* Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "libc/CATS/stdarg.cats"
%} // end of [%{#]

(* ****** ****** *)

(*
// this is declared in $ATSHOME/prelude/basics_sta.sats
absviewt@ype va_list (ts: types) = $extype"ats_va_list_viewtype"
*)

absviewt@ype va_list1 (t: t@ype, ts: types) = va_list (ts)

(* ****** ****** *)

typedef
va_arg_type (t:t@ype) =
  {ts:types} (&va_list1 (t, ts) >> va_list ts) -<> t
// end of [va_arg_type]

fun{t:t@ype} va_arg : va_arg_type (t)

fun va_arg_int : va_arg_type (int) = "atslib_va_arg_int"
fun va_arg_ptr : va_arg_type (ptr) = "atslib_va_arg_ptr"

fun va_arg_bool : va_arg_type (bool) = "atslib_va_arg_bool"
fun va_arg_char : va_arg_type (char) = "atslib_va_arg_char"

fun va_arg_string : va_arg_type (string) = "atslib_va_arg_ptr"

(* ****** ****** *)

(*
fun va_start ... // this one is built-in
*)

fun va_end (ap: &va_list >> va_list?):<> void = "atslib_va_end"

(* ****** ****** *)

praxi va_clear {ts:types}
  (arg: &va_list (ts) >> va_list):<> void
// end of [va_clear]

fun va_copy {ts:types}
  (dst: &va_list? >> va_list ts, src: va_list ts):<> void
  = "atslib_va_copy"
// end of [va_copy]

(* ****** ****** *)

(*
HX-2010-10-01: the following functions are in libc/printf
vprintf
vfprintf
vsnprint
*)

(* ****** ****** *)

(* end of [stdarg.sats] *)
