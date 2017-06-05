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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "libc/CATS/ctype.cats"
%} // end of [%{#]

(* ****** ****** *)

fun isalnum (c: int):<> int = "mac#atslib_isalnum"
fun isalpha (c: int):<> int = "mac#atslib_isalpha"
fun isascii (c: int):<> int = "mac#atslib_isascii"
fun isblank (c: int):<> int = "mac#atslib_isblank"
fun iscntrl (c: int):<> int = "mac#atslib_iscntrl"
fun isdigit (c: int):<> int = "mac#atslib_isdigit"
fun isgraph (c: int):<> int = "mac#atslib_isgraph"
fun islower (c: int):<> int = "mac#atslib_islower"
fun isprint (c: int):<> int = "mac#atslib_isprint"
fun ispunct (c: int):<> int = "mac#atslib_ispunct"
fun isspace (c: int):<> int = "mac#atslib_isspace"
fun isupper (c: int):<> int = "mac#atslib_isupper"
fun isxdigit (c: int):<> int = "mac#atslib_isxdigit"

(* ****** ****** *)

(* end of [ctype.sats] *)
