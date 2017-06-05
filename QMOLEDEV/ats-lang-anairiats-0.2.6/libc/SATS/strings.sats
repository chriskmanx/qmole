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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "libc/CATS/strings.cats"
%} // end of [%{#]

(* ****** ****** *)

/*
// int    bcmp(const void *, const void *, size_t); (LEGACY )
// void   bcopy(const void *, void *, size_t); (LEGACY )
// void   bzero(void *, size_t); (LEGACY )
// int    ffs(int); // find first (least significant) bit set in a word // HX: handled
// char  *index(const char *, int); (LEGACY )
// char  *rindex(const char *, int); (LEGACY )
// int    strcasecmp(const char *, const char *); // HX: handled
// int    strncasecmp(const char *, const char *, size_t); // HX: handled
*/

(* ****** ****** *)
//
// HX:
// find the first (least significant) bit set in a word
// note that position starts from 1 until 32 or 64; 0 is returned
// if no bit is set
//
fun ffs (i: int): [n:int | n >= 0] int n = "mac#atslib_ffs"

(* ****** ****** *)
//
// HX: string comparison like [strcmp] but case is ignored
//
fun strcasecmp (
  str1: !READ(string), str2: !READ(string)
) : int= "mac#atslib_strcasecmp" // end of [strcasecmp]

(* ****** ****** *)
//
// HX: string comparison like [strncmp] but case is ignored
//
fun strncasecmp {n:nat} (
  str1: !READ(string), str2: !READ(string), n: size_t n
) : int = "mac#atslib_strncasecmp" // end of [strncasecmp]

(* ****** ****** *)

(* end of [strings.sats] *)
