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

%{#
#include "libc/CATS/stdlib.cats"
%} // end of [%{#]

(* ****** ****** *)

staload FCNTL = "libc/SATS/fcntl.sats" // for [mkstemp]
stadef open_v = $FCNTL.open_v

(* ****** ****** *)

macdef EXIT_SUCCESS = $extval (int, "EXIT_SUCCESS")
macdef EXIT_FAILURE = $extval (int, "EXIT_FAILURE")

(* ****** ****** *)

fun atoi (inp: !READ(string)):<> int = "mac#atslib_atoi"
fun atof (inp: !READ(string)):<> double = "mac#atslib_atof"
fun atol (inp: !READ(string)):<> lint = "mac#atslib_atol"
fun atoll (inp: !READ(string)):<> llint = "mac#atslib_atoll"

(* ****** ****** *)

fun strtoi_errnul (
  inp: !READ(string), base: intBtw (2, 36+1)
) :<> int = "mac#atslib_strtoi_errnul"

fun strtol_errnul (
  inp: !READ(string), base: intBtw (2, 36+1)
) :<> lint = "mac#atslib_strtol_errnul"

fun strtoll_errnul (
  inp: !READ(string), base: intBtw (2, 36+1)
) :<> llint = "mac#atslib_strtoll_errnul"

(* ****** ****** *)

fun getenv (
  name: !READ(string)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "mac#atslib_getenv"
// end of [getenv]

//
// HX-201-09-29:
// [nameval] is copied and put into the environment.
// potential memory leak!!! 
//
fun putenv {l:agz} (nameval: !strptr l): int // 0/nz : succ/fail

//
// HX-2010-09-29:
// [name] and [value] are copied into the environment
// also note that the original value may be leaked out!!!
//
fun setenv ( // 0/-1 : succ/fail
  name: !READ(string), value: !READ(string), overwrite: int
) : int = "mac#atslib_setenv" // end of [atslib_setenv]

fun unsetenv
  (name: !READ(string)): int = "mac#atslib_unsetenv" // 0/-1: succ/fail
// end of [unsetenv]

(* ****** ****** *)

fun system (cmd: !READ(string)): int = "mac#atslib_system"

(* ****** ****** *)

fun abort (): void = "mac#atslib_abort"

(* ****** ****** *)

fun _Exit (status: int): void = "mac#atslib__Exit"
fun atexit (f: () -> void): int = "mac#atslib_atexit"

(* ****** ****** *)
(*
// HX: [mktemp] is not interface as it is BAD!!!
*)
(* ****** ****** *)
//
// HX: the last six characters of path much be XXXXXX
//
fun mkstemp
  {m,n:int | n >= 6} (
  path: &strbuf (m, n)
) : [i:int] (
  open_v (i) | int i
) = "mac#atslib_mkstemp"
// end of [mkstemp]

fun mkdtemp
  {m,n:int | n >= 6} {l:addr} (
  pf: !strbuf (m, n) @ l | p: ptr l
) : [l1:addr | l1==l || l1==null] ptr (l1)
  = "mac#atslib_mkdtemp" // null/nonnull: fail/succ
// end of [mkdtemp]

(* ****** ****** *)
//
// HX: this one returns an integer (not a pointer)!
//
fun bsearch {a:viewt@ype} {n:nat} (
  key: &a
, base: &(@[a][n]), nmemb: size_t n, size: sizeof_t a
, compar: (&a, &a) -<fun> int
) :<> intBtw (~1, n)
  = "atslib_bsearch" // function!
// end of [bsearch]

(* ****** ****** *)
//
// HX: a generic quicksort function
//
fun qsort
  {a:viewt@ype} {n:nat} (
  base: &(@[a][n])
, nmemb: size_t n, size: sizeof_t a
, compar: (&a, &a) -<fun> int
) :<> void
  = "atslib_qsort" // function!
// end of [qsort]

(* ****** ****** *)

(* end of [stdlib.sats] *)
