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

%{#
#include "libats/CATS/regexp.cats"
%} // end of [%{#]

(* ****** ****** *)

absviewtype REGEXPptr (l:addr) = ptr
viewtypedef REGEXPptr0 = [l:agez] REGEXPptr l
viewtypedef REGEXPptr1 = [l:addr | l > null] REGEXPptr l
castfn ptr_of_REGEXPptr {l:addr} (x: !REGEXPptr l): ptr l
overload ptr_of with ptr_of_REGEXPptr

(* ****** ****** *)

fun regexp_compile // implemented in C
  (pattern: !READ(string)): REGEXPptr0 = "atslib_regexp_compile"
// end of [regexp_compile]
fun regexp_compile_exn (pattern: string): REGEXPptr1

(* ****** ****** *)

castfn regexp_free_null (p: REGEXPptr null): ptr(*null*)

fun regexp_free
  {l:agz} (p: REGEXPptr l): void = "atslib_regexp_free" // !fun
// end of [regexp_free]

(* ****** ****** *)

fun regexp_match_string
  {l:agz} (re: !REGEXPptr l, str: string): bool
// end of [regexp_match_string]

(* ****** ****** *)
//
// HX: starting from a given offset
//
fun regexp_match_substring
  {l:agz} {n:int} {i,ln:nat | i + ln <= n}
  (re: !REGEXPptr l, str: string n, ofs: int i, len: int ln): bool
  = "atslib_regexp_match_substring"
// end of [regexp_match_substring]

(* ****** ****** *)

typedef strpos (n:int) =
  [i,j:int | i <= j; j <= n] (int i, int j)
viewtypedef strposlst (n:int) = List_vt (strpos n)

fun regexp_match_string_strposlst
  {l:agz} {n:nat} (re: !REGEXPptr l, str: string n): strposlst (n)
// end of [regexp_match_string_strposlst]

fun regexp_match_substring_strposlst
  {l:agz} {n:int} {i,ln:nat | i + ln <= n}
  (re: !REGEXPptr l, str: string n, ofs: int i, len: int ln): strposlst (ln)
  = "atslib_regexp_match_substring_strposlst"
// end of [regexp_match_substring_strposlst]

(* ****** ****** *)

fun regexp_split_string_list {l:agz}
  (re: !REGEXPptr l, str: !READ(string)): List_vt (strptr1)
// end of [regexp_string_split_list]

fun regexp_split_substring_list
  {l:agz} {n:int} {i,ln:nat | i+ln <= n} (
  re: !REGEXPptr l, str: !READ(string n), ofs: int i, len: int ln
) : List_vt (strptr1)
// end of [regexp_string_split_list]

(* ****** ****** *)

abstype REGEXPref // = ref (REGEXP)
castfn regexp_ref_make_ptr {l:agz} (p: REGEXPptr l):<> REGEXPref

(* ****** ****** *)

(* end of [regexp.sats] *)
