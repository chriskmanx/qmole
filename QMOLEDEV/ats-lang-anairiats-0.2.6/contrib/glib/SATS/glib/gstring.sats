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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//
(* ****** ****** *)

//
// HX-2010-03-03:
// it is just a plain vanilla style of implementation of strings; please
// do not expect it to be highly efficient because it is not implemented
// as such.
//

(* ****** ****** *)

%{#
#include "contrib/glib/CATS/glib/gstring.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// HX-2010-02-27: only need for individual testing
// staload "contrib/glib/SATS/glib/gtypes.sats"
//
(* ****** ****** *)

absviewtype GString_ptr (l:addr) // = GString*
viewtypedef GString_ptr0 = [l:agez] GString_ptr (l)
viewtypedef GString_ptr1 = [l:addr | l > null] GString_ptr (l)

(* ****** ****** *)

fun g_string_get_str
  {l:agz} (string: !GString_ptr l): ptr = "atsctrb_g_string_get_str"
// end of [g_string_get_str]

fun g_string_get_len
  {l:agz} (string: !GString_ptr l): gsize = "atsctrb_g_string_get_len"
// end of [g_string_get_len]

fun g_string_get_allocated_len
  {l:agz} (
  string: !GString_ptr l
) : gsize = "atsctrb_g_string_get_allocated_len"
// end of [g_string_get_allocated_len]

(* ****** ****** *)

symintr g_string_new

fun g_string_new_null
  (): GString_ptr1 = "mac#atsctrb_g_string_new_null"
overload g_string_new with g_string_new_null 

fun g_string_new_init
  {l:addr} (
  init: !READ(gstring(l))
) : GString_ptr1 = "mac#atsctrb_g_string_new_init"
overload g_string_new with g_string_new_init 

(* ****** ****** *)

absviewtype
gchararrptr (l:addr, n:int) // @[gchar][n] @ l

//
// HX-2010-05-28: [init] can be NULL
//
fun g_string_new_len
  {l:addr} {n,n1:nat | n1 <= n} (
  init: !gchararrptr (l, n), n1: gsize n1
) : GString_ptr1 = "mac#g_string_new_len"
// end of [g_string_new_len]

(* ****** ****** *)

fun g_string_sized_new
  {n:nat} (n: gsize n): GString_ptr1 = "mac#g_string_sized_new"
// end of [g_string_sized_new]

(* ****** ****** *)

// string = _val
fun g_string_assign
  {l,l1:addr | l > null} (
  string: !GString_ptr l, _val: !gstring l1
) : ptr l = "mac#atsctrb_g_string_assign"
// end of [g_string_assign]

(* ****** ****** *)
//
// HX: string = string ++ _val
//
fun g_string_append
  {l,l1:addr | l > null} (
  string: !GString_ptr l, _val: !gstring l1
) : ptr l = "mac#atsctrb_g_string_append"
// end of [g_string_append]
//
// HX: string = string ++ c
//
fun g_string_append_c {l:agz}
  (string: !GString_ptr l, c: gchar): ptr l = "mac#atsctrb_g_string_append_c"
// end of [g_string_append_c]
//
// string = string ++ _val
//
fun g_string_append_len
  {l,l1:addr | l > null}
  {n1,n2:nat | n2 <= n1} (
  string: !GString_ptr l, _val: !gchararrptr (l1, n1), n2: gsize n2
) : ptr l
  = "mac#atsctrb_g_string_append_len"
// end of [g_string_append_len]

(* ****** ****** *)

//
// the original content in [string] is overwritten
//
fun g_string_printf
  {l:agz} {ts:types} (
  string: !READ(GString_ptr(l)), fmt: printf_c ts, arg: ts
) : void
  = "mac#atsctrb_g_string_printf"
// end of [g_string_printf]

fun g_string_append_printf
  {l:agz} {ts:types} (
  string: !READ(GString_ptr(l)), fmt: printf_c ts, arg: ts
) : void
  = "mac#atsctrb_g_string_append_printf"
// end of [g_string_append_printf]

(* ****** ****** *)
//
// HX: string = _val ++ string
//
fun g_string_prepend {l:agz}
  (string: !GString_ptr l, _val: string): ptr l
  = "mac#atsctrb_g_string_prepend"
// end of [g_string_prepend]
//
// HX: string = c ++ string
//
fun g_string_prepend_c {l:agz}
  (string: !GString_ptr l, c: gchar): ptr l
  = "mac#atsctrb_g_string_prepend_c"
// end of [g_string_prepend_c]

//
// HX: string = _val ++ string
//
fun g_string_prepend_len
  {l,l1:addr | l > null} {n1,n2:nat} (
  string: !GString_ptr l, _val: !gchararrptr (l1, n1), n2: gsize n2
) : ptr l
  = "mac#atsctrb_g_string_prepend_len"
// end of [g_string_prepend_len]

(* ****** ****** *)

fun g_string_insert {l:agz}
  (string: !GString_ptr l, pos: gssize, _val: string): ptr l
  = "mac#atsctrb_g_string_insert"
// end of [g_string_insert]

fun g_string_insert_c {l:agz}
  (string: !GString_ptr l, pos: gssize, c: gchar): ptr l
  = "mac#atsctrb_g_string_insert_c"
// end of [g_string_insert_c]

fun g_string_insert_len
  {l,l1:addr | l > null} {n1,n2:nat} (
  string: !GString_ptr l, pos: gssize, _val: !gchararrptr (l1, n1), n2: gsize n2
) : ptr l = "mac#atsctrb_g_string_insert_len"
// end of [g_string_insert_len]

(* ****** ****** *)
//
// HX: since glib-2.14
//
fun g_string_overwrite {l:agz}
  (string: !GString_ptr l, pos: gssize, _val: string): ptr l
  = "mac#atsctrb_g_string_overwrite"
// end of [g_string_overwrite]

//
// HX: since glib-2.14
//
fun g_string_overwrite_len
  {l,l1:addr | l > null} {n1,n2:nat} (
  string: !GString_ptr l, pos: gssize, _val: !gchararrptr (l1, n1), n2: gsize n2
) : ptr l = "mac#atsctrb_g_string_overwrite_len"
// end of [g_string_overwrite_len]

(* ****** ****** *)

// HX: erase all if [n] is negative
fun g_string_erase {l:agz}
  (string: !GString_ptr l, pos: gssize, n: gssize): ptr l
  = "mac#atsctrb_g_string_erase"
// end of [g_string_erase]

(* ****** ****** *)

fun g_string_truncate {l:agz}
  (string: !GString_ptr l, n: gsize): ptr l = "mac#atsctrb_g_string_truncate"
// end of [g_string_truncate]

(* ****** ****** *)

fun g_string_set_size {l:agz}
  (string: !GString_ptr l, n: gsize): ptr l = "mac#atsctrb_g_string_set_size"
// end of [g_string_set_size]

(* ****** ****** *)

fun g_string_free_true
  (string: GString_ptr1): void = "mac#atsctrb_g_string_free_true"
// end of [g_string_free_true]

(* ****** ****** *)

fun g_string_hash {l:agz}
  (string1: !READ(GString_ptr(l))): guint = "mac#atsctrb_g_string_hash"
// end of [g_string_hash]

fun g_string_equal
  {l1,l2:agz} (
  string1: !READ(GString_ptr(l1)), string2: !READ(GString_ptr(l2))
) : gboolean
  = "mac#atsctrb_g_string_equal"
// end of [g_string_equal]

(* ****** ****** *)

(* end of [gstring.sats] *)
