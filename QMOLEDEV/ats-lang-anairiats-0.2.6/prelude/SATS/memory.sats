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
#print "Loading [memory.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

typedef bytes (n:int) = @[byte][n]
typedef b0ytes (n:int) = @[byte?][n]

(* ****** ****** *)

prfun bytes_v_split
  {n:int} {i:nat | i <= n} {l:addr}
  (pf: bytes(n) @ l): (bytes (i) @ l, bytes (n-i) @ l+i)
// end of [bytes_v_split]

prfun bytes_v_unsplit
  {n1,n2:nat} {l:addr}
  (pf1: bytes(n1) @ l, pf2: bytes(n2) @ l+n1): bytes(n1+n2) @ l
// end of [bytes_v_unsplit]

(* ****** ****** *)
//
praxi ptr_to_b0ytes_v
  : {a:viewt@ype} {l:addr} a? @ l -<prf> b0ytes (sizeof a) @ l
// end of [ptr_to_b0ytes_v]

(* ****** ****** *)
//
// the following declared functions are implemented in [ats_prelude_gcats.c]
//
(* ****** ****** *)

fun gc_init (): void = "ats_gc_init"

(* ****** ****** *)

fun gc_memlim_get_word (): size_t =
  "ats_gc_memlim_get_word"
fun gc_memlim_get_word_set (wsz: size_t): void =
  "ats_gc_memlim_set_word"

fun gc_memlim_get_page (): size_t =
  "ats_gc_memlim_get_page"
fun gc_memlim_get_page_set (wsz: size_t): void =
  "ats_gc_memlim_set_page"

(* ****** ****** *)

fun gc_max_memlim_get_word (): size_t =
  "ats_gc_max_memlim_get_word"
fun gc_max_memlim_get_word_set (wsz: size_t): void =
  "ats_gc_max_memlim_set_word"

fun gc_max_memlim_get_page (): size_t =
  "ats_gc_max_memlim_get_page"
fun gc_max_memlim_get_page_set (wsz: size_t): void =
  "ats_gc_max_memlim_set_page"

(* ****** ****** *)
//
// HX-2009-10-21: deprecated
//
fun gc_chunk_count_limit_get
  (): int = "ats_gc_chunk_count_limit_get"
fun gc_chunk_count_limit_set
  (n: int): void = "ats_gc_chunk_count_limit_set"
//
(* ****** ****** *)
//
// HX-2009-10-21: deprecated
fun gc_chunk_count_limit_max_get
  (): int = "ats_gc_chunk_count_limit_max_get"
fun gc_chunk_count_limit_max_set
  (n: int): void = "ats_gc_chunk_count_limit_max_set"
//
(* ****** ****** *)

fun malloc_gc
  {n:nat} (n: size_t n)
  :<> [l:agz] (freebyte_gc_v (n, l), b0ytes n @ l | ptr l)
  = "ats_malloc_gc"
// end of [malloc_gc]

fun calloc_gc
  {a:viewt@ype}
  {n:nat} (
  n: size_t n, tsz: sizeof_t a
) :<> [l:agz] (free_gc_v (a?, n, l), @[a?][n] @ l | ptr l)
  = "ats_calloc_gc"
// end of [calloc_gc]

fun free_gc
  {n:nat} {l:addr}
  (pfgc: freebyte_gc_v (n, l), pfat: b0ytes n @ l | p: ptr l):<> void
  = "ats_free_gc"
// end of [free_gc]

fun realloc_gc
  {n0,n:nat} {l0:addr} (
  pfgc: freebyte_gc_v (n0, l0), pfarr: b0ytes n0 @ l0 | _: ptr l0, _: size_t n
) :<> [l:agz] (freebyte_gc_v (n, l), b0ytes n @ l | ptr l)
  = "ats_realloc_gc"
// end of [realloc_gc]

(* ****** ****** *)

dataview
malloc_v (n:int, addr) =
  | {l:agz}
    malloc_v_succ (n, l) of (freebyte_ngc_v (n, l), b0ytes n @ l)
  | malloc_v_fail (n, null) of ()
// end of [malloc_v]
fun malloc_ngc {n:nat}
  (n: size_t n):<> [l:addr] (malloc_v (n, l) | ptr l) = "ats_malloc_ngc"
// end of [malloc_ngc]

dataview
calloc_v (a:viewt@ype, n:int, addr) =
  | {l:agz}
    calloc_v_succ (a, n, l) of (freebyte_ngc_v (n, l), @[a?][n] @ l)
  | calloc_v_fail (a, n, null) of ()
fun calloc_ngc
  {a:viewt@ype} {n:nat}
  (n: size_t n, tsz: sizeof_t a)
  :<> [l:addr] (calloc_v (a, n, l) | ptr l) = "ats_calloc_ngc"
// end of [calloc_ngc]

fun free_ngc {n:nat} {l:addr} (
  _: freebyte_ngc_v (n, l), _: b0ytes n @ l | p: ptr l
) :<> void= "ats_free_ngc" // end of [free_ngc]

dataview
realloc_v (
n0:int, n:int(*new*), addr, addr
) =
  | {l0,l:agz}
    realloc_v_succ (n0, n, l0, l) of (freebyte_ngc_v (n, l), b0ytes n @ l)
  | {l0:agz}
    realloc_v_fail (n0, n, l0, null) of (freebyte_ngc_v (n0, l0), b0ytes n0 @ l0)
fun realloc_ngc
  {n0,n:nat} {l0:addr} (
  pfgc: freebyte_ngc_v (n0, l0), pfarr: b0ytes n0 @ l0
| _: ptr l0, _: size_t n
) :<> [l:addr] (realloc_v (n0, n, l0, l) | ptr l) = "ats_realloc_ngc"
// end of [realloc_ngc]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [memory.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [memory.sats] *)
