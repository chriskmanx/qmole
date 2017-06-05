(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
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
// Time: June 2008
//
(* ****** ****** *)

%{#
#include "gcats1.cats"
%} // end of [%{#]

(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

abst@ype freeitm (wsz:int) (* size unit: word *)

viewtypedef freeitmptrsz (wsz:int, l:addr) =
  @{ ptr= ptr l, size= int wsz, view= freeitm wsz @ l }

viewtypedef freeitmptrsz () = [wsz:int] [l:addr] freeitmptrsz (wsz, l)

(* ****** ****** *)

abstype freeitmlst (n:int)
typedef freeitmlst0 = [n:nat] freeitmlst (n)
typedef freeitmlst1 = [n:pos] freeitmlst (n)

fun freeitmlst2ptr (x: freeitmlst0): ptr = "freeitmlst2ptr"

(* ****** ****** *)

//
// implemented in [gcats1_misc.dats]
//

fun gc_stack_dir_get (): int = "gc_stack_dir_get"

fun gc_stack_beg_get (): ptr = "gc_stack_beg_get"
fun gc_stack_beg_set (dir: int): void = "gc_stack_beg_set"

(* ****** ****** *)

//
// A variety of locks
//

absview gc_main_lock_v

fun gc_main_lock_acquire (): (gc_main_lock_v | void)
  = "gc_main_lock_acquire"
fun gc_main_lock_release (pf: gc_main_lock_v | (*none*)): void
  = "gc_main_lock_release"

//

absview globalentrylst_lock_v

fun the_globalentrylst_lock_acquire (): (globalentrylst_lock_v | void)
  = "the_globalentrylst_lock_acquire"
fun the_globalentrylst_lock_release (pf: globalentrylst_lock_v | (*none*)): void
  = "the_globalentrylst_lock_release"

//

absview manmemlst_lock_v

fun the_manmemlst_lock_acquire (): (manmemlst_lock_v | void)
  = "the_manmemlst_lock_acquire"
fun the_manmemlst_lock_release (pf: manmemlst_lock_v | (*none*)): void
  = "the_manmemlst_lock_release"

//

absview threadinfolst_lock_v

fun the_threadinfolst_lock_acquire (): (threadinfolst_lock_v | void)
  = "the_threadinfolst_lock_acquire"
fun the_threadinfolst_lock_release (pf: threadinfolst_lock_v | (*none*)): void
  = "the_threadinfolst_lock_release"

//

absview sweeplst_lock_v (int)
absview sweeplst_lock_all_v
absview sweeplst_lock_rest_v (int)

prval sweeplst_lock_all_takeout {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: sweeplst_lock_all_v | i: int i):<prf> (sweeplst_lock_v i, sweeplst_lock_rest_v i)

prval sweeplst_lock_all_complete {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf1: sweeplst_lock_v i, pf2: sweeplst_lock_rest_v i):<prf> sweeplst_lock_all_v

//

fun the_sweeplst_lock_acquire_one
  {i:nat | i < FREEITMLST_ARRAYSIZE} (i: int i): (sweeplst_lock_v i | void)
  = "the_sweeplst_lock_acquire_one"

fun the_sweeplst_lock_release_one
  {i:nat | i < FREEITMLST_ARRAYSIZE} (pf: sweeplst_lock_v i | i: int i): void
  = "the_sweeplst_lock_release_one"

//

fun the_sweeplst_lock_acquire_all (): (sweeplst_lock_all_v | void)
  = "the_sweeplst_lock_acquire_all"

fun the_sweeplst_lock_release_all (pf: sweeplst_lock_all_v | (*none*)): void
  = "the_sweeplst_lock_release_all"

//

fun the_sweeplst_lock_acquire_rest
  {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: sweeplst_lock_v i | i: int i): (sweeplst_lock_all_v | void)
  = "the_sweeplst_lock_acquire_rest"

fun the_sweeplst_lock_release_rest
  {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: sweeplst_lock_all_v | i: int i): (sweeplst_lock_v i | void)
  = "the_sweeplst_lock_release_rest"

(* ****** ****** *)

//
// some operations on freeitmlst
//

fun freeitmlst_is_nil {n:nat}
  (itms: freeitmlst n): bool (n==0) = "freeitmlst_is_nil"

fun freeitmlst_is_cons {n:nat}
  (itms: freeitmlst n): bool (n>0) = "freeitmlst_is_cons"

fun freeitmlst_cons {n:nat}
  (itm: ptr, itms: freeitmlst n): freeitmlst (n+1) = "freeitmlst_cons"

fun freeitmlst_tail_get {n:pos}
  (itms: freeitmlst n): freeitmlst (n-1) = "freeitmlst_tail_get"

fun freeitmlst_length {n:nat}
  (itms: freeitmlst n): int n = "freeitmlst_length"

//

//
// no lock for the following operations as
// [the_freeitmlst_array] is thread-local
//

fun the_freeitmlst_array_get
  (i: natLt FREEITMLST_ARRAYSIZE): freeitmlst0
  = "the_freeitmlst_array_get"

fun the_freeitmlst_array_set
  (i: natLt FREEITMLST_ARRAYSIZE, itms: freeitmlst0): void
  = "the_freeitmlst_array_set"

fun the_freeitmlst_array_clear_one
  (i: natLt FREEITMLST_ARRAYSIZE): void
  = "the_freeitmlst_array_clear_one"

fun the_freeitmlst_array_insert_at
  (itm: ptr, i: natLt FREEITMLST_ARRAYSIZE): void
  = "the_freeitmlst_array_insert_at"

//

fun the_freeitmlst_array_clear_all (): void = "the_freeitmlst_array_clear_all"
fun the_freeitmlst_array_mark_unset (): void = "the_freeitmlst_array_mark_unset"

(* ****** ****** *)

// some operation on chunks

abstype markbits

abstype chunklst (n:int)
typedef chunklst0 = [n:nat] chunklst (n)
typedef chunklst1 = [n:pos] chunklst (n)

fun chunklst2ptr (x: chunklst0): ptr = "chunklst2ptr"

fun chunklst_is_nil {n:nat} (chks: chunklst n): bool (n==0)
  = "chunklst_is_nil"

fun chunklst_is_cons {n:nat} (chks: chunklst n): bool (n>0)
  = "chunklst_is_cons"

fun chunklst_itembsz_get (chks: chunklst1): int = "chunklst_itembsz_get"

fun chunklst_itemwsz_get (chks: chunklst1): int = "chunklst_itemwsz_get"

fun chunklst_itemwsz_log_get (chks: chunklst1): intLt (FREEITMLST_ARRAYSIZE)
  = "chunklst_itemwsz_log_get"

fun chunklst_itemtot_get (chks: chunklst1): int = "chunklst_itemtot_get"

fun chunklst_markcnt_get (chks: chunklst1): int = "chunklst_markcnt_get"

#ifdef _ATS_MULTITHREAD
fun chunklst_freecnt_get (chks: chunklst1): int = "chunklst_freecnt_get"
fun chunklst_freecnt_inc (chks: chunklst1): void = "chunklst_freecnt_inc"
#endif

//

fun chunklst_data_get (chks: chunklst1): freeitmlst1
  = "chunklst_data_get"

fun chunklst_data_nonalign_get (chks: chunklst1): freeitmlst1
  = "chunklst_data_nonalign_get"

//

fun chunklst_markbits_get (chks: chunklst1): markbits = "chunklst_markbits_get"

//

fun chunklst_sweep_next_get {n:pos} (chks: chunklst n): chunklst (n-1)
  = "chunklst_sweep_next_get"

fun chunklst_sweep_length {n:nat} (chks: chunklst n): int n

//

fun chunklst_markcnt_dec (chks: chunklst1): void
  = "chunklst_markcnt_dec"

fun chunklst_markcnt_inc (chks: chunklst1): void
  = "chunklst_markcnt_inc"

//

fun chunklst_create
  (pf: !gc_main_lock_v | itemwsz_log: int, itemwsz: int): chunklst1
  = "chunklst_create"

fun chunklst_create_release
  (pf: gc_main_lock_v | itemwsz_log: int, itemwsz: int): chunklst1
  = "chunklst_create_release"

fun chunklst_destroy (pf: !gc_main_lock_v | chks: chunklst1): void
  = "chunklst_destroy"

(* ****** ****** *)

fun the_chunk_count_get (): int = "the_chunk_count_get"
fun the_chunk_count_dec_by (pf: !gc_main_lock_v | n: int): void
  = "the_chunk_count_dec_by"
fun the_chunk_count_inc_by (pf: !gc_main_lock_v | n: int): void
  = "the_chunk_count_inc_by"

fun the_chunk_count_limit_is_reached
  (pf: !gc_main_lock_v | (*none*)): bool
  = "the_chunk_count_limit_is_reached"

fun the_chunk_count_limit_is_not_reached
  (pf: !gc_main_lock_v | (*none*)): bool
  = "the_chunk_count_limit_is_not_reached"

fun the_chunk_count_limit_is_reached_within
  (pf: !gc_main_lock_v | n: int): bool
  = "the_chunk_count_limit_is_reached_within"

(* ****** ****** *)

fun gc_markbits_clear_the_topsegtbl (): void
  = "gc_markbits_clear_the_topsegtbl"

(* ****** ****** *)

absprop PTR_TOPSEG_GET_p (int)
abst@ype uintptr (i:int) = $extype "ats_uintptr1_type"

fun PTR_TOPSEG_GET (p: ptr): [i:nat] (PTR_TOPSEG_GET_p i | uintptr i)
  = "PTR_TOPSEG_GET"

fun PTR_BOTSEG_GET (p: ptr): natLt (BOTSEG_TABLESIZE)
  = "PTR_BOTSEG_GET"

fun PTR_CHKSEG_GET (p: ptr): natLt (CHKSEG_TABLESIZE)
  = "PTR_CHKSEG_GET"

//

abstype botsegtbllst (n:int)
typedef botsegtbllst0 = [n:nat] botsegtbllst (n)
typedef botsegtbllst1 = [n:pos] botsegtbllst (n)

//

fun botsegtbllst_is_nil {n:nat} (tbls: botsegtbllst n): bool (n==0)
  = "botsegtbllst_is_nil"

fun botsegtbllst_is_cons {n:nat} (tbls: botsegtbllst n): bool (n>0)
  = "botsegtbllst_is_cons"

//

#if (__WORDSIZE == 32)
fun botsegtbl_make_32 (): botsegtbllst (1) = "botsegtbl_make_32"
#endif // end of [__WORDSIZE == 32]

#if (__WORDSIZE == 64)
fun botsegtbl_make_64 {i,n:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i, tbls: botsegtbllst n)
  : botsegtbllst (n+1)
  = "botsegtbl_make_64"
#endif // end of [__WORDSIZE == 64]

//

fun botsegtbllst_get (
    tbls: botsegtbllst1
  , i: natLt (BOTSEG_TABLESIZE)
  ) : chunklst0
  = "botsegtbllst_get"

fun botsegtbllst_set (
    tbls: botsegtbllst1
  , i: natLt (BOTSEG_TABLESIZE)
  , chks: chunklst1
  ) : void
  = "botsegtbllst_set"

fun botsegtbllst_clear (
    tbls: botsegtbllst1
  , i: natLt (BOTSEG_TABLESIZE)
  ) : void
  = "botsegtbllst_clear"

(* ****** ****** *)

fun the_topsegtbl_get {i:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i): botsegtbllst0

fun the_topsegtbl_get_some {i:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i): botsegtbllst1

(* ****** ****** *)

fun gc_ptr_is_valid (p: ptr, ofs: &int): chunklst0
  = "gc_ptr_is_valid"

(* ****** ****** *)

//
// manmemlst
//

abstype manmemlst (n:int)
typedef manmemlst0 = [n:nat] manmemlst (n)
typedef manmemlst1 = [n:pos] manmemlst (n)

//

fun the_manmemlst_get (): manmemlst0 = "the_manmemlst_get"

//

fun manmemlst_is_nil {n:nat} (mms: manmemlst n): bool (n==0)
  = "manmemlst_is_nil"

fun manmemlst_is_cons {n:nat} (mms: manmemlst n): bool (n>0)
  = "manmemlst_is_cons"

//

fun manmemlst_length {n:nat} (mms: manmemlst n): int n
  = "manmemlst_length"

//

fun manmemlst_itemwsz_get (mms: manmemlst1): int
 = "manmemlst_itemwsz_get"

fun manmemlst_prev_get {n:pos}
  (mms: manmemlst1): [i:int | i == 0 || i == n+1] manmemlst i
  = "manmemlst_prev_get"

fun manmemlst_next_get {n:pos} (mms: manmemlst n): manmemlst (n-1)
  = "manmemlst_next_get"

fun manmemlst_data_get (mms: manmemlst1): freeitmlst0
  = "manmemlst_data_get"

(* ****** ****** *)

// [marking]

abstype markstackpagelst (n: int) // boxed type
typedef markstackpagelst0 = [n:nat] markstackpagelst (n)
typedef markstackpagelst1 = [n:pos] markstackpagelst (n)

//

fun markstackpagelst_nil (): markstackpagelst 0 = "markstackpagelst_nil"

fun markstackpagelst_is_nil {n:nat} (msps: markstackpagelst n): bool (n==0)
  = "markstackpagelst_is_nil"

fun markstackpagelst_is_cons {n:nat} (msps: markstackpagelst n): bool (n>0)
  = "markstackpagelst_is_cons"

fun markstackpagelst_next_get {n:pos}
  (msps: markstackpagelst n): markstackpagelst (n-1)
  = "markstackpagelst_next_get"

fun markstackpagelst_prev_get {n:pos}
  (msps: markstackpagelst n): [i:nat | i == 0 || i == n+1] markstackpagelst i
  = "markstackpagelst_prev_get"

fun markstackpagelst_length {n:nat} (msps: markstackpagelst n): int n
  = "markstackpagelst_length"

//

fun MARK_GET (x: markbits, i: int): int = "MARK_GET"
fun MARK_SET (x: markbits, i: int): void = "MARK_SET"
fun MARK_CLEAR (x: markbits, i: int): void = "MARK_CLEAR"

//

fun markstackpagelst_entry_get
  {n:pos} {i:nat | i < MARKSTACK_PAGESIZE}
  (msps: markstackpagelst n, i: int i, p: &ptr? >> ptr, wsz: &int? >> int): void
  = "markstackpagelst_entry_get"

fun markstackpagelst_entry_set
  {n:pos} {i:nat | i < MARKSTACK_PAGESIZE}
  (msps: markstackpagelst n, i: int i, p: ptr, wsz: int): void
  = "markstackpagelst_entry_set"

//

fun the_markstack_extend {n:nat} (n: int n): void
  = "the_markstack_extend"

//

fun markstack_pop (
  msps: &markstackpagelst1
, pos: &natLte(MARKSTACK_PAGESIZE)
, p: &ptr? >> ptr, wsz: &int? >> int
) : int (* underflow status *)
  = "markstack_pop"

fun markstack_push (
  msps: &markstackpagelst1
, pos: &natLte(MARKSTACK_PAGESIZE)
, p: ptr, wsz: int
) : int (* overflow status *)
  = "markstack_push"

fun the_markstack_pop (p: &ptr? >> ptr, wsz: &int? >> int): int
  = "the_markstack_pop"

fun the_markstack_push (p: ptr, wsz: int) : int (* overflow status *)
  = "the_markstack_push"

//

fun gc_mark_ptr (p: ptr): void = "gc_mark_ptr"

//

fun gc_overflow_mark_the_topsegtbl (): void = "gc_overflow_mark_the_topsegtbl"

//

fun gc_mark_the_stack (): void = "gc_mark_the_stack"
fun gc_mark_the_globalentrylst (): void = "gc_mark_the_globalentrylst"

// implemented in [gcats1_multithread.dats]
fun gc_mark_the_threadinfolst (): void = "gc_mark_the_threadinfolst"

// implemented in [gcats1_manops.dats]
fun gc_mark_the_manmemlst (): void = "gc_mark_the_manmemlst"

//

fun gc_mark_all (): int = "gc_mark_all"

(* ****** ****** *)

// [collecting]

fun the_sweeplst_array_get
  {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: !sweeplst_lock_v i | i: int i): chunklst0
  = "the_sweeplst_array_get"

fun the_sweeplst_array_set
  {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: !sweeplst_lock_v i | i: int i, itms: chunklst0): void
  = "the_sweeplst_array_set"

fun the_sweeplst_array_clear_one
  {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: !sweeplst_lock_v i | i: int i): void
  = "the_sweeplst_array_clear_one"

fun the_sweeplst_array_insert_at
  {i:nat | i < FREEITMLST_ARRAYSIZE}
  (pf: !sweeplst_lock_v i | itm: ptr, i: int i): void
  = "the_sweeplst_array_insert_at"

//

fun gc_sweeplst_build_the_topsegtbl (): void
  = "gc_sweeplst_build_the_topsegtbl"

//

fun gc_collect (
    _: !gc_main_lock_v
  , _: !globalentrylst_lock_v
  , _: !manmemlst_lock_v
  , _: !threadinfolst_lock_v
  , _: !sweeplst_lock_all_v
  | (*none*)
  ) : void = "gc_collect"

fun gc_chunk_threading (chks: chunklst1): freeitmlst0
  = "gc_chunk_threading"

fun gc_freeitmlst_generate (* gathering or creating *)
  (itemwsz_log: natLt FREEITMLST_ARRAYSIZE): freeitmlst1

(* ****** ****** *)

// [autops]: automatic management for freeing memory

fun gc_aut_malloc_bsz (bsz: int): ptr = "gc_aut_malloc_bsz"

fun gc_aut_malloc_wsz (wsz: int): ptr = "gc_aut_malloc_wsz"

fun gc_aut_calloc_bsz (n(*all*): int, bsz(*each*): int): ptr
  = "gc_aut_calloc_bsz"

fun gc_aut_free (itm: ptr): void = "gc_aut_free"

fun gc_aut_realloc_bsz (ptr: ptr, bsz_new: int): ptr
  = "gc_aut_realloc_bsz"

fun gc_aut_realloc_wsz (ptr: ptr, wsz_new: int): ptr

(* ****** ****** *)

// [manops]: manual management for freeing memory

fun gc_man_malloc_bsz (bsz: int): ptr = "gc_man_malloc_bsz"

fun gc_man_calloc_bsz (n(*all*): int, bsz(*each*): int): ptr
  = "gc_man_calloc_bsz"

fun gc_man_free (itm: ptr): void = "gc_man_free"

fun gc_man_realloc_bsz (ptr: ptr, bsz_new: int): ptr
  = "gc_man_realloc_bsz"

(* ****** ****** *)

// some toplevel operations

fun gc_chunk_count_limit_set {n:nat} (n: int n): void
  = "gc_chunk_count_limit_set"

fun gc_chunk_count_limit_max_set {n:nat} (n: int n): void
  = "gc_chunk_count_limit_max_set"

//

fun gc_markroot_bsz {n:nat} (p: ptr, bsz: int n): void
  = "gc_markroot_bsz"

//

fun gc_fprint_stats {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m): void

fun gc_print_stats (): void = "gc_print_stats"
fun gc_prerr_stats (): void = "gc_prerr_stats"

(* ****** ****** *)

(* end of [gcats1.sats] *)
