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

// the code handles the stack needed for marking

(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_marking_"

(* ****** ****** *)

implement markstackpagelst_length (msps) = let
  fun aux {i,j:nat}
    (msps: markstackpagelst i, j: int j): int (i+j) =
    if markstackpagelst_is_nil msps then j else begin
      aux (markstackpagelst_next_get msps, j+1)
    end // end of [if]
in
  aux (msps, 0)
end // end of [markstackpagelst_length]

(* ****** ****** *)

implement markstack_pop (msps, pos, p_r, wsz_r) = let
  val i = pos - 1
in
  if i >= 0 then let
    val () = markstackpagelst_entry_get (msps, i, p_r, wsz_r)
  in
    pos := i; 0 // underflow status
  end else let
    val prev = markstackpagelst_prev_get (msps)
  in
    if markstackpagelst_is_cons (prev) then let
      val () = msps := prev
      val i = MARKSTACK_PAGESIZE - 1
      val () = markstackpagelst_entry_get (msps, i, p_r, wsz_r)
    in
      pos := i; 0 // underflow status
    end else begin
      p_r := (null: ptr); wsz_r := (0: int); 1 // underflow status
    end // end of [if]
  end // end of [if]
end // end of [markstack_pop]

(* ****** ****** *)

implement markstack_push (msps, pos, p, wsz) = let
  val i = pos
in
  if i < MARKSTACK_PAGESIZE then let
    val () = markstackpagelst_entry_set (msps, i, p, wsz)
    val i = i + 1; val () = 
      if i < MARKSTACK_PAGESIZE then (pos := i)
      else let
        val next = markstackpagelst_next_get (msps)
      in
        if markstackpagelst_is_cons (next) then begin
          msps := next; pos := 0
        end else begin
          pos := MARKSTACK_PAGESIZE
        end // end of [if]
      end // end of [if]
  in
    0 // overflow status
  end else begin
    1 // overflow status
  end // end of [if]
end // end of [markstack_push]

(* ****** ****** *)

extern fun gc_overflow_mark_chunk
  (chks: chunklst1): void = "gc_overflow_mark_chunk"

extern fun gc_overflow_mark_botsegtbl
  (tbls: botsegtbllst1): void = "gc_overflow_mark_botsegtbl"

implement gc_overflow_mark_botsegtbl (tbls) = loop (tbls, 0) where {
  #define N BOTSEG_TABLESIZE
  fun loop {i:nat | i <= N} .<N-i>.
    (tbls: botsegtbllst1, i: int i): void =
    if i < BOTSEG_TABLESIZE then let
      val chks = botsegtbllst_get (tbls, i); val () = begin
        if chunklst_is_cons (chks) then gc_overflow_mark_chunk (chks)
      end // end of [val]
    in
      loop (tbls, i+1)
    end // end of [if]
} // end of [gc_overflow_mark_botsegtbl]

(* ****** ****** *)

extern fun the_markstack_overflow_get (): int
  = "the_markstack_overflow_get"

extern fun the_markstack_overflow_set (): void
  = "the_markstack_overflow_set"

extern fun the_markstack_overflow_clear (): void
  = "the_markstack_overflow_clear"

implement gc_mark_all () = let
  val () = gc_mark_the_globalentrylst ()
  val () = gc_mark_the_manmemlst ()
#ifdef _ATS_MULTITHREAD
  val () = gc_mark_the_threadinfolst ()
#endif
  val () = the_markstack_overflow_clear ()
  val () = gc_mark_the_stack ()
  val overflow0 = the_markstack_overflow_get ()
// (*
  val () =
    if overflow0 > 0 then begin
      prerr "GC: [gc_mark_all]: mark stack overflow happened!\n"
    end // end of [if]
// *)
  val () = loop (overflow0) where {
    fun loop (overflow: int): void =
      if (overflow > 0) then let
        val () = the_markstack_overflow_clear ()
        val () = gc_overflow_mark_the_topsegtbl ()
        val overflow = the_markstack_overflow_get ()         
      in
        loop (overflow)
      end // end of [if]
  } // end of [where]
in
  overflow0
end // end of [gc_mark_all]

(* ****** ****** *)

local

// [val] -> [var]
extern val the_markstackpagecnt: int = "the_markstackpagecnt"

// [val] -> [var]
extern val the_markstackpagelst_fst: markstackpagelst0
  = "the_markstackpagelst_fst"

extern val the_markstackpagelst_cur: markstackpagelst0
  = "the_markstackpagelst_cur"

// [val] -> [var]
extern val the_markstackposition: natLte(MARKSTACK_PAGESIZE)
  = "the_markstackposition"

in // in of [local]

implement the_markstackpagecnt = 0
implement the_markstackpagelst_fst = markstackpagelst_nil ()
implement the_markstackpagelst_cur = the_markstackpagelst_fst
implement the_markstackposition = 0

end // end of [local]

(* ****** ****** *)

%{$

extern ats_ptr_type the_markstackpagelst_fst ;

ats_void_type the_markstack_extend (ats_int_type n) {
  int i ; markstackpagelst p0, p ;
  p0 = the_markstackpagelst_fst ;

  if (the_markstackposition != 0) {
    fprintf (
      stderr
    , "GC: Fatal Error: the_markstack_extend: the_markstackposition = %i.\n"
    , the_markstackposition
    ) ;
    exit (1) ;
  } // end of [if]

  for (i = 0; i < n; i += 1) {
/*
    fprintf (stderr, "the_markstack_extend: i = %i\n", i) ;
*/
    p = (markstackpagelst)malloc (sizeof(markstackpage)) ; if (!p) {
      fprintf (
        stderr, "GC Fatal Error: [the_markstack_extend]: malloc failed!\n"
      ) ;
      exit (1) ;
    } // end of [if]

    if (p0 != NULL) p0->prev = p ; p->next = p0 ; p->prev = NULL ; p0 = p ;
  } // end of [while]

  if (n > 0) the_markstackpagecnt += n ;
  the_markstackpagelst_fst = the_markstackpagelst_cur = p0 ;

/*
  if (the_markstackpagecnt != markstackpagelst_length (p0)) {
    fprintf (
      stderr
    , "GC: Fatal Error: the_markstack_extend: the_markstackpagecnt = %i.\n"
    , the_markstackpagecnt
    ) ;
    exit (1) ;
  } // end of [if]
*/

  return ;
} /* end of [the_markstack_extend] */

/* ****** ****** */

extern ats_ptr_type gc_ptr_is_valid (ats_ptr_type, ats_ref_type) ;

ats_void_type gc_mark_ptr (ats_ptr_type ptr) {
  chunklst chks ;
  int ofs ; int itemwsz ;
  byte *markbits ;
  int i ; freeitmlst *ptr_i ;
  freeitmlst ptr_cand ;
  int overflow ;

/*
  fprintf (stderr, "gc_mark_ptr: first: ptr = %p(%i)\n", ptr, ptr) ;
*/
  chks = (chunklst)gc_ptr_is_valid (ptr, &ofs) ;
/*
  fprintf (stderr, "gc_mark_ptr: chks = %p\n", chks) ;
*/

  if (!chks) return ; // [ptr] is invalid
  markbits = chks->markbits ;
  if (MARK_GET(markbits, ofs)) return ; // already marked
  MARK_SET(markbits, ofs) ; chks->markcnt += 1 ;

  itemwsz = chks->itemwsz ;

  while (ptr) { // ptr != NULL
/*
    fprintf (stderr, "gc_mark_ptr: ptr = %p(%i)\n", ptr, ptr) ;
    fprintf (stderr, "gc_mark_ptr: itemwsz = %i\n", itemwsz);
*/
    if (itemwsz > MARKSTACK_CUTOFF) {
      overflow = the_markstack_push (
        (freeitmlst*)ptr + MARKSTACK_CUTOFF, itemwsz - MARKSTACK_CUTOFF
      ) ;
      if (overflow) the_markstack_overflow_set () ;
      itemwsz = MARKSTACK_CUTOFF ;
    } // end of [if]
    
    // push all the valid pointers onto the markstack except the last one
    ptr_i = (freeitmlst*)ptr ;
    for (i = 0; i < itemwsz - 1; i += 1, ptr_i += 1) {
      ptr_cand = *ptr_i ;
/*
      fprintf (stderr, "gc_mark_ptr: ptr_i = %p(%i)\n", ptr_i, ptr_i) ;
      fprintf (stderr, "gc_mark_ptr: ptr_cand = %p(%i)\n", ptr_cand, ptr_cand) ;
*/
      chks = (chunklst)gc_ptr_is_valid (ptr_cand, &ofs) ;
      if (!chks) continue ; // [ptr_cand] is invalid
      markbits = chks->markbits ;
      if (MARK_GET(markbits, ofs)) continue ; // already marked
      MARK_SET(markbits, ofs) ; chks->markcnt += 1 ;
      overflow = the_markstack_push (ptr_cand, chks->itemwsz) ;
      if (overflow) the_markstack_overflow_set () ;
    } // end of [for]

    ptr_cand = *ptr_i ;
/*
    fprintf (stderr, "gc_mark_ptr: ptr_i = %p(%i)\n", ptr_i, ptr_i) ;
    fprintf (stderr, "gc_mark_ptr: ptr_cand = %p(%i)\n", ptr_cand, ptr_cand) ;
*/
    chks = (chunklst)gc_ptr_is_valid (ptr_cand, &ofs) ;
    if (!chks) { // [ptr_cand] is invalid
      the_markstack_pop (&ptr, &itemwsz) ; continue ;
    } // end of [if]
    markbits = chks->markbits ;
    if (MARK_GET(markbits, ofs)) { // [ptr_cand] is marked
      the_markstack_pop (&ptr, &itemwsz) ; continue ;
    }
    MARK_SET(markbits, ofs) ; chks->markcnt += 1 ;
    ptr = ptr_cand ; itemwsz = chks->itemwsz ;
  } // end of [while]
} /* end of [gc_mark_ptr] */

%}

(* ****** ****** *)

%{$

// [chks] must not be NULL!
ats_void_type gc_overflow_mark_chunk (ats_ptr_type chks) {
  int i, j ; freeitmlst *pi, *pij ;
  int itemwsz, itemtot ; byte *markbits ;

  itemwsz = chunklst_itemwsz_get (chks) ;
  itemtot = chunklst_itemtot_get (chks) ;
  markbits = chunklst_markbits_get (chks) ;

  pi = (freeitmlst*)chunklst_data_get (chks) ;
  for (i = 0; i < itemtot; i += 1, pi += itemwsz) {
    if (MARK_GET(markbits, i)) {
      for (j = 0, pij = pi; j < itemwsz; j += 1, pij += 1) {
        gc_mark_ptr (pij) ;
      } // end of [for]
    } // end of [if]
  } // end of [for]
} /* end of [gc_overflow_mark_chunk] */

/* ****** ****** */

#if (__WORDSIZE == 32)

ats_void_type gc_overflow_mark_the_topsegtbl () {
  int i ; botsegtbllst tbls ;
  for (i = 0; i < TOPSEG_TABLESIZE; i += 1) {
    tbls = the_topsegtbl[i] ;
    if (tbls) gc_overflow_mark_botsegtbl (tbls) ;
  } // end of [for]
  return ;
} /* end of [gc_overflow_mark_the_topsegtbl] */

#endif // end of [__WORDSIZE == 32]

#if (__WORDSIZE == 64)

ats_void_type gc_overflow_mark_the_topsegtbl () {
  int i ; botsegtbllst tbls ;
  for (i = 0; i < TOPSEG_HASHTABLESIZE; i += 1) {
    tbls = the_topsegtbl[i] ;
    while (tbls) { // terminating: obvious
      gc_overflow_mark_botsegtbl (tbls) ; tbls = tbls->hash_next ;
    } // end of [while]
  } // end of [for]
  return ;
} /* end of [gc_overflow_mark_the_topsegtbl] */

#endif // end of [__WORDSIZE == 64]

%}

(* ****** ****** *)

%{$

extern ats_ptr_type the_globalentrylst ;
extern ats_void_type gc_mark_globalentrylst (ats_ptr_type ents) ;

ats_void_type gc_mark_the_globalentrylst () {
  gc_mark_globalentrylst (the_globalentrylst) ; return ;
}

extern ats_ptr_type gc_stack_beg_get () ;

ats_void_type gc_mark_the_stack () {
  intptr_t dir ; // make sure that [dir] is word-aligned!
  freeitmlst *_fr, *_to ;

  dir = gc_stack_dir_get () ;

  if (dir > 0) {
    _fr = gc_stack_beg_get () ; _to = (freeitmlst*)(&dir) - 1 ;
  } else {
    _to = gc_stack_beg_get () ; _fr = (freeitmlst*)(&dir) + 1 ;
  } // end of [if]
/*
  fprintf (stderr, "gc_mark_the_stack: _fr = %p(%u)\n", _fr, _fr) ;
  fprintf (stderr, "gc_mark_the_stack: _to = %p(%u)\n", _to, _to) ;
  fprintf (stderr, "gc_mark_the_stack: _to - _fr = %i\n", _to - _fr) ;
*/
  while (_fr <= _to) {
    gc_mark_ptr (*_fr) ; _fr += 1 ; // termination: obvious
  } // end of [while]

  return ;
} /* end of [gc_mark_the_stack] */

%}

(* ****** ****** *)

(* end of [gcats1_marking.dats] *)
