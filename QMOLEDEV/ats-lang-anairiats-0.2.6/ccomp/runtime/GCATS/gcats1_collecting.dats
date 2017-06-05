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

// the code handles sweeping and threading in GC

(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_collecting_"

(* ****** ****** *)

extern fun the_sweeplst_array_clear_all
  (pf: !sweeplst_lock_all_v | (*none*)): void
  = "the_sweeplst_array_clear_all"

implement the_sweeplst_array_clear_all (pf | (*none*)) = let
  #define N FREEITMLST_ARRAYSIZE
  fun loop {i:nat | i <= N} .<N-i>.
    (pf: !sweeplst_lock_all_v | i: int i): void =
    if i < N then let
      prval @(pfi, pfc) = sweeplst_lock_all_takeout (pf | i)
      val () = the_sweeplst_array_clear_one (pfi | i)
      val () = (pf := sweeplst_lock_all_complete (pfi, pfc))
    in
      loop (pf | i+1)
    end // end of [if]
in
  loop (pf | 0)
end // end of [the_sweeplst_array_clear_all]

(* ****** ****** *)

extern fun gc_sweeplst_build_chunk (chks: chunklst1): void
  = "gc_sweeplst_build_chunk"

extern fun gc_sweeplst_build_botsegtbl (tbls: botsegtbllst1): void
  = "gc_sweeplst_build_botsegtbl"

implement gc_sweeplst_build_botsegtbl (tbls) = let
  fun loop {i:nat | i <= BOTSEG_TABLESIZE} .<BOTSEG_TABLESIZE-i>.
    (tbls: botsegtbllst1, i: int i): void = begin
    if i < BOTSEG_TABLESIZE then let
      val chks = botsegtbllst_get (tbls, i); val () = begin
        if chunklst_is_cons (chks) then gc_sweeplst_build_chunk (chks)
      end
    in
      loop (tbls, i+1)
    end // end of [if]
  end // end of [loop]
in
  loop (tbls, 0)
end // end of [gc_sweeplst_build_botsegtbl]

(* ****** ****** *)

extern fun fprint_the_sweeplst_array_all (): void
  = "fprint_the_sweeplst_array_all"
  
implement gc_freeitmlst_generate (itemwsz_log) = let
  #define _log itemwsz_log
  #define itemwsz (1 << itemwsz_log)
(*
  val () = begin
    prerr "gc_freeitmlst_generate: _log = "; prerr _log; prerr_newline ()
  end
*)
  val (pf_sweeplst_one | ()) = the_sweeplst_lock_acquire_one (_log)
  val chks = the_sweeplst_array_get (pf_sweeplst_one | _log)
  val chks = (
    if chunklst_is_cons (chks) then chks else let
      val () = the_sweeplst_lock_release_one (pf_sweeplst_one | _log)
      val (pf_main | ()) = gc_main_lock_acquire ()
      val (pf1_sweeplst_one | ()) = the_sweeplst_lock_acquire_one (_log)
      prval () = pf_sweeplst_one := pf1_sweeplst_one
      val chks = the_sweeplst_array_get (pf_sweeplst_one | _log)
    in
      if chunklst_is_cons chks then begin
        gc_main_lock_release (pf_main | (*none*)); chks
      end else let
        val is_not_reached =
          the_chunk_count_limit_is_not_reached (pf_main | (*none*))
      in
        if (is_not_reached) then begin
          chunklst_create_release (pf_main | _log, itemwsz)
        end else let
          val (pf_globals | ()) = the_globalentrylst_lock_acquire ()
          val (pf_manmemlst | ()) = the_manmemlst_lock_acquire ()
          val (pf_threads | ()) = the_threadinfolst_lock_acquire ()
          val (pf_sweeplst_all | ()) = begin
            the_sweeplst_lock_acquire_rest (pf_sweeplst_one | _log)
          end
          val () = gc_collect (
            pf_main, pf_globals, pf_manmemlst, pf_threads, pf_sweeplst_all | (*none*)
          ) // end of [gc_collect]          
          val (pf1_sweeplst_one | ()) = begin
            the_sweeplst_lock_release_rest (pf_sweeplst_all | _log)
          end
          prval () = pf_sweeplst_one := pf1_sweeplst_one
          val () = the_threadinfolst_lock_release (pf_threads | (*none*))
          val () = the_manmemlst_lock_release (pf_manmemlst | (*none*))
          val () = the_globalentrylst_lock_release (pf_globals | (*none*))
          val chks = the_sweeplst_array_get (pf_sweeplst_one | _log)
(*
          val () = begin
            prerr "gc_freeitmlst_generate: chks = "; prerr (chunklst2ptr chks); prerr_newline ()
          end
*)
        in
          if chunklst_is_cons chks then begin
            gc_main_lock_release (pf_main | (*none*)); chks
          end else begin
            chunklst_create_release (pf_main | _log, itemwsz)
          end // end of [if]
        end // end of [if (is_not_reached) ...]
      end // end of [if chunklst_is_cons (chks) ...]
    end // end of [if chunklst_is_cons (chks) ...]
  ) : chunklst1
(*
  val () = begin
    prerr "gc_freeitmlst_generate: chks = "; prerr (chunklst2ptr chks); prerr_newline ()
  end
*)
  val chks1 = chunklst_sweep_next_get chks
  val () = the_sweeplst_array_set (pf_sweeplst_one | _log, chks1)
  val () = the_sweeplst_lock_release_one (pf_sweeplst_one | _log)
  val itms = gc_chunk_threading (chks)
(*
  val () = begin
    prerr "gc_freeitmlst_generate: itms = "; prerr (freeitmlst2ptr itms); prerr_newline ()
  end
*)
in
  if freeitmlst_is_nil itms then begin
    prerr "GC: Fatal Error";
    prerr ": [gc_freeitmlst_generate]: the generated freeitmlst is nil";
    prerr_newline ();
    exit {freeitmlst1} (1)
  end else begin
    itms // return value
  end // end of [if]
end // end of [gc_freeitmlst_generate]

(* ****** ****** *)

%{$

// [chks] must not be NULL!
ats_void_type gc_sweeplst_build_chunk (ats_ptr_type chks) {
  int i, j ; freeitmlst *pi, *pij ;
  int itemwsz_log, itemtot, markcnt ;
  int freecnt ;

  markcnt = chunklst_markcnt_get (chks) ;
/*
  fprintf (stderr, "gc_sweeplst_build_chunk: markcnt = %i\n", markcnt) ;
*/
#ifdef _ATS_MULTITHREAD
  // this logic is problematic!
  freecnt = chunklst_freecnt_get (chks) ;
  if (freecnt > 0) {
    fprintf (stderr, "gc_sweeplst_build_chunk: freecnt = %i\n", freecnt) ;
    return ;
  }
#endif
  if (markcnt == 0) { chunklst_destroy (chks) ; return ; }

  itemtot = chunklst_itemtot_get (chks) ;
  if (markcnt > itemtot * CHUNK_SWEEP_CUTOFF) return ;

  itemwsz_log = chunklst_itemwsz_log_get (chks) ;
/*
  if (itemwsz_log < 0) { // chks->itemtot = 1 and markcnt = 1
    fprintf (stderr, "GC: Fatal Error: [gc_sweeplst_build_chunk]") ;
    fprintf (stderr, ": itemwsz_log = %i\n", itemwsz_log) ;
    exit (1) ;
  }
*/
/*
  fprintf (stderr, "gc_sweeplst_build_chunk: itemwsz_log = %i\n", itemwsz_log) ;
*/
  the_sweeplst_array_insert_at (chks, itemwsz_log) ;
  return ;
} /* end of [gc_sweeplst_build_chunk] */

/* ****** ****** */

#if (__WORDSIZE == 32)

ats_void_type
gc_sweeplst_build_the_topsegtbl () {
  int i ; botsegtbllst tbls ;
  the_sweeplst_array_clear_all () ;
  for (i = 0; i < TOPSEG_TABLESIZE; i += 1) {
    tbls = the_topsegtbl[i] ;
    if (tbls) gc_sweeplst_build_botsegtbl (tbls) ;
  } // end of [for]
  return ;
} /* end of [gc_sweeplst_build_the_topsegtbl] */

#endif // end of [__WORDSIZE == 32]

#if (__WORDSIZE == 64)

ats_void_type
gc_sweeplst_build_the_topsegtbl () {
  int i ; botsegtbllst tbls ;
  the_sweeplst_array_clear_all () ;
  for (i = 0; i < TOPSEG_HASHTABLESIZE; i += 1) {
    tbls = the_topsegtbl[i] ;
    while (tbls) { // termination: obvious
      gc_sweeplst_build_botsegtbl (tbls) ; tbls = tbls->hash_next ;
    } // end of [while]
  } // end of [for]
  return ;
} /* end of [gc_sweeplst_build_the_topsegtbl] */

#endif // end of [__WORDSIZE == 64]

%}

(* ****** ****** *)

%{$

#define NCHUNK_PER_MARKSTACKPAGE 32
#define MARKSTACK_EXTEND_INCREMENT 2

extern int the_chunk_count ;
extern int the_chunk_count_limit ;
extern int the_chunk_count_limit_max ;
extern int the_markstackpagecnt ;

#ifdef _ATS_MULTITHREAD
extern sem_t the_sleep_semaphore ;
extern threadinfolst the_threadinfolst_fst ;
extern __thread threadinfolst the_threadinfolst_self ;
#endif

ats_void_type gc_collect () {
  int overflow, nchunk, nmarkstackpage ;
  jmp_buf reg_save ; // register contents are roots
#ifdef _ATS_MULTITHREAD
  threadinfolst infolst ; int nother ;
#endif
/*
  fprintf(
    stderr
  , "GC begs: the_chunk_count = %i and the_chunk_count_limit = %i\n"
  , the_chunk_count
  , the_chunk_count_limit
  ) ;
*/
  setjmp (reg_save) ; // push registers onto the stack
  asm volatile ("": : :"memory") ; // stop potential optimization

  nmarkstackpage = (the_chunk_count << CHUNK_WORDSIZE_LOG) ;
  nmarkstackpage /= (MARKSTACK_PAGESIZE * NCHUNK_PER_MARKSTACKPAGE) ;
  nmarkstackpage += 1 ; nmarkstackpage -= the_markstackpagecnt ;
/*
  fprintf (stderr, "gc_collect: nmarkstackpage = %i\n", nmarkstackpage) ;
*/
  the_markstack_extend (nmarkstackpage) ;

#ifdef _ATS_MULTITHREAD
  // put all of the other threads into sleep
  infolst = the_threadinfolst_fst ; nother = 0 ;
  while (infolst) {
    if (infolst != the_threadinfolst_self) {
      fprintf (stderr, "gc_collect: SIGUSR1: infolst->pid = %i\n", (int)(infolst->pid)) ;
      pthread_kill (infolst->pid, SIGUSR1) ; nother += 1 ;
    }
    infolst = infolst->next ;
  }
  while (nother) { // ordering is irrelevant
    fprintf (stderr, "gc_collect: sem_wait: bef: nother = %i\n", nother) ;
    sem_wait (&the_sleep_semaphore) ; nother -= 1 ;
    fprintf (stderr, "gc_collect: sem_wait: aft: nother = %i\n", nother) ;
  }
  fprintf (stderr, "gc_collect: nother = %i\n", nother) ;
#endif // end of [_ATS_MULTITHREAD]

  gc_markbits_clear_the_topsegtbl () ; // clear all mark bits
/*
  fprintf (
    stderr, "gc_collect: gc_markbits_clear_topsegtbl: done\n", nmarkstackpage
  ) ; // end of [fprintf]
*/

  overflow = gc_mark_all () ; // marking phase
/*
  fprintf (stderr, "gc_collect: gc_mark_all: done\n") ;
*/

#ifdef _ATS_MULTITHREAD
  // wake up all of the sleeping threads
  infolst = the_threadinfolst_fst ;
  while (infolst) {
    if (infolst != the_threadinfolst_self) {
      fprintf (stderr, "gc_collect: SIGUSR2: infolst->pid = %i\n", (int)(infolst->pid)) ;
      pthread_kill (infolst->pid, SIGUSR2) ;
    }
    infolst = infolst->next ;
  }
#endif // end of [_ATS_MULTITHREAD]

  the_freeitmlst_array_mark_unset () ; /* is this really needed? */
  // [gc_mark_threadinfolst_one] is not called on [the_threadinfolst_self]
  // Therefore [the_freeitmlst_array_clear_all] must be called!
  the_freeitmlst_array_clear_all () ; /* clear the freeitmlst array */
  gc_sweeplst_build_the_topsegtbl () ; // sweep chunks into [the_sweeplst_array]

  if (overflow > 0) {
/*
    fprintf (stderr, "gc_collect: markstack overflow happend.\n") ;
*/
    the_markstack_extend (MARKSTACK_EXTEND_INCREMENT) ;
  }
/*
  fprintf(
    stderr
  , "GC ends: the_chunk_count = %i and the_chunk_count_limit = %i\n"
  , the_chunk_count
  , the_chunk_count_limit
  ) ;
*/
  if (the_chunk_count_limit_max >= 0) {
    // [the_chunk_count_limit_max] is finite
    if (the_chunk_count_limit >= the_chunk_count_limit_max) {
      // fprintf(stderr, "GC: the_chunk_count_limit = %i\n", the_chunk_count_limit) ;
      // fprintf(stderr, "GC: the_chunk_count_limit_max = %i\n", the_chunk_count_limit_max) ;
      fprintf(stderr, "GC: Warning: the maximal chunk count limit is reached!\n") ;
      return ;
    }
  } /* end of [if] */

  if (the_chunk_count >= the_chunk_count_limit * CHUNK_LIMIT_EXTEND_CUTOFF)
    the_chunk_count_limit *= 2 ;
/*
  fprintf(
    stderr
  , "GC ends: update: the_chunk_count_limit = %i\n", the_chunk_count_limit
  ) ;
*/
  return ;

} /* end of [gc_collect] */

%}

(* ****** ****** *)

%{$

ats_ptr_type gc_chunk_threading (ats_ptr_type chks) {
  int i, j ;
  int itemwsz, itemtot ;
  freeitmlst data0, data, data_next ;
  int markcnt ; byte *markbits ;

  itemwsz = ((chunklst)chks)->itemwsz ;
  itemtot = ((chunklst)chks)->itemtot ;
  markcnt = ((chunklst)chks)->markcnt ;

  data0 = ((chunklst)chks)->data ;

  if (markcnt == 0) { // fast threading
    // threading all the freeitms in the created chunk
    data = data0 ; data_next = data0 ;
    for (i = 1; i < itemtot; i += 1) {
      data_next = (freeitmlst*)data_next + itemwsz ;
      *(freeitmlst*)data = data_next ; data = data_next ;
    } // end of [for]
    *(freeitmlst*)data = (freeitmlst)0 ;
    goto GC_CHUNK_THREADING_IS_DONE ;
  } // end of [if]

  markbits = ((chunklst)chks)-> markbits ;
  for (i = 0; i < itemtot; i += 1) {
    if (!MARK_GET (markbits, i)) break ;
  }

  if (i == itemtot) {
    data0 = (freeitmlst)0 ; goto GC_CHUNK_THREADING_IS_DONE ;
  }

  data0 = (freeitmlst*)data0 + i * itemwsz;
  data = data0 ; data_next = data0 ;
  for (j = i + 1; j < itemtot; j += 1) {
    data_next = (freeitmlst*)data_next + itemwsz ;
    if (!MARK_GET (markbits, j)) {
      *(freeitmlst*)data = data_next ; data = data_next ;
    } // end of [if]
  } // end of [for]

  *(freeitmlst*)data = (freeitmlst)0 ;

/*
  if (itemtot - markcnt != freeitmlst_length (data0)) {
    fprintf (stderr, "GC: Fatal Error") ;
    fprintf (stderr, ": [gc_chunk_threading]: consistency check failed.\n") ;
    fprintf (stderr, "itemtot = %i\n", itemtot) ;
    fprintf (stderr, "markcnt = %i\n", markcnt) ;
    fprintf (stderr, "freeitmlst_length(data0) = %i\n", freeitmlst_length(data0)) ;
    exit (1) ;
  }
*/

  GC_CHUNK_THREADING_IS_DONE: return data0 ;
  
} /* end of [gc_chunk_threading] */

%}

(* ****** ****** *)

(* end of [gcats1_collecting.dats] *)
