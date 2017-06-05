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

#include "prelude/params.hats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_top_"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

%{^

int the_chunk_count = 0 ;
// a soft threshold that can be extended
int the_chunk_count_limit = 1 ;
// int the_chunk_count_limit = 1024 ;
// a hard threshold that cannot be extended
int the_chunk_count_limit_max = 8192 ;

// a list of freed chunk data
freeitmlst the_freeitmlst_chunk_data = (freeitmlst)0 ;

#ifdef _ATS_MULTITHREAD
// this is the lock that protects the previous variables
pthread_mutex_t the_gc_main_lock = PTHREAD_MUTEX_INITIALIZER ;
#endif // [_ATS_MULTITHREAD]

/* ****** ****** */

chunklst the_sweeplst_array[FREEITMLST_ARRAYSIZE] = { (chunklst)0 } ;

#ifdef _ATS_MULTITHREAD
pthread_mutex_t the_sweeplst_lock_array[FREEITMLST_ARRAYSIZE] ;

ats_void_type gc_sweeplst_lock_array_init () {
  int i ; pthread_mutex_t *p_lock ;
  i = 0 ; p_lock = the_sweeplst_lock_array ;
  while (i < FREEITMLST_ARRAYSIZE) { 
    pthread_mutex_init (p_lock, NULL); i += 1; p_lock += 1 ;
  }
/*
  fprintf (stderr, "the_sweeplst_lock_array has been initialized.\n") ;
*/
  return ;
}
#endif

/* ****** ****** */

#ifdef _ATS_MULTITHREAD
// __thread
// freeitmlst the_freeitmlst_array[FREEITMLST_ARRAYSIZE] =
//  { (freeitmlst)0 } ;
__thread freeitmlst *the_freeitmlst_array = (freeitmlst*)0 ;
#else /* single thread */
freeitmlst the_freeitmlst_array[FREEITMLST_ARRAYSIZE] =
  { (freeitmlst)0 } ;
#endif

/* ****** ****** */

// protecting [the_globalentrylst] in [gc_globalentrylst.dats]
#ifdef _ATS_MULTITHREAD
pthread_mutex_t the_globalentrylst_lock = PTHREAD_MUTEX_INITIALIZER ;
#endif

/* ****** ****** */

// manually managed list of allocated memories
manmemlst the_manmemlst = (manmemlst)0 ;
#ifdef _ATS_MULTITHREAD
pthread_mutex_t the_manmemlst_lock = PTHREAD_MUTEX_INITIALIZER ;
#endif

/* ****** ****** */

#ifdef _ATS_MULTITHREAD
pthread_mutex_t the_threadinfolst_lock = PTHREAD_MUTEX_INITIALIZER ;
#endif

/* ****** ****** */

#if (__WORDSIZE == 32)
botsegtbllst the_topsegtbl[TOPSEG_TABLESIZE] = { (botsegtbllst)0 } ;
#endif // end of [__WORDSIZE == 32]

#if (__WORDSIZE == 64)
botsegtbllst the_topsegtbl[TOPSEG_HASHTABLESIZE] = { (botsegtbllst)0 } ;
#endif // end of [__WORDSIZE == 64]

/* ****** ****** */

ats_int_type the_markstatck_overflow = 0 ;

// for debugging purpose
// ats_void_type segfault (void) { return *(int*)0 ; }

%}

(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

%{$

ats_int_type
gc_chunk_count_limit_get () {
  return the_chunk_count_limit ; 
}

ats_void_type
gc_chunk_count_limit_set (ats_int_type n) {
  if (n >= 0) {
    the_chunk_count_limit = n ;
  } else {
    fprintf (stderr, "GC Fatal Error: [gc_chunk_count_limit_set]: negative argument.\n") ;
    exit (1) ;
  }
  return ;
}

ats_int_type
gc_chunk_count_limit_max_get () {
  return the_chunk_count_limit_max ; 
}

ats_void_type
gc_chunk_count_limit_max_set (ats_int_type n) {
  the_chunk_count_limit_max = n ; return ;
}

%}

(* ****** ****** *)

extern fun freeitmlst_chunk_data_get (): freeitmlst0
  = "freeitmlst_chunk_data_get"

%{$

extern
ats_ptr_type the_freeitmlst_chunk_data ;

ats_ptr_type freeitmlst_chunk_data_get () {
  return the_freeitmlst_chunk_data ;
}

%}

implement gc_fprint_stats {m} (pf | out) = let
  val () = let
    val nchunk = the_chunk_count_get ()
  in
    fprintf (pf | out, "the_chunk_count:\t%i\n", @(nchunk))
  end
  val () = let
    val ndata = freeitmlst_length (freeitmlst_chunk_data_get ())
  in
    fprintf (pf | out, "the_free_chunk_data_count:\t%i\n", @(ndata))
  end
  #define N FREEITMLST_ARRAYSIZE
  fun loop {i:nat | i <= N} .<N-i>.
    (out: &FILE m, i: int i): void = begin
    if i < N then let
      val itms = the_freeitmlst_array_get (i)
      val nitm = freeitmlst_length (itms); val () = begin
        fprintf (pf | out, "freeitmlst(%i):\t%i\n", @(i, nitm))
      end
    in
      loop (out, i+1)
    end
  end // end of [loop]
  val () = loop (out, 0)
in
  // empty
end // end of [gc_fprint_stats]

implement gc_print_stats () = let
  val (pf_stdout | ptr_stdout) = stdout_get ()
  val () = begin
    gc_fprint_stats (file_mode_lte_w_w | !ptr_stdout)
  end
in
  stdout_view_set (pf_stdout | (*none*))
end // end of [gc_print_stats]

implement gc_prerr_stats () = let
  val (pf_stderr | ptr_stderr) = stderr_get ()
  val () = begin
    gc_fprint_stats (file_mode_lte_w_w | !ptr_stderr)
  end
in
  stderr_view_set (pf_stderr | (*none*))  
end // end of [gc_prerr_stats]

(* ****** ****** *)

dynload "gcats1_misc.dats"
dynload "gcats1_freeitmlst.dats"
dynload "gcats1_chunk.dats"
dynload "gcats1_globalentry.dats"
#ifdef _ATS_MULTITHREAD
dynload "gcats1_multithread.dats"
#endif
dynload "gcats1_marking.dats"
dynload "gcats1_collecting.dats"
dynload "gcats1_autops.dats"
dynload "gcats1_manops.dats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // disabling implicit dynamic loading and
// using explicit dynamic loading instead
#define ATS_DYNLOADFUN_NAME "gc_init" // name for the dynload function

val () = let // initialization
  val dir = gc_stack_dir_get ()
  val () = gc_stack_beg_set (dir)

#ifdef _ATS_MULTITHREAD
  val () = gc_sweeplst_lock_array_init () where {
    extern fun gc_sweeplst_lock_array_init (): void = "gc_sweeplst_lock_array_init"
  }
  val () = gc_sem_init () where {
    extern fun gc_sem_init (): void = "gc_sem_init"
  }
  val () = gc_signal_init () where {
    extern fun gc_signal_init (): void = "gc_signal_init"
  }
  val () = gc_threadinfo_init () where {
    extern fun gc_threadinfo_init (): void = "gc_threadinfo_init"
  }
#endif // [_ATS_MULTITHREAD]

  val () = the_markstack_extend (1)

#if (ATS_GC_VERBOSE_LEVEL >= 1)
  val () = (prerr "GC initialization is done."; prerr_newline ())
#endif // [ATS_GC_VERBOSE_LEVEL]

in  
  (* empty *)
end // end of [gc_init]

(* ****** ****** *)

%{$

ats_ptr_type
  the_globalentrylst = (ats_ptr_type)0 ; // GLOBALENTRYLSTnil
// end of ...

extern void globalentrylst_insert
  (ats_ref_type ents, ats_ptr_type ptr, ats_int_type wsz) ;

ats_void_type gc_markroot_bsz
  (ats_ptr_type ptr, ats_int_type bsz/*bytesize*/) {
  int wsz = (bsz >> NBYTE_PER_WORD_LOG) ;
/*
  fprintf (stderr, "gc_markroot_bsz: ptr = %p\n", ptr) ;
  fprintf (stderr, "gc_markroot_bsz: bsz = %i\n", bsz) ;
  fprintf (stderr, "gc_markroot_bsz: wsz = %i\n", wsz) ;
*/
  globalentrylst_insert (&the_globalentrylst, ptr, wsz) ;
  return ;
} /* end of [gc_markroot_bsz] */

%}

(* ****** ****** *)

(* end of [gcats1_top.dats] *)
