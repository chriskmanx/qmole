/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
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
*/

/* ****** ****** */
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: June 2008
//
/* ****** ****** */

#ifndef __ATS_GC_CATS
#define __ATS_GC_CATS

/* ****** ****** */

#include "gcats1.hats"

/* ****** ****** */
//
#include <setjmp.h>
#include <unistd.h>
#include <stdio.h>
//
extern void free (void*) ; // see [stdlib.h]
extern void *memset (void *src, int c, size_t n) ; // see [string.h]
//
#ifdef _ATS_MULTITHREAD
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#endif
//
#include "ats_basics.h"
#include "ats_types.h"
//
/* ****** ****** */

#undef ATS_FREE
#define ATS_FREE free

#undef ATS_MALLOC
#define ATS_MALLOC malloc

#undef ATS_GC_MARKROOT
#define ATS_GC_MARKROOT(ptr, sz) do { ; } while (0)

/* ****** ****** */

static inline
ats_int_type log2_floor (ats_int_type n) {
  int c = 0 ;
  while (n >>= 1) c += 1 ;
  return c;
}

static inline
ats_int_type log2_ceil (ats_int_type n) {
  int c ;
  c = log2_floor (n) ; if (n > (1 << c)) c += 1 ;
  return c ;
}

/* ****** ****** */

typedef unsigned char byte ;

/* ****** ****** */

typedef void *freeitmlst ;

/* ****** ****** */

typedef uintptr_t ats_uintptr1_type ;

typedef struct {
ats_ptr_type atslab_ptr ;
ats_int_type atslab_size ;
} freeitmptrsz_t ;

// ------------------------------------------------------
//
// a variety of locks
//
// ------------------------------------------------------

//

#ifdef _ATS_MULTITHREAD
extern pthread_mutex_t the_gc_main_lock ;
#endif

static inline
ats_void_type gc_main_lock_acquire () {
#ifdef _ATS_MULTITHREAD
  // fprintf (stderr, "gc_main_lock_acquire ()\n") ;
  pthread_mutex_lock (&the_gc_main_lock) ;
#endif
  return ;
}

static inline
ats_void_type gc_main_lock_release () {
#ifdef _ATS_MULTITHREAD
  // fprintf (stderr, "gc_main_lock_release ()\n") ;
  pthread_mutex_unlock (&the_gc_main_lock) ;
#endif
  return ;
}

//

#ifdef _ATS_MULTITHREAD
extern pthread_mutex_t the_globalentrylst_lock ;
#endif

static inline
ats_void_type the_globalentrylst_lock_acquire () {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_globalentrylst_lock) ;
#endif
  return ;
}

static inline
ats_void_type the_globalentrylst_lock_release () {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_globalentrylst_lock) ;
#endif
  return ;
}

//

#ifdef _ATS_MULTITHREAD
extern pthread_mutex_t the_manmemlst_lock ;
#endif

static inline
ats_void_type the_manmemlst_lock_acquire () {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_manmemlst_lock) ;
#endif
  return ;
}

static inline
ats_void_type the_manmemlst_lock_release () {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_manmemlst_lock) ;
#endif
  return ;
}

//

#ifdef _ATS_MULTITHREAD
extern pthread_mutex_t the_threadinfolst_lock ;
#endif

static inline
ats_void_type the_threadinfolst_lock_acquire () {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_threadinfolst_lock) ;
#endif // end of [ifdef _ATS_MULTITHREAD]
  return ;
}

static inline
ats_void_type the_threadinfolst_lock_release () {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_threadinfolst_lock) ;
#endif // end of [ifdef _ATS_MULTITHREAD]
  return ;
}

//

#ifdef _ATS_MULTITHREAD
extern pthread_mutex_t the_sweeplst_lock_array[FREEITMLST_ARRAYSIZE] ;
#endif

static inline
ats_void_type
the_sweeplst_lock_acquire_one (ats_int_type i) {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_sweeplst_lock_array[i]) ;
#endif
  return ;
}

static inline
ats_void_type
the_sweeplst_lock_release_one (ats_int_type i) {
#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_sweeplst_lock_array[i]) ;
#endif
  return ;
}

static inline
ats_void_type the_sweeplst_lock_acquire_all () {
#ifdef _ATS_MULTITHREAD
  int i ;
  for (i = 0; i < FREEITMLST_ARRAYSIZE; i += 1) {
    pthread_mutex_lock (&the_sweeplst_lock_array[i]) ;     
  }
#endif
  return ;
}

//

static inline
ats_void_type the_sweeplst_lock_release_all () {
#ifdef _ATS_MULTITHREAD
  int i ;
  for (i = FREEITMLST_ARRAYSIZE - 1; i >= 0; i -= 1) {
    pthread_mutex_unlock (&the_sweeplst_lock_array[i]) ;     
  }
#endif
  return ;
}

//

static inline
ats_void_type
the_sweeplst_lock_acquire_rest (ats_int_type i0) {
#ifdef _ATS_MULTITHREAD
  int i ;
  for (i = 0; i < i0 ; i += 1) {
    pthread_mutex_lock (&the_sweeplst_lock_array[i]) ;     
  }
  // skipping [i0]
  for (i = i0 + 1; i < FREEITMLST_ARRAYSIZE ; i += 1) {
    pthread_mutex_lock (&the_sweeplst_lock_array[i]) ;     
  }
#endif
  return ;
}

static inline
ats_void_type
the_sweeplst_lock_release_rest (ats_int_type i0) {
#ifdef _ATS_MULTITHREAD
  int i ;
  for (i = FREEITMLST_ARRAYSIZE - 1; i > i0 ; i -= 1) {
    pthread_mutex_unlock (&the_sweeplst_lock_array[i]) ;     
  }
  // skipping [i0]
  for (i = i0 - 1; i >= 0 ; i -= 1) {
    pthread_mutex_unlock (&the_sweeplst_lock_array[i]) ;     
  }
#endif
  return ;
}

// ------------------------------------------------------
//
// freeitmlst
//
// ------------------------------------------------------

static inline
ats_ptr_type freeitmlst2ptr (ats_ptr_type x) { return x ; }

//

static inline
ats_bool_type freeitmlst_is_nil (ats_ptr_type itms) {
  return (itms ? ats_false_bool : ats_true_bool) ;
}

static inline
ats_bool_type freeitmlst_is_cons (ats_ptr_type itms) {
  return (itms ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_ptr_type freeitmlst_cons (ats_ptr_type itm, ats_ptr_type itms) {
  *(freeitmlst*)itm = itms ; return itm ;
}

static inline
ats_ptr_type freeitmlst_tail_get (ats_ptr_type itms) {
  return *((freeitmlst*)itms) ;
}

//

extern freeitmlst the_freeitmlst_chunk_data ;

static inline
ats_void_type freeitmlst_chunk_data_free (ats_ptr_type _data) {
  free (_data) ; return ;
} /* end of [freeitmlst_chunk_data_free] */

static inline
ats_void_type freeitmlst_chunk_data_recycle (ats_ptr_type _data) {
  *((freeitmlst*)_data) = the_freeitmlst_chunk_data;
  the_freeitmlst_chunk_data = _data ;
  return ;
} /* end of [freeitmlst_chunk_data_recycle] */

//

#ifdef _ATS_MULTITHREAD
extern __thread freeitmlst *the_freeitmlst_array ;
#else /* single thread */
extern freeitmlst the_freeitmlst_array[FREEITMLST_ARRAYSIZE] ;
#endif

//

static inline
ats_ptr_type the_freeitmlst_array_get (ats_int_type i) {
  return the_freeitmlst_array[i] ;
}

static inline
ats_void_type
the_freeitmlst_array_set (ats_int_type i, ats_ptr_type itms) {
  the_freeitmlst_array[i] = itms ; return ;
}

static inline
ats_void_type
the_freeitmlst_array_clear_one (ats_int_type i) {
  the_freeitmlst_array[i] = (freeitmlst)0 ; return ;
}

static inline
ats_void_type
the_freeitmlst_array_insert_at (ats_ptr_type itm, ats_int_type i) {
  *(freeitmlst*)itm = the_freeitmlst_array[i] ;
  the_freeitmlst_array[i] = (freeitmlst)itm ;
  return ;
}

// ------------------------------------------------------
//
// chunk operations
//
// ------------------------------------------------------

typedef struct chunk_struct {
  int itemwsz ; // word size of each free item: must be positive!
  // if [itemwsz_log = -1], then the chunk is large
  int itemwsz_log ; // itemwsz_log = log2 (itemwsz) if itemwsz_log >= 0
  int itemtot ; // the total number of free items
  int markcnt ; // the count of marked free items
#ifdef _ATS_MULTITHREAD
  // the count of freeitms taken from this chunk that are being used by
  // other threads.
  // [freecnt != 0] indicates that the chunk currently supplies freeitms
  // to other threads, and thus cannot be put into the sweeplst array.
  int freecnt ;
#endif
  struct chunk_struct *sweep_next ; // next swept chunk
  freeitmlst data ; // pointer to the data
  freeitmlst data_nonalign ; // pointer to the truncated beginning
  byte markbits[0] ; //  bits for marking
} chunk ;

typedef chunk *chunklst ;

/* ****** ****** */

static inline
ats_ptr_type chunklst2ptr (ats_ptr_type x) { return x ; }

//

static inline
ats_bool_type chunklst_is_nil (ats_ptr_type chks) {
  return (chks ? ats_false_bool : ats_true_bool) ;
}

static inline
ats_bool_type chunklst_is_cons (ats_ptr_type chks) {
  return (chks ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_void_type chunk_header_free (ats_ptr_type chks) {
  free (chks) ; return ;
}

//

static inline
ats_int_type chunklst_itembsz_get (ats_ptr_type chks) {
  return (((chunklst)chks)->itemwsz) << NBYTE_PER_WORD_LOG ;
}

static inline
ats_int_type chunklst_itemwsz_get (ats_ptr_type chks) {
  return ((chunklst)chks)->itemwsz ;
}

static inline
ats_int_type chunklst_itemwsz_log_get (ats_ptr_type chks) {
  return ((chunklst)chks)->itemwsz_log ;
}

static inline
ats_int_type chunklst_itemtot_get (ats_ptr_type chks) {
  return ((chunklst)chks)->itemtot ;
}

static inline
ats_int_type chunklst_markcnt_get (ats_ptr_type chks) {
  return ((chunklst)chks)->markcnt ;
}

//

#ifdef _ATS_MULTITHREAD

static inline
ats_int_type chunklst_freecnt_get (ats_ptr_type chks) {
  return ((chunklst)chks)->freecnt ;
}

static inline
ats_void_type chunklst_freecnt_inc (ats_ptr_type chks) {
  ((chunklst)chks)->freecnt += 1; return ;
}

#endif /* end of [ifdef _ATS_MULTITHREAD] */

//

static inline
ats_ptr_type chunklst_data_get (ats_ptr_type chks) {
  return ((chunklst)chks)->data ;
}

static inline
ats_ptr_type chunklst_data_nonalign_get (ats_ptr_type chks) {
  return ((chunklst)chks)->data_nonalign ;
}

//

static inline
ats_ptr_type chunklst_markbits_get (ats_ptr_type chks) {
  return ((chunklst)chks)->markbits ;
}

static inline
ats_ptr_type chunklst_sweep_next_get (ats_ptr_type chks) {
  return ((chunklst)chks)->sweep_next ;
}

//

static inline
ats_void_type chunklst_markcnt_dec (ats_ptr_type chks) {
  ((chunklst)chks)->markcnt -= 1 ; return ;
}

static inline
ats_void_type chunklst_markcnt_inc (ats_ptr_type chks) {
  ((chunklst)chks)->markcnt += 1 ; return ;
}

//

extern int the_chunk_count ;
extern int the_chunk_count_limit ;

static inline
ats_int_type the_chunk_count_get () { return the_chunk_count ; }

//

static inline
ats_void_type the_chunk_count_dec_by (ats_int_type n) {
  the_chunk_count -= n; return ;
} /* end of [the_chunk_count_dec_by] */

static inline
ats_void_type the_chunk_count_inc_by (ats_int_type n) {
  the_chunk_count += n; return ;
} /* end of [the_chunk_count_inc_by] */

//

static inline
ats_bool_type the_chunk_count_limit_is_reached () { return
  (the_chunk_count >= the_chunk_count_limit) ? ats_true_bool : ats_false_bool ;
} /* the_chunk_count_limit_is_reached */

static inline
ats_bool_type the_chunk_count_limit_is_not_reached () { return
  (the_chunk_count >= the_chunk_count_limit) ? ats_false_bool : ats_true_bool ;
} /* the_chunk_count_limit_is_not_reached */

static inline
ats_bool_type
the_chunk_count_limit_is_reached_within (ats_int_type n) { return
  (the_chunk_count + n > the_chunk_count_limit) ? ats_true_bool : ats_false_bool ;
} /* the_chunk_count_limit_is_reached_within */

/* ****** ****** */

static inline
ats_void_type
gc_markbits_clear_chunk (ats_ptr_type chks) {
  int itemtot ; // total number of items
  int nmarkbit ; // number of bytes for mark bits
  itemtot = ((chunklst)chks)->itemtot ;
  nmarkbit = (itemtot + NBIT_PER_BYTE_MASK) >> NBIT_PER_BYTE_LOG ;
  memset (((chunklst)chks)->markbits, 0, nmarkbit) ;
  ((chunklst)chks)->markcnt = 0 ;
#ifdef _ATS_MULTITHREAD
  ((chunklst)chks)->freecnt = 0 ;
#endif
  return ;
} /* gc_markbits_clear_chunk */

/* ****** ****** */

/*

#define MARK_GET_mac(x, i) \
  (((x)[(i) / NBIT_PER_BYTE] >> ((i) % NBIT_PER_BYTE)) & 0x1)

*/

#define MARK_GET_mac(x, i) \
  (((x)[(i) >> NBIT_PER_BYTE_LOG] >> ((i) & NBIT_PER_BYTE_MASK)) & 0x1)

static inline
ats_bool_type MARK_GET (ats_ptr_type x, ats_int_type i) {
  return MARK_GET_mac((byte*)x, i) ;
}

/*

#define MARK_SET_mac(x, i) \
  do { \
    (x)[(i) / NBIT_PER_BYTE] |= (1 << ((i) % NBIT_PER_BYTE)) ; \
  } while (0)

*/

#define MARK_SET_mac(x, i) \
  do { \
    (x)[(i) >> NBIT_PER_BYTE_LOG] |= (1 << ((i) & NBIT_PER_BYTE_MASK)) ; \
  } while (0)

static inline
ats_void_type MARK_SET (ats_ptr_type x, ats_int_type i) {
  MARK_SET_mac((byte*)x, i) ; return ;
}

/*

#define MARK_CLEAR_mac(x, i) \
  do { \
    (x)[(i) / NBIT_PER_BYTE] &= ~(1 << ((i) % NBIT_PER_BYTE)) ; \
  } while (0)

*/

#define MARK_CLEAR_mac(x, i) \
  do { \
    (x)[(i) >> NBIT_PER_BYTE_LOG] &= ~(1 << ((i) & NBIT_PER_BYTE_MASK)) ; \
  } while (0)

static inline
ats_void_type MARK_CLEAR (ats_ptr_type x, ats_int_type i) {
  MARK_CLEAR_mac((byte*)x, i) ; return ;
}

/* ****** ****** */

#define PTR_TOPSEG_GET_mac(p) \
  ((p) >> (PTR_BOTCHKSEG_SIZE + NBYTE_PER_WORD_LOG))

static inline
ats_uintptr1_type PTR_TOPSEG_GET (ats_ptr_type p) {
  return PTR_TOPSEG_GET_mac((ats_uintptr1_type)p) ;
}

//

#define PTR_BOTSEG_GET_mac(p) \
  (((p) >> (PTR_CHKSEG_SIZE + NBYTE_PER_WORD_LOG)) & BOTSEG_TABLESIZE_MASK)

static inline
ats_int_type PTR_BOTSEG_GET (ats_ptr_type p) {
  return PTR_BOTSEG_GET_mac((ats_uintptr1_type)p) ;
}

//

#define PTR_CHKSEG_GET_mac(p) \
  (((p) >> NBYTE_PER_WORD_LOG) & CHUNK_WORDSIZE_MASK)

static inline
ats_int_type PTR_CHKSEG_GET (ats_ptr_type p) {
  return PTR_CHKSEG_GET_mac((ats_uintptr1_type)p) ;
}

//

typedef struct botsegtbl_struct {
#if (__WORDSIZE == 64)
  uintptr_t key ;
  struct botsegtbl_struct *hash_next ;
#endif
  chunklst headers[BOTSEG_TABLESIZE] ;
} botsegtbl ;

typedef botsegtbl *botsegtbllst ;

//

static inline
ats_bool_type botsegtbllst_is_nil (ats_ptr_type tbls) {
  return (tbls ? ats_false_bool : ats_true_bool) ;
}

static inline
ats_bool_type botsegtbllst_is_cons (ats_ptr_type tbls) {
  return (tbls ? ats_true_bool : ats_false_bool) ;
}

//

static inline
ats_ptr_type botsegtbllst_get
  (ats_ptr_type tbls, ats_int_type i) {
  return ((botsegtbllst)tbls)->headers[i] ;
}

static inline
ats_void_type botsegtbllst_set
  (ats_ptr_type tbls, ats_int_type i, ats_ptr_type chks) {
  ((botsegtbllst)tbls)->headers[i] = chks ; return ;
}

static inline
ats_void_type botsegtbllst_clear
  (ats_ptr_type tbls, ats_int_type i) {
  ((botsegtbllst)tbls)->headers[i] = (chunklst)0 ; return ;
}

//

#if (__WORDSIZE == 32)

extern botsegtbllst the_topsegtbl[TOPSEG_TABLESIZE] ;

static inline
ats_ptr_type the_topsegtbl_get_32 (ats_uintptr1_type ofs) {
  return the_topsegtbl[ofs] ;
}

static inline
ats_void_type the_topsegtbl_set_32
  (ats_uintptr1_type ofs, ats_ptr_type tbls) {
  the_topsegtbl[ofs] = tbls ; return ;
}

#endif // end of [__WORDSIZE == 32]

/* ****** ****** */

#if (__WORDSIZE == 64)

extern botsegtbllst the_topsegtbl[TOPSEG_HASHTABLESIZE] ;

static inline
ats_ptr_type
the_topsegtbl_get_64 (ats_uintptr1_type ofs) {
  botsegtbllst tbls = the_topsegtbl[ofs % TOPSEG_HASHTABLESIZE] ;
  while (tbls) { 
    if (tbls->key == ofs) break ; tbls = tbls->hash_next ;
  } /* end of [while] */
  return tbls ;
}

static inline
ats_void_type
the_topsegtbl_set_64
  (ats_uintptr1_type ofs, ats_ptr_type tbls) {
  the_topsegtbl[ofs % TOPSEG_HASHTABLESIZE] = tbls ; return ;
}

static inline
ats_ptr_type
the_topsegtbl_getfst_64 (ats_uintptr1_type ofs) {
  return the_topsegtbl[ofs % TOPSEG_HASHTABLESIZE] ;
}

#endif // end of [__WORDSIZE == 64]

/* ****** ****** */

// ------------------------------------------------------
//
// manmemlst: for manually managed list of allocated memories
//
// ------------------------------------------------------

typedef struct manmem_struct {
  int itemwsz ;
  struct manmem_struct *prev ;
  struct manmem_struct *next ;
  byte data[] ;
} manmem ;

typedef manmem *manmemlst ;

/* ****** ****** */

extern manmemlst the_manmemlst ;

//

static inline
ats_ptr_type the_manmemlst_get (void) {
  return the_manmemlst ;
}

//

static inline
ats_bool_type manmemlst_is_nil (ats_ptr_type mms) {
  return (mms ? ats_false_bool : ats_true_bool) ;
}

static inline
ats_bool_type manmemlst_is_cons (ats_ptr_type mms) {
  return (mms ? ats_true_bool : ats_false_bool) ;
}

//

static inline
ats_int_type manmemlst_itemwsz_get (ats_ptr_type mms) {
  return ((manmemlst)mms)->itemwsz ;
}

static inline
ats_ptr_type manmemlst_prev_get (ats_ptr_type mms) {
  return ((manmemlst)mms)->prev ;
}

static inline
ats_ptr_type manmemlst_next_get (ats_ptr_type mms) {
  return ((manmemlst)mms)->next ;
}

static inline
ats_ptr_type manmemlst_data_get (ats_ptr_type mms) {
  return ((manmemlst)mms)->data ;
}

// ------------------------------------------------------
//
// multithread
//
// ------------------------------------------------------

#ifdef _ATS_MULTITHREAD

typedef struct threadinfo_struct {
  pthread_t pid ;
  ats_ptr_type stack_beg ; ats_ptr_type stack_end ;
  struct threadinfo_struct *prev ;
  struct threadinfo_struct *next ;
  freeitmlst *freeitmlst_array ;
} threadinfo ;

typedef threadinfo *threadinfolst ;

#endif

// ------------------------------------------------------
//
// marking
//
// ------------------------------------------------------

typedef struct markstackpage_struct {
  struct markstackpage_struct *next ;
  struct markstackpage_struct *prev ;
  freeitmptrsz_t entries[MARKSTACK_PAGESIZE] ;
} markstackpage ;

typedef markstackpage *markstackpagelst ;

//

static inline
ats_ptr_type markstackpagelst_nil () {
  return (markstackpagelst)0 ;
}

static inline
ats_bool_type markstackpagelst_is_nil (ats_ptr_type msps) {
  return (msps ? ats_false_bool : ats_true_bool) ;
}

static inline
ats_bool_type markstackpagelst_is_cons (ats_ptr_type msps) {
  return (msps ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_ptr_type markstackpagelst_next_get (ats_ptr_type msps) {
  return ((markstackpagelst)msps)->next ;
}

static inline
ats_ptr_type markstackpagelst_prev_get (ats_ptr_type msps) {
  return ((markstackpagelst)msps)->prev ;
}

//

static inline
ats_void_type markstackpagelst_entry_get
  (ats_ptr_type msps, ats_int_type i, ats_ref_type p_r, ats_ref_type wsz_r) {
  freeitmptrsz_t *ents, *ents_i ;
  ents = ((markstackpagelst)msps)->entries ;
  ents_i = &(ents[i]) ;
  *(ats_ptr_type*)p_r = ents_i->atslab_ptr ;
  *(ats_int_type*)wsz_r = ents_i->atslab_size ;
  return ;
} /* end of [markstackpage_entry_ptr_get] */

static inline
ats_void_type markstackpagelst_entry_set
  (ats_ptr_type msps, ats_int_type i, ats_ptr_type p, ats_int_type wsz) {
  freeitmptrsz_t *ents, *ents_i ;
  ents = ((markstackpagelst)msps)->entries ;
  ents_i = &(ents[i]) ;
  ents_i->atslab_ptr = p ; ents_i->atslab_size= wsz ;
  return ;
} /* end of [markstackpage_entry_ptr_set] */

//

extern ats_int_type the_markstackposition ;
extern ats_ptr_type the_markstackpagelst_cur ;

static inline
ats_int_type the_markstack_pop
  (ats_ref_type ptr_r, ats_ref_type wsz_r) {
  return markstack_pop (
    &the_markstackpagelst_cur, &the_markstackposition, ptr_r, wsz_r
  ) ;
} /* end of [the_markstack_pop] */

static inline
ats_int_type the_markstack_push
  (ats_ptr_type ptr, ats_int_type wsz) {
  return markstack_push (
    &the_markstackpagelst_cur, &the_markstackposition, ptr, wsz
  ) ;
} /* end of [the_markstack_push] */

/* ****** ****** */

extern int the_markstatck_overflow ;

static inline
ats_int_type the_markstack_overflow_get () {
  return the_markstatck_overflow ;
}

static inline
ats_void_type the_markstack_overflow_set () {
  the_markstatck_overflow = 1 ; return ;
}

static inline
ats_void_type the_markstack_overflow_clear () {
  the_markstatck_overflow = 0 ; return ;
}

// ------------------------------------------------------
//
// collecting
//
// ------------------------------------------------------

extern chunklst the_sweeplst_array[FREEITMLST_ARRAYSIZE] ;

static inline
ats_void_type fprint_the_sweeplst_array_all () {
  int i ;
  fprintf (stderr, "fprint_the_sweeplst_array_all:\n") ;
  fprintf (stderr, "&the_sweeplst_array = %p\n", &the_sweeplst_array) ;
  for (i = 0; i < FREEITMLST_ARRAYSIZE; i += 1) {
    fprintf (stderr, "the_sweeplst_array[%i] = %p\n", i, the_sweeplst_array[i]) ;
  }
  return ;
}

static inline
ats_ptr_type the_sweeplst_array_get (ats_int_type i) {
/*
  fprintf (stderr, "the_sweeplst_array_get: i = %i\n", i) ;
  fprintf (stderr, "the_sweeplst_array_get: chks = %p\n", the_sweeplst_array[i]) ;
*/
  return the_sweeplst_array[i] ;
}

static inline
ats_void_type
the_sweeplst_array_set (ats_int_type i, ats_ptr_type chks) {
  // fprintf (stderr, "the_sweeplst_array_set: i = %i\n", i) ;
  // fprintf (stderr, "the_sweeplst_array_set: chks = %p\n", chks) ;
  the_sweeplst_array[i] = chks ; return ;
}

static inline
ats_void_type the_sweeplst_array_clear_one (ats_int_type i) {
  // fprintf (stderr, "the_sweeplst_array_clear_one: i = %i\n", i) ;
  the_sweeplst_array[i] = (chunklst)0 ; return ;
}

static inline
ats_void_type
the_sweeplst_array_insert_at
  (ats_ptr_type chks, ats_int_type i) {
  // fprintf (stderr, "the_sweeplst_array_insert_at: chks = %p\n", chks) ;
  // fprintf (stderr, "the_sweeplst_array_insert_at: i = %i\n", i) ;
  ((chunklst)chks)->sweep_next = the_sweeplst_array[i] ;
  the_sweeplst_array[i] = (chunklst)chks ;
  return ;
} /* end of [the_sweeplst_array_insert]

/* ****** ****** */

#endif /* __ATS_GC_CATS */

/* end of [gcats1.cats] */
