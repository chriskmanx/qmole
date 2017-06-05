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

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_autops_"

(* ****** ****** *)

#define MALLOC_WORDSIZE_MAX (1 << 30) // temporarily

implement gc_aut_malloc_bsz (bsz) = let
  val wsz = (bsz + NBYTE_PER_WORD_MASK) >> NBYTE_PER_WORD_LOG
in
  gc_aut_malloc_wsz (wsz)
end // end of [gc_aut_malloc_wsz]

extern fun log2_ceil (n: int): Nat = "log2_ceil"

implement gc_aut_malloc_wsz (wsz) = let
(*
  val () = begin
    prerr "gc_aut_malloc_wsz: wsz = "; prerr wsz; prerr_newline ()
  end // end of [val]
*)
  val itemwsz_log = begin
    if wsz > MAX_CHUNK_BLOCK_WORDSIZE then ~1 else log2_ceil (wsz)
  end : Int
in
  case+ 0 of
  | _ when itemwsz_log >= 0 => let
      val () = assert // redundant at run-time
        (itemwsz_log <= MAX_CHUNK_BLOCK_WORDSIZE_LOG)
      val itms = the_freeitmlst_array_get (itemwsz_log)
      val itms = (
        if freeitmlst_is_nil itms then gc_freeitmlst_generate (itemwsz_log)
        else itms
      ) : freeitmlst1
      val ptr = freeitmlst2ptr itms
#if ATS_GC_RUNTIME_CHECK
      // checking that [ptr] is valid for [malloc]
      var ofs: int = 0
      val chks = gc_ptr_is_valid (ptr, ofs)
      val () = assert_errmsg
         (chunklst_is_cons chks, "gc_malloc_wsz: [chks] is nil: ")
      val itemwsz = chunklst_itemwsz_get (chks)
      val markbits = chunklst_markbits_get (chks)
      val mb = MARK_GET (markbits, ofs)
      val () = assert_errmsg (mb = 0, "gc_malloc_wsz: [mb = 0] failed: ")
      val () = MARK_SET (markbits, ofs)
      val () = chunklst_markcnt_inc (chks)
#endif
      val itms1 = freeitmlst_tail_get itms
      val () = the_freeitmlst_array_set (itemwsz_log, itms1)
(*
      extern fun prerr_self_pid (): void = "prerr_self_pid"
      val () = begin
        prerr "gc_aut_malloc_wsz: pid = "; prerr_self_pid (); prerr_newline ()
      end
*)
(*
      val () = begin
        prerr "gc_aut_malloc_wsz: return: ptr = "; prerr ptr; prerr_newline ()
      end
*)
    in
      ptr // return value
    end // end of [_ when ...]
  | _ => let // [itemwsz_log = -1]
      #define itemwsz wsz // [itemwsz] is [wsz]
      val () = // [itemwsz] is too large
        if itemwsz > MALLOC_WORDSIZE_MAX then let
          val () = begin
            prerr "[gc_aut_malloc_wsz]: argument is too large!"; prerr_newline ()
          end
        in
          exit (1)
        end // end of [if]
      val nchunk = (itemwsz + CHUNK_WORDSIZE_MASK) >> CHUNK_WORDSIZE_LOG
      val (pf_gc_main | ()) = gc_main_lock_acquire ()
      val is_reached_within = begin
        the_chunk_count_limit_is_reached_within (pf_gc_main | nchunk)
      end
      val () = // do garbage collection if needed
        if is_reached_within then let
          val (pf_globals | ()) = the_globalentrylst_lock_acquire ()
          val (pf_manmemlst | ()) = the_manmemlst_lock_acquire ()
          val (pf_threads | ()) = the_threadinfolst_lock_acquire ()
          val (pf_sweeplst_all | ()) = the_sweeplst_lock_acquire_all ()
          val () = gc_collect (
            pf_gc_main, pf_globals, pf_manmemlst, pf_threads, pf_sweeplst_all | (*none*)
          ) // end of [gc_collect]
          val () = the_sweeplst_lock_release_all (pf_sweeplst_all | (*none*))
          val () = the_threadinfolst_lock_release (pf_threads | (*none*))
          val () = the_manmemlst_lock_release (pf_manmemlst | (*none*))
          val () = the_globalentrylst_lock_release (pf_globals | (*none*))
        in
          // empty
        end // end of [if]
      val chks = chunklst_create (pf_gc_main | ~1(*itemwsz_log*), itemwsz)
      val () = gc_main_lock_release (pf_gc_main | (*none*))
      val itms = chunklst_data_get (chks)
      val ptr = freeitmlst2ptr itms
(*
      val () = begin
        prerr "gc_aut_malloc_wsz: return: ptr = "; prerr ptr; prerr_newline ()
      end
*)
    in
      ptr // return value
    end // end of [_]
end // end of [gc_aut_malloc_wsz]

(* ****** ****** *)

extern fun gc_aut_calloc_bsz (n(*all*): int, bsz(*each*): int): ptr
  = "gc_aut_calloc_bsz"

%{^

static inline
ats_void_type gc_aut_calloc_bsz_memset_bsz
  (ats_ptr_type p, ats_int_type c, ats_int_type bsz) {
  memset (p, c, bsz) ; return ;
}

%}

implement gc_aut_calloc_bsz (n, bsz) = let
(*
  val () = begin
    prerr "gc_aut_calloc_bsz: n = "; prerr n; prerr_newline ()
  end
  val () = begin
    prerr "gc_aut_calloc_bsz: bsz = "; prerr bsz; prerr_newline ()
  end
*)
  val nbsz = n * bsz
  val ptr = gc_aut_malloc_bsz (nbsz)
  val () = _memset_bsz (ptr, 0, nbsz) where {
    extern fun _memset_bsz (p: ptr, c: int, sz: int): void =
      "gc_aut_calloc_bsz_memset_bsz"
  } // end of [where]
(*
  val () = begin
    prerr "gc_aut_calloc_bsz: return: itms = "; prerr ptr; prerr_newline ()
  end
*)
in
  ptr // return value
end // end of [gc_aut_calloc_bsz]

(* ****** ****** *)

// [gc_aut_free] is supported by the availability of linear datatypes.

fn gc_aut_free_chunk
  (chks: chunklst1, ptr: ptr): void = let
(*
  val () = begin
    prerr "gc_aut_free_chunk: chks = ?"; prerr_newline ()
  end
*)
  val itemwsz_log = chunklst_itemwsz_log_get (chks)
(*
  val () = begin
    prerr "gc_aut_free_chunk: itemwsz_log = "; prerr itemwsz_log; prerr_newline ()
  end
*)
in
  if itemwsz_log >= 0 then let
#if ATS_GC_RUNTIME_CHECK
    // checking that [ptr] is valid for [free]
    val ofs_chkseg = PTR_CHKSEG_GET (ptr)
    val itemwsz = 1 << itemwsz_log
    val markbits = chunklst_markbits_get (chks)
    val () = assert_errmsg (
      ofs_chkseg mod itemwsz = 0
    , "gc_aut_free_chunk: [ofs_chkseg mod itemwsz = 0] failed: "
    ) // end of [val]
    val ofs = ofs_chkseg >> itemwsz_log
    val mb = MARK_GET (markbits, ofs)
    val () =
      if mb <> 1 then begin
        prerr "gc_aut_free_chunk: ptr = "; prerr ptr; prerr_newline ();
        prerr "gc_aut_free_chunk: itemwsz = "; prerr itemwsz; prerr_newline ()
      end
    val () = assert_errmsg
      (mb = 1, "gc_aut_free_chunk: [mb = 1] failed: ")
    val () = MARK_CLEAR (markbits, ofs)
    val () = chunklst_markcnt_dec (chks)
#endif
    val () = the_freeitmlst_array_insert_at (ptr, itemwsz_log)
  in
    // empty
  end else let // large item
    val itemtot = chunklst_itemtot_get (chks)
  in
    if (itemtot = 1) then let
      val (pf_main_lock | ()) = gc_main_lock_acquire ()
      val () = chunklst_destroy
        (pf_main_lock | chks) // the chunk contains no other items
      val () = gc_main_lock_release (pf_main_lock | (*none*))
    in
      // empty
    end else begin
      // no leak is caused here as GC can reclaim the freeitm later
      // this can happen only if [MAX_CHUNK_BLOCK_WORDSIZE < CHUNK_WORDSIZE]
    end // end of [it]
  end // end of [if]
end // end of [gc_aut_free_chunk]

//

implement gc_aut_free (ptr) = let
(*
  val () = begin
    prerr "gc_aut_free: ptr = "; prerr ptr; prerr_newline ()
  end
*)
  fn err_botsegtbl (ptr: ptr): void = begin
    prerr "GC: Fatal Error: [gc_aut_free] failed";
    prerr ": invalid pointer (botsegtbl is nil): "; prerr ptr;
    prerr_newline (); exit {void} (1)
  end
  fn err_chunk (ptr: ptr): void = begin
    prerr "GC: Fatal Error: [gc_aut_free] failed";
    prerr ": invalid pointer (chunk is nil): "; prerr ptr;
    prerr_newline (); exit {void} (1)
  end
  val (pf | ofs_topseg) = PTR_TOPSEG_GET (ptr)
  val tbls = the_topsegtbl_get (pf | ofs_topseg)
in
  case+ 0 of
  | _ when botsegtbllst_is_cons tbls => let
      val ofs_botseg = PTR_BOTSEG_GET (ptr)
      val chks = botsegtbllst_get (tbls, ofs_botseg)
    in
      case+ 0 of
      | _ when chunklst_is_cons (chks) => gc_aut_free_chunk (chks, ptr)
      | _ => err_chunk (ptr)
    end // end of [_ when ...]
  | _ => err_botsegtbl (ptr)
end // end of [gc_aut_free]

// implement gc_aut_free (ptr) = ()

(* ****** ****** *)

implement gc_aut_realloc_bsz (ptr, bsz) = let
  val wsz = (bsz + NBYTE_PER_WORD_MASK) >> NBYTE_PER_WORD_LOG
in
  gc_aut_realloc_wsz (ptr, wsz)
end // end of [gc_aut_relloc_wsz]

//

%{^

static inline
ats_void_type gc_aut_realloc_wsz_memcpy_wsz
  (ats_ptr_type p_dest, ats_ptr_type p_src, ats_int_type wsz) {
  memcpy (p_dest, p_src, wsz << NBYTE_PER_WORD_LOG) ;
  return ;
}

%}

//

implement
gc_aut_realloc_wsz (ptr, wsz_new) = let
(*
  val () = begin
    prerr "gc_aut_realloc_wsz: ptr = "; prerr ptr; prerr_newline ()
  end
  val () = begin
    prerr "gc_aut_realloc_wsz: wsz_new = "; prerr wsz_new; prerr_newline ()
  end
*)
  fn err (ptr: ptr): ptr = begin
    prerr "GC: Fatal Error: [gc_aut_free] failed";
    prerr ": invalid pointer: "; prerr ptr; prerr_newline ();
    exit {ptr} (1)
  end
  val (pf | ofs_topseg) = PTR_TOPSEG_GET (ptr)
  val tbls = the_topsegtbl_get (pf | ofs_topseg)
  fn aux_main
    (chks: chunklst1, ptr: ptr, wsz_new: int)
    : ptr = let
    extern fun _memcpy_wsz
      (_dest: ptr, _src: ptr, wsz: int): void = "gc_aut_realloc_wsz_memcpy_wsz"
    val itemwsz = chunklst_itemwsz_get (chks)
  in
    case+ 0 of
    | _ when wsz_new > itemwsz => let
(*
        val () = begin
          prerr "gc_aut_realloc_wsz: enlarging"; prerr_newline ()
        end
*)
        val ptr_new = gc_aut_malloc_wsz (wsz_new)
        val () = _memcpy_wsz (ptr_new, ptr, itemwsz)
        val () = gc_aut_free_chunk (chks, ptr)
      in
        ptr_new // return a new value
      end
    | _ when 2 * wsz_new <= itemwsz => let
(*
        val () = begin
          prerr "gc_aut_realloc_wsz: shrinking"; prerr_newline ()
        end
*)
        val ptr_new = gc_aut_malloc_wsz (wsz_new)
        // note: [wsz_new <= itemwsz / 2 < wsz]
        val () = _memcpy_wsz (ptr_new, ptr, wsz_new)
        val () = gc_aut_free_chunk (chks, ptr)
      in
        ptr_new // return a new value
      end
    | _ => ptr // return the original value
  end // end of [aux_main]
in
//
if ptr > null then (
  case+ 0 of
  | _ when botsegtbllst_is_cons tbls => let
      val ofs_botseg = PTR_BOTSEG_GET (ptr)
      val chks = botsegtbllst_get (tbls, ofs_botseg)
    in
      case+ 0 of
      | _ when chunklst_is_cons (chks) => let
          val ptr_new = aux_main (chks, ptr, wsz_new)
(*
          val () = begin
            prerr "gc_aut_realloc_wsz: return: ptr_new = "; prerr ptr_new ;
            prerr_newline ()
          end
*)
        in
          ptr_new
        end // end of [_ when ...]
      | _ => err (ptr)
    end
  | _ => err (ptr)
) else (
  gc_aut_malloc_wsz (wsz_new)
) // end of [if]
end // end of [gc_aut_realloc]

(* ****** ****** *)

(* end of [gcats1_autops.dats] *)
