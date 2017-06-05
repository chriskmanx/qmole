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

// the code handles accumulation of global roots for GC

(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_globalentry_"

(* ****** ****** *)

viewdef entrypage_v (l:addr) =
  array_v (freeitmptrsz (), GLOBALENTRYPAGESIZE, l)

// this may be unnecessarily complicated: a linked list should just be fine.
dataviewtype globalentrylst =
    // the slots with indexes in (u, GLOBALENTRYPAGESIZE) are all used
  | {u:int | ~1 <= u; u < GLOBALENTRYPAGESIZE} {l:addr}
      GLOBALENTRYLSTcons of (entrypage_v l | ptr l, int u, globalentrylst)
  | GLOBALENTRYLSTnil
// end of [globalentrylst]

//

extern fun globalentry_insert
  {u,n:nat | u < GLOBALENTRYPAGESIZE} {l0,l:addr} (
  pf_arr: !entrypage_v l0, pf: freeitm n @ l | p0: ptr l0, u: int u, p: ptr l, wsz: int n
) : void // end of [extern]
  = "globalentry_insert"

extern fun globalentrylst_insert {n:nat} {l:addr}
  (pf: freeitm n @ l | ents: &globalentrylst, p: ptr l, wsz: int n): void
  = "globalentrylst_insert"

extern fun globalentrylst_extend_insert {n:nat} {l:addr}
  (pf: freeitm n @ l | ents: &globalentrylst, p: ptr l, wsz: int n): void

//

implement globalentrylst_insert (pf | ents, p, wsz) = let
  extern prfun __leak {n:nat} {l:addr} (pf: freeitm n @ l): void
in
  case+ 0 of
  | _ when wsz > 0 => begin case+ ents of
    | GLOBALENTRYLSTcons (!pf_arr | p_arr, !used, _) => let
        val u = !used
      in
        if u >= 0 then let
          val () = globalentry_insert (!pf_arr, pf | p_arr, u, p, wsz)
        in
          !used := u - 1; fold@ ents
        end else begin
          fold@ ents; globalentrylst_extend_insert (pf | ents, p, wsz)
        end // end of [if]
      end // end of [GLOBALENTRYLSTcons]
    | GLOBALENTRYLSTnil () => begin
        fold@ ents; globalentrylst_extend_insert (pf | ents, p, wsz)
      end // end of [GLOBALENTRYLSTnil]
    end // end of [_ when ...]
  | _ => begin // no pointer inside!
      let prval () = __leak (pf) in () end 
    end
end // end of [globalentrylst_insert]

implement globalentrylst_extend_insert (pf | ents, p, wsz) = let
(*
  val () = begin
    prerr "globalentrylst_extend_insert: p = "; prerr p; prerr_newline ();
    prerr "globalentrylst_extend_insert: wsz = "; prerr wsz; prerr_newline ()
  end // end of [val]
*)
  val (pf_arr | p_arr) = entrypage_alloc () where {
    extern fun entrypage_alloc (): [l:addr] (entrypage_v l | ptr l)
      = "entrypage_alloc"
  }
  val u = GLOBALENTRYPAGESIZE - 1
  val () = globalentry_insert (pf_arr, pf | p_arr, u, p, wsz)
in
  ents := GLOBALENTRYLSTcons (pf_arr | p_arr, u - 1, ents)
end // end of [globalentrylst_extend_insert]

(* ****** ****** *)

extern fun gc_mark_entrypage
  {u:int | ~1 <= u; u < GLOBALENTRYPAGESIZE} {l:addr}
  (pf: !entrypage_v l | p0: ptr l, u(*used if higher*): int u): void
  = "gc_mark_entrypage"

extern fun gc_mark_globalentrylst (ents: !globalentrylst): void
  = "gc_mark_globalentrylst"

implement gc_mark_globalentrylst (ents) = begin
  case+ ents of
  | GLOBALENTRYLSTcons (!pf | p0, u, !ents1) => let
      val () = gc_mark_entrypage (!pf | p0, u)
      val () = gc_mark_globalentrylst (!ents1)
    in
      fold@ (ents)
    end // end of [GLOBALENTRYLSTcons]
  | GLOBALENTRYLSTnil () => begin
      fold@ (ents)
    end // end of [GLOBALENTRYLSTnil]
end // end of [gc_mark_globalentrylst]

(* ****** ****** *)

%{$

ats_ptr_type entrypage_alloc () {
  void* p ;
  p = malloc (GLOBALENTRYPAGESIZE * sizeof(freeitmptrsz_t)) ;
/*
  fprintf (stderr, "entrypage_alloc: p = %p\n", p) ;
*/
  if (!p) {
    fprintf (stderr, "GC Fatal Error: [gc_globalentry.dats]") ;
    fprintf (stderr, ": [entrypage_alloc]: [malloc] failed.\n") ;
    exit (1) ;
  } // end of [if]

  return p ;
} /* end of [entrypage_alloc] */

/* ****** ****** */

ats_void_type globalentry_insert
  (ats_ptr_type p0, ats_int_type u, ats_ptr_type p, ats_int_type wsz) {
  freeitmptrsz_t *p0_u ;
  p0_u = (freeitmptrsz_t*)p0 + u ;
  p0_u->atslab_ptr = p ; p0_u->atslab_size = wsz ;
  return ;
} /* globalentry_insert */

/* ****** ****** */

extern ats_void_type gc_mark_ptr (ats_ptr_type) ;

ats_void_type gc_mark_entrypage
  (ats_ptr_type p0, ats_int_type u/*used if higher*/) {
  
  int i, j ; freeitmptrsz_t *p0_i ; freeitmlst *p_j ; int wsz ;

  i = u + 1 ; p0_i = (freeitmptrsz_t*)p0 + i ;
/*
  fprintf (stderr, "gc_mark_entrypage: enter\n") ;
*/
  while (i < GLOBALENTRYPAGESIZE) { // termination: obvious
    p_j = (freeitmlst*)(p0_i->atslab_ptr) ; wsz = p0_i->atslab_size ;
/*
    fprintf (stderr, "gc_mark_entrypage: p_j = %p(%i)\n", p_j, p_j) ;
*/
    for (j = 0; j < wsz; j += 1, p_j += 1) { gc_mark_ptr (*p_j) ; }
    i += 1 ; p0_i += 1 ;
  } // end of [while]
/*
  fprintf (stderr, "gc_mark_entrypage: leave\n") ;
*/
  return ;
} /* end of [gc_mark_entrypage] */

%}

(* ****** ****** *)

(* end of [gcats1_globalentrylst.dats] *)
