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
//
// HX: some proof functions involving view containment.
//
(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [vsubrw.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

prfun vsubr_intr {v1,v2:view}
  (fpf: v2 -<prf> [v:view] (v1, v)): vsubr_p (v1, v2)
// implemented in [vsubrw.dats]

prfun vsubr_elim {v1,v2:view}
  (pf: vsubr_p (v1, v2)):<> v2 -<prf> [v:view] (v1, v)
// implemented in [vsubrw.dats]

prfun vsubr_refl {v:view} (): vsubr_p (v, v)

prfun vsubr_trans {v1,v2,v3:view}
  (pf12: vsubr_p (v1, v2), pf23: vsubr_p (v2, v3)): vsubr_p (v1, v3)
// end of [vcontain_trans]

prfun vsubr_of_vsubw {v1,v2:view} (pf: vsubw_p (v1, v2)): vsubr_p (v1, v2)

prfun vsubr_tup_2_0 {v0,v1:view} (): vsubr_p (v0, @(v0, v1))
prfun vsubr_tup_2_1 {v0,v1:view} (): vsubr_p (v1, @(v0, v1))

(* ****** ****** *)

prfun vsubw_intr {v1,v2:view}
  (fpf: v2 -<prf> (v1, v1 -<lin,prf> v2)): vsubw_p (v1, v2)
// implemented in [vsubrw.dats]

prfun vsubw_elim {v1,v2:view}
  (pf: vsubw_p (v1, v2)):<> v2 -<prf> (v1, v1 -<lin,prf> v2)
// implemented in [vsubrw.dats]

prfun vsubw_tup_2_0 {v0,v1:view} (): vsubw_p (v0, @(v0, v1))
prfun vsubw_tup_2_1 {v0,v1:view} (): vsubw_p (v1, @(v0, v1))

(* ****** ****** *)

prval vsubw_array_elt :
  {a:viewtype} {n,i:nat | i < n} {l:addr} {ofs:int}
  MUL (i, sizeof a, ofs) -<> vsubw_p (a @ l + ofs, @[a][n] @ l)

prval vsubw_array_subarray :
  {a:viewtype} {n0,i,n:nat | i+n <= n0} {l:addr} {ofs:int}
  MUL (i, sizeof a, ofs) -<> vsubw_p (@[a][n] @ l + ofs, @[a][n0] @ l)

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [vsubrw.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* end of [vsubrw.sats] *)
