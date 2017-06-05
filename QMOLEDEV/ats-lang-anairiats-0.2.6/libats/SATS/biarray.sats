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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** Bidirectional Arrays (arrays moving from left to right and vice versa)
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: September, 2011
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

absview biarray_v (a:viewt@ype, n:int, lbeg:addr, lend:addr)

(* ****** ****** *)

prfun array_v_of_biarray_v
  {a:viewt@ype} {n:int} {l1,l2:addr}
  (pf: biarray_v (a, n, l1, l2)): array_v (a, n, l1)
// end of [array_v_of_biarray_v]

prfun biarray_v_of_array_v
  {a:viewt@ype} {n:int} {l:addr} {ofs:int} (
  pfmul: MUL (n, sizeof(a), ofs), pfarr: array_v (a, n, l)
) : biarray_v (a, n, l, l+ofs)

(* ****** ****** *)

prfun biarray_v_offset
  {a:viewt@ype} {n:int} {l1,l2:addr}
  (pf: !biarray_v (a, n, l1, l2)): MUL (n, sizeof(a), l2-l1)
// end of [biarray_v_offset]

(* ****** ****** *)

prfun biarray_v_nil
  {a:viewt@ype} {l:addr} (): biarray_v (a, 0, l, l)
// end of [biarray_v_nil]

prfun biarray_v_unnil
  {a:viewt@ype} {l1,l2:addr} (pf: biarray_v (a, 0, l1, l2)): [l1==l2] void
// end of [biarray_v_unnil]

(* ****** ****** *)

prfun biarray_v_sing
  {a:viewt@ype} {l:addr} (pf: a @ l): biarray_v (a, 1, l, l+sizeof(a))
// end of [biarray_v_sing]

prfun biarray_v_unsing
  {a:viewt@ype} {l1,l2:addr} (pf: biarray_v (a, 1, l1, l2)): a @ l1
// end of [biarray_v_unsing]

(* ****** ****** *)

prfun biarray_v_cons
  {a:viewt@ype} {n:nat} {lbeg,lend:addr} (
  pf1: a @ lbeg, pf2: biarray_v (a, n, lbeg+sizeof(a), lend)
) : biarray_v (a, n+1, lbeg, lend) // end of [biarray_v_cons]

prfun biarray_v_uncons
  {a:viewt@ype} {n:pos} {lbeg,lend:addr}
  (pf: biarray_v (a, n, lbeg, lend)) : (
  a @ lbeg, biarray_v (a, n-1, lbeg+sizeof(a), lend)
) // end of [biarray_v_uncons]

(* ****** ****** *)

prfun biarray_v_snoc
  {a:viewt@ype} {n:nat} {lbeg,lend:addr} (
  pf1: biarray_v (a, n, lbeg, lend), pf2: a @ lend
) : biarray_v (a, n+1, lbeg, lend+sizeof(a))

prfun biarray_v_unsnoc
  {a:viewt@ype} {n:pos} {lbeg,lend:addr}
  (pf: biarray_v (a, n, lbeg, lend)) : (
  biarray_v (a, n-1, lbeg, lend-sizeof(a)), a @ lend-sizeof(a)
) // end of [biarray_v_unsnoc]

(* ****** ****** *)

(* end of [biarray.sats] *)
