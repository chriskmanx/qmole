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
** Reversed Arrays (arrays running from right to left)
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: September, 2010
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

dataview
rarray_v (
  a:viewt@ype+, int(*size*), addr(*loc*)
) = // for arrays in reversal order (from right to left)
  | {n:nat} {l:addr}
    rarray_v_cons (a, n+1, l+sizeof a) of (rarray_v (a, n, l), a @ l)
  | {l:addr} rarray_v_nil (a, 0, l) of ()
// end of [rarray_v]

(*
absview rarray_v (a:viewt@ype+, int(*size*), addr(*loc*))
*)
(* ****** ****** *)

prfun rarray_v_unnil
  {a:viewt@ype} {l:addr} (pfarr: rarray_v (a, 0, l)):<> void
// end of [rarray_v_unnil]

prfun rarray_v_uncons
  {a:viewt@ype} {n:pos} {l:addr}
  (pfarr: rarray_v (a, n, l)):<> (rarray_v (a, n-1, l-sizeof a), a @ l-sizeof a)
// end of [rarray_v_uncons]

(* ****** ****** *)

prfun rarray_v_of_array_v
  {a:viewt@ype} {n:nat} {l:addr} {ofs:int} (
  pfmul: MUL (n, sizeof a, ofs), pfarr: array_v (a, n, l)
) :<> rarray_v (a, n, l+ofs) // end of [rarray_v_of_array_v]

prfun array_v_of_rarray_v
  {a:viewt@ype} {n:nat} {l:addr} {ofs:int} (
  pfmul: MUL (n, sizeof a, ofs), pfarr: rarray_v (a, n, l)
) :<> array_v (a, n, l-ofs) // end of [rarray_v_of_array_v]

(* ****** ****** *)

fun{a:viewt@ype}
array2rarray_ptr
  {n:nat} {l:addr} (pf: array_v (a, n, l) | p: ptr l, n: size_t n)
  :<> [ofs:int] (MUL (n, sizeof a, ofs), rarray_v (a, n, l+ofs) | ptr (l+ofs))
// end of [array2rarray_ptr]

fun{a:viewt@ype}
rarray2array_ptr
  {n:nat} {l:addr} (pf: rarray_v (a, n, l) | p: ptr l, n: size_t n)
  :<> [ofs:int] (MUL (n, sizeof a, ofs), array_v (a, n, l-ofs) | ptr (l-ofs))
// end of [rarray2array_ptr]

(* ****** ****** *)

(*
** HX-2010-09-20: implemented in ATS (libats/DATS/rarray.dats)
*)

fun rarray_ptr_foreach_funenv_tsz
  {a:viewt@ype}
  {v:view} {vt:viewtype}
  {n:nat} {l:addr} (
  pf: !v, pfarr: !rarray_v (a, n, l)
| p: ptr l
, f: (!v | &a, !vt) -<> void, asz: size_t n, tsz: sizeof_t a
, env: !vt
) :<> void // end of [rarray_ptr_foreach_funenv_tsz]

fun{a:viewt@ype}
rarray_ptr_foreach_fun
  {n:nat} {l:addr} (
  pfarr: !rarray_v (a, n, l)
| p: ptr l, f: (&a) -<fun> void, asz: size_t n
) :<> void // end of [rarray_ptr_foreach_fun]

fun{a:viewt@ype}
rarray_ptr_foreach_vclo
  {v:view} {n:nat} {l:addr} (
  pf: !v, pfarr: !rarray_v (a, n, l)
| p: ptr l, f: &(!v | &a) -<clo> void, asz: size_t n
) :<> void // end of [rarray_ptr_foreach_vclo]

(* ****** ****** *)

fun{a:viewt@ype}
array_ptr_rforeach_vclo {v:view} {n:nat} {l:addr} (
  pf: !v | A: &(@[a][n]), f: &(!v | &a) -<clo> void, asz: size_t n
) :<> void // end of [array_ptr_rforeach_vclo]

(* ****** ****** *)

(* end of [rarray.sats] *)
