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

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: 2008
//

(* ****** ****** *)

//
// some common functions that iterate over natural numbers;
// The code mainly serves as an example for writing iterative loops
// in ATS
//

(* ****** ****** *)

fun foreach_funenv
  {v:view} {vt: viewtype} {n:nat} {f:eff}
  (pf: !v | n: int n, f: (!v | natLt n, !vt) -<f> void, env: !vt):<f> void
// end of [foreach_funenv]

fun foreach_fun {n:nat} {f:eff}
  (n: int n, f: (natLt n) -<f> void):<f> void
// end of [foreach_fun]

fun foreach_vclo {v:view} {n:nat} {f:eff}
  (pf: !v | n: int n, f: &(!v | natLt n) -<clo,f> void):<f> void
// end of [foreach_vclo]

// this one is the usual functional version
fun foreach_cloref {n:nat} {f:eff}
  (n: int n, f: (natLt n) -<cloref,f> void):<f> void
// end of [foreach_cloref]

(* ****** ****** *)

//
// HX-2010-03-23:
// the implementation for this function is row-major
//
fun foreach2_funenv
  {v:view} {vt: viewtype} {m,n:nat} {f:eff} (
  pf: !v
| m: int m, n: int n
, f: (!v | natLt m, natLt n, !vt) -<f> void
, env: !vt
) :<f> void // end of [foreach2_funenv]

fun foreach2_fun {m,n:nat} {f:eff}
  (m: int m, n: int n, f: (natLt m, natLt n) -<f> void) :<f> void
// end of [foreach2_fun]

fun foreach2_vclo {v:view} {m,n:nat} {f:eff}
  (pf: !v | m: int m, n: int n, f: &(!v | natLt m, natLt n) -<clo,f> void) :<f> void
// end of [foreach2_vclo]

// this one is the usual functional version
fun foreach2_cloref {m,n:nat} {f:eff}
  (m: int m, n: int n, f: (natLt m, natLt n) -<cloref,f> void) :<f> void
// end of [foreach2_cloref]

(* ****** ****** *)

fun repeat_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff}
  (pf: !v | n: int n, f: (!v | !vt) -<f> void, env: !vt):<f> void
// end of [repeat_funenv]

fun repeat_fun {n:nat} {f:eff} (n: int n, f: () -<f> void):<f> void

fun repeat_vclo {v:view} {n:nat} {f:eff}
  (pf: !v | n: int n, f: &(!v | (*none*)) -<clo,f> void):<f> void
// end of [repeat_vclo]

fun repeat_cloref
  {n:nat} {f:eff} (n: int n, f: () -<cloref,f> void):<f> void
// end of [repeat_cloref]

(* ****** ****** *)

(* end of [iterint.sats] *)
