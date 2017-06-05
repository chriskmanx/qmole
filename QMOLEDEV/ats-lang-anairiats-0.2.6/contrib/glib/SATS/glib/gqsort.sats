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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "contrib/glib/CATS/glib/gqsort.cats"
%}

(* ****** ****** *)

//
// HX-2010-02-27: only need for individual testing
// staload "contrib/glib/SATS/gtypes.sats"
//

(* ****** ****** *)

fun g_qsort_with_data
  {a:viewt@ype} {vt:viewtype} {n:nat} (
  pbase: &(@[a][n])
, n: gint n
, size: sizeof_t a
, compare_func: GCompareDataFuncRef (a, vt)
, env: !vt
) :<> void = "mac#atsctrb_g_qsort_with_data"
// end of [g_qsort_with_data]

(* ****** ****** *)

(* end of [gqsort.sats] *)
