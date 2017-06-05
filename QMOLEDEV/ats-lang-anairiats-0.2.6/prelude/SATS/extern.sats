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
// Start Time: January 29, 2010
//
(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)
//
// HX: this file is mostly used for building API's for external packages.
// The "tricks" presented here should be used sparringly in cases that do
// require special attention.
//
(* ****** ****** *)

viewdef vptrout // virtual ownership
  (a:viewt@ype, l:addr) = (a @ l, a @ l -<lin,prf> void)
// end of [vptrout]
viewdef vptroutopt // optional virtual ownership
  (a:viewt@ype, l:addr) = option_v (vptrout (a, l), l > null)
// end of [vptroutopt]

(* ****** ****** *)
//
// HX: note that (vt1 \minus v2) roughly means that a ticket of
// [v2] is taken from [vt1]; the ticket must be returned before
// [vt1] is consumed.
//
absview
minus_viewt0ype_view
  (vt1: viewt@ype, v2: view) = vt1
stadef minus = minus_viewt0ype_view
prfun minus_addback
  {vt1:viewt@ype} {v2:view} (pf1: minus (vt1, v2), pf2: v2 | x: !vt1): void
// end of [minus_addback]

(* ****** ****** *)
//
// HX-2010-04-18:
// the type [stamped] should only be used in a situation
// where the value cannot be uniquely identified by its type
//
absviewtype
stamped (a:viewtype, l:addr) = a
prfun stamped_encode
  {a:viewtype} (x: !a >> stamped (a, l)):<> #[l:addr] void
// end of [stamped_encode]
prfun stamped_decode
  {a:viewtype} {l:addr} (x: !stamped (a, l) >> a):<> void
// end of [stamped_decode]

(* ****** ****** *)
//
// HX-2010-11-26: mrsw: multiple-read-single-write
//
absviewt@ype
mrsw_viewt0ype_int_viewt0ype (a:viewt@ype, n:int) = a
stadef mrsw = mrsw_viewt0ype_int_viewt0ype
//
praxi mrsw_encode {a:viewt@ype} (x: !a >> mrsw (a, 0)):<prf> void
praxi mrsw_decode {a:viewt@ype} (x: !mrsw (a, 0) >> a):<prf> void
//
(* ****** ****** *)

(* end of [extern.sats] *)
