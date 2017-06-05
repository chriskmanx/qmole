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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//
(* ****** ****** *)

fun gdk_color3_set (
  color: &GdkColor, r: uint, b: uint, g: uint
) :<> void
  = "mac#atsctrb_gdk_color3_set"
// end of [gdk_color3_set]

fun gdk_color4_set (
  color: &GdkColor, pix: uint, r: uint, b: uint, g: uint
) :<> void
  = "mac#atsctrb_gdk_color4_set"
// end of [gdk_color4_set]

(* ****** ****** *)

(*
GdkColor *gdk_color_copy      (const GdkColor *color);
void      gdk_color_free      (GdkColor       *color);
*)

fun gdk_color_copy (
  color: &GdkColor
) : [l:addr] (GdkFree_v l, GdkColor @ l | ptr l)
  = "mac#atsctrb_gdk_color_copy"
// end of [gdk_color_copy]

fun gdk_color_free {l:addr} (
  pf1: GdkFree_v l, pf2: GdkColor @ l | p: ptr l
) : void = "mac#atsctrb_gdk_color_free"
// end of [gdk_color_free]
  
(* ****** ****** *)

fun gdk_color_parse {l:agz} (
  spec: !gstring l, color: &GdkColor? >> opt (GdkColor, b)
) : #[b:bool] gboolean b
  = "mac#atsctrb_gdk_color_parse"
// end of [gdk_color_parse]

(* ****** ****** *)

(* end of [gdkcolor.sats] *)
