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
// Time: May, 2010
//
(* ****** ****** *)

fun gdk_pixbuf_render_threshold_alpha
  {c1,c2:cls | c1 <= GdkPixbuf; c2 <= GdkBitmap}
  {l1,l2:agz} (
  pixbuf: !gobjref (c1, l1)
, bitmap: !gobjref (c2, l2)
, src_x: int
, src_y: int
, dst_x: int
, dst_y: int
, width: int
, height: int
, alpha_threshold: int
) : void 
  = "mac#atsctrb_gdk_pixbuf_render_threshold_alpha"
// end of [gdk_pixbuf_render_threshold_alpha]

(* ****** ****** *)

(* end of [gdkpixbuf.sats] *)
