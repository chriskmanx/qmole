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

fun gtk_scale_set_draw_value
  {c:cls | c <= GtkScale}
  {l:agz} (
  scale: !gobjref (c, l), draw_value: gboolean
) : void
  = "mac#atsctrb_gtk_scale_set_draw_value"
// end of [fun]

fun gtk_scale_set_digits
  {c:cls | c <= GtkScale}
  {l:agz} (
  scale: !gobjref (c, l), digits: gint
) : void
  = "mac#atsctrb_gtk_scale_set_digits"
// end of [fun]

fun gtk_scale_set_value_pos
  {c:cls | c <= GtkScale}
  {l:agz} (
  scale: !gobjref (c, l), pos: GtkPositionType
) : void
  = "mac#atsctrb_gtk_scale_set_value_pos"
// end of [fun]

(* ****** ****** *)

(* end of [gtkscale.sats] *)
