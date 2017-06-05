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

abst@ype
GtkToolbarSpaceStyle = $extype"GtkToolbarSpaceStyle"
macdef GTK_TOOLBAR_SPACE_EMPTY =
  $extval (GtkToolbarSpaceStyle, "GTK_TOOLBAR_SPACE_EMPTY")
macdef GTK_TOOLBAR_SPACE_LINE =
  $extval (GtkToolbarSpaceStyle, "GTK_TOOLBAR_SPACE_LINE")

(* ****** ****** *)

fun gtk_toolbar_new
  (): GtkToolbar_ref1 = "mac#atsctrb_gtk_toolbar_new"
// end of [gtk_toolbar_new]

(* ****** ****** *)

fun gtk_toolbar_get_style
  {c:cls | c <= GtkToolbar}
  {l:agz} (
  toolbar: !gobjref (c, l)
) : GtkToolbarStyle = "mac#atsctrb_gtk_toolbar_get_style"
// end of [gtk_toolbar_get_style]

fun gtk_toolbar_set_style
  {c:cls | c <= GtkToolbar}
  {l:agz} (
  toolbar: !gobjref (c, l), style: GtkToolbarStyle
) : void
  = "mac#atsctrb_gtk_toolbar_set_style"
// end of [gtk_toolbar_set_style]

(* ****** ****** *)

fun gtk_toolbar_insert
  {c1,c2:cls | c1 <= GtkToolbar; c2 <= GtkToolItem}
  {l1,l2:agz} (
  bar: !gobjref (c1, l1)
, itm: !gobjref (c2, l2)
, position: gint
) : void
  = "mac#atsctrb_gtk_toolbar_insert"
// end of [gtk_toolbar_insert]

(* ****** ****** *)

(* end of [gtktoolbar.sats] *)
