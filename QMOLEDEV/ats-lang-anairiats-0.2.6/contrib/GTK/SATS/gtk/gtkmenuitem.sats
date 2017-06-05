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

fun gtk_menu_item_new
  (): GtkMenuItem_ref1 = "mac#atsctrb_gtk_menu_item_new"
// end of [gtk_menu_item_new]

fun gtk_menu_item_new_with_label
  {l:agz} (name: !gstring l): GtkMenuItem_ref1
  = "mac#atsctrb_gtk_menu_item_new_with_label"
// end of [gtk_menu_item_new_with_label]

fun gtk_menu_item_new_with_mnemonic
  {l:agz} (name: !gstring l): GtkMenuItem_ref1
  = "mac#atsctrb_gtk_menu_item_new_with_mnemonic"
// end of [gtk_menu_item_new_with_mnemonic]

(* ****** ****** *)

//
// HX-2010-05-06: the returned gstring can be NULL!
//
fun gtk_menu_item_get_label
  {c:cls | c <= GtkMenuItem}
  {l:agz} (
  item: !gobjref (c, l)
) : [l1:addr] (
  minus (gobjref (c, l), gstring l1)
| gstring l1
) = "mac#atsctrb_gtk_menu_item_get_label"
// end of [gtk_menu_item_get_label]

fun gtk_menu_item_set_label
  {c:cls | c <= GtkMenuItem}
  {l1,l2:agz} (
  item: !gobjref (c, l1), name: !gstring l2
) : void
  = "mac#atsctrb_gtk_menu_item_set_label"
// end of [gtk_menu_item_set_label]

(* ****** ****** *)

fun gtk_menu_item_set_submenu
  {c1,c2:cls | c1 <= GtkMenuItem; c2 <= GtkWidget}
  {l1,l2:addr | l2 > null} (
  menuitem: !gobjref (c1, l1)
, submenu: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_menu_item_set_submenu"
// end of [gtk_menu_item_set_submenu]

(* ****** ****** *)

(* end of [gtkmenuitem.sats] *)
