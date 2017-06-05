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

fun gtk_check_menu_item_new
  (): GtkCheckMenuItem_ref1 = "mac#atsctrb_gtk_check_menu_item_new"
// end of [gtk_check_menu_item_new]

fun gtk_check_menu_item_new_with_label
  {l:agz} (name: !gstring l): GtkCheckMenuItem_ref1
  = "mac#atsctrb_gtk_check_menu_item_new_with_label"
// end of [gtk_check_menu_item_new_with_label]

fun gtk_check_menu_item_new_with_mnemonic
  {l:agz} (name: !gstring l): GtkCheckMenuItem_ref1
  = "mac#atsctrb_gtk_check_menu_item_new_with_mnemonic"
// end of [gtk_check_menu_item_new_with_mnemonic]

(* ****** ****** *)

fun gtk_check_menu_item_get_active
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_check_menu_item_get_active"
// end of [gtk_check_menu_item_get_active]

fun gtk_check_menu_item_set_active
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l), active: gboolean): void
  = "mac#atsctrb_gtk_check_menu_item_set_active"
// end of [gtk_check_menu_item_set_active]

(* ****** ****** *)

fun gtk_check_menu_item_toggled
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l)): void = "mac#atsctrb_gtk_check_menu_item_toggled"
// end of [gtk_check_menu_item_toggled]

(* ****** ****** *)

fun gtk_check_menu_item_get_inconsistent
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_check_menu_item_get_inconsistent"
// end of [gtk_check_menu_item_get_inconsistent]

fun gtk_check_menu_item_set_inconsistent
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l), inconsistent: gboolean): void
  = "mac#atsctrb_gtk_check_menu_item_set_inconsistent"
// end of [gtk_check_menu_item_set_inconsistent]

(* ****** ****** *)

fun gtk_check_menu_item_get_draw_as_radio
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_check_menu_item_get_draw_as_radio"
// end of [gtk_check_menu_item_get_draw_as_radio]

fun gtk_check_menu_item_set_draw_as_radio
  {c:cls | c <= GtkCheckMenuItem} {l:agz}
  (item: !gobjref (c, l), draw_as_radio: gboolean): void
  = "mac#atsctrb_gtk_check_menu_item_set_draw_as_radio"
// end of [gtk_check_menu_item_set_draw_as_radio]

(* ****** ****** *)

(* end of [gtkcheckmenuitem.sats] *)
