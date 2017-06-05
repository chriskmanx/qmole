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
// Start Time: April, 2010
//
(* ****** ****** *)

fun gtk_radio_button_new
  {n:nat} (
  group: !GSList_ptr (gpointer, n)
) : GtkRadioButton_ref1 = "mac#atsctrb_gtk_radio_button_new"
// end of [gtk_radio_button_new]

fun gtk_radio_button_new_with_label
  {n:nat} {l:agz} (
  group: !GSList_ptr (gpointer, n), name: !gstring l
) : GtkRadioButton_ref1
  = "mac#atsctrb_gtk_radio_button_new_with_label"
// end of [gtk_radio_button_new_with_label]

fun gtk_radio_button_new_with_mnemonic
  {n:nat} {l:agz} (
  group: !GSList_ptr (gpointer, n), name: !gstring l
) : GtkRadioButton_ref1
  = "mac#atsctrb_gtk_radio_button_new_with_mnemonic"
// end of [gtk_radio_button_new_with_mnemonic]

fun gtk_radio_button_new_from_widget
  {c:cls | c <= GtkRadioButton}
  {l:agz} (
  group: !gobjref (c, l)
) : GtkRadioButton_ref1
  = "mac#atsctrb_gtk_radio_button_new_from_widget"
// end of [gtk_radio_button_new_from_widget]

fun gtk_radio_button_new_with_label_from_widget
  {c:cls | c <= GtkRadioButton}
  {l1,l2:agz} (
  group: !gobjref (c, l1), name: !gstring l2
) : GtkRadioButton_ref1
  = "mac#atsctrb_gtk_radio_button_new_with_label_from_widget"
// end of [gtk_radio_button_new_with_label_from_widget]

(* ****** ****** *)

fun gtk_radio_button_get_group
  {c:cls | c <= GtkRadioButton}
  {l:agz} (
  button: !gobjref (c, l)
) : (
  minus (gobjref (c, l), GSList_ptr0 (gpointer)) | GSList_ptr0 (gpointer)
) = "mac#atsctrb_gtk_radio_button_get_group"
// end of [gtk_radio_button_get_group]

(* ****** ****** *)

(* end of [gtkradiobutton.sats] *)
