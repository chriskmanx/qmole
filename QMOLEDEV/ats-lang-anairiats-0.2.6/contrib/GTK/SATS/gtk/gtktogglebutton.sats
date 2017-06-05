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

fun gtk_toggle_button_new
  (): GtkToggleButton_ref1 = "mac#atsctrb_gtk_toggle_button_new"
// end of [gtk_toggle_button_new]

fun gtk_toggle_button_new_with_label
  {l:agz} (
  name: !gstring l
) : GtkToggleButton_ref1
  = "mac#atsctrb_gtk_toggle_button_new_with_label"
// end of [gtk_toggle_button_new_with_label]

fun gtk_toggle_button_new_with_mnemonic
  {l:agz} (
  name: !gstring l
) : GtkToggleButton_ref1
  = "mac#atsctrb_gtk_toggle_button_new_with_mnemonic"
// end of [gtk_toggle_button_new_with_mnemonic]

(* ****** ****** *)

fun gtk_toggle_button_get_active
  {c:cls | c <= GtkToggleButton}
  {l:agz} (
  button: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_toggle_button_get_active"
// end of [gtk_toggle_button_get_active]

fun gtk_toggle_button_set_active
  {c:cls | c <= GtkToggleButton}
  {l:agz} (
  button: !gobjref (c, l), isactive: gboolean
) : void
  = "mac#atsctrb_gtk_toggle_button_set_active"
// end of [gtk_toggle_button_set_active]

(* ****** ****** *)

(* end of [gtktogglebutton.sats] *)
