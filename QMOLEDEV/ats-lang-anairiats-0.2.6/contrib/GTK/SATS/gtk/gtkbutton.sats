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

fun gtk_button_new
  (): GtkButton_ref1 = "mac#atsctrb_gtk_button_new"
// end of [gtk_button_new]

fun gtk_button_new_with_label
  {l:agz} (label: !gstring l): GtkButton_ref1
  = "mac#atsctrb_gtk_button_new_with_label"
// end of [gtk_button_new_with_label]

fun gtk_button_new_with_mnemonic
  {l:agz} (label: !gstring l): GtkButton_ref1
  = "mac#atsctrb_gtk_button_new_with_mnemonic"
// end of [gtk_button_new_with_mnemonic]

fun gtk_button_new_from_stock
  {l:agz} (stock_id: !gstring l): GtkButton_ref1
  = "mac#atsctrb_gtk_button_new_from_stock"
// end of [gtk_button_new_from_stock]

(* ****** ****** *)

//
// HX-2010-04-26: the label string belongs to the widget!
// HX-2010-05-07: the label string can be NULL (if it is not set)
//
fun gtk_button_get_label
  {c:cls | c <= GtkButton}
  {l1:agz} (
  widget: !gobjref (c, l1)
) : [l2:addr] (
  minus (gobjref (c, l1), gstring l2) | gstring l2
) = "mac#atsctrb_gtk_button_get_label"

//
// HX-2010-05-07: checked: label = NULL -> labe = ""
//
fun gtk_button_set_label
  {c:cls | c <= GtkButton}
  {l1,l2:agz} (
  widget: !gobjref (c, l1)
, label: gstring l2
) : void
  = "mac#atsctrb_gtk_button_set_label"
// end of [gtk_button_set_label]

(* ****** ****** *)

(* end of [gtkbutton.sats] *)
