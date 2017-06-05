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
// Start Time: May, 2010
//
(* ****** ****** *)

abst@ype GtkFileChooserAction = $extype"GtkFileChooserAction"
macdef GTK_FILE_CHOOSER_ACTION_OPEN =
  $extval (GtkFileChooserAction, "GTK_FILE_CHOOSER_ACTION_OPEN")
macdef GTK_FILE_CHOOSER_ACTION_SAVE =
  $extval (GtkFileChooserAction, "GTK_FILE_CHOOSER_ACTION_SAVE")
macdef GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER =
  $extval (GtkFileChooserAction, "GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER")
macdef GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER =
  $extval (GtkFileChooserAction, "GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER")

(* ****** ****** *)

abst@ype GtkFileChooserConfirmation = $extype"GtkFileChooserConfirmation"
macdef GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM =
  $extval (GtkFileChooserConfirmation, "GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM")
macdef GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME =
  $extval (GtkFileChooserConfirmation, "GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME")
macdef GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN =
  $extval (GtkFileChooserConfirmation, "GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN")

(* ****** ****** *)

fun gtk_file_chooser_get_action
  {c:cls | c <= GtkFileChooser} {l:agz}
  (chooser: !gobjref (c, l)): GtkFileChooserAction
  = "mac#atsctrb_gtk_file_chooser_get_action"
// end of [gtk_file_chooser_get_action]

fun gtk_file_chooser_set_action
  {c:cls | c <= GtkFileChooser} {l:agz}
  (chooser: !gobjref (c, l), action: GtkFileChooserAction): void
  = "mac#atsctrb_gtk_file_chooser_set_action"
// end of [gtk_file_chooser_set_action]

(* ****** ****** *)

//
// HX-2010-05-05:
// the return may be a NULL gstring
//
fun gtk_file_chooser_get_filename
  {c:cls | c <= GtkFileChooser} {l:agz}
  (chooser: !gobjref (c, l)): gstring0 // it is copied out
  = "mac#atsctrb_gtk_file_chooser_get_filename"
// end of [gtk_file_chooser_get_filename]

fun gtk_file_chooser_set_filename
  {c:cls | c <= GtkFileChooser} {l1,l2:agz}
  (chooser: !gobjref (c, l1), filename: !gstring l2): gboolean
  = "mac#atsctrb_gtk_file_chooser_set_filename"
// end of [gtk_file_chooser_set_filename]

(* ****** ****** *)

fun gtk_file_chooser_set_current_name
  {c:cls | c <= GtkFileChooser} {l1,l2:agz}
  (chooser: !gobjref (c, l1), name: !gstring l2): void
  = "mac#atsctrb_gtk_file_chooser_set_current_name"
// end of [gtk_file_chooser_set_current_name]

(* ****** ****** *)

fun gtk_file_chooser_get_do_overwrite_confirmation
  {c:cls | c <= GtkFileChooser}
  {l:agz} (chooser: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_file_chooser_get_do_overwrite_confirmation"
// end of [gtk_file_chooser_get_do_overwrite_confirmation]

fun gtk_file_chooser_set_do_overwrite_confirmation
  {c:cls | c <= GtkFileChooser}
  {l:agz} (chooser: !gobjref (c, l), yesno: gboolean): void
  = "mac#atsctrb_gtk_file_chooser_set_do_overwrite_confirmation"
// end of [gtk_file_chooser_set_do_overwrite_confirmation]

(* ****** ****** *)

(* end of [gtkfilechooser.sats] *)
