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

abst@ype
GtkDialogFlags = $extype"GtkDialogFlags"

macdef GTK_DIALOG_MODAL =
  $extval (GtkDialogFlags, "GTK_DIALOG_MODAL")
macdef GTK_DIALOG_DESTROY_WITH_PARENT =
  $extval (GtkDialogFlags, "GTK_DIALOG_DESTROY_WITH_PARENT")
macdef GTK_DIALOG_NO_SEPARATOR =
  $extval (GtkDialogFlags, "GTK_DIALOG_NO_SEPARATOR")

fun lor_GtkDialogFlags_GtkDialogFlags (
  x1: GtkDialogFlags, x2: GtkDialogFlags
) :<> GtkDialogFlags
  = "mac#atsctrb_lor_GtkDialogFlags_GtkDialogFlags"
overload lor with lor_GtkDialogFlags_GtkDialogFlags

(* ****** ****** *)

abst@ype
GtkResponseType = $extype"GtkResponseType"

symintr GtkResponseType
castfn GtkResponseType_of_int (x: int):<>GtkResponseType
overload GtkResponseType with GtkResponseType_of_int

castfn gint_of_GtkResponseType (x: GtkResponseType):<> gint

(*
** GTK returns this if a response widget has no response_id,
** or if the dialog gets programmatically hidden or destroyed.
*)
// GTK_RESPONSE_NONE = -1
macdef GTK_RESPONSE_NONE = $extval (GtkResponseType, "GTK_RESPONSE_NONE")

(*
** GTK won't return these unless you pass them in
** as the response for an action widget. They are
** for your convenience.
*)
// GTK_RESPONSE_REJECT = -2
macdef GTK_RESPONSE_REJECT = $extval (GtkResponseType, "GTK_RESPONSE_REJECT")
// GTK_RESPONSE_ACCEPT = -3
macdef GTK_RESPONSE_ACCEPT = $extval (GtkResponseType, "GTK_RESPONSE_ACCEPT")

(* If the dialog is deleted *)
// GTK_RESPONSE_DELETE_EVENT = -4
macdef GTK_RESPONSE_DELETE_EVENT = $extval (GtkResponseType, "GTK_RESPONSE_DELETE_EVENT")

(*
** These are returned from GTK dialogs, and you can also use them
** yourself if you like.
*)
// GTK_RESPONSE_OK     = -5
macdef GTK_RESPONSE_OK = $extval (GtkResponseType, "GTK_RESPONSE_OK")
// GTK_RESPONSE_CANCEL = -6
macdef GTK_RESPONSE_CANCEL = $extval (GtkResponseType, "GTK_RESPONSE_CANCEL")
// GTK_RESPONSE_CLOSE  = -7
macdef GTK_RESPONSE_CLOSE = $extval (GtkResponseType, "GTK_RESPONSE_CLOSE")
// GTK_RESPONSE_YES    = -8
macdef GTK_RESPONSE_YES = $extval (GtkResponseType, "GTK_RESPONSE_YES")
// GTK_RESPONSE_NO     = -9
macdef GTK_RESPONSE_NO = $extval (GtkResponseType, "GTK_RESPONSE_NO")
// GTK_RESPONSE_APPLY  = -10
macdef GTK_RESPONSE_APPLY = $extval (GtkResponseType, "GTK_RESPONSE_APPLY")
// GTK_RESPONSE_HELP   = -11
macdef GTK_RESPONSE_HELP = $extval (GtkResponseType, "GTK_RESPONSE_HELP")

(* ****** ****** *)

fun gtk_dialog_new ()
  : GtkDialog_ref1 = "mac#atsctrb_gtk_dialog_new"
// end of [gtk_dialog_new]

(*
// HX: this one is not supported directly in ATS
GtkWidget*
gtk_dialog_new_with_buttons (
  const gchar *title
, GtkWindow *parent
, GtkDialogFlags flags
, const gchar *first_button_text
, ...
) ;
*)

(* ****** ****** *)

//
// HX-2010-04: needed?
// HX-2010-05-02:
// Yes. E.g, it may be needed for setting transient-window property
// for the parent window.
//
fun gtk_dialog_get_window
  {c:cls | c <= GtkDialog} {l:agz}
  (dialog: !gobjref (c, l))
  :<> [l_win:agz] ( // this one is actually 'getref'
  minus (gobjref (c, l), gobjref (GtkWindow, l_win)) | gobjref (GtkWindow, l_win)
) = "mac#atsctrb_gtk_dialog_get_window"
// end of [gtk_dialog_get_window]

fun gtk_dialog_get_vbox
  {c:cls | c <= GtkDialog} {l:agz}
  (dialog: !gobjref (c, l)):<> [l_box:agz] (
  minus (gobjref (c, l), gobjref (GtkVBox, l_box)) | gobjref (GtkVBox, l_box)
) = "mac#atsctrb_gtk_dialog_get_vbox"
// end of [gtk_dialog_get_vbox]

fun gtk_dialog_get_action_area
  {c:cls | c <= GtkDialog} {l:agz}
  (dialog: !gobjref (c, l)):<> [l_box:agz] (
  minus (gobjref (c, l), gobjref (GtkHBox, l_box)) | gobjref (GtkHBox, l_box)
) = "mac#atsctrb_gtk_dialog_get_action_area"
// end of [gtk_dialog_get_action_area]

(* ****** ****** *)

fun gtk_dialog_add_button
  {c:cls | c <= GtkDialog} {l1,l2:agz}
  (dialog: !gobjref (c, l1), name: !gstring l2, response_id: GtkResponseType)
  : [l3:agz] (
  minus (gobjref (c, l1), gobjref (GtkButton, l3)) | gobjref (GtkButton, l3)
) = "mac#atsctrb_gtk_dialog_add_button"
// end of [gtk_dialog_add_button]

(* ****** ****** *)
//
// HX-2010-04-10:
// If the diaglog widget is destroyed in the middle, all bets are off!!!
//
fun gtk_dialog_run
  {c:cls | c <= GtkDialog} {l:agz}
  (dialog: !gobjref (c, l)): gint = "mac#atsctrb_gtk_dialog_run"
// end of [gtk_dialog_run]

(* ****** ****** *)

fun gtk_dialog_response
  {c:cls | c <= GtkDialog}
  {l:agz} (
  dialog: !gobjref (c, l), response_id: gint
) : void = "mac#atsctrb_gtk_dialog_response"
// end of [gtk_dialog_response]

(* ****** ****** *)

(* end of [gtkdialog.sats] *)
