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
//
// HX-2010-04-19: this is all deprecated!!!
//
(* ****** ****** *)

fun gtk_file_selection_new
  {l:agz} (title: !gstring l): GtkFileSelection_ref1
  = "mac#atsctrb_gtk_file_selection_new"
// end of [gtk_file_selection_new]

(* ****** ****** *)

fun gtk_file_selection_get_ok_button
  {c:cls | c <= GtkFileSelection} {l:agz}
  (filesel: !gobjref (c, l)):<> [l_btn:agz] (
    minus (gobjref (c, l), gobjref (GtkButton, l_btn)) | gobjref (GtkButton, l_btn)
  ) = "atsctrb_gtk_file_selection_get_ok_button"
// end of [gtk_file_selection_get_ok_button]

fun gtk_file_selection_get_cancel_button
  {c:cls | c <= GtkFileSelection} {l:agz}
  (filesel: !gobjref (c, l)):<> [l_btn:agz] (
    minus (gobjref (c, l), gobjref (GtkButton, l_btn)) | gobjref (GtkButton, l_btn)
  ) = "atsctrb_gtk_file_selection_get_cancel_button"
// end of [gtk_file_selection_get_cancel_button]

(* ****** ****** *)

fun gtk_file_selection_get_filename
  {c:cls | c <= GtkFileSelection} {l1:agz}
  (filesel: !gobjref (c, l1))
  : [l2:addr] (minus (gobjref (c, l1), gstring l2) | gstring l2)
  = "mac#atsctrb_gtk_file_selection_get_filename"
// end of [gtk_file_selection_get_filename]

fun gtk_file_selection_set_filename
  {c:cls | c <= GtkFileSelection} {l1,l2:agz} (
    filesel: !gobjref (c, l1), filename: !gstring l2
  ) : void = "mac#atsctrb_gtk_file_selection_set_filename"
// end of [gtk_file_selection_set_filename]

(* ****** ****** *)

(* end of [gtkfilesel.sats] *)
