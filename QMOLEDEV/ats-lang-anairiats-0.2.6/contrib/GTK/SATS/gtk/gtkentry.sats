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

fun gtk_entry_new
  (): GtkEntry_ref1 = "mac#atsctrb_gtk_entry_new"
// end of [gtk_entry_new]

(* ****** ****** *)

fun gtk_entry_get_visibility
  {c:cls | c <= GtkEntry}
  {l:agz} (
  entry: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_entry_get_visibility"
// end of [gtk_entry_get_visibility]

fun gtk_entry_set_visibility
  {c:cls | c <= GtkEntry}
  {l:agz} (
  entry: !gobjref (c, l), visibility: gboolean
) : void
  = "mac#atsctrb_gtk_entry_set_visibility"
// end of [gtk_entry_set_visibility]

(* ****** ****** *)

fun gtk_entry_get_editable
  {c:cls | c <= GtkEntry}
  {l:agz} (
  entry: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_entry_get_editable"
// end of [gtk_entry_get_editable]

fun gtk_entry_set_editable
  {c:cls | c <= GtkEntry}
  {l:agz} (
  entry: !gobjref (c, l), editable: gboolean
) : void
  = "mac#atsctrb_gtk_entry_set_editable"
// end of [gtk_entry_set_editable]

(* ****** ****** *)

fun gtk_entry_get_max_length
  {c:cls | c <= GtkEntry} {l:agz} (entry: !gobjref (c, l)): gint
  = "mac#atsctrb_gtk_entry_get_max_length"
// end of [gtk_entry_get_max_length]

fun gtk_entry_set_max_length
  {c:cls | c <= GtkEntry}
  {l:agz} (
  entry: !gobjref (c, l), max: gint
) : void
  = "mac#atsctrb_gtk_entry_set_max_length"
// end of [gtk_entry_set_max_length]

(* ****** ****** *)

//
// HX-2010-05-07: assumed: the return cannot be NULL!
//
fun gtk_entry_get_text
  {c:cls | c <= GtkEntry} {l1:agz}
  (entry: !gobjref (c, l1))
  : [l2:agz] (
  minus (gobjref (c, l1), gstring l2)
| gstring l2
) = "mac#atsctrb_gtk_entry_get_text"
// end of [gtk_entry_get_text]

//
// HX-2010-05-07: checked: the input [text] cannot be NULL!
//
fun gtk_entry_set_text
  {c:cls | c <= GtkEntry}
  {l1,l2:agz} (
  entry: !gobjref (c, l1), text: !gstring l2
) : void = "mac#atsctrb_gtk_entry_set_text"
// end of [gtk_entry_set_text]

(* ****** ****** *)

(* end of [gtkentry.sats] *)
