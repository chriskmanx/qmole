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

fun gtk_statusbar_new
  (): GtkStatusbar_ref1 = "mac#atsctrb_gtk_statusbar_new"
// end of [gtk_statusbar_new]

fun gtk_statusbar_get_context_id
  {c:cls | c <= GtkStatusbar}
  {l1,l2:agz} (
  bar: !gobjref (c, l1)
, context: !gstring l2
) : guint
  = "mac#atsctrb_gtk_statusbar_get_context_id"
// end of [gtk_statusbar_get_context_id]

(* ****** ****** *)

fun gtk_statusbar_push
  {c:cls | c <= GtkStatusbar}
  {l1,l2:agz} (
  bar: !gobjref (c, l1)
, context_id: guint
, text: !gstring l2
) : guint(*msgid*)
  = "mac#atsctrb_gtk_statusbar_push"
// end of [gtk_statusbar_push]

fun gtk_statusbar_pop
  {c:cls | c <= GtkStatusbar}
  {l:agz} (
  bar: !gobjref (c, l), context_id: guint
 ) : void
  = "mac#atsctrb_gtk_statusbar_pop"
// end of [gtk_statusbar_pop]

fun gtk_statusbar_remove
  {c:cls | c <= GtkStatusbar}
  {l:agz} (
  bar: !gobjref (c, l)
, context_id: guint
, message_id: guint
) : void
  = "mac#atsctrb_gtk_statusbar_remove"
// end of [gtk_statusbar_remvoe]

(* ****** ****** *)

fun gtk_statusbar_get_has_resize_grip
  {c:cls | c <= GtkStatusbar}
  {l:agz} (
  bar: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_statusbar_get_has_resize_grip"
// end of [gtk_statusbar_get_has_resize_grip]

fun gtk_statusbar_set_has_resize_grip
  {c:cls | c <= GtkStatusbar}
  {l:agz} (
  bar: !gobjref (c, l), setting: gboolean
) : void
  = "mac#atsctrb_gtk_statusbar_set_has_resize_grip"
// end of [gtk_statusbar_set_has_resize_grip]

(* ****** ****** *)

(* end of [gtkstatusbar.sats] *)
