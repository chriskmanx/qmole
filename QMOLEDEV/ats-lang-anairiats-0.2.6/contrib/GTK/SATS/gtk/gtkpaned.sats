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

fun gtk_paned_add1
  {c1,c2:cls | c1 <= GtkPaned; c2 <= GtkWidget}
  {l1,l2:agz} (
  paned: !gobjref (c1, l1)
, child: !gobjref (c2, l2)
) : void = "mac#atsctrb_gtk_paned_add1"
// end of [gtk_paned_add1]

fun gtk_paned_add2
  {c1,c2:cls | c1 <= GtkPaned; c2 <= GtkWidget}
  {l1,l2:agz} (
  paned: !gobjref (c1, l1)
, child: !gobjref (c2, l2)
) : void = "mac#atsctrb_gtk_paned_add2"
// end of [gtk_paned_add2]

(* ****** ****** *)

fun gtk_paned_pack1
  {c1,c2:cls | c1 <= GtkPaned; c2 <= GtkWidget}
  {l1,l2:agz} (
  paned: !gobjref (c1, l1)
, child: !gobjref (c2, l2)
, expand: gboolean
, shrink: gboolean
) : void = "mac#atsctrb_gtk_paned_pack1"
// end of [gtk_paned_pack1]

fun gtk_paned_pack2
  {c1,c2:cls | c1 <= GtkPaned; c2 <= GtkWidget}
  {l1,l2:agz} (
  paned: !gobjref (c1, l1)
, child: !gobjref (c2, l2)
, expand: gboolean
, shrink: gboolean
) : void = "mac#atsctrb_gtk_paned_pack2"
// end of [gtk_paned_pack2]

(* ****** ****** *)
//
// HX-2010-05-12:
// checked: the reference count of the widget taken out is unchanged
//
fun gtk_paned_get_child1
  {c1:cls | c1 <= GtkPaned} {l1:agz} (
  paned: !gobjref (c1, l1)
) : [c2:cls;l2:addr | c2 <= GtkWidget] (
  minus (gobjref (c1, l1), gobjref (c2, l2)) | gobjref (c2, l2)
) = "mac#atsctrb_gtk_paned_get_child1"
// end of [gtk_paned_get_child1]

fun gtk_paned_get_child2
  {c1:cls | c1 <= GtkPaned} {l1:agz} (
  paned: !gobjref (c1, l1)
) : [c2:cls;l2:addr | c2 <= GtkWidget] (
  minus (gobjref (c1, l1), gobjref (c2, l2)) | gobjref (c2, l2)
) = "mac#atsctrb_gtk_paned_get_child2"
// end of [gtk_paned_get_child2]

(* ****** ****** *)

fun gtk_paned_get_position
  {c:cls | c <= GtkPaned}
  {l:agz} (
  paned: !gobjref (c, l)
 ) : gint
  = "mac#atsctrb_gtk_paned_get_position"
// end of [gtk_paned_get_position]

fun gtk_paned_set_position
  {c:cls | c <= GtkPaned}
  {l:agz} (
  paned: !gobjref (c, l), position: gint
) : void
  = "mac#atsctrb_gtk_paned_set_position"
// end of [gtk_paned_set_position]

(* ****** ****** *)

fun gtk_paned_get_handle_window
  {c1:cls | c1 <= GtkPaned} {l1:agz} (
  paned: !gobjref (c1, l1)
) : [l2:addr] (
  minus (gobjref (c1, l1), gobjref (GtkWindow, l2))
| gobjref (GtkWindow, l2)
) = "mac#atsctrb_gtk_paned_get_handle_window"
// end of [gtk_paned_get_handle_window]

(* ****** ****** *)

(* end of [gtkpaned.sats] *)
