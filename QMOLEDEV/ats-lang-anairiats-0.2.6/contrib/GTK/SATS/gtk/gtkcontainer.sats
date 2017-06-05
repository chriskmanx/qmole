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

fun gtk_container_add
  {c1,c2:cls | c1 <= GtkContainer; c2 <= GtkWidget}
  {l1,l2:agz} (
  container: !gobjref (c1, l1), widget: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_container_add"
// end of [gtk_container_add]

fun gtk_container_remove
  {c1,c2:cls | c1 <= GtkContainer; c2 <= GtkWidget}
  {l1,l2:agz} (
  container: !gobjref (c1, l1), widget: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_container_remove"
// end of [gtk_container_remove]

(* ****** ****** *)

fun gtk_container_set_border_width
  {c:cls | c <= GtkContainer}
  {l:agz} (
  container: !gobjref (c, l), width: guint
) : void
  = "mac#atsctrb_gtk_container_set_border_width"
// end of [gtk_container_set_border_width]

(* ****** ****** *)

(* end of [gtkcontainer.sats] *)
