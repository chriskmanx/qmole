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

fun gtk_menu_shell_append
  {c1,c2:cls | c1 <= GtkMenuShell; c2 <= GtkWidget}
  {l1,l2:agz} (
  container: !gobjref (c1, l1), widget: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_menu_shell_append"
// end of [gtk_menu_shell_append]

fun gtk_menu_shell_prepend
  {c1,c2:cls | c1 <= GtkMenuShell; c2 <= GtkWidget}
  {l1,l2:agz} (
  container: !gobjref (c1, l1), widget: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_menu_shell_prepend"
// end of [gtk_menu_shell_prepend]

(* ****** ****** *)

fun gtk_menu_shell_select_item
  {c1,c2:cls | c1 <= GtkMenuShell; c2 <= GtkWidget}
  {l1,l2:agz} (
  container: !gobjref (c1, l1), widget: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_menu_shell_select_item"
// end of [gtk_menu_shell_select_item]

fun gtk_menu_shell_select_first
  {c:cls | c <= GtkMenuShell} {l:agz}
  (container: !gobjref (c, l), search_sensitive: gboolean): void
  = "mac#atsctrb_gtk_menu_shell_select_first"
// end of [gtk_menu_shell_select_first]

(* ****** ****** *)

fun gtk_menu_shell_deselect
  {c:cls | c <= GtkMenuShell}
  {l:agz} (
  container: !gobjref (c, l)
) : void
  = "mac#atsctrb_gtk_menu_shell_deselect"
// end of [gtk_menu_shell_deselect]

(* ****** ****** *)

(* end of [gtkmenushell.sats] *)
