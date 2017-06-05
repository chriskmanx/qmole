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
// Time: May, 2010
//
(* ****** ****** *)

fun gtk_clipboard_get (selection: GdkAtom)
  : [l:agz] (GtkClipBoard_ref l -<lin,prf> void | GtkClipBoard_ref l)
  = "mac#atsctrb_gtk_clipboard_get"
// end of [gtk_clipboard_get]

fun gtk_clipboard_wait_is_text_available
  {c:cls | c <= GtkClipBoard} {l:agz} (cb: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_clipboard_wait_is_text_available"
// end of [gtk_clipboard_wait_is_text_available]

(* ****** ****** *)

(* end of [gtkclipboard.sats] *)
