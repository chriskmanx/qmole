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
// Time: June, 2010
//
(* ****** ****** *)

//
// HX-2010-06-02: this is all deprecated!!!
//

(* ****** ****** *)

fun gtk_progress_get_value
  {c:cls | c <= GtkProgress}
  {l:agz} (
  progress: !gobjref (c, l)
) : gdouble = "mac#atsctrb_gtk_progress_get_value"
// end of [gtk_progress_get_value]

fun gtk_progress_set_value
  {c:cls | c <= GtkProgress}
  {l:agz} (
  progress: !gobjref (c, l), value: gdouble
) : void = "mac#atsctrb_gtk_progress_set_value"
// end of [gtk_progress_set_value]

(* ****** ****** *)

(* end of [gtkprogress.sats] *)
