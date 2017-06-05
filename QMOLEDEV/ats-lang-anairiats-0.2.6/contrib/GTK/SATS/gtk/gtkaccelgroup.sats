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

abst@ype GtkAccelFlags = $extype"GtkAccelFlags"
macdef GTK_ACCEL_LOCKED = $extval (GtkAccelFlags, "GTK_ACCEL_LOCKED")
macdef GTK_ACCEL_MASK = $extval (GtkAccelFlags, "GTK_ACCEL_MASK")
macdef GTK_ACCEL_VISIBLE = $extval (GtkAccelFlags, "GTK_ACCEL_VISIBLE")

fun lor_GtkAccelFlags_GtkAccelFlags (
  x1: GtkAccelFlags, x2: GtkAccelFlags
) :<> GtkAccelFlags
  = "atsctrb_lor_GtkAccelFlags_GtkAccelFlags"
overload lor with lor_GtkAccelFlags_GtkAccelFlags

(* ****** ****** *)

fun gtk_accel_group_new
  (): GtkAccelGroup_ref1 = "mac#atsctrb_gtk_accel_group_new"
// end of [gtk_accel_group_new]

(* ****** ****** *)

(* end of [gtkaccelgroup.sats] *)
