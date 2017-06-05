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

fun gtk_misc_set_alignment
  {c:cls | c <= GtkMisc}
  {l:agz} (
  misc: !gobjref (c, l)
, xalign: gfloat, yalign: gfloat
) : void
  = "mac#atsctrb_gtk_misc_set_alignment"
// end of [gtk_misc_set_alignment]

fun gtk_misc_get_alignment
  {c:cls | c <= GtkMisc}
  {l:agz} (
  misc: !gobjref (c, l)
, xalign: &gfloat? >> gfloat, yalign: &gfloat? >> gfloat
) : void
  = "mac#atsctrb_gtk_misc_get_alignment"
// end of [gtk_misc_get_alignment]

(* ****** ****** *)

(* end of [gtkmisc.sats] *)
