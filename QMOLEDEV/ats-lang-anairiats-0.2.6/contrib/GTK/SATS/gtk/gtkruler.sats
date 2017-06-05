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

fun gtk_ruler_set_metric
  {c:cls | c <= GtkRuler}
  {l:agz} (
  ruler: !gobjref (c, l), metric: GtkMetricType
) : void
  = "mac#atsctrb_gtk_ruler_set_metric"
// end of [gtk_ruler_set_metric]

(* ****** ****** *)

typedef
gtk_ruler_set_range_type (a:t@ype) =
  {c:cls | c <= GtkRuler} {l:agz} (
  !gobjref (c, l)
, a // lower
, a // upper
, a // position
, a // max_size
) -<fun1> void // end of [gtk_ruler_set_range_type]

symintr gtk_ruler_set_range  

fun gtk_ruler_set_range__type
  : gtk_ruler_set_range_type (double)
  = "mac#atsctrb_gtk_ruler_set_range"
overload gtk_ruler_set_range with gtk_ruler_set_range__type 

fun gtk_ruler_set_range__gtype
  : gtk_ruler_set_range_type (gdouble)
  = "mac#atsctrb_gtk_ruler_set_range"
overload gtk_ruler_set_range with gtk_ruler_set_range__gtype 

(* ****** ****** *)

(* end of [gtkruler.sats] *)
