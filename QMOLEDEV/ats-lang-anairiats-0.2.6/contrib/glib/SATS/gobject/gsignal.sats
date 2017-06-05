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

abstype gsignal // gchar*
castfn gsignal (x: string):<> gsignal

macdef GSIGNAL_ACTIVATE = $extval (gsignal, "\"activate\"")
macdef GSIGNAL_CLICKED = $extval (gsignal, "\"clicked\"")
macdef GSIGNAL_DESTROY = $extval (gsignal, "\"destroy\"")
macdef GSIGNAL_EVENT = $extval (gsignal, "\"event\"")
macdef GSIGNAL_DELETE_EVENT = $extval (gsignal, "\"delete_event\"")

(* ****** ****** *)

fun g_signal_connect
  {c:cls | c <= GObject} {l:agz} (
  x: !gobjref (c, l)
, sig: gsignal
, handler: GCallback
, data: gpointer
) : guint
  = "mac#atsctrb_g_signal_connect"
// end of [g_signal_connect]

(* ****** ****** *)

fun g_signal_connect_after
  {c:cls | c <= GObject} {l:agz} (
  x: !gobjref (c, l)
, sig: gsignal
, handler: GCallback
, data: gpointer
) : guint
  = "mac#atsctrb_g_signal_connect_after"
// end of [g_signal_connect_after]

(* ****** ****** *)

fun g_signal_connect_swapped
  {c1,c2:cls | c1 <= GObject; c2 <= GObject}
  {l1,l2:agz} (
  x: !gobjref (c1, l1)
, sig: gsignal
, handler: GCallback
, data: !gobjref (c2, l2)
) : guint
  = "mac#atsctrb_g_signal_connect_swapped"
// end of [g_signal_connect_swapped]

(* ****** ****** *)

fun g_signal_emit_by_name
  {c:cls | c <= GObject}
  {l:agz} (
  x: !gobjref (c, l), sig: gsignal
) : void
  = "mac#atsctrb_g_signal_emit_by_name"
// end of [g_signal_emit_by_name]

(* ****** ****** *)

fun g_signal_stop_emission_by_name
  {c:cls | c <= GObject}
  {l:agz} (
  x: !gobjref (c, l), sig: gsignal
) : void
  = "mac#atsctrb_g_signal_stop_emission_by_name"
// end of [g_signal_stop_emission_by_name]

(* ****** ****** *)

(* end of [gsignal.sats] *)
