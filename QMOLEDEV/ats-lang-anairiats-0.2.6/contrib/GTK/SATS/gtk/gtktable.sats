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

fun gtk_table_new (
  nrow: guint
, ncol: guint
, homo: gboolean
) : GtkTable_ref1
  = "mac#atsctrb_gtk_table_new"
// end of [gtk_table_new]

(* ****** ****** *)

fun lor_GtkAttachOptions_GtkAttachOptions
  (x1: GtkAttachOptions, x2: GtkAttachOptions):<> GtkAttachOptions
  = "atsctrb_lor_GtkAttachOptions_GtkAttachOptions"
overload lor with lor_GtkAttachOptions_GtkAttachOptions

(* ****** ****** *)

symintr gtk_table_attach

typedef
gtk_table_attach_type (a:t@ype) =
  {c1,c2:cls | c1 <= GtkTable; c2 <= GtkWidget}
  {l1,l2:agz} (
  !gobjref (c1, l1) // table
, !gobjref (c2, l2) // widget
, a // left
, a // right
, a // top
, a // bot
, GtkAttachOptions // xopt
, GtkAttachOptions // yopt
, a // xpadding
, a // ypadding
) -<fun1> void // end of [gtk_table_attach_type]

fun gtk_table_attach__type
  : gtk_table_attach_type (uint)
  = "mac#atsctrb_gtk_table_attach"
overload gtk_table_attach with gtk_table_attach__type

fun gtk_table_attach__gtype
  : gtk_table_attach_type (guint)
  = "mac#atsctrb_gtk_table_attach"
overload gtk_table_attach with gtk_table_attach__gtype

(* ****** ****** *)

symintr gtk_table_attach_defaults

typedef
gtk_table_attach_defaults_type (a:t@ype) =
  {c1,c2:cls | c1 <= GtkTable; c2 <= GtkWidget}
  {l1,l2:agz} (
  !gobjref (c1, l1) // table
, !gobjref (c2, l2) // widget
, a // left
, a // right
, a // top
, a // bot
) -<fun1> void // end of [gtk_table_attach_defaults_type]

fun gtk_table_attach_defaults__type
  : gtk_table_attach_defaults_type (uint)
  = "mac#atsctrb_gtk_table_attach_defaults"
overload gtk_table_attach_defaults with gtk_table_attach_defaults__type

fun gtk_table_attach_defaults__gtype
  : gtk_table_attach_defaults_type (guint)
  = "mac#atsctrb_gtk_table_attach_defaults"
overload gtk_table_attach_defaults with gtk_table_attach_defaults__gtype

(* ****** ****** *)

fun gtk_table_resize
  {c:cls | c <= GtkTable}
  {l:addr} (
  table: !gobjref (c, l), nrow: guint, ncol: guint
) : void
  = "mac#atsctrb_gtk_table_resize"
// end of [gtk_table_resize]

(* ****** ****** *)

fun gtk_table_set_row_spacing
  {c:cls | c <= GtkTable} {l:addr}
  (table: !gobjref (c, l), row: guint, spacing: guint): void
  = "mac#atsctrb_gtk_table_set_row_spacing"
// end of [gtk_table_set_row_spacing]

fun gtk_table_set_col_spacing
  {c:cls | c <= GtkTable} {l:addr}
  (table: !gobjref (c, l), col: guint, spacing: guint): void
  = "mac#atsctrb_gtk_table_set_col_spacing"
// end of [gtk_table_set_col_spacing]

(* ****** ****** *)

fun gtk_table_set_row_spacings
  {c:cls | c <= GtkTable}
  {l:addr} (
  table: !gobjref (c, l), spacing: guint
) : void
  = "mac#atsctrb_gtk_table_set_row_spacings"
// end of [gtk_table_set_row_spacings]

fun gtk_table_set_col_spacings
  {c:cls | c <= GtkTable}
  {l:addr} (
  table: !gobjref (c, l), spacing: guint
) : void
  = "mac#atsctrb_gtk_table_set_col_spacings"
// end of [gtk_table_set_col_spacings]

(* ****** ****** *)

(* end of [gtktable.sats] *)
