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

symintr gtk_alignment_new

typedef
gtk_alignment_new_type (a:t@ype) = (
  a // xalign
, a // yalign
, a // xscale
, a // yscale
) -<fun1> GtkAlignment_ref1

fun gtk_alignment_new__type
  : gtk_alignment_new_type float = "mac#atsctrb_gtk_alignment_new"
overload gtk_alignment_new with gtk_alignment_new__type

fun gtk_alignment_new__gtype
  : gtk_alignment_new_type gfloat = "mac#atsctrb_gtk_alignment_new"
overload gtk_alignment_new with gtk_alignment_new__gtype

(* ****** ****** *)

symintr gtk_alignment_set

typedef
gtk_alignment_set_type (a:t@ype) = (
    a // xalign
  , a // yalign
  , a // xscale
  , a // yscale
  ) -<fun1> void

fun gtk_alignment_set__type
  : gtk_alignment_set_type float = "mac#atsctrb_gtk_alignment_set"
overload gtk_alignment_set with gtk_alignment_set__type

fun gtk_alignment_set__gtype
  : gtk_alignment_set_type gfloat = "mac#atsctrb_gtk_alignment_set"
overload gtk_alignment_set with gtk_alignment_set__gtype

(* ****** ****** *)

symintr gtk_alignment_get_padding

typedef
gtk_alignment_get_padding_type (a:t@ype) = (
    &a? >> a // xalign
  , &a? >> a // yalign
  , &a? >> a // xscale
  , &a? >> a // yscale
  ) -<fun1> void

fun gtk_alignment_get_padding__type
  : gtk_alignment_get_padding_type uint = "mac#atsctrb_gtk_alignment_get_padding"
overload gtk_alignment_get_padding with gtk_alignment_get_padding__type

fun gtk_alignment_get_padding__gtype
  : gtk_alignment_get_padding_type guint = "mac#atsctrb_gtk_alignment_get_padding"
overload gtk_alignment_get_padding with gtk_alignment_get_padding__gtype

(* ****** ****** *)

symintr gtk_alignment_set_padding

typedef
gtk_alignment_set_padding_type (a:t@ype) = (
  &a // xalign
, &a // yalign
, &a // xscale
, &a // yscale
) -<fun1> void // end of [typedef]

fun gtk_alignment_set_padding__type
  : gtk_alignment_set_padding_type uint = "mac#atsctrb_gtk_alignment_set_padding"
overload gtk_alignment_set_padding with gtk_alignment_set_padding__type

fun gtk_alignment_set_padding__gtype
  : gtk_alignment_set_padding_type guint = "mac#atsctrb_gtk_alignment_set_padding"
overload gtk_alignment_set_padding with gtk_alignment_set_padding__gtype

(* ****** ****** *)

(* end of [gtkalignment.sats] *)
