(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: May, 2010
//
(* ****** ****** *)

typedef PangoColor =
  $extype_struct "PangoColor" of {
  red= guint16
, green= guint16
, blue= guint16
} // end of [PangoColor]

(* ****** ****** *)

fun pango_color_copy (
  src: &PangoColor
) : [l:addr] (PangoColor @ l | ptr l) = "mac#atsctrb_pango_color_copy"
// end of [pango_color_copy]

fun pango_color_free {l:addr} (
  pf1: PangoFree_v l, pf2: PangoColor @ l | p: ptr l
) : void = "mac#atsctrb_pango_color_free"
// end of [pango_color_free]

fun pango_color_parse {l:agz} (
  clr: &PangoColor? >> opt (PangoColor, b), spec: !gstring l
) : #[b: bool] gboolean b = "mac#atsctrb_pango_color_parse"
// end of [pango_color_parse]

(* ****** ****** *)

abst@ype PangoAttrType = $extype"PangoAttrType"
macdef PANGO_ATTR_INVALID = $extval (PangoAttrType, "PANGO_ATTR_INVALID")
macdef PANGO_ATTR_LANGUAGE = $extval (PangoAttrType, "PANGO_ATTR_LANGUAGE")
macdef PANGO_ATTR_FAMILY = $extval (PangoAttrType, "PANGO_ATTR_FAMILY")
macdef PANGO_ATTR_STYLE = $extval (PangoAttrType, "PANGO_ATTR_STYLE")
macdef PANGO_ATTR_WEIGHT = $extval (PangoAttrType, "PANGO_ATTR_WEIGHT")
macdef PANGO_ATTR_VARIANT = $extval (PangoAttrType, "PANGO_ATTR_VARIANT")
macdef PANGO_ATTR_STRETCH = $extval (PangoAttrType, "PANGO_ATTR_STRETCH")
macdef PANGO_ATTR_SIZE = $extval (PangoAttrType, "PANGO_ATTR_SIZE")
macdef PANGO_ATTR_FONT_DESC = $extval (PangoAttrType, "PANGO_ATTR_FONT_DESC")
macdef PANGO_ATTR_FOREGROUND = $extval (PangoAttrType, "PANGO_ATTR_FOREGROUND")
macdef PANGO_ATTR_BACKGROUND = $extval (PangoAttrType, "PANGO_ATTR_BACKGROUND")
macdef PANGO_ATTR_UNDERLINE = $extval (PangoAttrType, "PANGO_ATTR_UNDERLINE")
macdef PANGO_ATTR_STRIKETHROUGH = $extval (PangoAttrType, "PANGO_ATTR_STRIKETHROUGH")
macdef PANGO_ATTR_RISE = $extval (PangoAttrType, "PANGO_ATTR_RISE")
macdef PANGO_ATTR_SHAPE = $extval (PangoAttrType, "PANGO_ATTR_SHAPE")
macdef PANGO_ATTR_SCALE = $extval (PangoAttrType, "PANGO_ATTR_SCALE")
macdef PANGO_ATTR_FALLBACK = $extval (PangoAttrType, "PANGO_ATTR_FALLBACK")
macdef PANGO_ATTR_LETTER_SPACING = $extval (PangoAttrType, "PANGO_ATTR_LETTER_SPACING")
macdef PANGO_ATTR_UNDERLINE_COLOR = $extval (PangoAttrType, "PANGO_ATTR_UNDERLINE_COLOR")
macdef PANGO_ATTR_STRIKETHROUGH_COLOR = $extval (PangoAttrType, "PANGO_ATTR_STRIKETHROUGH_COLOR")
macdef PANGO_ATTR_ABSOLUTE_SIZE = $extval (PangoAttrType, "PANGO_ATTR_ABSOLUTE_SIZE")

(* ****** ****** *)

abst@ype PangoUnderline = $extype"PangoUnderline"
macdef PANGO_UNDERLINE_NONE = $extval (PangoUnderline, "PANGO_UNDERLINE_NONE")
macdef PANGO_UNDERLINE_SINGLE = $extval (PangoUnderline, "PANGO_UNDERLINE_SINGLE")
macdef PANGO_UNDERLINE_DOUBLE = $extval (PangoUnderline, "PANGO_UNDERLINE_DOUBLE")
macdef PANGO_UNDERLINE_LOW = $extval (PangoUnderline, "PANGO_UNDERLINE_LOW")
macdef PANGO_UNDERLINE_ERROR = $extval (PangoUnderline, "PANGO_UNDERLINE_ERROR")

(* ****** ****** *)

typedef PangoAttribute =
  $extype_struct "PangoAttribute" of {
  start_index= guint, end_index= guint
, _rest = undefined_t // this field cannot be accessed
} // end of [PangoAttribute]

absviewtype PangoAttribute_ptr (l:addr) // HX: _not_ refcounted
viewtypedef PangoAttribute_ptr1 = [l:addr | l > null] PangoAttribute_ptr l

(* ****** ****** *)

fun pango_attribute_copy
  {l:agz} (
  attr: !PangoAttribute_ptr l
) : [l_new:addr] PangoAttribute_ptr l_new
  = "mac#atsctrb_pango_attribute_copy"
// end of [pango_attribute_copy]

fun pango_attribute_destroy
  {l:agz} (
  attr: PangoAttribute_ptr l
) : void = "mac#atsctrb_pango_attribute_destroy"
// end of [pango_attribute_destroy]

fun pango_attribute_equal
  {l1,l2:agz} (
  attr1: PangoAttribute_ptr l1, attr2: PangoAttribute_ptr l2
) : gboolean
  = "mac#atsctrb_pango_attribute_equal"
// end of [pango_attribute_equal]

(* ****** ****** *)

fun pango_attr_foreground_new
  (r: guint16, g: guint16, b: guint16): PangoAttribute_ptr1
  = "mac#atsctrb_pango_attr_foreground_new"
// end of [pango_attr_foreground_new]

fun pango_attr_background_new
  (r: guint16, g: guint16, b: guint16): PangoAttribute_ptr1
  = "mac#atsctrb_pango_attr_background_new"
// end of [pango_attr_background_new]

(* ****** ****** *)

absviewtype PangoAttrList_ref (l:addr)
viewtypedef PangoAttrList_ref1 = [l:addr | l > null] PangoAttrList_ref (l)

(* ****** ****** *)

fun pango_attr_list_new
  (): PangoAttrList_ref1
  = "mac#atsctrb_pango_attr_list_new"
fun pango_attr_list_ref {l:agz}
  (alist: !PangoAttrList_ref l): PangoAttrList_ref l
  = "mac#atsctrb_pango_attr_list_ref"
fun pango_attr_list_unref
  {l:agz} (alist: PangoAttrList_ref l): void
  = "mac#atsctrb_pango_attr_list_unref"
fun pango_attr_list_copy
  {l:agz} (alist: !PangoAttrList_ref l): PangoAttrList_ref1
  = "mac#atsctrb_pango_attr_list_copy"

// HX: [attr] is owned by [alist] after the call
fun pango_attr_list_insert
  {l1,l2:agz} (
  alist: !PangoAttrList_ref l1, attr: PangoAttribute_ptr l2
) : void
  = "mac#atsctrb_pango_attr_list_insert"
// end of [pango_attr_list_insert]

// HX: [attr] is owned by [alist] after the call
fun pango_attr_list_insert_before {l1,l2:agz}
  (alist: !PangoAttrList_ref l1, attr: PangoAttribute_ptr l2): void
  = "mac#atsctrb_pango_attr_list_insert_before"
// end of [pango_attr_list_insert_before]

// HX: [attr] is owned by [alist] after the call
fun pango_attr_list_change
  {l1,l2:agz} (
  alist: !PangoAttrList_ref l1, attr: PangoAttribute_ptr l2
) : void
  = "mac#atsctrb_pango_attr_list_change"
// end of [pango_attr_list_change]

(* ****** ****** *)

(* end of [pango-attributes.sats] *)
