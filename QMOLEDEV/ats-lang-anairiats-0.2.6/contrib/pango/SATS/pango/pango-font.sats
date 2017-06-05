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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: May, 2010

(* ****** ****** *)

//
// API for pango in ATS
//

(* ****** ****** *)

abst@ype PangoStyle = $extype"PangoStyle"
macdef PANGO_STYLE_NORMAL =
  $extval (PangoStyle, "PANGO_STYLE_NORMAL")
macdef PANGO_STYLE_OBLIQUE =
  $extval (PangoStyle, "PANGO_STYLE_OBLIQUE")
macdef PANGO_STYLE_ITALIC =
  $extval (PangoStyle, "PANGO_STYLE_ITALIC")

(* ****** ****** *)

abst@ype PangoVariant = $extype"PangoVariant"
macdef PANGO_VARIANT_NORMAL =
  $extval (PangoVariant, "PANGO_VARIANT_NORMAL")
macdef PANGO_VARIANT_SMALL_CAPS =
  $extval (PangoVariant, "PANGO_VARIANT_SMALL_CAPS")

(* ****** ****** *)

abst@ype PangoWeight = $extype"PangoWeight"
macdef PANGO_WEIGHT_ULTRALIGHT =
  $extval (PangoWeight, "PANGO_WEIGHT_ULTRALIGHT")
macdef PANGO_WEIGHT_LIGHT =
  $extval (PangoWeight, "PANGO_WEIGHT_LIGHT")
macdef PANGO_WEIGHT_NORMAL =
  $extval (PangoWeight, "PANGO_WEIGHT_NORMAL")
macdef PANGO_WEIGHT_SEMIBOLD =
  $extval (PangoWeight, "PANGO_WEIGHT_SEMIBOLD")
macdef PANGO_WEIGHT_BOLD =
  $extval (PangoWeight, "PANGO_WEIGHT_BOLD")
macdef PANGO_WEIGHT_ULTRABOLD =
  $extval (PangoWeight, "PANGO_WEIGHT_ULTRABOLD")
macdef PANGO_WEIGHT_HEAVY =
  $extval (PangoWeight, "PANGO_WEIGHT_HEAVY")

(* ****** ****** *)

abst@ype PangoStretch = $extype"PangoStretch"
macdef PANGO_STRETCH_ULTRA_CONDENSED =
  $extval (PangoStretch, "PANGO_STRETCH_ULTRA_CONDENSED")
macdef PANGO_STRETCH_EXTRA_CONDENSED =
  $extval (PangoStretch, "PANGO_STRETCH_EXTRA_CONDENSED")
macdef PANGO_STRETCH_CONDENSED =
  $extval (PangoStretch, "PANGO_STRETCH_CONDENSED")
macdef PANGO_STRETCH_SEMI_CONDENSED =
  $extval (PangoStretch, "PANGO_STRETCH_SEMI_CONDENSED")
macdef PANGO_STRETCH_NORMAL =
  $extval (PangoStretch, "PANGO_STRETCH_NORMAL")
macdef PANGO_STRETCH_SEMI_EXPANDED =
  $extval (PangoStretch, "PANGO_STRETCH_SEMI_EXPANDED")
macdef PANGO_STRETCH_EXPANDED =
  $extval (PangoStretch, "PANGO_STRETCH_EXPANDED")
macdef PANGO_STRETCH_EXTRA_EXPANDED =
  $extval (PangoStretch, "PANGO_STRETCH_EXTRA_EXPANDED")
macdef PANGO_STRETCH_ULTRA_EXPANDED =
  $extval (PangoStretch, "PANGO_STRETCH_ULTRA_EXPANDED")

(* ****** ****** *)

abst@ype PangoFontMask = $extype"PangoFontMask"
macdef PANGO_FONT_MASK_FAMILY =
  $extval (PangoFontMask, "PANGO_FONT_MASK_FAMILY")
macdef PANGO_FONT_MASK_STYLE =
  $extval (PangoFontMask, "PANGO_FONT_MASK_STYLE")
macdef PANGO_FONT_MASK_VARIANT =
  $extval (PangoFontMask, "PANGO_FONT_MASK_VARIANT")
macdef PANGO_FONT_MASK_WEIGHT =
  $extval (PangoFontMask, "PANGO_FONT_MASK_WEIGHT")
macdef PANGO_FONT_MASK_STRETCH =
  $extval (PangoFontMask, "PANGO_FONT_MASK_STRETCH")
macdef PANGO_FONT_MASK_SIZE =
  $extval (PangoFontMask, "PANGO_FONT_MASK_SIZE")

(* ****** ****** *)

macdef PANGO_SCALE_XX_SMALL = $extval (double, "PANGO_SCALE_XX_SMALL")
macdef PANGO_SCALE_X_SMALL = $extval (double, "PANGO_SCALE_X_SMALL")
macdef PANGO_SCALE_SMALL = $extval (double, "PANGO_SCALE_SMALL")
macdef PANGO_SCALE_MEDIUM = $extval (double, "PANGO_SCALE_MEDIUM")
macdef PANGO_SCALE_LARGE = $extval (double, "PANGO_SCALE_LARGE")
macdef PANGO_SCALE_X_LARGE = $extval (double, "PANGO_SCALE_X_LARGE")
macdef PANGO_SCALE_XX_LARGE = $extval (double, "PANGO_SCALE_XX_LARGE")

(* ****** ****** *)

absviewtype
PangoFontDescription_ptr (l:addr)
viewtypedef
PangoFontDescription_ptr0 = [l:addr] PangoFontDescription_ptr l
viewtypedef
PangoFontDescription_ptr1 = [l:addr | l > null] PangoFontDescription_ptr l

castfn ptr_of_PangoFontDescription_ptr
  {l:addr} (x: !PangoFontDescription_ptr l): ptr l
overload ptr_of with ptr_of_PangoFontDescription_ptr

(* ****** ****** *)

fun pango_font_description_new
  (): PangoFontDescription_ptr1 = "mac#atsctrb_pango_font_description_new"
// end of [pango_font_description_new]

fun pango_font_description_copy
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : PangoFontDescription_ptr l
  = "mac#atsctrb_pango_font_description_copy"
// end of [pango_font_description_copy]

fun pango_font_description_copy_static // shallow copy
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : PangoFontDescription_ptr l
  = "mac#atsctrb_pango_font_description_copy_static"
// end of [pango_font_description_copy]

fun pango_font_description_hash
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : guint
  = "mac#atsctrb_pango_font_description_hash"
// end of [pango_font_description_hash]

fun pango_font_description_equal
  {l1,l2:agz} (
  fd1: !PangoFontDescription_ptr l1, fd2: !PangoFontDescription_ptr l2
) : gboolean
  = "mac#atsctrb_pango_font_description_hash"
// end of [pango_font_description_hash]

fun pango_font_description_free
  {l:agz} (fd: PangoFontDescription_ptr l): void
  = "mac#atsctrb_pango_font_description_free"
// end of [pango_font_description_free]

(* ****** ****** *)

fun pango_font_description_get_family
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : [l1:addr] (
  minus (PangoFontDescription_ptr l, gstring l1)
| gstring l1
) = "mac#atsctrb_pango_font_description_get_family"

fun pango_font_description_set_family
  {l1,l2:agz} (
  fd: !PangoFontDescription_ptr l1, family: !gstring l2
) : void
  = "mac#atsctrb_pango_font_description_set_family"

fun pango_font_description_get_stretch
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : PangoStretch
  = "mac#atsctrb_pango_font_description_get_stretch"

fun pango_font_description_set_stretch
  {l:agz} (
  fd: !PangoFontDescription_ptr l, stretch: PangoStretch
) : void
  = "mac#atsctrb_pango_font_description_set_stretch"

fun pango_font_description_get_style
  {l:agz} (fd: !PangoFontDescription_ptr l): PangoStyle
  = "mac#atsctrb_pango_font_description_get_style"

fun pango_font_description_set_style
  {l:agz} (fd: !PangoFontDescription_ptr l, style: PangoStyle): void
  = "mac#atsctrb_pango_font_description_set_style"

fun pango_font_description_get_variant
  {l:agz} (fd: !PangoFontDescription_ptr l): PangoVariant
  = "mac#atsctrb_pango_font_description_get_variant"

fun pango_font_description_set_variant
  {l:agz} (
  fd: !PangoFontDescription_ptr l, variant: PangoVariant
) : void
  = "mac#atsctrb_pango_font_description_set_variant"

fun pango_font_description_get_weight
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : PangoWeight
  = "mac#atsctrb_pango_font_description_get_weight"

fun pango_font_description_set_weight
  {l:agz} (
  fd: !PangoFontDescription_ptr l, weight: PangoWeight
) : void
  = "mac#atsctrb_pango_font_description_set_weight"

(* ****** ****** *)

fun pango_font_description_get_size
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : gint
  = "mac#atsctrb_pango_font_description_get_size"

fun pango_font_description_set_size
  {l:agz} (
  fd: !PangoFontDescription_ptr l, size: gint
) : void
  = "mac#atsctrb_pango_font_description_set_size"

fun pango_font_description_set_absolute_size
  {l:agz} (
  fd: !PangoFontDescription_ptr l, size: gdouble
) : void
  = "mac#atsctrb_pango_font_description_set_absolute_size"
  
fun pango_font_description_get_size_is_absolute
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : gboolean
  = "mac#atsctrb_pango_font_description_get_size_is_absolute"

(* ****** ****** *)

(*

fun pango_font_description_get_gravity
  {l:agz} (fd: !PangoFontDescription_ptr l): PangoGravity
  = "mac#atsctrb_pango_font_description_get_gravity"

fun pango_font_description_set_gravity
  {l:agz} (fd: !PangoFontDescription_ptr l, gravity: PangoGravity): void
  = "mac#atsctrb_pango_font_description_set_gravity"

*)

(* ****** ****** *)

fun pango_font_description_get_set_fields
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : PangoFontMask
  = "mac#atsctrb_pango_font_description_get_set_fields"

fun pango_font_description_unset_fields
  {l:agz} (
  fd: !PangoFontDescription_ptr l, mask: PangoFontMask
) : void
  = "mac#atsctrb_pango_font_description_unset_fields"

(* ****** ****** *)

fun pango_font_description_from_string
  {l:agz} (
  name: !gstring l
) : PangoFontDescription_ptr1
  = "mac#atsctrb_pango_font_description_from_string"
// end of [pango_font_description_from_string]

fun pango_font_description_to_string
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : gstring1
  = "mac#atsctrb_pango_font_description_to_string"
// end of [pango_font_description_to_string]

fun pango_font_description_to_filename
  {l:agz} (
  fd: !PangoFontDescription_ptr l
) : gstring1
  = "mac#atsctrb_pango_font_description_to_filename"
// end of [pango_font_description_to_filename]

(* ****** ****** *)

(* end of [pango-font.sats] *)
