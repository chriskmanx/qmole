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
// Starting time: January, 2010

(* ****** ****** *)

%{#
#include "contrib/SDL/CATS/SDL_ttf.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for static loading at run-time

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"

(* ****** ****** *)

//
// HX-2010-jan: is this type refcounted?
//
absviewtype TTF_Font_ref (l:addr) // TTF_Font* or null
viewtypedef TTF_Font_ref0 = [l:agez] TTF_Font_ref l
viewtypedef TTF_Font_ref1 = [l:addr | l <> null] TTF_Font_ref l

fun TTF_Font_ref_null
  ():<> TTF_Font_ref null = "atsctrb_SDL_ref_null"
fun TTF_Font_ref_free_null
  (sf: TTF_Font_ref null):<> void = "atsctrb_SDL_ref_free_null"
// overload ref_free_null with TTF_Font_ref_free_null

fun TTF_Font_ref_is_null
  {l:addr} (x: !TTF_Font_ref l):<> bool (l == null)
  = "atsctrb_SDL_ref_is_null"
overload ref_is_null with TTF_Font_ref_is_null

fun TTF_Font_ref_isnot_null
  {l:addr} (x: !TTF_Font_ref l):<> bool (l <> null)
  = "atsctrb_SDL_ref_isnot_null"
overload ref_isnot_null with TTF_Font_ref_isnot_null

(* ****** ****** *)

fun TTF_Init (): int (*err*) = "atsctrb_TTF_Init"

(* ****** ****** *)

(*
extern
DECLSPEC TTF_Font * SDLCALL TTF_OpenFont(const char *file, int ptsize);
extern
DECLSPEC TTF_Font * SDLCALL TTF_OpenFontIndex(const char *file, int ptsize, long index);
extern
DECLSPEC TTF_Font * SDLCALL TTF_OpenFontRW(SDL_RWops *src, int freesrc, int ptsize);
extern
DECLSPEC TTF_Font * SDLCALL TTF_OpenFontIndexRW(SDL_RWops *src, int freesrc, int ptsize, long index);
*)

fun TTF_OpenFont
  (filename: string, ptsize: int): TTF_Font_ref0 = "mac#atsctrb_TTF_OpenFont"
// end of [TTF_OpenFont]

(* ****** ****** *)

macdef TTF_STYLE_NORMAL = $extval (int, "TTF_STYLE_NORMAL")
macdef TTF_STYLE_BOLD = $extval (int, "TTF_STYLE_BOLD")
macdef TTF_STYLE_ITALIC = $extval (int, "TTF_STYLE_ITALIC")
macdef TTF_STYLE_UNDERLINE = $extval (int, "TTF_STYLE_UNDERLINE")

fun TTF_GetFontStyle
  {l:agz} (font: !TTF_Font_ref l): int(*style*) = "mac#atsctrb_TTF_GetFontStyle"
// end of [TTF_GetFontStyle]
  
fun TTF_SetFontStyle {l:agz}
  (font: !TTF_Font_ref l, style: int): void = "mac#atsctrb_TTF_SetFontStyle"
// end of [TTF_SetFontStyle]

fun TTF_FontHeight
  {l:agz} (font: !TTF_Font_ref l): int(*height*) = "mac#atsctrb_TTF_FontHeight"
// end of [TTF_FontHeight]

fun TTF_FontAscent
  {l:agz} (font: !TTF_Font_ref l): int(*ascent*) = "mac#atsctrb_TTF_FontAscent"
// end of [TTF_FontAscent]

fun TTF_FontDescent
  {l:agz} (font: !TTF_Font_ref l): int(*descent*) = "mac#atsctrb_TTF_FontDescent"
// end of [TTF_FontDescent]

fun TTF_FontLineSkip
  {l:agz} (font: !TTF_Font_ref l): int(*lineskip*) = "mac#atsctrb_TTF_FontLineSkip"
// end of [TTF_FontLineSkip]

fun TTF_FontFaces {l:agz}
  (font: !TTF_Font_ref l): lint(*number of faces*) = "mac#atsctrb_TTF_FontFaces"
// end of [TTF_FontFaces]

(* ****** ****** *)

fun TTF_FontFaceIsFixedWidth
  {l:agz} (font: !TTF_Font_ref l): int = "mac#atsctrb_TTF_FontFaceIsFixedWidth"

fun TTF_FontFaceFamilyName
  {l:agz} (font: !TTF_Font_ref l): string = "mac#atsctrb_TTF_FontFaceFamilyName"

fun TTF_FontFaceStyleName
  {l:agz} (font: !TTF_Font_ref l): string = "mac#atsctrb_FontFaceStyleName"

(* ****** ****** *)

fun TTF_SizeText {l:agz}
  (font: !TTF_Font_ref l, txt: string, w: &int? >> int, h: &int? >> int): int(*err*)
  = "mac#atsctrb_TTF_SizeText"

fun TTF_SizeUTF8 {l:agz}
  (font: !TTF_Font_ref l, txt: string, w: &int? >> int, h: &int? >> int): int(*err*)
  = "mac#atsctrb_TTF_SizeUTF8"

(*
fun TTF_SizeUTF16 {l:agz}
  (font: !TTF_Font_ref l, txt: wstring, w: &int? >> int, h: &int? >> int): int
  = "mac#atsctrb_TTF_SizeUTF16"
*)

(* ****** ****** *)

fun TTF_GlyphMetrics {l:agz} (
  font: !TTF_Font_ref l, ch: Uint16
, minx: &int? >> int, maxx: &int? >> int
, miny: &int? >> int, maxy: &int? >> int
, advance: &int? >> int
) : int (*err*)
  = "mac#atsctrb_TTF_GlyphMetrics"
// end of [TTF_GlyphMetrics]

(* ****** ****** *)

fun TTF_RenderText_Solid {l:agz}
  (font: !TTF_Font_ref l, txt: string, fg: SDL_Color): SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderText_Solid"

fun TTF_RenderUTF8_Solid {l:agz}
  (font: !TTF_Font_ref l, txt: string, fg: SDL_Color): SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderUTF8_Solid"

(*
fun TTF_RenderUTF16_Solid {l:agz}
  (font: !TTF_Font_ref l, txt: wstring, fg: SDL_Color): SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderUTF16_Solid"
*)

(* ****** ****** *)

fun TTF_RenderText_Shaded
  {l:agz} (
  font: !TTF_Font_ref l, txt: string, fg: SDL_Color, bg: SDL_Color
) : SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderText_Shaded"
// end of [TTF_RenderText_Shaded]

fun TTF_RenderUTF8_Shaded
  {l:agz} (
  font: !TTF_Font_ref l, txt: string, fg: SDL_Color, bg: SDL_Color
) : SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderUTF8_Shaded"
// end of [TTF_RenderUTF8_Shaded]

(*
fun TTF_RenderUTF16_Shaded
  {l:agz} (
  font: !TTF_Font_ref l, txt: wstring, fg: SDL_Color, bg: SDL_Color
) : SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderUTF16_Shaded"
*)

(* ****** ****** *)

fun TTF_RenderGlyph_Solid
  {l:agz} (
  font: !TTF_Font_ref l, ch: Uint16, fg: SDL_Color
) : SDL_Surface_ref0
  = "mac#atsctrb_TTF_RenderGlyph_Solid"
// end of [TTF_RenderGlyph_Solid]

(* ****** ****** *)

fun TTF_CloseFont {l:agz}
  (font: TTF_Font_ref l): void = "mac#atsctrb_TTF_CloseFont"
// end of [TTF_CloseFont]

(* ****** ****** *)

fun TTF_Quit (): void = "mac#atsctrb_TTF_Quit"
fun TTF_WasInit (): int = "mac#atsctrb_TTF_WasInit"

(* ****** ****** *)

(* end of [SDL_ttf.sats] *)
