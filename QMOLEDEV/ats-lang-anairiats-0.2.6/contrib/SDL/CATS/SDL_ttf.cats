/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
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
*/

/* ****** ****** */

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010

/* ****** ****** */

#ifndef ATSCTRB_SDL_TTF_CATS
#define ATSCTRB_SDL_TTF_CATS

/* ****** ****** */

#include "SDL/SDL.h"
#include "SDL/SDL_ttf.h"

/* ****** ****** */

#define atsctrb_TTF_Init TTF_Init 

/* ****** ****** */

#define atsctrb_TTF_OpenFont TTF_OpenFont

/* ****** ****** */

#define atsctrb_TTF_GetFontStyle TTF_GetFontStyle
#define atsctrb_TTF_SetFontStyle TTF_SetFontStyle

#define atsctrb_TTF_GetFontHeight TTF_GetFontHeight

#define atsctrb_TTF_FontAscent TTF_FontAscent
#define atsctrb_TTF_FontDescent TTF_FontDescent
#define atsctrb_TTF_FontLineSkip TTF_FontLineSkip
#define atsctrb_TTF_FontFaces TTF_FontFaces

/* ****** ****** */

#define atsctrb_TTF_FontFaceIsFixedWidth TTF_FontFaceIsFixedWidth
#define atsctrb_TTF_FontFaceFamilyName
#define atsctrb_FontFaceStyleName

/* ****** ****** */

#define atsctrb_TTF_SizeText TTF_SizeText
#define atsctrb_TTF_SizeUTF8 TTF_SizeUTF8

/* ****** ****** */

#define atsctrb_TTF_GlyphMetrics TTF_GlyphMetrics

/* ****** ****** */

#define atsctrb_TTF_RenderText_Solid TTF_RenderText_Solid
#define atsctrb_TTF_RenderUTF8_Solid TTF_RenderUTF8_Solid

#define atsctrb_TTF_RenderText_Shaded TTF_RenderText_Shaded
#define atsctrb_TTF_RenderUTF8_Shaded TTF_RenderUTF8_Shaded

#define atsctrb_TTF_RenderGlyph_Solid TTF_RenderGlyph_Solid

/* ****** ****** */

#define atsctrb_TTF_CloseFont TTF_CloseFont

/* ****** ****** */

#define atsctrb_TTF_Quit TTF_Quit 
#define atsctrb_TTF_WasInit TTF_WasInit

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_TTF_CATS]

/* end of [SDL_ttf.cats] */
