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

#ifndef ATSCTRB_SDL_VIDEO_CATS
#define ATSCTRB_SDL_VIDEO_CATS

/* ****** ****** */

static inline
ats_void_type
atsctrb_SDL_Rect_init (
  ats_ref_type rect
, Sint16 x, Sint16 y, Uint16 w, Uint16 h
) {
  ((SDL_Rect*)rect)->x = x ;
  ((SDL_Rect*)rect)->y = y ;
  ((SDL_Rect*)rect)->w = w ;
  ((SDL_Rect*)rect)->h = h ;
  return ;
} // end of [atsctrb_SDL_Rect_init]

/* ****** ****** */

static inline
ats_void_type
atsctrb_SDL_Color_init (
  ats_ref_type color, Uint8 r, Uint8 g, Uint8 b
) {
  ((SDL_Color*)color)->r = r ;
  ((SDL_Color*)color)->g = g ;
  ((SDL_Color*)color)->b = b ;
  return ;
} // end of [atsctrb_SDL_Color_init]

/* ****** ****** */

#define atsctrb_SDL_Surface_flags(sf) (((SDL_Surface*)(sf))->flags)
#define atsctrb_SDL_Surface_format(sf) (((SDL_Surface*)(sf))->format)
#define atsctrb_SDL_Surface_w(sf) (((SDL_Surface*)(sf))->w)
#define atsctrb_SDL_Surface_h(sf) (((SDL_Surface*)(sf))->h)
#define atsctrb_SDL_Surface_pitch(sf) (((SDL_Surface*)(sf))->pitch)
#define atsctrb_SDL_Surface_clip_rect(sf) (((SDL_Surface*)(sf))->clip_rect)
#define atsctrb_SDL_Surface_refcount(sf) (((SDL_Surface*)(sf))->refcount)

/* ****** ****** */

#define atsctrb_SDL_SetVideoMode SDL_SetVideoMode

static inline
ats_ref_type
atsctrb_SDL_ResetVideoMode (
  ats_ref_type screen // not actually used
, ats_int_type width
, ats_int_type height
, ats_int_type bpp
, Uint32 flags
) {
  return SDL_SetVideoMode (width, height, bpp, flags) ;
} // end of [atsctrb_SDL_ResetVideoMode]

/* ****** ****** */

#define atsctrb_SDL_UpdateRect SDL_UpdateRect
#define atsctrb_SDL_UpdateRects SDL_UpdateRects
#define atsctrb_SDL_Flip SDL_Flip

/* ****** ****** */

#define atsctrb_SDL_MapRGB SDL_MapRGB
#define atsctrb_SDL_MapRGBA SDL_MapRGBA

/* ****** ****** */

#define atsctrb_SDL_CreateRGBSurface SDL_CreateRGBSurface
#define atsctrb_SDL_CreateRGBSurfaceFrom SDL_CreateRGBSurfaceFrom
#define atsctrb_SDL_FreeSurface SDL_FreeSurface

/* ****** ****** */

#define atsctrb_SDL_LoadBMP SDL_LoadBMP

/* ****** ****** */

#define atsctrb_SDL_SetColorKey SDL_SetColorKey
#define atsctrb_SDL_SetAlpha SDL_SetAlpha

/* ****** ****** */

#define atsctrb_SDL_GetClipRect SDL_GetClipRect
#define atsctrb_SDL_SetClipRect SDL_SetClipRect

/* ****** ****** */

#define atsctrb_SDL_UpperBlit SDL_UpperBlit

/* ****** ****** */

#define atsctrb_SDL_FillRect SDL_FillRect

/* ****** ****** */

#define atsctrb_SDL_DisplayFormat SDL_DisplayFormat
#define atsctrb_SDL_DisplayFormatAlpha SDL_DisplayFormatAlpha

/* ****** ****** */

#define atsctrb_SDL_WM_GetCaption SDL_WM_GetCaption
#define atsctrb_SDL_WM_SetCaption SDL_WM_SetCaption

/* ****** ****** */

#define atsctrb_SDL_GL_SwapBuffers SDL_GL_SwapBuffers

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_VIDEO_CATS]

/* end of [SDL_video.cats] */
