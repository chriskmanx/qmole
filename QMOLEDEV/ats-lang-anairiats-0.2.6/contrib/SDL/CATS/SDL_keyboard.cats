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

#ifndef ATSCTRB_SDL_KEYBOARD_CATS
#define ATSCTRB_SDL_KEYBOARD_CATS

/* ****** ****** */

#include "contrib/SDL/CATS/SDL_keysym.cats"

/* ****** ****** */

#define atsctrb_SDL_GetKeyState SDL_GetKeyState

static inline
ats_ref_type // SDL_KeyStateArr
atsctrb_SDL_GetKeyState_null () {
  return SDL_GetKeyState ((int*)0) ;
} // end of [atsctrb_SDL_GetKeyState_null]

static inline
ats_int_type // SDL_KeyStateArr
atsctrb_SDL_KeyStateArr_get (
  ats_ref_type keyarr, SDLKey key
) {
  return ((Uint8*)keyarr)[key] ;
} // end of [atsctrb_SDL_KeyStateArr_get]

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_KEYBOARD_CATS]

/* end of [SDL_keyboard.cats] */
