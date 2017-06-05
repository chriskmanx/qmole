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

#ifndef ATSCTRB_SDL_CATS
#define ATSCTRB_SDL_CATS

/* ****** ****** */

#include "SDL.h"

/* ****** ****** */

ATSinline()
ats_ptr_type
atsctrb_SDL_ref_null() { return ((ats_ptr_type)0) ; }
ATSinline()
ats_void_type
atsctrb_SDL_ref_free_null (ats_ptr_type p) { return ; }

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_SDL_ref_is_null
  (ats_ref_type p) {
  return (p == (ats_ptr_type)0 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_SDL_is_null]

ATSinline()
ats_bool_type
atsctrb_SDL_ref_isnot_null
  (ats_ref_type p) {
  return (p != (ats_ptr_type)0 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_SDL_ref_isnot_null]

/* ****** ****** */

#include "contrib/SDL/CATS/SDL_stdinc.cats"

/* ****** ****** */

#include "contrib/SDL/CATS/SDL_events.cats"
#include "contrib/SDL/CATS/SDL_timer.cats"
#include "contrib/SDL/CATS/SDL_thread.cats"
#include "contrib/SDL/CATS/SDL_video.cats"

/* ****** ****** */

#define atsctrb_SDL_Init SDL_Init
#define atsctrb_SDL_Quit SDL_Quit

/* ****** ****** */

#define atsctrb_SDL_InitSubSystem InitSubSystem
#define atsctrb_SDL_QuitSubSystem QuitSubSystem
#define atsctrb_SDL_WasInit SDL_WasInit

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_CATS]

/* end of [SDL.cats] */
