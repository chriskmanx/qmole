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

#ifndef ATSCTRB_SDL_EVENTS_CATS
#define ATSCTRB_SDL_EVENTS_CATS

/* ****** ****** */

#include "contrib/SDL/CATS/SDL_active.cats"
#include "contrib/SDL/CATS/SDL_keyboard.cats"

/* ****** ****** */

static inline
ats_bool_type
atsctrb_eq_SDL_EventType_SDL_EventType
  (SDL_EventType x1, SDL_EventType x2) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_eq_SDL_EventType_SDL_EventType]

/* ****** ****** */

#define atsctrb_SDL_Event_type(event) (((SDL_Event*)(event))->type)

/* ****** ****** */

#define atsctrb_SDL_KeyboardEvent_type(_ref) \
  ats_field_getval(SDL_KeyboardEvent, _ref, type)
#define atsctrb_SDL_KeyboardEvent_which(_ref) \
  ats_field_getval(SDL_KeyboardEvent, _ref, which)
#define atsctrb_SDL_KeyboardEvent_state(_ref) \
  ats_field_getval(SDL_KeyboardEvent, _ref, state)
#define atsctrb_SDL_KeyboardEvent_keysym(_ref) \
  ats_field_getval(SDL_KeyboardEvent, _ref, keysym)

/* ****** ****** */

#define atsctrb_SDL_PollEvent SDL_PollEvent
#define atsctrb_SDL_PollEvent_null () SDL_PollEvent((SDL_Event*)0) ;

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_EVENTS_CATS]

/* end of [SDL_events.cats] */
