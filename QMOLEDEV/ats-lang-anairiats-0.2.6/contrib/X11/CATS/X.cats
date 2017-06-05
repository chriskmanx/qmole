/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
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
*/

/* ****** ****** */

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010

/* ****** ****** */

#ifndef ATSCTRB_XLIB_X_CATS
#define ATSCTRB_XLIB_X_CATS

/* ****** ****** */

#include "X11/X.h"

/* ****** ****** */

typedef ats_lint_type InputEventMask_t;

static inline
InputEventMask_t
atsctrb_lor_InputEventMask_InputEventMask
  (InputEventMask_t x, InputEventMask_t y) { return (x | y) ; }
// end of [atsctrb_lor_InputEventMask_InputEventMask]

/* ****** ****** */

typedef ats_int_type EventType_t;

static inline
ats_bool_type
atsctrb_eq_EventType_EventType
  (EventType_t x, EventType_t y) { return (x==y ? ats_true_bool : ats_false_bool) ; }
// end of [atsctrb_eq_EventType_EventType]

/* ****** ****** */

#endif // end of [ATSCTRB_XLIB_X_CATS]

/* end of [X.cats] */
