/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi.
**
** ATS is  free software;  you can redistribute it and/or modify it under
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
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_LIBC_CURSES_CATS
#define ATS_LIBC_CURSES_CATS

/* ****** ****** */

#include <curses.h>

/* ****** ****** */

#define atslib_initscr initscr
#define atslib_endwin endwin
#define atslib_isendwin isendwin

/* ****** ****** */

#define atslib_raw raw
#define atslib_noraw noraw

/* ****** ****** */

#define atslib_clear clear
#define atslib_clrtobot clrtobot
#define atslib_clrtoeol clrtoeol
#define atslib_erase erase

/* ****** ****** */

#define atslib_beep beep
#define atslib_flush flush

/* ****** ****** */

#define atslib_addstr addstr
#define atslib_addnstr addnstr
#define atslib_mvaddstr mvaddstr
#define atslib_mvaddnstr mvaddnstr

/* ****** ****** */

#define atslib_refresh refresh
#define atslib_doupdate doupdate

/* ****** ****** */

#endif /* ATS_LIBC_CURSES_CATS */

/* end of [curses.cats] */
