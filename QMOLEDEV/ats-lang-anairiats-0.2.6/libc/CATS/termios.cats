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

#ifndef ATS_LIBC_TERMIOS_CATS
#define ATS_LIBC_TERMIOS_CATS

/* ****** ****** */

#include <termios.h>

/* ****** ****** */

typedef cc_t ats_cc_type ;
typedef tcflag_t ats_tcflag_type ;
typedef speed_t ats_speed_type ;

/* ****** ****** */

#define atslib_tcgetattr tcgetattr
#define atslib_tcsetattr tcsetattr

/* ****** ****** */

#define atslib_cfgetispeed cfgetispeed
#define atslib_cfsetispeed cfsetispeed

#define atslib_cfgetospeed cfgetospeed
#define atslib_cfsetospeed cfsetospeed

/* ****** ****** */

#define atslib_tcflow tcflow
#define atslib_tcdrain tcdrain
#define atslib_tcflush tcflush
#define atslib_tcsendbreak tcsendbreak

/* ****** ****** */

#define atslib_tcgetsid tcgetsid

/* ****** ****** */

#endif /* ATS_LIBC_TERMIOS_CATS */

/* end of [termios.cats] */
