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

#ifndef ATSCTRB_SDL_STDINC_CATS
#define ATSCTRB_SDL_STDINC_CATS

/* ****** ****** */

static inline
Uint8
atsctrb_land_Uint8_Uint8
  (Uint8 x1, Uint8 x2) { return (x1 & x2) ; }
// end of [atsctrb_land_Uint8_Uint8]

static inline
Uint8
atsctrb_lor_Uint8_Uint8
  (Uint8 x1, Uint8 x2) { return (x1 | x2) ; }
// end of [atsctrb_lor_Uint8_Uint8]

/* ****** ****** */

static inline
Uint16
atsctrb_add_Uint16_Uint16
  (Uint16 x1, Uint16 x2) { return (x1 + x2) ; }
// end of [atsctrb_add_Uint16_Uint16]

static inline
Uint16
atsctrb_sub_Uint16_Uint16
  (Uint16 x1, Uint16 x2) { return (x1 - x2) ; }
// end of [atsctrb_sub_Uint16_Uint16]

static inline
Uint16
atsctrb_land_Uint16_Uint16
  (Uint16 x1, Uint16 x2) { return (x1 & x2) ; }
// end of [atsctrb_land_Uint16_Uint16]

static inline
Uint16
atsctrb_lor_Uint16_Uint16
  (Uint16 x1, Uint16 x2) { return (x1 | x2) ; }
// end of [atsctrb_lor_Uint16_Uint16]

/* ****** ****** */

static inline
Uint32
atsctrb_add_Uint32_Uint32
  (Uint32 x1, Uint32 x2) { return (x1 + x2) ; }
// end of [atsctrb_add_Uint32_Uint32]

static inline
Uint32
atsctrb_sub_Uint32_Uint32
  (Uint32 x1, Uint32 x2) { return (x1 - x2) ; }
// end of [atsctrb_sub_Uint32_Uint32]

static inline
Uint32
atsctrb_land_Uint32_Uint32
  (Uint32 x1, Uint32 x2) { return (x1 & x2) ; }
// end of [atsctrb_land_Uint32_Uint32]

static inline
Uint32
atsctrb_lor_Uint32_Uint32
  (Uint32 x1, Uint32 x2) { return (x1 | x2) ; }
// end of [atsctrb_lor_Uint32_Uint32]

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_STDINC_CATS]

/* end of [SDL_stdinc.cats] */
