/*
 *  math_functions.h
 *	part of galculator
 *  	(c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef _MATH_FUNCTIONS_H
#define _MATH_FUNCTIONS_H 1

#include "g_real.h"

G_REAL pow10y (G_REAL y);
G_REAL reciprocal (G_REAL x);
G_REAL idx (G_REAL x);
G_REAL powx2 (G_REAL x);
G_REAL factorial (G_REAL n);

G_REAL cmp (G_REAL n);

G_REAL rad2deg (G_REAL value);
G_REAL rad2grad (G_REAL value);
G_REAL deg2rad (G_REAL value);
G_REAL grad2rad (G_REAL value);
/*
G_REAL asinh (G_REAL x);
G_REAL acosh (G_REAL x);
G_REAL atanh (G_REAL x);
*/
G_REAL sin_wrapper (G_REAL x);
G_REAL asin_wrapper (G_REAL x);
G_REAL cos_wrapper (G_REAL x);
G_REAL acos_wrapper (G_REAL x);
G_REAL tan_wrapper (G_REAL x);
G_REAL atan_wrapper (G_REAL x);

#endif /* math_functions.h */
