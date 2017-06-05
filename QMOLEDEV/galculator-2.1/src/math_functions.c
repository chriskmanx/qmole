/*
 *  math_functions.c - some mathematical functions for the calculator
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
 

#include <math.h>

#include "math_functions.h"
#include "general_functions.h"
#include "galculator.h"

#include <glib.h>		/* for G_PI etc */

G_REAL pow10y (G_REAL y)
{
	return G_POW (10., y);
}

G_REAL reciprocal (G_REAL x)
{
	return 1/x;
}

G_REAL idx (G_REAL x)
{
	return x;
}

G_REAL powx2 (G_REAL x)
{
	return G_POW (x, 2);
}

G_REAL factorial (G_REAL n)
{
	/* to avoid useless factorial computation of big numbers */
    if (n > 200) return INFINITY;
    /* undefined for negative numbers, patch by adrianb23 on sf.net */
    if (n < 0) return INFINITY;
    /* So we know we are positive, now check if n is an integer */
    if (n > G_FLOOR(n)) return INFINITY;

	if (n > 1) return n*factorial (n-1);
	else return 1;
}

#if HAVE_LIBQUADMATH

/* Compute complement (negation) of argument. See greal2hugeint for more information. */
G_REAL cmp (G_REAL n)
{
	G_REAL mask;
	G_HUGEINT2 h1, h2;
	int bits = 112;

	switch (current_status.number)
	{
	case CS_HEX: 
		bits = prefs.hex_bits; 
		break;
	case CS_OCT: 
		bits = prefs.oct_bits; 
		break;
	case CS_BIN: 
		bits = prefs.bin_bits; 
		break;
	}

	mask = scalbnq(1.0Q, bits) - 1;

	h1 = greal2hugeint(n);
	h2 = greal2hugeint(mask);
	h1.a = ~h1.a & h2.a;
	h1.b = ~h1.b & h2.b;
	n = hugeint2greal(h1);
	return n;
}

#else // HAVE_LIBQUADMATH

G_REAL cmp (G_REAL n)
{
	return (G_REAL)(~((G_HUGEINT)n));
}

#endif  // HAVE_LIBQUADMATH

/*
 * angle base conversions
 */

G_REAL rad2deg (G_REAL value)
{
	return (value/G_PI)*180;
}

G_REAL rad2grad (G_REAL value)
{
	return (value/G_PI)*200;
}

G_REAL deg2rad (G_REAL value)
{
	return (value/180)*G_PI;
}

G_REAL grad2rad (G_REAL value)
{
	return (value/200)*G_PI;
}

/*
G_REAL asinh (G_REAL x)
{
	return G_LOG (x + G_SQRT(x*x+1));
}

G_REAL acosh (G_REAL x)
{
	return G_LOG (x + G_SQRT(x*x-1));
}

G_REAL atanh (G_REAL x)
{
	return G_LOG ((1+x)/(1-x))/2;
}
*/

/* sine wrapper. interprete and convert x according to current_status.angle
 */

G_REAL sin_wrapper (G_REAL x) 
{
	return G_SIN(x2rad(x));
}

/* arcus sine wrapper. interprete and convert result according to 
 * current_status.angle
 */

G_REAL asin_wrapper (G_REAL x) 
{
	return rad2x(G_ASIN(x));
}

/* cosine wrapper. interprete and convert x according to current_status.angle
 */

G_REAL cos_wrapper (G_REAL x) 
{
	return G_COS(x2rad(x));
}

/* arcus cosine wrapper. interprete and convert result according to 
 * current_status.angle
 */

G_REAL acos_wrapper (G_REAL x) 
{
	return rad2x(G_ACOS(x));
}

/* tangens wrapper. interprete and convert x according to current_status.angle
 */

G_REAL tan_wrapper (G_REAL x) 
{
	return G_TAN(x2rad(x));
}

/* arcus tangens wrapper. interprete and convert result according to 
 * current_status.angle
 */

G_REAL atan_wrapper (G_REAL x) 
{
	return rad2x(G_ATAN(x));
}
