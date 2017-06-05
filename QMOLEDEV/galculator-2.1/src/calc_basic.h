/*
 *  calc_basic.h - arithmetic precedence handling and computing in basic 
 *			calculator mode.
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
 
#ifndef _CALC_BASIC_H
#define _CALC_BASIC_H 1

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifndef PROG_NAME
	#define PROG_NAME	PACKAGE
#endif

#ifndef BUG_REPORT
	#define BUG_REPORT	"Please submit a bugreport."
#endif

#define RPN_FINITE_STACK		3
#define RPN_INFINITE_STACK		-1

#include <glib.h>

#include "g_real.h"

typedef GSList ALG_OBJECT;

enum {THIS_LEVEL, LEVEL_UP, LEVEL_DOWN};

typedef struct {
	G_REAL		num;		/* numerator */
	G_REAL		denum;		/* denumerator */
} s_frac;

typedef struct {
	s_frac		real;
	s_frac		imag;
} s_complex;

typedef struct {
	G_REAL		number;
	G_REAL		(*func)(G_REAL);
	char		operation;
} s_cb_token;

typedef struct {
	G_REAL		(*func)(G_REAL);
	G_REAL		*number;
	char		*operation;
	int		size;
} s_alg_stack;
	
G_REAL id (G_REAL x);

G_REAL g_trunc(G_REAL x);

#if HAVE_LIBQUADMATH
G_HUGEINT2 greal2hugeint(G_REAL d);
G_REAL hugeint2greal(G_HUGEINT2 h);
#endif 

G_REAL alg_add_token (ALG_OBJECT **alg, s_cb_token this_token);
ALG_OBJECT *alg_init (int debug_level);
void alg_free (ALG_OBJECT *alg);

void rpn_init (int size, int debug_level);
void rpn_stack_set_array (G_REAL *values, int length);
void rpn_stack_push (G_REAL number);
G_REAL rpn_stack_operation (s_cb_token current_token);
G_REAL rpn_stack_rolldown (G_REAL x);
G_REAL rpn_stack_swapxy (G_REAL x);
G_REAL *rpn_stack_get (int length);
void rpn_stack_set_size (int size);
void rpn_free ();

char* float2string(const char*, G_REAL);
char* float2stringP(const char*, int, G_REAL);

#endif /* calc_basic.h */
