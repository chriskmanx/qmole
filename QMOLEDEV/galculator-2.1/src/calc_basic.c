/*
 *  calc_basic.c - arithmetic precedence handling and computing in basic 
 *			calculator mode.
 *	part of galculator
 *  	(c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
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
 
/*
 * compile with:
 * gcc calc_basic.c `pkg-config --cflags --libs glib-2.0` -Wall -lm
 *
 * this is calc_basic version 2.
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <math.h>

#include "calc_basic.h"
#include "galculator.h"

/* i18n */

#include <libintl.h>
#define _(String) gettext (String)
#define gettext_noop(String) String
#define N_(String) gettext_noop (String)

static char 	*operator_precedence[] = {"=)", "+-&|x", "*/<>%", "^", "(", "%", NULL};
static char 	*right_associative = "^";

static GArray	*rpn_stack;
static int		rpn_stack_size;
static int		alg_debug = 0, rpn_debug = 0;

/*
 * GENERAL STUFF
 */

/* id. The identity function. This is used as stack function, if none was given.
 */

G_REAL id (G_REAL x)
{
	return x;
}


/*
 * binary arithmetic
 */

#if HAVE_LIBQUADMATH

/* This is a helper function to implement and/or/xor with arguments of up to 
 * 112 bit length. This converts the given G_REAL (which is the type variables
 * are passed around in galculator) to G_HUGEINT2, a struct that is capable of
 * storing at least 128 bits. This function is called at the beginning of
 * and/or/xor to obtain integer representation for right and left hand side. 
 * These are subsequently combined and converted back to G_REAL with 
 * hugeint2greal.
 */
G_HUGEINT2 greal2hugeint(G_REAL d)
{
	G_HUGEINT2 h;
	int bits = 112;

	if (d < 0) {
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
		d += scalbnq(1.0Q, bits);
	}

	/* The first 128-56=56 bits */
	h.a = truncq(d / 0x100000000000000L);
	/* The remainding 56 bits */
	h.b = fmodq(d, 0x100000000000000L);
	/* this makes 112 bits in total */
	
	return h;
}


/* This is a helper function to implement and/or/xor with arguments of up to 
 * 112 bit length. This converts the given G_HUGEINT2 back to G_REAL.
 * 
 * See greal2hugeint for more information.
 */
G_REAL hugeint2greal(G_HUGEINT2 h)
{
	G_REAL d;
	G_REAL mask;
	G_HUGEINT2 maskh;
	int is_signed = 1;
	int bits = 112;

	switch (current_status.number)
	{
	case CS_HEX:
		is_signed = prefs.hex_signed;
		bits = prefs.hex_bits;
		break;
	case CS_OCT:
		is_signed = prefs.oct_signed;
		bits = prefs.oct_bits;
		break;
	case CS_BIN:
		is_signed = prefs.bin_signed;
		bits = prefs.bin_bits;
		break;
	}

	/* d = h.a*2^56 + h.b */
	d = scalbnq((__float128)(h.a & 0xffffffffffffffL), 56) +
		(__float128)(h.b & 0xffffffffffffffL);

	if (is_signed) {
		mask = scalbnq(1.0Q, bits - 1);
		maskh = greal2hugeint(mask);
		if ((h.a & maskh.a) | (h.b & maskh.b))
			d -= scalbnq(1.0Q, bits);
	}

	return d;
}

/* See greal2hugeint for more information. */
static G_REAL and(G_REAL left_hand, G_REAL right_hand)
{
	G_HUGEINT2 hl, hr, h;
	hl = greal2hugeint(left_hand);
	hr = greal2hugeint(right_hand);
	h.a = hl.a & hr.a;
	h.b = hl.b & hr.b;
	return hugeint2greal(h);
}

/* See greal2hugeint for more information. */
static G_REAL or(G_REAL left_hand, G_REAL right_hand)
{
	G_HUGEINT2 hl, hr, h;
	hl = greal2hugeint(left_hand);
	hr = greal2hugeint(right_hand);
	h.a = hl.a | hr.a;
	h.b = hl.b | hr.b;
	return hugeint2greal(h);
}

/* See greal2hugeint for more information. */
static G_REAL xor(G_REAL left_hand, G_REAL right_hand)
{
	G_HUGEINT2 hl, hr, h;
	hl = greal2hugeint(left_hand);
	hr = greal2hugeint(right_hand);
	h.a = hl.a ^ hr.a;
	h.b = hl.b ^ hr.b;
	return hugeint2greal(h);
}

#else // HAVE_LIBQUADMATH

static G_REAL and(G_REAL left_hand, G_REAL right_hand)
{
	return (G_HUGEINT)left_hand & (G_HUGEINT)right_hand;
}

static G_REAL or(G_REAL left_hand, G_REAL right_hand)
{
	return (G_HUGEINT)left_hand | (G_HUGEINT)right_hand;
}

static G_REAL xor(G_REAL left_hand, G_REAL right_hand)
{
	return (G_HUGEINT)left_hand ^ (G_HUGEINT)right_hand;
}

#endif // HAVE_LIBQUADMATH

/* debug_input. debug code: enter tokens on stdin.
 */
/*
void debug_input ()
{
	char		input[20];
	s_cb_token	current_token;
	
	scanf ("%s", input);
	current_token.number = atof (input);
	scanf ("%s", input);
	current_token.operation = input[0];
	current_token.func = NULL;
	printf ("\t\tdisplay value: %"G_LMOD"f\n", alg_add_token (current_token));
}
*/

/* reduce. TRUE if op1 comes before op2 in a computation.
 */

static int reduce (char op1, char op2)
{
	int	counter = 0, p1 = 0, p2 = 0;
	
	while (operator_precedence[counter] != NULL) {
		if (strchr (operator_precedence[counter], op1) != NULL)
			p1 = counter;
		if (strchr (operator_precedence[counter], op2) != NULL)
			p2 = counter;
		counter++;
	}
	if (p1 < p2) return FALSE;
	/* associativity only makes sense for same operations */
	else if ((op1 == op2) && (strchr (right_associative, op1) != NULL)) 
		return FALSE;
	else return TRUE;
}


/* compute_expression. here, the real copmputation is done.
 */

static G_REAL compute_expression (G_REAL left_hand, 
				char operator, 
				G_REAL right_hand)
{
	G_REAL result;
	
	switch (operator) {
	case '+':
		result = left_hand + right_hand;
		break;
	case '-':
		result = left_hand - right_hand;
		break;
	case '*':
		result = left_hand * right_hand;
		break;
	case '/':
		result = left_hand / right_hand;
		break;
	case '^':
		result = G_POW (left_hand, right_hand);
		break;
	case '<':
		/* left shift x*2^n */
		result = G_LDEXP (left_hand, (int) G_FLOOR(right_hand));
		/* truncate result to have integers as result only. negative exponent
		 * makes left shifitng a right shifting.
		 */
		result = g_trunc(result);
		break;
	case '>':
		/* right shift x*2^(-n). */
		result = G_LDEXP (left_hand, ((int) G_FLOOR(right_hand))*(-1));
		/* truncate result to have integers as result only */
		result = g_trunc(result);
		break;
    case 'm':
        result = G_FMOD (left_hand, right_hand);
		break;
	case '&':
		result = and(left_hand, right_hand);
		break;
	case '|':
		result = or(left_hand, right_hand);
		break;
	case 'x':
		result = xor(left_hand, right_hand);
		break;
	case '%':
		result = left_hand * right_hand/100.;
		break;
	default: 
		if (alg_debug+rpn_debug > 0) fprintf (stderr, _("[%s] %c - unknown operation \
character. %s\n"), PROG_NAME, operator, BUG_REPORT);
		result = left_hand;
		break;
	}	
	if (alg_debug + rpn_debug > 0) {
		char *slh = float2string("%"G_LMOD"f", left_hand);
		char *srh = float2string("%"G_LMOD"f", right_hand);
		char *sr = float2string("%"G_LMOD"f", result);
		fprintf (stderr, "[%s] computing: %s %c %s = %s\n", PROG_NAME, slh, operator, srh, sr);
		free(slh);
		free(srh);
		free(sr);
	}
	return result;
}

/* implement truncate with floor function:
 * 	x > 0 --> trunc(x) = floor(x)			trunc(0.4) = floor(0.4) = 0
 *  x < 0 --> trunc(x) = floor(x) + 1		trunc(-0.4) = 0 = floor(-0.4) + 1 = -1 + 1 = 0
 */

G_REAL g_trunc(G_REAL x)
{
	return G_FLOOR(x) + (x < 0);
}

/*
 * (PSEUDO)ALGEBRAIC MODE
 */

/* alg_stack_new. we are working with stacks. every bracket layer is a single 
 * stack. this function creates a new stack.
 */

static s_alg_stack *alg_stack_new (s_cb_token this_token)
{
	s_alg_stack	*new_stack;
	
	new_stack = (s_alg_stack *) malloc (sizeof(s_alg_stack));
	new_stack->func = this_token.func;
	new_stack->number = NULL;
	new_stack->operation = NULL;
	new_stack->size = 0;
	return new_stack;	
}

/* alg_stack_append. simply appends token (number and operation) to stack.
 * that's all (no computation etc!).
 */

static void alg_stack_append (s_alg_stack *stack, s_cb_token token)
{
	stack->size++;
	stack->number = (G_REAL *) realloc (stack->number, 
		stack->size * sizeof(G_REAL));
	stack->number[stack->size-1] = token.number;
	stack->operation = (char *) realloc (stack->operation,
		stack->size * sizeof(char));
	stack->operation[stack->size-1] = token.operation;
}

/* alg_stack_pool. do as many computation as possible with respect to
 * precedence. here, reduce from above is used.
 */

static G_REAL alg_stack_pool (s_alg_stack *stack)
{
	int	index;
	
	index = stack->size - 1;
	while ((index >= 1) && (reduce(stack->operation[index-1], 
		stack->operation[index]))) {
			stack->number[index - 1] = compute_expression (
				stack->number[index - 1],
				stack->operation[index - 1],
				stack->number[index]);
			stack->operation[index - 1] = stack->operation[index];
			index--;
	}
	if (stack->size != (index + 1)) {
		stack->size = index + 1;
		stack->number = (G_REAL *) realloc (stack->number,
			sizeof(G_REAL) * stack->size);
		stack->operation = (char *) realloc (stack->operation, 
			sizeof(char) * stack->size);
	}
	return stack->number[stack->size-1];
}

/* alg_stack_free. the stack destructor. 
 */

static void alg_stack_free (s_alg_stack *stack)
{
	if (!stack) {
		if (!stack->number) free (stack->number);
		if (!stack->operation) free (stack->operation);
		free (stack);
	}
}

/* alg_stack_free_gfunc. wrapper for alg_stack_free for g_slist_foreach.
 */

static void alg_stack_free_gfunc (gpointer data, gpointer user_data)
{
	s_alg_stack	*stack;
	
	stack = data;
	alg_stack_free (stack);
}

/* alg_add_token. call this from outside. 
 */

G_REAL alg_add_token (ALG_OBJECT **alg, s_cb_token this_token)
{
	static G_REAL	return_value;
	s_alg_stack	*current_stack;
	
	switch (this_token.operation) {
	case '(':
		if (this_token.func == NULL) this_token.func = id;
		*alg = g_slist_prepend (*alg, alg_stack_new (this_token));
		break;
	case ')':
		if (g_slist_length (*alg) < 2) break;
		current_stack = (s_alg_stack *) (*alg)->data;
		alg_stack_append (current_stack, this_token);
		return_value = current_stack->func (
			alg_stack_pool (current_stack));
		/* we may specify a function after a bracket enclosed expression
		 * this is necessary e.g. for factorial. This function is 
		 * applied after the stack function, so sin(3)! will be (sin(3))!
		 */
		if (this_token.func != NULL)
			return_value = this_token.func(return_value);
		alg_stack_free (current_stack);
		*alg = g_slist_delete_link (*alg, *alg);
		break;
	case '=':
		/* first close all open brackets */
		while (g_slist_length (*alg) > 1) {
			this_token.operation = ')';
			this_token.number = alg_add_token (alg, this_token);
		}
		this_token.operation = '=';
		current_stack = (s_alg_stack *) (*alg)->data;
		alg_stack_append (current_stack, this_token);
		return_value = alg_stack_pool (current_stack);
		alg_free (*alg);
		*alg = alg_init(alg_debug);
		break;
	default:
		current_stack = (s_alg_stack *) (*alg)->data;
		alg_stack_append (current_stack, this_token);
		return_value = alg_stack_pool (current_stack);
		break;
	}
	return return_value;
}

/* alg_init. use this from outside to initialize everything
 */

ALG_OBJECT *alg_init (int debug_level)
{
	s_cb_token	token;
	
	alg_debug = debug_level;
	token.func = id;
	return g_slist_prepend (NULL, alg_stack_new(token));
}

/* alg_free. call this from outside to clean up properly
 */

void alg_free (ALG_OBJECT *alg)
{
	if (!alg) {
		g_slist_foreach (alg, alg_stack_free_gfunc, NULL);
		g_slist_free (alg);
		alg = NULL;
	}
}

/*
 * RPN
 */

/* rpn_init. initializes everything from stack to sizes and debug.
 */

void rpn_init (int size, int debug_level)
{
	rpn_stack = g_array_new (FALSE, FALSE, sizeof(G_REAL));
	rpn_stack_size = size;
	rpn_debug = debug_level;
}

/* debug_rpn_stack_print. printf stack to stderr
 */

void debug_rpn_stack_print ()
{
	int 	counter;
	G_REAL	*stack;
	char *str;
	
	stack = rpn_stack_get (rpn_stack_size);
	for (counter = 0; counter < MAX(rpn_stack_size, (int)rpn_stack->len); counter++) {
		str = float2string("%"G_LMOD"f", stack[counter]);
		fprintf (stderr, "[%s]\t %02i: %s\n", PROG_NAME, counter, str);
		free(str);
	}
	free (stack);
}

/* rpn_stack_push. new values is prepended! then, if finite stack size, 
 * remove last one. in the end some debugs.
 */

void rpn_stack_push (G_REAL number)
{
	rpn_stack = g_array_prepend_val (rpn_stack, number);
	if (((int)rpn_stack->len > rpn_stack_size) && (rpn_stack_size > 0))
		rpn_stack = g_array_remove_index (rpn_stack, rpn_stack_size);
	if (rpn_debug > 0) fprintf (stderr, "[%s] RPN stack size is %i.\n", 
		PROG_NAME, (int)rpn_stack->len);
	if (rpn_debug > 1) debug_rpn_stack_print();
}

/* rpn_stack_operation. does some operation. This is also the place where the
 * stack is popped!
 */

G_REAL rpn_stack_operation (s_cb_token current_token)
{
	G_REAL	return_value;
	G_REAL	left_hand;
	G_REAL	last_on_stack;
	
	/* this function only serves binary operations. therefore, we need at 
	 * least one element on the stack. if this is not the case, work with 0.
	 */
	if (rpn_stack->len < 1) left_hand = 0;
	else {
		/* retrieve left_hand from stack */
		left_hand = g_array_index (rpn_stack, G_REAL, 0);
		last_on_stack = g_array_index (rpn_stack, G_REAL, (int)rpn_stack->len-1);
		rpn_stack = g_array_remove_index (rpn_stack, 0);
		/* last register is kept, if stack size is finite */
		if (((int) rpn_stack->len == rpn_stack_size-1) && (rpn_stack_size > 0))
			rpn_stack = g_array_append_val (rpn_stack, last_on_stack);
	}
	/* compute it */
	return_value = compute_expression (left_hand, current_token.operation, 
		current_token.number);
	if (rpn_debug > 0) fprintf (stderr, "[%s] RPN stack size is %i.\n", 
		PROG_NAME, (int)rpn_stack->len);
	if (rpn_debug > 1) debug_rpn_stack_print();
	return return_value;
}

/* rpn_stack_swapxy. swap first and second register. there are some special cases.
 */

G_REAL rpn_stack_swapxy (G_REAL x)
{
	G_REAL	*y, ret_val;
	
	if ((int)rpn_stack->len < 1) { 
		ret_val = 0.;
		rpn_stack = g_array_append_val (rpn_stack, x);
	} else {
		y = &g_array_index (rpn_stack, G_REAL, 0);
		ret_val = *y;
		*y = x;
	}
	if (rpn_debug > 0) fprintf (stderr, "[%s] RPN stack size is %i.\n", 
		PROG_NAME, (int)rpn_stack->len);
	if (rpn_debug > 1) debug_rpn_stack_print();
	return ret_val;
}

/* rpn_stack_rolldown. y->x, z->y, ..., x->t
 * return value ret_val is new result.
 */

G_REAL rpn_stack_rolldown (G_REAL x)
{
	G_REAL	*a, ret_val;
	int	counter;
	
	if (rpn_stack_size <= 0) return x;
	ret_val = 0.;
	/* in the following case we have to fill up with zeros. thus this is
	 * done virtually in rpn_stack_get.
	 */
	if ((rpn_stack_size > 0) && ((int)rpn_stack->len < rpn_stack_size))
		for (counter = rpn_stack->len; counter < rpn_stack_size; counter++)
			rpn_stack = g_array_append_val (rpn_stack, ret_val);
	ret_val = g_array_index (rpn_stack, G_REAL, 0);
	for (counter = 0; counter < (int) rpn_stack->len - 1; counter++) {
		a = &g_array_index (rpn_stack, G_REAL, counter);
		*a = g_array_index (rpn_stack, G_REAL, counter + 1);
	}
	a = &g_array_index (rpn_stack, G_REAL, rpn_stack->len - 1);
	*a = x;
	return ret_val;
}

/* rpn_stack_get. returns a G_REAL array with the first length elements of
 * stack. returned array should be freed.
 */

G_REAL *rpn_stack_get (int length)
{
	G_REAL		*return_array;
	int		counter;
	
	if (length <= 0) length = (int)rpn_stack->len;
	return_array = (G_REAL *) malloc (length*sizeof(G_REAL));
	for (counter = 0; counter < MIN (length, (int)rpn_stack->len); counter++)
		return_array[counter] = g_array_index (rpn_stack, G_REAL, counter);
	for (; counter < length; counter++) 
		return_array[counter] = 0.;
	return return_array;
}

/* rpn_stack_set_size. write rpn_stack_size.
 */

void rpn_stack_set_size (int size)
{
	int	counter;

	if ((size > 0) && ((size < rpn_stack_size) || (rpn_stack_size == -1)))
		for (counter = ((int)rpn_stack->len-1); counter >= size; counter--)
			rpn_stack = g_array_remove_index (rpn_stack, counter);
	rpn_stack_size = size;
}
/* rpn_free. the finalizer.
 */

void rpn_free ()
{
	if (!rpn_stack) g_array_free (rpn_stack, TRUE);
}

/*
int main (int argc, char *argv[])
{
	alg_init(1);
	while (1) debug_input();
	return 1;
}
*/

/*! \brief Convert floating point number to string.
 * 
 * With quad precision support the printing of numbers is a bit tricky. 
 * libquadmath documentation says the Q modifier should be supported for all
 * variants of printf, but it turned out this was not the case. Hence, this
 * work-around.
 * 
 * We implement this here in calc_basic as we don't want to include any further
 * files in here.
 * 
 * \param x Number to convert
 * 
 * \return Pointer to newly allocated memory storing a string representation
 * 		of given argument. Caller must free the code!
 */ 
char *float2string(const char* formatString, G_REAL x)
{
	char *s = (char *) malloc(128*sizeof(char));
	int len = 0;

#if HAVE_LIBQUADMATH
	len = quadmath_snprintf(s, 128*sizeof(char), formatString, x); 
#else // HAVE_LIBQUADMATH
	len = snprintf(s, 128*sizeof(char), formatString, x); 
#endif // HAVE_LIBQUADMATH

	if (len >= 128)
		fprintf (stderr, _("[%s] Conversion of floating point number in float2string \
failed because buffer was too small. %s\n)"), PACKAGE, BUG_REPORT);

	return s;
}

/*! \brief Convert floating point number to string of given precision
 * 
 * This is an add-on to float2string assuming the format string allows us 
 * to define the precision of the string representation.
 * 
 * \param prec Number of digits
 * \param x Number to convert
 * 
 * \return Pointer to newly allocated memory storing a string representation
 * 		of given argument. Caller must free the code!
 */ 
char *float2stringP(const char* formatString, int prec, G_REAL x)
{
	char *s = (char *) malloc(128*sizeof(char));
	int len = 0;

#if HAVE_LIBQUADMATH
	len = quadmath_snprintf(s, 128*sizeof(char), formatString, prec, x); 
#else // HAVE_LIBQUADMATH
	len = snprintf(s, 128*sizeof(char), formatString, prec, x); 
#endif // HAVE_LIBQUADMATH

	if (len >= 128)
		fprintf (stderr, _("[%s] Conversion of floating point number in float2stringP \
failed because buffer was too small. %s\n)"), PACKAGE, BUG_REPORT);

	return s;
}
