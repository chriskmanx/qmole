#ifndef _G_REAL_H
#define _G_REAL_H

/* We need to include config.h here in order to know about HAVE_LIBQUADMATH */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

/* 
 * The basic type in galculator is G_REAL. By default, this is a double 
 * precision floating point number. If we are compiling on gcc 4.6 and later,
 * there is libquadmath available and G_REAL is a quad-precision floating point
 * number.
 * 
 * Quad-precision means a 128-bit float, see
 * 	http://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format
 * We have a fraction of 112 bits and an exponent of 15 bits. So apart from
 * quad-precision floating point arithmetic, we may have 112 bit binary 
 * arithmetic. While +,-,* etc is inherited from floating points, this is 
 * achieved for missing opterators such as and/or/xor in math_functions. 
 */

#if HAVE_LIBQUADMATH

#include <quadmath.h>

typedef __float128 G_REAL;
/* this is at least 64-bit wide */
typedef unsigned long long int G_HUGEINT;
typedef struct {
	G_HUGEINT a;
	G_HUGEINT b;
} G_HUGEINT2;

#define G_SIN sinq
#define G_COS cosq
#define G_TAN tanq
#define G_ASIN asinq
#define G_ACOS acosq
#define G_ATAN atanq

#define G_SINH sinhq
#define G_COSH coshq
#define G_TANH tanhq
#define G_ASINH asinhq
#define G_ACOSH acoshq
#define G_ATANH atanhq

#define G_EXP expq
#define G_LOG10 log10q
#define G_LOG logq
#define G_POW powq
#define G_SQRT sqrtq

#define G_FLOOR floorq
#define G_FMOD fmodq
#define G_LDEXP ldexpq

#define G_LMOD "Q"

#else // HAVE_LIBQUADMATH

typedef double G_REAL;
/* this is at least 64-bit wide */
typedef long long int G_HUGEINT;

#define G_SIN sin
#define G_COS cos
#define G_TAN tan
#define G_ASIN asin
#define G_ACOS acos
#define G_ATAN atan

#define G_SINH sinh
#define G_COSH cosh
#define G_TANH tanh
#define G_ASINH asinh
#define G_ACOSH acosh
#define G_ATANH atanh

#define G_EXP exp
#define G_LOG10 log10
#define G_LOG log
#define G_POW pow
#define G_SQRT sqrt

#define G_FLOOR floor
#define G_FMOD fmod
#define G_LDEXP ldexp

#define G_LMOD ""

#endif // HAVE_LIBQUADMATH

#endif // G_REAL
