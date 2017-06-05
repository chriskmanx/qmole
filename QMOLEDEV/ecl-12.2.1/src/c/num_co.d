/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_co.c -- Operations on floating-point numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
	IMPLEMENTATION-DEPENDENT

	This file contains those functions
	that know the representation of floating-point numbers.
*/

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <float.h>
#ifndef HAVE_ISOC99
# define floorf floor
# define ceilf ceil
# define fabsf fabs
# define frexpf frexp
# define ldexpf ldexp
# define cosf cos
# define coshf cosh
# define expf exp
# define logf log
# define sinf sin
# define sqrtf sqrt
# define tanf tan
# define tanhf tanh
#endif

static cl_object
number_remainder(cl_object x, cl_object y, cl_object q)
{
	cl_object z;

	z = ecl_times(q, y);
	z = ecl_minus(x, z);
	return(z);
}

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

@(defun float (x &optional (y OBJNULL))
	cl_type ty, tx;
@
	if (y != OBJNULL) {
		ty = type_of(y);
	} else {
		ty = t_singlefloat;
	}
	switch (tx = type_of(x)) {
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		if (y == OBJNULL || ty == tx)
			break;
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		switch (ty) {
		case t_singlefloat:
			x = ecl_make_singlefloat(ecl_to_double(x)); break;
		case t_doublefloat:
			x = ecl_make_doublefloat(ecl_to_double(x)); break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			x = ecl_make_longfloat(ecl_to_long_double(x)); break;
#endif
		default:
                        FEwrong_type_nth_arg(@[float],2,y,@[float]);
		}
		break;
	default:
                FEwrong_type_nth_arg(@[float],1,x,@[real]);
	}
	@(return x)
@)

cl_object
cl_numerator(cl_object x)
{
	switch (type_of(x)) {
	case t_ratio:
		x = x->ratio.num;
		break;
	case t_fixnum:
	case t_bignum:
		break;
	default:
                FEwrong_type_nth_arg(@[numerator],1,x,@[rational]);
	}
	@(return x)
}

cl_object
cl_denominator(cl_object x)
{
	switch (type_of(x)) {
	case t_ratio:
		x = x->ratio.den;
		break;
	case t_fixnum:
	case t_bignum:
		x = MAKE_FIXNUM(1);
		break;
	default:
                FEwrong_type_nth_arg(@[numerator],1,x,@[rational]);
	}
	@(return x)
}

cl_object
ecl_floor1(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		v0 = ecl_floor2(x->ratio.num, x->ratio.den);
		v1 = ecl_make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = sf(x);
		float y = floorf(d);
		v0 = float_to_integer(y);
		v1 = ecl_make_singlefloat(d - y);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double y = floor(d);
		v0 = double_to_integer(y);
		v1 = ecl_make_doublefloat(d - y);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double y = floorl(d);
		v0 = long_double_to_integer(y);
		v1 = ecl_make_longfloat(d - y);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[floor],1,x,@[real]);
	}
	@(return v0 v1)
}

cl_object
ecl_floor2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_type ty;
        ty = type_of(y);
	if (ecl_unlikely(!REAL_TYPE(ty))) {
                FEwrong_type_nth_arg(@[floor],2,y,@[real]);
	}
	switch(type_of(x)) {
	case t_fixnum:
		switch(ty) {
		case t_fixnum: {	/* FIX / FIX */
		  cl_fixnum a = fix(x), b = fix(y);
		  cl_fixnum q = a / b,  r = a % b;
		  if ((r^b) < 0 && r) {	/* opposite sign and some remainder*/
		    v0 = MAKE_FIXNUM(q-1);
		    v1 = MAKE_FIXNUM(r+b);
		  } else {
		    v0 = MAKE_FIXNUM(q);
		    v1 = MAKE_FIXNUM(r);
		  }
		  break;
		}
		case t_bignum: {	/* FIX / BIG */
		  /* We must perform the division because there is the
		   * pathological case
		   *	x = MOST_NEGATIVE_FIXNUM
		   *    y = - MOST_NEGATIVE_FIXNUM
		   */
                  ECL_WITH_TEMP_BIGNUM(bx,4);
                  _ecl_big_set_fixnum(bx, fix(x));
                  v0 = _ecl_big_floor(bx, y, &v1);
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  v0 = ecl_floor2(ecl_times(x, y->ratio.den), y->ratio.num);
		  v1 = ecl_make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* FIX / SF */
		  float n = sf(y);
		  float p = fix(x) / n;
		  float q = floorf(p);
		  v0 = float_to_integer(q);
		  v1 = ecl_make_singlefloat((p - q)*n);
		  break;
		}
		case t_doublefloat: {	/* FIX / DF */
		  double n = df(y);
		  double p = fix(x) / n;
		  double q = floor(p);
		  v0 = double_to_integer(q);
		  v1 = ecl_make_doublefloat((p - q)*n);
		  break;
		}
#ifdef ECL_LONG_FLOAT
		case t_longfloat: {	/* FIX / LF */
		  long double n = ecl_long_float(y);
		  long double p = fix(x) / n;
		  long double q = floorl(p);
		  v0 = long_double_to_integer(q);
		  v1 = ecl_make_longfloat((p - q)*n);
		  break;
		}
#endif
		default:
		  (void)0; /* Never reached */
		}
		break;
	case t_bignum:
		switch(ty) {
		case t_fixnum: {	/* BIG / FIX */
                  ECL_WITH_TEMP_BIGNUM(by,4);
                  _ecl_big_set_fixnum(by, fix(y));
                  v0 = _ecl_big_floor(x, by, &v1);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
                  v0 = _ecl_big_floor(x, y, &v1);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  v0 = ecl_floor2(ecl_times(x, y->ratio.den), y->ratio.num);
		  v1 = ecl_make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* BIG / SF */
		  float n = sf(y);
		  float p = _ecl_big_to_double(x) / n;
		  float q = floorf(p);
		  v0 = float_to_integer(q);
		  v1 = ecl_make_singlefloat((p - q)*n);
		  break;
		}
		case t_doublefloat: {	/* BIG / DF */
		  double n = df(y);
		  double p = _ecl_big_to_double(x) / n;
		  double q = floor(p);
		  v0 = double_to_integer(q);
		  v1 = ecl_make_doublefloat((p - q)*n);
		  break;
		}
#ifdef ECL_LONG_FLOAT
		case t_longfloat: {	/* BIG / LF */
		  long double n = ecl_long_float(y);
		  long double p = _ecl_big_to_double(x) / n;
		  long double q = floorl(p);
		  v0 = long_double_to_integer(q);
		  v1 = ecl_make_longfloat((p - q)*n);
		  break;
		}
#endif
		default:
		  (void)0; /* Never reached */
		}
		break;
	case t_ratio:
		switch(ty) {
		case t_ratio:		/* RAT / RAT */
		  v0 = ecl_floor2(ecl_times(x->ratio.num, y->ratio.den),
				  ecl_times(x->ratio.den, y->ratio.num));
		  v1 = ecl_make_ratio(VALUES(1), ecl_times(x->ratio.den, y->ratio.den));
		  break;
		default:		/* RAT / ANY */
		  v0 = ecl_floor2(x->ratio.num, ecl_times(x->ratio.den, y));
		  v1 = ecl_divide(VALUES(1), x->ratio.den);
		}
		break;
	case t_singlefloat: {		/* SF / ANY */
		float n = ecl_to_double(y);
		float p = sf(x)/n;
		float q = floorf(p);
		v0 = float_to_integer(q);
		/* We cannot factor these two multiplications because
		 * if we have signed zeros (1 - 1) * (-1) = -0 while
		 * 1*(-1) - 1*(-1) = +0 */
		v1 = ecl_make_singlefloat(p*n - q*n);
		break;
	}
	case t_doublefloat: {		/* DF / ANY */
		double n = ecl_to_double(y);
		double p = df(x)/n;
		double q = floor(p);
		v0 = double_to_integer(q);
		v1 = ecl_make_doublefloat(p*n - q*n);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {		/* LF / ANY */
		long double n = ecl_to_long_double(y);
		long double p = ecl_long_float(x)/n;
		long double q = floorl(p);
		v0 = long_double_to_integer(q);
		v1 = ecl_make_longfloat(p*n - q*n);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[floor], 1, x, @[real]);
	}
	@(return v0 v1)
}

@(defun floor (x &optional (y OBJNULL))
@
	if (narg == 1)
		x = ecl_floor1(x);
	else
		x = ecl_floor2(x, y);
	returnn(x);
@)

cl_object
ecl_ceiling1(cl_object x)
{
	cl_object v0, v1;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		v0 = ecl_ceiling2(x->ratio.num, x->ratio.den);
		v1 = ecl_make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = sf(x);
		float y = ceilf(d);
		v0 = float_to_integer(y);
		v1 = ecl_make_singlefloat(d - y);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double y = ceil(d);
		v0 = double_to_integer(y);
		v1 = ecl_make_doublefloat(d - y);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double y = ceill(d);
		v0 = long_double_to_integer(y);
		v1 = ecl_make_longfloat(d - y);
		break;
	}
#endif
	default:
		FEwrong_type_nth_arg(@[ceiling],1,x,@[real]);
	}
	@(return v0 v1)
}

cl_object
ecl_ceiling2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_type ty;
        ty = type_of(y);
	if (ecl_unlikely(!REAL_TYPE(ty))) {
		FEwrong_type_nth_arg(@[ceiling],2, y, @[real]);
	}
	switch(type_of(x)) {
	case t_fixnum:
		switch(ty) {
		case t_fixnum: {	/* FIX / FIX */
		  cl_fixnum a = fix(x); cl_fixnum b = fix(y);
		  cl_fixnum q = a / b;  cl_fixnum r = a % b;
		  if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    v0 = MAKE_FIXNUM(q+1);
		    v1 = MAKE_FIXNUM(r-b);
		  } else {
		    v0 = MAKE_FIXNUM(q);
		    v1 = MAKE_FIXNUM(r);
		  }
		  break;
		}
		case t_bignum: {	/* FIX / BIG */
		  /* We must perform the division because there is the
		   * pathological case
		   *	x = MOST_NEGATIVE_FIXNUM
		   *    y = - MOST_NEGATIVE_FIXNUM
		   */
                  ECL_WITH_TEMP_BIGNUM(bx,4);
                  _ecl_big_set_fixnum(bx, fix(x));
                  v0 = _ecl_big_ceiling(bx, y, &v1);
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  v0 = ecl_ceiling2(ecl_times(x, y->ratio.den), y->ratio.num);
		  v1 = ecl_make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* FIX / SF */
		  float n = sf(y);
		  float p = fix(x)/n;
		  float q = ceilf(p);
		  v0 = float_to_integer(q);
		  v1 = ecl_make_singlefloat(p*n - q*n);
		  break;
		}
		case t_doublefloat: {	/* FIX / DF */
		  double n = df(y);
		  double p = fix(x)/n;
		  double q = ceil(p);
		  v0 = double_to_integer(q);
		  v1 = ecl_make_doublefloat(p*n - q*n);
		  break;
		}
#ifdef ECL_LONG_FLOAT
		case t_longfloat: {	/* FIX / LF */
		  long double n = ecl_long_float(y);
		  long double p = fix(x)/n;
		  long double q = ceill(p);
		  v0 = long_double_to_integer(q);
		  v1 = ecl_make_longfloat(p*n - q*n);
		  break;
		}
#endif
		default:
		  (void)0; /*Never reached */
		}
		break;
	case t_bignum:
		switch(type_of(y)) {
		case t_fixnum: {	/* BIG / FIX */
                  ECL_WITH_TEMP_BIGNUM(by,4);
                  _ecl_big_set_fixnum(by, fix(y));
                  v0 = _ecl_big_ceiling(x, by, &v1);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
                  v0 = _ecl_big_ceiling(x, y, &v1);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  v0 = ecl_ceiling2(ecl_times(x, y->ratio.den), y->ratio.num);
		  v1 = ecl_make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* BIG / SF */
		  float n = sf(y);
		  float p = _ecl_big_to_double(x)/n;
		  float q = ceilf(p);
		  v0 = float_to_integer(q);
		  v1 = ecl_make_singlefloat(p*n - q*n);
		  break;
		}
		case t_doublefloat: {	/* BIG / DF */
		  double n = df(y);
		  double p = _ecl_big_to_double(x)/n;
		  double q = ceil(p);
		  v0 = double_to_integer(q);
		  v1 = ecl_make_doublefloat(p*n - q*n);
		  break;
		}
#ifdef ECL_LONG_FLOAT
		case t_longfloat: {	/* BIG / LF */
		  long double n = ecl_long_float(y);
		  long double p = _ecl_big_to_double(x)/n;
		  long double q = ceill(p);
		  v0 = long_double_to_integer(q);
		  v1 = ecl_make_longfloat(p*n - q*n);
		  break;
		}
#endif
		default:
		  (void)0; /*Never reached */
		}
		break;
	case t_ratio:
		switch(type_of(y)) {
		case t_ratio:		/* RAT / RAT */
		  v0 = ecl_ceiling2(ecl_times(x->ratio.num, y->ratio.den),
				    ecl_times(x->ratio.den, y->ratio.num));
		  v1 = ecl_make_ratio(VALUES(1), ecl_times(x->ratio.den, y->ratio.den));
		  break;
		default:		/* RAT / ANY */
		  v0 = ecl_ceiling2(x->ratio.num, ecl_times(x->ratio.den, y));
		  v1 = ecl_divide(VALUES(1), x->ratio.den);
		}
		break;
	case t_singlefloat: {		/* SF / ANY */
		float n = ecl_to_double(y);
		float p = sf(x)/n;
		float q = ceilf(p);
		v0 = float_to_integer(q);
		v1 = ecl_make_singlefloat(p*n - q*n);
		break;
	}
	case t_doublefloat: {		/* DF / ANY */
		double n = ecl_to_double(y);
		double p = df(x)/n;
		double q = ceil(p);
		v0 = double_to_integer(q);
		v1 = ecl_make_doublefloat(p*n - q*n);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {		/* LF / ANY */
		long double n = ecl_to_long_double(y);
		long double p = ecl_long_float(x)/n;
		long double q = ceill(p);
		v0 = long_double_to_integer(q);
		v1 = ecl_make_longfloat(p*n - q*n);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[ceiling], 1, x, @[real]);
	}
	@(return v0 v1)
}

@(defun ceiling (x &optional (y OBJNULL))
@
	if (narg == 1)
		x = ecl_ceiling1(x);
	else
		x = ecl_ceiling2(x, y);
	returnn(x);
@)

cl_object
ecl_truncate1(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		v0 = ecl_truncate2(x->ratio.num, x->ratio.den);
		v1 = ecl_make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = sf(x);
		float y = d > 0? floorf(d) : ceilf(d);
		v0 = float_to_integer(y);
		v1 = ecl_make_singlefloat(d - y);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double y = d > 0? floor(d) : ceil(d);
		v0 = double_to_integer(y);
		v1 = ecl_make_doublefloat(d - y);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double y = d > 0? floorl(d) : ceill(d);
		v0 = long_double_to_integer(y);
		v1 = ecl_make_longfloat(d - y);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[truncate],1,x,@[real]);
	}
	@(return v0 v1)
}

cl_object
ecl_truncate2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	if (ecl_plusp(x) != ecl_plusp(y))
		return ecl_ceiling2(x, y);
	else
		return ecl_floor2(x, y);
}

@(defun truncate (x &optional (y OBJNULL))
@
	if (narg == 1)
		x = ecl_truncate1(x);
	else
		x = ecl_truncate2(x, y);
	returnn(x);
@)

static double
round_double(double d)
{
	if (d >= 0) {
		double q = floor(d += 0.5);
		if (q == d) {
			int i = (int)fmod(q, 10);
			if (i & 1) {
				return q-1;
			}
		}
		return q;
	} else {
		return -round_double(-d);
	}
}

#ifdef ECL_LONG_FLOAT
static long double
round_long_double(long double d)
{
	if (d >= 0) {
		long double q = floorl(d += 0.5);
		if (q == d) {
			int i = (int)fmodl(q, 10);
			if (i & 1) {
				return q-1;
			}
		}
		return q;
	} else {
		return -round_long_double(-d);
	}
}
#endif

cl_object
ecl_round1(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		v0 = ecl_round2(x->ratio.num, x->ratio.den);
		v1 = ecl_make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = sf(x);
		float q = round_double(d);
		v0 = float_to_integer(q);
		v1 = ecl_make_singlefloat(d - q);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double q = round_double(d);
		v0 = double_to_integer(q);
		v1 = ecl_make_doublefloat(d - q);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double q = round_long_double(d);
		v0 = long_double_to_integer(q);
		v1 = ecl_make_longfloat(d - q);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[round],1,x,@[real]);
	}
	@(return v0 v1)
}

cl_object
ecl_round2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_object q;

	q = ecl_divide(x, y);
	switch (type_of(q)) {
	case t_fixnum:
	case t_bignum:
		v0 = q;
		v1 = MAKE_FIXNUM(0);
		break;
	case t_ratio: {
		cl_object q1 = ecl_integer_divide(q->ratio.num, q->ratio.den);
		cl_object r = ecl_minus(q, q1);
		if (ecl_minusp(r)) {
			int c = ecl_number_compare(cl_core.minus_half, r);
			if (c > 0 || (c == 0 && ecl_oddp(q1))) {
				q1 = ecl_one_minus(q1);
			}
		} else {
			int c = ecl_number_compare(r, cl_core.plus_half);
			if (c > 0 || (c == 0 && ecl_oddp(q1))) {
				q1 = ecl_one_plus(q1);
			}
		}
		v0 = q1;
		v1 = number_remainder(x, y, q1);
		break;
	}
	default:
		v0 = q = ecl_round1(q);
		v1 = number_remainder(x, y, q);
	}
	@(return v0 v1)
}

@(defun round (x &optional (y OBJNULL))
@
	if (narg == 1)
		x = ecl_round1(x);
	else
		x = ecl_round2(x, y);
	returnn(x);
@)


cl_object
cl_mod(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	/* INV: #'floor always outputs two values */
	@floor(2, x, y);
	@(return VALUES(1))
}

cl_object
cl_rem(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	@truncate(2, x, y);
	@(return VALUES(1))
}

cl_object
cl_decode_float(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	int e, s;
	cl_type tx = type_of(x);
	float f;

	switch (tx) {
	case t_singlefloat: {
		f = sf(x);
	FLOAT:
		if (f >= 0.0) {
			s = 1;
		} else {
			f = -f;
			s = 0;
		}
		f = frexpf(f, &e);
		x = ecl_make_singlefloat(f);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		if (d >= 0.0) {
			s = 1;
		} else {
			d = -d;
			s = 0;
		}
		d = frexp(d, &e);
		x = ecl_make_doublefloat(d);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		if (d >= 0.0)
			s = 1;
		else {
			d = -d;
			s = 0;
		}
		d = frexpl(d, &e);
		x = ecl_make_longfloat(d);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[decode-float],1,x,@[float]);
	}
	@(return x MAKE_FIXNUM(e) ecl_make_singlefloat(s))
}

cl_object
cl_scale_float(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_fixnum k;

	if (FIXNUMP(y)) {
		k = fix(y);
	} else {
		FEwrong_type_nth_arg(@[scale-float],2,y,@[fixnum]);
	}
	switch (type_of(x)) {
	case t_singlefloat:
		x = ecl_make_singlefloat(ldexpf(sf(x), k));
		break;
	case t_doublefloat:
		x = ecl_make_doublefloat(ldexp(df(x), k));
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		x = ecl_make_longfloat(ldexpl(ecl_long_float(x), k));
		break;
#endif
	default:
                FEwrong_type_nth_arg(@[scale-float],1,x,@[float]);
	}
	@(return x)
}

cl_object
cl_float_radix(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	if (ecl_unlikely(cl_floatp(x) != Ct)) {
		FEwrong_type_nth_arg(@[float-radix],1,x,@[float]);
	}
	@(return MAKE_FIXNUM(FLT_RADIX))
}

int
ecl_signbit(cl_object x)
{
	switch (type_of(x)) {
	case t_singlefloat:
		return signbit(sf(x));
	case t_doublefloat:
		return signbit(df(x));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return signbit(ecl_long_float(x));
#endif
	default:
                FEwrong_type_nth_arg(@[float-sign],1,x,@[float]);
	}
}

@(defun float_sign (x &optional (y x yp))
	int negativep;
@
	if (!yp) {
		y = cl_float(2, MAKE_FIXNUM(1), x);
	}
	negativep = ecl_signbit(x);
	switch (type_of(y)) {
	case t_singlefloat: {
		float f = sf(y);
                if (signbit(f) != negativep) y = ecl_make_singlefloat(-f);
		break;
	}
	case t_doublefloat: {
		double f = df(y);
                if (signbit(f) != negativep) y = ecl_make_doublefloat(-f);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double f = ecl_long_float(y);
                if (signbit(f) != negativep) y = ecl_make_longfloat(-f);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[float-sign],2,y,@[float]);
	}
	@(return y);
@)

cl_object
cl_float_digits(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	switch (type_of(x)) {
	case t_singlefloat:
		x = MAKE_FIXNUM(FLT_MANT_DIG);
		break;
	case t_doublefloat:
		x = MAKE_FIXNUM(DBL_MANT_DIG);
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		x = MAKE_FIXNUM(LDBL_MANT_DIG);
		break;
#endif
	default:
                FEwrong_type_nth_arg(@[float-digits],1,x,@[float]);
	}
	@(return x)
}

cl_object
cl_float_precision(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	int precision;
	switch (type_of(x)) {
	case t_singlefloat: {
		float f = sf(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexpf(f, &exp);
			if (exp >= FLT_MIN_EXP) {
				precision = FLT_MANT_DIG;
			} else {
				precision = FLT_MANT_DIG - (FLT_MIN_EXP - exp);
			}
		}
		break;
	}
	case t_doublefloat: {
		double f = df(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexp(f, &exp);
			if (exp >= DBL_MIN_EXP) {
				precision = DBL_MANT_DIG;
			} else {
				precision = DBL_MANT_DIG - (DBL_MIN_EXP - exp);
			}
		}
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double f = ecl_long_float(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexp(f, &exp);
			if (exp >= LDBL_MIN_EXP) {
				precision = LDBL_MANT_DIG;
			} else {
				precision = LDBL_MANT_DIG - (LDBL_MIN_EXP - exp);
			}
		}
		break;
	}
#endif
	default:
		FEwrong_type_nth_arg(@[float-precision],1,x,@[float]);
	}
	@(return MAKE_FIXNUM(precision))
}

cl_object
cl_integer_decode_float(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	int e, s = 1;

	switch (type_of(x)) {
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
                if (signbit(d)) {
                        s = -1;
                        d = -d;
                }
		if (d == 0.0) {
			e = 0;
			x = MAKE_FIXNUM(0);
		} else {
                        d = frexpl(d, &e);
			x = long_double_to_integer(ldexpl(d, LDBL_MANT_DIG));
			e -= LDBL_MANT_DIG;
		}
		break;
	}
#endif
	case t_doublefloat: {
		double d = df(x);
                if (signbit(d)) {
                        s = -1;
                        d = -d;
                }
		if (d == 0.0) {
			e = 0;
			x = MAKE_FIXNUM(0);
		} else {
                        d = frexp(d, &e);
			x = double_to_integer(ldexp(d, DBL_MANT_DIG));
			e -= DBL_MANT_DIG;
		}
		break;
	}
	case t_singlefloat: {
		float d = sf(x);
                if (signbit(d)) {
                        s = -1;
                        d = -d;
                }
		if (d == 0.0) {
			e = 0;
			x = MAKE_FIXNUM(0);
		} else {
                        d = frexpf(d, &e);
			x = double_to_integer(ldexp(d, FLT_MANT_DIG));
			e -= FLT_MANT_DIG;
		}
		break;
	}
	default:
		FEwrong_type_nth_arg(@[integer-decode-float],1,x,@[float]);
	}
	@(return x MAKE_FIXNUM(e) MAKE_FIXNUM(s))
}


@(defun complex (r &optional (i MAKE_FIXNUM(0)))
@	/* INV: ecl_make_complex() checks types */
	@(return ecl_make_complex(r, i))
@)

cl_object
cl_realpart(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		break;
	case t_complex:
		x = x->complex.real;
		break;
	default:
		FEwrong_type_nth_arg(@[realpart],1,x,@[number]);
	}
	@(return x)
}

cl_object
cl_imagpart(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		x = MAKE_FIXNUM(0);
		break;
	case t_singlefloat:
                if (signbit(sf(x)))
                        x = cl_core.singlefloat_minus_zero;
                else
                        x = cl_core.singlefloat_zero;
		break;
	case t_doublefloat:
                if (signbit(df(x)))
                        x = cl_core.doublefloat_minus_zero;
                else
                        x = cl_core.doublefloat_zero;
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
                if (signbit(ecl_long_float(x)))
                        x = cl_core.longfloat_minus_zero;
                else
                        x = cl_core.longfloat_zero;
		break;
#endif
	case t_complex:
		x = x->complex.imag;
		break;
	default:
                FEwrong_type_nth_arg(@[imagpart],1,x,@[number]);
	}
	@(return x)
}
