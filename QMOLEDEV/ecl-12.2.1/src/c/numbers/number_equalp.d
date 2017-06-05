/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    number_compare.c  -- number comparison and sorting.
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

#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>
#include "numbers/float_fix_compare.d"

@(defun = (num &rest nums)
	int i;
@
	/* ANSI: Need not signal error for 1 argument */
	/* INV: For >= 2 arguments, ecl_number_equalp() performs checks */
	for (i = 1; i < narg; i++)
		if (!ecl_number_equalp(num, cl_va_arg(nums)))
			@(return Cnil)
	@(return Ct)
@)

/* Returns 1 if both numbers compare to equal */
int
ecl_number_equalp(cl_object x, cl_object y)
{
	double dx;
	/* INV: (= fixnum bignum) => 0 */
	/* INV: (= fixnum ratio) => 0 */
	/* INV: (= bignum ratio) => 0 */
 BEGIN:
	switch (type_of(x)) {
	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum:
		  	return x == y;
		case t_bignum:
		case t_ratio:
			return 0;
		case t_singlefloat:
			return double_fix_compare(fix(x), ecl_single_float(y)) == 0;
		case t_doublefloat:
			return double_fix_compare(fix(x), ecl_double_float(y)) == 0;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return long_double_fix_compare(fix(x), ecl_long_float(y)) == 0;
#endif
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEwrong_type_nth_arg(@[=], 2, y, @[number]);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			return 0;
		case t_bignum:
			return _ecl_big_compare(x, y)==0;
		case t_ratio:
			return 0;
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEwrong_type_nth_arg(@[=], 2, y, @[number]);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			return 0;
		case t_ratio:
			return (ecl_number_equalp(x->ratio.num, y->ratio.num) &&
				ecl_number_equalp(x->ratio.den, y->ratio.den));
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEwrong_type_nth_arg(@[=], 2, y, @[number]);
		}
	case t_singlefloat:
		dx = ecl_single_float(x);
		goto FLOAT;
	case t_doublefloat:
		dx = ecl_double_float(x);
	FLOAT:
		switch (type_of(y)) {
		case t_fixnum:
			return double_fix_compare(fix(y), dx) == 0;
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
		case t_singlefloat:
			return dx == ecl_single_float(y);
		case t_doublefloat:
			return dx == ecl_double_float(y);
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return dx == ecl_long_float(y);
#endif
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEwrong_type_nth_arg(@[=], 2, y, @[number]);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double dx = ecl_long_float(x);
		switch (type_of(y)) {
		case t_fixnum:
			return long_double_fix_compare(fix(y), dx) == 0;
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
		case t_singlefloat:
			return dx == ecl_single_float(y);
		case t_doublefloat:
			return dx == ecl_double_float(y);
		case t_longfloat:
			return dx == ecl_long_float(y);
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEwrong_type_nth_arg(@[=], 2, y, @[number]);
		}
	}
#endif
	Y_COMPLEX:
		if (!ecl_zerop(y->complex.imag))
			return 0;
		return ecl_number_equalp(x, y->complex.real);
	case t_complex:
                switch (type_of(y)) {
                case t_complex:
			return (ecl_number_equalp(x->complex.real, y->complex.real) &&
				ecl_number_equalp(x->complex.imag, y->complex.imag));
                case t_fixnum: case t_bignum: case t_ratio:
                case t_singlefloat: case t_doublefloat:
#ifdef ECL_LONG_FLOAT
                case t_longfloat:
#endif
			if (ecl_zerop(x->complex.imag))
				return ecl_number_equalp(x->complex.real, y) != 0;
			else
				return 0;
                default:
                        FEwrong_type_nth_arg(@[=], 2, y, @[number]);
                }
	default:
                FEwrong_type_nth_arg(@[=], 1, x, @[number]);
	}
}

@(defun /= (&rest nums &aux numi)
	int i, j;
@
	if (narg == 0)
		FEwrong_num_arguments_anonym();
	numi = cl_va_arg(nums);
	for (i = 2; i<=narg; i++) {
		cl_va_list numb;
		cl_va_start(numb, narg, narg, 0);
		numi = cl_va_arg(nums);
		for (j = 1; j<i; j++)
			if (ecl_number_equalp(numi, cl_va_arg(numb)))
				@(return Cnil)
	}
	@(return Ct)
@)
