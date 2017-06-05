/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_arith.c  -- Arithmetic operations
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/number.h>
#include <stdlib.h>

cl_object
ecl_integer_divide(cl_object x, cl_object y)
{
	cl_type tx, ty;

	tx = type_of(x);
	ty = type_of(y);
	if (tx == t_fixnum) {
 		if (ty == t_fixnum) {
			if (y == MAKE_FIXNUM(0))
				FEdivision_by_zero(x, y);
			return MAKE_FIXNUM(fix(x) / fix(y));
		} else if (ty == t_bignum) {
                        return _ecl_fix_divided_by_big(fix(x), y);
		} else {
                        FEwrong_type_nth_arg(@[round], 2, y, @[integer]);
                }
	}
	if (tx == t_bignum) {
		if (ty == t_bignum) {
			return _ecl_big_divided_by_big(x, y);
		} else if (ty == t_fixnum) {
                        return _ecl_big_divided_by_fix(x, fix(y));
		} else {
                        FEwrong_type_nth_arg(@[round], 2, y, @[integer]);
		}
	}
        FEwrong_type_nth_arg(@[round], 1, x, @[integer]);
}

@(defun gcd (&rest nums)
	cl_object gcd;
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: ecl_gcd() checks types */
	gcd = cl_va_arg(nums);
	if (narg == 1) {
		assert_type_integer(gcd);
		@(return (ecl_minusp(gcd) ? ecl_negate(gcd) : gcd))
	}
	while (--narg)
		gcd = ecl_gcd(gcd, cl_va_arg(nums));
	@(return gcd)
@)

cl_object
ecl_gcd(cl_object x, cl_object y)
{
	cl_object gcd;
        ECL_WITH_TEMP_BIGNUM(x_big,1);
        ECL_WITH_TEMP_BIGNUM(y_big,1);

	switch (type_of(x)) {
	case t_fixnum:
                _ecl_big_set_fixnum(x_big, fix(x));
                x = x_big;
	case t_bignum:
		break;
	default:
		FEwrong_type_nth_arg(@[gcd], 1, x, @[integer]);
	}
	switch (type_of(y)) {
	case t_fixnum:
                _ecl_big_set_fixnum(y_big, fix(y));
                y = y_big;
	case t_bignum:
                break;
	default:
		FEwrong_type_nth_arg(@[gcd], 2, y, @[integer]);
        }
        return _ecl_big_gcd(x, y);
}

@(defun lcm (&rest nums)
	cl_object lcm;
@
	if (narg == 0)
		@(return MAKE_FIXNUM(1))
	/* INV: ecl_gcd() checks types. By placing `numi' before `lcm' in
	   this call, we make sure that errors point to `numi' */
	lcm = cl_va_arg(nums);
	assert_type_integer(lcm);
	while (narg-- > 1) {
		cl_object numi = cl_va_arg(nums);
		cl_object t = ecl_times(lcm, numi);
		cl_object g = ecl_gcd(numi, lcm);
		if (g != MAKE_FIXNUM(0))
			lcm = ecl_divide(t, g);
	}
	@(return (ecl_minusp(lcm) ? ecl_negate(lcm) : lcm))
@)
