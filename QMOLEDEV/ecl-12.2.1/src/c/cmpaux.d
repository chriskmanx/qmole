/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cmpaux.c -- Auxiliaries used in compiled Lisp code.
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

#include <limits.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

cl_object
si_specialp(cl_object sym)
{
	@(return ((ecl_symbol_type(sym) & stp_special)? Ct : Cnil))
}

cl_fixnum
ecl_ifloor(cl_fixnum x, cl_fixnum y)
{
	if (y == 0)
		FEerror("Zero divizor", 0);
	else if (y > 0)
		if (x >= 0)
			return(x/y);
		else
			return(-((-x+y-1))/y);
	else
		if (x >= 0)
			return(-((x-y-1)/(-y)));
		else
			return((-x)/(-y));
}

cl_fixnum
ecl_imod(cl_fixnum x, cl_fixnum y)
{
	return(x - ecl_ifloor(x, y)*y);
}

/*
 * ----------------------------------------------------------------------
 *	Conversions to C
 * ----------------------------------------------------------------------
 */

char
ecl_to_char(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
		return fix(x);
	case t_character:
		return CHAR_CODE(x);
	default:
		FEerror("~S cannot be coerced to a C char.", 1, x);
	}
}

cl_fixnum
ecl_to_fixnum(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		return fixint(x);
/*	case t_character: return (cl_fixnum)CHAR_CODE(x); */
	case t_ratio:
		return (cl_fixnum)ecl_to_double(x);
	case t_singlefloat:
		return (cl_fixnum)sf(x);
	case t_doublefloat:
		return (cl_fixnum)df(x);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (cl_fixnum)ecl_long_float(x);
#endif
	default:
		FEerror("~S cannot be coerced to a C int.", 1, x);
	}
}

cl_index
ecl_to_unsigned_integer(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		return fixnnint(x);
	case t_ratio:
		return (cl_index)ecl_to_double(x);
	case t_singlefloat:
		return (cl_index)sf(x);
	case t_doublefloat:
		return (cl_index)df(x);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (cl_index)ecl_long_float(x);
#endif
	default:
		FEerror("~S cannot be coerced to a C unsigned int.", 1, x);
	}
}

float
ecl_to_float(cl_object x)
{
	if (FIXNUMP(x)) return(fix(x));	/* Immediate fixnum */

	switch (type_of(x)) {
/*	case t_fixnum: return fix(x);	*/
/*	case t_character: return CHAR_CODE(x); */
	case t_bignum:
	case t_ratio:
		return ecl_to_double(x);
	case t_singlefloat:
		return sf(x);
	case t_doublefloat:
		return df(x);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return ecl_long_float(x);
#endif
	default:
                FEwrong_type_nth_arg(@[coerce], 1, x, @[real]);
	}
}

int
ecl_aref_bv(cl_object x, cl_index index)
{
  index += x->vector.offset;
  return ((x->vector.self.bit[index/CHAR_BIT] & (0200>>index%CHAR_BIT)) != 0);
}

int
ecl_aset_bv(cl_object x, cl_index index, int value)
{
  index += x->vector.offset;
  if (value == 0)
    x->vector.self.bit[index/CHAR_BIT] &= ~(0200>>index%CHAR_BIT);
  else
    x->vector.self.bit[index/CHAR_BIT] |= 0200>>index%CHAR_BIT;
  return value;
}

void
cl_throw(cl_object tag)
{
	ecl_frame_ptr fr = frs_sch(tag);
	if (fr == NULL)
		FEcontrol_error("THROW: The catch ~S is undefined.", 1, tag);
	ecl_unwind(ecl_process_env(), fr);
}

void
cl_return_from(cl_object block_id, cl_object block_name)
{
	ecl_frame_ptr fr = frs_sch(block_id);
	if (fr == NULL)
		FEcontrol_error("RETURN-FROM: The block ~S with id ~S is missing.",
				2, block_name, block_id);
	ecl_unwind(ecl_process_env(), fr);
}

void
cl_go(cl_object tag_id, cl_object label)
{
	ecl_frame_ptr fr = frs_sch(tag_id);
	if (fr == NULL)
		FEcontrol_error("GO: The tagbody ~S is missing.", 1, tag_id);
	VALUES(0)=label;
	NVALUES=1;
	ecl_unwind(ecl_process_env(), fr);
}

cl_object
cl_grab_rest_args(cl_va_list args)
{
	cl_object rest = Cnil;
	cl_object *r = &rest;
	while (args[0].narg) {
		*r = ecl_list1(cl_va_arg(args));
		r = &ECL_CONS_CDR(*r);
	}
	return rest;
}

void
cl_parse_key(
     cl_va_list args,		/* actual args */
     int nkey,			/* number of keywords */
     cl_object *keys,		/* keywords for the function */
     cl_object *vars,		/* where to put values (vars[0..nkey-1])
				   and suppliedp (vars[nkey..2*nkey-1]) */
     cl_object *rest,		/* if rest != NULL, where to collect rest values */
     bool allow_other_keys)	/* whether other key are allowed */
{
	int i;
	cl_object supplied_allow_other_keys = OBJNULL;
	cl_object unknown_keyword = OBJNULL;

	if (rest != NULL) *rest = Cnil;

	for (i = 0; i < 2*nkey; i++)
		vars[i] = Cnil;             /* default values: NIL, supplied: NIL */
	if (args[0].narg <= 0) return;

	for (; args[0].narg > 1; ) {
		cl_object keyword = cl_va_arg(args);
		cl_object value = cl_va_arg(args);
		if (ecl_unlikely(!SYMBOLP(keyword)))
			FEprogram_error_noreturn("LAMBDA: Keyword expected, got ~S.",
                                                 1, keyword);
		if (rest != NULL) {
			rest = &ECL_CONS_CDR(*rest = ecl_list1(keyword));
			rest = &ECL_CONS_CDR(*rest = ecl_list1(value));
		}
		for (i = 0; i < nkey; i++) {
			if (keys[i] == keyword) {
				if (vars[nkey+i] == Cnil) {
					vars[i] = value;
					vars[nkey+i] = Ct;
				}
				goto goon;
			}
		}
		/* the key is a new one */
		if (keyword == @':allow-other-keys') {
			if (supplied_allow_other_keys == OBJNULL)
				supplied_allow_other_keys = value;
		} else if (unknown_keyword == OBJNULL)
			unknown_keyword = keyword;
	goon:;
	}
	if (ecl_unlikely(args[0].narg != 0))
		FEprogram_error_noreturn("Odd number of keys", 0);
	if (ecl_unlikely(unknown_keyword != OBJNULL && !allow_other_keys &&
                         (supplied_allow_other_keys == Cnil ||
                          supplied_allow_other_keys == OBJNULL)))
		FEprogram_error("Unknown keyword ~S", 1, unknown_keyword);
}
