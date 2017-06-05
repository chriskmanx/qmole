/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    assignment.c  -- Assignment.
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

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

cl_object
cl_set(cl_object var, cl_object val)
{
	const cl_env_ptr env = ecl_process_env();
	if (ecl_symbol_type(var) & stp_constant)
		FEinvalid_variable("Cannot assign to the constant ~S.", var);
	return1(ECL_SETQ(env, var, val));
}

static cl_object
unbound_setf_function_error(cl_narg narg, ...)
{
	const cl_env_ptr the_env = ecl_process_env();
        cl_object name = the_env->function->cclosure.env;
	FEundefined_function(cl_list(2, @'setf', name));
}

static cl_object
make_setf_function_error(cl_object name)
{
	return ecl_make_cclosure_va((cl_objectfn)unbound_setf_function_error,
				    name, Cnil);
}

cl_object
ecl_setf_definition(cl_object sym, cl_object createp)
{
	cl_env_ptr the_env = ecl_process_env();
	cl_object pair;
        ECL_WITH_GLOBAL_ENV_RDLOCK_BEGIN(the_env) {
                pair = ecl_gethash_safe(sym, cl_core.setf_definitions, Cnil);
		if (Null(pair) && !Null(createp)) {
			createp = make_setf_function_error(sym);
			pair = ecl_cons(createp, Cnil);
			ecl_sethash(sym, cl_core.setf_definitions, pair);
		}
        } ECL_WITH_GLOBAL_ENV_RDLOCK_END;
	return pair;
}

cl_object
si_setf_definition(cl_object sym, cl_object value)
{
	@(return ecl_setf_definition(sym, value))
}

static void
ecl_rem_setf_definition(cl_object sym)
{
	cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(the_env) {
                cl_object pair = ecl_gethash_safe(sym, cl_core.setf_definitions, Cnil);
		if (!Null(pair)) {
			ECL_RPLACA(pair, Cnil);
			ecl_remhash(sym, cl_core.setf_definitions);
		}
        } ECL_WITH_GLOBAL_ENV_WRLOCK_END;
}

@(defun si::fset (fname def &optional macro pprint)
	cl_object sym = si_function_block_name(fname);
	cl_object pack;
	bool mflag;
	int type;
@
	if (Null(cl_functionp(def)))
		FEinvalid_function(def);
	pack = ecl_symbol_package(sym);
	if (pack != Cnil && pack->pack.locked) {
		CEpackage_error("Attempt to redefine function ~S in locked package.",
				"Ignore lock and proceed", pack, 1, fname);
	}
	mflag = !Null(macro);
	type = ecl_symbol_type(sym);
	if ((type & stp_special_form) && !mflag) {
		FEerror("Given that ~S is a special form, ~S cannot be defined as a function.",
			2, sym, fname);
	}
	if (SYMBOLP(fname)) {
		if (mflag) {
			type |= stp_macro;
		} else {
			type &= ~stp_macro;
		}
		ecl_symbol_type_set(sym, type);
		SYM_FUN(sym) = def;
		ecl_clear_compiler_properties(sym);
#ifndef ECL_CMU_FORMAT
		if (pprint == Cnil)
			si_rem_sysprop(sym, @'si::pretty-print-format');
		else
			si_put_sysprop(sym, @'si::pretty-print-format', pprint);
#endif
	} else if (mflag) {
		FEerror("~S is not a valid name for a macro.", 1, fname);
	} else {
		cl_object pair = ecl_setf_definition(sym, def);
		ECL_RPLACA(pair, def);
		ECL_RPLACD(pair, sym);
	}
	@(return def)
@)

cl_object
cl_makunbound(cl_object sym)
{
	if (ecl_symbol_type(sym) & stp_constant)
		FEinvalid_variable("Cannot unbind the constant ~S.", sym);
	/* FIXME! The semantics of MAKUNBOUND is not very clear with local
	   bindings ... */
	ECL_SET(sym, OBJNULL);
	@(return sym)
}

cl_object
cl_fmakunbound(cl_object fname)
{
	cl_object sym = si_function_block_name(fname);
	cl_object pack = ecl_symbol_package(sym);
	if (pack != Cnil && pack->pack.locked) {
		CEpackage_error("Attempt to redefine function ~S in locked package.",
				"Ignore lock and proceed", pack, 1, fname);
	}
	if (SYMBOLP(fname)) {
		ecl_clear_compiler_properties(sym);
		SYM_FUN(sym) = Cnil;
		ecl_symbol_type_set(sym, ecl_symbol_type(sym) & ~stp_macro);
	} else {
		ecl_rem_setf_definition(sym);
		si_rem_sysprop(sym, @'si::setf-method');
	}
	@(return fname)
}

void
ecl_clear_compiler_properties(cl_object sym)
{
	if (ecl_get_option(ECL_OPT_BOOTED)) {
		funcall(2, @'si::clear-compiler-properties', sym);
	}
}

cl_object
si_get_sysprop(cl_object sym, cl_object prop)
{
	cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_ENV_RDLOCK_BEGIN(the_env) {
                cl_object plist = ecl_gethash_safe(sym, cl_core.system_properties, Cnil);
                prop = ecl_getf(plist, prop, OBJNULL);
        } ECL_WITH_GLOBAL_ENV_RDLOCK_END;
        if (prop == OBJNULL) {
                @(return Cnil Cnil);
        } else {
                @(return prop Ct);
        }
}

cl_object
si_put_sysprop(cl_object sym, cl_object prop, cl_object value)
{
	cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(the_env) {
                cl_object plist = ecl_gethash_safe(sym, cl_core.system_properties, Cnil);
                ecl_sethash(sym, cl_core.system_properties, si_put_f(plist, value, prop));
        } ECL_WITH_GLOBAL_ENV_WRLOCK_END;
	@(return value);
}

cl_object
si_rem_sysprop(cl_object sym, cl_object prop)
{
	cl_object plist, found;
	plist = ecl_gethash_safe(sym, cl_core.system_properties, Cnil);
	plist = si_rem_f(plist, prop);
	found = VALUES(1);
	ecl_sethash(sym, cl_core.system_properties, plist);
	@(return found);
}
