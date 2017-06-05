/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    gfun.c -- Dispatch for generic functions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/cache.h>

static cl_object generic_function_dispatch_vararg(cl_narg, ...);

cl_object
FEnot_funcallable_fixed()
{
        cl_env_ptr env = ecl_process_env();
        cl_object fun = env->function;
        FEerror("Not a funcallable instance ~A.", 1, fun);
        @(return);
}

cl_object
FEnot_funcallable_vararg(cl_narg narg, ...)
{
        return FEnot_funcallable_fixed();
}

static cl_object
user_function_dispatch(cl_narg narg, ...)
{
        int i;
        cl_object output;
	cl_env_ptr env = ecl_process_env();
	cl_object fun = env->function;
	struct ecl_stack_frame frame_aux;
	const cl_object frame = ecl_stack_frame_open(env, (cl_object)&frame_aux, narg);
        cl_va_list args; cl_va_start(args, narg, narg, 0);
        for (i = 0; i < narg; i++) {
                ECL_STACK_FRAME_SET(frame, i, cl_va_arg(args));
        }
        fun = fun->instance.slots[fun->instance.length - 1];
        output = ecl_apply_from_stack_frame(frame, fun);
        ecl_stack_frame_close(frame);
        return output;
}

static void
reshape_instance(cl_object x, int delta)
{
	cl_fixnum size = x->instance.length + delta;
	cl_object aux = ecl_allocate_instance(CLASS_OF(x), size);
        /* Except for the different size, this must match si_copy_instance */
        aux->instance.sig = x->instance.sig;
	memcpy(aux->instance.slots, x->instance.slots,
	       (delta < 0 ? aux->instance.length : x->instance.length) *
	       sizeof(cl_object));
	x->instance = aux->instance;
}

cl_object
clos_set_funcallable_instance_function(cl_object x, cl_object function_or_t)
{
	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[clos::set-funcallable-instance-function],
                                     1, x, @[ext::instance]);
	if (x->instance.isgf == ECL_USER_DISPATCH) {
		reshape_instance(x, -1);
		x->instance.isgf = ECL_NOT_FUNCALLABLE;
	}
	if (function_or_t == Ct) {
		x->instance.isgf = ECL_STANDARD_DISPATCH;
                x->instance.entry = generic_function_dispatch_vararg;
	} else if (function_or_t == Cnil) {
		x->instance.isgf = ECL_NOT_FUNCALLABLE;
                x->instance.entry = FEnot_funcallable_vararg;
	} else if (function_or_t == @'clos::standard-reader-method') {
		x->instance.isgf = ECL_READER_DISPATCH;
		x->instance.entry = ecl_slot_reader_dispatch;
	} else if (function_or_t == @'clos::standard-writer-method') {
		x->instance.isgf = ECL_WRITER_DISPATCH;
		x->instance.entry = ecl_slot_writer_dispatch;
	} else if (Null(cl_functionp(function_or_t))) {
		FEwrong_type_argument(@'function', function_or_t);
	} else {
		reshape_instance(x, +1);
		x->instance.slots[x->instance.length - 1] = function_or_t;
		x->instance.isgf = ECL_USER_DISPATCH;
                x->instance.entry = user_function_dispatch;
	}
	@(return x)
}

cl_object
si_generic_function_p(cl_object x)
{
	@(return ((ECL_INSTANCEP(x) && (x->instance.isgf))? Ct : Cnil))
}

static cl_object
fill_spec_vector(cl_object vector, cl_object frame, cl_object gf)
{
	cl_object *args = frame->frame.base;
	cl_index narg = frame->frame.size;
	cl_object spec_how_list = GFUN_SPEC(gf);
	cl_object *argtype = vector->vector.self.t;
	int spec_no = 1;
	argtype[0] = gf;
	loop_for_on_unsafe(spec_how_list) {
		cl_object spec_how = ECL_CONS_CAR(spec_how_list);
		cl_object spec_type = ECL_CONS_CAR(spec_how);
		int spec_position = fix(ECL_CONS_CDR(spec_how));
		if (spec_position >= narg)
			FEwrong_num_arguments(gf);
		argtype[spec_no++] =
			(ATOM(spec_type) ||
			 Null(ecl_memql(args[spec_position], spec_type))) ?
			cl_class_of(args[spec_position]) :
			args[spec_position];
		if (spec_no > vector->vector.dim)
			return OBJNULL;
	} end_loop_for_on_unsafe(spec_how_list);
	vector->vector.fillp = spec_no;
	return vector;
}

static cl_object
compute_applicable_method(cl_object frame, cl_object gf)
{
	/* method not cached */
	cl_object methods, arglist, func;
	cl_object *p;
	for (p = frame->frame.base + frame->frame.size, arglist = Cnil;
             p != frame->frame.base; ) {
		arglist = CONS(*(--p), arglist);
	}
	methods = _ecl_funcall3(@'compute-applicable-methods', gf, arglist);
	if (methods == Cnil) {
		func = _ecl_funcall3(@'no-applicable-method', gf, arglist);
		frame->frame.base[0] = OBJNULL;
		return func;
	} else {
		return _ecl_funcall4(@'clos::compute-effective-method', gf,
				     GFUN_COMB(gf), methods);
	}
}

cl_object
_ecl_standard_dispatch(cl_object frame, cl_object gf)
{
	cl_object func, vector;
        const cl_env_ptr env = frame->frame.env;
	ecl_cache_ptr cache = env->method_cache;
	/*
	 * We have to copy the frame because it might be stored in cl_env.values
	 * which will be wiped out by the next function call. However this only
	 * happens when we cannot reuse the values in the C stack.
	 */
#if !defined(ECL_USE_VARARG_AS_POINTER)
	struct ecl_stack_frame frame_aux;
	if (frame->frame.stack == (void*)0x1) {
                const cl_object new_frame = (cl_object)&frame_aux;
                ECL_STACK_FRAME_COPY(new_frame, frame);
                frame = new_frame;
	}
#endif
	
	vector = fill_spec_vector(cache->keys, frame, gf);
	if (vector == OBJNULL) {
		func = compute_applicable_method(frame, gf);
	} else {
		ecl_cache_record_ptr e = ecl_search_cache(cache);
		if (e->key != OBJNULL) {
			func = e->value;
		} else {
			/* The keys and the cache may change while we
			 * compute the applicable methods. We must save
			 * the keys and recompute the cache location if
			 * it was filled. */
			cl_object keys = cl_copy_seq(vector);
			func = compute_applicable_method(frame, gf);
			if (e->key != OBJNULL) {
				e = ecl_search_cache(cache);
			}
			e->key = keys;
			e->value = func;
		}
	}
	func = _ecl_funcall3(func, frame, Cnil);
	/* Only need to close the copy */
#if !defined(ECL_USE_VARARG_AS_POINTER)
	if (frame == (cl_object)&frame_aux)
		ecl_stack_frame_close(frame);
#endif
	return func;
}

static cl_object
generic_function_dispatch_vararg(cl_narg narg, ...)
{
        cl_object output;
        ECL_STACK_FRAME_VARARGS_BEGIN(narg, narg, frame);
	output = _ecl_standard_dispatch(frame, frame->frame.env->function);
        ECL_STACK_FRAME_VARARGS_END(frame);
        return output;
}


cl_object
si_clear_gfun_hash(cl_object what)
{
	/*
	 * This function clears the generic function call hashes selectively.
	 *	what = Ct means clear the hash completely
	 *	what = generic function, means cleans only these entries
	 * If we work on a multithreaded environment, we simply enqueue these
	 * operations and wait for the destination thread to update its own hash.
	 */
	cl_env_ptr the_env = ecl_process_env();
#ifdef ECL_THREADS
	cl_object list;
	for (list = mp_all_processes(); !Null(list); list = ECL_CONS_CDR(list)) {
		cl_object process = ECL_CONS_CAR(list);
		struct cl_env_struct *env = process->process.env;
		if (the_env != env) {
			ecl_cache_remove_one(env->method_cache, what);
			ecl_cache_remove_one(env->slot_cache, what);
		}
	}
#endif
	ecl_cache_remove_one(the_env->method_cache, what);
	ecl_cache_remove_one(the_env->slot_cache, what);
        return0();
}
