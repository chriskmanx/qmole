/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    init.c  -- Lisp Initialization.
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

#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

/*
 * HOOKS.
 *
 * The following functions are only used to bootstrap ECL. They divert
 * the calls to the interpreted code which is loaded by bare.lsp. Once
 * the whole of ECL is built, the file cinit.o will be replaced by the
 * actual initialization code, and the compiled function will be
 * called instead.
 */

extern cl_object
cl_upgraded_array_element_type(cl_narg narg, cl_object type, ...)
{
	return cl_funcall(2, @'upgraded-array-element-type', type);
}

extern cl_object
si_safe_eval(cl_narg narg, cl_object form, cl_object env, ...)
{
        if (narg == 3) {
                cl_object err_value;
                va_list args; va_start(args, env);
                err_value = va_arg(args, cl_object);
                return cl_funcall(4, @'ext::safe-eval', form, env, err_value);
        }
        return cl_funcall(3, @'ext::safe-eval', form, env);
}

extern cl_object
si_string_to_object(cl_narg narg, cl_object string, ...)
{
        if (narg == 2) {
                cl_object err_value;
                va_list args; va_start(args, string);
                err_value = va_arg(args, cl_object);
                return cl_funcall(3, @'si::string-to-object', string, err_value);
        }
        return cl_funcall(2, @'si::string-to-object', string);
}

extern cl_object
si_signal_simple_error(cl_narg narg, cl_object condition, cl_object continuable, cl_object format, cl_object format_args, ...)
{
	cl_va_list args;
	cl_va_start(args, format_args, narg, 4);
	return cl_apply(6, @'si::signal-simple-error', condition, continuable, format, format_args, cl_grab_rest_args(args));
}

extern cl_object
cl_set_difference(cl_narg narg, cl_object l1, cl_object l2, ...)
{
        @(return l1)
}

extern cl_object
cl_array_dimensions(cl_object array)
{
	return funcall(2, @'ARRAY-DIMENSIONS', array);
}

extern cl_object
cl_vector_push_extend(cl_narg narg, cl_object elt, cl_object vector, ...)
{
	cl_index fp;
	if (narg != 2) {
		FEerror("Too many arguments to interim cl_vector_push_extend (cinit.d)", 0);
	}
	fp = vector->vector.fillp;
	if (fp < vector->vector.dim) {
		vector->vector.fillp = fp+1;
		vector->vector.self.t[fp+1] = elt;
		@(return MAKE_FIXNUM(fp))
	}
	return funcall(3, @'VECTOR-PUSH-EXTEND', elt, vector);
}

extern cl_object
si_find_relative_package(cl_narg narg, cl_object package, ...)
{
	@(return Cnil);
}

static cl_object si_simple_toplevel ()
{
        cl_env_ptr env = ecl_process_env();
	cl_object output = cl_core.standard_output;
	cl_object sentence;
	int i;

	/* Simple minded top level loop */
        CL_CATCH_ALL_BEGIN(env) {
                writestr_stream(";*** Lisp core booted ****\n"
                                "ECL (Embeddable Common Lisp)\n",
                                output);
                ecl_force_output(output);
                for (i = 1; i<fix(si_argc()); i++) {
                        cl_object arg = si_argv(MAKE_FIXNUM(i));
                        cl_load(1, arg);
                }
                while (1) {
                        writestr_stream("\n> ", output);
                        sentence = @read(3, Cnil, Cnil, OBJNULL);
                        if (sentence == OBJNULL)
                                @(return);
                        ecl_prin1(si_eval_with_env(1, sentence), output);
                }
        } CL_CATCH_ALL_END;
}

int
main(int argc, char **args)
{
	cl_object top_level, features;

	/* This should be always the first call */
	cl_boot(argc, args);

	/* We are computing unnormalized numbers at some point */
	si_trap_fpe(Ct, Cnil);

#ifdef ECL_CMU_FORMAT
	ECL_SET(@'*load-verbose*', Cnil);
#endif
	ECL_SET(@'*package*', cl_core.system_package);

	features = ecl_symbol_value(@'*features*');
	features = CONS(ecl_make_keyword("ECL-MIN"), features);
#ifdef HAVE_UNAME
	features = CONS(ecl_make_keyword("UNAME"), features);
#endif
	ECL_SET(@'*features*', features);
	top_level = _ecl_intern("TOP-LEVEL", cl_core.system_package);
	ecl_def_c_function(top_level, si_simple_toplevel, 0);
	funcall(1, top_level);
	return(0);
}

#ifdef __cplusplus
extern "C" void init_lib_LSP(cl_object);
#endif

void init_lib_LSP(cl_object o) {}
