/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    main.c --
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

/******************************** IMPORTS *****************************/

#include <ecl/ecl.h>
#include <limits.h>
#if defined(ECL_MS_WINDOWS_HOST)
# include <windows.h>
# include <shellapi.h>
# define MAXPATHLEN 512
#endif
#ifndef MAXPATHLEN
# ifdef PATH_MAX
#   define MAXPATHLEN PATH_MAX
# else
#   define NO_PATH_MAX
#   include <unistd.h>
# endif
#endif
#ifdef ECL_USE_MPROTECT
# include <sys/mman.h>
# ifndef MAP_FAILED
#  define MAP_FAILED -1
# endif
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ecl/cache.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>
extern int GC_dont_gc;

#include "ecl_features.h"
#include "iso_latin_names.h"

/******************************* EXPORTS ******************************/

#if !defined(ECL_THREADS)
cl_env_ptr cl_env_p = NULL;
#elif defined(WITH___THREAD)
__thread cl_env_ptr cl_env_p = NULL;
#endif
const char *ecl_self;

/************************ GLOBAL INITIALIZATION ***********************/

static int ARGC;
static char **ARGV;
static cl_fixnum option_values[ECL_OPT_LIMIT+1] = {
#ifdef GBC_BOEHM_GENGC
	1,		/* ECL_OPT_INCREMENTAL_GC */
#else
	0,		/* ECL_OPT_INCREMENTAL_GC */
#endif
	1,		/* ECL_OPT_TRAP_SIGSEGV */
	1,		/* ECL_OPT_TRAP_SIGFPE */
	1,		/* ECL_OPT_TRAP_SIGINT */
	1,		/* ECL_OPT_TRAP_SIGILL */
	1,		/* ECL_OPT_TRAP_SIGBUS */
	1,		/* ECL_OPT_TRAP_SIGPIPE */
	1,		/* ECL_OPT_TRAP_SIGCHLD */
	1,		/* ECL_OPT_TRAP_INTERRUPT_SIGNAL */
	1,		/* ECL_OPT_SIGNAL_HANDLING_THREAD */
	128,		/* ECL_OPT_SIGNAL_QUEUE_SIZE */
	0,		/* ECL_OPT_BOOTED */
	8192,		/* ECL_OPT_BIND_STACK_SIZE */
	128,		/* ECL_OPT_BIND_STACK_SAFETY_AREA */
	2048,		/* ECL_OPT_FRAME_STACK_SIZE */
	128,		/* ECL_OPT_FRAME_STACK_SAFETY_AREA */
	32768,		/* ECL_OPT_LISP_STACK_SIZE */
	128,		/* ECL_OPT_LISP_STACK_SAFETY_AREA */
	128*sizeof(cl_index)*1024, /* ECL_OPT_C_STACK_SIZE */
	4*sizeof(cl_index)*1024, /* ECL_OPT_C_STACK_SAFETY_AREA */
	1,		/* ECL_OPT_SIGALTSTACK_SIZE */
#if ECL_FIXNUM_BITS <= 32
	1024*1024*1024, /* ECL_OPT_HEAP_SIZE */
#else
	4024*1024*1024, /* ECL_OPT_HEAP_SIZE */
#endif
	1024*1024, 	/* ECL_OPT_HEAP_SAFETY_AREA */
        0,		/* ECL_OPT_THREAD_INTERRUPT_SIGNAL */
        1,		/* ECL_OPT_SET_GMP_MEMORY_FUNCTIONS */
	0};

#if !defined(GBC_BOEHM)
static char stdin_buf[BUFSIZ];
static char stdout_buf[BUFSIZ];
#endif

cl_fixnum
ecl_get_option(int option)
{
	if (option >= ECL_OPT_LIMIT || option < 0) {
		FEerror("Invalid boot option ~D", 1, MAKE_FIXNUM(option));
	}
        return option_values[option];
}

void
ecl_set_option(int option, cl_fixnum value)
{
	if (option > ECL_OPT_LIMIT || option < 0) {
		FEerror("Invalid boot option ~D", 1, MAKE_FIXNUM(option));
	} else {
		if (option < ECL_OPT_BOOTED &&
		    option_values[ECL_OPT_BOOTED]) {
			FEerror("Cannot change option ~D while ECL is running",
				1, MAKE_FIXNUM(option));
		}
		option_values[option] = value;
	}
}

void
ecl_init_env(cl_env_ptr env)
{
	env->c_env = NULL;

	env->string_pool = Cnil;

	env->stack = NULL;
	env->stack_top = NULL;
	env->stack_limit = NULL;
	env->stack_size = 0;
	ecl_stack_set_size(env, ecl_get_option(ECL_OPT_LISP_STACK_SIZE));

#if !defined(ECL_CMU_FORMAT)
	env->fmt_aux_stream = ecl_make_string_output_stream(64, 1);
#endif
#ifdef HAVE_LIBFFI
        env->ffi_args_limit = 0;
        env->ffi_types = 0;
        env->ffi_values = 0;
        env->ffi_values_ptrs = 0;
#endif
#ifdef ECL_DYNAMIC_FFI
	env->fficall = ecl_alloc(sizeof(struct ecl_fficall));
	((struct ecl_fficall*)env->fficall)->registers = 0;
#endif

#ifdef CLOS
	env->method_cache = ecl_make_cache(64, 4096);
	env->slot_cache = ecl_make_cache(3, 4096);
#endif
        env->pending_interrupt = Cnil;

	init_stacks(env);

        {
	int i;
	for (i = 0; i < 3; i++) {
                cl_object x = ecl_alloc_object(t_bignum);
                _ecl_big_init2(x, ECL_BIG_REGISTER_SIZE);
		env->big_register[i] = x;
	}
        }

        env->trap_fpe_bits = 0;

        env->packages_to_be_created = Cnil;
        env->packages_to_be_created_p = Cnil;
}

void
_ecl_dealloc_env(cl_env_ptr env)
{
        /*
         * Environment cleanup. This is only required when the environment is
	 * allocated using mmap or some other method. We could do more, cleaning
	 * up stacks, etc, but we actually do not do it because that would need
	 * a lisp environment set up -- the allocator assumes one -- and we
	 * may have already cleaned up the value of ecl_process_env()
         */
#if defined(ECL_USE_MPROTECT)
	if (munmap(env, sizeof(*env)))
		ecl_internal_error("Unable to deallocate environment structure.");
#else
# if defined(ECL_USE_GUARD_PAGE)
        if (VirtualFree(env, sizeof(*env), MEM_RELEASE))
                ecl_internal_error("Unable to deallocate environment structure.");
# endif
#endif
}

cl_env_ptr
_ecl_alloc_env()
{
	/*
	 * Allocates the lisp environment for a thread. Depending on which
	 * mechanism we use for detecting delayed signals, we may allocate
	 * the environment using mmap or the garbage collector.
	 */
	cl_env_ptr output;
#if defined(ECL_USE_MPROTECT)
	output = mmap(0, sizeof(*output), PROT_READ | PROT_WRITE,
			MAP_ANON | MAP_PRIVATE, -1, 0);
	if (output == MAP_FAILED)
		ecl_internal_error("Unable to allocate environment structure.");
#else
# if defined(ECL_USE_GUARD_PAGE)
	output = VirtualAlloc(0, sizeof(*output), MEM_COMMIT,
			      PAGE_READWRITE);
	if (output == NULL)
		ecl_internal_error("Unable to allocate environment structure.");
# else
	static struct cl_env_struct first_env;
	if (!ecl_get_option(ECL_OPT_BOOTED)) {
		/* We have not set up any environment. Hence, we cannot call ecl_alloc()
		 * because it will need to stop interrupts and currently we rely on
		 * the environment for that */
		output = ecl_alloc_unprotected(sizeof(*output));
	} else {
		output = ecl_alloc(sizeof(*output));
	}
# endif
#endif
        {
                size_t bytes = cl_core.default_sigmask_bytes;
                if (bytes == 0) {
                        output->default_sigmask = 0;
                } else if (ecl_get_option(ECL_OPT_BOOTED)) {
                        output->default_sigmask = ecl_alloc_atomic(bytes);
                        memcpy(output->default_sigmask,
                               ecl_process_env()->default_sigmask,
                               bytes);
                } else {
                        output->default_sigmask = cl_core.default_sigmask;
                }
        }
	/*
	 * An uninitialized environment _always_ disables interrupts. They
	 * are activated later on by the thread entry point or init_unixint().
	 */
	output->disable_interrupts = 1;
	return output;
}

void
cl_shutdown(void)
{
	if (ecl_get_option(ECL_OPT_BOOTED) > 0) {
		cl_object l = ecl_symbol_value(@'si::*exit-hooks*');
		cl_object form = cl_list(2, @'funcall', Cnil);
		while (CONSP(l)) {
			ecl_elt_set(form, 1, ECL_CONS_CAR(l));
			si_safe_eval(3, form, Cnil, OBJNULL);
			l = CDR(l);
			ECL_SET(@'si::*exit-hooks*', l);
		}
#ifdef ENABLE_DLOPEN
		ecl_library_close_all();
#endif
#ifdef TCP
		ecl_tcp_close_all();
#endif
	}
	ecl_set_option(ECL_OPT_BOOTED, -1);
}

ecl_def_ct_single_float(default_rehash_size,1.5f,static,const);
ecl_def_ct_single_float(default_rehash_threshold,0.75f,static,const);
ecl_def_ct_base_string(str_common_lisp,"COMMON-LISP",11,static,const);
ecl_def_ct_base_string(str_common_lisp_user,"COMMON-LISP-USER",16,static,const);
ecl_def_ct_base_string(str_cl,"CL",2,static,const);
ecl_def_ct_base_string(str_cl_user,"CL-USER",7,static,const);
ecl_def_ct_base_string(str_LISP,"LISP",4,static,const);
ecl_def_ct_base_string(str_c,"C",1,static,const);
ecl_def_ct_base_string(str_compiler,"COMPILER",11,static,const);
ecl_def_ct_base_string(str_user,"USER",4,static,const);
ecl_def_ct_base_string(str_keyword,"KEYWORD",7,static,const);
ecl_def_ct_base_string(str_si,"SI",2,static,const);
ecl_def_ct_base_string(str_sys,"SYS",3,static,const);
ecl_def_ct_base_string(str_system,"SYSTEM",6,static,const);
ecl_def_ct_base_string(str_ext,"EXT",3,static,const);
#ifdef CLOS
ecl_def_ct_base_string(str_clos,"CLOS",4,static,const);
#endif
ecl_def_ct_base_string(str_mp,"MP",2,static,const);
ecl_def_ct_base_string(str_multiprocessing,"MULTIPROCESSING",15,static,const);
#ifdef ECL_CLOS_STREAMS
ecl_def_ct_base_string(str_gray,"GRAY",4,static,const);
#endif
ecl_def_ct_base_string(str_null,"Null",4,static,const);
ecl_def_ct_base_string(str_linefeed,"Linefeed",8,static,const);
ecl_def_ct_base_string(str_bell,"Bell",4,static,const);
ecl_def_ct_base_string(str_escape,"Escape",6,static,const);
ecl_def_ct_base_string(str_star_dot_star,"*.*",3,static,const);
ecl_def_ct_base_string(str_rel_star_dot_star,"./*.*",5,static,const);
ecl_def_ct_base_string(str_empty,"",0,static,const);
ecl_def_ct_base_string(str_G,"G",1,static,const);
ecl_def_ct_base_string(str_T,"T",1,static,const);
#ifdef ENABLE_DLOPEN
ecl_def_ct_base_string(str_fas,"fas",3,static,const);
ecl_def_ct_base_string(str_fasl,"fasl",4,static,const);
#endif
ecl_def_ct_base_string(str_fasb,"fasb",4,static,const);
ecl_def_ct_base_string(str_fasc,"fasc",4,static,const);
ecl_def_ct_base_string(str_FASB,"FASB",4,static,const);
ecl_def_ct_base_string(str_FASC,"FASC",4,static,const);
ecl_def_ct_base_string(str_lsp,"lsp",3,static,const);
ecl_def_ct_base_string(str_LSP,"LSP",3,static,const);
ecl_def_ct_base_string(str_lisp,"lisp",4,static,const);
ecl_def_ct_base_string(str_NIL,"NIL",3,static,const);
ecl_def_ct_base_string(str_slash,"/",1,static,const);

ecl_def_ct_single_float(flt_zero,0,static,const);
ecl_def_ct_single_float(flt_zero_neg,-0.0,static,const);
ecl_def_ct_double_float(dbl_zero,0,static,const);
ecl_def_ct_double_float(dbl_zero_neg,-0.0,static,const);
#ifdef ECL_LONG_FLOAT
ecl_def_ct_long_float(ldbl_zero,0,static,const);
ecl_def_ct_long_float(ldbl_zero_neg,-0.0l,static,const);
#endif
ecl_def_ct_ratio(plus_half,MAKE_FIXNUM(1),MAKE_FIXNUM(2),static,const);
ecl_def_ct_ratio(minus_half,MAKE_FIXNUM(-1),MAKE_FIXNUM(2),static,const);
ecl_def_ct_single_float(flt_one,1,static,const);
ecl_def_ct_single_float(flt_one_neg,-1,static,const);
ecl_def_ct_single_float(flt_two,2,static,const);
ecl_def_ct_complex(flt_imag_unit,&flt_zero_data,&flt_one_data,static,const);
ecl_def_ct_complex(flt_imag_unit_neg,&flt_zero_data,&flt_one_neg_data,static,const);
ecl_def_ct_complex(flt_imag_two,&flt_zero_data,&flt_two_data,static,const);

struct cl_core_struct cl_core = {
	Cnil, /* packages */
	Cnil, /* lisp_package */
	Cnil, /* user_package */
	Cnil, /* keyword_package */
	Cnil, /* system_package */
        Cnil, /* ext_package */
#ifdef CLOS
	Cnil, /* clos_package */
# ifdef ECL_CLOS_STREAMS
	Cnil, /* gray_package */
# endif
#endif
	Cnil, /* mp_package */
        Cnil, /* c_package */

	Cnil, /* pathname_translations */
        Cnil, /* library_pathname */

	Cnil, /* terminal_io */
	Cnil, /* null_stream */
	Cnil, /* standard_input */
	Cnil, /* standard_output */
	Cnil, /* error_output */
	Cnil, /* standard_readtable */
	Cnil, /* dispatch_reader */
	Cnil, /* default_dispatch_macro */

	Cnil, /* char_names */
	(cl_object)&str_empty_data, /* null_string */

	(cl_object)&plus_half_data, /* plus_half */
	(cl_object)&minus_half_data, /* minus_half */
	(cl_object)&flt_imag_unit_data, /* imag_unit */
	(cl_object)&flt_imag_unit_neg_data, /* minus_imag_unit */
	(cl_object)&flt_imag_two_data, /* imag_two */
	(cl_object)&flt_zero_data, /* singlefloat_zero */
	(cl_object)&dbl_zero_data, /* doublefloat_zero */
	(cl_object)&flt_zero_neg_data, /* singlefloat_minus_zero */
	(cl_object)&dbl_zero_neg_data, /* doublefloat_minus_zero */
#ifdef ECL_LONG_FLOAT
	(cl_object)&ldbl_zero_data, /* longfloat_zero */
	(cl_object)&ldbl_zero_neg_data, /* longfloat_minus_zero */
#endif

	(cl_object)&str_G_data, /* gensym_prefix */
	(cl_object)&str_T_data, /* gentemp_prefix */
	MAKE_FIXNUM(0), /* gentemp_counter */

	Cnil, /* Jan1st1970UT */

	Cnil, /* system_properties */
	Cnil, /* setf_definition */

#ifdef ECL_THREADS
	Cnil, /* processes */
	Cnil, /* global_lock */
        Cnil, /* error_lock */
        Cnil, /* global_env_lock */
#endif
	/* LIBRARIES is an adjustable vector of objects. It behaves as
	   a vector of weak pointers thanks to the magic in
	   gbc.d/alloc_2.d */
	Cnil, /* libraries */

	0, /* max_heap_size */
	Cnil, /* bytes_consed */
	Cnil, /* gc_counter */
	0, /* gc_stats */
	0, /* path_max */
#ifdef GBC_BOEHM
        NULL, /* safety_region */
#endif
#ifdef ECL_THREADS
        Cnil, /* signal_queue_lock */
#endif
	Cnil, /* signal_queue */

	NULL, /* default_sigmask */
        0, /* default_sigmask_bytes */

#ifdef ECL_THREADS
        0, /* last_var_index */
        Cnil, /* reused_indices */
#endif
	(cl_object)&str_slash_data, /* slash */

	Cnil, /* compiler_dispatch */

        (cl_object)&default_rehash_size_data, /* rehash_size */
        (cl_object)&default_rehash_threshold_data, /* rehash_threshold */

        Cnil, /* external_processes */
        Cnil /* external_processes_lock */
};

int
cl_boot(int argc, char **argv)
{
	cl_object aux;
	cl_object features;
	int i;
	cl_env_ptr env;

	i = ecl_get_option(ECL_OPT_BOOTED);
	if (i) {
		if (i < 0) {
			/* We have called cl_shutdown and want to use ECL again. */
			ecl_set_option(ECL_OPT_BOOTED, 1);
		}
		return 1;
	}

	/*ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0);*/

#if !defined(GBC_BOEHM)
	setbuf(stdin,  stdin_buf);
	setbuf(stdout, stdout_buf);
#endif

	ARGC = argc;
	ARGV = argv;
	ecl_self = argv[0];

	init_unixint(0);
	init_alloc();
	GC_disable();
	env = _ecl_alloc_env();
#ifdef ECL_THREADS
        init_threads(env);
#else
	cl_env_p = env;
#endif

	/*
	 * 1) Initialize symbols and packages
	 */

	Cnil_symbol->symbol.t = t_symbol;
	Cnil_symbol->symbol.dynamic = 0;
	Cnil_symbol->symbol.value = Cnil;
	Cnil_symbol->symbol.name = str_NIL;
	Cnil_symbol->symbol.gfdef = Cnil;
	Cnil_symbol->symbol.plist = Cnil;
	Cnil_symbol->symbol.hpack = Cnil;
	Cnil_symbol->symbol.stype = stp_constant;
#ifdef ECL_THREADS
	Cnil_symbol->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
#endif
	cl_num_symbols_in_core=1;

	Ct->symbol.t = (short)t_symbol;
	Ct->symbol.dynamic = 0;
	Ct->symbol.value = Ct;
	Ct->symbol.name = str_T;
	Ct->symbol.gfdef = Cnil;
	Ct->symbol.plist = Cnil;
	Ct->symbol.hpack = Cnil;
	Ct->symbol.stype = stp_constant;
#ifdef ECL_THREADS
	Ct->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
#endif
	cl_num_symbols_in_core=2;

#ifdef NO_PATH_MAX
	cl_core.path_max = sysconf(_PC_PATH_MAX);
#else
	cl_core.path_max = MAXPATHLEN;
#endif

        env->packages_to_be_created = Cnil;
	cl_core.lisp_package =
		ecl_make_package(str_common_lisp,
				 cl_list(2, str_cl, str_LISP),
				 Cnil);
	cl_core.user_package =
		ecl_make_package(str_common_lisp_user,
				 cl_list(2, str_cl_user, str_user),
				 ecl_list1(cl_core.lisp_package));
	cl_core.keyword_package =
		ecl_make_package(str_keyword, Cnil, Cnil);
	cl_core.ext_package =
		ecl_make_package(str_ext, Cnil,
				 ecl_list1(cl_core.lisp_package));
	cl_core.system_package =
		ecl_make_package(str_si,
                                 cl_list(2,str_system,str_sys),
				 cl_list(2,cl_core.ext_package,
                                         cl_core.lisp_package));
	cl_core.c_package =
		ecl_make_package(str_c,
                                 ecl_list1(str_compiler),
				 ecl_list1(cl_core.lisp_package));
#ifdef CLOS
	cl_core.clos_package =
		ecl_make_package(str_clos, Cnil, ecl_list1(cl_core.lisp_package));
#endif
	cl_core.mp_package =
		ecl_make_package(str_mp,
				 ecl_list1(str_multiprocessing),
				 ecl_list1(cl_core.lisp_package));
#ifdef ECL_CLOS_STREAMS
	cl_core.gray_package = ecl_make_package(str_gray, Cnil,
						CONS(cl_core.lisp_package, Cnil));
#endif

	Cnil_symbol->symbol.hpack = cl_core.lisp_package;
	cl_import2(Cnil, cl_core.lisp_package);
	cl_export2(Cnil, cl_core.lisp_package);

	Ct->symbol.hpack = cl_core.lisp_package;
	cl_import2(Ct, cl_core.lisp_package);
	cl_export2(Ct, cl_core.lisp_package);

	/* At exit, clean up */
	atexit(cl_shutdown);

	/* These must come _after_ the packages and NIL/T have been created */
	init_all_symbols();

	/*
	 * Initialize the per-thread data.
	 * This cannot come later, because some routines need the
	 * frame stack immediately (for instance SI:PATHNAME-TRANSLATIONS).
	 */
        init_big();
	ecl_init_env(env);
	ecl_cs_set_org(env);
#if !defined(GBC_BOEHM)
	/* We need this because a lot of stuff is to be created */
	init_GC();
#endif
	GC_enable();

        /*
         * Initialize default pathnames
         */
#if 1
	ECL_SET(@'*default-pathname-defaults*', si_getcwd(0));
#else
	ECL_SET(@'*default-pathname-defaults*',
		ecl_make_pathname(Cnil, Cnil, Cnil, Cnil, Cnil, Cnil, @':local'));
#endif

#ifdef ECL_THREADS
	env->bindings_array = si_make_vector(Ct, MAKE_FIXNUM(256),
                                            Cnil, Cnil, Cnil, Cnil);
        si_fill_array_with_elt(env->bindings_array, OBJNULL, MAKE_FIXNUM(0), Cnil);
        env->thread_local_bindings_size = env->bindings_array->vector.dim;
        env->thread_local_bindings = env->bindings_array->vector.self.t;
	ECL_SET(@'mp::*current-process*', env->own_process);
#endif

	/*
         * Load character names. The following hash table is a map
         * from names to character codes and viceversa. Note that we
         * need EQUALP because it has to be case insensitive.
	 */
	cl_core.char_names = aux =
	    cl__make_hash_table(@'equalp', MAKE_FIXNUM(128), /* size */
				cl_core.rehash_size,
                                cl_core.rehash_threshold);
	for (i = 0; char_names[i].elt.self; i++) {
                cl_object name = (cl_object)(char_names + i);
		cl_object code = MAKE_FIXNUM(i);
		ecl_sethash(name, aux, code);
		ecl_sethash(code, aux, name);
	}
        /* Linefeed is redundant with one of the names given in
         * iso_latin_names.h, but it can not be associated to the code
         * 10, because the default name must be Newline. Similar to
         * the other codes. */
        ecl_sethash(str_null, aux, MAKE_FIXNUM(0));
        ecl_sethash(str_linefeed, aux, MAKE_FIXNUM(10));
        ecl_sethash(str_bell, aux, MAKE_FIXNUM(7));
        ecl_sethash(str_escape, aux, MAKE_FIXNUM(27));

        /*
         * Initialize logical pathname translations. This must come after
         * the character database has been filled.
         */
	@si::pathname-translations(2,str_sys,
                                   ecl_list1(cl_list(2,str_star_dot_star,
                                                     str_rel_star_dot_star)));

	/*
	 * Initialize constants (strings, numbers and time).
	 */
	cl_core.system_properties =
	    cl__make_hash_table(@'equal', MAKE_FIXNUM(1024), /* size */
				cl_core.rehash_size,
                                cl_core.rehash_threshold);
	cl_core.setf_definitions =
	    cl__make_hash_table(@'eq', MAKE_FIXNUM(256), /* size */
				cl_core.rehash_size,
                                cl_core.rehash_threshold);

	ECL_SET(@'*random-state*', ecl_make_random_state(Ct));

	ECL_SET(@'si::c-int-max', ecl_make_integer(INT_MAX));
	ECL_SET(@'si::c-int-min', ecl_make_integer(INT_MIN));
	ECL_SET(@'si::c-long-max', ecl_make_integer(LONG_MAX));
	ECL_SET(@'si::c-long-min', ecl_make_integer(LONG_MIN));
	ECL_SET(@'si::c-uint-max', ecl_make_unsigned_integer(UINT_MAX));
	ECL_SET(@'si::c-ulong-max', ecl_make_unsigned_integer(ULONG_MAX));
#ifdef ecl_long_long_t
	ECL_SET(@'si::c-long-long-max', ecl_make_unsigned_integer(LLONG_MAX));
	ECL_SET(@'si::c-ulong-long-max', ecl_make_unsigned_integer(ULLONG_MAX));
#endif

	init_unixtime();

	/*
	 * Initialize I/O subsystem.
	 */
	init_file();
	init_read();

	ECL_SET(@'*print-case*', @':upcase');

	/*
	 * Set up hooks for LOAD, errors and macros.
	 */
#ifdef ECL_THREADS
	ECL_SET(@'mp::+load-compile-lock+',
		mp_make_lock(2, @':name', @'mp::+load-compile-lock+'));
#endif
	aux = cl_list(
#ifdef ENABLE_DLOPEN
		11,
                CONS(str_fas, @'si::load-binary'),
		CONS(str_fasl, @'si::load-binary'),
		CONS(str_fasb, @'si::load-binary'),
		CONS(str_FASB, @'si::load-binary'),
#else
		7,
#endif
		CONS(str_lsp, @'si::load-source'),
		CONS(str_lisp, @'si::load-source'),
		CONS(str_LSP, @'si::load-source'),
		CONS(str_LISP, @'si::load-source'),
		CONS(str_fasc, @'si::load-bytecodes'),
		CONS(str_FASC, @'si::load-bytecodes'),
		CONS(Cnil, @'si::load-source'));
	ECL_SET(@'ext::*load-hooks*', aux);
	init_error();
	init_macros();
	init_compiler();

	/*
	 * Set up infrastructure for CLOS.
	 */
#ifdef CLOS
	ECL_SET(@'si::*class-name-hash-table*',
		cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), /* size */
                                    cl_core.rehash_size,
                                    cl_core.rehash_threshold));
#endif

	/*
	 * Features.
	 */

	ECL_SET(@'LAMBDA-LIST-KEYWORDS',
		cl_list(8, @'&optional', @'&rest', @'&key', @'&allow-other-keys',
			@'&aux', @'&whole', @'&environment', @'&body'));

        for (i = 0, features = Cnil; feature_names[i].elt.self; i++) {
                int flag;
                cl_object name = (cl_object)(feature_names + i);
                cl_object key = ecl_intern(name, cl_core.keyword_package, &flag);
                features = CONS(key, features);
        }

	ECL_SET(@'*features*', features);

	ECL_SET(@'*package*', cl_core.lisp_package);

	/* This has to come before init_LSP/CLOS, because we need
	 * ecl_clear_compiler_properties() to work in init_CLOS(). */
	ecl_set_option(ECL_OPT_BOOTED, 1);

	ecl_init_module(OBJNULL,init_lib_LSP);

	/* Jump to top level */
	ECL_SET(@'*package*', cl_core.user_package);
	init_unixint(1);
	return 1;
}

/************************* ENVIRONMENT ROUTINES ***********************/

@(defun ext::quit (&optional (code MAKE_FIXNUM(0)) (kill_all_threads Ct))
@
{
#ifdef ECL_THREADS
        if (!Null(kill_all_threads)) {
                cl_object this = the_env->own_process;
                cl_object p, all_threads = mp_all_processes();
                for (p = all_threads; !Null(p); p = ECL_CONS_CDR(p)) {
                        cl_object process = ECL_CONS_CAR(p);
                        if (process != this && process->process.active)
                                mp_process_kill(process);
                }
                for (p = all_threads; !Null(p); p = ECL_CONS_CDR(p)) {
                        cl_object process = ECL_CONS_CAR(p);
                        if (process != this && process->process.active)
                                mp_process_join(process);
                }
        }
#endif
        ECL_SET(@'ext::*program-exit-code*', code);
        if (the_env->frs_org <= the_env->frs_top)
                ecl_unwind(the_env, the_env->frs_org);
        si_exit(1, code);
}
@)

@(defun ext::exit (&optional (code ECL_SYM_VAL(ecl_process_env(),@'ext::*program-exit-code*')))
@
        cl_shutdown();
        exit(FIXNUMP(code)? fix(code) : 0);
@)

cl_object
si_argc()
{
	@(return MAKE_FIXNUM(ARGC))
}

cl_object
si_argv(cl_object index)
{
	if (FIXNUMP(index)) {
		cl_fixnum i = fix(index);
		if (i >= 0 && i < ARGC)
			@(return make_base_string_copy(ARGV[i]));
	}
	FEerror("Illegal argument index: ~S.", 1, index);
}

cl_object
si_getenv(cl_object var)
{
	const char *value;

        /* Strings have to be null terminated base strings */
	var = si_copy_to_simple_base_string(var);
	value = getenv((char*)var->base_string.self);
	@(return ((value == NULL)? Cnil : make_base_string_copy(value)))
}

#if defined(HAVE_SETENV) || defined(HAVE_PUTENV)
cl_object
si_setenv(cl_object var, cl_object value)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_fixnum ret_val;

	/* Strings have to be null terminated base strings */
	var = si_copy_to_simple_base_string(var);
	if (value == Cnil) {
#ifdef HAVE_SETENV
		/* Remove the variable when setting to nil, so that
		 * (si:setenv "foo" nil), then (si:getenv "foo) returns
		 * the right thing. */
		unsetenv((char*)var->base_string.self);
#else
#if defined(ECL_MS_WINDOWS_HOST)
		si_setenv(var, cl_core.null_string);
#else
		putenv((char*)var->base_string.self);
#endif
#endif
		ret_val = 0;
	} else {
#ifdef HAVE_SETENV
		value = si_copy_to_simple_base_string(value);
		ret_val = setenv((char*)var->base_string.self,
				 (char*)value->base_string.self, 1);
#else
		value = cl_format(4, Cnil, make_constant_base_string("~A=~A"), var,
				  value);
		value = si_copy_to_simple_base_string(value);
		putenv((char*)value->base_string.self);
#endif
	}
	if (ret_val == -1)
		CEerror(Ct, "SI:SETENV failed: insufficient space in environment.",
			1, Cnil);
	@(return value)
}
#endif

cl_object
si_environ(void)
{
        cl_object output = Cnil;
#ifdef HAVE_ENVIRON
        char **p;
        extern char **environ;
        for (p = environ; *p; p++) {
                output = CONS(make_constant_base_string(*p), output);
        }
        output = cl_nreverse(output);
#else
# if defined(ECL_MS_WINDOWS_HOST)
        LPTCH p;
        for (p = GetEnvironmentStrings(); *p; ) {
                output = CONS(make_constant_base_string(p), output);
                do { (void)0; } while (*(p++));
        }
        output = cl_nreverse(output);
# endif
#endif /* HAVE_ENVIRON */
        @(return output)
}

cl_object
si_pointer(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	@(return ecl_make_unsigned_integer((cl_index)x))
}

#if defined(ECL_MS_WINDOWS_HOST)
void
ecl_get_commandline_args(int* argc, char*** argv) {
	LPWSTR *wArgs;
	int i;

	if (argc == NULL || argv == NULL)
		return;

	wArgs = CommandLineToArgvW(GetCommandLineW(), argc);
	*argv = (char**)malloc(sizeof(char*)*(*argc));
	for (i=0; i<*argc; i++) {
		int len = wcslen(wArgs[i]);
		(*argv)[i] = (char*)malloc(2*(len+1));
		wcstombs((*argv)[i], wArgs[i], len+1);
	}
	LocalFree(wArgs);
}
#endif
