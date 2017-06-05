/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    instance.c -- CLOS interface.
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
#include <string.h>

cl_object
ecl_allocate_instance(cl_object clas, cl_index size)
{
	cl_object x = ecl_alloc_instance(size);
	cl_index i;
	CLASS_OF(x) = clas;
	for (i = 0;  i < size;  i++)
		x->instance.slots[i] = ECL_UNBOUND;
	return x;
}

cl_object
si_allocate_raw_instance(cl_object orig, cl_object clas, cl_object size)
{
	cl_object output = ecl_allocate_instance(clas, ecl_to_size(size));
	if (orig == Cnil) {
		orig = output;
	} else {
		orig->instance.clas = clas;
		orig->instance.length = output->instance.length;
		orig->instance.slots = output->instance.slots;
	}
	@(return orig)
}

cl_object
si_instance_sig(cl_object x)
{
	@(return x->instance.sig);
}

cl_object
si_instance_sig_set(cl_object x)
{
	@(return (x->instance.sig = CLASS_SLOTS(CLASS_OF(x))));
}

cl_object
si_instance_class(cl_object x)
{
	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_only_arg(@[class-of], x, @[ext::instance]);
	@(return CLASS_OF(x))
}

cl_object
si_instance_class_set(cl_object x, cl_object y)
{
	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::instance-class-set], 1, x, @[ext::instance]);
	if (ecl_unlikely(!ECL_INSTANCEP(y)))
                FEwrong_type_nth_arg(@[si::instance-class-set], 2, y, @[ext::instance]);
	CLASS_OF(x) = y;
	@(return x)
}

cl_object
ecl_instance_ref(cl_object x, cl_fixnum i)
{
	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::instance-ref], 1, x, @[ext::instance]);
	if (ecl_unlikely(i < 0 || i >= (cl_fixnum)x->instance.length))
	        FEtype_error_index(x, i);
	return(x->instance.slots[i]);
}

cl_object
si_instance_ref(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::instance-ref], 1, x, @[ext::instance]);
	if (ecl_unlikely(!FIXNUMP(index) ||
                         (i = fix(index)) < 0 || i >= (cl_fixnum)x->instance.length))
	        FEtype_error_index(x, index);
	@(return x->instance.slots[i])
}

cl_object
si_instance_ref_safe(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::instance-ref], 1, x, @[ext::instance]);
	if (ecl_unlikely(!FIXNUMP(index) ||
                         (i = fix(index)) < 0 || i >= x->instance.length))
	        FEtype_error_index(x, index);
	x = x->instance.slots[i];
	if (ecl_unlikely(x == ECL_UNBOUND))
		cl_error(5, @'unbound-slot', @':name', index, @':instance', x);
	@(return x)
}

cl_object
ecl_instance_set(cl_object x, cl_fixnum i, cl_object v)
{
        if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::instance-set], 1, x, @[ext::instance]);
	if (ecl_unlikely(i >= x->instance.length || i < 0))
	        FEtype_error_index(x, i);
	x->instance.slots[i] = v;
	return(v);
}

cl_object
si_instance_set(cl_object x, cl_object index, cl_object value)
{
	cl_fixnum i;

	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::instance-set], 1, x, @[ext::instance]);
	if (ecl_unlikely(!FIXNUMP(index) ||
                         (i = fix(index)) >= (cl_fixnum)x->instance.length || i < 0))
		FEtype_error_index(x, index);
	x->instance.slots[i] = value;
	@(return value)
}

cl_object
si_instancep(cl_object x)
{
	@(return (ECL_INSTANCEP(x) ? MAKE_FIXNUM(x->instance.length) : Cnil))
}

cl_object
si_unbound()
{
	/* Returns an object that cannot be read or written and which
	   is used to represent an unitialized slot */
	@(return ECL_UNBOUND)
}

cl_object
si_sl_boundp(cl_object x)
{
	@(return ((x == ECL_UNBOUND) ? Cnil : Ct))
}

cl_object
si_sl_makunbound(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::sl-makunbound], 1, x, @[ext::instance]);
	if (ecl_unlikely(!FIXNUMP(index) ||
                         (i = fix(index)) >= x->instance.length || i < 0))
		FEtype_error_index(x, index);
	x->instance.slots[i] = ECL_UNBOUND;
	@(return x)
}

cl_object
si_copy_instance(cl_object x)
{
	cl_object y;

	if (ecl_unlikely(!ECL_INSTANCEP(x)))
                FEwrong_type_nth_arg(@[si::copy-instance], 1, x, @[ext::instance]);
	y = ecl_allocate_instance(x->instance.clas, x->instance.length);
	y->instance.sig = x->instance.sig;
	memcpy(y->instance.slots, x->instance.slots,
	       x->instance.length * sizeof(cl_object));
	@(return y)
}

@(defun find-class (name &optional (errorp Ct) env)
	cl_object class, hash;
@
	hash = ECL_SYM_VAL(the_env, @'si::*class-name-hash-table*');
	class = ecl_gethash_safe(name, hash, Cnil);
	if (class == Cnil) {
		if (!Null(errorp))
			FEerror("No class named ~S.", 1, name);
	}
	@(return class)
@)

cl_object
ecl_slot_value(cl_object x, const char *slot)
{
	cl_object slot_name = ecl_read_from_cstring(slot);
	return funcall(3, @'slot-value', x, slot_name);
}

cl_object
ecl_slot_value_set(cl_object x, const char *slot, cl_object value)
{
	cl_object slot_name = ecl_read_from_cstring(slot);
	cl_object slot_setter = ecl_read_from_cstring("(SETF SLOT-VALUE)");
	return funcall(4, ecl_fdefinition(slot_setter), value, x, slot_name);
}

enum ecl_built_in_classes {
	ECL_BUILTIN_T = 0,
	ECL_BUILTIN_SEQUENCE,
	ECL_BUILTIN_LIST,
	ECL_BUILTIN_CONS,
	ECL_BUILTIN_ARRAY,
	ECL_BUILTIN_VECTOR,
	ECL_BUILTIN_STRING,
#ifdef ECL_UNICODE
	ECL_BUILTIN_BASE_STRING,
#endif
	ECL_BUILTIN_BIT_VECTOR,
	ECL_BUILTIN_STREAM,
	ECL_BUILTIN_ANSI_STREAM,
	ECL_BUILTIN_FILE_STREAM,
	ECL_BUILTIN_ECHO_STREAM,
	ECL_BUILTIN_STRING_STREAM,
	ECL_BUILTIN_TWO_WAY_STREAM,
	ECL_BUILTIN_SYNONYM_STREAM,
	ECL_BUILTIN_BROADCAST_STREAM,
	ECL_BUILTIN_CONCATENATED_STREAM,
	ECL_BUILTIN_SEQUENCE_STREAM,
	ECL_BUILTIN_CHARACTER,
	ECL_BUILTIN_NUMBER,
	ECL_BUILTIN_REAL,
	ECL_BUILTIN_RATIONAL,
	ECL_BUILTIN_INTEGER,
	ECL_BUILTIN_RATIO,
	ECL_BUILTIN_FLOAT,
	ECL_BUILTIN_COMPLEX,
	ECL_BUILTIN_SYMBOL,
	ECL_BUILTIN_NULL,
	ECL_BUILTIN_KEYWORD,
	ECL_BUILTIN_METHOD_COMBINATION,
	ECL_BUILTIN_PACKAGE,
	ECL_BUILTIN_FUNCTION,
	ECL_BUILTIN_PATHNAME,
	ECL_BUILTIN_LOGICAL_PATHNAME,
	ECL_BUILTIN_HASH_TABLE,
	ECL_BUILTIN_RANDOM_STATE,
	ECL_BUILTIN_READTABLE,
	ECL_BUILTIN_CODE_BLOCK,
	ECL_BUILTIN_FOREIGN_DATA,
	ECL_BUILTIN_FRAME,
	ECL_BUILTIN_WEAK_POINTER
#ifdef ECL_THREADS
        ,
	ECL_BUILTIN_PROCESS,
	ECL_BUILTIN_LOCK,
	ECL_BUILTIN_CONDITION_VARIABLE
#endif
#ifdef ECL_SEMAPHORES
        , ECL_BUILTIN_SEMAPHORE
#endif
#ifdef ECL_SSE2
	, ECL_BUILTIN_SSE_PACK
#endif
};

cl_object
cl_class_of(cl_object x)
{
	size_t index;
	switch (type_of(x)) {
	case t_instance:
		@(return CLASS_OF(x))
	case t_fixnum:
	case t_bignum:
		index = ECL_BUILTIN_INTEGER; break;
	case t_ratio:
		index = ECL_BUILTIN_RATIO; break;
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		index = ECL_BUILTIN_FLOAT; break;
		/* XXX index = ECL_BUILTIN_long-float; break; */
	case t_complex:
		index = ECL_BUILTIN_COMPLEX; break;
	case t_character:
		index = ECL_BUILTIN_CHARACTER; break;
	case t_symbol:
		if (x->symbol.hpack == cl_core.keyword_package)
			index = ECL_BUILTIN_KEYWORD;
		else
			index = ECL_BUILTIN_SYMBOL;
		break;
	case t_package:
		index = ECL_BUILTIN_PACKAGE; break;
	case t_list:
		index = Null(x)? ECL_BUILTIN_NULL : ECL_BUILTIN_CONS; break;
	case t_hashtable:
		index = ECL_BUILTIN_HASH_TABLE; break;
	case t_array:
		index = ECL_BUILTIN_ARRAY; break;
	case t_vector:
		index = ECL_BUILTIN_VECTOR; break;
#ifdef ECL_UNICODE
	case t_string:
		index = ECL_BUILTIN_STRING; break;
	case t_base_string:
		index = ECL_BUILTIN_BASE_STRING; break;
#else
	case t_base_string:
		index = ECL_BUILTIN_STRING; break;
#endif
	case t_bitvector:
		index = ECL_BUILTIN_BIT_VECTOR; break;
	case t_stream:
		switch (x->stream.mode) {
		case smm_synonym:	index = ECL_BUILTIN_SYNONYM_STREAM; break;
		case smm_broadcast:	index = ECL_BUILTIN_BROADCAST_STREAM; break;
		case smm_concatenated:	index = ECL_BUILTIN_CONCATENATED_STREAM; break;
		case smm_two_way:	index = ECL_BUILTIN_TWO_WAY_STREAM; break;
		case smm_string_input:
		case smm_string_output:	index = ECL_BUILTIN_STRING_STREAM; break;
		case smm_echo:		index = ECL_BUILTIN_ECHO_STREAM; break;
                case smm_sequence_input:
                case smm_sequence_output: index = ECL_BUILTIN_SEQUENCE_STREAM; break;
		default:		index = ECL_BUILTIN_FILE_STREAM; break;
		}
		break;
	case t_readtable:
		index = ECL_BUILTIN_READTABLE; break;
	case t_pathname:
		index = ECL_BUILTIN_PATHNAME; break;
	case t_random:
		index = ECL_BUILTIN_RANDOM_STATE; break;
	case t_bytecodes:
	case t_bclosure:
	case t_cfun:
	case t_cfunfixed:
	case t_cclosure:
		index = ECL_BUILTIN_FUNCTION; break;
#ifdef ECL_THREADS
	case t_process:
		index = ECL_BUILTIN_PROCESS; break;
	case t_lock:
		index = ECL_BUILTIN_LOCK; break;
	case t_condition_variable:
		index = ECL_BUILTIN_CONDITION_VARIABLE; break;
#endif
#ifdef ECL_SEMAPHORES
	case t_semaphores:
		index = ECL_BUILTIN_SEMAPHORE; break;
#endif
	case t_codeblock:
		index = ECL_BUILTIN_CODE_BLOCK; break;
	case t_foreign:
		index = ECL_BUILTIN_FOREIGN_DATA; break;
	case t_frame:
		index = ECL_BUILTIN_FRAME; break;
	case t_weak_pointer:
		index = ECL_BUILTIN_WEAK_POINTER; break;
#ifdef ECL_SSE2
	case t_sse_pack:
		index = ECL_BUILTIN_SSE_PACK; break;
#endif
	default:
		ecl_internal_error("not a lisp data object");
	}
	{
		/* We have to be careful because +builtin-classes+ might be empty! */
		/* In any case, since +builtin-classes+ is a constant, we may
		 * optimize the slot access */
		cl_object v = @'clos::+builtin-classes+'->symbol.value;
		cl_object output = Null(v)?
			cl_find_class(1,@'t') :
			v->vector.self.t[index];
		@(return output)
	}
}

