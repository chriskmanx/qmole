/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/**
 * @file objpair.c
 * LibAST Object Infrastructure -- Paired Objects
 *
 * This file contains the objpair class.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.10 $
 * $Date: 2004/07/23 21:38:39 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: objpair.c,v 1.10 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
/**
 * The actual class structure for the @c objpair type.
 *
 * This structure is the actual class for the @c objpair type.  All LibAST
 * objects contain a spif_class_t member called @c cls which points to
 * a class structure, like this one.  The first structure member is a
 * pointer to the class name.  Each class uses the same pointer, so
 * you can compare the pointer values rather than having to compare
 * strings.  All other members are function pointers which reference
 * the object-agnostic routines that object supports.  ALL LibAST
 * objects support at least 8 operations:  new, init, done, del, show,
 * comp, dup, and type.  Other classes may define other standard
 * functions.  (This is used for doing interface classes.)
 *
 * @see @link DOXGRP_OBJPAIR LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
static SPIF_CONST_TYPE(class) o_class = {
    SPIF_DECL_CLASSNAME(objpair),
    (spif_func_t) spif_objpair_new,
    (spif_func_t) spif_objpair_init,
    (spif_func_t) spif_objpair_done,
    (spif_func_t) spif_objpair_del,
    (spif_func_t) spif_objpair_show,
    (spif_func_t) spif_objpair_comp,
    (spif_func_t) spif_objpair_dup,
    (spif_func_t) spif_objpair_type
};

/**
 * The class instance for the @c objpair type.
 *
 * This defines the spif_class_t for the @c objpair type so that it points
 * to the spif_const_class_t structure above.  This pointer value is
 * the very first thing stored in each * instance of an "objpair."
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
SPIF_TYPE(class) SPIF_CLASS_VAR(objpair) = &o_class;
/* *INDENT-ON* */



/*@{*/
/**
 * @name Paired Object Member Functions
 * ---
 *
 * These functions are members of the @c objpair class.  They can be
 * called directly or via the macros which dereference the function
 * pointers in the @c objpair class structure.  By convention, functions
 * are called directly when the object type is known and via macros
 * when the type is unknown.
 *
 * Most of these functions are not intended to actually be called.
 * Rather, they serve as models for the implementation of standard
 * methods in other (derived) object types.
 *
 * @ingroup DOXGRP_OBJPAIR
 */

/**
 * Create a new @c objpair instance.
 *
 * This function creates and returns a new instance of an @c objpair.  The
 * new instance is initialized using the spif_objpair_init() function.
 *
 * @return A new @c objpair instance.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 */
spif_objpair_t
spif_objpair_new(void)
{
    spif_objpair_t self;

    self = SPIF_ALLOC(objpair);
    if (!spif_objpair_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(objpair);
    }
    return self;
}

/**
 * Create a new @c objpair instance with a key object.
 *
 * This function creates and returns a new instance of an @c objpair
 * with a given key object.
 *
 * @param key  The key object for the pair.
 * @return     A new @c objpair instance whose key object is @a key.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 */
spif_objpair_t
spif_objpair_new_from_key(spif_obj_t key)
{
    spif_objpair_t self;

    self = SPIF_ALLOC(objpair);
    if (!spif_objpair_init_from_key(self, key)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(objpair);
    }
    return self;
}

/**
 * Create a new @c objpair instance with a value object.
 *
 * This function creates and returns a new instance of an @c objpair
 * with a given value object.
 *
 * @param value The value object for the pair.
 * @return      A new @c objpair instance whose value object is @a value.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 */
spif_objpair_t
spif_objpair_new_from_value(spif_obj_t value)
{
    spif_objpair_t self;

    self = SPIF_ALLOC(objpair);
    if (!spif_objpair_init_from_value(self, value)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(objpair);
    }
    return self;
}

/**
 * Create a new @c objpair instance with both key and value objects.
 *
 * This function creates and returns a new instance of an @c objpair
 * with given key and value objects.
 *
 * @param key   The key object for the pair.
 * @param value The value object for the pair.
 * @return      A new @c objpair instance whose key and value objects
 *              are @a key and @a value, respectively.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 */
spif_objpair_t
spif_objpair_new_from_both(spif_obj_t key, spif_obj_t value)
{
    spif_objpair_t self;

    self = SPIF_ALLOC(objpair);
    if (!spif_objpair_init_from_both(self, key, value)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(objpair);
    }
    return self;
}

/**
 * Initialize an @c objpair instance.
 *
 * This function initializes the member variables of the @c objpair
 * instance to their appropriate "bootstrap" values.
 *
 * @param self The @c objpair instance to be initialized.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_bool_t
spif_objpair_init(spif_objpair_t self)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), FALSE);
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(objpair));
    return TRUE;
}

/**
 * Initialize an @c objpair instance with a given key object.
 *
 * This function initializes the member variables of the @c objpair
 * instance to their appropriate "bootstrap" values, assigning @a key
 * to the key property of @a self.
 *
 * @param self The @c objpair instance to be initialized.
 * @param key  The key object for the pair.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_bool_t
spif_objpair_init_from_key(spif_objpair_t self, spif_obj_t key)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), FALSE);
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(key), FALSE);
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(objpair));
    self->key = SPIF_OBJ_DUP(SPIF_OBJ(key));
    self->value = SPIF_NULL_TYPE(obj);
    return TRUE;
}

/**
 * Initialize an @c objpair instance with a given value object.
 *
 * This function initializes the member variables of the @c objpair
 * instance to their appropriate "bootstrap" values, assigning @a value
 * to the value property of @a self.
 *
 * @param self  The @c objpair instance to be initialized.
 * @param value The value object for the pair.
 * @return      #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_bool_t
spif_objpair_init_from_value(spif_objpair_t self, spif_obj_t value)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), FALSE);
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(value), FALSE);
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(objpair));
    self->key = SPIF_NULL_TYPE(obj);
    self->value = SPIF_OBJ_DUP(SPIF_OBJ(value));
    return TRUE;
}

/**
 * Initialize an @c objpair instance with both key and value
 * objects.
 *
 * This function initializes the member variables of the @c objpair
 * instance to their appropriate "bootstrap" values, assigning @a key
 * to the key property of @self and @a value to the value property.
 *
 * @param self  The @c objpair instance to be initialized.
 * @param key   The key object for the pair.
 * @param value The value object for the pair.
 * @return      #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_bool_t
spif_objpair_init_from_both(spif_objpair_t self, spif_obj_t key, spif_obj_t value)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), FALSE);
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(key), FALSE);
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(value), FALSE);
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(objpair));
    self->key = SPIF_OBJ_DUP(SPIF_OBJ(key));
    self->value = SPIF_OBJ_DUP(SPIF_OBJ(value));
    return TRUE;
}

/**
 * Deallocate and reinitialize @c objpair resources.
 *
 * This function frees up any object resources and re-initializes them
 * to their "bootstrap" values.
 *
 * @param self The @c objpair instance to be zeroed and reinitialized.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_bool_t
spif_objpair_done(spif_objpair_t self)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), FALSE);
    if (!SPIF_OBJ_ISNULL(SPIF_OBJ(self->key))) {
        SPIF_OBJ_DEL(SPIF_OBJ(self->key));
    }
    self->key = SPIF_NULL_TYPE(obj);
    if (!SPIF_OBJ_ISNULL(SPIF_OBJ(self->value))) {
        SPIF_OBJ_DEL(SPIF_OBJ(self->value));
    }
    self->value = SPIF_NULL_TYPE(obj);

    return TRUE;
}

/**
 * Delete an @c objpair instance.
 *
 * This function deletes an instance of an @c objpair.  The done method,
 * spif_objpair_done(), is called to free any object resources prior to
 * deallocation of the object itself.
 *
 * @param self The @c objpair instance to be deleted.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_bool_t
spif_objpair_del(spif_objpair_t self)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), FALSE);
    spif_objpair_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

/**
 * Render an @c objpair as a text string.
 *
 * This function displays an @c objpair as a string using a
 * hierarchical representation similar to C syntax.
 *
 * @param self   The @c objpair instance.
 * @param name   The name of the variable passed as @a self.
 * @param buff   A @c str object, possibly NULL, used as the buffer.
 * @param indent The number of spaces with which to pad the line.
 * @return       The @c str object, or a new one if @a buff was NULL,
 *               describing @a self.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, SPIF_SHOW()
 * @ingroup DOXGRP_OBJPAIR
 */
spif_str_t
spif_objpair_show(spif_objpair_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_OBJPAIR_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(objpair, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_objpair_t) %s:  %10p \"%s\"\n",
             name, SPIF_CAST(ptr) self, SPIF_OBJ_CLASSNAME(self));
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }
    return buff;
}

/**
 * Compare two @c objpair instances.
 *
 * This function is used to compare two @c objpair instances.
 *
 * @param self  The first @c objpair instance.
 * @param other The second @c objpair instance.
 * @return      A spif_cmp_t value representing the comparison of @a
 *              self and @a other.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_str_comp(), spif_cmp_t, SPIF_CMP_FROM_INT()
 * @ingroup DOXGRP_OBJPAIR
 */
spif_cmp_t
spif_objpair_comp(spif_objpair_t self, spif_obj_t other)
{
    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    if (SPIF_OBJ_IS_OBJPAIR(other)) {
        return SPIF_OBJ_COMP(self->key, SPIF_OBJPAIR(other)->key);
    } else {
        return SPIF_OBJ_COMP(self->key, other);
    }
}

/**
 * Duplicate an @c objpair and its resources.
 *
 * This function is responsible for returning a new @c objpair
 * instance which contains the exact same value as the instance
 * supplied to it.
 *
 * @param self The @c objpair instance.
 * @return     An exact duplicate of @a self which is identical to,
 *             but programmatically independent of, the original.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_str_dup()
 * @ingroup DOXGRP_OBJPAIR */
spif_objpair_t
spif_objpair_dup(spif_objpair_t self)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), SPIF_NULL_TYPE(objpair));
    return spif_objpair_new_from_both(self->key, self->value);
}

/**
 * Obtain the class name of an @c objpair.
 *
 * This function is responsible for returning the class name (as a
 * spif_classname_t) of the supplied @c objpair.
 *
 * @param self The @c objpair instance.
 * @return     The class name of @a self.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink
 * @ingroup DOXGRP_OBJPAIR
 */
spif_classname_t
spif_objpair_type(spif_objpair_t self)
{
    ASSERT_RVAL(!SPIF_OBJPAIR_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(SPIF_OBJ(self));
}

SPIF_DEFINE_PROPERTY_FUNC(objpair, obj, key)
SPIF_DEFINE_PROPERTY_FUNC(objpair, obj, value)
/*@}*/
