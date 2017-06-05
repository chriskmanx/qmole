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
 * @file obj.c
 * LibAST Object Infrastructure -- Generic Objects
 *
 * This file contains the basic object class.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.27 $
 * $Date: 2004/07/23 21:38:39 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: obj.c,v 1.27 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
/**
 * The actual class structure for the @c obj type.
 *
 * This structure is the actual class for the @c obj type.  All LibAST
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
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
static SPIF_CONST_TYPE(class) o_class = {
    SPIF_DECL_CLASSNAME(obj),
    (spif_func_t) spif_obj_new,
    (spif_func_t) spif_obj_init,
    (spif_func_t) spif_obj_done,
    (spif_func_t) spif_obj_del,
    (spif_func_t) spif_obj_show,
    (spif_func_t) spif_obj_comp,
    (spif_func_t) spif_obj_dup,
    (spif_func_t) spif_obj_type
};

/**
 * The class instance for the @c obj type.
 *
 * This defines the spif_class_t for the @c obj type so that it points
 * to the spif_const_class_t structure above.  This pointer value is
 * the very first thing stored in each * instance of an "obj."
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
SPIF_TYPE(class) SPIF_CLASS_VAR(obj) = &o_class;
/* *INDENT-ON* */



/*@{*/
/**
 * @name Generic Object Member Functions
 * ---
 *
 * These functions are members of the @c obj class.  They can be
 * called directly or via the macros which dereference the function
 * pointers in the @c obj class structure.  By convention, functions
 * are called directly when the object type is known and via macros
 * when the type is unknown.
 *
 * Most of these functions are not intended to actually be called.
 * Rather, they serve as models for the implementation of standard
 * methods in other (derived) object types.
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Create a new @c obj instance.
 *
 * This function creates and returns a new instance of an @c obj.  The
 * new instance is initialized using the spif_obj_init() function.
 *
 * @return A new @c obj instance.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 */
spif_obj_t
spif_obj_new(void)
{
    spif_obj_t self;

    self = SPIF_ALLOC(obj);
    if (!spif_obj_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(obj);
    }
    return self;
}

/**
 * Delete an @c obj instance.
 *
 * This function deletes an instance of an @c obj.  The done method,
 * spif_obj_done(), is called to free any object resources prior to
 * deallocation of the object itself.
 *
 * @param self The @c obj instance to be deleted.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
spif_bool_t
spif_obj_del(spif_obj_t self)
{
    spif_bool_t t;

    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), FALSE);

    t = spif_obj_done(self);
    SPIF_DEALLOC(self);
    return t;
}

/**
 * Initialize an @c obj instance.
 *
 * This function initializes the member variables of the @c obj
 * instance to their appropriate "bootstrap" values.
 *
 * @note Though the calling of the parent's initializer is customary
 * and proper in derived classes, for subtypes of @c obj, this is not
 * strictly necessary.  All this function does is set the class to
 * that of an @c obj, which the child must undo anyway (by calling
 * <tt>spif_obj_set_class(self, SPIF_CLASS_VAR(XXX))</tt>, where
 * <tt>XXX</tt> is the class name, like below).  Thus, it is
 * acceptable for direct decendents of @c obj to not call this
 * function.  However, anything whose parent type is @a not @c obj
 * MUST call their parent's init function.
 *
 * @param self The @c obj instance to be initialized.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
spif_bool_t
spif_obj_init(spif_obj_t self)
{
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), FALSE);
    spif_obj_set_class(self, SPIF_CLASS_VAR(obj));
    return TRUE;
}

/**
 * Deallocate and reinitialize @c obj resources.
 *
 * This function frees up any object resources and re-initializes them
 * to their "bootstrap" values.
 *
 * @param self The @c obj instance to be zeroed and reinitialized.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
spif_bool_t
spif_obj_done(spif_obj_t self)
{
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), FALSE);
    return TRUE;
}

/**
 * Show an object and its contents.
 *
 * This function, as it is written here, doesn't do a whole hell of a
 * lot.  But this standard member function (i.e., all objects must
 * have one) provides the mechanism for which an object can display
 * not only itself and its particular values, but those of any parent
 * class or member object.  Besides the object to be displayed, this
 * function is passed the variable name for that object (usually a
 * constant string, like "foo"), a @c str object, possibly NULL, to be
 * added to and returned, and an indent level, possibly 0, to
 * represent how many leading spaces should pad the resulting output.
 * The @c str object returned is either @a buff, or a new @c str if @a
 * buff was passed as NULL.  Appended to it will be the description of
 * the object followed by a newline.  This description may include
 * descriptions of any number of child variables, parent objects, etc.
 *
 * Implementing this function properly is key to simplifying the
 * examination of objects through the use of debugging code.  I @b
 * highly recommend looking at some examples of how this function
 * should be implemented.
 *
 * The simplest way to display an object is to use the SPIF_SHOW()
 * macro.  This macro takes the object (@a self) and a file descriptor
 * (like @c stderr or #LIBAST_DEBUG_FD), calls the object's @c show
 * method, and prints the resulting string on the given file
 * descriptor, freeing it afterward.  No muss, no fuss.
 *
 * @param self   The @c obj instance.
 * @param name   The name of the variable passed as @a self.
 * @param buff   A @c str object, possibly NULL, used as the buffer.
 * @param indent The number of spaces with which to pad the line.
 * @return       The @c str object, or a new one if @a buff was NULL,
 *               describing @a self.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_SHOW()
 * @ingroup DOXGRP_OBJ
 */
spif_str_t
spif_obj_show(spif_obj_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_OBJ_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(obj, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_obj_t) %s:  %10p \"%s\"\n",
             name, SPIF_CAST(ptr) self, SPIF_OBJ_CLASSNAME(self));
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }
    return buff;
}

/**
 * Compare two objects.
 *
 * As with most of the other member functions of the @c obj class,
 * this one is really just a placeholder.  The @c comp standard member
 * function is used to compare two objects of a given type.  The
 * comparison can be implemented in any way, so long as it returns a
 * consistent spif_cmp_t value.  The simplest way is to compare the
 * two object variables, as shown below, but often a more sensible
 * method is warranted.
 *
 * @param self  The first @c obj instance.
 * @param other The second @c obj instance.
 * @return      A spif_cmp_t value representing the comparison of @a
 *              self and @a other.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_str_comp(), spif_cmp_t, SPIF_CMP_FROM_INT()
 * @ingroup DOXGRP_OBJ
 */
spif_cmp_t
spif_obj_comp(spif_obj_t self, spif_obj_t other)
{
    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    return SPIF_CMP_FROM_INT(SPIF_CAST(ulong) self - SPIF_CAST(ulong) other);
}

/**
 * Duplicate an @c obj and its resources.
 *
 * The @c dup standard member function is responsible for returning a
 * new object instance which contains the exact same value as the
 * instance supplied to it.  That means that any values are copied
 * from the original to the duplicate, and any references (pointers
 * and objects) are duplicated in the new instance, using MALLOC(),
 * the member object's @c dup function, etc.  The object returned MUST
 * be independent of the original; i.e., calling @c done or @c del on
 * the duplicate MUST NOT destroy or affect the original, or any of
 * its data, in any way.
 *
 * @param self The @c obj instance.
 * @return     An exact duplicate of @a self which is identical to,
 *             but programmatically independent of, the original.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_str_dup()
 * @ingroup DOXGRP_OBJ
 */
spif_obj_t
spif_obj_dup(spif_obj_t self)
{
    spif_obj_t tmp;

    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), SPIF_NULL_TYPE(obj));
    tmp = spif_obj_new();
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(obj));
    return tmp;
}

/**
 * Obtain the class name of an @c obj.
 *
 * The @c type standard member function is responsible for returning
 * the class name (as a spif_classname_t) of the supplied object.  You
 * will almost certainly want to implement it exactly as seen here.
 * If you don't, know why.
 *
 * @param self The @c obj instance.
 * @return     The class name of @a self.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
spif_classname_t
spif_obj_type(spif_obj_t self)
{
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

/**
 * Return the class of an object.
 *
 * This function returns the class (i.e., spif_class_t) of the
 * supplied @c obj instance.  Thanks to the Magic and Mystery of
 * typecasting, any LibAST object can be passed to this function to
 * obtain its class information, like so:
 * <tt>spif_obj_get_class(SPIF_OBJ(foo))</tt>  Or, simply use the
 * SPIF_OBJ_CLASS() macro.
 *
 * Keep in mind that this will return the @a entire class.  If you
 * simply want the class name string, use SPIF_OBJ_CLASSNAME()
 * instead.
 *
 * @param self The @c obj instance.
 * @return     The object's class, or NULL if @a self is NULL.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
spif_class_t
spif_obj_get_class(spif_obj_t self)
{
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), SPIF_NULL_TYPE(class));
    return SPIF_OBJ_CLASS(self);
}

/**
 * Set an object's class.
 *
 * This function sets the class (i.e., spif_class_t) of the supplied
 * @c obj instance.  Thanks to the Magic and Mystery of typecasting,
 * any LibAST object can be passed to this function to set its class
 * information, like so: <tt>spif_obj_set_class(SPIF_OBJ(foo),
 * SPIF_CLASS_VAR(XXX))</tt>, where <tt>XXX</tt> is the actual type of
 * the object (like @c str or @c regexp).  Any call to the @c init
 * member of a parent class MUST be immediately followed by a call to
 * this function like the one above.  Failure to do so results in
 * inaccurate class typing information, which kinda defeats the whole
 * point, ya know?
 *
 * @param self The @c obj instance.
 * @param cls  The @c class for the given instance.
 * @return     #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 * @ingroup DOXGRP_OBJ
 */
spif_bool_t
spif_obj_set_class(spif_obj_t self, spif_class_t cls)
{
    ASSERT_RVAL(!SPIF_OBJ_ISNULL(self), FALSE);
    self->cls = cls;
    return TRUE;
}

/*@}*/



/**
 * @defgroup DOXGRP_OBJ LibAST Object Infrastructure
 *
 * This group of types, functions, and pre-processor macros implements
 * a mechanism for defining and utilizing objects in native C.
 *
 * C, as you well know, is a procedural language.  It has no native
 * facilities for doing object-oriented programming.  And thusly was
 * born C++ -- native object facilities with much of the same C syntax
 * we all know and love.  But C++ has one very big (and fatal, IMHO)
 * flaw:  it requires a special compiler.
 *
 * That in and of itself is not the end of the world, but it does
 * create a number of issues in terms of portability, standardization,
 * speed, and efficiency.  Since C has been around for so much longer,
 * most C compilers are very stable and reliable, and their
 * optimization routines often do almost as good a job as writing raw
 * assembly code (particularly the vendor compilers).  C++ offers none
 * of these types of advantages, and C++ compiler availability has
 * historically been sketchy at best.
 *
 * There are really 2 possible solutions to this, both accomplishing
 * the same end result (using the native C compiler to manipulate an
 * object model and hierarchy) in two similar, but distinct, ways.
 * Both approaches require the use of some sort of preprocessor.
 * Option #1 would be to use a dedicated preprocessor, either
 * something like m4 or a new creation.  This option would probably
 * allow for cleaner, more native-looking syntax, but it has similar
 * (and potentially worse) portability problems to those of C++.
 *
 * For these reasons, I chose option #2:  a CPP-based (i.e.,
 * macro-based) object model.  As you might imagine, the syntax and
 * usage of such a model bears almost no resemblence whatsoever to
 * that of a native OO language, as it relies heavily on type-casting
 * and namespace safety measures implemented within the existing C/CPP
 * structure.  However, the resultant code is native C, which means
 * you can manipulate data using OO techniques like inheritance,
 * interface classes, etc., without incurring the speed/portability
 * penalties of using the C++ compiler.  Plus, you can build libraries
 * that can be easily linked to both C and C++ programs.
 *
 * If you'd like to see a sample program which demonstrates creation
 * and management of LibAST objects, please look here:
 * @link obj_example.c @endlink.
 */

/**
 * @example obj_example.c
 * Example code for using the LibAST Object Infrastructure
 *
 * This is a contrived, but informational, example of using LibAST's
 * object system.  MORE HERE
 *
 * Here's the complete source code:
 */
