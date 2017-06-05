/*
 * Copyvalue (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * values to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyvalue notice and this permission notice shall be included in
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

#ifndef _LIBAST_OBJPAIR_H_
#define _LIBAST_OBJPAIR_H_


/**
 * @file objpair.h
 * LibAST Object Infrastructure -- Paired Objects
 *
 * This file contains the declarations for the paired object type.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.3 $
 * $Date: 2004/03/01 19:22:48 $
 */

/*@{*/
/**
 * @name Paired Object Macros
 * ---
 *
 * This set of macros 
 *
 * @ingroup DOXGRP_OBJPAIR
 */

/**
 * Cast an arbitrary object to an objpair.
 *
 * This macro allows an arbitrary object of any valid object type
 * (i.e., anything derived from spif_objpair_t) to be treated as a generic
 * objpair (the spif_objpair_t type).
 *
 * @param o An objpair, or a descendent thereof.
 * @return  An objpair reference to that object.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, SPIF_CAST()
 */
#define SPIF_OBJPAIR(o)                    (SPIF_CAST(objpair) (o))

/**
 * Determine if an arbitrary object is of type "objpair."
 *
 * This macro returns a boolean value based on whether or not the
 * object passed to it is of type "objpair."
 *
 * @param o The object to test.
 * @return  Whether or not the object is of type "objpair."
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, SPIF_OBJ_IS_TYPE()
 */
#define SPIF_OBJ_IS_OBJPAIR(o)             (SPIF_OBJ_IS_TYPE(o, objpair))

/**
 * Determine if an object of type "objpair" is NULL.
 *
 * This macro returns a boolean value based on whether or not the
 * object passed to it is NULL.  The object is cast to type "objpair"
 * before the comparison, so it should work for any valid object.
 * This macro will not often be used by user code.  However, each
 * object type needs to define a macro like this named
 * SPIF_xxx_ISNULL() (where xxx is the object type), so this macro
 * serves as a template for how those should be written.
 *
 * @param o The object to test.
 * @return  Whether or not the object is NULL.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, SPIF_OBJPAIR(), SPIF_NULL_TYPE()
 */
#define SPIF_OBJPAIR_ISNULL(o)             (SPIF_OBJPAIR(o) == SPIF_NULL_TYPE(objpair))

/**
 * Create an instance of a objpair.
 *
 * This macro allocates and returns an object of type "objpair."
 *
 * @return An allocated object of type "objpair."
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_new()
 */
#define SPIF_OBJPAIR_NEW()                   SPIF_CAST(objpair) (SPIF_CLASS(SPIF_CLASS_VAR(objpair)))->(noo)()

/**
 * Initialize an objpair.
 *
 * This macro calls the @c init method of an objpair in order to
 * initialize it.
 *
 * @param o An already-allocated objpair.
 * @return  #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_init()
 */
#define SPIF_OBJPAIR_INIT(o)                 SPIF_CAST(bool) (SPIF_OBJPAIR_CALL_METHOD((o), init)(o))

/**
 * Clean up an objpair.
 *
 * This macro calls the @c done method of an objpair.  This basically
 * restores it to its original allocated-and-initialized state.
 *
 * @param o An objpair.
 * @return  #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_done()
 */
#define SPIF_OBJPAIR_DONE(o)                 SPIF_CAST(bool) (SPIF_OBJPAIR_CALL_METHOD((o), done)(o))

/**
 * Delete an objpair.
 *
 * This macro calls the @c del method of an objpair, destroying it and
 * freeing its memory.
 *
 * @param o An objpair.  It will cease to exist after this call.
 * @return  #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_del()
 */
#define SPIF_OBJPAIR_DEL(o)                  SPIF_CAST(bool) (SPIF_OBJPAIR_CALL_METHOD((o), del)(o))

/**
 * Convert the contents of an objpair to a string.
 *
 * This macro calls the @c show method of an objpair, returning a
 * spif_str_t object containing its string representation.
 *
 * @param o The objpair to display.
 * @param b An existing spif_str_t buffer to use.  If NULL, a new str
 *          object will be created and returned.
 * @param i Number of leading spaces to indent.
 * @return  A str object containing the string representation of @a o.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_show(), SPIF_SHOW()
 */
#define SPIF_OBJPAIR_SHOW(o, b, i)           SPIF_CAST(str) (SPIF_OBJPAIR_CALL_METHOD((o), show)(o, #o, b, i))

/**
 * Compare two objpairs.
 *
 * This macro calls the @c comp method of objpair #1 to compare the two
 * objpairs.
 *
 * @param o1 Objpair #1.
 * @param o2 Objpair #2.
 * @return   A spif_cmp_t value containing the comparison result.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_comp(), spif_comp_t
 */
#define SPIF_OBJPAIR_COMP(o1, o2)            SPIF_CAST(cmp) (SPIF_OBJPAIR_CALL_METHOD((o1),  comp)(o1, o2))

/**
 * Duplicate an objpair.
 *
 * This macro calls the @c dup method of an objpair.  A copy of the
 * objpair is returned.
 *
 * @param o An objpair.
 * @return  A duplicate of that objpair.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_dup()
 */
#define SPIF_OBJPAIR_DUP(o)                  SPIF_CAST(objpair) (SPIF_OBJPAIR_CALL_METHOD((o), dup)(o))

/**
 * Obtain the type of the objpair.
 *
 * This macro calls the @c type method of an objpair to obtain its
 * classname.
 *
 * @param o An objpair.
 * @return  The classname of that objpair.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, spif_objpair_type(), SPIF_OBJPAIR_CLASSNAME()
 */
#define SPIF_OBJPAIR_TYPE(o)                 SPIF_CAST(classname) (SPIF_OBJPAIR_CALL_METHOD((o), type)(o))


/**
 * Objpair structure.
 *
 * This class contains the objpair structure.  It contains the
 * parent type (obj) and all member variables.
 *
 * @note Doxygen doesn't understand how to handle the macro-based
 * class definition for this structure, so it thinks it's a function.
 * It's actually a struct definition (spif_const_objpair_t) and a
 * pointer definition (spif_objpair_t) as provided by the
 * SPIF_DECL_OBJ() macro.
 *
 * @see @link DOXGRP_OBJPAIR Paired Objects @endlink, SPIF_OBJPAIR_CALL_METHOD()
 */
SPIF_DECL_OBJ(objpair) {
    SPIF_DECL_PARENT_TYPE(obj);
    SPIF_DECL_PROPERTY(obj, key);
    SPIF_DECL_PROPERTY(obj, value);
};
/*@}*/

extern spif_class_t SPIF_CLASS_VAR(objpair);
extern spif_objpair_t spif_objpair_new(void);
extern spif_objpair_t spif_objpair_new_from_key(spif_obj_t key);
extern spif_objpair_t spif_objpair_new_from_value(spif_obj_t value);
extern spif_objpair_t spif_objpair_new_from_both(spif_obj_t key, spif_obj_t value);
extern spif_bool_t spif_objpair_del(spif_objpair_t self);
extern spif_bool_t spif_objpair_init(spif_objpair_t self);
extern spif_bool_t spif_objpair_init_from_key(spif_objpair_t self, spif_obj_t key);
extern spif_bool_t spif_objpair_init_from_value(spif_objpair_t self, spif_obj_t value);
extern spif_bool_t spif_objpair_init_from_both(spif_objpair_t self, spif_obj_t key, spif_obj_t value);
extern spif_bool_t spif_objpair_done(spif_objpair_t self);
extern spif_str_t spif_objpair_show(spif_objpair_t self, spif_charptr_t name, spif_str_t buff, size_t indent);
extern spif_cmp_t spif_objpair_comp(spif_objpair_t self, spif_obj_t other);
extern spif_objpair_t spif_objpair_dup(spif_objpair_t self);
extern spif_classname_t spif_objpair_type(spif_objpair_t self);
SPIF_DECL_PROPERTY_FUNC(objpair, obj, key);
SPIF_DECL_PROPERTY_FUNC(objpair, obj, value);

#endif /* _LIBAST_OBJPAIR_H_ */
