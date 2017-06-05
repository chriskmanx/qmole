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

#ifndef _LIBAST_OBJ_H_
#define _LIBAST_OBJ_H_


/**
 * @file obj.h
 * LibAST Object Infrastructure -- Generic Objects
 *
 * This file contains macros and type definitions for creating and
 * manipulating basic generic objects.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.30 $
 * $Date: 2004/12/15 00:00:21 $
 */

/*@{*/
/**
 * @name Object Definition and Declaration Macros
 * ---
 *
 * This set of macros is intended to abstract certain details about
 * the internal workings of objects and greatly simplify their
 * definition.  The results have been known to cause non-cpp-compliant
 * parsers to have the occasional fit, but they work. :-)
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Declare an object structure.
 *
 * This macro abstracts the actual name of the object structure to
 * help prevent its direct use.  Obviously the translation is plainly
 * visible, so those sufficiently determined to do it the Wrong Way
 * still can.  This macro is not used directly, but rather as part of
 * the SPIF_DECL_OBJ() macro (see below), which is in turn used to
 * define object types.
 *
 * @param t The object type as a non-quoted string (e.g., obj).
 * @return  The @c struct keyword followed by the structure name for
 *          the specified object type.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_DECL_OBJ(), SPIF_DECL_TYPE()
 */
#define SPIF_DECL_OBJ_STRUCT(t)  struct spif_ ## t ## _t_struct

/**
 * Declare an object type based on the structure definition immediately
 * following.
 *
 * This macro simplifies the creation of a new class by adding the
 * appropriate @c typedef along with introducing the structure
 * definition.  Although use of this macro presents a bit of an
 * unnatural parsing problem for non-cpp-aware tools (e.g., indent),
 * its use is recommended for encapsulation purposes.  Invocation of
 * this macro should be immediately followed by an open brace ('{')
 * and a normal C-style structure definition.
 *
 * @param t The object type as a non-quoted string (e.g., obj).
 * @return  An appropriate @c typedef and @c struct introducer.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_DECL_TYPE(), SPIF_DECL_OBJ_STRUCT()
 */
#define SPIF_DECL_OBJ(t)  SPIF_DECL_TYPE(t, SPIF_DECL_OBJ_STRUCT(t)); SPIF_DECL_OBJ_STRUCT(t)
/**
 * Declare the parent type of an object being defined.
 *
 * This macro is used to declare that a particular object type (e.g.,
 * obj) is the parent type of an object which is being defined via
 * SPIF_DECL_OBJ().  The call to this macro should @em immediately
 * follow the opening brace (which, in turn, immediately follows the
 * call to SPIF_DECL_OBJ()).  Declaring the parent type allows for
 * safe typecasting of any object of the current type to its parent
 * type (or any parent type in its "family tree").  At minimum, any
 * true object must at @em least be derived from the @c obj type so
 * that it can be safely cast to a generic object.
 *
 * @param t The object type as a non-quoted string (e.g., obj).
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_CONST_TYPE()
 */
#define SPIF_DECL_PARENT_TYPE(t)  SPIF_CONST_TYPE(t) parent

/**
 * Declare the class variable for objects of a given type.
 *
 * Every object type has a class variable which is declared using this
 * macro.  Any instance of that type of object contains a pointer to
 * this same class variable.  This class variable is both declared and
 * referenced using this macro so that its actual name is abstracted.
 *
 * @param type The object type as a non-quoted string (e.g., obj).
 * @return     The class variable for the given type.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 */
#define SPIF_CLASS_VAR(type)  spif_ ## type ## _class

/**
 * Declare a "property" of an object.
 *
 * This macro is used when declaring a class to specify that a
 * particular class object is a "property."  A "property" is an object
 * member which has public get/set methods of the same name.
 *
 * @param type The type of the property variable.
 * @param name The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 */
#define SPIF_DECL_PROPERTY(type, name)  SPIF_TYPE(type) name

/**
 * Declare a "property" of an object.
 *
 * This macro is identical to SPIF_DECL_PROPERTY(), except that a
 * native C type is used.
 *
 * @param type The C type of the property variable.
 * @param name The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_DECL_PROPERTY()
 */
#define SPIF_DECL_PROPERTY_C(type, name)  type name

/**
 * Declare the get/set methods of a "property" of an object.
 *
 * This macro is used to prototype the get/set methods of an object
 * property.
 *
 * @param otype The type of the object.
 * @param vtype The type of the property variable.
 * @param name  The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 */
#define SPIF_DECL_PROPERTY_FUNC(otype, vtype, name)  \
  SPIF_TYPE(vtype) spif_ ## otype ## _get_ ## name (SPIF_TYPE(otype)); \
  SPIF_TYPE(bool) spif_ ## otype ## _set_ ## name (SPIF_TYPE(otype), SPIF_TYPE(vtype))

/**
 * Declare the get/set methods of a "property" of an object.
 *
 * This macro is identical to SPIF_DECL_PROPERTY_FUNC(), except that a
 * native C type is used.
 *
 * @param otype The type of the object.
 * @param vtype The C type of the property variable.
 * @param name  The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_DECL_PROPERTY_FUNC()
 */
#define SPIF_DECL_PROPERTY_FUNC_C(otype, vtype, name)  \
  vtype spif_ ## otype ## _get_ ## name (SPIF_TYPE(otype)); \
  SPIF_TYPE(bool) spif_ ## otype ## _set_ ## name (SPIF_TYPE(otype), vtype)

/**
 * Define the get/set methods of a "property" of an object.
 *
 * This macro is used to define (i.e., create, i.e. insert the code
 * for) the get/set methods of an object property.
 *
 * @param otype The type of the object.
 * @param vtype The type of the property variable.
 * @param name  The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 */
#define SPIF_DEFINE_PROPERTY_FUNC(otype, vtype, name)  \
  SPIF_TYPE(vtype) spif_ ## otype ## _get_ ## name (SPIF_TYPE(otype) self) \
    { return (self->name); } \
  SPIF_TYPE(bool) spif_ ## otype ## _set_ ## name (SPIF_TYPE(otype) self, SPIF_TYPE(vtype) new_ ## name) \
    { \
        if (!SPIF_OBJ_ISNULL(self->name)) { \
            SPIF_OBJ_DEL(self->name); \
        } \
        self->name = new_ ## name; \
        return TRUE; \
    }

/**
 * Define the get/set methods of a "property" of an object.
 *
 * This macro is identical to SPIF_DEFINE_PROPERTY_FUNC(), except that
 * the property is treated as a non-object (i.e., its current value is
 * not checked, so no destructor is called for the current value).
 * Use this for spif_*_t types that are not objects, such as
 * spif_int32_t and spif_sockport_t.
 *
 * @param otype The type of the object.
 * @param vtype The type of the property variable.
 * @param name  The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_DEFINE_PROPERTY_FUNC()
 */
#define SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(otype, vtype, name)  \
  SPIF_TYPE(vtype) spif_ ## otype ## _get_ ## name (SPIF_TYPE(otype) self) \
    { return (self->name); } \
  SPIF_TYPE(bool) spif_ ## otype ## _set_ ## name (SPIF_TYPE(otype) self, SPIF_TYPE(vtype) new_ ## name) \
    { \
        self->name = new_ ## name; \
        return TRUE; \
    }

/**
 * Define the get/set methods of a "property" of an object.
 *
 * This macro is identical to SPIF_DEFINE_PROPERTY_FUNC(), except that
 * a native C type is used.
 *
 * @param otype The type of the object.
 * @param vtype The C type of the property variable.
 * @param name  The name of the property.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_DEFINE_PROPERTY_FUNC()
 */
#define SPIF_DEFINE_PROPERTY_FUNC_C(otype, vtype, name)  \
  vtype spif_ ## otype ## _get_ ## name (SPIF_TYPE(otype) self) \
    { return (self->name); } \
  SPIF_TYPE(bool) spif_ ## otype ## _set_ ## name (SPIF_TYPE(otype) self, vtype new_ ## name) \
    { \
        self->name = new_ ## name; \
        return TRUE; \
    }
/*@}*/

/*@{*/
/**
 * @name Generic Object/Class Casting Macros
 * ---
 *
 * This set of macros allows objects of any type (i.e., any object
 * descended from the basic "obj" or "class" type) to be treated as
 * generic object/class variables.  In other words, as long as an
 * object is properly defined (i.e., properly descended from its basic
 * type), it can be treated as that basic type via use of these
 * macros.  The end result is that any operation which is defined for
 * all objects can be executed on an object of any type without the
 * need to know its exact type.
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Cast an arbitrary class object to the generic class type.
 *
 * Most non-interface classes use the generic obj-style class
 * variable, containing only the basic methods (create, delete,
 * compare, etc.) that every object needs.  However, interface classes
 * require the use of more specific class objects with support for
 * additional object methods.  This macro allows an arbitrary class
 * object, be it the generic class type or a decendent thereof, to be
 * cast to the generic class type.  This allows basic method calls to
 * be performed on complex types by treating them as simple objects.
 *
 * @param cls An arbitrary class object.
 * @return    The class object cast to the generic type.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_CLASS(), SPIF_CAST()
 */
#define SPIF_CLASS(cls)                  (SPIF_CAST(class) (cls))

/**
 * Cast an arbitrary class object to a const generic class type.
 *
 * This macro is equivalent to SPIF_CLASS(), except that the resulting
 * object is a @c const object.
 *
 * @param cls An arbitrary class object.
 * @return    The class object cast to the generic type.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_CLASS(), SPIF_CONST_CAST()
 */
#define SPIF_CONST_CLASS(cls)            (SPIF_CONST_CAST(class) (cls))

/**
 * Cast an arbitrary object to an obj.
 *
 * This macro allows an arbitrary object of any valid object type
 * (i.e., anything derived from spif_obj_t) to be treated as a generic
 * object (the spif_obj_t type).  Any method defined for generic
 * objects (i.e., all objects) can be called on any object using a
 * typecast such as this, and any function or macro which needs an
 * arbitrary object can be passed any object using this macro.
 *
 * @param o An object of any valid type.
 * @return  A generic object reference to that object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_CAST()
 */
#define SPIF_OBJ(o)                    (SPIF_CAST(obj) (o))
/*@}*/

/*@{*/
/**
 * @name Generic Object Type Safety Macros
 * ---
 *
 * Macros in this group are used to verify the validity of an object
 * instance and its type.
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Determine if an arbitrary object is of type "obj."
 *
 * This macro returns a boolean value based on whether or not the
 * object passed to it is of type "obj."  Obviously, this will almost
 * never been the case, since the actual obj type is too generic to be
 * useful for anything other than a parent class.  However, each
 * object type needs to define a macro like this named
 * SPIF_OBJ_IS_xxx() (where xxx is the object type), so this macro
 * serves as a template for how those should be written.
 *
 * This type-verification class for each object type should always be
 * defined as this one is -- simply a wrapper around
 * SPIF_OBJ_IS_TYPE().
 *
 * @param o The object to test.
 * @return  Whether or not the object is of type "obj."
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_IS_TYPE()
 */
#define SPIF_OBJ_IS_OBJ(o)               (SPIF_OBJ_IS_TYPE(o, obj))

/**
 * Determine if an object of type "obj" is NULL.
 *
 * This macro returns a boolean value based on whether or not the
 * object passed to it is NULL.  The object is cast to type "obj"
 * before the comparison, so it should work for any valid object.
 * This macro will not often be used by user code.  However, each
 * object type needs to define a macro like this named
 * SPIF_xxx_ISNULL() (where xxx is the object type), so this macro
 * serves as a template for how those should be written.
 *
 * @param o The object to test.
 * @return  Whether or not the object is NULL.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ(), SPIF_NULL_TYPE()
 */
#define SPIF_OBJ_ISNULL(o)               (SPIF_OBJ(o) == SPIF_NULL_TYPE(obj))

/**
 * Determine if an object is of a given type.
 *
 * This macro returns a boolean value based on whether or not the
 * given object, @a o, is of type @a type.  It provides the driving
 * force behind all SPIF_OBJ_IS_xxx()-style macros.  Unlike the
 * SPIF_OBJ_CHECK_TYPE() macro, this macro will @em always verify the
 * object type.  If you need a debugging assertion instead, use
 * SPIF_OBJ_CHECK_TYPE(), as it will resolve to @c (1) in
 * non-debugging code.
 *
 * @param o    The object to test.
 * @param type The type to check for.
 * @return     Whether or not @a o is of type @a type.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_ISNULL(), SPIF_OBJ_CLASS(),
 *      SPIF_CLASS_VAR(), SPIF_OBJ_CHECK_TYPE()
 */
#define SPIF_OBJ_IS_TYPE(o, type)        ((!SPIF_OBJ_ISNULL(o)) && (SPIF_OBJ_CLASS(o) == SPIF_CLASS(SPIF_CLASS_VAR(type))))

/**
 * Provide debugging assertion that an object is of a given type.
 *
 * This macro returns a boolean value based on whether or not the
 * given object, @a o, is of type @a type.  It is intended for
 * situations where the test only needs to be performed in debugging
 * code.  If DEBUG is 0, it will resolve to @c (1).  If DEBUG is
 * between 1 and 4 inclusive, it will simply be a NULL check.  Higher
 * debug levels will treat it just like SPIF_OBJ_IS_TYPE().
 *
 * @param o    The object to test.
 * @param type The type to check for.
 * @return     Whether or not @a o is of type @a type.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_ISNULL(), SPIF_OBJ_IS_TYPE()
 */
#if DEBUG == 0
#  define SPIF_OBJ_CHECK_TYPE(o, type)   (1)
#elif DEBUG <= 4
#  define SPIF_OBJ_CHECK_TYPE(o, type)   (!SPIF_OBJ_ISNULL(o))
#else
#  define SPIF_OBJ_CHECK_TYPE(o, type)   SPIF_OBJ_IS_TYPE(o, type)
#endif
/*@}*/

/*@{*/
/**
 * @name Generic Object Instance Macros
 * ---
 *
 * This set of macros manipulates actual object instances in various
 * ways.  They can be used on any object.
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Access the class for a given object.
 *
 * Every object has a member variable that references its class.
 * There is a single class object for each individual object type, and
 * all instances of that type reference the same class object.  If you
 * know the type of an object, you can use SPIF_CLASS_VAR() to access
 * its class object (i.e., the class object for that type).  However,
 * this macro can be used to access the class object of @em any
 * object, regardless of whether or not you know its type.
 *
 * @note This macro returns an object of type spif_class_t, so only
 * methods common to all objects can be called using this macro.
 * Other class types should define their own SPIF_xxx_CLASS() macro to
 * access methods specific to that class.
 *
 * @param obj An object of arbitrary/unknown type.
 * @return    The class object for the given object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_CLASS(), SPIF_OBJ()
 */
#define SPIF_OBJ_CLASS(obj)              (SPIF_CLASS(SPIF_OBJ(obj)->cls))

/**
 * Obtain the string representation of an object's class name.
 *
 * This macro will access the classname for an object.  The classname
 * will be enclosed in exclamation marks ('!') so as to help identify
 * its origin.  It is most often used as the return value for the
 * @c type method of a given object type.
 *
 * @param obj An object of arbitrary/unknown type.
 * @return    The class name (as a spif_classname_t) for the given
 *            object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_CLASS()
 */
#define SPIF_OBJ_CLASSNAME(obj)          (SPIF_CAST(classname) SPIF_OBJ_CLASS(obj))

/**
 * Call the named method for a given object.
 *
 * The methods which can be called on a given object are defined by
 * that object's class.  Since all objects are derived from
 * spif_obj_t, and all classes are derived from spif_class_t (the
 * class type for "obj"), the methods defined by spif_class_t can be
 * called on any arbitrary object, regardless of its actual
 * object/class types.  This macro provides the mechanism by which
 * this is done.
 *
 * @note This macro should not be called directly.  It is used by the
 * SPIF_OBJ_*() macros and as a template for interface classes.
 *
 * @param obj  An object of arbitrary/unknown type.
 * @param meth The name of the method to call.
 * @return     A pointer to the specified method for that object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_CLASS()
 */
#define SPIF_OBJ_CALL_METHOD(obj, meth)  SPIF_OBJ_CLASS(obj)->meth

/**
 * Create an instance of a generic object.
 *
 * This macro allocates and returns an object of type "obj."  Almost
 * never used, but it's here for demonstration purposes anyway.
 *
 * @return An allocated object of type "obj."
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_new()
 */
#define SPIF_OBJ_NEW()                   SPIF_CAST(obj) (SPIF_CLASS(SPIF_CLASS_VAR(obj)))->(noo)()

/**
 * Initialize an object.
 *
 * This macro calls the @c init method of an object in order to
 * initialize it.
 *
 * @param o An already-allocated object.
 * @return  #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_init()
 */
#define SPIF_OBJ_INIT(o)                 SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), init)(o))

/**
 * Clean up an object.
 *
 * This macro calls the @c done method of an object.  This basically
 * restores it to its original allocated-and-initialized state.
 *
 * @param o An object.
 * @return  #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_done()
 */
#define SPIF_OBJ_DONE(o)                 SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), done)(o))

/**
 * Delete an object.
 *
 * This macro calls the @c del method of an object, destroying it and
 * freeing its memory.
 *
 * @param o An object.  It will cease to exist after this call.
 * @return  #TRUE if successful, #FALSE otherwise.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_del()
 */
#define SPIF_OBJ_DEL(o)                  SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), del)(o))

/**
 * Convert the contents of an object to a string.
 *
 * This macro calls the @c show method of an object, returning a
 * spif_str_t object containing its string representation.
 *
 * @param o The object to display.
 * @param b An existing spif_str_t buffer to use.  If NULL, a new str
 *          object will be created and returned.
 * @param i Number of leading spaces to indent.
 * @return  A str object containing the string representation of @a o.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_show(), SPIF_SHOW()
 */
#define SPIF_OBJ_SHOW(o, b, i)           SPIF_CAST(str) (SPIF_OBJ_CALL_METHOD((o), show)(o, #o, b, i))

/**
 * Compare two objects.
 *
 * This macro calls the @c comp method of object #1 to compare the two
 * objects.
 *
 * @param o1 Object #1.
 * @param o2 Object #2.
 * @return   A spif_cmp_t value containing the comparison result.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_comp(), spif_comp_t
 */
#define SPIF_OBJ_COMP(o1, o2)            SPIF_CAST(cmp) (SPIF_OBJ_CALL_METHOD((o1),  comp)(o1, o2))

/**
 * Duplicate an object.
 *
 * This macro calls the @c dup method of an object.  A copy of the
 * object is returned.
 *
 * @param o An object.
 * @return  A duplicate of that object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_dup()
 */
#define SPIF_OBJ_DUP(o)                  SPIF_CAST(obj) (SPIF_OBJ_CALL_METHOD((o), dup)(o))

/**
 * Obtain the type of the object.
 *
 * This macro calls the @c type method of an object to obtain its
 * classname.
 *
 * @param o An object.
 * @return  The classname of that object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_type(), SPIF_OBJ_CLASSNAME()
 */
#define SPIF_OBJ_TYPE(o)                 SPIF_CAST(classname) (SPIF_OBJ_CALL_METHOD((o), type)(o))
/*@}*/


/*@{*/
/**
 * @name Object Display Convenience Macros
 * ---
 *
 * This set of macros simplifies the process of displaying (as a
 * string) the contents of an object.  They can be used on any object.
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Convenience macro for displaying an object.
 *
 * This macro provides an easy way to output the string
 * representation of an object to a given file descriptor.  The macro
 * itself handles the creation and deletion of the temporary @c str
 * object required (hence the "convenience").
 *
 * @param o  The object to display.
 * @param fd The file descriptor to display it on.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_show()
 */
#define SPIF_SHOW(o, fd)                 do { \
                                           spif_str_t tmp__; \
                                           tmp__ = SPIF_OBJ_SHOW(o, SPIF_NULL_TYPE(str), 0); \
                                           fprintf(fd, "%s", SPIF_STR_STR(tmp__)); \
                                           spif_str_del(tmp__); \
                                         } while (0)

/**
 * Convenience macro for displaying a NULL value for an object.
 *
 * Obviously, one cannot invoke the @c show method of a NULL object.
 * This macro exists to provide a uniform way for object @c show
 * methods to easily (and uniformly) display NULL objects.
 *
 * @param t   The type of the NULL object.
 * @param n   The name of the NULL object (variable name).
 * @param b   A str object to which to assign the result.
 * @param i   Number of spaces to indent.
 * @param tmp A char[] buffer of fixed size used for temporary
 *            storage.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_show()
 */
#define SPIF_OBJ_SHOW_NULL(t, n, b, i, tmp)  do { \
                                               memset(tmp, ' ', (i)); \
                                               snprintf(SPIF_CAST_C(char *) tmp + (i), sizeof(tmp) - (i), \
                                                        "(spif_" #t "_t) %s:  " SPIF_NULLSTR_TYPE(t) "\n", \
                                                        NONULL(n)); \
                                               if (SPIF_STR_ISNULL(b)) { \
                                                 (b) = spif_str_new_from_ptr(SPIF_CAST(charptr) tmp); \
                                               } else { \
                                                 spif_str_append_from_ptr((b), SPIF_CAST(charptr) tmp); \
                                               } \
                                             } while (0)

/**
 * Convenience macro for handling NULL objects in a comparison.
 *
 * This macro exists because I got tired of typing the same thing over
 * and over again to handle comparisons where either object may be
 * NULL.  You should have this at the start of all of your *_comp()
 * functions. 
 *
 * @param s   The "self" (first) object.
 * @param o   The "other" (second) object.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, spif_obj_comp()
 */
#define SPIF_OBJ_COMP_CHECK_NULL(s, o) do { \
                                           if (SPIF_OBJ_ISNULL((s)) && SPIF_OBJ_ISNULL((o))) { \
                                               return SPIF_CMP_EQUAL; \
                                           } else if (SPIF_OBJ_ISNULL((s))) { \
                                               return SPIF_CMP_LESS; \
                                           } else if (SPIF_OBJ_ISNULL((o))) { \
                                               return SPIF_CMP_GREATER; \
                                           } \
                                       } while (0)
/*@}*/


/*@{*/
/**
 * @name Basic Object Class Definitions
 * ---
 *
 * These types form the foundation of the LibAST object hierarchy.
 *
 * @ingroup DOXGRP_OBJ
 */

/**
 * Object class structure.
 *
 * This class contains the object class structure.  It contains the
 * string representation of the class name followed by a series of
 * function pointers to the member functions for the class.  The basic
 * class type contains methods that all objects require.  The
 * structure members should not be accessed directly, but rather via
 * the appropriate macros.
 *
 * @note Doxygen doesn't understand how to handle the macro-based
 * class definition for this structure, so it thinks it's a function.
 * It's actually a struct definition (spif_const_class_t) and a
 * pointer definition (spif_class_t) as provided by the
 * SPIF_DEFINE_OBJ() macro.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink, SPIF_OBJ_CALL_METHOD()
 */
SPIF_DECL_OBJ(class) {
    /** Text representation of class name. */
    spif_classname_t classname;

    spif_func_t noo;
    spif_func_t init;
    spif_func_t done;
    spif_func_t del;
    spif_func_t show;
    spif_func_t comp;
    spif_func_t dup;
    spif_func_t type;
};

/* An obj is the most basic object type.  It contains simply a pointer to
   the class name (a const char * so you can test it with ==). */

/**
 * Generic object structure.
 *
 * The @c obj type is the parent of all other object types.  Since it
 * doesn't actually store any data, the only member of the @c obj
 * type is its spif_class_t 
 *
 * @note Doxygen doesn't understand how to handle the macro-based
 * class definition for this structure, so it thinks it's a function.
 * It's actually a struct definition (spif_const_obj_t) and a pointer
 * definition (spif_obj_t) as provided by the SPIF_DEFINE_OBJ()
 * macro.
 *
 * @see @link DOXGRP_OBJ LibAST Object Infrastructure @endlink
 */
SPIF_DECL_OBJ(obj) {
    spif_class_t cls;
};
/*@}*/



/* We need typedef's from here... */
#include <libast/str.h>

extern spif_class_t SPIF_CLASS_VAR(obj);
extern spif_obj_t spif_obj_new(void);
extern spif_bool_t spif_obj_del(spif_obj_t);
extern spif_bool_t spif_obj_init(spif_obj_t);
extern spif_bool_t spif_obj_done(spif_obj_t);
extern spif_class_t spif_obj_get_class(spif_obj_t);
extern spif_bool_t spif_obj_set_class(spif_obj_t, spif_class_t);
extern spif_str_t spif_obj_show(spif_obj_t, spif_charptr_t, spif_str_t, size_t);
extern spif_cmp_t spif_obj_comp(spif_obj_t, spif_obj_t);
extern spif_obj_t spif_obj_dup(spif_obj_t);
extern spif_classname_t spif_obj_type(spif_obj_t);

#endif /* _LIBAST_OBJ_H_ */
