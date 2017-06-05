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

#ifndef _LIBAST_VECTOR_IF_H_
#define _LIBAST_VECTOR_IF_H_

/*
 * interface goop
 */

/* Standard typecast macros.... */
#define SPIF_VECTOR(o)                                    (SPIF_CAST(vector) (o))
#define SPIF_VECTOR_CLASS(o)                              (SPIF_CAST(vectorclass) SPIF_OBJ_CLASS(o))

/* Name of class variable associated with vector interface */
#define SPIF_VECTORCLASS_VAR(type)                        spif_ ## type ## _vectorclass

/* Check if a vector is NULL */
#define SPIF_VECTOR_ISNULL(o)                             (SPIF_VECTOR(o) == SPIF_NULL_TYPE(vector))

/* Check if an object is a vector */
#define SPIF_OBJ_IS_VECTOR(o)                             SPIF_OBJ_IS_TYPE(o, vector)

/* Call a method on an instance of an implementation class */
#define SPIF_VECTOR_CALL_METHOD(o, meth)                  SPIF_VECTOR_CLASS(o)->meth

/* Calls to the basic functions. */
#define SPIF_VECTOR_NEW(type)                             SPIF_VECTOR((SPIF_CLASS(SPIF_VECTORCLASS_VAR(type)))->noo())
#define SPIF_VECTOR_INIT(o)                               SPIF_OBJ_INIT(o)
#define SPIF_VECTOR_DONE(o)                               SPIF_OBJ_DONE(o)
#define SPIF_VECTOR_DEL(o)                                SPIF_OBJ_DEL(o)
#define SPIF_VECTOR_SHOW(o, b, i)                         SPIF_OBJ_SHOW(o, b, i)
#define SPIF_VECTOR_COMP(o1, o2)                          SPIF_OBJ_COMP(o1, o2)
#define SPIF_VECTOR_DUP(o)                                SPIF_OBJ_DUP(o)
#define SPIF_VECTOR_TYPE(o)                               SPIF_OBJ_TYPE(o)

#define SPIF_VECTOR_CONTAINS(o, item)                     SPIF_CAST(bool) ((SPIF_VECTOR_CALL_METHOD((o), contains))(o, item))
#define SPIF_VECTOR_COUNT(o)                              SPIF_CAST_C(size_t) ((SPIF_VECTOR_CALL_METHOD((o), count))(o))
#define SPIF_VECTOR_FIND(o, item)                         SPIF_CAST(obj) ((SPIF_VECTOR_CALL_METHOD((o), find))(o, item))
#define SPIF_VECTOR_INSERT(o, item)                       SPIF_CAST(bool) ((SPIF_VECTOR_CALL_METHOD((o), insert))(o, item))
#define SPIF_VECTOR_ITERATOR(o)                           SPIF_CAST(iterator) ((SPIF_VECTOR_CALL_METHOD((o), iterator))(o))
#define SPIF_VECTOR_REMOVE(o, item)                       SPIF_CAST(obj) ((SPIF_VECTOR_CALL_METHOD((o), remove))(o, item))
#define SPIF_VECTOR_TO_ARRAY(o)                           SPIF_CAST_PTR(obj) ((SPIF_VECTOR_CALL_METHOD((o), to_array))(o))

typedef spif_obj_t spif_vector_t;

SPIF_DECL_OBJ(vectorclass) {
    SPIF_DECL_PARENT_TYPE(class);

    spif_func_t contains;
    spif_func_t count;
    spif_func_t find;
    spif_func_t insert;
    spif_func_t iterator;
    spif_func_t remove;
    spif_func_t to_array;
};

#endif /* _LIBAST_VECTOR_IF_H_ */
