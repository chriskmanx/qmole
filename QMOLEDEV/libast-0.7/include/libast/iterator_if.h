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

#ifndef _LIBAST_ITERATOR_IF_H_
#define _LIBAST_ITERATOR_IF_H_

/*
 * interface goop
 */

/* Standard typecast macros.... */
#define SPIF_ITERATOR(o)                                    (SPIF_CAST(iterator) (o))
#define SPIF_ITERATOR_CLASS(o)                              (SPIF_CAST(iteratorclass) SPIF_OBJ_CLASS(o))

/* Name of class variable associated with iterator interface */
#define SPIF_ITERATORCLASS_VAR(type)                        spif_ ## type ## _iteratorclass

/* Check if an iterator is NULL */
#define SPIF_ITERATOR_ISNULL(o)                             (SPIF_ITERATOR(o) == SPIF_NULL_TYPE(iterator))

/* Check if an object is an iterator */
#define SPIF_OBJ_IS_ITERATOR(o)                             SPIF_OBJ_IS_TYPE(o, iterator)

/* Call a method on an instance of an implementation class */
#define SPIF_ITERATOR_CALL_METHOD(o, meth)                  SPIF_ITERATOR_CLASS(o)->meth

/* Calls to the basic functions. */
#define SPIF_ITERATOR_NEW(type)                             SPIF_ITERATOR((SPIF_CLASS(SPIF_ITERATORCLASS_VAR(type)))->noo())
#define SPIF_ITERATOR_INIT(o)                               SPIF_OBJ_INIT(o)
#define SPIF_ITERATOR_DONE(o)                               SPIF_OBJ_DONE(o)
#define SPIF_ITERATOR_DEL(o)                                SPIF_OBJ_DEL(o)
#define SPIF_ITERATOR_SHOW(o, b, i)                         SPIF_OBJ_SHOW(o, b, i)
#define SPIF_ITERATOR_COMP(o1, o2)                          SPIF_OBJ_COMP(o1, o2)
#define SPIF_ITERATOR_DUP(o)                                SPIF_OBJ_DUP(o)
#define SPIF_ITERATOR_TYPE(o)                               SPIF_OBJ_TYPE(o)

#define SPIF_ITERATOR_HAS_NEXT(o)                           SPIF_CAST(bool) ((SPIF_ITERATOR_CALL_METHOD((o), has_next))(o))
#define SPIF_ITERATOR_NEXT(o)                               SPIF_CAST(obj) ((SPIF_ITERATOR_CALL_METHOD((o), next))(o))

typedef spif_obj_t spif_iterator_t;

SPIF_DECL_OBJ(iteratorclass) {
    SPIF_DECL_PARENT_TYPE(class);

    spif_func_t has_next;
    spif_func_t next;
};

#endif /* _LIBAST_ITERATOR_IF_H_ */
