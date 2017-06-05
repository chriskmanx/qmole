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

#ifndef _LIBAST_LIST_IF_H_
#define _LIBAST_LIST_IF_H_

/*
 * interface goop
 */

/* Standard typecast macros.... */
#define SPIF_LIST(o)                                    (SPIF_CAST(list) (o))
#define SPIF_LIST_CLASS(o)                              (SPIF_CAST(listclass) SPIF_OBJ_CLASS(o))

/* Name of class variable associated with list interface */
#define SPIF_LISTCLASS_VAR(type)                        spif_ ## type ## _listclass

/* Check if a list is NULL */
#define SPIF_LIST_ISNULL(o)                             (SPIF_LIST(o) == SPIF_NULL_TYPE(list))

/* Check if an object is a list */
#define SPIF_OBJ_IS_LIST(o)                             SPIF_OBJ_IS_TYPE(o, list)

/* Call a method on an instance of an implementation class */
#define SPIF_LIST_CALL_METHOD(o, meth)                  SPIF_LIST_CLASS(o)->meth

/* Calls to the basic functions. */
#define SPIF_LIST_NEW(type)                             SPIF_LIST((SPIF_CLASS(SPIF_LISTCLASS_VAR(type)))->noo())
#define SPIF_LIST_INIT(o)                               SPIF_OBJ_INIT(o)
#define SPIF_LIST_DONE(o)                               SPIF_OBJ_DONE(o)
#define SPIF_LIST_DEL(o)                                SPIF_OBJ_DEL(o)
#define SPIF_LIST_SHOW(o, b, i)                         SPIF_OBJ_SHOW(o, b, i)
#define SPIF_LIST_COMP(o1, o2)                          SPIF_OBJ_COMP(o1, o2)
#define SPIF_LIST_DUP(o)                                SPIF_OBJ_DUP(o)
#define SPIF_LIST_TYPE(o)                               SPIF_OBJ_TYPE(o)

#define SPIF_LIST_APPEND(o, item)                       SPIF_CAST(bool) ((SPIF_LIST_CALL_METHOD((o), append))(o, item))
#define SPIF_LIST_CONTAINS(o, item)                     SPIF_CAST(bool) ((SPIF_LIST_CALL_METHOD((o), contains))(o, item))
#define SPIF_LIST_COUNT(o)                              SPIF_CAST_C(size_t) ((SPIF_LIST_CALL_METHOD((o), count))(o))
#define SPIF_LIST_FIND(o, item)                         SPIF_CAST(obj) ((SPIF_LIST_CALL_METHOD((o), find))(o, item))
#define SPIF_LIST_GET(o, index)                         SPIF_CAST(obj) ((SPIF_LIST_CALL_METHOD((o), get))(o, index))
#define SPIF_LIST_INDEX(o, item)                        SPIF_CAST_C(size_t) ((SPIF_LIST_CALL_METHOD((o), index))(o, item))
#define SPIF_LIST_INSERT(o, item)                       SPIF_CAST(bool) ((SPIF_LIST_CALL_METHOD((o), insert))(o, item))
#define SPIF_LIST_INSERT_AT(o, item, index)             SPIF_CAST(bool) ((SPIF_LIST_CALL_METHOD((o), insert_at))(o, item, index))
#define SPIF_LIST_ITERATOR(o)                           SPIF_CAST(iterator) ((SPIF_LIST_CALL_METHOD((o), iterator))(o))
#define SPIF_LIST_PREPEND(o, item)                      SPIF_CAST(bool) ((SPIF_LIST_CALL_METHOD((o), prepend))(o, item))
#define SPIF_LIST_REMOVE(o, item)                       SPIF_CAST(obj) ((SPIF_LIST_CALL_METHOD((o), remove))(o, item))
#define SPIF_LIST_REMOVE_AT(o, index)                   SPIF_CAST(obj) ((SPIF_LIST_CALL_METHOD((o), remove_at))(o, index))
#define SPIF_LIST_REVERSE(o)                            SPIF_CAST(bool) ((SPIF_LIST_CALL_METHOD((o), reverse))(o))
#define SPIF_LIST_TO_ARRAY(o)                           SPIF_CAST_PTR(obj) ((SPIF_LIST_CALL_METHOD((o), to_array))(o))

typedef spif_obj_t spif_list_t;
typedef spif_int32_t spif_listidx_t;

SPIF_DECL_OBJ(listclass) {
    SPIF_DECL_PARENT_TYPE(class);

    spif_func_t append;
    spif_func_t contains;
    spif_func_t count;
    spif_func_t find;
    spif_func_t get;
    spif_func_t index;
    spif_func_t insert;
    spif_func_t insert_at;
    spif_func_t iterator;
    spif_func_t prepend;
    spif_func_t remove;
    spif_func_t remove_at;
    spif_func_t reverse;
    spif_func_t to_array;
};

#endif /* _LIBAST_LIST_IF_H_ */
