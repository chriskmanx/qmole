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

#ifndef _LIBAST_MAP_IF_H_
#define _LIBAST_MAP_IF_H_

/*
 * interface goop
 */

/* Standard typecast macros.... */
#define SPIF_MAP(o)                                    (SPIF_CAST(map) (o))
#define SPIF_MAP_CLASS(o)                              (SPIF_CAST(mapclass) SPIF_OBJ_CLASS(o))

/* Name of class variable associated with map interface */
#define SPIF_MAPCLASS_VAR(type)                        spif_ ## type ## _mapclass

/* Check if a map is NULL */
#define SPIF_MAP_ISNULL(o)                             (SPIF_MAP(o) == SPIF_NULL_TYPE(map))

/* Check if an object is a map */
#define SPIF_OBJ_IS_MAP(o)                             SPIF_OBJ_IS_TYPE(o, map)

/* Call a method on an instance of an implementation class */
#define SPIF_MAP_CALL_METHOD(o, meth)                  SPIF_MAP_CLASS(o)->meth

/* Calls to the basic functions. */
#define SPIF_MAP_NEW(type)                             SPIF_MAP((SPIF_CLASS(SPIF_MAPCLASS_VAR(type)))->noo())
#define SPIF_MAP_INIT(o)                               SPIF_OBJ_INIT(o)
#define SPIF_MAP_DONE(o)                               SPIF_OBJ_DONE(o)
#define SPIF_MAP_DEL(o)                                SPIF_OBJ_DEL(o)
#define SPIF_MAP_SHOW(o, b, i)                         SPIF_OBJ_SHOW(o, b, i)
#define SPIF_MAP_COMP(o1, o2)                          SPIF_OBJ_COMP(o1, o2)
#define SPIF_MAP_DUP(o)                                SPIF_OBJ_DUP(o)
#define SPIF_MAP_TYPE(o)                               SPIF_OBJ_TYPE(o)

#define SPIF_MAP_COUNT(o)                              SPIF_CAST_C(size_t) ((SPIF_MAP_CALL_METHOD((o), count))(o))
#define SPIF_MAP_GET(o, key)                           SPIF_CAST(obj) ((SPIF_MAP_CALL_METHOD((o), get))(o, key))
#define SPIF_MAP_GET_KEYS(o, l)                        SPIF_CAST(list) ((SPIF_MAP_CALL_METHOD((o), get_keys))(o, l))
#define SPIF_MAP_GET_PAIRS(o, l)                       SPIF_CAST(list) ((SPIF_MAP_CALL_METHOD((o), get_pairs))(o, l))
#define SPIF_MAP_GET_VALUES(o, l)                      SPIF_CAST(list) ((SPIF_MAP_CALL_METHOD((o), get_values))(o, l))
#define SPIF_MAP_HAS_KEY(o, key)                       SPIF_CAST(bool) ((SPIF_MAP_CALL_METHOD((o), has_key))(o, key))
#define SPIF_MAP_HAS_VALUE(o, value)                   SPIF_CAST(bool) ((SPIF_MAP_CALL_METHOD((o), has_value))(o, value))
#define SPIF_MAP_ITERATOR(o)                           SPIF_CAST(iterator) ((SPIF_MAP_CALL_METHOD((o), iterator))(o))
#define SPIF_MAP_REMOVE(o, item)                       SPIF_CAST(obj) ((SPIF_MAP_CALL_METHOD((o), remove))(o, item))
#define SPIF_MAP_SET(o, key, value)                    SPIF_CAST(bool) ((SPIF_MAP_CALL_METHOD((o), set))(o, key, value))

typedef spif_obj_t spif_map_t;

SPIF_DECL_OBJ(mapclass) {
    SPIF_DECL_PARENT_TYPE(class);

    spif_func_t count;
    spif_func_t get;
    spif_func_t get_keys;
    spif_func_t get_pairs;
    spif_func_t get_values;
    spif_func_t has_key;
    spif_func_t has_value;
    spif_func_t iterator;
    spif_func_t remove;
    spif_func_t set;
};

#endif /* _LIBAST_MAP_IF_H_ */
