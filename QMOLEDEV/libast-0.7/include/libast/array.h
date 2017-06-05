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

#ifndef _LIBAST_ARRAY_H_
#define _LIBAST_ARRAY_H_

/* Standard typecast macros.... */
#define SPIF_ARRAY(obj)                      (SPIF_CAST(array) (obj))

#define SPIF_ARRAY_ISNULL(o)                 (SPIF_ARRAY(o) == SPIF_NULL_TYPE(array))
#define SPIF_OBJ_IS_ARRAY(o)                 (SPIF_OBJ_IS_TYPE((o), array))

SPIF_DECL_OBJ(array) {
    SPIF_DECL_PARENT_TYPE(obj);
    spif_listidx_t len;
    spif_obj_t *items;
};

extern spif_listclass_t SPIF_LISTCLASS_VAR(array);
extern spif_vectorclass_t SPIF_VECTORCLASS_VAR(array);
extern spif_mapclass_t SPIF_MAPCLASS_VAR(array);

#endif /* _LIBAST_ARRAY_H_ */
