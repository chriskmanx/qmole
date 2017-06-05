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

#ifndef _LIBAST_DLINKED_LIST_H_
#define _LIBAST_DLINKED_LIST_H_

/* Standard typecast macros.... */
#define SPIF_DLINKED_LIST_ITEM(obj)                 (SPIF_CAST(dlinked_list_item) (obj))
#define SPIF_DLINKED_LIST(obj)                      (SPIF_CAST(dlinked_list) (obj))

#define SPIF_DLINKED_LIST_ITEM_ISNULL(o)            (SPIF_DLINKED_LIST_ITEM(o) == SPIF_NULL_TYPE(dlinked_list_item))
#define SPIF_OBJ_IS_DLINKED_LIST_ITEM(o)            (SPIF_OBJ_IS_TYPE((o), dlinked_list_item))

SPIF_DECL_OBJ(dlinked_list_item) {
    SPIF_DECL_PROPERTY(obj, data);
    SPIF_DECL_PROPERTY(dlinked_list_item, prev);
    SPIF_DECL_PROPERTY(dlinked_list_item, next);
};

SPIF_DECL_OBJ(dlinked_list) {
    SPIF_DECL_PARENT_TYPE(obj);
    SPIF_DECL_PROPERTY(listidx, len);
    SPIF_DECL_PROPERTY(dlinked_list_item, head);
    SPIF_DECL_PROPERTY(dlinked_list_item, tail);
};

extern spif_listclass_t SPIF_LISTCLASS_VAR(dlinked_list);
extern spif_vectorclass_t SPIF_VECTORCLASS_VAR(dlinked_list);
extern spif_mapclass_t SPIF_MAPCLASS_VAR(dlinked_list);

#endif /* _LIBAST_DLINKED_LIST_H_ */
