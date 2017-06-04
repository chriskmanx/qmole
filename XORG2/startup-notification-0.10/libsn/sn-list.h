/* List abstraction used internally */
/* 
 * Copyright (C) 2002 Red Hat, Inc.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


#ifndef __SN_LIST_H__
#define __SN_LIST_H__

#include <libsn/sn-util.h>

SN_BEGIN_DECLS

/* FIXME use sn_internal prefix for all this */

typedef struct SnList SnList;

typedef sn_bool_t (* SnListForeachFunc) (void *value, void *data);

SnList* sn_list_new     (void);
void    sn_list_free    (SnList            *list);
void    sn_list_prepend (SnList            *list,
                         void              *data);
void    sn_list_append  (SnList            *list,
                         void              *data);
void    sn_list_remove  (SnList            *list,
                         void              *data);
void    sn_list_foreach (SnList            *list,
                         SnListForeachFunc  func,
                         void              *data);
sn_bool_t sn_list_empty (SnList            *list);

SN_END_DECLS

#endif /* __SN_LIST_H__ */
