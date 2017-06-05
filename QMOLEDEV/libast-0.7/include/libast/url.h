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

#ifndef _LIBAST_URL_H_
#define _LIBAST_URL_H_

/* Cast an arbitrary object pointer to a url. */
#define SPIF_URL(o)                      (SPIF_CAST(url) (o))

/* Check to see if a pointer references an url. */
#define SPIF_OBJ_IS_URL(o)               (SPIF_OBJ_IS_TYPE(o, url))

/* Used for testing the NULL-ness of urls. */
#define SPIF_URL_ISNULL(o)               (SPIF_URL(o) == SPIF_NULL_TYPE(url))

/* Calls to the basic functions. */
#define SPIF_URL_NEW()                   SPIF_CAST(url) (SPIF_CLASS(SPIF_CLASS_VAR(url)))->(noo)()
#define SPIF_URL_INIT(o)                 SPIF_OBJ_INIT(o)
#define SPIF_URL_DONE(o)                 SPIF_OBJ_DONE(o)
#define SPIF_URL_DEL(o)                  SPIF_OBJ_DEL(o)
#define SPIF_URL_SHOW(o, b, i)           SPIF_OBJ_SHOW(o, b, i)
#define SPIF_URL_COMP(o1, o2)            SPIF_OBJ_COMP(o)
#define SPIF_URL_DUP(o)                  SPIF_OBJ_DUP(o)
#define SPIF_URL_TYPE(o)                 SPIF_OBJ_TYPE(o)


SPIF_DECL_OBJ(url) {
    SPIF_DECL_PARENT_TYPE(str);
    SPIF_DECL_PROPERTY(str, proto);
    SPIF_DECL_PROPERTY(str, user);
    SPIF_DECL_PROPERTY(str, passwd);
    SPIF_DECL_PROPERTY(str, host);
    SPIF_DECL_PROPERTY(str, port);
    SPIF_DECL_PROPERTY(str, path);
    SPIF_DECL_PROPERTY(str, query);
};

extern spif_class_t SPIF_CLASS_VAR(url);
extern spif_url_t spif_url_new(void);
extern spif_url_t spif_url_new_from_str(spif_str_t);
extern spif_url_t spif_url_new_from_ptr(spif_charptr_t);
extern spif_bool_t spif_url_del(spif_url_t);
extern spif_bool_t spif_url_init(spif_url_t);
extern spif_bool_t spif_url_init_from_str(spif_url_t, spif_str_t);
extern spif_bool_t spif_url_init_from_ptr(spif_url_t, spif_charptr_t);
extern spif_bool_t spif_url_done(spif_url_t);
extern spif_str_t spif_url_show(spif_url_t, spif_charptr_t, spif_str_t, size_t);
extern spif_cmp_t spif_url_comp(spif_url_t, spif_url_t);
extern spif_url_t spif_url_dup(spif_url_t);
extern spif_classname_t spif_url_type(spif_url_t);
SPIF_DECL_PROPERTY_FUNC(url, str, proto);
SPIF_DECL_PROPERTY_FUNC(url, str, user);
SPIF_DECL_PROPERTY_FUNC(url, str, passwd);
SPIF_DECL_PROPERTY_FUNC(url, str, host);
SPIF_DECL_PROPERTY_FUNC(url, str, port);
SPIF_DECL_PROPERTY_FUNC(url, str, path);
SPIF_DECL_PROPERTY_FUNC(url, str, query);
extern spif_bool_t spif_url_unparse(spif_url_t);

#endif /* _LIBAST_URL_H_ */
