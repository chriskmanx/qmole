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

#ifndef _LIBAST_REGEXP_H_
#define _LIBAST_REGEXP_H_

/* Cast an arbitrary object pointer to a regexp. */
#define SPIF_REGEXP(obj)                ((spif_regexp_t) (obj))

/* Check to see if a pointer references a regexping object. */
#define SPIF_OBJ_IS_REGEXP(obj)         (SPIF_OBJ_IS_TYPE(obj, regexp))

/* Check for NULL regexp object */
#define SPIF_REGEXP_ISNULL(s)           SPIF_OBJ_ISNULL(SPIF_OBJ(s))

#define SPIF_REGEXP_NEW(type)           SPIF_REGEXP((SPIF_CLASS(SPIF_CLASS_VAR(type)))->noo())
#define SPIF_REGEXP_INIT(obj)           SPIF_OBJ_INIT(obj)
#define SPIF_REGEXP_DONE(obj)           SPIF_OBJ_DONE(obj)
#define SPIF_REGEXP_DEL(obj)            SPIF_OBJ_DEL(obj)
#define SPIF_REGEXP_SHOW(obj, b, i)     SPIF_OBJ_SHOW(obj, b, i)
#define SPIF_REGEXP_COMP(o1, o2)        SPIF_OBJ_COMP(o1, o2)
#define SPIF_REGEXP_DUP(obj)            SPIF_OBJ_DUP(obj)
#define SPIF_REGEXP_TYPE(obj)           SPIF_OBJ_TYPE(obj)

SPIF_DECL_OBJ(regexp) {
    SPIF_DECL_PARENT_TYPE(str);
    spif_ptr_t data;
    int flags;
};

extern spif_class_t SPIF_CLASS_VAR(regexp);
extern spif_regexp_t spif_regexp_new(void);
extern spif_regexp_t spif_regexp_new_from_str(spif_str_t);
extern spif_regexp_t spif_regexp_new_from_ptr(spif_charptr_t);
extern spif_bool_t spif_regexp_del(spif_regexp_t);
extern spif_bool_t spif_regexp_init(spif_regexp_t);
extern spif_bool_t spif_regexp_init_from_str(spif_regexp_t, spif_str_t);
extern spif_bool_t spif_regexp_init_from_ptr(spif_regexp_t, spif_charptr_t);
extern spif_bool_t spif_regexp_done(spif_regexp_t);
extern spif_regexp_t spif_regexp_dup(spif_regexp_t);
extern spif_cmp_t spif_regexp_comp(spif_regexp_t, spif_regexp_t);
extern spif_str_t spif_regexp_show(spif_regexp_t, spif_charptr_t, spif_str_t, size_t);
extern spif_classname_t spif_regexp_type(spif_regexp_t);
extern spif_bool_t spif_regexp_compile(spif_regexp_t);
extern spif_bool_t spif_regexp_matches_str(spif_regexp_t, spif_str_t);
extern spif_bool_t spif_regexp_matches_ptr(spif_regexp_t self, spif_charptr_t subject);
extern int spif_regexp_get_flags(spif_regexp_t);
extern spif_bool_t spif_regexp_set_flags(spif_regexp_t, spif_charptr_t);

#endif /* _LIBAST_REGEXP_H_ */
