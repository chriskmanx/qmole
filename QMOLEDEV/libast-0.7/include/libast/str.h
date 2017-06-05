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

#ifndef _LIBAST_STR_H_
#define _LIBAST_STR_H_

#define SPIF_STR(obj)                    ((spif_str_t) (obj))
#define SPIF_STR_CLASS(o)                (SPIF_CAST(strclass) SPIF_OBJ_CLASS(o))
#define SPIF_OBJ_IS_STR(o)               (SPIF_OBJ_IS_TYPE(o, str))
#define SPIF_STR_ISNULL(s)               SPIF_OBJ_ISNULL(SPIF_OBJ(s))
#define SPIF_STRCLASS_VAR(type)          spif_ ## type ## _strclass
#define SPIF_STR_CALL_METHOD(o, meth)    SPIF_STR_CLASS(o)->meth

#define SPIF_STR_NEW(type)           SPIF_STR((SPIF_CLASS(SPIF_CLASS_VAR(type)))->noo())
#define SPIF_STR_INIT(obj)           SPIF_OBJ_INIT(obj)
#define SPIF_STR_DONE(obj)           SPIF_OBJ_DONE(obj)
#define SPIF_STR_DEL(obj)            SPIF_OBJ_DEL(obj)
#define SPIF_STR_SHOW(obj, b, i)     SPIF_OBJ_SHOW(obj, b, i)
#define SPIF_STR_COMP(o1, o2)        SPIF_OBJ_COMP(o1, o2)
#define SPIF_STR_DUP(obj)            SPIF_OBJ_DUP(obj)
#define SPIF_STR_TYPE(obj)           SPIF_OBJ_TYPE(obj)

#define SPIF_STR_NEW_FROM_PTR(type, p)          SPIF_STR((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_ptr)(p))
#define SPIF_STR_NEW_FROM_BUFF(type, b, s)      SPIF_STR((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_buff)(b, s))
#define SPIF_STR_NEW_FROM_FP(type, fp)          SPIF_STR((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_fp)(fp))
#define SPIF_STR_NEW_FROM_FD(type, fd)          SPIF_STR((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_fd)(fd))
#define SPIF_STR_NEW_FROM_NUM(type, num)        SPIF_STR((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_num)(num))
#define SPIF_STR_INIT_FROM_PTR(o, p)            SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), init_from_ptr)(o, p))
#define SPIF_STR_INIT_FROM_BUFF(o, b, s)        SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), init_from_buff)(o, b, s))
#define SPIF_STR_INIT_FROM_FP(o, fp)            SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), init_from_fp)(o, fp))
#define SPIF_STR_INIT_FROM_FD(o, fd)            SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), init_from_fd)(o, fd))
#define SPIF_STR_INIT_FROM_NUM(o, num)          SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), init_from_num)(o, num))
#define SPIF_STR_APPEND(o, x)                   SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), append)(o, x))
#define SPIF_STR_APPEND_CHAR(o, x)              SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), append_char)(o, x))
#define SPIF_STR_APPEND_FROM_PTR(o, x)          SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), append_from_ptr)(o, x))
#define SPIF_STR_CASECMP(o, x)                  SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), casecmp)(o, x))
#define SPIF_STR_CASECMP_WITH_PTR(o, x)         SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), casecmp_with_ptr)(o, x))
#define SPIF_STR_CLEAR(o, x)                    SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), clear)(o, x))
#define SPIF_STR_CMP(o, x)                      SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), cmp)(o, x))
#define SPIF_STR_CMP_WITH_PTR(o, x)             SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), cmp_with_ptr)(o, x))
#define SPIF_STR_DOWNCASE(o)                    SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), downcase)(o))
#define SPIF_STR_FIND(o, x)                     SPIF_CAST(stridx) (SPIF_STR_CALL_METHOD((o), find)(o, x))
#define SPIF_STR_FIND_FROM_PTR(o, x)            SPIF_CAST(stridx) (SPIF_STR_CALL_METHOD((o), find_from_ptr)(o, x))
#define SPIF_STR_INDEX(o, x)                    SPIF_CAST(stridx) (SPIF_STR_CALL_METHOD((o), index)(o, x))
#define SPIF_STR_NCASECMP(o, x, n)              SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), ncasecmp)(o, x, n))
#define SPIF_STR_NCASECMP_WITH_PTR(o, x, n)     SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), ncasecmp_with_ptr)(o, x, n))
#define SPIF_STR_NCMP(o, x, n)                  SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), ncmp)(o, x, n))
#define SPIF_STR_NCMP_WITH_PTR(o, x, n)         SPIF_CAST(cmp) (SPIF_STR_CALL_METHOD((o), ncmp_with_ptr)(o, x, n))
#define SPIF_STR_PREPEND(o, x)                  SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), prepend)(o, x))
#define SPIF_STR_PREPEND_CHAR(o, x)             SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), prepend_char)(o, x))
#define SPIF_STR_PREPEND_FROM_PTR(o, x)         SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), prepend_from_ptr)(o, x))
#define SPIF_STR_REVERSE(o)                     SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), reverse)(o))
#define SPIF_STR_RINDEX(o, x)                   SPIF_CAST(stridx) (SPIF_STR_CALL_METHOD((o), rindex)(o, x))
#define SPIF_STR_SPLICE(o, n1, n2, x)           SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), splice)(o, n1, n2, x))
#define SPIF_STR_SPLICE_FROM_PTR(o, n1, n2, x)  SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), splice_from_ptr)(o, n1, n2, x))
#define SPIF_STR_SUBSTR(o, n1, n2)              SPIF_CAST(str) (SPIF_STR_CALL_METHOD((o), substr)(o, n1, n2))
#define SPIF_STR_SUBSTR_TO_PTR(o, n1, n2)       SPIF_CAST(charptr) (SPIF_STR_CALL_METHOD((o), substr_to_ptr)(o, n1, n2))
#define SPIF_STR_TO_FLOAT(o)                    SPIF_CAST_C(double) (SPIF_STR_CALL_METHOD((o), to_float)(o))
#define SPIF_STR_TO_NUM(o, x)                   SPIF_CAST_C(size_t) (SPIF_STR_CALL_METHOD((o), to_num)(o, x))
#define SPIF_STR_TRIM(o)                        SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), trim)(o))
#define SPIF_STR_UPCASE(o)                      SPIF_CAST(bool) (SPIF_STR_CALL_METHOD((o), upcase)(o))

#define SPIF_STR_STR(obj)  (SPIF_CONST_CAST(charptr) ((SPIF_STR_ISNULL(obj)) \
                                                      ? (SPIF_CAST(charptr) "") \
                                                      : (SPIF_STR(obj)->s)))
typedef spif_int64_t spif_stridx_t;

SPIF_DECL_OBJ(str) {
    SPIF_DECL_PARENT_TYPE(obj);
    spif_charptr_t s;
    SPIF_DECL_PROPERTY_C(spif_stridx_t, size);
    SPIF_DECL_PROPERTY_C(spif_stridx_t, len);
};

SPIF_DECL_OBJ(strclass) {
    SPIF_DECL_PARENT_TYPE(class);

    spif_func_t new_from_ptr;
    spif_func_t new_from_buff;
    spif_func_t new_from_fp;
    spif_func_t new_from_fd;
    spif_func_t new_from_num;
    spif_func_t init_from_ptr;
    spif_func_t init_from_buff;
    spif_func_t init_from_fp;
    spif_func_t init_from_fd;
    spif_func_t init_from_num;
    spif_func_t append;
    spif_func_t append_char;
    spif_func_t append_from_ptr;
    spif_func_t casecmp;
    spif_func_t casecmp_with_ptr;
    spif_func_t clear;
    spif_func_t cmp;
    spif_func_t cmp_with_ptr;
    spif_func_t downcase;
    spif_func_t find;
    spif_func_t find_from_ptr;
    spif_func_t index;
    spif_func_t ncasecmp;
    spif_func_t ncasecmp_with_ptr;
    spif_func_t ncmp;
    spif_func_t ncmp_with_ptr;
    spif_func_t prepend;
    spif_func_t prepend_char;
    spif_func_t prepend_from_ptr;
    spif_func_t reverse;
    spif_func_t rindex;
    spif_func_t splice;
    spif_func_t splice_from_ptr;
    spif_func_t substr;
    spif_func_t substr_to_ptr;
    spif_func_t to_float;
    spif_func_t to_num;
    spif_func_t trim;
    spif_func_t upcase;
};

extern spif_class_t SPIF_CLASS_VAR(str);
extern spif_strclass_t SPIF_STRCLASS_VAR(str);
extern spif_str_t spif_str_new(void);
extern spif_str_t spif_str_new_from_ptr(spif_charptr_t);
extern spif_str_t spif_str_new_from_buff(spif_charptr_t, spif_stridx_t);
extern spif_str_t spif_str_new_from_fp(FILE *);
extern spif_str_t spif_str_new_from_fd(int);
extern spif_str_t spif_str_new_from_num(long);
extern spif_bool_t spif_str_del(spif_str_t);
extern spif_bool_t spif_str_init(spif_str_t);
extern spif_bool_t spif_str_init_from_ptr(spif_str_t, spif_charptr_t);
extern spif_bool_t spif_str_init_from_buff(spif_str_t, spif_charptr_t, spif_stridx_t);
extern spif_bool_t spif_str_init_from_fp(spif_str_t, FILE *);
extern spif_bool_t spif_str_init_from_fd(spif_str_t, int);
extern spif_bool_t spif_str_init_from_num(spif_str_t, long);
extern spif_bool_t spif_str_done(spif_str_t);
extern spif_str_t spif_str_show(spif_str_t, spif_charptr_t, spif_str_t, size_t);
extern spif_cmp_t spif_str_comp(spif_str_t, spif_str_t);
extern spif_str_t spif_str_dup(spif_str_t);
extern spif_classname_t spif_str_type(spif_str_t);

extern spif_bool_t spif_str_append(spif_str_t, spif_str_t);
extern spif_bool_t spif_str_append_char(spif_str_t, spif_char_t);
extern spif_bool_t spif_str_append_from_ptr(spif_str_t, spif_charptr_t);
extern spif_cmp_t spif_str_casecmp(spif_str_t, spif_str_t);
extern spif_cmp_t spif_str_casecmp_with_ptr(spif_str_t, spif_charptr_t);
extern spif_bool_t spif_str_clear(spif_str_t, spif_char_t);
extern spif_cmp_t spif_str_cmp(spif_str_t, spif_str_t);
extern spif_cmp_t spif_str_cmp_with_ptr(spif_str_t, spif_charptr_t);
extern spif_bool_t spif_str_downcase(spif_str_t);
extern spif_stridx_t spif_str_find(spif_str_t, spif_str_t);
extern spif_stridx_t spif_str_find_from_ptr(spif_str_t, spif_charptr_t);
extern spif_stridx_t spif_str_index(spif_str_t, spif_char_t);
extern spif_cmp_t spif_str_ncasecmp(spif_str_t, spif_str_t, spif_stridx_t);
extern spif_cmp_t spif_str_ncasecmp_with_ptr(spif_str_t, spif_charptr_t, spif_stridx_t);
extern spif_cmp_t spif_str_ncmp(spif_str_t, spif_str_t, spif_stridx_t);
extern spif_cmp_t spif_str_ncmp_with_ptr(spif_str_t, spif_charptr_t, spif_stridx_t);
extern spif_bool_t spif_str_prepend(spif_str_t, spif_str_t);
extern spif_bool_t spif_str_prepend_char(spif_str_t, spif_char_t);
extern spif_bool_t spif_str_prepend_from_ptr(spif_str_t, spif_charptr_t);
extern spif_bool_t spif_str_reverse(spif_str_t);
extern spif_stridx_t spif_str_rindex(spif_str_t, spif_char_t);
extern spif_bool_t spif_str_splice(spif_str_t, spif_stridx_t, spif_stridx_t, spif_str_t);
extern spif_bool_t spif_str_splice_from_ptr(spif_str_t, spif_stridx_t, spif_stridx_t, spif_charptr_t);
extern spif_str_t spif_str_substr(spif_str_t, spif_stridx_t, spif_stridx_t);
extern spif_charptr_t spif_str_substr_to_ptr(spif_str_t, spif_stridx_t, spif_stridx_t);
extern double spif_str_to_float(spif_str_t);
extern size_t spif_str_to_num(spif_str_t, int);
extern spif_bool_t spif_str_trim(spif_str_t);
extern spif_bool_t spif_str_upcase(spif_str_t);
SPIF_DECL_PROPERTY_FUNC_C(str, spif_stridx_t, size);
SPIF_DECL_PROPERTY_FUNC_C(str, spif_stridx_t, len);

#endif /* _LIBAST_STR_H_ */
