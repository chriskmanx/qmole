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

#ifndef _LIBAST_MBUFF_H_
#define _LIBAST_MBUFF_H_

#define SPIF_MBUFF(obj)                    ((spif_mbuff_t) (obj))
#define SPIF_MBUFF_CLASS(o)                (SPIF_CAST(mbuffclass) SPIF_OBJ_CLASS(o))
#define SPIF_OBJ_IS_MBUFF(o)               (SPIF_OBJ_IS_TYPE(o, mbuff))
#define SPIF_MBUFF_ISNULL(s)               SPIF_OBJ_ISNULL(SPIF_OBJ(s))
#define SPIF_MBUFFCLASS_VAR(type)          spif_ ## type ## _mbuffclass
#define SPIF_MBUFF_CALL_METHOD(o, meth)    SPIF_MBUFF_CLASS(o)->meth

#define SPIF_MBUFF_NEW(type)           SPIF_MBUFF((SPIF_CLASS(SPIF_CLASS_VAR(type)))->noo())
#define SPIF_MBUFF_INIT(obj)           SPIF_OBJ_INIT(obj)
#define SPIF_MBUFF_DONE(obj)           SPIF_OBJ_DONE(obj)
#define SPIF_MBUFF_DEL(obj)            SPIF_OBJ_DEL(obj)
#define SPIF_MBUFF_SHOW(obj, b, i)     SPIF_OBJ_SHOW(obj, b, i)
#define SPIF_MBUFF_COMP(o1, o2)        SPIF_OBJ_COMP(o1, o2)
#define SPIF_MBUFF_DUP(obj)            SPIF_OBJ_DUP(obj)
#define SPIF_MBUFF_TYPE(obj)           SPIF_OBJ_TYPE(obj)

#define SPIF_MBUFF_NEW_FROM_PTR(type, p)          SPIF_MBUFF((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_ptr)(p))
#define SPIF_MBUFF_NEW_FROM_BUFF(type, b, s)      SPIF_MBUFF((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_buff)(b, s))
#define SPIF_MBUFF_NEW_FROM_FP(type, fp)          SPIF_MBUFF((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_fp)(fp))
#define SPIF_MBUFF_NEW_FROM_FD(type, fd)          SPIF_MBUFF((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_fd)(fd))
#define SPIF_MBUFF_NEW_FROM_NUM(type, num)        SPIF_MBUFF((SPIF_CLASS(SPIF_CLASS_VAR(type)))->new_from_num)(num))
#define SPIF_MBUFF_INIT_FROM_PTR(o, p)            SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), init_from_ptr)(o, p))
#define SPIF_MBUFF_INIT_FROM_BUFF(o, b, s)        SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), init_from_buff)(o, b, s))
#define SPIF_MBUFF_INIT_FROM_FP(o, fp)            SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), init_from_fp)(o, fp))
#define SPIF_MBUFF_INIT_FROM_FD(o, fd)            SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), init_from_fd)(o, fd))
#define SPIF_MBUFF_INIT_FROM_NUM(o, num)          SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), init_from_num)(o, num))
#define SPIF_MBUFF_APPEND(o, x)                   SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), append)(o, x))
#define SPIF_MBUFF_APPEND_CHAR(o, x)              SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), append_char)(o, x))
#define SPIF_MBUFF_APPEND_FROM_PTR(o, x)          SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), append_from_ptr)(o, x))
#define SPIF_MBUFF_CLEAR(o, x)                    SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), clear)(o, x))
#define SPIF_MBUFF_CMP(o, x)                      SPIF_CAST(cmp) (SPIF_OBJ_CALL_METHOD((o), cmp)(o, x))
#define SPIF_MBUFF_CMP_WITH_PTR(o, x)             SPIF_CAST(cmp) (SPIF_OBJ_CALL_METHOD((o), cmp_with_ptr)(o, x))
#define SPIF_MBUFF_FIND(o, x)                     SPIF_CAST(memidx) (SPIF_OBJ_CALL_METHOD((o), find)(o, x))
#define SPIF_MBUFF_FIND_FROM_PTR(o, x)            SPIF_CAST(memidx) (SPIF_OBJ_CALL_METHOD((o), find_from_ptr)(o, x))
#define SPIF_MBUFF_INDEX(o, x)                    SPIF_CAST(memidx) (SPIF_OBJ_CALL_METHOD((o), index)(o, x))
#define SPIF_MBUFF_NCMP(o, x, n)                  SPIF_CAST(cmp) (SPIF_OBJ_CALL_METHOD((o), ncmp)(o, x, n))
#define SPIF_MBUFF_NCMP_WITH_PTR(o, x, n)         SPIF_CAST(cmp) (SPIF_OBJ_CALL_METHOD((o), ncmp_with_ptr)(o, x, n))
#define SPIF_MBUFF_PREPEND(o, x)                  SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), prepend)(o, x))
#define SPIF_MBUFF_PREPEND_CHAR(o, x)             SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), prepend_char)(o, x))
#define SPIF_MBUFF_PREPEND_FROM_PTR(o, x)         SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), prepend_from_ptr)(o, x))
#define SPIF_MBUFF_REVERSE(o)                     SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), reverse)(o))
#define SPIF_MBUFF_RINDEX(o, x)                   SPIF_CAST(memidx) (SPIF_OBJ_CALL_METHOD((o), rindex)(o, x))
#define SPIF_MBUFF_SPLICE(o, n1, n2, x)           SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), splice)(o, n1, n2, x))
#define SPIF_MBUFF_SPLICE_FROM_PTR(o, n1, n2, x)  SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), splice_from_ptr)(o, n1, n2, x))
#define SPIF_MBUFF_SUBBUFF(o, n1, n2)             SPIF_CAST(mbuff) (SPIF_OBJ_CALL_METHOD((o), subbuff)(o, n1, n2))
#define SPIF_MBUFF_SUBBUFF_TO_PTR(o, n1, n2)      SPIF_CAST(byteptr) (SPIF_OBJ_CALL_METHOD((o), subbuff_to_ptr)(o, n1, n2))
#define SPIF_MBUFF_TRIM(o)                        SPIF_CAST(bool) (SPIF_OBJ_CALL_METHOD((o), trim)(o))

#define SPIF_MBUFF_BUFF(obj)  (SPIF_CONST_CAST(byteptr) ((SPIF_MBUFF_ISNULL(obj)) \
                                                         ? (SPIF_CAST(byteptr) "") \
                                                         : (SPIF_MBUFF(obj)->buff)))
typedef spif_int64_t spif_memidx_t;

SPIF_DECL_OBJ(mbuff) {
    SPIF_DECL_PARENT_TYPE(obj);
    spif_byteptr_t buff;
    SPIF_DECL_PROPERTY_C(spif_memidx_t, size);
    SPIF_DECL_PROPERTY_C(spif_memidx_t, len);
};

SPIF_DECL_OBJ(mbuffclass) {
    SPIF_DECL_PARENT_TYPE(class);

    spif_func_t new_from_ptr;
    spif_func_t new_from_buff;
    spif_func_t new_from_fp;
    spif_func_t new_from_fd;
    spif_func_t init_from_ptr;
    spif_func_t init_from_buff;
    spif_func_t init_from_fp;
    spif_func_t init_from_fd;
    spif_func_t append;
    spif_func_t append_from_ptr;
    spif_func_t clear;
    spif_func_t cmp;
    spif_func_t cmp_with_ptr;
    spif_func_t find;
    spif_func_t find_from_ptr;
    spif_func_t index;
    spif_func_t ncmp;
    spif_func_t ncmp_with_ptr;
    spif_func_t prepend;
    spif_func_t prepend_from_ptr;
    spif_func_t reverse;
    spif_func_t rindex;
    spif_func_t splice;
    spif_func_t splice_from_ptr;
    spif_func_t subbuff;
    spif_func_t subbuff_to_ptr;
    spif_func_t trim;
};

extern spif_class_t SPIF_CLASS_VAR(mbuff);
extern spif_mbuffclass_t SPIF_MBUFFCLASS_VAR(mbuff);
extern spif_mbuff_t spif_mbuff_new(void);
extern spif_mbuff_t spif_mbuff_new_from_ptr(spif_byteptr_t, spif_memidx_t);
extern spif_mbuff_t spif_mbuff_new_from_buff(spif_byteptr_t, spif_memidx_t, spif_memidx_t);
extern spif_mbuff_t spif_mbuff_new_from_fp(FILE *);
extern spif_mbuff_t spif_mbuff_new_from_fd(int);
extern spif_bool_t spif_mbuff_del(spif_mbuff_t);
extern spif_bool_t spif_mbuff_init(spif_mbuff_t);
extern spif_bool_t spif_mbuff_init_from_ptr(spif_mbuff_t, spif_byteptr_t, spif_memidx_t);
extern spif_bool_t spif_mbuff_init_from_buff(spif_mbuff_t, spif_byteptr_t, spif_memidx_t, spif_memidx_t);
extern spif_bool_t spif_mbuff_init_from_fp(spif_mbuff_t, FILE *);
extern spif_bool_t spif_mbuff_init_from_fd(spif_mbuff_t, int);
extern spif_bool_t spif_mbuff_done(spif_mbuff_t);
extern spif_str_t spif_mbuff_show(spif_mbuff_t, spif_byteptr_t, spif_str_t, size_t);
extern spif_cmp_t spif_mbuff_comp(spif_mbuff_t, spif_mbuff_t);
extern spif_mbuff_t spif_mbuff_dup(spif_mbuff_t);
extern spif_classname_t spif_mbuff_type(spif_mbuff_t);

extern spif_bool_t spif_mbuff_append(spif_mbuff_t, spif_mbuff_t);
extern spif_bool_t spif_mbuff_append_from_ptr(spif_mbuff_t, spif_byteptr_t, spif_memidx_t);
extern spif_bool_t spif_mbuff_clear(spif_mbuff_t, spif_uint8_t);
extern spif_cmp_t spif_mbuff_cmp(spif_mbuff_t, spif_mbuff_t);
extern spif_cmp_t spif_mbuff_cmp_with_ptr(spif_mbuff_t, spif_byteptr_t, spif_memidx_t);
extern spif_memidx_t spif_mbuff_find(spif_mbuff_t, spif_mbuff_t);
extern spif_memidx_t spif_mbuff_find_from_ptr(spif_mbuff_t, spif_byteptr_t, spif_memidx_t);
extern spif_memidx_t spif_mbuff_index(spif_mbuff_t, spif_uint8_t);
extern spif_cmp_t spif_mbuff_ncmp(spif_mbuff_t, spif_mbuff_t, spif_memidx_t);
extern spif_cmp_t spif_mbuff_ncmp_with_ptr(spif_mbuff_t, spif_byteptr_t, spif_memidx_t);
extern spif_bool_t spif_mbuff_prepend(spif_mbuff_t, spif_mbuff_t);
extern spif_bool_t spif_mbuff_prepend_from_ptr(spif_mbuff_t, spif_byteptr_t, spif_memidx_t);
extern spif_bool_t spif_mbuff_reverse(spif_mbuff_t);
extern spif_memidx_t spif_mbuff_rindex(spif_mbuff_t, spif_uint8_t);
extern spif_bool_t spif_mbuff_splice(spif_mbuff_t, spif_memidx_t, spif_memidx_t, spif_mbuff_t);
extern spif_bool_t spif_mbuff_splice_from_ptr(spif_mbuff_t, spif_memidx_t, spif_memidx_t, spif_byteptr_t, spif_memidx_t);
extern spif_mbuff_t spif_mbuff_subbuff(spif_mbuff_t, spif_memidx_t, spif_memidx_t);
extern spif_byteptr_t spif_mbuff_subbuff_to_ptr(spif_mbuff_t, spif_memidx_t, spif_memidx_t);
extern spif_bool_t spif_mbuff_trim(spif_mbuff_t);
SPIF_DECL_PROPERTY_FUNC_C(mbuff, spif_memidx_t, size);
SPIF_DECL_PROPERTY_FUNC_C(mbuff, spif_memidx_t, len);

#endif /* _LIBAST_MBUFF_H_ */
