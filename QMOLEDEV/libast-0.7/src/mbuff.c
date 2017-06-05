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

static const char __attribute__((unused)) cvs_ident[] = "$Id: mbuff.c,v 1.1 2004/12/15 00:00:22 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
static SPIF_CONST_TYPE(mbuffclass) mb_class = {
    {
        SPIF_DECL_CLASSNAME(mbuff),
        (spif_func_t) spif_mbuff_new,
        (spif_func_t) spif_mbuff_init,
        (spif_func_t) spif_mbuff_done,
        (spif_func_t) spif_mbuff_del,
        (spif_func_t) spif_mbuff_show,
        (spif_func_t) spif_mbuff_comp,
        (spif_func_t) spif_mbuff_dup,
        (spif_func_t) spif_mbuff_type
    },
    (spif_func_t) spif_mbuff_new_from_ptr,
    (spif_func_t) spif_mbuff_new_from_buff,
    (spif_func_t) spif_mbuff_new_from_fp,
    (spif_func_t) spif_mbuff_new_from_fd,
    (spif_func_t) spif_mbuff_init_from_ptr,
    (spif_func_t) spif_mbuff_init_from_buff,
    (spif_func_t) spif_mbuff_init_from_fp,
    (spif_func_t) spif_mbuff_init_from_fd,
    (spif_func_t) spif_mbuff_append,
    (spif_func_t) spif_mbuff_append_from_ptr,
    (spif_func_t) spif_mbuff_clear,
    (spif_func_t) spif_mbuff_cmp,
    (spif_func_t) spif_mbuff_cmp_with_ptr,
    (spif_func_t) spif_mbuff_find,
    (spif_func_t) spif_mbuff_find_from_ptr,
    (spif_func_t) spif_mbuff_index,
    (spif_func_t) spif_mbuff_ncmp,
    (spif_func_t) spif_mbuff_ncmp_with_ptr,
    (spif_func_t) spif_mbuff_prepend,
    (spif_func_t) spif_mbuff_prepend_from_ptr,
    (spif_func_t) spif_mbuff_reverse,
    (spif_func_t) spif_mbuff_rindex,
    (spif_func_t) spif_mbuff_splice,
    (spif_func_t) spif_mbuff_splice_from_ptr,
    (spif_func_t) spif_mbuff_subbuff,
    (spif_func_t) spif_mbuff_subbuff_to_ptr,
    (spif_func_t) spif_mbuff_trim
};
SPIF_TYPE(class) SPIF_CLASS_VAR(mbuff) = SPIF_CAST(class) &mb_class;
SPIF_TYPE(mbuffclass) SPIF_MBUFFCLASS_VAR(mbuff) = &mb_class;
/* *INDENT-ON* */

spif_mbuff_t
spif_mbuff_new(void)
{
    spif_mbuff_t self;

    self = SPIF_ALLOC(mbuff);
    if (!spif_mbuff_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(mbuff);
    }
    return self;
}

spif_mbuff_t
spif_mbuff_new_from_ptr(spif_byteptr_t old, spif_memidx_t len)
{
    spif_mbuff_t self;

    self = SPIF_ALLOC(mbuff);
    if (!spif_mbuff_init_from_ptr(self, old, len)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(mbuff);
    }
    return self;
}

spif_mbuff_t
spif_mbuff_new_from_buff(spif_byteptr_t buff, spif_memidx_t len, spif_memidx_t size)
{
    spif_mbuff_t self;

    self = SPIF_ALLOC(mbuff);
    if (!spif_mbuff_init_from_buff(self, buff, len, size)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(mbuff);
    }
    return self;
}

spif_mbuff_t
spif_mbuff_new_from_fp(FILE * fp)
{
    spif_mbuff_t self;

    self = SPIF_ALLOC(mbuff);
    if (!spif_mbuff_init_from_fp(self, fp)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(mbuff);
    }
    return self;
}

spif_mbuff_t
spif_mbuff_new_from_fd(int fd)
{
    spif_mbuff_t self;

    self = SPIF_ALLOC(mbuff);
    if (!spif_mbuff_init_from_fd(self, fd)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(mbuff);
    }
    return self;
}

spif_bool_t
spif_mbuff_init(spif_mbuff_t self)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MBUFFCLASS_VAR(mbuff)));
    self->buff = SPIF_NULL_TYPE(byteptr);
    self->len = 0;
    self->size = 0;
    return TRUE;
}

spif_bool_t
spif_mbuff_init_from_ptr(spif_mbuff_t self, spif_byteptr_t old, spif_memidx_t len)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    REQUIRE_RVAL((old != SPIF_NULL_TYPE(byteptr)), spif_mbuff_init(self));
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MBUFFCLASS_VAR(mbuff)));
    self->len = self->size = len;
    self->buff = SPIF_CAST(byteptr) MALLOC(self->size);
    memcpy(self->buff, old, self->len);
    return TRUE;
}

spif_bool_t
spif_mbuff_init_from_buff(spif_mbuff_t self, spif_byteptr_t buff, spif_memidx_t len, spif_memidx_t size)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MBUFFCLASS_VAR(mbuff)));
    if (buff != SPIF_NULL_TYPE(byteptr)) {
        self->len = len;
    } else {
        self->len = 0;
    }
    self->size = MAX(size, self->len);
    self->buff = SPIF_CAST(byteptr) MALLOC(self->size);
    if (buff != SPIF_NULL_TYPE(byteptr)) {
        memcpy(self->buff, buff, self->len);
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_init_from_fp(spif_mbuff_t self, FILE *fp)
{
    spif_byteptr_t p;
    long file_pos;
    spif_memidx_t file_size;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    ASSERT_RVAL((fp != SPIF_NULL_TYPE_C(FILE *)), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MBUFFCLASS_VAR(mbuff)));

    file_pos = ftell(fp);
    fseek(fp, 0L, SEEK_END);
    file_size = ftell(fp);
    fseek(fp, file_pos, SEEK_SET);
    LOWER_BOUND(file_size, 0);
    if (file_size == 0) {
        spif_mbuff_init(self);
        return FALSE;
    }
    self->len = self->size = file_size;
    self->buff = SPIF_CAST(byteptr) MALLOC(self->size);

    if (fread(p, file_size, 1, fp) < 1) {
        FREE(self->buff);
        return FALSE;
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_init_from_fd(spif_mbuff_t self, int fd)
{
    spif_byteptr_t p;
    off_t file_pos;
    spif_memidx_t file_size;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    ASSERT_RVAL((fd >= 0), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MBUFFCLASS_VAR(mbuff)));

    file_pos = lseek(fd, SPIF_CAST_C(off_t) 0, SEEK_CUR);
    file_size = SPIF_CAST(memidx) lseek(fd, SPIF_CAST_C(off_t) 0, SEEK_END);
    lseek(fd, file_pos, SEEK_SET);
    LOWER_BOUND(file_size, 0);
    if (file_size == 0) {
        spif_mbuff_init(self);
        return FALSE;
    }
    self->len = self->size = file_size;
    self->buff = SPIF_CAST(byteptr) MALLOC(self->size);

    if (read(fd, p, file_size) < 1) {
        FREE(self->buff);
        return FALSE;
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_done(spif_mbuff_t self)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    if (self->size) {
        FREE(self->buff);
        self->len = 0;
        self->size = 0;
        self->buff = SPIF_NULL_TYPE(byteptr);
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_del(spif_mbuff_t self)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    spif_mbuff_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

spif_str_t
spif_mbuff_show(spif_mbuff_t self, spif_byteptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];
    spif_memidx_t j;

    if (SPIF_MBUFF_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(mbuff, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CHARPTR_C(tmp) + indent, sizeof(tmp) - indent,
             "(spif_mbuff_t) %s:  %10p (length %lu, size %lu) {\n",
             name, SPIF_CAST(ptr) self, SPIF_CAST(ulong) self->len, SPIF_CAST(ulong) self->size);
    if (SPIF_MBUFF_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    memset(tmp, ' ', indent + 2);
    for (j = 0; j < self->len; j += 8) {
        spif_memidx_t k, l, len;

        snprintf(SPIF_CHARPTR_C(tmp) + indent + 2, sizeof(tmp) - indent - 2, "%0lx    ", SPIF_CAST(long) j);
        len = strlen(SPIF_CHARPTR_C(tmp));
        if (len + ((8 * 3) + 3 + 8 + 1) < sizeof(tmp)) {
            l = ((self->len - j < 8) ? (self->len - j) : (8));
            for (k = 0; k < l; k++) {
                sprintf(SPIF_CHARPTR_C(tmp) + len, "%02x ", self->buff[j + k]);
            }
            for (; k < 8; k++) {
                strcat(SPIF_CHARPTR_C(tmp) + len, "   ");
            }
            sprintf(SPIF_CHARPTR_C(tmp) + len, "%-8s\n", spiftool_safe_str(SPIF_CAST(charptr) (self->buff + j), l));
        }
        spif_str_append_from_ptr(buff, tmp);
    }

    snprintf(SPIF_CHARPTR_C(tmp) + indent, sizeof(tmp) - indent, "}\n");
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

spif_cmp_t
spif_mbuff_comp(spif_mbuff_t self, spif_mbuff_t other)
{
    return spif_mbuff_cmp(self, other);
}

spif_mbuff_t
spif_mbuff_dup(spif_mbuff_t self)
{
    spif_mbuff_t tmp;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), SPIF_NULL_TYPE(mbuff));
    tmp = SPIF_ALLOC(mbuff);
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(mbuff));
    tmp->buff = SPIF_CAST(byteptr) MALLOC(self->size);
    memcpy(tmp->buff, self->buff, self->size);
    tmp->len = self->len;
    tmp->size = self->size;
    return tmp;
}

spif_classname_t
spif_mbuff_type(spif_mbuff_t self)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), SPIF_CAST(classname) SPIF_NULLSTR_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

spif_bool_t
spif_mbuff_append(spif_mbuff_t self, spif_mbuff_t other)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_MBUFF_ISNULL(other), FALSE);
    if (other->size && other->len) {
        self->size += other->size;
        self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, self->size);
        memcpy(self->buff + self->len, SPIF_MBUFF_BUFF(other), other->len);
        self->len += other->len;
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_append_from_ptr(spif_mbuff_t self, spif_byteptr_t other, spif_memidx_t len)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    REQUIRE_RVAL((other != SPIF_NULL_TYPE(byteptr)), FALSE);
    if (len) {
        self->size += len;
        self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, self->size);
        memcpy(self->buff + self->len, other, len);
        self->len += len;
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_clear(spif_mbuff_t self, spif_uint8_t c)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    memset(self->buff, c, self->len);
    return TRUE;
}

spif_cmp_t
spif_mbuff_cmp(spif_mbuff_t self, spif_mbuff_t other)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = memcmp(SPIF_MBUFF_BUFF(self), SPIF_MBUFF_BUFF(other), MIN(self->len, other->len));
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_mbuff_cmp_with_ptr(spif_mbuff_t self, spif_byteptr_t other, spif_memidx_t len)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = memcmp(SPIF_MBUFF_BUFF(self), other, len);
    return SPIF_CMP_FROM_INT(c);
}

spif_memidx_t
spif_mbuff_find(spif_mbuff_t self, spif_mbuff_t other)
{
    spif_byteptr_t tmp;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), (SPIF_CAST(memidx) -1));
    REQUIRE_RVAL(!SPIF_MBUFF_ISNULL(other), (SPIF_CAST(memidx) -1));
    tmp = SPIF_CAST(byteptr) memmem(SPIF_MBUFF_BUFF(self), self->len, SPIF_MBUFF_BUFF(other), other->len);
    if (tmp) {
        return SPIF_CAST(memidx) (SPIF_CAST(long) tmp - SPIF_CAST(long) (SPIF_MBUFF_BUFF(self)));
    } else {
        return SPIF_CAST(memidx) (self->len);
    }
}

spif_memidx_t
spif_mbuff_find_from_ptr(spif_mbuff_t self, spif_byteptr_t other, spif_memidx_t len)
{
    spif_byteptr_t tmp;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), (SPIF_CAST(memidx) -1));
    REQUIRE_RVAL((other != SPIF_NULL_TYPE(byteptr)), (SPIF_CAST(memidx) -1));
    tmp = SPIF_CAST(byteptr) memmem(SPIF_MBUFF_BUFF(self), self->len, other, len);
    if (tmp) {
        return SPIF_CAST(memidx) (SPIF_CAST(long) tmp - SPIF_CAST(long) (SPIF_MBUFF_BUFF(self)));
    } else {
        return SPIF_CAST(memidx) (self->len);
    }
}

spif_memidx_t
spif_mbuff_index(spif_mbuff_t self, spif_uint8_t c)
{
    spif_byteptr_t tmp;
    spif_memidx_t i;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), (SPIF_CAST(memidx) -1));
    for (tmp = self->buff, i = 0; (*tmp & (~c)) && (i < self->len); i++, tmp++);
    return SPIF_CAST(memidx) (SPIF_CAST(long) tmp - SPIF_CAST(long) self->buff);
}

spif_cmp_t
spif_mbuff_ncmp(spif_mbuff_t self, spif_mbuff_t other, spif_memidx_t cnt)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    if (cnt > self->len || cnt > other->len) {
        cnt = MIN(self->len, other->len);
    }
    c = memcmp(SPIF_MBUFF_BUFF(self), SPIF_MBUFF_BUFF(other), cnt);
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_mbuff_ncmp_with_ptr(spif_mbuff_t self, spif_byteptr_t other, spif_memidx_t cnt)
{
    return spif_mbuff_cmp_with_ptr(self, other, cnt);
}

spif_bool_t
spif_mbuff_prepend(spif_mbuff_t self, spif_mbuff_t other)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_MBUFF_ISNULL(other), FALSE);
    if (other->size && other->len) {
        self->size += other->size;
        self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, self->size);
        memmove(self->buff + other->len, self->buff, self->len);
        memcpy(self->buff, SPIF_MBUFF_BUFF(other), other->len);
        self->len += other->len;
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_prepend_from_ptr(spif_mbuff_t self, spif_byteptr_t other, spif_memidx_t len)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    REQUIRE_RVAL((other != SPIF_NULL_TYPE(byteptr)), FALSE);
    if (len) {
        self->size += len;
        self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, self->size);
        memmove(self->buff + len, self->buff, self->len);
        memcpy(self->buff, other, len);
        self->len += len;
    }
    return TRUE;
}

spif_bool_t
spif_mbuff_reverse(spif_mbuff_t self)
{
    spif_byteptr_t tmp = self->buff;
    int i, j;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    REQUIRE_RVAL(self->buff != SPIF_NULL_TYPE(byteptr), FALSE);

    for (j = 0, i = self->len - 1; i > j; i--, j++) {
        SWAP(tmp[j], tmp[i]);
    }
    return TRUE;
}

spif_memidx_t
spif_mbuff_rindex(spif_mbuff_t self, spif_uint8_t c)
{
    spif_byteptr_t tmp;
    spif_memidx_t i;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), (SPIF_CAST(memidx) -1));
    for (tmp = self->buff + self->len, i = 0; (*tmp & (~c)) && (i < self->len); i++, tmp--);

    if ((tmp == self->buff) && (*tmp != c)) {
        return SPIF_CAST(memidx) (self->len);
    } else {
        return SPIF_CAST(memidx) (SPIF_CAST(long) tmp - SPIF_CAST(long) self->buff);
    }
}

spif_bool_t
spif_mbuff_splice(spif_mbuff_t self, spif_memidx_t idx, spif_memidx_t cnt, spif_mbuff_t other)
{
    spif_byteptr_t tmp, ptmp;
    spif_memidx_t newsize;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    if (idx < 0) {
        idx = self->len + idx;
    }
    REQUIRE_RVAL(idx >= 0, FALSE);
    REQUIRE_RVAL(idx < self->len, FALSE);
    if (cnt < 0) {
        cnt = idx + self->len + cnt;
    }
    REQUIRE_RVAL(cnt >= 0, FALSE);
    REQUIRE_RVAL(cnt <= (self->len - idx), FALSE);

    newsize = self->len + ((SPIF_MBUFF_ISNULL(other)) ? (0) : (other->len)) - cnt + 1;
    ptmp = tmp = SPIF_CAST(byteptr) MALLOC(newsize);
    if (idx > 0) {
        memcpy(tmp, self->buff, idx);
        ptmp += idx;
    }
    if (!SPIF_MBUFF_ISNULL(other)) {
        memcpy(ptmp, other->buff, other->len);
        ptmp += other->len;
    }
    memcpy(ptmp, self->buff + idx + cnt, self->len - idx - cnt + 1);
    if (self->size < newsize) {
        self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, newsize);
        self->size = newsize;
    }
    self->len = newsize;
    memcpy(self->buff, tmp, newsize);
    FREE(tmp);
    return TRUE;
}

spif_bool_t
spif_mbuff_splice_from_ptr(spif_mbuff_t self, spif_memidx_t idx, spif_memidx_t cnt, spif_byteptr_t other, spif_memidx_t len)
{
    spif_byteptr_t tmp, ptmp;
    spif_memidx_t newsize;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    if (SPIF_PTR_ISNULL(other)) {
        len = 0;
    }
    if (idx < 0) {
        idx = self->len + idx;
    }
    REQUIRE_RVAL(idx >= 0, FALSE);
    REQUIRE_RVAL(idx < self->len, FALSE);
    if (cnt < 0) {
        cnt = idx + self->len + cnt;
    }
    REQUIRE_RVAL(cnt >= 0, FALSE);
    REQUIRE_RVAL(cnt <= (self->len - idx), FALSE);

    newsize = self->len + len - cnt;
    ptmp = tmp = SPIF_CAST(byteptr) MALLOC(newsize);
    if (idx > 0) {
        memcpy(tmp, self->buff, idx);
        ptmp += idx;
    }
    if (len) {
        memcpy(ptmp, other, len);
        ptmp += len;
    }
    memcpy(ptmp, self->buff + idx + cnt, self->len - idx - cnt);
    if (self->size < newsize) {
        self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, newsize);
        self->size = newsize;
    }
    self->len = newsize;
    memcpy(self->buff, tmp, newsize);
    FREE(tmp);
    return TRUE;
}

spif_mbuff_t
spif_mbuff_subbuff(spif_mbuff_t self, spif_memidx_t idx, spif_memidx_t cnt)
{
    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), SPIF_NULL_TYPE(mbuff));
    if (idx < 0) {
        idx = self->len + idx;
    }
    REQUIRE_RVAL(idx >= 0, SPIF_NULL_TYPE(mbuff));
    REQUIRE_RVAL(idx < self->len, SPIF_NULL_TYPE(mbuff));
    if (cnt <= 0) {
        cnt = self->len - idx + cnt;
    }
    REQUIRE_RVAL(cnt >= 0, SPIF_NULL_TYPE(mbuff));
    UPPER_BOUND(cnt, self->len - idx);
    return spif_mbuff_new_from_buff(self->buff + idx, cnt, cnt);
}

spif_byteptr_t
spif_mbuff_subbuff_to_ptr(spif_mbuff_t self, spif_memidx_t idx, spif_memidx_t cnt)
{
    spif_byteptr_t newmbuff;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), SPIF_NULL_TYPE(byteptr));
    if (idx < 0) {
        idx = self->len + idx;
    }
    REQUIRE_RVAL(idx >= 0, SPIF_NULL_TYPE(byteptr));
    REQUIRE_RVAL(idx < self->len, SPIF_NULL_TYPE(byteptr));
    if (cnt <= 0) {
        cnt = self->len - idx + cnt;
    }
    REQUIRE_RVAL(cnt >= 0, SPIF_NULL_TYPE(byteptr));
    UPPER_BOUND(cnt, self->len - idx);

    newmbuff = SPIF_CAST(byteptr) MALLOC(cnt + 1);
    memcpy(newmbuff, SPIF_MBUFF_BUFF(self) + idx, cnt);
    newmbuff[cnt] = 0;
    return newmbuff;
}

spif_bool_t
spif_mbuff_trim(spif_mbuff_t self)
{
    spif_byteptr_t start, end;

    ASSERT_RVAL(!SPIF_MBUFF_ISNULL(self), FALSE);
    start = self->buff;
    end = self->buff + self->len - 1;
    for (; isspace((spif_uchar_t) (*start)) && (start < end); start++);
    for (; isspace((spif_uchar_t) (*end)) && (start < end); end--);
    if (start > end) {
        return spif_mbuff_done(self);
    }
    *(++end) = 0;
    self->len = (spif_memidx_t) (end - start);
    self->size = self->len + 1;
    memmove(self->buff, start, self->size);
    self->buff = SPIF_CAST(byteptr) REALLOC(self->buff, self->size);
    return TRUE;
}

SPIF_DEFINE_PROPERTY_FUNC_C(mbuff, spif_memidx_t, size)
SPIF_DEFINE_PROPERTY_FUNC_C(mbuff, spif_memidx_t, len)
