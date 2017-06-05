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

static const char __attribute__((unused)) cvs_ident[] = "$Id: str.c,v 1.32 2004/12/15 00:00:22 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
static SPIF_CONST_TYPE(strclass) s_class = {
    {
        SPIF_DECL_CLASSNAME(str),
        (spif_func_t) spif_str_new,
        (spif_func_t) spif_str_init,
        (spif_func_t) spif_str_done,
        (spif_func_t) spif_str_del,
        (spif_func_t) spif_str_show,
        (spif_func_t) spif_str_comp,
        (spif_func_t) spif_str_dup,
        (spif_func_t) spif_str_type
    },
    (spif_func_t) spif_str_new_from_ptr,
    (spif_func_t) spif_str_new_from_buff,
    (spif_func_t) spif_str_new_from_fp,
    (spif_func_t) spif_str_new_from_fd,
    (spif_func_t) spif_str_new_from_num,
    (spif_func_t) spif_str_init_from_ptr,
    (spif_func_t) spif_str_init_from_buff,
    (spif_func_t) spif_str_init_from_fp,
    (spif_func_t) spif_str_init_from_fd,
    (spif_func_t) spif_str_init_from_num,
    (spif_func_t) spif_str_append,
    (spif_func_t) spif_str_append_char,
    (spif_func_t) spif_str_append_from_ptr,
    (spif_func_t) spif_str_casecmp,
    (spif_func_t) spif_str_casecmp_with_ptr,
    (spif_func_t) spif_str_clear,
    (spif_func_t) spif_str_cmp,
    (spif_func_t) spif_str_cmp_with_ptr,
    (spif_func_t) spif_str_downcase,
    (spif_func_t) spif_str_find,
    (spif_func_t) spif_str_find_from_ptr,
    (spif_func_t) spif_str_index,
    (spif_func_t) spif_str_ncasecmp,
    (spif_func_t) spif_str_ncasecmp_with_ptr,
    (spif_func_t) spif_str_ncmp,
    (spif_func_t) spif_str_ncmp_with_ptr,
    (spif_func_t) spif_str_prepend,
    (spif_func_t) spif_str_prepend_char,
    (spif_func_t) spif_str_prepend_from_ptr,
    (spif_func_t) spif_str_reverse,
    (spif_func_t) spif_str_rindex,
    (spif_func_t) spif_str_splice,
    (spif_func_t) spif_str_splice_from_ptr,
    (spif_func_t) spif_str_substr,
    (spif_func_t) spif_str_substr_to_ptr,
    (spif_func_t) spif_str_to_float,
    (spif_func_t) spif_str_to_num,
    (spif_func_t) spif_str_trim,
    (spif_func_t) spif_str_upcase
};
SPIF_TYPE(class) SPIF_CLASS_VAR(str) = SPIF_CAST(class) &s_class;
SPIF_TYPE(strclass) SPIF_STRCLASS_VAR(str) = &s_class;
/* *INDENT-ON* */

const size_t buff_inc = 4096;

spif_str_t
spif_str_new(void)
{
    spif_str_t self;

    self = SPIF_ALLOC(str);
    if (!spif_str_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(str);
    }
    return self;
}

spif_str_t
spif_str_new_from_ptr(spif_charptr_t old)
{
    spif_str_t self;

    self = SPIF_ALLOC(str);
    if (!spif_str_init_from_ptr(self, old)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(str);
    }
    return self;
}

spif_str_t
spif_str_new_from_buff(spif_charptr_t buff, spif_stridx_t size)
{
    spif_str_t self;

    self = SPIF_ALLOC(str);
    if (!spif_str_init_from_buff(self, buff, size)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(str);
    }
    return self;
}

spif_str_t
spif_str_new_from_fp(FILE * fp)
{
    spif_str_t self;

    self = SPIF_ALLOC(str);
    if (!spif_str_init_from_fp(self, fp)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(str);
    }
    return self;
}

spif_str_t
spif_str_new_from_fd(int fd)
{
    spif_str_t self;

    self = SPIF_ALLOC(str);
    if (!spif_str_init_from_fd(self, fd)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(str);
    }
    return self;
}

spif_str_t
spif_str_new_from_num(long num)
{
    spif_str_t self;

    self = SPIF_ALLOC(str);
    if (!spif_str_init_from_num(self, num)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(str);
    }
    return self;
}

spif_bool_t
spif_str_init(spif_str_t self)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_STRCLASS_VAR(str)));
    self->s = SPIF_NULL_TYPE(charptr);
    self->len = 0;
    self->size = 0;
    return TRUE;
}

spif_bool_t
spif_str_init_from_ptr(spif_str_t self, spif_charptr_t old)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    REQUIRE_RVAL((old != SPIF_NULL_TYPE(charptr)), spif_str_init(self));
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_STRCLASS_VAR(str)));
    self->len = strlen(SPIF_CONST_CAST_C(char *) old);
    self->size = self->len + 1;
    self->s = SPIF_CAST(charptr) MALLOC(self->size);
    memcpy(self->s, old, self->size);
    return TRUE;
}

spif_bool_t
spif_str_init_from_buff(spif_str_t self, spif_charptr_t buff, spif_stridx_t size)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_STRCLASS_VAR(str)));
    self->size = size;
    if (buff != SPIF_NULL_TYPE(charptr)) {
        self->len = strnlen(SPIF_CONST_CAST_C(char *) buff, size);
    } else {
        self->len = 0;
    }
    if (self->size == self->len) {
        self->size++;
    }
    self->s = SPIF_CAST(charptr) MALLOC(self->size);
    if (buff != SPIF_NULL_TYPE(charptr)) {
        memcpy(self->s, buff, self->len);
    }
    self->s[self->len] = 0;
    return TRUE;
}

spif_bool_t
spif_str_init_from_fp(spif_str_t self, FILE *fp)
{
    spif_charptr_t p, end = NULL;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    ASSERT_RVAL((fp != SPIF_NULL_TYPE_C(FILE *)), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_STRCLASS_VAR(str)));
    self->size = buff_inc;
    self->len = 0;
    self->s = SPIF_CAST(charptr) MALLOC(self->size);

    for (p = self->s; fgets(SPIF_CAST_C(char *)p, buff_inc, fp); p += buff_inc) {
        if ((end = SPIF_CAST(charptr)
             strchr(SPIF_CONST_CAST_C(char *)p, '\n')) == NULL) {
            self->size += buff_inc;
            self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
        } else {
            *end = 0;
            break;
        }
    }
    self->len = (spif_stridx_t) ((end)
                          ? (end - self->s)
                          : ((int) strlen(SPIF_CONST_CAST_C(char *)self->s)));
    self->size = self->len + 1;
    self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
    return TRUE;
}

spif_bool_t
spif_str_init_from_fd(spif_str_t self, int fd)
{
    int n;
    spif_charptr_t p;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    ASSERT_RVAL((fd >= 0), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_STRCLASS_VAR(str)));
    self->size = buff_inc;
    self->len = 0;
    self->s = SPIF_CAST(charptr) MALLOC(self->size);

    for (p = self->s; ((n = read(fd, p, buff_inc)) > 0) || (errno == EINTR);) {
        self->size += n;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
        p += n;
    }
    self->len = self->size - buff_inc;
    self->size = self->len + 1;
    self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
    self->s[self->len] = 0;
    return TRUE;
}

spif_bool_t
spif_str_init_from_num(spif_str_t self, long num)
{
    spif_char_t buff[28];

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_STRCLASS_VAR(str)));

    snprintf(SPIF_CHARPTR_C(buff), sizeof(buff), "%ld", num);
    self->len = strlen(SPIF_CHARPTR_C(buff));
    self->size = self->len + 1;
    self->s = SPIF_CAST(charptr) MALLOC(self->size);
    strcpy(SPIF_CHARPTR_C(self->s), SPIF_CHARPTR_C(buff));

    return TRUE;
}

spif_bool_t
spif_str_done(spif_str_t self)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    if (self->size) {
        FREE(self->s);
        self->len = 0;
        self->size = 0;
        self->s = SPIF_NULL_TYPE(charptr);
    }
    return TRUE;
}

spif_bool_t
spif_str_del(spif_str_t self)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    spif_str_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

spif_str_t
spif_str_show(spif_str_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_STR_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(str, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CHARPTR_C(tmp) + indent, sizeof(tmp) - indent,
             "(spif_str_t) %s:  %10p { \"",
             name, SPIF_CAST(ptr) self);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    spif_str_append(buff, self);

    snprintf(SPIF_CHARPTR_C(tmp), sizeof(tmp), "\", len %lu, size %lu }\n", (unsigned long) self->len,
             (unsigned long) self->size);
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

spif_cmp_t
spif_str_comp(spif_str_t self, spif_str_t other)
{
    return spif_str_cmp(self, other);
}

spif_str_t
spif_str_dup(spif_str_t self)
{
    spif_str_t tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), SPIF_NULL_TYPE(str));
    tmp = SPIF_ALLOC(str);
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(str));
    tmp->s = SPIF_CAST(charptr) STRDUP(SPIF_CONST_CAST_C(char *) SPIF_STR_STR(self));
    tmp->len = self->len;
    tmp->size = self->size;
    return tmp;
}

spif_classname_t
spif_str_type(spif_str_t self)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), SPIF_CAST(classname) SPIF_NULLSTR_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

spif_bool_t
spif_str_append(spif_str_t self, spif_str_t other)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_STR_ISNULL(other), FALSE);
    if (other->size && other->len) {
        self->size += other->size - 1;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
        memcpy(self->s + self->len, SPIF_STR_STR(other), other->len + 1);
        self->len += other->len;
    }
    return TRUE;
}

spif_bool_t
spif_str_append_char(spif_str_t self, spif_char_t c)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    self->len++;
    if (self->size <= self->len) {
        self->size++;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
    }
    self->s[self->len - 1] = c;
    self->s[self->len] = 0;
    return TRUE;
}

spif_bool_t
spif_str_append_from_ptr(spif_str_t self, spif_charptr_t other)
{
    spif_stridx_t len;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    REQUIRE_RVAL((other != SPIF_NULL_TYPE(charptr)), FALSE);
    len = strlen(SPIF_CONST_CAST_C(char *) other);
    if (len) {
        self->size += len;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
        memcpy(self->s + self->len, other, len + 1);
        self->len += len;
    }
    return TRUE;
}

spif_cmp_t
spif_str_casecmp(spif_str_t self, spif_str_t other)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strcasecmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(SPIF_STR_STR(other)));
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_str_casecmp_with_ptr(spif_str_t self, spif_charptr_t other)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strcasecmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(other));
    return SPIF_CMP_FROM_INT(c);
}

spif_bool_t
spif_str_clear(spif_str_t self, spif_char_t c)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    memset(self->s, c, self->size);
    self->s[self->len] = 0;
    return TRUE;
}

spif_cmp_t
spif_str_cmp(spif_str_t self, spif_str_t other)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strcmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(SPIF_STR_STR(other)));
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_str_cmp_with_ptr(spif_str_t self, spif_charptr_t other)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strcmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(other));
    return SPIF_CMP_FROM_INT(c);
}

spif_bool_t
spif_str_downcase(spif_str_t self)
{
    spif_charptr_t tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    for (tmp = self->s; *tmp; tmp++) {
        *tmp = tolower(*tmp);
    }
    return TRUE;
}

spif_stridx_t
spif_str_find(spif_str_t self, spif_str_t other)
{
    char *tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), (SPIF_CAST(stridx) -1));
    REQUIRE_RVAL(!SPIF_STR_ISNULL(other), (SPIF_CAST(stridx) -1));
    tmp = strstr(SPIF_CONST_CAST_C(char *) SPIF_STR_STR(self),
                 SPIF_CONST_CAST_C(char *) SPIF_STR_STR(other));
    if (tmp) {
        return SPIF_CAST(stridx) (SPIF_CAST(long) tmp - SPIF_CAST(long) (SPIF_STR_STR(self)));
    } else {
        return SPIF_CAST(stridx) (self->len);
    }
}

spif_stridx_t
spif_str_find_from_ptr(spif_str_t self, spif_charptr_t other)
{
    char *tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), (SPIF_CAST(stridx) -1));
    REQUIRE_RVAL((other != SPIF_NULL_TYPE(charptr)), (SPIF_CAST(stridx) -1));
    tmp = strstr(SPIF_CONST_CAST_C(char *) SPIF_STR_STR(self),
                 SPIF_CONST_CAST_C(char *) other);
    if (tmp) {
        return SPIF_CAST(stridx) (SPIF_CAST(long) tmp - SPIF_CAST(long) (SPIF_STR_STR(self)));
    } else {
        return SPIF_CAST(stridx) (self->len);
    }
}

spif_stridx_t
spif_str_index(spif_str_t self, spif_char_t c)
{
    char *tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), (SPIF_CAST(stridx) -1));
    tmp = index(SPIF_CONST_CAST_C(char *) SPIF_STR_STR(self), c);
    if (tmp) {
        return SPIF_CAST(stridx) (SPIF_CAST(long) tmp - SPIF_CAST(long) (SPIF_STR_STR(self)));
    } else {
        return SPIF_CAST(stridx) (self->len);
    }
}

spif_cmp_t
spif_str_ncasecmp(spif_str_t self, spif_str_t other, spif_stridx_t cnt)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strncasecmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(SPIF_STR_STR(other)), cnt);
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_str_ncasecmp_with_ptr(spif_str_t self, spif_charptr_t other, spif_stridx_t cnt)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strncasecmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(other), cnt);
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_str_ncmp(spif_str_t self, spif_str_t other, spif_stridx_t cnt)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strncmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(SPIF_STR_STR(other)), cnt);
    return SPIF_CMP_FROM_INT(c);
}

spif_cmp_t
spif_str_ncmp_with_ptr(spif_str_t self, spif_charptr_t other, spif_stridx_t cnt)
{
    int c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    c = strncmp(SPIF_CHARPTR_C(SPIF_STR_STR(self)), SPIF_CHARPTR_C(other), cnt);
    return SPIF_CMP_FROM_INT(c);
}

spif_bool_t
spif_str_prepend(spif_str_t self, spif_str_t other)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_STR_ISNULL(other), FALSE);
    if (other->size && other->len) {
        self->size += other->size - 1;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
        memmove(self->s + other->len, self->s, self->len + 1);
        memcpy(self->s, SPIF_STR_STR(other), other->len);
        self->len += other->len;
    }
    return TRUE;
}

spif_bool_t
spif_str_prepend_char(spif_str_t self, spif_char_t c)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    self->len++;
    if (self->size <= self->len) {
        self->size++;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
    }
    memmove(self->s + 1, self->s, self->len + 1);
    self->s[0] = SPIF_CAST(uchar) c;
    return TRUE;
}

spif_bool_t
spif_str_prepend_from_ptr(spif_str_t self, spif_charptr_t other)
{
    spif_stridx_t len;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    REQUIRE_RVAL((other != SPIF_NULL_TYPE(charptr)), FALSE);
    len = strlen(SPIF_CONST_CAST_C(char *) other);
    if (len) {
        self->size += len;
        self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
        memmove(self->s + len, self->s, self->len + 1);
        memcpy(self->s, other, len);
        self->len += len;
    }
    return TRUE;
}

spif_bool_t
spif_str_reverse(spif_str_t self)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    return ((strrev(SPIF_CAST_C(char *) self->s)) ? TRUE : FALSE);
}

spif_stridx_t
spif_str_rindex(spif_str_t self, spif_char_t c)
{
    char *tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), (SPIF_CAST(stridx) -1));
    tmp = rindex(SPIF_CONST_CAST_C(char *) SPIF_STR_STR(self), c);
    if (tmp) {
        return SPIF_CAST(stridx) (SPIF_CAST(long) tmp - SPIF_CAST(long) (SPIF_STR_STR(self)));
    } else {
        return SPIF_CAST(stridx) (self->len);
    }
}

spif_bool_t
spif_str_splice(spif_str_t self, spif_stridx_t idx, spif_stridx_t cnt, spif_str_t other)
{
    spif_charptr_t tmp, ptmp;
    spif_stridx_t newsize;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
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

    newsize = self->len + ((SPIF_STR_ISNULL(other)) ? (0) : (other->len)) - cnt + 1;
    ptmp = tmp = SPIF_CAST(charptr) MALLOC(newsize);
    if (idx > 0) {
        memcpy(tmp, self->s, idx);
        ptmp += idx;
    }
    if (!SPIF_OBJ_ISNULL(other)) {
        memcpy(ptmp, other->s, other->len);
        ptmp += other->len;
    }
    memcpy(ptmp, self->s + idx + cnt, self->len - idx - cnt + 1);
    if (self->size < newsize) {
        self->s = SPIF_CAST(charptr) REALLOC(self->s, newsize);
        self->size = newsize;
    }
    self->len = newsize - 1;
    memcpy(self->s, tmp, newsize);
    FREE(tmp);
    return TRUE;
}

spif_bool_t
spif_str_splice_from_ptr(spif_str_t self, spif_stridx_t idx, spif_stridx_t cnt, spif_charptr_t other)
{
    spif_charptr_t tmp, ptmp;
    spif_stridx_t len, newsize;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    len = (other ? strlen(SPIF_CONST_CAST_C(char *) other) : 0);
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

    newsize = self->len + len - cnt + 1;
    ptmp = tmp = SPIF_CAST(charptr) MALLOC(newsize);
    if (idx > 0) {
        memcpy(tmp, self->s, idx);
        ptmp += idx;
    }
    if (len) {
        memcpy(ptmp, other, len);
        ptmp += len;
    }
    memcpy(ptmp, self->s + idx + cnt, self->len - idx - cnt + 1);
    if (self->size < newsize) {
        self->s = SPIF_CAST(charptr) REALLOC(self->s, newsize);
        self->size = newsize;
    }
    self->len = newsize - 1;
    memcpy(self->s, tmp, newsize);
    FREE(tmp);
    return TRUE;
}

spif_str_t
spif_str_substr(spif_str_t self, spif_stridx_t idx, spif_stridx_t cnt)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), SPIF_NULL_TYPE(str));
    if (idx < 0) {
        idx = self->len + idx;
    }
    REQUIRE_RVAL(idx >= 0, SPIF_NULL_TYPE(str));
    REQUIRE_RVAL(idx < self->len, SPIF_NULL_TYPE(str));
    if (cnt <= 0) {
        cnt = self->len - idx + cnt;
    }
    REQUIRE_RVAL(cnt >= 0, SPIF_NULL_TYPE(str));
    UPPER_BOUND(cnt, self->len - idx);
    return spif_str_new_from_buff(SPIF_STR_STR(self) + idx, cnt);
}

spif_charptr_t
spif_str_substr_to_ptr(spif_str_t self, spif_stridx_t idx, spif_stridx_t cnt)
{
    spif_charptr_t newstr;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), SPIF_NULL_TYPE(charptr));
    if (idx < 0) {
        idx = self->len + idx;
    }
    REQUIRE_RVAL(idx >= 0, SPIF_NULL_TYPE(charptr));
    REQUIRE_RVAL(idx < self->len, SPIF_NULL_TYPE(charptr));
    if (cnt <= 0) {
        cnt = self->len - idx + cnt;
    }
    REQUIRE_RVAL(cnt >= 0, SPIF_NULL_TYPE(charptr));
    UPPER_BOUND(cnt, self->len - idx);

    newstr = SPIF_CAST(charptr) MALLOC(cnt + 1);
    memcpy(newstr, SPIF_STR_STR(self) + idx, cnt);
    newstr[cnt] = 0;
    return newstr;
}

double
spif_str_to_float(spif_str_t self)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), SPIF_CAST_C(double) NAN);
    return (double) (strtod(SPIF_CONST_CAST_C(char *)SPIF_STR_STR(self), (char **) NULL));
}

size_t
spif_str_to_num(spif_str_t self, int base)
{
    ASSERT_RVAL(!SPIF_STR_ISNULL(self), (SPIF_CAST_C(size_t) -1));
    return (size_t) (strtoul(SPIF_CONST_CAST_C(char *) SPIF_STR_STR(self), (char **) NULL, base));
}

spif_bool_t
spif_str_trim(spif_str_t self)
{
    spif_charptr_t start, end;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    start = self->s;
    end = self->s + self->len - 1;
    for (; isspace((spif_uchar_t) (*start)) && (start < end); start++);
    for (; isspace((spif_uchar_t) (*end)) && (start < end); end--);
    if (start > end) {
        return spif_str_done(self);
    }
    *(++end) = 0;
    self->len = (spif_stridx_t) (end - start);
    self->size = self->len + 1;
    memmove(self->s, start, self->size);
    self->s = SPIF_CAST(charptr) REALLOC(self->s, self->size);
    return TRUE;
}

spif_bool_t
spif_str_upcase(spif_str_t self)
{
    spif_charptr_t tmp;

    ASSERT_RVAL(!SPIF_STR_ISNULL(self), FALSE);
    for (tmp = self->s; *tmp; tmp++) {
        *tmp = toupper(*tmp);
    }
    return TRUE;
}

SPIF_DEFINE_PROPERTY_FUNC_C(str, spif_stridx_t, size)
SPIF_DEFINE_PROPERTY_FUNC_C(str, spif_stridx_t, len)
