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

static const char __attribute__((unused)) cvs_ident[] = "$Id: regexp.c,v 1.15 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
static SPIF_CONST_TYPE(class) r_class = {
    SPIF_DECL_CLASSNAME(regexp),
    (spif_func_t) spif_regexp_new,
    (spif_func_t) spif_regexp_init,
    (spif_func_t) spif_regexp_done,
    (spif_func_t) spif_regexp_del,
    (spif_func_t) spif_regexp_show,
    (spif_func_t) spif_regexp_comp,
    (spif_func_t) spif_regexp_dup,
    (spif_func_t) spif_regexp_type
};
SPIF_TYPE(class) SPIF_CLASS_VAR(regexp) = &r_class;
/* *INDENT-ON* */

spif_regexp_t
spif_regexp_new(void)
{
    spif_regexp_t self;

    self = SPIF_ALLOC(regexp);
    if (!spif_regexp_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(regexp);
    }
    return self;
}

spif_regexp_t
spif_regexp_new_from_str(spif_str_t other)
{
    spif_regexp_t self;

    self = SPIF_ALLOC(regexp);
    if (!spif_regexp_init_from_str(self, other)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(regexp);
    }
    return self;
}

spif_regexp_t
spif_regexp_new_from_ptr(spif_charptr_t other)
{
    spif_regexp_t self;

    self = SPIF_ALLOC(regexp);
    if (!spif_regexp_init_from_ptr(self, other)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(regexp);
    }
    return self;
}

spif_bool_t
spif_regexp_init(spif_regexp_t self)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    if (!spif_str_init(SPIF_STR(self))) {
        return FALSE;
    }
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(regexp));
    self->data = SPIF_NULL_TYPE(ptr);
    spif_regexp_set_flags(self, SPIF_NULL_TYPE(charptr));
    return TRUE;
}

spif_bool_t
spif_regexp_init_from_str(spif_regexp_t self, spif_str_t other)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    if (!spif_str_init_from_ptr(SPIF_STR(self), SPIF_STR_STR(other))) {
        return FALSE;
    }
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(regexp));
    self->data = SPIF_NULL_TYPE(ptr);
    spif_regexp_set_flags(self, SPIF_CHARPTR(""));
    return TRUE;
}

spif_bool_t
spif_regexp_init_from_ptr(spif_regexp_t self, spif_charptr_t other)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    if (!spif_str_init_from_ptr(SPIF_STR(self), other)) {
        return FALSE;
    }
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(regexp));
    self->data = SPIF_NULL_TYPE(ptr);
    spif_regexp_set_flags(self, SPIF_CHARPTR(""));
    return TRUE;
}

spif_bool_t
spif_regexp_done(spif_regexp_t self)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    spif_str_done(SPIF_STR(self));
    if (self->data != SPIF_NULL_TYPE(ptr)) {
        FREE(self->data);
    }
    self->flags = 0;
    return TRUE;
}

spif_bool_t
spif_regexp_del(spif_regexp_t self)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    spif_regexp_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

spif_str_t
spif_regexp_show(spif_regexp_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_REGEXP_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(regexp, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CHARPTR_C(tmp) + indent, sizeof(tmp) - indent,
             "(spif_regexp_t) %s:  %10p {\n",
             name, SPIF_CAST(ptr) self);
    if (SPIF_REGEXP_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    snprintf(SPIF_CHARPTR_C(tmp), sizeof(tmp), "}\n");
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

spif_cmp_t
spif_regexp_comp(spif_regexp_t self, spif_regexp_t other)
{
    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    return spif_str_comp(SPIF_STR(self), SPIF_STR(other));
}

spif_regexp_t
spif_regexp_dup(spif_regexp_t self)
{
    spif_regexp_t tmp;

    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), SPIF_NULL_TYPE(regexp));

    tmp = spif_regexp_new_from_str(SPIF_STR(self));
    tmp->flags = self->flags;
    spif_regexp_compile(tmp);
    return tmp;
}

spif_classname_t
spif_regexp_type(spif_regexp_t self)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

spif_bool_t
spif_regexp_compile(spif_regexp_t self)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    if (self->data != SPIF_NULL_TYPE(ptr)) {
        FREE(self->data);
    }
#if LIBAST_REGEXP_SUPPORT_PCRE
    {
        const char *errptr;
        int erroffset;

        self->data = SPIF_CAST(ptr) pcre_compile(SPIF_CHARPTR_C(SPIF_STR_STR(SPIF_STR(self))),
                                                 self->flags, &errptr, &erroffset, NULL);
        if (SPIF_PTR_ISNULL(self->data)) {
            libast_print_error("PCRE compilation of \"%s\" failed at offset %d -- %s\n",
                               SPIF_STR_STR(SPIF_STR(self)), erroffset, errptr);
            return FALSE;
        }
        return TRUE;
    }
#elif (LIBAST_REGEXP_SUPPORT_POSIX)
    {
        char buff[256];
        int errcode;

        self->data = SPIF_CAST(ptr) MALLOC(sizeof(regex_t));
        if ((errcode = regcomp(SPIF_CAST_C(regex_t *) self->data, SPIF_STR_STR(SPIF_STR(self)), (self->flags & 0xffff))) != 0) {
            regerror(errcode, SPIF_CAST_C(regex_t *) self->data, buff, sizeof(buff));
            libast_print_error("POSIX regexp compilation of \"%s\" failed -- %s\n", SPIF_STR_STR(SPIF_STR(self)), buff);
            FREE(self->data);
            return FALSE;
        }
        return TRUE;
    }
#elif (LIBAST_REGEXP_SUPPORT_BSD)
    return TRUE;
#endif
    ASSERT_NOTREACHED_RVAL(FALSE);
}

spif_bool_t
spif_regexp_matches_str(spif_regexp_t self, spif_str_t subject)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_STR_ISNULL(subject), FALSE);
#if LIBAST_REGEXP_SUPPORT_PCRE
    {
        int rc;

        rc = pcre_exec(SPIF_CAST_C(pcre *) self->data, NULL, SPIF_CHARPTR_C(SPIF_STR_STR(subject)),
                       spif_str_get_len(subject), 0, 0, NULL, 0);
        if (rc == 0) {
            return TRUE;
        } else if (rc == PCRE_ERROR_NOMATCH) {
            return FALSE;
        } else {
            libast_print_error("PCRE matching error %d on \"%s\"\n", rc, SPIF_STR_STR(subject));
            return FALSE;
        }
    }
#elif (LIBAST_REGEXP_SUPPORT_POSIX)
    {
        int rc;
        char errbuf[256];

        rc = regexec(SPIF_CAST_C(regex_t *) self->data, SPIF_STR_STR(subject), (size_t) 0, (regmatch_t *) NULL,
                     ((self->flags >> 8) & 0xffff));
        if (rc == 0) {
            return TRUE;
        } else if (rc == REG_NOMATCH) {
            return FALSE;
        } else {
            regerror(rc, SPIF_CAST_C(regex_t *) self->data, errbuf, sizeof(errbuf));
            libast_print_error("POSIX regexp matching error on \"%s\" -- %s\n", SPIF_STR_STR(subject), errbuf);
            return FALSE;
        }
    }
#elif (LIBAST_REGEXP_SUPPORT_BSD)
    {
        spif_charptr_t err;

        err = SPIF_CAST(charptr) re_comp(SPIF_STR_STR(SPIF_STR(self)));
        if (err != SPIF_NULL_TYPE(charptr)) {
            libast_print_error("BSD regexp compilation of \"%s\" failed -- %s\n", SPIF_STR_STR(SPIF_STR(self)), err);
            return FALSE;
        }
        return ((re_exec(SPIF_STR_STR(subject)) == 0) ? (FALSE) : (TRUE));
    }
#endif
}

spif_bool_t
spif_regexp_matches_ptr(spif_regexp_t self, spif_charptr_t subject)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_PTR_ISNULL(subject), FALSE);
#if LIBAST_REGEXP_SUPPORT_PCRE
    {
        int rc;

        rc = pcre_exec(SPIF_CAST_C(pcre *) self->data, NULL, SPIF_CHARPTR_C(subject),
                       strlen(SPIF_CHARPTR_C(subject)), 0, 0, NULL, 0);
        if (rc == 0) {
            return TRUE;
        } else if (rc == PCRE_ERROR_NOMATCH) {
            return FALSE;
        } else {
            libast_print_error("PCRE matching error %d on \"%s\"\n", rc, subject);
            return FALSE;
        }
    }
#elif (LIBAST_REGEXP_SUPPORT_POSIX)
    {
        int rc;
        char errbuf[256];

        rc = regexec(SPIF_CAST_C(regex_t *) self->data, subject, (size_t) 0, (regmatch_t *) NULL,
                     ((self->flags >> 8) & 0xffff));
        if (rc == 0) {
            return TRUE;
        } else if (rc == REG_NOMATCH) {
            return FALSE;
        } else {
            regerror(rc, SPIF_CAST_C(regex_t *) self->data, errbuf, sizeof(errbuf));
            libast_print_error("POSIX regexp matching error on \"%s\" -- %s\n", subject, errbuf);
            return FALSE;
        }
    }
#elif (LIBAST_REGEXP_SUPPORT_BSD)
    {
        spif_charptr_t err;

        err = SPIF_CAST(charptr) re_comp(SPIF_STR_STR(SPIF_STR(self)));
        if (err != SPIF_NULL_TYPE(charptr)) {
            libast_print_error("BSD regexp compilation of \"%s\" failed -- %s\n", SPIF_STR_STR(SPIF_STR(self)), err);
            return FALSE;
        }
        return ((re_exec(subject) == 0) ? (FALSE) : (TRUE));
    }
#endif
}

int
spif_regexp_get_flags(spif_regexp_t self)
{
    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), SPIF_CAST_C(int) 0);
    return self->flags;
}

spif_bool_t
spif_regexp_set_flags(spif_regexp_t self, spif_charptr_t flagstr)
{
    spif_charptr_t p;

    ASSERT_RVAL(!SPIF_REGEXP_ISNULL(self), FALSE);
#if LIBAST_REGEXP_SUPPORT_PCRE
    self->flags = 0;
#elif (LIBAST_REGEXP_SUPPORT_POSIX)
    self->flags = REG_EXTENDED | REG_NEWLINE;
#endif

    REQUIRE_RVAL(flagstr != SPIF_NULL_TYPE(charptr), FALSE);
    for (p = flagstr; *p; p++) {
        switch (*p) {
#if LIBAST_REGEXP_SUPPORT_PCRE
            case 'i':  self->flags |= PCRE_CASELESS; break;
            case 'm':  self->flags |= PCRE_MULTILINE; break;
            case 's':  self->flags |= PCRE_DOTALL; break;
            case 'x':  self->flags |= PCRE_EXTENDED; break;
            case 'u':  self->flags |= PCRE_UNGREEDY; break;
            case '8':  self->flags |= PCRE_UTF8; break;
            case '^':  self->flags |= PCRE_NOTBOL; break;
            case '$':  self->flags |= PCRE_NOTEOL; break;
            case 'E':  self->flags |= PCRE_NOTEMPTY; break;
#elif (LIBAST_REGEXP_SUPPORT_POSIX)
            case 'b':  self->flags &= ~REG_EXTENDED; break;
            case 'i':  self->flags |= REG_ICASE; break;
            case 'n':  self->flags |= REG_NOSUB; break;
            case 's':  self->flags &= ~REG_NEWLINE; break;
            case '^':  self->flags |= (REG_NOTBOL << 8); break;
            case '$':  self->flags |= (REG_NOTEOL << 8); break;
#endif
            default:
                libast_print_warning("Unrecognized regexp flag character \'%c\'\n", *p);
                break;
        }
    }
    return spif_regexp_compile(self);
}
