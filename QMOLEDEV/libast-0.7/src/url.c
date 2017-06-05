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

static const char __attribute__((unused)) cvs_ident[] = "$Id: url.c,v 1.20 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
static SPIF_CONST_TYPE(class) u_class = {
    SPIF_DECL_CLASSNAME(url),
    (spif_func_t) spif_url_new,
    (spif_func_t) spif_url_init,
    (spif_func_t) spif_url_done,
    (spif_func_t) spif_url_del,
    (spif_func_t) spif_url_show,
    (spif_func_t) spif_url_comp,
    (spif_func_t) spif_url_dup,
    (spif_func_t) spif_url_type
};
SPIF_TYPE(class) SPIF_CLASS_VAR(url) = &u_class;
/* *INDENT-ON* */

static spif_bool_t spif_url_parse(spif_url_t);

spif_url_t
spif_url_new(void)
{
    spif_url_t self;

    self = SPIF_ALLOC(url);
    if (!spif_url_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(url);
    }
    return self;
}

spif_url_t
spif_url_new_from_str(spif_str_t other)
{
    spif_url_t self;

    self = SPIF_ALLOC(url);
    if (!spif_url_init_from_str(self, other)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(url);
    }
    return self;
}

spif_url_t
spif_url_new_from_ptr(spif_charptr_t other)
{
    spif_url_t self;

    self = SPIF_ALLOC(url);
    if (!spif_url_init_from_ptr(self, other)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(url);
    }
    return self;
}

spif_bool_t
spif_url_init(spif_url_t self)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    if (!spif_str_init(SPIF_STR(self))) {
        return FALSE;
    }
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(url));
    self->proto = SPIF_NULL_TYPE(str);
    self->user = SPIF_NULL_TYPE(str);
    self->passwd = SPIF_NULL_TYPE(str);
    self->host = SPIF_NULL_TYPE(str);
    self->port = SPIF_NULL_TYPE(str);
    self->path = SPIF_NULL_TYPE(str);
    self->query = SPIF_NULL_TYPE(str);
    return TRUE;
}

spif_bool_t
spif_url_init_from_str(spif_url_t self, spif_str_t other)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    if (!spif_str_init_from_ptr(SPIF_STR(self), SPIF_STR_STR(other))) {
        return FALSE;
    }
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(url));
    self->proto = SPIF_NULL_TYPE(str);
    self->user = SPIF_NULL_TYPE(str);
    self->passwd = SPIF_NULL_TYPE(str);
    self->host = SPIF_NULL_TYPE(str);
    self->port = SPIF_NULL_TYPE(str);
    self->path = SPIF_NULL_TYPE(str);
    self->query = SPIF_NULL_TYPE(str);
    spif_url_parse(self);
    return TRUE;
}

spif_bool_t
spif_url_init_from_ptr(spif_url_t self, spif_charptr_t other)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    if (!spif_str_init_from_ptr(SPIF_STR(self), other)) {
        return FALSE;
    }
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS_VAR(url));
    self->proto = SPIF_NULL_TYPE(str);
    self->user = SPIF_NULL_TYPE(str);
    self->passwd = SPIF_NULL_TYPE(str);
    self->host = SPIF_NULL_TYPE(str);
    self->port = SPIF_NULL_TYPE(str);
    self->path = SPIF_NULL_TYPE(str);
    self->query = SPIF_NULL_TYPE(str);
    spif_url_parse(self);
    return TRUE;
}

spif_bool_t
spif_url_done(spif_url_t self)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    if (!SPIF_STR_ISNULL(self->proto)) {
        spif_str_del(self->proto);
        self->proto = SPIF_NULL_TYPE(str);
    }
    if (!SPIF_STR_ISNULL(self->user)) {
        spif_str_del(self->user);
        self->user = SPIF_NULL_TYPE(str);
    }
    if (!SPIF_STR_ISNULL(self->passwd)) {
        spif_str_del(self->passwd);
        self->passwd = SPIF_NULL_TYPE(str);
    }
    if (!SPIF_STR_ISNULL(self->host)) {
        spif_str_del(self->host);
        self->host = SPIF_NULL_TYPE(str);
    }
    if (!SPIF_STR_ISNULL(self->port)) {
        spif_str_del(self->port);
        self->port = SPIF_NULL_TYPE(str);
    }
    if (!SPIF_STR_ISNULL(self->path)) {
        spif_str_del(self->path);
        self->path = SPIF_NULL_TYPE(str);
    }
    if (!SPIF_STR_ISNULL(self->query)) {
        spif_str_del(self->query);
        self->query = SPIF_NULL_TYPE(str);
    }
    spif_str_done(SPIF_STR(self));
    return TRUE;
}

spif_bool_t
spif_url_del(spif_url_t self)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    spif_url_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

spif_str_t
spif_url_show(spif_url_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_URL_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(url, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CHARPTR_C(tmp) + indent, sizeof(tmp) - indent,
             "(spif_url_t) %s:  %10p {\n",
             name, SPIF_CAST(ptr) self);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    buff = spif_str_show(self->proto, SPIF_CHARPTR("proto"), buff, indent + 2);
    buff = spif_str_show(self->user, SPIF_CHARPTR("user"), buff, indent + 2);
    buff = spif_str_show(self->passwd, SPIF_CHARPTR("passwd"), buff, indent + 2);
    buff = spif_str_show(self->host, SPIF_CHARPTR("host"), buff, indent + 2);
    buff = spif_str_show(self->port, SPIF_CHARPTR("port"), buff, indent + 2);
    buff = spif_str_show(self->path, SPIF_CHARPTR("path"), buff, indent + 2);
    buff = spif_str_show(self->query, SPIF_CHARPTR("query"), buff, indent + 2);

    snprintf(SPIF_CHARPTR_C(tmp) + indent, sizeof(tmp) - indent, "}\n");
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

spif_cmp_t
spif_url_comp(spif_url_t self, spif_url_t other)
{
    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    return spif_str_comp(SPIF_STR(self), SPIF_STR(other));
}

spif_url_t
spif_url_dup(spif_url_t self)
{
    spif_url_t tmp;

    ASSERT_RVAL(!SPIF_URL_ISNULL(self), SPIF_NULL_TYPE(url));
    tmp = spif_url_new_from_str(SPIF_STR(self));
    return tmp;
}

spif_classname_t
spif_url_type(spif_url_t self)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

SPIF_DEFINE_PROPERTY_FUNC(url, str, proto)
SPIF_DEFINE_PROPERTY_FUNC(url, str, user)
SPIF_DEFINE_PROPERTY_FUNC(url, str, passwd)
SPIF_DEFINE_PROPERTY_FUNC(url, str, host)
SPIF_DEFINE_PROPERTY_FUNC(url, str, port)
SPIF_DEFINE_PROPERTY_FUNC(url, str, path)
SPIF_DEFINE_PROPERTY_FUNC(url, str, query)

static spif_bool_t
spif_url_parse(spif_url_t self)
{
    spif_charptr_t s = SPIF_STR_STR(SPIF_STR(self));
    spif_charptr_t pstr, pend, ptmp;

    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    pstr = s;

    /* Check for "proto:" at the beginning. */
    pend = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(s), ':'));
    if (pend != NULL) {
        for (; pstr < pend; pstr++) {
            if (!isalnum(*pstr)) {
                break;
            }
        }
        if (pstr == pend) {
            /* Got one. */
            self->proto = spif_str_new_from_buff(s, pend - s);
            pstr++;
        } else {
            /* Nope, reset. */
            pstr = s;
        }
    }

    if ((*pstr == '/') && (pstr[1] == '/')) {
        pstr += 2;
    }

    /* Knock out the path and query if they're there. */
    pend = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(pstr), '/'));
    if (pend != NULL) {
        spif_charptr_t tmp = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(pend), '?'));

        if (tmp != NULL) {
            self->query = spif_str_new_from_ptr(tmp + 1);
            self->path = spif_str_new_from_buff(pend, tmp - pend);
        } else {
          self->path = spif_str_new_from_ptr(pend);
        }
    } else if ((pend = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(pstr), '?'))) != NULL) {
        self->query = spif_str_new_from_ptr(pend + 1);
    } else {
        for (pend = pstr; *pend; pend++);
    }
    /* At this point, pend *must* point to the end of the user/pass/host/port part. */

    /* Check for an @ sign, which would mean we have auth info. */
    ptmp = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(pstr), '@'));
    if ((ptmp != NULL) && (ptmp < pend)) {
        spif_charptr_t tmp = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(pstr), ':'));

        if ((tmp != NULL) && (tmp < ptmp)) {
            /* Both username and password. */
            self->user = spif_str_new_from_buff(pstr, tmp - pstr);
            self->passwd = spif_str_new_from_buff((tmp + 1), ptmp - tmp - 1);
        } else {
            self->user = spif_str_new_from_buff(pstr, ptmp - pstr);
        }
        pstr = ptmp + 1;
    }

    /* All that remains now between pstr and pend is host and maybe port. */
    ptmp = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(pstr), ':'));
    if ((ptmp != NULL) && (ptmp < pend)) {
        self->host = spif_str_new_from_buff(pstr, ptmp - pstr);
        self->port = spif_str_new_from_buff((ptmp + 1), pend - ptmp - 1);
    } else if (pstr != pend) {
        self->host = spif_str_new_from_buff(pstr, pend - pstr);
    }

    /* If we have a proto but no port, see if we can resolve the port using the proto. */
    if (SPIF_STR_ISNULL(self->port) && !SPIF_STR_ISNULL(self->proto)) {
        spif_protoinfo_t proto;
        spif_servinfo_t serv;

        proto = getprotobyname(SPIF_CHARPTR_C(SPIF_STR_STR(self->proto)));
        if (proto == NULL) {
            /* If it's not a protocol, it's probably a service. */
            serv = getservbyname(SPIF_CHARPTR_C(SPIF_STR_STR(self->proto)), "tcp");
            if (serv == NULL) {
                serv = getservbyname(SPIF_CHARPTR_C(SPIF_STR_STR(self->proto)), "udp");
            }
            if (serv != NULL) {
                proto = getprotobyname(serv->s_proto);
                REQUIRE_RVAL(proto != NULL, FALSE);
            }
        }
        if (proto != NULL) {
            spif_char_t buff[32];

            snprintf(SPIF_CHARPTR_C(buff), sizeof(buff), "%d", ntohs(serv->s_port));
            self->port = spif_str_new_from_ptr(buff);
        }
    }

    return TRUE;
}

spif_bool_t
spif_url_unparse(spif_url_t self)
{
    ASSERT_RVAL(!SPIF_URL_ISNULL(self), FALSE);
    spif_str_done(SPIF_STR(self));
    spif_str_init_from_ptr(SPIF_STR(self), SPIF_CHARPTR(""));

    /* First, proto followed by a colon. */
    if (!SPIF_STR_ISNULL(self->proto)) {
        spif_str_append(SPIF_STR(self), self->proto);
        spif_str_append_char(SPIF_STR(self), ':');
    }

    /* If we have a port but no host, make it localhost. */
    if (!SPIF_STR_ISNULL(self->port) && SPIF_STR_ISNULL(self->host)) {
        self->host = spif_str_new_from_ptr(SPIF_CHARPTR("localhost"));
    }

    /* We need the // if we have a hostname. */
    if (!SPIF_STR_ISNULL(self->host)) {
        spif_str_append_from_ptr(SPIF_STR(self), SPIF_CHARPTR("//"));
    }

    if (!SPIF_STR_ISNULL(self->user)) {
        spif_str_append(SPIF_STR(self), self->user);
        if (!SPIF_STR_ISNULL(self->passwd)) {
            spif_str_append_char(SPIF_STR(self), ':');
            spif_str_append(SPIF_STR(self), self->passwd);
        }
        spif_str_append_char(SPIF_STR(self), '@');
    }

    if (!SPIF_STR_ISNULL(self->host)) {
        spif_str_append(SPIF_STR(self), self->host);
        if (!SPIF_STR_ISNULL(self->port)) {
            spif_str_append_char(SPIF_STR(self), ':');
            spif_str_append(SPIF_STR(self), self->port);
        }
    }

    if (!SPIF_STR_ISNULL(self->path)) {
        spif_str_append(SPIF_STR(self), self->path);
    }

    if (!SPIF_STR_ISNULL(self->query)) {
        spif_str_append_char(SPIF_STR(self), '?');
        spif_str_append(SPIF_STR(self), self->query);
    }
    return TRUE;
}
