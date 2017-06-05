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

static const char __attribute__((unused)) cvs_ident[] = "$Id: array.c,v 1.18 2004/07/16 23:22:18 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
SPIF_DECL_OBJ(array_iterator) {
    SPIF_DECL_PARENT_TYPE(obj);
    spif_array_t subject;
    spif_listidx_t current_index;
};
/* *INDENT-ON* */

static spif_array_t spif_array_list_new(void);
static spif_array_t spif_array_vector_new(void);
static spif_array_t spif_array_map_new(void);
static spif_bool_t spif_array_list_init(spif_array_t);
static spif_bool_t spif_array_vector_init(spif_array_t);
static spif_bool_t spif_array_map_init(spif_array_t);
static spif_bool_t spif_array_done(spif_array_t);
static spif_bool_t spif_array_del(spif_array_t);
static spif_str_t spif_array_show(spif_array_t, spif_charptr_t, spif_str_t, size_t);
static spif_cmp_t spif_array_comp(spif_array_t, spif_array_t);
static spif_array_t spif_array_list_dup(spif_array_t);
static spif_array_t spif_array_vector_dup(spif_array_t);
static spif_array_t spif_array_map_dup(spif_array_t);
static spif_classname_t spif_array_type(spif_array_t);
static spif_bool_t spif_array_append(spif_array_t, spif_obj_t);
static spif_bool_t spif_array_list_contains(spif_array_t, spif_obj_t);
static spif_bool_t spif_array_vector_contains(spif_array_t, spif_obj_t);
static spif_listidx_t spif_array_count(spif_array_t);
static spif_obj_t spif_array_list_find(spif_array_t, spif_obj_t);
static spif_obj_t spif_array_vector_find(spif_array_t, spif_obj_t);
static spif_obj_t spif_array_get(spif_array_t, spif_listidx_t);
static spif_obj_t spif_array_map_get(spif_array_t self, spif_obj_t key);
static spif_list_t spif_array_get_keys(spif_array_t self, spif_list_t key_list);
static spif_list_t spif_array_get_pairs(spif_array_t self, spif_list_t pair_list);
static spif_list_t spif_array_get_values(spif_array_t self, spif_list_t value_list);
static spif_bool_t spif_array_has_key(spif_array_t self, spif_obj_t key);
static spif_bool_t spif_array_has_value(spif_array_t self, spif_obj_t value);
static spif_listidx_t spif_array_index(spif_array_t, spif_obj_t);
static spif_bool_t spif_array_insert(spif_array_t, spif_obj_t);
static spif_bool_t spif_array_insert_at(spif_array_t, spif_obj_t, spif_listidx_t);
static spif_iterator_t spif_array_iterator(spif_array_t);
static spif_bool_t spif_array_prepend(spif_array_t, spif_obj_t);
static spif_obj_t spif_array_remove(spif_array_t, spif_obj_t);
static spif_obj_t spif_array_remove_at(spif_array_t, spif_listidx_t);
static spif_obj_t spif_array_map_remove(spif_array_t self, spif_obj_t item);
static spif_bool_t spif_array_reverse(spif_array_t);
static spif_bool_t spif_array_set(spif_array_t self, spif_obj_t key, spif_obj_t value);
static spif_obj_t *spif_array_to_array(spif_array_t);
static spif_array_iterator_t spif_array_iterator_new(spif_array_t subject);
static spif_bool_t spif_array_iterator_init(spif_array_iterator_t self, spif_array_t subject);
static spif_bool_t spif_array_iterator_done(spif_array_iterator_t self);
static spif_bool_t spif_array_iterator_del(spif_array_iterator_t self);
static spif_str_t spif_array_iterator_show(spif_array_iterator_t self, spif_charptr_t name, spif_str_t buff, size_t indent);
static spif_cmp_t spif_array_iterator_comp(spif_array_iterator_t self, spif_array_iterator_t other);
static spif_array_iterator_t spif_array_iterator_dup(spif_array_iterator_t self);
static spif_classname_t spif_array_iterator_type(spif_array_iterator_t self);
static spif_bool_t spif_array_iterator_has_next(spif_array_iterator_t self);
static spif_obj_t spif_array_iterator_next(spif_array_iterator_t self);

/* *INDENT-OFF* */
static spif_const_listclass_t a_class = {
    {
        SPIF_DECL_CLASSNAME(array),
        (spif_func_t) spif_array_list_new,
        (spif_func_t) spif_array_list_init,
        (spif_func_t) spif_array_done,
        (spif_func_t) spif_array_del,
        (spif_func_t) spif_array_show,
        (spif_func_t) spif_array_comp,
        (spif_func_t) spif_array_list_dup,
        (spif_func_t) spif_array_type
    },
    (spif_func_t) spif_array_append,
    (spif_func_t) spif_array_list_contains,
    (spif_func_t) spif_array_count,
    (spif_func_t) spif_array_list_find,
    (spif_func_t) spif_array_get,
    (spif_func_t) spif_array_index,
    (spif_func_t) spif_array_insert,
    (spif_func_t) spif_array_insert_at,
    (spif_func_t) spif_array_iterator,
    (spif_func_t) spif_array_prepend,
    (spif_func_t) spif_array_remove,
    (spif_func_t) spif_array_remove_at,
    (spif_func_t) spif_array_reverse,
    (spif_func_t) spif_array_to_array
};
spif_listclass_t SPIF_LISTCLASS_VAR(array) = &a_class;

static spif_const_vectorclass_t av_class = {
    {
        SPIF_DECL_CLASSNAME(array),
        (spif_func_t) spif_array_vector_new,
        (spif_func_t) spif_array_vector_init,
        (spif_func_t) spif_array_done,
        (spif_func_t) spif_array_del,
        (spif_func_t) spif_array_show,
        (spif_func_t) spif_array_comp,
        (spif_func_t) spif_array_vector_dup,
        (spif_func_t) spif_array_type
    },
    (spif_func_t) spif_array_vector_contains,
    (spif_func_t) spif_array_count,
    (spif_func_t) spif_array_vector_find,
    (spif_func_t) spif_array_insert,
    (spif_func_t) spif_array_iterator,
    (spif_func_t) spif_array_remove,
    (spif_func_t) spif_array_to_array
};
spif_vectorclass_t SPIF_VECTORCLASS_VAR(array) = &av_class;

static spif_const_mapclass_t am_class = {
    {
        SPIF_DECL_CLASSNAME(array),
        (spif_func_t) spif_array_map_new,
        (spif_func_t) spif_array_map_init,
        (spif_func_t) spif_array_done,
        (spif_func_t) spif_array_del,
        (spif_func_t) spif_array_show,
        (spif_func_t) spif_array_comp,
        (spif_func_t) spif_array_map_dup,
        (spif_func_t) spif_array_type
    },
    (spif_func_t) spif_array_count,
    (spif_func_t) spif_array_map_get,
    (spif_func_t) spif_array_get_keys,
    (spif_func_t) spif_array_get_pairs,
    (spif_func_t) spif_array_get_values,
    (spif_func_t) spif_array_has_key,
    (spif_func_t) spif_array_has_value,
    (spif_func_t) spif_array_iterator,
    (spif_func_t) spif_array_map_remove,
    (spif_func_t) spif_array_set
};
spif_mapclass_t SPIF_MAPCLASS_VAR(array) = &am_class;

static spif_const_iteratorclass_t ai_class = {
    {
        SPIF_DECL_CLASSNAME(array),
        (spif_func_t) spif_array_iterator_new,
        (spif_func_t) spif_array_iterator_init,
        (spif_func_t) spif_array_iterator_done,
        (spif_func_t) spif_array_iterator_del,
        (spif_func_t) spif_array_iterator_show,
        (spif_func_t) spif_array_iterator_comp,
        (spif_func_t) spif_array_iterator_dup,
        (spif_func_t) spif_array_iterator_type
    },
    (spif_func_t) spif_array_iterator_has_next,
    (spif_func_t) spif_array_iterator_next
};
spif_iteratorclass_t SPIF_ITERATORCLASS_VAR(array) = &ai_class;
/* *INDENT-ON* */

static spif_array_t
spif_array_list_new(void)
{
    spif_array_t self;

    self = SPIF_ALLOC(array);
    if (!spif_array_list_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(array);
    }
    return self;
}

static spif_bool_t
spif_array_list_init(spif_array_t self)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    if (!spif_obj_init(SPIF_OBJ(self))) {
        return FALSE;
    } else if (!spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_LISTCLASS_VAR(array)))) {
        return FALSE;
    }
    self->len = 0;
    self->items = SPIF_NULL_TYPE_C(spif_obj_t *);
    return TRUE;
}

static spif_array_t
spif_array_vector_new(void)
{
    spif_array_t self;

    self = SPIF_ALLOC(array);
    if (!spif_array_vector_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(array);
    }
    return self;
}

static spif_array_t
spif_array_map_new(void)
{
    spif_array_t self;

    self = SPIF_ALLOC(array);
    if (!spif_array_map_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(array);
    }
    return self;
}

static spif_bool_t
spif_array_vector_init(spif_array_t self)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    if (!spif_obj_init(SPIF_OBJ(self))) {
        return FALSE;
    } else if (!spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_VECTORCLASS_VAR(array)))) {
        return FALSE;
    }
    self->len = 0;
    self->items = SPIF_NULL_TYPE_C(spif_obj_t *);
    return TRUE;
}

static spif_bool_t
spif_array_map_init(spif_array_t self)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    if (!spif_obj_init(SPIF_OBJ(self))) {
        return FALSE;
    } else if (!spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MAPCLASS_VAR(array)))) {
        return FALSE;
    }
    self->len = 0;
    self->items = SPIF_NULL_TYPE_C(spif_obj_t *);
    return TRUE;
}

static spif_bool_t
spif_array_done(spif_array_t self)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    for (i = 0; i < self->len; i++) {
        if (!SPIF_OBJ_ISNULL(self->items[i])) {
            SPIF_OBJ_DEL(self->items[i]);
        }
    }
    self->len = 0;
    FREE(self->items);
    return TRUE;
}

static spif_bool_t
spif_array_del(spif_array_t self)
{
    spif_bool_t t;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    t = spif_array_done(self);
    SPIF_DEALLOC(self);
    return t;
}

static spif_str_t
spif_array_show(spif_array_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];
    spif_listidx_t i;

    if (SPIF_LIST_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(array, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_array_t) %s:  %10p {\n", name, SPIF_CAST(ptr) self);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, SPIF_CAST(charptr) tmp);
    }

    if (SPIF_ARRAY_ISNULL(self->items)) {
        spif_str_append_from_ptr(buff, SPIF_CAST(charptr) SPIF_NULLSTR_TYPE_PTR(obj));
    } else {
        for (i = 0; i < self->len; i++) {
            spif_obj_t o = self->items[i];
            sprintf(SPIF_CAST_C(char *) tmp, "item %d", i);
            if (SPIF_OBJ_ISNULL(o)) {
                char tmp2[4096];

                SPIF_OBJ_SHOW_NULL(obj, tmp, buff, indent + 2, tmp2);
            } else {
                buff = SPIF_OBJ_CALL_METHOD(o, show)(o, tmp, buff, indent + 2);
            }
        }
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent, "}\n");
    spif_str_append_from_ptr(buff, SPIF_CAST(charptr) tmp);
    return buff;
}

static spif_cmp_t
spif_array_comp(spif_array_t self, spif_array_t other)
{
    spif_listidx_t i;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    for (i = 0; i < self->len; i++) {
        spif_cmp_t c;

        if (SPIF_OBJ_ISNULL(self->items[i]) && SPIF_OBJ_ISNULL(other->items[i])) {
            continue;
        } else if (SPIF_OBJ_ISNULL(self->items[i])) {
            return SPIF_CMP_LESS;
        } else if (SPIF_OBJ_ISNULL(other->items[i])) {
            return SPIF_CMP_GREATER;
        } 
        c = SPIF_OBJ_COMP(self->items[i], other->items[i]);
        if (!SPIF_CMP_IS_EQUAL(c)) {
            return c;
        }
    }
    return SPIF_CMP_EQUAL;
}

static spif_array_t
spif_array_list_dup(spif_array_t self)
{
    spif_array_t tmp;
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(array));

    tmp = spif_array_list_new();
    REQUIRE_RVAL(!SPIF_ARRAY_ISNULL(tmp), SPIF_NULL_TYPE(array));
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(array));
    tmp->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * self->len);
    for (i = 0; i < self->len; i++) {
        tmp->items[i] = SPIF_CAST(obj) SPIF_OBJ_DUP(SPIF_OBJ(self->items[i]));
    }
    return tmp;
}

static spif_array_t
spif_array_vector_dup(spif_array_t self)
{
    spif_array_t tmp;
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(array));

    tmp = spif_array_vector_new();
    REQUIRE_RVAL(!SPIF_ARRAY_ISNULL(tmp), SPIF_NULL_TYPE(array));
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(array));
    tmp->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * self->len);
    for (i = 0; i < self->len; i++) {
        tmp->items[i] = SPIF_CAST(obj) SPIF_OBJ_DUP(SPIF_OBJ(self->items[i]));
    }
    return tmp;
}

static spif_array_t
spif_array_map_dup(spif_array_t self)
{
    spif_array_t tmp;
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(array));

    tmp = spif_array_map_new();
    REQUIRE_RVAL(!SPIF_ARRAY_ISNULL(tmp), SPIF_NULL_TYPE(array));
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(array));
    tmp->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * self->len);
    for (i = 0; i < self->len; i++) {
        tmp->items[i] = SPIF_CAST(obj) SPIF_OBJ_DUP(SPIF_OBJ(self->items[i]));
    }
    return tmp;
}

static spif_classname_t
spif_array_type(spif_array_t self)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

static spif_bool_t
spif_array_append(spif_array_t self, spif_obj_t obj)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    self->len++;
    if (self->items) {
        self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * self->len);
    } else {
        self->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * self->len);
    }
    self->items[self->len - 1] = obj;
    return TRUE;
}

static spif_bool_t
spif_array_list_contains(spif_array_t self, spif_obj_t obj)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    return ((SPIF_LIST_ISNULL(spif_array_list_find(self, obj))) ? (FALSE) : (TRUE));
}

static spif_bool_t
spif_array_vector_contains(spif_array_t self, spif_obj_t obj)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    return ((SPIF_VECTOR_ISNULL(spif_array_vector_find(self, obj))) ? (FALSE) : (TRUE));
}

static spif_listidx_t
spif_array_count(spif_array_t self)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(listidx));
    return self->len;
}

static spif_obj_t
spif_array_list_find(spif_array_t self, spif_obj_t obj)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), SPIF_NULL_TYPE(obj));
    for (i = 0; i < self->len; i++) {
        if (SPIF_OBJ_ISNULL(self->items[i])) {
            continue;
        }
        if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(self->items[i], obj))) {
            return self->items[i];
        }
    }
    return SPIF_NULL_TYPE(obj);
}

static spif_obj_t
spif_array_vector_find(spif_array_t self, spif_obj_t obj)
{
    spif_listidx_t start, end, mid;
    spif_cmp_t diff;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(self->len > 0, SPIF_NULL_TYPE(obj));

    for (start = 0, end = self->len - 1; start <= end; ) {
        mid = (end - start) / 2 + start;
        diff = SPIF_OBJ_COMP(self->items[mid], obj);
        if (SPIF_CMP_IS_EQUAL(diff)) {
            return self->items[mid];
        } else if (SPIF_CMP_IS_LESS(diff)) {
            start = mid + 1;
        } else {
            end = mid - 1;
            if (end == SPIF_CAST(listidx) -1) {
                break;
            }
        }
    }
    return SPIF_NULL_TYPE(obj);
}

static spif_obj_t
spif_array_get(spif_array_t self, spif_listidx_t idx)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    if (idx < 0) {
        idx += self->len;
    }
    return (((idx >= 0) && (idx < self->len)) ? (self->items[idx]) : (SPIF_NULL_TYPE(obj)));
}

static spif_obj_t
spif_array_map_get(spif_array_t self, spif_obj_t key)
{
    spif_listidx_t start, end, mid;
    spif_cmp_t diff;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(key), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(self->len > 0, SPIF_NULL_TYPE(obj));

    for (start = 0, end = self->len - 1; start <= end; ) {
        mid = (end - start) / 2 + start;
        diff = SPIF_OBJ_COMP(self->items[mid], key);
        if (SPIF_CMP_IS_EQUAL(diff)) {
            return SPIF_OBJPAIR(self->items[mid])->value;
        } else if (SPIF_CMP_IS_LESS(diff)) {
            start = mid + 1;
        } else {
            end = mid - 1;
            if (end == SPIF_CAST(listidx) -1) {
                break;
            }
        }
    }
    return SPIF_NULL_TYPE(obj);
}

static spif_list_t
spif_array_get_keys(spif_array_t self, spif_list_t key_list)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(list));
    if (SPIF_LIST_ISNULL(key_list)) {
        key_list = SPIF_LIST_NEW(array);
    }

    for (i = 0; i < self->len; i++) {
        SPIF_LIST_APPEND(key_list, SPIF_OBJ_DUP(SPIF_OBJPAIR(self->items[i])->key));
    }
    return key_list;
}

static spif_list_t
spif_array_get_pairs(spif_array_t self, spif_list_t pair_list)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(list));
    if (SPIF_LIST_ISNULL(pair_list)) {
        pair_list = SPIF_LIST_NEW(array);
    }

    for (i = 0; i < self->len; i++) {
        SPIF_LIST_APPEND(pair_list, SPIF_OBJ_DUP(SPIF_OBJPAIR(self->items[i])));
    }
    return pair_list;
}

static spif_list_t
spif_array_get_values(spif_array_t self, spif_list_t value_list)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(list));
    if (SPIF_LIST_ISNULL(value_list)) {
        value_list = SPIF_LIST_NEW(array);
    }

    for (i = 0; i < self->len; i++) {
        SPIF_LIST_APPEND(value_list, SPIF_OBJ_DUP(SPIF_OBJPAIR(self->items[i])->value));
    }
    return value_list;
}

static spif_bool_t
spif_array_has_key(spif_array_t self, spif_obj_t key)
{
    return ((SPIF_OBJ_ISNULL(spif_array_map_get(self, key))) ? FALSE : TRUE);
}

static spif_bool_t
spif_array_has_value(spif_array_t self, spif_obj_t value)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), FALSE);

    for (i = 0; i < self->len; i++) {
        spif_objpair_t pair;

        pair = SPIF_OBJPAIR(self->items[i]);
        if (SPIF_OBJ_ISNULL(value) && SPIF_OBJ_ISNULL(pair->value)) {
            return TRUE;
        } else if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(pair->value, value))) {
            return TRUE;
        }
    }
    return FALSE;
}

static spif_listidx_t
spif_array_index(spif_array_t self, spif_obj_t obj)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_CAST(listidx) -1);
    for (i = 0; i < self->len; i++) {
        if (SPIF_OBJ_ISNULL(self->items[i])) {
            if (SPIF_OBJ_ISNULL(obj)) {
                return i;
            }
            continue;
        }
        if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(self->items[i], obj))) {
            return i;
        }
    }
    return SPIF_CAST(listidx) (-1);
}

static spif_bool_t
spif_array_insert(spif_array_t self, spif_obj_t obj)
{
    spif_listidx_t i, left;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), FALSE);
    if (self->items) {
        self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * (self->len + 1));
    } else {
        self->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * (self->len + 1));
    }

    for (i = 0; i < self->len && SPIF_CMP_IS_GREATER(SPIF_OBJ_COMP(obj, self->items[i])); i++);
    left = self->len - i;
    if (left) {
        memmove(self->items + i + 1, self->items + i, SPIF_SIZEOF_TYPE(obj) * left);
    }
    self->items[i] = obj;
    self->len++;
    return TRUE;
}

static spif_bool_t
spif_array_insert_at(spif_array_t self, spif_obj_t obj, spif_listidx_t idx)
{
    spif_listidx_t left;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), FALSE);
    if (idx < 0) {
        /* Negative indexes go backward from the end of the list. */
        idx += self->len;
    }
    REQUIRE_RVAL((idx + 1) >= 0, FALSE);

    if (idx > self->len) {
        /* The array is going to grow by more than 1; we'll need to pad with NULL's. */
        left = -(idx - self->len);
        self->len = idx;
    } else {
        left = self->len - idx;
    }

    if (self->items) {
        self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * (self->len + 1));
    } else {
        self->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * (self->len + 1));
    }

    if (left > 0) {
        /* Move the stuff to the right of idx over one. */
        memmove(self->items + idx + 1, self->items + idx, SPIF_SIZEOF_TYPE(obj) * left);
    } else if (left < 0) {
        /* NULL out the new gap in the list. */
        left = -left;
        MEMSET(self->items + (idx - left), 0, SPIF_SIZEOF_TYPE(obj) * left);
    }
    self->items[idx] = obj;
    self->len++;
    return TRUE;
}

static spif_iterator_t
spif_array_iterator(spif_array_t self)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(iterator));
    return SPIF_CAST(iterator) spif_array_iterator_new(self);
}

static spif_bool_t
spif_array_prepend(spif_array_t self, spif_obj_t obj)
{
    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), FALSE);
    if (self->items) {
        self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * (self->len + 1));
    } else {
        self->items = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * (self->len + 1));
    }

    memmove(self->items + 1, self->items, SPIF_SIZEOF_TYPE(obj) * self->len);
    self->items[0] = obj;
    self->len++;
    return TRUE;
}

static spif_obj_t
spif_array_remove(spif_array_t self, spif_obj_t item)
{
    spif_obj_t tmp;
    spif_listidx_t i, left;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(item), SPIF_NULL_TYPE(obj));
    for (i = 0; i < self->len && !SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(item, self->items[i])); i++);
    if (i == self->len) {
        return SPIF_NULL_TYPE(obj);
    }

    left = self->len - i - 1;
    tmp = self->items[i];
    memmove(self->items + i, self->items + i + 1, SPIF_SIZEOF_TYPE(obj) * left);
    self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * (--(self->len)));

    return tmp;
}

static spif_obj_t
spif_array_map_remove(spif_array_t self, spif_obj_t item)
{
    spif_obj_t tmp;
    spif_listidx_t i, left;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(item), SPIF_NULL_TYPE(obj));
    for (i = 0; i < self->len && !SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(self->items[i], item)); i++);
    if (i == self->len) {
        return SPIF_NULL_TYPE(obj);
    }

    left = self->len - i - 1;
    tmp = self->items[i];
    memmove(self->items + i, self->items + i + 1, SPIF_SIZEOF_TYPE(obj) * left);
    self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * (--(self->len)));

    return tmp;
}

static spif_obj_t
spif_array_remove_at(spif_array_t self, spif_listidx_t idx)
{
    spif_obj_t tmp;
    spif_listidx_t left;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE(obj));
    if (idx < 0) {
        /* Negative indexes go backward from the end of the list. */
        idx += self->len;
    }
    if ((idx < 0) || (idx >= self->len)) {
        return SPIF_NULL_TYPE(obj);
    }

    left = self->len - idx - 1;
    tmp = self->items[idx];
    memmove(self->items + idx, self->items + idx + 1, SPIF_SIZEOF_TYPE(obj) * left);
    self->items = SPIF_CAST_C(spif_obj_t *) REALLOC(self->items, SPIF_SIZEOF_TYPE(obj) * (--(self->len)));

    return tmp;
}

static spif_bool_t
spif_array_reverse(spif_array_t self)
{
    spif_listidx_t i, j;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), FALSE);
    for (i = 0, j = self->len - 1; i < j; i++, j--) {
        SWAP(self->items[i], self->items[j]);
    }
    return TRUE;
}

static spif_bool_t
spif_array_set(spif_array_t self, spif_obj_t key, spif_obj_t value)
{
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(key), FALSE);

    if (SPIF_OBJ_IS_OBJPAIR(key) && SPIF_OBJ_ISNULL(value)) {
        value = SPIF_OBJ(SPIF_OBJPAIR(key)->value);
        key = SPIF_OBJ(SPIF_OBJPAIR(key)->key);
    }

    for (i = 0; i < self->len && !SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(self->items[i], key)); i++);
    if (i == self->len) {
        spif_array_insert(self, SPIF_OBJ(spif_objpair_new_from_both(key, value)));
        return FALSE;
    } else {
        spif_objpair_set_value(SPIF_OBJPAIR(self->items[i]), SPIF_OBJ_DUP(value));
        return TRUE;
    }
}

static spif_obj_t *
spif_array_to_array(spif_array_t self)
{
    spif_obj_t *tmp;
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_ARRAY_ISNULL(self), SPIF_NULL_TYPE_PTR(obj));
    tmp = SPIF_CAST_PTR(obj) MALLOC(SPIF_SIZEOF_TYPE(obj) * self->len);
    for (i = 0; i < self->len; i++) {
        tmp[i] = SPIF_OBJ(self->items[i]);
    }
    return tmp;
}

static spif_array_iterator_t
spif_array_iterator_new(spif_array_t subject)
{
    spif_array_iterator_t self;

    self = SPIF_ALLOC(array_iterator);
    if (!spif_array_iterator_init(self, subject)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(array_iterator);
    }
    return self;
}

static spif_bool_t
spif_array_iterator_init(spif_array_iterator_t self, spif_array_t subject)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    if (!spif_obj_init(SPIF_OBJ(self))) {
        return FALSE;
    } else if (!spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_ITERATORCLASS_VAR(array)))) {
        return FALSE;
    }
    self->subject = subject;
    self->current_index = 0;
    return TRUE;
}

static spif_bool_t
spif_array_iterator_done(spif_array_iterator_t self)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    self->subject = SPIF_NULL_TYPE(array);
    self->current_index = 0;
    return TRUE;
}

static spif_bool_t
spif_array_iterator_del(spif_array_iterator_t self)
{
    spif_bool_t t;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    t = spif_array_iterator_done(self);
    SPIF_DEALLOC(self);
    return t;
}

static spif_str_t
spif_array_iterator_show(spif_array_iterator_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_ITERATOR_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(iterator, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_array_iterator_t) %s:  %10p {\n", name, SPIF_CAST(ptr) self);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    buff = spif_array_show(self->subject, SPIF_CAST(charptr) "subject", buff, indent + 2);

    memset(tmp, ' ', indent + 2);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "  (spif_listidx_t) current_index:  %lu\n",
             SPIF_CAST_C(unsigned long) self->current_index);
    spif_str_append_from_ptr(buff, tmp);

    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent, "}\n");
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

static spif_cmp_t
spif_array_iterator_comp(spif_array_iterator_t self, spif_array_iterator_t other)
{
    spif_cmp_t c;

    SPIF_OBJ_COMP_CHECK_NULL(self, other);

    c = spif_array_comp(self->subject, other->subject);
    if (SPIF_CMP_IS_EQUAL(c)) {
        return SPIF_CMP_FROM_INT((int) (self->current_index - other->current_index));
    } else {
        return c;
    }
}

static spif_array_iterator_t
spif_array_iterator_dup(spif_array_iterator_t self)
{
    spif_array_iterator_t tmp;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), SPIF_NULL_TYPE(array_iterator));
    tmp = spif_array_iterator_new(self->subject);
    tmp->current_index = self->current_index;
    return tmp;
}

static spif_classname_t
spif_array_iterator_type(spif_array_iterator_t self)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

static spif_bool_t
spif_array_iterator_has_next(spif_array_iterator_t self)
{
    spif_array_t subject;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    subject = self->subject;
    REQUIRE_RVAL(!SPIF_LIST_ISNULL(subject), FALSE);
    if (self->current_index >= subject->len) {
        return FALSE;
    } else {
        return TRUE;
    }
}

static spif_obj_t
spif_array_iterator_next(spif_array_iterator_t self)
{
    spif_obj_t tmp;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_LIST_ISNULL(self->subject), SPIF_NULL_TYPE(obj));
    tmp = spif_array_get(self->subject, self->current_index);
    self->current_index++;
    return tmp;
}
