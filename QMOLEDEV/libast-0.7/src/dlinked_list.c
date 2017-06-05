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

static const char __attribute__((unused)) cvs_ident[] = "$Id: dlinked_list.c,v 1.21 2004/07/20 22:32:08 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/* *INDENT-OFF* */
SPIF_DECL_OBJ(dlinked_list_iterator) {
    SPIF_DECL_PARENT_TYPE(obj);
    SPIF_DECL_PROPERTY(dlinked_list, subject);
    SPIF_DECL_PROPERTY(dlinked_list_item, current);
};
/* *INDENT-ON* */

static spif_dlinked_list_item_t spif_dlinked_list_item_new(void);
static spif_bool_t spif_dlinked_list_item_init(spif_dlinked_list_item_t);
static spif_bool_t spif_dlinked_list_item_done(spif_dlinked_list_item_t);
static spif_bool_t spif_dlinked_list_item_del(spif_dlinked_list_item_t);
static spif_str_t spif_dlinked_list_item_show(spif_dlinked_list_item_t, spif_charptr_t, spif_str_t, size_t);
static spif_cmp_t spif_dlinked_list_item_comp(spif_dlinked_list_item_t, spif_dlinked_list_item_t);
static spif_dlinked_list_item_t spif_dlinked_list_item_dup(spif_dlinked_list_item_t);
static spif_classname_t spif_dlinked_list_item_type(spif_dlinked_list_item_t);
SPIF_DECL_PROPERTY_FUNC(dlinked_list_item, obj, data);
SPIF_DECL_PROPERTY_FUNC(dlinked_list_item, dlinked_list_item, prev);
SPIF_DECL_PROPERTY_FUNC(dlinked_list_item, dlinked_list_item, next);

static spif_dlinked_list_t spif_dlinked_list_new(void);
static spif_dlinked_list_t spif_dlinked_list_vector_new(void);
static spif_dlinked_list_t spif_dlinked_list_map_new(void);
static spif_bool_t spif_dlinked_list_init(spif_dlinked_list_t);
static spif_bool_t spif_dlinked_list_vector_init(spif_dlinked_list_t);
static spif_bool_t spif_dlinked_list_map_init(spif_dlinked_list_t);
static spif_bool_t spif_dlinked_list_done(spif_dlinked_list_t);
static spif_bool_t spif_dlinked_list_del(spif_dlinked_list_t);
static spif_str_t spif_dlinked_list_show(spif_dlinked_list_t, spif_charptr_t, spif_str_t, size_t);
static spif_cmp_t spif_dlinked_list_comp(spif_dlinked_list_t, spif_dlinked_list_t);
static spif_dlinked_list_t spif_dlinked_list_dup(spif_dlinked_list_t);
static spif_dlinked_list_t spif_dlinked_list_vector_dup(spif_dlinked_list_t);
static spif_dlinked_list_t spif_dlinked_list_map_dup(spif_dlinked_list_t);
static spif_classname_t spif_dlinked_list_type(spif_dlinked_list_t);
static spif_bool_t spif_dlinked_list_append(spif_dlinked_list_t self, spif_obj_t obj);
static spif_bool_t spif_dlinked_list_contains(spif_dlinked_list_t self, spif_obj_t obj);
static spif_bool_t spif_dlinked_list_vector_contains(spif_dlinked_list_t self, spif_obj_t obj);
static spif_listidx_t spif_dlinked_list_count(spif_dlinked_list_t self);
static spif_obj_t spif_dlinked_list_find(spif_dlinked_list_t self, spif_obj_t obj);
static spif_obj_t spif_dlinked_list_vector_find(spif_dlinked_list_t self, spif_obj_t obj);
static spif_obj_t spif_dlinked_list_get(spif_dlinked_list_t self, spif_listidx_t idx);
static spif_obj_t spif_dlinked_list_map_get(spif_dlinked_list_t self, spif_obj_t key);
static spif_list_t spif_dlinked_list_get_keys(spif_dlinked_list_t self, spif_list_t key_list);
static spif_list_t spif_dlinked_list_get_pairs(spif_dlinked_list_t self, spif_list_t pair_list);
static spif_list_t spif_dlinked_list_get_values(spif_dlinked_list_t self, spif_list_t value_list);
static spif_bool_t spif_dlinked_list_has_key(spif_dlinked_list_t self, spif_obj_t key);
static spif_bool_t spif_dlinked_list_has_value(spif_dlinked_list_t self, spif_obj_t value);
static spif_listidx_t spif_dlinked_list_index(spif_dlinked_list_t self, spif_obj_t obj);
static spif_bool_t spif_dlinked_list_insert(spif_dlinked_list_t self, spif_obj_t obj);
static spif_bool_t spif_dlinked_list_insert_at(spif_dlinked_list_t self, spif_obj_t obj, spif_listidx_t idx);
static spif_iterator_t spif_dlinked_list_iterator(spif_dlinked_list_t self);
static spif_bool_t spif_dlinked_list_prepend(spif_dlinked_list_t self, spif_obj_t obj);
static spif_obj_t spif_dlinked_list_remove(spif_dlinked_list_t self, spif_obj_t item);
static spif_obj_t spif_dlinked_list_map_remove(spif_dlinked_list_t self, spif_obj_t item);
static spif_obj_t spif_dlinked_list_remove_at(spif_dlinked_list_t self, spif_listidx_t idx);
static spif_bool_t spif_dlinked_list_reverse(spif_dlinked_list_t self);
static spif_bool_t spif_dlinked_list_set(spif_dlinked_list_t self, spif_obj_t key, spif_obj_t value);
static spif_obj_t * spif_dlinked_list_to_array(spif_dlinked_list_t self);
SPIF_DECL_PROPERTY_FUNC(dlinked_list, listidx, len);
SPIF_DECL_PROPERTY_FUNC(dlinked_list, dlinked_list_item, head);
SPIF_DECL_PROPERTY_FUNC(dlinked_list, dlinked_list_item, tail);

static spif_dlinked_list_iterator_t spif_dlinked_list_iterator_new(spif_dlinked_list_t subject);
static spif_bool_t spif_dlinked_list_iterator_init(spif_dlinked_list_iterator_t self, spif_dlinked_list_t subject);
static spif_bool_t spif_dlinked_list_iterator_done(spif_dlinked_list_iterator_t self);
static spif_bool_t spif_dlinked_list_iterator_del(spif_dlinked_list_iterator_t self);
static spif_str_t spif_dlinked_list_iterator_show(spif_dlinked_list_iterator_t self, spif_charptr_t name, spif_str_t buff, size_t indent);
static spif_cmp_t spif_dlinked_list_iterator_comp(spif_dlinked_list_iterator_t self, spif_dlinked_list_iterator_t other);
static spif_dlinked_list_iterator_t spif_dlinked_list_iterator_dup(spif_dlinked_list_iterator_t self);
static spif_classname_t spif_dlinked_list_iterator_type(spif_dlinked_list_iterator_t self);
static spif_bool_t spif_dlinked_list_iterator_has_next(spif_dlinked_list_iterator_t self);
static spif_obj_t spif_dlinked_list_iterator_next(spif_dlinked_list_iterator_t self);
SPIF_DECL_PROPERTY_FUNC(dlinked_list_iterator, dlinked_list, subject);
SPIF_DECL_PROPERTY_FUNC(dlinked_list_iterator, dlinked_list_item, current);

/* *INDENT-OFF* */
static SPIF_CONST_TYPE(class) dli_class = {
    SPIF_DECL_CLASSNAME(dlinked_list_item),
    (spif_func_t) spif_dlinked_list_item_new,
    (spif_func_t) spif_dlinked_list_item_init,
    (spif_func_t) spif_dlinked_list_item_done,
    (spif_func_t) spif_dlinked_list_item_del,
    (spif_func_t) spif_dlinked_list_item_show,
    (spif_func_t) spif_dlinked_list_item_comp,
    (spif_func_t) spif_dlinked_list_item_dup,
    (spif_func_t) spif_dlinked_list_item_type
};
SPIF_TYPE(class) SPIF_CLASS_VAR(dlinked_list_item) = &dli_class;

static spif_const_listclass_t dl_class = {
    {
        SPIF_DECL_CLASSNAME(dlinked_list),
        (spif_func_t) spif_dlinked_list_new,
        (spif_func_t) spif_dlinked_list_init,
        (spif_func_t) spif_dlinked_list_done,
        (spif_func_t) spif_dlinked_list_del,
        (spif_func_t) spif_dlinked_list_show,
        (spif_func_t) spif_dlinked_list_comp,
        (spif_func_t) spif_dlinked_list_dup,
        (spif_func_t) spif_dlinked_list_type
    },
    (spif_func_t) spif_dlinked_list_append,
    (spif_func_t) spif_dlinked_list_contains,
    (spif_func_t) spif_dlinked_list_count,
    (spif_func_t) spif_dlinked_list_find,
    (spif_func_t) spif_dlinked_list_get,
    (spif_func_t) spif_dlinked_list_index,
    (spif_func_t) spif_dlinked_list_insert,
    (spif_func_t) spif_dlinked_list_insert_at,
    (spif_func_t) spif_dlinked_list_iterator,
    (spif_func_t) spif_dlinked_list_prepend,
    (spif_func_t) spif_dlinked_list_remove,
    (spif_func_t) spif_dlinked_list_remove_at,
    (spif_func_t) spif_dlinked_list_reverse,
    (spif_func_t) spif_dlinked_list_to_array
};
spif_listclass_t SPIF_LISTCLASS_VAR(dlinked_list) = &dl_class;

static spif_const_vectorclass_t dlv_class = {
    {
        SPIF_DECL_CLASSNAME(dlinked_list),
        (spif_func_t) spif_dlinked_list_vector_new,
        (spif_func_t) spif_dlinked_list_vector_init,
        (spif_func_t) spif_dlinked_list_done,
        (spif_func_t) spif_dlinked_list_del,
        (spif_func_t) spif_dlinked_list_show,
        (spif_func_t) spif_dlinked_list_comp,
        (spif_func_t) spif_dlinked_list_vector_dup,
        (spif_func_t) spif_dlinked_list_type
    },
    (spif_func_t) spif_dlinked_list_vector_contains,
    (spif_func_t) spif_dlinked_list_count,
    (spif_func_t) spif_dlinked_list_vector_find,
    (spif_func_t) spif_dlinked_list_insert,
    (spif_func_t) spif_dlinked_list_iterator,
    (spif_func_t) spif_dlinked_list_remove,
    (spif_func_t) spif_dlinked_list_to_array
};
spif_vectorclass_t SPIF_VECTORCLASS_VAR(dlinked_list) = &dlv_class;

static spif_const_mapclass_t dllm_class = {
    {
        SPIF_DECL_CLASSNAME(dlinked_list),
        (spif_func_t) spif_dlinked_list_map_new,
        (spif_func_t) spif_dlinked_list_map_init,
        (spif_func_t) spif_dlinked_list_done,
        (spif_func_t) spif_dlinked_list_del,
        (spif_func_t) spif_dlinked_list_show,
        (spif_func_t) spif_dlinked_list_comp,
        (spif_func_t) spif_dlinked_list_map_dup,
        (spif_func_t) spif_dlinked_list_type
    },
    (spif_func_t) spif_dlinked_list_count,
    (spif_func_t) spif_dlinked_list_map_get,
    (spif_func_t) spif_dlinked_list_get_keys,
    (spif_func_t) spif_dlinked_list_get_pairs,
    (spif_func_t) spif_dlinked_list_get_values,
    (spif_func_t) spif_dlinked_list_has_key,
    (spif_func_t) spif_dlinked_list_has_value,
    (spif_func_t) spif_dlinked_list_iterator,
    (spif_func_t) spif_dlinked_list_map_remove,
    (spif_func_t) spif_dlinked_list_set
};
spif_mapclass_t SPIF_MAPCLASS_VAR(dlinked_list) = &dllm_class;

static spif_const_iteratorclass_t dlli_class = {
    {
        SPIF_DECL_CLASSNAME(dlinked_list),
        (spif_func_t) spif_dlinked_list_iterator_new,
        (spif_func_t) spif_dlinked_list_iterator_init,
        (spif_func_t) spif_dlinked_list_iterator_done,
        (spif_func_t) spif_dlinked_list_iterator_del,
        (spif_func_t) spif_dlinked_list_iterator_show,
        (spif_func_t) spif_dlinked_list_iterator_comp,
        (spif_func_t) spif_dlinked_list_iterator_dup,
        (spif_func_t) spif_dlinked_list_iterator_type
    },
    (spif_func_t) spif_dlinked_list_iterator_has_next,
    (spif_func_t) spif_dlinked_list_iterator_next
};
spif_iteratorclass_t SPIF_ITERATORCLASS_VAR(dlinked_list) = &dlli_class;
/* *INDENT-ON* */

static spif_dlinked_list_item_t
spif_dlinked_list_item_new(void)
{
    spif_dlinked_list_item_t self;

    self = SPIF_ALLOC(dlinked_list_item);
    if (!spif_dlinked_list_item_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(dlinked_list_item);
    }
    return self;
}

static spif_bool_t
spif_dlinked_list_item_init(spif_dlinked_list_item_t self)
{
    ASSERT_RVAL(!SPIF_DLINKED_LIST_ITEM_ISNULL(self), FALSE);
    self->data = SPIF_NULL_TYPE(obj);
    self->prev = SPIF_NULL_TYPE(dlinked_list_item);
    self->next = SPIF_NULL_TYPE(dlinked_list_item);
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_item_done(spif_dlinked_list_item_t self)
{
    ASSERT_RVAL(!SPIF_DLINKED_LIST_ITEM_ISNULL(self), FALSE);
    if (self->data != SPIF_NULL_TYPE(obj)) {
        SPIF_OBJ_DEL(self->data);
    }
    self->data = SPIF_NULL_TYPE(obj);
    self->prev = SPIF_NULL_TYPE(dlinked_list_item);
    self->next = SPIF_NULL_TYPE(dlinked_list_item);
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_item_del(spif_dlinked_list_item_t self)
{
    ASSERT_RVAL(!SPIF_DLINKED_LIST_ITEM_ISNULL(self), FALSE);
    spif_dlinked_list_item_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

static spif_str_t
spif_dlinked_list_item_show(spif_dlinked_list_item_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(dlinked_list_item, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_dlinked_list_item_t) %s (%9p <- %9p -> %9p):  ",
             name, SPIF_CAST(ptr) self->prev, SPIF_CAST(ptr) self,
             SPIF_CAST(ptr) self->next);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, SPIF_CAST(charptr) tmp);
    }
    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self->data)) {
        spif_str_append_from_ptr(buff, SPIF_CAST(charptr) SPIF_NULLSTR_TYPE(obj));
    } else {
        buff = SPIF_OBJ_SHOW(self->data, buff, 0);
    }
    return buff;
}

static spif_cmp_t
spif_dlinked_list_item_comp(spif_dlinked_list_item_t self, spif_dlinked_list_item_t other)
{
    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    return SPIF_OBJ_COMP(SPIF_OBJ(self->data), SPIF_OBJ(other->data));
}

static spif_dlinked_list_item_t
spif_dlinked_list_item_dup(spif_dlinked_list_item_t self)
{
    spif_dlinked_list_item_t tmp;

    ASSERT_RVAL(!SPIF_DLINKED_LIST_ITEM_ISNULL(self), SPIF_NULL_TYPE(dlinked_list_item));
    tmp = spif_dlinked_list_item_new();
    tmp->data = SPIF_OBJ_DUP(self->data);
    return tmp;
}

static spif_classname_t
spif_dlinked_list_item_type(spif_dlinked_list_item_t self)
{
    ASSERT_RVAL(!SPIF_DLINKED_LIST_ITEM_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_CLASS_VAR(dlinked_list_item)->classname;
}

SPIF_DEFINE_PROPERTY_FUNC(dlinked_list_item, obj, data)
SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(dlinked_list_item, dlinked_list_item, prev)
SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(dlinked_list_item, dlinked_list_item, next)


static spif_dlinked_list_t
spif_dlinked_list_new(void)
{
    spif_dlinked_list_t self;

    self = SPIF_ALLOC(dlinked_list);
    if (!spif_dlinked_list_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(dlinked_list);
    }
    return self;
}

static spif_dlinked_list_t
spif_dlinked_list_vector_new(void)
{
    spif_dlinked_list_t self;

    self = SPIF_ALLOC(dlinked_list);
    if (!spif_dlinked_list_vector_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(dlinked_list);
    }
    return self;
}

static spif_dlinked_list_t
spif_dlinked_list_map_new(void)
{
    spif_dlinked_list_t self;

    self = SPIF_ALLOC(dlinked_list);
    if (!spif_dlinked_list_map_init(self)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(dlinked_list);
    }
    return self;
}

static spif_bool_t
spif_dlinked_list_init(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_LISTCLASS_VAR(dlinked_list)));
    self->len = 0;
    self->head = SPIF_NULL_TYPE(dlinked_list_item);
    self->tail = SPIF_NULL_TYPE(dlinked_list_item);
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_vector_init(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_VECTORCLASS_VAR(dlinked_list)));
    self->len = 0;
    self->head = SPIF_NULL_TYPE(dlinked_list_item);
    self->tail = SPIF_NULL_TYPE(dlinked_list_item);
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_map_init(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_MAP_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_MAPCLASS_VAR(dlinked_list)));
    self->len = 0;
    self->head = SPIF_NULL_TYPE(dlinked_list_item);
    self->tail = SPIF_NULL_TYPE(dlinked_list_item);
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_done(spif_dlinked_list_t self)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    if (self->len) {
        for (current = self->head; current;) {
            spif_dlinked_list_item_t tmp;

            tmp = current;
            current = current->next;
            spif_dlinked_list_item_del(tmp);
        }
        self->len = 0;
        self->head = SPIF_NULL_TYPE(dlinked_list_item);
        self->tail = SPIF_NULL_TYPE(dlinked_list_item);
    }
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_del(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    spif_dlinked_list_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

static spif_str_t
spif_dlinked_list_show(spif_dlinked_list_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];
    spif_dlinked_list_item_t current;
    spif_listidx_t i;

    if (SPIF_LIST_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(dlinked_list, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_dlinked_list_t) %s:  %10p {\n", name, SPIF_CAST(ptr) self);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self->head)) {
        spif_str_append_from_ptr(buff, SPIF_CAST(charptr) SPIF_NULLSTR_TYPE(obj));
    } else {
        for (current = self->head, i = 0; current; current = current->next, i++) {
            sprintf(SPIF_CAST_C(char *) tmp, "item %d", i);
            buff = spif_dlinked_list_item_show(current, tmp, buff, indent + 2);
        }
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent, "}\n");
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

static spif_cmp_t
spif_dlinked_list_comp(spif_dlinked_list_t self, spif_dlinked_list_t other)
{
    SPIF_OBJ_COMP_CHECK_NULL(self, other);
    return (spif_obj_comp(SPIF_OBJ(self), SPIF_OBJ(other)));
}

static spif_dlinked_list_t
spif_dlinked_list_dup(spif_dlinked_list_t self)
{
    spif_dlinked_list_t tmp;
    spif_dlinked_list_item_t src, dest, prev;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(dlinked_list));
    tmp = spif_dlinked_list_new();
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(dlinked_list));
    tmp->head = spif_dlinked_list_item_dup(self->head);
    for (src = self->head, dest = tmp->head, prev = SPIF_NULL_TYPE(dlinked_list_item);
         src->next;
         src = src->next, prev = dest, dest = dest->next) {
        dest->next = spif_dlinked_list_item_dup(src->next);
        dest->prev = prev;
    }
    dest->next = SPIF_NULL_TYPE(dlinked_list_item);
    tmp->tail = prev;
    return tmp;
}

static spif_dlinked_list_t
spif_dlinked_list_vector_dup(spif_dlinked_list_t self)
{
    spif_dlinked_list_t tmp;
    spif_dlinked_list_item_t src, dest, prev;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(dlinked_list));
    tmp = spif_dlinked_list_vector_new();
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(dlinked_list));
    tmp->head = spif_dlinked_list_item_dup(self->head);
    for (src = self->head, dest = tmp->head, prev = SPIF_NULL_TYPE(dlinked_list_item);
         src->next;
         src = src->next, prev = dest, dest = dest->next) {
        dest->next = spif_dlinked_list_item_dup(src->next);
        dest->prev = prev;
    }
    dest->next = SPIF_NULL_TYPE(dlinked_list_item);
    tmp->tail = prev;
    return tmp;
}

static spif_dlinked_list_t
spif_dlinked_list_map_dup(spif_dlinked_list_t self)
{
    spif_dlinked_list_t tmp;
    spif_dlinked_list_item_t src, dest, prev;

    ASSERT_RVAL(!SPIF_MAP_ISNULL(self), SPIF_NULL_TYPE(dlinked_list));
    tmp = spif_dlinked_list_map_new();
    memcpy(tmp, self, SPIF_SIZEOF_TYPE(dlinked_list));
    tmp->head = spif_dlinked_list_item_dup(self->head);
    for (src = self->head, dest = tmp->head, prev = SPIF_NULL_TYPE(dlinked_list_item);
         src->next;
         src = src->next, prev = dest, dest = dest->next) {
        dest->next = spif_dlinked_list_item_dup(src->next);
        dest->prev = prev;
    }
    dest->next = SPIF_NULL_TYPE(dlinked_list_item);
    tmp->tail = prev;
    return tmp;
}

static spif_classname_t
spif_dlinked_list_type(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

static spif_bool_t
spif_dlinked_list_append(spif_dlinked_list_t self, spif_obj_t obj)
{
    spif_dlinked_list_item_t item;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    /* Create list member object "item" */
    item = spif_dlinked_list_item_new();
    spif_dlinked_list_item_set_data(item, obj);

    /* Append "item" to the end of the list. */
    if (self->tail) {
        item->prev = self->tail;
        item->prev->next = item;
        self->tail = item;
    } else {
        self->tail = self->head = item;
        item->prev = SPIF_NULL_TYPE(dlinked_list_item);
    }
    item->next = SPIF_NULL_TYPE(dlinked_list_item);
    self->len++;
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_contains(spif_dlinked_list_t self, spif_obj_t obj)
{
    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    return ((SPIF_LIST_ISNULL(spif_dlinked_list_find(self, obj))) ? (FALSE) : (TRUE));
}

static spif_bool_t
spif_dlinked_list_vector_contains(spif_dlinked_list_t self, spif_obj_t obj)
{
    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), FALSE);
    return ((SPIF_LIST_ISNULL(spif_dlinked_list_vector_find(self, obj))) ? (FALSE) : (TRUE));
}

static spif_listidx_t
spif_dlinked_list_count(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    return self->len;
}

static spif_obj_t
spif_dlinked_list_find(spif_dlinked_list_t self, spif_obj_t obj)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), SPIF_NULL_TYPE(obj));
    for (current = self->head; current; current = current->next) {
        if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(obj, current->data))) {
            return current->data;
        }
    }
    return SPIF_NULL_TYPE(obj);
}

static spif_obj_t
spif_dlinked_list_vector_find(spif_dlinked_list_t self, spif_obj_t obj)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(obj), SPIF_NULL_TYPE(obj));
    for (current = self->head; current; current = current->next) {
        spif_cmp_t c;

        c = SPIF_OBJ_COMP(obj, current->data);
        if (SPIF_CMP_IS_EQUAL(c)) {
            return current->data;
        } else if (SPIF_CMP_IS_LESS(c)) {
            break;
        }
    }
    return SPIF_NULL_TYPE(obj);
}

static spif_obj_t
spif_dlinked_list_get(spif_dlinked_list_t self, spif_listidx_t idx)
{
    spif_listidx_t i;
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(obj));
    if (idx < 0) {
        /* Negative indexes go backward from the end of the list. */
        idx += self->len;
    }
    REQUIRE_RVAL(idx >= 0, SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(idx < self->len, SPIF_NULL_TYPE(obj));

    if (idx > (self->len / 2)) {
        for (current = self->tail, i = self->len - 1; current && i > idx; i--, current = current->prev);
        return (current ? (current->data) : SPIF_NULL_TYPE(obj));
    } else {
        for (current = self->head, i = 0; current && i < idx; i++, current = current->next);
        return (current ? (current->data) : SPIF_NULL_TYPE(obj));
    }
}

static spif_obj_t
spif_dlinked_list_map_get(spif_dlinked_list_t self, spif_obj_t key)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(key), SPIF_NULL_TYPE(obj));
    for (current = self->head; current; current = current->next) {
        spif_cmp_t c;

        /* current->data is always non-NULL in maps. */
        ASSERT_RVAL(!SPIF_OBJ_ISNULL(current->data), SPIF_NULL_TYPE(obj));
        c = SPIF_OBJ_COMP(current->data, key);
        if (SPIF_CMP_IS_EQUAL(c)) {
            return SPIF_OBJPAIR(current->data)->value;
        } else if (SPIF_CMP_IS_GREATER(c)) {
            break;
        }
    }
    return SPIF_NULL_TYPE(obj);
}

static spif_list_t
spif_dlinked_list_get_keys(spif_dlinked_list_t self, spif_list_t key_list)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(list));
    if (SPIF_LIST_ISNULL(key_list)) {
        key_list = SPIF_LIST_NEW(linked_list);
    }
    for (current = self->head; current; current = current->next) {
        /* current->data is always non-NULL in maps. */
        SPIF_LIST_APPEND(key_list, SPIF_OBJ_DUP(SPIF_OBJPAIR(current->data)->key));
    }
    return key_list;
}

static spif_list_t
spif_dlinked_list_get_pairs(spif_dlinked_list_t self, spif_list_t pair_list)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(list));
    if (SPIF_LIST_ISNULL(pair_list)) {
        pair_list = SPIF_LIST_NEW(linked_list);
    }
    for (current = self->head; current; current = current->next) {
        /* current->data is always non-NULL in maps. */
        SPIF_LIST_APPEND(pair_list, SPIF_OBJ_DUP(SPIF_OBJPAIR(current->data)));
    }
    return pair_list;
}

static spif_list_t
spif_dlinked_list_get_values(spif_dlinked_list_t self, spif_list_t value_list)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), SPIF_NULL_TYPE(list));
    if (SPIF_LIST_ISNULL(value_list)) {
        value_list = SPIF_LIST_NEW(linked_list);
    }
    for (current = self->head; current; current = current->next) {
        /* current->data is always non-NULL in maps. */
        SPIF_LIST_APPEND(value_list, SPIF_OBJ_DUP(SPIF_OBJPAIR(current->data)->value));
    }
    return value_list;
}

static spif_bool_t
spif_dlinked_list_has_key(spif_dlinked_list_t self, spif_obj_t key)
{
    return ((SPIF_OBJ_ISNULL(spif_dlinked_list_map_get(self, key))) ? FALSE : TRUE);
}

static spif_bool_t
spif_dlinked_list_has_value(spif_dlinked_list_t self, spif_obj_t value)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_VECTOR_ISNULL(self), FALSE);

    for (current = self->head; current; current = current->next) {
        spif_objpair_t pair;

        /* current->data is always non-NULL in maps. */
        pair = SPIF_OBJPAIR(current->data);
        if (SPIF_OBJ_ISNULL(value) && SPIF_OBJ_ISNULL(pair->value)) {
            return TRUE;
        } else if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(pair->value, value))) {
            return TRUE;
        }
    }
    return FALSE;
}

static spif_listidx_t
spif_dlinked_list_index(spif_dlinked_list_t self, spif_obj_t obj)
{
    spif_listidx_t i;
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_CAST(listidx) -1);
    for (current = self->head, i = 0; current && !SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(obj, current->data)); i++, current = current->next);
    return (current ? i : ((spif_listidx_t) (-1)));
}

static spif_bool_t
spif_dlinked_list_insert(spif_dlinked_list_t self, spif_obj_t obj)
{
    spif_dlinked_list_item_t item, current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    item = spif_dlinked_list_item_new();
    spif_dlinked_list_item_set_data(item, obj);

    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self->head)) {
        self->head = self->tail = item;
    } else if (SPIF_CMP_IS_LESS(spif_dlinked_list_item_comp(item, self->head))) {
        item->next = self->head;
        self->head->prev = item;
        self->head = item;
    } else if (SPIF_CMP_IS_GREATER(spif_dlinked_list_item_comp(item, self->tail))) {
        item->prev = self->tail;
        self->tail->next = item;
        self->tail = item;
    } else {
        for (current = self->head;
             current->next && SPIF_CMP_IS_GREATER(spif_dlinked_list_item_comp(item, current->next));
             current = current->next);
        item->next = current->next;
        item->prev = current;
        current->next->prev = item;
        current->next = item;
    }
    self->len++;
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_insert_at(spif_dlinked_list_t self, spif_obj_t obj, spif_listidx_t idx)
{
    spif_listidx_t i;
    spif_dlinked_list_item_t item, current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    if (idx < 0) {
        /* Negative indexes go backward from the end of the list. */
        idx += self->len;
    }
    REQUIRE_RVAL((idx + 1) > 0, FALSE);

    if (idx == 0 || SPIF_DLINKED_LIST_ITEM_ISNULL(self->head)) {
        return spif_dlinked_list_prepend(self, obj);
    } else if (idx == (self->len - 1) || SPIF_DLINKED_LIST_ITEM_ISNULL(self->tail)) {
        return spif_dlinked_list_append(self, obj);
    } else if (idx > self->len) {
        for (i = self->len; i < idx; i++) {
            spif_dlinked_list_append(self, SPIF_NULL_TYPE(obj));
        }
        return spif_dlinked_list_append(self, obj);
    } else if (idx > (self->len / 2)) {
        for (current = self->tail, i = self->len - 1; current->prev && i > idx; i--, current = current->prev);
        if (i != idx) {
            return FALSE;
        }
    } else {
        for (current = self->head, i = 1; current->next && i < idx; i++, current = current->next);
        if (i != idx) {
            return FALSE;
        }
    }
    item = spif_dlinked_list_item_new();
    spif_dlinked_list_item_set_data(item, obj);

    item->next = current->next;
    item->prev = current;
    current->next->prev = item;
    current->next = item;
    self->len++;
    return TRUE;
}

static spif_iterator_t
spif_dlinked_list_iterator(spif_dlinked_list_t self)
{
    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(iterator));
    return SPIF_CAST(iterator) spif_dlinked_list_iterator_new(self);
}

static spif_bool_t
spif_dlinked_list_prepend(spif_dlinked_list_t self, spif_obj_t obj)
{
    spif_dlinked_list_item_t item, current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    /* Create list member object "item" */
    item = spif_dlinked_list_item_new();
    spif_dlinked_list_item_set_data(item, obj);

    /* Set "item" to be at the front of the list. */
    if (self->head) {
        current = self->head;
        self->head = item;
        current->prev = item;
        item->next = current;
    } else {
        self->head = self->tail = item;
    }
    self->len++;
    return TRUE;
}

static spif_obj_t
spif_dlinked_list_remove(spif_dlinked_list_t self, spif_obj_t item)
{
    spif_dlinked_list_item_t current, tmp;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(obj));
    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self->head)) {
        return SPIF_NULL_TYPE(obj);
    }

    for (current = self->head; current && !SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(item, current->data)); current = current->next);
    if (SPIF_DLINKED_LIST_ITEM_ISNULL(current)) {
        return SPIF_NULL_TYPE(obj);
    }
    tmp = current;
    if (!SPIF_DLINKED_LIST_ITEM_ISNULL(tmp->prev)) {
        tmp->prev->next = tmp->next;
    }
    if (!SPIF_DLINKED_LIST_ITEM_ISNULL(tmp->next)) {
        tmp->next->prev = tmp->prev;
    }
    if (tmp == self->head) {
        self->head = tmp->next;
    }
    if (tmp == self->tail) {
        self->tail = tmp->prev;
    }
    item = tmp->data;
    tmp->data = SPIF_NULL_TYPE(obj);
    spif_dlinked_list_item_del(tmp);

    self->len--;
    return item;
}

static spif_obj_t
spif_dlinked_list_map_remove(spif_dlinked_list_t self, spif_obj_t item)
{
    spif_dlinked_list_item_t current, tmp;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(item), SPIF_NULL_TYPE(obj));
    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self->head)) {
        return SPIF_NULL_TYPE(obj);
    } else if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(self->head->data, item))) {
        tmp = self->head;
        self->head = self->head->next;
    } else {
        for (current = self->head; current->next && !SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(current->next->data, item)); current = current->next);
        if (current->next) {
            tmp = current->next;
            current->next = current->next->next;
        } else {
            return SPIF_NULL_TYPE(obj);
        }
    }
    item = tmp->data;
    tmp->data = SPIF_NULL_TYPE(obj);
    spif_dlinked_list_item_del(tmp);

    self->len--;
    return item;
}

static spif_obj_t
spif_dlinked_list_remove_at(spif_dlinked_list_t self, spif_listidx_t idx)
{
    spif_listidx_t i;
    spif_dlinked_list_item_t current;
    spif_obj_t tmp;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE(obj));
    if (SPIF_DLINKED_LIST_ITEM_ISNULL(self->head)) {
        return SPIF_NULL_TYPE(obj);
    }

    if (idx < 0) {
        /* Negative indexes go backward from the end of the list. */
        idx += self->len;
    }
    REQUIRE_RVAL(idx >= 0, SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(idx < self->len, SPIF_NULL_TYPE(obj));

    if (idx > (self->len / 2)) {
        for (current = self->tail, i = self->len - 1; current && i > idx; i--, current = current->prev);
    } else {
        for (current = self->head, i = 0; current && i < idx; i++, current = current->next);
    }

    if (SPIF_DLINKED_LIST_ITEM_ISNULL(current)) {
        return SPIF_NULL_TYPE(obj);
    }
    if (!SPIF_DLINKED_LIST_ITEM_ISNULL(current->prev)) {
        current->prev->next = current->next;
    }
    if (!SPIF_DLINKED_LIST_ITEM_ISNULL(current->next)) {
        current->next->prev = current->prev;
    }
    if (current == self->head) {
        self->head = current->next;
    }
    if (current == self->tail) {
        self->tail = current->prev;
    }

    tmp = spif_dlinked_list_item_get_data(current);
    current->data = SPIF_NULL_TYPE(obj);
    spif_dlinked_list_item_del(current);

    self->len--;
    return tmp;
}

static spif_bool_t
spif_dlinked_list_reverse(spif_dlinked_list_t self)
{
    spif_dlinked_list_item_t current, tmp;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    for (current = self->head; current; ) {
        tmp = current;
        current = current->next;
        SWAP(tmp->prev, tmp->next);
    }
    self->head = tmp;
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_set(spif_dlinked_list_t self, spif_obj_t key, spif_obj_t value)
{
    spif_dlinked_list_item_t current;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), FALSE);
    REQUIRE_RVAL(!SPIF_OBJ_ISNULL(key), FALSE);

    if (SPIF_OBJ_IS_OBJPAIR(key) && SPIF_OBJ_ISNULL(value)) {
        value = SPIF_OBJ(SPIF_OBJPAIR(key)->value);
        key = SPIF_OBJ(SPIF_OBJPAIR(key)->key);
    }
    for (current = self->head; current; current = current->next) {
        if (SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(current->data, key))) {
            break;
        }
    }

    if (SPIF_DLINKED_LIST_ITEM_ISNULL(current)) {
        spif_dlinked_list_insert(self, SPIF_OBJ(spif_objpair_new_from_both(key, value)));
        return FALSE;
    } else {
        spif_objpair_set_value(SPIF_OBJPAIR(current->data), SPIF_OBJ_DUP(value));
        return TRUE;
    }
}

static spif_obj_t *
spif_dlinked_list_to_array(spif_dlinked_list_t self)
{
    spif_obj_t *tmp;
    spif_dlinked_list_item_t current;
    spif_listidx_t i;

    ASSERT_RVAL(!SPIF_LIST_ISNULL(self), SPIF_NULL_TYPE_PTR(obj));
    tmp = SPIF_CAST_C(spif_obj_t *) MALLOC(SPIF_SIZEOF_TYPE(obj) * self->len);
    for (i = 0, current = self->head; i < self->len; current = current->next, i++) {
        tmp[i] = SPIF_CAST(obj) SPIF_OBJ(spif_dlinked_list_item_get_data(current));
    }
    return tmp;
}

SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(dlinked_list, listidx, len)
SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(dlinked_list, dlinked_list_item, head)
SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(dlinked_list, dlinked_list_item, tail)


static spif_dlinked_list_iterator_t
spif_dlinked_list_iterator_new(spif_dlinked_list_t subject)
{
    spif_dlinked_list_iterator_t self;

    self = SPIF_ALLOC(dlinked_list_iterator);
    if (!spif_dlinked_list_iterator_init(self, subject)) {
        SPIF_DEALLOC(self);
        self = SPIF_NULL_TYPE(dlinked_list_iterator);
    }
    return self;
}

static spif_bool_t
spif_dlinked_list_iterator_init(spif_dlinked_list_iterator_t self, spif_dlinked_list_t subject)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    /* ***NOT NEEDED*** spif_obj_init(SPIF_OBJ(self)); */
    spif_obj_set_class(SPIF_OBJ(self), SPIF_CLASS(SPIF_ITERATORCLASS_VAR(dlinked_list)));
    self->subject = subject;
    if (SPIF_LIST_ISNULL(self->subject)) {
        self->current = SPIF_NULL_TYPE(dlinked_list_item);
    } else {
        self->current = self->subject->head;
    }
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_iterator_done(spif_dlinked_list_iterator_t self)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    self->subject = SPIF_NULL_TYPE(dlinked_list);
    self->current = SPIF_NULL_TYPE(dlinked_list_item);
    return TRUE;
}

static spif_bool_t
spif_dlinked_list_iterator_del(spif_dlinked_list_iterator_t self)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    spif_dlinked_list_iterator_done(self);
    SPIF_DEALLOC(self);
    return TRUE;
}

static spif_str_t
spif_dlinked_list_iterator_show(spif_dlinked_list_iterator_t self, spif_charptr_t name, spif_str_t buff, size_t indent)
{
    spif_char_t tmp[4096];

    if (SPIF_ITERATOR_ISNULL(self)) {
        SPIF_OBJ_SHOW_NULL(iterator, name, buff, indent, tmp);
        return buff;
    }

    memset(tmp, ' ', indent);
    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent,
             "(spif_dlinked_list_iterator_t) %s:  %10p {\n", name,
             SPIF_CAST(ptr) self);
    if (SPIF_STR_ISNULL(buff)) {
        buff = spif_str_new_from_ptr(tmp);
    } else {
        spif_str_append_from_ptr(buff, tmp);
    }

    buff = spif_dlinked_list_show(self->subject, SPIF_CAST(charptr) "subject",
                                  buff, indent + 2);
    buff = spif_dlinked_list_item_show(self->current, SPIF_CAST(charptr) "current",
                                       buff, indent + 2);

    snprintf(SPIF_CAST_C(char *) tmp + indent, sizeof(tmp) - indent, "}\n");
    spif_str_append_from_ptr(buff, tmp);
    return buff;
}

static spif_cmp_t
spif_dlinked_list_iterator_comp(spif_dlinked_list_iterator_t self, spif_dlinked_list_iterator_t other)
{
    return spif_dlinked_list_comp(self->subject, other->subject);
}

static spif_dlinked_list_iterator_t
spif_dlinked_list_iterator_dup(spif_dlinked_list_iterator_t self)
{
    spif_dlinked_list_iterator_t tmp;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), SPIF_NULL_TYPE(dlinked_list_iterator));
    tmp = spif_dlinked_list_iterator_new(self->subject);
    tmp->current = self->current;
    return tmp;
}

static spif_classname_t
spif_dlinked_list_iterator_type(spif_dlinked_list_iterator_t self)
{
    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), SPIF_NULL_TYPE(classname));
    return SPIF_OBJ_CLASSNAME(self);
}

static spif_bool_t
spif_dlinked_list_iterator_has_next(spif_dlinked_list_iterator_t self)
{
    spif_dlinked_list_t subject;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), FALSE);
    subject = self->subject;
    REQUIRE_RVAL(!SPIF_LIST_ISNULL(subject), FALSE);
    if (self->current) {
        return TRUE;
    } else {
        return FALSE;
    }
}

static spif_obj_t
spif_dlinked_list_iterator_next(spif_dlinked_list_iterator_t self)
{
    spif_obj_t tmp;

    ASSERT_RVAL(!SPIF_ITERATOR_ISNULL(self), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_LIST_ISNULL(self->subject), SPIF_NULL_TYPE(obj));
    REQUIRE_RVAL(!SPIF_DLINKED_LIST_ITEM_ISNULL(self->current), SPIF_NULL_TYPE(obj));
    tmp = self->current->data;
    self->current = self->current->next;
    return tmp;
}

SPIF_DEFINE_PROPERTY_FUNC(dlinked_list_iterator, dlinked_list, subject)
SPIF_DEFINE_PROPERTY_FUNC_NONOBJ(dlinked_list_iterator, dlinked_list_item, current)
