/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include "gconf-changeset.h"
#include "gconf-internals.h"

typedef enum {
  CHANGE_INVALID,
  CHANGE_SET,
  CHANGE_UNSET
} ChangeType;

typedef struct _Change Change;

struct _Change {
  gchar* key;
  ChangeType type;
  GConfValue* value;
};

static Change* change_new    (const gchar* key);
static void    change_set    (Change* c, GConfValue* value);
static void    change_unset  (Change* c);
static void    change_destroy(Change* c);

struct _GConfChangeSet {
  guint refcount;
  GHashTable* hash;
  gint in_foreach;
  gpointer user_data;
  GDestroyNotify dnotify;
};

GType
gconf_change_set_get_type (void)
{
  static GType our_type = 0;

  if (our_type == 0)
    our_type = g_boxed_type_register_static ("GConfChangeSet",
					     (GBoxedCopyFunc) gconf_change_set_ref,
					     (GBoxedFreeFunc) gconf_change_set_unref);

  return our_type;
}

GConfChangeSet*
gconf_change_set_new      (void)
{
  GConfChangeSet* cs;

  cs = g_new(GConfChangeSet, 1);

  cs->refcount = 1;
  cs->hash = g_hash_table_new(g_str_hash, g_str_equal);
  cs->in_foreach = 0;
  cs->user_data = NULL;
  cs->dnotify = NULL;
  
  return cs;
}

GConfChangeSet*
gconf_change_set_ref      (GConfChangeSet* cs)
{
  g_return_val_if_fail(cs != NULL, NULL);
  
  cs->refcount += 1;

  return cs;
}

void
gconf_change_set_unref    (GConfChangeSet* cs)
{
  g_return_if_fail(cs != NULL);
  g_return_if_fail(cs->refcount > 0);

  cs->refcount -= 1;

  if (cs->refcount == 0)
    {
      if (cs->in_foreach > 0)
        g_warning("GConfChangeSet refcount reduced to 0 during a foreach");
      
      gconf_change_set_clear(cs);

      g_hash_table_destroy(cs->hash);
      
      g_free(cs);
    }
}

/**
 * gconf_change_set_set_user_data: (skip)
 * @cs: a #GConfChangeSet.
 * @data: a #gpointer.
 * @dnotify: a pointer to the function to be called during destroy.
 *
 * Sets the user_data and the destroy notification function fields of the
 * #GConfChangeSet.
 */
void
gconf_change_set_set_user_data (GConfChangeSet *cs,
                                gpointer        data,
                                GDestroyNotify  dnotify)
{
  if (cs->dnotify)
    (* cs->dnotify) (cs->user_data);

  cs->user_data = data;
  cs->dnotify = dnotify;
}

/**
 * gconf_change_set_get_user_data: (skip)
 * @cs: a #GConfChangeSet.
 *
 * Returns the user_data field of the #GConfChangeSet.
 *
 * Return value: a pointer to the user_data.
 */
gpointer
gconf_change_set_get_user_data (GConfChangeSet *cs)
{
  return cs->user_data;
}

static Change*
get_change_unconditional (GConfChangeSet* cs,
                          const gchar* key)
{
  Change* c;

  c = g_hash_table_lookup(cs->hash, key);

  if (c == NULL)
    {
      c = change_new(key);

      g_hash_table_insert(cs->hash, c->key, c);
    }

  return c;
}

static gboolean
destroy_foreach (gpointer key, gpointer value, gpointer user_data)
{
  Change* c = value;

  g_assert(c != NULL);

  change_destroy(c);

  return TRUE; /* remove from hash */
}

void
gconf_change_set_clear    (GConfChangeSet* cs)
{
  g_return_if_fail(cs != NULL);

  g_hash_table_foreach_remove (cs->hash, destroy_foreach, NULL);
}

guint
gconf_change_set_size     (GConfChangeSet* cs)
{
  g_return_val_if_fail(cs != NULL, 0);
  
  return g_hash_table_size(cs->hash);
}

void
gconf_change_set_remove   (GConfChangeSet* cs,
                           const gchar* key)
{
  Change* c;
  
  g_return_if_fail(cs != NULL);
  g_return_if_fail(cs->in_foreach == 0);
  
  c = g_hash_table_lookup(cs->hash, key);

  if (c != NULL)
    {
      g_hash_table_remove(cs->hash, c->key);
      change_destroy(c);
    }
}


struct ForeachData {
  GConfChangeSet* cs;
  GConfChangeSetForeachFunc func;
  gpointer user_data;
};

static void
foreach(gpointer key, gpointer value, gpointer user_data)
{
  Change* c;
  struct ForeachData* fd = user_data;
  
  c = value;

  /* assumes that an UNSET change has a NULL value */
  (* fd->func) (fd->cs, c->key, c->value, fd->user_data);
}

/**
 * gconf_change_set_foreach:
 * @cs: a #GConfChangeSet.
 * @func: (scope call): function to call for each change in the change set.
 * @user_data: user data to pass to the #GConfChangeSetForeachFunc.
 *
 * Iterates over a #GConfChangeSet by calling a
 * #GConfChangeSetForeachFunc for each change in the set. See the
 * description of #GConfChangeSetForeachFunc for details.  You may not
 * call gconf_change_set_remove() during the iteration, because you'll
 * confuse the internal data structures and cause memory corruption.
 */
void
gconf_change_set_foreach  (GConfChangeSet* cs,
                           GConfChangeSetForeachFunc func,
                           gpointer user_data)
{
  struct ForeachData fd;
  
  g_return_if_fail(cs != NULL);
  g_return_if_fail(func != NULL);
  
  fd.cs = cs;
  fd.func = func;
  fd.user_data = user_data;

  gconf_change_set_ref(cs);

  cs->in_foreach += 1;
  
  g_hash_table_foreach(cs->hash, foreach, &fd);

  cs->in_foreach -= 1;
  
  gconf_change_set_unref(cs);
}

gboolean
gconf_change_set_check_value   (GConfChangeSet* cs, const gchar* key,
                                GConfValue** value_retloc)
{
  Change* c;
  
  g_return_val_if_fail(cs != NULL, FALSE);

  c = g_hash_table_lookup(cs->hash, key);

  if (c == NULL)
    return FALSE;
  else
    {
      if (value_retloc != NULL)
        *value_retloc = c->value;

      return TRUE;
    }
}

void
gconf_change_set_set_nocopy  (GConfChangeSet* cs, const gchar* key,
                              GConfValue* value)
{
  Change* c;
  
  g_return_if_fail(cs != NULL);
  g_return_if_fail(value != NULL);

  c = get_change_unconditional(cs, key);

  change_set(c, value);
}

void
gconf_change_set_set (GConfChangeSet* cs, const gchar* key,
                      GConfValue* value)
{
  g_return_if_fail(value != NULL);
  
  gconf_change_set_set_nocopy(cs, key, gconf_value_copy(value));
}

void
gconf_change_set_unset      (GConfChangeSet* cs, const gchar* key)
{
  Change* c;
  
  g_return_if_fail(cs != NULL);

  c = get_change_unconditional(cs, key);

  change_unset(c);
}

void
gconf_change_set_set_float   (GConfChangeSet* cs, const gchar* key,
                              gdouble val)
{
  GConfValue* value;
  
  g_return_if_fail(cs != NULL);

  value = gconf_value_new(GCONF_VALUE_FLOAT);
  gconf_value_set_float(value, val);
  
  gconf_change_set_set_nocopy(cs, key, value);
}

void
gconf_change_set_set_int     (GConfChangeSet* cs, const gchar* key,
                              gint val)
{
  GConfValue* value;
  
  g_return_if_fail(cs != NULL);

  value = gconf_value_new(GCONF_VALUE_INT);
  gconf_value_set_int(value, val);
  
  gconf_change_set_set_nocopy(cs, key, value);
}

void
gconf_change_set_set_string  (GConfChangeSet* cs, const gchar* key,
                              const gchar* val)
{
  GConfValue* value;
  
  g_return_if_fail(cs != NULL);
  g_return_if_fail(key != NULL);
  g_return_if_fail(val != NULL);
  
  value = gconf_value_new(GCONF_VALUE_STRING);
  gconf_value_set_string(value, val);
  
  gconf_change_set_set_nocopy(cs, key, value);
}

void
gconf_change_set_set_bool    (GConfChangeSet* cs, const gchar* key,
                              gboolean val)
{
  GConfValue* value;
  
  g_return_if_fail(cs != NULL);

  value = gconf_value_new(GCONF_VALUE_BOOL);
  gconf_value_set_bool(value, val);
  
  gconf_change_set_set_nocopy(cs, key, value);
}

void
gconf_change_set_set_schema  (GConfChangeSet* cs, const gchar* key,
                              GConfSchema* val)
{
  GConfValue* value;
  
  g_return_if_fail(cs != NULL);

  value = gconf_value_new(GCONF_VALUE_SCHEMA);
  gconf_value_set_schema(value, val);
  
  gconf_change_set_set_nocopy(cs, key, value);
}

void
gconf_change_set_set_list    (GConfChangeSet* cs, const gchar* key,
                              GConfValueType list_type,
                              GSList* list)
{
  GConfValue* value_list;
  
  g_return_if_fail(cs != NULL);
  g_return_if_fail(key != NULL);
  g_return_if_fail(list_type != GCONF_VALUE_INVALID);
  g_return_if_fail(list_type != GCONF_VALUE_LIST);
  g_return_if_fail(list_type != GCONF_VALUE_PAIR);
  
  value_list = gconf_value_list_from_primitive_list (list_type, list, NULL);
  
  gconf_change_set_set_nocopy(cs, key, value_list);
}


void
gconf_change_set_set_pair    (GConfChangeSet* cs, const gchar* key,
                              GConfValueType car_type, GConfValueType cdr_type,
                              gconstpointer address_of_car,
                              gconstpointer address_of_cdr)
{
  GConfValue* pair;
  
  g_return_if_fail(cs != NULL);
  g_return_if_fail(key != NULL);
  g_return_if_fail(car_type != GCONF_VALUE_INVALID);
  g_return_if_fail(car_type != GCONF_VALUE_LIST);
  g_return_if_fail(car_type != GCONF_VALUE_PAIR);
  g_return_if_fail(cdr_type != GCONF_VALUE_INVALID);
  g_return_if_fail(cdr_type != GCONF_VALUE_LIST);
  g_return_if_fail(cdr_type != GCONF_VALUE_PAIR);
  g_return_if_fail(address_of_car != NULL);
  g_return_if_fail(address_of_cdr != NULL);

  pair = gconf_value_pair_from_primitive_pair (car_type, cdr_type,
                                               address_of_car, address_of_cdr,
                                               NULL);
  
  gconf_change_set_set_nocopy(cs, key, pair);
}


/*
 * Change
 */

static Change*
change_new    (const gchar* key)
{
  Change* c;

  c = g_new(Change, 1);

  c->key  = g_strdup(key);
  c->type = CHANGE_INVALID;
  c->value = NULL;

  return c;
}

static void
change_destroy(Change* c)
{
  g_return_if_fail(c != NULL);
  
  g_free(c->key);

  if (c->value)
    gconf_value_free(c->value);

  g_free(c);
}

static void
change_set    (Change* c, GConfValue* value)
{
  g_return_if_fail(value == NULL ||
                   GCONF_VALUE_TYPE_VALID(value->type));
  
  c->type = CHANGE_SET;

  if (value == c->value)
    return;
  
  if (c->value)
    gconf_value_free(c->value);

  c->value = value;
}

static void
change_unset  (Change* c)
{
  c->type = CHANGE_UNSET;

  if (c->value)
    gconf_value_free(c->value);

  c->value = NULL;
}

/*
 * Actually send it upstream
 */

struct CommitData {
  GConfEngine* conf;
  GError* error;
  GSList* remove_list;
  gboolean remove_committed;
};

static void
commit_foreach (GConfChangeSet* cs,
                const gchar* key,
                GConfValue* value,
                gpointer user_data)
{
  struct CommitData* cd = user_data;

  g_assert(cd != NULL);

  if (cd->error != NULL)
    return;
  
  if (value)
    gconf_engine_set   (cd->conf, key, value, &cd->error);
  else
    gconf_engine_unset (cd->conf, key, &cd->error);

  if (cd->error == NULL && cd->remove_committed)
    {
      /* Bad bad bad; we keep the key reference, knowing that it's
         valid until we modify the change set, to avoid string copies.  */
      cd->remove_list = g_slist_prepend(cd->remove_list, (gchar*)key);
    }
}

gboolean
gconf_engine_commit_change_set   (GConfEngine* conf,
                           GConfChangeSet* cs,
                           gboolean remove_committed,
                           GError** err)
{
  struct CommitData cd;
  GSList* tmp;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(cs != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  cd.conf = conf;
  cd.error = NULL;
  cd.remove_list = NULL;
  cd.remove_committed = remove_committed;

  /* Because the commit could have lots of side
     effects, this makes it safer */
  gconf_change_set_ref(cs);
  gconf_engine_ref(conf);
  
  gconf_change_set_foreach(cs, commit_foreach, &cd);

  tmp = cd.remove_list;
  while (tmp != NULL)
    {
      const gchar* key = tmp->data;
      
      gconf_change_set_remove(cs, key);

      /* key is now invalid due to our little evil trick */

      tmp = g_slist_next(tmp);
    }

  g_slist_free(cd.remove_list);
  
  gconf_change_set_unref(cs);
  gconf_engine_unref(conf);

  if (cd.error != NULL)
    {
      if (err != NULL)
        *err = cd.error;
      else
        g_error_free(cd.error);

      return FALSE;
    }
  else
    {
      return TRUE;
    }
}

struct RevertData {
  GConfEngine* conf;
  GError* error;
  GConfChangeSet* revert_set;
};

static void
revert_foreach (GConfChangeSet* cs,
                const gchar* key,
                GConfValue* value,
                gpointer user_data)
{
  struct RevertData* rd = user_data;
  GConfValue* old_value;
  GError* error = NULL;
  
  g_assert(rd != NULL);

  if (rd->error != NULL)
    return;

  old_value = gconf_engine_get_without_default(rd->conf, key, &error);

  if (error != NULL)
    {
      /* FIXME */
      g_warning("error creating revert set: %s", error->message);
      g_error_free(error);
      error = NULL;
    }
  
  if (old_value == NULL &&
      value == NULL)
    return; /* this commit will have no effect. */

  if (old_value == NULL)
    gconf_change_set_unset(rd->revert_set, key);
  else
    gconf_change_set_set_nocopy(rd->revert_set, key, old_value);
}


GConfChangeSet*
gconf_engine_reverse_change_set  (GConfEngine* conf,
                                  GConfChangeSet* cs,
                                  GError** err)
{
  struct RevertData rd;

  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  rd.error = NULL;
  rd.conf = conf;
  rd.revert_set = gconf_change_set_new();

  gconf_change_set_foreach(cs, revert_foreach, &rd);

  if (rd.error != NULL)
    {
      if (err != NULL)
        *err = rd.error;
      else
        g_error_free(rd.error);
    }
  
  return rd.revert_set;
}

GConfChangeSet*
gconf_engine_change_set_from_currentv (GConfEngine* conf,
                                       const gchar** keys,
                                       GError** err)
{
  GConfValue* old_value;
  GConfChangeSet* new_set;
  const gchar** keyp;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  new_set = gconf_change_set_new();
  
  keyp = keys;

  while (*keyp != NULL)
    {
      GError* error = NULL;
      const gchar* key = *keyp;
      
      old_value = gconf_engine_get_without_default(conf, key, &error);

      if (error != NULL)
        {
          /* FIXME */
          g_warning("error creating change set from current keys: %s", error->message);
          g_error_free(error);
          error = NULL;
        }
      
      if (old_value == NULL)
        gconf_change_set_unset(new_set, key);
      else
        gconf_change_set_set_nocopy(new_set, key, old_value);

      ++keyp;
    }

  return new_set;
}

GConfChangeSet*
gconf_engine_change_set_from_current (GConfEngine* conf,
                                      GError** err,
                                      const gchar* first_key,
                                      ...)
{
  GSList* keys = NULL;
  va_list args;
  const gchar* arg;
  const gchar** vec;
  GConfChangeSet* retval;
  GSList* tmp;
  guint i;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  va_start (args, first_key);

  arg = first_key;

  while (arg != NULL)
    {
      keys = g_slist_prepend(keys, (/*not-const*/gchar*)arg);

      arg = va_arg (args, const gchar*);
    }
  
  va_end (args);

  vec = g_new0(const gchar*, g_slist_length(keys) + 1);

  i = 0;
  tmp = keys;

  while (tmp != NULL)
    {
      vec[i] = tmp->data;
      
      ++i;
      tmp = g_slist_next(tmp);
    }

  g_slist_free(keys);
  
  retval = gconf_engine_change_set_from_currentv(conf, vec, err);
  
  g_free(vec);

  return retval;
}
