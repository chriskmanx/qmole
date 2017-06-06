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

#include <gconf/gconf-listeners.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

static void
check(gboolean condition, const gchar* fmt, ...)
{
  va_list args;
  gchar* description;
  
  va_start (args, fmt);
  description = g_strdup_vprintf(fmt, args);
  va_end (args);
  
  if (condition)
    {
      printf(".");
      fflush(stdout);
    }
  else
    {
      fprintf(stderr, "\n*** FAILED: %s\n", description);
      exit(1);
    }
  
  g_free(description);
}

static const gchar*
keys[] = {
  "/testing/foo/tar",
  "/testing/foo/bar",
  "/testing/quad",
  "/testing/blah",
  "/testing/q/a/b/c/z/w/x/y/z",
  "/testing/foo/baz",
  "/testing/dup",
  "/testing/oops/bloo",
  "/testing/oops/snoo",
  "/testing/dup",
  "/testing/oops/kwoo",
  "/testing/foo/quaz",
  "/testing",
  "/testing/oops",
  "/testing/dup",
  "/",
  "/blah/blah/blah",
  "/blah/blah/foo",
  "/blah/blah/bar",
  "/boo",
  "/baz",
  "/bap",
  "/duplicate",
  "/duplicate",
  NULL
};

struct stuff {
  guint id;
  gchar* key;
  guint* id_retloc;
  gchar** key_retloc;
};

static void
test_destroy_notify(gpointer data)
{
  struct stuff* s = data;

  *s->id_retloc = s->id;

  *s->key_retloc = s->key;

  g_free(s);
}

static void
check_add_remove(GConfListeners* listeners)
{
  guint ids[128];
  guint i;
  const gchar** keyp;
  guint id_retloc;
  gchar* key_retloc;
  
  memset(ids, 0, sizeof(ids[0]) * 128);
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      struct stuff* s;

      s = g_new0(struct stuff, 1);

      s->id_retloc = &id_retloc;
      s->key_retloc = &key_retloc;
      
      s->id = ids[i] = gconf_listeners_add(listeners,
                                           *keyp,
                                           s,
                                           test_destroy_notify);

      s->key = g_strdup(*keyp);

      check(ids[i] != 0, "invalid connection ID returned for added listener");
      
      ++i;
      ++keyp;
    }

  check(gconf_listeners_count(listeners) == i,
        "number of listeners added (%u) don't now exist in the GConfListeners (%u exist)", i, gconf_listeners_count(listeners));
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      id_retloc = 0;
      key_retloc = NULL;
      
      gconf_listeners_remove(listeners, ids[i]);

      check(strcmp(key_retloc, *keyp) == 0,
            "listener removed has different key from listener added (`%s' vs. `%s')", *keyp, key_retloc);

      g_free(key_retloc);

      check(ids[i] == id_retloc, "listener removed had different id from that added (%u vs. %u)", ids[i], id_retloc);            
      
      ++i;
      ++keyp;
    }

  check(gconf_listeners_count(listeners) == 0,
        "listener count isn't 0 after removing all the listeners");
}

static void
check_immediate_remove_after_add(GConfListeners* listeners)
{
  guint ids[128];
  guint i;
  const gchar** keyp;
  guint id_retloc;
  gchar* key_retloc;
  
  memset(ids, 0, sizeof(ids[0]) * 128);
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      struct stuff* s;

      s = g_new0(struct stuff, 1);

      s->id_retloc = &id_retloc;
      s->key_retloc = &key_retloc;
      
      s->id = ids[i] = gconf_listeners_add(listeners,
                                           *keyp,
                                           s,
                                           test_destroy_notify);

      s->key = g_strdup(*keyp);

      check(ids[i] != 0, "invalid connection ID returned for added listener");

      if (i > 0)
        {
          check((ids[i] & 0xFFFFFF) == (ids[i-1] & 0xFFFFFF), "connection ID was not properly recycled");
          check(ids[i] != ids[i-1], "connection ID was not properly uniqueized");
        }
          
      check(gconf_listeners_count(listeners) == 1,
            "didn't have 1 listener as expected");
      
      id_retloc = 0;
      key_retloc = NULL;
      
      gconf_listeners_remove(listeners, ids[i]);

      check(strcmp(key_retloc, *keyp) == 0,
            "listener removed has different key from listener added (`%s' vs. `%s')", *keyp, key_retloc);

      g_free(key_retloc);
      
      check(ids[i] == id_retloc, "listener removed had different id from that added (%u vs. %u)", ids[i], id_retloc);

      check(gconf_listeners_count(listeners) == 0,
            "didn't have 0 listeners as expected");
      
      ++i;
      ++keyp;
    }
  
  check(gconf_listeners_count(listeners) == 0,
        "listener count isn't 0 after removing all the listeners");
}

static void
check_double_add_remove(GConfListeners* listeners)
{
  guint ids[128];
  guint i;
  const gchar** keyp;
  guint id_retloc;
  gchar* key_retloc;
  
  memset(ids, 0, sizeof(ids[0]) * 128);
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      struct stuff* s;

      s = g_new0(struct stuff, 1);

      s->id_retloc = &id_retloc;
      s->key_retloc = &key_retloc;
      
      s->id = ids[i] = gconf_listeners_add(listeners,
                                           *keyp,
                                           s,
                                           test_destroy_notify);

      s->key = g_strdup(*keyp);

      check(ids[i] != 0, "invalid connection ID returned for added listener");
      
      ++i;
      if ((i % 2) == 0)
        ++keyp;
    }

  check(gconf_listeners_count(listeners) == i,
        "number of listeners added (%u) don't now exist in the GConfListeners (%u exist)", i, gconf_listeners_count(listeners));
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      id_retloc = 0;
      key_retloc = NULL;
      
      gconf_listeners_remove(listeners, ids[i]);

      check(strcmp(key_retloc, *keyp) == 0,
            "listener removed has different key from listener added (`%s' vs. `%s')", *keyp, key_retloc);

      g_free(key_retloc);

      check(ids[i] == id_retloc, "listener removed had different id from that added (%u vs. %u)", ids[i], id_retloc);            
      
      ++i;
      if ((i % 2) == 0)
        ++keyp;
    }

  check(gconf_listeners_count(listeners) == 0,
        "listener count isn't 0 after removing all the listeners");
}

static gboolean
should_be_notified(const gchar* changed_key,
                   const gchar* listener_watchpoint)
{
  return strncmp(changed_key, listener_watchpoint, strlen(listener_watchpoint)) == 0;
}

struct notify_data {
  const gchar* notify_key;
  GSList* notified;
};

void
notify_callback(GConfListeners* listeners,
                const gchar* all_above_key,
                guint cnxn_id,
                gpointer listener_data,
                gpointer user_data)
{
  struct notify_data* nd = user_data;
  struct stuff* s = listener_data;
  
  check(strcmp(all_above_key, nd->notify_key) == 0,        "notify key `%s' is not the notify key received in callback `%s'",
        nd->notify_key, all_above_key);

  check(cnxn_id == s->id,
        "listener ID is wrong in callback (%u vs. %u)",
        cnxn_id, s->id);
  
  check(should_be_notified(all_above_key, s->key),
        "listener %u at `%s' shouldn't have been notified of change to `%s'",
        s->id, s->key, all_above_key);

  nd->notified = g_slist_prepend(nd->notified, s);
}

void
check_notification(GConfListeners* listeners)
{
  guint ids[128];
  guint i;
  const gchar** keyp;
  guint id_retloc;
  gchar* key_retloc;
  
  memset(ids, 0, sizeof(ids[0]) * 128);
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      struct stuff* s;

      s = g_new0(struct stuff, 1);

      s->id_retloc = &id_retloc;
      s->key_retloc = &key_retloc;
      
      s->id = ids[i] = gconf_listeners_add(listeners,
                                           *keyp,
                                           s,
                                           test_destroy_notify);

      s->key = g_strdup(*keyp);

      check(ids[i] != 0, "invalid connection ID returned for added listener");

      /*       printf("%u added at `%s'\n", s->id, s->key); */
      
      ++i;
      ++keyp;
    }

  check(gconf_listeners_count(listeners) == i,
        "number of listeners added (%u) don't now exist in the GConfListeners (%u exist)", i, gconf_listeners_count(listeners));

  keyp = keys;

  while (*keyp)
    {
      GSList* tmp = NULL;
      const gchar** sub_keyp;
      struct notify_data nd = { NULL, NULL };

      nd.notify_key = *keyp;
      
      gconf_listeners_notify(listeners, *keyp,
                             notify_callback,
                             &nd);
      
      /* Check that the list that was notified matches
         the list that should have been */
      sub_keyp = keys;
      while (*sub_keyp)
        {
          struct stuff* s = NULL;
          gboolean should_be = should_be_notified(*keyp, *sub_keyp);

          tmp = nd.notified;
          while (tmp != NULL)
            {
              s = tmp->data;
              
              if (strcmp(s->key, *sub_keyp) == 0)
                break;
              
              tmp = g_slist_next(tmp);
            }

          if (should_be)
            {
              check (tmp != NULL, "listener at `%s' should have been notified of change to `%s' and was not", *sub_keyp, *keyp);
              s = tmp->data;
              /* remove so we can handle duplicate keys */
              nd.notified = g_slist_remove(nd.notified, tmp->data);

#if 0              
              g_assert(strcmp(s->key, *sub_keyp) == 0);
              printf("%u at `%s' notified properly of `%s'\n",
                     s->id, *sub_keyp, *keyp);
#endif
            }
          else
            {
              check(tmp == NULL, "listener at `%s' should not have been notified of change to `%s' but it was", *sub_keyp, *keyp);
#if 0
              printf("`%s' properly not notified of `%s'\n",
                     *sub_keyp, *keyp);
#endif
            }
          
          ++sub_keyp;
        }

      if (nd.notified != NULL)
        {
          GSList* tmp = nd.notified;
          while (tmp != NULL)
            {
              struct stuff* s = tmp->data;
              fprintf(stderr, "leftover: %u at `%s' notified of `%s'\n",
                      s->id, s->key, *keyp);
              tmp = g_slist_next(tmp);
            }
        }
      check (nd.notified == NULL, "superfluous listeners were notified of `%s'; perhaps listeners were notified twice?", *keyp);
      
      ++keyp;
    }
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      id_retloc = 0;
      key_retloc = NULL;
      
      gconf_listeners_remove(listeners, ids[i]);

      check(strcmp(key_retloc, *keyp) == 0,
            "listener removed has different key from listener added (`%s' vs. `%s')", *keyp, key_retloc);

      g_free(key_retloc);

      check(ids[i] == id_retloc, "listener removed had different id from that added (%u vs. %u)", ids[i], id_retloc);            
      
      ++i;
      ++keyp;
    }

  check(gconf_listeners_count(listeners) == 0,
        "listener count isn't 0 after removing all the listeners");
}

struct destroy_data {
  gchar* key;
  guint* destroy_count_loc;
};

static void
destroy_test_destroy_notify(gpointer data)
{
  struct destroy_data* d = data;

  *d->destroy_count_loc += 1;
  
  g_free(d->key);
  
  g_free(d);
}

/* This is mostly for use with memory leak detection tools */
void
check_destroy(void)
{
  GConfListeners* listeners;
  guint ids[128];
  guint i;
  const gchar** keyp;
  guint destroy_count = 0;
  
  listeners = gconf_listeners_new();
  
  memset(ids, 0, sizeof(ids[0]) * 128);
  
  i = 0;
  keyp = keys;

  while (i < 128 && *keyp)
    {
      struct destroy_data* d;

      d = g_new0(struct destroy_data, 1);
      
      d->destroy_count_loc = &destroy_count;
      
      ids[i] = gconf_listeners_add(listeners,
                                   *keyp,
                                   d,
                                   destroy_test_destroy_notify);

      d->key = g_strdup(*keyp);

      check(ids[i] != 0, "invalid connection ID returned for added listener");
      
      ++i;
      ++keyp;
    }

  check(gconf_listeners_count(listeners) == i,
        "number of listeners added (%u) don't now exist in the GConfListeners (%u exist)", i, gconf_listeners_count(listeners));

  gconf_listeners_free(listeners);

  check(destroy_count == i,
        "number of listeners added (%u) doesn't match number destroyed (%u) on GConfListeners destruction", i, destroy_count);
}

int 
main (int argc, char** argv)
{
  GConfListeners* listeners;

  listeners = gconf_listeners_new();

  check_add_remove(listeners);

  g_assert(gconf_listeners_count(listeners) == 0);
  
  check_immediate_remove_after_add(listeners);

  g_assert(gconf_listeners_count(listeners) == 0);
  
  check_double_add_remove(listeners);

  g_assert(gconf_listeners_count(listeners) == 0);
  
  check_notification(listeners);

  g_assert(gconf_listeners_count(listeners) == 0);
  
  gconf_listeners_free(listeners);

  check_destroy();
  
  printf("\n");
  
  return 0;
}
