/*
 * testing-util.c
 * This file is part of libpeas
 *
 * Copyright (C) 2011 - Garrett Regier
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>

#include <glib.h>
#include <girepository.h>

#include "libpeas/peas-engine-priv.h"

#include "testing-util.h"

typedef struct {
  const gchar *pattern;
  gboolean hit;
} LogHook;

static PeasEngine *engine = NULL;
static GPtrArray *log_hooks = NULL;

/* These are warnings and criticals that just have to happen
 * for testing purposes and as such we don't want to abort on them.
 *
 * If the warnings are for specific tests use
 * testing_util_push_log_hook() and testing_util_pop_log_hook() which
 * will assert that the warning or critical actually happened.
 *
 * Don't bother putting errors in here as GLib always aborts on errors.
 */
static const gchar *allowed_patterns[] = {
  "*lib*loader.so*cannot open shared object file: No such file or directory*"
};

static void
log_handler (const gchar    *log_domain,
             GLogLevelFlags  log_level,
             const gchar    *message,
             gpointer        user_data)
{
  guint i;

  /* We always want to log debug, info and message logs */
  if ((log_level & G_LOG_LEVEL_DEBUG) != 0 ||
      (log_level & G_LOG_LEVEL_INFO) != 0 ||
      (log_level & G_LOG_LEVEL_MESSAGE) != 0)
    {
      g_log_default_handler (log_domain, log_level, message, user_data);
      return;
    }

  /* Don't bother trying to match errors as GLib always aborts on them */
  if ((log_level & G_LOG_LEVEL_ERROR) != 0)
    {
      g_log_default_handler (log_domain, log_level, message, user_data);

      /* Call abort() as GLib may call G_BREAKPOINT() instead */
      abort ();
    }

  for (i = 0; i < log_hooks->len; ++i)
    {
      LogHook *hook = g_ptr_array_index (log_hooks, i);

      if (g_pattern_match_simple (hook->pattern, message))
        {
          hook->hit = TRUE;
          return;
        }
    }

  /* Check the allowed_patterns after the log hooks to
   * avoid issues when an allowed_pattern would match a hook
   */
  for (i = 0; i < G_N_ELEMENTS (allowed_patterns); ++i)
    {
      if (g_pattern_match_simple (allowed_patterns[i], message))
        return;
    }

  /* Warnings and criticals are not allowed to be unhandled */
  if ((log_level & G_LOG_LEVEL_WARNING) != 0)
    message = g_strdup_printf ("Unhandled warning: %s: %s", log_domain, message);
  else
    message = g_strdup_printf ("Unhandled critical: %s: %s", log_domain, message);

  /* Use the default log handler directly to avoid recurse complaints */
  g_log_default_handler (G_LOG_DOMAIN, G_LOG_LEVEL_ERROR,
                         message, user_data);

  testing_util_pop_log_hooks ();

  /* The default handler does not actually abort */
  abort ();
}

void
testing_util_init (void)
{
  GError *error = NULL;
  static gboolean initialized = FALSE;

  if (initialized)
    return;

  /* Don't abort on warnings or criticals */
  g_log_set_always_fatal (G_LOG_LEVEL_ERROR);

  g_log_set_default_handler (log_handler, NULL);

  g_irepository_prepend_search_path (BUILDDIR "/libpeas");

  g_setenv ("PEAS_PLUGIN_LOADERS_DIR", BUILDDIR "/loaders", TRUE);

  g_irepository_require (g_irepository_get_default (), "Peas", "1.0", 0, &error);
  g_assert_no_error (error);

  log_hooks = g_ptr_array_new_with_free_func (g_free);

  initialized = TRUE;
}

PeasEngine *
testing_util_engine_new (void)
{
  /* testing_util_engine_free() checks that the
   * engine is freed so only one engine can be created
   */
  g_assert (engine == NULL);

  testing_util_init ();

  /* Must be after requiring typelibs */
  engine = peas_engine_new ();

  g_object_add_weak_pointer (G_OBJECT (engine), (gpointer *) &engine);

  peas_engine_add_search_path (engine, BUILDDIR "/tests/plugins",
                                       SRCDIR   "/tests/plugins");

  return engine;
}

void
testing_util_engine_free (PeasEngine *engine_)
{
  /* In case a test needs to free the engine */
  if (engine != NULL)
    {
      g_object_unref (engine_);

      /* Make sure that at the end of every test the engine is freed */
      g_assert (engine == NULL);
    }

  /* Pop the log hooks so the test cases don't have to */
  testing_util_pop_log_hooks ();
}

int
testing_util_run_tests (void)
{
  int retval;

  retval = g_test_run ();

  if (log_hooks != NULL)
    {
      g_assert_cmpuint (log_hooks->len, ==, 0);
      g_ptr_array_unref (log_hooks);
    }

  /* Cannot call this with atexit() because
   * gcov does not register that it was called.
   */
  peas_engine_shutdown ();

  return retval;
}

void
testing_util_push_log_hook (const gchar *pattern)
{
  LogHook *hook;

  g_return_if_fail (log_hooks != NULL);
  g_return_if_fail (pattern != NULL && *pattern != '\0');

  hook = g_new (LogHook, 1);
  hook->pattern = pattern;
  hook->hit = FALSE;

  g_ptr_array_add (log_hooks, hook);
}

/* Optional - see testing_util_engine_free() */
void
testing_util_pop_log_hook (void)
{
  LogHook *hook;

  g_return_if_fail (log_hooks != NULL);
  g_return_if_fail (log_hooks->len > 0);
  
  hook = g_ptr_array_index (log_hooks, log_hooks->len - 1);

  if (!hook->hit)
    testing_util_pop_log_hooks ();

  g_ptr_array_remove_index (log_hooks, log_hooks->len - 1);
}

void
testing_util_pop_log_hooks (void)
{
  guint i;
  LogHook *hook;
  GPtrArray *unhit_hooks;

  g_return_if_fail (log_hooks != NULL);

  if (log_hooks->len == 0)
    return;

  unhit_hooks = g_ptr_array_new ();

  for (i = 0; i < log_hooks->len; ++i)
    {
      hook = g_ptr_array_index (log_hooks, i);

      if (!hook->hit)
        g_ptr_array_add (unhit_hooks, hook);
    }

  if (unhit_hooks->len == 1)
    {
      gchar *msg;

      hook = g_ptr_array_index (unhit_hooks, 0);
      msg = g_strdup_printf ("Log hook was not triggered: '%s'",
                             hook->pattern);

      /* Use the default log handler directly to avoid recurse complaints */
      g_log_default_handler (G_LOG_DOMAIN, G_LOG_LEVEL_ERROR, msg, NULL);
      abort ();
    }
  else if (unhit_hooks->len > 1)
    {
      /* Use the default log handler directly to avoid recurse complaints */
      g_log_default_handler (G_LOG_DOMAIN, G_LOG_LEVEL_ERROR,
                             "Log hooks were not triggered:\n", NULL);

      for (i = 0; i < unhit_hooks->len; ++i)
        {
          hook = g_ptr_array_index (unhit_hooks, i);

          g_print ("\t'%s'\n", hook->pattern);
        }

      abort ();
    }

  g_ptr_array_unref (unhit_hooks);

  g_ptr_array_set_size (log_hooks, 0);
}
