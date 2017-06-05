/* Regression test for object registration and unregistration
 *
 * Copyright © 2009 Collabora Ltd. <http://www.collabora.co.uk/>
 * Copyright © 2009-2011 Nokia Corporation
 *
 * In preparation for dbus-glib relicensing (if it ever happens), this file is
 * licensed under (at your option) either the AFL v2.1, the GPL v2 or later,
 * or an MIT/X11-style license:
 *
 * Licensed under the Academic Free License version 2.1
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <config.h>

#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>

#include "my-object.h"

GMainLoop *loop = NULL;

typedef struct {
    DBusError dbus_error;
    DBusGConnection *bus;
    DBusGConnection *bus2;
    GObject *object;
    DBusMessage *frobnicate1_message;
    DBusMessage *frobnicate2_message;
    gboolean received_objectified;
} Fixture;

#define assert_no_error(e) _assert_no_error (e, __FILE__, __LINE__)
static void
_assert_no_error (const DBusError *e,
    const char *file,
    int line)
{
  if (G_UNLIKELY (dbus_error_is_set (e)))
    g_error ("%s:%d: expected success but got error: %s: %s",
        file, line, e->name, e->message);
}

static void
setup (Fixture *f,
    gconstpointer path_to_use)
{
  dbus_error_init (&f->dbus_error);

  f->bus = dbus_g_bus_get_private (DBUS_BUS_SESSION, NULL, NULL);
  g_assert (f->bus != NULL);

  f->bus2 = dbus_g_bus_get_private (DBUS_BUS_SESSION, NULL, NULL);
  g_assert (f->bus2 != NULL);

  f->object = g_object_new (MY_TYPE_OBJECT, NULL);
  g_assert (MY_IS_OBJECT (f->object));
}

static void
teardown (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  /* we close the connection before releasing the object, to test fd.o #5688
   * in test_lookup() */
  if (f->bus != NULL)
    {
      dbus_connection_close (dbus_g_connection_get_connection (f->bus));
      dbus_g_connection_unref (f->bus);
    }

  if (f->bus2 != NULL)
    {
      dbus_connection_close (dbus_g_connection_get_connection (f->bus2));
      dbus_g_connection_unref (f->bus2);
    }

  if (f->object != NULL)
    {
      g_object_unref (f->object);
    }

  /* This is safe to call on an initialized-but-unset DBusError, a bit like
   * g_clear_error */
  dbus_error_free (&f->dbus_error);
}

static void
test_lookup (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  /* teardown() closes the connection before f->object is destroyed, which
   * used to be broken */
  g_test_bug ("5688");

  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);
  /* this was briefly broken while fixing fd.o#5688 */
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/bar") == NULL);
}

static void
test_unregister (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  /* feature test: objects can be unregistered */
  g_test_bug ("21219");

  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);
  dbus_g_connection_unregister_g_object (f->bus, f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") == NULL);
}

static void
test_unregister_on_last_unref (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  gpointer weak_pointer;

  weak_pointer = f->object;
  g_object_add_weak_pointer (weak_pointer, &weak_pointer);

  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);
  /* implicit unregistration by the last-unref of the object */
  g_object_unref (f->object);
  f->object = NULL;

  g_assert (weak_pointer == NULL);

  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") == NULL);
}

static void
test_unregister_on_forced_dispose (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);
  /* implicit unregistration by dispose() of the object (don't try
   * this at home) */
  g_object_run_dispose (f->object);

  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") == NULL);
}

static void
test_reregister (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);

  /* Before 0.82, re-registering the same object path was leaky but successful.
   * 0.82 disallowed this behaviour. Since 0.84 it was meant to be allowed
   * again, and a no-op, but it actually had the effect of removing all
   * record of the registrations (while leaving the object registered with
   * libdbus). */
  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);

  /* This would critical in 0.84. */
  dbus_g_connection_unregister_g_object (f->bus, f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") == NULL);
}

static DBusHandlerResult
frobnicate_cb (DBusConnection *conn,
    DBusMessage *message,
    void *user_data)
{
  Fixture *f = user_data;

  if (dbus_message_is_signal (message,
        "org.freedesktop.DBus.GLib.Tests.MyObject", "Frobnicate"))
    {
      const char *sender = dbus_message_get_sender (message);
      const char *path = dbus_message_get_path (message);

      g_assert (sender != NULL);
      g_assert (path != NULL);

      if (g_strcmp0 (path, "/foo") == 0)
        {
          g_assert_cmpstr (sender, ==, dbus_bus_get_unique_name (
                dbus_g_connection_get_connection (f->bus)));

          g_assert (f->frobnicate1_message == NULL);
          f->frobnicate1_message = dbus_message_ref (message);
        }
      else
        {
          g_assert_cmpstr (path, ==, "/bar");
          g_assert_cmpstr (sender, ==, dbus_bus_get_unique_name (
                dbus_g_connection_get_connection (f->bus2)));

          g_assert (f->frobnicate2_message == NULL);
          f->frobnicate2_message = dbus_message_ref (message);
        }
    }

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static void
test_twice (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  dbus_bool_t mem;

  g_test_bug ("32087");

  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);

  dbus_g_connection_register_g_object (f->bus2, "/bar", f->object);

  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus2, "/bar") ==
      f->object);

  dbus_bus_add_match (dbus_g_connection_get_connection (f->bus),
      "type='signal'", &f->dbus_error);
  assert_no_error (&f->dbus_error);
  mem = dbus_connection_add_filter (dbus_g_connection_get_connection (f->bus),
      frobnicate_cb, f, NULL);
  g_assert (mem);

  my_object_emit_frobnicate ((MyObject *) f->object, NULL);

  /* The object should emit the signal once onto each connection,
   * from the appropriate location */
  while (f->frobnicate1_message == NULL || f->frobnicate2_message == NULL)
    g_main_context_iteration (NULL, TRUE);

  dbus_message_unref (f->frobnicate1_message);
  f->frobnicate1_message = NULL;
  dbus_message_unref (f->frobnicate2_message);
  f->frobnicate2_message = NULL;

  /* try again, to catch any extra emissions, but first unregister one of the
   * object's locations */
  dbus_g_connection_unregister_g_object (f->bus, f->object);
  my_object_emit_frobnicate ((MyObject *) f->object, NULL);

  while (f->frobnicate2_message == NULL)
    g_main_context_iteration (NULL, TRUE);

  g_assert (f->frobnicate1_message == NULL);
  dbus_message_unref (f->frobnicate2_message);
  f->frobnicate2_message = NULL;
}

static void
test_clean_slate (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  DBusError e;
  dbus_bool_t mem;

  dbus_bus_add_match (dbus_g_connection_get_connection (f->bus),
      "type='signal'", &f->dbus_error);
  assert_no_error (&f->dbus_error);
  mem = dbus_connection_add_filter (dbus_g_connection_get_connection (f->bus),
      frobnicate_cb, f, NULL);
  g_assert (mem);

  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);

  my_object_emit_frobnicate ((MyObject *) f->object, NULL);

  while (f->frobnicate1_message == NULL)
    g_main_context_iteration (NULL, TRUE);

  dbus_message_unref (f->frobnicate1_message);
  f->frobnicate1_message = NULL;

  /* unregister the object from its last object path, then put it back
   * in the same location */
  dbus_g_connection_unregister_g_object (f->bus, f->object);
  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);

  /* bug: in 0.92, this would be emitted twice because the hook was added
   * twice */
  my_object_emit_frobnicate ((MyObject *) f->object, NULL);

  while (f->frobnicate1_message == NULL)
    g_main_context_iteration (NULL, TRUE);

  dbus_message_unref (f->frobnicate1_message);
  f->frobnicate1_message = NULL;

  /* unregister the object from its last object path, then put it back
   * at a different location */
  dbus_g_connection_unregister_g_object (f->bus, f->object);
  dbus_g_connection_register_g_object (f->bus2, "/bar", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus2, "/bar") ==
      f->object);

  my_object_emit_frobnicate ((MyObject *) f->object, NULL);

  while (f->frobnicate2_message == NULL)
    g_main_context_iteration (NULL, TRUE);

  /* check that this wasn't received anyway, which would indicate that
   * either unregistration from /foo was unsuccessful, or the double
   * emission mentioned above was seen */
  g_assert (f->frobnicate1_message == NULL);

  dbus_message_unref (f->frobnicate2_message);
  f->frobnicate2_message = NULL;
}

static DBusHandlerResult
objectified_cb (DBusConnection *conn,
    DBusMessage *message,
    void *user_data)
{
  Fixture *f = user_data;

  if (dbus_message_is_signal (message,
        "org.freedesktop.DBus.GLib.Tests.MyObject", "Objectified"))
    {
      const char *sender = dbus_message_get_sender (message);
      const char *path = dbus_message_get_path (message);
      dbus_bool_t ok;
      DBusError e;

      dbus_error_init (&e);

      g_assert (sender != NULL);
      g_assert (path != NULL);

      g_assert_cmpstr (path, ==, "/foo");
      g_assert_cmpstr (sender, ==, dbus_bus_get_unique_name (
            dbus_g_connection_get_connection (f->bus)));

      path = NULL;
      ok = dbus_message_get_args (message, &e,
          DBUS_TYPE_OBJECT_PATH, &path,
          DBUS_TYPE_INVALID);

      if (dbus_error_is_set (&e))
        g_error ("%s: %s", e.name, e.message);

      g_assert (ok);
      g_assert_cmpstr (path, ==, "/foo");

      f->received_objectified = TRUE;
    }

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static void
test_marshal_object (Fixture *f,
    gconstpointer test_data G_GNUC_UNUSED)
{
  dbus_bool_t mem;

  g_test_bug ("37852");

  dbus_g_connection_register_g_object (f->bus, "/foo", f->object);
  g_assert (dbus_g_connection_lookup_g_object (f->bus, "/foo") ==
      f->object);

  dbus_bus_add_match (dbus_g_connection_get_connection (f->bus),
      "type='signal'", &f->dbus_error);
  assert_no_error (&f->dbus_error);
  mem = dbus_connection_add_filter (dbus_g_connection_get_connection (f->bus),
      objectified_cb, f, NULL);
  g_assert (mem);

  my_object_emit_objectified ((MyObject *) f->object, f->object);

  while (!f->received_objectified)
    g_main_context_iteration (NULL, TRUE);
}

int
main (int argc, char **argv)
{
  loop = g_main_loop_new (NULL, FALSE);

  g_type_init ();
  g_log_set_always_fatal (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);
  dbus_g_type_specialized_init ();
  g_test_bug_base ("https://bugs.freedesktop.org/show_bug.cgi?id=");
  g_test_init (&argc, &argv, NULL);

  g_test_add ("/registrations/lookup", Fixture, NULL,
      setup, test_lookup, teardown);
  g_test_add ("/registrations/unregister", Fixture, NULL,
      setup, test_unregister, teardown);
  g_test_add ("/registrations/unregister-on-last-unref", Fixture, NULL,
      setup, test_unregister_on_last_unref, teardown);
  g_test_add ("/registrations/unregister-on-forced-dispose", Fixture, NULL,
      setup, test_unregister_on_forced_dispose, teardown);
  g_test_add ("/registrations/reregister", Fixture, NULL,
      setup, test_reregister, teardown);
  g_test_add ("/registrations/twice", Fixture, NULL,
      setup, test_twice, teardown);
  g_test_add ("/registrations/clean-slate", Fixture, NULL,
      setup, test_clean_slate, teardown);
  g_test_add ("/registrations/marshal-object", Fixture, NULL,
      setup, test_marshal_object, teardown);

  return g_test_run ();
}
