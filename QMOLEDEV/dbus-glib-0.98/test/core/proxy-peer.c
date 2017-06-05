/* Regression tests for using a DBusGProxy on a peer-to-peer connection.
 *
 * Author: Simon McVittie <simon.mcvittie@collabora.co.uk>
 * Copyright © 2011 Nokia Corporation
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <config.h>

#include <glib.h>

#include <dbus/dbus.h>
#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>

#include "my-object.h"

GMainLoop *loop;

typedef struct {
    DBusError e;

    DBusServer *server;
    DBusConnection *server_conn;
    DBusGConnection *server_gconn;

    DBusConnection *client_conn;
    DBusGConnection *client_gconn;

    DBusGProxy *proxy;
    gboolean proxy_destroyed;
    GObject *object;
    GHashTable *in_flight;
    GHashTable *completed;
} Fixture;

static void
assert_no_error (const DBusError *e)
{
  if (G_UNLIKELY (dbus_error_is_set (e)))
    g_error ("expected success but got error: %s: %s", e->name, e->message);
}

static void
new_conn_cb (DBusServer *server,
    DBusConnection *server_conn,
    void *data)
{
  Fixture *f = data;

  g_assert (f->server_conn == NULL);
  f->server_conn = dbus_connection_ref (server_conn);
  dbus_connection_setup_with_g_main (server_conn, NULL);
  f->server_gconn = dbus_connection_get_g_connection (server_conn);
}

static void
destroy_cb (DBusGProxy *proxy,
    gpointer user_data)
{
  Fixture *f = user_data;

  g_assert (proxy == f->proxy);

  f->proxy_destroyed = TRUE;
}

static void
setup (Fixture *f,
    gconstpointer addr)
{
  dbus_error_init (&f->e);

  f->server = dbus_server_listen (addr, &f->e);
  assert_no_error (&f->e);
  g_assert (f->server != NULL);

  dbus_server_set_new_connection_function (f->server,
      new_conn_cb, f, NULL);
  dbus_server_setup_with_g_main (f->server, NULL);

  g_assert (f->server_conn == NULL);

  f->client_conn = dbus_connection_open_private (
      dbus_server_get_address (f->server), &f->e);
  assert_no_error (&f->e);
  g_assert (f->client_conn != NULL);
  dbus_connection_setup_with_g_main (f->client_conn, NULL);
  f->client_gconn = dbus_connection_get_g_connection (f->client_conn);

  while (f->server_conn == NULL)
    {
      g_print (".");
      g_main_context_iteration (NULL, TRUE);
    }

  f->object = g_object_new (MY_TYPE_OBJECT,
      NULL);
  dbus_g_connection_register_g_object (f->server_gconn,
      "/org/freedesktop/DBus/GLib/Tests/MyTestObject", f->object);

  f->proxy = dbus_g_proxy_new_for_peer (f->client_gconn,
      "/org/freedesktop/DBus/GLib/Tests/MyTestObject",
      "org.freedesktop.DBus.GLib.Tests.MyObject");
  g_assert (f->proxy != NULL);
  g_assert (DBUS_IS_G_PROXY (f->proxy));

  g_signal_connect (f->proxy, "destroy", G_CALLBACK (destroy_cb), f);

  f->in_flight = g_hash_table_new (NULL, NULL);
  f->completed = g_hash_table_new (NULL, NULL);
}

static void
call_cb (DBusGProxy *proxy,
    DBusGProxyCall *call,
    gpointer data)
{
  Fixture *f = data;
  gboolean found;

  found = g_hash_table_remove (f->in_flight, call);
  g_assert (found);
  g_hash_table_insert (f->completed, call, call);
}

static void
test_method (Fixture *f,
    gconstpointer addr)
{
  GError *error = NULL;
  gboolean ok;
  DBusGProxyCall *call;

  call = dbus_g_proxy_begin_call (f->proxy, "DoNothing", call_cb, f, NULL,
      G_TYPE_INVALID);
  g_assert (call != NULL);
  g_hash_table_insert (f->in_flight, call, call);

  while (g_hash_table_size (f->in_flight) > 0)
    {
      g_print (".");
      g_main_context_iteration (NULL, TRUE);
    }

  ok = g_hash_table_remove (f->completed, call);
  g_assert (ok);
  ok = dbus_g_proxy_end_call (f->proxy, call, &error,
      G_TYPE_INVALID);
  g_assert_no_error (error);
  g_assert (ok);
}

static void
test_disconnect (Fixture *f,
    gconstpointer addr)
{
  GError *error = NULL;
  gboolean ok;
  DBusGProxyCall *fail;

  g_test_bug ("38406");

  dbus_connection_close (f->client_conn);
  dbus_connection_close (f->server_conn);

  fail = dbus_g_proxy_begin_call (f->proxy, "DoNothing", call_cb, f, NULL,
      G_TYPE_INVALID);
  g_assert (fail == NULL);

  ok = dbus_g_proxy_end_call (f->proxy, fail, &error,
      G_TYPE_INVALID);
  g_assert_error (error, DBUS_GERROR, DBUS_GERROR_DISCONNECTED);
  g_assert (!ok);
  g_clear_error (&error);

  while (!f->proxy_destroyed)
    {
      g_print (".");
      g_main_context_iteration (NULL, TRUE);
    }
}

static void
teardown (Fixture *f,
    gconstpointer addr G_GNUC_UNUSED)
{
  f->client_gconn = NULL;
  f->server_gconn = NULL;

  if (f->in_flight != NULL)
    {
      g_hash_table_unref (f->in_flight);
      f->in_flight = NULL;
    }

  if (f->completed != NULL)
    {
      g_hash_table_unref (f->completed);
      f->completed = NULL;
    }

  if (f->proxy != NULL)
    {
      g_signal_handlers_disconnect_by_func (f->proxy, destroy_cb, f);
      g_object_unref (f->proxy);
      f->proxy = NULL;
    }

  if (f->object != NULL)
    {
      g_object_unref (f->object);
      f->object = NULL;
    }

  if (f->client_conn != NULL)
    {
      dbus_connection_close (f->client_conn);
      dbus_connection_unref (f->client_conn);
      f->client_conn = NULL;
    }

  if (f->server_conn != NULL)
    {
      dbus_connection_close (f->server_conn);
      dbus_connection_unref (f->server_conn);
      f->server_conn = NULL;
    }

  if (f->server != NULL)
    {
      dbus_server_disconnect (f->server);
      dbus_server_unref (f->server);
      f->server = NULL;
    }
}

int
main (int argc,
    char **argv)
{
  g_test_init (&argc, &argv, NULL);
  g_type_init ();
  dbus_g_type_specialized_init ();

  g_test_bug_base ("https://bugs.freedesktop.org/show_bug.cgi?id=");

  g_test_add ("/proxy/method", Fixture, "unix:tmpdir=/tmp", setup,
      test_method, teardown);

  g_test_add ("/proxy/disconnect", Fixture, "unix:tmpdir=/tmp", setup,
      test_disconnect, teardown);

  return g_test_run ();
}
