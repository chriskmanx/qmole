#include <config.h>

#include <stdlib.h>
#include <string.h>
#include "dbus/dbus-gparser.h"
#include "test-song-bindings.h"
#include "test-hello-bindings.h"
#include "test-goodbye-bindings.h"
#include "test-dup-prop-a-bindings.h"
#include "test-dup-prop-b-bindings.h"

#define TEST_NAMESPACE "org.freedesktop.DBus.GLib.Test.Interfaces"
#define TEST_OBJECT_PATH "/org/freedesktop/DBus/GLib/Test/Interfaces"
#define TEST_DP_OBJECT_PATH "/org/freedesktop/DBus/GLib/Test/DupPropInterfaces"

#define TEST_DP_IFACE_A "org.freedesktop.DBus.GLib.Test.Interfaces.A"
#define TEST_DP_IFACE_B "org.freedesktop.DBus.GLib.Test.Interfaces.B"

static void
test_dp_property (DBusGProxy *proxy,
                  const char *detail,
                  const char *iface,
                  guint expected,
                  gboolean get_only)
{
  GError *error = NULL;
  gboolean success;
  GValue get_value = {0,};

  if (!get_only)
    {
      GValue set_value = {0,};

      g_value_init (&set_value, G_TYPE_UINT);
      g_value_set_uint (&set_value, expected);
      success = dbus_g_proxy_call (proxy, "Set", &error,
                                   G_TYPE_STRING, iface,
                                   G_TYPE_STRING, "Foobar",
                                   G_TYPE_VALUE, &set_value,
                                   G_TYPE_INVALID,
                                   G_TYPE_INVALID);
      g_value_unset (&set_value);
      if (!success)
        {
          g_print ("Error while setting DupProp Interface %s property: %s\n", detail, error->message);
          g_error_free (error);
          exit(1);
        }
      else
        g_print ("Set DupProp Interface %s property with success\n", detail);
    }

  success = dbus_g_proxy_call (proxy, "Get", &error,
                               G_TYPE_STRING, iface,
                               G_TYPE_STRING, "Foobar",
                               G_TYPE_INVALID,
                               G_TYPE_VALUE, &get_value,
                               G_TYPE_INVALID);
  if (!success)
    {
      g_print ("Error while getting DupProp Interface %s property: %s\n", detail, error->message);
      g_error_free (error);
      exit(1);
    }
  else
      g_print ("Got DupProp Interface %s property with success\n", detail);

  if (!G_VALUE_HOLDS_UINT (&get_value))
    {
      g_print ("Error comparing DupProp %s Interface property: unexpected type %s\n",
               detail, G_VALUE_TYPE_NAME (&get_value));
      g_error_free (error);
      exit(1);
    }
  else if (g_value_get_uint (&get_value) != expected)
    {
      g_print ("Error comparing DupProp %s Interface property: expected %d, got %d\n",
               detail, expected, g_value_get_uint (&get_value));
      g_error_free (error);
      exit(1);
    }
  else
      g_print ("Got DupProp Interface %s property value matched expected\n", detail);
}

int
main (int    argc,
      char **argv)
{
  DBusGConnection *connection;
  DBusGProxy *proxy;
  GError *error = NULL;
  gchar *str;
  gboolean success;
  DBusGProxy *dp_proxy;

  g_type_init ();

  connection = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
  if (connection == NULL)
    {
      g_error ("Failed to make connection to session bus: %s", error->message);
      g_error_free (error);
      exit(1);
    }

  proxy = dbus_g_proxy_new_for_name (connection, TEST_NAMESPACE, TEST_OBJECT_PATH,
                                     "org.freedesktop.DBus.GLib.Test.Interfaces.Song");
  success = org_freedesktop_DBus_GLib_Test_Interfaces_Song_get_title (proxy, &str, &error);
  g_object_unref (proxy);

  if (!success)
    {
      g_print ("Error while calling Parent object method: %s\n", error->message);
      g_error_free (error);
      exit(1);
    }
  else
    {
      g_free (str);
      g_print ("Called Parent object method with success\n");
    }

  proxy = dbus_g_proxy_new_for_name (connection, TEST_NAMESPACE, TEST_OBJECT_PATH,
                                     "org.freedesktop.DBus.GLib.Test.Interfaces.Hello");
  g_assert (proxy != NULL);
  success = org_freedesktop_DBus_GLib_Test_Interfaces_Hello_say_hello (proxy, &str, &error);
  g_object_unref (proxy);

  if (!success)
    {
      g_print ("Error while calling Parent Interface object method: %s\n", error->message);
      g_error_free (error);
      exit(1);
    }
  else
    {
      g_free (str);
      g_print ("Called Parent Interface object method with success\n");
    }

  proxy = dbus_g_proxy_new_for_name (connection, TEST_NAMESPACE, TEST_OBJECT_PATH,
                                     "org.freedesktop.DBus.GLib.Test.Interfaces.Goodbye");
  success = org_freedesktop_DBus_GLib_Test_Interfaces_Goodbye_say_goodbye (proxy, &str, &error);
  g_object_unref (proxy);

  if (!success)
    {
      g_print ("Error while calling Object Interface object method: %s\n", error->message);
      g_error_free (error);
      exit(1);
    }
  else
    {
      g_free (str);
      g_print ("Called Object Interface object method with success\n");
    }

  /* Test interfaces with conflicting property names on the same GObject */
  dp_proxy = dbus_g_proxy_new_for_name (connection, TEST_NAMESPACE, TEST_DP_OBJECT_PATH,
                                        "org.freedesktop.DBus.Properties");

  /* test that setting the property and reading it back works */
  test_dp_property (dp_proxy, "A", TEST_DP_IFACE_A, 235235, FALSE);
  test_dp_property (dp_proxy, "B", TEST_DP_IFACE_B, 11981241, FALSE);

  /* Test that setting A does not change B */
  test_dp_property (dp_proxy, "B", TEST_DP_IFACE_B, 11981241, FALSE);
  test_dp_property (dp_proxy, "A", TEST_DP_IFACE_A, 235235, FALSE);
  test_dp_property (dp_proxy, "B", TEST_DP_IFACE_B, 11981241, TRUE);

  /* And test that setting B does not change A */
  test_dp_property (dp_proxy, "A", TEST_DP_IFACE_A, 235235, FALSE);
  test_dp_property (dp_proxy, "B", TEST_DP_IFACE_B, 11981241, FALSE);
  test_dp_property (dp_proxy, "A", TEST_DP_IFACE_A, 235235, TRUE);

  g_object_unref (dp_proxy);

  /* Ensure the properties are introspectable */
  dp_proxy = dbus_g_proxy_new_for_name (connection, TEST_NAMESPACE, TEST_DP_OBJECT_PATH,
                                        "org.freedesktop.DBus.Introspectable");

  g_print ("Testing duplicate property name introspection\n");
  if (!dbus_g_proxy_call (dp_proxy, "Introspect", &error,
                          G_TYPE_INVALID,
                          G_TYPE_STRING, &str,
                          G_TYPE_INVALID))
    {
      g_print ("Error while introspecting duplicate properties: %s\n", error->message);
      g_error_free (error);
      exit(1);
    }
  else
      g_print ("Introspected duplicate properties with success\n");

  {
    NodeInfo *node;
    GSList *elt;
    gboolean found_introspectable = FALSE;
    gboolean found_properties = FALSE;
    gboolean found_iface_a = FALSE;
    gboolean found_iface_a_prop = FALSE;
    gboolean found_iface_b = FALSE;
    gboolean found_iface_b_prop = FALSE;

    node = description_load_from_string (str, strlen (str), &error);
    if (!node)
      {
        g_print ("Failed to parse introspection data: %s\n", error->message);
        g_error_free (error);
        exit(1);
      }

    for (elt = node_info_get_interfaces (node); elt ; elt = elt->next)
      {
        InterfaceInfo *iface = elt->data;

        if (!found_introspectable && strcmp (interface_info_get_name (iface), "org.freedesktop.DBus.Introspectable") == 0)
          found_introspectable = TRUE;
        else if (!found_properties && strcmp (interface_info_get_name (iface), "org.freedesktop.DBus.Properties") == 0)
          found_properties = TRUE;
        else if (!found_iface_a && strcmp (interface_info_get_name (iface), "org.freedesktop.DBus.GLib.Test.Interfaces.A") == 0)
          {
            GSList *elt;

            found_iface_a = TRUE;

            for (elt = interface_info_get_properties (iface); elt; elt = elt->next)
              {
                PropertyInfo *prop;

                prop = elt->data;
                if (strcmp (property_info_get_name (prop), "Foobar") == 0)
                  {
                    found_iface_a_prop = TRUE;
                    break;
                  }
              }
          }
        else if (!found_iface_b && strcmp (interface_info_get_name (iface), "org.freedesktop.DBus.GLib.Test.Interfaces.B") == 0)
          {
            GSList *elt;

            found_iface_b = TRUE;

            for (elt = interface_info_get_properties (iface); elt; elt = elt->next)
              {
                PropertyInfo *prop;

                prop = elt->data;
                if (strcmp (property_info_get_name (prop), "Foobar") == 0)
                  {
                    found_iface_b_prop = TRUE;
                    break;
                  }
              }
          }
      }
    g_free (str);

    if (!found_iface_a_prop || !found_iface_b_prop)
      {
        g_print ("Failed to find Foobar properties in introspection data\n");
        g_error_free (error);
        exit(1);
      }
  }

  exit(0);
}

/* ex:ts=2:et: */

