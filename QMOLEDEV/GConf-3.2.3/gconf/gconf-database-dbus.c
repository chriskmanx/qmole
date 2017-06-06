/* -*- mode: C; c-file-style: "gnu" -*- */
/* GConf
 * Copyright (C) 2003, 2004 Imendio HB
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
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include <string.h>
#include "gconfd.h"
#include "gconf-dbus-utils.h"
#include "gconfd-dbus.h"
#include "gconf-database-dbus.h"

#define DATABASE_OBJECT_PATH "/org/gnome/GConf/Database"

static gint object_nr = 0;

typedef struct {
  char  *namespace_section;
  GList *clients;
} NotificationData;

typedef struct {
  gchar *service;
  gint nr_of_notifications;
} ListeningClientData;

static void              database_unregistered_func         (DBusConnection   *connection,
							     GConfDatabase    *db);
static DBusHandlerResult database_message_func              (DBusConnection   *connection,
							     DBusMessage      *message,
							     GConfDatabase    *db);
static DBusHandlerResult database_filter_func               (DBusConnection   *connection,
							     DBusMessage      *message,
							     GConfDatabase    *db);
static DBusHandlerResult database_handle_name_owner_changed (DBusConnection   *connection,
							     DBusMessage      *message,
							     GConfDatabase    *db);

static void     database_handle_lookup            (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_lookup_ext        (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_lookup_default    (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_set               (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_unset             (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_recursive_unset   (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_dir_exists        (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_get_all_entries   (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_get_all_dirs      (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_set_schema        (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_suggest_sync      (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static void     database_handle_add_notify        (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);
static gboolean database_remove_notification_data (GConfDatabase    *db,
						   NotificationData *notification,
						   const char       *client);
static void     database_handle_remove_notify     (DBusConnection   *conn,
						   DBusMessage      *message,
						   GConfDatabase    *db);

static ListeningClientData *database_add_listening_client      (GConfDatabase       *db,
								const gchar         *service);
static void                 database_remove_listening_client   (GConfDatabase       *db,
								ListeningClientData *client);


static DBusObjectPathVTable database_vtable = {
  (DBusObjectPathUnregisterFunction) database_unregistered_func,
  (DBusObjectPathMessageFunction)    database_message_func,
  NULL,
};
 
static void
database_unregistered_func (DBusConnection *connection, GConfDatabase *db)
{
}

static DBusHandlerResult
database_message_func (DBusConnection *connection,
                       DBusMessage    *message,
                       GConfDatabase  *db)
{
  if (gconfd_dbus_check_in_shutdown (connection, message))
    return DBUS_HANDLER_RESULT_HANDLED;

  if (dbus_message_is_method_call (message,
				   GCONF_DBUS_DATABASE_INTERFACE,
				   GCONF_DBUS_DATABASE_LOOKUP)) {
    database_handle_lookup (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_LOOKUP_EXTENDED)) {
    database_handle_lookup_ext (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_LOOKUP_DEFAULT)) {
    database_handle_lookup_default (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_SET)) {
    database_handle_set (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_UNSET)) {
    database_handle_unset (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_RECURSIVE_UNSET)) {
    database_handle_recursive_unset (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_DIR_EXISTS)) {
    database_handle_dir_exists (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_GET_ALL_ENTRIES)) {
    database_handle_get_all_entries (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_GET_ALL_DIRS)) {
    database_handle_get_all_dirs (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_SET_SCHEMA)) {
    database_handle_set_schema (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_SUGGEST_SYNC)) {
    database_handle_suggest_sync (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_ADD_NOTIFY)) {
	  database_handle_add_notify (connection, message, db);
  }
  else if (dbus_message_is_method_call (message,
					GCONF_DBUS_DATABASE_INTERFACE,
					GCONF_DBUS_DATABASE_REMOVE_NOTIFY)) {
	  database_handle_remove_notify (connection, message, db);
  } else {
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
  }

  return DBUS_HANDLER_RESULT_HANDLED;
}

static void
get_all_notifications_func (gpointer key,
			    gpointer value,
			    gpointer user_data)
{
  GList **list = user_data;
  
  *list = g_list_prepend (*list, value);
}

static DBusHandlerResult
database_filter_func (DBusConnection *connection,
		      DBusMessage    *message,
		      GConfDatabase  *db)
{
#if 0
  /* Debug output. */
  if (dbus_message_get_member (message)) {
    g_print ("Message: %s\n", dbus_message_get_member (message));
  }
#endif
  
  if (dbus_message_is_signal (message,
			      DBUS_INTERFACE_DBUS,
                              "NameOwnerChanged"))
    return database_handle_name_owner_changed (connection, message, db);

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusHandlerResult
database_handle_name_owner_changed (DBusConnection *connection,
				    DBusMessage    *message,
				    GConfDatabase  *db)
{  
  gchar               *service;
  gchar               *old_owner;
  gchar               *new_owner;
  GList               *notifications = NULL, *l;
  NotificationData    *notification;
  ListeningClientData *client;
  
  dbus_message_get_args (message,
			 NULL,
			 DBUS_TYPE_STRING, &service,
			 DBUS_TYPE_STRING, &old_owner,
			 DBUS_TYPE_STRING, &new_owner,
			 DBUS_TYPE_INVALID);
  
  if (strcmp (new_owner, "") != 0) 
    {
      /* Service still exist, don't remove notifications */
      return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }

  g_hash_table_foreach (db->notifications, get_all_notifications_func,
			&notifications);
  
  /* Note: This might be a bit too slow to do like this. We could add a hash
   * table that maps client base service names to notification data, instead of
   * going through the entire list of notifications and clients.
   */
  for (l = notifications; l; l = l->next)
    {
      notification = l->data;
      
      database_remove_notification_data (db, notification, service);
    }
 
  client = g_hash_table_lookup (db->listening_clients, service);
  if (client)
    database_remove_listening_client (db, client);

  g_list_free (notifications);

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}
    
static void
database_handle_lookup (DBusConnection *conn,
                        DBusMessage    *message,
                        GConfDatabase  *db)
{
  GConfValue *value;
  DBusMessage *reply;
  gchar *key;
  gchar *locale;
  GConfLocaleList *locales;
  gboolean use_schema_default;
  GError *gerror = NULL;
  DBusMessageIter iter;
  
  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &key,
				     DBUS_TYPE_STRING, &locale,
				     DBUS_TYPE_BOOLEAN, &use_schema_default,
				     DBUS_TYPE_INVALID))
    return;
  
  locales = gconfd_locale_cache_lookup (locale);
  
  value = gconf_database_query_value (db, key, locales->list, 
				      use_schema_default,
				      NULL, NULL, NULL, &gerror);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    goto fail;

  reply = dbus_message_new_method_return (message);

  dbus_message_iter_init_append (reply, &iter);
  gconf_dbus_utils_append_value (&iter, value);

  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
  
 fail:
  if (value)
    gconf_value_free (value);
}

static void 
database_handle_lookup_ext (DBusConnection *conn,
			    DBusMessage    *message,
			    GConfDatabase  *db)
{
  GConfValue *value;
  gchar *schema_name = NULL;
  gboolean value_is_default;
  gboolean value_is_writable;
  DBusMessage *reply;
  gchar *key;
  gchar *locale;
  GConfLocaleList *locales;
  gboolean use_schema_default;
  GError *gerror = NULL;
  DBusMessageIter iter;
  
  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &key,
				     DBUS_TYPE_STRING, &locale,
				     DBUS_TYPE_BOOLEAN, &use_schema_default,
				     DBUS_TYPE_INVALID))
    return;
  
  locales = gconfd_locale_cache_lookup (locale);
  
  value = gconf_database_query_value (db, key, locales->list,
				      use_schema_default,
				      &schema_name, &value_is_default, 
				      &value_is_writable, &gerror);
  
  if (gconfd_dbus_set_exception (conn, message, &gerror))
    goto fail;
  
  reply = dbus_message_new_method_return (message);

  dbus_message_iter_init_append (reply, &iter);

  gconf_dbus_utils_append_entry_values (&iter,
					  key,
					  value,
					  value_is_default,
					  value_is_writable,
					  schema_name);
  
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);

 fail:
  g_free (schema_name);

  if (value)
    gconf_value_free (value);
}

static void 
database_handle_lookup_default (DBusConnection *conn,
				DBusMessage    *message,
				GConfDatabase  *db)
{
  GConfValue *value;
  DBusMessage *reply;
  gchar *key;
  gchar *locale;
  GConfLocaleList *locales;
  GError *gerror = NULL;
  DBusMessageIter iter;
  
  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &key,
				     DBUS_TYPE_STRING, &locale,
				     DBUS_TYPE_INVALID))
    return;

  locales = gconfd_locale_cache_lookup (locale);

  value = gconf_database_query_default_value (db, key, locales->list,
					      NULL,
					      &gerror);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    goto fail;
  
  reply = dbus_message_new_method_return (message);

  dbus_message_iter_init_append (reply, &iter);

  if (value)
    gconf_dbus_utils_append_value (&iter, value);
  
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);

 fail:
  if (value)
    gconf_value_free (value);
}

static void
database_handle_set (DBusConnection *conn,
                     DBusMessage    *message,
                     GConfDatabase  *db)
{
  gchar *key;
  GConfValue *value = NULL; 
  GError *gerror = NULL;
  DBusMessage *reply;
  DBusMessageIter iter;

  dbus_message_iter_init (message, &iter);
  dbus_message_iter_get_basic (&iter, &key);

  dbus_message_iter_next (&iter);
  value = gconf_dbus_utils_get_value (&iter);

  gconf_database_set (db, key, value, &gerror);
  gconf_value_free (value);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;

  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}

static void
database_handle_unset (DBusConnection *conn,
		       DBusMessage    *message,
		       GConfDatabase  *db)
{
  gchar *key;
  gchar *locale;
  GError *gerror = NULL;
  DBusMessage *reply;

  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &key,
				     DBUS_TYPE_STRING, &locale,
				     DBUS_TYPE_INVALID))
    return;

  if (locale[0] == '\0')
    {
      locale = NULL;
    }
  
  gconf_database_unset (db, key, locale, &gerror);
  
  gconf_database_sync (db, NULL);
  
  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;
 
  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);

}
                                                                               
static void
database_handle_recursive_unset  (DBusConnection *conn,
                                  DBusMessage    *message,
                                  GConfDatabase  *db)
{
  gchar       *key;
  gchar       *locale;
  GError      *gerror = NULL;
  guint32      unset_flags;
  DBusMessage *reply;
  
  if (!gconfd_dbus_get_message_args (conn, message, 
				     DBUS_TYPE_STRING, &key,
				     DBUS_TYPE_STRING, &locale,
				     DBUS_TYPE_UINT32, &unset_flags,
				     DBUS_TYPE_INVALID))
    return;

  if (locale[0] == '\0')
    {
      locale = NULL;
    }
  
  gconf_database_recursive_unset (db, key, locale, unset_flags, &gerror);
  
  gconf_database_sync (db, NULL);
  
  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;

  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}
                                                                                
static void
database_handle_dir_exists (DBusConnection *conn,
                            DBusMessage    *message,
                            GConfDatabase  *db)
{
  gboolean     exists;
  gchar       *dir;
  GError      *gerror = NULL;
  DBusMessage *reply;
 
  if (!gconfd_dbus_get_message_args (conn, message, 
				     DBUS_TYPE_STRING, &dir,
				     DBUS_TYPE_INVALID))
    return;

  exists = gconf_database_dir_exists (db, dir, &gerror);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;

  reply = dbus_message_new_method_return (message);
  dbus_message_append_args (reply,
			    DBUS_TYPE_BOOLEAN, &exists,
			    DBUS_TYPE_INVALID);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}
                                                                                
static void
database_handle_get_all_entries (DBusConnection *conn,
                                 DBusMessage    *message,
                                 GConfDatabase  *db)
{
  GSList *entries, *l;
  gchar  *dir;
  gchar  *locale;
  GError *gerror = NULL;
  GConfLocaleList* locales;
  DBusMessage *reply;
  DBusMessageIter iter;

  if (!gconfd_dbus_get_message_args (conn, message, 
				     DBUS_TYPE_STRING, &dir,
				     DBUS_TYPE_STRING, &locale,
				     DBUS_TYPE_INVALID)) 
    return;

  locales = gconfd_locale_cache_lookup (locale);

  entries = gconf_database_all_entries (db, dir, 
					locales->list, &gerror);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;

  reply = dbus_message_new_method_return (message);

  dbus_message_iter_init_append (reply, &iter);

  gconf_dbus_utils_append_entries (&iter, entries);

  for (l = entries; l; l = l->next)
    {
      GConfEntry *entry = l->data;
      
      gconf_entry_free (entry);
    }
  
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);

  g_slist_free (entries);
}
                                                                                
static void
database_handle_get_all_dirs (DBusConnection *conn,
                              DBusMessage    *message,
                              GConfDatabase  *db)
{
  GSList          *dirs, *l;
  gchar           *dir;
  GError          *gerror = NULL;
  DBusMessage     *reply;
  DBusMessageIter  iter;
  DBusMessageIter  array_iter;

  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &dir,
				     DBUS_TYPE_INVALID)) 
    return;

  dirs = gconf_database_all_dirs (db, dir, &gerror);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;
  
  reply = dbus_message_new_method_return (message);

  dbus_message_iter_init_append (reply, &iter);

  dbus_message_iter_open_container (&iter,
				    DBUS_TYPE_ARRAY,
				    DBUS_TYPE_STRING_AS_STRING,
				    &array_iter);
  
  for (l = dirs; l; l = l->next) 
    {
      gchar *str = (gchar *) l->data;

      dbus_message_iter_append_basic (&array_iter,
				      DBUS_TYPE_STRING,
				      &str);
      
      g_free (l->data);
    }

  dbus_message_iter_close_container (&iter, &array_iter);
 
  g_slist_free (dirs);
  
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}
                                                                                
static void
database_handle_set_schema (DBusConnection *conn,
                            DBusMessage    *message,
                            GConfDatabase  *db)
{
  gchar       *key;
  gchar       *schema_key;
  GError      *gerror = NULL;
  DBusMessage *reply;

  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &key,
				     DBUS_TYPE_STRING, &schema_key,
				     DBUS_TYPE_INVALID)) 
    return;

  /* Empty schema key name means unset. */ 
  if (schema_key[0] == '\0')
    schema_key = NULL;
  
  gconf_database_set_schema (db, key, schema_key, &gerror);

  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;

  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}

static void
database_handle_suggest_sync (DBusConnection *conn,
		              DBusMessage    *message,
		              GConfDatabase  *db)
{
  GError *gerror = NULL;
  DBusMessage *reply;
  
  gconf_database_sync (db, &gerror);
  
  if (gconfd_dbus_set_exception (conn, message, &gerror))
    return;

  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}

static void
database_handle_add_notify (DBusConnection    *conn,
                            DBusMessage       *message,
                            GConfDatabase *db)
{
  gchar *namespace_section;
  DBusMessage *reply;
  const char *sender;
  NotificationData *notification;
  ListeningClientData *client;

  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &namespace_section,
				     DBUS_TYPE_INVALID)) 
    return;

  sender = dbus_message_get_sender (message);
  
  client = g_hash_table_lookup (db->listening_clients, sender);
  if (!client)
    {
      client = database_add_listening_client (db, sender);
    }
  else
    {
      client->nr_of_notifications++;
    }
  
  notification = g_hash_table_lookup (db->notifications, namespace_section);
  
  if (notification == NULL)
    {
      notification = g_new0 (NotificationData, 1);
      notification->namespace_section = g_strdup (namespace_section);

      g_hash_table_insert (db->notifications,
			   notification->namespace_section, notification);
    }
  
  notification->clients = g_list_prepend (notification->clients,
					  g_strdup (sender));
  
  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}

static gboolean
database_remove_notification_data (GConfDatabase *db,
				   NotificationData *notification,
				   const char *client)
{
  GList *element;
  
  element = g_list_find_custom (notification->clients, client,
			     (GCompareFunc)strcmp);
  if (element == NULL)
    return FALSE;
  
  notification->clients = g_list_remove_link (notification->clients, element);
  if (notification->clients == NULL)
    {
      g_hash_table_remove (db->notifications,
			   notification->namespace_section);

      g_free (notification->namespace_section);
      g_free (notification);
    }
  
  g_free (element->data);
  g_list_free_1 (element);

  return TRUE;
}

static void
database_handle_remove_notify (DBusConnection    *conn,
			       DBusMessage       *message,
			       GConfDatabase *db)
{
  gchar *namespace_section;
  DBusMessage *reply;
  const char *sender;
  NotificationData *notification;
  ListeningClientData *client;
  
  if (!gconfd_dbus_get_message_args (conn, message,
				     DBUS_TYPE_STRING, &namespace_section,
				     DBUS_TYPE_INVALID)) 
    return;

  sender = dbus_message_get_sender (message);
  
  notification = g_hash_table_lookup (db->notifications, namespace_section);

  client = g_hash_table_lookup (db->listening_clients, sender);
  if (client) {
    client->nr_of_notifications--;
    
    if (client->nr_of_notifications == 0) {
      database_remove_listening_client (db, client);
    }
  }
    
  /* Notification can be NULL if the client and server get out of sync. */
  if (notification == NULL || !database_remove_notification_data (db, notification, sender))
    {
      gconf_log (GCL_DEBUG, _("Notification on %s doesn't exist"),
                 namespace_section);
    }
  
  reply = dbus_message_new_method_return (message);
  dbus_connection_send (conn, reply, NULL);
  dbus_message_unref (reply);
}

static gchar *
get_rule_for_service (const gchar *service)
{
  gchar *rule;
  
  rule = g_strdup_printf ("type='signal',member='NameOwnerChanged',arg0='%s'", service);

  return rule;
}

static ListeningClientData *
database_add_listening_client (GConfDatabase *db, 
			       const gchar   *service)
{
  ListeningClientData *client;
  gchar               *rule;

  client = g_new0 (ListeningClientData, 1);
  client->service = g_strdup (service);
  client->nr_of_notifications = 1;

  g_hash_table_insert (db->listening_clients, client->service, client);
  
  rule = get_rule_for_service (service);
  dbus_bus_add_match (gconfd_dbus_get_connection (), rule, NULL);
  g_free (rule);

  return client;
}

static void
database_remove_listening_client (GConfDatabase       *db,
				  ListeningClientData *client)
{
  gchar *rule;

  rule = get_rule_for_service (client->service);
  dbus_bus_remove_match (gconfd_dbus_get_connection (), rule, NULL);
  g_free (rule);

  g_hash_table_remove (db->listening_clients, client->service);
  g_free (client->service);
  g_free (client);
}

void
gconf_database_dbus_setup (GConfDatabase *db)
{
  DBusConnection *conn;
  
  g_assert (db->object_path == NULL);
  
  db->object_path = g_strdup_printf ("%s/%d", 
				     DATABASE_OBJECT_PATH, 
				     object_nr++);

  conn = gconfd_dbus_get_connection ();
  
  dbus_connection_register_object_path (conn,
					db->object_path,
					&database_vtable,
					db);

  db->notifications = g_hash_table_new (g_str_hash, g_str_equal);
  db->listening_clients = g_hash_table_new (g_str_hash, g_str_equal);
 
  dbus_connection_add_filter (conn,
			      (DBusHandleMessageFunction)database_filter_func,
			      db,
			      NULL);
}

void
gconf_database_dbus_teardown (GConfDatabase *db)
{
  DBusConnection *conn;

  conn = gconfd_dbus_get_connection ();

  gconfd_emit_db_gone (db->object_path);
  dbus_connection_unregister_object_path (conn, db->object_path);
  
  dbus_connection_remove_filter (conn,
				 (DBusHandleMessageFunction)database_filter_func,
				 db);
  g_free (db->object_path);
  db->object_path = NULL;

  g_hash_table_destroy (db->notifications);
  db->notifications = NULL;

  g_hash_table_destroy (db->listening_clients);
  db->listening_clients = NULL;
}

const char *
gconf_database_dbus_get_path (GConfDatabase *db)
{
  return db->object_path;
}

void
gconf_database_dbus_notify_listeners (GConfDatabase    *db,
				      GConfSources     *modified_sources,
				      const gchar      *key,
				      const GConfValue *value,
				      gboolean          is_default,
				      gboolean          is_writable,
				      gboolean          notify_others)
{
  char             *dir, *sep;
  GList            *l;
  NotificationData *notification;
  DBusMessage      *message;
  gboolean          last;
  
  dir = g_strdup (key);

  /* Lookup the key in the namespace hierarchy, start with the full key and then
   * remove the leaf, lookup again, remove the leaf, and so on until a match is
   * found. Notify the clients (identified by their base service) that
   * correspond to the found namespace.
   */
  last = FALSE;
  while (1)
    {
      notification = g_hash_table_lookup (db->notifications, dir);

      if (notification)
	{
	  for (l = notification->clients; l; l = l->next)
	    {
	      const char *base_service = l->data;
	      DBusMessageIter iter;
	      
	      message = dbus_message_new_method_call (base_service,
						      GCONF_DBUS_CLIENT_OBJECT,
						      GCONF_DBUS_CLIENT_INTERFACE,
						      "Notify");

	      dbus_message_append_args (message,
					DBUS_TYPE_OBJECT_PATH, &db->object_path,
					DBUS_TYPE_STRING, &dir,
					DBUS_TYPE_INVALID);

	      dbus_message_iter_init_append (message, &iter);
	      
	      gconf_dbus_utils_append_entry_values (&iter,
						    key,
						    value,
						    is_default,
						    is_writable,
						    NULL);
	      
	      dbus_message_set_no_reply (message, TRUE);
	      
	      dbus_connection_send (gconfd_dbus_get_connection (), message, NULL);
	      dbus_message_unref (message);
	    }
	}

      if (last)
	break;
      
      sep = strrchr (dir, '/');

      /* Special case to catch notifications on the root. */
      if (sep == dir)
	{
	  last = TRUE;
	  sep[1] = '\0';
	}
      else
	*sep = '\0';
    }

    
  g_free (dir);

  if (modified_sources)
    {
      if (notify_others)
        gconfd_notify_other_listeners (db, modified_sources, key);

      g_list_free (modified_sources->sources);
      g_free (modified_sources);
    }
}
