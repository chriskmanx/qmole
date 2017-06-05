/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gproxy.c Proxy for remote objects
 *
 * Copyright (C) 2003, 2004, 2005 Red Hat, Inc.
 * Copyright (C) 2005 Nokia
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <config.h>

#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>
#include <dbus/dbus-signature.h>
#include "dbus-gutils.h"
#include "dbus-gsignature.h"
#include "dbus-gvalue.h"
#include "dbus-gvalue-utils.h"
#include "dbus-gobject.h"
#include <string.h>
#include <gobject/gvaluecollector.h>
#include <gio/gio.h>

#define DBUS_G_PROXY_CALL_TO_ID(x) (GPOINTER_TO_UINT(x))
#define DBUS_G_PROXY_ID_TO_CALL(x) (GUINT_TO_POINTER(x))
#define DBUS_G_PROXY_GET_PRIVATE(o)  \
       (G_TYPE_INSTANCE_GET_PRIVATE ((o), DBUS_TYPE_G_PROXY, DBusGProxyPrivate))

static void oom (void) G_GNUC_NORETURN;
static void
oom (void)
{
  g_error ("no memory");
}

typedef struct _DBusGProxyManager DBusGProxyManager;

typedef struct _DBusGProxyPrivate DBusGProxyPrivate;

struct _DBusGProxyPrivate
{
  DBusGProxyManager *manager; /**< Proxy manager */
  char *name;                 /**< Name messages go to or NULL */
  char *path;                 /**< Path messages go to or NULL */
  char *interface;            /**< Interface messages go to or NULL */

  DBusGProxyCall *name_call;  /**< Pending call id to retrieve name owner */
  guint for_owner : 1;        /**< Whether or not this proxy is for a name owner */
  guint associated : 1;       /**< Whether or not this proxy is associated (for name proxies) */

  /* FIXME: make threadsafe? */
  guint call_id_counter;      /**< Integer counter for pending calls */

  GData *signal_signatures;   /**< D-BUS signatures for each signal */

  GHashTable *pending_calls;  /**< Calls made on this proxy which have not yet returned */

  int default_timeout; /**< Default timeout to use, see dbus_g_proxy_set_default_timeout */
};

static void dbus_g_proxy_init               (DBusGProxy      *proxy);
static void dbus_g_proxy_class_init         (DBusGProxyClass *klass);
static GObject *dbus_g_proxy_constructor    (GType                  type,
					     guint                  n_construct_properties,
					     GObjectConstructParam *construct_properties);
static void     dbus_g_proxy_set_property       (GObject               *object,
						 guint                  prop_id,
						 const GValue          *value,
						 GParamSpec            *pspec);
static void     dbus_g_proxy_get_property       (GObject               *object,
						 guint                  prop_id,
						 GValue                *value,
						 GParamSpec            *pspec);

static void dbus_g_proxy_finalize           (GObject         *object);
static void dbus_g_proxy_dispose            (GObject         *object);
static void dbus_g_proxy_destroy            (DBusGProxy      *proxy);
static void dbus_g_proxy_emit_remote_signal (DBusGProxy      *proxy,
                                             DBusMessage     *message);

static DBusGProxyCall *manager_begin_bus_call (DBusGProxyManager    *manager,
					       const char          *method,
					       DBusGProxyCallNotify notify,
					       gpointer             data,
					       GDestroyNotify       destroy,
					       GType                first_arg_type,
					       ...);
static guint dbus_g_proxy_begin_call_internal (DBusGProxy          *proxy,
					       const char          *method,
					       DBusGProxyCallNotify notify,
					       gpointer             data,
					       GDestroyNotify       destroy,
					       GValueArray         *args,
					       int timeout );
static gboolean dbus_g_proxy_end_call_internal (DBusGProxy        *proxy,
						guint              call_id,
						GError           **error,
						GType              first_arg_type,
						va_list            args);

/*
 * A list of proxies with a given name+path+interface, used to
 * route incoming signals.
 */
typedef struct
{
  GSList *proxies; /**< The list of proxies */

  char name[4]; /**< name (empty string for none), nul byte,
                 *   path, nul byte,
                 *   interface, nul byte
                 */
  
} DBusGProxyList;

/*
 * DBusGProxyManager's primary task is to route signals to the proxies
 * those signals are emitted on. In order to do this it also has to
 * track the owners of the names proxies are bound to.
 */
struct _DBusGProxyManager
{
  GStaticMutex lock; /**< Thread lock */
  int refcount;      /**< Reference count */
  DBusConnection *connection; /**< Connection we're associated with. */

  DBusGProxy *bus_proxy; /**< Special internal proxy used to talk to the bus */

  GHashTable *proxy_lists; /**< Hash used to route incoming signals
                            *   and iterate over proxies
                            *   tristring -> DBusGProxyList
                            */
  GHashTable *owner_match_rules; /**< Hash to keep track of match rules of
                                  * NameOwnerChanged.
                                  * gchar *name -> guint *refcount
                            */
  GHashTable *owner_names; /**< Hash to keep track of mapping from
                            *   char *    -> GSList of DBusGProxyNameOwnerInfo
			    *   base name -> [name,name,...] for proxies which
			    *   are for names.
			    */
  GSList *unassociated_proxies;     /**< List of name proxies for which
				     *   there was no result for
				     *   GetNameOwner
				     */
};

static DBusGProxyManager *dbus_g_proxy_manager_ref    (DBusGProxyManager *manager);
static DBusHandlerResult  dbus_g_proxy_manager_filter (DBusConnection    *connection,
                                                       DBusMessage       *message,
                                                       void              *user_data);


/** Lock the DBusGProxyManager */
#define LOCK_MANAGER(mgr)   (g_static_mutex_lock (&(mgr)->lock))
/** Unlock the DBusGProxyManager */
#define UNLOCK_MANAGER(mgr) (g_static_mutex_unlock (&(mgr)->lock))

static int g_proxy_manager_slot = -1;

/* Lock controlling get/set manager as data on each connection */
static GStaticMutex connection_g_proxy_lock = G_STATIC_MUTEX_INIT;

static DBusGProxyManager*
dbus_g_proxy_manager_get (DBusConnection *connection)
{
  DBusGProxyManager *manager;

  dbus_connection_allocate_data_slot (&g_proxy_manager_slot);
  if (g_proxy_manager_slot < 0)
    g_error ("out of memory");
  
  g_static_mutex_lock (&connection_g_proxy_lock);
  
  manager = dbus_connection_get_data (connection, g_proxy_manager_slot);
  if (manager != NULL)
    {
      dbus_connection_free_data_slot (&g_proxy_manager_slot);
      dbus_g_proxy_manager_ref (manager);
      g_static_mutex_unlock (&connection_g_proxy_lock);
      return manager;
    }
  
  manager = g_new0 (DBusGProxyManager, 1);

  manager->refcount = 1;
  manager->connection = connection;

  g_static_mutex_init (&manager->lock);

  /* Proxy managers keep the connection alive, which means that
   * DBusGProxy indirectly does. To free a connection you have to free
   * all the proxies referring to it.
   */
  dbus_connection_ref (manager->connection);

  dbus_connection_set_data (connection, g_proxy_manager_slot,
                            manager, NULL);

  dbus_connection_add_filter (connection, dbus_g_proxy_manager_filter,
                              manager, NULL);
  
  g_static_mutex_unlock (&connection_g_proxy_lock);
  
  return manager;
}

static DBusGProxyManager * 
dbus_g_proxy_manager_ref (DBusGProxyManager *manager)
{
  g_assert (manager != NULL);
  g_assert (manager->refcount > 0);

  LOCK_MANAGER (manager);
  
  manager->refcount += 1;

  UNLOCK_MANAGER (manager);

  return manager;
}

static void
dbus_g_proxy_manager_unref (DBusGProxyManager *manager)
{
  g_assert (manager != NULL);
  g_assert (manager->refcount > 0);

  LOCK_MANAGER (manager);
  manager->refcount -= 1;
  if (manager->refcount == 0)
    {
      UNLOCK_MANAGER (manager);

      if (manager->bus_proxy)
	g_object_unref (manager->bus_proxy);

      if (manager->proxy_lists)
        {
          /* can't have any proxies left since they hold
           * a reference to the proxy manager.
           */
          g_assert (g_hash_table_size (manager->proxy_lists) == 0);
          
          g_hash_table_destroy (manager->proxy_lists);
          manager->proxy_lists = NULL;

        }

      if (manager->owner_match_rules)
        {
	  /* Since we destroyed all proxies, none can be tracking
	   * name owners
	   */
          g_assert (g_hash_table_size (manager->owner_match_rules) == 0);
          g_hash_table_destroy (manager->owner_match_rules);
          manager->owner_match_rules = NULL;
        }

      if (manager->owner_names)
	{
	  /* Since we destroyed all proxies, none can be tracking
	   * name owners
	   */
          g_assert (g_hash_table_size (manager->owner_names) == 0);

	  g_hash_table_destroy (manager->owner_names);
	  manager->owner_names = NULL;
	}

      g_assert (manager->unassociated_proxies == NULL);
      
      g_static_mutex_free (&manager->lock);

      g_static_mutex_lock (&connection_g_proxy_lock);

      dbus_connection_remove_filter (manager->connection, dbus_g_proxy_manager_filter,
                                     manager);
      
      dbus_connection_set_data (manager->connection,
                                g_proxy_manager_slot,
                                NULL, NULL);

      g_static_mutex_unlock (&connection_g_proxy_lock);
      
      dbus_connection_unref (manager->connection);
      g_free (manager);

      dbus_connection_free_data_slot (&g_proxy_manager_slot);
    }
  else
    {
      UNLOCK_MANAGER (manager);
    }
}

static guint
tristring_hash (gconstpointer key)
{
  const char *p = key;
  guint h = *p;

  if (h)
    {
      for (p += 1; *p != '\0'; p++)
        h = (h << 5) - h + *p;
    }

  /* skip nul and do the next substring */
  for (p += 1; *p != '\0'; p++)
    h = (h << 5) - h + *p;

  /* skip nul again and another substring */
  for (p += 1; *p != '\0'; p++)
    h = (h << 5) - h + *p;
  
  return h;
}

static gboolean
strequal_len (const char *a,
              const char *b,
              size_t     *lenp)
{
  size_t a_len;
  size_t b_len;

  a_len = strlen (a);
  b_len = strlen (b);

  if (a_len != b_len)
    return FALSE;

  if (memcmp (a, b, a_len) != 0)
    return FALSE;
  
  *lenp = a_len;

  return TRUE;
}

static gboolean
tristring_equal (gconstpointer  a,
                 gconstpointer  b)
{
  const char *ap = a;
  const char *bp = b;
  size_t len;

  if (!strequal_len (ap, bp, &len))
    return FALSE;

  ap += len + 1;
  bp += len + 1;

  if (!strequal_len (ap, bp, &len))
    return FALSE;

  ap += len + 1;
  bp += len + 1;

  if (strcmp (ap, bp) != 0)
    return FALSE;
  
  return TRUE;
}

static char*
tristring_alloc_from_strings (size_t      padding_before,
                              const char *name,
                              const char *path,
                              const char *interface)
{
  size_t name_len, iface_len, path_len, len;
  char *tri;
  
  if (name)
    name_len = strlen (name);
  else
    name_len = 0;

  path_len = strlen (path);
  
  iface_len = strlen (interface);

  tri = g_malloc (padding_before + name_len + path_len + iface_len + 3);

  len = padding_before;
  
  if (name)
    memcpy (&tri[len], name, name_len);

  len += name_len;
  tri[len] = '\0';
  len += 1;

  g_assert (len == (padding_before + name_len + 1));
  
  memcpy (&tri[len], path, path_len);
  len += path_len;
  tri[len] = '\0';
  len += 1;

  g_assert (len == (padding_before + name_len + path_len + 2));
  
  memcpy (&tri[len], interface, iface_len);
  len += iface_len;
  tri[len] = '\0';
  len += 1;

  g_assert (len == (padding_before + name_len + path_len + iface_len + 3));

  return tri;
}

static char*
tristring_from_proxy (DBusGProxy *proxy)
{
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  return tristring_alloc_from_strings (0,
                                       priv->name,
                                       priv->path,
                                       priv->interface);
}

static char*
tristring_from_message (DBusMessage *message)
{
  const char *path;
  const char *interface;

  path = dbus_message_get_path (message);
  interface = dbus_message_get_interface (message);

  g_assert (path);
  g_assert (interface);
  
  return tristring_alloc_from_strings (0,
                                       dbus_message_get_sender (message),
                                       path, interface);
}

static DBusGProxyList*
g_proxy_list_new (DBusGProxy *first_proxy)
{
  DBusGProxyList *list;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(first_proxy);
  
  list = (void*) tristring_alloc_from_strings (G_STRUCT_OFFSET (DBusGProxyList, name),
                                               priv->name,
                                               priv->path,
                                               priv->interface);
  list->proxies = NULL;

  return list;
}

static void
g_proxy_list_free (DBusGProxyList *list)
{
  /* we don't hold a reference to the proxies in the list,
   * as they ref the GProxyManager
   */
  g_slist_free (list->proxies);  

  g_free (list);
}

static char*
g_proxy_get_signal_match_rule (DBusGProxy *proxy)
{
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  /* FIXME Escaping is required here */
  
  if (priv->name)
    return g_strdup_printf ("type='signal',sender='%s',path='%s',interface='%s'",
                            priv->name, priv->path, priv->interface);
  else
    return g_strdup_printf ("type='signal',path='%s',interface='%s'",
                            priv->path, priv->interface);
}

static char *
get_owner_match_rule (const gchar *name)
{
      return g_strdup_printf ("type='signal',sender='" DBUS_SERVICE_DBUS
        "',path='" DBUS_PATH_DBUS
        "',interface='" DBUS_INTERFACE_DBUS
        "',member='NameOwnerChanged'"
        ",arg0='%s'", name);
}

typedef struct
{
  char *name;
  guint refcount;
} DBusGProxyNameOwnerInfo;

static gint
find_name_in_info (gconstpointer a, gconstpointer b)
{
  const DBusGProxyNameOwnerInfo *info = a;
  const char *name = b;

  return strcmp (info->name, name);
}

typedef struct
{
  const char *name;
  const char *owner;
  DBusGProxyNameOwnerInfo *info;
} DBusGProxyNameOwnerForeachData;

static void
name_owner_foreach (gpointer key, gpointer val, gpointer data)
{
  const char *owner;
  DBusGProxyNameOwnerForeachData *foreach_data;
  GSList *names;
  GSList *link;

  owner = key;
  names = val;
  foreach_data = data;

  if (foreach_data->owner != NULL)
    return;

  g_assert (foreach_data->info == NULL);

  link = g_slist_find_custom (names, foreach_data->name, find_name_in_info);
  if (link)
    {
      foreach_data->owner = owner;
      foreach_data->info = link->data;
    }
}

static gboolean
dbus_g_proxy_manager_lookup_name_owner (DBusGProxyManager        *manager,
					const char               *name,
					DBusGProxyNameOwnerInfo **info,
					const char              **owner)
{
  DBusGProxyNameOwnerForeachData foreach_data;

  foreach_data.name = name;
  foreach_data.owner = NULL;
  foreach_data.info = NULL;
  
  g_hash_table_foreach (manager->owner_names, name_owner_foreach, &foreach_data);

  *info = foreach_data.info;
  *owner = foreach_data.owner;
  return *info != NULL;
}

static void
insert_nameinfo (DBusGProxyManager       *manager,
		 const char              *owner,
		 DBusGProxyNameOwnerInfo *info)
{
  GSList *names;
  gboolean insert;

  names = g_hash_table_lookup (manager->owner_names, owner);

  /* Only need to g_hash_table_insert the first time */
  insert = (names == NULL);

  names = g_slist_append (names, info); 

  if (insert)
    g_hash_table_insert (manager->owner_names, g_strdup (owner), names);
}

static void
dbus_g_proxy_manager_monitor_name_owner (DBusGProxyManager  *manager,
					 const char         *owner,
					 const char         *name)
{
  GSList *names;
  GSList *link;
  DBusGProxyNameOwnerInfo *nameinfo;

  names = g_hash_table_lookup (manager->owner_names, owner);
  link = g_slist_find_custom (names, name, find_name_in_info);
  
  if (!link)
    {
      nameinfo = g_new0 (DBusGProxyNameOwnerInfo, 1);
      nameinfo->name = g_strdup (name);
      nameinfo->refcount = 1;

      insert_nameinfo (manager, owner, nameinfo);
    }
  else
    {
      nameinfo = link->data;
      nameinfo->refcount++;
    }
}

static void
dbus_g_proxy_manager_unmonitor_name_owner (DBusGProxyManager  *manager,
					   const char         *name)
{
  DBusGProxyNameOwnerInfo *info;
  const char *owner;
  gboolean ret;

  ret = dbus_g_proxy_manager_lookup_name_owner (manager, name, &info, &owner);
  g_assert (ret);
  g_assert (info != NULL);
  g_assert (owner != NULL);

  info->refcount--;
  if (info->refcount == 0)
    {
      GSList *names;
      GSList *link;

      names = g_hash_table_lookup (manager->owner_names, owner);
      link = g_slist_find_custom (names, name, find_name_in_info);
      names = g_slist_delete_link (names, link);
      if (names != NULL)
	g_hash_table_insert (manager->owner_names, g_strdup (owner), names);
      else
	g_hash_table_remove (manager->owner_names, owner);

      g_free (info->name);
      g_free (info);
    }
}

typedef struct
{
  const char *name;
  GSList *destroyed;
} DBusGProxyUnassociateData;

static void
unassociate_proxies (gpointer key, gpointer val, gpointer user_data)
{
  DBusGProxyList *list;
  const char *name;
  GSList *tmp;
  DBusGProxyUnassociateData *data;

  list = val;
  data = user_data;
  name = data->name;
  
  for (tmp = list->proxies; tmp; tmp = tmp->next)
    {
      DBusGProxy *proxy = DBUS_G_PROXY (tmp->data);
      DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
      DBusGProxyManager *manager;

      manager = priv->manager;

      if (!strcmp (priv->name, name))
	{
	  if (!priv->for_owner)
	    {
	      /* If a service appeared and then vanished very quickly,
	       * it's conceivable we have an inflight request for
	       * GetNameOwner here.  Cancel it.
	       * https://bugs.freedesktop.org/show_bug.cgi?id=18573
	       */
	      if (priv->name_call)
		dbus_g_proxy_cancel_call (manager->bus_proxy, priv->name_call);

	      priv->name_call = NULL;

	      priv->associated = FALSE;
	      manager->unassociated_proxies = g_slist_prepend (manager->unassociated_proxies, proxy);
	    }
	  else
	    {
	      data->destroyed = g_slist_prepend (data->destroyed, proxy);
              /* make contents of list into weak pointers in case the objects
               * unref each other when disposing */
              g_object_add_weak_pointer (G_OBJECT (proxy),
                  &(data->destroyed->data));
	    }
	}
    }
}

static void
dbus_g_proxy_manager_replace_name_owner (DBusGProxyManager  *manager,
					 const char         *name,
					 const char         *prev_owner,
					 const char         *new_owner)
{
  GSList *names;
	  
  if (prev_owner[0] == '\0')
    {
      GSList *tmp;
      GSList *removed;

      /* We have a new service, look at unassociated proxies */

      removed = NULL;

      for (tmp = manager->unassociated_proxies; tmp ; tmp = tmp->next)
	{
	  DBusGProxy *proxy = tmp->data;
	  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

	  if (!strcmp (priv->name, name))
	    {
	      removed = g_slist_prepend (removed, tmp);
	      
	      dbus_g_proxy_manager_monitor_name_owner (manager, new_owner, name);
	      priv->associated = TRUE;
	    }
	}

      for (tmp = removed; tmp; tmp = tmp->next)
	manager->unassociated_proxies = g_slist_delete_link (manager->unassociated_proxies, tmp->data);
      g_slist_free (removed);
    }
  else
    {
      DBusGProxyNameOwnerInfo *info;
      GSList *link;

      /* Name owner changed or deleted */ 

      names = g_hash_table_lookup (manager->owner_names, prev_owner);

      info = NULL;
      if (names != NULL)
        {
	  link = g_slist_find_custom (names, name, find_name_in_info);

	  if (link != NULL)
	    {
	      info = link->data;
	  
	      names = g_slist_delete_link (names, link);

              if (names == NULL)
                {
                  g_hash_table_remove (manager->owner_names, prev_owner);
                }
              else
                {
                  g_hash_table_insert (manager->owner_names,
                                       g_strdup (prev_owner), names);
                }
            }
        }

      if (new_owner[0] == '\0')
	{
	  DBusGProxyUnassociateData data;
	  GSList *tmp;

	  data.name = name;
	  data.destroyed = NULL;

	  /* A service went away, we need to unassociate proxies */
	  g_hash_table_foreach (manager->proxy_lists,
				unassociate_proxies, &data);

	  UNLOCK_MANAGER (manager);

          /* the destroyed list's data pointers are weak pointers, so that we
           * don't end up calling destroy on proxies which have already been
           * freed up as a result of other ones being destroyed */
	  for (tmp = data.destroyed; tmp; tmp = tmp->next)
            if (tmp->data != NULL)
              {
                g_object_remove_weak_pointer (G_OBJECT (tmp->data),
                    &(tmp->data));
                dbus_g_proxy_destroy (tmp->data);
              }
	  g_slist_free (data.destroyed);

	  LOCK_MANAGER (manager);

	  if (info)
	    {
	      g_free (info->name);
	      g_free (info);
	    }
	}
      else if (info)
	{
	  insert_nameinfo (manager, new_owner, info);
	}
    }
}

static void
got_name_owner_cb (DBusGProxy       *bus_proxy,
		   DBusGProxyCall   *call,
		   void             *user_data)
{
  DBusGProxy *proxy = user_data;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  GError *error;
  char *owner;

  error = NULL;
  owner = NULL;

  LOCK_MANAGER (priv->manager);

  if (!dbus_g_proxy_end_call (bus_proxy, call, &error,
			      G_TYPE_STRING, &owner,
			      G_TYPE_INVALID))
    {
      if (error->domain == DBUS_GERROR && error->code == DBUS_GERROR_NAME_HAS_NO_OWNER)
	{
	  priv->manager->unassociated_proxies = g_slist_prepend (priv->manager->unassociated_proxies, proxy);
	}
      else if (error->domain == DBUS_GERROR && error->code == DBUS_GERROR_REMOTE_EXCEPTION)
	g_warning ("Couldn't get name owner (%s): %s",
		   dbus_g_error_get_name (error),
		   error->message);
      else
	g_warning ("Couldn't get name owner (code %d): %s",
                   error->code, error->message);
      g_clear_error (&error);
      goto out;
    }
  else
    {
      dbus_g_proxy_manager_monitor_name_owner (priv->manager, owner, priv->name);
      priv->associated = TRUE;
    }

 out:
  priv->name_call = NULL;
  UNLOCK_MANAGER (priv->manager);
  g_free (owner);
}

static char *
get_name_owner (DBusConnection     *connection,
		const char         *name,
		GError            **error)
{
  DBusError derror;
  DBusMessage *request, *reply;
  char *base_name;
  
  dbus_error_init (&derror);

  base_name = NULL;
  reply = NULL;

  request = dbus_message_new_method_call (DBUS_SERVICE_DBUS,
					  DBUS_PATH_DBUS,
					  DBUS_INTERFACE_DBUS,
					  "GetNameOwner");
  if (request == NULL)
    g_error ("Out of memory");
  
  if (!dbus_message_append_args (request, 
				 DBUS_TYPE_STRING, &name, 
				 DBUS_TYPE_INVALID))
    g_error ("Out of memory");

  reply =
    dbus_connection_send_with_reply_and_block (connection,
                                               request,
                                               2000, &derror);
  if (reply == NULL)
    goto error;

  if (dbus_set_error_from_message (&derror, reply))
    goto error;

  if (!dbus_message_get_args (reply, &derror, 
			      DBUS_TYPE_STRING, &base_name, 
			      DBUS_TYPE_INVALID))
    goto error;

  base_name = g_strdup (base_name);
  goto out;

 error:
  g_assert (dbus_error_is_set (&derror));
  dbus_set_g_error (error, &derror);
  dbus_error_free (&derror);

 out:
  if (request)
    dbus_message_unref (request);
  if (reply)
    dbus_message_unref (reply);

  return base_name;
}


static void
guint_slice_free (gpointer data)
{
  g_slice_free (guint, data);
}


static void
dbus_g_proxy_manager_register (DBusGProxyManager *manager,
                               DBusGProxy        *proxy)
{
  DBusGProxyList *list;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  LOCK_MANAGER (manager);

  if (manager->proxy_lists == NULL)
    {
      g_assert (manager->owner_names == NULL);
      g_assert (manager->owner_match_rules == NULL);

      list = NULL;
      manager->proxy_lists = g_hash_table_new_full (tristring_hash,
                                                    tristring_equal,
                                                    NULL,
                                                    (GFreeFunc) g_proxy_list_free);
      manager->owner_names = g_hash_table_new_full (g_str_hash,
                                                    g_str_equal,
                                                    g_free,
                                                    NULL);
      manager->owner_match_rules = g_hash_table_new_full (g_str_hash,
                                                          g_str_equal,
                                                          g_free,
                                                          guint_slice_free);
    }
  else
    {
      char *tri;

      tri = tristring_from_proxy (proxy);
      
      list = g_hash_table_lookup (manager->proxy_lists, tri);

      g_free (tri);
    }
      
  if (list == NULL)
    {
      list = g_proxy_list_new (proxy);
      
      g_hash_table_replace (manager->proxy_lists,
                            list->name, list);
    }

  if (list->proxies == NULL && priv->name)
    {
      /* We have to add match rules to the server,
       * but only if the server is a message bus,
       * not if it's a peer.
       */
      char *rule;
      guint *refcount;

      rule = g_proxy_get_signal_match_rule (proxy);
      /* We don't check for errors; it's not like anyone would handle them, and
       * we don't want a round trip here.
       */
      dbus_bus_add_match (manager->connection, rule, NULL);
      g_free (rule);
       
      refcount = g_hash_table_lookup (manager->owner_match_rules, priv->name);

      if (refcount != NULL)
        {
          g_assert (*refcount != 0);
          g_assert (*refcount < G_MAXUINT);
          (*refcount)++;
        }
      else
        {
          char *rule;
          rule = get_owner_match_rule (priv->name);
          dbus_bus_add_match (manager->connection,
                              rule, NULL);
          g_free (rule);

          refcount = g_slice_new (guint);
          *refcount = 1;
          g_hash_table_insert (manager->owner_match_rules,
                               g_strdup (priv->name), refcount);
        }
    }

  g_assert (g_slist_find (list->proxies, proxy) == NULL);
  
  list->proxies = g_slist_prepend (list->proxies, proxy);

  if (!priv->for_owner)
    {
      const char *owner;
      DBusGProxyNameOwnerInfo *info;

      if (!dbus_g_proxy_manager_lookup_name_owner (manager, priv->name, &info, &owner))
	{
	  priv->name_call = manager_begin_bus_call (manager, "GetNameOwner",
						     got_name_owner_cb,
						     proxy, NULL,
						     G_TYPE_STRING,
						     priv->name, 
						     G_TYPE_INVALID);
	  
	  priv->associated = FALSE;
	}
      else
	{
	  info->refcount++;
	  priv->associated = TRUE;
	}
    }
  
  UNLOCK_MANAGER (manager);
}

static void
dbus_g_proxy_manager_unregister (DBusGProxyManager *manager,
                                DBusGProxy        *proxy)
{
  DBusGProxyList *list;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  char *tri;
  
  LOCK_MANAGER (manager);

#ifndef G_DISABLE_CHECKS
  if (manager->proxy_lists == NULL)
    {
      g_warning ("Trying to unregister a proxy but there aren't any registered");
      return;
    }
#endif

  tri = tristring_from_proxy (proxy);
  
  list = g_hash_table_lookup (manager->proxy_lists, tri);

#ifndef G_DISABLE_CHECKS
  if (list == NULL)
    {
      g_warning ("Trying to unregister a proxy but it isn't registered");
      return;
    }
#endif

  g_assert (g_slist_find (list->proxies, proxy) != NULL);
  
  list->proxies = g_slist_remove (list->proxies, proxy);

  g_assert (g_slist_find (list->proxies, proxy) == NULL);

  if (!priv->for_owner)
    {
      if (!priv->associated)
	{
	  GSList *link;

	  if (priv->name_call != 0)
	    {
	      dbus_g_proxy_cancel_call (manager->bus_proxy, priv->name_call);
	      priv->name_call = 0;
	    }
	  else
	    {
              link = g_slist_find (manager->unassociated_proxies, proxy);
              if (link != NULL)
                {
                  manager->unassociated_proxies = g_slist_delete_link (
                      manager->unassociated_proxies, link);
                }
	    }
	}
      else
	{
	  g_assert (priv->name_call == 0);
	  
	  dbus_g_proxy_manager_unmonitor_name_owner (manager, priv->name);
	}
    }

  if (list->proxies == NULL)
    {
      char *rule;
      g_hash_table_remove (manager->proxy_lists,
                           tri);

      rule = g_proxy_get_signal_match_rule (proxy);
      dbus_bus_remove_match (manager->connection,
                             rule, NULL);
      g_free (rule);

      if (priv->name)
        {
          guint *refcount;
          refcount = g_hash_table_lookup (manager->owner_match_rules, priv->name);
          (*refcount)--;

          if (*refcount == 0)
            {
              rule = get_owner_match_rule (priv->name);
              dbus_bus_remove_match (manager->connection,
                                     rule, NULL);
              g_free (rule);
              g_hash_table_remove (manager->owner_match_rules, priv->name);
            }
        }
    }
  
  if (g_hash_table_size (manager->proxy_lists) == 0)
    {
      g_hash_table_destroy (manager->proxy_lists);
      manager->proxy_lists = NULL;
    }

  if (g_hash_table_size (manager->owner_match_rules) == 0)
    {
      g_hash_table_destroy (manager->owner_match_rules);
      manager->owner_match_rules = NULL;
    }

  g_free (tri);
      
  UNLOCK_MANAGER (manager);
}

static void
list_proxies_foreach (gpointer key,
                      gpointer value,
                      gpointer user_data)
{
  DBusGProxyList *list;
  GSList **ret;
  GSList *tmp;
  
  list = value;
  ret = user_data;

  tmp = list->proxies;
  while (tmp != NULL)
    {
      DBusGProxy *proxy = DBUS_G_PROXY (tmp->data);

      g_object_ref (proxy);
      *ret = g_slist_prepend (*ret, proxy);
      
      tmp = tmp->next;
    }
}

static GSList*
dbus_g_proxy_manager_list_all (DBusGProxyManager *manager)
{
  GSList *ret;

  ret = NULL;

  if (manager->proxy_lists)
    {
      g_hash_table_foreach (manager->proxy_lists,
                            list_proxies_foreach,
                            &ret);
    }

  return ret;
}

static DBusHandlerResult
dbus_g_proxy_manager_filter (DBusConnection    *connection,
                             DBusMessage       *message,
                             void              *user_data)
{
  DBusGProxyManager *manager;
  
  if (dbus_message_get_type (message) != DBUS_MESSAGE_TYPE_SIGNAL)
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;

  manager = user_data;

  dbus_g_proxy_manager_ref (manager);
  
  LOCK_MANAGER (manager);
  
  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    {
      /* Destroy all the proxies, quite possibly resulting in unreferencing
       * the proxy manager and the connection as well.
       */
      GSList *all;
      GSList *tmp;

      all = dbus_g_proxy_manager_list_all (manager);

      tmp = all;
      while (tmp != NULL)
        {
          DBusGProxy *proxy;

          proxy = DBUS_G_PROXY (tmp->data);

          UNLOCK_MANAGER (manager);
          dbus_g_proxy_destroy (proxy);
          g_object_unref (G_OBJECT (proxy));
          LOCK_MANAGER (manager);
          
          tmp = tmp->next;
        }

      g_slist_free (all);

#ifndef G_DISABLE_CHECKS
      if (manager->proxy_lists != NULL)
        g_warning ("Disconnection emitted \"destroy\" on all DBusGProxy, but somehow new proxies were created in response to one of those destroy signals. This will cause a memory leak.");
#endif
    }
  else
    {
      char *tri;
      GSList *full_list;
      GSList *owned_names;
      GSList *tmp;
      const char *sender;

      /* First we handle NameOwnerChanged internally */
      if (dbus_message_is_signal (message,
				  DBUS_INTERFACE_DBUS,
				  "NameOwnerChanged"))
	{
	  const char *name;
	  const char *prev_owner;
	  const char *new_owner;
	  DBusError derr;

	  dbus_error_init (&derr);
	  if (!dbus_message_get_args (message,
				      &derr,
				      DBUS_TYPE_STRING,
				      &name,
				      DBUS_TYPE_STRING,
				      &prev_owner,
				      DBUS_TYPE_STRING,
				      &new_owner,
				      DBUS_TYPE_INVALID))
	    {
	      /* Ignore this error */
	      dbus_error_free (&derr);
	    }
	  else if (manager->owner_names != NULL)
	    {
	      dbus_g_proxy_manager_replace_name_owner (manager, name, prev_owner, new_owner);
	    }
	}

      sender = dbus_message_get_sender (message);

      /* dbus spec requires these, libdbus validates */
      g_assert (dbus_message_get_path (message) != NULL);
      g_assert (dbus_message_get_interface (message) != NULL);
      g_assert (dbus_message_get_member (message) != NULL);
      
      tri = tristring_from_message (message);

      if (manager->proxy_lists)
	{
	  DBusGProxyList *owner_list;
	  owner_list = g_hash_table_lookup (manager->proxy_lists, tri);
	  if (owner_list)
	    full_list = g_slist_copy (owner_list->proxies);
	  else
	    full_list = NULL;
	}
      else
	full_list = NULL;

      g_free (tri);

      if (manager->owner_names && sender)
	{
	  owned_names = g_hash_table_lookup (manager->owner_names, sender);
	  for (tmp = owned_names; tmp; tmp = tmp->next)
	    {
	      DBusGProxyList *owner_list;
	      DBusGProxyNameOwnerInfo *nameinfo;

	      nameinfo = tmp->data;
	      g_assert (nameinfo->refcount > 0);
	      tri = tristring_alloc_from_strings (0, nameinfo->name,
						  dbus_message_get_path (message),
						  dbus_message_get_interface (message));

	      owner_list = g_hash_table_lookup (manager->proxy_lists, tri);
	      if (owner_list != NULL) 
                {
	          GSList *elt;

	          /* Ignore duplicates when adding to full_list */
	          for (elt = owner_list->proxies; elt; elt = g_slist_next (elt)) 
                    {
	              if (!g_slist_find (full_list, elt->data))
	                full_list = g_slist_append (full_list, elt->data);
	            }
	        }
	      g_free (tri);
	    }
	}

#if 0
      g_print ("proxy got %s,%s,%s = list %p\n",
               tri,
               tri + strlen (tri) + 1,
               tri + strlen (tri) + 1 + strlen (tri + strlen (tri) + 1) + 1,
               list);
#endif
      
      /* Emit the signal */
      
      g_slist_foreach (full_list, (GFunc) g_object_ref, NULL);
      
      for (tmp = full_list; tmp; tmp = tmp->next)
	{
	  DBusGProxy *proxy;
	  
	  proxy = DBUS_G_PROXY (tmp->data);
	  
	  UNLOCK_MANAGER (manager);
	  dbus_g_proxy_emit_remote_signal (proxy, message);
	  g_object_unref (G_OBJECT (proxy));
	  LOCK_MANAGER (manager);
	}
      g_slist_free (full_list);
    }

  UNLOCK_MANAGER (manager);
  dbus_g_proxy_manager_unref (manager);
  
  /* "Handling" signals doesn't make sense, they are for everyone
   * who cares
   */
  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}



/*      ---------- DBusGProxy --------------   */
#define DBUS_G_PROXY_DESTROYED(proxy)  (DBUS_G_PROXY_GET_PRIVATE(proxy)->manager == NULL)

static void
marshal_dbus_message_to_g_marshaller (GClosure     *closure,
                                      GValue       *return_value,
                                      guint         n_param_values,
                                      const GValue *param_values,
                                      gpointer      invocation_hint,
                                      gpointer      marshal_data);
enum
{
  PROP_0,
  PROP_NAME,
  PROP_PATH,
  PROP_INTERFACE,
  PROP_CONNECTION
};

enum
{
  DESTROY,
  RECEIVED,
  LAST_SIGNAL
};

static void *parent_class;
static guint signals[LAST_SIGNAL] = { 0 };

static void
dbus_g_proxy_init (DBusGProxy *proxy)
{
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  
  g_datalist_init (&priv->signal_signatures);
  priv->pending_calls = g_hash_table_new_full (NULL, NULL, NULL,
				(GDestroyNotify) dbus_pending_call_unref);
  priv->name_call = 0;
  priv->associated = FALSE;
  priv->default_timeout = -1;
}

static GObject *
dbus_g_proxy_constructor (GType                  type,
			  guint                  n_construct_properties,
			  GObjectConstructParam *construct_properties)
{
  DBusGProxy *proxy;
  DBusGProxyClass *klass;
  GObjectClass *parent_class;
  DBusGProxyPrivate *priv;

  klass = DBUS_G_PROXY_CLASS (g_type_class_peek (DBUS_TYPE_G_PROXY));

  parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));

  proxy = DBUS_G_PROXY (parent_class->constructor (type, n_construct_properties,
						    construct_properties));

  priv = DBUS_G_PROXY_GET_PRIVATE (proxy);

  /* if these assertions fail, a deriving class has not set our required
   * parameters - our own public constructors do return_if_fail checks
   * on these parameters being provided. unfortunately we can't assert
   * for manager because it's allowed to be NULL when tha mangager is
   * setting up a bus proxy for its own calls */
  g_assert (priv->path != NULL);
  g_assert (priv->interface != NULL);

  if (priv->manager != NULL)
    {
      dbus_g_proxy_manager_register (priv->manager, proxy);
    }

  return G_OBJECT (proxy);
}

static void
dbus_g_proxy_class_init (DBusGProxyClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  g_type_class_add_private (klass, sizeof (DBusGProxyPrivate));

  object_class->set_property = dbus_g_proxy_set_property;
  object_class->get_property = dbus_g_proxy_get_property;

  g_object_class_install_property (object_class,
				   PROP_NAME,
				   g_param_spec_string ("name",
							"name",
							"name",
							NULL,
							G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property (object_class,
				   PROP_PATH,
				   g_param_spec_string ("path",
							"path",
							"path",
							NULL,
							G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property (object_class,
				   PROP_INTERFACE,
				   g_param_spec_string ("interface",
							"interface",
							"interface",
							NULL,
							G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  
  g_object_class_install_property (object_class,
				   PROP_CONNECTION,
				   g_param_spec_boxed ("connection",
							"connection",
							"connection",
							DBUS_TYPE_G_CONNECTION,
							G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  
  object_class->finalize = dbus_g_proxy_finalize;
  object_class->dispose = dbus_g_proxy_dispose;
  object_class->constructor = dbus_g_proxy_constructor;
  
  signals[DESTROY] =
    g_signal_new ("destroy",
		  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_CLEANUP | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                  0,
		  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  signals[RECEIVED] =
    g_signal_new ("received",
		  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  marshal_dbus_message_to_g_marshaller,
                  G_TYPE_NONE, 2, DBUS_TYPE_MESSAGE, G_TYPE_POINTER);
}

static gboolean
cancel_pending_call (gpointer key, gpointer val, gpointer data)
{
  DBusPendingCall *pending = val;

  dbus_pending_call_cancel (pending);

  return TRUE;
}

static void
dbus_g_proxy_dispose (GObject *object)
{
  DBusGProxy *proxy = DBUS_G_PROXY (object);
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  if (priv->pending_calls == NULL) 
    {
      return;
    }

  /* Cancel outgoing pending calls */
  g_hash_table_foreach_remove (priv->pending_calls, cancel_pending_call, NULL);
  g_hash_table_destroy (priv->pending_calls);
  priv->pending_calls = NULL;

  if (priv->manager && proxy != priv->manager->bus_proxy)
    {
      dbus_g_proxy_manager_unregister (priv->manager, proxy);
      dbus_g_proxy_manager_unref (priv->manager);
    }
  priv->manager = NULL;
  
  g_datalist_clear (&priv->signal_signatures);
  
  g_signal_emit (object, signals[DESTROY], 0);
  
  G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
dbus_g_proxy_finalize (GObject *object)
{
  DBusGProxy *proxy = DBUS_G_PROXY (object);
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  
  g_return_if_fail (DBUS_G_PROXY_DESTROYED (proxy));
  
  g_free (priv->name);
  g_free (priv->path);
  g_free (priv->interface);
  
  G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
dbus_g_proxy_destroy (DBusGProxy *proxy)
{
  /* FIXME do we need the GTK_IN_DESTRUCTION style flag
   * from GtkObject?
   */
  g_object_run_dispose (G_OBJECT (proxy));
}

static void
dbus_g_proxy_set_property (GObject *object,
			   guint prop_id,
			   const GValue *value,
			   GParamSpec *pspec)
{
  DBusGProxy *proxy = DBUS_G_PROXY (object);
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  DBusGConnection *connection;

  switch (prop_id)
    {
    case PROP_NAME:
      priv->name = g_strdup (g_value_get_string (value));
      if (priv->name)
        priv->for_owner = (priv->name[0] == ':');
      else
        priv->for_owner = TRUE;
      break;
    case PROP_PATH:
      priv->path = g_strdup (g_value_get_string (value));
      break;
    case PROP_INTERFACE:
      priv->interface = g_strdup (g_value_get_string (value));
      break;
    case PROP_CONNECTION:
      connection = g_value_get_boxed (value);
      if (connection != NULL)
        {
          priv->manager = dbus_g_proxy_manager_get (DBUS_CONNECTION_FROM_G_CONNECTION (connection));
        }
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void 
dbus_g_proxy_get_property (GObject *object,
			   guint prop_id,
			   GValue *value,
			   GParamSpec *pspec)
{
  DBusGProxy *proxy = DBUS_G_PROXY (object);
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  switch (prop_id)
    {
    case PROP_NAME:
      g_value_set_string (value, priv->name);
      break;
    case PROP_PATH:
      g_value_set_string (value, priv->path);
      break;
    case PROP_INTERFACE:
      g_value_set_string (value, priv->interface);
      break;
    case PROP_CONNECTION:
      g_value_set_boxed (value, DBUS_G_CONNECTION_FROM_CONNECTION(priv->manager->connection));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/* this is to avoid people using g_signal_connect() directly,
 * to avoid confusion with local signal names, and because
 * of the horribly broken current setup (signals are added
 * globally to all proxies)
 */
static char*
create_signal_name (const char *interface,
                    const char *signal)
{
  GString *str;
  char *p;

  str = g_string_new (interface);

  g_string_append (str, "-");
  
  g_string_append (str, signal);

  /* GLib will silently barf on '.' in signal names */
  p = str->str;
  while (*p)
    {
      if (*p == '.')
        *p = '-';
      ++p;
    }
  
  return g_string_free (str, FALSE);
}

static void
marshal_dbus_message_to_g_marshaller (GClosure     *closure,
                                      GValue       *return_value,
                                      guint         n_param_values,
                                      const GValue *param_values,
                                      gpointer      invocation_hint,
                                      gpointer      marshal_data)
{
  /* Incoming here we have three params, the instance (Proxy), the
   * DBusMessage, the signature. We want to convert that to an
   * expanded GValue array, then call an appropriate normal GLib
   * marshaller.
   */
#define MAX_SIGNATURE_ARGS 20
  GValueArray *value_array;
  GSignalCMarshaller c_marshaller;
  DBusGProxy *proxy;
  DBusMessage *message;
  GArray *gsignature;
  const GType *types;
  DBusGProxyPrivate *priv;

  g_assert (n_param_values == 3);

  proxy = g_value_get_object (&param_values[0]);
  message = g_value_get_boxed (&param_values[1]);
  gsignature = g_value_get_pointer (&param_values[2]);

  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (message != NULL);
  g_return_if_fail (gsignature != NULL);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  c_marshaller = _dbus_gobject_lookup_marshaller (G_TYPE_NONE, gsignature->len,
						  (GType*) gsignature->data);

  g_return_if_fail (c_marshaller != NULL);
  
  {
    DBusGValueMarshalCtx context;
    context.recursion_depth = 0;
    context.gconnection = DBUS_G_CONNECTION_FROM_CONNECTION (priv->manager->connection);
    context.proxy = proxy;

    types = (const GType*) gsignature->data;
    value_array = _dbus_gvalue_demarshal_message (&context, message,
						 gsignature->len, types, NULL);
  }

  if (value_array == NULL)
    return;
  
  g_value_array_prepend (value_array, NULL);
  g_value_init (g_value_array_get_nth (value_array, 0), G_TYPE_FROM_INSTANCE (proxy));
  g_value_set_instance (g_value_array_get_nth (value_array, 0), proxy);

  (* c_marshaller) (closure, return_value, value_array->n_values,
		    value_array->values, invocation_hint, marshal_data);
  
  g_value_array_free (value_array);
}

static void
dbus_g_proxy_emit_remote_signal (DBusGProxy  *proxy,
                                 DBusMessage *message)
{
  const char *interface;
  const char *signal;
  char *name;
  GQuark q;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  GArray *msg_gsignature = NULL;

  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));

  interface = dbus_message_get_interface (message);
  signal = dbus_message_get_member (message);

  g_assert (interface != NULL);
  g_assert (signal != NULL);

  name = create_signal_name (interface, signal);

  /* If the quark isn't preexisting, there's no way there
   * are any handlers connected. We don't want to create
   * extra quarks for every possible signal.
   */
  q = g_quark_try_string (name);

  if (q != 0)
    {
      GArray *gsignature;
      guint i;
      
      gsignature = g_datalist_id_get_data (&priv->signal_signatures, q);
      if (gsignature == NULL)
	goto out;
      
      msg_gsignature = _dbus_gtypes_from_arg_signature (dbus_message_get_signature (message),
						       TRUE);
      for (i = 0; i < gsignature->len; i++)
	{
	  if (msg_gsignature->len == i
	      || g_array_index (gsignature, GType, i) != g_array_index (msg_gsignature, GType, i))
	    goto mismatch;
	}
      if (msg_gsignature->len != i)
	goto mismatch;
      
      g_signal_emit (proxy,
		     signals[RECEIVED],
		     q,
		     message,
		     msg_gsignature);
    }

 out:
  g_free (name);
  if (msg_gsignature)
    g_array_free (msg_gsignature, TRUE);
  return;
 mismatch:
#if 0
  /* Don't spew on remote errors */
  g_warning ("Unexpected message signature '%s' for signal '%s'\n",
	     dbus_message_get_signature (message),
	     name);
#endif
  goto out;
}

/**
 * DBusGProxyCallNotify:
 * @proxy: the proxy on which the method was called
 * @call_id: the call in progress
 * @user_data: data passed to dbus_g_proxy_begin_call() or similar
 *
 * Called when a reply to the call represented by @call_id arrives.
 * Use dbus_g_proxy_end_call() to see whether @call_id succeeded or
 * failed, and get the arguments returned (if any) on success.
 */

typedef struct
{
  DBusGProxy *proxy;
  guint call_id;
  DBusGProxyCallNotify func;
  void *data;
  GDestroyNotify free_data_func;
} GPendingNotifyClosure;

static void
d_pending_call_notify (DBusPendingCall *dcall,
                       void            *data)
{
  GPendingNotifyClosure *closure = data;

  (* closure->func) (closure->proxy, DBUS_G_PROXY_ID_TO_CALL (closure->call_id), closure->data);
}

static void
d_pending_call_free (void *data)
{
  GPendingNotifyClosure *closure = data;
  
  if (closure->free_data_func)
    (* closure->free_data_func) (closure->data);

  g_free (closure);
}

#define DBUS_G_VALUE_ARRAY_COLLECT_ALL(VALARRAY, FIRST_ARG_TYPE, ARGS) \
G_STMT_START { \
  GType valtype; \
  guint i = 0; \
  \
  VALARRAY = g_value_array_new (6); \
  valtype = FIRST_ARG_TYPE; \
  \
  while (valtype != G_TYPE_INVALID) \
    { \
      gchar *collect_err; \
      GValue *val; \
      \
      g_value_array_append (VALARRAY, NULL); \
      val = g_value_array_get_nth (VALARRAY, i); \
      g_value_init (val, valtype); \
      collect_err = NULL; \
      G_VALUE_COLLECT (val, ARGS, G_VALUE_NOCOPY_CONTENTS, &collect_err); \
      \
      if (collect_err) \
        { \
          g_critical ("%s: unable to collect argument %u: %s", \
              G_STRFUNC, i, collect_err); \
          g_free (collect_err); \
          g_value_array_free (VALARRAY); \
          VALARRAY = NULL; \
          break; \
        } \
      \
      valtype = va_arg (ARGS, GType); \
      i++; \
    } \
} G_STMT_END

DBusGProxyCall *
manager_begin_bus_call (DBusGProxyManager    *manager,
			const char           *method,
			DBusGProxyCallNotify  notify,
			gpointer              user_data,
			GDestroyNotify        destroy,
			GType                 first_arg_type,
			...)
{
  guint call_id = 0;
  DBusGProxyPrivate *priv;
  va_list args;
  GValueArray *arg_values;
  
  va_start (args, first_arg_type);

  if (!manager->bus_proxy)
    {
      manager->bus_proxy = g_object_new (DBUS_TYPE_G_PROXY,
					 "name", DBUS_SERVICE_DBUS,
					 "path", DBUS_PATH_DBUS,
					 "interface", DBUS_INTERFACE_DBUS,
					 NULL);
      priv = DBUS_G_PROXY_GET_PRIVATE(manager->bus_proxy);
      priv->manager = manager;
    }

  DBUS_G_VALUE_ARRAY_COLLECT_ALL (arg_values, first_arg_type, args);

  if (arg_values != NULL)
    {
      call_id = dbus_g_proxy_begin_call_internal (manager->bus_proxy, method,
          notify, user_data, destroy, arg_values, -1);

      g_value_array_free (arg_values);
    }

  va_end (args);

  return DBUS_G_PROXY_ID_TO_CALL (call_id);
}

/**
 * SECTION:dbus-gproxy
 * @short_description: DBus Proxy
 * @see_also: #DBusGProxy
 * @stability: Stable
 *
 * A #DBusGProxy is a #GObject representing a remote object in a D-Bus
 * service.
 */

/**
 * DBusGProxy:
 *
 * A #GObject representing a remote object in a D-Bus service.
 */

/**
 * DBusGProxyCall:
 *
 * An opaque pointer representing an asynchronous call in progress.
 */

/*
 * dbus_g_proxy_get_type:
 * Standard GObject get_type() function for DBusGProxy.
 *
 * Returns: type ID for DBusGProxy class
 */
GType
dbus_g_proxy_get_type (void)
{
  static GType object_type = 0;

  if (!object_type)
    {
      static const GTypeInfo object_info =
        {
          sizeof (DBusGProxyClass),
          (GBaseInitFunc) NULL,
          (GBaseFinalizeFunc) NULL,
          (GClassInitFunc) dbus_g_proxy_class_init,
          NULL,           /* class_finalize */
          NULL,           /* class_data */
          sizeof (DBusGProxy),
          0,              /* n_preallocs */
          (GInstanceInitFunc) dbus_g_proxy_init,
        };
      
      object_type = g_type_register_static (G_TYPE_OBJECT,
                                            "DBusGProxy",
                                            &object_info, 0);
    }
  
  return object_type;
}

static DBusGProxy*
dbus_g_proxy_new (DBusGConnection *connection,
                  const char      *name,
                  const char      *path_name,
                  const char      *interface_name)
{
  DBusGProxy *proxy;

  g_assert (connection != NULL);
  
  proxy = g_object_new (DBUS_TYPE_G_PROXY, 
                        "name", name, 
                        "path", path_name, 
                        "interface", interface_name, 
                        "connection", connection, NULL);

  return proxy;
}

/**
 * dbus_g_proxy_new_for_name:
 * @connection: the connection to the remote bus
 * @name: any name on the message bus
 * @path: name of the object instance to call methods on
 * @iface: name of the interface to call methods on
 *
 * Creates a new proxy for a remote interface exported by a connection
 * on a message bus. Method calls and signal connections over this
 * proxy will go to the name owner; the name's owner is expected to
 * support the given interface name. THE NAME OWNER MAY CHANGE OVER
 * TIME, for example between two different method calls, unless the
 * name is a unique name. If you need a fixed owner, you need to
 * request the current owner and bind a proxy to its unique name
 * rather than to the generic name; see
 * dbus_g_proxy_new_for_name_owner().
 *
 * A name-associated proxy only makes sense with a message bus, not
 * for app-to-app direct dbus connections.
 *
 * This proxy will only emit the "destroy" signal if the
 * #DBusConnection is disconnected, the proxy has no remaining
 * references, or the name is a unique name and its owner
 * disappears. If a well-known name changes owner, the proxy will
 * still be alive.
 *
 * Returns: new proxy object
 */
DBusGProxy*
dbus_g_proxy_new_for_name (DBusGConnection *connection,
                           const char      *name,
                           const char      *path,
                           const char      *iface)
{
  g_return_val_if_fail (connection != NULL, NULL);
  g_return_val_if_fail (g_dbus_is_name (name), NULL);
  g_return_val_if_fail (g_variant_is_object_path (path), NULL);
  g_return_val_if_fail (g_dbus_is_interface_name (iface), NULL);

  return dbus_g_proxy_new (connection, name, path, iface);
}

/**
 * dbus_g_proxy_new_for_name_owner:
 * @connection: the connection to the remote bus
 * @name: any name on the message bus
 * @path: name of the object inside the service to call methods on
 * @iface: name of the interface to call methods on
 * @error: return location for an error
 *
 * Similar to dbus_g_proxy_new_for_name(), but makes a round-trip
 * request to the message bus to get the current name owner, then
 * binds the proxy to the unique name of the current owner, rather
 * than to the well-known name. As a result, the name owner will
 * not change over time, and the proxy will emit the "destroy" signal
 * when the owner disappears from the message bus.
 *
 * An example of the difference between dbus_g_proxy_new_for_name()
 * and dbus_g_proxy_new_for_name_owner(): if you provide the well-known name
 * "org.freedesktop.Database" dbus_g_proxy_new_for_name() remains bound
 * to that name as it changes owner. dbus_g_proxy_new_for_name_owner()
 * will fail if the name has no owner. If the name has an owner,
 * dbus_g_proxy_new_for_name_owner() will bind to the unique name
 * of that owner rather than the generic name.
 * 
 * Returns: new proxy object, or %NULL on error
 */
DBusGProxy*
dbus_g_proxy_new_for_name_owner (DBusGConnection          *connection,
                                 const char               *name,
                                 const char               *path,
                                 const char               *iface,
                                 GError                  **error)
{
  DBusGProxy *proxy;
  char *unique_name;

  g_return_val_if_fail (connection != NULL, NULL);
  g_return_val_if_fail (g_dbus_is_name (name), NULL);
  g_return_val_if_fail (g_variant_is_object_path (path), NULL);
  g_return_val_if_fail (g_dbus_is_interface_name (iface), NULL);

  if (!(unique_name = get_name_owner (DBUS_CONNECTION_FROM_G_CONNECTION (connection), name, error)))
    return NULL;

  proxy = dbus_g_proxy_new (connection, unique_name, path, iface);
  g_free (unique_name);
  return proxy;
}

/**
 * dbus_g_proxy_new_from_proxy:
 * @proxy: the proxy to use as a template
 * @iface: name of the interface to call methods on
 * @path: of the object inside the peer to call methods on
 *
 * Creates a proxy using an existing proxy as a template, substituting
 * the specified interface and path.  Either or both may be NULL.
 *
 * Returns: new proxy object
 */
DBusGProxy*
dbus_g_proxy_new_from_proxy (DBusGProxy        *proxy,
			     const char        *iface,
			     const char        *path)
{
  DBusGProxyPrivate *priv;

  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), NULL);
  g_return_val_if_fail (path == NULL || g_variant_is_object_path (path), NULL);
  g_return_val_if_fail (iface == NULL ||
      g_dbus_is_interface_name (iface), NULL);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  
  if (iface == NULL)
    iface = priv->interface;
  if (path == NULL)
    path = priv->path;

  return dbus_g_proxy_new (DBUS_G_CONNECTION_FROM_CONNECTION (priv->manager->connection),
			   priv->name,
			   path, iface);
}

/**
 * dbus_g_proxy_new_for_peer:
 * @connection: the connection to the peer
 * @path: name of the object inside the peer to call methods on
 * @iface: name of the interface to call methods on
 *
 * Creates a proxy for an object in peer application (one
 * we're directly connected to). That is, this function is
 * intended for use when there's no message bus involved,
 * we're doing a simple 1-to-1 communication between two
 * applications.
 *
 * Returns: new proxy object
 */
DBusGProxy*
dbus_g_proxy_new_for_peer (DBusGConnection          *connection,
                           const char               *path,
                           const char               *iface)
{
  DBusGProxy *proxy;
  
  g_return_val_if_fail (connection != NULL, NULL);
  g_return_val_if_fail (g_variant_is_object_path (path), NULL);
  g_return_val_if_fail (g_dbus_is_interface_name (iface), NULL);

  proxy = dbus_g_proxy_new (connection, NULL, path, iface);

  return proxy;
}

/**
 * dbus_g_proxy_get_bus_name:
 * @proxy: the proxy
 *
 * Gets the bus name a proxy is bound to (may be %NULL in some cases).
 * If you created the proxy with dbus_g_proxy_new_for_name(), then
 * the name you passed to that will be returned.
 * If you created it with dbus_g_proxy_new_for_name_owner(), then the
 * unique connection name will be returned. If you created it
 * with dbus_g_proxy_new_for_peer() then %NULL will be returned.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * Returns: the bus name the proxy sends messages to
 */
const char*
dbus_g_proxy_get_bus_name (DBusGProxy        *proxy)
{
  DBusGProxyPrivate *priv;

  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), NULL);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), NULL);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  return priv->name;
}

/**
 * dbus_g_proxy_get_interface:
 * @proxy: the proxy
 *
 * Gets the object interface proxy is bound to (may be %NULL in some cases).
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * Returns: an object interface 
 */
const char*
dbus_g_proxy_get_interface (DBusGProxy        *proxy)
{
  DBusGProxyPrivate *priv;
  
  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), NULL);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), NULL);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  return priv->interface;
}

/**
 * dbus_g_proxy_set_interface:
 * @proxy: the proxy
 * @interface_name: an object interface 
 *
 * Sets the object interface proxy is bound to
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 */
void
dbus_g_proxy_set_interface (DBusGProxy        *proxy,
			    const char        *interface_name)
{
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));
  g_return_if_fail (g_dbus_is_interface_name (interface_name));

  /* FIXME - need to unregister when we switch interface for now
   * later should support idea of unset interface
   */
  dbus_g_proxy_manager_unregister (priv->manager, proxy);
  g_free (priv->interface);
  priv->interface = g_strdup (interface_name);
  dbus_g_proxy_manager_register (priv->manager, proxy);
}

/**
 * dbus_g_proxy_get_path:
 * @proxy: the proxy
 *
 * Gets the path this proxy is bound to
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * Returns: an object path
 */
const char*
dbus_g_proxy_get_path (DBusGProxy        *proxy)
{
  DBusGProxyPrivate *priv;
  
  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), NULL);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), NULL);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  return priv->path;
}

static DBusMessage *
dbus_g_proxy_marshal_args_to_message (DBusGProxy  *proxy,
				      const char  *method,
				      GValueArray *args)
{
  DBusMessage *message;
  DBusMessageIter msgiter;
  guint i;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  message = dbus_message_new_method_call (priv->name,
                                          priv->path,
                                          priv->interface,
                                          method);
  if (message == NULL)
    return NULL;

  dbus_message_iter_init_append (message, &msgiter);
  for (i = 0; i < args->n_values; i++)
    {
      GValue *gvalue;

      gvalue = g_value_array_get_nth (args, i);

      if (!_dbus_gvalue_marshal (&msgiter, gvalue))
        {
          /* This is a programming error by the caller, most likely */
          gchar *contents = g_strdup_value_contents (gvalue);

          g_critical ("Could not marshal argument %u for %s: type %s, value %s",
              i, method, G_VALUE_TYPE_NAME (gvalue), contents);
          g_free (contents);
          dbus_message_unref (message);
          return NULL;
        }
    }

  return message;
}

static guint
dbus_g_proxy_begin_call_internal (DBusGProxy          *proxy,
				  const char          *method,
				  DBusGProxyCallNotify notify,
				  gpointer             user_data,
				  GDestroyNotify       destroy,
				  GValueArray         *args,
				  int timeout)
{
  DBusMessage *message;
  DBusPendingCall *pending;
  GPendingNotifyClosure *closure;
  guint call_id;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  pending = NULL;

  message = dbus_g_proxy_marshal_args_to_message (proxy, method, args);

  /* can only happen on a programming error or OOM; we already critical'd */
  if (!message)
    return 0;

  if (!dbus_connection_send_with_reply (priv->manager->connection,
                                        message,
                                        &pending,
                                        timeout))
    oom ();

  dbus_message_unref (message);
  
  /* If we got a NULL pending, that means the connection was disconnected,
   * and we need to abort this call.  
   * https://bugs.freedesktop.org/show_bug.cgi?id=12675
   */
  if (pending == NULL)
    return 0;

  call_id = ++priv->call_id_counter;

  if (notify != NULL)
    {
      closure = g_new (GPendingNotifyClosure, 1);
      closure->proxy = proxy; /* No need to ref as the lifecycle is tied to proxy */
      closure->call_id = call_id;
      closure->func = notify;
      closure->data = user_data;
      closure->free_data_func = destroy;
      dbus_pending_call_set_notify (pending, d_pending_call_notify,
				    closure,
				    d_pending_call_free);
    }

  g_hash_table_insert (priv->pending_calls, GUINT_TO_POINTER (call_id), pending);

  return call_id;
}

static gboolean
dbus_g_proxy_end_call_internal (DBusGProxy        *proxy,
				guint              call_id,
				GError           **error,
				GType              first_arg_type,
				va_list            args)
{
  DBusMessage *reply;
  DBusMessageIter msgiter;
  DBusError derror;
  va_list args_unwind;
  guint over;
  int n_retvals_processed;
  gboolean ret;
  GType valtype;
  DBusPendingCall *pending;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  if (call_id == 0)
    {
      /* Being disconnected is the only reason this can happen, except
       * for programmer error; if it was programmer error, we already
       * emitted a critical warning. */
      g_set_error (error, DBUS_GERROR, DBUS_GERROR_DISCONNECTED,
          "Disconnected from D-Bus (or argument error during call)");
      return FALSE;
    }

  reply = NULL;
  ret = FALSE;
  n_retvals_processed = 0;
  over = 0;

  /* Keep around a copy of output arguments so we can free on error. */
  G_VA_COPY(args_unwind, args);

  pending = g_hash_table_lookup (priv->pending_calls, GUINT_TO_POINTER (call_id));
  
  dbus_pending_call_block (pending);
  reply = dbus_pending_call_steal_reply (pending);

  g_assert (reply != NULL);

  dbus_error_init (&derror);

  switch (dbus_message_get_type (reply))
    {
    case DBUS_MESSAGE_TYPE_METHOD_RETURN:
      dbus_message_iter_init (reply, &msgiter);
      valtype = first_arg_type;
      while (valtype != G_TYPE_INVALID)
	{
	  int arg_type;
	  gpointer return_storage;
	  GValue gvalue = { 0, };
	  DBusGValueMarshalCtx context;

          context.recursion_depth = 0;
	  context.gconnection = DBUS_G_CONNECTION_FROM_CONNECTION (priv->manager->connection);
	  context.proxy = proxy;

	  arg_type = dbus_message_iter_get_arg_type (&msgiter);
	  if (arg_type == DBUS_TYPE_INVALID)
	    {
	      g_set_error (error, DBUS_GERROR,
			   DBUS_GERROR_INVALID_ARGS,
			   "Too few arguments in reply");
	      goto out;
	    }

	  return_storage = va_arg (args, gpointer);
	  if (return_storage == NULL)
	    goto next;

	  /* We handle variants specially; the caller is expected
	   * to have already allocated storage for them.
	   */
	  if (arg_type == DBUS_TYPE_VARIANT
	      && g_type_is_a (valtype, G_TYPE_VALUE))
	    {
	      if (!_dbus_gvalue_demarshal_variant (&context, &msgiter, (GValue*) return_storage, NULL))
		{
		  g_set_error (error,
			       DBUS_GERROR,
			       DBUS_GERROR_INVALID_ARGS,
			       "Couldn't convert argument, expected \"%s\"",
			       g_type_name (valtype));
		  goto out;
		}
	    }
	  else
	    {
	      g_value_init (&gvalue, valtype);

	      if (!_dbus_gvalue_demarshal (&context, &msgiter, &gvalue, error))
		goto out;

	      /* Anything that can be demarshaled must be storable */
	      if (!_dbus_gvalue_store (&gvalue, return_storage))
		g_assert_not_reached ();
	      /* Ownership of the value passes to the client, don't unset */
	    }
	  
	next:
	  n_retvals_processed++;
	  dbus_message_iter_next (&msgiter);
	  valtype = va_arg (args, GType);
	}
      
      while (dbus_message_iter_get_arg_type (&msgiter) != DBUS_TYPE_INVALID)
	{
	  over++;
	  dbus_message_iter_next (&msgiter);
	}

      if (over > 0)
	{
	  g_set_error (error, DBUS_GERROR,
		       DBUS_GERROR_INVALID_ARGS,
		       "Too many arguments in reply; expected %d, got %d",
		       n_retvals_processed, over);
	  goto out;
	}
      break;
    case DBUS_MESSAGE_TYPE_ERROR:
      dbus_set_error_from_message (&derror, reply);
      dbus_set_g_error (error, &derror);
      dbus_error_free (&derror);
      goto out;
      break;
    default:
      dbus_set_error (&derror, DBUS_ERROR_FAILED,
                      "Reply was neither a method return nor an exception");
      dbus_set_g_error (error, &derror);
      dbus_error_free (&derror);
      goto out;
      break;
    }

  ret = TRUE;
 out:
  if (ret == FALSE)
    {
      int i;

      valtype = first_arg_type;
      for (i = 0; i < n_retvals_processed; i++)
	{
          GValue value = {0,};
	  gpointer retval;

          g_value_init (&value, valtype);

	  retval = va_arg (args_unwind, gpointer);
          if (retval == NULL)
            {
              i--;
              continue;
            }
            
          _dbus_gvalue_take (&value, retval);
          g_value_unset (&value);

          valtype = va_arg (args_unwind, GType);
	}
    }
  va_end (args_unwind);
  va_end (args);

  g_hash_table_remove (priv->pending_calls, GUINT_TO_POINTER (call_id));

  if (reply)
    dbus_message_unref (reply);
  return ret;
}

/**
 * dbus_g_proxy_begin_call:
 * @proxy: a proxy for a remote interface
 * @method: the name of the method to invoke
 * @notify: callback to be invoked when method returns
 * @user_data: user data passed to callback
 * @destroy: function called to destroy user_data
 * @first_arg_type: type of the first argument, or %G_TYPE_INVALID if there
 *    are no arguments
 * @...: first argument, followed by any further type/value pairs, followed
 *    by %G_TYPE_INVALID
 *
 * Asynchronously invokes a method on a remote interface. The method
 * call will not be sent over the wire until the application returns
 * to the main loop, or blocks in dbus_g_connection_flush() to write out
 * pending data.  The call will be completed after a timeout, or when
 * a reply is received.  When the call returns, the callback specified
 * will be invoked; you can then collect the results of the call
 * (which may be an error, or a reply), use dbus_g_proxy_end_call().
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * TODO this particular function shouldn't die on out of memory,
 * since you should be able to do a call with large arguments.
 * 
 * Returns: call identifier.
 */
DBusGProxyCall *
dbus_g_proxy_begin_call (DBusGProxy          *proxy,
			 const char          *method,
			 DBusGProxyCallNotify notify,
			 gpointer             user_data,
			 GDestroyNotify       destroy,
			 GType                first_arg_type,
			 ...)
{
  guint call_id = 0;
  va_list args;
  GValueArray *arg_values;
  DBusGProxyPrivate *priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  
  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), NULL);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), NULL);
  g_return_val_if_fail (g_dbus_is_member_name (method), NULL);

  va_start (args, first_arg_type);

  DBUS_G_VALUE_ARRAY_COLLECT_ALL (arg_values, first_arg_type, args);

  if (arg_values != NULL)
    {
      call_id = dbus_g_proxy_begin_call_internal (proxy, method, notify,
          user_data, destroy, arg_values, priv->default_timeout);

      g_value_array_free (arg_values);
    }

  va_end (args);

  return DBUS_G_PROXY_ID_TO_CALL (call_id);
}

/**
 * dbus_g_proxy_begin_call_with_timeout:
 * @proxy: a proxy for a remote interface
 * @method: the name of the method to invoke
 * @notify: callback to be invoked when method returns
 * @user_data: user data passed to callback
 * @destroy: function called to destroy user_data
 * @timeout: the timeout in milliseconds, or -1 to use a default
 * @first_arg_type: type of the first argument, or %G_TYPE_INVALID if there
 *    are no arguments
 * @...: first argument, followed by any further type/value pairs, followed
 *    by %G_TYPE_INVALID
 *
 * Asynchronously invokes a method on a remote interface. The method
 * call will not be sent over the wire until the application returns
 * to the main loop, or blocks in dbus_g_connection_flush() to write out
 * pending data.  The call will be completed after a timeout, or when
 * a reply is received.  When the call returns, the callback specified
 * will be invoked; you can then collect the results of the call
 * (which may be an error, or a reply), use dbus_g_proxy_end_call().
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * TODO this particular function shouldn't die on out of memory,
 * since you should be able to do a call with large arguments.
 *
 * Returns: call identifier.
 */
DBusGProxyCall *
dbus_g_proxy_begin_call_with_timeout (DBusGProxy          *proxy,
                         const char          *method,
                         DBusGProxyCallNotify notify,
                         gpointer             user_data,
                         GDestroyNotify       destroy,
			 int timeout,
                         GType                first_arg_type,
                         ...)
{
  guint call_id = 0;
  va_list args;
  GValueArray *arg_values;

  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), NULL);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), NULL);
  g_return_val_if_fail (g_dbus_is_member_name (method), NULL);
  g_return_val_if_fail (timeout >= 0 || timeout == -1, NULL);

  va_start (args, first_arg_type);

  DBUS_G_VALUE_ARRAY_COLLECT_ALL (arg_values, first_arg_type, args);

  if (arg_values != NULL)
    {
      call_id = dbus_g_proxy_begin_call_internal (proxy, method, notify,
          user_data, destroy, arg_values, timeout);

      g_value_array_free (arg_values);
    }

  va_end (args);

  return DBUS_G_PROXY_ID_TO_CALL (call_id);
}

/**
 * dbus_g_proxy_end_call:
 * @proxy: a proxy for a remote interface
 * @call: the pending call ID from dbus_g_proxy_begin_call()
 * @error: return location for an error
 * @first_arg_type: type of first "out" argument, or %G_TYPE_INVALID if
 *    there are no "out" arguments
 * @...: return location for first "out" argument, followed by any further
 *    type/location pairs, followed by %G_TYPE_INVALID
 *
 * Collects the results of a method call. The method call was normally
 * initiated with dbus_g_proxy_end_call(). You may use this function
 * outside of the callback given to dbus_g_proxy_begin_call; in that
 * case this function will block if the results haven't yet been
 * received.
 *
 * All D-Bus method calls can fail with a remote error. If this occurs,
 * the @error will be set and this function will return %FALSE.
 *
 * Otherwise, the "out" parameters and return value of the
 * method are stored in the provided varargs list.
 * The list should be terminated with G_TYPE_INVALID.
 *
 * Returns: %TRUE on success
 */
gboolean
dbus_g_proxy_end_call (DBusGProxy          *proxy,
                       DBusGProxyCall      *call,
                       GError             **error,
                       GType                first_arg_type,
                       ...)
{
  gboolean ret;
  va_list args;

  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), FALSE);

  va_start (args, first_arg_type);

  ret = dbus_g_proxy_end_call_internal (proxy, GPOINTER_TO_UINT (call), error, first_arg_type, args);

  va_end (args);
  
  return ret;
}

/**
 * dbus_g_proxy_call:
 * @proxy: a proxy for a remote interface
 * @method: method to invoke
 * @error: return location for an error
 * @first_arg_type: type of first "in" argument, or %G_TYPE_INVALID if none
 * @...: value of first "in" argument, any further type/value pairs,
 *    %G_TYPE_INVALID, type/location pairs for "out" arguments,
 *    and %G_TYPE_INVALID again
 *
 * Function for synchronously invoking a method and receiving reply
 * values.  This function is equivalent to dbus_g_proxy_begin_call
 * followed by dbus_g_proxy_end_call.  All of the input arguments are
 * specified first, followed by G_TYPE_INVALID, followed by all of the
 * output values, followed by a second G_TYPE_INVALID.  Note that  
 * this means you must always specify G_TYPE_INVALID twice.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * Returns: %TRUE if the method succeeds, %FALSE if it fails
 */
gboolean
dbus_g_proxy_call (DBusGProxy        *proxy,
		   const char        *method,
		   GError           **error,
		   GType              first_arg_type,
		   ...)
{
  gboolean ret;
  guint call_id = 0;
  va_list args;
  GValueArray *in_args;
  DBusGProxyPrivate *priv;

  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), FALSE);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), FALSE);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  va_start (args, first_arg_type);

  DBUS_G_VALUE_ARRAY_COLLECT_ALL (in_args, first_arg_type, args);

  if (in_args != NULL)
    {
      call_id = dbus_g_proxy_begin_call_internal (proxy, method, NULL, NULL,
          NULL, in_args, priv->default_timeout);

      g_value_array_free (in_args);
    }

  first_arg_type = va_arg (args, GType);
  ret = dbus_g_proxy_end_call_internal (proxy, call_id, error, first_arg_type,
      args);

  va_end (args);

  return ret;
}

/**
 * dbus_g_proxy_call_with_timeout:
 * @proxy: a proxy for a remote interface
 * @method: method to invoke
 * @timeout: the timeout in milliseconds, or -1 to use a default
 * @error: return location for an error
 * @first_arg_type: type of first "in" argument
 * @...: as for dbus_g_proxy_call()
 *
 * Function for synchronously invoking a method and receiving reply
 * values.  This function is equivalent to dbus_g_proxy_begin_call
 * followed by dbus_g_proxy_end_call.  All of the input arguments are
 * specified first, followed by G_TYPE_INVALID, followed by all of the
 * output values, followed by a second G_TYPE_INVALID.  Note that
 * this means you must always specify G_TYPE_INVALID twice.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * Returns: %TRUE if the method succeeds, %FALSE if it fails
 */
gboolean
dbus_g_proxy_call_with_timeout (DBusGProxy        *proxy,
                   const char        *method,
		   int timeout,
                   GError           **error,
                   GType              first_arg_type,
                   ...)
{
  gboolean ret;
  guint call_id = 0;
  va_list args;
  GValueArray *in_args;

  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), FALSE);
  g_return_val_if_fail (!DBUS_G_PROXY_DESTROYED (proxy), FALSE);
  g_return_val_if_fail (g_dbus_is_member_name (method), FALSE);
  g_return_val_if_fail (timeout >= 0 || timeout == -1, FALSE);

  va_start (args, first_arg_type);

  DBUS_G_VALUE_ARRAY_COLLECT_ALL (in_args, first_arg_type, args);

  if (in_args != NULL)
    {
      call_id = dbus_g_proxy_begin_call_internal (proxy, method, NULL, NULL,
          NULL, in_args, timeout);

      g_value_array_free (in_args);
    }

  first_arg_type = va_arg (args, GType);
  ret = dbus_g_proxy_end_call_internal (proxy, call_id, error,
      first_arg_type, args);

  va_end (args);

  return ret;
}

/**
 * dbus_g_proxy_call_no_reply:
 * @proxy: a proxy for a remote interface
 * @method: the name of the method to invoke
 * @first_arg_type: type of the first argument, or %G_TYPE_INVALID to call
 *    the method without arguments
 * @...: the first argument and any remaining type/argument pairs, followed by
 *    %G_TYPE_INVALID to terminate the list
 *
 * Sends a method call message as with dbus_g_proxy_begin_call(), but
 * does not ask for a reply or allow you to receive one.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * TODO: this particular function shouldn't die on out of memory,
 * since you should be able to do a call with large arguments.
 */
void
dbus_g_proxy_call_no_reply (DBusGProxy               *proxy,
			    const char               *method,
			    GType                     first_arg_type,
			    ...)
{
  DBusMessage *message = NULL;
  va_list args;
  GValueArray *in_args;
  DBusGProxyPrivate *priv;
  
  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (g_dbus_is_member_name (method));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  va_start (args, first_arg_type);
  DBUS_G_VALUE_ARRAY_COLLECT_ALL (in_args, first_arg_type, args);

  if (in_args != NULL)
    {
      message = dbus_g_proxy_marshal_args_to_message (proxy, method, in_args);

      g_value_array_free (in_args);
    }

  va_end (args);

  /* can only happen on a programming error or OOM; we already critical'd */
  if (!message)
    return;

  dbus_message_set_no_reply (message, TRUE);

  if (!dbus_connection_send (priv->manager->connection,
                             message,
                             NULL))
    oom ();

  dbus_message_unref (message);
}

/**
 * dbus_g_proxy_cancel_call
 * @proxy: a proxy for a remote interface
 * @call: the pending call ID from dbus_g_proxy_begin_call()
 *
 * Cancels a pending method call. The method call was normally
 * initiated with dbus_g_proxy_begin_call().  This function
 * may not be used on pending calls that have already been
 * ended with dbus_g_proxy_end_call.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 */
void
dbus_g_proxy_cancel_call (DBusGProxy        *proxy,
			  DBusGProxyCall    *call)
{
  guint call_id;
  DBusPendingCall *pending;
  DBusGProxyPrivate *priv;
  
  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  call_id = DBUS_G_PROXY_CALL_TO_ID (call);

  if (call_id == 0)
    {
      /* nothing to cancel */
      return;
    }

  pending = g_hash_table_lookup (priv->pending_calls, GUINT_TO_POINTER (call_id));
  g_hash_table_remove (priv->pending_calls, GUINT_TO_POINTER (call_id));
  g_return_if_fail (pending != NULL);

  dbus_pending_call_cancel (pending);
}

/**
 * dbus_g_proxy_send:
 * @proxy: a proxy for a remote interface
 * @message: the message to address and send
 * @client_serial: return location for message's serial, or %NULL
 *
 * Sends a message to the interface we're proxying for.  Does not
 * block or wait for a reply. The message is only actually written out
 * when you return to the main loop or block in
 * dbus_g_connection_flush().
 *
 * The message is modified to be addressed to the target interface.
 * That is, a destination name field or whatever is needed will be
 * added to the message. The basic point of this function is to add
 * the necessary header fields, otherwise it's equivalent to
 * dbus_connection_send().
 *
 * This function adds a reference to the message, so the caller
 * still owns its original reference.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 */
void
dbus_g_proxy_send (DBusGProxy          *proxy,
                   DBusMessage         *message,
                   dbus_uint32_t       *client_serial)
{
  DBusGProxyPrivate *priv;
  
  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));
  
  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  
  if (priv->name)
    {
      if (!dbus_message_set_destination (message, priv->name))
        g_error ("Out of memory");
    }
  if (priv->path)
    {
      if (!dbus_message_set_path (message, priv->path))
        g_error ("Out of memory");
    }
  if (priv->interface)
    {
      if (!dbus_message_set_interface (message, priv->interface))
        g_error ("Out of memory");
    }
  
  if (!dbus_connection_send (priv->manager->connection, message, client_serial))
    g_error ("Out of memory\n");
}

static void
array_free_all (gpointer array)
{
  g_array_free (array, TRUE);
}

/**
 * dbus_g_proxy_add_signal:
 * @proxy: the proxy for a remote interface
 * @signal_name: the name of the signal
 * @first_type: the first argument type, or %G_TYPE_INVALID if none
 * @...: any subsequent argument types, followed by %G_TYPE_INVALID
 *
 * Specifies the argument signature of a D-Bus signal. When the signal is
 * emitted by the remote object, if the GTypes corresponding to its arguments'
 * types do not match the types given here, the signal will be ignored.
 *
 * It is an error to add the same @signal_name to the same @proxy more than
 * once, even if the argument types given are the same.
 *
 * It is also an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 */
void
dbus_g_proxy_add_signal  (DBusGProxy        *proxy,
                          const char        *signal_name,
			  GType              first_type,
                          ...)
{
  GQuark q;
  char *name;
  GArray *gtypesig;
  GType gtype;
  va_list args;
  DBusGProxyPrivate *priv;

  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));
  g_return_if_fail (g_dbus_is_member_name (signal_name));

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);

  name = create_signal_name (priv->interface, signal_name);
  
  q = g_quark_from_string (name);
  
  g_return_if_fail (g_datalist_id_get_data (&priv->signal_signatures, q) == NULL);

  gtypesig = g_array_new (FALSE, TRUE, sizeof (GType));

  va_start (args, first_type);
  gtype = first_type;
  while (gtype != G_TYPE_INVALID)
    {
      g_array_append_val (gtypesig, gtype);
      gtype = va_arg (args, GType);
    }
  va_end (args);

#ifndef G_DISABLE_CHECKS
  if (_dbus_gobject_lookup_marshaller (G_TYPE_NONE, gtypesig->len, (const GType*) gtypesig->data) == NULL)
    g_warning ("No marshaller for signature of signal '%s'", signal_name);
#endif

  
  g_datalist_id_set_data_full (&priv->signal_signatures,
                               q, gtypesig,
                               array_free_all);

  g_free (name);
}

/**
 * dbus_g_proxy_connect_signal:
 * @proxy: a proxy for a remote interface
 * @signal_name: the DBus signal name to listen for
 * @handler: the handler to connect
 * @data: data to pass to handler
 * @free_data_func: callback function to destroy data
 *
 * Connect a signal handler to a proxy for a remote interface.  When
 * the remote interface emits the specified signal, the proxy will
 * emit a corresponding GLib signal.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 */
void
dbus_g_proxy_connect_signal (DBusGProxy             *proxy,
                             const char             *signal_name,
                             GCallback               handler,
                             void                   *data,
                             GClosureNotify          free_data_func)
{
  char *name;
  GClosure *closure;
  GQuark q;
  DBusGProxyPrivate *priv;

  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));
  g_return_if_fail (g_dbus_is_member_name (signal_name));
  g_return_if_fail (handler != NULL);
  
  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  name = create_signal_name (priv->interface, signal_name);

  q = g_quark_try_string (name);

#ifndef G_DISABLE_CHECKS
  if (q == 0 || g_datalist_id_get_data (&priv->signal_signatures, q) == NULL)
    {
      g_warning ("Must add the signal '%s' with dbus_g_proxy_add_signal() prior to connecting to it\n", name);
      g_free (name);
      return;
    }
#endif
  
  closure = g_cclosure_new (G_CALLBACK (handler), data, free_data_func);
  
  g_signal_connect_closure_by_id (G_OBJECT (proxy),
                                  signals[RECEIVED],
                                  q,
                                  closure, FALSE);
  
  g_free (name);
}

/**
 * dbus_g_proxy_disconnect_signal:
 * @proxy: a proxy for a remote interface
 * @signal_name: the DBus signal name to disconnect
 * @handler: the handler to disconnect
 * @data: the data that was registered with handler
 *
 * Disconnect all signal handlers from a proxy that match the given
 * criteria.
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 */
void
dbus_g_proxy_disconnect_signal (DBusGProxy             *proxy,
                                const char             *signal_name,
                                GCallback               handler,
                                void                   *data)
{
  char *name;
  GQuark q;
  DBusGProxyPrivate *priv;
  
  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));
  g_return_if_fail (g_dbus_is_member_name (signal_name));
  g_return_if_fail (handler != NULL);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  name = create_signal_name (priv->interface, signal_name);

  q = g_quark_try_string (name);
  
  if (q != 0)
    {
      g_signal_handlers_disconnect_matched (G_OBJECT (proxy),
                                            G_SIGNAL_MATCH_DETAIL |
                                            G_SIGNAL_MATCH_FUNC   |
                                            G_SIGNAL_MATCH_DATA,
                                            signals[RECEIVED],
                                            q,
                                            NULL,
                                            G_CALLBACK (handler), data);
    }
  else
    {
      g_warning ("Attempt to disconnect from signal '%s' which is not registered\n",
                 name);
    }

  g_free (name);
}

/**
 * dbus_g_proxy_set_default_timeout:
 * @proxy: a proxy for a remote interface
 * @timeout: the timeout in milliseconds, or -1 to reset to the libdbus default
 *
 * Sets the default timeout to use for a proxy. This timeout will be
 * used in calls where the timeout is not specified, or is specified to be -1.
 * If this timeout is also set to -1, libdbus will use a reasonable default
 * value.
 *
 * This is useful for long-running operations that takes longer than
 * the default timeout (which is a on the order of magnitude of tens
 * of seconds). For some applications, consider using a pattern where
 * the method returns once the operation is underway
 * (e.g. immediately) and emits a signal when the operation terminates
 * (though beware of leaking information with/in the signal return value).
 *
 * It is an error to call this method on a proxy that has emitted
 * the #DBusGProxy::destroy signal.
 *
 * Since: 0.75
 */
void
dbus_g_proxy_set_default_timeout (DBusGProxy        *proxy,
                                  int                timeout)
{
  DBusGProxyPrivate *priv;

  g_return_if_fail (DBUS_IS_G_PROXY (proxy));
  g_return_if_fail (!DBUS_G_PROXY_DESTROYED (proxy));
  g_return_if_fail (timeout >= 0 || timeout == -1);

  priv = DBUS_G_PROXY_GET_PRIVATE(proxy);
  priv->default_timeout = timeout;
}
