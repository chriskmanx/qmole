/* GIO - GLib Input, Output and Streaming Library
 *
 * Copyright (C) 2010 Collabora, Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Author: Nicolas Dufresne <nicolas.dufresne@collabora.co.uk>
 */

#include "config.h"

#include <proxy.h>
#include <stdlib.h>
#include <string.h>

#include "glibproxyresolver.h"

#include <glib.h>
#include <glib/gi18n-lib.h>

struct _GLibProxyResolver {
  GObject parent_instance;
  pxProxyFactory *factory;
};

static void g_libproxy_resolver_iface_init (GProxyResolverInterface *iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (GLibProxyResolver,
				g_libproxy_resolver,
				G_TYPE_OBJECT, 0,
				G_IMPLEMENT_INTERFACE_DYNAMIC (G_TYPE_PROXY_RESOLVER,
							       g_libproxy_resolver_iface_init))

static void  g_libproxy_resolver_class_finalize (GLibProxyResolverClass *klass)
{
}

static void
g_libproxy_resolver_finalize (GObject *object)
{
  GLibProxyResolver *resolver = G_LIBPROXY_RESOLVER (object);
  
  if (resolver->factory)
    {
      px_proxy_factory_free (resolver->factory);
      resolver->factory = NULL;
    }

  /* must chain up */
  G_OBJECT_CLASS (g_libproxy_resolver_parent_class)->finalize (object);
}

static void
g_libproxy_resolver_init (GLibProxyResolver *resolver)
{
  resolver->factory = px_proxy_factory_new ();
}

static gboolean
g_libproxy_resolver_is_supported (GProxyResolver *object)
{
  GLibProxyResolver *resolver = G_LIBPROXY_RESOLVER (object);
  return resolver->factory != NULL;
}

static gchar **
copy_proxies (gchar **proxies)
{
  gchar **copy;
  int len = 0;
  int i, j;

  for (i = 0; proxies[i]; i++)
    {
      if (!strncmp ("socks://", proxies[i], 8))
        len += 3;
      else
        len++;
    }

  copy = g_new (gchar *, len + 1);
  for (i = j = 0; proxies[i]; i++, j++)
    {
      if (!strncmp ("socks://", proxies[i], 8))
        {
          copy[j++] = g_strdup_printf ("socks5://%s", proxies[i] + 8);
          copy[j++] = g_strdup_printf ("socks4a://%s", proxies[i] + 8);
          copy[j] = g_strdup_printf ("socks4://%s", proxies[i] + 8);
        }
      else
        {
          copy[j] = g_strdup (proxies[i]);
        }
    }
  copy[j] = NULL;

  return copy;
}

static void
free_libproxy_proxies (gchar **proxies)
{
  int i;

  for (i = 0; proxies[i]; i++)
    free (proxies[i]);
  free (proxies);
}

static gchar **
get_libproxy_proxies (GLibProxyResolver  *resolver,
		      const gchar        *uri,
		      GCancellable       *cancellable,
		      GError            **error)
{
  gchar **proxies;

  /* FIXME: this is not really cancellable; to do it right we'd
   * need to run this function in a thread pool like GThreadedResolver.
   */

  if (g_cancellable_set_error_if_cancelled (cancellable, error))
    return NULL;

  proxies = px_proxy_factory_get_proxies (resolver->factory, uri);
  if (!proxies)
    {
      g_set_error_literal (error, G_IO_ERROR, G_IO_ERROR_FAILED,
			   _("Proxy resolver internal error."));
    }

  return proxies;
}

static gchar **
g_libproxy_resolver_lookup (GProxyResolver  *iresolver,
			    const gchar     *uri,
			    GCancellable    *cancellable,
			    GError         **error)
{
  GLibProxyResolver *resolver;
  gchar **proxies;

  g_return_val_if_fail (G_IS_LIBPROXY_RESOLVER (iresolver), NULL);
  g_return_val_if_fail (uri != NULL, NULL);

  resolver = G_LIBPROXY_RESOLVER (iresolver);

  proxies = get_libproxy_proxies (resolver, uri, cancellable, error);

  /* We always copy to be able to translate "socks" entry into
   * three entries ("socks5", "socks4a", "socks4").
   */
  if (proxies)
    {
      gchar **copy;

      copy = copy_proxies (proxies);
      free_libproxy_proxies (proxies);
      proxies = copy;
    }

  return proxies;
}

static void
_lookup_async (GSimpleAsyncResult *simple,
	       GObject *object,
	       GCancellable *cancellable)
{
  GLibProxyResolver *resolver = G_LIBPROXY_RESOLVER (object);
  GError *error = NULL;
  gchar **proxies = NULL;
  gchar *uri;

  uri = g_simple_async_result_get_op_res_gpointer (simple);

  proxies = get_libproxy_proxies (resolver, uri, cancellable, &error);

  if (error)
    {
      g_simple_async_result_set_from_error (simple, error);
      g_error_free (error);
    }
  else
    g_simple_async_result_set_op_res_gpointer (simple, proxies, (GDestroyNotify)free_libproxy_proxies);
}

static void
g_libproxy_resolver_lookup_async (GProxyResolver      *resolver,
				  const gchar         *uri,
				  GCancellable        *cancellable,
				  GAsyncReadyCallback  callback,
				  gpointer             user_data)
{
  GSimpleAsyncResult *simple;

  simple = g_simple_async_result_new (G_OBJECT (resolver),
				      callback, user_data,
				      g_libproxy_resolver_lookup_async);
  g_simple_async_result_set_op_res_gpointer (simple,
					     g_strdup (uri),
					     (GDestroyNotify) g_free);
  g_simple_async_result_run_in_thread (simple, _lookup_async,
				       G_PRIORITY_DEFAULT, cancellable);
  g_object_unref (simple);
}

static gchar **
g_libproxy_resolver_lookup_finish (GProxyResolver     *resolver,
				   GAsyncResult       *result,
				   GError            **error)
{
  GSimpleAsyncResult *simple;
  gchar **proxies;

  g_return_val_if_fail (g_simple_async_result_is_valid (result, G_OBJECT (resolver), g_libproxy_resolver_lookup_async), NULL);

  simple = G_SIMPLE_ASYNC_RESULT (result);

  if (g_simple_async_result_propagate_error (simple, error))
    return NULL;

  proxies = g_simple_async_result_get_op_res_gpointer (simple);
  return copy_proxies (proxies);
}

static void
g_libproxy_resolver_class_init (GLibProxyResolverClass *resolver_class)
{
  GObjectClass *object_class;
  
  object_class = G_OBJECT_CLASS (resolver_class);
  object_class->finalize = g_libproxy_resolver_finalize;
}

static void
g_libproxy_resolver_iface_init (GProxyResolverInterface *iface)
{
  iface->is_supported = g_libproxy_resolver_is_supported;
  iface->lookup = g_libproxy_resolver_lookup;
  iface->lookup_async = g_libproxy_resolver_lookup_async;
  iface->lookup_finish = g_libproxy_resolver_lookup_finish;
}

void
g_libproxy_resolver_register (GIOModule *module)
{
  g_libproxy_resolver_register_type (G_TYPE_MODULE (module));
  g_io_extension_point_implement (G_PROXY_RESOLVER_EXTENSION_POINT_NAME,
				  g_libproxy_resolver_get_type(),
				  "libproxy",
				  0);
}
