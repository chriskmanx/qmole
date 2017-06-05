/* GIO - GLib Input, Output and Streaming Library
 *
 * Copyright (C) 2010 Red Hat, Inc.
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
 */

#include "config.h"

#include <gio/gio.h>

#include "gtlsbackend-gnutls.h"


void
g_io_module_load (GIOModule *module)
{
  g_tls_backend_gnutls_register (module);
}

void
g_io_module_unload (GIOModule *module)
{
}

gchar **
g_io_module_query (void)
{
  gchar *eps[] = {
    G_TLS_BACKEND_EXTENSION_POINT_NAME,
    NULL
  };
  return g_strdupv (eps);
}
