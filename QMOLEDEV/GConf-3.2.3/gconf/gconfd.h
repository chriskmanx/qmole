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

#ifndef GCONF_GCONFD_H
#define GCONF_GCONFD_H

#include <glib.h>

G_BEGIN_DECLS

#include "gconf-error.h"

#ifdef HAVE_CORBA
#include "GConfX.h"
#endif

#include "gconf-database.h"

#ifdef HAVE_CORBA
PortableServer_POA gconf_get_poa (void);

/* return TRUE if the exception was set, clear err if needed */
gboolean gconf_set_exception (GError** err, CORBA_Environment* ev);

gboolean gconfd_logfile_change_listener (GConfDatabase *db,
                                         gboolean add,
                                         guint connection_id,
                                         ConfigListener listener,
                                         const gchar *where,
                                         GError **err);

gboolean gconfd_check_in_shutdown (CORBA_Environment *ev);
#endif

void gconfd_notify_other_listeners (GConfDatabase *modified_db,
				    GConfSources  *modified_sources,
                                    const char    *key);

void
gconfd_clear_cache_for_sources (GConfSources *sources);


void     gconfd_need_log_cleanup (void);
void     gconfd_main_quit        (void);
gboolean gconfd_in_shutdown      (void);

GConfDatabase* gconfd_obtain_database (GSList  *addresses,
                                       GError **err);

G_END_DECLS

#endif



