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

#ifndef GCONF_XML_CACHE_H
#define GCONF_XML_CACHE_H

#include "gconf/gconf.h"
#include <libxml/tree.h>
#include "xml-dir.h"

typedef struct _Cache Cache;

Cache*   cache_get        (const gchar  *root_dir,
                           guint         dir_mode,
                           guint         file_mode);
void     cache_unref      (Cache        *cache);
gboolean cache_sync       (Cache        *cache,
                           GError      **err);
void     cache_clean      (Cache        *cache,
                           GTime         older_than);
Dir*     cache_lookup     (Cache        *cache,
                           const gchar  *key,
                           gboolean      create_if_missing,
                           GError      **err);

void xml_test_cache (void);

#endif

