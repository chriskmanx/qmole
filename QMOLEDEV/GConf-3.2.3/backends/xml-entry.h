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

#ifndef GCONF_XML_ENTRY_H
#define GCONF_XML_ENTRY_H

#include "gconf/gconf.h"
#include <libxml/tree.h>

/* Entry stores all the information about a given key */

typedef struct _Entry Entry;
Entry*         entry_new             (const gchar  *relative_name);
void           entry_destroy         (Entry        *entry);
const gchar*   entry_get_name        (Entry        *entry);



/* no set_name, you can't change an entry's name */
void           entry_set_node        (Entry        *entry,
                                      xmlNodePtr    node);
xmlNodePtr     entry_get_node        (Entry        *entry);
void           entry_fill_from_node  (Entry        *entry);
void           entry_sync_to_node    (Entry        *entry);
GConfValue*    entry_get_value       (Entry        *entry,
                                      const gchar **locales,
                                      GError  **err);
void           entry_set_value       (Entry        *entry,
                                      const GConfValue *value);
gboolean       entry_unset_value     (Entry        *entry,
                                      const gchar  *locale);
GConfMetaInfo* entry_get_metainfo    (Entry        *entry);
void           entry_set_mod_time    (Entry        *entry,
                                      GTime         mod_time);
void           entry_set_mod_user    (Entry        *e,
                                      const gchar  *user);
const gchar*   entry_get_schema_name (Entry        *e);
void           entry_set_schema_name (Entry        *e,
                                      const gchar  *name);


void my_xmlSetProp(xmlNodePtr node,
                   const gchar* name,
                   const gchar* str);

char* my_xmlGetProp(xmlNodePtr node,
                    const gchar* name);

void xml_test_entry (void);

#endif
