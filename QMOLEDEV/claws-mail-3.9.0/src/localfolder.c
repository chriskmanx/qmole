/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 by the Claws Mail Team and Hiroyuki Yamamoto
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include <glib.h>

#include "folder.h"
#include "localfolder.h"
#include "xml.h"
#include "utils.h"

void folder_local_folder_init(Folder *folder, const gchar *name,
			      const gchar *path)
{
	folder_init(folder, name);
	LOCAL_FOLDER(folder)->rootpath = g_strdup(path);
}

void folder_local_folder_destroy(LocalFolder *lfolder)
{
	cm_return_if_fail(lfolder != NULL);

	g_free(lfolder->rootpath);
}

void folder_local_set_xml(Folder *_folder, XMLTag *tag)
{
	LocalFolder *folder = LOCAL_FOLDER(_folder);
	GList *cur;

	folder_set_xml(_folder, tag);

	for (cur = tag->attr; cur != NULL; cur = g_list_next(cur)) {
		XMLAttr *attr = (XMLAttr *) cur->data;

		if (!attr || !attr->name || !attr->value) continue;
		if (!strcmp(attr->name, "path")) {
			g_free(folder->rootpath);
			folder->rootpath = g_strdup(attr->value);
		}
	}
}

XMLTag *folder_local_get_xml(Folder *_folder)
{
	LocalFolder *folder = LOCAL_FOLDER(_folder);
	XMLTag *tag;

	tag = folder_get_xml(_folder);

	xml_tag_add_attr(tag, xml_attr_new("path", folder->rootpath));

	return tag;
}
