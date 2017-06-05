/*
 * $Id: addressbook-dbus.h,v 1.1.4.3 2012-05-27 17:30:48 claws Exp $
 */
/* vim:et:ts=4:sw=4:et:sts=4:ai:set list listchars=tab\:��,trail\:�: */

/*
 * Claws-contacts is a proposed new design for the address book feature
 * in Claws Mail. The goal for this new design was to create a
 * solution more suitable for the term lightweight and to be more
 * maintainable than the present implementation.
 *
 * More lightweight is achieved by design, in that sence that the whole
 * structure is based on a plugable design.
 *
 * Claws Mail is Copyright (C) 1999-2012 by the Claws Mail Team and
 * Claws-contacts is Copyright (C) 2011 by Michael Rasmussen.
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

#ifndef __ADDRESSBOOK_DBUS_H__
#define __ADDRESSBOOK_DBUS_H__

#include <glib.h>

G_BEGIN_DECLS

#include <gtk/gtk.h>
#include "folder.h"
#include "compose.h"

typedef struct {
	gchar*		cn;
	gchar*		email;
	gchar*		remarks;
	gchar*		name;
	gchar*		book;
	GdkPixbuf* picture;
} ContactData;

gboolean addressbook_start_service(GError** error);
void addressbook_install_hooks(GError** error);
int addressbook_dbus_add_contact(ContactData* contact, GError** error);
gboolean addrindex_dbus_load_completion(gint (*callBackFunc)
										(const gchar* name,
										 const gchar* address,
										 const gchar* nick,
										 const gchar* alias,
										 GList* grp_emails),
										 GError** error);
void addressbook_dbus_open(gboolean compose, GError** error);
GSList* addressbook_dbus_get_books(GError** error);
void contact_data_free(ContactData** data);
void addressbook_harvest(FolderItem *folderItem,
						 gboolean sourceInd,
						 GList *msgList );
void addressbook_connect_signals(Compose* compose);
gchar* addressbook_get_vcard(const gchar* account, GError** error);
gboolean addressbook_add_vcard(const gchar* abook, const gchar* vcard, GError** error);

G_END_DECLS

#endif
