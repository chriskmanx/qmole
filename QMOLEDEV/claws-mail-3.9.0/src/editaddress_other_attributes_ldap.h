/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Michael Rasmussen and the Claws Mail team
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

/*
 * Edit address item data.
 */

#ifndef __EDITADDRESS_OTHER_ATTRIBUTES_LDAP_H__
#define __EDITADDRESS_OTHER_ATTRIBUTES_LDAP_H__

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

typedef struct _PersonEdit_dlg PersonEditDlg;
struct _PersonEdit_dlg {
	GtkWidget *container;
	GtkWidget *notebook;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *statusbar;	/* used when prefs_common.addressbook_use_editaddress_dialog is TRUE */
	GtkWidget *title;	/* used when prefs_common.addressbook_use_editaddress_dialog is FALSE */
	gint status_cid;

	/* User data tab */
	GtkWidget *image;
	gboolean picture_set;
	GtkWidget *entry_name;
	GtkWidget *entry_first;
	GtkWidget *entry_last;
	GtkWidget *entry_nick;

	/* EMail data tab */
	GtkWidget *entry_email;
	GtkWidget *entry_alias;
	GtkWidget *entry_remarks;
	GtkWidget *clist_email;
	GtkWidget *email_up;
	GtkWidget *email_down;
	GtkWidget *email_del;
	GtkWidget *email_mod;
	GtkWidget *email_add;

	/* Attribute data tab */
	GtkWidget *entry_atname;
	GtkWidget *entry_atvalue;
	GtkWidget *clist_attrib;
	GtkWidget *attrib_add;
	GtkWidget *attrib_del;
	GtkWidget *attrib_mod;

	gint rowIndEMail;
	gint rowIndAttrib;
	gboolean editNew;
	gboolean read_only;
	gboolean ldap;
};

#ifdef USE_LDAP

static const char *ATTRIBUTE[] = {
	"telephoneNumber",
	/*"description (Remarks)",*/
	"title",
	"telexNumber",
	"facsimileTelephoneNumber",
	"street",
	"postOfficeBox",
	"postalCode",
	"postalAddress",
	"st", /* state or province */
	"l", /* locality Name */
	"departmentNumber",
	"homePhone",
	"homePostalAddress",
	"initials",
	"labeledURI",
	"mobile",
	"pager",
	"roomNumber",
	"jpegPhoto",
	NULL
};

static const int ATTRIBUTE_SIZE = (sizeof(ATTRIBUTE) / sizeof(*ATTRIBUTE)) - 1;

/* Function proto types */
void addressbook_edit_person_page_attrib_ldap(PersonEditDlg *personEditDlg, gint pageNum, gchar *pageLbl);
int get_attribute_index(const gchar *string_literal);

#endif	/* USE_LDAP */

#endif /* __EDITADDRESS_OTHER_ATTRIBUTES_LDAP_H__ */
