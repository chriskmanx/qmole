/*
 * $Id: addressbook-dbus.c,v 1.1.4.3 2012-05-27 17:30:48 claws Exp $
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

#ifdef HAVE_CONFIG_H
#       include <config.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <dbus/dbus.h>
#include <dbus/dbus-glib-bindings.h>
#include "dbus-contact.h"
#include "addrgather.h"
#include "folder.h"
#include "compose.h"
#include "hooks.h"

#include "addressbook-dbus.h"
#include "client-bindings.h"

static DBusGProxy* proxy = NULL;
static DBusGConnection* connection = NULL;
static Compose* compose_instance = NULL;

static GQuark client_object_error_quark() {
        static GQuark quark = 0;
        if (!quark)
                quark = g_quark_from_static_string ("client_object_error");

        return quark;
}

static gboolean init(GError** error) {
    connection = dbus_g_bus_get (DBUS_BUS_SESSION, error);
    if (connection == NULL || *error) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Unable to connect to dbus");
        g_warning("Unable to connect to dbus: %s\n", (*error)->message);
        return FALSE;
    }
    
    proxy = dbus_g_proxy_new_for_name (connection,
            "org.clawsmail.Contacts",
            "/org/clawsmail/contacts",
            "org.clawsmail.Contacts");
    if (proxy == NULL) {
        g_warning("Could not get a proxy object\n");
        g_set_error(error, client_object_error_quark(), 1, "Could not get a proxy object");
        return FALSE;
    }
    
    return TRUE;
}

static void dbus_contact_free(const DBusContact* contact) {
    g_hash_table_destroy(contact->data);
    g_ptr_array_free(contact->emails, TRUE);
}

static GHashTable* hash_table_new(void) {
	GHashTable* hash_table;
	
	hash_table = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
		
	return hash_table;
}

static void g_value_email_free(gpointer data) {
	GValueArray* email = (GValueArray *) data;
	GValue* email_member;
	guint i;
	
	for (i = 0; i < email->n_values; i++) {
		email_member = g_value_array_get_nth(email, i);
		g_value_unset(email_member);
	}
}

static GPtrArray* g_value_email_new() {
	return g_ptr_array_new_with_free_func(g_value_email_free);
}

static gchar* convert_2_utf8(gchar* locale) {
    gsize read, write;
    GError* error = NULL;
    gchar *current, *utf8;
    const gchar* charset;

    if (g_get_charset(&charset) || g_utf8_validate(locale, -1, 0))
        return g_strdup(locale);

    if (strcmp("ANSI_X3.4-1968", charset) == 0)
        current = g_strdup("ISO-8859-1");
    else
        current = g_strdup(charset);

    utf8 = g_convert(locale, -1, "UTF-8", current, &read, &write, &error);
    if (error) {
        g_warning("Fail to convert [%s]: %s\n", charset, error->message);
        g_free(current);
        return NULL;
    }
    g_free(current);

    return utf8;
}

static void format_contact(DBusContact* contact, ContactData* c) {
    gchar* firstname;
    gchar* lastname;
    GValueArray* email = NULL;
    GValue email_member = {0};
    gchar* str;   
	gchar* image = NULL;
	gsize size;
    
    contact->data = hash_table_new();
    contact->emails = g_value_email_new();
    firstname = lastname = NULL;
    
    if (c->name) {
        gchar* pos = strchr(c->name, ' ');
        if (pos) {
            firstname = g_strndup(c->name, pos - c->name);
            lastname = g_strdup(++pos);
            g_hash_table_replace(contact->data,
                g_strdup("first-name"), convert_2_utf8(firstname));
            g_hash_table_replace(contact->data,
                g_strdup("last-name"), convert_2_utf8(lastname));
        }
        else {
            lastname = g_strdup(c->name);
            g_hash_table_replace(contact->data,
                g_strdup("last-name"), convert_2_utf8(lastname));
        }
        g_free(firstname);
        g_free(lastname);
    }
	if (c->cn) {
		g_hash_table_replace(contact->data,
			g_strdup("cn"), convert_2_utf8(c->cn));
	}

	if (c->picture) {
		gdk_pixbuf_save_to_buffer(
			c->picture, &image, &size, "png", NULL, NULL);
		g_hash_table_replace(contact->data,
			g_strdup("image"), g_base64_encode((const guchar *) image, size));
	}
	
    email = g_value_array_new(0);

	/* Alias is not available but needed so make an empty string */
    g_value_init(&email_member, G_TYPE_STRING);
    g_value_set_string(&email_member, "");
    g_value_array_append(email, &email_member);
    g_value_unset(&email_member);

    if (c->email)
        str = convert_2_utf8(c->email);
    else
        str = g_strdup("");

    g_value_init(&email_member, G_TYPE_STRING);
    g_value_set_string(&email_member, str);
    g_value_array_append(email, &email_member);
    g_value_unset(&email_member);
    g_free(str);

    if (c->remarks)
        str = convert_2_utf8(c->remarks);
    else
        str = g_strdup("");
    
    g_value_init(&email_member, G_TYPE_STRING);
    g_value_set_string(&email_member, str);
    g_value_array_append(email, &email_member);
    g_value_unset(&email_member);
    g_free(str);

    g_ptr_array_add(contact->emails, email);
}

static DBusHandlerResult contact_add_signal(DBusConnection* bus,
							   				DBusMessage* message,
							   				gpointer data) {
	DBusError error;
    gchar *s = NULL;

	if (! compose_instance) {
		g_message("Missing compose instance\n");
		return DBUS_HANDLER_RESULT_HANDLED;
	}

   	dbus_error_init (&error);

	if (dbus_message_is_signal(message, "org.clawsmail.Contacts", "ContactMailTo")) {
    	if (dbus_message_get_args(
				message, &error, DBUS_TYPE_STRING, &s, DBUS_TYPE_INVALID)) {
      		debug_print("ContactMailTo address received: %s\n", s);
			compose_entry_append(compose_instance, s, COMPOSE_TO, PREF_NONE);
    	}
		else {
      		debug_print("ContactMailTo received with error: %s\n", error.message);
      		dbus_error_free(&error);
    	}
	}
	else if (dbus_message_is_signal(message, "org.clawsmail.Contacts", "ContactMailCc")) {
    	if (dbus_message_get_args(
				message, &error, DBUS_TYPE_STRING, &s, DBUS_TYPE_INVALID)) {
      		debug_print("ContactMailTo address received: %s\n", s);
			compose_entry_append(compose_instance, s, COMPOSE_CC, PREF_NONE);
    	}
		else {
      		debug_print("ContactMailTo received with error: %s\n", error.message);
      		dbus_error_free(&error);
    	}
	}
	else if (dbus_message_is_signal(message, "org.clawsmail.Contacts", "ContactMailBcc")) {
    	if (dbus_message_get_args(
				message, &error, DBUS_TYPE_STRING, &s, DBUS_TYPE_INVALID)) {
      		debug_print("ContactMailTo address received: %s\n", s);
			compose_entry_append(compose_instance, s, COMPOSE_BCC, PREF_NONE);
    	}
		else {
      		debug_print("ContactMailTo received with error: %s\n", error.message);
      		dbus_error_free(&error);
    	}
	}
	else {
		if (error.message) {
			g_warning("Reception error: %s", error.message);
			dbus_error_free(&error);
		}
		debug_print("Unhandled signal received\n");
		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	}

	return DBUS_HANDLER_RESULT_HANDLED;
}

gboolean addressbook_start_service(GError** error) {
	gchar* reply = NULL;
	gboolean result = FALSE;
	
	if (! init(error))
		return result;

	if (!org_clawsmail_Contacts_ping(proxy, &reply, error)) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Woops remote method failed");
		g_warning ("Woops remote method failed: %s", (*error)->message);
	}
	if (reply && strcmp("PONG", reply) == 0)
		result = TRUE;
		
	return result;
}

int addressbook_dbus_add_contact(ContactData* contact, GError** error) {
	DBusContact dbus_contact;
	
	if (! init(error))
		return -1;

	format_contact(&dbus_contact, contact);
	if (!org_clawsmail_Contacts_add_contact(
		proxy, contact->book, dbus_contact.data, dbus_contact.emails, error)) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Woops remote method failed");
		g_warning ("Woops remote method failed: %s", (*error)->message);
		dbus_contact_free(&dbus_contact);
		return -1;
	}
	dbus_contact_free(&dbus_contact);
	return 0;
}

gboolean addrindex_dbus_load_completion(gint (*callBackFunc)
										(const gchar* name,
										 const gchar* address,
										 const gchar* nick,
										 const gchar* alias,
										 GList* grp_emails),
										 GError** error) {
	gchar **list = NULL, **contacts;
	gchar *name, *email;
	
	if (! init(error))
		return FALSE;

	if (!org_clawsmail_Contacts_search_addressbook(
			proxy, "*", NULL, &list, error)) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Woops remote method failed");
		g_warning ("Woops remote method failed: %s", (*error)->message);
	    g_strfreev(list);
	    return FALSE;
    }
	for (contacts = list; *contacts != NULL; contacts += 1) {
		gchar* tmp = g_strdup(*contacts);
		gchar* pos = g_strrstr(tmp, "\"");
		if (pos) {
		    /* Contact has a name as part of email address */
		    *pos = '\0';
		    name = tmp;
		    name += 1;
		    pos += 3;
		    email = pos;
		    pos = g_strrstr(email, ">");
		    if (pos)
			*pos = '\0';
		}
		else {
		    name = "";
		    email = tmp;
		}
		debug_print("Adding: %s <%s> to completition\n", name, email);
		callBackFunc(name, email, NULL, NULL, NULL);
		g_free(tmp);
	}

    return TRUE;
}

void addressbook_dbus_open(gboolean compose, GError** error) {
	if (! init(error))
		return;

	if (!org_clawsmail_Contacts_show_addressbook(proxy, compose, error)) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Woops remote method failed");
		g_warning ("Woops remote method failed: %s", (*error)->message);
	}
}

GSList* addressbook_dbus_get_books(GError** error) {
	gchar **book_names = NULL, **cur;
	GSList* books = NULL;
	
	if (! init(error)) {
		return books;
	}
	
	if (!org_clawsmail_Contacts_book_list(proxy, &book_names, error)) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Woops remote method failed");
		g_warning ("Woops remote method failed: %s", (*error)->message);
		g_strfreev(book_names);
		return books;
	}
	for (cur = book_names; *cur; cur += 1)
		books = g_slist_prepend(books, g_strdup(*cur));
	
	g_strfreev(book_names);
	
	return books;
}

void contact_data_free(ContactData** data) {
	ContactData* contact;
	
	if (! data && ! *data)
		return;
		
	contact = *data;
	g_free(contact->cn);
	g_free(contact->email);
	g_free(contact->remarks);
	g_free(contact->name);
	g_free(contact->book);
	g_free(contact);
	contact = NULL;
}

void addressbook_harvest(FolderItem *folderItem,
						 gboolean sourceInd,
						 GList *msgList ) {
	addrgather_dlg_execute(folderItem, sourceInd, msgList);
}

void addressbook_connect_signals(Compose* compose) {
	DBusConnection* bus;
  	DBusError* error = NULL;
  	
	g_return_if_fail(compose != NULL);
	
	bus = dbus_bus_get (DBUS_BUS_SESSION, error);
  	if (!bus) {
    	g_warning ("Failed to connect to the D-BUS daemon: %s", error->message);
    	dbus_error_free(error);
    	return;
  	}
	
  	debug_print("Compose: %p\n", compose);
	compose_instance = compose;
	dbus_bus_add_match(bus, "type='signal',interface='org.clawsmail.Contacts'", error);
	if (error) {
    	debug_print("Failed to add match to the D-BUS daemon: %s", error->message);
    	dbus_error_free(error);
    	return;
	}
	dbus_connection_add_filter(bus, contact_add_signal, NULL, NULL);
}

gchar* addressbook_get_vcard(const gchar* account, GError** error) {
	gchar* vcard = NULL;
	
	g_return_val_if_fail(account != NULL, vcard);
	
	if (! init(error)) {
		return vcard;
	}
	
	if (!org_clawsmail_Contacts_get_vcard(proxy, account, &vcard, error)) {
		if (! *error)
			g_set_error(error, client_object_error_quark(), 1, "Woops remote method failed");
		g_warning ("Woops remote method failed: %s", (*error)->message);
		g_free(vcard);
		vcard = NULL;
	}
	
	return vcard;
}

gboolean addressbook_add_vcard(const gchar* abook, const gchar* vcard, GError** error) {
	gboolean result = FALSE;
	
	return result;
}

static gboolean my_compose_create_hook(gpointer source, gpointer user_data) {
	Compose *compose = (Compose*) source;
	GError* error = NULL;

	gchar* vcard = addressbook_get_vcard("test", &error);
	if (error) {
		g_warning("%s", error->message);
		g_clear_error(&error);
	}
	else {
		debug_print("test.vcf:\n%s\n", vcard);
		g_free(vcard);
	}
	
	return FALSE;
}

void addressbook_install_hooks(GError** error) {
	if ((guint)-1 == hooks_register_hook(
			COMPOSE_CREATED_HOOKLIST, my_compose_create_hook, NULL)) {
		g_warning("Could not register hook for adding vCards\n");
		if (error) {
			g_set_error(error, client_object_error_quark(), 1,
				"Could not register hook for adding vCards");
		}
	}
}
