/* Copyright (C) 2006,2007 Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2008 by AbiSource Corporation B.V.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "ServiceUnixAccountHandler.h"

AccountHandlerConstructor ServiceAccountHandlerConstructor = &ServiceUnixAccountHandler::static_constructor;

AccountHandler * ServiceUnixAccountHandler::static_constructor()
{
	return static_cast<AccountHandler *>(new ServiceUnixAccountHandler());
}

ServiceUnixAccountHandler::ServiceUnixAccountHandler()
	: ServiceAccountHandler(),
	table(NULL),
	username_entry(NULL),
	password_entry(NULL),
	autoconnect_button(NULL)
#if DEBUG
	, uri_entry(NULL),
	verify_webapp_host_button(NULL),
	verify_realm_host_button(NULL)
#endif
{
}

void ServiceUnixAccountHandler::embedDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("ServiceUnixAccountHandler::embedDialogWidgets()\n"));
	UT_return_if_fail(pEmbeddingParent);

	table = gtk_table_new(2, 2, FALSE);
	GtkVBox* parent = (GtkVBox*)pEmbeddingParent;

	// username	
	GtkWidget* username_label = gtk_label_new("E-mail address:");
	gtk_misc_set_alignment(GTK_MISC(username_label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), username_label, 0, 1, 0, 1);
	username_entry = gtk_entry_new();
	gtk_table_attach_defaults(GTK_TABLE(table), username_entry, 1, 2, 0, 1);

	// password
	GtkWidget* password_label = gtk_label_new("Password:");
	gtk_misc_set_alignment(GTK_MISC(password_label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), password_label, 0, 1, 1, 2);
	password_entry = gtk_entry_new();
	gtk_entry_set_visibility(GTK_ENTRY(password_entry), false);
	gtk_table_attach_defaults(GTK_TABLE(table), password_entry, 1, 2, 1, 2);
	
	// autoconnect
	autoconnect_button = gtk_check_button_new_with_label ("Connect on application startup");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(autoconnect_button), true);
	gtk_table_attach_defaults(GTK_TABLE(table), autoconnect_button, 0, 2, 4, 5);

#ifdef DEBUG
	// uri	
	GtkWidget* uri_label = gtk_label_new("WebApp SOAP url:");
	gtk_misc_set_alignment(GTK_MISC(uri_label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), uri_label, 0, 1, 5, 6);
	uri_entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(uri_entry), "https://abicollab.net/soap/");
	gtk_table_attach_defaults(GTK_TABLE(table), uri_entry, 1, 2, 5, 6);

	// check webapp hostname
	verify_webapp_host_button = gtk_check_button_new_with_label ("Verify WebApp hostname:");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(verify_webapp_host_button), true);
	gtk_table_attach_defaults(GTK_TABLE(table), verify_webapp_host_button, 0, 2, 6, 7);

	// check realm hostname
	verify_realm_host_button = gtk_check_button_new_with_label ("Verify Realm hostname:");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(verify_realm_host_button), false);
	gtk_table_attach_defaults(GTK_TABLE(table), verify_realm_host_button, 0, 2, 7, 8);
#endif
	
	gtk_box_pack_start(GTK_BOX(parent), table, FALSE, TRUE, 0);
	gtk_widget_show_all(GTK_WIDGET(parent));
}

void ServiceUnixAccountHandler::removeDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("ServiceUnixAccountHandler::removeDialogWidgets()\n"));
	UT_return_if_fail(pEmbeddingParent);
	
	// this will conveniently destroy all contained widgets as well
	if (table && GTK_IS_WIDGET(table))
		gtk_widget_destroy(table);
}

void ServiceUnixAccountHandler::storeProperties()
{
	UT_DEBUGMSG(("ServiceUnixAccountHandler::storeProperties()\n"));	

	if (username_entry && GTK_IS_ENTRY(username_entry))
		addProperty("email", gtk_entry_get_text(GTK_ENTRY(username_entry)));

	if (password_entry && GTK_IS_ENTRY(password_entry))
		addProperty("password", gtk_entry_get_text(GTK_ENTRY(password_entry)));
	
	if (autoconnect_button && GTK_IS_TOGGLE_BUTTON(autoconnect_button))
		addProperty("autoconnect", gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(autoconnect_button)) ? "true" : "false" );	

#ifdef DEBUG
	if (uri_entry && GTK_IS_ENTRY(uri_entry))
		addProperty("uri", gtk_entry_get_text(GTK_ENTRY(uri_entry)));

	if (verify_webapp_host_button && GTK_IS_TOGGLE_BUTTON(verify_webapp_host_button))
		addProperty("verify-webapp-host", gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(verify_webapp_host_button)) ? "true" : "false" );

	if (verify_realm_host_button && GTK_IS_TOGGLE_BUTTON(verify_realm_host_button))
		addProperty("verify-realm-host", gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(verify_realm_host_button)) ? "true" : "false" );

	addProperty("verify-realm-host", "false");
#else
	addProperty("uri", "https://abicollab.net/soap/");
	addProperty("verify-webapp-host", "true");
	addProperty("verify-realm-host", "false");
#endif
}
