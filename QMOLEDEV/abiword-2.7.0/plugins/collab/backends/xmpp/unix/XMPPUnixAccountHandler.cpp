/* 
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

#include "XMPPUnixAccountHandler.h"

AccountHandlerConstructor XMPPAccountHandlerConstructor = &XMPPUnixAccountHandler::static_constructor;

XMPPUnixAccountHandler::XMPPUnixAccountHandler()
	: XMPPAccountHandler(),
	table(NULL),
	username_entry(NULL),
	password_entry(NULL),
	server_entry(NULL),
	port_entry(NULL),
	autoconnect_button(NULL)
{
}

AccountHandler * XMPPUnixAccountHandler::static_constructor()
{
	return static_cast<AccountHandler *>(new XMPPUnixAccountHandler());
}

void XMPPUnixAccountHandler::embedDialogWidgets(void* pEmbeddingParent)
{
	UT_return_if_fail(pEmbeddingParent);

	table = gtk_table_new(5, 2, FALSE);
	GtkVBox* parent = (GtkVBox*)pEmbeddingParent;
	
	// username	
	GtkWidget* username_label = gtk_label_new("Username:");
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

	// server
	GtkWidget* server_label = gtk_label_new("Server:");
	gtk_misc_set_alignment(GTK_MISC(server_label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), server_label, 0, 1, 2, 3);
	server_entry = gtk_entry_new();
	gtk_table_attach_defaults(GTK_TABLE(table), server_entry, 1, 2, 2, 3);

	// port
	GtkWidget* port_label = gtk_label_new("Port:");
	gtk_misc_set_alignment(GTK_MISC(port_label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), port_label, 0, 1, 3, 4);
	port_entry = gtk_entry_new(); // TODO: should be a numerical entry
	gtk_table_attach_defaults(GTK_TABLE(table), port_entry, 1, 2, 3, 4);
	
	// autoconnect
	autoconnect_button = gtk_check_button_new_with_label ("Connect on application startup");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(autoconnect_button), true);
	gtk_table_attach_defaults(GTK_TABLE(table), autoconnect_button, 0, 2, 4, 5);
	
	gtk_box_pack_start(GTK_BOX(parent), table, false, TRUE, 0);
	gtk_widget_show_all(GTK_WIDGET(parent));
	
	// some convenient default values
	gtk_entry_set_text(GTK_ENTRY(port_entry), "5222");	
}

void XMPPUnixAccountHandler::removeDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("XMPPUnixAccountHandler::removeDialogWidgets\n"));
	
	// this will conveniently destroy all contained widgets as well
	if (table && GTK_IS_WIDGET(table))
		gtk_widget_destroy(table);
}

void XMPPUnixAccountHandler::storeProperties()
{
	if (username_entry && GTK_IS_ENTRY(username_entry))
		addProperty("username", gtk_entry_get_text(GTK_ENTRY(username_entry)));

	if (password_entry && GTK_IS_ENTRY(password_entry))
		addProperty("password", gtk_entry_get_text(GTK_ENTRY(password_entry)));

	if (server_entry && GTK_IS_ENTRY(server_entry))
		addProperty("server", gtk_entry_get_text(GTK_ENTRY(server_entry)));
		
	if (port_entry && GTK_IS_ENTRY(server_entry))
		addProperty("port", gtk_entry_get_text(GTK_ENTRY(port_entry)));	
		
	if (autoconnect_button && GTK_IS_TOGGLE_BUTTON(autoconnect_button))
		addProperty("autoconnect", gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(autoconnect_button)) ? "true" : "false" );
		
	// TODO: make this a global define
	addProperty("resource", "abicollab_protocol");
}
