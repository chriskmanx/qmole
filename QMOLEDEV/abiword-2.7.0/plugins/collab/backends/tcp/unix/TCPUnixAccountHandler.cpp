/* Copyright (C) 2007 by Marc Maurer <uwog@uwog.net>
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

#include "TCPUnixAccountHandler.h"

AccountHandlerConstructor TCPAccountHandlerConstructor = &TCPUnixAccountHandler::static_constructor;

AccountHandler * TCPUnixAccountHandler::static_constructor()
{
	return static_cast<AccountHandler *>(new TCPUnixAccountHandler());
}

void s_group_changed(GtkToggleButton* /*button*/, TCPUnixAccountHandler* pHandler)
{
	pHandler->eventGroupChanged();
}

TCPUnixAccountHandler::TCPUnixAccountHandler()
	: TCPAccountHandler(),
	vbox(NULL),
	server_button(NULL),
	client_button(NULL),
	server_entry(NULL),
	port_button(NULL),
	ssl_button(NULL),
	autoconnect_button(NULL)
{
}

void TCPUnixAccountHandler::embedDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("TCPUnixAccountHandler::embedDialogWidgets()\n"));
	UT_return_if_fail(pEmbeddingParent);

	vbox = gtk_vbox_new(FALSE, 6);
	GtkVBox* parent = (GtkVBox*)pEmbeddingParent;
	
	// host a session (we should really use a GtkAction for this)
	server_button = gtk_radio_button_new_with_label(NULL, "Accept incoming connections");
	gtk_box_pack_start(GTK_BOX(vbox), server_button, TRUE, TRUE, 0);
	
	// join a session
	client_button = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(server_button), "Connect to a server");
	gtk_box_pack_start(GTK_BOX(vbox), client_button, TRUE, TRUE, 0);

	// add a table to hold the server and port options
	GtkWidget* table = gtk_table_new(1, 3, FALSE);

	// spacer
	GtkWidget* spacer = gtk_label_new("");
	gtk_widget_set_size_request(spacer, 12, -1);
	gtk_table_attach_defaults(GTK_TABLE(table), spacer, 0, 1, 0, 1);

	// host	
	GtkWidget* server_label = gtk_label_new("Address:");
	gtk_misc_set_alignment(GTK_MISC(server_label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), server_label, 1, 2, 0, 1);
	server_entry = gtk_entry_new();
	gtk_table_attach_defaults(GTK_TABLE(table), server_entry, 2, 3, 0, 1);
	gtk_widget_set_sensitive(server_entry, false);
	
	gtk_box_pack_start(GTK_BOX(vbox), table, TRUE, TRUE, 0);
	
	// port
	GtkWidget* portHBox = gtk_hbox_new(FALSE, 6);
	GtkWidget* port_label = gtk_label_new("Port:");
	gtk_misc_set_alignment(GTK_MISC(port_label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(portHBox), port_label, false, false, 0);	
	port_button = gtk_spin_button_new_with_range(1, 65536, 1);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(port_button), DEFAULT_TCP_PORT);
	gtk_box_pack_start(GTK_BOX(portHBox), port_button, false, false, 0);
	gtk_box_pack_start(GTK_BOX(vbox), portHBox, false, false, 0);
	
	// ssl
	ssl_button = gtk_check_button_new_with_label("Use a secure connection (SSL)");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ssl_button), false);
	gtk_box_pack_start(GTK_BOX(vbox), ssl_button, TRUE, TRUE, 0);	
	gtk_widget_set_sensitive(ssl_button, false); // not supported for now
	
	// autoconnect
	autoconnect_button = gtk_check_button_new_with_label("Connect on application startup");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(autoconnect_button), true);
	gtk_box_pack_start(GTK_BOX(vbox), autoconnect_button, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(parent), vbox, FALSE, FALSE, 0);
	gtk_widget_show_all(GTK_WIDGET(parent));
	
	// attach some signals
	g_signal_connect(G_OBJECT(server_button),
							"toggled",
							G_CALLBACK(s_group_changed),
							static_cast<gpointer>(this));
	
}

void TCPUnixAccountHandler::removeDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("TCPAccountHandler::removeDialogWidgets()\n"));
	UT_return_if_fail(pEmbeddingParent);
	
	// this will conveniently destroy all contained widgets as well
	if (vbox && GTK_IS_WIDGET(vbox))
		gtk_widget_destroy(vbox);
}

void TCPUnixAccountHandler::storeProperties()
{
	UT_DEBUGMSG(("TCPUnixAccountHandler::storeProperties()\n"));
	
	bool serve = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(server_button));
	if (!serve)
	{
		if (server_entry && GTK_IS_ENTRY(server_entry))
			addProperty("server", gtk_entry_get_text(GTK_ENTRY(server_entry)));
	}
	
	if (port_button && GTK_IS_ENTRY(port_button))
			addProperty("port", gtk_entry_get_text(GTK_ENTRY(port_button)));	
			
	if (autoconnect_button && GTK_IS_TOGGLE_BUTTON(autoconnect_button))
		addProperty("autoconnect", gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(autoconnect_button)) ? "true" : "false" );
}

void TCPUnixAccountHandler::eventGroupChanged()
{
	UT_DEBUGMSG(("TCPUnixAccountHandler::eventGroupChanged()\n"));
	
	bool serve = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(server_button));
	gtk_widget_set_sensitive(server_entry, !serve);
}
