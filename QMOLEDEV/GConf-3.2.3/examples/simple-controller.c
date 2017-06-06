/* -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
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

/* A very simple program that sets a single key value when you type
   it in an entry and press return */

#include <gconf/gconf-client.h>
#include <gtk/gtk.h>

static void
entry_activated_callback(GtkWidget* entry, gpointer user_data)
{
  GConfClient* client;
  gchar* str;
  
  client = GCONF_CLIENT(user_data);

  str = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);

  gconf_client_set_string(client, "/extra/test/directory/key",
                          str, NULL);

  g_free(str);
}

int
main(int argc, char** argv)
{
  GtkWidget* window;
  GtkWidget* entry;
  GConfClient* client;

  gtk_init(&argc, &argv);
  gconf_init(argc, argv, NULL);
  
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  entry = gtk_entry_new();

  gtk_container_add(GTK_CONTAINER(window), entry);  

  client = gconf_client_get_default();

  gconf_client_add_dir(client,
                       "/extra/test/directory",
                       GCONF_CLIENT_PRELOAD_NONE,
                       NULL);


  g_signal_connect (G_OBJECT (entry), "activate",
                    G_CALLBACK (entry_activated_callback),
                    client);

  /* If key isn't writable, then set insensitive */
  gtk_widget_set_sensitive (entry,
                            gconf_client_key_is_writable (client,
                                                          "/extra/test/directory/key", NULL));
  
  gtk_widget_show_all(window);

  gtk_main();

  return 0;
}


