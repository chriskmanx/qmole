/* -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* GConf
 * Copyright (C) 1999 - 2001 Red Hat Inc.
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

/* This program demonstrates how to use GConf.  The key thing is that
 * the main window and the prefs dialog have NO KNOWLEDGE of one
 * another as far as configuration values are concerned; they don't
 * even have to be in the same process. That is, the GConfClient acts
 * as the data "model" for configuration information; the main
 * application is a "view" of the model; and the prefs dialog is a
 * "controller."
 *
 * You can tell if your application has done this correctly by
 * using "gconftool" instead of your preferences dialog to set
 * preferences. For example:
 *
 *   gconftool --type=string --set /apps/basic-gconf-app/foo "My string"
 *
 * If that doesn't work every bit as well as setting the value
 * via the prefs dialog, then you aren't doing things right. ;-)
 *
 *
 * If you really want to be mean to your app, make it survive
 * this:
 *
 *   gconftool --break-key /apps/basic-gconf-app/foo
 *
 * Remember, the GConf database is just like an external file or
 * the network - it may have bogus values in it. GConf admin
 * tools will let people put in whatever they can think of.
 *
 * GConf does guarantee that string values will be valid UTF-8, for
 * convenience.
 *   
 */

/* Throughout, this program is letting GConfClient use its default
 * error handlers rather than checking for errors or attaching custom
 * handlers to the "unreturned_error" signal. Thus the last arg to
 * GConfClient functions is NULL.
 */

/* Special mention of an idiom often used in GTK+ apps that does
 * not work right with GConf but may appear to at first:
 *
 *  gboolean i_am_changing_value;
 *
 *  i_am_changing_value = TRUE;
 *  change_value (value);
 *  i_am_changing_value = FALSE;
 *
 * This breaks for several reasons: notification of changes
 * may be asynchronous, you may get notifications that are not
 * caused by change_value () while change_value () is running,
 * since GConf will enter the main loop, and also if you need
 * this code to work you are probably going to have issues
 * when someone other than yourself sets the value.
 *
 * A robust solution in this case is often to compare the old
 * and new values to see if they've really changed, thus avoiding
 * whatever loop you were trying to avoid.
 *
 */ 

 /* Be clean and pure */

#include <gconf/gconf-client.h>
#include <gtk/gtk.h>

static GtkWidget* create_main_window  (GConfClient *client);
static GtkWidget* create_prefs_dialog (GtkWidget   *parent,
                                       GConfClient *client);

int
main (int argc, char** argv)
{
  GConfClient *client;
  GtkWidget *main_window;
  
  gtk_init (&argc, &argv);

  /* Get the default client */
  client = gconf_client_get_default ();

  /* Tell GConfClient that we're interested in the given directory.
   * This means GConfClient will receive notification of changes
   * to this directory, and cache keys under this directory.
   * So _don't_ add "/" or something silly like that or you'll end
   * up with a copy of the whole GConf database. ;-)
   *
   * We pass NULL for the error to use the default error handler;
   * and use PRELOAD_NONE to avoid loading all config keys on
   * startup. If your app pretty much reads all config keys
   * on startup, then preloading the cache may make sense.
   */
   
  gconf_client_add_dir (client, "/apps/basic-gconf-app",
                        GCONF_CLIENT_PRELOAD_NONE, NULL);

  main_window = create_main_window (client);
  
  gtk_widget_show_all (main_window);
  
  gtk_main ();

  /* This ensures we cleanly detach from the GConf server (assuming
   * we hold the last reference). It's purely a bit of cleanliness,
   * the server does survive fine if we crash.
   */
  g_object_unref (G_OBJECT (client));
  
  return 0;
}

/* Quit app when window is destroyed */
static void
destroy_callback (GtkWidget *window,
                  gpointer   data)
{
  gtk_main_quit ();  
}

/* Remove the notification callback when the widget monitoring
 * notifications is destroyed
 */
static void
configurable_widget_destroy_callback (GtkWidget *widget,
                                      gpointer   data)
{
  guint notify_id;
  GConfClient *client;

  client = g_object_get_data (G_OBJECT (widget), "client");
  notify_id = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (widget), "notify_id"));

  if (notify_id != 0)
    gconf_client_notify_remove (client, notify_id);
}

/* Notification callback for our label widgets that
 * monitor the current value of a gconf key. i.e.
 * we are conceptually "configuring" the label widgets
 */
static void
configurable_widget_config_notify (GConfClient *client,
                                   guint        cnxn_id,
                                   GConfEntry  *entry,
                                   gpointer     user_data)
{
  GtkWidget *label = user_data;

  g_return_if_fail (GTK_IS_LABEL (label));

  /* Note that value can be NULL (unset) or it can have
   * the wrong type! Need to check that to survive
   * gconftool --break-key
   */
  
  if (gconf_entry_get_value (entry) == NULL)
    {
      gtk_label_set_text (GTK_LABEL (label), "");
    }
  else if (gconf_entry_get_value (entry)->type == GCONF_VALUE_STRING)
    {
      gtk_label_set_text (GTK_LABEL (label),
                          gconf_value_get_string (gconf_entry_get_value (entry)));
    }
  else
    {
      /* A real app would probably fall back to a reasonable default
       * in this case, instead of putting funky stuff in the GUI.
       */
      gtk_label_set_text (GTK_LABEL (label), "!type error!");
    }
}

/* Create a GtkLabel inside a frame, that we can "configure"
 * (the label displays the value of the config key).
 */
static GtkWidget*
create_configurable_widget (GConfClient *client,
                            const gchar *config_key)
{
  GtkWidget *frame;
  GtkWidget *label;
  guint notify_id;
  gchar *str;

  str = g_strdup_printf ("Value of \"%s\"", config_key);
  frame = gtk_frame_new (str);
  g_free (str);
  
  label = gtk_label_new ("");

  gtk_container_add (GTK_CONTAINER (frame), label);
  
  str = gconf_client_get_string (client, config_key, NULL);

  if (str != NULL)
    {
      gtk_label_set_text (GTK_LABEL (label), str);
      g_free (str);
    }

  notify_id = gconf_client_notify_add (client,
                                       config_key,
                                       configurable_widget_config_notify,
                                       label,
                                       NULL, NULL);

  /* Note that notify_id will be 0 if there was an error,
   * so we handle that in our destroy callback.
   */
  
  g_object_set_data (G_OBJECT (label), "notify_id", GUINT_TO_POINTER (notify_id));
  g_object_set_data (G_OBJECT (label), "client", client);

  g_signal_connect (G_OBJECT (label), "destroy",
                    G_CALLBACK (configurable_widget_destroy_callback),
                    NULL);
  
  return frame;
}

static void
prefs_dialog_destroyed (GtkWidget *dialog,
                        gpointer   main_window)
{
  g_object_set_data (G_OBJECT (main_window), "prefs", NULL);
}

/* prefs button clicked */
static void
prefs_clicked (GtkWidget *button,
               gpointer   data)
{
  GtkWidget *prefs_dialog;
  GtkWidget *main_window = data;
  GConfClient *client;

  prefs_dialog = g_object_get_data (G_OBJECT (main_window), "prefs");

  if (prefs_dialog == NULL)
    {
      client = g_object_get_data (G_OBJECT (main_window), "client");
      
      prefs_dialog = create_prefs_dialog (main_window, client);

      g_object_set_data (G_OBJECT (main_window), "prefs", prefs_dialog);

      g_signal_connect (G_OBJECT (prefs_dialog), "destroy",
                        G_CALLBACK (prefs_dialog_destroyed),
                        main_window);
      
      gtk_widget_show_all (prefs_dialog);
    }
  else 
    {
      /* show existing dialog */
      gtk_window_present (GTK_WINDOW (prefs_dialog));
    }
}

static GtkWidget*
create_main_window (GConfClient *client)
{
  GtkWidget *w;
  GtkWidget *vbox;
  GtkWidget *config;
  GtkWidget *prefs;
  
  w = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title (GTK_WINDOW (w), "basic-gconf-app Main Window");
  
  vbox = gtk_vbox_new (FALSE, 5);

  gtk_container_add (GTK_CONTAINER (w), vbox);

  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
  
  /* Create labels that we can "configure" */
  config = create_configurable_widget (client, "/apps/basic-gconf-app/foo");
  gtk_box_pack_start (GTK_BOX (vbox), config, TRUE, TRUE, 0);

  config = create_configurable_widget (client, "/apps/basic-gconf-app/bar");
  gtk_box_pack_start (GTK_BOX (vbox), config, TRUE, TRUE, 0);
  
  config = create_configurable_widget (client, "/apps/basic-gconf-app/baz");
  gtk_box_pack_start (GTK_BOX (vbox), config, TRUE, TRUE, 0);

  config = create_configurable_widget (client, "/apps/basic-gconf-app/blah");
  gtk_box_pack_start (GTK_BOX (vbox), config, TRUE, TRUE, 0);

  g_signal_connect (G_OBJECT (w), "destroy",
                    G_CALLBACK (destroy_callback), NULL);

  g_object_set_data (G_OBJECT (w), "client", client);
  
  prefs = gtk_button_new_with_mnemonic ("_Prefs");
  gtk_box_pack_end (GTK_BOX (vbox), prefs, FALSE, FALSE, 0);
  g_signal_connect (G_OBJECT (prefs), "clicked",
                    G_CALLBACK (prefs_clicked), w);
  
  return w;
}



/*
 * Preferences dialog code. NOTE that the prefs dialog knows NOTHING
 * about the existence of the main window; it is purely a way to fool
 * with the GConf database. It never does something like change
 * the main window directly; it ONLY changes GConf keys via
 * GConfClient. This is _important_, because people may configure
 * your app without using your preferences dialog.
 *
 * This is an instant-apply prefs dialog. For a complicated
 * apply/revert/cancel dialog as in GNOME 1, see the
 * complex-gconf-app.c example. But don't actually copy that example
 * in GNOME 2, thanks. ;-) complex-gconf-app.c does show how
 * to use GConfChangeSet.
 */

/* Commit changes to the GConf database. */
static gboolean
config_entry_commit (GtkWidget *entry, GdkEvent *event, gpointer callback_data)
{
  gchar *text;
  const gchar *key;
  GConfClient *client;
  
  client = g_object_get_data (G_OBJECT (entry), "client");  

  text = gtk_editable_get_chars (GTK_EDITABLE (entry), 0, -1);

  key = g_object_get_data (G_OBJECT (entry), "key");

  /* Unset if the string is zero-length, otherwise set */
  if (*text != '\0')
    gconf_client_set_string (client, key, text, NULL);
  else
    gconf_client_unset (client, key, NULL);
  
  g_free (text);

  return FALSE;
}

/* Create an entry used to edit the given config key */
static GtkWidget*
create_config_entry (GtkWidget   *prefs_dialog,
                     GConfClient *client,
                     const gchar *config_key,
                     gboolean     focus)
{
  GtkWidget *hbox;
  GtkWidget *entry;
  GtkWidget *label;
  char *str;

  hbox = gtk_hbox_new (FALSE, 5);

  label = gtk_label_new (config_key);
  
  entry = gtk_entry_new ();

  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (hbox), entry, FALSE, FALSE, 0);

  /* this will print an error via default error handler
   * if the key isn't set to a string
   */
  str = gconf_client_get_string (client, config_key, NULL);

  if (str)
    {
      gtk_entry_set_text (GTK_ENTRY (entry), str);
      g_free (str);
    }
  
  g_object_set_data (G_OBJECT (entry), "client", client);
  g_object_set_data_full (G_OBJECT (entry), "key",
                          g_strdup (config_key),
                          (GDestroyNotify) g_free);

  /* Commit changes if the user focuses out, or hits enter; we don't
   * do this on "changed" since it'd probably be a bit too slow to
   * round-trip to the server on every "changed" signal.
   */
  g_signal_connect (G_OBJECT (entry), "focus_out_event",
                    G_CALLBACK (config_entry_commit),
                    NULL);

  g_signal_connect (G_OBJECT (entry), "activate",
                    G_CALLBACK (config_entry_commit),
                    NULL);  

  /* Set the entry insensitive if the key it edits isn't writable.
   * Technically, we should update this sensitivity if the key gets
   * a change notify, but that's probably overkill.
   */
  gtk_widget_set_sensitive (entry,
                            gconf_client_key_is_writable (client,
                                                          config_key, NULL));

  if (focus)
    gtk_widget_grab_focus (entry);
  
  return hbox;
}

static GtkWidget*
create_prefs_dialog (GtkWidget   *parent,
                     GConfClient *client)
{
  GtkWidget* dialog;
  GtkWidget* vbox;
  GtkWidget* entry;
  
  dialog = gtk_dialog_new_with_buttons ("basic-gconf-app Preferences",
                                        GTK_WINDOW (parent),
                                        0,
                                        GTK_STOCK_CLOSE,
                                        GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* destroy dialog on button press */
  g_signal_connect (G_OBJECT (dialog), "response",
                    G_CALLBACK (gtk_widget_destroy),
                    NULL);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  /* resizing doesn't grow the entries anyhow */
  gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);
  
  vbox = gtk_vbox_new (FALSE, 5);

  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
  
  gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area(dialog)),
                      vbox, TRUE, TRUE, 0);

  entry = create_config_entry (dialog, client, "/apps/basic-gconf-app/foo",
                               TRUE);
  gtk_box_pack_start (GTK_BOX (vbox), entry, 
                      FALSE, FALSE, 0);
  
  entry = create_config_entry (dialog, client, "/apps/basic-gconf-app/bar",
                               FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), entry, 
                      FALSE, FALSE, 0);
  
  entry = create_config_entry (dialog, client, "/apps/basic-gconf-app/baz",
                               FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), entry, 
                      FALSE, FALSE, 0);

  entry = create_config_entry (dialog, client, "/apps/basic-gconf-app/blah",
                               FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), entry, 
                      FALSE, FALSE, 0);
  
  return dialog;
}
