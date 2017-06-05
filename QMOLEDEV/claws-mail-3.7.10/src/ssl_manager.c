/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Colin Leroy <colin@colino.net> 
 * and the Claws Mail team
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
#  include "config.h"
#endif

#ifdef USE_GNUTLS
#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <sys/types.h>
#include <dirent.h>

#include "ssl_manager.h"
#include "ssl_certificate.h"
#include "manage_window.h"
#include "utils.h"
#include "mainwindow.h"
#include "gtksctree.h"
#include "alertpanel.h"
#include "sslcertwindow.h"
#include "prefs_common.h"

enum {
	SSL_MANAGER_HOST,
	SSL_MANAGER_PORT,
	SSL_MANAGER_CERT,
	N_SSL_MANAGER_COLUMNS
};


static struct SSLManager
{
	GtkWidget *window;
	GtkWidget *hbox1;
	GtkWidget *vbox1;
	GtkWidget *certlist;
	GtkWidget *view_btn;
	GtkWidget *delete_btn;
	GtkWidget *close_btn;
} manager;

static void ssl_manager_view_cb		(GtkWidget *widget, gpointer data);
static void ssl_manager_delete_cb	(GtkWidget *widget, gpointer data);
static void ssl_manager_close_cb	(GtkWidget *widget, gpointer data);
static gboolean key_pressed		(GtkWidget *widget, GdkEventKey *event,
					 gpointer data);
static void ssl_manager_load_certs	(void);
static void ssl_manager_double_clicked(GtkTreeView		*list_view,
				   	GtkTreePath		*path,
				   	GtkTreeViewColumn	*column,
				   	gpointer		 data);

void ssl_manager_open(MainWindow *mainwin)
{
	if (!manager.window)
		ssl_manager_create();

	manage_window_set_transient(GTK_WINDOW(manager.window));
	gtk_widget_grab_focus(manager.close_btn);

	ssl_manager_load_certs();

	gtk_widget_show(manager.window);

}

static GtkListStore* ssl_manager_create_data_store(void)
{
	return gtk_list_store_new(N_SSL_MANAGER_COLUMNS,
				  G_TYPE_STRING,
				  G_TYPE_STRING,
  				  G_TYPE_POINTER,
				  -1);
}

static void ssl_manager_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Server"),
		 renderer,
		 "text", SSL_MANAGER_HOST,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Port"),
		 renderer,
		 "text", SSL_MANAGER_PORT,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
}

static GtkWidget *ssl_manager_list_view_create	(void)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(ssl_manager_create_data_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	
	
 	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model),
                                             0, GTK_SORT_ASCENDING);
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	
	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	g_signal_connect(G_OBJECT(list_view), "row_activated",
	                 G_CALLBACK(ssl_manager_double_clicked),
			 list_view);

	/* create the columns */
	ssl_manager_create_list_view_columns(GTK_WIDGET(list_view));

	return GTK_WIDGET(list_view);

}

void ssl_manager_create(void) 
{
	GtkWidget *window;
	GtkWidget *hbox1;
	GtkWidget *vbox1;
	GtkWidget *certlist;
	GtkWidget *view_btn;
	GtkWidget *delete_btn;
	GtkWidget *close_btn;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "ssl_manager");
	gtk_window_set_title (GTK_WINDOW(window),
			      _("Saved SSL Certificates"));

	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW (window), TRUE);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(ssl_manager_close_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT (window);

	hbox1 = gtk_hbox_new(FALSE, 6);
	vbox1 = gtk_vbox_new(FALSE, 0);
	delete_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	
	g_signal_connect(G_OBJECT(delete_btn), "clicked",
			 G_CALLBACK(ssl_manager_delete_cb), NULL);

	view_btn = gtk_button_new_from_stock(GTK_STOCK_PROPERTIES);
	g_signal_connect(G_OBJECT(view_btn), "clicked",
			 G_CALLBACK(ssl_manager_view_cb), NULL);

	close_btn = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
	g_signal_connect(G_OBJECT(close_btn), "clicked",
			 G_CALLBACK(ssl_manager_close_cb), NULL);

	certlist = ssl_manager_list_view_create();
	
	gtk_box_pack_start(GTK_BOX(hbox1), certlist, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox1), vbox1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox1), view_btn, FALSE, FALSE, 4);
	gtk_box_pack_start(GTK_BOX(vbox1), delete_btn, FALSE, FALSE, 4);
	gtk_box_pack_end(GTK_BOX(vbox1), close_btn, FALSE, FALSE, 4);
	
	gtk_widget_show(certlist);
	gtk_widget_show(hbox1);
	gtk_widget_show(vbox1);
	gtk_widget_show(close_btn);
	gtk_widget_show(delete_btn);
	gtk_widget_show(view_btn);
	gtk_container_add(GTK_CONTAINER (window), hbox1);

	manager.window = window;
	manager.hbox1 = hbox1;
	manager.vbox1 = vbox1;
	manager.certlist = certlist;
	manager.view_btn = view_btn;
	manager.delete_btn = delete_btn;
	manager.close_btn = close_btn;

	gtk_widget_show(window);
		
}

static char *get_server(char *str)
{
	char *ret = NULL, *tmp = g_strdup(str);
	char *first_pos = NULL, *last_pos = NULL;
	char *previous_pos = NULL, *pre_previous_pos = NULL;
	int previous_dot_pos;

	if (!strchr(tmp, ':')) {
		/* no fingerprint */
		if (strstr(tmp, ".cert"))
			*(strstr(tmp, ".cert")+1) = '.';
	}

	first_pos = tmp;
	while (tmp && (tmp = strstr(tmp,".")) != NULL) {
		tmp++;
		pre_previous_pos = previous_pos;
		previous_pos = last_pos;
		last_pos = tmp;
	}
	previous_dot_pos = (pre_previous_pos - first_pos);
	if (previous_dot_pos - 1 > 0)
		ret = g_strndup(first_pos, previous_dot_pos - 1);
	else 
		ret = g_strdup(first_pos);
	g_free(first_pos);
	return ret;
}

static char *get_port(char *str)
{
	char *ret = NULL, *tmp = g_strdup(str);
	char *last_pos = NULL;
	char *previous_pos = NULL, *pre_previous_pos = NULL;

	if (!strchr(tmp, ':')) {
		/* no fingerprint */
		if (strstr(tmp, ".cert"))
			*(strstr(tmp, ".cert")+1) = '.';
	}

	while (tmp && (tmp = strstr(tmp,".")) != NULL) {
		tmp++;
		pre_previous_pos = previous_pos;
		previous_pos = last_pos;
		last_pos = tmp;
	}
	if (previous_pos && pre_previous_pos && (int)(previous_pos - pre_previous_pos - 1) > 0)
		ret = g_strndup(pre_previous_pos, (int)(previous_pos - pre_previous_pos - 1));
	else
		ret = g_strdup("0");
	g_free(tmp);
	return ret;
	
}

static char *get_fingerprint(char *str)
{
	char *ret = NULL, *tmp = g_strdup(str);
	char *previous_pos = NULL, *last_pos = NULL;

	if (!strchr(tmp, ':')) {
		/* no fingerprint */
		if (strstr(tmp, ".cert"))
			*(strstr(tmp, ".cert")+1) = '.';
	}

	while (tmp && (tmp = strstr(tmp,".")) != NULL) {
		tmp++;
		previous_pos = last_pos;
		last_pos = tmp;
	}
	if (last_pos && previous_pos && (int)(last_pos - previous_pos - 1) > 0)
		ret = g_strndup(previous_pos, (int)(last_pos - previous_pos - 1));
	else
		ret = NULL;
	g_free(tmp);
	return ret;
	
}

static void ssl_manager_list_view_insert_cert(GtkWidget *list_view,
						  GtkTreeIter *row_iter,
						  gchar *host, 
						  gchar *port,
						  SSLCertificate *cert) 
{
	GtkTreeIter iter;
	GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model
					(GTK_TREE_VIEW(list_view)));

	if (row_iter == NULL) {
		/* append new */
		gtk_list_store_append(list_store, &iter);
		gtk_list_store_set(list_store, &iter,
				   SSL_MANAGER_HOST, host,
				   SSL_MANAGER_PORT, port,
				   SSL_MANAGER_CERT, cert,
				   -1);
	} else {
		gtk_list_store_set(list_store, row_iter,
				   SSL_MANAGER_HOST, host,
				   SSL_MANAGER_PORT, port,
				   SSL_MANAGER_CERT, cert,
				   -1);
	}
}

static void ssl_manager_load_certs (void) 
{
	DIR *dir;
	struct dirent *d;
	gchar *path;
	int row = 0;
	GtkListStore *store;

	store = GTK_LIST_STORE(gtk_tree_view_get_model
				(GTK_TREE_VIEW(manager.certlist)));

	gtk_list_store_clear(store);

	path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, 
			  "certs", G_DIR_SEPARATOR_S, NULL);

	if((dir = opendir(path)) == NULL) {
		perror("opendir");
		return;
	}
	
	while ((d = readdir(dir)) != NULL) {
		gchar *server, *port, *fp;
		SSLCertificate *cert;

		if(!strstr(d->d_name, ".cert")) 
			continue;

		server = get_server(d->d_name);
		port = get_port(d->d_name);
		fp = get_fingerprint(d->d_name);
		
		cert = ssl_certificate_find_lookup(server, atoi(port), fp, FALSE);

		ssl_manager_list_view_insert_cert(manager.certlist, NULL, 
						  server, port, cert);
		
		g_free(server);
		g_free(port);
		g_free(fp);
		row++;
	}
	closedir(dir);
	g_free(path);
}

static void ssl_manager_close(void) 
{
	gtk_widget_hide(manager.window);
}

static void ssl_manager_close_cb(GtkWidget *widget,
			         gpointer data) 
{
	ssl_manager_close();
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_Escape)
		ssl_manager_close();
	return FALSE;
}

static void ssl_manager_double_clicked(GtkTreeView		*list_view,
				   	GtkTreePath		*path,
				   	GtkTreeViewColumn	*column,
				   	gpointer		 data)
{
	SSLCertificate *cert;
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);

	if (!gtk_tree_model_get_iter(model, &iter, path))
		return;

	gtk_tree_model_get(model, &iter, 
			   SSL_MANAGER_CERT, &cert,
			   -1);

	if (!cert)
		return;

	sslcertwindow_show_cert(cert);

	return;
}



static void ssl_manager_delete_cb(GtkWidget *widget, 
			      gpointer data) 
{
	SSLCertificate *cert;
	int val;
	GtkTreeIter sel;
	GtkTreeModel *model;

	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection
				(GTK_TREE_VIEW(manager.certlist)),
				&model, &sel))
		return;
	
	gtk_tree_model_get(model, &sel,
			   SSL_MANAGER_CERT, &cert,
			   -1);
	if (!cert)
		return;

	val = alertpanel_full(_("Delete certificate"),
			      _("Do you really want to delete this certificate?"),
		 	      GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL, FALSE,
			      NULL, ALERT_WARNING, G_ALERTDEFAULT);

			     
	if (val != G_ALERTALTERNATE)
		return;
	
	ssl_certificate_delete_from_disk(cert);
	ssl_certificate_destroy(cert);
	gtk_list_store_remove(GTK_LIST_STORE(model), &sel);
}

static void ssl_manager_view_cb(GtkWidget *widget, 
			        gpointer data) 
{
	SSLCertificate *cert;
	GtkTreeIter sel;
	GtkTreeModel *model;

	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection
				(GTK_TREE_VIEW(manager.certlist)),
				&model, &sel))
		return;
	
	gtk_tree_model_get(model, &sel,
			   SSL_MANAGER_CERT, &cert,
			   -1);
	if (!cert)
		return;

	sslcertwindow_show_cert(cert);
}
#endif
