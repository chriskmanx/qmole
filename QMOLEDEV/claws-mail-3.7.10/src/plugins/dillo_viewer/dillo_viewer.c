/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 the Claws Mail Team
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

#include <unistd.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>

#include "common/claws.h"
#include "common/version.h"
#include "plugin.h"
#include "utils.h"
#include "mimeview.h"
#include "addr_compl.h"

#include "dillo_prefs.h"

#define PLUGIN_NAME (_("Dillo HTML Viewer"))

typedef struct _DilloViewer DilloViewer;

struct _DilloViewer
{
	MimeViewer	 mimeviewer;
	GtkWidget	*widget;	
	GtkWidget	*socket;
	gchar		*filename;
};

static MimeViewerFactory dillo_viewer_factory;

static GtkWidget *dillo_get_widget(MimeViewer *_viewer)
{
	DilloViewer *viewer = (DilloViewer *) _viewer;

	debug_print("dillo_get_widget\n");

	return GTK_WIDGET(viewer->widget);
}

static gboolean socket_destroy_cb(GtkObject *object, gpointer data)
{
	DilloViewer *viewer = (DilloViewer *) data;
	debug_print("Destroyed dillo socket %p\n", viewer->socket);
	viewer->socket = NULL;
	return FALSE;
}

static gboolean found_in_addressbook(const gchar *address)
{
	gchar *addr = NULL;
	gboolean found = FALSE;
	gint num_addr = 0;
	
	if (!address)
		return FALSE;
	
	addr = g_strdup(address);
	extract_address(addr);
	num_addr = complete_address(addr);
	if (num_addr > 1) {
		/* skip first item (this is the search string itself) */
		int i = 1;
		for (; i < num_addr && !found; i++) {
			gchar *caddr = get_complete_address(i);
			extract_address(caddr);
			if (strcasecmp(caddr, addr) == 0)
				found = TRUE;
			g_free(caddr);
		}
	}
	g_free(addr);
	return found;
}

static gboolean load_images(DilloViewer *viewer)
{
	MessageView *messageview = ((MimeViewer *)viewer)->mimeview 
					? ((MimeViewer *)viewer)->mimeview->messageview 
					: NULL;
	MsgInfo *msginfo = NULL;
	gchar *ab_folderpath = NULL;

	if (messageview == NULL)
		return FALSE;
	
	msginfo = messageview->msginfo;
	
	if (msginfo == NULL)
		return FALSE;

	/* don't load remote images, period. */
	if (dillo_prefs.local)
		return FALSE;
	
	/* don't do whitelisting -> load images */
	if (!dillo_prefs.whitelist_ab)
		return TRUE;

	if (*dillo_prefs.whitelist_ab_folder != '\0' &&
	    strcasecmp(dillo_prefs.whitelist_ab_folder, _("Any")) != 0)
		ab_folderpath = dillo_prefs.whitelist_ab_folder;

	start_address_completion(ab_folderpath);

	/* do whitelisting -> check sender */
	if (found_in_addressbook(msginfo->from)) {
		end_address_completion();
		return TRUE;
	}
	
	end_address_completion();
	return FALSE;
}

static void dillo_show_mimepart(MimeViewer *_viewer,
				const gchar *infile,
				MimeInfo *partinfo)
{
	DilloViewer *viewer = (DilloViewer *) _viewer;

	debug_print("dillo_show_mimepart\n");

	if (viewer->filename != NULL) {
		claws_unlink(viewer->filename);
		g_free(viewer->filename);
	}

	viewer->filename = procmime_get_tmp_file_name(partinfo);
	
	if (!(procmime_get_part(viewer->filename, partinfo) < 0)) {
		gchar *cmd;

		if (viewer->socket)
			gtk_widget_destroy(viewer->socket);
		viewer->socket = gtk_socket_new();
		debug_print("Adding dillo socket %p", viewer->socket);
		gtk_container_add(GTK_CONTAINER(viewer->widget),
				  viewer->socket);
		gtk_widget_realize(viewer->socket);
		gtk_widget_show(viewer->socket);
		g_signal_connect(G_OBJECT(viewer->socket), "destroy", 
				 G_CALLBACK(socket_destroy_cb), viewer);

		cmd = g_strdup_printf("dillo %s%s-x %d \"%s\"",
				      (!load_images(viewer) ? "-l " : ""),
				      (dillo_prefs.full ? "-f " : ""),
				      (gint) GDK_WINDOW_XWINDOW(viewer->socket->window),
				      viewer->filename);

		execute_command_line(cmd, TRUE);
		g_free(cmd);
	}
}

static void dillo_clear_viewer(MimeViewer *_viewer)
{
	DilloViewer *viewer = (DilloViewer *) _viewer;

	debug_print("dillo_clear_viewer\n");
	debug_print("Removing dillo socket %p\n", viewer->socket);

	if (viewer->socket) {
		gtk_widget_destroy(viewer->socket);
	}
}

static void dillo_destroy_viewer(MimeViewer *_viewer)
{
	DilloViewer *viewer = (DilloViewer *) _viewer;

	debug_print("dillo_destroy_viewer\n");

	if (viewer->socket) {
		gtk_widget_destroy(viewer->socket);
	}

	g_object_unref(GTK_WIDGET(viewer->widget));
	claws_unlink(viewer->filename);
	g_free(viewer->filename);
    	g_free(viewer);
}

static MimeViewer *dillo_viewer_create(void)
{
	DilloViewer *viewer;

	debug_print("dillo_viewer_create\n");
	
	viewer = g_new0(DilloViewer, 1);
	viewer->mimeviewer.factory = &dillo_viewer_factory;
	viewer->mimeviewer.get_widget = dillo_get_widget;
	viewer->mimeviewer.show_mimepart = dillo_show_mimepart;
	viewer->mimeviewer.clear_viewer = dillo_clear_viewer;
	viewer->mimeviewer.destroy_viewer = dillo_destroy_viewer;	
	viewer->mimeviewer.get_selection = NULL;
	viewer->widget = gtk_event_box_new();

	gtk_widget_show(viewer->widget);
	g_object_ref(viewer->widget);

	viewer->filename = NULL;

	return (MimeViewer *) viewer;
}

static gchar *content_types[] = 
	{"text/html", NULL};

static MimeViewerFactory dillo_viewer_factory =
{
	content_types,	
	0,

	dillo_viewer_create,
};

gint plugin_init(gchar **error)
{
	gchar *dillo_path = NULL;
	if (!check_plugin_version(MAKE_NUMERIC_VERSION(2,9,2,72),
				VERSION_NUMERIC, PLUGIN_NAME, error))
  		return -1;

	if ((dillo_path = g_find_program_in_path("dillo")) == NULL) {
		*error = g_strdup(_("Can't find the dillo executable in PATH. Is it installed?"));
		return -1;
	}
	g_free(dillo_path);

        dillo_prefs_init();

	mimeview_register_viewer_factory(&dillo_viewer_factory);

	return 0;	
}

gboolean plugin_done(void)
{
	mimeview_unregister_viewer_factory(&dillo_viewer_factory);

        dillo_prefs_done();
	return TRUE;
}

const gchar *plugin_name(void)
{
	return PLUGIN_NAME;
}

const gchar *plugin_desc(void)
{
	return _("This plugin renders HTML mail using the Dillo "
		"web browser.\n"
	        "\n"
	        "Options can be found in /Configuration/Preferences/Plugins/Dillo Browser");
}

const gchar *plugin_type(void)
{
	return "GTK2";
}

const gchar *plugin_licence(void)
{
	return "GPL3+";
}

const gchar *plugin_version(void)
{
	return VERSION;
}

struct PluginFeature *plugin_provides(void)
{
	static struct PluginFeature features[] = 
		{ {PLUGIN_MIMEVIEWER, "text/html"},
		  {PLUGIN_NOTHING, NULL}};
	return features;
}
