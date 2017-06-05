/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* drop_box.c - the "drop something here" widget */

#include "config.h"

#include <gtk/gtk.h>
#include <errno.h>
#include <string.h>

#include "global.h"

#include "dropbox.h"
#include "main.h"
#include "type.h"
#include "pixmaps.h"
#include "support.h"
#include "gui_support.h"
#include "run.h"

struct _DropBoxClass {
	GtkFrameClass parent;

	void (*path_dropped)(GtkWidget *drop_box, const guchar *path);
	void (*clear)(GtkWidget *drop_box);
};

struct _DropBox {
	GtkFrame frame;

	GtkWidget *buttons;
	GtkWidget *label;
	gchar *path;
};

/* Static prototypes */
static void drop_box_class_init(gpointer gclass, gpointer data);
static void drop_box_init(GTypeInstance *object, gpointer gclass);
static void open_dir_clicked(GtkWidget *button, DropBox *drop_box);
static void clear_clicked(GtkWidget *button, DropBox *drop_box);
static void drop_box_drag_data_received(GtkWidget *drop_box,
			      GdkDragContext    *context,
			      gint              x,
			      gint              y,
			      GtkSelectionData  *selection_data,
			      guint             drag_info,
			      guint32           time);


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

GtkWidget *drop_box_new(const char *message)
{
	GtkWidget *button, *label, *vbox, *icon;
	MaskedPixmap *mp;
	DropBox *drop_box;
	GtkTargetEntry 	targets[] = {
		{"text/uri-list", 0, 0},
	};

	drop_box = g_object_new(drop_box_get_type(), NULL);

	gtk_frame_set_shadow_type(GTK_FRAME(drop_box), GTK_SHADOW_IN);
	gtk_container_set_border_width(GTK_CONTAINER(drop_box), 4);

	gtk_drag_dest_set(GTK_WIDGET(drop_box), GTK_DEST_DEFAULT_ALL,
			targets, sizeof(targets) / sizeof(*targets),
			GDK_ACTION_COPY);

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(drop_box), vbox);

	label = gtk_label_new(message);
	gtk_misc_set_padding(GTK_MISC(label), 10, 10);
	gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

	drop_box->label = gtk_label_new(NULL);
	gtk_box_pack_start(GTK_BOX(vbox), drop_box->label, FALSE, TRUE, 0);
	gtk_misc_set_padding(GTK_MISC(drop_box->label), 2, 2);

	drop_box->buttons = gtk_hbutton_box_new();
	gtk_box_pack_start(GTK_BOX(vbox), drop_box->buttons, FALSE, TRUE, 0);

	button = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	gtk_box_pack_start(GTK_BOX(drop_box->buttons), button, FALSE, TRUE, 0);
	g_signal_connect(button, "clicked",
			G_CALLBACK(clear_clicked), drop_box);

	mp = type_to_icon(inode_directory);
	pixmap_make_small(mp);
	icon = gtk_image_new_from_pixbuf(mp->sm_pixbuf);
	g_object_unref(mp);
	button = button_new_image_text(icon, _("Show"));
	gtk_box_pack_start(GTK_BOX(drop_box->buttons), button, FALSE, TRUE, 0);
	g_signal_connect(button, "clicked",
			G_CALLBACK(open_dir_clicked), drop_box);

	gtk_tooltips_set_tip(tooltips, button,
			_("Show the current choice in a filer window"), NULL);

	drop_box_set_path(drop_box, NULL);

	gtk_widget_show_all(vbox);

	return GTK_WIDGET(drop_box);
}

GType drop_box_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (DropBoxClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			drop_box_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(DropBox),
			0,			/* n_preallocs */
			drop_box_init
		};

		type = g_type_register_static(gtk_frame_get_type(),
						"DropBox", &info, 0);
	}

	return type;
}

void drop_box_set_path(DropBox *drop_box, const guchar *path)
{
	char *copy;

	null_g_free(&drop_box->path);
	drop_box->path = g_strdup(path);

	if (path)
	{
		int l;
		l = strlen(path);
		if (l > 40)
			copy = g_strdup_printf("...%s", path + l - 38);
		else
			copy = g_strdup(path);
		gtk_widget_set_sensitive(drop_box->buttons, TRUE);
	}
	else
	{
		copy = g_strdup(_("<nothing>"));
		gtk_widget_set_sensitive(drop_box->buttons, FALSE);
	}

	gtk_label_set_text(GTK_LABEL(drop_box->label), copy);
	g_free(copy);
}

const gchar *drop_box_get_path(DropBox *drop_box)
{
	g_return_val_if_fail(drop_box != NULL, NULL);

	return drop_box->path;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void drop_box_class_init(gpointer gclass, gpointer data)
{
	GtkWidgetClass *widget = (GtkWidgetClass *) gclass;
	DropBoxClass *drop_box = (DropBoxClass *) gclass;

	drop_box->path_dropped = NULL;
	drop_box->clear = NULL;
	
	widget->drag_data_received = drop_box_drag_data_received;

	g_signal_new("path_dropped",
			G_TYPE_FROM_CLASS(gclass),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET(DropBoxClass, path_dropped),
			NULL, NULL,
			g_cclosure_marshal_VOID__STRING,
			G_TYPE_NONE, 1, G_TYPE_STRING);

	g_signal_new("clear",
			G_TYPE_FROM_CLASS(gclass),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET(DropBoxClass, clear),
			NULL, NULL,
			g_cclosure_marshal_VOID__VOID,
			G_TYPE_NONE, 0);
}

static void drop_box_init(GTypeInstance *object, gpointer gclass)
{
	DropBox *drop_box = (DropBox *) object;

	drop_box->path = NULL;
	drop_box->label = NULL;
	drop_box->buttons = NULL;
}

static void clear_clicked(GtkWidget *button, DropBox *drop_box)
{
	g_signal_emit_by_name(drop_box, "clear");
}

static void open_dir_clicked(GtkWidget *button, DropBox *drop_box)
{
	if (drop_box->path)
		open_to_show(drop_box->path);
	else
		delayed_error(_("I can't show you the currently set item, "
				"because nothing is currently set. Drag "
				"something onto me!"));
}

static void drop_box_drag_data_received(GtkWidget *drop_box,
			      GdkDragContext    *context,
			      gint              x,
			      gint              y,
			      GtkSelectionData  *selection_data,
			      guint             drag_info,
			      guint32           time)
{
	GList *uris = NULL;
	guchar *path = NULL;
	gboolean success = FALSE;

	if (!selection_data->data)
		goto err; 		/* Timeout? */

	uris = uri_list_to_glist(selection_data->data);

	if (g_list_length(uris) != 1)
	{
		delayed_error(_("Sorry, you need to drop exactly one file "
				"onto the drop area."));
		goto err;
	}
		
	path = get_local_path((EscapedPath *) uris->data);

	if (!path)
	{
		delayed_error(
			_("Sorry, I can't use '%s' because it's not a local "
			  "file."), (guchar *) uris->data);
		goto err;
	}

	if (!file_exists(path))
	{
		delayed_error(_("Can't access '%s':\n%s"), path,
				g_strerror(errno));
		goto err;
	}

	g_signal_emit_by_name(drop_box, "path_dropped", path);

	success = TRUE;
err:
	if (path)
		g_free(path);
	
	if (uris)
		g_list_free(uris);
	gtk_drag_finish(context, success, FALSE, time);	/* Failure */
}
