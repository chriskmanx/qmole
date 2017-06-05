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

/* dnd.c - code for handling drag and drop */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <sys/param.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>

#include "global.h"

#include "view_iface.h"
#include "dnd.h"
#include "type.h"
#include "filer.h"
#include "action.h"
#include "pixmaps.h"
#include "gui_support.h"
#include "support.h"
#include "options.h"
#include "run.h"
#include "pinboard.h"
#include "dir.h"
#include "diritem.h"
#include "usericons.h"
#include "menu.h"
#include "bookmarks.h"

#define MAXURILEN 4096		/* Longest URI to allow */

gint drag_start_x, drag_start_y;
MotionType motion_state = MOTION_NONE;

static GList *prompt_local_paths = NULL;
static gchar *prompt_dest_path = NULL;

/* This keeps track of how many mouse buttons are currently down.
 * We add a grab when it does 0->1 and release it on 1<-0.
 *
 * It may also be set to zero to disable the motion system (eg,
 * when popping up a menu).
 */
gint motion_buttons_pressed = 0;

/* Static prototypes */
static void set_xds_prop(GdkDragContext *context, const char *text);
static void desktop_drag_data_received(GtkWidget      		*widget,
				GdkDragContext  	*context,
				gint            	x,
				gint            	y,
				GtkSelectionData 	*selection_data,
				guint               	info,
				guint32             	time,
				FilerWindow		*filer_window);
static void got_data_xds_reply(GtkWidget 		*widget,
		  		GdkDragContext 		*context,
				GtkSelectionData 	*selection_data,
				guint32             	time);
static void got_data_raw(GtkWidget 		*widget,
			GdkDragContext 		*context,
			GtkSelectionData 	*selection_data,
			guint32             	time);
static void got_uri_list(GtkWidget 		*widget,
			 GdkDragContext 	*context,
			 const char	 	*selection_data,
			 guint32             	time);
static gboolean drag_drop(GtkWidget 	  *widget,
			  GdkDragContext  *context,
			  gint            x,
			  gint            y,
			  guint           time,
			  gpointer	  data);
static void drag_data_received(GtkWidget      		*widget,
			GdkDragContext  	*context,
			gint            	x,
			gint            	y,
			GtkSelectionData 	*selection_data,
			guint               	info,
			guint32             	time,
			gpointer		user_data);
static gboolean spring_now(gpointer data);
static void spring_win_destroyed(GtkWidget *widget, gpointer data);
static void menuitem_response(gpointer data, guint action, GtkWidget *widget);
static void prompt_action(GList *paths, gchar *dest);

typedef enum {
	MENU_COPY,
	MENU_MOVE,
	MENU_LINK_REL,
	MENU_LINK_ABS,
} MenuActionType;

#undef N_
#define N_(x) x
static GtkItemFactoryEntry menu_def[] = {
{N_("Copy"),		NULL, menuitem_response, MENU_COPY, 	NULL},
{N_("Move"),		NULL, menuitem_response, MENU_MOVE, 	NULL},
{N_("Link (relative)"),	NULL, menuitem_response, MENU_LINK_REL, NULL},
{N_("Link (absolute)"),	NULL, menuitem_response, MENU_LINK_ABS,	NULL},
};
static GtkWidget *dnd_menu = NULL;

/* Possible values for drop_dest_type (can also be NULL).
 * In either case, drop_dest_path is the app/file/dir to use.
 */
const char *drop_dest_prog = "drop_dest_prog";	/* Run a program */
const char *drop_dest_dir  = "drop_dest_dir";	/* Save to path */
const char *drop_dest_pass_through  = "drop_dest_pass";	/* Pass to parent */
const char *drop_dest_bookmark = "drop_dest_bookmark";	/* Add to bookmarks */

GdkAtom XdndDirectSave0;
GdkAtom xa_text_plain;
GdkAtom text_uri_list;
GdkAtom text_x_moz_url;
GdkAtom xa_application_octet_stream;
GdkAtom xa_string; /* Not actually used for DnD, but the others are here! */

int spring_in_progress = 0;	/* Non-zero changes filer_opendir slightly */

Option o_dnd_drag_to_icons;
Option o_dnd_spring_open;
static Option o_dnd_spring_delay;
static Option o_dnd_middle_menu;
Option o_dnd_left_menu;
static Option o_dnd_uri_handler;

void dnd_init(void)
{
	XdndDirectSave0 = gdk_atom_intern("XdndDirectSave0", FALSE);
	xa_text_plain = gdk_atom_intern("text/plain", FALSE);
	text_uri_list = gdk_atom_intern("text/uri-list", FALSE);
	text_x_moz_url = gdk_atom_intern("text/x-moz-url", FALSE);
	xa_application_octet_stream = gdk_atom_intern("application/octet-stream",
			FALSE);
	xa_string = gdk_atom_intern("STRING", FALSE);

	option_add_int(&o_dnd_drag_to_icons, "dnd_drag_to_icons", 1);
	option_add_int(&o_dnd_spring_open, "dnd_spring_open", 0);
	option_add_int(&o_dnd_spring_delay, "dnd_spring_delay", 400);
	option_add_int(&o_dnd_left_menu, "dnd_left_menu", TRUE);
	option_add_int(&o_dnd_middle_menu, "dnd_middle_menu", TRUE);

	option_add_string(&o_dnd_uri_handler, "dnd_uri_handler",
			"xterm -e wget $1");
}

/*			SUPPORT FUNCTIONS			*/

/* Set the XdndDirectSave0 property on the source window for this context */
static void set_xds_prop(GdkDragContext *context, const char *text)
{
	gdk_property_change(context->source_window,
			XdndDirectSave0,
			xa_text_plain, 8,
			GDK_PROP_MODE_REPLACE,
			text,
			strlen(text));
}

static char *get_xds_prop(GdkDragContext *context)
{
	guchar	*prop_text;
	gint	length;

	if (gdk_property_get(context->source_window,
			XdndDirectSave0,
			xa_text_plain,
			0, MAXURILEN,
			FALSE,
			NULL, NULL,
			&length, &prop_text) && prop_text)
	{
		/* Terminate the string */
		prop_text = g_realloc(prop_text, length + 1);
		prop_text[length] = '\0';
		/* Note: assuming UTF-8 (should convert here) */
		return prop_text;
	}

	return NULL;
}

/* Is the sender willing to supply this target type? */
gboolean provides(GdkDragContext *context, GdkAtom target)
{
	GList	    *targets = context->targets;

	while (targets && ((GdkAtom) targets->data != target))
		targets = targets->next;

	return targets != NULL;
}

/*			DRAGGING FROM US			*/

/* The user has held the mouse button down over a group of item and moved - 
 * start a drag. 'uri_list' is copied, so you can delete it straight away.
 */
void drag_selection(GtkWidget *widget, GdkEventMotion *event, guchar *uri_list)
{
	GdkPixbuf	*pixbuf;
	GdkDragContext 	*context;
	GdkDragAction	actions;
	GtkTargetList   *target_list;
	GtkTargetEntry 	target_table[] = {
		{"text/uri-list", 0, TARGET_URI_LIST},
		{"UTF8_STRING", 0, TARGET_UTF8},
	};
		
	if (event->state & GDK_BUTTON1_MASK)
		actions = GDK_ACTION_COPY | GDK_ACTION_MOVE
			| GDK_ACTION_LINK | GDK_ACTION_ASK;
	else
	{
		if (o_dnd_middle_menu.int_value)
			actions = GDK_ACTION_ASK;
		else
			actions = GDK_ACTION_MOVE;
	}
	
	target_list = gtk_target_list_new(target_table,
					G_N_ELEMENTS(target_table));

	context = gtk_drag_begin(widget,
			target_list,
			actions,
			(event->state & GDK_BUTTON1_MASK) ? 1 :
			(event->state & GDK_BUTTON2_MASK) ? 2 : 3,
			(GdkEvent *) event);

	g_dataset_set_data_full(context, "uri_list",
				g_strdup(uri_list), g_free);

	pixbuf = gtk_widget_render_icon(widget, GTK_STOCK_DND_MULTIPLE,
					GTK_ICON_SIZE_DIALOG, NULL);
	gtk_drag_set_icon_pixbuf(context, pixbuf, 0, 0);
	g_object_unref(pixbuf);
}

/* Copy/Load this item into another directory/application */
void drag_one_item(GtkWidget		*widget,
		   GdkEventMotion	*event,
		   const guchar		*full_path,
		   DirItem		*item,
		   MaskedPixmap		*image)
{
	guchar		*uri, *tmp;
	GdkDragContext 	*context;
	GdkDragAction	actions;
	GtkTargetList   *target_list;
	GtkTargetEntry 	target_table[] = {
		{"text/uri-list", 0, TARGET_URI_LIST},
		{"UTF8_STRING", 0, TARGET_UTF8},
		{"application/octet-stream", 0, TARGET_RAW},
		{"", 0, TARGET_RAW},
	};

	g_return_if_fail(full_path != NULL);
	g_return_if_fail(item != NULL);

	if (!image)
		image = di_image(item);

	if (item->base_type == TYPE_FILE)
	{
		MIME_type *t = item->mime_type;
		
		target_table[3].target = g_strconcat(t->media_type, "/",
						     t->subtype, NULL);
		target_list = gtk_target_list_new(target_table,
					G_N_ELEMENTS(target_table));
		g_free(target_table[3].target);
	}
	else
		target_list = gtk_target_list_new(target_table, 2);

	if (event->state & GDK_BUTTON1_MASK)
		actions = GDK_ACTION_COPY | GDK_ACTION_ASK
			| GDK_ACTION_MOVE | GDK_ACTION_LINK;
	else
	{
		if (o_dnd_middle_menu.int_value)
			actions = GDK_ACTION_ASK;
		else
			actions = GDK_ACTION_MOVE;
	}
	
	context = gtk_drag_begin(widget,
			target_list,
			actions,
			(event->state & GDK_BUTTON1_MASK) ? 1 :
			(event->state & GDK_BUTTON2_MASK) ? 2 : 3,
			(GdkEvent *) event);

	g_dataset_set_data_full(context, "full_path",
			g_strdup(full_path), g_free);
	tmp = (char *) encode_path_as_uri(full_path);
	uri = g_strconcat(tmp, "\r\n", NULL);
	/*printf("%s\n", tmp);*/
	g_free(tmp);
	g_dataset_set_data_full(context, "uri_list", uri, g_free);

	g_return_if_fail(image != NULL);

	gtk_drag_set_icon_pixbuf(context, image->pixbuf, 0, 0);
}

/* Convert text/uri-list data to UTF8_STRING.
 * g_free() the result.
 */
static gchar *uri_list_to_utf8(const char *uri_list)
{
	GString *new;
	GList *uris, *next_uri;
	char *string;

	new = g_string_new(NULL);

	uris = uri_list_to_glist(uri_list);

	for (next_uri = uris; next_uri; next_uri = next_uri->next)
	{
		EscapedPath *uri = next_uri->data;
		char *local;

		local = get_local_path(uri);

		if (new->len)
			g_string_append_c(new, ' ');

		if (local)
		{
			g_string_append(new, local);
			g_free(local);
		}
		else
			g_warning("Not local!\n");

		g_free(uri);
	}

	if (uris)	
		g_list_free(uris);

	string = new->str;
	g_string_free(new, FALSE);

	return string;
}

/* Called when a remote app wants us to send it some data.
 * TODO: Maybe we should handle errors better (ie, let the remote app know
 * the drag has failed)?
 */
void drag_data_get(GtkWidget          		*widget,
			GdkDragContext     	*context,
			GtkSelectionData   	*selection_data,
			guint               	info,
			guint32             	time,
			gpointer		data)
{
	char		*to_send = "E";	/* Default to sending an error */
	long		to_send_length = 1;
	gboolean	delete_once_sent = FALSE;
	GdkAtom		type;
	guchar		*path;

	type = selection_data->target;

	switch (info)
	{
		case	TARGET_RAW:
			path = g_dataset_get_data(context, "full_path");
			if (path && load_file(path, &to_send, &to_send_length))
			{
				delete_once_sent = TRUE;
				break;
			}
			g_warning("drag_data_get: Can't find path!\n");
			return;
		case	TARGET_UTF8:
		{
			char *uri_list;
			uri_list = g_dataset_get_data(context, "uri_list");
			to_send = uri_list_to_utf8(uri_list);
			to_send_length = strlen(to_send);
			delete_once_sent = TRUE;
			break;
		}
		case	TARGET_URI_LIST:
			to_send = g_dataset_get_data(context, "uri_list");
			to_send_length = strlen(to_send);
			type = text_uri_list;		/* (needed for xine) */
			delete_once_sent = FALSE;
			break;
		default:
			delayed_error("drag_data_get: %s",
					_("Internal error - bad info type"));
			break;
	}

	gtk_selection_data_set(selection_data,
			type,
			8,
			to_send,
			to_send_length);

	if (delete_once_sent)
		g_free(to_send);
}

/*			DRAGGING TO US				*/

/* Set up this widget as a drop-target.
 * Does not attach any motion handlers.
 */
void make_drop_target(GtkWidget *widget, GtkDestDefaults defaults)
{
	GtkTargetEntry 	target_table[] =
	{
		{"text/uri-list", 0, TARGET_URI_LIST},
		{"text/x-moz-url", 0, TARGET_MOZ_URL},
		{"XdndDirectSave0", 0, TARGET_XDS},
		{"application/octet-stream", 0, TARGET_RAW},
	};

	gtk_drag_dest_set(widget,
			defaults,
			target_table,
			sizeof(target_table) / sizeof(*target_table),
			GDK_ACTION_COPY | GDK_ACTION_ASK | GDK_ACTION_MOVE
			| GDK_ACTION_LINK | GDK_ACTION_PRIVATE);

	g_signal_connect(widget, "drag_drop", G_CALLBACK(drag_drop), NULL);
	g_signal_connect(widget, "drag_data_received",
			G_CALLBACK(drag_data_received), NULL);
}

/* Like drag_set_dest, but for a pinboard-type widget */
void drag_set_pinboard_dest(GtkWidget *widget)
{
	GtkTargetEntry 	target_table[] = {
		{"text/uri-list", 0, TARGET_URI_LIST},
	};

	gtk_drag_dest_set(widget,
			  GTK_DEST_DEFAULT_DROP,
			  target_table,
			  sizeof(target_table) / sizeof(*target_table),
			  GDK_ACTION_LINK);
	g_signal_connect(widget, "drag_data_received",
			    G_CALLBACK(desktop_drag_data_received), NULL);
}

/* item is the item the file is held over, NULL for directory background.
 * 'item' may be NULL on exit if the drop should be treated as onto the
 * background. Disallow drags to a selected icon before calling this.
 *
 * Returns NULL to reject the drop, or drop_dest_prog/drop_dest_dir to
 * accept. Build the path based on item.
 */
const guchar *dnd_motion_item(GdkDragContext *context, DirItem **item_p)
{
	DirItem	*item = *item_p;

	if (item)
	{
		/* If we didn't drop onto a directory, application or
		 * executable file then act as though the drop is to the
		 * window background.
		 */
		if (item->base_type != TYPE_DIRECTORY && !EXECUTABLE_FILE(item))
		{
			item = NULL;
			*item_p = NULL;
		}
	}

	if (!item)
	{
		/* Drop onto the window background */

		return drop_dest_dir;
	}

	/* Drop onto a program/directory of some sort */

	if (item->base_type == TYPE_DIRECTORY &&
			!(item->flags & ITEM_FLAG_APPDIR))
	{
		/* A normal directory */
		if (provides(context, text_uri_list) ||
				provides(context, text_x_moz_url) ||
				provides(context, XdndDirectSave0))
			return drop_dest_dir;
	}
	else
	{
		if (provides(context, text_uri_list) ||
				provides(context, text_x_moz_url) ||
				provides(context, xa_application_octet_stream))
			return drop_dest_prog;
	}

	return NULL;
}

/* User has tried to drop some data on us. Decide what format we would
 * like the data in.
 */
static gboolean drag_drop(GtkWidget 	  *widget,
			  GdkDragContext  *context,
			  gint            x,
			  gint            y,
			  guint           time,
			  gpointer	  data)
{
	const char	*error = NULL;
	char		*leafname = NULL;
	GdkAtom		target = GDK_NONE;
	char		*dest_path;
	char		*dest_type = NULL;
	
	dest_path = g_dataset_get_data(context, "drop_dest_path");
	dest_type = g_dataset_get_data(context, "drop_dest_type");

	if (dest_type == drop_dest_pass_through)
		return FALSE;	/* Let the parent widget handle it */

	if (dest_type == drop_dest_bookmark)
	{
		if (provides(context, text_uri_list))
			gtk_drag_get_data(widget, context, text_uri_list, time);
		else
		{
			gtk_drag_finish(context, FALSE, FALSE, time);
			delayed_error(_("Drag a directory here to "
					"bookmark it."));
		}
		return TRUE;
	}

	g_return_val_if_fail(dest_path != NULL, TRUE);

	if (dest_type == drop_dest_dir && provides(context, XdndDirectSave0))
	{
		leafname = get_xds_prop(context);
		if (leafname)
		{
			if (strchr(leafname, '/'))
			{
				error = _("XDS protocol error: "
					"leafname may not contain '/'\n");
				null_g_free(&leafname);
			}
			else
			{
				char *dest_uri;

				/* Not escaped. */
				dest_uri = g_strconcat("file://",
						our_host_name_for_dnd(),
						dest_path, NULL);

				set_xds_prop(context,
					make_path(dest_uri, leafname));

				g_free(dest_uri);

				target = XdndDirectSave0;
				g_dataset_set_data_full(context, "leafname",
						leafname, g_free);
			}
		}
		else
			error = _(
				"XdndDirectSave0 target provided, but the atom "
				"XdndDirectSave0 (type text/plain) did not "
					"contain a leafname\n");
	}
	else if (provides(context, text_uri_list))
		target = text_uri_list;
	else if (provides(context, text_x_moz_url))
		target = text_x_moz_url;
	else if (provides(context, xa_application_octet_stream))
		target = xa_application_octet_stream;
	else
	{
		if (dest_type == drop_dest_dir)
			error = _("Sorry - I require a target type of "
				"text/uri-list or XdndDirectSave0.");
		else
			error = _("Sorry - I require a target type of "
				"text/uri-list or application/octet-stream.");
	}

	if (error)
	{
		gtk_drag_finish(context, FALSE, FALSE, time);	/* Failure */
		
		delayed_error("%s", error);
	}
	else
		gtk_drag_get_data(widget, context, target, time);

	return TRUE;
}

/* Called when a text/uri-list arrives */
static void desktop_drag_data_received(GtkWidget      	*widget,
				       GdkDragContext  	*context,
				       gint            	x,
				       gint            	y,
				       GtkSelectionData *selection_data,
				       guint            info,
				       guint32          time,
				       FilerWindow	*filer_window)
{
	GList	*uris, *next;
	char *error_example = NULL;
	gint dx, dy;

	if (!selection_data->data)
	{
		/* Timeout? */
		return;
	}

	if (pinboard_drag_in_progress)
	{
		pinboard_move_icons();
		return;
	}
	
	gdk_window_get_position(widget->window, &dx, &dy);
	x += dx;
	y += dy;

	uris = uri_list_to_glist(selection_data->data);

	for (next = uris; next; next = next->next)
	{
		guchar	*path;

		path = get_local_path((EscapedPath *) next->data);
		if (path)
		{
			pinboard_pin(path, NULL, x, y, NULL);
			x += 64;
			g_free(path);
		}
		else if (!error_example)
			error_example = g_strdup(next->data);

		g_free(next->data);
	}

	if (uris)	
		g_list_free(uris);

	if (error_example)
	{
		delayed_error(_("Failed to add some items to the pinboard, "
			"because they are on a remote machine. For example:\n"
			"\n%s"), error_example);
		g_free(error_example);
	}
}

/* Convert Mozilla's text/x-moz-uri into a text/uri-list */
static void got_moz_uri(GtkWidget 		*widget,
			GdkDragContext 		*context,
			GtkSelectionData	*selection_data,
			guint32        		time)
{
	gchar *utf8, *uri_list, *eol;

	utf8 = g_utf16_to_utf8((gunichar2 *) selection_data->data,
			(glong) selection_data->length,
			NULL, NULL, NULL);

	eol = utf8 ? strchr(utf8, '\n') : NULL;
	if (!eol)
	{
		delayed_error("Invalid UTF16 from text/x-moz-url target");
		g_free(utf8);
		gtk_drag_finish(context, FALSE, FALSE, time);
		return;
	}

	*eol = '\0';
	uri_list = g_strconcat(utf8, "\r\n", NULL);
	g_free(utf8);

	got_uri_list(widget, context, uri_list, time);

	g_free(uri_list);
}

/* Called when some data arrives from the remote app (which we asked for
 * in drag_drop).
 */
static void drag_data_received(GtkWidget      	*widget,
			       GdkDragContext  	*context,
			       gint            	x,
			       gint            	y,
			       GtkSelectionData *selection_data,
			       guint            info,
			       guint32          time,
			       gpointer		user_data)
{
	if (!selection_data->data)
	{
		/* Timeout? */
		gtk_drag_finish(context, FALSE, FALSE, time);	/* Failure */
		return;
	}

	switch (info)
	{
		case TARGET_XDS:
			got_data_xds_reply(widget, context,
					selection_data, time);
			break;
		case TARGET_RAW:
			got_data_raw(widget, context, selection_data, time);
			break;
		case TARGET_URI_LIST:
			got_uri_list(widget, context, selection_data->data,
					time);
			break;
		case TARGET_MOZ_URL:
			got_moz_uri(widget, context, selection_data, time);
			break;
		default:
			gtk_drag_finish(context, FALSE, FALSE, time);
			delayed_error("drag_data_received: %s",
					_("Unknown target"));
			break;
	}
}

static void got_data_xds_reply(GtkWidget 		*widget,
		  		GdkDragContext 		*context,
				GtkSelectionData 	*selection_data,
				guint32             	time)
{
	gboolean	mark_unsafe = TRUE;
	char		response = *selection_data->data;
	const char	*error = NULL;
	char		*dest_path;

	dest_path = g_dataset_get_data(context, "drop_dest_path");

	if (selection_data->length != 1)
		response = '?';

	if (response == 'F')
	{
		/* Sender couldn't save there - ask for another
		 * type if possible.
		 */
		if (provides(context, xa_application_octet_stream))
		{
			mark_unsafe = FALSE;	/* Wait and see */

			gtk_drag_get_data(widget, context,
					xa_application_octet_stream, time);
		}
		else
			error = _("Remote app can't or won't send me "
					"the data - sorry");
	}
	else if (response == 'S')
	{
		/* Success - data is saved */
		mark_unsafe = FALSE;	/* It really is safe */
		gtk_drag_finish(context, TRUE, FALSE, time);

		refresh_dirs(dest_path);
	}
	else if (response != 'E')
	{
		error = _("XDS protocol error: "
			"return code should be 'S', 'F' or 'E'\n");
	}
	/* else: error has been reported by the sender */

	if (mark_unsafe)
	{
		set_xds_prop(context, "");
		/* Unsave also implies that the drag failed */
		gtk_drag_finish(context, FALSE, FALSE, time);
	}

	if (error)
		delayed_error("%s", error);
}

static void got_data_raw(GtkWidget 		*widget,
			GdkDragContext 		*context,
			GtkSelectionData 	*selection_data,
			guint32             	time)
{
	const char	*leafname;
	int		fd;
	const char	*error = NULL;
	const char	*dest_path;

	g_return_if_fail(selection_data->data != NULL);

	dest_path = g_dataset_get_data(context, "drop_dest_path");

	if (context->action == GDK_ACTION_ASK)
	{
		gtk_drag_finish(context, FALSE, FALSE, time);	/* Failure */
		delayed_error(_("Sorry, can't display a menu of actions "
				"for a remote file / raw data."));
		return;
	}

	if (g_dataset_get_data(context, "drop_dest_type") == drop_dest_prog)
	{
		/* The data needs to be sent to an application */
		run_with_data(dest_path,
				selection_data->data, selection_data->length);
		gtk_drag_finish(context, TRUE, FALSE, time);    /* Success! */
		return;
	}

	leafname = g_dataset_get_data(context, "leafname");
	if (!leafname)
		leafname = _("UntitledData");
	
	fd = open(make_path(dest_path, leafname),
		O_WRONLY | O_CREAT | O_EXCL | O_NOCTTY,
			S_IRUSR | S_IRGRP | S_IROTH |
			S_IWUSR | S_IWGRP | S_IWOTH);

	if (fd == -1)
		error = g_strerror(errno);
	else
	{
		if (write(fd,
			selection_data->data,
			selection_data->length) == -1)
				error = g_strerror(errno);

		if (close(fd) == -1 && !error)
			error = g_strerror(errno);

		refresh_dirs(dest_path);
	}
	
	if (error)
	{
		if (provides(context, XdndDirectSave0))
			set_xds_prop(context, "");
		gtk_drag_finish(context, FALSE, FALSE, time);	/* Failure */
		delayed_error(_("Error saving file: %s"), error);
	}
	else
		gtk_drag_finish(context, TRUE, FALSE, time);    /* Success! */
}

static gboolean uri_is_local(const EscapedPath *uri)
{
	char *path;
	path = get_local_path(uri);
	if (!path)
		return FALSE;
	g_free(path);
	return TRUE;
}

/* Run the shell command 'command', replacing $1 with 'arg' */
static void run_with_argument(const char *dir,
				const char *command,
				const char *arg)
{
	GPtrArray	*argv;

	argv = g_ptr_array_new();

	g_ptr_array_add(argv, "sh");
	g_ptr_array_add(argv, "-c");
	g_ptr_array_add(argv, (char *) command);
	g_ptr_array_add(argv, "sh");
	g_ptr_array_add(argv, (char *) arg);
	g_ptr_array_add(argv, NULL);

	rox_spawn(dir, (const gchar **) argv->pdata);

	g_ptr_array_free(argv, TRUE);
}

/* We've got a list of URIs from somewhere (probably another filer window).
 * If the files are on the local machine then try to copy them ourselves,
 * otherwise, if there was only one file and application/octet-stream was
 * provided, get the data via the X server.
 * For http:, https: or ftp: schemes, use the download handler.
 */
static void got_uri_list(GtkWidget 		*widget,
			 GdkDragContext 	*context,
			 const char 		*selection_data,
			 guint32             	time)
{
	GList		*uri_list;
	const char	*error = NULL;
	GList		*next_uri;
	gboolean	send_reply = TRUE;
	char		*dest_path;
	char		*type;
	
	dest_path = g_dataset_get_data(context, "drop_dest_path");
	type = g_dataset_get_data(context, "drop_dest_type");

	uri_list = uri_list_to_glist(selection_data);

	if (type == drop_dest_bookmark)
	{
		GList *next;
		for (next = uri_list; next; next = next->next)
			bookmarks_add_uri((EscapedPath *) next->data);
		destroy_glist(&uri_list);
		gtk_drag_finish(context, TRUE, FALSE, time);    /* Success! */
		return;
	}

	g_return_if_fail(dest_path != NULL);

	if (!uri_list)
		error = _("No URIs in the text/uri-list (nothing to do!)");
	else if (context->action != GDK_ACTION_ASK && type == drop_dest_prog)
		run_with_files(dest_path, uri_list);
	else if ((!uri_list->next) && !uri_is_local(uri_list->data))
	{
		/* There is one URI in the list, and it's not on the local
		 * machine. Get it via the X server if possible.
		 */

		if (provides(context, xa_application_octet_stream))
		{
			char	*leaf;
			leaf = strrchr(uri_list->data, '/');
			if (leaf)
				leaf++;
			else
				leaf = uri_list->data;
			g_dataset_set_data_full(context, "leafname",
				unescape_uri((EscapedPath *) leaf), g_free);
			gtk_drag_get_data(widget, context,
					xa_application_octet_stream, time);
			send_reply = FALSE;
		}
		else if ((strncasecmp(uri_list->data, "http:", 5) == 0) ||
			 (strncasecmp(uri_list->data, "https:", 6) == 0) ||
                         (strncasecmp(uri_list->data, "ftp:", 4) == 0))
		{
			run_with_argument(dest_path,
					o_dnd_uri_handler.value,
					(char *) uri_list->data);
                }
                else
			error = _("Can't get data from remote machine "
				"(application/octet-stream not provided)");
	}
	else
	{
		GList *local_paths = NULL;

		/* Either one local URI, or a list. If everything in the list
		 * isn't local then we are stuck.
		 */

		for (next_uri = uri_list; next_uri; next_uri = next_uri->next)
		{
			char *path;

			path = get_local_path((EscapedPath *) next_uri->data);
			/*printf("%s -> %s\n", (char *) next_uri->data,
			  path? path: "NULL");*/

			if (path) 
				local_paths = g_list_append(local_paths,
								path);
			else
				error = _("Some of these files are on a "
					"different machine - they will be "
					"ignored - sorry");
		}

		if (!local_paths)
		{
			error = _("None of these files are on the local "
				"machine - I can't operate on multiple "
				"remote files - sorry.");
		}
		else if (context->action == GDK_ACTION_ASK)
			prompt_action(local_paths, dest_path);
		else if (context->action == GDK_ACTION_MOVE)
			action_move(local_paths, dest_path, NULL, -1);
		else if (context->action == GDK_ACTION_COPY)
			action_copy(local_paths, dest_path, NULL, -1);
		else if (context->action == GDK_ACTION_LINK)
			action_link(local_paths, dest_path, NULL, TRUE);
		else
			error = _("Unknown action requested");

		destroy_glist(&local_paths);
	}

	if (error)
	{
		gtk_drag_finish(context, FALSE, FALSE, time);	/* Failure */
		delayed_error(_("Error getting file list: %s"), error);
	}
	else if (send_reply)
		gtk_drag_finish(context, TRUE, FALSE, time);    /* Success! */

	destroy_glist(&uri_list);
}

/* Called when an item from the ACTION_ASK menu is chosen */
static void menuitem_response(gpointer data, guint action, GtkWidget *widget)
{
	if (action == MENU_MOVE)
		action_move(prompt_local_paths, prompt_dest_path, NULL, -1);
	else if (action == MENU_COPY)
		action_copy(prompt_local_paths, prompt_dest_path, NULL, -1);
	else if (action == MENU_LINK_REL)
		action_link(prompt_local_paths, prompt_dest_path, NULL, TRUE);
	else if (action == MENU_LINK_ABS)
		action_link(prompt_local_paths, prompt_dest_path, NULL, FALSE);
} 

/* When some local files are dropped somewhere with ACTION_ASK, this
 * function is called to display the menu.
 */
static void prompt_action(GList *paths, gchar *dest)
{
	GList		*next;
	GdkEvent	*event;

	if (prompt_local_paths)
	{
		destroy_glist(&prompt_local_paths);
		null_g_free(&prompt_dest_path);
	}
	
	/* Make a copy of the arguments */
	for (next = paths; next; next = next->next)
		prompt_local_paths = g_list_append(prompt_local_paths,
						g_strdup((gchar *) next->data));
	prompt_dest_path = g_strdup(dest);

	if (!dnd_menu)
	{
		GtkItemFactory	*item_factory;

		item_factory = menu_create(menu_def,
				sizeof(menu_def) / sizeof(*menu_def),
				"<dnd>", NULL);
		dnd_menu = gtk_item_factory_get_widget(item_factory, "<dnd>");
	}

	/* Shade 'Set Icon' if there are multiple files */
	menu_set_items_shaded(dnd_menu, g_list_length(paths) != 1, 4, 1);

	event = gtk_get_current_event();
	show_popup_menu(dnd_menu, event, 1);
	if (event)
		gdk_event_free(event);
}


/*			SPRING-LOADING 				*/

/* This is the code that makes directories pop open if you hold a
 * file over them...
 *
 * First, call dnd_spring_load(context) to arm the system.
 * After a timeout (1/2 a second) the dest_path directory will be
 * opened in a new window, unless dnd_spring_abort is called first.
 */

static gint spring_timeout = -1;
static GdkDragContext *spring_context = NULL;
static FilerWindow *spring_window = NULL;
static FilerWindow *spring_src_window = NULL;

void dnd_spring_load(GdkDragContext *context, FilerWindow *src_win)
{
	g_return_if_fail(context != NULL);

	if (!o_dnd_spring_open.int_value)
		return;

	if (spring_context)
		dnd_spring_abort();
	
	spring_context = context;
	g_object_ref(spring_context);
	spring_src_window = src_win;
	spring_timeout = gtk_timeout_add(
			o_dnd_spring_delay.int_value, spring_now, NULL);
}

void dnd_spring_abort(void)
{
	if (!spring_context)
		return;

	g_object_unref(spring_context);
	spring_context = NULL;
	gtk_timeout_remove(spring_timeout);
}

/* If all mod keys are released, no buttons are pressed, and the
 * mouse is outside the spring window, then close it.
 */
static gboolean spring_check_idle(gpointer data)
{
	int	p_x, p_y;

	if (!spring_window)
		return FALSE;

	if (!get_pointer_xy(&p_x, &p_y))
	{
		/*
		GdkWindow	*win = spring_window->window->window;
		int		x, y;
		int		w, h;

		gdk_window_get_position(win, &x, &y);
		gdk_window_get_size(win, &w, &h);

		if (p_x < x || p_x > x + w || p_y < y || p_y > y + h)
		{
		*/

		gtk_widget_destroy(spring_window->window);
		return FALSE;		/* Got it! */
	}

	return TRUE;	/* Try again later */
}

static gboolean spring_now(gpointer data)
{
	const char	*type;
	const guchar	*dest_path;
	gint		x, y;
	
	g_return_val_if_fail(spring_context != NULL, FALSE);
	g_return_val_if_fail(!spring_in_progress, FALSE);

	type = g_dataset_get_data(spring_context, "drop_dest_type");
	if (type == drop_dest_bookmark)
	{
		bookmarks_edit();
		goto out;
	}

	dest_path = g_dataset_get_data(spring_context, "drop_dest_path");
	g_return_val_if_fail(dest_path != NULL, FALSE);

	/*
	 * Note: Due to a bug in gtk, if a window disappears during
	 * a drag and the pointer moves over where the window was,
	 * the sender crashes! Therefore, do not close any windows
	 * while dragging! (fixed in later versions)
	 */
	/*
	if (spring_window)
		gtk_widget_destroy(spring_window->window);
		*/

	get_pointer_xy(&x, &y);
	
	spring_in_progress++;
	if (spring_window)
	{
		view_cursor_to_iter(spring_window->view, NULL);
		filer_change_to(spring_window, dest_path, NULL);
		/* DON'T move the window. Gtk+ sometimes doesn't
		 * notice :-(
		 */
	}
	else
	{
		spring_window = filer_opendir(dest_path,
						spring_src_window, NULL);
		if (spring_window)
		{
			gtk_timeout_add(500, spring_check_idle, NULL);
			g_signal_connect(spring_window->window, "destroy",
					G_CALLBACK(spring_win_destroyed), NULL);
			centre_window(spring_window->window->window, x, y);
		}
	}
	spring_in_progress--;

out:
	dnd_spring_abort();

	return FALSE;
}

static void spring_win_destroyed(GtkWidget *widget, gpointer data)
{
	spring_window = NULL;
}

/*			HANDLING MOTION EVENTS				*/

/* If not-NULL, then this widget has a grab */
static GtkWidget *motion_widget = NULL;

/* If TRUE, we must gdk_pointer_ungrab() too when finishing */
static gboolean  motion_pointer_grab = FALSE;

/* Call this on a button press event. It stores the mouse position
 * as the start of the new drag and returns TRUE if all is well.
 * Further motions events are disabled at this point - you must
 * then call dnd_motion_start() to set the type of motion expected.
 * Grabs the widget on the first press.
 *
 * If the system is not ready to handle a motion event (because a
 * button is already held down?) it does nothing and returns FALSE.
 *
 * If the event is not a single click then it simply returns TRUE.
 */
gboolean dnd_motion_press(GtkWidget *widget, GdkEventButton *event)
{
	if (event->type != GDK_BUTTON_PRESS)
		return TRUE;		/* Not a click event! */

	motion_buttons_pressed++;
	if (motion_buttons_pressed == 1)
	{
		/* g_print("[ grab! ]\n"); */
		gtk_grab_add(widget);
		motion_widget = widget;
	}

	if (motion_state != MOTION_NONE)
		return FALSE;		/* Ignore clicks - we're busy! */
	
	motion_state = MOTION_DISABLED;
	drag_start_x = event->x_root;
	drag_start_y = event->y_root;

	return TRUE;
}

/* After the button press event, decide what kind of motion is expected.
 * If you don't call this then the motion system is disabled - call
 * dnd_motion_release() to reset it.
 *
 * Note: If you open a popup menu or start DND call dnd_motion_ungrab()
 * instead.
 */
void dnd_motion_start(MotionType motion)
{
	g_return_if_fail(motion_state == MOTION_DISABLED);

	motion_state = motion;
}

/* Call this on a button release event. If some buttons are still pressed,
 * returns TRUE and does nothing.
 *
 * Otherwise, it resets the motion system to be ready again and returns TRUE.
 *
 * If the motion system wasn't being used (MOTION_NONE) then it does nothing
 * and returns FALSE - process the release event yourself as it isn't part
 * of a motion. This also happens if a motion was primed but never happened.
 */
gboolean dnd_motion_release(GdkEventButton *event)
{
	MotionType	motion = motion_state;
	gint		drag_threshold;
	int		dx, dy;

	if (motion_buttons_pressed == 0)
		return TRUE;		/* We were disabled */

	if (motion_buttons_pressed == 1)
		dnd_motion_ungrab();
	else
	{
		motion_buttons_pressed--;
		return TRUE;
	}

	if (motion == MOTION_REPOSITION || motion == MOTION_DISABLED)
		return TRUE;	/* Already done something - eat the event */

	/* Eat release events that happen too far from the click
	 * source. Otherwise, allow the caller to treat this as a click
	 * that never became a motion.
	 */
	dx = event->x_root - drag_start_x;
	dy = event->y_root - drag_start_y;

	g_object_get(gtk_settings_get_default(),
		"gtk-dnd-drag-threshold", &drag_threshold,
		NULL);

	return ABS(dx) > drag_threshold || ABS(dy) > drag_threshold;
}

/* Use this to disable the motion system. The system will be reset once
 * all mouse buttons are released.
 */
void dnd_motion_disable(void)
{
	g_return_if_fail(motion_state != MOTION_NONE &&
			 motion_state != MOTION_DISABLED);

	motion_state = MOTION_DISABLED;
}

/* Use this if something else is going to grab the pointer so that
 * we won't get any more motion or release events.
 */
void dnd_motion_ungrab(void)
{
	if (motion_buttons_pressed > 0)
	{
		if (motion_pointer_grab)
		{
			gdk_pointer_ungrab(GDK_CURRENT_TIME);
			motion_pointer_grab = FALSE;
			/* g_print("[ ungrab_pointer ]\n"); */
		}
		gtk_grab_remove(motion_widget);
		motion_widget = NULL;
		motion_buttons_pressed = 0;
		/* g_print("[ ungrab ]\n"); */
	}

	motion_state = MOTION_NONE;
}

/* Call this on motion events. If the mouse position is far enough
 * from the click position, returns TRUE and does dnd_motion_ungrab().
 * You should then start regular drag-and-drop.
 * 
 * Otherwise, returns FALSE.
 */
gboolean dnd_motion_moved(GdkEventMotion *event)
{
	gint	drag_threshold;
	int	dx, dy;

	g_object_get(gtk_settings_get_default(),
		"gtk-dnd-drag-threshold", &drag_threshold,
		NULL);

	dx = event->x_root - drag_start_x;
	dy = event->y_root - drag_start_y;

	if (ABS(dx) <= drag_threshold && ABS(dy) <= drag_threshold)
		return FALSE;		/* Not far enough */

	dnd_motion_ungrab();

	return TRUE;
}

/* Normally, the X server will automatically grab the pointer on a
 * button press and ungrab on release. However, if the grab widget
 * is reparented then call this to re-aquire the grab.
 */
void dnd_motion_grab_pointer(void)
{
	g_return_if_fail(motion_widget != NULL);

	gdk_pointer_grab(motion_widget->window, FALSE,
			GDK_POINTER_MOTION_MASK |
			GDK_BUTTON_RELEASE_MASK,
			FALSE, NULL, GDK_CURRENT_TIME);

	motion_pointer_grab = TRUE;
}
