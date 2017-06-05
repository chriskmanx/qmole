/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2006 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "leafpad.h"
#include "string.h"

#define DV(x)

static void dnd_drag_data_recieved_handler(GtkWidget *widget,
	GdkDragContext *context, gint x, gint y,
	GtkSelectionData *selection_data, guint info, guint time);
static gboolean dnd_drag_motion_handler(GtkWidget *widget,
	GdkDragContext *context, gint x, gint y, guint time);

enum {
	TARGET_SELF,
	TARGET_UTF8_STRING,
	TARGET_COMPOUND_TEXT,
	TARGET_PLAIN,
	TARGET_URI_LIST,
};

static GtkTargetEntry drag_types[] =
{
#if !GTK_CHECK_VERSION(2, 10, 0)
//	{ "application/x-gtk-text-buffer-rich-text", GTK_TARGET_SAME_WIDGET, TARGET_SELF },
	{ "GTK_TEXT_BUFFER_CONTENTS", GTK_TARGET_SAME_WIDGET, TARGET_SELF },
#endif
	{ "UTF8_STRING", 0, TARGET_UTF8_STRING },
	{ "COMPOUND_TEXT", 0, TARGET_COMPOUND_TEXT },
	{ "text/plain", 0, TARGET_PLAIN },
	{ "text/uri-list", 0, TARGET_URI_LIST }
};

static gint n_drag_types = sizeof(drag_types) / sizeof(drag_types[0]);

void dnd_init(GtkWidget *widget)
{
	gtk_drag_dest_set(widget, GTK_DEST_DEFAULT_ALL,
		drag_types, n_drag_types, GDK_ACTION_COPY);
	g_signal_connect(G_OBJECT(widget), "drag_data_received",
		G_CALLBACK(dnd_drag_data_recieved_handler), NULL);
	g_signal_connect(G_OBJECT(widget), "drag_motion",
		G_CALLBACK(dnd_drag_motion_handler), NULL);
}


static void dnd_open_first_file(gchar *filename)
{
	FileInfo *fi;
	
	if (check_text_modification())
		return;
	fi = g_malloc(sizeof(FileInfo));
	fi->filename = g_strdup(filename);
	fi->charset = pub->fi->charset_flag ? g_strdup(pub->fi->charset) : NULL;
	fi->charset_flag = pub->fi->charset_flag;
	fi->lineend = LF;
	if (file_open_real(pub->mw->view, fi))
		g_free(fi);
	else {
		g_free(pub->fi);
		pub->fi = fi;
		undo_clear_all(pub->mw->buffer);
		set_main_window_title();
//		undo_init(sd->mainwin->textview, sd->mainwin->textbuffer, sd->mainwin->menubar);
	}
}	

static void dnd_drag_data_recieved_handler(GtkWidget *widget,
	GdkDragContext *context, gint x, gint y,
	GtkSelectionData *selection_data, guint info, guint time)
{
	static gboolean flag_called_once = FALSE;
	gchar **files;
	gchar *filename;
	gchar *comline;
	gint i = 0, j = 0;
	gchar *filename_sh;
	gchar **strs;
#ifdef ENABLE_CSDI
	j = 1;
#endif
DV(g_print("DND start!\n"));
	
#if GTK_CHECK_VERSION(2, 10, 0)
	if (g_strcasecmp(gdk_atom_name(context->targets->data),
	    "GTK_TEXT_BUFFER_CONTENTS") != 0) {
#else
	if (info != TARGET_SELF) {
#endif
		if (flag_called_once) {
			flag_called_once = FALSE;
			g_signal_stop_emission_by_name(widget, "drag_data_received");
DV(g_print("second drop signal killed.\n"));
			return;
		} else
			flag_called_once = TRUE;
	}
	
DV({	
	g_print("info                      = %d\n", info);
	g_print("time                      = %d\n", time);
	g_print("context->protocol         = %d\n", context->protocol);
	g_print("context->is_source        = %d\n", context->is_source);
	g_print("context->targets          = %d\n", g_list_length(context->targets));
	g_print("context->target           = %s\n", gdk_atom_name(context->targets->data));
/*	g_print("context->target           = %s\n", gdk_atom_name(context->targets->next->data));
	g_print("context->target           = %s\n", gdk_atom_name(context->targets->next->next->data));
	g_print("context->actions          = %d\n", context->actions);
	g_print("context->suggested_action = %d\n", context->suggested_action);
	g_print("context->action           = %d\n", context->action);
	g_print("selection_data->selection = %s\n", gdk_atom_name(selection_data->selection));
	g_print("selection_data->target    = %s\n", gdk_atom_name(selection_data->target));
*/	g_print("selection_data->type      = %s\n", gdk_atom_name(selection_data->type));
	g_print("selection_data->format    = %d\n", selection_data->format);
	g_print("selection_data->length    = %d\n", selection_data->length);
	g_print("%s\n", selection_data->data);
});	
	
	if (selection_data->data && info == TARGET_URI_LIST) {
		files = g_strsplit((gchar *)selection_data->data, "\n" , -1);
		while (files[i]) {
			if (strlen(files[i]) == 0)
				break;
			filename = g_strstrip(parse_file_uri(files[i]));
			if (i + j == 0)
				dnd_open_first_file(filename);
			else {
				if (i + j == 1)
					save_config_file();
				if (strstr(filename, " ")) {
					strs = g_strsplit(filename, " ", -1);
					filename_sh = g_strjoinv("\\ ", strs);
					g_strfreev(strs);
				} else
					filename_sh = g_strdup(filename);
				comline = g_strdup_printf("%s %s", PACKAGE, filename_sh);
DV(g_print(">%s\n", comline));
				g_free(filename_sh);
				g_spawn_command_line_async(comline, NULL);
				g_free(comline);
			}
			g_free(filename);
			i++;
		}
		g_strfreev(files);
	}
	else {
		clear_current_keyval();
		undo_set_sequency(FALSE);
#if GTK_CHECK_VERSION(2, 10, 0)
		if (info == TARGET_UTF8_STRING) {
#else
		if (info == TARGET_SELF) {
#endif
			undo_set_sequency_reserve();
			context->action = GDK_ACTION_MOVE;
		} else if (info == TARGET_PLAIN 
			&& g_utf8_validate((gchar *)selection_data->data, -1, NULL)) {
			selection_data->type =
				gdk_atom_intern("UTF8_STRING", FALSE);
		}
	}
	
	return;
}

static gboolean dnd_drag_motion_handler(GtkWidget *widget,
	GdkDragContext *context, gint x, gint y, guint time)
{
	GList *targets;
	gchar *name;
	gboolean flag = FALSE;
	
	targets = context->targets;
	while (targets) {
		name = gdk_atom_name(targets->data);
DV(g_print("%s\n", name));
		if (g_ascii_strcasecmp(name, "text/uri-list") == 0)
			flag = TRUE;
		g_free(name);
		targets = targets->next;
	}
/*	if (flag)
		context->action = GDK_ACTION_DEFAULT;
	else
		context->action = GDK_ACTION_COPY;
//	g_signal_stop_emission_by_name(widget, "drag_motion");
*/	
/*	if (!flag) {
		gint bx, by;
		GtkTextIter iter;
		
		gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(widget),
			GTK_TEXT_WINDOW_WIDGET,
			x, y, &bx, &by);
		gtk_text_view_get_iter_at_location(GTK_TEXT_VIEW(widget), &iter, bx, by);
		if (!dnd_mark) {
			dnd_mark = gtk_text_buffer_create_mark(GTK_TEXT_VIEW(widget)->buffer,
			    NULL, &iter, TRUE);
			gtk_text_mark_set_visible(dnd_mark, TRUE);
		} else
			gtk_text_mark_set_visible(dnd_mark, FALSE);
			gtk_text_buffer_move_mark(GTK_TEXT_VIEW(widget)->buffer,
			    dnd_mark, &iter);
			gtk_text_mark_set_visible(dnd_mark, TRUE);
	}
*/	
	return flag;
}

