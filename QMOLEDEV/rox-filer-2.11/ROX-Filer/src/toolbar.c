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

/* toolbar.c - for the button bars that go along the tops of windows */

#include "config.h"

#include <string.h>

#include "global.h"

#include "toolbar.h"
#include "options.h"
#include "support.h"
#include "main.h"
#include "menu.h"
#include "dnd.h"
#include "filer.h"
#include "display.h"
#include "pixmaps.h"
#include "bind.h"
#include "type.h"
#include "dir.h"
#include "diritem.h"
#include "view_iface.h"
#include "bookmarks.h"
#include "gui_support.h"

typedef struct _Tool Tool;

typedef enum {DROP_NONE, DROP_TO_PARENT, DROP_TO_HOME, DROP_BOOKMARK} DropDest;

struct _Tool {
	const gchar	*label;
	const gchar	*name;
	const gchar	*tip;		/* Tooltip */
	void		(*clicked)(GtkWidget *w, FilerWindow *filer_window);
	DropDest	drop_action;
	gboolean	enabled;
	gboolean	menu;		/* Activate on button-press */
};

Option o_toolbar, o_toolbar_info, o_toolbar_disable;
Option o_toolbar_min_width;

static FilerWindow *filer_window_being_counted;

/* TRUE if the button presses (or released) should open a new window,
 * rather than reusing the existing one.
 */
#define NEW_WIN_BUTTON(button_event)	\
  (o_new_button_1.int_value		\
   	? ((GdkEventButton *) button_event)->button == 1	\
	: ((GdkEventButton *) button_event)->button != 1)

/* Static prototypes */
static void toolbar_close_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_up_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_home_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_bookmarks_clicked(GtkWidget *widget,
				      FilerWindow *filer_window);
static void toolbar_help_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_refresh_clicked(GtkWidget *widget,
				    FilerWindow *filer_window);
static void toolbar_size_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_autosize_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_details_clicked(GtkWidget *widget,
				    FilerWindow *filer_window);
static void toolbar_hidden_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static void toolbar_select_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static void toolbar_sort_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static GtkWidget *add_button(GtkWidget *bar, Tool *tool,
				FilerWindow *filer_window);
static GtkWidget *create_toolbar(FilerWindow *filer_window);
static gboolean drag_motion(GtkWidget		*widget,
                            GdkDragContext	*context,
                            gint		x,
                            gint		y,
                            guint		time,
			    FilerWindow		*filer_window);
static void drag_leave(GtkWidget	*widget,
                       GdkDragContext	*context,
		       guint32		time,
		       FilerWindow	*filer_window);
static void handle_drops(FilerWindow *filer_window,
			 GtkWidget *button,
			 DropDest dest);
static void toggle_selected(GtkToggleButton *widget, gpointer data);
static void option_notify(void);
static GList *build_tool_options(Option *option, xmlNode *node, guchar *label);
static void tally_items(gpointer key, gpointer value, gpointer data);

static Tool all_tools[] = {
	{N_("Close"), GTK_STOCK_CLOSE, N_("Close filer window"),
	 toolbar_close_clicked, DROP_NONE, FALSE,
	 FALSE},
	 
	{N_("Up"), GTK_STOCK_GO_UP, N_("Change to parent directory"),
	 toolbar_up_clicked, DROP_TO_PARENT, TRUE,
	 FALSE},
	 
	{N_("Home"), GTK_STOCK_HOME, N_("Change to home directory"),
	 toolbar_home_clicked, DROP_TO_HOME, TRUE,
	 FALSE},
	
	{N_("Bookmarks"), ROX_STOCK_BOOKMARKS, N_("Bookmarks menu"),
	 toolbar_bookmarks_clicked, DROP_BOOKMARK, FALSE,
	 TRUE},

	{N_("Scan"), GTK_STOCK_REFRESH, N_("Rescan directory contents"),
	 toolbar_refresh_clicked, DROP_NONE, TRUE,
	 FALSE},
	
	{N_("Size"), GTK_STOCK_ZOOM_IN, N_("Change icon size"),
	 toolbar_size_clicked, DROP_NONE, TRUE,
	 FALSE},
	
	{N_("Size"), GTK_STOCK_ZOOM_FIT, N_("Automatic size mode"),
	 toolbar_autosize_clicked, DROP_NONE, TRUE,
	 FALSE},
	
	{N_("Details"), ROX_STOCK_SHOW_DETAILS, N_("Show extra details"),
	 toolbar_details_clicked, DROP_NONE, TRUE,
	 FALSE},
	
	{N_("Sort"), GTK_STOCK_SORT_ASCENDING, N_("Change sort criteria"),
	 toolbar_sort_clicked, DROP_NONE, FALSE,
	 FALSE},
	
	{N_("Hidden"), ROX_STOCK_SHOW_HIDDEN, N_("Left: Show/hide hidden files\n"
						 "Right: Show/hide thumbnails"),
	 toolbar_hidden_clicked, DROP_NONE, TRUE,
	 FALSE},
	
	{N_("Select"), ROX_STOCK_SELECT, N_("Select all/invert selection"),
	 toolbar_select_clicked, DROP_NONE, FALSE,
	 FALSE},
 	
	{N_("Help"), GTK_STOCK_HELP, N_("Show ROX-Filer help"),
	 toolbar_help_clicked, DROP_NONE, TRUE,
	 FALSE},
};


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void toolbar_init(void)
{
	option_add_int(&o_toolbar, "toolbar_type", TOOLBAR_NORMAL);
	option_add_int(&o_toolbar_info, "toolbar_show_info", 1);
	option_add_string(&o_toolbar_disable, "toolbar_disable",
					GTK_STOCK_CLOSE);
	option_add_int(&o_toolbar_min_width, "toolbar_min_width", 1);
	option_add_notify(option_notify);
	
	option_register_widget("tool-options", build_tool_options);
}

void toolbar_update_info(FilerWindow *filer_window)
{
	gchar		*label;
	ViewIface	*view;
	int		n_selected;

	g_return_if_fail(filer_window != NULL);

	if (o_toolbar.int_value == TOOLBAR_NONE || !o_toolbar_info.int_value)
		return;		/* Not showing info */

	if (filer_window->target_cb)
		return;

	view = filer_window->view;

	n_selected = view_count_selected(view);

	if (n_selected == 0)
	{
		gchar *s = NULL;
		int   n_items;

		if (filer_window->scanning)
		{
			gtk_label_set_text(
				GTK_LABEL(filer_window->toolbar_text), "");
			return;
		}

		if (!(filer_window->show_hidden ||
		      filer_window->temp_show_hidden) ||
		    filer_window->filter!=FILER_SHOW_ALL)
		{
			GHashTable *hash = filer_window->directory->known_items;
			int	   tally = 0;

			filer_window_being_counted=filer_window;
			g_hash_table_foreach(hash, tally_items, &tally);

			if (tally)
				s = g_strdup_printf(_(" (%u hidden)"), tally);
		}

		n_items = view_count_items(view);

		if (n_items)
			label = g_strdup_printf("%d %s%s",
					n_items,
					n_items != 1 ? _("items") : _("item"),
					s ? s : "");
		else /* (French plurals work differently for zero) */
			label = g_strdup_printf(_("No items%s"),
					s ? s : "");
		g_free(s);
	}
	else
	{
		double	size = 0;
		ViewIter iter;
		DirItem *item;

		view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);

		while ((item = iter.next(&iter)))
		{
			if (item->base_type != TYPE_DIRECTORY &&
			    item->base_type != TYPE_UNKNOWN)
				size += (double) item->size;
		}

		label = g_strdup_printf(_("%u selected (%s)"),
				n_selected, format_double_size(size));
	}

	gtk_label_set_text(GTK_LABEL(filer_window->toolbar_text), label);
	g_free(label);
}

/* Create, destroy or recreate toolbar for this window so that it
 * matches the option setting.
 */
void toolbar_update_toolbar(FilerWindow *filer_window)
{
	g_return_if_fail(filer_window != NULL);

	if (filer_window->toolbar)
	{
		gtk_widget_destroy(filer_window->toolbar);
		filer_window->toolbar = NULL;
		filer_window->toolbar_text = NULL;
	}

	if (o_toolbar.int_value != TOOLBAR_NONE)
	{
		filer_window->toolbar = create_toolbar(filer_window);
		gtk_box_pack_start(filer_window->toplevel_vbox,
				filer_window->toolbar, FALSE, TRUE, 0);
		gtk_box_reorder_child(filer_window->toplevel_vbox,
				filer_window->toolbar, 0);
		gtk_widget_show_all(filer_window->toolbar);
	}

	filer_target_mode(filer_window, NULL, NULL, NULL);
	toolbar_update_info(filer_window);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Wrapper for gtk_get_current_event() which creates a fake release event
 * if there is no current event. This is for ATK.
 */
static GdkEvent *get_current_event(int default_type)
{
	GdkEvent *event;

	event = gtk_get_current_event();

	if (event)
		return event;

	event = gdk_event_new(default_type);
	if (default_type == GDK_BUTTON_PRESS || default_type == GDK_BUTTON_RELEASE)
	{
		GdkEventButton *bev;
		bev = (GdkEventButton *) event;
		bev->button = 1;
	}
	return event;
}

static void toolbar_help_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE &&
			((GdkEventButton *) event)->button != 1)
		menu_rox_help(NULL, HELP_MANUAL, NULL);
	else
		filer_opendir(make_path(app_dir, "Help"), NULL, NULL);
	gdk_event_free(event);
}

static void toolbar_refresh_clicked(GtkWidget *widget,
				    FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE &&
			((GdkEventButton *) event)->button != 1)
	{
		filer_opendir(filer_window->sym_path, filer_window, NULL);
	}
	else
		filer_refresh(filer_window);
	gdk_event_free(event);
}

static void toolbar_home_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE && NEW_WIN_BUTTON(event))
	{
		filer_opendir(home_dir, filer_window, NULL);
	}
	else
		filer_change_to(filer_window, home_dir, NULL);
	gdk_event_free(event);
}

static void toolbar_bookmarks_clicked(GtkWidget *widget,
				      FilerWindow *filer_window)
{
	GdkEvent	*event;

	g_return_if_fail(filer_window != NULL);

	event = get_current_event(GDK_BUTTON_PRESS);
	if (event->type == GDK_BUTTON_PRESS &&
			((GdkEventButton *) event)->button == 1)
	{
		bookmarks_show_menu(filer_window);
	}
	else if (event->type == GDK_BUTTON_RELEASE &&
			((GdkEventButton *) event)->button != 1)
	{
		bookmarks_edit();
	}
	gdk_event_free(event);
}

static void toolbar_close_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEvent	*event;

	g_return_if_fail(filer_window != NULL);

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE &&
			((GdkEventButton *) event)->button != 1)
	{
		filer_opendir(filer_window->sym_path, filer_window, NULL);
	}
	else if (!filer_window_delete(filer_window->window, NULL, filer_window))
		gtk_widget_destroy(filer_window->window);
	gdk_event_free(event);
}

static void toolbar_up_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE && NEW_WIN_BUTTON(event))
	{
		filer_open_parent(filer_window);
	}
	else
		change_to_parent(filer_window);
	gdk_event_free(event);
}

static void toolbar_autosize_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEventButton	*bev;

	bev = (GdkEventButton *) get_current_event(GDK_BUTTON_RELEASE);
	if (bev->type == GDK_BUTTON_RELEASE)
	{
		display_set_layout(filer_window, AUTO_SIZE_ICONS, filer_window->details_type,
				TRUE);
	}
	gdk_event_free((GdkEvent *) bev);
}

static void toolbar_size_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEventButton	*bev;

	bev = (GdkEventButton *) get_current_event(GDK_BUTTON_RELEASE);
	if (bev->type == GDK_BUTTON_RELEASE)
		display_change_size(filer_window, bev->button == 1);
	gdk_event_free((GdkEvent *) bev);
}

static void toolbar_sort_clicked(GtkWidget *widget,
				    FilerWindow *filer_window)
{
	GdkEventButton	*bev;
	int i, current, next, next_wrapped;
	gboolean adjust;
	GtkSortType dir;
	gchar *tip;

	static const SortType sorts[]={
		SORT_NAME, SORT_TYPE, SORT_DATE, SORT_SIZE,
		SORT_OWNER, SORT_GROUP, 
	};
	static const char *sort_names[] = {
		N_("Sort by name"), N_("Sort by type"), N_("Sort by date"), 
		N_("Sort by size"), N_("Sort by owner"), N_("Sort by group"), 
	};

	bev = (GdkEventButton *) get_current_event(GDK_BUTTON_RELEASE);
	adjust = (bev->button != 1) && bev->type == GDK_BUTTON_RELEASE;
	gdk_event_free((GdkEvent *) bev);

	current = -1;
	dir = filer_window->sort_order;
	for (i=0; i < G_N_ELEMENTS(sort_names); i++)
	{
		if (filer_window->sort_type == sorts[i])
		{
			current = i;
			break;
		}
	}

	if (current == -1)
		next = 0;
	else if (adjust)
		next = current - 1;
	else
		next = current + 1;

	next_wrapped = next % G_N_ELEMENTS(sorts);

	if (next_wrapped != next)
		dir = (dir == GTK_SORT_ASCENDING)
			? GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;

	display_set_sort_type(filer_window, sorts[next_wrapped], dir);
 	tip = g_strconcat(_(sort_names[next_wrapped]), ", ",
			dir == GTK_SORT_ASCENDING 
				? _("ascending") : _("descending"),
			NULL);
	tooltip_show(tip);
	g_free(tip);
}

static void toolbar_details_clicked(GtkWidget *widget,
				    FilerWindow *filer_window)
{
	if (filer_window->view_type == VIEW_TYPE_DETAILS)
		filer_set_view_type(filer_window, VIEW_TYPE_COLLECTION);
	else
		filer_set_view_type(filer_window, VIEW_TYPE_DETAILS);
}

static void toolbar_hidden_clicked(GtkWidget *widget,
				   FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE &&
			((GdkEventButton *) event)->button == 1)
	{
		display_set_hidden(filer_window, !filer_window->show_hidden);
	}
	else
	{
		display_set_thumbs(filer_window, !filer_window->show_thumbs);
	}
}

static gboolean invert_cb(ViewIter *iter, gpointer data)
{
	return !view_get_selected((ViewIface *) data, iter);
}

static void toolbar_select_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE)
	{
		if (((GdkEventButton *) event)->button == 1)
			view_select_all(filer_window->view);
		else
			view_select_if(filer_window->view, invert_cb,
				       filer_window->view);
	}
	filer_window->temp_item_selected = FALSE;
	gdk_event_free(event);
}

/* If filer_window is NULL, the toolbar is for the options window */
static GtkWidget *create_toolbar(FilerWindow *filer_window)
{
	GtkWidget	*bar;
	GtkWidget	*b;
	int		i;
	int		width;

	bar = gtk_toolbar_new();

	if (o_toolbar.int_value == TOOLBAR_NORMAL || !filer_window)
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_ICONS);
	else if (o_toolbar.int_value == TOOLBAR_HORIZONTAL)
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_BOTH_HORIZ);
	else
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_BOTH);

	width=0;
	for (i = 0; i < sizeof(all_tools) / sizeof(*all_tools); i++)
	{
		Tool	*tool = &all_tools[i];
		GtkRequisition req;

		if (filer_window && !tool->enabled)
			continue;

		b = add_button(bar, tool, filer_window);
		
		gtk_widget_size_request(b, &req);
		width+=req.width;
		
		if (filer_window && tool->drop_action != DROP_NONE)
			handle_drops(filer_window, b, tool->drop_action);
	}

	if (filer_window)
	{
		if(o_toolbar_min_width.int_value)
		{
			/* Make the toolbar wide enough for all icons to be
			   seen, plus a little for the (start of the) text
			   label */
			gtk_widget_set_size_request(bar, width+32, -1);
		} else {
			gtk_widget_set_size_request(bar, 100, -1);
		}
		
		filer_window->toolbar_text = gtk_label_new("");
		gtk_misc_set_alignment(GTK_MISC(filer_window->toolbar_text),
					0, 0.5);
		gtk_toolbar_append_widget(GTK_TOOLBAR(bar),
				filer_window->toolbar_text, NULL, NULL);
	}

	return bar;
}

/* This is used to simulate a click when button 3 is used (GtkButton
 * normally ignores this).
 */
static gint toolbar_other_button = 0;
static gint toolbar_button_pressed(GtkButton *button,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	gint	b = event->button;
	Tool	*tool;

	tool = g_object_get_data(G_OBJECT(button), "rox-tool");
	g_return_val_if_fail(tool != NULL, TRUE);

	if (tool->menu && b == 1)
	{
		tool->clicked((GtkWidget *) button, filer_window);
		return TRUE;
	}

	if ((b == 2 || b == 3) && toolbar_other_button == 0)
	{
		toolbar_other_button = event->button;
		gtk_grab_add(GTK_WIDGET(button));
		gtk_button_pressed(button);

		return TRUE;
	}

	return FALSE;
}

static gint toolbar_button_released(GtkButton *button,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	if (event->button == toolbar_other_button)
	{
		toolbar_other_button = 0;
		gtk_grab_remove(GTK_WIDGET(button));
		gtk_button_released(button);

		return TRUE;
	}

	return FALSE;
}

/* If filer_window is NULL, the toolbar is for the options window */
static GtkWidget *add_button(GtkWidget *bar, Tool *tool,
				FilerWindow *filer_window)
{
	GtkWidget 	*button, *icon_widget;

	icon_widget = gtk_image_new_from_stock(tool->name,
						GTK_ICON_SIZE_LARGE_TOOLBAR);

	button = gtk_toolbar_insert_element(GTK_TOOLBAR(bar),
			   filer_window ? GTK_TOOLBAR_CHILD_BUTTON
					: GTK_TOOLBAR_CHILD_TOGGLEBUTTON,
			   NULL,
			   _(tool->label),
			   _(tool->tip), NULL,
			   icon_widget,
			   NULL, NULL,	/* CB, userdata */
			   GTK_TOOLBAR(bar)->num_children);
	GTK_WIDGET_UNSET_FLAGS(button, GTK_CAN_FOCUS);
	
	if (o_toolbar.int_value == TOOLBAR_HORIZONTAL)
	{
		GtkWidget *hbox, *label;
		GList	  *kids;
		hbox = GTK_BIN(button)->child;
		kids = gtk_container_get_children(GTK_CONTAINER(hbox));
		label = g_list_nth_data(kids, 1);
		g_list_free(kids);
		
		if (label)
		{
			gtk_box_set_child_packing(GTK_BOX(hbox), label,
						TRUE, TRUE, 0, GTK_PACK_END);
			gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
		}
	}

	g_object_set_data(G_OBJECT(button), "rox-tool", tool);

	if (filer_window)
	{
		g_signal_connect(button, "clicked",
			G_CALLBACK(tool->clicked), filer_window);
		g_signal_connect(button, "button_press_event",
			G_CALLBACK(toolbar_button_pressed), filer_window);
		g_signal_connect(button, "button_release_event",
			G_CALLBACK(toolbar_button_released), filer_window);
	}
	else
	{
		g_signal_connect(button, "clicked",
			G_CALLBACK(toggle_selected), NULL);
		g_object_set_data(G_OBJECT(button), "tool_name",
				  (gpointer) tool->name);
	}

	return button;
}

static void toggle_selected(GtkToggleButton *widget, gpointer data)
{
	option_check_widget(&o_toolbar_disable);
}

/* Called during the drag when the mouse is in a widget registered
 * as a drop target. Returns TRUE if we can accept the drop.
 */
static gboolean drag_motion(GtkWidget		*widget,
                            GdkDragContext	*context,
                            gint		x,
                            gint		y,
                            guint		time,
			    FilerWindow		*filer_window)
{
	GdkDragAction	action = context->suggested_action;
	DropDest	dest;
	gpointer	type = (gpointer) drop_dest_dir;

	dest = (DropDest) g_object_get_data(G_OBJECT(widget), "toolbar_dest");

	if ((context->actions & GDK_ACTION_ASK) && o_dnd_left_menu.int_value &&
		dest != DROP_BOOKMARK)
	{
		guint state;
		gdk_window_get_pointer(NULL, NULL, NULL, &state);
		if (state & GDK_BUTTON1_MASK)
			action = GDK_ACTION_ASK;
	}

	if (dest == DROP_TO_HOME)
		g_dataset_set_data(context, "drop_dest_path",
				   (gchar *) home_dir);
	else if (dest == DROP_BOOKMARK)
		type = (gpointer) drop_dest_bookmark;
	else
		g_dataset_set_data_full(context, "drop_dest_path",
				g_path_get_dirname(filer_window->sym_path),
				g_free);
	
	g_dataset_set_data(context, "drop_dest_type", type);
	gdk_drag_status(context, action, time);
	
	dnd_spring_load(context, filer_window);
	gtk_button_set_relief(GTK_BUTTON(widget), GTK_RELIEF_NORMAL);

	return TRUE;
}

static void drag_leave(GtkWidget	*widget,
                       GdkDragContext	*context,
		       guint32		time,
		       FilerWindow	*filer_window)
{
	gtk_button_set_relief(GTK_BUTTON(widget), GTK_RELIEF_NONE);
	dnd_spring_abort();
}

static void handle_drops(FilerWindow *filer_window,
			 GtkWidget *button,
			 DropDest dest)
{
	make_drop_target(button, 0);
	g_signal_connect(button, "drag_motion",
			G_CALLBACK(drag_motion), filer_window);
	g_signal_connect(button, "drag_leave",
			G_CALLBACK(drag_leave), filer_window);
	g_object_set_data(G_OBJECT(button), "toolbar_dest", (gpointer) dest);
}

static void tally_items(gpointer key, gpointer value, gpointer data)
{
	DirItem *item = (DirItem *) value;
	int     *tally = (int *) data;

	if (!filer_match_filter(filer_window_being_counted, item))
		(*tally)++;
}

static void option_notify(void)
{
	int		i;
	gboolean	changed = FALSE;
	guchar		*list = o_toolbar_disable.value;

	for (i = 0; i < sizeof(all_tools) / sizeof(*all_tools); i++)
	{
		Tool	*tool = &all_tools[i];
		gboolean old = tool->enabled;

		tool->enabled = !in_list(tool->name, list);

		if (old != tool->enabled)
			changed = TRUE;
	}

	if (changed || o_toolbar.has_changed || o_toolbar_info.has_changed)
	{
		GList	*next;

		for (next = all_filer_windows; next; next = next->next)
		{
			FilerWindow *filer_window = (FilerWindow *) next->data;
			
			toolbar_update_toolbar(filer_window);
		}
	}
}

static void update_tools(Option *option)
{
	GList	*next, *kids;

	kids = gtk_container_get_children(GTK_CONTAINER(option->widget));

	for (next = kids; next; next = next->next)
	{
		GtkToggleButton	*kid = (GtkToggleButton *) next->data;
		guchar		*name;

		name = g_object_get_data(G_OBJECT(kid), "tool_name");

		g_return_if_fail(name != NULL);
		
		gtk_toggle_button_set_active(kid,
					 !in_list(name, option->value));
	}

	g_list_free(kids);
}

static guchar *read_tools(Option *option)
{
	GList	*next, *kids;
	GString	*list;
	guchar	*retval;

	list = g_string_new(NULL);

	kids = gtk_container_get_children(GTK_CONTAINER(option->widget));

	for (next = kids; next; next = next->next)
	{
		GtkToggleButton	*kid = (GtkToggleButton *) next->data;
		guchar		*name;

		if (!gtk_toggle_button_get_active(kid))
		{
			name = g_object_get_data(G_OBJECT(kid), "tool_name");
			g_return_val_if_fail(name != NULL, list->str);

			if (list->len)
				g_string_append(list, ", ");
			g_string_append(list, name);
		}
	}

	g_list_free(kids);
	retval = list->str;
	g_string_free(list, FALSE);

	return retval;
}

static GList *build_tool_options(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*bar;

	g_return_val_if_fail(option != NULL, NULL);

	bar = create_toolbar(NULL);

	option->update_widget = update_tools;
	option->read_widget = read_tools;
	option->widget = bar;

	return g_list_append(NULL, bar);
}
