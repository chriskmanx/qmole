/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#include "main.h"
#include "menu.h"

#include "cache_maint.h"
#include "collect.h"
#include "collect-dlg.h"
#include "dupe.h"
#include "editors.h"
#include "filedata.h"
#include "img-view.h"
#include "pixbuf_util.h"
#include "preferences.h"
#include "slideshow.h"
#include "utilops.h"
#include "ui_fileops.h"
#include "ui_tabcomp.h"
#include "ui_menu.h"

static GtkWidget *real_submenu_add_alter(GtkWidget *menu, GCallback func, gpointer data,
					 GtkAccelGroup *accel_group);

/*
 *-----------------------------------------------------------------------------
 * menu utils
 *-----------------------------------------------------------------------------
 */

static GtkWidget *add_menu_item(GtkWidget *menu, gchar *label, GtkAccelGroup *accel_group,
				guint accel_key, guint accel_mods, GCallback func, gpointer data)
{
	GtkWidget *item;

	item = gtk_menu_item_new_with_label(label);
	gtk_widget_add_accelerator(item, "activate", accel_group, accel_key, accel_mods, GTK_ACCEL_VISIBLE);
	g_signal_connect(G_OBJECT(item), "activate", func, data);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
	gtk_widget_show(item);

	return item;
}

gpointer submenu_item_get_data(GtkWidget *menu)
{
	if (!menu->parent || !GTK_IS_MENU(menu->parent)) return NULL;

	return g_object_get_data(G_OBJECT(menu->parent), "submenu_data");
}

/*
 *-----------------------------------------------------------------------------
 * edit menu
 *-----------------------------------------------------------------------------
 */
static void edit_item_destroy_cb(GtkWidget *widget, gpointer data)
{
	g_free(data);
}

static void add_edit_items(GtkWidget *menu, GCallback func, GList *fd_list)
{
	GList *editors_list = editor_list_get();
	GList *work = editors_list;

	while (work)
		{
		const EditorDescription *editor = work->data;
		work = work->next;
		gboolean active = TRUE;

		if (fd_list && EDITOR_ERRORS(editor_command_parse(editor, fd_list, FALSE, NULL)))
			active = FALSE;

		if (active)
			{
			GtkWidget *item;
			const gchar *stock_id = NULL;
			gchar *key = g_strdup(editor->key);

			if (editor->icon && register_theme_icon_as_stock(key, editor->icon))
				stock_id = key;

			item = menu_item_add_stock(menu, editor->name, stock_id, func, key);
			g_signal_connect(G_OBJECT(item), "destroy", G_CALLBACK(edit_item_destroy_cb), key);
			}
		}
	
	g_list_free(editors_list);
}


GtkWidget *submenu_add_edit(GtkWidget *menu, GtkWidget **menu_item, GCallback func, gpointer data, GList *fd_list)
{
	GtkWidget *item;
	GtkWidget *submenu;

	item = menu_item_add(menu, _("_Edit"), NULL, NULL);

	submenu = gtk_menu_new();
	g_object_set_data(G_OBJECT(submenu), "submenu_data", data);
	add_edit_items(submenu, func, fd_list);

	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);

	if (menu_item) *menu_item = item;

	return submenu;
}

/*
 *-----------------------------------------------------------------------------
 * sorting
 *-----------------------------------------------------------------------------
 */

gchar *sort_type_get_text(SortType method)
{
	switch (method)
		{
		case SORT_SIZE:
			return _("Sort by size");
			break;
		case SORT_TIME:
			return _("Sort by date");
			break;
		case SORT_NONE:
			return _("Unsorted");
			break;
		case SORT_PATH:
			return _("Sort by path");
			break;
		case SORT_NUMBER:
			return _("Sort by number");
			break;
		case SORT_NAME:
		default:
			return _("Sort by name");
			break;
		}

	return "";
}

static GtkWidget *submenu_add_sort_item(GtkWidget *menu,
					GCallback func, SortType type,
					gboolean show_current, SortType show_type)
{
	GtkWidget *item;

	if (show_current)
		{
		item = menu_item_add_radio(menu,
					   sort_type_get_text(type), GINT_TO_POINTER((gint)type), (type == show_type),
					   func, GINT_TO_POINTER((gint)type));
		}
	else
		{
		item = menu_item_add(menu, sort_type_get_text(type),
				     func, GINT_TO_POINTER((gint)type));
		}

	return item;
}

GtkWidget *submenu_add_sort(GtkWidget *menu, GCallback func, gpointer data,
			    gboolean include_none, gboolean include_path,
			    gboolean show_current, SortType type)
{
	GtkWidget *submenu;

	submenu = gtk_menu_new();
	g_object_set_data(G_OBJECT(submenu), "submenu_data", data);

	submenu_add_sort_item(submenu, func, SORT_NAME, show_current, type);
#ifdef HAVE_STRVERSCMP
	submenu_add_sort_item(submenu, func, SORT_NUMBER, show_current, type);
#endif
	submenu_add_sort_item(submenu, func, SORT_TIME, show_current, type);
	submenu_add_sort_item(submenu, func, SORT_SIZE, show_current, type);
	if (include_path) submenu_add_sort_item(submenu, func, SORT_PATH, show_current, type);
	if (include_none) submenu_add_sort_item(submenu, func, SORT_NONE, show_current, type);

	if (menu)
		{
		GtkWidget *item;

		item = menu_item_add(menu, _("Sort"), NULL, NULL);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);
		return item;
		}

	return submenu;
}

/*
 *-----------------------------------------------------------------------------
 * altering
 *-----------------------------------------------------------------------------
 */

gchar *alter_type_get_text(AlterType type)
{
	switch (type)
		{
		case ALTER_ROTATE_90:
			return _("_Rotate clockwise");
			break;
		case ALTER_ROTATE_90_CC:
			return _("Rotate _counterclockwise");
			break;
		case ALTER_ROTATE_180:
			return _("Rotate _180");
			break;
		case ALTER_MIRROR:
			return _("_Mirror");
			break;
		case ALTER_FLIP:
			return _("_Flip");
			break;
		case ALTER_NONE:
			return _("_Original state");
			break;
		default:
			break;
		}

	return "";
}

static void submenu_add_alter_item(GtkWidget *menu, GCallback func, AlterType type,
				   GtkAccelGroup *accel_group, guint accel_key, guint accel_mods)
{
	if (accel_group)
		{
		add_menu_item(menu, alter_type_get_text(type), accel_group,
			      accel_key, accel_mods, func, GINT_TO_POINTER((gint)type));

		}
	else
		{
		menu_item_add(menu, alter_type_get_text(type), func, GINT_TO_POINTER((gint)type));
		}
}

static GtkWidget *real_submenu_add_alter(GtkWidget *menu, GCallback func, gpointer data,
					 GtkAccelGroup *accel_group)
{
	GtkWidget *submenu;

	submenu = gtk_menu_new();
	g_object_set_data(G_OBJECT(submenu), "submenu_data", data);

	submenu_add_alter_item(submenu, func, ALTER_ROTATE_90, accel_group, ']', 0);
	submenu_add_alter_item(submenu, func, ALTER_ROTATE_90_CC, accel_group, '[', 0);
	submenu_add_alter_item(submenu, func, ALTER_ROTATE_180, accel_group, 'R', GDK_SHIFT_MASK);
	submenu_add_alter_item(submenu, func, ALTER_MIRROR, accel_group, 'M', GDK_SHIFT_MASK);
	submenu_add_alter_item(submenu, func, ALTER_FLIP, accel_group, 'F', GDK_SHIFT_MASK);
	submenu_add_alter_item(submenu, func, ALTER_NONE, accel_group, 'O', GDK_SHIFT_MASK);

	if (menu)
		{
		GtkWidget *item;

		item = menu_item_add(menu, _("_Orientation"), NULL, NULL);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);
		return item;
		}

	return submenu;
}

GtkWidget *submenu_add_alter(GtkWidget *menu, GCallback func, gpointer data)
{
	return real_submenu_add_alter(menu, func, data, NULL);
}
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
