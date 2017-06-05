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

/* icon.c - abstract base class for pinboard and panel icons.
 *
 * An Icon contains the full pathname of its file and the DirItem for that
 * file. Icons emit the following signals:
 *
 * redraw - the image, details or selection state have changed.
 * update - the name or path has changed.
 * destroy - someone wishes to remove the icon.
 *
 * Note that an icon may be removed without emitting 'destroy'.
 */

#include "config.h"

#include <stdarg.h>
#include <string.h>
#include <gtk/gtk.h>
#include <X11/keysym.h>
#include <gdk/gdkx.h>

#include "global.h"

#include "main.h"
#include "gui_support.h"
#include "support.h"
#include "icon.h"
#include "diritem.h"
#include "menu.h"
#include "appmenu.h"
#include "dnd.h"
#include "run.h"
#include "infobox.h"
#include "pixmaps.h"
#include "mount.h"
#include "type.h"
#include "usericons.h"
#include "pinboard.h"	/* For pinboard_set_backdrop_box */

static gboolean have_primary = FALSE;	/* We own the PRIMARY selection? */

GtkWidget		*icon_menu;		/* The popup icon menu */
static GtkWidget	*icon_file_menu;	/* The file submenu */
static GtkWidget	*icon_file_item;	/* 'File' label */
static GtkWidget	*file_shift_item;	/* 'Shift Open' label */

/* A list of selected Icons. Every icon in the list is from the same group
 * (eg, you can't have icons from two different panels selected at the
 * same time).
 */
GList *icon_selection = NULL;

/* A list of Icons which have grabs in effect. The same combo may be
 * listed more than once, but has only one X grab.
 */
GList *icon_shortcuts = NULL;

#define CLICK_TO_SET _("(click to set)")

static unsigned int AltMask;
static unsigned int MetaMask;
static unsigned int NumLockMask;
static unsigned int ScrollLockMask;
static unsigned int CapsLockMask;
static unsigned int SuperMask;
static unsigned int HyperMask;

/* {MyKey -> Number of grabs} */
static GHashTable *grab_counter = NULL;

/* Each entry is a GList of Icons which have the given pathname.
 * This allows us to update all necessary icons when something changes.
 */
static GHashTable *icons_hash = NULL;	/* path -> [Icon] */

static Icon *menu_icon = NULL;	/* Item clicked if there is no selection */

/* Static prototypes */
static void rename_activate(GtkWidget *dialog);
static void menu_closed(GtkWidget *widget);
static void lose_selection(GtkClipboard *primary, gpointer data);
static void selection_get(GtkClipboard *primary, 
		       GtkSelectionData *selection_data,
		       guint      info,
		       gpointer   data);
static void remove_items(gpointer data, guint action, GtkWidget *widget);
static void file_op(gpointer data, guint action, GtkWidget *widget);
static void show_rename_box(Icon *icon);
static void icon_set_selected_int(Icon *icon, gboolean selected);
static void icon_class_init(gpointer gclass, gpointer data);
static void icon_init(GTypeInstance *object, gpointer gclass);
static void icon_hash_path(Icon *icon);
static void icon_unhash_path(Icon *icon);
static void ungrab_key(Icon *icon);
static void grab_key(Icon *icon);
static void parseKeyString(MyKey *key, const char *str);
static void icon_wink(Icon *icon);
static void initModifiers(void);
static void create_menu(void);

enum {
	ACTION_SHIFT,
	ACTION_PROPERTIES,
	ACTION_RUN_ACTION,
	ACTION_SET_ICON,
	ACTION_EDIT,
	ACTION_LOCATION,
};

#undef N_
#define N_(x) x
static GtkItemFactoryEntry menu_def[] = {
{N_("ROX-Filer"),		NULL, NULL, 0, "<Branch>"},
{">" N_("About ROX-Filer..."),	NULL, menu_rox_help, HELP_ABOUT, "<StockItem>", GTK_STOCK_DIALOG_INFO},
{">" N_("Show Help Files"),	NULL, menu_rox_help, HELP_DIR, "<StockItem>", GTK_STOCK_HELP},
{">" N_("Manual"),		NULL, menu_rox_help, HELP_MANUAL, NULL},
{">",				NULL, NULL, 0, "<Separator>"},
{">" N_("Options..."),		NULL, menu_show_options, 0, "<StockItem>", GTK_STOCK_PREFERENCES},
{">" N_("Home Directory"),	NULL, open_home, 0, "<StockItem>", GTK_STOCK_HOME},
{N_("File"),			NULL, NULL, 0, "<Branch>"},
{">" N_("Shift Open"),   	NULL, file_op, ACTION_SHIFT, NULL},
{">" N_("Properties"),    	NULL, file_op, ACTION_PROPERTIES, "<StockItem>", GTK_STOCK_PROPERTIES},
{">" N_("Set Run Action..."),	NULL, file_op, ACTION_RUN_ACTION, "<StockItem>", GTK_STOCK_EXECUTE},
{">" N_("Set Icon..."),		NULL, file_op, ACTION_SET_ICON, NULL},
{N_("Edit Item"),  		NULL, file_op, ACTION_EDIT, "<StockItem>", GTK_STOCK_PROPERTIES},
{N_("Show Location"),  		NULL, file_op, ACTION_LOCATION, "<StockItem>", GTK_STOCK_JUMP_TO},
{N_("Remove Item(s)"),		NULL, remove_items, 0, "<StockItem>", GTK_STOCK_REMOVE},
{"",				NULL, NULL, 0, "<Separator>"},
};

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Called when the pointer moves over the icon */
void icon_may_update(Icon *icon)
{
	MaskedPixmap	*image;
	int		flags;

	g_return_if_fail(icon != NULL);

	image = di_image(icon->item);
	flags = icon->item->flags;

	if (image)
		g_object_ref(image);
	mount_update(FALSE);
	diritem_restat(icon->path, icon->item, NULL);

	if (di_image(icon->item) != image || icon->item->flags != flags)
	{
		/* Appearance changed; need to redraw */
		g_signal_emit_by_name(icon, "redraw");
	}

	if (image)
		g_object_unref(image);
}

/* If path is on an icon then it may have changed... check! */
void icons_may_update(const gchar *path)
{
	GList	*affected;

	if (icons_hash)
	{
		affected = g_hash_table_lookup(icons_hash, path);

		for (; affected; affected = affected->next)
			icon_may_update((Icon *) affected->data);
	}
}

typedef struct _CheckData CheckData;
struct _CheckData {
	const gchar *path;
	gboolean    found;
};

static void check_has(gpointer key, GList *icons, CheckData *check)
{
	Icon	*icon;
	
	g_return_if_fail(icons != NULL);
	
	icon = icons->data;

	if (is_sub_dir(icon->path, check->path))
		check->found = TRUE;
}

/* Returns TRUE if any icon links to this file (or any file inside
 * this directory). Used to check that it's OK to delete 'path'.
 */
gboolean icons_require(const gchar *path)
{
	CheckData	check;

	if (!icons_hash)
		return FALSE;

	check.path = path;
	check.found = FALSE;
	g_hash_table_foreach(icons_hash, (GHFunc) check_has, &check);

	return check.found;
}

static gboolean any_selected_item_is_locked()
{
	GList *next;

	for (next = icon_selection; next; next = next->next)
	{
		Icon *icon = (Icon *)next->data;

		if (icon->locked)
			return TRUE;
	}

	return FALSE;
}

/* Menu was clicked over this icon. Set things up correctly (shade items,
 * add app menu stuff, etc).
 * You should show icon_menu after calling this...
 * panel_name is NULL for the pinboard.
 */
void icon_prepare_menu(Icon *icon, GtkWidget *options_item, ...)
{
	gboolean shaded;
	GSList *link;
	va_list ap;
	GtkWidget *trailing;
	static GtkWidget *current_options_item;	/* Pin/Pan Options */
	static GSList *current_trailing_items = NULL;

	appmenu_remove();

	if (current_options_item)
	{
		gtk_widget_destroy(current_options_item);
		current_options_item = NULL;
	}
	for (link = current_trailing_items; link; link = g_slist_next(link))
	{
		gtk_widget_destroy(link->data);
	}
	if (current_trailing_items)
	{
		g_slist_free(current_trailing_items);
		current_trailing_items = NULL;
	}

	menu_icon = icon;

	if (!icon_menu)
		create_menu();

	current_options_item = options_item;
	add_stock_to_menu_item(options_item, GTK_STOCK_PREFERENCES);

	gtk_menu_shell_append(GTK_MENU_SHELL(icon_menu), options_item);
	gtk_widget_show_all(options_item);

	va_start(ap, options_item);
	while ((trailing = va_arg(ap, GtkWidget *)) != NULL)
	{
		current_trailing_items = g_slist_prepend(current_trailing_items,
				trailing);
		gtk_menu_shell_append(GTK_MENU_SHELL(icon_menu), trailing);
		gtk_widget_show(trailing);
	}
	va_end(ap);
	
	/* Shade Remove Item(s) if any item is locked or nothing is selected */
	if (icon_selection)
		shaded = any_selected_item_is_locked();
	else if (menu_icon)
		shaded = menu_icon->locked;
	else
		shaded = TRUE;
	
	menu_set_items_shaded(icon_menu, shaded, 4, 1);

	menu_show_shift_action(file_shift_item, icon ? icon->item : NULL,
			FALSE);

	/* Shade the File/Edit/Show items unless an item was clicked */
	if (icon)
	{
		guchar *tmp;

		menu_set_items_shaded(icon_menu, FALSE, 1, 3);
		menu_set_items_shaded(icon_file_menu, FALSE, 0, 5);
		if (!can_set_run_action(icon->item))
			menu_set_items_shaded(icon_file_menu, TRUE, 2, 1);

		tmp = g_strdup_printf(_("%s '%s'"),
				basetype_name(icon->item),
				icon->item->leafname);
		gtk_label_set_text(GTK_LABEL(icon_file_item), tmp);
		g_free(tmp);

		/* Check for app-specific menu */
		appmenu_add(icon->path, icon->item, icon_menu);
	}
	else
	{
		menu_set_items_shaded(icon_menu, TRUE, 1, 3);
		menu_set_items_shaded(icon_file_menu, TRUE, 0, 5);
		gtk_label_set_text(GTK_LABEL(icon_file_item), _("Nothing"));
	}
}

/* Set whether this icon is selected. Will automatically clear the selection
 * if it contains icons from a different group.
 */
void icon_set_selected(Icon *icon, gboolean selected)
{
	if (selected && icon_selection)
	{
		IconClass	*iclass;
		Icon		*other = (Icon *) icon_selection->data;

		iclass = (IconClass *) G_OBJECT_GET_CLASS(icon);
		if (!iclass->same_group(icon, other))
		{
			icon_select_only(icon);
			return;
		}
	}

	icon_set_selected_int(icon, selected);
}

/* Clear everything, except 'select', which is selected.
 * If select is NULL, unselects everything.
 */
void icon_select_only(Icon *select)
{
	GList	*to_clear, *next;
	
	if (select)
		icon_set_selected_int(select, TRUE);

	to_clear = g_list_copy(icon_selection);

	if (select)
		to_clear = g_list_remove(to_clear, select);
		
	for (next = to_clear; next; next = next->next)
		icon_set_selected_int((Icon *) next->data, FALSE);

	g_list_free(to_clear);
}

/* Destroys this icon's widget, causing it to be removed from the screen */
void icon_destroy(Icon *icon)
{
	g_return_if_fail(icon != NULL);

	icon_set_selected_int(icon, FALSE);

	if (!icon->locked)
		g_signal_emit_by_name(icon, "destroy");
}

/* Return a text/uri-list of all the icons in the selection */
gchar *icon_create_uri_list(void)
{
	GString	*tmp;
	guchar	*retval;
	GList	*next;

	tmp = g_string_new(NULL);

	for (next = icon_selection; next; next = next->next)
	{
		Icon *icon = (Icon *) next->data;
		EscapedPath *uri;

		uri = encode_path_as_uri(icon->path);
		g_string_append(tmp, (char *) uri);
		g_free(uri);
		g_string_append(tmp, "\r\n");
	}

	retval = tmp->str;
	g_string_free(tmp, FALSE);
	
	return retval;
}

GType icon_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (IconClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			icon_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(Icon),
			0,			/* n_preallocs */
			icon_init
		};

		type = g_type_register_static(G_TYPE_OBJECT, "Icon",
					      &info, 0);
	}

	return type;
}

/* Sets, unsets or changes the pathname and name for an icon.
 * Updates icons_hash and (re)stats the item.
 * If name is NULL then gets the leafname from pathname.
 * If pathname is NULL, frees everything.
 */
void icon_set_path(Icon *icon, const char *pathname, const char *name)
{
	if (icon->path)
	{
		icon_unhash_path(icon);
		icon->src_path = NULL;
		icon->path = NULL;

		diritem_free(icon->item);
		icon->item = NULL;
	}

	if (pathname)
	{
		if (g_utf8_validate(pathname, -1, NULL))
			icon->src_path = g_strdup(pathname);
		else
			icon->src_path = to_utf8(pathname);
		icon->path = expand_path(icon->src_path);

		icon_hash_path(icon);

		if (!name)
			name = g_basename(icon->src_path);

		icon->item = diritem_new(name);
		diritem_restat(icon->path, icon->item, NULL);
	}
}

void icon_set_shortcut(Icon *icon, const gchar *shortcut)
{
	g_return_if_fail(icon != NULL);

	if (shortcut && !*shortcut)
		shortcut = NULL;
	if (icon->shortcut == shortcut)
		return;
	if (icon->shortcut && shortcut && strcmp(icon->shortcut, shortcut) == 0)
		return;

	initModifiers();

	ungrab_key(icon);

	g_free(icon->shortcut);
	icon->shortcut = g_strdup(shortcut);
	parseKeyString(&icon->shortcut_key, shortcut);

	grab_key(icon);
}

void icon_set_arguments(Icon *icon, const gchar *args)
{
	g_return_if_fail(icon != NULL);
	g_return_if_fail(args == NULL || icon->args != args);

	if (args && !*args)
		args = NULL;
	if (icon->args && args && strcmp(icon->args, args) == 0)
		return;

	g_free(icon->args);
	icon->args = g_strdup(args);
}

void icon_run(Icon *icon)
{
	if (icon->args == NULL)
		run_diritem(icon->path, icon->item, NULL, NULL, FALSE);
	else
		run_with_args(icon->path, icon->item, icon->args);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* The icons_hash table allows us to convert from a path to a list
 * of icons that use that path.
 * Add this icon to the list for its path.
 */
static void icon_hash_path(Icon *icon)
{
	GList	*list;

	g_return_if_fail(icon != NULL);

	/* g_print("[ hashing '%s' ]\n", icon->path); */

	list = g_hash_table_lookup(icons_hash, icon->path);
	list = g_list_prepend(list, icon);
	g_hash_table_insert(icons_hash, icon->path, list);
}

/* Remove this icon from the icons_hash table */
static void icon_unhash_path(Icon *icon)
{
	GList	*list;

	g_return_if_fail(icon != NULL);

	/* g_print("[ unhashing '%s' ]\n", icon->path); */
	
	list = g_hash_table_lookup(icons_hash, icon->path);
	g_return_if_fail(list != NULL);

	list = g_list_remove(list, icon);

	/* Remove it first; the hash key may have changed address */
	g_hash_table_remove(icons_hash, icon->path);
	if (list)
		g_hash_table_insert(icons_hash,
				((Icon *) list->data)->path, list);
}

static void rename_activate(GtkWidget *dialog)
{
	GtkWidget *entry, *src, *shortcut, *arg, *lock_state;
	Icon *icon;
	const guchar *new_name, *new_src, *new_shortcut, *new_args;
	gboolean new_lock_state;
	
	entry = g_object_get_data(G_OBJECT(dialog), "new_name");
	icon = g_object_get_data(G_OBJECT(dialog), "callback_icon");
	src = g_object_get_data(G_OBJECT(dialog), "new_path");
	shortcut = g_object_get_data(G_OBJECT(dialog), "new_shortcut");
	arg = g_object_get_data(G_OBJECT(dialog), "new_arg");
	lock_state = g_object_get_data(G_OBJECT(dialog), "new_lock_state");

	g_return_if_fail(entry != NULL &&
			 src != NULL &&
			 icon != NULL &&
			 shortcut != NULL &&
			 arg != NULL &&
			 lock_state != NULL);

	new_name = gtk_entry_get_text(GTK_ENTRY(entry));
	new_src = gtk_entry_get_text(GTK_ENTRY(src));
	new_shortcut = gtk_label_get_text(GTK_LABEL(shortcut));
	if (strcmp(new_shortcut, CLICK_TO_SET) == 0)
		new_shortcut = NULL;
	new_args = gtk_entry_get_text(GTK_ENTRY(arg));
	new_lock_state = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(lock_state));

	if (*new_src == '\0')
		report_error(
			_("The location must contain at least one character!"));
	else
	{
		icon_set_path(icon, new_src, new_name);
		icon_set_shortcut(icon, new_shortcut);
		icon_set_arguments(icon, new_args);
		icon->locked = new_lock_state;
		g_signal_emit_by_name(icon, "update");
		gtk_widget_destroy(dialog);
	}
}

static void menu_closed(GtkWidget *widget)
{
	appmenu_remove();
	menu_icon = NULL;
}

/* Called when another application takes the selection away from us */
static void lose_selection(GtkClipboard *primary, gpointer data)
{
	have_primary = FALSE;
	icon_select_only(NULL);
}

/* Called when another application wants the contents of our selection */
static void selection_get(GtkClipboard	*primary,
		          GtkSelectionData *selection_data,
		          guint		info,
		          gpointer	data)
{
	gchar *text;

	if (info == TARGET_URI_LIST)
		text = icon_create_uri_list();
	else
	{
		GList	*next;
		GString	*str;

		str = g_string_new(NULL);

		for (next = icon_selection; next; next = next->next)
		{
			Icon	*icon = (Icon *) next->data;

			g_string_append(str, icon->path);
			g_string_append_c(str, ' ');
		}

		text = str->str;
		g_string_free(str, FALSE);
	}

	gtk_selection_data_set_text(selection_data, text, strlen(text));
}

static void remove_items(gpointer data, guint action, GtkWidget *widget)
{
	IconClass	*iclass;

	if (menu_icon)
	{
		if (menu_icon->locked)
		{
			delayed_error(_("You must unlock '%s' before removing it"),
					menu_icon->item->leafname);
			return;
		}
		icon_set_selected(menu_icon, TRUE);
	}
	
	if (!icon_selection)
	{
		delayed_error(
			_("You must first select some items to remove"));
		return;
	}

	if (any_selected_item_is_locked())
	{
		delayed_error(_("An item must be unlocked before it can be removed."));
		return;
	}

	iclass = (IconClass *)
		  G_OBJECT_GET_CLASS(G_OBJECT(icon_selection->data));

	iclass->remove_items();
}

static void file_op(gpointer data, guint action, GtkWidget *widget)
{
	if (!menu_icon)
	{
		delayed_error(_("You must open the menu over an item"));
		return;
	}

	switch (action)
	{
		case ACTION_SHIFT:
			run_diritem(menu_icon->path, menu_icon->item,
					NULL, NULL, TRUE);
			break;
		case ACTION_EDIT:
			show_rename_box(menu_icon);
			break;
		case ACTION_LOCATION:
			open_to_show(menu_icon->path);
			break;
		case ACTION_PROPERTIES:
			infobox_new(menu_icon->path);
			break;
		case ACTION_RUN_ACTION:
			if (can_set_run_action(menu_icon->item))
				type_set_handler_dialog(
						menu_icon->item->mime_type);
			else
				report_error(
				_("You can only set the run action for a "
				"regular file"));
			break;
		case ACTION_SET_ICON:
			icon_set_handler_dialog(menu_icon->item,
						menu_icon->path);
			break;
	}
}

static void edit_response(GtkWidget *dialog, gint response, gpointer data)
{
	if (response == GTK_RESPONSE_OK)
		rename_activate(dialog);
	else if (response == GTK_RESPONSE_CANCEL)
		gtk_widget_destroy(dialog);
}

static GdkFilterReturn filter_get_key(GdkXEvent *xevent,
				      GdkEvent *event,
				      gpointer data)
{
	XKeyEvent *kev = (XKeyEvent *) xevent;
	GtkWidget *popup = (GtkWidget *) data;
	Display *dpy = GDK_DISPLAY();

	if (kev->type != KeyRelease && kev->type != ButtonPressMask)
		return GDK_FILTER_CONTINUE;

	initModifiers();

	if (kev->type == KeyRelease)
	{
		gchar *str;
		KeySym sym;
		unsigned int m = kev->state;

		sym = XKeycodeToKeysym(dpy, kev->keycode, 0);
		if (!sym)
			return GDK_FILTER_CONTINUE;

		str = g_strdup_printf("%s%s%s%s%s%s%s",
			m & ControlMask ? "Control+" : "",
			m & ShiftMask ? "Shift+" : "",
			m & AltMask ? "Alt+" : "",
			m & MetaMask ? "Meta+" : "",
			m & SuperMask ? "Super+" : "",
			m & HyperMask ? "Hyper+" : "",
			XKeysymToString(sym));
			
		g_object_set_data(G_OBJECT(popup), "chosen-key", str);
	}

	gdk_window_remove_filter(popup->window, filter_get_key, data);
	gtk_widget_destroy(popup);

	return GDK_FILTER_REMOVE;
}

static void may_set_shortcut(GtkWidget *popup, GtkWidget *label)
{
	gchar *str;

	str = g_object_get_data(G_OBJECT(popup), "chosen-key");
	if (str)
	{
		gtk_label_set_text(GTK_LABEL(label), str);
		g_free(str);
		g_object_set_data(G_OBJECT(popup), "chosen-key", NULL);
	}
}

static void get_shortcut(GtkWidget *button, GtkWidget *label)
{
	GtkWidget *popup, *frame, *msg;
	Window    xid;
	Display	  *dpy = GDK_DISPLAY();

	popup = gtk_window_new(GTK_WINDOW_POPUP);

	gtk_window_set_position(GTK_WINDOW(popup), GTK_WIN_POS_CENTER);

	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(popup), frame);

	msg = gtk_label_new(_("Press the desired shortcut (eg, Control+F1)"));

	gtk_misc_set_padding(GTK_MISC(msg), 20, 20);
	gtk_container_add(GTK_CONTAINER(frame), msg);

	gtk_window_set_modal(GTK_WINDOW(popup), TRUE);

	gtk_widget_add_events(popup,
			GDK_KEY_RELEASE_MASK | GDK_BUTTON_PRESS_MASK);

	g_signal_connect(popup, "destroy",
			G_CALLBACK(may_set_shortcut), label);

	gtk_widget_show_all(popup);

	gdk_window_add_filter(popup->window, filter_get_key, popup);

	xid = gdk_x11_drawable_get_xid(popup->window);

	if (XGrabKeyboard(dpy, xid, False, GrabModeAsync, GrabModeAsync,
			gtk_get_current_event_time()) != Success)
	{
		delayed_error(_("Failed to get keyboard grab!"));
		gtk_widget_destroy(popup);
	}

	if (XGrabPointer(dpy, xid, False, ButtonPressMask, GrabModeAsync,
				GrabModeAsync, xid, None,
				gtk_get_current_event_time()) != Success)
	{
		g_warning("Failed to get mouse grab");
	}
}

static void clear_shortcut(GtkButton *clear, GtkLabel *label)
{
	gtk_label_set_text(GTK_LABEL(label), CLICK_TO_SET);
}

/* Opens a box allowing the user to change the name of a pinned icon.
 * If the icon is destroyed then the box will close.
 * If the user chooses OK then the callback is called once the icon's
 * name, src_path and path fields have been updated and the item field
 * restatted.
 */
static void show_rename_box(Icon *icon)
{
	GtkDialog	*dialog;
	GtkWidget	*label, *entry, *button, *button2, *hbox, *spacer, *lock_state;
	GtkBox		*vbox;

	if (icon->dialog)
	{
		gtk_window_present(GTK_WINDOW(icon->dialog));
		return;
	}

	icon->dialog = gtk_dialog_new();
	gtk_dialog_set_has_separator(GTK_DIALOG(icon->dialog), FALSE);
	g_signal_connect(icon->dialog, "destroy",
			G_CALLBACK(gtk_widget_destroyed), &icon->dialog);

	dialog = GTK_DIALOG(icon->dialog);
	
	vbox = GTK_BOX(gtk_vbox_new(FALSE, 1));
	gtk_box_pack_start(GTK_BOX(dialog->vbox), (GtkWidget *) vbox,
			   TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);

	gtk_window_set_title(GTK_WINDOW(dialog), _("Edit Item"));
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);

	label = gtk_label_new(_("Clicking the icon opens:"));
	gtk_box_pack_start(vbox, label, TRUE, TRUE, 0);

	entry = gtk_entry_new();
	gtk_box_pack_start(vbox, entry, TRUE, FALSE, 2);
	gtk_entry_set_text(GTK_ENTRY(entry), icon->src_path);
	g_object_set_data(G_OBJECT(dialog), "new_path", entry);
	g_signal_connect_swapped(entry, "activate",
			G_CALLBACK(rename_activate), dialog);

	label = gtk_label_new(_("Arguments to pass (for executables):"));
	gtk_box_pack_start(vbox, label, TRUE, TRUE, 0);

	entry = gtk_entry_new();
	gtk_box_pack_start(vbox, entry, TRUE, FALSE, 2);
	gtk_entry_set_text(GTK_ENTRY(entry), icon->args ? icon->args : "");
	g_object_set_data(G_OBJECT(dialog), "new_arg", entry);
	g_signal_connect_swapped(entry, "activate",
			G_CALLBACK(rename_activate), dialog);

	spacer = gtk_drawing_area_new();
	gtk_widget_set_size_request(spacer, 4, 4);
	gtk_box_pack_start(vbox, spacer, FALSE, FALSE, 0);

	label = gtk_label_new(_("The text displayed under the icon is:"));
	gtk_box_pack_start(vbox, label, TRUE, TRUE, 0);
	entry = gtk_entry_new();
	gtk_box_pack_start(vbox, entry, TRUE, FALSE, 2);
	gtk_entry_set_text(GTK_ENTRY(entry), icon->item->leafname);
	gtk_widget_grab_focus(entry);
	g_object_set_data(G_OBJECT(dialog), "new_name", entry);
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	
	spacer = gtk_drawing_area_new();
	gtk_widget_set_size_request(spacer, 4, 4);
	gtk_box_pack_start(vbox, spacer, FALSE, FALSE, 0);

	label = gtk_label_new(_("The keyboard shortcut is:"));
	gtk_box_pack_start(vbox, label, TRUE, TRUE, 0);

	hbox = gtk_hbox_new(FALSE, 2);
	gtk_box_pack_start(vbox, hbox, TRUE, FALSE, 0);
	button = gtk_button_new_with_label(icon->shortcut
						? icon->shortcut
						: CLICK_TO_SET);
	gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(dialog), "new_shortcut",
				GTK_BIN(button)->child);
	g_signal_connect(button, "clicked",
			G_CALLBACK(get_shortcut),
			GTK_BIN(button)->child);
	button2 = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	gtk_box_pack_start(GTK_BOX(hbox), button2, FALSE, FALSE, 0);
	g_signal_connect(button2, "clicked",
			G_CALLBACK(clear_shortcut),
			GTK_BIN(button)->child);
			
	lock_state = gtk_check_button_new_with_label(_("Locked"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(lock_state), icon->locked);
	gtk_box_pack_start(vbox, lock_state, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(dialog), "new_lock_state", lock_state);
	gtk_tooltips_set_tip(tooltips, lock_state,
			_("Locking an item prevents it from being accidentally removed"),
			NULL);

	g_object_set_data(G_OBJECT(dialog), "callback_icon", icon);

	gtk_dialog_add_buttons(dialog,
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			GTK_STOCK_OK, GTK_RESPONSE_OK,
			NULL);
	gtk_dialog_set_default_response(dialog, GTK_RESPONSE_OK);

	g_signal_connect(dialog, "response", G_CALLBACK(edit_response), NULL);
	
	gtk_widget_show_all(GTK_WIDGET(dialog));
}

static gpointer parent_class = NULL;

static void icon_finialize(GObject *object)
{
	Icon *icon = (Icon *) object;

	g_return_if_fail(!icon->selected);

	if (icon->dialog)
		gtk_widget_destroy(icon->dialog);

	if (icon == menu_icon)
		menu_icon = NULL;

	icon_set_path(icon, NULL, NULL);
	icon_set_shortcut(icon, NULL);
	icon_set_arguments(icon, NULL);

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void icon_class_init(gpointer gclass, gpointer data)
{
	GObjectClass *object = (GObjectClass *) gclass;
	IconClass *icon = (IconClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	object->finalize = icon_finialize;
	icon->destroy = NULL;
	icon->redraw = NULL;
	icon->update = NULL;
	icon->same_group = NULL;
	icon->wink = NULL;

	g_signal_new("update",
			G_TYPE_FROM_CLASS(gclass),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET(IconClass, update),
			NULL, NULL,
			g_cclosure_marshal_VOID__VOID,
			G_TYPE_NONE, 0);

	g_signal_new("destroy",
			G_TYPE_FROM_CLASS(gclass),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET(IconClass, destroy),
			NULL, NULL,
			g_cclosure_marshal_VOID__VOID,
			G_TYPE_NONE, 0);

	g_signal_new("redraw",
			G_TYPE_FROM_CLASS(gclass),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET(IconClass, redraw),
			NULL, NULL,
			g_cclosure_marshal_VOID__VOID,
			G_TYPE_NONE, 0);

	icons_hash = g_hash_table_new(g_str_hash, g_str_equal);
}

static void icon_init(GTypeInstance *object, gpointer gclass)
{
	Icon *icon = (Icon *) object;

	icon->selected = FALSE;
	icon->src_path = NULL;
	icon->path = NULL;
	icon->item = NULL;
	icon->dialog = NULL;
	icon->shortcut = NULL;
	icon->shortcut_key.keycode = 0;
	icon->shortcut_key.modifier = 0;
	icon->args = NULL;
	icon->locked = FALSE;
}

/* As icon_set_selected(), but doesn't automatically unselect incompatible
 * icons.
 */
static void icon_set_selected_int(Icon *icon, gboolean selected)
{
	static GtkClipboard *primary;

	g_return_if_fail(icon != NULL);

	if (icon->selected == selected)
		return;

	if (!primary)
		primary = gtk_clipboard_get(gdk_atom_intern("PRIMARY", FALSE));

	if (selected)
	{
		icon_selection = g_list_prepend(icon_selection, icon);
		if (!have_primary)
		{
			GtkTargetEntry target_table[] =
			{
				{"text/uri-list", 0, TARGET_URI_LIST},
				{"UTF8", 0, TARGET_STRING},
				{"COMPOUND_TEXT", 0, TARGET_STRING},
				{"STRING", 0, TARGET_STRING},
			};

			/* Grab selection */
			have_primary = gtk_clipboard_set_with_data(primary,
				target_table,
				sizeof(target_table) / sizeof(*target_table),
				selection_get, lose_selection, NULL);
		}
	}
	else
	{
		icon_selection = g_list_remove(icon_selection, icon);
		if (have_primary && !icon_selection)
		{
			have_primary = FALSE;
			gtk_clipboard_clear(primary);
		}
	}

	icon->selected = selected;
	g_signal_emit_by_name(icon, "redraw");
}

/* From xfwm4 */
static void initModifiers(void)
{
	static gboolean need_init = TRUE;
	Display *dpy = GDK_DISPLAY();
	XModifierKeymap *xmk = XGetModifierMapping(dpy);
	int m, k;

	if (!need_init)
		return;
	need_init = FALSE;

	AltMask = MetaMask = NumLockMask = ScrollLockMask = CapsLockMask =
		SuperMask = HyperMask = 0;

	/* Work out which mask to use for each modifier group */
	if (xmk)
	{
		KeyCode *c = xmk->modifiermap;
		KeyCode numLockKeyCode;
		KeyCode scrollLockKeyCode;
		KeyCode capsLockKeyCode;
		KeyCode altKeyCode;
		KeyCode metaKeyCode;
		KeyCode superKeyCode;
		KeyCode hyperKeyCode;

		/* Find the codes to search for... */
		numLockKeyCode = XKeysymToKeycode(dpy, XK_Num_Lock);
		scrollLockKeyCode = XKeysymToKeycode(dpy, XK_Scroll_Lock);
		capsLockKeyCode = XKeysymToKeycode(dpy, XK_Caps_Lock);
		altKeyCode = XKeysymToKeycode(dpy, XK_Alt_L);
		metaKeyCode = XKeysymToKeycode(dpy, XK_Meta_L);
		superKeyCode = XKeysymToKeycode(dpy, XK_Super_L);
		hyperKeyCode = XKeysymToKeycode(dpy, XK_Hyper_L);

		/* If some are missing, try alternatives... */
		if (!altKeyCode)
			altKeyCode = XKeysymToKeycode(dpy, XK_Alt_R);
		if (!metaKeyCode)
			metaKeyCode = XKeysymToKeycode(dpy, XK_Meta_R);
		if (!superKeyCode)
			superKeyCode = XKeysymToKeycode(dpy, XK_Super_R);
		if (!hyperKeyCode)
			hyperKeyCode = XKeysymToKeycode(dpy, XK_Hyper_R);

		/* Check each of the eight modifier lists.
		 * The idea (I think) is that we name the modifier group which
		 * includes the Alt key as the 'Alt group', and so on for
		 * the other modifiers.
		 */
		for (m = 0; m < 8; m++)
		{
			for (k = 0; k < xmk->max_keypermod; k++, c++)
			{
				if (*c == NoSymbol)
					continue;
				if (*c == numLockKeyCode)
					NumLockMask = (1 << m);
				if (*c == scrollLockKeyCode)
					ScrollLockMask = (1 << m);
				if (*c == capsLockKeyCode)
					CapsLockMask = (1 << m);
				if (*c == altKeyCode)
					AltMask = (1 << m);
				if (*c == metaKeyCode)
					MetaMask = (1 << m);
				if (*c == superKeyCode)
					SuperMask = (1 << m);
				if (*c == hyperKeyCode)
					HyperMask = (1 << m);
			}
		}
		XFreeModifiermap(xmk);
	}

	if(MetaMask == AltMask)
		MetaMask = 0;
	
	if (AltMask != 0 && MetaMask == Mod1Mask)
	{
		MetaMask = AltMask;
		AltMask = Mod1Mask;
	}

	if (AltMask == 0 && MetaMask != 0)
	{
		if (MetaMask != Mod1Mask)
			AltMask = Mod1Mask;
		else
		{
			AltMask = MetaMask;
			MetaMask = 0;
		}
	}

	if (AltMask == 0)
		AltMask = Mod1Mask;
}


/* Fill in key from str. Sets keycode to zero if str is NULL.
 * Stolen from xfwm4 and modified. Call initModifiers before this.
 */
static void parseKeyString(MyKey *key, const char *str)
{
	char *k;
	Display *dpy = GDK_DISPLAY();

	key->keycode = 0;
	key->modifier = 0;

	if (!str)
		return;

	k = strrchr(str, '+');
	key->keycode = XKeysymToKeycode(dpy, XStringToKeysym(k ? k + 1 : str));
	if (k)
	{
		gchar *tmp;

		tmp = g_ascii_strdown(str, -1);

		if (strstr(tmp, "shift"))
			key->modifier = key->modifier | ShiftMask;
		if (strstr(tmp, "control"))
			key->modifier = key->modifier | ControlMask;
		if (strstr(tmp, "alt") || strstr(tmp, "mod1"))
			key->modifier = key->modifier | AltMask;
		if (strstr(tmp, "meta") || strstr(tmp, "mod2"))
			key->modifier = key->modifier | MetaMask;
		if (strstr(tmp, "hyper"))
			key->modifier = key->modifier | HyperMask;
		if (strstr(tmp, "super"))
			key->modifier = key->modifier | SuperMask;

		g_free(tmp);
	}

	if (!key->keycode)
		g_warning("Can't parse key '%s'\n", str);
}

static GdkFilterReturn filter_keys(GdkXEvent *xevent,
				   GdkEvent *event,
				   gpointer  data)
{
	GList *next;
	XKeyEvent *kev = (XKeyEvent *) xevent;
	guint state;

	if (kev->type != KeyPress)
		return GDK_FILTER_CONTINUE;

	state = kev->state & (ShiftMask | ControlMask | AltMask | MetaMask |
			      HyperMask | SuperMask);

	for (next = icon_shortcuts; next; next = next->next)
	{
		Icon  *icon = (Icon *) next->data;

		if (icon->shortcut_key.keycode == kev->keycode &&
		    icon->shortcut_key.modifier == state)
		{
			icon_wink(icon);
			icon_run(icon);
		}
	}
	
	return GDK_FILTER_CONTINUE;
}

#define GRAB(key, mods) XGrabKey(dpy, key->keycode, key->modifier | mods, \
				 root, False, GrabModeAsync, GrabModeAsync)
#define UNGRAB(key, mods) XUngrabKey(dpy, key->keycode, key->modifier | mods, \
				 root)

static guint mykey_hash(gconstpointer key)
{
	MyKey *k = (MyKey *) key;
	
	return (k->keycode << 8) + k->modifier;
}

static gboolean mykey_cmp(gconstpointer a, gconstpointer b)
{
	MyKey *ka = (MyKey *) a;
	MyKey *kb = (MyKey *) b;

	return ka->keycode == kb->keycode && kb->modifier == kb->modifier;
}

/* Stolen from xfwm4 and modified.
 * FALSE on error. Call initModifiers before this.
 */
static gboolean grabKey(MyKey *key)
{
	Window root;
	Display *dpy = GDK_DISPLAY();
	static gboolean need_init = TRUE;

	if (need_init)
	{
		need_init = FALSE;
		gdk_window_add_filter(gdk_get_default_root_window(),
				filter_keys, NULL);
	}

	gdk_error_trap_push();

	root = GDK_ROOT_WINDOW();

	GRAB(key, 0);

	/* Here we grab all combinations of well known modifiers */
	GRAB(key, ScrollLockMask);
	GRAB(key, NumLockMask);
	GRAB(key, CapsLockMask);
	GRAB(key, ScrollLockMask | NumLockMask);
	GRAB(key, ScrollLockMask | CapsLockMask);
	GRAB(key, CapsLockMask | NumLockMask);
	GRAB(key, ScrollLockMask | CapsLockMask | NumLockMask);

	gdk_flush();
	return gdk_error_trap_pop() == Success;
}

static gboolean ungrabKey(MyKey *key)
{
	Window root = GDK_ROOT_WINDOW();
	Display *dpy = GDK_DISPLAY();

	gdk_error_trap_push();

	UNGRAB(key, 0);

	UNGRAB(key, ScrollLockMask);
	UNGRAB(key, NumLockMask);
	UNGRAB(key, CapsLockMask);
	UNGRAB(key, ScrollLockMask | NumLockMask);
	UNGRAB(key, ScrollLockMask | CapsLockMask);
	UNGRAB(key, CapsLockMask | NumLockMask);
	UNGRAB(key, ScrollLockMask | CapsLockMask | NumLockMask);

	gdk_flush();
	return gdk_error_trap_pop() == Success;
}

static void ungrab_key(Icon *icon)
{
	int *count;
	
	g_return_if_fail(icon != NULL);

	if (!icon->shortcut_key.keycode)
		return;

	icon_shortcuts = g_list_remove(icon_shortcuts, icon);

	count = g_hash_table_lookup(grab_counter, &icon->shortcut_key);
	g_return_if_fail(count != NULL);

	(*count)--;

	if (*count > 0)
		return;
	
	g_hash_table_remove(grab_counter, &icon->shortcut_key);

	if (!ungrabKey(&icon->shortcut_key))
		g_warning("Failed to ungrab shortcut '%s' for '%s' icon.",
			   icon->shortcut, icon->item->leafname);
}

static void grab_key(Icon *icon)
{
	MyKey *hash_key;
	int   *count;
		
	g_return_if_fail(icon != NULL);

	g_return_if_fail(g_list_find(icon_shortcuts, icon) == NULL);

	if (!icon->shortcut_key.keycode)
		return;

	icon_shortcuts = g_list_prepend(icon_shortcuts, icon);

	if (!grab_counter)
		grab_counter = g_hash_table_new_full(mykey_hash, mykey_cmp,
						     g_free, NULL);

	count = g_hash_table_lookup(grab_counter, &icon->shortcut_key);
	if (count)
	{
		(*count)++;
		return;	/* Already grabbed */
	}

	hash_key = g_new(MyKey, 1);
	*hash_key = icon->shortcut_key;
	count = g_new(int, 1);
	*count = 1;
	g_hash_table_insert(grab_counter, hash_key, count);

	if (!grabKey(&icon->shortcut_key))
		g_warning("Failed to grab shortcut '%s' for '%s' icon.\n"
			  "Some other application may be already using it!\n",
			  icon->shortcut, icon->item->leafname);
	
}

static void icon_wink(Icon *icon)
{
	IconClass	*iclass;

	iclass = (IconClass *) G_OBJECT_GET_CLASS(icon);
	g_return_if_fail(iclass->wink != NULL);

	iclass->wink(icon);
}

/* Sets icon_menu */
static void create_menu(void)
{
	GList		*items;
	guchar		*tmp;
	GtkItemFactory	*item_factory;

	g_return_if_fail(icon_menu == NULL);

	item_factory = menu_create(menu_def,
				sizeof(menu_def) / sizeof(*menu_def),
				 "<icon>", NULL);

	tmp = g_strconcat("<icon>/", _("File"), NULL);
	icon_menu = gtk_item_factory_get_widget(item_factory, "<icon>");
	icon_file_menu = gtk_item_factory_get_widget(item_factory, tmp);
	g_free(tmp);

	/* File '' label... */
	items = gtk_container_get_children(GTK_CONTAINER(icon_menu));
	icon_file_item = GTK_BIN(g_list_nth(items, 1)->data)->child;
	g_list_free(items);

	/* Shift Open... label */
	items = gtk_container_get_children(GTK_CONTAINER(icon_file_menu));
	file_shift_item = GTK_BIN(items->data)->child;
	g_list_free(items);

	g_signal_connect(icon_menu, "unmap_event",
			G_CALLBACK(menu_closed), NULL);
}

