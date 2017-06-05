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

/* infobox.c - code for showing a file's attributes */

#include "config.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <signal.h>
#include <libxml/parser.h>

#include <gtk/gtk.h>

#include "global.h"

#include "support.h"
#include "main.h"
#include "gui_support.h"
#include "diritem.h"
#include "type.h"
#include "infobox.h"
#include "appinfo.h"
#include "dnd.h"	/* For xa_string */
#include "run.h"	/* For show_help_files() */
#include "xml.h"
#include "mount.h"
#include "pixmaps.h"
#include "xtypes.h"
#include "filer.h"

typedef struct _FileStatus FileStatus;

/* This is for the 'file(1) says...' thing */
struct _FileStatus
{
	int	fd;	/* FD to read from, -1 if closed */
	int	input;	/* Input watcher tag if fd valid */
	GtkLabel *label;	/* Widget to output to */
	gchar	*text;	/* String so far */
};

typedef struct du {
	gchar        *path;
	GtkListStore *store;
	guint         watch;
	GIOChannel   *chan;
	gint          child;
} DU;

typedef struct _Permissions Permissions;

struct _Permissions
{
	gchar *path;
	DirItem *item;
	GtkWidget *bits[12];
};

/* Static prototypes */
static void refresh_info(GObject *window);
static GtkWidget *make_vbox(const guchar *path, GObject *window);
static GtkWidget *make_details(const guchar *path, DirItem *item,
				GObject *window);
static GtkWidget *make_about(const guchar *path, XMLwrapper *ai);
static GtkWidget *make_about_desktop(const gchar *path);
static GtkWidget *make_file_says(const guchar *path);
static GtkWidget *make_permissions(const gchar *path, DirItem *item);
static GtkWidget *make_unmount_options(const gchar *path);
static void add_file_output(FileStatus *fs,
			    gint source, GdkInputCondition condition);
static const gchar *pretty_type(DirItem *file, const guchar *path);
static void got_response(GObject *window, gint response, gpointer data);
static void file_info_destroyed(GtkWidget *widget, FileStatus *fs);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Open each item in a new infobox. Confirms if there are a large
 * number of items to show.
 */
void infobox_show_list(GList *paths)
{
	int n;

	n = g_list_length(paths);

	if (n >= 10)
	{
		gchar *message;
		gboolean ok;

		message = g_strdup_printf(
			_("Are you sure you want to open %d windows?"), n);
		ok = confirm(message, GTK_STOCK_YES, _("Show Info"));
		g_free(message);
		if (!ok)
			return;
	}

	g_list_foreach(paths, (GFunc) infobox_new, NULL);
}

/* Create and display a new info box showing details about this item */
void infobox_new(const gchar *pathname)
{
	GtkWidget	*window, *details;
	gchar		*path;
	GObject		*owindow;

	g_return_if_fail(pathname != NULL);

	path = g_strdup(pathname); /* Gets attached to window & freed later */

	window = gtk_dialog_new_with_buttons(
			g_utf8_validate(path, -1, NULL) ? path
							: _("(bad utf-8)"),
				NULL, GTK_DIALOG_NO_SEPARATOR,
				GTK_STOCK_CLOSE, GTK_RESPONSE_CANCEL,
				GTK_STOCK_REFRESH, GTK_RESPONSE_APPLY,
				NULL);

	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_MOUSE);

	owindow = G_OBJECT(window);
	details = make_vbox(path, owindow);
	gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(window)->vbox),
				    details);

	g_object_set_data(owindow, "details", details);
	g_object_set_data_full(owindow, "path", path, g_free);

	g_signal_connect(window, "response", G_CALLBACK(got_response), NULL);

	number_of_windows++;
	gtk_widget_show_all(window);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void got_response(GObject *window, gint response, gpointer data)
{
	if (response == GTK_RESPONSE_APPLY)
		refresh_info(window);
	else
	{
		gtk_widget_destroy(GTK_WIDGET(window));
		one_less_window();
	}
}

static void refresh_info(GObject *window)
{
	GtkWidget	*details, *vbox;
	guchar		*path;

	path = g_object_get_data(window, "path");
	details = g_object_get_data(window, "details");
	g_return_if_fail(details != NULL);
	g_return_if_fail(path != NULL);

	vbox = details->parent;
	gtk_widget_destroy(details);

	details = make_vbox(path, window);
	g_object_set_data(window, "details", details);
	gtk_box_pack_start_defaults(GTK_BOX(vbox), details);
	gtk_widget_show_all(details);
}

static void add_frame(GtkBox *vbox, GtkWidget *list)
{
	GtkWidget	*frame;

	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(frame), list);
	gtk_box_pack_start_defaults(vbox, frame);
}

/* Create the VBox widget that contains the details.
 * Note that 'path' must not be freed until the vbox is destroyed.
 */
static GtkWidget *make_vbox(const guchar *path, GObject *window)
{
	DirItem		*item;
	GtkBox		*vbox;
	XMLwrapper	*ai;
	xmlNode 	*about = NULL;
	gchar		*help_dir;
	GtkWidget	*hbox, *name, *label;
	MaskedPixmap    *thumb;

	g_return_val_if_fail(path[0] == '/', NULL);
	
	item = diritem_new(g_basename(path));
	diritem_restat(path, item, NULL);

	ai = appinfo_get(path, item);
	if (ai)
		about = xml_get_section(ai, NULL, "About");

	vbox = GTK_BOX(gtk_vbox_new(FALSE, 4));
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 4);

	/* Heading, with icon and name */
	hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(vbox, hbox, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox),
			   gtk_image_new_from_pixbuf(di_image(item)->pixbuf),
			   FALSE, FALSE, 4);

	if (g_utf8_validate(item->leafname, -1, NULL))
		name = gtk_label_new(item->leafname);
	else
	{
		guchar *u8;

		u8 = to_utf8(item->leafname);
		name = gtk_label_new(u8);
		g_free(u8);
	}
	gtk_label_set_selectable(GTK_LABEL(name), TRUE);
	gtk_label_set_line_wrap(GTK_LABEL(name), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), name, FALSE, TRUE, 4);
	
	make_heading(name, PANGO_SCALE_X_LARGE);

	thumb=pixmap_try_thumb(path, FALSE);
	if(thumb)
	{
		gtk_box_pack_start(GTK_BOX(hbox),
				   gtk_image_new_from_pixbuf(thumb->src_pixbuf),
				   FALSE, FALSE, 4);
		g_object_unref(thumb);
	}

	/* List of file attributes */
	add_frame(vbox, make_details(path, item, window));

	help_dir = g_strconcat(path, "/Help", NULL);

	if (access(help_dir, F_OK) == 0)
	{
		GtkWidget *button, *align;

		align = gtk_alignment_new(0.5, 0.5, 0, 0);
		
		button = button_new_mixed(GTK_STOCK_JUMP_TO,
				_("Show _Help Files"));
		gtk_box_pack_start(vbox, align, FALSE, TRUE, 0);
		gtk_container_add(GTK_CONTAINER(align), button);
		g_signal_connect_swapped(button, "clicked", 
				G_CALLBACK(show_help_files),
				(gpointer) path);
	}
	g_free(help_dir);

	if (!(item->flags & ITEM_FLAG_SYMLINK))
	{
		label = gtk_label_new(NULL);
		gtk_label_set_markup(GTK_LABEL(label),
				     _("<b>Permissions</b>"));
		gtk_misc_set_alignment(GTK_MISC(label), 0, 1);
		gtk_box_pack_start(vbox, label, FALSE, TRUE, 2);

		gtk_box_pack_start(vbox, make_permissions(path, item),
				   FALSE, TRUE, 0);
	}

	if (about)
		add_frame(vbox, make_about(path, ai));
	else if (item->mime_type == application_x_desktop)
	{
		add_frame(vbox, make_about_desktop(path));
	}
	else if (item->base_type == TYPE_FILE)
	{
		label = gtk_label_new(NULL);
		gtk_label_set_markup(GTK_LABEL(label),
				_("<b>Contents indicate...</b>"));
		gtk_misc_set_alignment(GTK_MISC(label), 0, 1);
		gtk_box_pack_start(vbox, label, FALSE, TRUE, 2);

		gtk_box_pack_start_defaults(vbox, make_file_says(path));
	}
	else if (item->flags & ITEM_FLAG_MOUNT_POINT)
	{
		label = gtk_label_new(NULL);
		gtk_label_set_markup(GTK_LABEL(label),
				     _("<b>When all directories are closed</b>"));
		gtk_misc_set_alignment(GTK_MISC(label), 0, 1);
		gtk_box_pack_start(vbox, label, FALSE, TRUE, 2);
	    gtk_box_pack_start(vbox, make_unmount_options(path), FALSE, TRUE, 0);
	}

	if (ai)
		g_object_unref(ai);

	diritem_free(item);

	return (GtkWidget *) vbox;
}

/* The selection has changed - grab or release the primary selection */
static void set_selection(GtkTreeView *view, gpointer data)
{
	static GtkClipboard *primary = NULL;
	GtkTreeModel *model;
	GtkTreePath *path = NULL;
	GtkTreeIter iter;
	gchar	*text;

	gtk_tree_view_get_cursor(view, &path, NULL);
	if (!path)
		return;

	if (!primary)
		primary = gtk_clipboard_get(gdk_atom_intern("PRIMARY", FALSE));
	
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));

	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_path_free(path);

	gtk_tree_model_get(model, &iter, 1, &text, -1);

	gtk_clipboard_set_text(primary, text, -1);

	g_free(text);
}

/* Returns a GtkTreePath for the item */
static const gchar *add_row(GtkListStore *store, const gchar *label,
		      const gchar *data)
{
	GtkTreeIter	iter;
	gchar		*u8 = NULL;
	GtkTreePath     *tpath;
	static gchar    *last = NULL;

	if (!g_utf8_validate(data, -1, NULL))
		u8 = to_utf8(data);

	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter, 0, label, 1, u8 ? u8 : data, -1);

	g_free(u8);

	tpath = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
	if (last)
		g_free(last);
	last = gtk_tree_path_to_string(tpath);
	gtk_tree_path_free(tpath);

	return last;
}

static void add_row_and_free(GtkListStore *store,
			     const gchar *label, gchar *data)
{
	add_row(store, label, data);
	g_free(data);
}

/* Create an empty list view, ready to place some data in */
static void make_list(GtkListStore **list_store, GtkWidget **list_view,
			GCallback cell_edited)
{
	GtkListStore	*store;
	GtkTreeView	*view;
	GtkCellRenderer *cell_renderer;

	/* Field name, value, editable */
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING,
					G_TYPE_BOOLEAN);
	view = GTK_TREE_VIEW(
			gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
	g_object_unref(G_OBJECT(store));
	gtk_tree_view_set_headers_visible(view, FALSE);

	cell_renderer = gtk_cell_renderer_text_new();
	g_object_set(G_OBJECT(cell_renderer), "xalign", 1.0, NULL);
	gtk_tree_view_insert_column_with_attributes(view,
			0, NULL, cell_renderer, "text", 0, NULL);

	cell_renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_insert_column_with_attributes(view,
			1, NULL, cell_renderer, "text", 1, "editable", 2, NULL);

	if (cell_edited) {
		g_signal_connect(G_OBJECT(cell_renderer), "edited",
				G_CALLBACK(cell_edited), store);
	}

	g_signal_connect(view, "cursor_changed",
			G_CALLBACK(set_selection), NULL);

	*list_store = store;
	*list_view = (GtkWidget *) view;
}

static void set_cell(GtkListStore *store, const gchar *path,
		     const gchar *ctext)
{
	GtkTreeIter iter;

	gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(store),
					    &iter, path);
	gtk_list_store_set(store, &iter, 1, ctext, -1);
}

static void insert_size(DU *du, const char *line)
{
	off_t size;
	gchar *cell;

#ifdef	LARGE_FILE_SUPPORT
	size = strtoll(line, NULL, 10);
#else
	size = strtol(line, NULL, 10);
#endif
	size <<= 10; /* Because du reports in K */
	cell = (size >= PRETTY_SIZE_LIMIT)
		? g_strdup_printf("%s (%" SIZE_FMT " %s)",
				format_size(size),
				size, _("bytes"))
		: g_strdup(format_size(size));

	set_cell(du->store, du->path, cell);

	g_free(cell);
}

static gboolean read_du_output(GIOChannel *source, GIOCondition cond, DU *du)
{
	GString *line;
	GIOStatus stat;
	GError *err = NULL;

	line = g_string_new("");
	stat = g_io_channel_read_line_string(source, line, NULL, &err);
	switch (stat)
	{
		case G_IO_STATUS_NORMAL:
			insert_size(du, line->str);
			break;
		case G_IO_STATUS_EOF:
		        set_cell(du->store, du->path,
				 _("Failed to read size"));
			break;
		case G_IO_STATUS_AGAIN:
			g_string_free(line, TRUE);
			return TRUE;
		case G_IO_STATUS_ERROR:
			set_cell(du->store, du->path, err->message);
			break;
	}
	g_string_free(line, TRUE);
	
	return FALSE;
}

static void kill_du_output(GtkWidget *widget, DU *du)
{
        g_source_remove(du->watch);
	g_io_channel_shutdown(du->chan, FALSE, NULL);
	g_io_channel_unref(du->chan);
	kill((pid_t) du->child, SIGTERM);
	g_object_unref(G_OBJECT(du->store));
	g_free(du->path);
	g_free(du);
}

static gboolean refresh_info_idle(gpointer data)
{
	GObject *window = G_OBJECT(data);

	refresh_info(window);
	g_object_unref(window);
	return FALSE;
}

static void cell_edited(GtkCellRendererText *cell,
	     const gchar *path_string,
	     const gchar *new_text,
	     gpointer data)
{
	GtkTreeModel *model = (GtkTreeModel *) data;
	GtkTreePath *path;
	GtkTreeIter iter;
	GObject *window;
	const char *fullpath;
	char *oldlink;

	window = g_object_get_data(G_OBJECT(model), "rox_window");
	g_return_if_fail(window != NULL);

	fullpath = g_object_get_data(window, "path");
	g_return_if_fail(fullpath != NULL);

	path = gtk_tree_path_new_from_string(path_string);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_path_free(path);

	oldlink = readlink_dup(fullpath);
	if (!oldlink) {
		/* Must use delayed_error(), as this can be called
		 * from a focus-out event (causes a crash).
		 */
		delayed_error(_("'%s' is no longer a symlink"), fullpath);
		return;
	}
	if (strcmp(oldlink, new_text) == 0)
		return;	/* No change */
	g_free(oldlink);
	if (unlink(fullpath)) {
		delayed_error(_("Failed to unlink '%s':\n%s"),
				fullpath, g_strerror(errno));
		return;
	}
	if (symlink(new_text, fullpath)) {
		delayed_error(_("Failed to create symlink from '%s':\n%s\n"
				"(note: old link has been deleted)"),
				fullpath, g_strerror(errno));
		return;
	}

	g_object_ref(window);
	g_idle_add(refresh_info_idle, window);
}

/* Create the TreeView widget with the file's details */
static GtkWidget *make_details(const guchar *path, DirItem *item,
				GObject *window)
{
	GtkListStore	*store;
	GtkWidget	*view;
	gchar		*tmp, *tmp2;

	make_list(&store, &view, G_CALLBACK(cell_edited));
	g_object_set_data(G_OBJECT(store), "rox_window", window);

	/* For a symlink to an error, don't show the error */
	if (item->base_type == TYPE_ERROR && item->lstat_errno)
	{
		add_row(store, _("Error:"), g_strerror(item->lstat_errno));
		return view;
	}

	tmp = g_path_get_dirname(path);
	tmp2 = pathdup(tmp);
	if (strcmp(tmp, tmp2) != 0)
		add_row_and_free(store, _("Real directory:"), tmp2);
	g_free(tmp);

	add_row_and_free(store, _("Owner, Group:"),
			 g_strdup_printf("%s, %s",
					 user_name(item->uid),
					 group_name(item->gid)));

	if (item->base_type != TYPE_DIRECTORY)
	{
		add_row_and_free(store, _("Size:"),
				 item->size >= PRETTY_SIZE_LIMIT
				 ? g_strdup_printf("%s (%" SIZE_FMT " %s)",
						   format_size(item->size),
						   item->size, _("bytes"))
				 : g_strdup(format_size(item->size)));
	}
	else
	{
		gchar *stt=NULL;

		if(item->flags & ITEM_FLAG_MOUNTED)
			stt=mount_get_fs_size(path);

		if(stt) {
			add_row_and_free(store, _("Size:"), stt);
		} else {
			DU *du;
			int out;
			
			gchar *args[] = {"du", "-sk", "", NULL};

			du = g_new(DU, 1);
			du->store = store;
			du->path = g_strdup(add_row(store, _("Size:"),
						    _("Scanning")));

			args[2] = (gchar *) path;
			if (g_spawn_async_with_pipes(NULL, args, NULL,
						     G_SPAWN_SEARCH_PATH,
						     NULL, NULL, &du->child,
						     NULL, &out, NULL,
						     NULL))
			{
				du->chan = g_io_channel_unix_new(out);
				/* Select binary encoding so we don't get an
				 * error with non-UTF-8 filenames.
				 */
				g_io_channel_set_encoding(du->chan, NULL, NULL);
				du->watch = g_io_add_watch(du->chan,
						   G_IO_IN|G_IO_ERR|G_IO_HUP,
						 (GIOFunc) read_du_output, du);
				g_object_ref(G_OBJECT(du->store));
				g_signal_connect(G_OBJECT(view),
						 "destroy",
						 G_CALLBACK(kill_du_output),
						 du);
			}
			else
			{
				set_cell(store, du->path, _("Failed to scan"));
				g_free(du->path);
				g_free(du);
			}
		}
	}

	add_row_and_free(store, _("Change time:"), pretty_time(&item->ctime));
	
	add_row_and_free(store, _("Modify time:"), pretty_time(&item->mtime));

	add_row_and_free(store, _("Access time:"), pretty_time(&item->atime));

	add_row(store, _("Type:"), pretty_type(item, path));

	if (item->mime_type)
		add_row(store, "", mime_type_comment(item->mime_type));

	if (xattr_supported(NULL)) {
		add_row(store, _("Extended attributes:"),
		  (item->flags & ITEM_FLAG_HAS_XATTR)
		  	? _("Present")
			: xattr_supported(path) ? _("None")
						: _("Not supported"));
	}

	if (item->flags & ITEM_FLAG_SYMLINK)
	{
		GtkTreeIter iter;
		GtkTreeModel *model = GTK_TREE_MODEL(store);
		char *target;

		target = readlink_dup(path);
		if (!target)
			target = g_strdup(g_strerror(errno));
		add_row_and_free(store, _("Link target:"), target);

		/* Make cell editable */
		gtk_tree_model_iter_nth_child(model, &iter,
			NULL, gtk_tree_model_iter_n_children(model, NULL) - 1);

		gtk_list_store_set(store, &iter, 2, TRUE, -1);
	}

	if (item->base_type != TYPE_DIRECTORY)
	{
		if (EXECUTABLE_FILE(item))
			add_row(store, _("Run action:"), _("Execute file"));
		else
		{
			add_row_and_free(store, _("Run action:"),
				describe_current_command(item->mime_type));
		}
	}

	return view;
}
	
/* Create the TreeView widget with the application's details */
static GtkWidget *make_about(const guchar *path, XMLwrapper *ai)
{
	GtkListStore	*store;
	GtkWidget	*view;
	xmlNode 	*prop;
	xmlNode		*about, *about_trans;
	GHashTable	*translate;

	g_return_val_if_fail(ai != NULL, NULL);

	about_trans = xml_get_section(ai, NULL, "About");

	about = xmlDocGetRootElement(ai->doc)->xmlChildrenNode;
	for (; about; about = about->next)
	{
		if (about->type != XML_ELEMENT_NODE)
			continue;
		if (about->ns == NULL && strcmp(about->name, "About") == 0)
			break;
	}

	g_return_val_if_fail(about != NULL, NULL);
	
	make_list(&store, &view, NULL);

	/* Add each field in about to the list, but overriding each element
	 * with about_trans if a translation is supplied.
	 */
	translate = g_hash_table_new(g_str_hash, g_str_equal);
	if (about_trans != about)
	{
		xmlNode *p;
		for (p = about_trans->xmlChildrenNode; p; p = p->next)
		{
			if (p->type != XML_ELEMENT_NODE)
				continue;
			g_hash_table_insert(translate, (char *) p->name, p);
		}
	}
	for (prop = about->xmlChildrenNode; prop; prop = prop->next)
	{
		if (prop->type == XML_ELEMENT_NODE)
		{
			char *label = NULL;
			char *value = NULL;
			char *tmp = NULL;
			xmlNode *trans;

			trans = g_hash_table_lookup(translate, prop->name);
			if (!trans)
				trans = prop;

			tmp = xmlGetProp(trans, "label");
			label = g_strconcat(tmp ? tmp
						: (char *) trans->name,
					    ":", NULL);
			g_free(tmp);
			value = xmlNodeListGetString(trans->doc,
					   trans->xmlChildrenNode, 1);
			if (!value)
				value = xmlNodeListGetString(prop->doc,
						prop->xmlChildrenNode, 1);
			if (!value)
				value = g_strdup("-");
			add_row_and_free(store, label, value);
			g_free(label);
		}
	}

	g_hash_table_destroy(translate);

	return view;
}

/* Create the TreeView widget with the desktop entry's details */
static GtkWidget *make_about_desktop(const gchar *path)
{
	GtkListStore	*store;
	GtkWidget	*view;
	GError          *error=NULL;
	gchar           *name=NULL, *comment=NULL, *exec=NULL;

	make_list(&store, &view, NULL);

	if(!get_values_from_desktop_file(path, &error,
					 "Desktop Entry", "Name", &name,
					 "Desktop Entry", "Comment", &comment,
					 "Desktop Entry", "Exec", &exec,
					 NULL))
	{
		/* Report it? */
		delayed_error("%s", error->message);
		if(error)
			g_error_free(error);
		return view;
	}

	if(name)
		add_row_and_free(store, _("Name"), name);
	if(comment)
		add_row_and_free(store, _("Comment"), comment);
	if(exec)
		add_row_and_free(store, _("Execute"), exec);
	
	return view;
}

static GtkWidget *make_file_says(const guchar *path)
{
	GtkWidget	*w_file_label;
	GtkLabel	*l_file_label;
	int		file_data[2];
	char 		*argv[] = {"file", "-b", NULL, NULL};
	FileStatus 	*fs = NULL;
	guchar 		*tmp;

	w_file_label = gtk_label_new(_("<nothing yet>"));
	l_file_label = GTK_LABEL(w_file_label);
	gtk_label_set_line_wrap(l_file_label, TRUE);
	gtk_label_set_selectable(l_file_label, TRUE);
	
	if (pipe(file_data))
	{
		tmp = g_strdup_printf("pipe(): %s", g_strerror(errno));
		gtk_label_set_text(l_file_label, tmp);
		g_free(tmp);
		return w_file_label;
	}

	switch (fork())
	{
		case -1:
			tmp = g_strdup_printf("pipe(): %s", g_strerror(errno));
			gtk_label_set_text(l_file_label, tmp);
			g_free(tmp);
			close(file_data[0]);
			close(file_data[1]);
			break;
		case 0:
			/* We are the child */
			close(file_data[0]);
			dup2(file_data[1], STDOUT_FILENO);
			dup2(file_data[1], STDERR_FILENO);
#ifdef FILE_B_FLAG
			argv[2] = (char *) path;
#else
			argv[1] = (char *) g_basename(path);
			chdir(g_path_get_dirname(path));
#endif
			if (execvp(argv[0], argv))
				fprintf(stderr, "execvp() error: %s\n",
						g_strerror(errno));
			_exit(0);
		default:
			/* We are the parent */
			close(file_data[1]);
			fs = g_new(FileStatus, 1);
			fs->label = l_file_label;
			fs->fd = file_data[0];
			fs->text = g_strdup("");
			fs->input = gdk_input_add_full(fs->fd, GDK_INPUT_READ,
				(GdkInputFunction) add_file_output,
				fs, NULL);
			g_signal_connect(w_file_label, "destroy",
				G_CALLBACK(file_info_destroyed), fs);
			break;
	}

	return w_file_label;
}

/* Got some data from file(1) - stick it in the window. */
static void add_file_output(FileStatus *fs,
			    gint source, GdkInputCondition condition)
{
	char	buffer[20];
	char	*str;
	int	got;

	got = read(source, buffer, sizeof(buffer) - 1);
	if (got <= 0)
	{
		int	err = errno;
		g_source_remove(fs->input);
		close(source);
		fs->fd = -1;
		if (got < 0)
			delayed_error(_("file(1) says... %s"),
					g_strerror(err));
		return;
	}
	buffer[got] = '\0';

	str = g_strconcat(fs->text, buffer, NULL);
	g_free(fs->text);
	fs->text = str;
	
	str = to_utf8(fs->text);
	g_strstrip(str);
	gtk_label_set_text(fs->label, str);
	g_free(str);
}

static void file_info_destroyed(GtkWidget *widget, FileStatus *fs)
{
	if (fs->fd != -1)
	{
		g_source_remove(fs->input);
		close(fs->fd);
	}

	g_free(fs->text);
	g_free(fs);
}

static void permissions_destroyed(GtkWidget *widget, Permissions *perm)
{
	g_free(perm->path);
	diritem_free(perm->item);
	
	g_free(perm);
}

static void permissions_apply(GtkWidget *widget, Permissions *perm)
{
	mode_t nmode;
	int i;

	nmode=0;
	
	for (i = 0; i < 9; i++)
	{
		GtkToggleButton *bit = GTK_TOGGLE_BUTTON(perm->bits[i]);
		if (gtk_toggle_button_get_active(bit))
			nmode |= 1 << i;
	}
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(perm->bits[9])))
		nmode |= S_ISUID;
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(perm->bits[10])))
		nmode |= S_ISGID;
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(perm->bits[11])))
		nmode |= S_ISVTX;
	
	if (chmod(perm->path, nmode))
		report_error(_("Could not change permissions: %s"),
			     g_strerror(errno));
}

static GtkWidget *make_permissions(const gchar *path, DirItem *item)
{
	Permissions *perm;
	GtkWidget *table;
	GtkWidget *tick, *label;
	int i, x, y;

	perm = g_new(Permissions, 1);

	perm->path = g_strdup(path);
	perm->item = diritem_new(path);

	table = gtk_table_new(4, 5, TRUE);

	label = gtk_label_new(_("Owner"));
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 1, 2);
	label = gtk_label_new(_("Group"));
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 2, 3);
	label = gtk_label_new(_("World"));
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 3, 4);

	label = gtk_label_new(_("Read"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), label, 1, 2, 0, 1);
	label = gtk_label_new(_("Write"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), label, 2, 3, 0, 1);
	label = gtk_label_new(_("Exec"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach_defaults(GTK_TABLE(table), label, 3, 4, 0, 1);

	for (i = 0; i < 9; i++)
	{
		x = 1 + 2 - i % 3;
		y = 1 + 2 - i / 3;
		perm->bits[i] = tick = gtk_check_button_new();
		gtk_table_attach_defaults(GTK_TABLE(table), tick,
					  x, x + 1, y, y + 1);
		if (item->mode & (1 << i))
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tick),
						     TRUE);
		g_signal_connect(tick, "toggled",
				G_CALLBACK(permissions_apply), perm);
	}

	tick = gtk_check_button_new_with_label(_("SUID"));
	gtk_table_attach_defaults(GTK_TABLE(table), tick, 4, 5, 1, 2);
	if (item->mode & S_ISUID)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tick), TRUE);
	g_signal_connect(tick, "toggled", G_CALLBACK(permissions_apply), perm);
	perm->bits[9] = tick;

	tick = gtk_check_button_new_with_label(_("SGID"));
	gtk_table_attach_defaults(GTK_TABLE(table), tick, 4, 5, 2, 3);
	if (item->mode & S_ISGID)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tick), TRUE);
	g_signal_connect(tick, "toggled", G_CALLBACK(permissions_apply), perm);
	perm->bits[10] = tick;

	tick = gtk_check_button_new_with_label(_("Sticky"));
	gtk_table_attach_defaults(GTK_TABLE(table), tick, 4, 5, 3, 4);
	if (item->mode & S_ISVTX)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tick), TRUE);
	g_signal_connect(tick, "toggled", G_CALLBACK(permissions_apply), perm);
	perm->bits[11] = tick;

	g_signal_connect(table, "destroy",
				G_CALLBACK(permissions_destroyed), perm);
			
	gtk_widget_show_all(table);
	return table;
}

static void unmount_option_toggled(GtkToggleButton *toggle, const char *path)
{
    if (gtk_toggle_button_get_active(toggle))
    {
        filer_set_unmount_action(path,
                GPOINTER_TO_INT(g_object_get_data(G_OBJECT(toggle),
                        "unmount_action")));
    }
}

static GtkWidget *pack_unmount_radio(const char *path,
        UnmountPrompt path_value, const char *label,
        UnmountPrompt btn_value, GtkWidget *group_owner, GtkWidget *hbox)
{
    GtkWidget *radio;
    
    if (group_owner)
    {
        radio = gtk_radio_button_new_with_label_from_widget(
                GTK_RADIO_BUTTON(group_owner), label);
    }
    else
    {
        radio = gtk_radio_button_new_with_label(NULL, label);
    }
    if (path_value == btn_value)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);
    g_object_set_data(G_OBJECT(radio), "unmount_action",
            GINT_TO_POINTER(btn_value));
    g_signal_connect(radio, "toggled", G_CALLBACK(unmount_option_toggled),
            (gpointer) path);
    gtk_box_pack_start(GTK_BOX(hbox), radio, FALSE, FALSE, 0);
    return radio;
}

static GtkWidget *make_unmount_options(const char *path)
{
    GtkWidget *hbox, *radio;
    UnmountPrompt upval = filer_get_unmount_action(path);
    
    hbox = gtk_hbox_new(TRUE, 4);
    radio = pack_unmount_radio(path, upval,
            _("Do nothing"), UNMOUNT_PROMPT_NO_CHANGE, NULL, hbox);
    radio = pack_unmount_radio(path, upval,
            _("Unmount"), UNMOUNT_PROMPT_UNMOUNT, radio, hbox);
    radio = pack_unmount_radio(path, upval,
            _("Eject"), UNMOUNT_PROMPT_EJECT, radio, hbox);
    pack_unmount_radio(path, upval,
            _("Ask"), UNMOUNT_PROMPT_ASK, radio, hbox);
    return hbox;
}

/* Don't g_free() the result */
static const gchar *pretty_type(DirItem *file, const guchar *path)
{
	static gchar *text = NULL;

	null_g_free(&text);

	if (file->flags & ITEM_FLAG_SYMLINK)
		return _("Symbolic link");

	if (file->flags & ITEM_FLAG_APPDIR)
		return _("ROX application");

	if (file->flags & ITEM_FLAG_MOUNT_POINT)
	{
		MountPoint *mp;
		const gchar *mounted;

		mounted = mount_is_mounted(path, NULL, NULL)
			  ? _("mounted") : _("unmounted");

		mp = g_hash_table_lookup(fstab_mounts, path);
		if (mp)
			text = g_strdup_printf(_("Mount point for %s (%s)"),
					       mp->name, mounted);
		else
			text = g_strdup_printf(_("Mount point (%s)"), mounted);
		return text;
	}

	if (file->mime_type)
	{
		text = g_strconcat(file->mime_type->media_type, "/",
					  file->mime_type->subtype, NULL);
		return text;
	}

	return "-";
}
