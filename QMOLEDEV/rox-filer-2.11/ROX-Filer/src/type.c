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

/* type.c - code for dealing with filetypes */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <sys/param.h>
#include <fnmatch.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef WITH_GNOMEVFS
# include <libgnomevfs/gnome-vfs.h>
# include <libgnomevfs/gnome-vfs-mime.h>
# include <libgnomevfs/gnome-vfs-mime-handlers.h>
# include <libgnomevfs/gnome-vfs-application-registry.h>
#endif

#include "global.h"

#include "string.h"
#include "fscache.h"
#include "main.h"
#include "pixmaps.h"
#include "run.h"
#include "gui_support.h"
#include "choices.h"
#include "type.h"
#include "support.h"
#include "diritem.h"
#include "dnd.h"
#include "options.h"
#include "filer.h"
#include "action.h"		/* (for action_chmod) */
#include "xml.h"
#include "dropbox.h"
#include "xdgmime.h"
#include "xtypes.h"
#include "run.h"

#define TYPE_NS "http://www.freedesktop.org/standards/shared-mime-info"
enum {SET_MEDIA, SET_TYPE};

/* Colours for file types (same order as base types) */
static gchar *opt_type_colours[][2] = {
	{"display_err_colour",  "#ff0000"},
	{"display_unkn_colour", "#000000"},
	{"display_dir_colour",  "#000080"},
	{"display_pipe_colour", "#444444"},
	{"display_sock_colour", "#ff00ff"},
	{"display_file_colour", "#000000"},
	{"display_cdev_colour", "#000000"},
	{"display_bdev_colour", "#000000"},
	{"display_door_colour", "#ff00ff"},
	{"display_exec_colour", "#006000"},
	{"display_adir_colour", "#006000"}
};
#define NUM_TYPE_COLOURS\
		(sizeof(opt_type_colours) / sizeof(opt_type_colours[0]))

/* Parsed colours for file types */
static Option o_type_colours[NUM_TYPE_COLOURS];
static GdkColor	type_colours[NUM_TYPE_COLOURS];

/* Static prototypes */
static void alloc_type_colours(void);
static void options_changed(void);
static char *get_action_save_path(GtkWidget *dialog);
static MIME_type *get_mime_type(const gchar *type_name, gboolean can_create);
static gboolean remove_handler_with_confirm(const guchar *path);
static void set_icon_theme(void);
static GList *build_icon_theme(Option *option, xmlNode *node, guchar *label);

/* Hash of all allocated MIME types, indexed by "media/subtype".
 * MIME_type structs are never freed; this table prevents memory leaks
 * when rereading the config files.
 */
static GHashTable *type_hash = NULL;

/* Most things on Unix are text files, so this is the default type */
MIME_type *text_plain;
MIME_type *inode_directory;
MIME_type *inode_mountpoint;
MIME_type *inode_pipe;
MIME_type *inode_socket;
MIME_type *inode_block_dev;
MIME_type *inode_char_dev;
MIME_type *application_executable;
MIME_type *application_octet_stream;
MIME_type *application_x_shellscript;
MIME_type *application_x_desktop;
MIME_type *inode_unknown;
MIME_type *inode_door;

static Option o_display_colour_types;
static Option o_icon_theme;

static GtkIconTheme *icon_theme = NULL;
static GtkIconTheme *rox_theme = NULL;
static GtkIconTheme *gnome_theme = NULL;

void type_init(void)
{
	int	    i;

	icon_theme = gtk_icon_theme_new();
	
	type_hash = g_hash_table_new(g_str_hash, g_str_equal);

	text_plain = get_mime_type("text/plain", TRUE);
	inode_directory = get_mime_type("inode/directory", TRUE);
	inode_mountpoint = get_mime_type("inode/mount-point", TRUE);
	inode_pipe = get_mime_type("inode/fifo", TRUE);
	inode_socket = get_mime_type("inode/socket", TRUE);
	inode_block_dev = get_mime_type("inode/blockdevice", TRUE);
	inode_char_dev = get_mime_type("inode/chardevice", TRUE);
	application_executable = get_mime_type("application/x-executable", TRUE);
	application_octet_stream = get_mime_type("application/octet-stream", TRUE);
	application_x_shellscript = get_mime_type("application/x-shellscript", TRUE);
	application_x_desktop = get_mime_type("application/x-desktop", TRUE);
	application_x_desktop->executable = TRUE;
	inode_unknown = get_mime_type("inode/unknown", TRUE);
	inode_door = get_mime_type("inode/door", TRUE);

	option_add_string(&o_icon_theme, "icon_theme", "ROX");
	option_add_int(&o_display_colour_types, "display_colour_types", TRUE);
	option_register_widget("icon-theme-chooser", build_icon_theme);
	
	for (i = 0; i < NUM_TYPE_COLOURS; i++)
		option_add_string(&o_type_colours[i],
				  opt_type_colours[i][0],
				  opt_type_colours[i][1]);
	alloc_type_colours();

	set_icon_theme();

	option_add_notify(options_changed);
}

/* Read-load all the glob patterns.
 * Note: calls filer_update_all.
 */
void reread_mime_files(void)
{
	gtk_icon_theme_rescan_if_needed(icon_theme);

	xdg_mime_shutdown();

	filer_update_all();
}

/* Returns the MIME_type structure for the given type name. It is looked
 * up in type_hash and returned if found. If not found (and can_create is
 * TRUE) then a new MIME_type is made, added to type_hash and returned.
 * NULL is returned if type_name is not in type_hash and can_create is
 * FALSE, or if type_name does not contain a '/' character.
 */
static MIME_type *get_mime_type(const gchar *type_name, gboolean can_create)
{
        MIME_type *mtype;
	gchar *slash;

	mtype = g_hash_table_lookup(type_hash, type_name);
	if (mtype || !can_create)
		return mtype;

	slash = strchr(type_name, '/');
	if (slash == NULL)
	{
		g_warning("MIME type '%s' does not contain a '/' character!",
			  type_name);
		return NULL;
	}

	mtype = g_new(MIME_type, 1);
	mtype->media_type = g_strndup(type_name, slash - type_name);
	mtype->subtype = g_strdup(slash + 1);
	mtype->image = NULL;
	mtype->comment = NULL;

	mtype->executable = xdg_mime_mime_type_subclass(type_name,
						"application/x-executable");

	g_hash_table_insert(type_hash, g_strdup(type_name), mtype);

	return mtype;
}

const char *basetype_name(DirItem *item)
{
	if (item->flags & ITEM_FLAG_SYMLINK)
		return _("Sym link");
	else if (item->flags & ITEM_FLAG_MOUNT_POINT)
		return _("Mount point");
	else if (item->flags & ITEM_FLAG_APPDIR)
		return _("App dir");

	switch (item->base_type)
	{
		case TYPE_FILE:
			return _("File");
		case TYPE_DIRECTORY:
			return _("Dir");
		case TYPE_CHAR_DEVICE:
			return _("Char dev");
		case TYPE_BLOCK_DEVICE:
			return _("Block dev");
		case TYPE_PIPE:
			return _("Pipe");
		case TYPE_SOCKET:
			return _("Socket");
		case TYPE_DOOR:
			return _("Door");
	}
	
	return _("Unknown");
}

struct mime_list {
	GList *list;
	gboolean only_regular;
};

static void append_names(gpointer key, gpointer value, gpointer udata)
{
	struct mime_list *mlist = (struct mime_list*) udata;

	if(!mlist->only_regular || strncmp((char *)key, "inode/", 6)!=0)
		mlist->list = g_list_prepend(mlist->list, key);
}

/* Return list of all mime type names. Caller must free the list
 * but NOT the strings it contains (which are never freed).
 If only_regular is true then inode types are excluded.
 */
GList *mime_type_name_list(gboolean only_regular)
{
	struct mime_list list;

	list.list=NULL;
	list.only_regular=only_regular;
		
	g_hash_table_foreach(type_hash, append_names, &list);
	list.list = g_list_sort(list.list, (GCompareFunc) strcmp);

	return list.list;
}

/*			MIME-type guessing 			*/

/* Get the type of this file - stats the file and uses that if
 * possible. For regular or missing files, uses the pathname.
 */
MIME_type *type_get_type(const guchar *path)
{
	DirItem		*item;
	MIME_type	*type = NULL;

	item = diritem_new("");
	diritem_restat(path, item, NULL);
	if (item->base_type != TYPE_ERROR)
		type = item->mime_type;
	diritem_free(item);

	if (type)
		return type;

	type = type_from_path(path);

	if (!type)
		return text_plain;

	return type;
}

/* Returns a pointer to the MIME-type.
 *
 * Tries all enabled methods:
 * - Look for extended attribute
 * - If no attribute, check file name
 * - If no name rule, check contents
 *
 * NULL if we can't think of anything.
 */
MIME_type *type_from_path(const char *path)
{
	MIME_type *mime_type = NULL;
	const char *type_name;

	/* Check for extended attribute first */
	mime_type = xtype_get(path);
	if (mime_type)
		return mime_type;

	/* Try name and contents next */
	type_name = xdg_mime_get_mime_type_for_file(path, NULL);
	if (type_name)
		return get_mime_type(type_name, TRUE);

	return NULL;
}

/* Returns the file/dir in Choices for handling this type.
 * NULL if there isn't one. g_free() the result.
 */
char *handler_for(MIME_type *type)
{
	char	*type_name;
	char	*open;
	char	*target;

	type_name = g_strconcat(type->media_type, "_", type->subtype, NULL);
	open = choices_find_xdg_path_load(type_name, "MIME-types", SITE);
	g_free(type_name);

	if (!open)
		open = choices_find_xdg_path_load(type->media_type,
						  "MIME-types", SITE);

	if (!open)
		return NULL;

	/* Some programs behave differently depending on the command
	 * name (e.g., 'vim' vs 'gvim'), so symlinks need to be followed here.
	 */
	target = readlink_dup(open);
	if (!target)
	{
		return open;
	}

	if (target[0] == '/')
	{
		/* Absolute path */
		g_free(open);
		return target;
	}
	else
	{
		/* Relative path (shouldn't normally be needed) */
		gchar *dir;
		char *abs_path;

		dir = g_path_get_dirname(open);
		g_free(open);

		abs_path = g_strconcat(dir, "/", target, NULL);
		g_free(target);
		g_free(dir);

		return abs_path;
	}
}

MIME_type *mime_type_lookup(const char *type)
{
	return get_mime_type(type, TRUE);
}

static void init_aux_theme(GtkIconTheme **ptheme, const char *name)
{
	if (*ptheme)
		return;
	*ptheme = gtk_icon_theme_new();
	gtk_icon_theme_set_custom_theme(*ptheme, name);
}

inline static void init_rox_theme(void)
{
	init_aux_theme(&rox_theme, "ROX");
}

inline static void init_gnome_theme(void)
{
	init_aux_theme(&gnome_theme, "gnome");
}

/* We don't want ROX to override configured theme so try all possibilities
 * in icon_theme first */
static GtkIconInfo *mime_type_lookup_icon_info(GtkIconTheme *theme,
		MIME_type *type)
{
	char *type_name = g_strconcat(type->media_type, "-", type->subtype, NULL);
	GtkIconInfo *full = gtk_icon_theme_lookup_icon(theme, type_name, HUGE_HEIGHT, 0);

	g_free(type_name);
	if (!full)
	{
		/* Ugly hack... try for a GNOME icon */
		if (type == inode_directory)
			type_name = g_strdup("gnome-fs-directory");
		else
			type_name = g_strconcat("gnome-mime-", type->media_type,
					"-", type->subtype, NULL);
		full = gtk_icon_theme_lookup_icon(theme, type_name, HUGE_HEIGHT, 0);
		g_free(type_name);
	}
	if (!full)
	{
		/* Try for a media type */
		type_name = g_strconcat(type->media_type, "-x-generic", NULL);
		full = gtk_icon_theme_lookup_icon(theme, type_name, HUGE_HEIGHT, 0);
		g_free(type_name);
	}
	if (!full)
	{
		/* Ugly hack... try for a GNOME default media icon */
		type_name = g_strconcat("gnome-mime-", type->media_type, NULL);

		full = gtk_icon_theme_lookup_icon(theme, type_name, HUGE_HEIGHT, 0);
		g_free(type_name);
	}
	return full;
}

/*			Actions for types 			*/

/* Return the image for this type, loading it if needed.
 * Places to check are: (eg type="text_plain", base="text")
 * 1. <Choices>/MIME-icons/base_subtype
 * 2. Icon theme 'mime-base:subtype'
 * 3. Icon theme 'mime-base'
 * 4. Unknown type icon.
 *
 * Special case: If an icon cannot be found for inode/mount-point, the icon for
 * inode/directory will be returned (if possible).
 *
 * Note: You must g_object_unref() the image afterwards.
 */
MaskedPixmap *type_to_icon(MIME_type *type)
{
	GtkIconInfo *full;
	char	*type_name, *path;
	time_t	now;

	if (type == NULL)
	{
		g_object_ref(im_unknown);
		return im_unknown;
	}

	now = time(NULL);
	/* Already got an image? */
	if (type->image)
	{
		/* Yes - don't recheck too often */
		if (abs(now - type->image_time) < 2)
		{
			g_object_ref(type->image);
			return type->image;
		}
		g_object_unref(type->image);
		type->image = NULL;
	}

again:
	type_name = g_strconcat(type->media_type, "_", type->subtype,
				".png", NULL);
	path = choices_find_xdg_path_load(type_name, "MIME-icons", SITE);
	g_free(type_name);
	if (path)
	{
		type->image = g_fscache_lookup(pixmap_cache, path);
		g_free(path);
	}

	if (type->image)
		goto out;

	full = mime_type_lookup_icon_info(icon_theme, type);
	if (!full && icon_theme != rox_theme)
	{
		init_rox_theme();
		full = mime_type_lookup_icon_info(rox_theme, type);
	}
	if (!full && icon_theme != gnome_theme)
	{
		init_gnome_theme();
		full = mime_type_lookup_icon_info(gnome_theme, type);
	}
	if (!full && type == inode_mountpoint)
	{
		/* Try to use the inode/directory icon for inode/mount-point */
		type = inode_directory;
		goto again;
	}
	if (full)
	{
		const char *icon_path;
		/* Get the actual icon through our cache, not through GTK, because
		 * GTK doesn't cache icons.
		 */
		icon_path = gtk_icon_info_get_filename(full);
		if (icon_path != NULL)
			type->image = g_fscache_lookup(pixmap_cache, icon_path);
		/* else shouldn't happen, because we didn't use
		 * GTK_ICON_LOOKUP_USE_BUILTIN.
		 */
		gtk_icon_info_free(full);
	}

out:
	if (!type->image)
	{
		/* One ref from the type structure, one returned */
		type->image = im_unknown;
		g_object_ref(im_unknown);
	}

	type->image_time = now;
	
	g_object_ref(type->image);
	return type->image;
}

GdkAtom type_to_atom(MIME_type *type)
{
	char	*str;
	GdkAtom	retval;
	
	g_return_val_if_fail(type != NULL, GDK_NONE);

	str = g_strconcat(type->media_type, "/", type->subtype, NULL);
	retval = gdk_atom_intern(str, FALSE);
	g_free(str);
	
	return retval;
}

static void show_shell_help(gpointer data)
{
	info_message(_("Enter a shell command which will load \"$@\" into "
			"a suitable program. Eg:\n\n"
			"gimp \"$@\""));
}

/* Called if the user clicks on the OK button. Returns FALSE if an error
 * was displayed instead of performing the action.
 */
static gboolean set_shell_action(GtkWidget *dialog)
{
	GtkEntry *entry;
	const guchar *command;
	gchar	*tmp, *path;
	int	error = 0, len;
	int	fd;

	entry = g_object_get_data(G_OBJECT(dialog), "shell_command");

	g_return_val_if_fail(entry != NULL, FALSE);

	command = gtk_entry_get_text(entry);
	
	if (!strchr(command, '$'))
	{
		show_shell_help(NULL);
		return FALSE;
	}

	path = get_action_save_path(dialog);
	if (!path)
		return FALSE;
		
	tmp = g_strdup_printf("#! /bin/sh\nexec %s\n", command);
	len = strlen(tmp);
	
	fd = open(path, O_CREAT | O_WRONLY, 0755);
	if (fd == -1)
		error = errno;
	else
	{
		FILE *file;

		file = fdopen(fd, "w");
		if (file)
		{
			if (fwrite(tmp, 1, len, file) < len)
				error = errno;
			if (fclose(file) && error == 0)
				error = errno;
		}
		else
			error = errno;
	}

	if (error)
		report_error(g_strerror(error));

	g_free(tmp);
	g_free(path);

	gtk_widget_destroy(dialog);

	return TRUE;
}

static void set_action_response(GtkWidget *dialog, gint response, gpointer data)
{
	if (response == GTK_RESPONSE_OK)
		if (!set_shell_action(dialog))
			return;
	gtk_widget_destroy(dialog);
}

/* Return the path of the file in choices that handles this type and
 * radio setting.
 * NULL if nothing is defined for it.
 */
static guchar *handler_for_radios(GObject *dialog)
{
	Radios	*radios;
	MIME_type *type;

	radios = g_object_get_data(G_OBJECT(dialog), "rox-radios");
	type = g_object_get_data(G_OBJECT(dialog), "mime_type");
	
	g_return_val_if_fail(radios != NULL, NULL);
	g_return_val_if_fail(type != NULL, NULL);
	
	switch (radios_get_value(radios))
	{
		case SET_MEDIA:
			return choices_find_xdg_path_load(type->media_type,
							  "MIME-types", SITE);
		case SET_TYPE:
		{
			gchar *tmp, *handler;
			tmp = g_strconcat(type->media_type, "_",
					  type->subtype, NULL);
			handler = choices_find_xdg_path_load(tmp,
							     "MIME-types",
							     SITE);
			g_free(tmp);
			return handler;
		}
		default:
			g_warning("Bad type");
			return NULL;
	}
}

/* (radios can be NULL if called from clear_run_action) */
static void run_action_update(Radios *radios, gpointer data)
{
	guchar *handler;
	DropBox *drop_box;
	GObject *dialog = G_OBJECT(data);

	drop_box = g_object_get_data(dialog, "rox-dropbox");

	g_return_if_fail(drop_box != NULL);

	handler = handler_for_radios(dialog);

	if (handler)
	{
		char *old = handler;

		handler = readlink_dup(old);
		if (handler)
			g_free(old);
		else
			handler = old;
	}

	drop_box_set_path(DROP_BOX(drop_box), handler);
	g_free(handler);
}

static void clear_run_action(GtkWidget *drop_box, GtkWidget *dialog)
{
	guchar *handler;

	handler = handler_for_radios(G_OBJECT(dialog));

	if (handler)
		remove_handler_with_confirm(handler);

	run_action_update(NULL, dialog);
}

/* Called when a URI list is dropped onto the box in the Set Run Action
 * dialog. Make sure it's an application, and make that the default
 * handler.
 */
static void drag_app_dropped(GtkWidget	*drop_box,
			     const guchar *app,
			     GtkWidget	*dialog)
{
	DirItem	*item;

	item = diritem_new("");
	diritem_restat(app, item, NULL);
	if (item->flags & ITEM_FLAG_APPDIR || EXECUTABLE_FILE(item))
	{
		guchar	*path;

		path = get_action_save_path(dialog);

		if (path)
		{
			if (symlink(app, path))
				delayed_error("symlink: %s",
						g_strerror(errno));
			else
				destroy_on_idle(dialog);

			g_free(path);
		}
	}
	else
		delayed_error(
			_("This is not a program! Give me an application "
			"instead!"));

	diritem_free(item);
}

/* Find the current command which is used to run files of this type.
 * Returns NULL on failure. g_free() the result.
 */
static guchar *get_current_command(MIME_type *type)
{
	struct stat	info;
	char *handler, *nl, *data = NULL;
	long len;
	guchar *command = NULL;

	handler = handler_for(type);

	if (!handler)
		return NULL;		/* No current handler */

	if (stat(handler, &info))
		goto out;		/* Can't stat */

	if ((!S_ISREG(info.st_mode)) || info.st_size > 256)
		goto out;		/* Only use small regular files */
	
	if (!load_file(handler, &data, &len))
		goto out;		/* Didn't load OK */

	if (strncmp(data, "#! /bin/sh\nexec ", 16) != 0)
		goto out;		/* Not one of ours */

	nl = strchr(data + 16, '\n');
	if (!nl)
		goto out;		/* No newline! */

	command = g_strndup(data + 16, nl - data - 16);
out:
	g_free(handler);
	g_free(data);
	return command;
}

/* Find the current command which is used to run files of this type,
 * and return a textual description of it.
 * Only call for non-executable files.
 * g_free() the result.
 */
gchar *describe_current_command(MIME_type *type)
{
	char *handler;
	char *desc = NULL;
	struct stat info;

	g_return_val_if_fail(type != NULL, NULL);

	handler = handler_for(type);

	if (!handler)
		return g_strdup(_("No run action defined"));

	if (mc_stat(handler, &info) !=0 )
	{
		desc = g_strdup_printf(_("Error in handler %s: %s"), handler,
					g_strerror(errno));
		goto out;
	}

	if (S_ISDIR(info.st_mode))
	{
		const guchar *tmp;
		uid_t dir_uid = info.st_uid;

		tmp = make_path(handler, "AppRun");

		if (mc_lstat(tmp, &info) != 0 || info.st_uid != dir_uid
			|| !(info.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)))
			desc = g_strdup_printf(
				_("Invalid application %s (bad AppRun)"),
				handler);
		/* Else, just report handler... */

		goto out;
	}

	/* It's not an application directory, and it's not a symlink... */

	if (access(handler, X_OK) != 0)
	{
		desc = g_strdup_printf(_("Non-executable %s"), handler);
		goto out;
	}

	desc = get_current_command(type);
out:
	if (!desc)
		desc = handler;
	else
		g_free(handler);

	return desc;
}

/* Display a dialog box allowing the user to set the default run action
 * for this type.
 */
void type_set_handler_dialog(MIME_type *type)
{
	guchar		*tmp;
	GtkDialog	*dialog;
	GtkWidget	*frame, *entry, *label, *button;
	GtkWidget	*hbox;
	Radios		*radios;

	g_return_if_fail(type != NULL);

	dialog = GTK_DIALOG(gtk_dialog_new());
	gtk_dialog_set_has_separator(dialog, FALSE);
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);

	g_object_set_data(G_OBJECT(dialog), "mime_type", type);

	gtk_window_set_title(GTK_WINDOW(dialog), _("Set run action"));

	radios = radios_new(run_action_update, dialog);
	g_object_set_data(G_OBJECT(dialog), "rox-radios", radios);

	radios_add(radios,
			_("If a handler for the specific type isn't set up, "
			  "use this as the default."), SET_MEDIA,
			_("Set default for all `%s/<anything>'"),
			type->media_type);
	
	radios_add(radios,
			_("Use this application for all files with this MIME "
			  "type."), SET_TYPE,
			_("Only for the type `%s' (%s/%s)"),
			mime_type_comment(type),
			type->media_type, type->subtype);

	radios_set_value(radios, SET_TYPE);

	frame = drop_box_new(_("Drop a suitable application here"));

	g_object_set_data(G_OBJECT(dialog), "rox-dropbox", frame);
	
	radios_pack(radios, GTK_BOX(dialog->vbox));
	gtk_box_pack_start(GTK_BOX(dialog->vbox), frame, TRUE, TRUE, 0);

	g_signal_connect(frame, "path_dropped",
			G_CALLBACK(drag_app_dropped), dialog);
	g_signal_connect(frame, "clear",
			G_CALLBACK(clear_run_action), dialog);

	hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(dialog->vbox), hbox, FALSE, TRUE, 4);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_hseparator_new(), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(_("OR")),
						FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_hseparator_new(), TRUE, TRUE, 0);

	hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(dialog->vbox), hbox, FALSE, TRUE, 0);

	label = gtk_label_new(_("Enter a shell command:")),
	gtk_misc_set_alignment(GTK_MISC(label), 0, .5);
	gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 4);

	gtk_box_pack_start(GTK_BOX(hbox),
			new_help_button(show_shell_help, NULL), FALSE, TRUE, 0);

	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(dialog->vbox), entry, FALSE, TRUE, 0);
	gtk_widget_grab_focus(entry);
	g_object_set_data(G_OBJECT(dialog), "shell_command", entry);
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);

	/* If possible, fill in the entry box with the current command */
	tmp = get_current_command(type);
	if (tmp)
	{
		gtk_entry_set_text(GTK_ENTRY(entry), tmp);
		gtk_editable_set_position(GTK_EDITABLE(entry), -1);
		g_free(tmp);
	}
	else
	{
		gtk_entry_set_text(GTK_ENTRY(entry), " \"$@\"");
		gtk_editable_set_position(GTK_EDITABLE(entry), 0);
	}

	gtk_dialog_add_button(dialog, GTK_STOCK_CLOSE, GTK_RESPONSE_CANCEL);

	button = button_new_mixed(GTK_STOCK_OK, _("_Use Command"));
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_dialog_add_action_widget(dialog, button, GTK_RESPONSE_OK);

	hbox = gtk_hbox_new(TRUE, 4);
	gtk_box_pack_start(GTK_BOX(dialog->vbox), hbox, FALSE, TRUE, 0);

	gtk_dialog_set_default_response(dialog, GTK_RESPONSE_OK);
	
	g_signal_connect(dialog, "response",
			G_CALLBACK(set_action_response), NULL);

	gtk_widget_show_all(GTK_WIDGET(dialog));
}

/* path is an entry in Choices. If it's a symlink or a very small executable
 * then just get rid of it, otherwise confirm first. It it doesn't exist,
 * do nothing.
 *
 * FALSE on error (abort operation).
 */
static gboolean remove_handler_with_confirm(const guchar *path)
{
	struct stat info;

	if (lstat(path, &info) == 0)
	{
		/* A binding already exists... */
		if (S_ISREG(info.st_mode) && info.st_size > 256)
		{
			if (!confirm(_("A run action already exists and is "
				      "quite a big program - are you sure "
				      "you want to delete it?"),
				    GTK_STOCK_DELETE, NULL))
			{
				return FALSE;
			}
		}
		
		if (unlink(path))
		{
			report_error(_("Can't remove %s: %s"),
				path, g_strerror(errno));
			return FALSE;
		}
	}

	return TRUE;
}

/* The user wants to set a new default action for files of this type (or just
 * clear the action). Removes the current binding if possible and returns the
 * path to save the new one to. NULL means cancel. g_free() the result.
 */
static char *get_action_save_path(GtkWidget *dialog)
{
	guchar		*path = NULL;
	guchar 		*type_name = NULL;
	MIME_type	*type;
	Radios		*radios;

	g_return_val_if_fail(dialog != NULL, NULL);

	type = g_object_get_data(G_OBJECT(dialog), "mime_type");
	radios = g_object_get_data(G_OBJECT(dialog), "rox-radios");

	g_return_val_if_fail(radios != NULL && type != NULL, NULL);

	if (radios_get_value(radios) == SET_MEDIA)
		type_name = g_strdup(type->media_type);
	else
		type_name = g_strconcat(type->media_type, "_",
				type->subtype, NULL);

	path = choices_find_xdg_path_save("", PROJECT, SITE, FALSE);
	if (!path)
	{
		report_error(
		_("Choices saving is disabled by CHOICESPATH variable"));
		goto out;
	}
	g_free(path);

	path = choices_find_xdg_path_save(type_name, "MIME-types", SITE, TRUE);

	if (!remove_handler_with_confirm(path))
		null_g_free(&path);
out:
	g_free(type_name);
	return path;
}

MIME_type *mime_type_from_base_type(int base_type)
{
	switch (base_type)
	{
		case TYPE_FILE:
			return text_plain;
		case TYPE_DIRECTORY:
			return inode_directory;
		case TYPE_PIPE:
			return inode_pipe;
		case TYPE_SOCKET:
			return inode_socket;
		case TYPE_BLOCK_DEVICE:
			return inode_block_dev;
		case TYPE_CHAR_DEVICE:
			return inode_char_dev;
	        case TYPE_DOOR:
	                return inode_door;
	}
	return inode_unknown;
}

/* Takes the st_mode field from stat() and returns the base type.
 * Should not be a symlink.
 */
int mode_to_base_type(int st_mode)
{
	if (S_ISREG(st_mode))
		return TYPE_FILE;
	else if (S_ISDIR(st_mode))
		return TYPE_DIRECTORY;
	else if (S_ISBLK(st_mode))
		return TYPE_BLOCK_DEVICE;
	else if (S_ISCHR(st_mode))
		return TYPE_CHAR_DEVICE;
	else if (S_ISFIFO(st_mode))
		return TYPE_PIPE;
	else if (S_ISSOCK(st_mode))
		return TYPE_SOCKET;
	else if (S_ISDOOR(st_mode))
		return TYPE_DOOR;

	return TYPE_ERROR;
}

/* Returns TRUE is this is something that is run by looking up its type
 * in MIME-types and, hence, can have its run action set.
 */
gboolean can_set_run_action(DirItem *item)
{
	g_return_val_if_fail(item != NULL, FALSE);

	return item->base_type == TYPE_FILE && !EXECUTABLE_FILE(item);
}

/* Parse file type colours and allocate/free them as necessary */
static void alloc_type_colours(void)
{
	gboolean	success[NUM_TYPE_COLOURS];
	int		change_count = 0;	/* No. needing realloc */
	int		i;
	static gboolean	allocated = FALSE;

	/* Parse colours */
	for (i = 0; i < NUM_TYPE_COLOURS; i++)
	{
		GdkColor *c = &type_colours[i];
		gushort r = c->red;
		gushort g = c->green;
		gushort b = c->blue;

		gdk_color_parse(o_type_colours[i].value, &type_colours[i]);

		if (allocated && (c->red != r || c->green != g || c->blue != b))
			change_count++;
	}
	
	/* Free colours if they were previously allocated and
	 * have changed or become unneeded.
	 */
	if (allocated && (change_count || !o_display_colour_types.int_value))
	{
		gdk_colormap_free_colors(gdk_rgb_get_colormap(),
					 type_colours, NUM_TYPE_COLOURS);
		allocated = FALSE;
	}

	/* Allocate colours, unless they are still allocated (=> they didn't
	 * change) or we don't want them anymore.
	 * XXX: what should be done if allocation fails?
	 */
	if (!allocated && o_display_colour_types.int_value)
	{
		gdk_colormap_alloc_colors(gdk_rgb_get_colormap(),
				type_colours, NUM_TYPE_COLOURS,
				FALSE, TRUE, success);
		allocated = TRUE;
	}
}

static void expire_timer(gpointer key, gpointer value, gpointer data)
{
	MIME_type *type = value;

	type->image_time = 0;
}

static void options_changed(void)
{
	alloc_type_colours();
	if (o_icon_theme.has_changed)
	{
		set_icon_theme();
		g_hash_table_foreach(type_hash, expire_timer, NULL);
		full_refresh();
	}
}

/* Return a pointer to a (static) colour for this item. If colouring is
 * off, returns normal.
 */
GdkColor *type_get_colour(DirItem *item, GdkColor *normal)
{
	int type = item->base_type;

	if (!o_display_colour_types.int_value)
		return normal;

	if (EXECUTABLE_FILE(item))
		type = TYPE_EXEC;
	else if (item->flags & ITEM_FLAG_APPDIR)
		type = TYPE_APPDIR;

	g_return_val_if_fail(type >= 0 && type < NUM_TYPE_COLOURS, normal);

	return &type_colours[type];
}

static char **get_xdg_data_dirs(int *n_dirs)
{
	const char *env;
	char **dirs;
	int i, n;

	env = getenv("XDG_DATA_DIRS");
	if (!env)
		env = "/usr/local/share/:/usr/share/";
	dirs = g_strsplit(env, ":", 0);
	g_return_val_if_fail(dirs != NULL, NULL);
	for (n = 0; dirs[n]; n++)
		;
	for (i = n; i > 0; i--)
		dirs[i] = dirs[i - 1];
	env = getenv("XDG_DATA_HOME");
	if (env)
		dirs[0] = g_strdup(env);
	else
		dirs[0] = g_build_filename(g_get_home_dir(), ".local",
					   "share", NULL);
	*n_dirs = n + 1;
	return dirs;
}

/* Try to fill in 'type->comment' from this document */
static void get_comment(MIME_type *type, const guchar *path)
{
	xmlNode *node;
	XMLwrapper *doc;
	
	doc = xml_cache_load(path);
	if (!doc)
		return;

	node = xml_get_section(doc, TYPE_NS, "comment");

	if (node)
	{
		char *val;
		g_return_if_fail(type->comment == NULL);
		val= xmlNodeListGetString(node->doc, node->xmlChildrenNode, 1);
		type->comment = g_strdup(val);
		xmlFree(val);
	}

	g_object_unref(doc);
}

/* Fill in the comment field for this MIME type */
static void find_comment(MIME_type *type)
{
	char **dirs;
	int i, n_dirs = 0;

	if (type->comment)
	{
		g_free(type->comment);
		type->comment = NULL;
	}

	dirs = get_xdg_data_dirs(&n_dirs);
	g_return_if_fail(dirs != NULL);

	for (i = 0; i < n_dirs; i++)
	{
		guchar *path;
		
		path = g_strdup_printf("%s/mime/%s/%s.xml", dirs[i],
				type->media_type, type->subtype);
		get_comment(type, path);
		g_free(path);
		if (type->comment)
			break;
	}

	if (!type->comment)
		type->comment = g_strdup_printf("%s/%s", type->media_type,
						type->subtype);

	for (i = 0; i < n_dirs; i++)
		g_free(dirs[i]);
	g_free(dirs);
}

const char *mime_type_comment(MIME_type *type)
{
	if (!type->comment)
		find_comment(type);

	return type->comment;
}

static void unref_icon_theme(void)
{
	if (icon_theme && icon_theme != rox_theme && icon_theme != gnome_theme)
		g_object_unref(icon_theme);
}

static void set_icon_theme(void)
{
	struct stat info;
	char *icon_home;
	const char *theme_dir;
	const char *theme_name = o_icon_theme.value;

	if (!theme_name || !*theme_name)
		theme_name = "ROX";

	if (!strcmp(theme_name, "ROX"))
	{
		unref_icon_theme();
		init_rox_theme();
		icon_theme = rox_theme;
	}
	else if (!strcmp(theme_name, "gnome"))
	{
		unref_icon_theme();
		init_gnome_theme();
		icon_theme = gnome_theme;
	}
	else
	{
		if (icon_theme == rox_theme || icon_theme == gnome_theme)
			icon_theme = gtk_icon_theme_new();
		gtk_icon_theme_set_custom_theme(icon_theme, theme_name);
	}

	/* Ensure the ROX theme exists. */

	icon_home = g_build_filename(home_dir, ".icons", "ROX", NULL);
	if (stat(icon_home, &info) == 0)
		return;	/* Already exists */

	/* First, create the .icons directory */
	theme_dir = make_path(home_dir, ".icons");
	if (!file_exists(theme_dir))
		mkdir(theme_dir, 0755);

	if (lstat(icon_home, &info) == 0)
	{
		/* Probably a broken symlink, then. Remove it. */
		if (unlink(icon_home))
			g_warning("Error removing broken symlink %s: %s", icon_home, g_strerror(errno));
		else
			g_warning("Removed broken symlink %s", icon_home);
	}

	if (symlink(make_path(app_dir, "ROX"), icon_home))
	{
		delayed_error(_("Failed to create symlink '%s':\n%s"), icon_home, g_strerror(errno));
		open_to_show(icon_home);
	}
	g_free(icon_home);

	gtk_icon_theme_rescan_if_needed(icon_theme);
}

static guchar *read_theme(Option *option)
{
	GtkOptionMenu *om = GTK_OPTION_MENU(option->widget);
	GtkLabel *item;

	item = GTK_LABEL(GTK_BIN(om)->child);

	g_return_val_if_fail(item != NULL, g_strdup("ROX"));

	return g_strdup(gtk_label_get_text(item));
}

static void update_theme(Option *option)
{
	GtkOptionMenu *om = GTK_OPTION_MENU(option->widget);
	GtkWidget *menu;
	GList *kids, *next;
	int i = 0;

	menu = gtk_option_menu_get_menu(om);

	kids = gtk_container_get_children(GTK_CONTAINER(menu));
	for (next = kids; next; next = next->next, i++)
	{
		GtkLabel *item = GTK_LABEL(GTK_BIN(next->data)->child);
		const gchar *label;

		/* The label actually moves from the menu!! */
		if (!item)
			item = GTK_LABEL(GTK_BIN(om)->child);

		label = gtk_label_get_text(item);

		g_return_if_fail(label != NULL);

		if (strcmp(label, option->value) == 0)
			break;
	}
	g_list_free(kids);
	
	if (next)
		gtk_option_menu_set_history(om, i);
	else
		g_warning("Theme '%s' not found", option->value);
}

static void add_themes_from_dir(GPtrArray *names, const char *dir)
{
	GPtrArray *list;
	int i;

	if (access(dir, F_OK) != 0)
		return;

	list = list_dir(dir);
	g_return_if_fail(list != NULL);

	for (i = 0; i < list->len; i++)
	{
		char *index_path;

		index_path = g_build_filename(dir, list->pdata[i],
						"index.theme", NULL);
		
		if (access(index_path, F_OK) == 0)
			g_ptr_array_add(names, list->pdata[i]);
		else
			g_free(list->pdata[i]);

		g_free(index_path);
	}

	g_ptr_array_free(list, TRUE);
}

static GList *build_icon_theme(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *button, *menu, *hbox;
	GPtrArray *names;
	gchar **theme_dirs = NULL;
	gint n_dirs = 0;
	int i;

	g_return_val_if_fail(option != NULL, NULL);
	g_return_val_if_fail(label != NULL, NULL);

	hbox = gtk_hbox_new(FALSE, 4);

	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(_(label)),
				FALSE, TRUE, 0);

	button = gtk_option_menu_new();
	gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);

	menu = gtk_menu_new();
	gtk_option_menu_set_menu(GTK_OPTION_MENU(button), menu);

	gtk_icon_theme_get_search_path(icon_theme, &theme_dirs, &n_dirs);
	names = g_ptr_array_new();
	for (i = 0; i < n_dirs; i++)
		add_themes_from_dir(names, theme_dirs[i]);
	g_strfreev(theme_dirs);

	g_ptr_array_sort(names, strcmp2);

	for (i = 0; i < names->len; i++)
	{
		GtkWidget *item;
		char *name = names->pdata[i];

		item = gtk_menu_item_new_with_label(name);
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		gtk_widget_show_all(item);

		g_free(name);
	}

	g_ptr_array_free(names, TRUE);

	option->update_widget = update_theme;
	option->read_widget = read_theme;
	option->widget = button;

	gtk_signal_connect_object(GTK_OBJECT(button), "changed",
			GTK_SIGNAL_FUNC(option_check_widget),
			(GtkObject *) option);

	return g_list_append(NULL, hbox);
}

GtkIconInfo *theme_lookup_icon(const gchar *icon_name, gint size,
		GtkIconLookupFlags flags)
{
	GtkIconInfo *result = gtk_icon_theme_lookup_icon(icon_theme,
			icon_name, size, flags);

	if (!result && icon_theme != rox_theme)
	{
		init_rox_theme();
		result = gtk_icon_theme_lookup_icon(rox_theme,
			icon_name, size, flags);
	}
	if (!result && icon_theme != gnome_theme)
	{
		init_gnome_theme();
		result = gtk_icon_theme_lookup_icon(gnome_theme,
			icon_name, size, flags);
	}
	return result;
}

GdkPixbuf *theme_load_icon(const gchar *icon_name, gint size,
		GtkIconLookupFlags flags, GError **perror)
{
	GdkPixbuf *result = gtk_icon_theme_load_icon(icon_theme,
			icon_name, size, flags, NULL);

	if (!result && icon_theme != gnome_theme)
	{
		init_gnome_theme();
		result = gtk_icon_theme_load_icon(gnome_theme,
			icon_name, size, flags, NULL);
	}
	if (!result && icon_theme != rox_theme)
	{
		init_rox_theme();
		result = gtk_icon_theme_load_icon(rox_theme,
			icon_name, size, flags, perror);
	}
	return result;
}

