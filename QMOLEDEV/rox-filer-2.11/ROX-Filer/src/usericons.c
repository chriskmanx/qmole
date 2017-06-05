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

/* usericons.c - handle user-defined icons. Diego Zamboni, Feb 7, 2001. */

#include "config.h"

#include <gtk/gtk.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fnmatch.h>
#include <libxml/parser.h>
#include <time.h>

#include "global.h"

#include "fscache.h"
#include "diritem.h"
#include "dir.h"
#include "gui_support.h"
#include "choices.h"
#include "pixmaps.h"
#include "type.h"
#include "run.h"
#include "dnd.h"
#include "support.h"
#include "usericons.h"
#include "main.h"
#include "menu.h"
#include "filer.h"
#include "action.h"
#include "display.h"
#include "xml.h"
#include "dropbox.h"
#include "icon.h"

#define SET_MEDIA 2
#define SET_TYPE 1
#define SET_PATH 0		/* Store in globicons */
#define SET_COPY 3		/* Create .DirIcon */

static GHashTable *glob_icons = NULL; /* Pathname -> Icon pathname */

/* Static prototypes */
static const char *process_globicons_line(gchar *line);
static gboolean free_globicon(gpointer key, gpointer value, gpointer data);
static void write_globicons(void);
static void drag_icon_dropped(GtkWidget	*drop_box,
				  const guchar	*path,
				  GtkWidget	*dialog);
static gboolean set_icon_for_type(MIME_type *type, const gchar *iconpath,
				  gboolean just_media);
static gboolean convert_to_png(const gchar *src, const gchar *dest);
static void radios_changed(Radios *radios, gpointer data);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Read glob-pattern -> icon mappings from "globicons" in Choices */
void read_globicons()
{
	static time_t last_read = (time_t) 0;
	struct stat info;
	guchar *path;
	xmlDocPtr doc;

	if (!glob_icons)
		glob_icons = g_hash_table_new(g_str_hash, g_str_equal);

	path = choices_find_xdg_path_load("globicons", PROJECT, SITE);
	if (!path)
		return;	/* Nothing to load */

	if (mc_stat(path, &info))
		goto out;

	if (info.st_mtime <= last_read)
		goto out;  /* File hasn't been modified since we last read it */

	g_hash_table_foreach_remove(glob_icons, free_globicon, NULL);

	doc = xmlParseFile(path);
	if (doc)
	{
		xmlNodePtr node, icon, root;
		char	   *match;
		
		root = xmlDocGetRootElement(doc);
		
		/* Handle the new XML file format */
		for (node = root->xmlChildrenNode; node; node = node->next)
		{
			gchar *path, *icon_path;

			if (node->type != XML_ELEMENT_NODE)
				continue;
			if (strcmp(node->name, "rule") != 0)
				continue;
			icon = get_subnode(node, NULL, "icon");
			if (!icon)
				continue;
			match = xmlGetProp(node, "match");
			if (!match)
				continue;

			icon_path = xmlNodeGetContent(icon);
			path = expand_path(match);
			g_hash_table_insert(glob_icons, path, icon_path);
			g_free(match);
		}

		xmlFreeDoc(doc);
	}
	else
	{
		/* Handle the old non-XML format */
		parse_file(path, process_globicons_line);
		if (g_hash_table_size(glob_icons))
			write_globicons();	/* Upgrade to new format */
	}

	last_read = time(NULL);   /* Update time stamp */
out:
	g_free(path);
}

/* Set an item's image field according to the globicons patterns if
 * it matches one of them and the file exists.
 */
void check_globicon(const guchar *path, DirItem *item)
{
	gchar *gi;

	g_return_if_fail(item && !item->_image);

	gi = g_hash_table_lookup(glob_icons, path);
	if (gi)
		item->_image = g_fscache_lookup(pixmap_cache, gi);
}

static gboolean create_diricon(const guchar *filepath, const guchar *iconpath)
{
	if (!convert_to_png(iconpath, make_path(filepath, ".DirIcon")))
		return FALSE;

	dir_check_this(filepath);
	icons_may_update(filepath);

	return TRUE;
}

/* Add a globicon mapping for the given file to the given icon path */
static gboolean set_icon_path(const guchar *filepath, const guchar *iconpath)
{
	MaskedPixmap *pic;

	/* Check if file exists */
	if (!file_exists(iconpath))
	{
		delayed_error(_("The pathname you gave does not exist. "
			      	    "The icon has not been changed."));
		return FALSE;
	}

	/* Check if we can load the image, warn the user if not. */
	pic = g_fscache_lookup(pixmap_cache, iconpath);
	if (!pic)
	{
		delayed_error(
			_("Unable to load image file -- maybe it's not in a "
			  "format I understand, or maybe the permissions are "
			  "wrong?\n"
			  "The icon has not been changed."));
		return FALSE;
	}
	g_object_unref(pic);

	/* Add the globicon mapping and update visible icons */
	add_globicon(filepath, iconpath);

	return TRUE;
}

static void dialog_response(GtkWidget *dialog, gint response, gpointer data)
{
	gtk_widget_destroy(dialog);
}

static void clear_icon(DropBox *drop_box, GObject *dialog)
{
	Radios *radios;
	const guchar *pathname;

	pathname = g_object_get_data(G_OBJECT(dialog), "pathname");
	g_return_if_fail(pathname != NULL);
	
	radios = g_object_get_data(G_OBJECT(dialog), "radios");
	g_return_if_fail(radios != NULL);

	if (radios_get_value(radios) == SET_PATH)
	{
		delete_globicon(pathname);
	}
	else
	{
		const guchar *path;
		guchar *tmp;
		DropBox *drop_box;

		drop_box = g_object_get_data(G_OBJECT(dialog), "rox-dropbox");
		g_return_if_fail(drop_box != NULL);

		path = drop_box_get_path(drop_box);
		g_return_if_fail(path != NULL);

		tmp = g_strdup_printf(_("Really delete icon '%s'?"), path);
		if (confirm(tmp, GTK_STOCK_DELETE, NULL))
		{
			if (unlink(path))
				delayed_error(_("Can't delete '%s':\n%s"),
						path, g_strerror(errno));
			else
			{
				dir_check_this(pathname);
				icons_may_update(pathname);
			}
		}
		g_free(tmp);
	}

	full_refresh();
	radios_changed(g_object_get_data(dialog, "radios"), dialog);
}

/* Display a dialog box allowing the user to set the icon for
 * a file or directory.
 */
void icon_set_handler_dialog(DirItem *item, const guchar *path)
{
	struct stat	info;
	GtkDialog	*dialog;
	GtkWidget	*frame;
	Radios		*radios;
	
	g_return_if_fail(item != NULL && path != NULL);

	dialog = GTK_DIALOG(gtk_dialog_new());
	gtk_dialog_set_has_separator(dialog, FALSE);
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
	g_object_set_data_full(G_OBJECT(dialog), "pathname",
				 strdup(path), g_free);

	gtk_window_set_title(GTK_WINDOW(dialog), _("Set icon"));

	radios = radios_new(radios_changed, dialog);

	g_object_set_data(G_OBJECT(dialog), "radios", radios);
	g_object_set_data(G_OBJECT(dialog), "mime-type", item->mime_type);

#if 0
	radios_add(radios,
			_("Use a copy of the image as the default for all "
			  "files of these MIME types."), SET_MEDIA,
			_("Set icon for all `%s/<anything>'"),
			item->mime_type->media_type);
#endif
	
	radios_add(radios,
			_("Use a copy of the image for all files of this MIME "
			  "type."), SET_TYPE,
			_("For all files of type `%s' (%s/%s)"),
			mime_type_comment(item->mime_type),
			item->mime_type->media_type,
			item->mime_type->subtype);

	radios_add(radios,
			_("Add the file and image filenames to your "
			"personal list. The setting will be lost if the image "
			"or the file is moved."), SET_PATH,
			_("Only for the file `%s'"), path);

	radios_set_value(radios, SET_PATH);

	/* If it's a directory, offer to create a .DirIcon */
	if (mc_stat(path, &info) == 0 && S_ISDIR(info.st_mode))
	{
		radios_add(radios,
			_("Copy the image inside the directory, as "
			"a hidden file called '.DirIcon'. "
			"All users will then see the "
			"icon, and you can move the directory around safely. "
			"This is usually the best option if you can write to "
			"the directory."), SET_COPY,
			_("Copy image into directory"));
		if (access(path, W_OK) == 0)
			radios_set_value(radios, SET_COPY);
	}


	frame = drop_box_new(_("Drop an icon file here"));
	g_object_set_data(G_OBJECT(dialog), "rox-dropbox", frame);

	/* Make sure rox-dropbox is set before packing (calls changed) */
	radios_pack(radios, GTK_BOX(dialog->vbox));
	gtk_box_pack_start(GTK_BOX(dialog->vbox), frame, TRUE, TRUE, 4);

	g_signal_connect(frame, "path_dropped",
			G_CALLBACK(drag_icon_dropped), dialog);
	g_signal_connect(frame, "clear",
			G_CALLBACK(clear_icon), dialog);

	gtk_dialog_add_buttons(dialog,
			GTK_STOCK_CLOSE, GTK_RESPONSE_OK,
			NULL);
	g_signal_connect(dialog, "response", G_CALLBACK(dialog_response), NULL);
	gtk_dialog_set_default_response(dialog, GTK_RESPONSE_OK);

	gtk_widget_show_all(GTK_WIDGET(dialog));
}


/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* The dropbox shows the path for the currently selected radio setting.
 */
static void radios_changed(Radios *radios, gpointer data)
{
	GObject *dialog = G_OBJECT(data);
	DropBox *drop_box;
	const guchar *path;
	MIME_type *mime_type;

	path = g_object_get_data(dialog, "pathname");
	drop_box = g_object_get_data(dialog, "rox-dropbox");
	mime_type = g_object_get_data(dialog, "mime-type");

	g_return_if_fail(radios != NULL);
	g_return_if_fail(path != NULL);
	g_return_if_fail(drop_box != NULL);
	g_return_if_fail(mime_type != NULL);
	
	switch (radios_get_value(radios))
	{
		case SET_MEDIA:
		{
			char *path, *type;

			type = g_strconcat(mime_type->media_type, ".png", NULL);
			path = choices_find_xdg_path_load(type, "MIME-icons",
							  SITE);
			g_free(type);
			drop_box_set_path(drop_box, path);
			g_free(path);
			break;
		}
		case SET_TYPE:
		{
			char *path, *type;

			type = g_strconcat(mime_type->media_type, "_",
					   mime_type->subtype, ".png", NULL);
			path = choices_find_xdg_path_load(type, "MIME-icons",
							  SITE);
			g_free(type);
			drop_box_set_path(drop_box, path);
			g_free(path);
			break;
		}
		case SET_PATH:
		{
			const char	*gi;
			gi = g_hash_table_lookup(glob_icons, path);
			drop_box_set_path(drop_box, gi);
			break;
		}
		case SET_COPY:
		{
			const char *diricon;
			diricon = make_path(path, ".DirIcon");
			if (file_exists(diricon))
				drop_box_set_path(drop_box, diricon);
			else
				drop_box_set_path(drop_box, NULL);
			break;
		}
		default:
			drop_box_set_path(drop_box, NULL);
			break;
	}
}

static gboolean free_globicon(gpointer key, gpointer value, gpointer data)
{
	g_free(key);
	g_free(value);

	return TRUE;		/* For g_hash_table_foreach_remove() */
}

static void write_globicon(gpointer key, gpointer value, gpointer data)
{
	xmlNodePtr doc = (xmlNodePtr) data;
	xmlNodePtr tree;

	tree = xmlNewTextChild(doc, NULL, "rule", NULL);
	xmlSetProp(tree, "match", key);
	xmlNewTextChild(tree, NULL, "icon", value);
}

/* Write globicons file */
static void write_globicons(void)
{
	gchar *save = NULL, *save_new = NULL;
	xmlDocPtr doc = NULL;

	save = choices_find_xdg_path_save("globicons", PROJECT, SITE, TRUE);

	if (!save)
		return;		/* Saving is disabled */

	save_new = g_strconcat(save, ".new", NULL);

	doc = xmlNewDoc("1.0");
	xmlDocSetRootElement(doc,
		             xmlNewDocNode(doc, NULL, "special-files", NULL));

	g_hash_table_foreach(glob_icons, write_globicon,
			     xmlDocGetRootElement(doc));

	if (save_xml_file(doc, save_new) || rename(save_new, save))
		delayed_error(_("Error saving %s: %s"),
				save, g_strerror(errno));

	g_free(save_new);
	g_free(save);

	if (doc)
		xmlFreeDoc(doc);
}

/* Process a globicon line. Format:
   	glob-pattern	icon-path
   Example:
       /home/<*>/Mail   /usr/local/icons/mailbox.xpm
         (<*> represents a single asterisk, enclosed in brackets to not break
         the C comment).
*/
static const char *process_globicons_line(gchar *line)
{
	guchar *pattern, *iconpath;
	
	pattern = strtok(line, " \t");
	/* We ignore empty lines, but they are no cause for a message */
	if (pattern == NULL)
		return NULL;
	
	iconpath = strtok(NULL, " \t");
	
	/* If there is no icon, then we worry */
	g_return_val_if_fail(iconpath != NULL,
			"Invalid line in globicons: no icon specified");

	g_hash_table_insert(glob_icons, g_strdup(pattern), g_strdup(iconpath));

	return NULL;
}

/* Add a globicon entry to the list. If another one with the same
 * path exists, it is replaced. Otherwise, the new entry is
 * added to the top of the list (so that it takes precedence over
 * other entries).
 */
void add_globicon(const gchar *path, const gchar *icon)
{
	g_hash_table_insert(glob_icons, g_strdup(path), g_strdup(icon));

	/* Rewrite the globicons file */
	write_globicons();

	/* Make sure any visible icons for the file are updated */
	examine(path);
}

/* Remove the globicon for a certain path */
void delete_globicon(const gchar *path)
{
	gpointer key, value;

	if (!g_hash_table_lookup_extended(glob_icons, path, &key, &value))
		return;

	g_hash_table_remove(glob_icons, path);

	g_free(key);
	g_free(value);

	write_globicons();
	examine(path);
}

/* Set the icon for this dialog's file to 'icon' */
static void do_set_icon(GtkWidget *dialog, const gchar *icon)
{
	guchar  *path = NULL;
	Radios *radios;
	int op;

	path = g_object_get_data(G_OBJECT(dialog), "pathname");
	g_return_if_fail(path != NULL);

	radios = g_object_get_data(G_OBJECT(dialog), "radios");
	g_return_if_fail(radios != NULL);

	op = radios_get_value(radios);

	if (op == SET_PATH)
	{
		if (!set_icon_path(path, icon))
			return;
	}
	else if (op == SET_COPY)
	{
		if (!create_diricon(path, icon))
			return;
	}
	else
	{
		gboolean just_media = (op == SET_MEDIA);
		MIME_type *type;
		
		type = g_object_get_data(G_OBJECT(dialog), "mime-type");

		if (!set_icon_for_type(type, icon, just_media))
			return;
	}

	destroy_on_idle(dialog);
}

/* Called when a URI list is dropped onto the box in the Set Icon
 * dialog. Make that the default icon.
 */
static void drag_icon_dropped(GtkWidget	*drop_box,
			      const guchar *path,
			      GtkWidget	*dialog)
{
	do_set_icon(dialog, path);
}

/* Set the icon for the given MIME type.  We copy the file. */
static gboolean set_icon_for_type(MIME_type *type, const gchar *iconpath,
				  gboolean just_media)
{
	gchar *target;
	gchar *leaf;

	if (just_media)
		leaf = g_strconcat(type->media_type, ".png", NULL);
	else
		leaf = g_strconcat(type->media_type, "_", type->subtype,
								".png", NULL);

	target = choices_find_xdg_path_save(leaf, "MIME-icons", SITE, TRUE);
	g_free(leaf);

	if (!target)
	{
		delayed_error(_("Setting icon disabled by CHOICESPATH"));
		return FALSE;
	}

	if (!convert_to_png(iconpath, target))
	{
		g_free(target);
		return FALSE;
	}

	g_free(target);

	full_refresh();

	return TRUE;
}

/* Load image 'src', and save it as an icon-sized image in png format.
 * TRUE on success, error is already reported on failure.
 */
static gboolean convert_to_png(const gchar *src, const gchar *dest)
{
	MaskedPixmap *pic;
	GError	*error = NULL;

	pic = g_fscache_lookup(pixmap_cache, src);
	if (!pic)
	{
		delayed_error(
			_("Unable to load image file -- maybe it's not in a "
			  "format I understand, or maybe the permissions are "
			  "wrong?\n"
			  "The icon has not been changed."));
		return FALSE;
	}

	gdk_pixbuf_save(pic->src_pixbuf, dest,
			"png", &error,
			"tEXt::Software", PROJECT,
			NULL);
	g_object_unref(pic);

	if (error)
	{
		delayed_error(_("Error creating image '%s':\n%s"),
				dest, error->message);
		g_error_free(error);
		return FALSE;
	}

	return TRUE;
}
