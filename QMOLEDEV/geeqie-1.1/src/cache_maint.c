/*
 * Geeqie
 * (C) 2006 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include "main.h"
#include "cache_maint.h"

#include "cache.h"
#include "filedata.h"
#include "layout.h"
#include "thumb.h"
#include "thumb_standard.h"
#include "ui_fileops.h"
#include "ui_misc.h"
#include "ui_spinner.h"
#include "ui_tabcomp.h"
#include "ui_utildlg.h"


typedef struct _CMData CMData;
struct _CMData
{
	GList *list;
	GList *done_list;
	guint idle_id; /* event source id */
	GenericDialog *gd;
	GtkWidget *entry;
	GtkWidget *spinner;
	GtkWidget *button_stop;
	GtkWidget *button_close;
	gboolean clear;
	gboolean metadata;
};

#define PURGE_DIALOG_WIDTH 400


/*
 *-------------------------------------------------------------------
 * cache maintenance
 *-------------------------------------------------------------------
 */

#if 0
static gint extension_truncate(gchar *path, const gchar *ext)
{
	gint l;
	gint el;

	if (!path || !ext) return FALSE;

	l = strlen(path);
	el = strlen(ext);

	if (l < el || strcmp(path + (l - el), ext) != 0) return FALSE;

	path[l - el] = '\0';

	return TRUE;
}
#endif

static gchar *extension_find_dot(gchar *path)
{
	gchar *dot = NULL;

	if (!path) return NULL;

	while (*path != '\0')
		{
		if (*path == '.') dot = path;
		path++;
		}

	return dot;
}

static gboolean isempty(const gchar *path)
{
	DIR *dp;
	struct dirent *dir;
	gchar *pathl;

	pathl = path_from_utf8(path);
	dp = opendir(pathl);
	g_free(pathl);
	if (!dp) return FALSE;

	while ((dir = readdir(dp)) != NULL)
		{
		gchar *name = dir->d_name;

		if (!(name[0] == '.' && (name[1] == '\0' || (name[1] == '.' && name[2] == '\0'))) )
			{
			closedir(dp);
			return FALSE;
			}
		}

	closedir(dp);
	return TRUE;
}

static void cache_maintain_home_close(CMData *cm)
{
	if (cm->idle_id) g_source_remove(cm->idle_id);
	if (cm->gd) generic_dialog_close(cm->gd);
	filelist_free(cm->list);
	g_list_free(cm->done_list);
	g_free(cm);
}

static void cache_maintain_home_stop(CMData *cm)
{
	if (cm->idle_id)
		{
		g_source_remove(cm->idle_id);
		cm->idle_id = 0;
		}

	gtk_entry_set_text(GTK_ENTRY(cm->entry), _("done"));
	spinner_set_interval(cm->spinner, -1);

	gtk_widget_set_sensitive(cm->button_stop, FALSE);
	gtk_widget_set_sensitive(cm->button_close, TRUE);
}

static gboolean cache_maintain_home_cb(gpointer data)
{
	CMData *cm = data;
	GList *dlist = NULL;
	GList *list = NULL;
	FileData *fd;
	gboolean just_done = FALSE;
	gboolean still_have_a_file = TRUE;
	gsize base_length;
	const gchar *cache_folder;

	if (cm->metadata)
		{
		cache_folder = get_metadata_cache_dir();
		}
	else
		{
		cache_folder = get_thumbnails_cache_dir();
		}

	base_length = strlen(cache_folder);

	if (!cm->list)
		{
		DEBUG_1("purge chk done.");
		cm->idle_id = 0;
		cache_maintain_home_stop(cm);
		return FALSE;
		}

	fd = cm->list->data;

	DEBUG_1("purge chk (%d) \"%s\"", (cm->clear && !cm->metadata), fd->path);

	if (g_list_find(cm->done_list, fd) == NULL)
		{
		cm->done_list = g_list_prepend(cm->done_list, fd);

		if (filelist_read(fd, &list, &dlist))
			{
			GList *work;

			just_done = TRUE;
			still_have_a_file = FALSE;

			work = list;
			while (work)
				{
				FileData *fd_list = work->data;
				gchar *path_buf = g_strdup(fd_list->path);
				gchar *dot;

				dot = extension_find_dot(path_buf);

				if (dot) *dot = '\0';
				if ((!cm->metadata && cm->clear) ||
				    (strlen(path_buf) > base_length && !isfile(path_buf + base_length)) )
					{
					if (dot) *dot = '.';
					if (!unlink_file(path_buf)) log_printf("failed to delete:%s\n", path_buf);
					}
				else
					{
					still_have_a_file = TRUE;
					}
				g_free(path_buf);
				work = work->next;
				}
			}
		}
	filelist_free(list);

	cm->list = g_list_concat(dlist, cm->list);

	if (cm->list && g_list_find(cm->done_list, cm->list->data) != NULL)
		{
		/* check if the dir is empty */

		if (cm->list->data == fd && just_done)
			{
			if (!still_have_a_file && !dlist && cm->list->next && !rmdir_utf8(fd->path))
				{
				log_printf("Unable to delete dir: %s\n", fd->path);
				}
			}
		else
			{
			/* must re-check for an empty dir */
			if (isempty(fd->path) && cm->list->next && !rmdir_utf8(fd->path))
				{
				log_printf("Unable to delete dir: %s\n", fd->path);
				}
			}

		fd = cm->list->data;
		cm->done_list = g_list_remove(cm->done_list, fd);
		cm->list = g_list_remove(cm->list, fd);
		file_data_unref(fd);
		}

	if (cm->list)
		{
		const gchar *buf;

		fd = cm->list->data;
		if (strlen(fd->path) > base_length)
			{
			buf = fd->path + base_length;
			}
		else
			{
			buf = "...";
			}
		gtk_entry_set_text(GTK_ENTRY(cm->entry), buf);
		}

	return TRUE;
}

static void cache_maintain_home_close_cb(GenericDialog *gd, gpointer data)
{
	CMData *cm = data;

#if GTK_CHECK_VERSION(2,20,0)
	if (!gtk_widget_get_sensitive(cm->button_close)) return;
#else
	if (!GTK_WIDGET_SENSITIVE(cm->button_close)) return;
#endif

	cache_maintain_home_close(cm);
}

static void cache_maintain_home_stop_cb(GenericDialog *gd, gpointer data)
{
	CMData *cm = data;

	cache_maintain_home_stop(cm);
}

/* sorry for complexity (cm->done_list), but need it to remove empty dirs */
void cache_maintain_home(gboolean metadata, gboolean clear, GtkWidget *parent)
{
	CMData *cm;
	GList *dlist;
	FileData *dir_fd;
	const gchar *msg;
	const gchar *cache_folder;
	GtkWidget *hbox;

	if (metadata)
		{
		cache_folder = get_metadata_cache_dir();
		}
	else
		{
		cache_folder = get_thumbnails_cache_dir();
		}

	dir_fd = file_data_new_dir(cache_folder);
	if (!filelist_read(dir_fd, NULL, &dlist))
		{
		file_data_unref(dir_fd);
		return;
		}

	dlist = g_list_append(dlist, dir_fd);

	cm = g_new0(CMData, 1);
	cm->list = dlist;
	cm->done_list = NULL;
	cm->clear = clear;
	cm->metadata = metadata;

	if (metadata)
		{
		msg = _("Removing old metadata...");
		}
	else if (clear)
		{
		msg = _("Clearing cached thumbnails...");
		}
	else
		{
		msg = _("Removing old thumbnails...");
		}

	cm->gd = generic_dialog_new(_("Maintenance"),
				    "main_maintenance",
				    parent, FALSE,
				    NULL, cm);
	cm->gd->cancel_cb = cache_maintain_home_close_cb;
	cm->button_close = generic_dialog_add_button(cm->gd, GTK_STOCK_CLOSE, NULL,
						     cache_maintain_home_close_cb, FALSE);
	gtk_widget_set_sensitive(cm->button_close, FALSE);
	cm->button_stop = generic_dialog_add_button(cm->gd, GTK_STOCK_STOP, NULL,
						    cache_maintain_home_stop_cb, FALSE);

	generic_dialog_add_message(cm->gd, NULL, msg, NULL);
	gtk_window_set_default_size(GTK_WINDOW(cm->gd->dialog), PURGE_DIALOG_WIDTH, -1);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(cm->gd->vbox), hbox, FALSE, FALSE, 5);
	gtk_widget_show(hbox);

	cm->entry = gtk_entry_new();
	GTK_WIDGET_UNSET_FLAGS(cm->entry, GTK_CAN_FOCUS);
	gtk_editable_set_editable(GTK_EDITABLE(cm->entry), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), cm->entry, TRUE, TRUE, 0);
	gtk_widget_show(cm->entry);

	cm->spinner = spinner_new(NULL, SPINNER_SPEED);
	gtk_box_pack_start(GTK_BOX(hbox), cm->spinner, FALSE, FALSE, 0);
	gtk_widget_show(cm->spinner);

	gtk_widget_show(cm->gd->dialog);

	cm->idle_id = g_idle_add(cache_maintain_home_cb, cm);
}

#if 0
/* This checks all files in ~/GQ_RC_DIR/thumbnails and
 * removes them if thay have no source counterpart.
 * (this assumes all cache files have an extension of 4 chars including '.')
 */
gint cache_maintain_home_dir(const gchar *dir, gint recursive, gint clear)
{
	gchar *base;
	gint base_length;
	GList *dlist = NULL;
	FileData *dir_fd;
	GList *flist = NULL;
	gboolean still_have_a_file = FALSE;

	DEBUG_1("maintainance check: %s", dir);

	base_length = strlen(homedir()) + strlen("/") + strlen(GQ_CACHE_RC_THUMB);
	base = g_strconcat(homedir(), "/", GQ_CACHE_RC_THUMB, dir, NULL);
	dir_fd = file_data_new_dir(base);
	g_free(base);

	if (filelist_read(dir_fd, &flist, &dlist))
		{
		GList *work;

		work = dlist;
		while (work)
			{
			FileData *fd = work->data;
			if (recursive && strlen(fd->path) > base_length &&
			    !cache_maintain_home_dir(fd->path + base_length, recursive, clear))
				{
				DEBUG_1("Deleting thumb dir: %s", fd->path);
				if (!rmdir_utf8(fd->path))
					{
					log_printf("Unable to delete dir: %s\n", fd->path);
					}
				}
			else
				{
				still_have_a_file = TRUE;
				}
			work = work->next;
			}

		work = flist;
		while (work)
			{
			FileData *fd = work->data;
			gchar *path = g_strdup(fd->path);
			gchar *dot;

			dot = extension_find_dot(path);

			if (dot) *dot = '\0';
			if (clear ||
			    (strlen(path) > base_length && !isfile(path + base_length)) )
				{
				if (dot) *dot = '.';
				if (!unlink_file(path)) log_printf("failed to delete:%s\n", path);
				}
			else
				{
				still_have_a_file = TRUE;
				}
			g_free(path);

			work = work->next;
			}
		}

	filelist_free(dlist);
	filelist_free(flist);
	file_data_unref(dir_fd);

	return still_have_a_file;
}

/* This checks relative caches in dir/.thumbnails and
 * removes them if they have no source counterpart.
 */
gint cache_maintain_dir(FileData *dir_fd, gint recursive, gint clear)
{
	GList *list = NULL;
	gchar *cachedir;
	FileData *cachedir_fd;
	gboolean still_have_a_file = FALSE;
	GList *work;

	cachedir = g_build_filename(dir, GQ_CACHE_LOCAL_THUMB, NULL);
	cachedir_fd = file_data_new_dir(cachedir);
	g_free(cachedir);

	filelist_read(cachedir_fd, &list, NULL);
	work = list;

	while (work)
		{
		FileData *fd;
		gchar *source;

		fd = work->data;
		work = work->next;

		source = g_build_filename(dir->path, fd->name, NULL);

		if (clear ||
		    extension_truncate(source, GQ_CACHE_EXT_THUMB) ||
		    extension_truncate(source, GQ_CACHE_EXT_SIM))
			{
			if (!clear && isfile(source))
				{
				still_have_a_file = TRUE;
				}
			else
				{
				if (!unlink_file(fd->path))
					{
					DEBUG_1("Failed to remove cache file %s", fd->path);
					still_have_a_file = TRUE;
					}
				}
			}
		else
			{
			still_have_a_file = TRUE;
			}
		g_free(source);
		}

	filelist_free(list);
	file_data_unref(cachedir_fd);

	if (recursive)
		{
		list = NULL;

		filelist_read(dir_fd, NULL, &list);
		work = list;
		while (work)
			{
			FileData *fd = work->data;
			work = work->next;

			still_have_a_file |= cache_maintain_dir(fd->path, recursive, clear);
			}

		filelist_free(list);
		}

	return still_have_a_file;
}
#endif

static void cache_file_move(const gchar *src, const gchar *dest)
{
	if (!dest || !src || !isfile(src)) return;

	if (!move_file(src, dest))
		{
		DEBUG_1("Failed to move cache file \"%s\" to \"%s\"", src, dest);
		/* we remove it anyway - it's stale */
		unlink_file(src);
		}
}

static void cache_maint_moved(FileData *fd)
{
	gchar *base;
	mode_t mode = 0755;
	const gchar *src = fd->change->source;
	const gchar *dest = fd->change->dest;

	if (!src || !dest) return;

	base = cache_get_location(CACHE_TYPE_THUMB, dest, FALSE, &mode);
	if (recursive_mkdir_if_not_exists(base, mode))
		{
		gchar *buf;
		gchar *d;

		buf = cache_find_location(CACHE_TYPE_THUMB, src);
		d = cache_get_location(CACHE_TYPE_THUMB, dest, TRUE, NULL);
		cache_file_move(buf, d);
		g_free(d);
		g_free(buf);

		buf = cache_find_location(CACHE_TYPE_SIM, src);
		d = cache_get_location(CACHE_TYPE_SIM, dest, TRUE, NULL);
		cache_file_move(buf, d);
		g_free(d);
		g_free(buf);
		}
	else
		{
		log_printf("Failed to create cache dir for move %s\n", base);
		}
	g_free(base);

	base = cache_get_location(CACHE_TYPE_METADATA, dest, FALSE, &mode);
	if (recursive_mkdir_if_not_exists(base, mode))
		{
		gchar *buf;
		gchar *d;

		buf = cache_find_location(CACHE_TYPE_METADATA, src);
		d = cache_get_location(CACHE_TYPE_METADATA, dest, TRUE, NULL);
		cache_file_move(buf, d);
		g_free(d);
		g_free(buf);
		}
	g_free(base);

	if (options->thumbnails.enable_caching && options->thumbnails.spec_standard)
		thumb_std_maint_moved(src, dest);
}

static void cache_file_remove(const gchar *path)
{
	if (path && isfile(path) && !unlink_file(path))
		{
		DEBUG_1("Failed to remove cache file %s", path);
		}
}

static void cache_maint_removed(FileData *fd)
{
	gchar *buf;

	buf = cache_find_location(CACHE_TYPE_THUMB, fd->path);
	cache_file_remove(buf);
	g_free(buf);

	buf = cache_find_location(CACHE_TYPE_SIM, fd->path);
	cache_file_remove(buf);
	g_free(buf);

	buf = cache_find_location(CACHE_TYPE_METADATA, fd->path);
	cache_file_remove(buf);
	g_free(buf);

	if (options->thumbnails.enable_caching && options->thumbnails.spec_standard)
		thumb_std_maint_removed(fd->path);
}

static void cache_maint_copied(FileData *fd)
{
	gchar *dest_base;
	gchar *src_cache;
	mode_t mode = 0755;

	src_cache = cache_find_location(CACHE_TYPE_METADATA, fd->change->source);
	if (!src_cache) return;

	dest_base = cache_get_location(CACHE_TYPE_METADATA, fd->change->dest, FALSE, &mode);
	if (recursive_mkdir_if_not_exists(dest_base, mode))
		{
		gchar *path;

		path = cache_get_location(CACHE_TYPE_METADATA, fd->change->dest, TRUE, NULL);
		if (!copy_file(src_cache, path))
			{
			DEBUG_1("failed to copy metadata %s to %s", src_cache, path);
			}
		g_free(path);
		}

	g_free(dest_base);
	g_free(src_cache);
}

void cache_notify_cb(FileData *fd, NotifyType type, gpointer data)
{
	if (!(type & NOTIFY_CHANGE) || !fd->change) return;
	
	DEBUG_1("Notify cache_maint: %s %04x", fd->path, type);
	switch (fd->change->type)
		{
		case FILEDATA_CHANGE_MOVE:
		case FILEDATA_CHANGE_RENAME:
			cache_maint_moved(fd);
			break;
		case FILEDATA_CHANGE_COPY:
			cache_maint_copied(fd);
			break;
		case FILEDATA_CHANGE_DELETE:
			cache_maint_removed(fd);
			break;
		case FILEDATA_CHANGE_UNSPECIFIED:
		case FILEDATA_CHANGE_WRITE_METADATA:
			break;
		}
}


/*
 *-------------------------------------------------------------------
 * new cache maintenance utilities
 *-------------------------------------------------------------------
 */

typedef struct _CacheManager CacheManager;
struct _CacheManager
{
	GenericDialog *dialog;
	GtkWidget *folder_entry;
	GtkWidget *progress;

	GList *list_todo;

	gint count_total;
	gint count_done;
};

typedef struct _CleanData CleanData;
struct _CleanData
{
	GenericDialog *gd;
	ThumbLoaderStd *tl;

	GList *list;
	GList *list_dir;

	gint days;
	gboolean clear;

	GtkWidget *button_close;
	GtkWidget *button_stop;
	GtkWidget *button_start;
	GtkWidget *progress;
	GtkWidget *spinner;

	GtkWidget *group;
	GtkWidget *entry;

	gint count_total;
	gint count_done;

	gboolean local;
	gboolean recurse;

	guint idle_id; /* event source id */
};

static void cache_manager_render_reset(CleanData *cd)
{
	filelist_free(cd->list);
	cd->list = NULL;

	filelist_free(cd->list_dir);
	cd->list_dir = NULL;

	thumb_loader_free((ThumbLoader *)cd->tl);
	cd->tl = NULL;
}

static void cache_manager_render_close_cb(GenericDialog *fd, gpointer data)
{
	CleanData *cd = data;

#if GTK_CHECK_VERSION(2,20,0)
	if (!gtk_widget_get_sensitive(cd->button_close)) return;
#else
	if (!GTK_WIDGET_SENSITIVE(cd->button_close)) return;
#endif

	cache_manager_render_reset(cd);
	generic_dialog_close(cd->gd);
	g_free(cd);
}

static void cache_manager_render_finish(CleanData *cd)
{
	cache_manager_render_reset(cd);

	gtk_entry_set_text(GTK_ENTRY(cd->progress), _("done"));
	spinner_set_interval(cd->spinner, -1);

	gtk_widget_set_sensitive(cd->group, TRUE);
	gtk_widget_set_sensitive(cd->button_start, TRUE);
	gtk_widget_set_sensitive(cd->button_stop, FALSE);
	gtk_widget_set_sensitive(cd->button_close, TRUE);
}

static void cache_manager_render_stop_cb(GenericDialog *fd, gpointer data)
{
	CleanData *cd = data;

	cache_manager_render_finish(cd);
}

static void cache_manager_render_folder(CleanData *cd, FileData *dir_fd)
{
	GList *list_d = NULL;
	GList *list_f = NULL;

	if (cd->recurse)
		{
		filelist_read(dir_fd, &list_f, &list_d);
		}
	else
		{
		filelist_read(dir_fd, &list_f, NULL);
		}

	list_f = filelist_filter(list_f, FALSE);
	list_d = filelist_filter(list_d, TRUE);

	cd->list = g_list_concat(list_f, cd->list);
	cd->list_dir = g_list_concat(list_d, cd->list_dir);
}

static gboolean cache_manager_render_file(CleanData *cd);

static void cache_manager_render_thumb_done_cb(ThumbLoader *tl, gpointer data)
{
	CleanData *cd = data;

	thumb_loader_free((ThumbLoader *)cd->tl);
	cd->tl = NULL;

	while (cache_manager_render_file(cd));
}

static gboolean cache_manager_render_file(CleanData *cd)
{
	if (cd->list)
		{
		FileData *fd;
		gint success;

		fd = cd->list->data;
		cd->list = g_list_remove(cd->list, fd);

		cd->tl = (ThumbLoaderStd *)thumb_loader_new(options->thumbnails.max_width, options->thumbnails.max_height);
		thumb_loader_set_callbacks((ThumbLoader *)cd->tl,
					   cache_manager_render_thumb_done_cb,
					   cache_manager_render_thumb_done_cb,
					   NULL, cd);
		thumb_loader_set_cache((ThumbLoader *)cd->tl, TRUE, cd->local, TRUE);
		success = thumb_loader_start((ThumbLoader *)cd->tl, fd);
		if (success)
			{
			gtk_entry_set_text(GTK_ENTRY(cd->progress), fd->path);
			}
		else
			{
			thumb_loader_free((ThumbLoader *)cd->tl);
			cd->tl = NULL;
			}

		file_data_unref(fd);

		return (!success);
		}
	else if (cd->list_dir)
		{
		FileData *fd;

		fd = cd->list_dir->data;
		cd->list_dir = g_list_remove(cd->list_dir, fd);

		cache_manager_render_folder(cd, fd);

		file_data_unref(fd);

		return TRUE;
		}

	cache_manager_render_finish(cd);

	return FALSE;
}

static void cache_manager_render_start_cb(GenericDialog *fd, gpointer data)
{
	CleanData *cd = data;
	gchar *path;

#if GTK_CHECK_VERSION(2,20,0)
	if (cd->list || !gtk_widget_get_sensitive(cd->button_start)) return;
#else
	if (cd->list || !GTK_WIDGET_SENSITIVE(cd->button_start)) return;
#endif

	path = remove_trailing_slash((gtk_entry_get_text(GTK_ENTRY(cd->entry))));
	parse_out_relatives(path);

	if (!isdir(path))
		{
		warning_dialog(_("Invalid folder"),
				_("The specified folder can not be found."),
			       GTK_STOCK_DIALOG_WARNING, cd->gd->dialog);
		}
	else
		{
		FileData *dir_fd;
		gtk_widget_set_sensitive(cd->group, FALSE);
		gtk_widget_set_sensitive(cd->button_start, FALSE);
		gtk_widget_set_sensitive(cd->button_stop, TRUE);
		gtk_widget_set_sensitive(cd->button_close, FALSE);

		spinner_set_interval(cd->spinner, SPINNER_SPEED);

		dir_fd = file_data_new_dir(path);
		cache_manager_render_folder(cd, dir_fd);
		file_data_unref(dir_fd);
		while (cache_manager_render_file(cd));
		}

	g_free(path);
}

static void cache_manager_render_dialog(GtkWidget *widget, const gchar *path)
{
	CleanData *cd;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *button;

	cd = g_new0(CleanData, 1);

	cd->gd = generic_dialog_new(_("Create thumbnails"),
				    "create_thumbnails",
				    widget, FALSE,
				    NULL, cd);
	gtk_window_set_default_size(GTK_WINDOW(cd->gd->dialog), PURGE_DIALOG_WIDTH, -1);
	cd->gd->cancel_cb = cache_manager_render_close_cb;
	cd->button_close = generic_dialog_add_button(cd->gd, GTK_STOCK_CLOSE, NULL,
						     cache_manager_render_close_cb, FALSE);
	cd->button_start = generic_dialog_add_button(cd->gd, GTK_STOCK_OK, _("S_tart"),
						     cache_manager_render_start_cb, FALSE);
	cd->button_stop = generic_dialog_add_button(cd->gd, GTK_STOCK_STOP, NULL,
						    cache_manager_render_stop_cb, FALSE);
	gtk_widget_set_sensitive(cd->button_stop, FALSE);

	generic_dialog_add_message(cd->gd, NULL, _("Create thumbnails"), NULL);

	hbox = pref_box_new(cd->gd->vbox, FALSE, GTK_ORIENTATION_HORIZONTAL, 0);
	pref_spacer(hbox, PREF_PAD_INDENT);
	cd->group = pref_box_new(hbox, TRUE, GTK_ORIENTATION_VERTICAL, PREF_PAD_GAP);

	hbox = pref_box_new(cd->group, FALSE, GTK_ORIENTATION_HORIZONTAL, PREF_PAD_SPACE);
	pref_label_new(hbox, _("Folder:"));

	label = tab_completion_new(&cd->entry, path, NULL, NULL);
	tab_completion_add_select_button(cd->entry,_("Select folder") , TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);
	gtk_widget_show(label);

	pref_checkbox_new_int(cd->group, _("Include subfolders"), FALSE, &cd->recurse);
	button = pref_checkbox_new_int(cd->group, _("Store thumbnails local to source images"), FALSE, &cd->local);
	gtk_widget_set_sensitive(button, options->thumbnails.spec_standard);

	pref_line(cd->gd->vbox, PREF_PAD_SPACE);
	hbox = pref_box_new(cd->gd->vbox, FALSE, GTK_ORIENTATION_HORIZONTAL, PREF_PAD_SPACE);

	cd->progress = gtk_entry_new();
	GTK_WIDGET_UNSET_FLAGS(cd->progress, GTK_CAN_FOCUS);
	gtk_editable_set_editable(GTK_EDITABLE(cd->progress), FALSE);
	gtk_entry_set_text(GTK_ENTRY(cd->progress), _("click start to begin"));
	gtk_box_pack_start(GTK_BOX(hbox), cd->progress, TRUE, TRUE, 0);
	gtk_widget_show(cd->progress);

	cd->spinner = spinner_new(NULL, -1);
	gtk_box_pack_start(GTK_BOX(hbox), cd->spinner, FALSE, FALSE, 0);
	gtk_widget_show(cd->spinner);

	cd->list = NULL;

	gtk_widget_show(cd->gd->dialog);
}




static void cache_manager_standard_clean_close_cb(GenericDialog *gd, gpointer data)
{
	CleanData *cd = data;

#if GTK_CHECK_VERSION(2,20,0)
	if (!gtk_widget_get_sensitive(cd->button_close)) return;
#else
	if (!GTK_WIDGET_SENSITIVE(cd->button_close)) return;
#endif

	generic_dialog_close(cd->gd);

	thumb_loader_std_thumb_file_validate_cancel(cd->tl);
	filelist_free(cd->list);
	g_free(cd);
}

static void cache_manager_standard_clean_done(CleanData *cd)
{
	gtk_widget_set_sensitive(cd->button_stop, FALSE);
	gtk_widget_set_sensitive(cd->button_close, TRUE);

	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(cd->progress), 1.0);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(cd->progress), _("done"));

	if (cd->idle_id)
		{
		g_source_remove(cd->idle_id);
		cd->idle_id = 0;
		}

	thumb_loader_std_thumb_file_validate_cancel(cd->tl);
	cd->tl = NULL;

	filelist_free(cd->list);
	cd->list = NULL;
}

static void cache_manager_standard_clean_stop_cb(GenericDialog *gd, gpointer data)
{
	CleanData *cd = data;

	cache_manager_standard_clean_done(cd);
}

static gint cache_manager_standard_clean_clear_cb(gpointer data)
{
	CleanData *cd = data;

	if (cd->list)
		{
		FileData *next_fd;

		next_fd = cd->list->data;
		cd->list = g_list_remove(cd->list, next_fd);

		DEBUG_1("thumb removed: %s", next_fd->path);

		unlink_file(next_fd->path);
		file_data_unref(next_fd);

		cd->count_done++;
		if (cd->count_total != 0)
			{
			gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(cd->progress),
						      (gdouble)cd->count_done / cd->count_total);
			}

		return TRUE;
		}

	cd->idle_id = 0;
	cache_manager_standard_clean_done(cd);
	return FALSE;
}

static void cache_manager_standard_clean_valid_cb(const gchar *path, gboolean valid, gpointer data)
{
	CleanData *cd = data;

	if (path)
		{
		if (!valid)
			{
			DEBUG_1("thumb cleaned: %s", path);
			unlink_file(path);
			}

		cd->count_done++;
		if (cd->count_total != 0)
			{
			gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(cd->progress),
						      (gdouble)cd->count_done / cd->count_total);
			}
		}

	cd->tl = NULL;
	if (cd->list)
		{
		FileData *next_fd;

		next_fd = cd->list->data;
		cd->list = g_list_remove(cd->list, next_fd);

		cd->tl = thumb_loader_std_thumb_file_validate(next_fd->path, cd->days,
							      cache_manager_standard_clean_valid_cb, cd);
		file_data_unref(next_fd);
		}
	else
		{
		cache_manager_standard_clean_done(cd);
		}
}

static void cache_manager_standard_clean_start_cb(GenericDialog *gd, gpointer data)
{
	CleanData *cd = data;
	GList *list;
	gchar *path;
	FileData *dir_fd;

#if GTK_CHECK_VERSION(2,20,0)
	if (cd->list || !gtk_widget_get_sensitive(cd->button_start)) return;
#else
	if (cd->list || !GTK_WIDGET_SENSITIVE(cd->button_start)) return;
#endif

	gtk_widget_set_sensitive(cd->button_start, FALSE);
	gtk_widget_set_sensitive(cd->button_stop, TRUE);
	gtk_widget_set_sensitive(cd->button_close, FALSE);

	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(cd->progress), _("running..."));

	path = g_build_filename(homedir(), THUMB_FOLDER_GLOBAL, THUMB_FOLDER_NORMAL, NULL);
	dir_fd = file_data_new_dir(path);
	filelist_read(dir_fd, &list, NULL);
	cd->list = list;
	file_data_unref(dir_fd);
	g_free(path);

	path = g_build_filename(homedir(), THUMB_FOLDER_GLOBAL, THUMB_FOLDER_LARGE, NULL);
	dir_fd = file_data_new_dir(path);
	filelist_read(dir_fd, &list, NULL);
	cd->list = g_list_concat(cd->list, list);
	file_data_unref(dir_fd);
	g_free(path);

	path = g_build_filename(homedir(), THUMB_FOLDER_GLOBAL, THUMB_FOLDER_FAIL, NULL);
	dir_fd = file_data_new_dir(path);
	filelist_read(dir_fd, &list, NULL);
	cd->list = g_list_concat(cd->list, list);
	file_data_unref(dir_fd);
	g_free(path);

	cd->count_total = g_list_length(cd->list);
	cd->count_done = 0;

	/* start iterating */
	if (cd->clear)
		{
		cd->idle_id = g_idle_add(cache_manager_standard_clean_clear_cb, cd);
		}
	else
		{
		cache_manager_standard_clean_valid_cb(NULL, TRUE, cd);
		}
}

static void cache_manager_standard_process(GtkWidget *widget, gboolean clear)
{
	CleanData *cd;
	const gchar *stock_id;
	const gchar *msg;

	cd = g_new0(CleanData, 1);
	cd->clear = clear;

	if (clear)
		{
		stock_id = GTK_STOCK_DELETE;
		msg = _("Clearing thumbnails...");
		}
	else
		{
		stock_id = GTK_STOCK_CLEAR;
		msg = _("Removing old thumbnails...");
		}

	cd->gd = generic_dialog_new(_("Maintenance"),
				    "standard_maintenance",
				    widget, FALSE,
				    NULL, cd);
	cd->gd->cancel_cb = cache_manager_standard_clean_close_cb;
	cd->button_close = generic_dialog_add_button(cd->gd, GTK_STOCK_CLOSE, NULL,
						     cache_manager_standard_clean_close_cb, FALSE);
	cd->button_start = generic_dialog_add_button(cd->gd, GTK_STOCK_OK, _("S_tart"),
						     cache_manager_standard_clean_start_cb, FALSE);
	cd->button_stop = generic_dialog_add_button(cd->gd, GTK_STOCK_STOP, NULL,
						    cache_manager_standard_clean_stop_cb, FALSE);
	gtk_widget_set_sensitive(cd->button_stop, FALSE);

	generic_dialog_add_message(cd->gd, stock_id, msg, NULL);

	cd->progress = gtk_progress_bar_new();
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(cd->progress), _("click start to begin"));
	gtk_box_pack_start(GTK_BOX(cd->gd->vbox), cd->progress, FALSE, FALSE, 0);
	gtk_widget_show(cd->progress);

	cd->days = 30;
	cd->tl = NULL;
	cd->idle_id = 0;

	gtk_widget_show(cd->gd->dialog);
}

static void cache_manager_standard_clean_cb(GtkWidget *widget, gpointer data)
{
	cache_manager_standard_process(widget, FALSE);
}

static void cache_manager_standard_clear_cb(GtkWidget *widget, gpointer data)
{
	cache_manager_standard_process(widget, TRUE);
}


static void cache_manager_main_clean_cb(GtkWidget *widget, gpointer data)
{
	cache_maintain_home(FALSE, FALSE, widget);
}


static void dummy_cancel_cb(GenericDialog *gd, gpointer data)
{
	/* no op, only so cancel button appears */
}

static void cache_manager_main_clear_ok_cb(GenericDialog *gd, gpointer data)
{
	cache_maintain_home(FALSE, TRUE, NULL);
}

void cache_manager_main_clear_confirm(GtkWidget *parent)
{
	GenericDialog *gd;

	gd = generic_dialog_new(_("Clear cache"),
				"clear_cache", parent, TRUE,
				dummy_cancel_cb, NULL);
	generic_dialog_add_message(gd, GTK_STOCK_DIALOG_QUESTION, _("Clear cache"),
				   _("This will remove all thumbnails that have\nbeen saved to disk, continue?"));
	generic_dialog_add_button(gd, GTK_STOCK_OK, NULL, cache_manager_main_clear_ok_cb, TRUE);

	gtk_widget_show(gd->dialog);
}

static void cache_manager_main_clear_cb(GtkWidget *widget, gpointer data)
{
	cache_manager_main_clear_confirm(widget);
}

static void cache_manager_render_cb(GtkWidget *widget, gpointer data)
{
	const gchar *path = layout_get_path(NULL);

	if (!path || !*path) path = homedir();
	cache_manager_render_dialog(widget, path);
}

static void cache_manager_metadata_clean_cb(GtkWidget *widget, gpointer data)
{
	cache_maintain_home(TRUE, FALSE, widget);
}


static CacheManager *cache_manager = NULL;

static void cache_manager_close_cb(GenericDialog *gd, gpointer data)
{
	generic_dialog_close(gd);

	g_free(cache_manager);
	cache_manager = NULL;
}

static GtkWidget *cache_manager_location_label(GtkWidget *group, const gchar *subdir)
{
	GtkWidget *label;
	gchar *buf;
	gchar *path;

	path = g_build_filename(homedir(), subdir, NULL);
	buf = g_strdup_printf(_("Location: %s"), path);
	g_free(path);
	label = pref_label_new(group, buf);
	g_free(buf);
	gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	
	return label;
}

void cache_manager_show(void)
{
	GenericDialog *gd;
	GtkWidget *group;
	GtkWidget *button;
	GtkWidget *table;
	GtkSizeGroup *sizegroup;

	if (cache_manager)
		{
		gtk_window_present(GTK_WINDOW(cache_manager->dialog->dialog));
		return;
		}

	cache_manager = g_new0(CacheManager, 1);

	cache_manager->dialog = generic_dialog_new(_("Cache Maintenance"),
						   "cache_manager",
						   NULL, FALSE,
						   NULL, cache_manager);
	gd = cache_manager->dialog;

	gd->cancel_cb = cache_manager_close_cb;
	generic_dialog_add_button(gd, GTK_STOCK_CLOSE, NULL,
				  cache_manager_close_cb, FALSE);

	generic_dialog_add_message(gd, NULL, _("Cache and Data Maintenance"), NULL);

	sizegroup = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

	group = pref_group_new(gd->vbox, FALSE, _("Thumbnail cache"), GTK_ORIENTATION_VERTICAL);

	cache_manager_location_label(group, get_thumbnails_cache_dir());

	table = pref_table_new(group, 2, 2, FALSE, FALSE);

	button = pref_table_button(table, 0, 0, GTK_STOCK_CLEAR, _("Clean up"), FALSE,
				   G_CALLBACK(cache_manager_main_clean_cb), cache_manager);
	gtk_size_group_add_widget(sizegroup, button);
	pref_table_label(table, 1, 0, _("Remove orphaned or outdated thumbnails."), 0.0);

	button = pref_table_button(table, 0, 1, GTK_STOCK_DELETE, _("Clear cache"), FALSE,
				   G_CALLBACK(cache_manager_main_clear_cb), cache_manager);
	gtk_size_group_add_widget(sizegroup, button);
	pref_table_label(table, 1, 1, _("Delete all cached thumbnails."), 0.0);


	group = pref_group_new(gd->vbox, FALSE, _("Shared thumbnail cache"), GTK_ORIENTATION_VERTICAL);

	cache_manager_location_label(group, THUMB_FOLDER_GLOBAL);

	table = pref_table_new(group, 2, 2, FALSE, FALSE);

	button = pref_table_button(table, 0, 0, GTK_STOCK_CLEAR, _("Clean up"), FALSE,
				   G_CALLBACK(cache_manager_standard_clean_cb), cache_manager);
	gtk_size_group_add_widget(sizegroup, button);
	pref_table_label(table, 1, 0, _("Remove orphaned or outdated thumbnails."), 0.0);

	button = pref_table_button(table, 0, 1, GTK_STOCK_DELETE, _("Clear cache"), FALSE,
				   G_CALLBACK(cache_manager_standard_clear_cb), cache_manager);
	gtk_size_group_add_widget(sizegroup, button);
	pref_table_label(table, 1, 1, _("Delete all cached thumbnails."), 0.0);

	group = pref_group_new(gd->vbox, FALSE, _("Create thumbnails"), GTK_ORIENTATION_VERTICAL);

	table = pref_table_new(group, 2, 1, FALSE, FALSE);

	button = pref_table_button(table, 0, 1, GTK_STOCK_EXECUTE, _("Render"), FALSE,
				   G_CALLBACK(cache_manager_render_cb), cache_manager);
	gtk_size_group_add_widget(sizegroup, button);
	pref_table_label(table, 1, 1, _("Render thumbnails for a specific folder."), 0.0);

	group = pref_group_new(gd->vbox, FALSE, _("Metadata"), GTK_ORIENTATION_VERTICAL);

	cache_manager_location_label(group, get_metadata_cache_dir());

	table = pref_table_new(group, 2, 1, FALSE, FALSE);

	button = pref_table_button(table, 0, 0, GTK_STOCK_CLEAR, _("Clean up"), FALSE,
				   G_CALLBACK(cache_manager_metadata_clean_cb), cache_manager);
	gtk_size_group_add_widget(sizegroup, button);
	pref_table_label(table, 1, 0, _("Remove orphaned keywords and comments."), 0.0);

	gtk_widget_show(cache_manager->dialog->dialog);
}
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
