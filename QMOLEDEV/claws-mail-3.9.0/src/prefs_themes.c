/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 Hiroyuki Yamamoto & the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "defs.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "utils.h"
#include "codeconv.h"
#include "prefs_common.h"
#include "prefs_gtk.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"
#include "gtk/filesel.h"

#include "stock_pixmap.h"
#include "mainwindow.h"
#include "compose.h"
#include "alertpanel.h"

#define IS_CURRENT_THEME(path)  (strcmp(prefs_common.pixmap_theme_path, path) == 0)
#define IS_INTERNAL_THEME(path) (strcmp(DEFAULT_PIXMAP_THEME, path) == 0)
#define IS_SYSTEM_THEME(path)   (prefs_themes_is_system_theme(path))

#define PREVIEW_ICONS 7

typedef struct _ThemesPage
{
	PrefsPage page;

	GtkWidget *window;		/* do not modify */

	GtkWidget *op_menu;
	GtkWidget *btn_install;
	GtkWidget *btn_more;
	GtkWidget *global;

	GtkWidget *name;
	GtkWidget *author;
	GtkWidget *url;
	GtkWidget *status;
	
	GtkWidget *icons[PREVIEW_ICONS];
	
	GtkWidget *btn_use;
	GtkWidget *btn_remove;

	GdkPixbuf *pixbufs[PREVIEW_ICONS];

	/* gchar     *theme_path; */
} ThemesPage;

typedef struct _ThemeInfo
{
	gchar *name;
	gchar *author;
	gchar *url;
	gchar *status;
} ThemeInfo;

typedef struct _ThemeName
{
	gchar *name;
	GList *item;
} ThemeName;

typedef struct _ThemesData
{
	GList      *themes;
	GList	   *names;
	gchar      *displayed;
	ThemesPage *page;
} ThemesData;

typedef void (*FileFunc) (const gchar *filename, gpointer data);

typedef struct _DirInfo {
	gint bytes;
	gint files;
	gint pixms;
} DirInfo;

typedef struct _CopyInfo {
	gchar *dest;
	gchar *status;
} CopyInfo;

static ThemesData *prefs_themes_data;

StockPixmap prefs_themes_icons[PREVIEW_ICONS] = { 
	STOCK_PIXMAP_DIR_CLOSE,
	STOCK_PIXMAP_MAIL_SEND,
	STOCK_PIXMAP_MAIL_RECEIVE, 
	STOCK_PIXMAP_MAIL_ATTACH,
	STOCK_PIXMAP_BOOK, 
	STOCK_PIXMAP_MIME_TEXT_PLAIN, 
	STOCK_PIXMAP_REPLIED
};



static void prefs_themes_btn_use_clicked_cb	(GtkWidget *widget, gpointer data);
static void prefs_themes_btn_remove_clicked_cb	(GtkWidget *widget, gpointer data);
static void prefs_themes_btn_install_clicked_cb	(GtkWidget *widget, gpointer data);
static void prefs_themes_menu_item_activated_cb	(GtkWidget *widget, gpointer data);

static void prefs_themes_update_buttons		(const ThemesData *tdata);
static void prefs_themes_display_global_stats	(const ThemesData *tdata);
static void prefs_themes_get_theme_info         (ThemesData *tdata);
static void prefs_themes_display_theme_info     (ThemesData *tdata, const ThemeInfo *info);
static void prefs_themes_get_themes_and_names	(ThemesData *tdata);
static int prefs_themes_cmp_name(gconstpointer a, gconstpointer b);
static void prefs_themes_free_names		(ThemesData *tdata);

static void prefs_themes_set_themes_menu	(GtkComboBox *combo, const ThemesData *tdata);

static gchar *prefs_themes_get_theme_stats	(const gchar *dirname);
static gboolean prefs_themes_is_system_theme	(const gchar *dirname);

static void prefs_themes_create_widget          (PrefsPage *page, GtkWindow *window, gpointer data);
static void prefs_themes_destroy_widget         (PrefsPage *page);
static void prefs_themes_save                   (PrefsPage *page);

static void prefs_themes_foreach_file		(const gchar *dirname, const FileFunc func, gpointer data);
static void prefs_themes_file_stats		(const gchar *filename, gpointer data);
static void prefs_themes_file_remove		(const gchar *filename, gpointer data);
static void prefs_themes_file_install		(const gchar *filename, gpointer data);



static void prefs_themes_file_stats(const gchar *filename, gpointer data)
{
	struct stat s;
	DirInfo    *di = (DirInfo *)data;
	gint        len;
	
	if (0 == g_stat(filename, &s) && 0 != S_ISREG(s.st_mode)) {
		di->bytes += s.st_size;
		di->files++;
		len = strlen(filename);
		if (len > 4) {
			const gchar *extension = filename+(len-4);
			if (!strcmp(extension, ".xpm"))
				di->pixms++;
			else if (!strcmp(extension, ".png"))
				di->pixms++;
		}
	}
}
	
static void prefs_themes_file_remove(const gchar *filename, gpointer data)
{
	gchar **status = (gchar **)data;
	gchar *base;
	
	if ((*status) != NULL)
		return;
	
	base = g_path_get_basename(filename);
	if (TRUE == is_dir_exist(filename)) {
		if (strcmp(base, ".") != 0 && strcmp(base, "..") != 0)
			g_warning("prefs_themes_file_remove(): subdir in theme dir skipped: '%s'.\n",
						base);
	}
	else if (0 != claws_unlink(filename)) {
		(*status) = g_strdup(filename);
	}
	g_free(base);
}

static void prefs_themes_file_install(const gchar *filename, gpointer data)
{
	CopyInfo *ci = (CopyInfo *)data;
	gchar *base;
	
	if (ci->status != NULL)
		return;
	
	base = g_path_get_basename(filename);
	if (TRUE == is_dir_exist(filename)) {
		if (strcmp(base, ".") != 0 && strcmp(base, "..") !=0 )
			g_warning("prefs_themes_file_install(): subdir in theme dir skipped: '%s'.\n",
						base);
	}
	else {
		gchar *fulldest;
		
		fulldest = g_strconcat(ci->dest, G_DIR_SEPARATOR_S, base, NULL);
		
		if (0 != copy_file(filename, fulldest, FALSE)) {
			ci->status = g_strdup(filename);
		}
		g_free(fulldest);
	}
	g_free(base);
}

static void prefs_themes_foreach_file(const gchar *dirname, const FileFunc func, gpointer data)
{
	struct dirent *d;
	DIR           *dp;

	cm_return_if_fail(dirname != NULL);
	cm_return_if_fail(func != NULL);
	
	if ((dp = opendir(dirname)) == NULL) {
		debug_print("directory %s not found\n", dirname);
		return;
	}

	while ((d = readdir(dp)) != NULL) {
		gchar *entry;
		gchar *fullentry;

		entry     = d->d_name;
		fullentry = g_strconcat(dirname, G_DIR_SEPARATOR_S, entry, NULL);

		(*func)(fullentry, data);
		
		g_free(fullentry);
	}
	closedir(dp);
}

static gboolean prefs_themes_is_system_theme(const gchar *dirname)
{
	gint len;
	gchar *system_theme_dir;
	gboolean is_sys = FALSE;

	cm_return_val_if_fail(dirname != NULL, FALSE);

	system_theme_dir = stock_pixmap_get_system_theme_dir_for_theme(NULL);
	len = strlen(system_theme_dir);
	if (strlen(dirname) > len && 0 == strncmp(dirname, system_theme_dir, len))
		is_sys = TRUE;
	
	g_free(system_theme_dir);

	return is_sys;
}

static void prefs_themes_set_themes_menu(GtkComboBox *combo, const ThemesData *tdata)
{
	GtkListStore *store;
	GtkTreeIter iter;
	GList	  *themes = tdata->names;
	gint       i = 0, active = 0;
	GList     *sorted_list = NULL;

	cm_return_if_fail(combo != NULL);

	/* sort theme data list by data name */
	while (themes != NULL) {
		ThemeName *tname = (ThemeName *)(themes->data);

		sorted_list = g_list_insert_sorted(sorted_list, (gpointer)(tname),
				 		   (GCompareFunc)prefs_themes_cmp_name);

		themes = g_list_next(themes);
	}

	store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_POINTER);
	
	/* feed gtk_menu w/ sorted themes names */
	themes = sorted_list;
	while (themes != NULL) {
		ThemeName *tname = (ThemeName *)(themes->data);
		gchar     *tpath = (gchar *)(tname->item->data);

		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
				   0, tname->name,
				   1, tname->item->data, -1);

		if (tdata->displayed != NULL && !strcmp2(tdata->displayed,tpath))
			active = i;
		++i;

		themes = g_list_next(themes);
	}

	g_list_free(sorted_list);

	g_signal_connect(G_OBJECT(combo), "changed",
			 G_CALLBACK(prefs_themes_menu_item_activated_cb),
			 NULL);

	gtk_combo_box_set_model(combo, GTK_TREE_MODEL(store));
	gtk_combo_box_set_active(combo, active);
}

static int prefs_themes_cmp_name(gconstpointer a_p, gconstpointer b_p)
{
	/* compare two ThemeData structures by their name attribute */
	return strcmp2((gchar *)(((ThemeName*)a_p)->name),
					(gchar *)(((ThemeName*)b_p)->name));
}

static void prefs_themes_get_themes_and_names(ThemesData *tdata)
{
	GList *tpaths;
	
	cm_return_if_fail(tdata != NULL);
	
	if (tdata->themes != NULL)
		stock_pixmap_themes_list_free(tdata->themes);
	if (tdata->names != NULL)
		prefs_themes_free_names(tdata);
	
	tdata->themes = stock_pixmap_themes_list_new();
	
	tpaths = tdata->themes;
	while (tpaths != NULL) {
		ThemeName *name = g_new0(ThemeName, 1);
		gchar *sname = g_path_get_basename((const gchar *)(tpaths->data));
		
		if (IS_INTERNAL_THEME(sname))
			name->name = g_strdup(_("Default internal theme"));
		else
			name->name = g_strdup(sname);
		name->item = tpaths;
			
		tdata->names = g_list_append(tdata->names, name);
		if (!strcmp2(tpaths->data, prefs_common.pixmap_theme_path)) {
			tdata->displayed = (gchar *)tpaths->data;
		}
		tpaths = g_list_next(tpaths);
		g_free(sname);	
	}
}

void prefs_themes_init(void)
{
	ThemesData   *tdata;
	ThemesPage   *page;
	GList        *tpaths;
	static gchar *path[3];

	path[0] = _("Display");
	path[1] = _("Themes");
	path[2] = NULL;

	debug_print("Creating preferences for themes...\n");
	
	tdata = g_new0(ThemesData, 1);
	prefs_themes_data = tdata;

	prefs_themes_get_themes_and_names(tdata);
	
	page = g_new0(ThemesPage, 1);
	
	page->page.path = path;
	page->page.create_widget = prefs_themes_create_widget;
	page->page.destroy_widget = prefs_themes_destroy_widget;
	page->page.save_page = prefs_themes_save;
	page->page.weight = 130.0;
	prefs_gtk_register_page((PrefsPage *) page);

	tdata->page = page;

	tpaths = g_list_first(tdata->themes);
	if (tdata->displayed == NULL)
		tdata->displayed = (gchar *)(tpaths->data);
}

static void prefs_themes_free_names(ThemesData *tdata)
{
	GList *names;
	
	names = tdata->names;
	while (names != NULL) {
		ThemeName *tn = (ThemeName *)(names->data);
		
		tn->item = NULL;
		g_free(tn->name);
		g_free(tn);
		
		names = g_list_next(names);
	}
	g_list_free(names);
	tdata->names = NULL;
}

void prefs_themes_done(void)
{
	ThemesData *tdata = prefs_themes_data;

	debug_print("Finished preferences for themes.\n");
	
	stock_pixmap_themes_list_free(tdata->themes);
	prefs_themes_free_names(tdata); 
	g_free(tdata->page);
	g_free(tdata);
}

static void prefs_themes_btn_use_clicked_cb(GtkWidget *widget, gpointer data)
{
	ThemesData *tdata = prefs_themes_data;
	gchar      *theme_str;

	theme_str = tdata->displayed;
	
	g_free(prefs_common.pixmap_theme_path);
	
        prefs_common.pixmap_theme_path = g_strdup(theme_str);
       
        main_window_reflect_prefs_all_real(TRUE);
        compose_reflect_prefs_pixmap_theme();
       
	prefs_themes_update_buttons(tdata);
}

static void prefs_themes_btn_remove_clicked_cb(GtkWidget *widget, gpointer data)
{
	ThemesData *tdata = prefs_themes_data;
	gchar      *theme_str;
	gchar      *alert_title = NULL;
	AlertValue  val = 0;
	gchar      *tmp = NULL;

	theme_str = tdata->displayed;
	
	tmp = g_path_get_basename(theme_str);

	if (IS_SYSTEM_THEME(theme_str)) {
		if (!superuser_p()) {
			alertpanel_error(_("Only root can remove system themes"));
			return;
		}
		alert_title = g_strdup_printf(_("Remove system theme '%s'"), tmp);
	}
	if (NULL == alert_title) {
		alert_title = g_strdup_printf(_("Remove theme '%s'"), tmp);
	}

	g_free(tmp);

	val = alertpanel(alert_title,
			 _("Are you sure you want to remove this theme?"),
			 GTK_STOCK_NO, GTK_STOCK_YES, NULL);
	g_free(alert_title);

	if (G_ALERTALTERNATE == val) {
		gchar *status = NULL;
		
		prefs_themes_foreach_file(theme_str, prefs_themes_file_remove, &status); 
		if (0 != rmdir(theme_str)) {
			if (status != NULL) {
				alertpanel_error(_("File %s failed\nwhile removing theme."), status);
				g_free(status);
			}
			else
				alertpanel_error(_("Removing theme directory failed."));
		}
		else {	
			alertpanel_notice(_("Theme removed successfully"));
			/* update interface back to first theme */
			prefs_themes_get_themes_and_names(tdata);
			prefs_themes_set_themes_menu(GTK_COMBO_BOX(tdata->page->op_menu), tdata);
			prefs_themes_display_global_stats(tdata);
			tdata->displayed = (gchar *)((g_list_first(tdata->themes))->data);
			prefs_themes_get_theme_info(tdata);
		}
	}
}

static void prefs_themes_btn_install_clicked_cb(GtkWidget *widget, gpointer data)
{
	gchar      *filename, *source;
	gchar 	   *themeinfo, *themename;
	gchar      *alert_title = NULL;
	CopyInfo   *cinfo;
	AlertValue  val = 0;
	ThemesData *tdata = prefs_themes_data;
	
	filename = filesel_select_file_open_folder(_("Select theme folder"), NULL);
	if (filename == NULL) 
		return;
	
	if (filename[strlen(filename) - 1] != G_DIR_SEPARATOR)
		filename = g_strconcat(filename, G_DIR_SEPARATOR_S, NULL);
	else
		filename = g_strdup(filename);

	cinfo = g_new0(CopyInfo, 1);
	source = g_path_get_dirname(filename);
	themename = g_path_get_basename(source);
	debug_print("Installing '%s' theme from %s\n", themename, filename);

	themeinfo = g_strconcat(source, G_DIR_SEPARATOR_S, THEMEINFO_FILE, NULL);
	alert_title = g_strdup_printf(_("Install theme '%s'"), themename);
	if (file_exist(themeinfo, FALSE) == FALSE) {
		val = alertpanel(alert_title,
				 _("This folder doesn't seem to be a theme folder.\nInstall anyway?"),
				 GTK_STOCK_NO, GTK_STOCK_YES, NULL);
		if (G_ALERTALTERNATE != val)
			goto end_inst;
	}
	if (superuser_p ()) {
		val = alertpanel(alert_title,
				 _("Do you want to install theme for all users?"),
				 GTK_STOCK_NO, GTK_STOCK_YES, NULL);
		switch (val) {
		case G_ALERTALTERNATE:
			cinfo->dest = stock_pixmap_get_system_theme_dir_for_theme(
						themename);
			break;
		case G_ALERTDEFAULT:
			break;
		default:
			goto end_inst;
		}
	}
	g_free(alert_title);
	if (cinfo->dest == NULL) {
		cinfo->dest = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
					  PIXMAP_THEME_DIR, G_DIR_SEPARATOR_S,
					  themename, NULL);
	}
	if (TRUE == is_dir_exist(cinfo->dest)) {
		AlertValue val = alertpanel_full(_("Theme exists"),
				_("A theme with the same name is\nalready installed in this location.\n\n"
				  "Do you want to replace it?"),
				GTK_STOCK_CANCEL, _("Overwrite"), NULL, FALSE,
				NULL, ALERT_WARNING, G_ALERTDEFAULT);
		if (val == G_ALERTALTERNATE) {
			if (remove_dir_recursive(cinfo->dest) < 0) {
				alertpanel_error(_("Couldn't delete the old theme in %s."), cinfo->dest);
				goto end_inst;
			}
		} else {
			goto end_inst;
		}
	}
	if (0 != make_dir_hier(cinfo->dest)) {
		alertpanel_error(_("Couldn't create destination directory %s."), cinfo->dest);
		goto end_inst;
	}
	prefs_themes_foreach_file(source, prefs_themes_file_install, cinfo);
	if (cinfo->status == NULL) {
		GList *insted;

		/* update interface to show newly installed theme */
		prefs_themes_get_themes_and_names(tdata);
		insted = g_list_find_custom(tdata->themes, 
					    (gpointer)(cinfo->dest), 
					    (GCompareFunc)strcmp2);
		if (NULL != insted) {
			alertpanel_notice(_("Theme installed successfully."));
			tdata->displayed = (gchar *)(insted->data);
			prefs_themes_set_themes_menu(GTK_COMBO_BOX(tdata->page->op_menu), tdata);
			prefs_themes_display_global_stats(tdata);
			prefs_themes_get_theme_info(tdata);
		}
		else
			alertpanel_error(_("Failed installing theme"));
	}
	else
		alertpanel_error(_("File %s failed\nwhile installing theme."), cinfo->status);
end_inst:
	g_free(cinfo->dest);
	g_free(filename);
	g_free(source);
	g_free(themeinfo);
	g_free(cinfo);
	g_free(themename);
}

static void prefs_themes_menu_item_activated_cb(GtkWidget *widget, gpointer data)
{
	ThemesData *tdata = prefs_themes_data;
	gchar      *path;
	GtkTreeModel *model;
	GtkTreeIter iter;
	
	cm_return_if_fail(gtk_combo_box_get_active_iter(GTK_COMBO_BOX(widget), &iter));
	
	model = gtk_combo_box_get_model(GTK_COMBO_BOX(widget));			
	gtk_tree_model_get(model, &iter, 1, &path, -1);	

	tdata->displayed = path;
	prefs_themes_get_theme_info(tdata);
}

static void prefs_themes_update_buttons(const ThemesData *tdata)
{
	ThemesPage *theme = tdata->page;
	gboolean    can_rem, can_use;

	can_use = !IS_CURRENT_THEME(tdata->displayed);
	can_rem = can_use && !IS_INTERNAL_THEME(tdata->displayed);
	
	if (theme->btn_use != NULL)
		gtk_widget_set_sensitive(theme->btn_use, can_use);
	if (theme->btn_remove != NULL)
		gtk_widget_set_sensitive(theme->btn_remove, can_rem);
}

/* placeholders may already be utf8 (i18n) */
#define SET_LABEL_TEXT_UTF8(label, text)				\
{									\
	gchar *tmpstr;							\
									\
	if (!g_utf8_validate(text, -1, NULL))				\
		tmpstr = conv_codeset_strdup(text,			\
			conv_get_locale_charset_str(),	CS_UTF_8);	\
	else								\
		tmpstr = g_strdup(text);				\
									\
	gtk_label_set_text(GTK_LABEL(label), tmpstr);			\
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);		\
	g_free(tmpstr);							\
}
static void prefs_themes_display_theme_info(ThemesData *tdata, const ThemeInfo *info)
{
	ThemesPage *theme = tdata->page;
	gchar *save_prefs_path;
	gint   i;

	SET_LABEL_TEXT_UTF8(theme->name,	info->name);
	SET_LABEL_TEXT_UTF8(theme->author,	info->author);
	SET_LABEL_TEXT_UTF8(theme->url,		info->url);
	SET_LABEL_TEXT_UTF8(theme->status,	info->status);

	save_prefs_path = prefs_common.pixmap_theme_path;
	prefs_common.pixmap_theme_path = tdata->displayed;
	for (i = 0; i < PREVIEW_ICONS; ++i) {
		stock_pixbuf_gdk(theme->window, prefs_themes_icons[i], 
				&(theme->pixbufs[i]));
		gtk_image_set_from_pixbuf(GTK_IMAGE(theme->icons[i]),
				theme->pixbufs[i]);
	}
	prefs_common.pixmap_theme_path = save_prefs_path;

	prefs_themes_update_buttons(tdata);
}
#undef SET_LABEL_TEXT_UTF8

static void prefs_themes_display_global_stats(const ThemesData *tdata)
{
	ThemesPage *theme = tdata->page;
	GList      *tnames = tdata->names;
	gchar      *gstats;
	gint        sys = 0;
	gint        usr = 0;
	gint        all = 0;

	while (tnames != NULL) {
		ThemeName *tname = (ThemeName *)(tnames->data);
		gchar     *tpath = (gchar *)(tname->item->data);
		
		if (IS_SYSTEM_THEME(tpath)) 
			++sys;
		else if (!IS_INTERNAL_THEME(tpath)) 
			++usr;
		++all;
		tnames = g_list_next(tnames);
	}

	gstats = g_strdup_printf(_("%d themes available (%d user, %d system, 1 internal)"),
				 all, usr, sys);
	gtk_label_set_text(GTK_LABEL(theme->global), gstats);
	gtk_label_set_justify (GTK_LABEL (theme->global), GTK_JUSTIFY_LEFT);
	gtkut_widget_set_small_font_size (theme->global);
	g_free(gstats);
}

#define INFOFILE_LINE_LEN 80

#define FGETS_INFOFILE_LINE() \
	line[0] = '\0'; \
	if (fgets(line, INFOFILE_LINE_LEN, finfo) != NULL && (len = strlen(line)) > 0) { \
		if (line[len - 1] == '\n') line[len - 1] = '\0'; \
	} \
	else { \
		strcpy(line, _("Unknown")); \
	}

static void prefs_themes_get_theme_info(ThemesData *tdata)
{
	FILE  *finfo;
	gchar *sinfo;
	gchar *path;
	gchar  line[INFOFILE_LINE_LEN];
	gint   len;
	ThemeInfo *info;
	ThemesPage *theme = tdata->page;

	cm_return_if_fail(theme != NULL);
	path = tdata->displayed;
	cm_return_if_fail(path != NULL);

	debug_print("Getting theme info for %s\n", path);
	
	info = g_new0(ThemeInfo, 1);
	
	if (IS_INTERNAL_THEME(path)) {
		info->name = g_strdup(_("Default internal theme"));
		info->author = g_strdup(_("The Claws Mail Team"));
		info->url = g_strdup(HOMEPAGE_URI);
		info->status = g_strdup_printf(_("Internal theme has %d icons"), N_STOCK_PIXMAPS);
	}
	else {
		sinfo = g_strconcat(path, G_DIR_SEPARATOR_S, THEMEINFO_FILE, NULL);
		finfo = g_fopen(sinfo, "r");
		if (finfo == NULL) {
			info->name = g_strdup(_("No info file available for this theme"));
			info->author = g_strdup(_("Unknown"));
			info->url = g_strdup(_("Unknown"));
		}
		else {
			FGETS_INFOFILE_LINE()
			info->name = g_strdup(line);
			FGETS_INFOFILE_LINE()
			info->author = g_strdup(line);
			FGETS_INFOFILE_LINE()
			info->url = g_strdup(line);
		
			fclose(finfo);
		}
		g_free(sinfo);

		info->status = prefs_themes_get_theme_stats(path);
		if (info->status == NULL) {
			info->status = g_strdup(_("Error: couldn't get theme status"));
		}
	}

	prefs_themes_display_theme_info(tdata, info);

	g_free(info->name);
	g_free(info->author);
	g_free(info->url);
	g_free(info->status);
	
	g_free(info);
}

#undef FGETS_INFOFILE_LINE

static gchar *prefs_themes_get_theme_stats(const gchar *dirname)
{
	gchar   *stats;
	DirInfo *dinfo;

	dinfo = g_new0(DirInfo, 1);
	
	prefs_themes_foreach_file(dirname, prefs_themes_file_stats, dinfo);
	stats = g_strdup_printf(_("%d files (%d icons), size: %s"), 
				dinfo->files, dinfo->pixms, to_human_readable((goffset)dinfo->bytes));
	
	g_free(dinfo);
	return stats;
}

static void prefs_themes_create_widget(PrefsPage *page, GtkWindow *window, gpointer data)
{
	ThemesPage *prefs_themes = (ThemesPage *)page;
	ThemesData *tdata = prefs_themes_data;

	GtkWidget *vbox1;
	GtkWidget *frame1;
	GtkWidget *vbox2;
	GtkWidget *hbox3;
	GtkWidget *menu_themes;
	GtkWidget *btn_install;
	GtkWidget *btn_more;
	GtkWidget *label_global_status;
	GtkWidget *frame_info;
	GtkWidget *table1;
	GtkWidget *label1;
	GtkWidget *label2;
	GtkWidget *label3;
	GtkWidget *label_name;
	GtkWidget *label_author;
	GtkWidget *label_url;
	GtkWidget *label4;
	GtkWidget *label_status;
	GtkWidget *frame_preview;
	GtkWidget *hbox1;
	GtkWidget *icon_1;
	GtkWidget *icon_2;
	GtkWidget *icon_3;
	GtkWidget *icon_4;
	GtkWidget *icon_5;
	GtkWidget *icon_6;
	GtkWidget *icon_7;
	GtkWidget *frame_buttons;
	GtkWidget *hbuttonbox1;
	GtkWidget *btn_use;
	GtkWidget *btn_remove;
	GtkCellRenderer *renderer;

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);
	gtk_widget_show (vbox1);

	vbox2 = gtkut_get_options_frame(vbox1, &frame1, _("Selector"));

	hbox3 = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox3);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox3, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox3), 5);

	menu_themes = gtk_combo_box_new();
	gtk_widget_show (menu_themes);
	gtk_box_pack_start (GTK_BOX (hbox3), menu_themes, FALSE, FALSE, 0);

	btn_install = gtk_button_new_with_label (_("Install new..."));
	gtk_widget_show (btn_install);
	gtk_box_pack_start (GTK_BOX (hbox3), btn_install, FALSE, FALSE, 0);
	gtkut_widget_set_can_default (btn_install, TRUE);

	btn_more = gtkut_get_link_btn((GtkWidget *)window, THEMES_URI, _("Get more..."));
	gtk_widget_show (btn_more);
	gtk_box_pack_start (GTK_BOX (hbox3), btn_more, FALSE, FALSE, 0);

	label_global_status = gtk_label_new ("");
	gtk_widget_show (label_global_status);
	gtk_box_pack_start (GTK_BOX (vbox2), label_global_status, FALSE, FALSE, 0);
	gtk_label_set_justify (GTK_LABEL (label_global_status), GTK_JUSTIFY_LEFT);
	gtk_misc_set_alignment (GTK_MISC (label_global_status), 0, 0.5);
	gtk_misc_set_padding (GTK_MISC (label_global_status), 6, 0);

	PACK_FRAME(vbox1, frame_info, _("Information"));

	table1 = gtk_table_new (4, 2, FALSE);
	gtk_widget_show (table1);
	gtk_container_add (GTK_CONTAINER (frame_info), table1);

	label1 = gtk_label_new (_("Name: "));
	gtk_widget_show (label1);
	gtk_table_attach (GTK_TABLE (table1), label1, 0, 1, 0, 1,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 8, 2);
	gtk_label_set_justify (GTK_LABEL (label1), GTK_JUSTIFY_LEFT);
	gtk_misc_set_alignment (GTK_MISC (label1), 0, 0.5);

	label2 = gtk_label_new (_("Author: "));
	gtk_widget_show (label2);
	gtk_table_attach (GTK_TABLE (table1), label2, 0, 1, 1, 2,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 8, 2);
	gtk_label_set_justify (GTK_LABEL (label2), GTK_JUSTIFY_LEFT);
	gtk_misc_set_alignment (GTK_MISC (label2), 0, 0.5);

	label3 = gtk_label_new (_("URL:"));
	gtk_widget_show (label3);
	gtk_table_attach (GTK_TABLE (table1), label3, 0, 1, 2, 3,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 8, 2);
	gtk_misc_set_alignment (GTK_MISC (label3), 0, 0.5);

	label_name = gtk_label_new ("");
	gtk_widget_show (label_name);
	gtk_table_attach (GTK_TABLE (table1), label_name, 1, 2, 0, 1,
			(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label_name), 0, 0.5);

	label_author = gtk_label_new ("");
	gtk_widget_show (label_author);
	gtk_table_attach (GTK_TABLE (table1), label_author, 1, 2, 1, 2,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label_author), 0, 0.5);

	label_url = gtk_label_new ("");
	gtk_widget_show (label_url);
	gtk_table_attach (GTK_TABLE (table1), label_url, 1, 2, 2, 3,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label_url), 0, 0.5);

	label4 = gtk_label_new (_("Status:"));
	gtk_widget_show (label4);
	gtk_table_attach (GTK_TABLE (table1), label4, 0, 1, 3, 4,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 8, 2);
	gtk_misc_set_alignment (GTK_MISC (label4), 0, 0.5);

	label_status = gtk_label_new ("");
	gtk_widget_show (label_status);
	gtk_table_attach (GTK_TABLE (table1), label_status, 1, 2, 3, 4,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label_status), 0, 0.5);

	PACK_FRAME(vbox1, frame_preview, _("Preview"));

	hbox1 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox1);
	gtk_container_add (GTK_CONTAINER (frame_preview), hbox1);

	icon_1 = gtk_image_new();
	gtk_widget_show (icon_1);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_1, TRUE, TRUE, 2);
	gtk_misc_set_padding (GTK_MISC (icon_1), 0, 5);

	icon_2 = gtk_image_new();
	gtk_widget_show (icon_2);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_2, TRUE, TRUE, 2);
	gtk_misc_set_padding (GTK_MISC (icon_2), 0, 5);

	icon_3 = gtk_image_new();
	gtk_widget_show (icon_3);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_3, TRUE, TRUE, 2);
	gtk_misc_set_padding (GTK_MISC (icon_3), 0, 5);

	icon_4 = gtk_image_new();
	gtk_widget_show (icon_4);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_4, TRUE, TRUE, 2);
	gtk_misc_set_padding (GTK_MISC (icon_4), 0, 5);

	icon_5 = gtk_image_new();
	gtk_widget_show (icon_5);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_5, TRUE, TRUE, 2);
	gtk_misc_set_padding (GTK_MISC (icon_5), 0, 5);

	icon_6 = gtk_image_new();
	gtk_widget_show (icon_6);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_6, TRUE, TRUE, 0);
	gtk_misc_set_padding (GTK_MISC (icon_6), 0, 5);

	icon_7 = gtk_image_new();
	gtk_widget_show (icon_7);
	gtk_box_pack_start (GTK_BOX (hbox1), icon_7, TRUE, TRUE, 0);
	gtk_misc_set_padding (GTK_MISC (icon_7), 0, 5);

	PACK_FRAME(vbox1, frame_buttons, _("Actions"));

	hbuttonbox1 = gtk_hbutton_box_new ();
	gtk_widget_show (hbuttonbox1);
	gtk_container_add (GTK_CONTAINER (frame_buttons), hbuttonbox1);
	gtk_container_set_border_width (GTK_CONTAINER (hbuttonbox1), 5);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (hbuttonbox1), GTK_BUTTONBOX_START);
	gtk_box_set_spacing (GTK_BOX (hbuttonbox1), 5);

	btn_use = gtk_button_new_with_label (_("Use this"));
	gtk_widget_show (btn_use);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), btn_use);
	gtkut_widget_set_can_default (btn_use, TRUE);

	btn_remove = gtk_button_new_with_label (_("Remove"));
	gtk_widget_show (btn_remove);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), btn_remove);
	gtkut_widget_set_can_default (btn_remove, TRUE);

	g_signal_connect(G_OBJECT(btn_use), "clicked",
			 G_CALLBACK(prefs_themes_btn_use_clicked_cb),
			 NULL);
	g_signal_connect(G_OBJECT(btn_remove), "clicked",
			 G_CALLBACK(prefs_themes_btn_remove_clicked_cb),
			 NULL);
	g_signal_connect(G_OBJECT(btn_install), "clicked",
			 G_CALLBACK(prefs_themes_btn_install_clicked_cb),
			 NULL);

	prefs_themes->window = GTK_WIDGET(window);
	
	prefs_themes->name   = label_name;
	prefs_themes->author = label_author;
	prefs_themes->url    = label_url;
	prefs_themes->status = label_status;
	prefs_themes->global = label_global_status;

	prefs_themes->icons[0] = icon_1;
	prefs_themes->icons[1] = icon_2;
	prefs_themes->icons[2] = icon_3;
	prefs_themes->icons[3] = icon_4;
	prefs_themes->icons[4] = icon_5;
	prefs_themes->icons[5] = icon_6;
	prefs_themes->icons[6] = icon_7;
	
	prefs_themes->btn_use     = btn_use;
	prefs_themes->btn_remove  = btn_remove;
	prefs_themes->btn_install = btn_install;
	prefs_themes->btn_more    = btn_more;

	prefs_themes->op_menu     = menu_themes;

	prefs_themes->page.widget = vbox1;
	
	prefs_themes_set_themes_menu(GTK_COMBO_BOX(menu_themes), tdata);
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(menu_themes), renderer, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(menu_themes), renderer,
					"text", 0, NULL);
	
	prefs_themes_get_theme_info(tdata);
	prefs_themes_display_global_stats(tdata);
}

static void prefs_themes_destroy_widget(PrefsPage *page)
{
	/* ThemesPage *theme = (ThemesPage *)page; */
}

static void prefs_themes_save(PrefsPage *page)
{
	/* ThemesPage *theme = (ThemesPage *)page; */
}

