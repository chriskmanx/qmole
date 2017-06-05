/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _RUN_H
#define _RUN_H

#include <gtk/gtk.h>

void run_app(const char *path);
void run_with_files(const char *path, GList *uri_list);
void run_with_data(const char *path, gpointer data, gulong length);
gboolean run_by_path(const guchar *full_path);
gboolean run_by_uri(const gchar *uri, gchar **errmsg);
gboolean run_diritem(const guchar *full_path,
		     DirItem *item,
		     FilerWindow *filer_window,
		     FilerWindow *src_window,
		     gboolean edit);
void open_to_show(const guchar *path);
void examine(const guchar *path);
void show_help_files(const char *dir);
void run_with_args(const char *path, DirItem *item, const char *args);

#endif /* _RUN_H */
