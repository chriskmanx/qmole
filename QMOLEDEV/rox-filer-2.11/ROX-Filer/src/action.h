/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _ACTION_H
#define _ACTION_H

#include <gtk/gtk.h>

void action_init(void);

void action_usage(GList *paths);
void action_mount(GList	*paths, gboolean open_dir, gboolean mount, int quiet);
void action_delete(GList *paths);
void action_chmod(GList *paths, gboolean force_recurse, const char *action);
void action_find(GList *paths);
void action_move(GList *paths, const char *dest, const char *leaf, int quiet);
void action_copy(GList *paths, const char *dest, const char *leaf, int quiet);
void action_link(GList *paths, const char *dest, const char *leaf,
		 gboolean relative);
void action_eject(GList *paths);
void show_condition_help(gpointer data);
void set_find_string_colour(GtkWidget *widget, const guchar *string);
void action_settype(GList *paths, gboolean force_recurse, const char *oldtype);

#endif /* _ACTION_H */
