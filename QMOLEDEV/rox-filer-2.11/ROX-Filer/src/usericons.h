/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 *
 * Diego Zamboni, Feb 7, 2001
 */

#ifndef _USERICONS_H_
#define _USERICONS_H_

/* Public interface */
void read_globicons(void);
void check_globicon(const guchar *path, DirItem *item);
void icon_set_handler_dialog(DirItem *item, const guchar *path);
void add_globicon(const gchar *path, const gchar *icon);
void delete_globicon(const gchar *path);

#endif
