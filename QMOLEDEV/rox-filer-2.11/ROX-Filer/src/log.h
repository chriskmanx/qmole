/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _LOG_H
#define _LOG_H

void log_init(void);
void log_info_paths(const gchar *message, GList *paths, const gchar *dest);
void log_show_window(void);

#endif /* _LOG_H */
