/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis, Vladimir Nadvornik, Laurent Monin
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef URI_UTILS_H
#define URI_UTILS_H

/* dnd data parsers (uris) */

gchar *uri_text_escape(const gchar *text);
void uri_text_decode(gchar *text);

GList *uri_list_from_text(gchar *data, gboolean files_only);
GList *uri_filelist_from_text(gchar *data, gboolean files_only);
gchar *uri_text_from_list(GList *list, gint *len, gboolean plain_text);
gchar *uri_text_from_filelist(GList *list, gint *len, gboolean plain_text);

#endif /* URI_UTILS_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
