/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Authors: Vladimir Nadvornik / Laurent Monin
 *
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef MISC_H
#define MISC_H

gdouble get_zoom_increment(void);
gchar *utf8_validate_or_convert(const gchar *text);
gint utf8_compare(const gchar *s1, const gchar *s2, gboolean case_sensitive);
gchar *expand_tilde(const gchar *filename);
int runcmd(gchar *cmd);

#endif /* MISC_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
