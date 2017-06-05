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

#ifndef COMPAT_H
#define COMPAT_H

void radio_action_set_current_value(GtkRadioAction *action, gint current_value);
GList* hash_table_get_keys(GHashTable *hash_table);

#if !GTK_CHECK_VERSION(2,12,0)
#define gtk_widget_set_tooltip_text(widget, text) /* ignored */
#endif


/* Some systems (BSD,MacOsX,HP-UX,...) define MAP_ANON and not MAP_ANONYMOUS */
#if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#define	MAP_ANONYMOUS	MAP_ANON
#elif defined(MAP_ANONYMOUS) && !defined(MAP_ANON)
#define	MAP_ANON	MAP_ANONYMOUS
#endif


#endif /* COMPAT_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
