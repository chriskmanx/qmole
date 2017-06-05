/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef LAYOUT_CONFIG_H
#define LAYOUT_CONFIG_H


#include "layout.h"


GtkWidget *layout_config_new(void);

void layout_config_set(GtkWidget *widget, gint style, const gchar *order);
gchar *layout_config_get(GtkWidget *widget, gint *style);


gchar *layout_config_order_to_text(gint a, gint b, gint c);
void layout_config_order_from_text(const gchar *text, gint *a, gint *b, gint *c);

void layout_config_parse(gint style, const gchar *order,
			 LayoutLocation *a, LayoutLocation *b, LayoutLocation *c);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
