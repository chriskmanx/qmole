/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __MANUAL_H__
#define __MANUAL_H__

typedef enum
{
	MANUAL_MANUAL_CLAWS,
	MANUAL_FAQ_CLAWS,
} ManualType;

#define MANUAL_ANCHOR_WIZARD		"start_wizard"
#define MANUAL_ANCHOR_FILTERING		"handling_filters"
#define MANUAL_ANCHOR_SEARCHING		"handling_searching"
#define MANUAL_ANCHOR_ACCOUNTPREFS	"ch_account"
#define MANUAL_ANCHOR_ADDRBOOK		"ch_addrbook"
#define MANUAL_ANCHOR_ACTIONS		"adv_actions"
#define MANUAL_ANCHOR_TEMPLATES		"adv_templates"
#define MANUAL_ANCHOR_PROCESSING	"adv_processing"
#define MANUAL_ANCHOR_PLUGINS		"adv_plugins"
#define MANUAL_ANCHOR_TAGS		"adv_tags"

gboolean manual_available	(ManualType type);
void	 manual_open		(ManualType type, gchar *url_anchor);
void	 manual_open_with_anchor_cb(GtkWidget *widget, gchar *url_anchor);

#endif /* __MANUAL_H__ */
