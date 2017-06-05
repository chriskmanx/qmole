/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
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

#ifndef __PREFS_SUMMARY_OPEN_H__
#define __PREFS_SUMMARY_OPEN_H__

#define SUMMARY_OPEN_ACTIONS 8

void prefs_summary_open_open		(void);
void prefs_summary_open_set_defaults	(void);
const gchar *summary_open_get_label	(int act);

#endif /* __PREFS_SUMMARY_OPEN_H__ */
