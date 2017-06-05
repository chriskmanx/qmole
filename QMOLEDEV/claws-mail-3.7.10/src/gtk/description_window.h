/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef _DESCRIPTION_WINDOW_H
#define _DESCRIPTION_WINDOW_H

typedef struct _DescriptionWindow DescriptionWindow;

struct _DescriptionWindow
{
	GtkWidget 	* window;
	GtkWidget	* parent;
	/** Number of columns for each line of data **/
	int		  columns;
	/** title of the window **/
	gchar		* title;
	/** description **/
	gchar		* description;
	/** points to the table of strings to be show in the window */
	gchar 		** symbol_table;
};

void description_window_create(DescriptionWindow *dwindow);

#endif /* _DESCRIPTION_WINDOW_H */
