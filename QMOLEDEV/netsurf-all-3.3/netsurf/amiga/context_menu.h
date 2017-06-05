/*
 * Copyright 2008-9, 2011 Chris Young <chris@unsatisfactorysoftware.co.uk>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef AMIGA_CONTEXT_MENU_H
#define AMIGA_CONTEXT_MENU_H

struct tree;
struct gui_window;
struct gui_window_2;
struct form_control;

void ami_context_menu_init(void);
void ami_context_menu_free(void);
BOOL ami_context_menu_mouse_trap(struct gui_window_2 *gwin, BOOL trap);
void ami_context_menu_show(struct gui_window_2 *gwin, int x, int y);

void gui_create_form_select_menu(struct gui_window *g, struct form_control *control);

#endif

