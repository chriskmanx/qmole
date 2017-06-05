/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _MENU_H
#define _MENU_H

void menu_sensitivity_from_modified_flag(gboolean is_text_modified);
void menu_sensitivity_from_selection_bound(gboolean is_bound_exist);
//void menu_sensitivity_from_clipboard(gboolean is_clipboard_exist);
void menu_sensitivity_from_clipboard(void);
GtkWidget *create_menu_bar(GtkWidget *window);

#endif /* _MENU_H */
