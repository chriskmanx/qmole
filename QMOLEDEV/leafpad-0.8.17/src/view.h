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

#ifndef _VIEW_H
#define _VIEW_H

gint get_current_keyval(void);
void clear_current_keyval(void);
void scroll_to_cursor(GtkTextBuffer *buffer, gdouble within_margin);
void force_call_cb_modified_changed(GtkWidget *view);
void force_block_cb_modified_changed(GtkWidget *view);
void force_unblock_cb_modified_changed(GtkWidget *view);
void set_view_scroll(void);
gint check_text_modification(void);
GtkWidget *create_text_view(void);

#endif /* _VIEW_H */
