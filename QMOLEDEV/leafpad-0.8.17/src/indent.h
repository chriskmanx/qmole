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

#ifndef _INDENT_H
#define _INDENT_H

gint get_current_tab_width(void);
void indent_set_state(gboolean state);
gboolean indent_get_state(void);
void indent_real(GtkWidget *text_view);
void indent_refresh_tab_width(GtkWidget *text_view);
void indent_toggle_tab_width(GtkWidget *text_view);
void indent_set_default_tab_width(gint width);
void indent_multi_line_indent(GtkTextBuffer *buffer);
void indent_multi_line_unindent(GtkTextBuffer *buffer);
//void indent_init(GtkWidget *text_view);

#endif /* _INDENT_H */
