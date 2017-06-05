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

#ifndef _UNDO_H
#define _UNDO_H

void undo_reset_modified_step(GtkTextBuffer *buffer);
void undo_clear_all(GtkTextBuffer *buffer);
void undo_init(GtkWidget *view, GtkWidget *undo_button, GtkWidget *redo_button);
void undo_set_sequency(gboolean seq);
void undo_set_sequency_reserve(void);
void undo_undo(GtkTextBuffer *buffer);
void undo_redo(GtkTextBuffer *buffer);

#endif /* _UNDO_H */
