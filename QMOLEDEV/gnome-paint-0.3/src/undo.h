/***************************************************************************
 *            undo.h
 *
 *  Sat Jan 23 14:05:22 2010
 *  Copyright  2010  Rogério Ferro do Nascimento
 *  <rogerioferro@gmail.com>
 ****************************************************************************/

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */
#include <gtk/gtk.h>
#include "toolbar.h"


void undo_create_mask   ( gint        width, 
                          gint        height, 
                          GdkBitmap   **mask, 
                          GdkGC       **gc_mask );

void undo_add           ( GdkRectangle  *rect, 
                          GdkBitmap     *mask,
                          GdkPixmap     *background,
                          gp_tool_enum  tool );

void undo_add_resize    ( gint width, gint height );
void undo_clear          ( void );


/* GUI CallBack */
void on_menu_undo_activate		( GtkMenuItem *menuitem, gpointer user_data );
void on_menu_redo_activate		( GtkMenuItem *menuitem, gpointer user_data );


 