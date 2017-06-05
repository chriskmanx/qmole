/***************************************************************************
 *            file.h
 *
 *  Thu Jun 11 13:19:47 2009
 *  Copyright  2009  rogerio
 *  <rogerio@<host>>
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
 

void		file_set_parent_window	( GtkWindow * wnd );
gboolean	file_save_dialog		( void );
gboolean    file_is_save            ( void );
void		file_set_unsave		    ( void );
void        file_set_save           ( void );
gboolean	file_open				( const gchar * filename );


/* GUI CallBacks */
void on_menu_open_activate		( GtkMenuItem *menuitem, gpointer user_data);
void on_menu_save_activate		( GtkMenuItem *menuitem, gpointer user_data);
void on_menu_save_as_activate	( GtkMenuItem *menuitem, gpointer user_data);


