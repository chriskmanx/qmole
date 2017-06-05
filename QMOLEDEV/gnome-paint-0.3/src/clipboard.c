/***************************************************************************
 *            clipboard.c
 *
 *  Sun May 23 16:37:33 2010
 *  Copyright  2010  rogerio
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

#include "clipboard.h"
#include "cv_drawing.h"

void 
on_menu_copy_activate ( GtkMenuItem *menuitem, gpointer user_data )
{
    GdkPixbuf       *pixbuf;
    GtkClipboard    *clipboard;
//    gp_canvas *cv = cv_get_canvas ();

    clipboard   =   gtk_clipboard_get_for_display (gdk_display_get_default (),
	                                             GDK_SELECTION_CLIPBOARD);
    pixbuf = cv_get_pixbuf ();
    
    gtk_clipboard_set_image ( clipboard, pixbuf );
}

void 
on_menu_paste_activate ( GtkMenuItem *menuitem, gpointer user_data )
{
    GdkPixbuf       *pixbuf;
    GtkClipboard    *clipboard;
//    gp_canvas *cv = cv_get_canvas ();

    clipboard   =   gtk_clipboard_get_for_display (gdk_display_get_default (),
	                                             GDK_SELECTION_CLIPBOARD);
    
    pixbuf = gtk_clipboard_wait_for_image ( clipboard );

    if ( pixbuf != NULL )
    {
        cv_set_pixbuf ( pixbuf );
        g_object_unref ( pixbuf );
    }
}
