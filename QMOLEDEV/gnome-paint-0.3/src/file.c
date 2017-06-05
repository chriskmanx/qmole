/***************************************************************************
 *            file.c
 *
 *  Thu Jun 11 13:19:47 2009
 *  Copyright  2009  Rogério Ferro
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

#include "common.h"
#include "file.h"
#include "pixbuf-file-chooser.h"
#include "cv_drawing.h"
#include "undo.h"

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

/*private functions*/
static void		file_print_title	(void);
static void		file_set_name		(const char *name);
static void		file_set_type		(const char *type);
static void		file_set_title		(const char *title);
static void		file_set_untitle	(void);
static gboolean	file_save			(const gchar *filename, const gchar *type);
static void		file_error_dlg		(const gchar *message, const GError *error);

/*private data*/
GtkWindow 	*parent_window	=	NULL;
gchar 		*file_name		=	NULL;
gchar 		*file_title		=	NULL;
gchar 		*file_type		=	NULL;
gboolean	b_saved			=	TRUE;
gboolean	b_untitle		=	TRUE;


/*
 *   CODE
 */

void 
file_set_parent_window	( GtkWindow * wnd )
{
	parent_window	=	wnd;
	b_saved	=	TRUE;
	file_set_untitle ();
}

gboolean
file_is_save ( void )
{
    return b_saved;
}

void		
file_set_unsave ( void )
{
	b_saved	=	FALSE;
	file_print_title ();
}

void
file_set_save ( void )
{
	b_saved	=	TRUE;
	file_print_title ();    
}

gboolean
file_save_dialog ( void )
{
	gboolean cancel = FALSE;
	if (!b_saved)
	{
		gint result;
		GtkWidget *dlg;
        dlg = gtk_message_dialog_new(
               parent_window, 
               GTK_DIALOG_MODAL, 
               GTK_MESSAGE_QUESTION, 
               GTK_BUTTONS_NONE, 
               _("Do you want to save the changes you made to \"%s\"?"),
               file_title);
		gtk_message_dialog_format_secondary_text(
		    GTK_MESSAGE_DIALOG (dlg), 
		    _("Your changes will be lost if you don't save them.") );

		gtk_dialog_add_button (GTK_DIALOG(dlg), GTK_STOCK_DISCARD, GTK_RESPONSE_NO);
        gtk_dialog_add_button (GTK_DIALOG(dlg), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gtk_dialog_add_button (GTK_DIALOG(dlg), GTK_STOCK_SAVE, GTK_RESPONSE_YES);
		gtk_dialog_set_default_response (GTK_DIALOG(dlg), GTK_RESPONSE_YES);

		result = gtk_dialog_run (GTK_DIALOG (dlg));
		gtk_widget_destroy (dlg);
		switch (result)
        {
            case GTK_RESPONSE_YES:
				on_menu_save_activate (NULL, NULL);
                if ( b_saved )
                {
                    cancel = FALSE;
                }
                else
                {
                    cancel = TRUE;
                }
                break;
            case GTK_RESPONSE_CANCEL:
                cancel = TRUE;
                break;
         }
	}
	return cancel;
}




gboolean	
file_open ( const gchar * filename )
{
	gboolean	ok		=	FALSE;
	GError 		*error	= NULL;
	GdkPixbuf 	*pixbuf	= NULL;
	pixbuf = gdk_pixbuf_new_from_file (filename, &error);
	if (pixbuf != NULL)
	{
		if (!file_save_dialog () )
		{
			GdkPixbufFormat	*format	=	gdk_pixbuf_get_file_info (filename, NULL, NULL);
			GdkPixbuf		*orientation_changed_pixbuf;
			orientation_changed_pixbuf	=	gdk_pixbuf_apply_embedded_orientation (pixbuf);
			cv_set_pixbuf	( orientation_changed_pixbuf );
			g_object_unref	( orientation_changed_pixbuf );
            undo_clear ();
			ok	=	TRUE;
			if (gdk_pixbuf_format_is_writable (format))
			{
				gchar 	*title	=	g_filename_display_basename (filename);
				gchar	*type	=	gdk_pixbuf_format_get_name (format);
				b_untitle	=	FALSE;
				b_saved		=	TRUE;
				file_set_type	(type);
				file_set_name	(filename);
				file_set_title	(title);
				g_free (title);
				g_free (type);
			}
			else
			{
				b_saved	=	FALSE;
				file_set_untitle ();
			}
		}
	}
	else
	{
		gchar *basename	=	g_path_get_basename (filename);
		gchar *message	=	g_strdup_printf (_("Error loading file \"%s\""), basename);
		file_error_dlg ( message, error);
		g_error_free (error);
		g_free (basename);
		g_free (message);
	}
	g_object_unref (pixbuf);
	return ok;
}



/* GUI CallBacks */

void
on_menu_open_activate	( GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *dialog;
	gint response;

	dialog = pixbuf_file_chooser_new (parent_window, GTK_FILE_CHOOSER_ACTION_OPEN);

	response = gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_hide (dialog);

	if (response == GTK_RESPONSE_OK) 
	{
		gchar * name	= gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		file_open (name);
		g_free (name);
	}
	gtk_widget_destroy (dialog);
}

void 
on_menu_save_activate	( GtkMenuItem *menuitem, gpointer user_data)
{
	if (b_untitle)
	{
		on_menu_save_as_activate (menuitem, user_data);
	}
	else
	{
		if (file_save (file_name, file_type))
		{
			b_saved		=	TRUE;
			file_print_title ();
		}
	}
}

void
on_menu_save_as_activate	( GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *dialog;
	gint response;

	dialog = pixbuf_file_chooser_new (parent_window, GTK_FILE_CHOOSER_ACTION_SAVE);
	pixbuf_file_chooser_set_current_name ( PIXBUF_FILE_CHOOSER (dialog), file_title);
	pixbuf_file_chooser_set_current_filter( PIXBUF_FILE_CHOOSER (dialog), "png");

	response = gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_hide (dialog);
	
	if (response == GTK_RESPONSE_OK) 
	{
		gchar * name				= gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gchar * title				= pixbuf_file_chooser_get_name (PIXBUF_FILE_CHOOSER(dialog));
		GdkPixbufFormat * format	= pixbuf_file_chooser_get_format (PIXBUF_FILE_CHOOSER(dialog));
		gchar * type				= gdk_pixbuf_format_get_name (format);


		if (file_save (name, type))
		{
			b_untitle	=	FALSE;
			b_saved		=	TRUE;

			file_set_name	(name);
			file_set_type	(type);
			file_set_title	(title);
		}
		
		g_free (name);
		g_free (title);
		g_free (type);
	}
	gtk_widget_destroy (dialog);	
}


/*Private*/

static void
file_print_title (void)
{
	gchar *str;

	str = g_strdup_printf (_("%s - gnome-paint"), file_title);
	
	if (!b_saved)
	{
		gchar *copy;
		copy = g_strdup_printf ("%c%s", '*', str);
		g_free (str);
		str = copy;
	}

	gtk_window_set_title ( parent_window, str);

	g_free (str);
}

static void
file_set_name (const char *name)
{
	g_free (file_name);
	file_name = g_strdup (name);
}

static void
file_set_type (const char *type)
{
	g_free (file_type);
	file_type = g_strdup (type);
}

static void 
file_set_title	(const char *title)
{
	g_free( file_title );
	file_title	= g_strdup (title);	
	file_print_title ();
}

static void
file_set_untitle (void)
{
	b_untitle		=	TRUE;
	file_set_title (_("untitled"));
}

static gboolean
file_save (const gchar *filename, const gchar *type)
{
	gboolean	ok		=	TRUE;
	GdkPixbuf *	pixbuf	=	cv_get_pixbuf ();
	GError *	error	=	NULL;

	if ( !gdk_pixbuf_save ( pixbuf, filename, type, &error, NULL) )
	{
		gchar *basename	=	g_path_get_basename (filename);
		gchar *message	=	g_strdup_printf (_("Error saving file \"%s\""), basename);
		file_error_dlg ( message, error);
		g_error_free (error);
		g_free (basename);
		g_free (message);
		ok				=	FALSE;
	}
	g_object_unref (pixbuf);
	return ok;
}

static void
file_error_dlg (const gchar *message, const GError *error)
{
		gchar *error_message = _("unknown error");
		GtkWidget *dlg;
		if (error !=  NULL )
		{
			error_message = error->message;
		}
		g_warning ("%s: %s\n", message, error_message);
		dlg = gtk_message_dialog_new ( parent_window,
		                               GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
                              		   GTK_MESSAGE_ERROR,
                              		   GTK_BUTTONS_CLOSE,
                                       "%s",message);
		gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG (dlg), "%s",error_message);
		gtk_dialog_run (GTK_DIALOG (dlg));
		gtk_widget_destroy (dlg);
}
