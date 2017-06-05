/* Gnome Paint - 
 *
 * Copyright (C) 2009 The Free Software Foundation
 *
 * Author: Rogério Ferro <rogerioferro@gmail.com>
 *
 * Based on eog code (src/eog-file-chooser.c) by:
 * 	- Lucas Rocha <lucasr@gnome.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pixbuf-file-chooser.h"

#include <glib/gi18n.h>
#include <gtk/gtk.h>

static char *last_dir[] = { NULL, NULL, NULL, NULL };

#define FILE_FORMAT_KEY "file-format"

#define PIXBUF_FILE_CHOOSER_GET_PRIVATE(object)	\
	(G_TYPE_INSTANCE_GET_PRIVATE ((object), PIXBUF_TYPE_FILE_CHOOSER, PixbufFileChooserPrivate))


static GdkPixbufFormat*	pixbuf_get_format_by_suffix		(const gchar *suffix);
static gchar*			get_suffix_from_basename 		(const gchar *basename);


enum 
{
	TEXT_COL,
	FILTER_COL,
	N_COLUMNS
};

struct _PixbufFileChooserPrivate
{
	GtkWidget	*combo_filter;
	GSList		*filters;
	gchar		*name;
};


G_DEFINE_TYPE(PixbufFileChooser, pixbuf_file_chooser, GTK_TYPE_FILE_CHOOSER_DIALOG)

static void
pixbuf_file_chooser_finalize (GObject *object)
{
	PixbufFileChooserPrivate *priv;
	GSList *it;

	priv	=	PIXBUF_FILE_CHOOSER (object)->priv;

	for (it = priv->filters; it != NULL; it = it->next)
    {
		GtkFileFilter *filter;
		filter = GTK_FILE_FILTER (it->data);
		g_object_unref (filter);
    }
	g_slist_free (priv->filters);
	g_free (priv->name);
	
	(* G_OBJECT_CLASS (pixbuf_file_chooser_parent_class)->finalize) (object);
}

static void
pixbuf_file_chooser_class_init (PixbufFileChooserClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;

	object_class->finalize = pixbuf_file_chooser_finalize;

	g_type_class_add_private (object_class, sizeof (PixbufFileChooserPrivate));
}

static void
pixbuf_file_chooser_init (PixbufFileChooser *chooser)
{
	chooser->priv = PIXBUF_FILE_CHOOSER_GET_PRIVATE (chooser);
	chooser->priv->combo_filter	= NULL;
	chooser->priv->filters 		= NULL;
	chooser->priv->name			= g_strdup ("untitled");
}

static void
response_cb (GtkDialog *dlg, gint id, gpointer data)
{
	gchar *dir;
	GtkFileChooserAction action;

	if (id == GTK_RESPONSE_OK) 
	{
		dir		= gtk_file_chooser_get_current_folder (GTK_FILE_CHOOSER (dlg));
		action	= gtk_file_chooser_get_action (GTK_FILE_CHOOSER (dlg));

		g_assert (action < G_N_ELEMENTS (last_dir));
		if (last_dir [action] != NULL)
			g_free (last_dir [action]);

		last_dir [action] = dir;
	}
}
/*
static void
add_custom_button_to_dialog (GtkDialog   *dialog,
			     			 const gchar *mnemonic_label,
						     const gchar *stock_id,
						     gint         response_id)
{
  GtkWidget *button;

  button = gtk_button_new_with_mnemonic (mnemonic_label);
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  gtk_button_set_image (GTK_BUTTON (button),
			gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_BUTTON));
  gtk_widget_show (button);

  gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, response_id);
}
*/

static gboolean
pixbuf_file_chooser_add_ext (PixbufFileChooser * chooser)
{
	gboolean changed = FALSE;
	GFile *file;
	gchar *file_name, *suffix;
	GdkPixbufFormat	*format, *my_format;
	
	file	= gtk_file_chooser_get_file (GTK_FILE_CHOOSER (chooser));
	
	if ( file != NULL )
	{
		file_name	= g_file_get_basename (file);
		g_object_unref (file);
	}
	else
	{
		file_name	= g_strdup (chooser->priv->name);
	}

	suffix		= get_suffix_from_basename (file_name);
	format		= pixbuf_get_format_by_suffix (suffix);

	my_format 	= pixbuf_file_chooser_get_format (chooser);

	if (format != my_format)
	{
		gchar *new_file_name, *extension;
		if (format != NULL)
		{
			*--suffix = '\0';
		}
		extension		= gdk_pixbuf_format_get_name ( my_format );
		new_file_name	= g_strdup_printf ("%s.%s", file_name, extension);
		pixbuf_file_chooser_set_current_name ( chooser, new_file_name);
		g_free (new_file_name);
		g_free (extension);
		changed = TRUE;
	}
	g_free (file_name);
	
	return changed;
}

static void
save_response_cb (GtkDialog *dlg, gint id, gpointer data)
{
	gboolean _ok = TRUE;

	if (id != GTK_RESPONSE_OK)
		return;
	if (pixbuf_file_chooser_add_ext (PIXBUF_FILE_CHOOSER (dlg)))
	{
		/* We changed the file name, then
		   we need to test if the file already exist*/
		GFile	*file;
		file	= gtk_file_chooser_get_file (GTK_FILE_CHOOSER (dlg));
		if ( g_file_query_exists (file, NULL) )
		{
			_ok = FALSE;
			gtk_file_chooser_set_file (GTK_FILE_CHOOSER (dlg), file, NULL);
			g_signal_stop_emission_by_name (dlg, "response");
			{
				GtkWidget * widget	= gtk_dialog_get_content_area (dlg);
				GtkWidget * child 	= gtk_container_get_focus_child (GTK_CONTAINER (widget));
				while (!GTK_IS_BUTTON (child) )
				{
					child 	= gtk_container_get_focus_child (GTK_CONTAINER (child));
				}
				gtk_button_clicked (GTK_BUTTON (child));
			}
		}
		g_object_unref (file);
	}


	if (_ok)
	{
		response_cb (dlg, id, data);
	}
}

static GSList *
pixbuf_get_filters ( gboolean savable )
{
	GtkFileFilter	*all_img_filter = NULL;
	GSList 			*list;
	GSList 			*write_list = NULL;
	GSList 			*it;

	if (!savable)
	{
		/* All Image Filter */
		all_img_filter = gtk_file_filter_new ();
		gtk_file_filter_set_name (all_img_filter, _("All Images"));
		write_list = g_slist_prepend (write_list, all_img_filter);
	}

	list = gdk_pixbuf_get_formats ();

	for (it = list; it != NULL; it = it->next)
	{
		GdkPixbufFormat *format;

		format = (GdkPixbufFormat*) it->data;
		if (gdk_pixbuf_format_is_writable (format) || (!savable))
		{
			gint i;
			gchar **mime_types, **pattern;
			gchar  *description;
			GtkFileFilter	*filter;
			filter	= gtk_file_filter_new ();

			/* Filter name.*/
			description	= gdk_pixbuf_format_get_description (format);
			gtk_file_filter_set_name (filter, description);
			g_free (description);

			mime_types = gdk_pixbuf_format_get_mime_types (format);
			for (i = 0; mime_types[i] != NULL; i++) 
			{
				gtk_file_filter_add_mime_type (filter, mime_types[i]);
				if (!savable)
				{
					gtk_file_filter_add_mime_type (all_img_filter, mime_types[i]);
				}
			}
			g_strfreev (mime_types);

			pattern = gdk_pixbuf_format_get_extensions (format);
			for (i = 0; pattern[i] != NULL; i++) 
			{
				gchar *tmp;
				tmp = g_strconcat ("*.", pattern[i], NULL);
				gtk_file_filter_add_pattern (filter, tmp);
				if (!savable)
				{
					gtk_file_filter_add_pattern (all_img_filter, tmp);
				}
				g_free (tmp);
			}
			g_strfreev (pattern);
	

			/* attach GdkPixbufFormat to filter, see also
			 * pixbuf_file_chooser_get_format. */
			g_object_set_data (G_OBJECT (filter), FILE_FORMAT_KEY, format );
			
			write_list = g_slist_prepend (write_list, filter);
		}
	}

	g_slist_free (list);
	write_list = g_slist_reverse (write_list);

	return write_list;
}

void
chage_type_cb (GtkComboBox *combo, GtkFileChooser * chooser)
{
	GtkTreeModel 	*model;
	GtkTreeIter 	iter;
	GtkFileChooserAction	action;

	model = gtk_combo_box_get_model (combo);

	if (gtk_combo_box_get_active_iter (combo, &iter))
	{
      	GtkFileFilter	*filter;
		
      	gtk_tree_model_get (model, &iter,
	                        FILTER_COL, &filter,
	                        -1);

		gtk_file_chooser_set_filter (chooser, filter);
	}

	action = gtk_file_chooser_get_action (chooser);
	if (action == GTK_FILE_CHOOSER_ACTION_SAVE)
	{
		if ( pixbuf_file_chooser_add_ext ( PIXBUF_FILE_CHOOSER (chooser)) )
		{
			/*To select filename*/
			gchar 			*dir;
			dir = gtk_file_chooser_get_current_folder (chooser);
			if (dir != NULL )
			{
				gtk_file_chooser_set_current_folder ( chooser, dir  );
			}
			g_free (dir);
		}
	}
}

static void
pixbuf_file_chooser_add_filter (PixbufFileChooser *chooser)
{
	GSList					*it;
	GSList					*filters;
	GtkWidget 				*combo;	
	GtkWidget 				*box;
	GtkListStore 			*store;
	GtkCellRenderer 		*renderer;
	GtkFileChooserAction	action;

	action = gtk_file_chooser_get_action (GTK_FILE_CHOOSER (chooser));

	if (action == GTK_FILE_CHOOSER_ACTION_OPEN)
	{
		GtkFileFilter			*all_file_filter;
		/* All Files Filter */
		all_file_filter = gtk_file_filter_new ();
		gtk_file_filter_set_name (all_file_filter, _("All Files"));
		gtk_file_filter_add_pattern (all_file_filter, "*");

		filters = pixbuf_get_filters (FALSE);
		filters = g_slist_prepend (filters, all_file_filter);
	}
	else
	{	
		filters = pixbuf_get_filters (TRUE);
	}

	store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_POINTER);

	/* filters */
	for (it = filters; it != NULL; it = it->next) 
	{
		GtkTreeIter 	iter;
		GtkFileFilter	*filter;

		filter =  (GtkFileFilter*)g_object_ref_sink (it->data);
		
		/* Add filter to combo */
		gtk_list_store_append (store, &iter);
		gtk_list_store_set (store, &iter,
					  		TEXT_COL, gtk_file_filter_get_name (filter),
			                FILTER_COL, filter,
					  		-1);
	}
	/*store filter list to unref*/
	chooser->priv->filters = filters;

	box		= gtk_hbox_new (FALSE,0);
	combo	= gtk_combo_box_new_with_model (GTK_TREE_MODEL (store));

    renderer = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), renderer, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), renderer,
                                    "text", TEXT_COL,
								    NULL);	

	gtk_box_pack_end (GTK_BOX(box), combo, FALSE, TRUE, 0);
	g_signal_connect 	(combo, "changed",
			  			G_CALLBACK (chage_type_cb),
			  			chooser);
	gtk_widget_show (combo);
	gtk_combo_box_set_focus_on_click (GTK_COMBO_BOX (combo), FALSE);
	gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER (chooser), box);

	chooser->priv->combo_filter = combo;
	gtk_combo_box_set_active (GTK_COMBO_BOX (combo), 1);
}


GtkWidget *
pixbuf_file_chooser_new (GtkWindow *parent, GtkFileChooserAction action)
{
	GtkWidget *chooser;
	gchar *title = NULL;

	chooser = g_object_new (PIXBUF_TYPE_FILE_CHOOSER,
				"action", action,
				"select-multiple", (action == GTK_FILE_CHOOSER_ACTION_OPEN),
				"local-only", FALSE,
				NULL);

	gtk_window_set_transient_for ( GTK_WINDOW( chooser), parent);

	switch (action) 
	{
		case GTK_FILE_CHOOSER_ACTION_OPEN:
			gtk_dialog_add_buttons (GTK_DIALOG (chooser),
						GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
						GTK_STOCK_OPEN, GTK_RESPONSE_OK,
						NULL);
			title = _("Load Image");
			break;

		case GTK_FILE_CHOOSER_ACTION_SAVE:
			gtk_dialog_add_buttons (GTK_DIALOG (chooser),
						GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
						GTK_STOCK_SAVE, GTK_RESPONSE_OK,
						NULL);
			title = _("Save Image");
			break;

		case GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER:
			gtk_dialog_add_buttons (GTK_DIALOG (chooser),
						GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
						GTK_STOCK_OPEN, GTK_RESPONSE_OK,
						NULL);
			title = _("Open Folder");
			break;

		default:
			g_assert_not_reached ();
	}


	if (last_dir[action] == NULL) 
	{
		gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser), 
		                                     g_get_user_special_dir (G_USER_DIRECTORY_PICTURES));
	}
	else
	{
		gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser), last_dir [action]);
	}


	g_signal_connect 	(chooser, "response",
			  			G_CALLBACK ((action == GTK_FILE_CHOOSER_ACTION_SAVE) ?
					 		save_response_cb : response_cb),
			  			NULL);

 	gtk_window_set_title (GTK_WINDOW (chooser), title);
	gtk_dialog_set_default_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (chooser), TRUE);

	if (action != GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER)
	{
		pixbuf_file_chooser_add_filter (PIXBUF_FILE_CHOOSER (chooser));
	}

	return chooser;
}

GdkPixbufFormat *
pixbuf_file_chooser_get_format (PixbufFileChooser *chooser)
{
	GtkFileFilter *filter;
	GdkPixbufFormat* format;

	g_return_val_if_fail (PIXBUF_IS_FILE_CHOOSER (chooser), NULL);

	filter = gtk_file_chooser_get_filter (GTK_FILE_CHOOSER (chooser));
	if (filter == NULL)
		return NULL;

	format = g_object_get_data (G_OBJECT (filter), FILE_FORMAT_KEY);

	return format;
}

void 
pixbuf_file_chooser_set_current_filter (PixbufFileChooser *chooser, const gchar *extension)
{
	GdkPixbufFormat			*format;

	g_return_if_fail (extension != NULL);
	g_return_if_fail (PIXBUF_IS_FILE_CHOOSER (chooser));

	format = pixbuf_get_format_by_suffix (extension);

	if (format != NULL)
	{
		GtkWidget *combo = chooser->priv->combo_filter;

		if (combo != NULL)
		{
			GtkTreeModel 	*model;
			GtkTreeIter 	iter;
			gboolean		valid;
			model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo));
			valid = gtk_tree_model_get_iter_first (model, &iter);
			while (valid)
		    {
			  	GtkFileFilter	*filter;
				GdkPixbufFormat	*it_format;
			  	gtk_tree_model_get (model, &iter,
					                FILTER_COL, &filter,
					                -1);
				it_format = g_object_get_data (G_OBJECT (filter), FILE_FORMAT_KEY);
				if (format == it_format)
				{
					gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combo), &iter);
					valid = FALSE;
				}
				else
				{
					valid = gtk_tree_model_iter_next (model, &iter);
				}
			}
		}
    }
}

void				
pixbuf_file_chooser_set_current_name (PixbufFileChooser *chooser, const gchar *name)
{
	g_return_if_fail (name != NULL);
	g_return_if_fail (PIXBUF_IS_FILE_CHOOSER (chooser));
	
	g_free ( chooser->priv->name );
	chooser->priv->name = g_strdup ( name );
	gtk_file_chooser_set_current_name ( GTK_FILE_CHOOSER (chooser), chooser->priv->name);
}

gchar *
pixbuf_file_chooser_get_name (PixbufFileChooser *chooser)
{
	GFile *file;
	gchar *file_name, *suffix_start;
	GdkPixbufFormat *format = NULL;

	g_return_val_if_fail (PIXBUF_IS_FILE_CHOOSER (chooser), NULL);

	file 		= gtk_file_chooser_get_file (GTK_FILE_CHOOSER (chooser));
	file_name	= g_file_get_basename (file);
	g_object_unref (file);

	suffix_start = get_suffix_from_basename (file_name);
	if (suffix_start != NULL)
	{
		format	= pixbuf_get_format_by_suffix (suffix_start);
	}
	if (format != NULL)
	{
		*--suffix_start = '\0';
	}

	return file_name;
}

static GdkPixbufFormat*
pixbuf_get_format_by_suffix (const gchar *suffix)
{
	GSList *list;
	GSList *it;
	GdkPixbufFormat *result = NULL;

	if ( suffix == NULL ) return NULL;

	list = gdk_pixbuf_get_formats ();

	for (it = list; (it != NULL) && (result == NULL); it = it->next)
	{
		GdkPixbufFormat *format;
		gchar **extensions;
		gint i;

		format = (GdkPixbufFormat*) it->data;

		extensions = gdk_pixbuf_format_get_extensions (format);
		for (i = 0; extensions[i] != NULL; i++)
		{
			if (g_ascii_strcasecmp (suffix, extensions[i]) == 0)
			{
				result = format;
				break;
			}
		}
		g_strfreev (extensions);
	}
	g_slist_free (list);

	return result;
}

static gchar*
get_suffix_from_basename (const gchar *basename)
{
	gchar *suffix_start;

	/* FIXME: does this work for all locales? */
	suffix_start = g_utf8_strrchr (basename, -1, '.');

	if (suffix_start == NULL)
		return NULL;

	return ++suffix_start;
}
