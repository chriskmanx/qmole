/* Gnome Paint - 
 *
 * Copyright (C) 2009 The Free Software Foundation
 *
 * Author: Rogério Ferro <rogerioferro@gmail.com>
 *
 * Based on eog code (src/eog-file-chooser.h) by:
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

#ifndef _PIXBUF_FILE_CHOOSER_H_
#define _PIXBUF_FILE_CHOOSER_H_

#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

#define PIXBUF_TYPE_FILE_CHOOSER          (pixbuf_file_chooser_get_type ())
#define PIXBUF_FILE_CHOOSER(o)            (G_TYPE_CHECK_INSTANCE_CAST ((o), PIXBUF_TYPE_FILE_CHOOSER, PixbufFileChooser))
#define PIXBUF_FILE_CHOOSER_CLASS(k)      (G_TYPE_CHECK_CLASS_CAST((k), PIXBUF_TYPE_FILE_CHOOSER, PixbufFileChooserClass))

#define PIXBUF_IS_FILE_CHOOSER(o)         (G_TYPE_CHECK_INSTANCE_TYPE ((o), PIXBUF_TYPE_FILE_CHOOSER))
#define PIXBUF_IS_FILE_CHOOSER_CLASS(k)   (G_TYPE_CHECK_CLASS_TYPE ((k), PIXBUF_TYPE_FILE_CHOOSER))
#define PIXBUF_FILE_CHOOSER_GET_CLASS(o)  (G_TYPE_INSTANCE_GET_CLASS ((o), PIXBUF_TYPE_FILE_CHOOSER, PixbufFileChooserClass))

typedef struct _PixbufFileChooser         PixbufFileChooser;
typedef struct _PixbufFileChooserClass    PixbufFileChooserClass;
typedef struct _PixbufFileChooserPrivate  PixbufFileChooserPrivate;

struct _PixbufFileChooser
{
	GtkFileChooserDialog  parent;

	PixbufFileChooserPrivate *priv;
};

struct _PixbufFileChooserClass
{
	GtkFileChooserDialogClass  parent_class;
};


GType		 		pixbuf_file_chooser_get_type			(void) G_GNUC_CONST;
GtkWidget*			pixbuf_file_chooser_new					(GtkWindow *parent, GtkFileChooserAction action);
GdkPixbufFormat*	pixbuf_file_chooser_get_format			(PixbufFileChooser *chooser);
void 				pixbuf_file_chooser_set_current_filter 	(PixbufFileChooser *chooser, const gchar *extension);
void				pixbuf_file_chooser_set_current_name			(PixbufFileChooser *chooser, const gchar *name);
gchar*				pixbuf_file_chooser_get_name			(PixbufFileChooser *chooser);

G_END_DECLS

#endif /* _PIXBUF_FILE_CHOOSER_H_ */
