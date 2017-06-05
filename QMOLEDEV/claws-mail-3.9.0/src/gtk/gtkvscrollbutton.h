/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */
 
 /*
  * Modified by the Sylpheed Team and others 2003
  */
 

#ifndef __GTK_VSCROLLBUTTON_H__
#define __GTK_VSCROLLBUTTON_H__


#include <gdk/gdk.h>
#include <gtk/gtk.h>


#define GTK_TYPE_VSCROLLBUTTON            (gtk_vscrollbutton_get_type ())
#define GTK_VSCROLLBUTTON(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_VSCROLLBUTTON, GtkVScrollbutton))
#define GTK_VSCROLLBUTTON_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_VSCROLLBUTTON, GtkVScrollbuttonClass))
#define GTK_IS_VSCROLLBUTTON(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_VSCROLLBUTTON))
#define GTK_IS_VSCROLLBUTTON_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_VSCROLLBUTTON))

typedef struct _GtkVScrollbutton       GtkVScrollbutton;
typedef struct _GtkVScrollbuttonClass  GtkVScrollbuttonClass;

struct _GtkVScrollbutton
{
	GtkVBox vbox;
	GtkWidget *upbutton;
	GtkWidget *downbutton;
	guint need_timer : 1;
	guint32 timer;
	GtkAdjustment *adjustment;
	gint button;
	GtkScrollType scroll_type;

};

struct _GtkVScrollbuttonClass
{
	GtkVBoxClass parent_class;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


GType    gtk_vscrollbutton_get_type	(void);
GtkWidget* gtk_vscrollbutton_new	(GtkAdjustment *adjustment);
gboolean gtk_vscrollbutton_scroll	(GtkVScrollbutton *scrollbutton);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_VSCROLLBUTTON_H__ */
