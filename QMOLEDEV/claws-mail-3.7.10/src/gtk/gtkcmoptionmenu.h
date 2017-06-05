/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __GTK_CMOPTION_MENU_H__
#define __GTK_CMOPTION_MENU_H__


#include <gdk/gdk.h>
#include <gtk/gtk.h>


G_BEGIN_DECLS

#define GTK_TYPE_CMOPTION_MENU              (gtk_cmoption_menu_get_type ())
#define GTK_CMOPTION_MENU(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_CMOPTION_MENU, GtkCMOptionMenu))
#define GTK_CMOPTION_MENU_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_CMOPTION_MENU, GtkCMOptionMenuClass))
#define GTK_IS_CMOPTION_MENU(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_CMOPTION_MENU))
#define GTK_IS_CMOPTION_MENU_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_CMOPTION_MENU))
#define GTK_CMOPTION_MENU_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_CMOPTION_MENU, GtkCMOptionMenuClass))


typedef struct _GtkCMOptionMenu       GtkCMOptionMenu;
typedef struct _GtkCMOptionMenuClass  GtkCMOptionMenuClass;

struct _GtkCMOptionMenu
{
  GtkButton button;
  
  GtkWidget *menu;
  GtkWidget *menu_item;
  
  guint16 width;
  guint16 height;
};

struct _GtkCMOptionMenuClass
{
  GtkButtonClass parent_class;

  void (*changed) (GtkCMOptionMenu *option_menu);

  /* Padding for future expansion */
  void (*_gtk_reserved1) (void);
  void (*_gtk_reserved2) (void);
  void (*_gtk_reserved3) (void);
  void (*_gtk_reserved4) (void);
};


GType      gtk_cmoption_menu_get_type    (void) G_GNUC_CONST;
GtkWidget* gtk_cmoption_menu_new         (void);
GtkWidget* gtk_cmoption_menu_get_menu    (GtkCMOptionMenu *option_menu);
void       gtk_cmoption_menu_set_menu    (GtkCMOptionMenu *option_menu,
					GtkWidget     *menu);
void       gtk_cmoption_menu_remove_menu (GtkCMOptionMenu *option_menu);
gint       gtk_cmoption_menu_get_history (GtkCMOptionMenu *option_menu);
void       gtk_cmoption_menu_set_history (GtkCMOptionMenu *option_menu,
					guint          index_);


G_END_DECLS

#endif /* __GTK_CMOPTION_MENU_H__ */
