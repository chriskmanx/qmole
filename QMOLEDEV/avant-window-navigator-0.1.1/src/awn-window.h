/*
 *  Copyright (C) 2007 Neil Jagdish Patel <njpatel@gmail.com>
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
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA.
 *
 *  Author : Neil Jagdish Patel <njpatel@gmail.com>
*/


#ifndef	_AWN_WINDOW_H
#define	_AWN_WINDOW_H

#include <glib.h>
#include <gtk/gtk.h>

#include "awn-gconf.h"

G_BEGIN_DECLS

#define AWN_WINDOW_TYPE      (awn_window_get_type())
#define AWN_WINDOW(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), AWN_TYPE_WINDOW, AwnWindow))
#define AWN_WINDOW_CLASS(obj)		(G_TYPE_CHECK_CLASS_CAST ((obj), AWN_WINDOW, AwnWindowClass))
#define AWN_IS_WINDOW(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), AWN_TYPE_WINDOW))
#define AWN_IS_WINDOW_CLASS(obj)	(G_TYPE_CHECK_CLASS_TYPE ((obj), AWN_TYPE_WINDOW))
#define AWN_WINDOW_GET_CLASS		(G_TYPE_INSTANCE_GET_CLASS ((obj), AWN_TYPE_WINDOW, AwnWindowClass))

typedef struct _AwnWindow AwnWindow;
typedef struct _AwnWindowClass AwnWindowClass;

struct _AwnWindow {
        GtkWindow parent;
      
};

struct _AwnWindowClass {
        GtkWindowClass parent_class;
};



GType awn_window_get_type(void);

GtkWidget *awn_window_new(AwnSettings *settings);

void awn_window_resize(AwnWindow *window);


G_END_DECLS


#endif /* _AWN_WINDOW_H */

