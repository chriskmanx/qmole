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


#ifndef	_AWN_TITLE_H
#define	_AWN_TITLE_H

#include <glib.h>
#include <gtk/gtk.h>

#include "awn-gconf.h"

G_BEGIN_DECLS

#define AWN_TITLE_TYPE      (awn_title_get_type())
#define AWN_TITLE(o)        (G_TYPE_CHECK_INSTANCE_CAST((o), AWN_TITLE_TYPE, AwnTitle))
#define AWN_TITLE_CLASS(c)  (G_TYPE_CHECK_CLASS_CAST((c), AWN_TITLE_TYPE, AwnTitleClass))
#define IS_AWN_TITLE(o)     (G_TYPE_CHECK_INSTANCE_TYPE((o), AWN_TITLE_TYPE))
#define IS_AWN_TITLE_CLASS  (G_TYPE_INSTANCE_GET_CLASS((o), AWN_TITLE_TYPE, AwnTitleClass))

typedef struct _AwnTitle AwnTitle;
typedef struct _AwnTitleClass AwnTitleClass;

struct _AwnTitle {
        GtkWindow win;
        
        char *text;
        gint x_pos;
      
};

struct _AwnTitleClass {
        GtkWindowClass parent_class;
        

};

GType awn_title_get_type (void);

GtkWidget *awn_title_new (AwnSettings *sets);

void awn_title_show (AwnTitle *title, const char *name, gint x, gint y);
void awn_title_hide (AwnTitle *title);

G_END_DECLS


#endif /* _AWN_TITLE_H */

