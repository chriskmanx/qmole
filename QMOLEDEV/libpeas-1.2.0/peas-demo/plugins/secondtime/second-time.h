/*
 * second-time.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Steve Frécinaux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#ifndef __PEASDEMO_SECOND_TIME_H__
#define __PEASDEMO_SECOND_TIME_H__

#include <gtk/gtk.h>
#include <libpeas/peas.h>

G_BEGIN_DECLS

#define PEASDEMO_TYPE_SECOND_TIME         (peasdemo_second_time_get_type ())
#define PEASDEMO_SECOND_TIME(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), PEASDEMO_TYPE_SECOND_TIME, PeasDemoSecondTime))
#define PEASDEMO_SECOND_TIME_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), PEASDEMO_TYPE_SECOND_TIME, PeasDemoSecondTime))
#define PEASDEMO_IS_SECOND_TIME(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), PEASDEMO_TYPE_SECOND_TIME))
#define PEASDEMO_IS_SECOND_TIME_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), PEASDEMO_TYPE_SECOND_TIME))
#define PEASDEMO_SECOND_TIME_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), PEASDEMO_TYPE_SECOND_TIME, PeasDemoSecondTimeClass))

typedef struct _PeasDemoSecondTime       PeasDemoSecondTime;
typedef struct _PeasDemoSecondTimeClass  PeasDemoSecondTimeClass;

struct _PeasDemoSecondTime {
  PeasExtensionBase parent_instance;

  GtkWidget *window;
  GtkWidget *label;
};

struct _PeasDemoSecondTimeClass {
  PeasExtensionBaseClass parent_class;
};

GType                 peasdemo_second_time_get_type               (void) G_GNUC_CONST;
G_MODULE_EXPORT void  peas_register_types                         (PeasObjectModule *module);

G_END_DECLS

#endif /* __PEASDEMO_SECOND_TIME_H__ */
