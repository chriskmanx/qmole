/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- */
/*
 * pick_color
 * Copyright (C) Rogério Ferro do Nascimento 2010 <rogerioferro@gmail.com>
 * 
 * pick_color is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * pick_color is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _COLOR_PICKER_H_
#define _COLOR_PICKER_H_

//#include <glib-object.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define COLOR_TYPE_PICKER             (color_picker_get_type ())
#define COLOR_PICKER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), COLOR_TYPE_PICKER, ColorPicker))
#define COLOR_PICKER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), COLOR_TYPE_PICKER, ColorPickerClass))
#define COLOR_IS_PICKER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), COLOR_TYPE_PICKER))
#define COLOR_IS_PICKER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), COLOR_TYPE_PICKER))
#define COLOR_PICKER_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), COLOR_TYPE_PICKER, ColorPickerClass))

typedef struct _ColorPickerClass ColorPickerClass;
typedef struct _ColorPicker ColorPicker;
typedef struct _ColorPickerPrivate  ColorPickerPrivate;


struct _ColorPickerClass
{
	GObjectClass parent_class;

	/* Signals */
	void (*color_changed)	(ColorPicker *color_picker);
	void (*released)		(ColorPicker *color_picker);
};

struct _ColorPicker
{
	GObject parent_instance;

	ColorPickerPrivate  *priv;
};

GType color_picker_get_type (void) G_GNUC_CONST;

ColorPicker *   color_picker_new				(void);
void			color_picker_get_screen_color   (ColorPicker *colorpicker, GtkWidget *widget);
GdkColor	*   color_picker_get_color			(ColorPicker *colorpicker);


G_END_DECLS

#endif /* _COLOR_PICKER_H_ */
