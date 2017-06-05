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

#include  <gdk/gdkkeysyms.h>

#include "color-picker.h"


enum {
	COLOR_CHANGED,
	RELEASED,
	LAST_SIGNAL
};


/* The cursor for the dropper */
#define DROPPER_WIDTH 17
#define DROPPER_HEIGHT 17
#define DROPPER_X_HOT 2
#define DROPPER_Y_HOT 16

#define BIG_STEP 20


static const guchar dropper_bits[] = {
  0xff, 0x8f, 0x01, 0xff, 0x77, 0x01, 0xff, 0xfb, 0x00, 0xff, 0xf8, 0x00,
  0x7f, 0xff, 0x00, 0xff, 0x7e, 0x01, 0xff, 0x9d, 0x01, 0xff, 0xd8, 0x01,
  0x7f, 0xd4, 0x01, 0x3f, 0xee, 0x01, 0x1f, 0xff, 0x01, 0x8f, 0xff, 0x01,
  0xc7, 0xff, 0x01, 0xe3, 0xff, 0x01, 0xf3, 0xff, 0x01, 0xfd, 0xff, 0x01,
  0xff, 0xff, 0x01, };

static const guchar dropper_mask[] = {
  0x00, 0x70, 0x00, 0x00, 0xf8, 0x00, 0x00, 0xfc, 0x01, 0x00, 0xff, 0x01,
  0x80, 0xff, 0x01, 0x00, 0xff, 0x00, 0x00, 0x7f, 0x00, 0x80, 0x3f, 0x00,
  0xc0, 0x3f, 0x00, 0xe0, 0x13, 0x00, 0xf0, 0x01, 0x00, 0xf8, 0x00, 0x00,
  0x7c, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x1e, 0x00, 0x00, 0x0d, 0x00, 0x00,
  0x02, 0x00, 0x00, };


static void		shutdown_eyedropper (ColorPicker *colorpicker);
static gboolean mouse_press 		(GtkWidget         *invisible,
                            		 GdkEventButton    *event,
                            		 gpointer           data);



static guint picker_signals[LAST_SIGNAL] = { 0 };


struct _ColorPickerPrivate
{
	guint		has_grab : 1;
	GdkColor	color;
	/* Window for grabbing on */
	GtkWidget   *dropper_grab_widget;
	guint32		grab_time;
};

G_DEFINE_TYPE (ColorPicker, color_picker, G_TYPE_OBJECT);

#define COLOR_PICKER_GET_PRIVATE(object)	\
	(G_TYPE_INSTANCE_GET_PRIVATE ((object), COLOR_TYPE_PICKER, ColorPickerPrivate))

static void
color_picker_init (ColorPicker *object)
{
	object->priv = COLOR_PICKER_GET_PRIVATE (object);
	object->priv->dropper_grab_widget = NULL;
}

static void
color_picker_finalize (GObject *object)
{
	ColorPickerPrivate *priv;
	priv	=	COLOR_PICKER (object)->priv;
	if ( priv->dropper_grab_widget != NULL )
	{
		g_object_unref ( priv->dropper_grab_widget );
	}

	G_OBJECT_CLASS (color_picker_parent_class)->finalize (object);
}

static void
color_picker_class_init (ColorPickerClass *klass)
{
	GObjectClass* object_class = G_OBJECT_CLASS (klass);
//	GObjectClass* parent_class = G_OBJECT_CLASS (klass);

	object_class->finalize = color_picker_finalize;

	g_type_class_add_private (object_class, sizeof (ColorPickerPrivate));

	
//	klass->color_changed = color_picker_color_changed;

	picker_signals[COLOR_CHANGED] =
		g_signal_new ("color-changed",
		              G_OBJECT_CLASS_TYPE (klass),
		              G_SIGNAL_RUN_FIRST,
		              G_STRUCT_OFFSET (ColorPickerClass, color_changed),
		              NULL, NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE, 0);

	picker_signals[RELEASED] =
		g_signal_new ("released",
		              G_OBJECT_CLASS_TYPE (klass),
		              G_SIGNAL_RUN_FIRST,
		              G_STRUCT_OFFSET (ColorPickerClass, released),
		              NULL, NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE, 0);

}



static gboolean 
color_picker_grab_broken (GtkWidget          *widget,
			   			  GdkEventGrabBroken *event,
                          gpointer   data)
{
  shutdown_eyedropper (COLOR_PICKER (data));

  return TRUE;
}



static GdkCursor *
make_picker_cursor (GdkScreen *screen)
{
  GdkCursor *cursor;

  cursor = gdk_cursor_new_from_name (gdk_screen_get_display (screen),
				     "color-picker");

  if (!cursor)
    {
      GdkColor bg = { 0, 0xffff, 0xffff, 0xffff };
      GdkColor fg = { 0, 0x0000, 0x0000, 0x0000 };
      GdkWindow *window;
      GdkPixmap *pixmap, *mask;

      window = gdk_screen_get_root_window (screen);
      
      pixmap =
	gdk_bitmap_create_from_data (window, (gchar *) dropper_bits,
				     DROPPER_WIDTH, DROPPER_HEIGHT);
      
      mask =
	gdk_bitmap_create_from_data (window, (gchar *) dropper_mask,
				     DROPPER_WIDTH, DROPPER_HEIGHT);
      
      cursor = gdk_cursor_new_from_pixmap (pixmap, mask, &fg, &bg,
					   DROPPER_X_HOT, DROPPER_Y_HOT);
      
      g_object_unref (pixmap);
      g_object_unref (mask);
    }
      
  return cursor;
}

static void
grab_color_at_mouse (GdkScreen *screen,
		     gint       x_root,
		     gint       y_root,
		     gpointer   data)
{
  GdkImage *image;
  guint32 pixel;
  ColorPicker *colorpicker = data;
  ColorPickerPrivate *priv;
  GdkColormap *colormap = gdk_screen_get_system_colormap (screen);
  GdkWindow *root_window = gdk_screen_get_root_window (screen);
  
  priv = colorpicker->priv;

  image = gdk_drawable_get_image (root_window, x_root, y_root, 1, 1);
  if (!image)
    {
      gint x, y;
      GdkDisplay *display = gdk_screen_get_display (screen);
      GdkWindow *window = gdk_display_get_window_at_pointer (display, &x, &y);
      if (!window)
	return;
      image = gdk_drawable_get_image (window, x, y, 1, 1);
      if (!image)
	return;
    }
  pixel = gdk_image_get_pixel (image, 0, 0);
  g_object_unref (image);

  gdk_colormap_query_color (colormap, pixel, &priv->color);

  g_signal_emit (colorpicker, picker_signals[COLOR_CHANGED], 0);
}

static void
shutdown_eyedropper (ColorPicker *colorpicker)
{
	ColorPickerPrivate *priv;
	priv = colorpicker->priv;    

	if (priv->has_grab)
	{
		GdkDisplay *display = gtk_widget_get_display (priv->dropper_grab_widget);
		gdk_display_keyboard_ungrab (display, priv->grab_time);
		gdk_display_pointer_ungrab (display, priv->grab_time);
		gtk_grab_remove (priv->dropper_grab_widget);

		priv->has_grab = FALSE;
	}
}


static void
mouse_motion (GtkWidget      *invisible,
			  GdkEventMotion *event,
			  gpointer        data)
{
  grab_color_at_mouse (gdk_event_get_screen ((GdkEvent *)event),
		       event->x_root, event->y_root, data); 
}

static gboolean
mouse_release (GtkWidget      *invisible,
	   		   GdkEventButton *event,
	           gpointer        data)
{
	ColorPicker *colorpicker = COLOR_PICKER (data);

  if (event->button != 1)
    return FALSE;

  grab_color_at_mouse (gdk_event_get_screen ((GdkEvent *)event),
		       event->x_root, event->y_root, data);

  shutdown_eyedropper (colorpicker);
  
  g_signal_handlers_disconnect_by_func (invisible,
					mouse_motion,
					data);
  g_signal_handlers_disconnect_by_func (invisible,
					mouse_release,
					data);

	g_signal_emit (colorpicker, picker_signals[RELEASED], 0);

  return TRUE;
}

static gboolean
key_press (GtkWidget   *invisible,
           GdkEventKey *event,
           gpointer     data)
{  
  GdkDisplay *display = gtk_widget_get_display (invisible);
  GdkScreen *screen = gdk_event_get_screen ((GdkEvent *)event);
  guint state = event->state & gtk_accelerator_get_default_mod_mask ();
  gint x, y;
  gint dx, dy;

  gdk_display_get_pointer (display, NULL, &x, &y, NULL);

  dx = 0;
  dy = 0;

  switch (event->keyval) 
    {
    case GDK_space:
    case GDK_Return:
    case GDK_ISO_Enter:
    case GDK_KP_Enter:
    case GDK_KP_Space:
      grab_color_at_mouse (screen, x, y, data);
      /* fall through */

    case GDK_Escape:
      shutdown_eyedropper (COLOR_PICKER (data));
      
      g_signal_handlers_disconnect_by_func (invisible,
					    mouse_press,
					    data);
      g_signal_handlers_disconnect_by_func (invisible,
					    key_press,
					    data);
	  g_signal_emit ( data, picker_signals[RELEASED], 0);
      
      return TRUE;

#if defined GDK_WINDOWING_X11 || defined GDK_WINDOWING_WIN32
    case GDK_Up:
    case GDK_KP_Up:
      dy = state == GDK_MOD1_MASK ? -BIG_STEP : -1;
      break;

    case GDK_Down:
    case GDK_KP_Down:
      dy = state == GDK_MOD1_MASK ? BIG_STEP : 1;
      break;

    case GDK_Left:
    case GDK_KP_Left:
      dx = state == GDK_MOD1_MASK ? -BIG_STEP : -1;
      break;

    case GDK_Right:
    case GDK_KP_Right:
      dx = state == GDK_MOD1_MASK ? BIG_STEP : 1;
      break;
#endif

    default:
      return FALSE;
    }

  gdk_display_warp_pointer (display, screen, x + dx, y + dy);
  
  return TRUE;

}

static gboolean
mouse_press (GtkWidget      *invisible,
	     GdkEventButton *event,
	     gpointer        data)
{
  /* ColorPicker *colorpicker = data; */

    
  if (event->type == GDK_BUTTON_PRESS &&
      event->button == 1)
    {

      g_signal_connect (invisible, "motion-notify-event",
                        G_CALLBACK (mouse_motion),
                        data);
      g_signal_connect (invisible, "button-release-event",
                        G_CALLBACK (mouse_release),
                        data);
      g_signal_handlers_disconnect_by_func (invisible,
					    mouse_press,
					    data);
      g_signal_handlers_disconnect_by_func (invisible,
					    key_press,
					    data);
      return TRUE;
    }

  return FALSE;
}


/**
 * color_picker_new:
 * 
 * Creates a new ColorPicker.
 * 
 * Return value: a new #ColorPicker
 **/
ColorPicker *
color_picker_new (void)
{
	ColorPicker *colorpicker;

	colorpicker = g_object_new (COLOR_TYPE_PICKER, NULL);

	return colorpicker;
}

/**
 * color_picker_get_screen_color:
 * @colorpicker: a #ColorPicker.
 * @widget: a #GtkWidget .
 *
 * Get the color of the screen.
 * 
 **/
void
color_picker_get_screen_color (ColorPicker *colorpicker, GtkWidget *widget)
{
  ColorPickerPrivate *priv = colorpicker->priv;
  GdkScreen *screen = gtk_widget_get_screen (GTK_WIDGET (widget));
  GdkCursor *picker_cursor;
  GdkGrabStatus grab_status;
  GtkWidget *grab_widget, *toplevel;
  guint32 time = gtk_get_current_event_time ();

	
  if ( priv->dropper_grab_widget == NULL )
  {
      grab_widget = gtk_window_new (GTK_WINDOW_POPUP);
      gtk_window_set_screen (GTK_WINDOW (grab_widget), screen);
      gtk_window_resize (GTK_WINDOW (grab_widget), 1, 1);
      gtk_window_move (GTK_WINDOW (grab_widget), -100, -100);
      gtk_widget_show (grab_widget);

      gtk_widget_add_events (grab_widget,
                             GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_POINTER_MOTION_MASK);
      
      toplevel = gtk_widget_get_toplevel ( widget );
  
      if (GTK_IS_WINDOW (toplevel))
	  {
		if (GTK_WINDOW (toplevel)->group)
			gtk_window_group_add_window (GTK_WINDOW (toplevel)->group, 
										 GTK_WINDOW (grab_widget));
	  }

      priv->dropper_grab_widget = grab_widget;
   }

  if (gdk_keyboard_grab (priv->dropper_grab_widget->window,
                         FALSE, time) != GDK_GRAB_SUCCESS)
    return;
  
  picker_cursor = make_picker_cursor (screen);
  grab_status = gdk_pointer_grab (priv->dropper_grab_widget->window,
				  FALSE,
				  GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_POINTER_MOTION_MASK,
				  NULL,
				  picker_cursor,
				  time);
  gdk_cursor_unref (picker_cursor);
  
  if (grab_status != GDK_GRAB_SUCCESS)
  {
      gdk_display_keyboard_ungrab (gtk_widget_get_display (widget), time);
      return;
  }

  gtk_grab_add (priv->dropper_grab_widget);
  priv->grab_time = time;
  priv->has_grab = TRUE;
  
  g_signal_connect (priv->dropper_grab_widget, "grab-broken-event",
                    G_CALLBACK (color_picker_grab_broken), colorpicker);
  g_signal_connect (priv->dropper_grab_widget, "button-press-event",
                    G_CALLBACK (mouse_press), colorpicker);
  g_signal_connect (priv->dropper_grab_widget, "key-press-event",
                    G_CALLBACK (key_press), colorpicker);
}

GdkColor	*   
color_picker_get_color (ColorPicker *colorpicker)
{
	ColorPickerPrivate *priv = colorpicker->priv;
	return &priv->color;
}
