/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- */
/*
 * color.c
 * Copyright (C) Rogério Ferro do Nascimento 2009 <rogerioferro@gmail.com>
 * 
 * color.c is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * color.c is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "common.h"
#include "color.h"
#include "cv_drawing.h"
#include "pixbuf_util.h"

#include <glib/gi18n.h>

static  GtkWidget	*background_widget = NULL;
static  GdkColor	background_color;
static  GtkWidget	*foreground_widget = NULL;
static  GdkColor	foreground_color;

#define NUM_PALETTES	28

static guint16 init_palette_values[NUM_PALETTES][3] = 
{
	/*First line*/
	{0x0000,0x0000,0x0000},{0x4600,0x4600,0x4600},{0x7800,0x7800,0x7800},
	{0x9900,0x0000,0x3000},{0xED00,0x1C00,0x2400},{0xFFFF,0x7E00,0x0000},
	{0xFFFF,0xC200,0x0E00},{0xFFFF,0xF200,0x0000},{0xA800,0xE600,0x1D00},
	{0x2200,0xB100,0x4C00},{0x0000,0xB700,0xEF00},{0x4D00,0x6D00,0xF300},
	{0x2F00,0x3600,0x9900},{0x6F00,0x3100,0x9800},
	/*Second line*/
	{0xFFFF,0xFFFF,0xFFFF},{0xDC00,0xDC00,0xDC00},{0xB400,0xB400,0xB400},
	{0x9C00,0x5A00,0x3C00},{0xFFFF,0xA300,0xB100},{0xE500,0xAA00,0x7A00},
	{0xF500,0xE400,0x9C00},{0xFFFF,0xF900,0xBD00},{0xD300,0xF900,0xBC00},
	{0x9D00,0xBB00,0x6100},{0x9900,0xD900,0xEA00},{0x7000,0x9A00,0xD100},
	{0x5400,0x6D00,0x8E00},{0xB500,0xA500,0xD500}
};

static  GtkWidget   *pallete_widgets[NUM_PALETTES] = 
{
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL
};

static  GdkColor	pallete_colors[NUM_PALETTES];

/* private functions */
static void background_set_color_from_palette   ( guint palette );
static void foreground_set_color_from_palette   ( guint palette );
static void palette_color_picker				( guint palette );
static void color_dialog						( GdkColor *color, gchar * title );
static void background_show						( void );
static void foreground_show						( void );
static void pallete_show						( guint palette );


/*
 *   CODE
 */

void foreground_set_color  ( GdkColor *color )
{
	foreground_color.red	=   color->red;
	foreground_color.green	=   color->green;
	foreground_color.blue	=   color->blue;
	foreground_show ();
}


void foreground_set_color_from_rgb  ( guint color )
{
   foreground_color.red   = getr(color);
   foreground_color.red   <<= 8;
   foreground_color.green = getg(color);
   foreground_color.green <<= 8;
   foreground_color.blue  = getb(color);
   foreground_color.blue  <<= 8;
   foreground_show ();
   /*g_debug("%s %d", __FILE__, __LINE__);*/
}


void
on_background_color_picker_realize (GtkWidget *widget, gpointer user_data)
{
	background_widget		=   widget;
	background_color.red	=   0xFFFF;
	background_color.green  =   0xFFFF;
	background_color.blue   =   0xFFFF;
	background_show ();
}

void
on_foreground_color_picker_realize (GtkWidget *widget, gpointer user_data)
{
	foreground_widget		=   widget;
	foreground_color.red	=   0x0000;
	foreground_color.green  =   0x0000;
	foreground_color.blue   =   0x0000;
	foreground_show ();
}

/*
 * Initialize the colors on the color palette tool bar.
 */
void
on_color_palette_entry_realize (GtkWidget *widget, gpointer user_data) 
{
	const gchar *name;
	guint palette;
	name = gtk_buildable_get_name ( widget );
	palette = ( (guint)(name[0] - '0') * 10 ) + (guint)(name[1] - '0');
	g_return_if_fail( palette < NUM_PALETTES );
	pallete_widgets[palette]		= widget;
	pallete_colors[palette].red		= init_palette_values[palette][0];
	pallete_colors[palette].green   = init_palette_values[palette][1];
	pallete_colors[palette].blue	= init_palette_values[palette][2];
	pallete_show (palette);
}


gboolean 
on_background_color_picker_button_release_event ( GtkWidget			*widget, 
												  GdkEventButton	*event,
												  gpointer			user_data )
{
	if ( event->button == LEFT_BUTTON )
	{
		color_dialog( &background_color, _("Select Background Color") );
		background_show ();
	}
	return TRUE;
}

gboolean 
on_foreground_color_picker_button_release_event	(   GtkWidget	   *widget, 
													GdkEventButton *event,
													gpointer       user_data )
{
	if ( event->button == LEFT_BUTTON )
	{
		color_dialog( &foreground_color, _("Select Foreground Color") );
		foreground_show ();
	}
	return TRUE;
}

gboolean 
on_color_palette_entry_button_press_event ( GtkWidget	   *widget, 
											GdkEventButton *event,
											gpointer       user_data )
{
	const gchar *name;
	guint i;
	name = gtk_buildable_get_name ( widget );
	i = ( (guint)(name[0] - '0') * 10 ) + (guint)(name[1] - '0');
	
	if ( event->type == GDK_2BUTTON_PRESS )
	{
		palette_color_picker ( i );
	}
	if ( event->button == LEFT_BUTTON )
	{
		foreground_set_color_from_palette ( i );
		notify_brush_of_fg_color_change();
	}
	else if ( event->button == RIGHT_BUTTON )
	{
		background_set_color_from_palette ( i );
	}

	
	return TRUE;
}


/* Private functions */

static void background_set_color_from_palette  ( guint palette )
{
	g_return_if_fail( palette < NUM_PALETTES );
	background_color.red	=   pallete_colors[palette].red;
	background_color.green  =   pallete_colors[palette].green;
	background_color.blue   =   pallete_colors[palette].blue;
	background_show ();	
}

static void foreground_set_color_from_palette  ( guint palette )
{
	g_return_if_fail( palette < NUM_PALETTES );
	foreground_color.red	=   pallete_colors[palette].red;
	foreground_color.green  =   pallete_colors[palette].green;
	foreground_color.blue   =   pallete_colors[palette].blue;
	foreground_show ();	
}


static void palette_color_picker ( guint palette )
{
	g_return_if_fail( palette < NUM_PALETTES );
	color_dialog( &pallete_colors[palette], _("Select Color") );
	pallete_show (palette);
}	

static void 
color_dialog( GdkColor *color, gchar * title )
{
	GtkResponseType result;
	GtkColorSelection *colorsel;
	GtkWidget *dialog = gtk_color_selection_dialog_new( title );
	colorsel = GTK_COLOR_SELECTION(
                   GTK_COLOR_SELECTION_DIALOG(dialog)->colorsel );
	gtk_color_selection_set_has_palette ( colorsel, TRUE );
	gtk_color_selection_set_current_color ( colorsel, color );
	result = gtk_dialog_run(GTK_DIALOG(dialog));
	if (result == GTK_RESPONSE_OK)
	{
		gtk_color_selection_get_current_color(colorsel, color);
	} 
	gtk_widget_destroy(dialog);
}

static void 
background_show ( void )
{
	g_return_if_fail( background_widget != NULL );
	gtk_widget_modify_bg ( background_widget, GTK_STATE_NORMAL , &background_color );
	cv_set_color_bg ( &background_color );
}

static void 
foreground_show ( void )
{
	g_return_if_fail( foreground_widget != NULL );
	gtk_widget_modify_bg ( foreground_widget, GTK_STATE_NORMAL , &foreground_color );
	cv_set_color_fg ( &foreground_color );
}

static void 
pallete_show ( guint palette )
{
	g_return_if_fail( pallete_widgets[palette] != NULL );
	gtk_widget_modify_bg ( pallete_widgets[palette], GTK_STATE_NORMAL , &pallete_colors[palette] );
}