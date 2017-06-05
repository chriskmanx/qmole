/***************************************************************************
    Copyright (C) Rogério Ferro do Nascimento 2010 <rogerioferro@gmail.com>
    Contributed by Juan Balderas

    This file is part of gnome-paint.

    gnome-paint is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    gnome-paint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with gnome-paint.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************/
 
 #include <gtk/gtk.h>

#include "cv_flood_fill_tool.h"
#include "file.h"
#include "cv_drawing.h"
#include "pixbuf_util.h"
//#include "color.h"

guint get_fg_color_from_gc(GdkGC *gc);

/*Member functions*/
static gboolean	button_press	( GdkEventButton *event );
static gboolean	button_release	( GdkEventButton *event );
static gboolean	button_motion	( GdkEventMotion *event );
static void		draw			( void );
static void		reset			( void );
static void		destroy			( gpointer data  );

/*private data*/
typedef struct {
	gp_tool			tool;
	gp_canvas *		cv;
	GdkGC *			gc;
	gint 			x0,y0;
	guint			button;
	gboolean 		is_draw;
	guint			fill_color;
} private_data;

static private_data		*m_priv = NULL;

static void
create_private_data( void )
{
	if (m_priv == NULL)
	{
		m_priv = g_new0 (private_data,1);
		m_priv->cv		=	NULL;
		m_priv->gc		=	NULL;
		m_priv->button	=	0;
		m_priv->is_draw	=	FALSE;
	}
}

static void
destroy_private_data( void )
{
	g_free (m_priv);
	m_priv = NULL;
}


gp_tool * 
tool_flood_fill_init ( gp_canvas * canvas )
{
	create_private_data ();
	m_priv->cv					= canvas;
	m_priv->tool.button_press	= button_press;
	m_priv->tool.button_release	= button_release;
	m_priv->tool.button_motion	= button_motion;
	m_priv->tool.draw			= draw;
	m_priv->tool.reset			= reset;
	m_priv->tool.destroy		= destroy;
	return &m_priv->tool;
}

gboolean
button_press ( GdkEventButton *event )
{
	if ( event->type == GDK_BUTTON_PRESS )
	{
		if ( event->button == LEFT_BUTTON )
		{
			m_priv->gc = m_priv->cv->gc_fg;
		}
		else if ( event->button == RIGHT_BUTTON )
		{
			m_priv->gc = m_priv->cv->gc_bg;
		}

		m_priv->is_draw = !m_priv->is_draw;
		
		if( m_priv->is_draw ) m_priv->button = event->button;
		m_priv->x0 = (gint)event->x;
		m_priv->y0 = (gint)event->y;
		if( !m_priv->is_draw ) gtk_widget_queue_draw ( m_priv->cv->widget );

		m_priv->fill_color = get_fg_color_from_gc(m_priv->gc);
	}
	return TRUE;
}

gboolean
button_release ( GdkEventButton *event )
{
	GdkPixbuf *pixbuf;

	if ( event->type == GDK_BUTTON_RELEASE )
	{
		if( m_priv->button == event->button )
		{
			if( m_priv->is_draw )
			{
				pixbuf = cv_get_pixbuf ( );
				if(GDK_IS_PIXBUF ( pixbuf ) )
				{
					if(!gdk_pixbuf_get_has_alpha ( pixbuf ) )
					{
						GdkPixbuf *tmp ;
						tmp = gdk_pixbuf_add_alpha(pixbuf, FALSE, 0, 0, 0);
						g_object_unref(pixbuf);
						pixbuf = tmp;
					}

					fill_draw( GDK_DRAWABLE( m_priv->cv->pixmap ), 
				    	      m_priv->gc, 
				    	      m_priv->fill_color, 
				    	      m_priv->x0, 
			    		      m_priv->y0);
					
					g_object_unref(pixbuf);

					file_set_unsave ();
				}
			}
			gtk_widget_queue_draw ( m_priv->cv->widget );
			m_priv->is_draw = FALSE;
		}
	}
	return TRUE;
}

gboolean
button_motion ( GdkEventMotion *event )
{
	if( m_priv->is_draw )
	{

	}
	return TRUE;
}

void	
draw ( void )
{
	GdkPixbuf *pixbuf;

	if ( m_priv->is_draw )
	{
		pixbuf = cv_get_pixbuf ( );
		if(GDK_IS_PIXBUF(pixbuf))
		{
			if(!gdk_pixbuf_get_has_alpha (pixbuf))
			{
				GdkPixbuf *tmp ;

				tmp = gdk_pixbuf_add_alpha( pixbuf, FALSE, 0, 0, 0 );
				g_object_unref(pixbuf);
				pixbuf = tmp;
			}

			fill_draw( GDK_DRAWABLE( m_priv->cv->pixmap ), 
			          m_priv->gc, 
			          m_priv->fill_color, 
			          m_priv->x0, 
			          m_priv->y0);
			g_object_unref(pixbuf);

			file_set_unsave ();
		}
	}
}

void reset ( void )
{
	GdkCursor *cursor = gdk_cursor_new ( GDK_CROSSHAIR );
	g_assert(cursor);
	gdk_window_set_cursor ( m_priv->cv->drawing, cursor );
	gdk_cursor_unref( cursor );
	m_priv->is_draw = FALSE;
}

void destroy ( gpointer data  )
{
	destroy_private_data ();
	g_print("flood fill tool destroy\n");
}

guint get_fg_color_from_gc(GdkGC *gc)
{
	GdkGCValues values;
	guint color = 0;
	GdkColor tmp;

	gdk_gc_get_values(gc, &values);

	tmp = values.foreground;
	gdk_colormap_query_color (gtk_widget_get_colormap(m_priv->cv->widget), 
	                          values.foreground.pixel, 
	                          &tmp);

	values.foreground = tmp;
	values.foreground.red /= 256;
	values.foreground.green /= 256;
	values.foreground.blue /= 256;
	
	color = col((guchar)values.foreground.red,
	            (guchar)values.foreground.green,
	            (guchar)values.foreground.blue);
	
	return color;
}


