/***************************************************************************
 *            cv_line_tool.c
 *
 *  Wed Jun 10 21:22:13 2009
 *  Copyright  2009  rogerio
 *  <rogerio@<host>>
 ****************************************************************************/

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */
 
 #include <gtk/gtk.h>

#include "cv_line_tool.h"
#include "cv_drawing.h"
#include "file.h"
#include "undo.h"
#include "gp_point_array.h"




/*Member functions*/
static gboolean	button_press	( GdkEventButton *event );
static gboolean	button_release	( GdkEventButton *event );
static gboolean	button_motion	( GdkEventMotion *event );
static void		draw			( void );
static void		reset			( void );
static void		destroy			( gpointer data  );
static void     save_undo       ( void );

/*private data*/
typedef struct {
	gp_tool			tool;
	gp_canvas *		cv;
	GdkGC *			gc;
	gint 			x0,y0,x1,y1;
	guint			button;
	gboolean 		is_draw;
} private_data;

static private_data		*m_priv = NULL;

static void
create_private_data( void )
{
	if (m_priv == NULL)
	{
		m_priv = g_slice_new0 (private_data);
		m_priv->cv		=	NULL;
		m_priv->gc		=	NULL;
		m_priv->button	=	0;
		m_priv->is_draw	=	FALSE;
	}
}

static void
destroy_private_data( void )
{
	g_slice_free (private_data, m_priv);
	m_priv = NULL;
}




gp_tool * 
tool_line_init ( gp_canvas * canvas )
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
		if( m_priv->is_draw ) 
		{
			m_priv->button = event->button;
		}
		m_priv->x0 = m_priv->x1 = (gint)event->x;
		m_priv->y0 = m_priv->y1 = (gint)event->y;
        gtk_widget_queue_draw ( m_priv->cv->widget );
	}
	return TRUE;
}

gboolean
button_release ( GdkEventButton *event )
{
	if ( event->type == GDK_BUTTON_RELEASE )
	{
		if( m_priv->button == event->button )
		{
			if( m_priv->is_draw )
             {
                save_undo ();
				gdk_draw_line ( m_priv->cv->pixmap, m_priv->gc, m_priv->x0, m_priv->y0, m_priv->x1, m_priv->y1 );
				file_set_unsave ();
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
		m_priv->x1 = (gint)event->x;
		m_priv->y1 = (gint)event->y;
		gtk_widget_queue_draw ( m_priv->cv->widget );
	}
	return TRUE;
}

void	
draw ( void )
{
	if ( m_priv->is_draw )
	{
        gdk_draw_line ( m_priv->cv->drawing, m_priv->gc, m_priv->x0, m_priv->y0, m_priv->x1, m_priv->y1 );
	}
}

void reset ( void )
{
    GdkCursor *cursor = gdk_cursor_new ( GDK_DOTBOX );
	g_assert(cursor);
	gdk_window_set_cursor ( m_priv->cv->drawing, cursor );
	gdk_cursor_unref( cursor );
	m_priv->is_draw = FALSE;
}

void destroy ( gpointer data  )
{
	destroy_private_data ();
	g_print("line tool destroy\n");
}

static void     
save_undo ( void )
{
    GdkRectangle    rect;
    GdkRectangle    rect_max;
    GdkBitmap       *mask;
    GdkGC	        *gc_mask;
    gp_point_array  *pa  = gp_point_array_new ();

    gp_point_array_append (pa, m_priv->x0, m_priv->y0 );
    gp_point_array_append (pa, m_priv->x1, m_priv->y1 );

    cv_get_rect_size ( &rect_max );
    gp_point_array_get_clipbox ( pa, &rect, m_priv->cv->line_width, &rect_max );
     
    undo_create_mask ( rect.width, rect.height, &mask, &gc_mask );
    gdk_draw_line ( mask, gc_mask, 
                    m_priv->x0 - rect.x, m_priv->y0 - rect.y,
                    m_priv->x1 - rect.x, m_priv->y1 - rect.y );
    undo_add ( &rect, mask, NULL, TOOL_LINE );

    gp_point_array_free (pa);
    g_object_unref (gc_mask);
    g_object_unref (mask);
}

