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

#include "cv_ellipse_tool.h"
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
static void		draw_in_pixmap	( GdkDrawable *drawable );
static void     save_undo       ( void );

/*private data*/
typedef struct {
	gp_tool			tool;
	gp_canvas *		cv;
	GdkGC *			gcf;
	GdkGC *			gcb;
    gp_point_array  *pa;
	guint			button;
	gboolean 		is_draw;
} private_data;

static private_data		*m_priv = NULL;

static void
create_private_data( void )
{
	if (m_priv == NULL)
	{
		m_priv = g_new0 (private_data,1);
		m_priv->cv		=	NULL;
		m_priv->gcf		=	NULL;
		m_priv->gcb		=	NULL;
		m_priv->button	=	0;
		m_priv->is_draw	=	FALSE;
        m_priv->pa      =   gp_point_array_new();
	}
}

static void
destroy_private_data( void )
{
    gp_point_array_free( m_priv->pa );
	g_free (m_priv);
	m_priv = NULL;
}

gp_tool * 
tool_ellipse_init ( gp_canvas * canvas )
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

static gboolean
button_press ( GdkEventButton *event )
{
	if ( event->type == GDK_BUTTON_PRESS )
	{
		if ( event->button == LEFT_BUTTON )
		{
			m_priv->gcf = m_priv->cv->gc_fg;
			m_priv->gcb = m_priv->cv->gc_bg;
		}
		else if ( event->button == RIGHT_BUTTON )
		{
			m_priv->gcf = m_priv->cv->gc_bg;
			m_priv->gcb = m_priv->cv->gc_fg;
		}
		m_priv->is_draw = !m_priv->is_draw;
		if( m_priv->is_draw ) m_priv->button = event->button;

        gp_point_array_clear ( m_priv->pa );
		/*add two point*/
        gp_point_array_append ( m_priv->pa, (gint)event->x, (gint)event->y );
        gp_point_array_append ( m_priv->pa, (gint)event->x, (gint)event->y );

        if( !m_priv->is_draw ) gtk_widget_queue_draw ( m_priv->cv->widget );
		gtk_widget_queue_draw ( m_priv->cv->widget );
	}
	return TRUE;
}

static gboolean
button_release ( GdkEventButton *event )
{
	if ( event->type == GDK_BUTTON_RELEASE )
	{
		if( m_priv->button == event->button )
		{
			if( m_priv->is_draw )
			{
   	            save_undo ();
				draw_in_pixmap (m_priv->cv->pixmap);
				file_set_unsave ();
			}
			gtk_widget_queue_draw ( m_priv->cv->widget );
            gp_point_array_clear ( m_priv->pa );
			m_priv->is_draw = FALSE;
		}
	}
	return TRUE;
}

static gboolean
button_motion ( GdkEventMotion *event )
{
	if( m_priv->is_draw )
	{
        gp_point_array_set ( m_priv->pa, 1, (gint)event->x, (gint)event->y );
		gtk_widget_queue_draw ( m_priv->cv->widget );
	}
	return TRUE;
}

static void	
draw ( void )
{
	if ( m_priv->is_draw )
	{
		draw_in_pixmap (m_priv->cv->drawing);
	}
}

static void 
reset ( void )
{
	GdkCursor *cursor = gdk_cursor_new ( GDK_DOTBOX );
	g_assert(cursor);
	gdk_window_set_cursor ( m_priv->cv->drawing, cursor );
	gdk_cursor_unref( cursor );
	m_priv->is_draw = FALSE;
}

static void 
destroy ( gpointer data  )
{
	destroy_private_data ();
	g_print("ellipse tool destroy\n");
}

static void
draw_in_pixmap ( GdkDrawable *drawable )
{
    GdkRectangle    rect;
    GdkPoint        *p = gp_point_array_data (m_priv->pa);
	rect.x      = MIN(p[0].x,p[1].x);
	rect.y      = MIN(p[0].y,p[1].y);
	rect.width  = ABS(p[1].x-p[0].x);
	rect.height = ABS(p[1].y-p[0].y);

    if ( m_priv->cv->filled == FILLED_BACK )
	{
		gdk_draw_arc (drawable, m_priv->gcb, TRUE, rect.x, rect.y, rect.width, rect.height, 0, 23040);
	}
	else
	if ( m_priv->cv->filled == FILLED_FORE )
	{
		gdk_draw_arc (drawable, m_priv->gcf, TRUE, rect.x, rect.y, rect.width, rect.height, 0, 23040);
	}
	gdk_draw_arc (drawable, m_priv->gcf, FALSE, rect.x, rect.y, rect.width, rect.height, 0, 23040);
}

static void     
save_undo ( void )
{
    GdkRectangle    rect;
    GdkRectangle    rect_max;
    GdkPoint        *p = gp_point_array_data (m_priv->pa);
    GdkBitmap       *mask;
    GdkGC	        *gc_mask;
    gint            x,y,w,h;

    x   = MIN(p[0].x,p[1].x);
    y   = MIN(p[0].y,p[1].y);
    w   = ABS(p[1].x-p[0].x);
    h   = ABS(p[1].y-p[0].y);

    cv_get_rect_size ( &rect_max );
    gp_point_array_get_clipbox ( m_priv->pa, &rect, m_priv->cv->line_width, &rect_max );
    undo_create_mask ( rect.width, rect.height, &mask, &gc_mask );

    gdk_draw_arc ( mask, gc_mask, FALSE, 
                   x - rect.x, y - rect.y, w, h, 0, 23040);
    if ( m_priv->cv->filled != FILLED_NONE )
    {
        gdk_draw_arc ( mask, gc_mask, TRUE, 
                       x - rect.x, y - rect.y, w, h, 0, 23040);        
    }
    undo_add ( &rect, mask, NULL, TOOL_ELLIPSE );
    g_object_unref (gc_mask);
    g_object_unref (mask);
}
