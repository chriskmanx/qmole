/***************************************************************************
 *            cv_pencil_tool.c
 *
 *  Thu Set 10 22:35:13 2009
 *  Copyright  2009  Rogério Ferro do Nascimento
 *  <rogerioferro@gmail.com>
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

#include "cv_pencil_tool.h"
#include "gp_point_array.h"
#include "undo.h"
#include "file.h"

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
	GdkGC *			gc;
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
		m_priv->gc		=	NULL;
		m_priv->button	=	NONE_BUTTON;
        m_priv->pa      =   gp_point_array_new();
		m_priv->is_draw	=	FALSE;
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
tool_pencil_init ( gp_canvas * canvas )
{
	create_private_data ();
	m_priv->cv					=	canvas;
	m_priv->tool.button_press	= 	button_press;
	m_priv->tool.button_release	= 	button_release;
	m_priv->tool.button_motion	= 	button_motion;
	m_priv->tool.draw			= 	draw;
	m_priv->tool.reset			= 	reset;
	m_priv->tool.destroy		= 	destroy;
	return &m_priv->tool;
}

static gboolean
button_press ( GdkEventButton *event )
{
	if ( event->type == GDK_BUTTON_PRESS )
	{
		if ( event->button == LEFT_BUTTON )
		{
			m_priv->gc = m_priv->cv->gc_fg_pencil;
		}
		else if ( event->button == RIGHT_BUTTON )
		{
			m_priv->gc = m_priv->cv->gc_bg_pencil;
		}
		m_priv->is_draw = !m_priv->is_draw;
		if( m_priv->is_draw ) m_priv->button = event->button;
        gp_point_array_clear ( m_priv->pa );
        gp_point_array_append ( m_priv->pa, (gint)event->x, (gint)event->y );
		if( !m_priv->is_draw ) gtk_widget_queue_draw ( m_priv->cv->widget );
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
                gp_point_array_append ( m_priv->pa, (gint)event->x, (gint)event->y );
                save_undo ();
				draw_in_pixmap (m_priv->cv->pixmap);
				file_set_unsave ();
			}
			gtk_widget_queue_draw ( m_priv->cv->widget );
			m_priv->is_draw = FALSE;
            gp_point_array_clear ( m_priv->pa );
		}
	}
	return TRUE;
}

static gboolean
button_motion ( GdkEventMotion *event )
{
	if( m_priv->is_draw )
	{
        gp_point_array_append ( m_priv->pa, (gint)event->x, (gint)event->y );
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
	GdkCursor *cursor = gdk_cursor_new ( GDK_PENCIL );
	g_assert(cursor);
	gdk_window_set_cursor ( m_priv->cv->drawing, cursor );
	gdk_cursor_unref( cursor );
	m_priv->is_draw = FALSE;
}

static void 
destroy ( gpointer data  )
{
	destroy_private_data ();
	g_print("pencil tool destroy\n");
}

static void
draw_in_pixmap ( GdkDrawable *drawable )
{
	GdkPoint *	points		=	gp_point_array_data (m_priv->pa);
	gint		n_points	=	gp_point_array_size (m_priv->pa);
	gdk_draw_lines (drawable, m_priv->gc, points, n_points );
}

static void     
save_undo ( void )
{
    GdkRectangle    rect;
    GdkRectangle    rect_max;
    GdkBitmap       *mask;
    GdkGC	        *gc_mask;
    gp_point_array  *pa;
    GdkPoint        *points;
    gint	        n_points;

    pa = gp_point_array_new ();
    gp_point_array_copy ( m_priv->pa, pa );
    points 		=	gp_point_array_data ( pa );
    n_points	=	gp_point_array_size ( pa );

    cv_get_rect_size ( &rect_max );
    gp_point_array_get_clipbox ( pa, &rect, m_priv->cv->line_width, &rect_max );
    undo_create_mask ( rect.width, rect.height, &mask, &gc_mask );
    gp_point_array_offset ( pa, -rect.x, -rect.y);

    gdk_draw_lines ( mask, gc_mask, points, n_points);

    undo_add ( &rect, mask, NULL, TOOL_LINE );

    gp_point_array_free ( pa );
    g_object_unref (gc_mask);
    g_object_unref (mask);
 }


