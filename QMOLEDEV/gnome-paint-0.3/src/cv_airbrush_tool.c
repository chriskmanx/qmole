/***************************************************************************
	Copyright (C) Rogério Ferro do Nascimento 2010 <rogerioferro@gmail.com>
	Copyright (C) Juan Balderas 2010

	cv_airbrush_tool.c is part of gnome-paint.

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
/*
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
*/
#include <gtk/gtk.h>
#include <stdlib.h>
#include <time.h>

#include "cv_airbrush_tool.h"
#include "gp_point_array.h"
#include "undo.h"
#include "file.h"


/* mspaint's airbrushes widths are 25, 17, 9 */
#define DIAMETER	17
#define RADIUS		((DIAMETER)/(2))

/* number of times to loop 
 * I counted 10 pixels set on a quick
 * click in mspaint.
 */
#define NTIMES	10

/* Get a random number from 0 to N. Thanks c-faq! */
#define get_rnum(N) ((int)((double)rand() / ((double)(RAND_MAX)+(1))*(N)))

static void draw_airbrush(GdkDrawable *drawable);
static gboolean timer_func(gpointer data);
static gint pt_in_circle(gint center_x, gint center_y, gint radius, gint x, gint y);

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
    GdkPoint		pt;		/* Location of mouse */
    gboolean		ret;	/* Variable to control timer function */
	gint			diam;	/* Diameter of brush circle */
    gint			rad;	/* Radius of brush circle */
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
		m_priv->diam	=	DIAMETER;
		m_priv->rad		=	RADIUS;
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
tool_airbrush_init ( gp_canvas * canvas )
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
		m_priv->pt.x = (gint)event->x;
		m_priv->pt.y = (gint)event->y;
	
		/* Offset so we draw right under mouse pointer */
		m_priv->pt.x -= m_priv->rad;
		m_priv->pt.y -= m_priv->rad;
	
		draw_airbrush(m_priv->cv->pixmap);
	
		m_priv->ret = TRUE;
		g_timeout_add(125, timer_func, NULL);
		
		printf("airbrush button_press\n");

		if( !m_priv->is_draw ) gtk_widget_queue_draw ( m_priv->cv->widget );
	}
	return TRUE;
}

static gboolean
button_release ( GdkEventButton *event )
{
	/* Kill the timer */
	m_priv->ret = FALSE;
	
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
            // gp_point_array_clear ( m_priv->pa );
		}
	}
	
	return TRUE;
}

static gboolean
button_motion ( GdkEventMotion *event )
{
	GdkModifierType state;
	gint x, y;

	if( m_priv->is_draw )
	{
        if (event->is_hint)
		{
			gdk_window_get_pointer (event->window, &x, &y, &state);
		}
		else
		{
			x = event->x;
			y = event->y;
			state = event->state;
		}
		
		m_priv->pt.x = x;
		m_priv->pt.y = y;
		
		/* Offset so we draw right under mouse pointer */
		m_priv->pt.x -= m_priv->rad;
		m_priv->pt.y -= m_priv->rad;
		
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
		draw_in_pixmap (m_priv->cv->pixmap);
	}
}


/* Location of airbrush icon */
#define AIRBRUSH_ICON PACKAGE_DATA_DIR G_DIR_SEPARATOR_S "gnome-paint" G_DIR_SEPARATOR_S "icons" \
		G_DIR_SEPARATOR_S "hicolor" G_DIR_SEPARATOR_S "16x16" G_DIR_SEPARATOR_S "actions" \
		G_DIR_SEPARATOR_S "stock-tool-airbrush.png"
static void 
reset ( void )
{
	printf("Debug: %s\n", AIRBRUSH_ICON);
	GdkCursor *cursor;
	GdkPixbuf *pixbuf;

	pixbuf = gdk_pixbuf_new_from_file (AIRBRUSH_ICON, NULL);
	cursor = gdk_cursor_new_from_pixbuf(gdk_display_get_default (), pixbuf, 0, 14);
	g_object_unref(pixbuf);
	
	if(!cursor){
		cursor = gdk_cursor_new ( GDK_CROSSHAIR );
		g_assert(cursor);
	}
	gdk_window_set_cursor ( m_priv->cv->drawing, cursor );
	gdk_cursor_unref( cursor );
	m_priv->is_draw = FALSE;
}

static void 
destroy ( gpointer data  )
{
	destroy_private_data ();
	g_print("airbrush tool destroy\n");
}

static void
draw_in_pixmap ( GdkDrawable *drawable )
{
	/*GdkPoint *	points		=	gp_point_array_data (m_priv->pa);
	gint		n_points	=	gp_point_array_size (m_priv->pa);
	*/
	draw_airbrush(drawable);
	//printf("draw_in_pixmap ");
}

static void draw_airbrush(GdkDrawable *drawable)
{
	gint x, y, i;

	for(i = 0; i < NTIMES; i++)
	{
		x = m_priv->pt.x + get_rnum(m_priv->diam);
		y = m_priv->pt.y + get_rnum(m_priv->diam);

		if(pt_in_circle(m_priv->pt.x + m_priv->rad,
						m_priv->pt.y + m_priv->rad ,
						m_priv->rad, x, y))
		{
			gdk_draw_line(drawable, m_priv->gc, x, y, x, y);
		}
	}
}

static gboolean timer_func(gpointer data)
{
	/* Kill it before it draws anything else */
	if(!m_priv->ret)
	{
		printf("Debug: timer_func ");
		return m_priv->ret;
	}
	
	draw_airbrush(m_priv->cv->pixmap);
	gtk_widget_queue_draw ( m_priv->cv->widget );
	//printf("Debug: timer_func ");

	return m_priv->ret;
}

/* Test to see if a point is within a circle */
static gint pt_in_circle(gint center_x, gint center_y, gint radius, gint x, gint y)
{
	gint square_dist;

	square_dist = ((center_x - x) * (center_x - x)) + ((center_y - y) * (center_y - y));
	return square_dist <= (radius * radius);
}

static void     
save_undo ( void )
{
/********
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
    gp_point_array_get_clipbox ( pa, &rect, m_priv->diam, &rect_max );
    undo_create_mask ( rect.width, rect.height, &mask, &gc_mask );
    gp_point_array_offset ( pa, -rect.x, -rect.y);

    printf("Debug: airbrush save_undo\n");
    gdk_draw_drawable(mask, gc_mask, m_priv->cv->pixmap, rect.x, rect.y,
    				  0, 0, rect.width, rect.height);

    undo_add ( &rect, mask, NULL);

    gp_point_array_free ( pa );
    g_object_unref (gc_mask);
    g_object_unref (mask);
  **********/
 }


