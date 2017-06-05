/***************************************************************************
 *            undo.c
 *
 *  Sat Jan 23 14:05:22 2010
 *  Copyright  2010  Rogério Ferro do Nascimento
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

#include "undo.h"
#include "common.h"
#include "cv_drawing.h"
#include "gp-image.h"
#include "gp_point_array.h"
#include "file.h"



typedef enum
{
	UNDO_IMAGE,
	UNDO_RESIZE
} undo_type;

typedef struct
{
	GpImageData     *im_data;
	gint            x;
	gint            y;
    gp_tool_enum    tool;
} GpUndoImage;

typedef struct
{
	GpImageData *im_data_width;
	GpImageData *im_data_height;
	gint		width;
	gint		height;
} GpUndoResize;

typedef struct
{
	gpointer	t_data;
	undo_type	type;
} GpUndo;

/*statics queue*/
GQueue _undo_queue = G_QUEUE_INIT;
GQueue _redo_queue = G_QUEUE_INIT;
/**/

GQueue	*undo_queue	=	&_undo_queue;
GQueue	*redo_queue	=	&_redo_queue;

GpUndo  *undo_saved =   NULL;

static GpUndo * 	undo_image_new	   	( const GpImage *image,
                                          gint x, gint y, 
                                          gp_tool_enum  tool );
static GpUndo *     undo_resize_new     ( gp_canvas	*cv, gint width, gint height );
static void			undo_free	        ( GpUndo *undo );
static GpUndo *     draw_undo           ( GpUndo *undo );
static GpImage *    get_redo_image      ( const GpImage *image, gint x, gint y );
static void         free_redo_queue     ( void );
static void         free_undo_queue     ( void );


/* CODE */

void 
undo_create_mask ( gint width, gint height, GdkBitmap **mask, GdkGC **gc_mask )
{
    GdkColor    color;
	gp_canvas   *cv	=	cv_get_canvas();
     
    *mask 		=	gdk_pixmap_new (NULL, width, height, 1 );
    *gc_mask	=	gdk_gc_new ( *mask );
    gdk_gc_set_line_attributes ( *gc_mask, cv->line_width, GDK_LINE_SOLID, 
                             	 GDK_CAP_ROUND, GDK_JOIN_ROUND );

    color.pixel = 0;
    gdk_gc_set_foreground (*gc_mask, &color);
    gdk_draw_rectangle (*mask, *gc_mask, TRUE, 0, 0, width, height);

    color.pixel = 1;
    gdk_gc_set_foreground (*gc_mask, &color);
    return;
}

void
undo_add (GdkRectangle *rect, GdkBitmap * mask, GdkPixmap *background, gp_tool_enum  tool )
{
	GpUndo		*undo;
	GpImage     *image;
	gp_canvas	*cv	    = cv_get_canvas();

    if (mask != NULL)
    {
        image = gp_image_new_from_pixmap ( cv->pixmap, rect, TRUE );
        gp_image_set_mask ( image, mask );
    }
    else
    if ( background != NULL )
    {
        image = gp_image_new_from_pixmap ( background, rect, TRUE );
        gp_image_set_diff_pixmap ( image, cv->pixmap, rect->x, rect->y );
    }
    else
    {
        image = gp_image_new_from_pixmap ( cv->pixmap, rect, FALSE );        
    }

    
	undo	=	undo_image_new (image, rect->x, rect->y, tool );
	g_queue_push_head	( undo_queue, undo );
	g_object_unref (image);
    free_redo_queue ();
}

void 
undo_add_resize ( gint width, gint height )
{
	GpUndo      *undo;
	gp_canvas   *cv = cv_get_canvas();
    undo	=	undo_resize_new ( cv, width, height );
	g_queue_push_head	( undo_queue, undo );
    free_redo_queue ();
}

void 
undo_clear ( void )
{
    free_redo_queue ();
    free_undo_queue ();
}

/* GUI CallBack */
void 
on_menu_undo_activate ( GtkMenuItem *menuitem, gpointer user_data)
{
	GpUndo		*undo	=	g_queue_pop_head ( undo_queue );
	if ( undo != NULL )
	{
        g_queue_push_head	( redo_queue, draw_undo ( undo ) );
		undo_free (undo);
	}
    else
        g_queue_clear (undo_queue);
}

void 
on_menu_redo_activate ( GtkMenuItem *menuitem, gpointer user_data)
{
	GpUndo		*undo	=	g_queue_pop_head ( redo_queue );
	if ( undo != NULL )
	{
        g_queue_push_head	( undo_queue, draw_undo ( undo ) );
		undo_free (undo);
	}
    else
        g_queue_clear (redo_queue);
}

/*private*/
static GpUndo * 
undo_image_new ( const GpImage *image, 
                  gint x, gint y, 
                  gp_tool_enum  tool )
{
	GpUndo	*undo	=	NULL;
	if ( image != NULL )
	{
		GpUndoImage	*t_data   =	g_slice_new (GpUndoImage);
        t_data->im_data =   gp_image_get_data ( image );
		t_data->x	    =	x;
		t_data->y		=	y;
        t_data->tool    =   tool;
		undo			=	g_slice_new (GpUndo);
		undo->t_data	=	(gpointer)t_data;
		undo->type		=	UNDO_IMAGE;
        if ( file_is_save() ) undo_saved = undo;
	}
	return undo;
}

static GpUndo * 		
undo_resize_new	( gp_canvas	*cv, gint width, gint height )
{
    GdkRectangle    cv_rect;
	GpUndo	        *undo	=	NULL;
    GpUndoResize	*t_data	=	g_slice_new (GpUndoResize);
    cv_get_rect_size ( &cv_rect );

    t_data->width     =   cv_rect.width;
    t_data->height    =   cv_rect.height;

    if ( width < cv_rect.width )
    {
        GpImage         *image;
        GdkRectangle    rect;
        rect.x        = width;
        rect.width    = cv_rect.width - width;
        rect.y        = 0;
        rect.height   = cv_rect.height;
        image         = gp_image_new_from_pixmap ( cv->pixmap, &rect, FALSE );
        t_data->im_data_width  =   gp_image_get_data ( image );
        g_object_unref (image);
    }
    else
    {
        t_data->im_data_width  = NULL;
    }

    if ( height < cv_rect.height )
    {
        GpImage         *image;
        GdkRectangle    rect;
        rect.x        = 0;
        rect.width    = MIN ( width, cv_rect.width );
        rect.y        = height;
        rect.height   = cv_rect.height - height;
        image         = gp_image_new_from_pixmap ( cv->pixmap, &rect, FALSE );
        t_data->im_data_height =   gp_image_get_data ( image );
        g_object_unref (image);
    }
    else
    {
        t_data->im_data_height =   NULL;
    }    

    undo			=	g_slice_new (GpUndo);
	undo->t_data	=	(gpointer)t_data;
	undo->type		=	UNDO_RESIZE;
    if ( file_is_save() ) undo_saved = undo;
	return undo;		
}


static void
undo_free ( GpUndo *undo )
{
    if (undo->type == UNDO_IMAGE)
	{
		GpUndoImage    *t_data	=	(GpUndoImage*)undo->t_data;
        gp_image_data_free ( t_data->im_data );
    	g_slice_free (GpUndoImage, undo->t_data);
	}
    else
    if (undo->type == UNDO_RESIZE)
    {
        GpUndoResize    *t_data	=	(GpUndoResize*)undo->t_data;
        if ( t_data->im_data_width != NULL )
        {
            gp_image_data_free ( t_data->im_data_width );
        }
        if ( t_data->im_data_height != NULL )
        {
            gp_image_data_free ( t_data->im_data_height );
        }
    	g_slice_free (GpUndoResize, undo->t_data);
    }
	g_slice_free (GpUndo,undo);
	return;
}

static void
free_redo_queue ( void )
{
	GpUndo		*undo;
    while ( ( undo = g_queue_pop_head ( redo_queue ) ) != NULL )
    {
        undo_free ( undo );
    }
    g_queue_clear (redo_queue);
}

static void         
free_undo_queue ( void )
{
	GpUndo		*undo;
    while ( ( undo = g_queue_pop_head ( undo_queue ) ) != NULL )
    {
        undo_free ( undo );
    }
    g_queue_clear (undo_queue);
}

static GpImage *
get_redo_image ( const GpImage *image, gint x, gint y )
{
	gp_canvas       *cv = cv_get_canvas();
  	GpImage         *ret_image;
    GdkRectangle    rect;
    gboolean        has_alpha;
    rect.x      =   x;
    rect.y      =   y;
    rect.width  =   gp_image_get_width      ( image );
    rect.height =   gp_image_get_height     ( image );
    has_alpha   =   gp_image_get_has_alpha  ( image );
    ret_image   =   gp_image_new_from_pixmap ( cv->pixmap, &rect, has_alpha );
    if ( has_alpha )
    {
        GdkBitmap   *mask;
        mask    =   gp_image_get_mask ( image );
        gp_image_set_mask ( ret_image, mask );
        g_object_unref ( mask );            
    }
    return ret_image;
}


static GpUndo *
draw_undo ( GpUndo *undo )
{
    GpUndo      *ret_undo   = NULL;
    gp_canvas   *cv	        = cv_get_canvas();

	if (undo->type == UNDO_IMAGE)
	{
		GpUndoImage	*t_data	=	(GpUndoImage*)undo->t_data;
        GpImage     *image, *redo_image;
        image       =   gp_image_new_from_data ( t_data->im_data );
        redo_image  =   get_redo_image ( image, t_data->x, t_data->y );
        ret_undo	=	undo_image_new (redo_image, t_data->x, t_data->y, t_data->tool );
        g_object_unref (redo_image);
        gp_image_draw ( image, cv->pixmap, cv->gc_fg, t_data->x, t_data->y );
        g_object_unref ( image );
    }
    else
    if (undo->type == UNDO_RESIZE)
    {
        GdkRectangle    cv_rect;
        GpUndoResize    *t_data	=	(GpUndoResize*)undo->t_data;
        ret_undo	=	undo_resize_new (cv, t_data->width, t_data->height );
        cv_get_rect_size ( &cv_rect );
        cv_resize_pixmap ( t_data->width, t_data->height );
        if ( t_data->im_data_width != NULL )
        {
            GpImage     *image;
            image   =   gp_image_new_from_data ( t_data->im_data_width );
            gp_image_draw ( image, cv->pixmap, cv->gc_fg, cv_rect.width, 0 );
            g_object_unref ( image );
        }
        if ( t_data->im_data_height != NULL )
        {
            GpImage     *image;
            image   =   gp_image_new_from_data ( t_data->im_data_height );
            gp_image_draw ( image, cv->pixmap, cv->gc_fg, 0, cv_rect.height );
            g_object_unref ( image );
        }
    }
    if ( undo_saved == undo )   file_set_save();
    else                        file_set_unsave();
    
       
    gtk_widget_queue_draw (cv->widget);
    return ret_undo;
}
