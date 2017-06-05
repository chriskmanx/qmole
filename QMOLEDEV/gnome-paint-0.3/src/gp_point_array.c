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

#include "gp_point_array.h"

struct _gp_point_array
{
    GArray *array;
};


gp_point_array *    
gp_point_array_new ( void )
{
    gp_point_array *pa =  g_slice_new(gp_point_array);
    pa->array = g_array_new(FALSE, FALSE, sizeof(GdkPoint)); 
    return pa;
}

void                
gp_point_array_free ( gp_point_array *pa )
{
    g_array_free(pa->array, TRUE);
    g_slice_free(gp_point_array, pa);
}

gint          
gp_point_array_size ( gp_point_array *pa )
{
    return pa->array->len;
}

GdkPoint *
gp_point_array_data ( gp_point_array *pa )
{
    return (GdkPoint*)(pa->array->data);
}

void
gp_point_array_append ( gp_point_array *pa, int x, int y )
{
	GdkPoint	point	=	{ x, y };
	g_array_append_val ( pa->array, point );
    
}

void
gp_point_array_set ( gp_point_array *pa, int index, int x, int y)
{
    g_assert(index>=0 && index<pa->array->len);
    g_array_index(pa->array, GdkPoint, index).x = x;
    g_array_index(pa->array, GdkPoint, index).y = y;
}

void
gp_point_array_clear ( gp_point_array *pa )
{
    g_array_set_size(pa->array, 0);
}

void
gp_point_array_get_clipbox ( gp_point_array *pa, 
                            GdkRectangle *rectangle,
                            gint pixel_width,
                            GdkRectangle *rect_max)
{
    gint i;
    gint x_min = G_MAXINT;
    gint y_min = G_MAXINT;
    gint x_max = G_MININT;
    gint y_max = G_MININT;
    GdkPoint* pts = gp_point_array_data( pa );

    if (pa->array->len==0)
    {
       rectangle->x = 0;
       rectangle->y = 0;
       rectangle->width = 0;
       rectangle->height = 0;
       return;
    }

    for (i = 0; i < pa->array->len; i++)
    {
      if ( x_min > pts[i].x ) x_min = pts[i].x;
      if ( y_min > pts[i].y ) y_min = pts[i].y;
      if ( x_max < pts[i].x ) x_max = pts[i].x;
      if ( y_max < pts[i].y ) y_max = pts[i].y;
    }

    
    x_min -= pixel_width/2;
    y_min -= pixel_width/2;
    x_max += pixel_width/2;
    y_max += pixel_width/2;

    if ( rect_max != NULL )
    {
        gint x_mmin, y_mmin, x_mmax, y_mmax;
        x_mmin   =   rect_max->x;
        y_mmin   =   rect_max->y;
        x_mmax   =   rect_max->width + x_mmin - 1;
        y_mmax   =   rect_max->height + y_mmin - 1;
        if (x_min < x_mmin ) x_min = x_mmin;
        if (y_min < y_mmin ) y_min = y_mmin;
        if (x_max > x_mmax ) x_max = x_mmax;
        if (y_max > y_mmax ) y_max = y_mmax;
    }

    rectangle->x = x_min;
    rectangle->y = y_min;
    rectangle->width  = x_max - x_min + 1;
    rectangle->height = y_max - y_min + 1;
}

void                
gp_point_array_offset       ( gp_point_array *pa, 
                              gint dx, gint dy)
{
    GdkPoint* pts = gp_point_array_data( pa );
    gint i;
    for (i = 0; i < pa->array->len; i++)
    {
        pts[i].x += dx;
        pts[i].y += dy;
    }    
}

void
gp_point_array_copy(gp_point_array *src, gp_point_array *dst )
{
    g_array_set_size(dst->array, src->array->len);
    memcpy(dst->array->data, src->array->data, sizeof(GdkPoint) * dst->array->len);
}



