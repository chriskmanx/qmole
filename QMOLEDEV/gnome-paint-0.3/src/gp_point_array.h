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
 
#ifndef __GP_POINT_ARRAY_H__
#define __GP_POINT_ARRAY_H__

#include <gtk/gtk.h>

typedef struct _gp_point_array gp_point_array;

gp_point_array *    gp_point_array_new          ( void );
void                gp_point_array_free         ( gp_point_array *pa );
gint                gp_point_array_size         ( gp_point_array *pa );
GdkPoint *          gp_point_array_data         ( gp_point_array *pa );
void                gp_point_array_append       ( gp_point_array *pa, 
                                                 int x, int y );
void                gp_point_array_set          ( gp_point_array *pa, 
                                                 int index, int x, int y);
void                gp_point_array_clear        ( gp_point_array *pa );
void                gp_point_array_get_clipbox  ( gp_point_array *pa,
                                                 GdkRectangle *rectangle,
                                                 gint pixel_width,
                                                 GdkRectangle *rect_max);
void                gp_point_array_offset       ( gp_point_array *pa, 
                                                  gint dx, gint dy);
void                gp_point_array_copy         ( gp_point_array *src, 
                                                  gp_point_array *dst );


#endif /*__GP_POINT_ARRAY_H__*/