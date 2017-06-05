 /***************************************************************************
 *            toolbar.h
 *
 *  Sat May  9 15:13:23 2009
 *  Copyright (C) Rogério Ferro do Nascimento 2009 
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

#ifndef __TOOLBAR_H__
#define __TOOLBAR_H__

#include <gtk/gtk.h>

#include "color-picker.h"

typedef enum {
    TOOL_NONE,
    TOOL_FREE_SELECT,
    TOOL_RECT_SELECT,
    TOOL_ERASER,
    TOOL_COLOR_PICKER,
    TOOL_PENCIL,
    TOOL_AIRBRUSH,
    TOOL_BUCKET_FILL,
    TOOL_ZOOM,
    TOOL_PAINTBRUSH,
    TOOL_TEXT,
    TOOL_LINE,
    TOOL_RECTANGLE,
    TOOL_ELLIPSE,
    TOOL_CURVE,
    TOOL_POLYGON,
    TOOL_ROUNDED_RECTANGLE
} gp_tool_enum;


void toolbar_set_color_picker       ( ColorPicker *color_picker);
void toolbar_go_to_previous_tool    ( void );


/* GUI CallBack */


void on_tool_pencil_realize				(GtkToggleToolButton *button, gpointer user_data);

/*Toolbar toggled functions*/
void on_tool_free_select_toggled		(GtkToggleToolButton *button, gpointer user_data);
void on_tool_rect_select_toggled		(GtkToggleToolButton *button, gpointer user_data);
void on_tool_eraser_toggled				(GtkToggleToolButton *button, gpointer user_data);
void on_tool_color_picker_toggled		(GtkToggleToolButton *button, gpointer user_data);
void on_tool_pencil_toggled				(GtkToggleToolButton *button, gpointer user_data);
void on_tool_airbrush_toggled			(GtkToggleToolButton *button, gpointer user_data);
void on_tool_bucket_fill_toggled		(GtkToggleToolButton *button, gpointer user_data);
void on_tool_zoom_toggled				(GtkToggleToolButton *button, gpointer user_data);
void on_tool_paintbrush_toggled			(GtkToggleToolButton *button, gpointer user_data);
void on_tool_text_toggled				(GtkToggleToolButton *button, gpointer user_data);
void on_draw_line_toggled				(GtkToggleToolButton *button, gpointer user_data);
void on_draw_rectangle_toggled			(GtkToggleToolButton *button, gpointer user_data);
void on_draw_ellipse_toggled			(GtkToggleToolButton *button, gpointer user_data);
void on_draw_curve_toggled				(GtkToggleToolButton *button, gpointer user_data);
void on_draw_polygon_toggled			(GtkToggleToolButton *button, gpointer user_data);
void on_draw_rounded_rectangle_toggled  (GtkToggleToolButton *button, gpointer user_data);
/*Option Bar realize funcitons*/
void on_notebook_realize				(GtkObject *object, gpointer user_data);
void on_frame_rect_realize  			(GtkObject *object, gpointer user_data);
/*Selection Bar realize functions*/
void on_sel1_realize   					(GtkObject *object, gpointer user_data);
void on_sel2_realize   					(GtkObject *object, gpointer user_data);
/*Rect Bar realize functions*/
void on_rect0_realize   				(GtkObject *object, gpointer user_data);
void on_rect1_realize   				(GtkObject *object, gpointer user_data);
void on_rect2_realize  					(GtkObject *object, gpointer user_data);
/*Line Bar realize functions*/
void on_line0_realize   				(GtkObject *object, gpointer user_data);
void on_line1_realize   				(GtkObject *object, gpointer user_data);
void on_line2_realize   				(GtkObject *object, gpointer user_data);
void on_line3_realize   				(GtkObject *object, gpointer user_data);
void on_line4_realize   				(GtkObject *object, gpointer user_data);
/*Erase Bar realize functions*/
void on_erase0_realize   				(GtkObject *object, gpointer user_data);
void on_erase1_realize   				(GtkObject *object, gpointer user_data);
void on_erase2_realize   				(GtkObject *object, gpointer user_data);
void on_erase3_realize   				(GtkObject *object, gpointer user_data);
/*Brush Bar realize functions*/
void on_brush0_realize   				(GtkObject *object, gpointer user_data);
void on_brush1_realize   				(GtkObject *object, gpointer user_data);
void on_brush2_realize   				(GtkObject *object, gpointer user_data);
void on_brush3_realize   				(GtkObject *object, gpointer user_data);
void on_brush4_realize   				(GtkObject *object, gpointer user_data);
void on_brush5_realize   				(GtkObject *object, gpointer user_data);
void on_brush6_realize   				(GtkObject *object, gpointer user_data);
void on_brush7_realize   				(GtkObject *object, gpointer user_data);
void on_brush8_realize   				(GtkObject *object, gpointer user_data);
void on_brush9_realize   				(GtkObject *object, gpointer user_data);
void on_brush10_realize   				(GtkObject *object, gpointer user_data);
void on_brush11_realize   				(GtkObject *object, gpointer user_data);




/*Line toolbar toggled functions*/
void on_line0_toggled					(GtkToggleToolButton *button, gpointer user_data);
void on_line1_toggled					(GtkToggleToolButton *button, gpointer user_data);
void on_line2_toggled					(GtkToggleToolButton *button, gpointer user_data);
void on_line3_toggled					(GtkToggleToolButton *button, gpointer user_data);
void on_line4_toggled					(GtkToggleToolButton *button, gpointer user_data);
/*Rectangle toolbar toggled functions*/
void on_rect0_toggled					(GtkToggleToolButton *button, gpointer user_data);
void on_rect1_toggled					(GtkToggleToolButton *button, gpointer user_data);
void on_rect2_toggled					(GtkToggleToolButton *button, gpointer user_data);


#endif /*__TOOLBAR_H__*/
