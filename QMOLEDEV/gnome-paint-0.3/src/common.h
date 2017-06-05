/***************************************************************************
 *            common.h
 *
 *  Wed May 27 15:28:49 2009
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

#ifndef __COMMON_H__
#define __COMMON_H__

#include <gtk/gtk.h>

/* Mouse Button Type */
typedef enum
{
	NONE_BUTTON		=	0,
    LEFT_BUTTON		=   1,
    MIDDLE_BUTTON	=   2,
	RIGHT_BUTTON	=   3
} gp_button;

/* Filled enum */
typedef enum
{
	FILLED_NONE,
	FILLED_BACK,
	FILLED_FORE
} gp_filled;

/* Canvas Type */
typedef struct
{
	GtkWidget *		toplevel;
	GtkWidget *		widget;
	GdkPixmap *		drawing;
	GdkGC *			gc_fg;
	GdkGC *			gc_bg;
	GdkGC *			gc_fg_pencil;
	GdkGC *			gc_bg_pencil;
	GdkPixmap *		pixmap;
	gp_filled		filled;
	gint			line_width;
} gp_canvas;

/* Tool Type*/
typedef struct
{
	gboolean	(*button_press)		( GdkEventButton *event );
	gboolean	(*button_release)	( GdkEventButton *event );
	gboolean	(*button_motion)	( GdkEventMotion *event );
	void		(*draw)				( void );
	void		(*reset)			( void );
	void		(*destroy)			( gpointer data );
} gp_tool;

#endif /*__COMMON_H__*/
