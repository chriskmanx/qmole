/*
 *  Copyright (C) 2007 Neil Jagdish Patel <njpatel@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA.
 *
 *  Author : Neil Jagdish Patel <njpatel@gmail.com>
 *
 *  Notes : This is the actual icon on the app, the "Application Icon" 
*/

#ifndef	_AWN_APP_H
#define	_AWN_APP_H

#include <glib.h>
#include <gtk/gtk.h>

#define WNCK_I_KNOW_THIS_IS_UNSTABLE 1
#include <libwnck/libwnck.h>

#include "awn-gconf.h"

typedef enum {
	AWN_APP_STATE_OPENING=0,
	AWN_APP_STATE_NORMAL,
	AWN_APP_STATE_ACTIVE,
	AWN_APP_STATE_MOUSE_HOVER,
	AWN_APP_STATE_NEEDS_ATTENTION,
	AWN_APP_STATE_CLOSING

} AwnAppState;

typedef enum {
	AWN_APP_EFFECT_OPENING=0,
	AWN_APP_EFFECT_NORMAL,
	AWN_APP_EFFECT_ACTIVE,
	AWN_APP_EFFECT_MOUSE_HOVER,
	AWN_APP_EFFECT_NEEDS_ATTENTION,
	AWN_APP_EFFECT_CLOSING

} AwnAppEffect;

typedef enum {
	AWN_APP_EFFECT_DIRECTION_UP = 0,
	AWN_APP_EFFECT_DIRECTION_DOWN
} AwnAppEffectDirection;

typedef struct {
	WnckWindow *window;
	guint xid;
	GtkWidget *alignment;
	GtkWidget *event_box;
	GtkWidget *image;
	
	GdkPixbuf *current_icon;
	GdkPixbuf *wnck_icon;
	GdkPixbuf *active_icon;
	
	AwnAppState current_state;
	AwnAppEffect current_effect;
	gboolean effect_lock;
	
	gint current_step;
	gfloat current_scale;
	AwnAppEffectDirection effect_direction;
	
	gboolean mouse_in;
	
} AwnApp;


AwnApp* awn_app_new (WnckWindow *window, AwnSettings *sets);

void awn_app_close (AwnApp *app);
void awn_app_set_needs_attention(AwnApp *app, gboolean needs_attention);

void awn_app_set_active(AwnApp *app, gboolean active);



#endif /* _AWN_APP_H */
