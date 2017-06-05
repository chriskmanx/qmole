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
*/

#include "awn-gconf.h"

#include "config.h"


#define BAR_PATH		"/apps/avant-window-navigator/bar"
#define BAR_ROUNDED_CORNERS	"/apps/avant-window-navigator/bar/rounded_corners"	/* bool */
#define BAR_CORNER_RADIUS 	"/apps/avant-window-navigator/bar/corner_radius" 	/* float */
#define BAR_RENDER_PATTERN	"/apps/avant-window-navigator/bar/render_pattern"	/* bool */
#define BAR_PATTERN_URI		"/apps/avant-window-navigator/bar/pattern_uri" 		/* string */
#define BAR_PATTERN_ALPHA 	"/apps/avant-window-navigator/bar/pattern_alpha" 	/* float */
#define BAR_GLASS_STEP_1	"/apps/avant-window-navigator/bar/glass_step_1"		/*string */
#define BAR_GLASS_STEP_2	"/apps/avant-window-navigator/bar/glass_step_2"		/*string */
#define BAR_GLASS_HISTEP_1	"/apps/avant-window-navigator/bar/glass_histep_1"		/*string */
#define BAR_GLASS_HISTEP_2	"/apps/avant-window-navigator/bar/glass_histep_2"		/*string */
#define BAR_BORDER_COLOR	"/apps/avant-window-navigator/bar/border_color"		/*string */
#define BAR_HILIGHT_COLOR	"/apps/avant-window-navigator/bar/hilight_color"		/*string */

#define WINMAN_PATH		"/apps/avant-window-navigator/window_manager"
#define WINMAN_SHOW_ALL_WINS	"/apps/avant-window-navigator/window_manager/show_all_windows" /*bool*/

#define APP_PATH		"/apps/avant-window-navigator/app"
#define APP_ACTIVE_PNG		"/apps/avant-window-navigator/app/active_png" /*bool*/

#define TITLE_PATH		"/apps/avant-window-navigator/title"
#define TITLE_TEXT_COLOR	"/apps/avant-window-navigator/title/text_color" /*color*/
#define TITLE_SHADOW_COLOR	"/apps/avant-window-navigator/title/shadow_color" /*color*/
#define TITLE_ITALIC		"/apps/avant-window-navigator/title/italic" /*bool*/
#define TITLE_BOLD		"/apps/avant-window-navigator/title/bold" /*bool*/
#define TITLE_FONT_SIZE		"/apps/avant-window-navigator/title/font_size" /*bool*/

/* globals */
static AwnSettings *settings		= NULL;
static GConfClient *client 		= NULL;
static GtkWidget *update_window		= NULL;

/* prototypes */
static void awn_load_bool(GConfClient *client, const gchar* key, gboolean *data);
static void awn_load_string(GConfClient *client, const gchar* key, gchar **data);
static void awn_load_float(GConfClient *client, const gchar* key, gfloat *data);
static void awn_load_color(GConfClient *client, const gchar* key, AwnColor *color);

static void awn_notify_bool (GConfClient *client, guint cid, GConfEntry *entry, gboolean* data);
static void awn_notify_string (GConfClient *client, guint cid, GConfEntry *entry, gchar** data);
static void awn_notify_float (GConfClient *client, guint cid, GConfEntry *entry, gfloat* data);
static void awn_notify_color (GConfClient *client, guint cid, GConfEntry *entry, AwnColor *color);

static void hex2float(char* HexColor, float* FloatColor);

AwnSettings* 
awn_gconf_new()
{
	AwnSettings *s = NULL;
	
	s = g_new(AwnSettings, 1);
	settings = s;
	client = gconf_client_get_default();
	
	
	/* Bar settings first */
	gconf_client_add_dir(client, BAR_PATH, GCONF_CLIENT_PRELOAD_NONE, NULL);
	
	awn_load_bool(client, BAR_ROUNDED_CORNERS, &s->rounded_corners);
	awn_load_float(client, BAR_CORNER_RADIUS, &s->corner_radius);	
	awn_load_bool(client, BAR_RENDER_PATTERN, &s->render_pattern);	
	awn_load_string(client, BAR_PATTERN_URI, &s->pattern_uri);
	awn_load_float(client, BAR_PATTERN_ALPHA, &s->pattern_alpha);
	
	awn_load_color(client, BAR_GLASS_STEP_1, &s->g_step_1);
	awn_load_color(client, BAR_GLASS_STEP_2, &s->g_step_2);
	awn_load_color(client, BAR_GLASS_HISTEP_1, &s->g_histep_1);
	awn_load_color(client, BAR_GLASS_HISTEP_2, &s->g_histep_2);
	
	awn_load_color(client, BAR_BORDER_COLOR, &s->border_color);
	awn_load_color(client, BAR_HILIGHT_COLOR, &s->hilight_color);
	
	/* Window Manager settings */
	gconf_client_add_dir(client, WINMAN_PATH, GCONF_CLIENT_PRELOAD_NONE, NULL);
	awn_load_bool(client, WINMAN_SHOW_ALL_WINS, &s->show_all_windows);
	
	/* App settings */
	gconf_client_add_dir(client, APP_PATH, GCONF_CLIENT_PRELOAD_NONE, NULL);
	awn_load_string(client, APP_ACTIVE_PNG, &s->active_png);
	
	/* Title settings */
	gconf_client_add_dir(client, TITLE_PATH, GCONF_CLIENT_PRELOAD_NONE, NULL);
	awn_load_color(client, TITLE_TEXT_COLOR, &s->text_color);
	awn_load_color(client, TITLE_SHADOW_COLOR, &s->shadow_color);
	awn_load_bool(client, TITLE_ITALIC, &s->italic);
	awn_load_bool(client, TITLE_BOLD, &s->bold);
	awn_load_float(client, TITLE_FONT_SIZE, &s->font_size);	
	return s;
}

static void
awn_update_window ()
{
	/* TODO : Force an update of the bar */
}

static void 
awn_notify_bool (GConfClient *client, guint cid, GConfEntry *entry, gboolean* data)
{
	GConfValue *value = NULL;
	
	value = gconf_entry_get_value(entry);
	*data = gconf_value_get_bool(value);
}

static void 
awn_notify_string (GConfClient *client, guint cid, GConfEntry *entry, gchar** data)
{
	GConfValue *value = NULL;
	
	value = gconf_entry_get_value(entry);
	*data = gconf_value_get_string(value);
	
}

static void 
awn_notify_float (GConfClient *client, guint cid, GConfEntry *entry, gfloat* data)
{
	GConfValue *value = NULL;
	
	value = gconf_entry_get_value(entry);
	*data = gconf_value_get_float(value);
	
}

static void 
awn_notify_color (GConfClient *client, guint cid, GConfEntry *entry, AwnColor* color)
{
	GConfValue *value = NULL;
	float colors[4];
	
	value = gconf_entry_get_value(entry);
	hex2float(gconf_value_get_string(value), colors);
	
	color->red = colors[0];
	color->green = colors[1];
	color->blue = colors[2];
	color->alpha = colors[3];
	
}


static void
awn_load_bool(GConfClient *client, const gchar* key, gboolean *data)
{
	*data = gconf_client_get_bool(client, key, NULL);
	gconf_client_notify_add (client, key, awn_notify_bool, data, NULL, NULL);
	awn_update_window ();
}

static void
awn_load_string(GConfClient *client, const gchar* key, gchar **data)
{
	*data = gconf_client_get_string(client, key, NULL);
	gconf_client_notify_add (client, key, awn_notify_string, data, NULL, NULL);
	awn_update_window ();
}

static void
awn_load_float(GConfClient *client, const gchar* key, gfloat *data)
{
	*data = gconf_client_get_float(client, key, NULL);
	gconf_client_notify_add (client, key, awn_notify_float, data, NULL, NULL);
	awn_update_window ();
}

static void
awn_load_color(GConfClient *client, const gchar* key, AwnColor *color)
{
	float colors[4];
	
	hex2float (gconf_client_get_string(client, key, NULL), colors);
	color->red = colors[0];
	color->green = colors[1];
	color->blue = colors[2];
	color->alpha = colors[3];
	
	gconf_client_notify_add (client, key, awn_notify_color, color, NULL, NULL);
	awn_update_window ();
}

void 
awn_gconf_set_window_to_update(GtkWidget *window)
{
	update_window = window;
}

static int 
getdec(char hexchar)
{
   if ((hexchar >= '0') && (hexchar <= '9')) return hexchar - '0';
   if ((hexchar >= 'A') && (hexchar <= 'F')) return hexchar - 'A' + 10;
   if ((hexchar >= 'a') && (hexchar <= 'f')) return hexchar - 'a' + 10;

   return -1; // Wrong character

}

static void 
hex2float(char* HexColor, float* FloatColor)
{
   char* HexColorPtr = HexColor;

   int i = 0;
   for (i = 0; i < 4; i++)
   {
     int IntColor = (getdec(HexColorPtr[0]) * 16) +
                     getdec(HexColorPtr[1]);

     FloatColor[i] = (float) IntColor / 255.0;
     HexColorPtr += 2;
   }

}
