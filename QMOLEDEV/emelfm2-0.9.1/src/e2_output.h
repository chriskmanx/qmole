/* $Id: e2_output.h 2878 2013-10-28 02:17:59Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_OUTPUT_H__
#define __E2_OUTPUT_H__

#include "e2_option.h"

typedef enum
{
	E2_OUTPUT_NEWLINE           = 1<<1,
	E2_OUTPUT_PREPEND_NEWLINE   = 1<<2,
	E2_OUTPUT_STYLE_ERROR       = 1<<3,
	E2_OUTPUT_STYLE_LINK        = 1<<4,
	E2_OUTPUT_STYLE_IMPORTANT   = 1<<5,
	E2_OUTPUT_STYLE_UNIMPORTANT = 1<<6,
	E2_OUTPUT_DEBUG             = 1<<7,
	E2_OUTPUT_LINE_FEED         = 1<<8,
} E2_OUTPUT_PRINT_FLAGS;

//these things are tab-specific, swapped in and out
typedef struct _E2_OutputTabRuntime
{
	GtkWidget *scroll;	//scrolled window containing the textview
	GtkTextView *text;
	GtkTextBuffer *buffer;
	GtkTextMark *mark;	//buffer mark used for checking if output is being followed
	volatile gint onscreen;	//1 when scroll mark is presently visible in the tab's textview
//	GHashTable *origins;	//UNUSED table of textbuffer contexts & data
	//WARNING this must be protected by print_mutex, against threaded misuse
	gchar *origin_lastime;	//origin used for the previous message in this tab
#ifdef USE_GTK2_10
	guint labelnum;	//integer form of tab label, for getting new labels
#endif
#ifdef E2_TABS_DETACH
	GtkWidget *dropwindow;
	gboolean detached;	//TRUE when the tab has been dropped to a separate window
#endif
#ifdef E2_OUTPUTSTYLES
	GHashTable *style_tags; //table of GtkTextTag's available in this textbuffer, keyed by integer
	GHashTable *style_trios; //table of per-origin E2_Trio's (each having the pointers described below), keyed by origin-name
		//->a = GtkTextMark*, textbuffer mark where styling represented by data in ->b begins
		//->b = GArray*, array of ints processed from the most recent occurrence of 'ESC[N....m in the displayed text
	    //->c = gchar*, 'carryover' string from previous message, containing what is possibly a partial escape sequence
#endif
	E2_OptionSet *opt_wrap;
} E2_OutputTabRuntime;

typedef struct _E2_OutputRuntime
{
	E2_OptionSet *opt_show_on_new;
	E2_OptionSet *opt_show_on_focus_in;
	E2_OptionSet *opt_hide_on_focus_out;
	E2_OptionSet *opt_jump;	//option for scrolling to new output
	E2_OptionSet *opt_jump_follow;	//option for scrolling to new output when not manually scrolled away
	E2_OptionSet *opt_jump_end;	//option for scrolling only when new output is into the last context in the textbuffer
	gboolean visible;	//whether output pane is visible
	gboolean showhide_blocked;
	gint font_size;	//can be points or pixels
} E2_OutputRuntime;

/* UNUSED
typedef struct _E2_OutputOrigin
{
	gchar *name;
	GtkTextMark *mark_start;
	GtkTextMark *mark_insert_start;
	GtkTextMark *mark_insert_end;
	GtkTextMark *mark_end;
	GString *str;
	glong num_in_str;
	glong num_in_buffer;
} E2_OutputOrigin; */

#define E2_ERRORTAGS "bold", "negat"
#define TABDEFINE E2_OutputTabRuntime *_e2t1_;
#define TABLOGCURRENT E2_OutputTabRuntime *_e2t1_ = curr_tab;
#define TABLOG(tab) E2_OutputTabRuntime *_e2t1_ = tab;
#define e2_output_print_same(msg) \
 e2_output_print ((_e2t1_ == curr_tab) ? &app.tab : _e2t1_, msg, NULL, FALSE, NULL);
//#define e2_output_print_default(msg) e2_output_print (&app.tab, msg, NULL, FALSE, NULL);
//#define status_messagef(msg, first_tag, args...) e2_output_print (msg, NULL, FALSE, first_tag, ## args);

gchar *e2_output_get_active_text (gboolean expand) G_GNUC_MALLOC;
void e2_output_print_end (E2_OutputTabRuntime *tab, gboolean beep);
void e2_output_print_error (gchar *msg, gboolean freemsg);
void e2_output_print_strerrno (void);
void e2_output_print (E2_OutputTabRuntime *tab, gchar *msg, gchar *origin, //gboolean error, gboolean debug,
	gboolean newline, const gchar *first_tag, ...);
void e2_output_print2 (E2_OutputTabRuntime *tab, const gchar *msg,
	const gchar *origin, E2_OUTPUT_PRINT_FLAGS flags, const gchar *first_tag, ...);

void e2_output_mark_end (E2_OutputTabRuntime *rt);
void e2_output_scroll_to_end (E2_OutputTabRuntime *rt);

void e2_output_register_keybindings (GtkWidget *textview);
#ifdef E2_MOUSECUSTOM
void e2_output_register_pointerbindings (GtkWidget *textview);
#endif
#ifdef E2_OUTPUTSTYLES
void e2_output_clear_styles (E2_OutputTabRuntime *rt, const gchar *origin);
#endif
void e2_output_update_style (void);
GtkWidget *e2_output_initialise (void) G_GNUC_MALLOC;
void e2_output_set_menu_position (GtkWidget *menu, gint *x, gint *y,
	gboolean *push_in, GtkWidget *textview);
void e2_output_actions_register (void);
void e2_output_options_register (void);

#endif //ndef __E2_OUTPUT_H__
