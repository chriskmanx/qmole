/* $Id: e2_output.c 2949 2013-11-17 11:05:46Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 2004 Florian Zähringer <flo.zaehringer@web.de>
Portions copyright (C) 1999 Michael Clark.

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/e2_output.c
@brief output pane creation and action functions

includes actions related to output contents, but not
to the output visibility
*/
/**
\page output the output pane

ToDo - descibe how this works

\section tabs output pane tabs

ToDo
*/

#include "emelfm2.h"
#include <string.h>
#include <pthread.h>
#include <ctype.h>
#include "e2_output.h"
#include "e2_dialog.h"
//for pane-text activation
#include "e2_task.h"
#include "e2_filetype.h"
#include "e2_menu.h"
//for selection-save
#include "e2_view_dialog.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"

#define E2_PANED_TOLERANCE 10

#ifdef E2_OUTPUTSTYLES
//define to filter out [K sequences
//# define E2_ADAMK
#endif

//#define VOL volatile
#define VOL

static void _e2_output_copy_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt);
static void _e2_output_edit_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt);
#ifdef E2_TABS_DETACH
static void _e2_output_tabattach_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt);
static void _e2_output_clear_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt);
#endif
static void _e2_output_scrolled_cb (GtkAdjustment *adjust, E2_OutputTabRuntime *rt);
static gboolean _e2_output_button_press_cb (GtkWidget *textview,
	GdkEventButton *event, E2_OutputTabRuntime *rt);
static gboolean _e2_output_popup_menu_cb (GtkWidget *widget, E2_OutputTabRuntime *rt);

//static gboolean output_activated = FALSE;
#ifdef USE_GLIB2_32
static GRecMutex print_mutex;
#else
static GStaticRecMutex print_mutex;
#endif
extern pthread_mutex_t task_mutex;

#ifdef E2_TABS_DETACH
#define TARGET_NOTEBOOK_TAB 3
static GtkTargetEntry target_table2[] =
{
	{ "GTK_NOTEBOOK_TAB", GTK_TARGET_SAME_APP, TARGET_NOTEBOOK_TAB },
};
static guint n_targets2 = sizeof(target_table2) / sizeof(GtkTargetEntry);
#endif

#ifdef E2_OUTPUTSTYLES
//ansi/xterm colour-codes
//when a corresponding tag is created, it's named "Fcolor0" ... "Fcolor255" or "Bcolor0" ... "Bcolor255"
static gchar *text_colors[256] =
{
	"#000000",	//0 ansi + xterm(sort of) normal
	"#cd0000",	//1
	"#00cd00",	//2
	"#cdcd00",	//3
	"#0000ee",	//4
	"#cd00cd",	//5
	"#00cdcd",	//6
	"#e5e5e5",	//7
	"#7f7f7f",	//8 ansi + xterm(sort of) bright
	"#ff0000",	//9
	"#00ff00",	//10
	"#ffff00",	//11
	"#5c5cff",	//12
	"#ff00ff",	//13
	"#00ffff",	//14
	"#ffffff",	//15

	"#000000",	//16 xterm only
	"#00005f",	//17
	"#000087",	//18
	"#0000af",	//19
	"#0000d7",	//20
	"#0000ff",	//21
	"#005f00",	//22
	"#005f5f",	//23
	"#005f87",	//24
	"#005faf",	//25
	"#005fd7",	//26
	"#005fff",	//27
	"#008700",	//28
	"#00875f",	//29
	"#008787",	//30
	"#0087af",	//31
	"#0087d7",	//32
	"#0087ff",	//33
	"#00af00",	//34
	"#00af5f",	//35
	"#00af87",	//36
	"#00afaf",	//37
	"#00afd7",	//38
	"#00afff",	//39
	"#00d700",	//40
	"#00d75f",	//41
	"#00d787",	//42
	"#00d7af",	//43
	"#00d7d7",	//44
	"#00d7ff",	//45
	"#00ff00",	//46
	"#00ff5f",	//47
	"#00ff87",	//48
	"#00ffaf",	//49
	"#00ffd7",	//50
	"#00ffff",	//51
	"#5f0000",	//52
	"#5f005f",	//53
	"#5f0087",	//54
	"#5f00af",	//55
	"#5f00d7",	//56
	"#5f00ff",	//57
	"#5f5f00",	//58
	"#5f5f5f",	//59
	"#5f5f87",	//60
	"#5f5faf",	//61
	"#5f5fd7",	//62
	"#5f5fff",	//63
	"#5f8700",	//64
	"#5f875f",	//65
	"#5f8787",	//66
	"#5f87af",	//67
	"#5f87d7",	//68
	"#5f87ff",	//69
	"#5faf00",	//70
	"#5faf5f",	//71
	"#5faf87",	//72
	"#5fafaf",	//73
	"#5fafd7",	//74
	"#5fafff",	//75
	"#5fd700",	//76
	"#5fd75f",	//77
	"#5fd787",	//78
	"#5fd7af",	//79
	"#5fd7d7",	//80
	"#5fd7ff",	//81
	"#5fff00",	//82
	"#5fff5f",	//83
	"#5fff87",	//84
	"#5fffaf",	//85
	"#5fffd7",	//86
	"#5fffff",	//87
	"#870000",	//88
	"#87005f",	//89
	"#870087",	//90
	"#8700af",	//91
	"#8700d7",	//92
	"#8700ff",	//93
	"#875f00",	//94
	"#875f5f",	//95
	"#875f87",	//96
	"#875faf",	//97
	"#875fd7",	//98
	"#875fff",	//99
	"#878700",	//100
	"#87875f",	//101
	"#878787",	//102
	"#8787af",	//103
	"#8787d7",	//104
	"#8787ff",	//105
	"#87af00",	//106
	"#87af5f",	//107
	"#87af87",	//108
	"#87afaf",	//109
	"#87afd7",	//110
	"#87afff",	//111
	"#87d700",	//112
	"#87d75f",	//113
	"#87d787",	//114
	"#87d7af",	//115
	"#87d7d7",	//116
	"#87d7ff",	//117
	"#87ff00",	//118
	"#87ff5f",	//119
	"#87ff87",	//120
	"#87ffaf",	//121
	"#87ffd7",	//122
	"#87ffff",	//123
	"#af0000",	//124
	"#af005f",	//125
	"#af0087",	//126
	"#af00af",	//127
	"#af00d7",	//128
	"#af00ff",	//129
	"#af5f00",	//130
	"#af5f5f",	//131
	"#af5f87",	//132
	"#af5faf",	//133
	"#af5fd7",	//134
	"#af5fff",	//135
	"#af8700",	//136
	"#af875f",	//137
	"#af8787",	//138
	"#af87af",	//139
	"#af87d7",	//140
	"#af87ff",	//141
	"#afaf00",	//142
	"#afaf5f",	//143
	"#afaf87",	//144
	"#afafaf",	//145
	"#afafd7",	//146
	"#afafff",	//147
	"#afd700",	//148
	"#afd75f",	//149
	"#afd787",	//150
	"#afd7af",	//151
	"#afd7d7",	//152
	"#afd7ff",	//153
	"#afff00",	//154
	"#afff5f",	//155
	"#afff87",	//156
	"#afffaf",	//157
	"#afffd7",	//158
	"#afffff",	//159
	"#d70000",	//160
	"#d7005f",	//161
	"#d70087",	//162
	"#d700af",	//163
	"#d700d7",	//164
	"#d700ff",	//165
	"#d75f00",	//166
	"#d75f5f",	//167
	"#d75f87",	//168
	"#d75faf",	//169
	"#d75fd7",	//170
	"#d75fff",	//171
	"#d78700",	//172
	"#d7875f",	//173
	"#d78787",	//174
	"#d787af",	//175
	"#d787d7",	//176
	"#d787ff",	//177
	"#d7af00",	//178
	"#d7af5f",	//179
	"#d7af87",	//180
	"#d7afaf",	//181
	"#d7afd7",	//182
	"#d7afff",	//183
	"#d7d700",	//184
	"#d7d75f",	//185
	"#d7d787",	//186
	"#d7d7af",	//187
	"#d7d7d7",	//188
	"#d7d7ff",	//189
	"#d7ff00",	//190
	"#d7ff5f",	//191
	"#d7ff87",	//192
	"#d7ffaf",	//193
	"#d7ffd7",	//194
	"#d7ffff",	//195
	"#ff0000",	//196
	"#ff005f",	//197
	"#ff0087",	//198
	"#ff00af",	//199
	"#ff00d7",	//200
	"#ff00ff",	//201
	"#ff5f00",	//202
	"#ff5f5f",	//203
	"#ff5f87",	//204
	"#ff5faf",	//205
	"#ff5fd7",	//206
	"#ff5fff",	//207
	"#ff8700",	//208
	"#ff875f",	//209
	"#ff8787",	//210
	"#ff87af",	//211
	"#ff87d7",	//212
	"#ff87ff",	//213
	"#ffaf00",	//214
	"#ffaf5f",	//215
	"#ffaf87",	//216
	"#ffafaf",	//217
	"#ffafd7",	//218
	"#ffafff",	//219
	"#ffd700",	//220
	"#ffd75f",	//221
	"#ffd787",	//222
	"#ffd7af",	//223
	"#ffd7d7",	//224
	"#ffd7ff",	//225
	"#ffff00",	//226
	"#ffff5f",	//227
	"#ffff87",	//228
	"#ffffaf",	//229
	"#ffffd7",	//230
	"#ffffff",	//231

	"#080808",	//232
	"#121212",	//233
	"#1c1c1c",	//234
	"#262626",	//235
	"#303030",	//236
	"#3a3a3a",	//237
	"#444444",	//238
	"#4e4e4e",	//239
	"#585858",	//240
	"#626262",	//241
	"#6c6c6c",	//242
	"#767676",	//243
	"#808080",	//244
	"#8a8a8a",	//245
	"#949494",	//246
	"#9e9e9e",	//247
	"#a8a8a8",	//248
	"#b2b2b2",	//249
	"#bcbcbc",	//250
	"#c6c6c6",	//251
	"#d0d0d0",	//252
	"#dadada",	//253
	"#e4e4e4",	//254
	"#eeeeee"	//255
};
#endif

  /*****************/
 /***** utils *****/
/*****************/

#ifdef E2_OUTPUTSTYLES
/**
@brief cleanup allocated memory for @a data and its contents
@param data pointer to data value stored in hash table

@return
*/
static void _e2_output_clear_styletrio (E2_Trio *data)
{
	//ignore GtkTextmark @ data->a
	g_array_free ((GArray*)data->b, TRUE);
	g_free (data->c); //any carryover string
	DEALLOCATE (E2_Trio, data);
}
/**
@brief array value comparison function, for sorting
@param a pointer to int
@param b pointer to int

@return <0, 0 or >0 according to whether a belongs before, with or after b
*/
static gint _e2_output_compare_codes (gconstpointer a, gconstpointer b)
{
	return (GPOINTER_TO_INT(a) - GPOINTER_TO_INT(b));
}
/**
@brief save a carryover string for @a origin
@param rt pointer to tab data struct
@param origin name of context to which this carryover applies
@param save the string to be retained

@return
*/
static void _e2_output_carryover (E2_OutputTabRuntime *rt, const gchar *origin, const gchar *save)
{
	E2_Trio *data;
	if (rt->style_trios == NULL)
	{
		rt->style_trios = g_hash_table_new_full (
			g_str_hash, g_str_equal, NULL,  //assume constant string for key
			(GDestroyNotify)_e2_output_clear_styletrio);
		data = NULL;
	}
	else
		data = g_hash_table_lookup (rt->style_trios, origin);
	if (data == NULL)
	{
		data = ALLOCATE0 (E2_Trio);
		g_hash_table_insert (rt->style_trios, (gpointer)origin, data);
	}
	else
		g_free (data->c);
	data->c = g_strdup (save);
}
/**
@brief get terminal-style tag for the current buffer, corresponding to @a tagnum
The requested tag is returned from the hash-table tags cache, if present there,
or else the tag is created and added to the cache before returning it.
@param tagnum enumerator (+/-) of wanted tag
@param rt pointer to tab data struct

@return the tag
*/
static GtkTextTag *_e2_output_get_styletag (gint tagnum, E2_OutputTabRuntime *rt)
{
	guint tt;
	gboolean add;
	GtkTextTag *tag;
	gchar tname[12]; //enough for names like "Fcolor255"
#ifdef USE_GTK2_20
	GtkTextTagTable *table = gtk_text_buffer_get_tag_table (rt->buffer);
#endif
	tt = (tagnum >= 0) ? tagnum : -tagnum;
	add = TRUE;

	if (rt->style_tags == NULL)
	{
		rt->style_tags = g_hash_table_new_full (g_direct_hash, NULL, NULL, NULL);
		tag = NULL;
	}
	else
		tag = g_hash_table_lookup (rt->style_tags, GINT_TO_POINTER(tt));

	if (tag == NULL)
	{
		//colour?
		if (tt >= 256 && tt < 512)
		{
			snprintf (tname, sizeof(tname), "Fcolor%u", tt-256);
			tag = gtk_text_tag_new (tname);
			g_object_set ((gpointer)tag, "foreground", text_colors[tt-256], NULL);
		}
		else if (tt >= 512 && tt < 768)
		{
			 snprintf (tname, sizeof(tname), "Bcolor%u", tt-512);
			 tag = gtk_text_tag_new (tname);
			 g_object_set ((gpointer)tag, "background", text_colors[tt-512], NULL);
		}
		else
		{
			switch (tt)
			{
				case 1:
#ifdef USE_GTK2_20
				 tag = gtk_text_tag_table_lookup (table, "bold");
#else
				 tag = gtk_text_tag_table_lookup (rt->buffer->tag_table, "bold");
#endif
				 add = FALSE;
				 break;
				case 2:
				 tag = gtk_text_tag_new ("faint");
				 g_object_set ((gpointer)tag, "weight", PANGO_WEIGHT_LIGHT, NULL);
				 break;
				case 3:
#ifdef USE_GTK2_20
				 tag = gtk_text_tag_table_lookup (table, "italic");
#else
				 tag = gtk_text_tag_table_lookup (rt->buffer->tag_table, "italic");
#endif
				 add = FALSE;
				 break;
				case 4:
#ifdef USE_GTK2_20
				 tag = gtk_text_tag_table_lookup (table, "uline");
#else
				 tag = gtk_text_tag_table_lookup (rt->buffer->tag_table, "uline");
#endif
				 add = FALSE;
				 break;
				case 5: //gtk/pango doesn't do blinking, so we fall back to ?
				case 6:
				 tag = gtk_text_tag_new ("blink");
				 g_object_set ((gpointer)tag, "variant", PANGO_VARIANT_SMALL_CAPS, "weight", PANGO_WEIGHT_SEMIBOLD, NULL);
				 break;
				case 9:
				 tag = gtk_text_tag_new ("strike");
				 g_object_set ((gpointer)tag, "strikethrough", TRUE, NULL);
				 break;
			}
		}
		if (tag != NULL)
		{
			g_hash_table_insert (rt->style_tags, GINT_TO_POINTER(tt), (gpointer)tag);
			if (add)
#ifdef USE_GTK2_20
				gtk_text_tag_table_add (table, tag);
#else
				gtk_text_tag_table_add (rt->buffer->tag_table, tag);
#endif
		}
	}
	return tag;
}
/**
@brief remove values < 0 from array stored at @a codes
The array is not re-ordered
@param codes pointer to where the array is stored

@return
*/
static void _e2_output_clean_removed_codes (GArray **codes)
{
	guint indx;
	gint32 *ip;
restart:
	for (indx = 0, ip = (gint32*)(*codes)->data; indx < (*codes)->len; indx++, ip++)
	{
		if (*ip < 0)
		{
			*codes = g_array_remove_index (*codes, indx);
			if (indx == 0)
				goto restart;
			indx--;
			ip--;
		}
	}
}
/**
@brief update style-enumerators in array stored at @a codes, in accord with new code @a num
Because there's no control sequence to turn off a colour, nesting of colours is
not valid, so here, any new colour simply replaces any previous colour
@param codes store for array of style enumerators
@param num style enumerator <0 or >0

@return
*/
static void _e2_output_update_code (GArray **codes, gint32 num)
{
	guint indx;
	gint32 *ip;
	if (num > 0)
	{
		guint8 color;
		gboolean clean;
		if (num >= 256 && num < 512) color = 1;
		else if (num >= 512 && num < 768) color = 2;
		else color = 0;
		clean = FALSE;

		for (indx = 0, ip = (gint32*)(*codes)->data; indx < (*codes)->len; indx++, ip++)
		{
			if (*ip == num)
				return;
			if (color == 1)
			{
				if (*ip >= 256 && *ip < 512)
				{
					*ip = - *ip;
					clean = TRUE;
				}
			}
			else if (color == 2)
			{
				if (*ip >= 512 && *ip < 768)
				{
					*ip = - *ip;
					clean = TRUE;
				}
			}
		}
		if (clean)
			_e2_output_clean_removed_codes (codes);
		*codes = g_array_append_val (*codes, num);
	}
	else if (num < 0)
	{
		num = -num;
		for (indx = 0, ip = (gint32*)(*codes)->data; indx < (*codes)->len; indx++, ip++)
		{
			if (*ip == num)
			{
				*ip = -num;
				return;
			}
		}
	}
}
/**
@brief reconcile applicable tags for inserted text
Updated tag enumerators are stored in @a currentcodes.
Supported terminal styling codes - from text sequences [N....m
	0	all attributes off - remove all added style-tags
	1	bold on, buffer tag "bold" exists by default
	2	faint on - create tag "faint" using WEIGHT_LIGHT
	3	italic on, buffer tag "italic" exists by default
	4	underline on, buffer tag  "uline" exists by default
	5	slow blink on N/A for gtk, use semi-bold small caps instead
	6	fast blink on N/A for gtk, as for 5
	7	reverse video on - create tag "reverse" ATTR_FOREGROUND <> ATTR_BACKGROUND
	8	concealed on
	9	strikethrough on - create tag "strike" using ATTR_STRIKETHROUGH
	10	default font  N/A
	11-19 alternate fonts 1-9 N/A
	20	fraktur N/A
	21	bold off >> -1
	22	normal colour or weight >> -X1,-X2 ... for each appplied color or weight
	23	italic off >> -3
    24	underline off >> -4
	25	blink off N/A for gtk, stipling off instead
	26	reserved
	27	reverse video off >> -7 ATTR_FOREGROUND <> ATTR_BACKGROUND
	28	concealed off
	29	strikethrough off >> -9
	30	text color 0 - create tag "Fcolor0"
	31	text color 1 - create tag "Fcolor1"
	32	text color 2 - create tag "Fcolor2"
	33	text color 3 - create tag "Fcolor3"
	34	text color 4 - create tag "Fcolor4"
	35	text color 5 - create tag "Fcolor5"
	36	text color 6 - create tag "Fcolor6"
	37	text color 7 - create tag "Fcolor7"
	38	xterm 256-colour, extended - create tag "FcolorN", N = 0..255
	39	default text colour >> -X1 X1 = applied color
	40	background color 0 create tag "Bcolor0" or "Bcolor8"
	41	background color 1 create tag "Bcolor1" or "Bcolor9"
	42	background color 2 create tag "Bcolor2" or "Bcolor10"
	43	background color 3 create tag "Bcolor3" or "Bcolor11"
	44	background color 4 create tag "Bcolor4" or "Bcolor12"
	45	background color 5 create tag "Bcolor5" or "Bcolor13"
	46	background color 6 create tag "Bcolor6" or "Bcolor14"
	47	background color 7 create tag "Bcolor7" or "Bcolor15"
	48	background xterm 256-colour, extended
	49	default background colour >> -X1 X1 = applied color
@param start pointer to part of string to be displayed, the ESC in a ESC[<....>m
  terminal control sequence
@param currentcodes store for for ptr to array of ints, if array is non-NULL then
  each member a number from the previous sequence

@return pointer to next byte of output string to be processed upstream, normally the byte after a concluding 'm'
*/
static gchar *_e2_output_parse_escape (gchar *start, GArray **currentcodes)
{
	gulong lnum;
	gint num;
	gchar *ss, *se;
	GArray *codes;
	guint indx;
	gint32 *ip, i, x1, x2;

	if (*currentcodes != NULL)
	{
        //working copy of current array
		codes = g_array_sized_new (FALSE, FALSE, sizeof(gint32), (*currentcodes)->len);
		memcpy (codes->data, (*currentcodes)->data, (*currentcodes)->len * sizeof(gint32));
		codes->len = (*currentcodes)->len;
	}
	else
		codes = g_array_sized_new (FALSE, FALSE, sizeof(gint32), 3);

	ss = start;
	while (TRUE)
	{
		//TODO handle incomplete sequence at end of string being printed
		if (*ss != 27) break;
		if (*++ss != '[') break;
		ss++;
series:
		if (*ss == 'm')
		{
			lnum = 0;
			se = ss;
		}
#ifdef E2_ADAMK
		else if (*ss == 'K')
		{
			ss++;
			continue;
		}
#endif
		else if (strchr (ss + 1, 'm') == NULL) //very rough check!
			break;
		else
		{
			lnum = strtoul (ss, &se, 10);
			if (ss == se) //minimal error check
			{
				ss -= 2; //back to 
				break;
			}
			while (*se == ' ') se++;
		}
		if (*se == ';' || *se == 'm')
		{
			//process this code
			num = (gint)lnum;
			switch (num)
			{
			//add to codes if not found
			case 6:
				num = 5; //no distinct 'blink' rates
			case 1:
			case 2:
			case 3:
			case 4:
			case 5:
			case 9:
				_e2_output_update_code (&codes, num);
			    break;
			//remove from codes if found
			case 21:
			case 23:
			case 24:
			case 25:
			case 29:
				_e2_output_update_code (&codes, 20 - num);
			    break;
			//some values - special cases
			case 0://all attributes off
				for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
				{
					//assumes all current values > 0
					*ip = -*ip;
				}
			   break;
			case 7:
			case 27:
				//reverse video on/off
			    x1 = 0; x2 = 0;
				for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
				{
					i = *ip;
					if (i >= 256 && i < 512)
					{
						*ip = -i; //clear this one
						x1  = i + 256; //log new background
					}
					else if (i >= 512 && i < 768)
					{
						*ip = -i;
						x2 = i - 256; //log new foreground
					}
				}
				if (x1 > 0)
					codes = g_array_append_val (codes, x1);
				if (x2 > 0)
					codes = g_array_append_val (codes, x2);
			    break;
			case 8: //concealed = black on black
				_e2_output_update_code (&codes, 256);
				_e2_output_update_code (&codes, 512);
				break;
			case 28: //end concealed
				_e2_output_update_code (&codes, -256);
				_e2_output_update_code (&codes, -512);
				break;
			case 22://normal colour or weight remove colors, weights
				for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
				{
					i = *ip;
					if ( i == 1 || i == 2 ||
						(i >= 30 && i < 38) ||
						(i >= 40 && i < 48) ||
						(i >= 256 && i < 768))
						*ip = -i;
				}
			    break;
			case 30:
			case 31:
			case 32:
			case 33:
			case 34:
			case 35:
			case 36:
			case 37:
				_e2_output_update_code (&codes, num-30+256); //bright checked later
			    break;
			case 39://default text colour = foreground added colour off
				for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
				{
					i = *ip;
					if (i >= 256 && i < 512)
						*ip = -i;
				}
			    break;
			case 40:
			case 41:
			case 42:
			case 43:
			case 44:
			case 45:
			case 46:
			case 47:
				_e2_output_update_code (&codes, num-40+512); //bright checked later
			   break;
			case 49://default background colour = background added colour off
				for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
				{
					i = *ip;
					if (i >= 512 && i < 768)
						*ip = -i;
				}
			    break;
			case 38: //xterm extended code : foreground
			case 48: //background
				if (*se == '\0') break;
				ss = se + 1;
			    lnum = strtoul (ss, &se, 10);
			    if (lnum == 5)
				{
					while (*se == ' ' || *se == ';') se++;
				    lnum = strtoul (se, &se, 10);
					if (lnum >= 0 && lnum < 256)
					{
						num = (num == 38)?(gint)lnum + 256:(gint)lnum + 512;//xterm foreground or background
						_e2_output_update_code (&codes, num);
					}
				}
			    break;
			default:
				g_array_free (codes, TRUE); //CHECKME more tolerant ?
				return start;
			}
			ss = se + 1;
			if (*se == ';') goto series;
		}
		else
		{
			ss -= 2; //back to 
			break;
		}
	}
	//reconcile bright colours
	gboolean bright, color;
	bright = color = FALSE;
	//any order is allowed
	for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
	{
		i = *ip;
		if (i == 1)
			bright = TRUE;
		else if (i >= 256 && i < 264)
			color = TRUE;
		else if (i >= 512 && i < 520)
			color = TRUE;
	}
	if (bright && color)
	{
		bright = FALSE;
		for (indx = 0, ip = (gint32*)codes->data; indx < codes->len; indx++, ip++)
		{
			i = *ip;
			if (i == 1)
			{
				bright = TRUE; //TODO remove this 1-code
				codes = g_array_remove_index (codes, indx);
				indx--; //CHECKME OK at indx 0?
				ip--;
			}
			else if (i == -1)
			{
				bright = FALSE; //TODO remove this -1-code
				codes = g_array_remove_index (codes, indx);
				indx--; //CHECKME OK at indx 0?
				ip--;
			}
			else if ((i >= 256 && i < 264) || (i >= 512 && i < 520))
			{
				if (bright)
					*ip = i + 8;
			}
		}
	}

	if (codes != NULL && codes->len > 1)
		g_array_sort (codes, _e2_output_compare_codes);

	if (*currentcodes == NULL)
		*currentcodes = codes; //maybe still NULL
	else if (codes != NULL)
	{
		g_array_free (*currentcodes, TRUE);
		*currentcodes = codes;
	}
	return ss;
}
/**
@brief put text into buffer associated with @a rt
Insertion starts at the GtkTextmark for @a origin, possibly after some adjustment.
If that mark doesn't exist in the buffer, it's created at the end of the buffer.
Also sets @a rt->mark to start of inserted text, more-or-less.
Cursor position is not used or changed.
@param rt pointer to tab data struct
@param origin non-NULL name of buffer context into which text is to be inserted ("default" when appropriate)
@param ss pointer to 1st char of string to be inserted
@param se pointer to last char of string to be inserted (may be before or same as @a ss)
@param strtiter pointer to iter for returning position of start of insert, or NULL
@param enditer pointer to iter for returning position of end of insert, or NULL
@param newline TRUE if newline to be added before the text at @a ss
@param back_prior no. of backspaces at start of inserted text

@return
*/
static void _e2_output_insert_text (E2_OutputTabRuntime *rt, gchar *origin,
	const gchar *ss, const gchar *se, GtkTextIter *strtiter, GtkTextIter *enditer,
	gboolean newline, gint back_prior)
{
	gboolean is_default_origin;
	//flag whether this origin is new
	gboolean is_new;
	//get the text buffer for the output pane
	VOL GtkTextBuffer *buffer = rt->buffer;
	VOL GtkTextMark *origin_mark;
	VOL GtkTextIter iter;

	is_default_origin = strcmp (origin, "default") == 0;
	//try to get insertion-point mark - it exists if there has been output in this origin before
	origin_mark = gtk_text_buffer_get_mark ((GtkTextBuffer*)buffer, origin);
	//if not, create it at the end
	if (origin_mark == NULL)
	{
		is_new = TRUE;
		gtk_text_buffer_get_end_iter ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter);
		//insertion-point mark has left-gravity, it will stay at left of text inserted there
		origin_mark = gtk_text_buffer_create_mark ((GtkTextBuffer*)buffer, origin,
			(GtkTextIter*)&iter, TRUE);
	}
	else
	{
		VOL GtkTextMark *mark_del;
		gchar *mark_del_name;

		is_new = FALSE;
		//the default context always outputs to the end of the textview
		//(ie the output is not "glued together")
		if (is_default_origin)
		{
			gtk_text_buffer_get_end_iter ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter);
			gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)origin_mark, (GtkTextIter*)&iter);
		}
		else
			gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter, (GtkTextMark*)origin_mark);
		//do we have to overwrite a previous message's characters in front of us?
		//(if a previous message had a carriage return '\r' in it)
		mark_del_name = g_strconcat (origin, "-del", NULL);	//no translate
		mark_del = gtk_text_buffer_get_mark ((GtkTextBuffer*)buffer, (gchar*)mark_del_name);
		g_free ((gchar *)mark_del_name);
		if (mark_del != NULL)
		{
			VOL GtkTextIter iter_del;
			gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter_del, (GtkTextMark*)mark_del);
			gtk_text_buffer_delete ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter, (GtkTextIter*)&iter_del);
			//delete the mark, no further use this time
			gtk_text_buffer_delete_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)mark_del);
		}
	}
	//move the scroll-helper mark
	gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, rt->mark, (GtkTextIter*)&iter);
//	gtk_text_buffer_place_cursor ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter);

	if (back_prior > 0)
	{	//want to backspace into previous stored text for this context
		VOL gint _offset = gtk_text_iter_get_offset ((GtkTextIter*)&iter);
		GtkTextIter backi;
		//FIXME limit backshift to start of current context
		//CHECKME ignore \n for the purposes of going backwards
		gtk_text_buffer_get_iter_at_offset ((GtkTextBuffer*)buffer, &backi, _offset - back_prior);
		gtk_text_buffer_delete ((GtkTextBuffer*)buffer, &backi, (GtkTextIter*)&iter);
//		gtk_text_buffer_get_iter_at_offset ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter, _offset - back_prior);
		iter = backi;
	}

	VOL gint lfcount = 0;

	//if necessary, insert a newline character before the message because
	//it has a different context from the last one and the last one hasn't
	//printed one yet;
/*#ifdef USE_GLIB2_32
	g_rec_mutex_lock (&print_mutex)
#else
	g_static_rec_mutex_lock (&print_mutex);
#endif
*/
	if (rt->origin_lastime != NULL
		&& strcmp (rt->origin_lastime, origin)
		&& !gtk_text_iter_starts_line ((GtkTextIter*)&iter)
		&& (is_new || is_default_origin))
			lfcount = 1;
/*#ifdef USE_GLIB2_32
	g_rec_mutex_unlock (&print_mutex)
#else
	g_static_rec_mutex_unlock (&print_mutex);
#endif
*/
	if (newline && (!gtk_text_iter_starts_line ((GtkTextIter*)&iter)))
	{
		printd (DEBUG, "inserted additional newline because the message requested it");
		lfcount++;
	}

	if (lfcount > 0)
	{
		gtk_text_buffer_insert ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter, "\n", lfcount);
	}
	//finally, put content (if any) in
	if (ss < se + 1)
		gtk_text_buffer_insert ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter, (gchar*)ss, se-ss+1);
	if (strtiter != NULL)
		//report the insertion-start
		gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, strtiter,
			(GtkTextMark*)origin_mark);
	if (enditer != NULL)
		//report the insertion-end
		*enditer = iter;
	//re-position ready for next insertion
	//TODO handle CR which involves origin_mark moved back, and mark_del at iter
	gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, origin_mark, (GtkTextIter*)&iter);
}
#endif //def E2_OUTPUTSTYLES
/**
@brief set flag according to whether @a iter in an output pane text buffer is presently visible

@param rt output pane tab runtime data struct
@param iter pointer to data for the position in @a text_view that is to be checked

@return
*/
static void _e2_output_iter_check_visible (E2_OutputTabRuntime *rt,
	VOLATILE GtkTextIter *iter)
{
	GdkRectangle visible_rect, iter_rect;
	gtk_text_view_get_visible_rect (rt->text, &visible_rect);
	gtk_text_view_get_iter_location (rt->text, iter, &iter_rect);
	gboolean off =
	//this makes the pane scroll 1 line too many, but is faster
	 ((iter_rect.y + iter_rect.height) > (visible_rect.y + visible_rect.height)
	//this causes jiggling, due to trailing blank line (\n at line-ends) being
	//re-scrolled off-screeen-bottom
//	 (iter_rect.y > (visible_rect.y + visible_rect.height)
	|| iter_rect.y < visible_rect.y);
	//atomic change to minimise race with continuing output
	g_atomic_int_set (&rt->onscreen, (off) ? 0:1);
}
/**
@brief move the 'page' displayed in the output pane, relative to its text content

@param down TRUE to move down, FALSE to move up
@param arg string containing a number, the no. of 'moves'
@param page TRUE move @a arg 'pages', FALSE move @a arg 'steps'

@return
*/
static void _e2_output_scroll_helper (gboolean down, gchar *arg, gboolean page)
{
	printd (DEBUG, "scroll_helper (down:%d,arg:%s,page:%d)", down, arg, page);
	E2_OutputTabRuntime *rt = &app.tab;
	g_return_if_fail (rt->scroll != NULL);
	gchar *end = NULL;
	gdouble times = 1.0;
	if (arg != NULL)
		times = g_ascii_strtod (arg, &end);
	if (end == arg)
		times = 1.0;
	GtkAdjustment *vadj;
	vadj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (rt->scroll));
	gdouble value = gtk_adjustment_get_value (vadj);
	gdouble inc = page ?
#ifdef USE_GTK2_14
		gtk_adjustment_get_page_increment (vadj):
		gtk_adjustment_get_step_increment (vadj);
#else
		vadj->page_increment : vadj->step_increment;
#endif

	if (down)
	{
		value += (inc * times);
		gdouble rest =
#ifdef USE_GTK2_14
			gtk_adjustment_get_upper (vadj) - gtk_adjustment_get_page_size (vadj);
#else
			vadj->upper - vadj->page_size;
#endif
		if (value > rest)
			value = rest;
	}
	else
		value -= (inc * times);

	gtk_adjustment_set_value (vadj, value);
}
/**
@brief set a mark at the position of the bottom-left of the output pane
This supports e2_output_scroll_to_bottom()
@param rt pointer to tab data

@return
*/
void e2_output_mark_end (E2_OutputTabRuntime *rt)
{
	g_return_if_fail (rt->scroll != NULL);

	GdkRectangle visible_rect;
	gtk_text_view_get_visible_rect (rt->text, &visible_rect);

	GtkTextIter iter;
	gtk_text_view_get_iter_at_location (rt->text, &iter,
		visible_rect.x, visible_rect.y + visible_rect.height - 1);

	GtkTextMark *mark = gtk_text_buffer_get_mark (rt->buffer, "bottom-scroll");
	if (mark != NULL)
		gtk_text_buffer_move_mark (rt->buffer, mark, &iter);
	else
		gtk_text_buffer_create_mark (rt->buffer, "bottom-scroll", &iter, TRUE);
}
/**
@brief idle callback to scroll the 'page' displayed in an output-pane tab to the end of content
Expects BGL open/off
@param rt pointer to data for the relevant output tab

@return FALSE to remove the source
*/
static gboolean _e2_output_do_scroll (E2_OutputTabRuntime *rt)
{
	if (rt->scroll != NULL)
	{
		GtkTextMark *mark = gtk_text_buffer_get_mark (rt->buffer, "bottom-scroll");
		if (G_LIKELY (mark != NULL))
		{
			CLOSEBGL
			gtk_text_view_scroll_to_mark (rt->text, mark, 0.0, TRUE, 0.0, 1.0);
			OPENBGL
		}
	}
	return FALSE;
}
/**
@brief move the 'page' displayed in the output pane, to show a point at the bottom

@param rt pointer to tab data

@return
*/
void e2_output_scroll_to_end (E2_OutputTabRuntime *rt)
{
	//this won't work if a higher priority is applied
	g_idle_add ((GSourceFunc) _e2_output_do_scroll, rt);
}
/**
@brief move the displayed text window to show text printed by a child process

@param item UNUSED the selected item from a children-menu
@param rt pointer to data for the command related to @a item, or NULL for "no children" item

@return
*/
static void _e2_output_scroll_to_child (GtkMenuItem *item, E2_TaskRuntime *rt)
{
	if (rt != NULL)
	{	//we have a child
		pthread_mutex_lock (&task_mutex);
		GList *member = g_list_find (app.taskhistory, rt);
		pthread_mutex_unlock (&task_mutex);
		if (member != NULL)	//command data still exists
		{
			//the buffer may be cleared while the menu is active,
			//so check again for matching content
			GtkTextMark *origin_mark = gtk_text_buffer_get_mark
				(app.tab.buffer, rt->pidstr);
			if (origin_mark != NULL)
				gtk_text_view_scroll_to_mark (app.tab.text, origin_mark, 0.0,
					TRUE, 0.0, 1.0);
			else
			{
				gchar *msg = g_strdup_printf (_("Cannot find any output from process %s"), rt->pidstr);
				e2_output_print_error (msg, TRUE);
			}
		}
	}
}
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu the GtkMenu to be positioned
@param x place to store gint representing the menu left
@param y place to store gint representing the menu top
@param push_in place to store pushin flag
@param textview output pane widget in focus when the menu key was pressed

@return
*/
void e2_output_set_menu_position (GtkWidget *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *textview)
{
	gint left, top;
	GtkAllocation alloc;
	gtk_window_get_position (GTK_WINDOW (app.main_window), &left, &top);
#ifdef USE_GTK3_0
	//gtk3 bug, reports wrong value for textview alloc.y
	gtk_widget_get_allocation (app.main_window, &alloc);
	top = top + alloc.height * app.window.output_paned_ratio;
#endif
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (textview, &alloc);
#else
	alloc = textview->allocation;
#endif
	*x = left + alloc.x + alloc.width/2;
#ifdef USE_GTK3_0
	*y = top + alloc.height/2;
#else
	*y = top + alloc.y + alloc.height/2;
#endif
	*push_in = FALSE;
}
/**
@brief update gtk's flag which sets output pane text wrapping

This is a hook fn

@param pvalue pointerised form of the GtkWrapMode-enum value to be set
@param rt pointer to data struct for an output pane tab

@return TRUE always, so func is not delisted after run
*/
static gboolean _e2_output_set_op_wrap_hook (gpointer pvalue, E2_OutputTabRuntime *rt)
{
//	printd (DEBUG, "_e2_output_set_op_wrap (pvalue:_,rt:_)");
	gint value = GPOINTER_TO_INT (pvalue);
	gint cur = gtk_text_view_get_wrap_mode (rt->text);
	if (cur != value)
		gtk_text_view_set_wrap_mode (rt->text, value);
	return TRUE;
}
/**
@brief execute action corresponding to item selected from filetype tasks menu
This is the callback for handling a selection of a filetype action from
the context menu
@param widget the activated menu item widget

@return
*/
static void _e2_output_choose_filetype_action_cb (GtkMenuItem *menu_item)
{
	gpointer *command = g_object_get_data (G_OBJECT (menu_item), "e2-file-operation");
	NEEDCLOSEBGL
	e2_command_run ((gchar *) command, E2_COMMAND_RANGE_DEFAULT, menu_item
#ifdef E2_COMMANDQ
	, FALSE
#endif
	);
	NEEDOPENBGL
}
/**
@brief populate @a menu with items for the actions for a filetype
Each member of @a actions is like "command" or "label@command"
@param text the path of the item to open, utf8 string
@param menu the menu widget to which the action menu-items are to be added
@param actions NULL-terminated array of utf8 strings, each a command for a filetype

@return
*/
static void _e2_output_menu_create_filetype_actions_menu (gchar *text,
	GtkWidget *menu, const gchar **actions)
{
	gchar *sep, *fullcmd;
	GtkWidget *menu_item;

	while (*actions != NULL)
	{
		if ((sep = strchr (*actions, '@')) != NULL)  //if always ascii @, don't need g_utf8_strchr()
		{
			*sep = '\0';
			menu_item = e2_menu_add (menu, (gchar *)*actions, NULL, NULL,
				_e2_output_choose_filetype_action_cb, NULL);
			fullcmd = e2_utils_replace_name_macros (sep + sizeof (gchar), text);
			if (fullcmd == sep + sizeof (gchar))	//no replaced macro in action-string
			{
//tag E2_BADQUOTES
				gchar *qp = e2_utils_quote_string (text);
				fullcmd = g_strconcat (sep + sizeof (gchar), " ", qp, NULL);
				g_free (qp);
			}
			*sep = '@';	//revert to original form (this is the 'source' data)
		}
		else
		{
			menu_item = e2_menu_add (menu, (gchar *)*actions, NULL, NULL,
				_e2_output_choose_filetype_action_cb, NULL);
			fullcmd = e2_utils_replace_name_macros ((gchar *)*actions, text);
			if (fullcmd == (gchar *)*actions)	//no replaced macro in *actions
			{
//tag E2_BADQUOTES
				gchar *qp = e2_utils_quote_string (text);
				fullcmd = g_strconcat (*actions, " ", qp, NULL);
				g_free (qp);
			}
		}
		g_object_set_data_full (G_OBJECT (menu_item), "e2-file-operation", fullcmd,
			g_free);
		actions++;
	}
}
/**
@brief create a filetypes sub-menu for @a menu

@param menu menu widget to populate
@param item UTF-8 string describing the item to process, may have absolute or relative or no path

@return
*/
static void _e2_output_populate_filetype_menu (GtkWidget *menu, gchar *item)
{
	struct stat statbuf;
	E2_ERR_DECLARE

	gchar *usepath = e2_utils_translate_relative_path (curr_view->dir, item);
	*(usepath + strlen(usepath) - sizeof(gchar)) = '\0'; //stat() hates trailing separator
	gchar *local = F_FILENAME_TO_LOCALE (usepath);
#ifdef E2_VFS
# ifdef E2_VFSTMP
	get relevant spacedata, or allow only local (NULL)
# endif
	VPATH sdata = {local, NULL};
	if (e2_fs_stat (&sdata, &statbuf E2_ERR_PTR()))
#else
	if (e2_fs_stat (local, &statbuf E2_ERR_PTR()))
#endif
	{
#ifdef E2_VFSTMP
		//FIXME handle error
#endif
		E2_ERR_CLEAR
		g_free (usepath);
		F_FREE (local, usepath);
		return;
	}

	gboolean exec;
	const gchar **actions = NULL;
	gchar *ext, *ext2 = NULL, *mimetype = NULL;
	gchar *base = g_path_get_basename (item);
#ifdef E2_VFS
	if (e2_fs_is_dir3 (&sdata E2_ERR_NONE()))
#else
	if (e2_fs_is_dir3 (local E2_ERR_NONE()))
#endif
	{
		exec = FALSE;	//no special treatment of dirs
		ext = ext2 = g_strconcat (".", _("<directory>"), NULL);
	}
	else
	{
		exec = e2_fs_is_exec2 (
#ifdef E2_VFS
			&sdata E2_ERR_NONE());
#else
			local E2_ERR_NONE());
#endif
		if (!exec)
#ifdef E2_VFS
			mimetype = e2_utils_get_mimetype (&ddata);
#else
			mimetype = e2_utils_get_mimetype (local);
#endif
		//extension is the part of the name after the leftmost '.'
		ext = (*base == '.') ? base + sizeof (gchar) : base; //FIXME generalise hidden-item test
		ext = strchr (ext, '.');	//'.' is ascii
		if ((ext == NULL || *(ext + sizeof (gchar)) == '\0')  //no extension
			&& e2_fs_is_text (
#ifdef E2_VFS
				&sdata E2_ERR_NONE()))
#else
				local E2_ERR_NONE()))
#endif
			//fake text extension
			//too bad if this is not a recognised text extension in filetypes data
			ext = ".txt";
	}
	if (ext != NULL)
	{
		//check all possible extensions for a matching filetype
		do
		{
			//skip leading dot "."
			ext += sizeof (gchar);	//ascii '.'. always single char
			actions = e2_filetype_get_actions (ext);
			if (actions != NULL)
			{
				_e2_output_menu_create_filetype_actions_menu (usepath, menu, actions);
				break;
			}
		} while ((ext = strchr (ext, '.')) != NULL);	//always ascii '.', don't need g_utf8_strchr()
	}
	if (exec)
	{
		//add exec-filetype items unless item has been found in that type already
		const gchar **acts2 = e2_filetype_get_actions (_("<executable>"));
		if (actions != NULL //was a matching extension
			&& actions != acts2 && acts2 != NULL)	//CHECKME this test
				_e2_output_menu_create_filetype_actions_menu (usepath, menu, acts2);
		else if (acts2 != NULL)
			_e2_output_menu_create_filetype_actions_menu (usepath, menu, acts2);
		//add any relevant desktop-file actions
		gchar *localname = F_FILENAME_TO_LOCALE (base);
		e2_menu_add_desktop_actions (menu, localname);
		F_FREE (localname, base);
	}
	else if (mimetype != NULL)
	{
		e2_menu_add_desktop_mime (menu, mimetype);
		g_free (mimetype);
	}
	g_free (usepath);
	F_FREE (local, usepath);
	g_free (base);
	if (ext2 != NULL)
		g_free (ext2);
}
/**
@brief check for or open a taskable item, asking for choice if @a run is TRUE
Expects BGL on/closed
@param item UTF-8 string describing the item to open, may have absolute or relative or no path
@param run TRUE to execute a matched command, FALSE to just return the match status

@return TRUE if the item was processed (@a run = FALSE) or found (@a run = TRUE)
*/
static gboolean _e2_output_open_text (gchar *item, gboolean run)
{
	gboolean retval;
	struct stat statbuf;
	gchar *usepath, *local;

	usepath = e2_utils_translate_relative_path (curr_view->dir, item);
	*(usepath + strlen(usepath) - sizeof(gchar)) = '\0'; //stat() hates trailing separator
	local = F_FILENAME_TO_LOCALE (usepath);

	//make sure the text means something
#ifdef E2_VFS
# ifdef E2_VFSTMP
	assume text from local command run on local items
# endif
	VPATH sdata = { local, NULL };

	if (e2_fs_stat (&sdata, &statbuf E2_ERR_NONE()))
#else
	if (e2_fs_stat (local, &statbuf E2_ERR_NONE()))
#endif
	{
		retval = FALSE;
		if (run)
		{
			gchar *msg = g_strdup_printf (_("Cannot get information about %s"), item);
			e2_output_print_error (msg, TRUE);
		}
	}
	else if (run)
	{
#ifdef E2_VFS
		retval = e2_task_backend_open (&sdata, TRUE);
#else
		retval = e2_task_backend_open (local, TRUE);
#endif
	}
	else
	{	//just check for a known filetype
#ifdef E2_VFS
		if (e2_fs_is_dir3 (&sdata E2_ERR_NONE())	//we can always handle dirs
		|| !e2_fs_access (&sdata, X_OK E2_ERR_NONE()))	//and executable items
#else
		if (e2_fs_is_dir3 (local E2_ERR_NONE())	//we can always handle dirs
		|| !e2_fs_access (local, X_OK E2_ERR_NONE()))	//and executable items
#endif
			retval = TRUE;
		else
		{
			gchar *base = g_path_get_basename (item);
			gchar *ext = strchr (base, '.');	//assumes '.' is ascii
			if (ext == NULL //no extension
				|| ext == base) //hidden file
			{
#ifdef E2_VFS
				retval = e2_fs_is_text (&sdata E2_ERR_NONE());
#else
				retval = e2_fs_is_text (local E2_ERR_NONE());
#endif
			}
			else
			{
				gchar *action;
				retval = FALSE;
				do
				{
					ext += sizeof (gchar); //skip the . prefix
					action = e2_filetype_get_default_action (ext);
					if (action != NULL)
					{
						retval = TRUE;
						g_free (action);
						break;
					}
				} while ((ext = strchr (ext, '.')) != NULL);	//if always ascii '.', don't need g_utf8_strchr()
			}
			g_free (base);
		}
	}
	g_free (usepath);
	F_FREE (local, usepath);
	return retval;
}

static gunichar *_e2_output_unistrchr (gunichar *stack, guint len, gunichar needle)
{
	guint i;
	for (i = 0; i < len; i++)
	{
		if (stack[i] == needle)
			return &stack[i];
	}
	return NULL;
}

/**
@brief get and optionally select text (if any) surrounding event-position
If there is a selection at cursor position, that selection is returned.
Otherwise surrounding whitespace or newline chars are scanned for.
@param x event x coordinate
@param y event y coordinate
@param rt pointer to data struct for output pane tab
@param select TRUE to select text if any

@return newly allocated string containing item, or NULL if no suitable text found
*/
static gchar *_e2_output_get_item_at_pointer (gint x, gint y,
	E2_OutputTabRuntime *rt, gboolean select)
{
	GtkTextIter iter, start, end;
	gint buffer_x, buffer_y;
	gboolean quoted;
	gunichar c, d;
	gtk_text_buffer_get_bounds (rt->buffer, &start, &end);
	if (gtk_text_iter_equal (&start, &end))
		return NULL;
	gtk_text_view_window_to_buffer_coords (rt->text, GTK_TEXT_WINDOW_TEXT,
		x, y, &buffer_x, &buffer_y);
	gtk_text_view_get_iter_at_location (rt->text, &iter, buffer_x, buffer_y);
	if (!gtk_text_buffer_get_selection_bounds (rt->buffer, &start, &end)
		|| !gtk_text_iter_in_range (&iter, &start, &end))
	{
		c = gtk_text_iter_get_char (&iter);
		if (g_unichar_isspace (c))
			return NULL;
		const gchar *breakers = e2_option_str_get ("output-select-separators");
//		if (*breakers == \'0') if space-checks are folded into this
//			breakers = " ";
		glong sepcount = 0;
		gunichar *bounds = g_utf8_to_ucs4_fast (breakers, -1, &sepcount);
		//word separators include valid path chars, so need char iteration
		while (gtk_text_iter_backward_char (&iter))
		{
			c = gtk_text_iter_get_char (&iter);
			if (g_unichar_isspace (c))
			{
				gtk_text_iter_forward_char (&iter);
				break;
			}
			else if (sepcount > 0 && _e2_output_unistrchr (bounds, sepcount, c) != NULL)
			{
				gtk_text_iter_forward_char (&iter);
				break;
			}
		}
		c = gtk_text_iter_get_char (&iter);
		quoted = (c == (gunichar) '"' || c == (gunichar) '\'');
		if (quoted)
		{
			if (!gtk_text_iter_forward_char (&iter))
				return NULL;
		}
		start = iter;
		while (gtk_text_iter_forward_word_end (&iter))
		{
nextchar:
			d = gtk_text_iter_get_char (&iter);
			if (quoted && d == c)
				break;
			else if (g_unichar_isspace (d))
				break;
			else if (sepcount > 0 && _e2_output_unistrchr (bounds, sepcount, d) != NULL)
				break;
			else if (ispunct ((guchar) d))
			{
				if (gtk_text_iter_forward_char (&iter))
					goto nextchar;
				break;
			}
		}
		g_free (bounds);
		end = iter;

		if (select)
			gtk_text_buffer_select_range (rt->buffer, &start, &end);
	}

	return (gtk_text_iter_get_text (&start, &end));
}
/**
@brief construct and pop up destroyable context-menu for output-pane

@param textview the textview widget where the click/press happened
@param event_button which mouse button was clicked (0 for a menu-key press)
@param event_time time that the event happened (0 for a menu-key press)
@param rt runtime struct for the tab being processed

@return
*/
static void _e2_output_show_context_menu (GtkWidget *textview,
	guint event_button, gint event_time, E2_OutputTabRuntime *rt)
{
	gchar *item_name;
	GtkWidget *item;
	GtkWidget *menu = e2_menu_get ();
#ifdef E2_TABS_DETACH
	if (!rt->detached)
	{
#endif
		item_name = g_strconcat (_A(10),".",_A(33),NULL);
		e2_menu_add_action (menu, _("_Hide"), "output_hide"E2ICONTB,
			_("Do not show the output pane"), item_name, "1");  //no arg-string translation
		g_free (item_name);
		item_name = g_strconcat (_A(10),".",_A(33),NULL);
		e2_menu_add_action (menu, _("_Toggle full"),
			(app.window.output_paned_ratio > 0.01) ?
				STOCK_NAME_ZOOM_FIT : STOCK_NAME_ZOOM_OUT,
			_("Toggle output pane size to/from the full window size"),
			item_name, "0,*");  //no string translation
		g_free (item_name);
		item_name = g_strconcat (_A(10),".",_A(31),NULL);
		e2_menu_add_action (menu, _("_New tab"), STOCK_NAME_ADD,
			_("Add another tab for the output pane"), item_name, NULL);
		g_free (item_name);
		item_name = g_strconcat (_A(10),".",_A(45),NULL);
		item = e2_menu_add_action (menu, _("_Remove tab"), STOCK_NAME_REMOVE,
			_("Close this this tab"), item_name, NULL);
		g_free (item_name);
		if (item != NULL && app.tabcount == 1)
			gtk_widget_set_sensitive (item, FALSE);
#ifdef E2_TABS_DETACH
	}
	else //rt->detached
	{
		e2_menu_add (menu, _("_Attach"), NULL,
		_("Move this tab back to output pane"), _e2_output_tabattach_cb, rt);
	}
#endif

	e2_menu_add_separator (menu);

#ifdef E2_TABS_DETACH
	e2_menu_add (menu, _("C_lear"), STOCK_NAME_CLEAR,
		_("Clear this tab"), _e2_output_clear_cb, rt);
#else
	item_name = g_strconcat (_A(10),".",_A(36),NULL);
	e2_menu_add_action (menu, _("C_lear"), STOCK_NAME_CLEAR,
		_("Clear this tab"), item_name, NULL);
	g_free (item_name);
#endif
	item = e2_menu_add (menu, _("_Copy"), STOCK_NAME_COPY,
		_("Copy selected text"), _e2_output_copy_cb, rt);
	gtk_widget_set_sensitive (item,
		gtk_text_buffer_get_selection_bounds (rt->buffer, NULL, NULL));
/*	e2_menu_add (menu, _("Save as.."), "save_selection"E2ICOND,	//no suitable mnemonic
		_("Save the selected text"), _e2_output_savesel_cb, rt);
	if (!gtk_text_buffer_get_selection_bounds (rt->buffer, NULL, NULL))
		gtk_widget_set_sensitive (item, FALSE); */
	e2_menu_add (menu, _("_Edit"), STOCK_NAME_EDIT,
		_("Edit the tab contents"), _e2_output_edit_cb, rt);

	item = e2_menu_add (menu, _("_Open"), STOCK_NAME_EXECUTE, NULL, NULL, NULL);

	gint x, y;
#ifdef USE_GTK3_0
/* we arrived here via
  _e2_output_button_press_cb(), GDK_BUTTON_PRESS event, event_button = 3, useful event->device
OR
  _e2_output_popup_menu_cb(), GDK_KEY_PRESS event event_button = 0, device is keyboard
*/
	if (e2_utils_get_pointer_position (textview, &x, &y))
#else //gtk 2
	if (gdk_window_get_pointer (
# ifdef USE_GTK2_14
			gtk_widget_get_window (textview),
# else
			textview->window,
# endif
			&x, &y, NULL) != NULL)
#endif
		item_name = _e2_output_get_item_at_pointer (x, y, rt, FALSE);
	else
		item_name = NULL;

	if (item_name == NULL || !_e2_output_open_text (item_name, FALSE))
		gtk_widget_set_sensitive (item, FALSE);
	else
	{
		GtkWidget *submenu = e2_menu_get ();
		_e2_output_populate_filetype_menu (submenu, item_name);
		gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
		gtk_widget_show (submenu);
	}
	g_free (item_name);	//??

/*	item_name = g_strconcat (_A(10),".",_A(57),NULL);
	e2_menu_add_action (menu, _("C_ommand help"), STOCK_NAME_HELP,
		_("Show information about using the command line"),
		item_name, NULL);
	g_free (item_name);
*/
	item = e2_menu_add (menu, _("Co_mmand output"), "ps"E2ICONTB,
		_("Show output from a completed command"), NULL, NULL);
	GtkWidget *submenu = e2_menu_create_child_menu (E2_CHILD_OUTPUT,
		_e2_output_scroll_to_child);
	if (submenu == NULL)
		gtk_widget_set_sensitive (item, FALSE);
	else
		gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);

#ifdef E2_TABS_DETACH
	if (!rt->detached)
	{
#endif
		e2_menu_add_separator (menu);
		submenu = e2_menu_add_submenu (menu, _("_Settings"), STOCK_NAME_PREFERENCES);
		e2_menu_create_options_menu (GTK_WIDGET (rt->text), submenu,
			rt->opt_wrap, NULL, NULL,
			app.output.opt_show_on_new, NULL, NULL,
			app.output.opt_show_on_focus_in, NULL, NULL,
			app.output.opt_hide_on_focus_out, NULL, NULL,
			app.output.opt_jump, NULL, NULL,
			app.output.opt_jump_follow, NULL, NULL,
			app.output.opt_jump_end, NULL, NULL,
			NULL);
		item_name = g_strconcat (_A(3),".",_A(34),NULL);
		e2_menu_add_action (submenu, _("_Other"), NULL,
			_("Open the configuration dialog at the output options page"),
			item_name, _C(28)); //_("output")
		g_free (item_name);
#ifdef E2_TABS_DETACH
	}
#endif
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);
	if (event_button == 0)
		gtk_menu_popup ((GtkMenu*)menu, NULL, NULL,
			(GtkMenuPositionFunc) e2_output_set_menu_position,
			textview, 0, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup ((GtkMenu*)menu, NULL, NULL,
			NULL, NULL, event_button, event_time);
}
/**
@brief create a new textview and buffer and some basic tags

@param rt runtime data struct for the tab (NOT app.tab)

@return scrolled window containing a textview
*/
static GtkWidget *_e2_output_create_view (E2_OutputTabRuntime *rt)
{
	//init some vars
	//at the outset, assume that the current content is on-screen
	rt->onscreen = 1;
	//visible flag is set elsewhere, depending on cache data
	//create scrolled window
	rt->scroll = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_OUT);

	//create text view
	rt->text = GTK_TEXT_VIEW (gtk_text_view_new ());
	gtk_container_add (GTK_CONTAINER (rt->scroll), GTK_WIDGET (rt->text));
	gtk_text_view_set_editable (rt->text, FALSE);
	gtk_text_view_set_cursor_visible (rt->text, FALSE);
//	allow focus so popup menu signal can happen, & can select text in the pane
//	GTK_WIDGET_UNSET_FLAGS (rt->text, GTK_CAN_FOCUS);

	gtk_text_view_set_wrap_mode (rt->text,
		e2_option_int_get ("output-wrap-mode"));
	gtk_text_view_set_left_margin (rt->text,
		e2_option_int_get ("output-left-margin"));
	gtk_text_view_set_right_margin (rt->text,
		e2_option_int_get ("output-right-margin"));
	const gchar *fntname = e2_utils_get_output_font ();
	PangoFontDescription *font_desc = pango_font_description_from_string
			(fntname);
#ifdef USE_GTK3_0
	gtk_widget_override_font (GTK_WIDGET (rt->text), font_desc);
#else
	gtk_widget_modify_font (GTK_WIDGET (rt->text), font_desc);
#endif
	pango_font_description_free (font_desc);

	//signal used for "links" in the output pane
//	g_signal_connect (G_OBJECT (rt->text), "motion-notify-event",
//		G_CALLBACK (test_cb), NULL);
	gtk_widget_set_events (GTK_WIDGET (rt->text),
		gtk_widget_get_events (GTK_WIDGET (rt->text))
		| GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK);
	g_signal_connect (G_OBJECT (rt->text), "popup-menu",
		G_CALLBACK (_e2_output_popup_menu_cb), rt);

#ifdef E2_MOUSECUSTOM
	e2_output_register_pointerbindings (GTK_WIDGET (rt->text));
#endif
	//this cb does generic stuff whether or not E2_MOUSECUSTOM applies
	g_signal_connect (G_OBJECT (rt->text), "button-press-event",
		G_CALLBACK (_e2_output_button_press_cb), rt);
//	g_signal_connect_after (G_OBJECT (rt->text), "button-release-event",
//		G_CALLBACK (_e2_output_button_release_cb), rt); if this is used, log the presses
	e2_output_register_keybindings (GTK_WIDGET (rt->text));

	//arrange to retain any manual scroll away from latest output
	//YUK no "activate" available, nor so for any ancestor
//	g_signal_connect_after (G_OBJECT (rt->text->vadjustment), "value-changed",
	GtkAdjustment *vadj = gtk_scrolled_window_get_vadjustment
		(GTK_SCROLLED_WINDOW (rt->scroll));
	g_signal_connect_after (G_OBJECT (vadj), "value-changed",
		G_CALLBACK (_e2_output_scrolled_cb), rt);

	gtk_widget_show (GTK_WIDGET (rt->text));

	rt->buffer = gtk_text_view_get_buffer (rt->text);

	gtk_text_buffer_create_tag (rt->buffer, "bold", "weight", PANGO_WEIGHT_BOLD, NULL);
	gtk_text_buffer_create_tag (rt->buffer, "italic", "style", PANGO_STYLE_ITALIC, NULL);
	gtk_text_buffer_create_tag (rt->buffer, "uline", "underline", PANGO_UNDERLINE_SINGLE, NULL);
	gtk_text_buffer_create_tag (rt->buffer, "small", "weight", PANGO_WEIGHT_NORMAL,
		"size", (gint) (PANGO_SCALE_SMALL * app.output.font_size), NULL);
	gtk_text_buffer_create_tag (rt->buffer, "cmand", "foreground",
		e2_option_str_get ("color-command"), NULL);
	gtk_text_buffer_create_tag (rt->buffer, "posit", "foreground",
		e2_option_str_get ("color-positive"), NULL);
	gtk_text_buffer_create_tag (rt->buffer, "negat", "foreground",
		e2_option_str_get ("color-negative"), NULL);
	gtk_text_buffer_create_tag (rt->buffer, "unimp", "foreground",
		e2_option_str_get ("color-unimportant"), NULL);
/* these not needed unless output links are parsed
	gtk_text_buffer_create_tag (rt->tab.buffer, "link", "foreground", "blue",
		"underline", PANGO_UNDERLINE_NONE, NULL);
	gtk_text_buffer_create_tag (rt->tab.buffer, "link-active", "foreground", "blue",
		"underline", PANGO_UNDERLINE_SINGLE, NULL); */

	GtkTextIter start;
	gtk_text_buffer_get_start_iter (rt->buffer, &start);
	rt->mark = gtk_text_buffer_create_mark (rt->buffer, NULL, &start, FALSE); //scroll-helper at start

	//attach options
	rt->opt_wrap = e2_option_attach_value_changed ("output-wrap-mode",
		GTK_WIDGET (rt->text), (HookFunc)_e2_output_set_op_wrap_hook, rt);

	gtk_widget_show (rt->scroll);
	return rt->scroll;
}
#ifdef E2_TABS_DETACH
/**
@brief clear text buffer of notebook tab associated with @a rt
This assumes BGL is closed
@param rt tab runtime data struct

@return
*/
static void _e2_output_clear_buffer (E2_OutputTabRuntime *rt)
{
	//order of things here is to minimise risk when clearing during an ongoing print operation
	GtkTextTagTable *table = gtk_text_buffer_get_tag_table (rt->buffer);
	GtkTextBuffer *buffer = gtk_text_buffer_new (table);
	GtkTextIter start;
	gtk_text_buffer_get_start_iter (buffer, &start);
	GtkTextMark *mark = gtk_text_buffer_create_mark (buffer, NULL, &start, FALSE); //scroll-helper at start
	WAIT_FOR_EVENTS
	gtk_text_view_set_buffer (rt->text, buffer);
	rt->buffer = buffer;
	rt->mark = mark;
	g_atomic_int_set (&rt->onscreen, 1);
	g_object_unref (G_OBJECT (buffer));
#ifdef E2_OUTPUTSTYLES
	//style-tags hash retained, in case of re-use
	if (rt->style_trios != NULL)
	{
		g_hash_table_destroy (rt->style_trios);
		rt->style_trios = NULL;
	}
#endif
}
/**
@brief set or clear detached-related data for tab whose widget is @a child

@param child the notebook tab widget
@param dest_notebook the notebook into which a moved tab will go, NULL = app.outbook

@return
*/
static void _e2_output_tab_set_detached_state (GtkWidget *child, GtkWidget *dest_notebook)
{
	gboolean attaching;
	GtkWidget *window;

	attaching = (dest_notebook == NULL || dest_notebook == app.outbook);
	if (attaching)	//moving to main notebook
		window = NULL;
	else //moving from main notebook or some other drop window
	{
		window = gtk_widget_get_toplevel (dest_notebook);
#ifdef USE_GTK2_18
		if (!gtk_widget_is_toplevel (window))
#else
		if (!GTK_WIDGET_TOPLEVEL (window))
#endif
		{
			//FIXME
			printd (DEBUG, "can't find top window for moved tab");
			return;
		}
	}

	if (child == app.tab.scroll)
	{
		app.tab.detached = !attaching;
		app.tab.dropwindow = window;
	}
	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
	{
		if (((E2_OutputTabRuntime *)member->data)->scroll == child)
		{
			((E2_OutputTabRuntime *)member->data)->detached = !attaching;
			printd (DEBUG, "detached-flag of tab %d set %s",
				((E2_OutputTabRuntime *)member->data)->labelnum, (attaching) ? "FALSE":"TRUE");
			((E2_OutputTabRuntime *)member->data)->dropwindow = window;
			break;
		}
	}
}
/**
@brief move tab whose widget is @a child from @a newbook to @a oldbook

@param child tab widget
@param frombook notebook from which the tab will be removed
@param tobook notebook to which the tab will be appended

@return
*/
static void _e2_output_tab_move (GtkWidget *child,
	GtkNotebook *frombook, GtkNotebook *tobook)
{
	GtkWidget *tab_label, *menu_label;
	gboolean tab_expand, tab_fill;	//, reorderable, detachable;
	guint tab_pack;
	//this is essentially the same process that gtk uses
	g_object_ref (G_OBJECT (child));
	tab_label = gtk_notebook_get_tab_label (frombook, child);
	if (tab_label)
		g_object_ref (G_OBJECT (tab_label));
	menu_label = gtk_notebook_get_menu_label (frombook, child);
	if (menu_label)
		g_object_ref (G_OBJECT (menu_label));

	gtk_container_child_get (GTK_CONTAINER (frombook), child,
		"tab-expand", &tab_expand,
		"tab-fill", &tab_fill,
		"tab-pack", &tab_pack,
//		"reorderable", &reorderable,
//		"detachable", &detachable,
		NULL);

	gtk_container_remove (GTK_CONTAINER (frombook), child);
	gtk_notebook_append_page_menu (tobook, child, tab_label, menu_label);

	gtk_container_child_set (GTK_CONTAINER (tobook), child,
		"tab-pack", tab_pack,
		"tab-expand", tab_expand,
		"tab-fill", tab_fill,
		"reorderable", TRUE,	//reorderable,
		"detachable", TRUE,	//detachable,
		NULL);

	g_object_unref (G_OBJECT (child));
	if (tab_label)
		g_object_unref (G_OBJECT (tab_label));
	if (menu_label)
		g_object_unref (G_OBJECT (menu_label));

	if (child == app.tab.scroll)
	{	//this is the default tab
		gtk_notebook_set_current_page (tobook, -1);
	}
	//update detached-data for the tab
	_e2_output_tab_set_detached_state (child, GTK_WIDGET (tobook));
}
#endif //def E2_TABS_DETACH

  /*********************/
 /***** callbacks *****/
/*********************/
/* *
@brief save selected output-pane text

@param menuitem UNUSED the selected widget, or NULL
@param rt runtime struct to work on

@return
*/
/*static void _e2_output_savesel_cb (GtkWidget *menuitem,	E2_OutputTabRuntime *rt)
{
	NEEDCLOSEBGL
	e2_edit_dialog_save_selected (rt->buffer,
#ifdef E2_VFS
	NULL,	//local namespace assumed
#endif
	app.main_window);
	NEEDOPENBGL
} */
/**
@brief edit output-pane-tab text

@param menuitem UNUSED the activated widget, or NULL
@param rt data struct for the tab

@return
*/
static void _e2_output_edit_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt)
{
	//editing an empty buffer will cause a freeze, so we fake some content...
	GtkTextIter start, end;
	NEEDCLOSEBGL
	gtk_text_buffer_get_bounds (rt->buffer, &start, &end);
	if (gtk_text_iter_equal (&start, &end))
		e2_output_print (rt, " ", NULL, FALSE, NULL);

	e2_edit_dialog_create (NULL, rt->buffer);
	NEEDOPENBGL
}
/**
@brief handle an output-pane tab change
This is callback for the notebook's "switch-page" signal
Essentially, tab-specific data are swapped between stack and heap space,
and pointers for affected running commands are adjusted accordingly
@param notebook the notebook widget
@param page UNUSED (GTK2=UNDOCUMENTED STRUCT, GTK3=page widget) data for notebook page which is now focused
@param page_num the 0-based index of the new page
@param data UNUSED pointer to data specified when callback was connected

@return
*/
static void _e2_output_tabchange_cb (GtkNotebook *notebook,
#ifdef USE_GTK3_0
	GtkWidget *page,
#else
	GtkNotebookPage *page,
#endif
	guint page_num, gpointer data)
{
	E2_OutputTabRuntime *newtab;
	NEEDCLOSEBGL
#ifdef E2_TABS_DETACH
	GtkWidget *child = gtk_notebook_get_nth_page (notebook, page_num);
	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
	{
		newtab = (E2_OutputTabRuntime *)member->data;
		if (newtab->scroll == child)
		{
			printd (DEBUG, "output tab change cb, new current-tab ID is %d", newtab->labelnum);
			break;
		}
	}
	if (member == NULL)
	{
		NEEDOPENBGL
		return;	//should never happen
	}
#else
	printd (DEBUG, "output tab change cb, new current-tab index %d", page_num);
//	printd (DEBUG, "curr_tab is %x", curr_tab);
	newtab = g_list_nth_data (app.tabslist, page_num);
#endif
	//swap with mimimum race-risk ...
	*curr_tab = app.tab;	//backup current tab's data from stack to heap
	app.tab = *newtab;	//get the replacement stuff into stackspace
	curr_tab = newtab;
//	printd (DEBUG, "curr_tab NOW is %x", curr_tab);
	//adjust all relevant child foreground-tab pointers to/from the stacked tab data
	e2_command_retab_children (&app.tab, curr_tab);
	NEEDOPENBGL
}
/* *
@brief process an 'intercepted' double-click on the output pane
This handles 'activated' text in the output pane.

@param rt runtime struct for the pane

@return TRUE if the press was handled
*/
/*static gboolean _e2_output_activated_cb (E2_OutputTabRuntime *rt)
{
	printd (DEBUG, "output activated cb");
	GtkTextIter start, end;
	NEEDCLOSEBGL
	if (gtk_text_buffer_get_selection_bounds (rt->buffer, &start, &end))
	{
		gchar *seltext = gtk_text_buffer_get_text (rt->buffer, &start, &end, FALSE);
		printd (DEBUG, "output text is %s", seltext);
		_e2_output_open_text (seltext, TRUE);
		NEEDOPENBGL
		g_free (seltext);
//BAD - this locks text DnD on
//		output_activated = TRUE;	//prevent normal button-release handling
		return TRUE;	//block internal click handling
	}
	NEEDOPENBGL
	printd (DEBUG, "nothing selected in output");
//	output_activated = FALSE;	//allow normal button-release handling
	return FALSE;
}
*/
# ifdef E2_PTRGESTURES
/* *
@brief process mouse button-release in the output pane
This is to block default handling of modified events, so that gestures etc are
not interfered with
@param textview the widget where the click happened
@param event gdk event data struct
@param user_data UNUSED pointer specified when cb was connected

@return TRUE if modifier key(s) were pressed
*/
/*BUGGY
static gboolean _e2_output_button_filter_cb (GtkWidget *textview,
	GdkEventButton *event, gpointer user_data)
{
	return ((event->state & E2_MODIFIER_MASK) != 0);
}
*/
#endif
/**
@brief process mouse button-press in the output pane

1/L = focus, 2/M = hide, 3/R = context menu
<Ctrl>L = open
This also detects left-button double-clicks (for which there is no API)
and performs an open if relevant

@param textview the widget where the click happened
@param event gdk event data struct
@param rt runtime struct to work on

@return TRUE if the signal has been handled here
*/
static gboolean _e2_output_button_press_cb (GtkWidget *textview,
	GdkEventButton *event, E2_OutputTabRuntime *rt)
{
/*	GtkTextWindowType wtype = gtk_text_view_get_window_type (
		GTK_TEXT_VIEW (textview), event->window);
	if (wtype != GTK_TEXT_WINDOW_TEXT)
		return FALSE;
*/
	printd (DEBUG, "output button press cb");
	gboolean retval = FALSE;
	NEEDCLOSEBGL
#ifdef E2_TABS_DETACH
	//just changing notebook page is not sufficient to set current tab when
	//there's > 1 notebook
	if (event->type == GDK_BUTTON_PRESS)
	{
		if (rt != curr_tab)
		{
			//swap with mimimum race-risk ...
			*curr_tab = app.tab;	//backup current tab's data from stack to heap
			app.tab = *rt;	//get the replacement stuff into stackspace
			curr_tab = rt;
			//adjust all relevant child foreground-tab pointers to/from the stacked tab data
			e2_command_retab_children (&app.tab, curr_tab);
			printd (DEBUG, "output tab change, new current-tab ID is %d", curr_tab->labelnum);
		}
	}
#endif
	E2_OutputTabRuntime *rrt = (rt == curr_tab) ? &app.tab : rt;
	/*for double-clicks, the callback sequence is:
	1, then another < click interval, then 1 more with 0 click interval
	for triple-clicks, the callback sequence is:
	1, then 2 * last 2 of double click sequence = 5 total
	The 0-click interval clicks are of type GDK_2BUTTON_PRESS or
	GDK_3BUTTON_PRESS as appropriate
	*/
	if (event->button == 1 && event->type == GDK_BUTTON_PRESS //no multi-click
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		static gboolean valid_selection = FALSE;
		static GtkTextIter start, end;
		extern guint click_interval;
		static guint32 last_event_time = 0;
		guint32 interval;

#ifndef E2_MOUSECUSTOM
		if (event->state & GDK_CONTROL_MASK)
		{
			gchar *text = _e2_output_get_item_at_pointer (event->x, event->y, rrt, TRUE);
			if (text != NULL)
			{
				_e2_output_open_text (text, TRUE);
				g_free (text);
				NEEDOPENBGL
				return TRUE; //CHECKME FALSE if open_text failed ?
			}
		}
#endif
		interval = event->time - last_event_time;
		last_event_time = event->time;
//		printd (DEBUG, "output button press interval %d", interval);
		//we don't want to repeat the action, if there was a double/triple click
		if (interval >= click_interval)
		{

//			output_activated = FALSE;	//reinstate normal release handling
			//because a selection will be cleared by thge normal release
			//callback, note what is selected in case it's a double-click
			valid_selection = gtk_text_buffer_get_selection_bounds (rrt->buffer,
				&start, &end);
/*			//CHECKME is this sensible ?? it does not focus the commandline
			gchar *action_name = g_strconcat (_A(1),".",_A(50),NULL);  //_("command.focus")
			OPENBGL
			e2_action_run_simple (action_name, textview);
			CLOSEBGL
			g_free (action_name);
*/
			//focus the output tab if it's not going to be hidden
			if (!e2_option_bool_get_direct (app.output.opt_hide_on_focus_out)
#ifdef E2_TABS_DETACH
				|| rrt->detached
#endif
				)
				gtk_widget_grab_focus (textview);
		}
		else
		{
			//reselect what was de-selected by the intervening release callback,
			//ready for the next press callback (type GDK_2BUTTON_PRESS etc)
			if (valid_selection)
			{
				gtk_text_buffer_select_range (rrt->buffer, &start, &end);
				valid_selection = FALSE;
			}
		}
	}
#ifndef E2_MOUSECUSTOM
	else if (event->button == 1 &&
	//		(
			event->type == GDK_2BUTTON_PRESS //|| event->type == GDK_3BUTTON_PRESS)
			)
	{
		printd (DEBUG, "output button press event type %d", event->type);
		gchar *text = _e2_output_get_item_at_pointer (event->x, event->y, rrt, TRUE);
		if (text != NULL)
		{
			retval = _e2_output_open_text (text, TRUE);
			g_free (text);
//			if (retval) retval = _e2_output_activated_cb (rrt);
		}
		//FIXME stop the selected text from being de-selected in the
		//button-release callback - but we can't just block that !!
	}
	else if (event->button == 2
# ifdef E2_TABS_DETACH
		&& !rrt->detached
# endif
		&& event->state & GDK_CONTROL_MASK )	//differentiate from default gtk btn2 behaviour
	{
		e2_window_output_hide (NULL, NULL, NULL);
		retval = TRUE;
	}
#endif //ndef E2_MOUSECUSTOM
	else if (event->button == 3)
	{
		if ((event->state & E2_MODIFIER_MASK) == 0)
		{
			_e2_output_show_context_menu (textview, 3, event->time, rrt);
			retval = TRUE;
		}
	}
	NEEDOPENBGL
	return retval;
}
/**
@brief process mouse button-release in the output pane

This is essentially to re-select text which is de-selected by
the standard release callback

@param textview the widget where the release happened
@param event gdk event data struct
@param rt runtime struct to work on

@return TRUE if the signal has been handled
*/
/*static gboolean _e2_output_button_release_cb (GtkWidget *textview,
	GdkEventButton *event, E2_OutputTabRuntime *rt)
{
	printd (DEBUG, "output button release cb");
	return FALSE;
	E2_OutputTabRuntime *rrt = (rt == curr_tab) ? &app.tab : rt;
	NEEDCLOSEBGL
	gtk_text_buffer_select_range (rrt->buffer, &start, &end);
	NEEDOPENBGL
	return TRUE;
} */
/**
@brief process menu-key press in the output pane

@param widget the textview widget where the press happened
@param rt output runtime struct to work on

@return TRUE, as the signal is always handled
*/
static gboolean _e2_output_popup_menu_cb (GtkWidget *widget, E2_OutputTabRuntime *rt)
{
	E2_OutputTabRuntime *rrt = (rt == curr_tab) ? &app.tab : rt;
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_output_show_context_menu (widget, 0, event_time, rrt);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief Determine whether the buffer end-mark is visible in its output pane-tab, after manual change to the vertical adjustment

This is a callback for manual changes to the output textview vertical adjustment

@param adjust UNUSED the vertical adjustment for the textview
@param rt data struct for the output pane

@return
*/
static void _e2_output_scrolled_cb (GtkAdjustment *adjust, E2_OutputTabRuntime *rt)
{
	//use active-tab data when relevant
	E2_OutputTabRuntime *rrt = (rt == curr_tab) ? &app.tab : rt;

	//FIXME make this faster - local copy of the option value ?
	GtkTextIter iter;
	if (e2_option_bool_get_direct (app.output.opt_jump_end))
		//scrolling only when new output belongs to the last context in the textbuffer
		gtk_text_buffer_get_end_iter (rrt->buffer, &iter);
	else
		gtk_text_buffer_get_iter_at_mark (rrt->buffer, &iter, rrt->mark);

#ifdef DEBUG_MESSAGES
	gint old = g_atomic_int_get (&rrt->onscreen);
#endif
	_e2_output_iter_check_visible (rrt, &iter);
#ifdef DEBUG_MESSAGES
	if (g_atomic_int_get (&rrt->onscreen) == 0)
	{
		if (old == 1)
			printd (NOTICE, "not following anymore");
	}
	else // == 1
		if (old == 0)
			printd (NOTICE, "following again");
#endif
}
/*
static gboolean test_cb (GtkWidget *widget, GdkEventMotion *event)
{
	if (event->is_hint)
	{
		gint x, y;
		gpointer pos;

		NEEDCLOSEBGL
#ifdef USE_GTK3_0
		pos = gdk_window_get_device_position (event->window, event->device,
				&x, &y, NULL);
#else
		pos = gdk_window_get_pointer (event->window, &x, &y, NULL);
#endif
		NEEDOPENBGL
		//CHECKME pos = NULL check may be irrelevant
		if (pos != NULL)
			printd (DEBUG, "x: %d y: %d", x, y);
	}
	else
	{
		printd (DEBUG, "x: %lf y: %lf", event->x, event->y);
	}
	return FALSE;
}
*/

/**
@brief perform copy

@param menuitem UNUSED the selected widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_output_copy_cb (GtkMenuItem *menuitem,
	E2_OutputTabRuntime *rt)
{
	NEEDCLOSEBGL
	GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
	gtk_text_buffer_copy_clipboard (rt->buffer, cb);
	NEEDOPENBGL
}

#ifdef E2_TABS_DETACH
static void _e2_output_tabattach_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt)
{
	GtkNotebook *newbook = GTK_NOTEBOOK (gtk_bin_get_child (GTK_BIN (rt->dropwindow)));
	NEEDCLOSEBGL
	_e2_output_tab_move (rt->scroll, newbook, GTK_NOTEBOOK (app.outbook));
	NEEDOPENBGL
}

static void _e2_output_clear_cb (GtkMenuItem *menuitem, E2_OutputTabRuntime *rt)
{
	NEEDCLOSEBGL
	_e2_output_clear_buffer (rt);
	NEEDOPENBGL
}
/**
@brief process a tab being dragged
This is called for second and later drops onto a tabdrop window and for all
drops back onto the output-pane main notebook
Page-widget properties are not changed when the page is dragged
@param dest_notebook the notebook to which a tab is being dragged
@param context drag context data
@param x X coordinate where the drop happens
@param y Y coordinate where the drop happens
@param sel_data the received data
@param info the info registered for the target in target_table2
@param time timestamp at which the data was received
@param user_data UNUSED data specified when the callback was connected

@return
*/
static void _e2_output_tabdrag_data_received_cb (
	GtkWidget        *dest_notebook,
	GdkDragContext   *context,
	gint              x,
	gint              y,
	GtkSelectionData *sel_data,
	guint             info,
	guint             time,
	gpointer          user_data)
{
	printd (DEBUG, "_e2_output_tab_drag_data_received_cb");
	gboolean success;
	NEEDCLOSEBGL
#ifdef USE_GTK2_14
	if (gtk_selection_data_get_length(sel_data) > 0)
#else
	if (sel_data->length > 0)
#endif
	{
		GtkWidget *source_notebook = gtk_drag_get_source_widget (context);
#ifdef USE_GTK2_14
		GtkWidget *child = *(GtkWidget **) gtk_selection_data_get_data (sel_data);
#else
		GtkWidget *child = *(GtkWidget **)sel_data->data;
#endif
		if (source_notebook == dest_notebook)
		{
//			printd (DEBUG, "trying to drag to same place");
			success = FALSE;
		}
		else if (source_notebook == app.outbook
			&& gtk_notebook_get_n_pages (GTK_NOTEBOOK (source_notebook)) == 1)
		{	//prevent dragging the only output-pane tab
//			printd (DEBUG, "trying to drag last output tab");
			success = FALSE;
		}
		else
		{
			success = TRUE;
			_e2_output_tab_set_detached_state (child, dest_notebook);
			//prevent changes of default tab, by block here
			//(WAS unblock in tab-removed cb, but that spits error - seems
			//that gtk unblocks this all by itself !
			g_signal_handlers_block_by_func (G_OBJECT (source_notebook),
				_e2_output_tabchange_cb, NULL);
		}
	}
	else
		success = FALSE;
#ifdef USE_GTK3_0
//CHECKME nothing available for changing target
#else
	if (!success)
		sel_data->target = GDK_NONE;
#endif

	gtk_drag_finish (context, success, FALSE, time);

	NEEDOPENBGL
}
/**
@brief cleanup when a tab-drop window becomes empty
This is a callback for the "page-removed" signal on @a notebook
@param notebook the affected notebook
@param child UNUSED the widget for the removed page
@param page_num UNUSED the child page number
@param window the parent window for @a notebook

@return
*/
static void _e2_output_tabgone_cb (GtkNotebook *notebook, GtkWidget *child,
	guint page_num, GtkWidget *window)
{
	printd (DEBUG, "_e2_output_tabgone_cb");
	if (notebook != GTK_NOTEBOOK (app.outbook) &&
		gtk_notebook_get_n_pages (notebook) == 0)
	{
		NEEDCLOSEBGL
		gtk_widget_destroy (window);
		NEEDOPENBGL
	}
/* this generates warning - seems as if gtk already unblocked the thing
	else
		//revert the block added when the drag was in progress
		g_signal_handlers_unblock_by_func (G_OBJECT (notebook),
			_e2_output_tabchange_cb, NULL);
*/
}
/**
@brief cleanup when closing a tab-drop window
This moves the tab(s) in the closing window back to the output-pane notebook
@param window the window being closed
@param event UNUSED event data struct
@param data UNUSED data specified when callback was connected

@return FALSE to allow event to propogate
*/
static gboolean _e2_output_tabrestore_cb (GtkWidget *window, GdkEvent *event,
	gpointer data)
{
	printd (DEBUG, "_e2_output_tabrestore_cb ()");
	gint j;
	NEEDCLOSEBGL
	GtkNotebook *newbook = GTK_NOTEBOOK (gtk_bin_get_child (GTK_BIN (window)));
	if (newbook != NULL && (j = gtk_notebook_get_n_pages (newbook)) > 0)
	{
		//prevent sequential tab-changes during the cleanout process
		g_signal_handlers_disconnect_by_func ((gpointer)newbook,
			_e2_output_tabchange_cb, NULL);
		//no need to go back and check for empty book
		g_signal_handlers_disconnect_by_func ((gpointer)newbook,
			_e2_output_tabgone_cb, window);
		GtkWidget *defchild = app.tab.scroll;	//remember which page is default now
		gint i;
		for (i = 0; i < j; i++)
		{
			GtkWidget *child = gtk_notebook_get_nth_page (newbook, 0);
			_e2_output_tab_move (child, newbook, GTK_NOTEBOOK (app.outbook));
		}
		//reset the default page in the destination book
		gint indx = gtk_notebook_page_num (GTK_NOTEBOOK (app.outbook), defchild);
		if (indx != -1)
		{
			g_signal_handlers_block_by_func (G_OBJECT (app.outbook),
				_e2_output_tabchange_cb, NULL);
			gtk_notebook_set_current_page (GTK_NOTEBOOK (app.outbook), indx);
			g_signal_handlers_unblock_by_func (G_OBJECT (app.outbook),
				_e2_output_tabchange_cb, NULL);
		}
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief when a mouse-button is pressed on tab "label", update current-page data if needed
This is a callback for the "grab-focus" signal on @a notebook
It's called after a tab-change, if any
@param notebook the affected notebook
@param user_data UNUSED data specified when the callback was connected

@return
*/
static void _e2_output_grab_focus_cb (GtkWidget *notebook, gpointer user_data)
{
	printd (DEBUG, "_e2_output_grab_focus_cb");
	NEEDCLOSEBGL
	gint curindx = gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook));
	GtkWidget *child = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook),
		curindx);
	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
	{
		E2_OutputTabRuntime *tab;
		tab = (E2_OutputTabRuntime *)member->data;
		if (tab->scroll == child)
		{
			if (tab != curr_tab)
			{
				//swap with mimimum race-risk ...
				*curr_tab = app.tab;	//backup current tab's data from stack to heap
				app.tab = *tab;	//get the replacement stuff into stackspace
				curr_tab = tab;
				//adjust all relevant child foreground-tab pointers to/from the stacked tab data
				e2_command_retab_children (&app.tab, curr_tab);
				printd (DEBUG, "output tab change, new current-tab ID is %d", tab->labelnum);
			}
			break;
		}
	}
	NEEDOPENBGL
}
//for "focus-in-event" NO USE
/*static gboolean _e2_output_focus_in_cb (GtkWidget *widget, GdkEventFocus *event,
	gpointer user_data)
{
	printd (DEBUG, "_e2_output_focus_in_cb");
	return FALSE;
}
void _e2_output_page_add_cb (GtkNotebook *notebook, GtkWidget *child,
		guint page_num, gpointer user_data)
{
	printd (DEBUG, "_e2_output_page_add_cb");
} */
/**
@brief when a detached notebook tab is dropped in an empty area, create a
window containing a notebook to receive the tab
Page-widget properties are not changed when dragged
Expects BGL closed. For gtk >= 2.12, this is a callback. For gtk 2.10, it's a
GtkNotebookWindowCreationFunc
@param source the source GtkNotebook of the drag operation
@param sw the child GtkWidget to be dropped
@param x the X coordinate where the drop happens
@param y the Y coordinate where the drop happens
@param data data specified when the arrangement was set up

@return the created GtkNotebook where the tab will be attached, or NULL to cancel the drag
*/
static GtkNotebook *_e2_output_tab_drop_new (GtkNotebook *source, GtkWidget *sw,
	gint x, gint y, gpointer data)
{
	printd (DEBUG, "_e2_output_tab_drop_new ()");
	printd (DEBUG, "name of current tab is %d", curr_tab->labelnum);

	if (gtk_notebook_get_n_pages (source) == 1)
		return NULL;	//prevent dragging of the only tab

	GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	e2_window_set_title (window, _("output tabs")); //needs BGL closed
//	gtk_window_set_role (GTK_WINDOW (window), "tabdrop");
//	gtk_window_set_wmclass (GTK_WINDOW (window), "main", BINNAME);
#ifdef E2_COMPOSIT
	e2_window_set_opacity (window, -1);
#endif
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
	//restore dropped tab(s) when new window is closed
	g_signal_connect (G_OBJECT (window), "delete-event",
		G_CALLBACK (_e2_output_tabrestore_cb), NULL);

	GtkWidget *notebook = gtk_notebook_new ();
	printd (DEBUG, "new notebook widget = %x", notebook);
	gtk_container_add (GTK_CONTAINER (window), notebook);
//CRASHER gtk_drag_source_set (notebook, GDK_BUTTON1_MASK, target_table2, n_targets2,
//		GDK_ACTION_PRIVATE);
	//setup to process further tabs being dragged here
	gtk_drag_dest_set (notebook, GTK_DEST_DEFAULT_DROP, target_table2, n_targets2,
		GDK_ACTION_MOVE);
	g_signal_connect (G_OBJECT (notebook), "drag-data-received",
		G_CALLBACK (_e2_output_tabdrag_data_received_cb), NULL);	//CHECKME user_data
	//close window when all tabs dragged out or re-attached by menu-selection
	g_signal_connect (G_OBJECT (notebook), "page-removed",
		G_CALLBACK (_e2_output_tabgone_cb), window);
	//capture clicks on tab labels
	g_signal_connect (G_OBJECT (notebook), "grab-focus",
		G_CALLBACK (_e2_output_grab_focus_cb), NULL);
//	g_signal_connect (G_OBJECT (notebook), "focus-in-event",
//		G_CALLBACK (_e2_output_focus_in_cb), NULL);
//	g_signal_connect (G_OBJECT (notebook), "page-added",
//		G_CALLBACK (_e2_output_page_add_cb), NULL);
	g_signal_connect (G_OBJECT (notebook), "switch-page",
		G_CALLBACK (_e2_output_tabchange_cb), NULL);	//no data
#ifdef USE_GTK2_12DND
	//allow further dragging from this window
	//(this seems to be un-necessary here, though that may be a gtk deficiency)
	g_signal_connect (G_OBJECT (notebook), "create-window",
		G_CALLBACK (_e2_output_tab_drop_new), NULL);	//CHECKME user_data
#endif
#ifdef USE_GTK3_0
	g_signal_connect (G_OBJECT (window), "key-press-event",
		G_CALLBACK (e2_window_key_cb), GUINT_TO_POINTER(1)); //includes translation, non-NULL data to avoid disconnection during bindings setup
	g_signal_connect (G_OBJECT (window), "key-release-event",
		G_CALLBACK (e2_window_key_cb), NULL);
#else
	//arrange for translation of mod-alphabetic-keycodes from locale to ascii
	g_signal_connect (G_OBJECT (window), "key-press-event",
		G_CALLBACK (e2_utils_key_translate_cb), GUINT_TO_POINTER(1));	//non-NULL data to avoid disconnection during bindings setup
	g_signal_connect (G_OBJECT (window), "key-release-event",
		G_CALLBACK (e2_utils_key_translate_cb), NULL);
#endif
	e2_keybinding_enrol (window, _C(17), (void(*)(E2_OptionSet*))NULL);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (window, _C(17), (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (window, _C(17), (void(*)(E2_OptionSet*))NULL);
# endif
#endif

	GtkNotebook *book = GTK_NOTEBOOK (notebook);
	gtk_notebook_popup_enable (book);
	gtk_notebook_set_scrollable (book, TRUE);
	gtk_notebook_set_show_border (book, FALSE);
	gtk_notebook_set_show_tabs (book, TRUE);

	GtkCornerType where = (GtkCornerType) e2_option_sel_get ("scrollbar-position");
#ifdef ADJACENT_TABS
	if (where == GTK_CORNER_TOP_LEFT || where == GTK_CORNER_BOTTOM_LEFT)
		gtk_notebook_set_tab_pos (book, GTK_POS_RIGHT);
	else
		gtk_notebook_set_tab_pos (book, GTK_POS_LEFT);
#else
	if (where == GTK_CORNER_TOP_LEFT || where == GTK_CORNER_BOTTOM_LEFT)
		gtk_notebook_set_tab_pos (book, GTK_POS_LEFT);
	else
		gtk_notebook_set_tab_pos (book, GTK_POS_RIGHT);
#endif
	// these 2 needed ?
//	gtk_notebook_set_tab_reorderable (book, sw, TRUE);
//	gtk_notebook_set_tab_detachable (book, sw, TRUE);
#ifdef USE_GTK2_12DND
# ifdef USE_GTK3_0
	const gchar *group = gtk_notebook_get_group_name (source);
	gtk_notebook_set_group_name (book, group);
# else
	gpointer group = gtk_notebook_get_group (source);
	gtk_notebook_set_group (book, group);
# endif
#else
	gint group_id = gtk_notebook_get_group_id (source);
	gtk_notebook_set_group_id (book, group_id);
#endif
#ifdef USE_GTK2_18
	GtkAllocation alloc;
	GtkAllocation alloc2;
	gtk_widget_get_allocation (app.main_window, &alloc);
	gtk_widget_get_allocation (app.tab.scroll, &alloc2);
#endif
	gtk_window_set_default_size (GTK_WINDOW (window),
#ifdef USE_GTK2_18
		alloc.width, alloc2.height);
#else
		app.main_window->allocation.width, app.tab.scroll->allocation.height);
#endif
	gtk_widget_show_all (window);

	//dragging has made the dragged tab "current", so we can set data for that
	app.tab.dropwindow = window;
	//set flag for tab context-menu so it shows only items relevant to separate window
	app.tab.detached = TRUE;
	printd (DEBUG, "detached-flag of tab %d set TRUE", app.tab.labelnum);
	//gtk will change the active tab in the source notebook and then the active
	//page in the dest notebook - any way to prevent the former of these ?

	return book;
}
#endif	//def E2_TABS_DETACH

  /*******************/
 /***** actions *****/
/*******************/
/**
@brief print to output pane, with default settings

Any escaped newlines are converted to real ones.

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_output_print_action (gpointer from, E2_ActionRuntime *art)
{
	gchar *real_message = e2_utils_str_replace ((gchar *)art->data , "\\n", "\n");
	e2_output_print (&app.tab, real_message, NULL, FALSE, NULL);
	g_free (real_message);
	return TRUE;
}
/**
@brief print help message to output pane
This expects as data a string indicating which message is to be displayed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if type of help was recognised
*/
static gboolean _e2_output_help_action (gpointer from, E2_ActionRuntime *art)
{
	gchar *arg = (gchar *)art->data;
	if ((arg == NULL) || (*arg == '\0') || g_str_has_prefix (arg, _("commands")))
	{
		e2_command_output_help ();
		return TRUE;
	}
	else if (g_str_has_prefix (arg, _("keys")))	//this must be translated same as in default aliases
	{
		e2_keybinding_output_help (arg);
		return TRUE;
	}
#ifdef E2_MOUSECUSTOM
	else if (g_str_has_prefix (arg, _("buttons")))	//this must be translated same as in default aliases
	{
		e2_mousebinding_output_help (arg);
		return TRUE;
	}
#endif
	return FALSE;
}
/**
@brief move the displayed text window by @a arg 'steps'
This expects as data a string containing the no. of 'steps' to move
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_output_scroll_step (gpointer from, E2_ActionRuntime *art)
{
	//action data = TRUE to move down, FALSE to move up
	gboolean down = GPOINTER_TO_INT (art->action->data);
	_e2_output_scroll_helper (down, (gchar *)art->data, FALSE);
	return TRUE;
}
/**
@brief move the displayed text window by @a arg 'pages'
This expects as data a string containing the no. of 'pages' to move
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_output_scroll_page (gpointer from, E2_ActionRuntime *art)
{
	//action data = TRUE to move down, FALSE to move up
	gboolean down = GPOINTER_TO_INT (art->action->data);
	_e2_output_scroll_helper (down, (gchar *)art->data, TRUE);
	return TRUE;
}
/**
@brief move the displayed text window to start or end of its content

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_output_scroll_all (gpointer from, E2_ActionRuntime *art)
{
	E2_OutputTabRuntime *rt = &app.tab;
	g_return_val_if_fail (rt->scroll != NULL, FALSE);
	GtkAdjustment *vadj;
	vadj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (rt->scroll));
	//action data = TRUE to move down, FALSE to move up
	gboolean down = GPOINTER_TO_INT (art->action->data);
	if (down)
		gtk_adjustment_set_value (vadj,
#ifdef USE_GTK2_14
			(gtk_adjustment_get_upper (vadj) - gtk_adjustment_get_page_size (vadj))
#else
			vadj->upper - vadj->page_size
#endif
		);
	else
		gtk_adjustment_set_value (vadj, 0.0);
	return TRUE;
}
/**
@brief clear all content of an output-pane tab

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_output_clear_action (gpointer from, E2_ActionRuntime *art)
{
#ifdef E2_TABS_DETACH
	_e2_output_clear_buffer (&app.tab);
#else
	//order of things here is to handle clearing when the text buffer is being added to
	GtkTextTagTable *table = gtk_text_buffer_get_tag_table (app.tab.buffer);
	GtkTextBuffer *buffer = gtk_text_buffer_new (table);
	GtkTextIter start;
	gtk_text_buffer_get_start_iter (buffer, &start);
	GtkTextMark *mark = gtk_text_buffer_create_mark (buffer, NULL, &start, FALSE);
	WAIT_FOR_EVENTS
	gtk_text_view_set_buffer (app.tab.text, buffer);

	app.tab.buffer = buffer;
	app.tab.mark = mark;
	g_atomic_int_set (&app.tab.onscreen, 1);
	g_object_unref (G_OBJECT (buffer));
#endif
	*(curr_tab) = app.tab; //listed tab data needs the updated values too
	return TRUE;
}
/**
@brief create another output pane tab, and go there

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_output_tab_add (gpointer from, E2_ActionRuntime *art)
{
	E2_OutputTabRuntime *tab = ALLOCATE0 (E2_OutputTabRuntime);
	CHECKALLOCATEDWARN (tab, return FALSE;)
	GtkWidget *sw = _e2_output_create_view (tab);
	GtkWidget *wid;
#ifdef USE_GTK2_10
	//there may be gaps in the tab labels
	//and for gtk >= 2.10, tabs can be in any order or in another notebook,
	//so check all tabs to find the last one and bump its label
	gint lablid, new = 1;
	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
	{
		lablid = ((E2_OutputTabRuntime *)member->data)->labelnum;
		if (lablid >= new)
			new = lablid + 1;
	}
	tab->labelnum = new;	//remember, for later searching
#else
	wid = gtk_notebook_get_nth_page (GTK_NOTEBOOK (app.outbook), app.tabcount-1);
	const gchar *labtxt = gtk_notebook_get_tab_label_text (GTK_NOTEBOOK (app.outbook), wid);
	gint new = atoi (labtxt) + 1;
#endif
	gchar *txt = g_strdup_printf ("%d", new);	//no translation
	wid = gtk_label_new (txt);
	g_free (txt);
	gtk_notebook_append_page (GTK_NOTEBOOK (app.outbook), sw, wid);
#ifdef USE_GTK2_10
	gtk_notebook_set_tab_reorderable (GTK_NOTEBOOK (app.outbook), sw, TRUE);
#ifdef E2_TABS_DETACH
	gtk_notebook_set_tab_detachable (GTK_NOTEBOOK (app.outbook), sw, TRUE);
#endif
#endif
	app.tabslist = g_list_append (app.tabslist, tab);
	app.tabcount++;	//log, for cache
	if (app.tabcount == 2)	//doesn't matter where the tabs are
		gtk_notebook_set_show_tabs (GTK_NOTEBOOK (app.outbook), TRUE);
	//focus the new tab
#ifdef E2_TABS_DETACH
	new = gtk_notebook_get_n_pages (GTK_NOTEBOOK (app.outbook));
	gtk_notebook_set_current_page (GTK_NOTEBOOK (app.outbook), new-1);
#else
	gtk_notebook_set_current_page (GTK_NOTEBOOK (app.outbook), app.tabcount - 1); //uses new last-tab index
#endif
	return TRUE;
}
/**
@brief remove currently focused output pane tab
This may be called from a keybinding or context menu item
Essentially, tab-specific data are swpped between stack and heap space,
and pointers for affected running commands are adjusted accordingly
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the removal was done
*/
static gboolean _e2_output_tab_remove (gpointer from, E2_ActionRuntime *art)
{
#ifdef E2_TABS_DETACH
	//ignore removal of only tab in this notebook
	gint homecount = gtk_notebook_get_n_pages (GTK_NOTEBOOK (app.outbook));
	if (homecount == 1)
		return FALSE;
	//cleanup the notebook
	//FIXME only if tab in this notebook (context menu cleanup to prevent this ?)
	gint index = gtk_notebook_get_current_page (GTK_NOTEBOOK (app.outbook));
	GtkWidget *sw = gtk_notebook_get_nth_page (GTK_NOTEBOOK (app.outbook), index);
	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
	{
		if (((E2_OutputTabRuntime *)member->data)->scroll == sw)
			break;
	}
	if (member == NULL)
		return FALSE;	//should never happen

	gtk_notebook_remove_page (GTK_NOTEBOOK (app.outbook), index);
	if (--app.tabcount == 1)
		gtk_notebook_set_show_tabs (GTK_NOTEBOOK (app.outbook), FALSE);

	index = gtk_notebook_get_current_page (GTK_NOTEBOOK (app.outbook));
	sw = gtk_notebook_get_nth_page (GTK_NOTEBOOK (app.outbook), index);

	GList *newmember;
	for (newmember = app.tabslist; newmember != NULL; newmember = newmember->next)
	{
		if (((E2_OutputTabRuntime *)newmember->data)->scroll == sw)
			break;
	}
	if (newmember == NULL)
		return FALSE;	//should never happen
/*//CHECKME does this conform to gtk behaviour ?
	GList *member2;
	if (member == app.tabslist)
		member2 = member->next;
	else
		member2 = member->prev;

	if (member2 != newmember)
		printd (DEBUG, "replacement tab not correct !!");
*/
#else
	//ignore removal of only tab
	if (app.tabcount == 1)
		return FALSE;
	//cleanup the notebook
	gint index = gtk_notebook_get_current_page (GTK_NOTEBOOK (app.outbook));
	gtk_notebook_remove_page (GTK_NOTEBOOK (app.outbook), index);
	if (--app.tabcount == 1)
		gtk_notebook_set_show_tabs (GTK_NOTEBOOK (app.outbook), FALSE);

	//CHECKME does this conform to gtk behaviour ?
	GList *member = g_list_nth (app.tabslist, index);
	GList *newmember;
	if (index == 0)
		newmember = member->next;
	else
		newmember = member->prev;
#endif
	E2_OutputTabRuntime *newtab = newmember->data;
//	printd (DEBUG, "curr_tab is %x", curr_tab);
//	printd (DEBUG, "replacement tab at %x", newtab);
	//quickly install replacement data, ready for use by any printing children
	app.tab = *newtab;
	//anthing running in the focused tab stays there,
	//so no need to update foreground pointers
	//but do update background-tab ptrs of children using the tab
	e2_command_retab2_children (member->data, newtab);  // != curr_tab?
	//now its ok to update the list pointer
	curr_tab = newtab;
	//clean the old tab's data FIXME any leak ? text buffer ?
	if (((E2_OutputTabRuntime *)member->data)->origin_lastime!= NULL)
		g_free (((E2_OutputTabRuntime *)member->data)->origin_lastime);
//	GHashTable *hash = ((E2_OutputTabRuntime *)member->data)->origins;
//	if (hash != NULL)
//		g_hash_table_destroy (hash);
//	printd (DEBUG, "removing tab at %x", member->data);
	gchar *category = g_strconcat (_C(17),".",_C(28),NULL);  //_("general.output
	e2_keybinding_disrol (GTK_WIDGET (((E2_OutputTabRuntime*)member->data)->text), category);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_disrol (GTK_WIDGET (((E2_OutputTabRuntime*)member->data)->text), category);
# ifdef E2_PTRGESTURES
	e2_mousegesture_disrol (GTK_WIDGET (((E2_OutputTabRuntime*)member->data)->text), category);
# endif
#endif
	g_free (category);
#ifdef E2_OUTPUTSTYLES
	if (((E2_OutputTabRuntime*)member->data)->style_tags != NULL)
        g_hash_table_destroy (((E2_OutputTabRuntime*)member->data)->style_tags);
	if (((E2_OutputTabRuntime*)member->data)->style_trios != NULL)
        g_hash_table_destroy (((E2_OutputTabRuntime*)member->data)->style_trios);
#endif
	DEALLOCATE (E2_OutputTabRuntime, member->data);
	app.tabslist = g_list_delete_link (app.tabslist, member);	//counter adjusted above
	return TRUE;
}
/**
@brief focus output pane
@param from UNUSED the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the tab was found and made current
*/
static gboolean _e2_output_focus_action (gpointer from, E2_ActionRuntime *art)
{
	gtk_widget_grab_focus (GTK_WIDGET (curr_tab->text));
	//action data, if any, is a string with tab number
	if (art->data == NULL || *(gchar *)art->data == '\0')
		return TRUE;
	else
	{
		gchar *end;
		gulong num = strtoul ((gchar *)art->data, &end, 10);
		if (*end != '\0')
			return FALSE;
		if (num == 0 || num > g_list_length (app.tabslist))
			return FALSE;
#ifdef USE_GTK2_10
		//there may be gaps in the tab labels
		//and for gtk >= 2.10, tabs can be in any order or in any notebook,
		//so check all tabs to find the matching label
		GList *member;
		for (member = app.tabslist; member != NULL; member = member->next)
		{
			E2_OutputTabRuntime *tab = (E2_OutputTabRuntime *)member->data;
			if (tab->labelnum == num)
			{
				GtkWidget *book;
# ifdef E2_TABS_DETACH
				if (tab->detached)	//tab has been dropped to a separate window
				{
					book = gtk_widget_get_ancestor (tab->scroll, GTK_TYPE_NOTEBOOK);
					gtk_widget_grab_focus (tab->dropwindow);
				}
				else
# endif
					book = app.outbook;
				gint indx = gtk_notebook_page_num (GTK_NOTEBOOK (book), tab->scroll);
				gtk_notebook_set_current_page (GTK_NOTEBOOK (book), indx);
				if (tab != curr_tab)
				{
					//swap with mimimum race-risk ...
					*curr_tab = app.tab;	//backup current tab's data from stack to heap
					app.tab = *tab;	//get the replacement stuff into stackspace
					curr_tab = tab;
					//adjust all relevant child foreground-tab pointers to/from the stacked tab data
					e2_command_retab_children (&app.tab, curr_tab);
					printd (DEBUG, "output tab change, new current-tab ID is %d", curr_tab->labelnum);
				}
				break;
			}
		}
		return (member != NULL);
#else
		gtk_notebook_set_current_page (GTK_NOTEBOOK (app.outbook), num -1);
		E2_OutputTabRuntime *tab = g_list_nth_data (app.tabslist, num - 1);
		if (tab != curr_tab)
		{
			//swap with mimimum race-risk ...
			*curr_tab = app.tab;	//backup current tab's data from stack to heap
			app.tab = *tab;	//get the replacement stuff into stackspace
			curr_tab = tab;
			//adjust all relevant child foreground-tab pointers to/from the stacked tab data
			e2_command_retab_children (&app.tab, curr_tab);
			printd (DEBUG, "output tab change, new current-tab ID is %d", num - 1);
		}
		return TRUE;
#endif
	}
}
/**
@brief activate filetype item if any at the pointer in output pane
This is primarily for a pointer-button action
@param from the button, menu item etc which was activated
@param art UNUSED action runtime data

@return TRUE if there was a recognised filetype string to open
*/
static gboolean _e2_output_open_filetype (gpointer from, E2_ActionRuntime *art)
{
	gint x, y;
#ifdef USE_GTK3_0
	if (!e2_utils_get_pointer_position (GTK_WIDGET (curr_tab->text), &x, &y))
		return FALSE;
#else //gtk 2
	if (gdk_window_get_pointer (
# ifdef USE_GTK2_14
			gtk_widget_get_window (GTK_WIDGET (curr_tab->text)),
# else
			GTK_WIDGET (curr_tab->text)->window,
# endif
			&x, &y, NULL) == NULL)
		return FALSE;
#endif

	gchar *text = _e2_output_get_item_at_pointer (x, y, curr_tab, TRUE);
	if (text != NULL)
	{
		gboolean retval = _e2_output_open_text (text, TRUE);
		g_free (text);
		return retval;
	}

	return FALSE;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief get selected, or near-cursor, text
@param expand TRUE to try to match a file name/path around the cursor

@return an allocated string, or NULL
*/
gchar *e2_output_get_active_text (gboolean expand)
{
	GtkTextIter start, end;
	E2_OutputTabRuntime *rt = &app.tab;
	gtk_text_buffer_get_bounds (rt->buffer, &start, &end);
	if (gtk_text_iter_equal (&start, &end))
		return NULL;

	if (expand)
	{
		GdkRectangle rect;
		gint win_x, win_y;
#ifdef USE_GTK3_0
		gtk_text_view_get_cursor_locations (rt->text, NULL, &rect, NULL);
#else
		gtk_text_buffer_get_iter_at_mark (rt->buffer, &start,
			gtk_text_buffer_get_insert (rt->buffer));
		gtk_text_view_get_iter_location (rt->text, &start, &rect);
#endif
		gtk_text_view_buffer_to_window_coords (rt->text, GTK_TEXT_WINDOW_WIDGET,
			rect.x, rect.y, &win_x, &win_y);
		return _e2_output_get_item_at_pointer (win_x, win_y, rt, FALSE);
	}
	else if (gtk_text_buffer_get_selection_bounds (rt->buffer, &start, &end))
		return gtk_text_buffer_get_text (rt->buffer, &start, &end, FALSE);

	return NULL;
}

#ifdef E2_OUTPUTSTYLES
void e2_output_clear_styles (E2_OutputTabRuntime *rt, const gchar *origin)
{
	if (rt->style_trios != NULL)
		g_hash_table_remove (rt->style_trios, origin);
}
#endif
/**
@brief [re]register all key-binding callbacks for an output-tab textview
There are also generic keypress-event bindings with 'higher priority' than these
@param textview the widget being processed
@return
*/
void e2_output_register_keybindings (GtkWidget *textview)
{
	//other "key-press-event" connection(s) have non-NULL data
	//also, if needed, could use bindings cb func
	guint id = g_signal_lookup ("key-press-event", GTK_TYPE_TEXT_VIEW);
	g_signal_handlers_disconnect_matched (G_OBJECT (textview),
		G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_DATA , id, 0, NULL, NULL, NULL);
	gchar *category = g_strconcat(_C(17),".",_C(23),".",_C(28),NULL);  //_("general.main.output
	e2_keybinding_enrol (textview, category, (void(*)(E2_OptionSet*))NULL); //connection may be sync or async, see E2_IDLE_KEYSYNC
	g_free (category);
}
#ifdef E2_MOUSECUSTOM
/**
@brief [re]register all button-event callbacks for an output-tab textview
This should be applied before 'normal' button-event callbacks are connected
@param textview the widget being processed
@return
*/
void e2_output_register_pointerbindings (GtkWidget *textview)
{
	gchar *category = g_strconcat(_C(17),".",_C(23),".",_C(28),NULL);  //_("general.main.output
	//default bindings added centrally for core widgets, so NULL default-setter funcs here
	e2_mousebinding_enrol (textview, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (textview, category, (void(*)(E2_OptionSet*))NULL);
# endif
	g_free (category);
}
#endif
/**
@brief show end-of-output message in tab associated with @a tab

@param tab pointer to tab runtime
@param beep TRUE to sound when the message is printed

@return
*/
void e2_output_print_end (E2_OutputTabRuntime *tab, gboolean beep)
{
	e2_output_print (tab, _("-- end-of-output --"), NULL, TRUE, "small", "grey", NULL);
	if (beep)
		e2_utils_beep ();
}
/**
@brief show error message @a msg in current output pane tab, with beep

@param msg message
@param freemsg TRUE to free @a msg after it has been shown

@return
*/
//FIXME use the "correct" tab instead of always the current one
void e2_output_print_error (gchar *msg, gboolean freemsg)
{
	e2_output_print (&app.tab, msg, NULL, TRUE, E2_ERRORTAGS, NULL);
	if (freemsg)
		g_free (msg);
	e2_utils_beep ();
}
/**
@brief show system error message in current output pane tab, with beep

@return
*/
//FIXME use the "correct" tab instead of always the current one
void e2_output_print_strerrno (void)
{
	e2_output_print (&app.tab, (gchar *) g_strerror (errno), NULL, TRUE,
		E2_ERRORTAGS, NULL);
	e2_utils_beep ();
}
/**
@brief show @a msg in output pane

@a msg is converted to UTF-8 if it's not that encoding already
Provides special handling of any '\\b', '\\r' char(s) in @a msg
If E2_ESCAPE_COLOR is defined, provides special handling of terminal escape-codes
to colorise desired parts of displayed text
For an error message, style parameters are ignored
Expects BGL to be on/closed

@param tab pointer to data structure for the tab to get the message
@param msg actual message
@param origin context to which the message belongs (eg a pid string) or NULL for default
@param newline TRUE if @a msg is to be printed with pre- and post- newline
@param first_tag one or more tags to apply, terminated by NULL

@return
*/
void e2_output_print (E2_OutputTabRuntime *tab, gchar *msg, gchar *origin,
	gboolean newline, const gchar *first_tag, ...)
{
	//early exit
	if (msg == NULL || *msg == '\0') return;

	VOL gchar *utf, *s1, *s2;
#ifndef E2_OUTPUTSTYLES
	VOL gboolean is_default_origin;
#endif
	//ensure that there's an origin set
	if (origin == NULL)
	{
		origin = "default";	//no translate
#ifndef E2_OUTPUTSTYLES
		is_default_origin = TRUE;
#endif
	}
#ifndef E2_OUTPUTSTYLES
	else
		is_default_origin = (strcmp (origin, "default") == 0);
#endif
//	printd (DEBUG, "e2_ouput_print (msg:,origin:%s,newline:%d,first_tag:)",
//		origin, newline);

	//leading backspaces delete characters from the last message of this context, if any
	gint back_prior = 0;
	while (msg[0] == '\b')
	{
		back_prior++;
		msg++;
	}

	//show the output pane if the user wishes that
	if (!app.output.visible && e2_option_bool_get_direct (app.output.opt_show_on_new))
	{
		e2_window_output_show (NULL, NULL);
	}

	if (g_utf8_validate (msg, -1, NULL))
		utf = msg;
	else
	{
		//convert to utf before inserting
		GError *error = NULL;
		utf = g_locale_to_utf8 (msg, -1, NULL, NULL, &error);
		if (error != NULL)
		{
			printd (WARN, "locale string to UTF8 conversion failed: %s", error->message);
			g_error_free (error);
			utf = e2_utf8_from_locale_fallback (msg);
		}
	}
#ifdef E2_OUTPUTSTYLES
	//prepend any carryover from previous message
	E2_Trio *style_data;
	if (tab->style_trios != NULL)
	{
		style_data = g_hash_table_lookup (tab->style_trios, origin);
		if (style_data != NULL && style_data->c != NULL)
		{
			s1 = utf;
			utf = e2_utils_strcat ((gchar*)style_data->c, s1);
			if (s1 != msg)
				g_free (s1);
			g_free (style_data->c);
			style_data->c = NULL;
		}
	}
#endif

	VOL gboolean line_return = FALSE;
#ifdef E2_OUTPUTSTYLES
	gchar *strst = utf; //start of current text, somewhere in utf, to be inserted into text buffer
#endif
	gchar *strend = (gchar*)(utf + strlen ((gchar*)utf));;
#ifdef E2_OUTPUTSTYLES
	VOL GtkTextMark *msg_start_mark = NULL;
#endif
	VOL GtkTextIter start, end;

	//parse the displayed string to handle special characters

	for (s1 = utf; s1 < strend; s1++)
	{
		register gchar c = *s1;
//		printd (DEBUG, "%c - %d", c, (gint) c;
#ifdef E2_OUTPUTSTYLES
		if (c == 27)		//escape
		{
			if (s1 < strend - 1 && *(s1+1) == '[')
			{
				if (strchr (s1+1, 'm') != NULL)
				{	//possibly a control-sequence we can process
					guint indx;
					gint i, *ip;
					//we will apply styling from logged array, if any
					if (tab->style_trios != NULL)
						style_data = g_hash_table_lookup (tab->style_trios, origin);
					else
					{
						tab->style_trios = g_hash_table_new_full (
							g_str_hash, g_str_equal, NULL,  //assume constant string for key
							(GDestroyNotify)_e2_output_clear_styletrio);
						style_data = NULL;
					}
					if (style_data == NULL)
					{
						style_data = ALLOCATE0 (E2_Trio);
						g_hash_table_insert (tab->style_trios, origin, style_data);
					}
					//put relevant part of line (if any) into buffer
					_e2_output_insert_text (tab, origin, strst, s1 - 1, &start, &end,
						(strst == msg) && newline, (strst == msg)?back_prior:0);
				    if (strst == msg) //the inserted content (if any) is the start of msg
						//set anonymous mark at start of inserted msg, in case overall styling needed
						msg_start_mark = gtk_text_buffer_create_mark (tab->buffer,
							NULL, &start, TRUE);
					if (style_data->a == NULL)
						//set temporary anonymous mark at start of this inserted text block
						style_data->a = gtk_text_buffer_create_mark (tab->buffer,
							NULL, &start, TRUE);
					else
						gtk_text_buffer_get_iter_at_mark (tab->buffer, &start, (GtkTextMark*)style_data->a);
					if (style_data->b != NULL)
					{
						//add and remove tags
						for (indx = 0, ip = (gint32*)((GArray*)style_data->b)->data;
							 indx < ((GArray*)style_data->b)->len;
							 indx++, ip++)
						{
							i = *ip;
							GtkTextTag *tag = _e2_output_get_styletag (i, tab);
							if (i < 0)
								gtk_text_buffer_remove_tag (tab->buffer, tag, &start, &end);
							else
								gtk_text_buffer_apply_tag (tab->buffer, tag, &start, &end);
						}
						_e2_output_clean_removed_codes ((GArray**)&(style_data->b));
					}
					//move mark to end of inserted text
					gtk_text_buffer_move_mark (tab->buffer, (GtkTextMark*)style_data->a, &end);
					//overall-line styling from first_tag etc is applied after the whole string has been inserted
					//parse this escape-sequence and put reconciled numbers into style_data->b
					s2 = _e2_output_parse_escape (s1, (GArray**)&style_data->b);
					if (s2 > s1)
					{
						//resume parsing after the processed seqence
						strst = s2;
						s1 = s2 - 1; //allow for the loop increment
					}
					continue;
				}
				else //escape detected, but not sure what sequence is, or whether complete
				{
					//handle partial sequence from end of upstream buffer
					if (strcmp (s1 + 1, utf + 1) == 0)
					{	//sequence is end of msg
						_e2_output_carryover (tab, origin, s1);
						return;
					}
				}
/* if support clearing, only the current context
				else if (s1 < strend - 3 && *(s1+2) == '2' && *(s1+3) == 'J') //clear [2J
				{
					_e2_output_clear_buffer (tab);
					return;
				}
*/
			}
			else //escape detected, but not sure what sequence is, or whether complete
			{
				//handle partial sequence from end of upstream buffer
				if (strcmp (s1 + 1, utf + 1) == 0)
				{	//sequence is end of msg
					_e2_output_carryover (tab, origin, s1);
					return;
				}
			}
		}
		else
#endif
		if (c == '\r' && *(s1 + sizeof(gchar)) != '\n')		//carriage return
		{
			if (s1 + sizeof(gchar) == strend)
			{	//return is at end of msg
				*s1 = '\0';	//doesn't matter if it's also the start of msg
				line_return = TRUE;	//wait until next msg before doing anything
			}
			else
			{
				s2 = s1 - sizeof (gchar);
				while (s2 >= utf)
				{
					if (*s2 == '\n')
					{
						gint slide = (strend - s1);	//include trailing 0
						memmove ((gchar *)(s2 + 1), (gchar *)(s1 + 1), slide);
						strend -= (s1-s2);
						s1 = s2;	//allow for loop-end increment
						break;
					}
					s2--;
				}
				if (s2 < utf)
				{
					//FIXME properly handle sole-\r in msg
					*s1 = '\n';
					line_return = TRUE;
				}
			}
		}
		else if (c == '\b')	//backspace
		{
			do
			{
				//find a character back in the string that's not already a backspace
				s2 = s1 - sizeof (gchar);
				while (s2 >= utf)
				{
					c = *s2;
					if (c >= 0x80)	//part of UTF-8 multi-byte char
					{
						//overwrite it with backspaces
						do
						{
							*s2 = '\b';
						} while (--s2 >= utf && *s2 > 0x80);
						break;
					}
					else if (c != '\b')
					{
						//overwrite it with backspace
						*s2 = '\b';
						break;
					}
					else
					{
						//scan more
						s2--;
					}
				}
				//if there is no prior non-backspace character,
				//setup to delete one from the previous message in this context
				if (s2 < utf)
					back_prior++;

				s1++;
			} while (s1 < strend && *s1 == '\b');	//adjacent backspace in msg

			if (s2 < utf)
				s2 = utf;
			//s1 now points after last \b char, s2 points to first one
			//get rid of them
			gint slide = (strend + sizeof (gchar) - s1);	//include trailing 0
			memmove ((gchar *)s2, (gchar *)s1, slide);
			strend -= (s1-s2);
			s1 = s2 - sizeof (gchar);	//allow for loop-end increment
		}
	}
#ifdef E2_OUTPUTSTYLES
	_e2_output_insert_text (tab, origin, strst, s1 - 1, &start, &end,
		(strst == msg) && newline, (strst == msg)?back_prior:0);
	VOL GtkTextBuffer *buffer = tab->buffer;
	if (msg_start_mark != NULL) //some of msg inserted before this batch
	{
		//get the overall start of inserted msg
		gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, (GtkTextIter*)&start,
			(GtkTextMark*)msg_start_mark);
		gtk_text_buffer_delete_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)msg_start_mark);
	}
//	static gchar dbg[3] = {'S','0',0};
//	static gchar dbg2[3] = {'E','0',0};
//	GtkTextMark *debug_mark;
//	debug_mark = gtk_text_buffer_create_mark ((GtkTextBuffer*)buffer, NULL, &start, FALSE);
//	dbg[1]++;dbg2[1]++;
//	gtk_text_buffer_insert ((GtkTextBuffer*)buffer, &start, dbg, 2);
	VOL GtkTextMark *origin_mark = gtk_text_buffer_get_mark ((GtkTextBuffer*)buffer, origin);
//	gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, &end, origin_mark);
//	gtk_text_buffer_insert ((GtkTextBuffer*)buffer, &end, dbg2, 2);
//	gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, &start, debug_mark);
//	gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, &end, origin_mark);

	VOL gchar *mark_del_name = g_strconcat ((origin == NULL)?"default":origin, "-del", NULL);	//no translate
#else
	//flag whether this origin is new
	VOL gboolean is_new;
	//get the text buffer for the output pane
	VOL GtkTextBuffer *buffer = tab->buffer;

	VOL gchar *mark_del_name = g_strconcat (origin, "-del", NULL);	//no translate
	VOL GtkTextMark *mark_del = gtk_text_buffer_get_mark ((GtkTextBuffer*)buffer, (gchar*)mark_del_name);
	//try to get insert mark
	//it exists if there has been output in this origin before
	VOL GtkTextMark *origin_mark = gtk_text_buffer_get_mark ((GtkTextBuffer*)buffer, origin);
	//if not, create it at the end
	if (origin_mark == NULL)
	{
		is_new = TRUE;
		gtk_text_buffer_get_end_iter ((GtkTextBuffer*)buffer, (GtkTextIter*)&end);
		origin_mark = gtk_text_buffer_create_mark ((GtkTextBuffer*)buffer, origin, (GtkTextIter*)&end, TRUE);
	}
	else
	{
		is_new = FALSE;
		//the default context always outputs to the end of the textview
		//(ie the output is not "glued together"
		if (is_default_origin)
		{
			gtk_text_buffer_get_end_iter ((GtkTextBuffer*)buffer, (GtkTextIter*)&end);
			gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)origin_mark, (GtkTextIter*)&end);
		}
		else
			gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, (GtkTextIter*)&end, (GtkTextMark*)origin_mark);

		//do we have to overwrite a previous message's characters in front of us?
		//(if a previous message had a carriage return '\r' in it)
		if (mark_del != NULL)
		{
			VOL GtkTextIter iter_del;
			gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, (GtkTextIter*)&iter_del, (GtkTextMark*)mark_del);
			gtk_text_buffer_delete ((GtkTextBuffer*)buffer, (GtkTextIter*)&end, (GtkTextIter*)&iter_del);
			//delete the mark, no further use this time
			gtk_text_buffer_delete_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)mark_del);
		}
	}
	//move the scroll-helper mark
	gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, tab->mark, (GtkTextIter*)&end);
	gtk_text_buffer_place_cursor ((GtkTextBuffer*)buffer, (GtkTextIter*)&end);

	if (back_prior > 0)
	{	//want to backspace into previuos message for this context
		VOL gint _offset = gtk_text_iter_get_offset ((GtkTextIter*)&end);
		GtkTextIter backi;
		//FIXME limit backshift to start of current context
		//CHECKME ignore \n for the purposes of going backwards
		gtk_text_buffer_get_iter_at_offset ((GtkTextBuffer*)buffer, &backi, _offset - back_prior);
		gtk_text_buffer_delete ((GtkTextBuffer*)buffer, &backi, (GtkTextIter*)&end);
//		gtk_text_buffer_get_iter_at_offset ((GtkTextBuffer*)buffer, (GtkTextIter*)&end, _offset - back_prior);
		end = backi;
	}

	VOL gint lfcount = 0;

	//if necessary, insert a newline character before the message because
	//it has a different context from the last one and the last one hasn't
	//printed one yet;
/*#ifdef USE_GLIB2_32
	g_rec_mutex_lock (&print_mutex);
#else
	g_static_rec_mutex_lock (&print_mutex);
#endif
*/
	if (tab->origin_lastime != NULL
		&& strcmp (tab->origin_lastime, origin)
		&& !gtk_text_iter_starts_line ((GtkTextIter*)&end)
		&& (is_new || is_default_origin))
			lfcount = 1;
/*#ifdef USE_GLIB2_32
	g_rec_mutex_unlock (&print_mutex);
#else
	g_static_rec_mutex_unlock (&print_mutex);
#endif
*/
	if (newline && (!gtk_text_iter_starts_line ((GtkTextIter*)&end)))
	{
		printd (DEBUG, "inserted additional newline because the message requested it");
		lfcount++;
	}

	if (lfcount > 0)
		gtk_text_buffer_insert ((GtkTextBuffer*)buffer, (GtkTextIter*)&end, "\n", lfcount);

	//AT LAST, WE PUT IT IN !
	gtk_text_buffer_insert ((GtkTextBuffer*)buffer, (GtkTextIter*)&end, (gchar*)utf, -1);
	//get start and end of text just inserted
	gtk_text_buffer_get_iter_at_mark ((GtkTextBuffer*)buffer, (GtkTextIter*)&start,
		(GtkTextMark*)origin_mark);
#endif //ndef E2_OUTPUTSTYLES

	if (first_tag != NULL)
	{
		//apply style-tags for added text
#ifdef USE_GTK2_20
		GtkTextTagTable *ttbl = gtk_text_buffer_get_tag_table ((GtkTextBuffer*)buffer);
#endif
		va_list args;
		va_start (args, first_tag);
		VOL const gchar *tag_name = first_tag;
		while (tag_name != NULL)
		{
			GtkTextTag *tag;
#ifdef USE_GTK2_20
			tag = gtk_text_tag_table_lookup (ttbl, (gchar*)tag_name);
#else
			tag = gtk_text_tag_table_lookup (((GtkTextBuffer*)buffer)->tag_table, (gchar*)tag_name);
#endif
/* tags are all pre-configured
			if (tag == NULL)
				printd (WARN, "%s: no tag with name '%s'!", G_STRLOC, tag_name);
			else */
//			gtk_text_tag_set_priority (tag, 0);
			gtk_text_buffer_apply_tag ((GtkTextBuffer*)buffer, tag, (GtkTextIter*)&start, (GtkTextIter*)&end);

			tag_name = va_arg (args, const gchar*);
		}
		va_end (args);
	}
#ifndef E2_OUTPUTSTYLES
/*	when there is a context after the current one, the font weight of
	the later one (eg bold) is sometimes (eg current has error then normal)
	used instead of the proper font (normal)
	EVEN IF the inserted text is explicitly set to normal style
	probably a gtk bug as the font color is correct
	the following is a workaround */
	else //if (!gtk_text_iter_is_end (&end))
	{
		//gtk_text_buffer_remove_all_tags (buffer, &start, &end);
		VOL GSList *tags = gtk_text_iter_get_tags ((GtkTextIter*)&start);
		if (tags != NULL)
		{
			gtk_text_buffer_remove_all_tags ((GtkTextBuffer*)buffer, (GtkTextIter*)&start, (GtkTextIter*)&end);
			g_slist_free ((GSList*)tags);
		}
	}
#endif //ndef E2_OUTPUTSTYLES
	//if there was a carriage return, save marks for processing during the next message
	if (line_return)
	{
		//find the start of current line
		VOL GtkTextIter backsearch;
		if (!gtk_text_iter_backward_search ((GtkTextIter*)&end, "\n",
			GTK_TEXT_SEARCH_TEXT_ONLY, NULL, (GtkTextIter*)&backsearch, NULL))
				gtk_text_buffer_get_start_iter ((GtkTextBuffer*)buffer, (GtkTextIter*)&backsearch);
		gtk_text_buffer_place_cursor ((GtkTextBuffer*)buffer, (GtkTextIter*)&backsearch);
		//move the origin-mark there
		gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)origin_mark, (GtkTextIter*)&backsearch);
		gtk_text_buffer_create_mark ((GtkTextBuffer*)buffer, (gchar*)mark_del_name, (GtkTextIter*)&end, TRUE);
	}
	else
		if (newline)	//CHECKME handle newline and line_return together ?
	{	//trailing \n
		gtk_text_buffer_insert ((GtkTextBuffer*)buffer, (GtkTextIter*)&end, "\n", 1);
	}
	//if there was no carriage return, set mark for next message in this context
	if (!line_return)
		gtk_text_buffer_move_mark ((GtkTextBuffer*)buffer, (GtkTextMark*)origin_mark, (GtkTextIter*)&end);

	//scroll down if necessary
	if (//scrolling to new output is in effect
		e2_option_bool_get_direct (app.output.opt_jump)
		&& (//manual scrolling has not moved the insert mark off-screen
			g_atomic_int_get (&tab->onscreen) == 1
			//OR scroll regardless of whether manually scrolled away
			|| !e2_option_bool_get_direct (app.output.opt_jump_follow))
		&& (
			//added text is at the end of the buffer
			gtk_text_iter_is_end ((GtkTextIter*)&end)
			//OR don't care whether added text is at the end of the buffer
			|| !e2_option_bool_get_direct (app.output.opt_jump_end)))
	{
		_e2_output_iter_check_visible (tab, (GtkTextIter*)&end);
		if (g_atomic_int_compare_and_exchange (&tab->onscreen, 0, 1))
			//the end of the added text is not visible now
			gtk_text_view_scroll_to_mark (tab->text, (GtkTextMark*)origin_mark, 0.0, TRUE, 0.1, 1.0);
	}

	if (utf != msg)
		g_free ((gchar *)utf);

	g_free ((gchar *)mark_del_name);

	//protect threaded clearing of origin_buffer
#ifdef USE_GLIB2_32
	g_rec_mutex_lock (&print_mutex);
#else
	g_static_rec_mutex_lock (&print_mutex);
#endif
	if (tab->origin_lastime == NULL
		|| strcmp (tab->origin_lastime, origin))
	{
		//save the origin of the last message
		if (tab->origin_lastime != NULL)
			g_free (tab->origin_lastime);
		tab->origin_lastime = g_strdup (origin);
	}
#ifdef USE_GLIB2_32
	g_rec_mutex_unlock (&print_mutex);
#else
	g_static_rec_mutex_unlock (&print_mutex);
#endif
}
#undef VOL
/**
@brief update several output pane textview settings to conform to current config parameters
Used only during window re-creation
@return
*/
void e2_output_update_style (void)
{
	const gchar *fntname = e2_utils_get_output_font ();
	PangoFontDescription *font_desc = pango_font_description_from_string
			(fntname);
	app.output.font_size = pango_font_description_get_size (font_desc);	//(pixels or points) * PANGO_SCALE

	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
	{
		GtkTextView *tvw = ((E2_OutputTabRuntime *)member->data)->text;
		gtk_text_view_set_wrap_mode (tvw,
			e2_option_int_get ("output-wrap-mode"));
		gtk_text_view_set_left_margin (tvw,
			e2_option_int_get ("output-left-margin"));
		gtk_text_view_set_right_margin (tvw,
			e2_option_int_get ("output-right-margin"));
#ifdef USE_GTK3_0
		gtk_widget_override_font (GTK_WIDGET (tvw), font_desc);
#else
		gtk_widget_modify_font (GTK_WIDGET (tvw), font_desc);
#endif
		GtkTextBuffer *buf = ((E2_OutputTabRuntime *)member->data)->buffer;
		GtkTextTagTable *table = gtk_text_buffer_get_tag_table (buf);

		GtkTextTag *tag = gtk_text_tag_table_lookup (table, "cmand");
		g_object_set (G_OBJECT (tag), "foreground",
			e2_option_str_get ("color-command"), NULL);
		tag = gtk_text_tag_table_lookup (table, "posit");
		g_object_set (G_OBJECT (tag), "foreground",
			e2_option_str_get ("color-positive"), NULL);
		tag = gtk_text_tag_table_lookup (table, "negat");
		g_object_set (G_OBJECT (tag), "foreground",
			e2_option_str_get ("color-negative"), NULL);
		tag = gtk_text_tag_table_lookup (table, "unimp");
		g_object_set (G_OBJECT (tag), "foreground",
			e2_option_str_get ("color-unimportant"), NULL);
		tag = gtk_text_tag_table_lookup (table, "small");
		g_object_set (G_OBJECT (tag), "size",
			(gint) (PANGO_SCALE_SMALL * app.output.font_size), NULL);
	}
	pango_font_description_free (font_desc);
}
/**
@brief create output pane

Create initial output pane - with initial no. of panes as per cache

@return notebook, one or more pages, each containing an output pane
*/
GtkWidget *e2_output_initialise (void)
{
	//create notebook for tabs, no callback until all pages created
	app.outbook = gtk_notebook_new ();

	GtkNotebook *book = GTK_NOTEBOOK (app.outbook);
	gtk_notebook_set_show_tabs (book, (app.tabcount > 1));
	gtk_notebook_popup_enable (book);

	GtkCornerType where = (GtkCornerType) e2_option_sel_get ("scrollbar-position");
#ifdef ADJACENT_TABS
	if (where == GTK_CORNER_TOP_LEFT || where == GTK_CORNER_BOTTOM_LEFT)
		gtk_notebook_set_tab_pos (book, GTK_POS_RIGHT);
	else
		gtk_notebook_set_tab_pos (book, GTK_POS_LEFT);
#else
	if (where == GTK_CORNER_TOP_LEFT || where == GTK_CORNER_BOTTOM_LEFT)
		gtk_notebook_set_tab_pos (book, GTK_POS_LEFT);
	else
		gtk_notebook_set_tab_pos (book, GTK_POS_RIGHT);
#endif
	gtk_notebook_set_scrollable (book, TRUE);
	gtk_notebook_set_show_border (book, FALSE);
	const gchar *fntname = e2_utils_get_output_font ();
	PangoFontDescription *font_desc = pango_font_description_from_string
			(fntname);
	app.output.font_size = pango_font_description_get_size (font_desc);//(pixels or points) * PANGO_SCALE
	pango_font_description_free (font_desc);
	if (app.output.font_size == 0)	//in case of invalid font name string
	{
		//set 10-point default
		app.output.font_size =
			(pango_font_description_get_size_is_absolute (font_desc)) ?
			10 * PANGO_SCALE * 96 / 72 :	//assume screen is 96 DPI
			10 * PANGO_SCALE;
	}

	printd (DEBUG, "stacked tab is at %x", &app.tab);
	//we're always going to have at least 1 tab, which gets the initial focus
	gint i;
	E2_OutputTabRuntime *tab = NULL;	//assignment for complier-warning prevention only
	//iterate backward so that we end with the first (default) tab
	for (i = app.tabcount ; i > 0 ; i--)
	{
		tab = ALLOCATE0 (E2_OutputTabRuntime);	//FIXME only deallocated by the user, not at session end
		CHECKALLOCATEDFATAL (tab);
//		printd (DEBUG, "created tab data at %x", tab);
		app.tabslist = g_list_prepend (app.tabslist, tab);
		GtkWidget *sw = _e2_output_create_view (tab);
		gchar *labltxt = g_strdup_printf ("%d", i);	//tab labels are numbers
		GtkWidget *label = gtk_label_new (labltxt);
		gtk_notebook_prepend_page (book, sw, label);
		g_free (labltxt);
#ifdef USE_GTK2_10
		tab->labelnum = i;	//save tab id for matching
		gtk_notebook_set_tab_reorderable (book, sw, TRUE);
# ifdef E2_TABS_DETACH
		gtk_notebook_set_tab_detachable (book, sw, TRUE);
# endif
#endif
	}
	//last-added one becomes current
	//its current-page is set when the window is shown
	app.tab = *tab;
	curr_tab = tab;

#ifdef E2_TABS_DETACH
	//enable tab dragging to new windows
# ifdef USE_GTK2_12DND
#  ifdef USE_GTK3_0
	gchar bookname[16];
	snprintf (bookname, 16, "%x", GPOINTER_TO_UINT(app.outbook));	//create instance-specific name
	gtk_notebook_set_group_name (book, bookname);
#  else
	gtk_notebook_set_group (book, app.outbook);	//instance-specific pointer
#  endif
	g_signal_connect (G_OBJECT (app.outbook), "create-window",
		G_CALLBACK (_e2_output_tab_drop_new), NULL);	//CHECKME user_data
# else
	//this is the gtk 2.10 approach
	gtk_notebook_set_group_id (book, getpid());
	gtk_notebook_set_window_creation_hook
		((GtkNotebookWindowCreationFunc) _e2_output_tab_drop_new,
		NULL, //CHECKME gpointer data
		NULL);	//(GDestroyNotify) destroy
# endif
	//enable processing of tabs being dragged back from a new window
	gtk_drag_dest_set (app.outbook, GTK_DEST_DEFAULT_DROP, target_table2, n_targets2,
		GDK_ACTION_MOVE);
	g_signal_connect (G_OBJECT (app.outbook), "drag-data-received",
		G_CALLBACK (_e2_output_tabdrag_data_received_cb), NULL);	//CHECKME user_data
	//capture clicks on tab labels
	g_signal_connect (G_OBJECT (app.outbook), "grab-focus",
		G_CALLBACK (_e2_output_grab_focus_cb), NULL);
//	g_signal_connect (G_OBJECT (app.outbook), "focus-in-event",
//		G_CALLBACK (_e2_output_focus_in_cb), NULL);
	//for unblocking
	g_signal_connect (G_OBJECT (app.outbook), "page-removed",
		G_CALLBACK (_e2_output_tabgone_cb), app.main_window);
#endif
	g_signal_connect (G_OBJECT (app.outbook), "switch-page",
		G_CALLBACK (_e2_output_tabchange_cb), NULL);	//no data

	//setup data common to all tabs
	//repetitive, but option data structs may move after options hash recreation,
	//and possibly without re-running the options init function
	app.output.opt_show_on_new = e2_option_get ("show-output-window-on-output");
//	app.output.opt_show_on_focus_in = e2_option_get ("command-line-show-output-on-focus-in"); UNUSED DIRECTLY
	app.output.opt_hide_on_focus_out = e2_option_get ("command-line-hide-output-on-focus-out");
	app.output.opt_jump = e2_option_get ("output-jump-new");
	app.output.opt_jump_follow = e2_option_get ("output-jump-new-following");
	app.output.opt_jump_end = e2_option_get ("output-jump-new-end");

	//setup mutex to protect threaded access to print-function static variables
#ifdef USE_GLIB2_32
	g_rec_mutex_init (&print_mutex); //maybe not needed for static data
#else
	g_static_rec_mutex_init (&print_mutex);
#endif
	return app.outbook;
}
/**
@brief register actions related to output pane

@return
*/
void e2_output_actions_register (void)
{
	E2_Action actions[] =
	{
	{g_strconcat(_A(10),".",_A(30),NULL),_e2_output_open_filetype,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL},
	{g_strconcat(_A(10),".",_A(36),NULL),_e2_output_clear_action, FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL},
	{g_strconcat(_A(10),".",_A(74),NULL),_e2_output_print_action, TRUE, E2_ACTION_TYPE_ITEM,0,NULL,NULL},
	{g_strconcat(_A(10),".",_A(57),NULL),_e2_output_help_action,  TRUE, E2_ACTION_TYPE_ITEM,0,NULL,NULL},
	{g_strconcat(_A(10),".",_A(80),NULL),_e2_output_scroll_step,  TRUE, E2_ACTION_TYPE_ITEM,0,GINT_TO_POINTER(TRUE),NULL},
	{g_strconcat(_A(10),".",_A(81),NULL),_e2_output_scroll_step,  TRUE, E2_ACTION_TYPE_ITEM,0,GINT_TO_POINTER(FALSE),NULL},
	{g_strconcat(_A(10),".",_A(71),NULL),_e2_output_scroll_page,  TRUE, E2_ACTION_TYPE_ITEM,0,GINT_TO_POINTER(TRUE),NULL},
	{g_strconcat(_A(10),".",_A(72),NULL),_e2_output_scroll_page,  TRUE, E2_ACTION_TYPE_ITEM,0,GINT_TO_POINTER(FALSE),NULL},
	{g_strconcat(_A(10),".",_A(55),NULL),_e2_output_scroll_all,   TRUE, E2_ACTION_TYPE_ITEM,0,GINT_TO_POINTER(TRUE),NULL},
	{g_strconcat(_A(10),".",_A(56),NULL),_e2_output_scroll_all,   TRUE, E2_ACTION_TYPE_ITEM,0,GINT_TO_POINTER(FALSE),NULL},
	{g_strconcat(_A(10),".",_A(31),NULL),_e2_output_tab_add,      FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL},
	{g_strconcat(_A(10),".",_A(45),NULL),_e2_output_tab_remove,   FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL},	//FIXME data
	{g_strconcat(_A(10),".",_A(50),NULL),_e2_output_focus_action, TRUE, E2_ACTION_TYPE_ITEM,0,NULL,NULL},	//optional arg is string tab number
	};
	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
}
/**
@brief register options related to output pane

@return
*/
void e2_output_options_register (void)
{
	gchar *group_name = g_strconcat(_C(6),".",_C(28),":",_C(26),NULL); //_("commands.output:miscellaneous"
	e2_option_bool_register ("show-output-window-on-output", group_name,
		_("show output pane when a new message appears"),
		_("This will ensure you don't miss any messages"), NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("command-line-show-output-on-focus-in", group_name,
		_("show output pane if the command line is focused"),
		_("This causes the output pane to be opened when you are about enter a command"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED); //? | E2_OPTION_FLAG_BUILDBARS);
	e2_option_bool_register ("command-line-hide-output-on-focus-out", group_name,
		_("hide output pane if the command line is unfocused"),
		_("This causes the output pane to be closed when you move focus away from the command line"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED); //? | E2_OPTION_FLAG_BUILDBARS);
	e2_option_bool_register ("fileop-show", group_name, _("show commands"),
 		_("This echoes the commands that are run and their exit value"), NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("output-jump-new", group_name, _("scroll to new output"),
		_("This will automatically scroll the output pane content, to show new message(s)"), NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("output-jump-new-following", group_name, _("only scroll when really following"),
		_("This stops automatic pane scrolling to new output if you've manually scrolled away"),
		"output-jump-new", TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("output-jump-new-end", group_name, _("only scroll when new output is at the end"),
		_("This stops pane scrolling if a different process has displayed text after the current insert position"),
  		"output-jump-new", FALSE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_str_register ("output-select-separators", group_name, _("auto-selection boundary characters"),
		_("Character(s), usually punctuation, any of which will set a boundary for auto-selected text"),
		NULL, "", E2_OPTION_FLAG_ADVANCED);
	const gchar *opt_wrap_mode[] = {_("none"), _("everywhere"), _("words"), NULL};
	group_name = g_strconcat(_C(6),".",_C(28),":",_C(39),NULL);	//_("commands.output:style"
	e2_option_sel_register ("output-wrap-mode", group_name, _("line wrap mode"),
		_("If mode is 'none', a horizontal scrollbar will be available. Mode 'words' will only break the line between words"),
		NULL, 2, opt_wrap_mode,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDALL);
	e2_option_int_register ("output-left-margin", group_name, _("left margin (pixels)"),
		_("This is the left margin between the output-pane edge and the text in it"),
		NULL, 6, 0, 1000,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_int_register ("output-right-margin", group_name, _("right margin (pixels)"),
		_("This is the right margin between the output-pane edge and the text in it"),
		 NULL, 2, 0, 1000,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("custom-output-font",
		group_name, _("use custom font"),
		_("If activated, the font specified below will be used, instead of the theme default"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDOUT);
	e2_option_font_register ("output-font", group_name, _("custom output font"),
		_("This is the font used for text in the output pane"), "custom-output-font", "Sans 10", 	//_I( font name
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDOUT);
//	group_name = g_strconcat(_C(33),".",_C(2),":",_C(28),NULL); //_("panes.other colors:output"
	group_name = g_strconcat(_C(6),".",_C(28),":",_C(2),NULL);	//_("commands.output:colors"
	//CHECKME which of these output colors really do need the whole window to be reconstructed ?
	e2_option_color_register ("color-positive", group_name, _("positive color"),
		_("This color is used for messages about successful operations or other 'positive events'"),
		NULL, "dark green",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDOUT);
	e2_option_color_register ("color-negative", group_name, _("negative color"),
		_("This color is used for errors, warnings and other messages that should grab attention"),
		NULL, "red",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDOUT);
	e2_option_color_register ("color-command", group_name, _("commands color"),
		_("This color is used for echoing issued commands"),
		NULL, "blue",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDOUT);
	e2_option_color_register ("color-unimportant", group_name, _("unimportant color"),
		_("This color is used for messages of minor importance or other miscellaneous events"),
		NULL, "light grey",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDOUT);
}
