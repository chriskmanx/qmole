/*
 *  config_file.h - header file for config_file.c, manages config file access.
 *	part of galculator
 *  	(c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
#include <gtk/gtk.h>
#include <glib.h>

#include "galculator.h"

#ifndef _CONFIG_FILE_H
#define _CONFIG_FILE_H 1

#define MAX_FILE_LINE_LENGTH	1024

#define SECTION_GENERAL "[general]"
#define SECTION_CONSTANTS "[constants]"
#define SECTION_USER_FUNCTIONS "[user functions]"

#define DEFAULT_BKG_COLOR			"#f2ffc2"
#define DEFAULT_RESULT_FONT			"Sans Bold 26"
#define	DEFAULT_RESULT_COLOR 		"black"
#define DEFAULT_STACK_FONT			"Sans Bold 11"
#define DEFAULT_STACK_COLOR			"black"
#define DEFAULT_MOD_FONT			"Sans Bold 8"
#define DEFAULT_ACT_MOD_COLOR		"black"
#define DEFAULT_INACT_MOD_COLOR		"grey"
#define	DEFAULT_VIS_NUMBER			TRUE
#define	DEFAULT_VIS_ANGLE			TRUE
#define	DEFAULT_VIS_NOTATION		TRUE
#define DEFAULT_VIS_ARITH			TRUE
#define DEFAULT_VIS_BRACKET			TRUE
#define DEFAULT_CUSTOM_BUTTON_FONT	FALSE
#define DEFAULT_BUTTON_FONT			"Sans 10"
#define DEFAULT_BUTTON_WIDTH 		40
#define DEFAULT_BUTTON_HEIGHT 		25
#define DEFAULT_VIS_FUNCS			TRUE
#define DEFAULT_VIS_DISPCTRL		TRUE
#define DEFAULT_VIS_LOGIC			TRUE
#define DEFAULT_VIS_STANDARD		TRUE
#define DEFAULT_MODE				BASIC_MODE
#define DEFAULT_DEC_SEP				FALSE
#define DEFAULT_DEC_SEP_LENGTH		3
#define DEFAULT_DEC_SEP_CHAR		" "
#define DEFAULT_HEX_BITS			32
#define DEFAULT_HEX_SIGNED			TRUE
#define DEFAULT_HEX_SEP				FALSE
#define DEFAULT_HEX_SEP_LENGTH		4
#define DEFAULT_HEX_SEP_CHAR		" "
#define DEFAULT_OCT_BITS			32

#define DEFAULT_OCT_SIGNED			TRUE
#define DEFAULT_OCT_SEP				FALSE
#define DEFAULT_OCT_SEP_LENGTH		3
#define DEFAULT_OCT_SEP_CHAR		" "
#define DEFAULT_BIN_BITS			16
#define DEFAULT_BIN_SIGNED			TRUE
#define DEFAULT_BIN_FIXED			FALSE
#define DEFAULT_BIN_LENGTH			8
#define DEFAULT_BIN_SEP				FALSE
#define DEFAULT_BIN_SEP_LENGTH		4
#define DEFAULT_BIN_SEP_CHAR		" "
#define DEFAULT_NUMBER				CS_DEC
#define DEFAULT_ANGLE				CS_RAD
#define DEFAULT_NOTATION			CS_PAN
#define DEFAULT_STACK_SIZE			3
#define DEFAULT_REM_DISPLAY			FALSE
#define	DEFAULT_REM_VALUEX			"0"	/* must not end with a newline! */
#define	DEFAULT_REM_VALUEY			"0"
#define	DEFAULT_REM_VALUEZ			"0"
#define	DEFAULT_REM_VALUET			"0"
#define DEFAULT_SHOW_MENU			TRUE

typedef struct {
	/* 1st pref page */
	char 		*bkg_color;		/* gdk_color_parse */
	char		*result_font; 		/* pango_font_description_from_string */
	char 		*result_color;
	char		*stack_font;
	char		*stack_color;
	char		*mod_font;
	char 		*act_mod_color;
	char 		*inact_mod_color;
	gboolean	vis_number;
	gboolean	vis_angle;
	gboolean	vis_notation;
	gboolean	vis_arith;
	gboolean	vis_bracket;
	/* 2nd pref page */
	gboolean	custom_button_font;
	char 		*button_font;		/* buttons */
	int		button_width;
	int		button_height;
	gboolean	vis_funcs;
	gboolean	vis_logic;
	gboolean	vis_dispctrl;
	gboolean	vis_standard;
	int		mode;
	/* 3rd pref page */
	/* constants. handled different */
	/* 4th pref page */
	gboolean	dec_sep;
	int		dec_sep_length;
	char 		*dec_sep_char;
	int		hex_bits;
	gboolean	hex_signed;
	gboolean	hex_sep;
	int		hex_sep_length;
	char 		*hex_sep_char;
	int		oct_bits;
	gboolean	oct_signed;
	gboolean	oct_sep;
	int		oct_sep_length;
	char 		*oct_sep_char;
	int		bin_bits;
	gboolean	bin_signed;
	gboolean	bin_fixed;
	gboolean	bin_length;
	gboolean	bin_sep;
	int		bin_sep_length;
	char 		*bin_sep_char;
	/* 5th pref page */
	int		def_number;		/* in accordance with enums in */
	int		def_angle;		/* galculator.h */
	int		def_notation;
	int		stack_size;
	gboolean	rem_display;
	char		*rem_valuex;		/* done as string */
	char		*rem_valuey;
	char		*rem_valuez;
	char		*rem_valuet;
	gboolean	show_menu;
} s_preferences;

/* default value ? */
/* update_handler */

typedef struct {
	char 	*key;
	void	*variable;
	int	key_type;
	char 	*widget_name;
	void	(*set_handler)(GtkBuilder *, char *, void *);
} s_prefs_entry;

enum {
	STRING,
	BOOLEAN,
	INTEGER,
	NOT_FOUND
};

enum {
	GENERAL,
	CONSTANTS,
	USER_FUNCTIONS
};

s_preferences config_file_read (char *filename);
void config_file_write (char *filename, s_preferences this_prefs, s_constant *this_constants, s_user_function *this_user_functions);
s_prefs_entry *config_file_get_prefs_list();
s_constant *config_file_get_constants();
s_user_function  *config_file_get_user_functions();
#endif /* config_file.h */
