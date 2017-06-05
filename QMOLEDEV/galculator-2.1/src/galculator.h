/*
 *  galculator.h - general definitions.
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

#ifndef _GALCULATOR_H
#define _GALCULATOR_H 1

#include <glib.h>

#include "g_real.h"

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef WITH_HILDON
#include "hildon/hildon-program.h"
#include "hildon/hildon-window.h"
#include "glade/glade-build.h"
#endif

#define DEFAULT_DEC_POINT '.'

#define CLEARED_DISPLAY	"0"

/* old, non-XDG method */
#define CONFIG_FILE_NAME_OLD ".galculator"
/* XDG spec */
#define CONFIG_FILE_NAME "galculator.conf"

#ifdef WITH_HILDON
#define MAIN_GLADE_FILE 			PACKAGE_UI_DIR "/main_frame_hildon.ui"
#else
#define MAIN_GLADE_FILE 			PACKAGE_UI_DIR "/main_frame.ui"
#endif
#define SCIENTIFIC_GLADE_FILE		PACKAGE_UI_DIR "/scientific_buttons.ui"
#define BASIC_GLADE_FILE			PACKAGE_UI_DIR "/basic_buttons.ui"
#define ABOUT_GLADE_FILE 			PACKAGE_UI_DIR "/about.ui"
#ifdef WITH_HILDON
#define PREFS_GLADE_FILE 			PACKAGE_UI_DIR "/prefs-ume.ui"
#else
#define PREFS_GLADE_FILE 			PACKAGE_UI_DIR "/prefs.ui"
#endif
#define FONT_GLADE_FILE 			PACKAGE_UI_DIR "/font.ui"
#define COLOR_GLADE_FILE 			PACKAGE_UI_DIR "/color.ui"
#define CLASSIC_VIEW_GLADE_FILE		PACKAGE_UI_DIR "/classic_view.ui"
#define PAPER_VIEW_GLADE_FILE		PACKAGE_UI_DIR "/paper_view.ui"
#define DISPCTRL_RIGHT_GLADE_FILE	PACKAGE_UI_DIR "/dispctrl_right.ui"
#define DISPCTRL_RIGHTV_GLADE_FILE	PACKAGE_UI_DIR "/dispctrl_right_vertical.ui"
#define DISPCTRL_BOTTOM_GLADE_FILE	PACKAGE_UI_DIR "/dispctrl_bottom.ui"

#define MY_INFINITY_STRING "inf"

/* i18n */

#include <libintl.h>
#define _(String) gettext (String)
#define gettext_noop(String) String
#define N_(String) gettext_noop (String)

/* also change this in calc_basic.h */
#ifndef BUG_REPORT
	#define BUG_REPORT	_("Please submit a bugreport.")
#endif

/* if we do not get infinity from math.h, we try to define it by ourselves */
#include <math.h>
#ifndef INFINITY
	#define INFINITY 1.0 / 0.0
#endif

#ifndef PROG_NAME
	#define PROG_NAME	PACKAGE
#endif

/* CS_xxxx define flags for current_status. */

enum {
	CS_DEC,
	CS_HEX,
	CS_OCT,
	CS_BIN,
	NR_NUMBER_BASES
};

enum {
	CS_DEG,
	CS_RAD,
	CS_GRAD,
	NR_ANGLE_BASES
};

enum {
	CS_PAN,			/* _P_seudo _A_lgebraic _N_otation */
	CS_RPN,			/* reverse polish notation */
	CS_FORMULA,		/* formula entry */
	NR_NOTATION_MODES
};

enum {
	CS_FMOD_FLAG_INV,
	CS_FMOD_FLAG_HYP
};

enum {
	BASIC_MODE,
	SCIENTIFIC_MODE,
	PAPER_MODE,
	NR_MODES
};

enum {
	CONST_NAME_COLUMN,
	CONST_VALUE_COLUMN,
	CONST_DESC_COLUMN,
	NR_CONST_COLUMNS
};

enum {
	UFUNC_NAME_COLUMN,
	UFUNC_VARIABLE_COLUMN,
	UFUNC_EXPRESSION_COLUMN,
	NR_UFUNC_COLUMNS
};

enum {
	DISPCTRL_NONE,
	DISPCTRL_RIGHT,
	DISPCTRL_RIGHTV,
	DISPCTRL_BOTTOM,
	NR_DISPCTRL_LOCS
};

typedef struct {
	unsigned	number:2;
	unsigned	angle:2;
	unsigned	notation:2;
	unsigned	fmod:2;
	gboolean	calc_entry_start_new;
	gboolean	rpn_stack_lift_enabled;
	gboolean	allow_arith_op;
} s_current_status;

typedef struct {
	char		*button_name;
	/* for simplicity we put the display_names not in an array */
	char		*display_names[4];
	G_REAL		(*func[4])(G_REAL);
} s_function_map;

typedef struct {
	char		*button_name;
	char 		*display_string;
	void		(*func)();
} s_gfunc_map;

typedef struct {
	char		*button_name;
	/* display_string: what to display in history or formula entry. hasn't 
	 * to be the button label, e.g. "n!" and "!" 
	 */
	char 		*display_string;
	int		operation;
} s_operation_map;

typedef struct {
	int		x;
	int		y;
} s_point;

typedef struct {
	char		*desc;
	char		*name;
	char		*value;
} s_constant;

typedef struct {
	char 		*name;
	char 		*variable;
	char 		*expression;
} s_user_function;

typedef struct {
	G_REAL		*data;
	int		len;
} s_array;

extern s_array		memory;
#include "config_file.h"
extern s_preferences	prefs;
extern s_constant 	*constant;
extern s_user_function	*user_function;
extern s_current_status	current_status;
#include "calc_basic.h"
extern ALG_OBJECT	*main_alg;

#endif /* galculator.h */
