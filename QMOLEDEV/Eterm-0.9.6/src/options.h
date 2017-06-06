/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

/************ Macros and Definitions ************/
# define VT_OPTIONS_CONSOLE                       (1LU <<  0)
# define VT_OPTIONS_VISUAL_BELL                   (1LU <<  1)
# define VT_OPTIONS_MAP_ALERT                     (1LU <<  2)
# define VT_OPTIONS_REVERSE_VIDEO                 (1LU <<  3)
# define VT_OPTIONS_META8                         (1LU <<  4)
# define VT_OPTIONS_HOME_ON_OUTPUT                (1LU <<  5)
# define VT_OPTIONS_HOME_ON_INPUT                 (1LU <<  6)
# define VT_OPTIONS_REPORT_AS_KEYSYMS             (1LU <<  7)
# define VT_OPTIONS_PROPORTIONAL                  (1LU <<  8)
# define VT_OPTIONS_SECONDARY_SCREEN              (1LU <<  9)
# define VT_OPTIONS_OVERSTRIKE_BOLD               (1LU << 10)
# define VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND     (1LU << 11)
# define VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND    (1LU << 12)
# define VT_OPTIONS_COLORS_SUPPRESS_BOLD          (1LU << 13)
# define VT_OPTIONS_URG_ALERT                     (1LU << 14)

# define ETERM_OPTIONS_LOGIN_SHELL                (1LU <<  0)
# define ETERM_OPTIONS_ICONIC                     (1LU <<  1)
# define ETERM_OPTIONS_WRITE_UTMP                 (1LU <<  2)
# define ETERM_OPTIONS_SCROLLBAR                  (1LU <<  3)
# define ETERM_OPTIONS_SCROLLBAR_RIGHT            (1LU <<  4)
# define ETERM_OPTIONS_BORDERLESS                 (1LU <<  5)
# define ETERM_OPTIONS_NO_INPUT                   (1LU <<  6)
# define ETERM_OPTIONS_NO_CURSOR                  (1LU <<  7)
# define ETERM_OPTIONS_PAUSE                      (1LU <<  8)
# define ETERM_OPTIONS_XTERM_SELECT               (1LU <<  9)
# define ETERM_OPTIONS_SELECT_WHOLE_LINE          (1LU << 10)
# define ETERM_OPTIONS_SCROLLBAR_POPUP            (1LU << 11)
# define ETERM_OPTIONS_SELECT_TRAILING_SPACES     (1LU << 12)
# define ETERM_OPTIONS_INSTALL                    (1LU << 13)
# define ETERM_OPTIONS_SCROLLBAR_FLOATING         (1LU << 14)
# define ETERM_OPTIONS_DOUBLE_BUFFER              (1LU << 15)
# define ETERM_OPTIONS_MBYTE_CURSOR               (1LU << 16)
# define ETERM_OPTIONS_RESIZE_GRAVITY             (1LU << 17)
# define ETERM_OPTIONS_STICKY                     (1LU << 18)

# define IMAGE_OPTIONS_TRANS                      (1U  <<  0)
# define IMAGE_OPTIONS_ITRANS                     (1U  <<  1)
# define IMAGE_OPTIONS_VIEWPORT                   (1U  <<  2)

# define BBAR_FORCE_TOGGLE              (0x03)

# define SAVE_THEME_CONFIG              ((unsigned char) 1)
# define SAVE_USER_CONFIG               ((unsigned char) 0)

#define PARSE_TRY_USER_THEME            ((unsigned char) 0x01)
#define PARSE_TRY_DEFAULT_THEME         ((unsigned char) 0x02)
#define PARSE_TRY_NO_THEME              ((unsigned char) 0x04)
#define PARSE_TRY_ALL                   ((unsigned char) 0x07)

#define TO_KEYSYM(p,s)             do { KeySym sym; \
                                     if (s && ((sym = XStringToKeysym(s)) != 0)) *p = sym; \
                                   } while (0)
#define CHECK_VALID_INDEX(i)       (((i) >= image_bg) && ((i) < image_max))

#define RESET_AND_ASSIGN(var, val)  do {if ((var)) FREE(var);  (var) = (val);} while (0)

#define BITFIELD_SET(var, field)       ((var) |= (field))
#define BITFIELD_CLEAR(var, field)     ((var) &= ~(field))
#define BITFIELD_IS_SET(var, field)    ((var) & (field))

/************ Structures ************/

/************ Variables ************/
extern unsigned long eterm_options, vt_options, image_options;
extern char *theme_dir, *user_dir;
extern       char **rs_exec_args;       /* Args to exec (-e or --exec) */
extern       char  *rs_title;		/* Window title */
extern       char  *rs_iconName;	/* Icon name */
extern       char  *rs_geometry;	/* Geometry string */
extern        int   rs_desktop;         /* Startup desktop */
extern        int   rs_saveLines;	/* Lines in the scrollback buffer */
extern unsigned short rs_min_anchor_size; /* Minimum size, in pixels, of the scrollbar anchor */
extern       char  *rs_finished_title;	/* Text added to window title (--pause) */
extern       char  *rs_finished_text;	/* Text added to scrollback (--pause) */
extern       char  *rs_term_name;
extern       char  *rs_icon;
extern       char  *rs_scrollbar_type;
extern unsigned long rs_scrollbar_width;
extern       char  *rs_scrollbar_type;
extern       char  *rs_anim_pixmap_list;
extern       char **rs_anim_pixmaps;
extern     time_t   rs_anim_delay;
extern char *rs_path;
extern char *rs_no_cursor;
#ifdef USE_XIM
extern char *rs_input_method;
extern char *rs_preedit_type;
#endif
extern char *rs_name;
extern char *rs_theme;
extern char *rs_config_file;
#ifdef ESCREEN
extern char *rs_url;
extern char *rs_hop;
extern int rs_delay;
extern unsigned char rs_es_dock;
extern char *rs_es_font;
#endif
extern spif_uint32_t rs_opacity;
extern unsigned int rs_line_space;
extern spif_charptr_t rs_beep_command;
extern unsigned int rs_meta_mod, rs_alt_mod, rs_numlock_mod;
#ifndef NO_BOLDFONT
extern char *rs_boldFont;
#endif
#ifdef PRINTPIPE
extern char *rs_print_pipe;
#endif
extern char *rs_cutchars;
#ifdef CUTCHAR_OPTION
extern char *rs_cutchars;
#endif
extern const char *true_vals[];
extern const char *false_vals[];
#ifdef KEYSYM_ATTRIBUTE
extern unsigned char *KeySym_map[256];
#endif
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
extern KeySym ks_bigfont;
extern KeySym ks_smallfont;
#endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern char *spifconf_parse_theme(char **theme, char *spifconf_name, unsigned char fallback);
extern void init_libast(void);
extern void init_defaults(void);
extern void post_parse(void);
unsigned char save_config(char *, unsigned char);

_XFUNCPROTOEND

#endif	/* _OPTIONS_H_ */
