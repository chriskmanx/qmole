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

#ifndef _STARTUP_H
#define _STARTUP_H
#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "misc.h"
#ifdef ESCREEN
#include "scream.h"
#endif

/************ Macros and Definitions ************/
#ifndef EXIT_SUCCESS		/* missing from <stdlib.h> */
# define EXIT_SUCCESS	0	/* exit function success */
# define EXIT_FAILURE	1	/* exit function failure */
#endif

#define THEME_CFG	"theme.cfg"
#define USER_CFG	"user.cfg"

#define MAX_COLS	250
#define MAX_ROWS	128

#define SHADOW	2

/* convert pixel dimensions to row/column values */
#define Pixel2Width(x)          ((x) / TermWin.fwidth)
#define Pixel2Height(y)         ((y) / TermWin.fheight)
#define Pixel2Col(x)            Pixel2Width((x) - TermWin.internalBorder)
#define Pixel2Row(y)            Pixel2Height((y) - TermWin.internalBorder)
#define Width2Pixel(n)          ((n) * TermWin.fwidth)
#define Height2Pixel(n)         ((n) * TermWin.fheight)
#define Col2Pixel(col)          (Width2Pixel(col) + TermWin.internalBorder)
#define Row2Pixel(row)          (Height2Pixel(row) + TermWin.internalBorder)

#define TermWin_TotalWidth()    (TermWin.width  + 2 * TermWin.internalBorder)
#define TermWin_TotalHeight()   (TermWin.height + 2 * TermWin.internalBorder)

#define TERM_WINDOW_FULL_WIDTH()           (TermWin.width  + 2 * TermWin.internalBorder)
#define TERM_WINDOW_FULL_HEIGHT()          (TermWin.height + 2 * TermWin.internalBorder)
#define TERM_WINDOW_GET_WIDTH()            (TermWin.width)
#define TERM_WINDOW_GET_HEIGHT()           (TermWin.height)
#define TERM_WINDOW_SET_WIDTH()            (TermWin.width = TERM_WINDOW_GET_COLS() * TermWin.fwidth)
#define TERM_WINDOW_SET_HEIGHT()           (TermWin.height = TERM_WINDOW_GET_ROWS() * TermWin.fheight)
#define TERM_WINDOW_GET_REPORTED_ROWS()    (TermWin.nrow)
#define TERM_WINDOW_GET_REPORTED_COLS()    (TermWin.ncol)
#define TERM_WINDOW_GET_COLS()             (TermWin.ncol)
#define TERM_WINDOW_SET_COLS(c)            (TermWin.ncol = (c))
#ifdef ESCREEN
#  define TERM_WINDOW_GET_ROWS()           (NS_MAGIC_LINE(TermWin.screen_mode) ? TermWin.nrow - 1 : TermWin.nrow)
#  define TERM_WINDOW_SET_ROWS(r)          (TermWin.nrow = (NS_MAGIC_LINE(TermWin.screen_mode) ? (r + 1) : (r)))
#else
#  define TERM_WINDOW_GET_ROWS()           (TermWin.nrow)
#  define TERM_WINDOW_SET_ROWS(r)          (TermWin.nrow = (r))
#endif

#define Xscreen                 DefaultScreen(Xdisplay)
#define Xcmap                   DefaultColormap(Xdisplay, Xscreen)
#define Xroot                   DefaultRootWindow(Xdisplay)
#define Xvisual                 DefaultVisual(Xdisplay, Xscreen)
#ifdef DEBUG_DEPTH
#  define Xdepth                DEBUG_DEPTH
#else
#  define Xdepth                DefaultDepth(Xdisplay, Xscreen)
#endif

enum {
  PROP_TEXT,
  PROP_COMPOUND_TEXT,
  PROP_UTF8_STRING,
  PROP_CLIPBOARD,
  PROP_DESKTOP,
  PROP_TRANS_PIXMAP,
  PROP_TRANS_COLOR,
  PROP_SELECTION_DEST,
  PROP_SELECTION_INCR,
  PROP_SELECTION_TARGETS,
  PROP_ENL_COMMS,
  PROP_ENL_VERSION,
  PROP_ENL_MSG,
  PROP_DELETE_WINDOW,
  PROP_DND_PROTOCOL,
  PROP_DND_SELECTION,
  PROP_EWMH_ICON,
  PROP_EWMH_OPACITY,
  PROP_EWMH_STARTUP_ID,
  PROP_EWMH_STATE,
  PROP_EWMH_STATE_STICKY,
  NUM_PROPS
};

/************ Structures ************/
typedef struct {
  int   internalBorder; 	/* Internal border size */
  short x, y;                   /* TermWin.parent coordinates */
  short width,  height;  	/* window size [pixels] */
  short fwidth, fheight;	/* font width and height [pixels] */
  unsigned int fprop:1;		/* font is proportional */
  unsigned int focus:1;		/* window has focus */
  short ncol, nrow;		/* window size [characters] */
  short saveLines;		/* number of lines that fit in scrollback */
  short nscrolled;		/* number of line actually scrolled */
  short view_start;		/* scrollback view starts here */
  Window parent, vt;		/* parent (main) and vt100 window */
  GC gc;			/* GC for drawing text */
  long mask;                    /* X Event mask for TermWin.vt */
  XFontStruct	* font;		/* main font structure */
  XFontSet fontset;
#ifndef NO_BOLDFONT
  XFontStruct	* boldFont;	/* bold font */
#endif
#ifdef MULTI_CHARSET
  XFontStruct	* mfont;	/* multibyte font structure */
#endif
#ifdef ESCREEN
  _ns_sess      *screen;
  short          screen_pending;
  short          screen_mode;
#endif
} TermWin_t;

/************ Variables ************/
extern TermWin_t TermWin;
extern Window root;
extern Display *Xdisplay;
extern Colormap cmap;
extern char *orig_argv0;
#ifdef PIXMAP_SUPPORT
extern short bg_needs_update;
#endif
extern char *display_name;
extern Atom props[NUM_PROPS];

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN
extern int eterm_bootstrap(int argc, char *argv[]);
_XFUNCPROTOEND

#endif
