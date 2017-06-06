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

#ifndef _FONT_H_
#define _FONT_H_

#include <stdio.h>
#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

/************ Macros and Definitions ************/
#define FONT_TYPE_X             (0x01)
#define FONT_TYPE_TTF           (0x02)
#define FONT_TYPE_FNLIB         (0x03)

#define font_cache_add_ref(font) ((font)->ref_cnt++)

#define NFONTS 5
#define FONT_CMD       '#'
#define BIGGER_FONT    "#+"
#define SMALLER_FONT   "#-"

/* These are subscripts for the arrays in a fontshadow_t */
#define SHADOW_TOP_LEFT      0
#define SHADOW_TOP           1
#define SHADOW_TOP_RIGHT     2
#define SHADOW_LEFT          3
#define SHADOW_RIGHT         4
#define SHADOW_BOTTOM_LEFT   5
#define SHADOW_BOTTOM        6
#define SHADOW_BOTTOM_RIGHT  7

/* The macros are used to advance to the next/previous font as with Ctrl-> and Ctrl-< */
#define NEXT_FONT(i)   do { if (font_idx + ((i)?(i):1) >= font_cnt) {font_idx = font_cnt - 1;} else {font_idx += ((i)?(i):1);} \
                            while (!etfonts[font_idx]) {if (font_idx == font_cnt) {font_idx--; break;} font_idx++;} } while (0)
#define PREV_FONT(i)   do { if (font_idx - ((i)?(i):1) < 0) {font_idx = 0;} else {font_idx -= ((i)?(i):1);} \
                            while (!etfonts[font_idx]) {if (font_idx == 0) break; font_idx--;} } while (0)
#define DUMP_FONTS()   do {unsigned char i; D_FONT(("DUMP_FONTS():  Font count is %u\n", (unsigned int) font_cnt)); \
                           for (i = 0; i < font_cnt; i++) {D_FONT(("DUMP_FONTS():  Font %u == \"%s\"\n", (unsigned int) i, NONULL(etfonts[i])));}} while (0)

/************ Structures ************/
typedef struct cachefont_struct {
  char *name;             /* Font name in canonical format */
  unsigned char type;     /* Font type (FONT_TYPE_* from above */
  unsigned char ref_cnt;  /* Reference count */
  union {
    /* This union will eventually have members for TTF/Fnlib fonts */
    XFontStruct *xfontinfo;
  } fontinfo;
  struct cachefont_struct *next;
} cachefont_t;

typedef struct fontshadow_struct {
  Pixel color[8];
  unsigned char shadow[8];
  unsigned char do_shadow;
} fontshadow_t;

/************ Variables ************/
extern unsigned char font_idx, font_cnt, font_chg;
extern int def_font_idx;
extern const char *def_fontName[];
extern char *rs_font[NFONTS];
extern char **etfonts, **etmfonts;
extern fontshadow_t fshadow;
# ifdef MULTI_CHARSET
extern const char *def_mfontName[];
extern char *rs_mfont[NFONTS];
# endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void eterm_font_add(char ***plist, const char *fontname, unsigned char idx);
extern void eterm_font_delete(char **flist, unsigned char idx);
extern void eterm_font_list_clear(void);
extern void font_cache_clear(void);
extern void *load_font(const char *, const char *, unsigned char);
extern void free_font(const void *);
extern void change_font(int, const char *);
extern const char *get_font_name(void *);
extern void set_shadow_color_by_name(unsigned char, const char *);
extern void set_shadow_color_by_pixel(unsigned char, Pixel);
extern unsigned char parse_font_fx(char *line);

_XFUNCPROTOEND

#endif	/* _FONT_H_ */
