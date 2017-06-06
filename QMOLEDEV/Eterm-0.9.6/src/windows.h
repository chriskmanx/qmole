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

#ifndef _WINDOWS_H_
#define _WINDOWS_H_
/* includes */
#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

/************ Macros and Definitions ************/

/************ Variables ************/
extern char *rs_color[NRS_COLORS];
extern Pixel PixColors[NRS_COLORS + EXTRA_COLORS];
extern XSetWindowAttributes Attributes;
extern XWindowAttributes attr;
extern XSizeHints szHint;

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void set_text_property(Window, char *, char *);
extern unsigned long get_tint_by_color_name(const char *);
extern Pixel get_bottom_shadow_color(Pixel, const char *);
extern Pixel get_top_shadow_color(Pixel, const char *);
extern Pixel get_color_by_name(const char *, const char *);
extern Pixel get_color_by_pixel(Pixel, Pixel);
extern void process_colors(void);
extern void set_pointer_colors(const char *, const char *);
extern void Create_Windows(int, char * []);
extern void resize_parent(unsigned int, unsigned int);
extern void set_width(unsigned short);
extern void update_size_hints(void);
extern void term_resize(int, int);
extern void parent_resize(void);
extern void handle_resize(unsigned int, unsigned int);
extern void handle_move(int, int);
#ifdef XTERM_COLOR_CHANGE
extern void stored_palette(char);
extern void set_window_color(int, const char *);
#else
# define stored_palette(c)           NOP
# define set_window_color(idx,color) NOP
#endif /* XTERM_COLOR_CHANGE */
extern Window find_window_by_coords(Window, int, int, int, int);

_XFUNCPROTOEND

#endif	/* _WINDOWS_H_ */
