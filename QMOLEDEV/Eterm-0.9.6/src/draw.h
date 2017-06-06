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

#ifndef _DRAW_H_
#define _DRAW_H_

#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */
#include "pixmap.h"

/************ Macros and Definitions ************/
#define DRAW_ARROW_UP    (1UL << 0)
#define DRAW_ARROW_DOWN  (1UL << 1)
#define DRAW_ARROW_LEFT  (1UL << 2)
#define DRAW_ARROW_RIGHT (1UL << 3)

#define draw_uparrow_raised(d, g1, g2, x, y, w, s)      draw_arrow(d, g1, g2, x, y, w, s, DRAW_ARROW_UP)
#define draw_uparrow_clicked(d, g1, g2, x, y, w, s)     draw_arrow(d, g2, g1, x, y, w, s, DRAW_ARROW_UP)
#define draw_downarrow_raised(d, g1, g2, x, y, w, s)    draw_arrow(d, g1, g2, x, y, w, s, DRAW_ARROW_DOWN)
#define draw_downarrow_clicked(d, g1, g2, x, y, w, s)   draw_arrow(d, g2, g1, x, y, w, s, DRAW_ARROW_DOWN)

#define DRAW_SOLID_BEVEL(d, w, h, p, u, s)  draw_shadow_from_colors((d), ((u) ? get_top_shadow_color(p, "") : get_bottom_shadow_color(p, "")), \
                                                                    ((u) ? get_bottom_shadow_color(p, "") : get_top_shadow_color(p, "")), \
                                                                    0, 0, (w), (h), (s));

/************ Structures ************/

/************ Variables ************/

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void draw_shadow(Drawable d, GC gc_top, GC gc_bottom, int x, int y, int w, int h, int shadow);
extern void draw_shadow_from_colors(Drawable d, Pixel top, Pixel bottom, int x, int y, int w, int h, int shadow);
extern void draw_arrow(Drawable d, GC gc_top, GC gc_bottom, int x, int y, int w, int shadow, unsigned char type);
extern void draw_arrow_from_colors(Drawable d, Pixel top, Pixel bottom, int x, int y, int w, int shadow, unsigned char type);
extern void draw_box(Drawable d, GC gc_top, GC gc_bottom, int x, int y, int w, int h);
extern void bevel_pixmap(Pixmap p, int w, int h, Imlib_Border *bord, unsigned char up);

_XFUNCPROTOEND

#endif	/* _DRAW_H_ */
