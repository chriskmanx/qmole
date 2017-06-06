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

static const char cvs_ident[] = "$Id: draw.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include "draw.h"
#include "misc.h"
#include "pixmap.h"
#include "startup.h"

void
draw_shadow(Drawable d, GC gc_top, GC gc_bottom, int x, int y, int w, int h, int shadow)
{

    ASSERT(w != 0);
    ASSERT(h != 0);
    LOWER_BOUND(shadow, 1);

    for (w += x - 1, h += y - 1; shadow > 0; shadow--, w--, h--) {
        XDrawLine(Xdisplay, d, gc_top, x, y, w, y);
        XDrawLine(Xdisplay, d, gc_top, x, y, x, h);
        x++;
        y++;
        XDrawLine(Xdisplay, d, gc_bottom, w, h, w, y);
        XDrawLine(Xdisplay, d, gc_bottom, w, h, x, h);
    }
}

void
draw_shadow_from_colors(Drawable d, Pixel top, Pixel bottom, int x, int y, int w, int h, int shadow)
{
    static GC gc_top = (GC) 0, gc_bottom = (GC) 0;

    if (gc_top == 0) {
        gc_top = LIBAST_X_CREATE_GC(0, NULL);
        gc_bottom = LIBAST_X_CREATE_GC(0, NULL);
    }

    XSetForeground(Xdisplay, gc_top, top);
    XSetForeground(Xdisplay, gc_bottom, bottom);
    draw_shadow(d, gc_top, gc_bottom, x, y, w, h, shadow);
}

void
draw_arrow(Drawable d, GC gc_top, GC gc_bottom, int x, int y, int w, int shadow, unsigned char type)
{

    BOUND(shadow, 1, 2);

    switch (type) {
        case DRAW_ARROW_UP:
            for (; shadow > 0; shadow--, x++, y++, w--) {
                XDrawLine(Xdisplay, d, gc_top, x, y + w, x + w / 2, y);
                XDrawLine(Xdisplay, d, gc_bottom, x + w, y + w, x + w / 2, y);
                XDrawLine(Xdisplay, d, gc_bottom, x + w, y + w, x, y + w);
            }
            break;
        case DRAW_ARROW_DOWN:
            for (; shadow > 0; shadow--, x++, y++, w--) {
                XDrawLine(Xdisplay, d, gc_top, x, y, x + w / 2, y + w);
                XDrawLine(Xdisplay, d, gc_top, x, y, x + w, y);
                XDrawLine(Xdisplay, d, gc_bottom, x + w, y, x + w / 2, y + w);
            }
            break;
        case DRAW_ARROW_LEFT:
            for (; shadow > 0; shadow--, x++, y++, w--) {
                XDrawLine(Xdisplay, d, gc_bottom, x + w, y + w, x + w, y);
                XDrawLine(Xdisplay, d, gc_bottom, x + w, y + w, x, y + w / 2);
                XDrawLine(Xdisplay, d, gc_top, x, y + w / 2, x + w, y);
            }
            break;
        case DRAW_ARROW_RIGHT:
            for (; shadow > 0; shadow--, x++, y++, w--) {
                XDrawLine(Xdisplay, d, gc_top, x, y, x, y + w);
                XDrawLine(Xdisplay, d, gc_top, x, y, x + w, y + w / 2);
                XDrawLine(Xdisplay, d, gc_bottom, x, y + w, x + w, y + w / 2);
            }
            break;
        default:
            break;
    }
}

void
draw_arrow_from_colors(Drawable d, Pixel top, Pixel bottom, int x, int y, int w, int shadow, unsigned char type)
{
    static GC gc_top = (GC) 0, gc_bottom = (GC) 0;

    if (gc_top == 0) {
        gc_top = LIBAST_X_CREATE_GC(0, NULL);
        gc_bottom = LIBAST_X_CREATE_GC(0, NULL);
    }

    XSetForeground(Xdisplay, gc_top, top);
    XSetForeground(Xdisplay, gc_bottom, bottom);
    draw_arrow(d, gc_top, gc_bottom, x, y, w, shadow, type);
}

void
draw_box(Drawable d, GC gc_top, GC gc_bottom, int x, int y, int w, int h)
{
    XDrawLine(Xdisplay, d, gc_top, x + w, y, x, y);
    XDrawLine(Xdisplay, d, gc_top, x, y, x, y + h);
    XDrawLine(Xdisplay, d, gc_bottom, x, y + h, x + w, y + h);
    XDrawLine(Xdisplay, d, gc_bottom, x + w, y + h, x + w, y);
}

#define SHADE_PIXEL(pixel, dir, tmp) do {(tmp) = ((((double)pixel)/depth_factor) + ((dir) ? 0.2 : -0.2)) * depth_factor; \
                                         if ((tmp) > (depth_factor-1)) (tmp) = depth_factor - 1; else if ((tmp) < 0) (tmp) = 0;} while (0)
#define MOD_PIXEL_HIGH(x, y, up) do {v = XGetPixel(ximg, (x), (y)); r = (int) ((v >> br) & mr); g = (int) ((v >> bg) & mg); b = (int) ((v << bb) & mb); \
	                             SHADE_PIXEL(r, (up), dv); r = (int) dv; SHADE_PIXEL(g, (up), dv); g = (int) dv; SHADE_PIXEL(b, (up), dv); b = (int) dv; \
	                             v = ((r & mr) << br) | ((g & mg) << bg) | ((b & mb) >> bb); XPutPixel(ximg, (x), (y), v);} while (0)

void
bevel_pixmap(Pixmap p, int w, int h, Imlib_Border * bord, unsigned char up)
{
    XImage *ximg;
    register unsigned long v;
    double dv;
    short x, y, xbound, ybound;
    unsigned int r, g, b;
    int real_depth = 0, depth_factor;
    register int br, bg, bb;    /* Bitshifts */
    register unsigned int mr, mg, mb;   /* Bitmasks */
    GC gc;

    if (!bord)
        return;

    depth_factor = 1 << Xdepth;
    if (Xdepth <= 8) {
        D_PIXMAP(("Depth of %d is not supported.  Punt!\n", Xdepth));
        return;
    } else if (Xdepth == 16) {

        XWindowAttributes xattr;

        XGetWindowAttributes(Xdisplay, Xroot, &xattr);
        if ((xattr.visual->red_mask == 0x7c00) && (xattr.visual->green_mask == 0x3e0) && (xattr.visual->blue_mask == 0x1f)) {
            real_depth = 15;
            depth_factor = 1 << 15;
        }
    }
    if (!real_depth) {
        real_depth = Xdepth;
    }
    ximg = XGetImage(Xdisplay, p, 0, 0, w, h, -1, ZPixmap);
    if (!ximg) {
        return;
    }
    /* Determine bitshift and bitmask values */
    switch (real_depth) {
        case 15:
            br = 7;
            bg = 2;
            bb = 3;
            mr = mg = mb = 0xf8;
            break;
        case 16:
            br = 8;
            bg = bb = 3;
            mr = mb = 0xf8;
            mg = 0xfc;
            break;
        case 24:
        case 32:
            br = 16;
            bg = 8;
            bb = 0;
            mr = mg = mb = 0xff;
            break;
        default:
            return;
    }

    /* Left edge */
    for (y = bord->top; y < h; y++) {
        xbound = h - y;
        if (xbound > bord->left)
            xbound = bord->left;
        for (x = 0; x < xbound; x++) {
            MOD_PIXEL_HIGH(x, y, up);
        }
    }

    /* Right edge */
    ybound = h - bord->bottom;
    for (y = 0; y < ybound; y++) {
        xbound = bord->right - y;
        if (xbound < 0)
            xbound = 0;
        for (x = xbound; x < bord->right; x++) {
            MOD_PIXEL_HIGH(x + (w - bord->right), y, !up);
        }
    }

    /* Top edge */
    for (y = 0; y < bord->top; y++) {
        xbound = w - y;
        for (x = 0; x < xbound; x++) {
            MOD_PIXEL_HIGH(x, y, up);
        }
    }

    /* Bottom edge */
    for (y = h - bord->bottom; y < h; y++) {
        for (x = h - y - 1; x < w; x++) {
            MOD_PIXEL_HIGH(x, y, !up);
        }
    }
    gc = LIBAST_X_CREATE_GC(0, NULL);
    XPutImage(Xdisplay, p, gc, ximg, 0, 0, 0, 0, w, h);
    LIBAST_X_FREE_GC(gc);
    XDestroyImage(ximg);
}
