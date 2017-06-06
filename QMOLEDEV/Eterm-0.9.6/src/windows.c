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

static const char cvs_ident[] = "$Id: windows.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <X11/cursorfont.h>

#include "buttons.h"
#include "command.h"
#include "e.h"
#include "events.h"
#include "font.h"
#include "startup.h"
#include "menus.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "scrollbar.h"
#include "term.h"
#include "windows.h"

XWindowAttributes attr;
XSetWindowAttributes Attributes;
XSizeHints szHint = {
    PMinSize | PResizeInc | PBaseSize,
    0, 0, 80, 24,               /* x, y, width, height */
    1, 1,                       /* Min width, height */
    0, 0,                       /* Max width, height - unused */
    1, 1,                       /* increments: width, height */
    {1, 1},                     /* increments: x, y */
    {0, 0},                     /* Aspect ratio - unused */
    0, 0,                       /* base size: width, height */
    NorthWestGravity            /* gravity */
};
Cursor TermWin_cursor;          /* cursor for vt window */

void
set_text_property(Window win, char *propname, char *value)
{
    XTextProperty prop;
    Atom atom;

    ASSERT(propname != NULL);

    if (!value) {
        atom = XInternAtom(Xdisplay, propname, True);
        if (atom == None) {
            return;
        }
        XDeleteProperty(Xdisplay, win, atom);
    } else {
        atom = XInternAtom(Xdisplay, propname, False);
        prop.value = (unsigned char *) value;
        prop.encoding = XA_STRING;
        prop.format = 8;
        prop.nitems = strlen(value);
        XSetTextProperty(Xdisplay, win, &prop, atom);
    }
}

unsigned long
get_tint_by_color_name(const char *color)
{
    XColor wcol, xcol;
    unsigned long r, g, b, t;

    wcol.pixel = WhitePixel(Xdisplay, Xscreen);
    XQueryColor(Xdisplay, cmap, &wcol);

    D_PIXMAP(("Tint string is \"%s\", white color is rgbi:%d/%d/%d\n", color, wcol.red, wcol.green, wcol.blue));
    if (!XParseColor(Xdisplay, cmap, color, &xcol)) {
        libast_print_error("Unable to parse tint color \"%s\".  Ignoring.\n", color);
        return 0xffffff;
    }

    D_PIXMAP(("RGB values for color are %d/%d/%d\n", xcol.red, xcol.green, xcol.blue));
    if ((wcol.flags & DoRed) && (xcol.flags & DoRed)) {
        r = (xcol.red << 8) / wcol.red;
        D_PIXMAP(("Got red == %lu\n", r));
        if (r >= 0x100)
            r = 0xff;
    } else {
        r = 0xff;
    }
    if ((wcol.flags & DoGreen) && (xcol.flags & DoGreen)) {
        g = (xcol.green << 8) / wcol.green;
        D_PIXMAP(("Got green == %lu\n", g));
        if (g >= 0x100)
            g = 0xff;
    } else {
        g = 0xff;
    }
    if ((wcol.flags & DoBlue) && (xcol.flags & DoBlue)) {
        b = (xcol.blue << 8) / wcol.blue;
        D_PIXMAP(("Got blue == %lu\n", b));
        if (b >= 0x100)
            b = 0xff;
    } else {
        b = 0xff;
    }
    t = (r << 16) | (g << 8) | b;
    D_PIXMAP(("Final tint is 0x%06x\n", t));
    return t;
}

Pixel
get_bottom_shadow_color(Pixel norm_color, const char *type)
{

    XColor xcol;

    xcol.pixel = norm_color;
    XQueryColor(Xdisplay, cmap, &xcol);

    xcol.red /= 2;
    xcol.green /= 2;
    xcol.blue /= 2;

    if (!XAllocColor(Xdisplay, cmap, &xcol)) {
        libast_print_error("Unable to allocate \"%s\" (0x%08x:  0x%04x, 0x%04x, 0x%04x) in the color map.\n", type, xcol.pixel, xcol.red,
                    xcol.green, xcol.blue);
        xcol.pixel = PixColors[minColor];
    }
    return (xcol.pixel);
}

Pixel
get_top_shadow_color(Pixel norm_color, const char *type)
{

    XColor xcol, white;

# ifdef PREFER_24BIT
    white.red = white.green = white.blue = r = g = b = ~0;
    XAllocColor(Xdisplay, cmap, &white);
# else
    white.pixel = WhitePixel(Xdisplay, Xscreen);
    XQueryColor(Xdisplay, cmap, &white);
# endif

    xcol.pixel = norm_color;
    XQueryColor(Xdisplay, cmap, &xcol);

    xcol.red = MAX((white.red / 5), xcol.red);
    xcol.green = MAX((white.green / 5), xcol.green);
    xcol.blue = MAX((white.blue / 5), xcol.blue);

    xcol.red = MIN(white.red, (xcol.red * 7) / 5);
    xcol.green = MIN(white.green, (xcol.green * 7) / 5);
    xcol.blue = MIN(white.blue, (xcol.blue * 7) / 5);

    if (!XAllocColor(Xdisplay, cmap, &xcol)) {
        libast_print_error("Unable to allocate \"%s\" (0x%08x:  0x%04x, 0x%04x, 0x%04x) in the color map.\n", type, xcol.pixel, xcol.red,
                    xcol.green, xcol.blue);
        xcol.pixel = PixColors[WhiteColor];
    }
    return (xcol.pixel);
}

Pixel
get_color_by_name(const char *name, const char *fallback)
{
    XColor xcol;

    if (!name) {
        if (!fallback) {
            return ((Pixel) - 1);
        } else {
            name = fallback;
        }
    } else if (isdigit(*name)) {
        unsigned long c;

        c = strtoul(name, (char **) NULL, 0);
        if (c <= 15) {
            name = rs_color[c + minColor];
        }
    }
    if (!XParseColor(Xdisplay, cmap, name, &xcol)) {
        libast_print_warning("Unable to resolve \"%s\" as a color name.  Falling back on \"%s\".\n", name, NONULL(fallback));
        name = fallback;
        if (name) {
            if (!XParseColor(Xdisplay, cmap, name, &xcol)) {
                libast_print_warning
                    ("Unable to resolve \"%s\" as a color name.  This should never fail.  Please repair/restore your RGB database.\n",
                     name);
                return ((Pixel) - 1);
            }
        } else {
            return ((Pixel) - 1);
        }
    }
    if (!XAllocColor(Xdisplay, cmap, &xcol)) {
        libast_print_warning("Unable to allocate \"%s\" (0x%08x:  0x%04x, 0x%04x, 0x%04x) in the color map.  Falling back on \"%s\".\n",
                      name, xcol.pixel, xcol.red, xcol.green, xcol.blue, NONULL(fallback));
        name = fallback;
        if (name) {
            if (!XAllocColor(Xdisplay, cmap, &xcol)) {
                libast_print_warning("Unable to allocate \"%s\" (0x%08x:  0x%04x, 0x%04x, 0x%04x) in the color map.\n", name, xcol.pixel,
                              xcol.red, xcol.green, xcol.blue);
                return ((Pixel) - 1);
            }
        } else {
            return ((Pixel) - 1);
        }
    }
    return (xcol.pixel);
}

Pixel
get_color_by_pixel(Pixel pixel, Pixel fallback)
{
    XColor xcol;

    xcol.pixel = pixel;
    if (!XQueryColor(Xdisplay, cmap, &xcol)) {
        libast_print_warning("Unable to convert pixel value 0x%08x to an XColor structure.  Falling back on 0x%08x.\n", pixel, fallback);
        xcol.pixel = fallback;
        if (!XQueryColor(Xdisplay, cmap, &xcol)) {
            libast_print_warning("Unable to convert pixel value 0x%08x to an XColor structure.\n", xcol.pixel);
            return ((Pixel) 0);
        }
    }
    if (!XAllocColor(Xdisplay, cmap, &xcol)) {
        libast_print_warning("Unable to allocate 0x%08x (0x%04x, 0x%04x, 0x%04x) in the color map.  Falling back on 0x%08x.\n", xcol.pixel,
                      xcol.red, xcol.green, xcol.blue, fallback);
        xcol.pixel = fallback;
        if (!XAllocColor(Xdisplay, cmap, &xcol)) {
            libast_print_warning("Unable to allocate 0x%08x (0x%04x, 0x%04x, 0x%04x) in the color map.\n", xcol.pixel, xcol.red,
                          xcol.green, xcol.blue);
            return ((Pixel) 0);
        }
    }
    return (xcol.pixel);
}

void
process_colors(void)
{
    int i;
    Pixel pixel;

    for (i = 0; i < NRS_COLORS; i++) {
        D_COLORS(("Adding color %d of %d (%s)\n", i, NRS_COLORS, def_colorName[i]));
        if ((Xdepth <= 2) || ((pixel = get_color_by_name(rs_color[i], def_colorName[i])) == (Pixel) (-1))) {
            switch (i) {
                case fgColor:
                    pixel = WhitePixel(Xdisplay, Xscreen);
                    break;
                case bgColor:
                    pixel = BlackPixel(Xdisplay, Xscreen);
                    break;
#ifndef NO_CURSORCOLOR
                case cursorColor:
                    pixel = PixColors[bgColor];
                    break;
                case cursorColor2:
                    pixel = PixColors[fgColor];
                    break;
#endif /* NO_CURSORCOLOR */
#ifndef NO_BOLDUNDERLINE
                case colorBD:
                    pixel = PixColors[fgColor];
                    break;
                case colorUL:
                    pixel = PixColors[fgColor];
                    break;
#endif
#ifdef ESCREEN
                case ES_COLOR_CURRENT:
                    pixel = PixColors[YellowColor];
                    break;
                case ES_COLOR_ACTIVE:
                    pixel = PixColors[BlueColor];
                    break;
#endif
                case pointerColor:
                    pixel = PixColors[fgColor];
                    break;
                case borderColor:
                    pixel = PixColors[bgColor];
                    break;
                default:
                    pixel = PixColors[fgColor]; /* None */
                    break;
            }
        }
        D_COLORS(("Pixel : %x\n", pixel));
        PixColors[i] = pixel;
    }

    if (Xdepth <= 2) {          /* Monochrome */
        PixColors[topShadowColor] = PixColors[fgColor];
        PixColors[bottomShadowColor] = PixColors[fgColor];
        PixColors[unfocusedTopShadowColor] = PixColors[fgColor];
        PixColors[unfocusedBottomShadowColor] = PixColors[fgColor];

        PixColors[menuTopShadowColor] = PixColors[fgColor];
        PixColors[menuBottomShadowColor] = PixColors[fgColor];
        PixColors[unfocusedMenuTopShadowColor] = PixColors[fgColor];
        PixColors[unfocusedMenuBottomShadowColor] = PixColors[fgColor];
    } else {
        PixColors[bottomShadowColor] = get_bottom_shadow_color(images[image_sb].norm->bg, "bottomShadowColor");
        PixColors[unfocusedBottomShadowColor] =
            get_bottom_shadow_color(images[image_sb].disabled->bg, "unfocusedBottomShadowColor");
        PixColors[topShadowColor] = get_top_shadow_color(images[image_sb].norm->bg, "topShadowColor");
        PixColors[unfocusedTopShadowColor] = get_top_shadow_color(images[image_sb].disabled->bg, "unfocusedTopShadowColor");

        PixColors[menuBottomShadowColor] = get_bottom_shadow_color(images[image_menu].norm->bg, "menuBottomShadowColor");
        PixColors[unfocusedMenuBottomShadowColor] =
            get_bottom_shadow_color(images[image_menu].disabled->bg, "unfocusedMenuBottomShadowColor");
        PixColors[menuTopShadowColor] = get_top_shadow_color(images[image_menu].norm->bg, "menuTopShadowColor");
        PixColors[unfocusedMenuTopShadowColor] =
            get_top_shadow_color(images[image_menu].disabled->bg, "unfocusedMenuTopShadowColor");
    }
    stored_palette(SAVE);
}

void
set_pointer_colors(const char *fg_name, const char *bg_name)
{
    XColor fg, bg;

    if (fg_name) {
        fg.pixel = get_color_by_name(fg_name, COLOR_NAME(pointerColor));
    } else {
        fg.pixel = PixColors[pointerColor];
    }
    XQueryColor(Xdisplay, cmap, &fg);
    if (bg_name) {
        bg.pixel = get_color_by_name(bg_name, COLOR_NAME(bgColor));
    } else {
        bg.pixel = PixColors[bgColor];
    }
    XQueryColor(Xdisplay, cmap, &bg);
    XRecolorCursor(Xdisplay, TermWin_cursor, &fg, &bg);
}

/* Create_Windows() - Open and map the window */
void
Create_Windows(int argc, char *argv[])
{

    Cursor cursor;
    XClassHint classHint;
    XWMHints wmHint;
    Atom prop = None;
    CARD32 val;
    int x = 0, y = 0, flags;
    unsigned int width = 0, height = 0;
    MWMHints mwmhints;

    if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_BORDERLESS)) {
        mwmhints.flags = MWM_HINTS_DECORATIONS;
        mwmhints.decorations = 0;
    } else {
        mwmhints.flags = 0;
    }
    Attributes.colormap = cmap;

    szHint.base_width = (2 * TermWin.internalBorder + ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR))
                                                       ? (scrollbar_get_width() + (2 * scrollbar_get_shadow())) : 0));
    szHint.base_height = (2 * TermWin.internalBorder) + bbar_calc_docked_height(BBAR_DOCKED);

    flags = (rs_geometry ? XParseGeometry(rs_geometry, &x, &y, &width, &height) : 0);
    D_X11(("XParseGeometry(geom, %d, %d, %d, %d)\n", x, y, width, height));

    if (flags & WidthValue) {
        szHint.width = width;
        szHint.flags |= USSize;
    }
    if (flags & HeightValue) {
        szHint.height = height;
        szHint.flags |= USSize;
    }
    TERM_WINDOW_SET_COLS(szHint.width);
    TERM_WINDOW_SET_ROWS(szHint.height);

    change_font(1, NULL);

    if (flags & XValue) {
        if (flags & XNegative) {
            x += (DisplayWidth(Xdisplay, Xscreen) - (szHint.width + TermWin.internalBorder));
        }
        szHint.x = x;
        szHint.flags |= USPosition;
    }
    if (flags & YValue) {
        if (flags & YNegative) {
            y += (DisplayHeight(Xdisplay, Xscreen) - (szHint.height + TermWin.internalBorder));
        }
        szHint.y = y;
        szHint.flags |= USPosition;
    }
    if (flags) {
        D_X11(("Geometry values after parsing:  %dx%d%+d%+d\n", width, height, x, y));
    }

    Attributes.background_pixel = PixColors[bgColor];
    Attributes.border_pixel = PixColors[bgColor];
    D_X11(("Size Hints:  x %d, y %d.  Width/Height:  Base %dx%d, Minimum %dx%d, Current %dx%d, Increment %dx%d\n",
           szHint.x, szHint.y, szHint.base_width, szHint.base_height, szHint.min_width, szHint.min_height, szHint.width,
           szHint.height, szHint.width_inc, szHint.height_inc));
    TermWin.parent = XCreateWindow(Xdisplay, Xroot, szHint.x, szHint.y, szHint.width, szHint.height, 0, Xdepth, InputOutput,
#ifdef PREFER_24BIT
                                   Xvisual,
#else
                                   CopyFromParent,
#endif
                                   CWBackPixel | CWBorderPixel | CWColormap | CWOverrideRedirect, &Attributes);

    xterm_seq(ESCSEQ_XTERM_TITLE, rs_title);
    xterm_seq(ESCSEQ_XTERM_ICONNAME, rs_iconName);
    classHint.res_name = (char *) rs_name;
    classHint.res_class = APL_NAME;
    wmHint.window_group = TermWin.parent;
    wmHint.input = ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_NO_INPUT)) ? False : True);
    wmHint.initial_state = (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_ICONIC) ? IconicState : NormalState);
    wmHint.window_group = TermWin.parent;
    wmHint.flags = (InputHint | StateHint | WindowGroupHint);
#ifdef PIXMAP_SUPPORT
    set_icon_pixmap(rs_icon, &wmHint);
#endif

    XSetWMProperties(Xdisplay, TermWin.parent, NULL, NULL, argv, argc, &szHint, &wmHint, &classHint);
    XSelectInput(Xdisplay, Xroot, PropertyChangeMask);
    XSelectInput(Xdisplay, TermWin.parent,
                 (KeyPressMask | FocusChangeMask | StructureNotifyMask | VisibilityChangeMask | PropertyChangeMask));
    if (mwmhints.flags) {
        prop = XInternAtom(Xdisplay, "_MOTIF_WM_HINTS", False);
        XChangeProperty(Xdisplay, TermWin.parent, prop, prop, 32,
                        PropModeReplace, (unsigned char *) &mwmhints, PROP_MWM_HINTS_ELEMENTS);
    }

    /* vt cursor: Black-on-White is standard, but this is more popular */
    TermWin_cursor = XCreateFontCursor(Xdisplay, XC_xterm);
    set_pointer_colors(NULL, NULL);

    /* cursor (menu/scrollbar): Black-on-White */
    cursor = XCreateFontCursor(Xdisplay, XC_left_ptr);

    /* the vt window */
    TermWin.x = (((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR))
                  && !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)))
                 ? (scrollbar_get_width() + (2 * scrollbar_get_shadow())) : 0);
    TermWin.y = bbar_calc_docked_height(BBAR_DOCKED_TOP);
    TermWin.vt = XCreateWindow(Xdisplay, TermWin.parent, TermWin.x, TermWin.y, szHint.width, szHint.height,
                               0, Xdepth, InputOutput, CopyFromParent,
                               CWBackPixel | CWBorderPixel | CWOverrideRedirect | CWColormap, &Attributes);
    D_X11(("Created terminal window 0x%08x at %dx%d\n", TermWin.vt, TermWin.x, TermWin.y));
    if (!(background_is_pixmap()) && !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_BORDERLESS))) {
        XSetWindowBackground(Xdisplay, TermWin.vt, PixColors[bgColor]);
        XClearWindow(Xdisplay, TermWin.vt);
    }
    XDefineCursor(Xdisplay, TermWin.vt, TermWin_cursor);
    TermWin.mask =
        (EnterWindowMask | LeaveWindowMask | ExposureMask | ButtonPressMask | ButtonReleaseMask | Button1MotionMask |
         Button2MotionMask | Button3MotionMask);
    XSelectInput(Xdisplay, TermWin.vt, TermWin.mask);

    /* If the user wants a specific desktop, tell the WM that */
    if (rs_desktop != -1) {
        val = rs_desktop;
        XChangeProperty(Xdisplay, TermWin.parent, props[PROP_DESKTOP], XA_CARDINAL, 32, PropModeReplace, (unsigned char *) &val, 1);
    }

    /* Make window sticky if requested */
    if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_STICKY)) {
        XChangeProperty(Xdisplay, TermWin.parent, props[PROP_EWMH_STATE], XA_ATOM, 32, PropModeReplace,
                        (unsigned char *) &props[PROP_EWMH_STATE_STICKY], 1);
    }

    /* Set startup ID property if given by the launching application. */
    if (getenv("DESKTOP_STARTUP_ID")) {
        Atom atom;
        unsigned char *tmp = (spif_uchar_t *) getenv("DESKTOP_STARTUP_ID");

        atom = XInternAtom(Xdisplay, "UTF8_STRING", False);
        XChangeProperty(Xdisplay, TermWin.parent, props[PROP_EWMH_STARTUP_ID], atom, 8, PropModeReplace, tmp, strlen(tmp) + 1);
        unsetenv("DESKTOP_STARTUP_ID");
    }

    /* Set window opacity if needed. */
    if ((props[PROP_EWMH_OPACITY] != None) && (rs_opacity != 0xff)) {
        XChangeProperty(Xdisplay, TermWin.parent, props[PROP_EWMH_OPACITY],
                        XA_CARDINAL, 32, PropModeReplace, (spif_uchar_t *) &rs_opacity, 1);
        XChangeProperty(Xdisplay, TermWin.vt, props[PROP_EWMH_OPACITY],
                        XA_CARDINAL, 32, PropModeReplace, (spif_uchar_t *) &rs_opacity, 1);
    }

    /* We're done creating our windows.  Now let's initialize the event subsystem to handle them. */
    event_init_subsystem((event_dispatcher_t) process_x_event, (event_dispatcher_init_t) event_init_primary_dispatcher);

    XMapWindow(Xdisplay, TermWin.vt);
    XMapWindow(Xdisplay, TermWin.parent);
    XSetWindowBackground(Xdisplay, TermWin.vt, PixColors[bgColor]);

    render_simage(images[image_bg].current, TermWin.vt, TermWin_TotalWidth(), TermWin_TotalHeight(), image_bg, 0);
    if (image_mode_is(image_bg, MODE_AUTO)) {
        enl_ipc_sync();
    }

    /* graphics context for the vt window */
    {
        XGCValues gcvalue;

        gcvalue.font = TermWin.font->fid;
        gcvalue.foreground = PixColors[fgColor];
        gcvalue.background = PixColors[bgColor];
        gcvalue.graphics_exposures = 0;
        TermWin.gc = LIBAST_X_CREATE_GC(GCForeground | GCBackground | GCFont | GCGraphicsExposures, &gcvalue);
        D_X11(("Created GC 0x%08x for TermWin.gc\n", TermWin.gc));
    }

    if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_NO_CURSOR)) {
        scr_cursor_visible(0);
    }
}

/* resize window keeping one point (determined by window geometry) in place */
void
resize_parent(unsigned int width, unsigned int height)
{
    XWindowAttributes attr;

    if (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_RESIZE_GRAVITY)) || !XGetWindowAttributes(Xdisplay, TermWin.parent, &attr)) {
        XResizeWindow(Xdisplay, TermWin.parent, width, height);
    } else {
        Window junkwin;
        int x, y, scr_w, scr_h;
        int dx = 0, dy = 0;

        scr_w = WidthOfScreen(attr.screen);
        scr_h = HeightOfScreen(attr.screen);
        dx = attr.width - width;
        dy = attr.height - height;
        XTranslateCoordinates(Xdisplay, TermWin.parent, attr.root, 0, 0, &x, &y, &junkwin);
        /* Check position of the center of the window */
        if (x < (scr_w - attr.width) / 2) {
            /* left half */
            dx = 0;
        } else if (x == (scr_w - attr.width) / 2) {
            /* exact center */
            dx /= 2;
        }
        if (y < (scr_h - attr.height) / 2) {
            /* top half */
            dy = 0;
        } else if (y == (scr_h - attr.height) / 2) {
            /* exact center */
            dy /= 2;
        }
        D_X11(("Calling XMoveResizeWindow(Xdisplay, 0x%08x, %d + %d, %d + %d, %d, %d)\n", TermWin.parent, x, dx, y, dy, width,
               height));
        XMoveResizeWindow(Xdisplay, TermWin.parent, x + dx, y + dy, width, height);
    }
}

/* good for toggling 80/132 columns */
void
set_width(unsigned short width)
{
    unsigned short height = TERM_WINDOW_GET_REPORTED_ROWS();

    if (width != TERM_WINDOW_GET_REPORTED_COLS()) {
        width = szHint.base_width + width * TermWin.fwidth;
        height = szHint.base_height + height * TermWin.fheight;

        resize_parent(width, height);
        handle_resize(width, height);
    }
}

void
update_size_hints(void)
{
    D_X11(("Called.\n"));
    szHint.base_width = (2 * TermWin.internalBorder) + ((scrollbar_is_visible())? (scrollbar_trough_width()) : (0));
    szHint.base_height = (2 * TermWin.internalBorder) + bbar_calc_docked_height(BBAR_DOCKED);

    szHint.width_inc = TermWin.fwidth;
    szHint.height_inc = TermWin.fheight;

    D_X11(("Size Hints:  base width/height == %lux%lu, width/height increment == %lux%lu\n", szHint.base_width, szHint.base_height,
           szHint.width_inc, szHint.height_inc));

    szHint.min_width = szHint.base_width + szHint.width_inc;
    szHint.min_height = szHint.base_height + szHint.height_inc;
    szHint.width = szHint.base_width + TERM_WINDOW_GET_WIDTH();
    szHint.height = szHint.base_height + TERM_WINDOW_GET_HEIGHT();
    D_X11(("             Minimum width/height == %lux%lu, width/height == %lux%lu\n", szHint.min_width, szHint.min_height,
           szHint.width, szHint.height));

    szHint.flags = PMinSize | PResizeInc | PBaseSize;
    XSetWMNormalHints(Xdisplay, TermWin.parent, &szHint);
}

/* Resize terminal window and scrollbar window */
void
term_resize(int width, int height)
{
    static int last_width = 0, last_height = 0;

    D_X11(("term_resize(%d, %d)\n", width, height));
    TERM_WINDOW_SET_WIDTH();
    TERM_WINDOW_SET_HEIGHT();
    D_X11((" -> New TermWin width/height == %lux%lu\n", TERM_WINDOW_GET_WIDTH(), TERM_WINDOW_GET_HEIGHT()));
    width = TERM_WINDOW_FULL_WIDTH();
    height = TERM_WINDOW_FULL_HEIGHT();
    XMoveResizeWindow(Xdisplay, TermWin.vt, ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)) ? (0)
                                             : ((scrollbar_is_visible())? (scrollbar_trough_width()) : (0))),
                      bbar_calc_docked_height(BBAR_DOCKED_TOP), width, height);
    if (width != last_width || height != last_height) {
        render_simage(images[image_bg].current, TermWin.vt, width, height, image_bg, 0);
        scr_reset();
        scr_touch();
        if (image_mode_is(image_bg, MODE_AUTO)) {
            enl_ipc_sync();
        }
        last_width = width;
        last_height = height;
    }
#ifdef USE_XIM
    xim_set_status_position();
#endif
}

/* Resize due to font change; update size hints and child windows */
void
parent_resize(void)
{
    D_X11(("Called.\n"));
    update_size_hints();
    resize_parent(szHint.width, szHint.height);
    D_X11((" -> New parent width/height == %lux%lu\n", szHint.width, szHint.height));
    term_resize(szHint.width, szHint.height);
    scrollbar_resize(szHint.width, szHint.height - bbar_calc_docked_height(BBAR_DOCKED));
    bbar_resize_all(szHint.width);
}

void
handle_resize(unsigned int width, unsigned int height)
{
    static short first_time = 1;
    int new_ncol = (width - szHint.base_width) / TermWin.fwidth;
    int new_nrow = (height - szHint.base_height) / TermWin.fheight;

    D_EVENTS(("handle_resize(%u, %u)\n", width, height));

    if (first_time || (new_ncol != TERM_WINDOW_GET_REPORTED_ROWS()) || (new_nrow != TERM_WINDOW_GET_REPORTED_COLS())) {
        TERM_WINDOW_SET_COLS(new_ncol);
        TERM_WINDOW_SET_ROWS(new_nrow);
        term_resize(width, height);
        szHint.width = szHint.base_width + TERM_WINDOW_GET_WIDTH();
        szHint.height = szHint.base_height + TERM_WINDOW_GET_HEIGHT();
        D_X11((" -> New szHint.width/height == %lux%lu\n", szHint.width, szHint.height));
        scrollbar_resize(width, szHint.height - bbar_calc_docked_height(BBAR_DOCKED));
        bbar_resize_all(szHint.width);
        first_time = 0;
    }
}

void
handle_move(int x, int y)
{
    int dx, dy;

    if ((TermWin.x != x) || (TermWin.y != y)) {
        dx = abs(TermWin.x - x);
        dy = abs(TermWin.y - y);
        TermWin.x = x;
        TermWin.y = y;
        /* If we've moved an even multiple of the screen size, there's no
           need to redraw trans/viewport images; the images will line up. */
        if (image_mode_any(MODE_TRANS | MODE_VIEWPORT)) {
            if ((dx % DisplayWidth(Xdisplay, Xscreen)) || (dy % DisplayHeight(Xdisplay, Xscreen))) {
                redraw_images_by_mode(MODE_TRANS | MODE_VIEWPORT);
            }
        }
    }
}

#ifdef XTERM_COLOR_CHANGE
void
stored_palette(char op)
{
    static Pixel default_colors[NRS_COLORS + EXTRA_COLORS];
    static unsigned char stored = 0;
    unsigned int i;

    if (op == SAVE) {
        for (i = 0; i < NRS_COLORS; i++) {
            default_colors[i] = PixColors[i];
        }
        stored = 1;
    } else if (op == RESTORE && stored) {
        for (i = 0; i < NRS_COLORS; i++) {
            PixColors[i] = default_colors[i];
        }
    }
}

void
set_window_color(int idx, const char *color)
{
    XColor xcol;
    int i;

    D_X11(("idx == %d, color == \"%s\"\n", idx, NONULL(color)));

    if (!color || *color == '\0')
        return;

    /* handle color aliases */
    if (isdigit(*color)) {
        i = atoi(color);
        if (i >= 8 && i <= 15) {        /* bright colors */
            i -= 8;
            PixColors[idx] = PixColors[minBright + i];
        } else if (i >= 0 && i <= 7) {  /* normal colors */
            PixColors[idx] = PixColors[minColor + i];
        } else {
            libast_print_warning("Color index %d is invalid.\n", i);
            return;
        }
    } else if (XParseColor(Xdisplay, cmap, color, &xcol)) {
        if (!XAllocColor(Xdisplay, cmap, &xcol)) {
            libast_print_warning("Unable to allocate \"%s\" in the color map.\n", color);
            return;
        }
        if ((idx > maxBright) && (idx < 256) && (PixColors[idx])) {
            XFreeColors(Xdisplay, cmap, (unsigned long *) &(PixColors[idx]), 1, 0);
        }
        PixColors[idx] = xcol.pixel;
    } else {
        libast_print_warning("Unable to resolve \"%s\" as a color name.\n", color);
        return;
    }
    set_colorfgbg();
    scr_touch();
    scr_refresh(DEFAULT_REFRESH);
    redraw_image(image_bg);
}
#endif /* XTERM_COLOR_CHANGE */

Window
find_window_by_coords(Window win, int win_x, int win_y, int rel_x, int rel_y)
{
    Window *children = NULL;
    XWindowAttributes attr;
    Window child = 0, parent_win = 0, root_win = 0;
    int i;
    unsigned int ww, wh, num;
    int wx, wy;

    D_X11(("win 0x%08x at %d, %d.  Coords are %d, %d.\n", win, win_x, win_y, rel_x, rel_y));

    /* Bad or invisible window. */
    if ((!XGetWindowAttributes(Xdisplay, win, &attr)) || (attr.map_state != IsViewable)) {
        return None;
    }
    wx = attr.x + win_x;
    wy = attr.y + win_y;
    ww = attr.width;
    wh = attr.height;

    if (!((rel_x >= wx) && (rel_y >= wy) && (rel_x < (int) (wx + ww)) && (rel_y < (int) (wy + wh)))) {
        return None;
    }

    if (!XQueryTree(Xdisplay, win, &root_win, &parent_win, &children, &num)) {
        return win;
    }
    if (children) {
        D_X11(("%d children.\n", num));
        for (i = num - 1; i >= 0; i--) {
            D_X11(("Trying children[%d] (0x%08x)\n", i, children[i]));
            if ((child = find_window_by_coords(children[i], wx, wy, rel_x, rel_y)) != None) {
                D_X11(("Match!\n"));
                XFree(children);
                return child;
            }
        }
        D_X11(("XFree(children)\n"));
        XFree(children);
    }
    D_X11(("Returning 0x%08x\n", win));
    return win;
}
