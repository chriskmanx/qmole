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

static const char cvs_ident[] = "$Id: buttons.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <X11/cursorfont.h>

#include "buttons.h"
#include "command.h"
#include "draw.h"
#include "e.h"
#include "events.h"
#include "font.h"
#include "startup.h"
#include "menus.h"
#include "misc.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "script.h"
#include "term.h"
#include "windows.h"
#ifdef ESCREEN
#  include "screamcfg.h"
#endif

static inline void draw_string(buttonbar_t *, Drawable, GC, int, int, char *, size_t);

buttonbar_t *buttonbar = NULL;

#ifdef ESCREEN
button_t *drag = NULL;
#endif
long bbar_total_h = -1;

static inline void
draw_string(buttonbar_t *bbar, Drawable d, GC gc, int x, int y, char *str, size_t len)
{

    D_BBAR(("Writing string \"%s\" (length %lu) using font 0x%08x onto drawable 0x%08x at %d, %d\n",
            str, len, bbar->font, d, x, y));
    REQUIRE(bbar != NULL);
    REQUIRE(d != None);
    REQUIRE(gc != None);

#ifdef MULTI_CHARSET
    if (bbar->fontset && encoding_method != LATIN1)
        XmbDrawString(Xdisplay, d, bbar->fontset, gc, x, y, str, len);
    else
#endif
        XDrawString(Xdisplay, d, gc, x, y, str, len);
    return;
}

buttonbar_t *bbar_create(void)
{
    buttonbar_t *bbar;
    Cursor cursor;
    long mask;
    XGCValues gcvalue;
    XSetWindowAttributes xattr;

    bbar = (buttonbar_t *) MALLOC(sizeof(buttonbar_t));
    MEMSET(bbar, 0, sizeof(buttonbar_t));

    xattr.border_pixel = BlackPixel(Xdisplay, Xscreen);
    xattr.save_under = FALSE;
    xattr.override_redirect = TRUE;
    xattr.colormap = cmap;

    cursor = XCreateFontCursor(Xdisplay, XC_left_ptr);
    mask = KeyPressMask | EnterWindowMask | LeaveWindowMask | PointerMotionMask
        | ButtonMotionMask | ButtonPressMask | ButtonReleaseMask;
    gcvalue.foreground = xattr.border_pixel;

    bbar->font = load_font(etfonts[def_font_idx], "fixed", FONT_TYPE_X);
    bbar->fwidth = bbar->font->max_bounds.width;
    bbar->fheight = bbar->font->ascent + bbar->font->descent;
    bbar->h = 1;
    bbar->w = 1;
    gcvalue.font = bbar->font->fid;

    bbar->win = XCreateWindow(Xdisplay, Xroot, bbar->x, bbar->y, bbar->w, bbar->h, 0, Xdepth, InputOutput, CopyFromParent,
                              CWOverrideRedirect | CWSaveUnder | CWBorderPixel | CWColormap, &xattr);
    XDefineCursor(Xdisplay, bbar->win, cursor);
    XSelectInput(Xdisplay, bbar->win, mask);
    XStoreName(Xdisplay, bbar->win, "Eterm Button Bar");

    bbar->gc = LIBAST_X_CREATE_GC(GCForeground | GCFont, &gcvalue);
    bbar_set_docked(bbar, BBAR_DOCKED_TOP);
    bbar_set_visible(bbar, 1);
    bbar->image_state = IMAGE_STATE_CURRENT;

    D_BBAR(("bbar created:  Window 0x%08x, dimensions %dx%d\n", bbar->win, bbar->w, bbar->h));
    return bbar;
}

void
bbar_free(buttonbar_t *bbar)
{
    if (bbar->next) {
        bbar_free(bbar->next);
    }
    if (bbar->rbuttons) {
        button_free(bbar->rbuttons);
    }
    if (bbar->buttons) {
        button_free(bbar->buttons);
    }
#ifdef MULTI_CHARSET
    if (bbar->fontset) {
        XFreeFontSet(Xdisplay, bbar->fontset);
    }
#endif
    if (bbar->font) {
        free_font(bbar->font);
    }
    if (bbar->gc != None) {
        LIBAST_X_FREE_GC(bbar->gc);
    }
    if (bbar->win != None) {
        XDestroyWindow(Xdisplay, bbar->win);
    }
    FREE(bbar);
}

void
bbar_init(buttonbar_t *bbar, int width)
{
    event_register_dispatcher(bbar_dispatch_event, bbar_event_init_dispatcher);
    for (; bbar; bbar = bbar->next) {
        XSetForeground(Xdisplay, bbar->gc, images[image_bbar].norm->fg);
        bbar_redock(bbar);
        if (bbar_is_visible(bbar)) {
            bbar_set_visible(bbar, 0);
            bbar_show(bbar, 1);
        }
        bbar_resize(bbar, -width);
        bbar_reset_total_height();
    }
}

void
bbar_event_init_dispatcher(void)
{
    buttonbar_t *bbar;

    /* FIXME:  The event subsystem needs to be able to pass a pointer to the event data structure. */
    EVENT_DATA_ADD_HANDLER(buttonbar->event_data, EnterNotify, bbar_handle_enter_notify);
    EVENT_DATA_ADD_HANDLER(buttonbar->event_data, LeaveNotify, bbar_handle_leave_notify);
    EVENT_DATA_ADD_HANDLER(buttonbar->event_data, ButtonPress, bbar_handle_button_press);
    EVENT_DATA_ADD_HANDLER(buttonbar->event_data, ButtonRelease, bbar_handle_button_release);
    EVENT_DATA_ADD_HANDLER(buttonbar->event_data, MotionNotify, bbar_handle_motion_notify);

    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        event_data_add_mywin(&buttonbar->event_data, bbar->win);
    }
}

unsigned char
bbar_handle_enter_notify(event_t *ev)
{
    buttonbar_t *bbar;
    button_t *b;
    Window unused_root, unused_child;
    int unused_root_x, unused_root_y;
    unsigned int unused_mask;

    D_EVENTS(("bbar_handle_enter_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &buttonbar->event_data), 0);

    if (!(bbar = find_bbar_by_window(ev->xany.window))) {
        return 0;
    }
    bbar_draw(bbar, IMAGE_STATE_SELECTED, 0);
    XQueryPointer(Xdisplay, bbar->win, &unused_root, &unused_child, &unused_root_x, &unused_root_y, &(ev->xbutton.x),
                  &(ev->xbutton.y), &unused_mask);
    b = find_button_by_coords(bbar, ev->xbutton.x, ev->xbutton.y);
    if (b) {
        bbar_select_button(bbar, b);
    }
    return 1;
}

unsigned char
bbar_handle_leave_notify(event_t *ev)
{
    buttonbar_t *bbar;

    D_EVENTS(("bbar_handle_leave_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &buttonbar->event_data), 0);

    if (!(bbar = find_bbar_by_window(ev->xany.window))) {
        return 0;
    }
    bbar_draw(bbar, IMAGE_STATE_NORMAL, 0);
    if (bbar->current) {
        bbar_deselect_button(bbar, bbar->current);
    }
    return 1;
}

unsigned char
bbar_handle_button_press(event_t *ev)
{
    buttonbar_t *bbar;

    D_EVENTS(("bbar_handle_button_press(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &buttonbar->event_data), 0);

    if (!(bbar = find_bbar_by_window(ev->xany.window))) {
        D_EVENTS((" -> No buttonbar found for this window.\n"));
        return 0;
    }
    if (bbar->current) {
        bbar_click_button(bbar, bbar->current);
        button_check_action(bbar, bbar->current, ev->xbutton.button, ev->xbutton.time);
#ifdef ESCREEN
        drag = bbar->current;
#endif
    }
    return 1;
}

unsigned char
bbar_handle_button_release(event_t *ev)
{
    buttonbar_t *bbar;
    button_t *b;
    Window unused_root, unused_child;
    int unused_root_x, unused_root_y;
    unsigned int unused_mask;

    D_EVENTS(("bbar_handle_button_release(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

#ifdef ESCREEN
    if (drag && TermWin.screen && TermWin.screen->backend && TermWin.screen->userdef) {
        buttonbar_t *bbar = (buttonbar_t *) TermWin.screen->userdef;
        button_t *b;
        int fm = 0, to = 0;

        D_ESCREEN(("Checking for dragged button.\n"));
        if (bbar && (b = bbar->buttons) && (drag != bbar->current)) {
            while (b && (b != drag)) {
                b = b->next;
                fm++;
            }
            if (!b) {
                D_ESCREEN((" -> Dragged button is not on the Escreen buttonbar.\n"));
                drag = NULL;
            } else {

                if (bbar->current) {
                    b = bbar->buttons;
                    while (b && (b != bbar->current)) {
                        b = b->next;
                        to++;
                    }
                    if (!b) {
                        D_ESCREEN((" -> Target button is not on the Escreen buttonbar.\n"));
                        drag = NULL;
                    }
                }
            }
        } else {
            drag = NULL;
        }

        if (drag) {
            if (!bbar->current) {
                char *u = ns_get_url(TermWin.screen, fm);

                D_ESCREEN(("Button for display %d dragged off.\n", fm));
                if (u) {
                    char *c;
                    size_t l = strlen(orig_argv0) + strlen(u) + 7;

                    if ((c = MALLOC(l))) {
                        snprintf(c, l, "%s%s -U %s", ((orig_argv0[0] == '/')
                                                      || ((orig_argv0[0] == '.')
                                                          && (orig_argv0[1] == '/'))) ? "" : "./", orig_argv0, u);
                        D_ESCREEN(("(experimental) creating other frame using \"%s\"\n", c));
                        (void) ns_run(TermWin.screen->efuns, c);
                        FREE(c);
                    }
                    FREE(u);
                }
                return 1;
            } else if (bbar->current != drag) {
                D_ESCREEN(("Button for display %d dragged to display %d\n", fm, to));
                ns_mov_disp(TermWin.screen, fm, to);
                bbar->current = drag = NULL;
                return 1;
            }
        }
    }
    D_ESCREEN(("No drag detected.  Proceeding with normal handling.\n"));
    drag = NULL;
#endif

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &buttonbar->event_data), 0);

    if (!(bbar = find_bbar_by_window(ev->xany.window))) {
        D_EVENTS((" -> No buttonbar found for this window.\n"));
        return 0;
    }

    XQueryPointer(Xdisplay, bbar->win, &unused_root, &unused_child, &unused_root_x, &unused_root_y, &(ev->xbutton.x),
                  &(ev->xbutton.y), &unused_mask);

    b = find_button_by_coords(bbar, ev->xbutton.x, ev->xbutton.y);
    if (b) {
        D_EVENTS(("Event in buttonbar %8p, button %8p (%s)\n", bbar, b, NONULL(b->text)));
        if (bbar->current && (b != bbar->current)) {
            D_EVENTS(("Current button %8p (%s) doesn't match event button %8p (%s)\n", bbar->current, NONULL(bbar->current->text),
                      b, NONULL(b->text)));
            bbar_deselect_button(bbar, bbar->current);
        } else {
            bbar_select_button(bbar, b);
            button_check_action(bbar, b, 0, ev->xbutton.time);
        }
    } else {
        D_EVENTS(("Event in buttonbar %8p but no button.\n", bbar));
    }
    return 1;
}

unsigned char
bbar_handle_motion_notify(event_t *ev)
{
    buttonbar_t *bbar;
    button_t *b;
    Window unused_root, unused_child;
    int unused_root_x, unused_root_y;
    unsigned int mask;

    D_EVENTS(("bbar_handle_motion_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &buttonbar->event_data), 0);

    if (!(bbar = find_bbar_by_window(ev->xany.window))) {
        return 0;
    }
    while (XCheckTypedWindowEvent(Xdisplay, ev->xany.window, MotionNotify, ev));
    XQueryPointer(Xdisplay, bbar->win, &unused_root, &unused_child, &unused_root_x, &unused_root_y, &(ev->xbutton.x),
                  &(ev->xbutton.y), &mask);
    D_BBAR((" -> Pointer is at %d, %d with mask 0x%08x\n", ev->xbutton.x, ev->xbutton.y, mask));

    b = find_button_by_coords(bbar, ev->xbutton.x, ev->xbutton.y);
    if (b != bbar->current) {
        if (bbar->current) {
            bbar_deselect_button(bbar, bbar->current);
        }
        if (b) {
            if (mask & (Button1Mask | Button2Mask | Button3Mask)) {
                bbar_click_button(bbar, b);
            } else {
                bbar_select_button(bbar, b);
            }
        }
    }

    return 1;
}

unsigned char
bbar_dispatch_event(event_t *ev)
{
    if (buttonbar->event_data.handlers[ev->type]) {
        return ((buttonbar->event_data.handlers[ev->type]) (ev));
    }
    return (0);
}

buttonbar_t *find_bbar_by_window(Window win)
{
    buttonbar_t *bbar;

    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        if (bbar->win == win) {
            return bbar;
        }
    }
    return ((buttonbar_t *) NULL);
}

void
bbar_add(buttonbar_t *bbar)
{
    if (buttonbar) {
        buttonbar_t *bb;

        for (bb = buttonbar; bb->next; bb = bb->next);
        bb->next = bbar;
    } else {
        buttonbar = bbar;
    }
    bbar->next = NULL;
    bbar_reset_total_height();
    event_data_add_mywin(&buttonbar->event_data, bbar->win);
}

unsigned short
bbar_calc_height(buttonbar_t *bbar)
{
    button_t *b;
    Imlib_Border *bbord, *bord;

    D_BBAR(("bbar_calc_height(%8p):  font ascent == %d, font descent == %d, h == %d\n",
            bbar, bbar->font->ascent, bbar->font->descent, bbar->h));

    if (image_mode_is(image_bbar, MODE_MASK)) {
        bbord = images[image_bbar].norm->iml->border;
    } else if (images[image_bbar].norm->iml->bevel) {
        bbord = images[image_bbar].norm->iml->bevel->edges;
    } else {
        bbord = NULL;
    }
    if (image_mode_is(image_button, MODE_MASK)) {
        bord = images[image_button].norm->iml->border;
    } else if (images[image_button].norm->iml->bevel) {
        bord = images[image_button].norm->iml->bevel->edges;
    } else {
        bord = NULL;
    }

    bbar->h = bbar->fheight + 1;
    if (bord) {
        bbar->h += bord->top + bord->bottom;
    }

    for (b = bbar->buttons; b; b = b->next) {
        if (b->h != bbar->h) {
            b->h = bbar->h;
            button_calc_size(bbar, b);
        }
    }
    for (b = bbar->rbuttons; b; b = b->next) {
        if (b->h != bbar->h) {
            b->h = bbar->h;
            button_calc_size(bbar, b);
        }
    }
    if (bbord) {
        bbar->h += bbord->top + bbord->bottom;
    }
    D_BBAR(("Final height is %d\n", bbar->h));
    return bbar->h;
}

void
bbar_calc_button_sizes(buttonbar_t *bbar)
{
    button_t *b;

    D_BBAR(("bbar == %8p\n", bbar));

    for (b = bbar->buttons; b; b = b->next) {
        button_calc_size(bbar, b);
    }
    for (b = bbar->rbuttons; b; b = b->next) {
        button_calc_size(bbar, b);
    }
}

void
bbar_calc_button_positions(buttonbar_t *bbar)
{
    button_t *b;
    unsigned short x, y;
    Imlib_Border *border;

    D_BBAR(("bbar == %8p\n", bbar));

    if (image_mode_is(image_bbar, MODE_MASK)) {
        border = images[image_bbar].norm->iml->border;
    } else if (images[image_bbar].norm->iml->bevel) {
        border = images[image_bbar].norm->iml->bevel->edges;
    } else {
        border = NULL;
    }

    y = ((border) ? (border->top) : 0);
    if (bbar->buttons) {
        x = ((border) ? (border->left) : 0) + MENU_HGAP;
        for (b = bbar->buttons; b; b = b->next) {
            b->x = x;
            b->y = y;
            D_BBAR(("Set button \"%s\" (%8p, width %d) to coordinates %d, %d\n", b->text, b, b->w, x, y));
            x += b->w + MENU_HGAP;
            button_calc_rel_coords(bbar, b);
        }
    }
    if (bbar->rbuttons) {
        x = bbar->w - ((border) ? (border->right) : 0);
        for (b = bbar->rbuttons; b; b = b->next) {
            x -= b->w + MENU_HGAP;
            b->x = x;
            b->y = y;
            button_calc_rel_coords(bbar, b);
            D_BBAR(("Set rbutton \"%s\" (%8p, width %d) to coordinates %d, %d\n", b->text, b, b->w, x, y));
        }
    }
}

void
button_calc_size(buttonbar_t *bbar, button_t *button)
{
    Imlib_Border *bord;
    int ascent, descent, direction;
    XCharStruct chars;

    D_BBAR(("button_calc_size(%8p, %8p):  XTextExtents(%8p, %s, %d, ...)\n", bbar, button, bbar->font, button->text, button->len));

    if (image_mode_is(image_button, MODE_MASK)) {
        bord = images[image_button].norm->iml->border;
    } else if (images[image_button].norm->iml->bevel) {
        bord = images[image_button].norm->iml->bevel->edges;
    } else {
        bord = NULL;
    }

    button->w = 0;
    if (button->len) {
        XTextExtents(bbar->font, button->text, button->len, &direction, &ascent, &descent, &chars);
        button->w += chars.width;
    }
    if (bord) {
        button->w += bord->left + bord->right;
    }
    if (button->h == 0) {
        button->h = bbar->font->ascent + bbar->font->descent + 1;
        if (bord) {
            button->h += bord->top + bord->bottom;
        }
    }
#ifdef PIXMAP_SUPPORT
    if (button->icon) {
        unsigned short b;

        if (bord) {
            b = button->h - bord->top - bord->bottom;
        } else {
            b = button->h;
        }
        imlib_context_set_image(button->icon->iml->im);
        button->icon_w = imlib_image_get_width();
        button->icon_h = imlib_image_get_height();
        D_BBAR((" -> Initial icon dimensions are %hux%hu\n", button->icon_w, button->icon_h));
        if (button->icon_h > b) {
            button->icon_w = (unsigned short) ((float) button->icon_w / button->icon_h * b);
            button->icon_h = b;
        }
        button->w += button->icon_w;
        if (button->len) {
            button->w += MENU_HGAP;
        }
        D_BBAR((" -> Final icon dimensions are %hux%hu\n", button->icon_w, button->icon_h));
    }
#endif
    D_BBAR((" -> Set button to %dx%d at %d, %d and icon to %dx%d\n", button->w, button->h, button->x, button->y, button->icon_w,
            button->icon_h));
}

void
button_calc_rel_coords(buttonbar_t *bbar, button_t *button)
{
    Imlib_Border *bord;

    D_BBAR(("bbar == %8p, button == %8p\n", bbar, button));

    if (image_mode_is(image_button, MODE_MASK)) {
        bord = images[image_button].norm->iml->border;
    } else if (images[image_button].norm->iml->bevel) {
        bord = images[image_button].norm->iml->bevel->edges;
    } else {
        bord = NULL;
    }

#ifdef PIXMAP_SUPPORT
    if (button->icon) {
        unsigned short b = 0;

        if (bord) {
            b = button->h - bord->top - bord->bottom - 2;
        }
        if (button->icon_h == button->h) {
            button->icon_y = button->y + ((bord) ? (bord->top) : 0);
        } else {
            button->icon_y = button->y + ((b - button->icon_h) / 2) + ((bord) ? (bord->top) : 0);
        }
        button->icon_x = button->x + ((bord) ? (bord->left) : 0);
    }
#endif

    if (button->len) {
        button->text_x = button->x + ((button->icon_w) ? (button->icon_w + MENU_HGAP) : 0) + ((bord) ? (bord->left) : (0));
        button->text_y = button->y + button->h - ((bord) ? (bord->bottom) : (0)) - bbar->font->descent;
    }
    D_BBAR((" -> Text is at %d, %d and icon is at %d, %d\n", button->text_x, button->text_y, button->icon_x, button->icon_y));
}

void
bbar_add_button(buttonbar_t *bbar, button_t *button)
{
    button_t *b;

    D_BBAR(("bbar_add_button(%8p, %8p):  Adding button \"%s\".\n", bbar, button, button->text));

    ASSERT(bbar != NULL);

    if (bbar->buttons) {
        for (b = bbar->buttons; b->next; b = b->next);
        b->next = button;
    } else {
        bbar->buttons = button;
    }
    button->next = NULL;
}

void
bbar_add_rbutton(buttonbar_t *bbar, button_t *button)
{
    button_t *b;

    D_BBAR(("bbar_add_rbutton(%8p, %8p):  Adding right-justified button \"%s\".\n", bbar, button, button->text));

    b = ((bbar->rbuttons) ? (bbar->rbuttons) : NULL);
    bbar->rbuttons = button;
    button->next = b;
}

unsigned char
bbar_set_font(buttonbar_t *bbar, const char *fontname)
{
    XFontStruct *font;

    ASSERT_RVAL(fontname != NULL, 0);

    D_BBAR(("bbar_set_font(%8p, \"%s\"):  Current font is %8p, dimensions %d/%d/%d\n", bbar, fontname, bbar->font, bbar->fwidth,
            bbar->fheight, bbar->h));
    if (bbar->font) {
        free_font(bbar->font);
    }
#ifdef MULTI_CHARSET
    if (bbar->fontset) {
        XFreeFontSet(Xdisplay, bbar->fontset);
    }
#endif

    font = (XFontStruct *) load_font(fontname, "fixed", FONT_TYPE_X);
#ifdef MULTI_CHARSET
    bbar->fontset = create_fontset(fontname, etmfonts[def_font_idx]);
#endif

    bbar->font = font;
    bbar->fwidth = font->max_bounds.width;
    bbar->fheight = font->ascent + font->descent;
    XSetFont(Xdisplay, bbar->gc, font->fid);
    bbar_reset_total_height();
    D_BBAR(("Font is \"%s\" (0x%08x).  New dimensions are %d/%d/%d\n", NONULL(fontname), font, bbar->fwidth, bbar->fheight,
            bbar->h));

    bbar_calc_height(bbar);
    return 1;
}

button_t *find_button_by_text(buttonbar_t *bbar, char *text)
{
    register button_t *b;

    REQUIRE_RVAL(text != NULL, NULL);

    for (b = bbar->buttons; b; b = b->next) {
        if (!strcasecmp(b->text, text)) {
            return (b);
        }
    }
    for (b = bbar->rbuttons; b; b = b->next) {
        if (!strcasecmp(b->text, text)) {
            return (b);
        }
    }
    return NULL;
}

button_t *find_button_by_index(buttonbar_t *bbar, long idx)
{
    register button_t *b;
    long i;

    if (idx < 0) {
        idx = -idx;
        b = bbar->rbuttons;
    } else {
        b = bbar->buttons;
    }
    for (i = 0; (b) && (i < idx); b = b->next, i++);
    return ((i == idx) ? (b) : (NULL));
}

button_t *find_button_by_coords(buttonbar_t *bbar, int x, int y)
{
    register button_t *b;

    ASSERT_RVAL(bbar != NULL, NULL);

    for (b = bbar->buttons; b; b = b->next) {
        if ((x >= b->x) && (y >= b->y) && (x < b->x + b->w) && (y < b->y + b->h)) {
            return (b);
        }
    }
    for (b = bbar->rbuttons; b; b = b->next) {
        if ((x >= b->x) && (y >= b->y) && (x < b->x + b->w) && (y < b->y + b->h)) {
            return (b);
        }
    }
    return NULL;
}

button_t *button_create(char *text)
{
    button_t *button;

    button = (button_t *) MALLOC(sizeof(button_t));
    MEMSET(button, 0, sizeof(button_t));

    if (text) {
        button->text = STRDUP(text);
        button->len = strlen(text);
    } else {
        button->text = STRDUP("");
        button->len = 0;
    }
    return button;
}

void
button_free(button_t *button)
{
    if (button->next) {
        button_free(button->next);
    }
    if (button->text) {
        FREE(button->text);
    }
    if (button->type == ACTION_STRING || button->type == ACTION_ECHO) {
        FREE(button->action.string);
    }
    if (button->icon) {
        free_simage(button->icon);
    }
    FREE(button);
}

unsigned char
button_set_text(button_t *button, const char *text)
{
    ASSERT_RVAL(button != NULL, 0);

    if (button->text) {
        FREE(button->text);
    }
    if (text) {
        button->text = STRDUP(text);
        button->len = strlen(text);
    } else {
        button->text = STRDUP("");
        button->len = 0;
    }
    return 1;
}

unsigned char
button_set_icon(button_t *button, simage_t *icon)
{
    ASSERT_RVAL(button != NULL, 0);
    ASSERT_RVAL(icon != NULL, 0);

    button->icon = icon;
    return 1;
}

unsigned char
button_set_action(button_t *button, action_type_t type, char *action)
{
    ASSERT_RVAL(button != NULL, 0);

    button->type = type;
    switch (type) {
        case ACTION_MENU:
            button->action.menu = find_menu_by_title(menu_list, action);
            return ((!button->action.menu) ? (0) : (1));
            break;
        case ACTION_STRING:
        case ACTION_ECHO:
            button->action.string = (char *) MALLOC(strlen(action) + 2);
            strcpy(button->action.string, action);
            parse_escaped_string(button->action.string);
            return ((!button->action.string) ? (0) : (1));
            break;
        case ACTION_SCRIPT:
            button->action.script = (char *) MALLOC(strlen(action) + 2);
            strcpy(button->action.script, action);
            return ((!button->action.script) ? (0) : (1));
            break;
        default:
            break;
    }
    return 0;
}

void
bbar_select_button(buttonbar_t *bbar, button_t *button)
{
    bbar->current = button;
    if (image_mode_is(image_button, MODE_MASK)) {
        paste_simage(images[image_button].selected, image_button, bbar->win, bbar->win, button->x, button->y, button->w, button->h);
    } else {
        Pixel p1, p2;

        p1 = get_top_shadow_color(images[image_button].selected->bg, "");
        p2 = get_bottom_shadow_color(images[image_button].selected->bg, "");
        XSetForeground(Xdisplay, bbar->gc, images[image_button].selected->bg);
        XFillRectangle(Xdisplay, bbar->win, bbar->gc, button->x, button->y, button->w, button->h);
        draw_shadow_from_colors(bbar->win, p1, p2, button->x, button->y, button->w, button->h, 2);
    }
    if (image_mode_is(image_button, MODE_AUTO)) {
        enl_ipc_sync();
    }
    if (button->icon) {
        paste_simage(button->icon, image_max, bbar->win, bbar->win, button->icon_x, button->icon_y, button->icon_w, button->icon_h);
    }
    if (button->len) {
        XSetForeground(Xdisplay, bbar->gc, images[image_bbar].selected->fg);
        draw_string(bbar, bbar->win, bbar->gc, button->text_x, button->text_y, button->text, button->len);
        XSetForeground(Xdisplay, bbar->gc, images[image_bbar].norm->fg);
    }
}

void
bbar_deselect_button(buttonbar_t *bbar, button_t *button)
{
    XClearArea(Xdisplay, bbar->win, button->x, button->y, button->w, button->h, False);
    bbar->current = NULL;
}

void
bbar_click_button(buttonbar_t *bbar, button_t *button)
{
    REQUIRE(button != NULL);

    D_BBAR(("Drawing clicked button %8p (%s) on buttonbar %8p\n", button, NONULL(button->text), bbar));

    bbar->current = button;
    if (image_mode_is(image_button, MODE_MASK)) {
        paste_simage(images[image_button].clicked, image_button, bbar->win, bbar->win, button->x, button->y, button->w, button->h);
    } else {
        draw_shadow_from_colors(bbar->win, PixColors[menuBottomShadowColor], PixColors[menuTopShadowColor], button->x, button->y,
                                button->w, button->h, 2);
    }
    if (image_mode_is(image_button, MODE_AUTO)) {
        enl_ipc_sync();
    }
    if (button->icon) {
        paste_simage(button->icon, image_max, bbar->win, bbar->win, button->icon_x, button->icon_y, button->icon_w, button->icon_h);
    }
    if (button->len) {
        XSetForeground(Xdisplay, bbar->gc, images[image_bbar].clicked->fg);
        draw_string(bbar, bbar->win, bbar->gc, button->text_x, button->text_y, button->text, button->len);
        XSetForeground(Xdisplay, bbar->gc, images[image_bbar].norm->fg);
    }
}

void
button_check_action(buttonbar_t *bbar, button_t *button, unsigned char press, Time t)
{
    static unsigned char prvs = 0;

    REQUIRE(button != NULL);

    D_BBAR(("Checking action for button %8p (%s) on buttonbar %8p, press %d, prvs %d, time %lu\n", button, NONULL(button->text),
            bbar, (int) press, (int) prvs, (unsigned long) t));

    switch (button->type) {
        case ACTION_MENU:
            D_BBAR((" -> Menu button found.\n"));
            if (press) {
                menu_invoke(button->x, button->y + button->h, bbar->win, button->action.menu, t);
            }
            break;
        case ACTION_STRING:
            D_BBAR((" -> String button found.\n"));
            if (!press) {
                size_t len;

                len = strlen(button->action.string);
                D_BBAR(("Writing \"%s\" to command buffer.\n", safe_print_string(button->action.string, len)));
                cmd_write((unsigned char *) button->action.string, strlen(button->action.string));
            }
            break;
        case ACTION_ECHO:
            D_BBAR((" -> Echo button found.\n"));
            if (!press) {
                size_t len;

#ifdef ESCREEN
                if (TermWin.screen && TermWin.screen->backend) {        /* translate escapes */
                    button_t *b = bbar->buttons;
                    _ns_disp *d2 = TermWin.screen->dsps;
                    int n = (button->action.string)[1] - '0';

                    if (b && (b->flags & NS_SCREAM_BUTTON)) {
                        D_ESCREEN(("Looking for active display, n == %d, press == %d, prvs == %d\n", n, (int) press, (int) prvs));
                        if (prvs != 1) {
                            /* find active disp */
                            for (; b && !(b->flags & NS_SCREAM_CURR); b = b->next);

                            if (b && b != button) {
                                D_ESCREEN((" -> Found button %8p (%s) for current display.\n", b, NONULL(b->text)));

                                /* when trying to change name of non- */
                                /* active display, make that disp active */
                                button->flags |= NS_SCREAM_CURR;
                                b->flags &= ~NS_SCREAM_CURR;
                                bbar_draw(bbar, IMAGE_STATE_CURRENT, MODE_MASK);
                                button->flags &= ~NS_SCREAM_CURR;
                                b->flags |= NS_SCREAM_CURR;

                                for (; d2 && d2->index != n; d2 = d2->next);
                                if (d2) {
                                    /* pre-adjust curr ptr */
                                    TermWin.screen->curr = d2;
                                } else {
                                    D_ESCREEN(("no display %d in this session : (\n", n));
                                }
                                ns_go2_disp(TermWin.screen, n);
                            }

                            if (prvs == 2) {
                                /* middle button -- kill */
                                D_ESCREEN((" -> Remove display %d\n", n));
                                ns_rem_disp(TermWin.screen, n, TRUE);
                            } else {
                                /* right button -- rename */
                                D_ESCREEN((" -> Rename display %d\n", n));
                                ns_ren_disp(TermWin.screen, n, NULL);
                            }
                        } else {
                            /* left button -- select */
                            D_ESCREEN((" -> Go to display %d\n", n));
                            ns_go2_disp(TermWin.screen, n);
                        }
                        break;
                    } else {
                        D_ESCREEN(("Non-screen button, handling normally.\n"));
                    }
                }
#endif

                /* not in screen-mode, use normal facilities */
                len = strlen(button->action.string);
                D_BBAR(("Writing \"%s\" to subprocess.\n", safe_print_string(button->action.string, len)));
                tt_write((unsigned char *) button->action.string, len);
            }
            break;
        case ACTION_SCRIPT:
            D_BBAR((" -> Script button found.\n"));
            if (!press) {
                script_parse((char *) button->action.script);
            }
            break;
        default:
            D_BBAR((" -> Unknown button type 0x%08x?!\n", button->type));
            break;
    }
    prvs = press;
}

unsigned char
bbar_show(buttonbar_t *bbar, unsigned char visible)
{
    unsigned char changed = 0;

    D_BBAR(("bbar_show(%8p, %d) called.\n", bbar, visible));
    if (visible && !bbar_is_visible(bbar)) {
        D_BBAR((" -> Making bbar visible.\n"));
        bbar_set_visible(bbar, 1);
        XMapWindow(Xdisplay, bbar->win);
        bbar_draw(bbar, IMAGE_STATE_CURRENT, MODE_MASK);
        changed = 1;
    } else if (!visible && bbar_is_visible(bbar)) {
        D_BBAR((" -> Making bbar invisible.\n"));
        bbar_set_visible(bbar, 0);
        XUnmapWindow(Xdisplay, bbar->win);
        changed = 1;
    }
    return changed;
}

void
bbar_show_all(signed char visible)
{
    buttonbar_t *bbar;

    D_BBAR(("visible == %d\n", (int) visible));
    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        bbar_show(bbar, ((visible == -1) ? (!bbar_is_visible(bbar)) : visible));
    }
}

void
bbar_resize(buttonbar_t *bbar, int w)
{
    D_BBAR(("bbar_resize(%8p, %d) called.\n", bbar, w));
    if ((w >= 0) && !bbar_is_visible(bbar)) {
        D_BBAR((" -> Buttonbar is not visible, returning."));
        return;
    }
    if (w < 0) {
        bbar_calc_button_sizes(bbar);
        bbar_calc_height(bbar);
        bbar_reset_total_height();
        w = -w;
    }
    if (bbar->w != w) {
        bbar->w = w;
        bbar_calc_button_positions(bbar);
        D_BBAR(("Resizing window 0x%08x to %dx%d\n", bbar->win, bbar->w, bbar->h));
        XResizeWindow(Xdisplay, bbar->win, bbar->w, bbar->h);
        bbar_draw(bbar, IMAGE_STATE_CURRENT, MODE_MASK);
    }
}

void
bbar_resize_all(int width)
{
    buttonbar_t *bbar;

    D_BBAR(("width == %d\n", width));
    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        bbar_resize(bbar, width);
    }
    bbar_calc_positions();
}

void
bbar_draw(buttonbar_t *bbar, unsigned char image_state, unsigned char force_modes)
{
    button_t *button;

    ASSERT(bbar != NULL);

    D_BBAR(("bbar_draw(%8p, 0x%02x, 0x%02x) called.\n", bbar, image_state, force_modes));
    if (image_state != IMAGE_STATE_CURRENT) {
        if ((image_state == IMAGE_STATE_NORMAL) && (bbar->image_state != IMAGE_STATE_NORMAL)) {
            images[image_bbar].current = images[image_bbar].norm;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_SELECTED) && (bbar->image_state != IMAGE_STATE_SELECTED)) {
            images[image_bbar].current = images[image_bbar].selected;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_CLICKED) && (bbar->image_state != IMAGE_STATE_CLICKED)) {
            images[image_bbar].current = images[image_bbar].clicked;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_DISABLED) && (bbar->image_state != IMAGE_STATE_DISABLED)) {
            images[image_bbar].current = images[image_bbar].disabled;
            force_modes = MODE_MASK;
        }
    }
    if (image_mode_is(image_bbar, MODE_MASK) && !((images[image_bbar].mode & MODE_MASK) & (force_modes))) {
        return;
    } else if (!bbar_is_visible(bbar)) {
        return;
    } else {
        render_simage(images[image_bbar].current, bbar->win, bbar->w, bbar->h, image_bbar, RENDER_FORCE_PIXMAP);
        bbar->bg = images[image_bbar].current->pmap->pixmap;
        REQUIRE(bbar->bg != None);
    }
    XSetForeground(Xdisplay, bbar->gc, images[image_bbar].current->fg);
    for (button = bbar->buttons; button; button = button->next) {
        if (button->icon) {
            paste_simage(button->icon, image_max, bbar->win, bbar->bg, button->icon_x, button->icon_y, button->icon_w,
                         button->icon_h);
        }
        if (button->len) {
#ifdef ESCREEN
            /* evil temporary hack */
            int f = button->flags & ~NS_SCREAM_BUTTON;

            if (f & NS_SCREAM_CURR) {
                f = ES_COLOR_CURRENT;
            } else if (f & NS_SCREAM_ACT) {
                f = ES_COLOR_ACTIVE;
            } else {
                f = 0;
            }

            D_BBAR(("bbar_draw: text \"%s\", color %d.\n", button->text, f));
            if (f) {
                GC gc;

                gc = LIBAST_X_CREATE_GC(0, NULL);
                XCopyGC(Xdisplay, bbar->gc, GCFont, gc);
                XSetForeground(Xdisplay, gc, PixColors[f]);

                draw_string(bbar, bbar->bg, gc, button->text_x, button->text_y, button->text, button->len);
                LIBAST_X_FREE_GC(gc);
            } else
#endif
                draw_string(bbar, bbar->bg, bbar->gc, button->text_x, button->text_y, button->text, button->len);
        }
    }
    for (button = bbar->rbuttons; button; button = button->next) {
        if (button->icon) {
            paste_simage(button->icon, image_max, bbar->win, bbar->bg, button->icon_x, button->icon_y, button->icon_w,
                         button->icon_h);
        }
        if (button->len) {
            draw_string(bbar, bbar->bg, bbar->gc, button->text_x, button->text_y, button->text, button->len);
        }
    }
    XSetWindowBackgroundPixmap(Xdisplay, bbar->win, bbar->bg);
    XClearWindow(Xdisplay, bbar->win);
    XSetForeground(Xdisplay, bbar->gc, images[image_bbar].norm->fg);
    if (bbar->current) {
        bbar_select_button(bbar, bbar->current);
    }
}

void
bbar_draw_all(unsigned char image_state, unsigned char force_modes)
{
    buttonbar_t *bbar;

    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        bbar_draw(bbar, image_state, force_modes);
    }
}

void
bbar_dock(buttonbar_t *bbar, unsigned char dock)
{
    D_BBAR(("bbar_dock(%8p, %d) called.\n", bbar, dock));
    if (dock == BBAR_DOCKED_TOP) {
        bbar_set_docked(bbar, BBAR_DOCKED_TOP);
        bbar_calc_positions();
    } else if (dock == BBAR_DOCKED_BOTTOM) {
        bbar_set_docked(bbar, BBAR_DOCKED_BOTTOM);
        bbar_calc_positions();
    } else {
        bbar_set_docked(bbar, 0);
        bbar_calc_positions();
        XReparentWindow(Xdisplay, bbar->win, Xroot, bbar->x, bbar->y);
        XMoveResizeWindow(Xdisplay, bbar->win, bbar->x, bbar->y, bbar->w, bbar->h);
    }
}

void
bbar_calc_positions(void)
{
    register buttonbar_t *bbar;
    unsigned short top_y, bottom_y;

    top_y = 0;
    bottom_y = szHint.height;
    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        if (!bbar_is_visible(bbar) || !bbar_is_docked(bbar)) {
            D_BBAR(("Skipping invisible/undocked buttonbar %8p\n", bbar));
            continue;
        }

        D_BBAR(("top_y %lu, bottom_y %lu\n", top_y, bottom_y));
        bbar->x = 0;
        if (bbar_is_bottom_docked(bbar)) {
            bottom_y = bottom_y - bbar->h;
            bbar->y = bottom_y;
        } else {
            bbar->y = top_y;
            top_y += bbar->h;
        }
        D_BBAR(("Set coordinates for buttonbar %8p (window 0x%08x) to %lu, %lu\n", bbar, bbar->win, bbar->x, bbar->y));
        if (TermWin.parent != None) {
            XReparentWindow(Xdisplay, bbar->win, TermWin.parent, bbar->x, bbar->y);
            XMoveResizeWindow(Xdisplay, bbar->win, bbar->x, bbar->y, bbar->w, bbar->h);
        }
    }
}

unsigned long
bbar_calc_total_height(void)
{
    register buttonbar_t *bbar;

    bbar_total_h = 0;
    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        if (bbar_is_visible(bbar)) {
            bbar_total_h += bbar->h;
        }
    }
    D_BBAR(("Height of all visible buttonbars:  %lu\n", bbar_total_h));
    return bbar_total_h;
}

unsigned long
bbar_calc_docked_height(register unsigned char dock_flag)
{
    register buttonbar_t *bbar;
    register unsigned long h = 0;

    for (bbar = buttonbar; bbar; bbar = bbar->next) {
        if ((bbar->state & dock_flag) && bbar_is_visible(bbar)) {
            h += bbar->h;
        }
    }
    D_BBAR(("Height of buttonbars with dock state 0x%02x:  %lu\n", (unsigned) dock_flag, h));
    return h;
}

/* redraw a button bar */
void
bbar_redraw(buttonbar_t *bbar)
{
    bbar_calc_height(bbar);
    bbar_calc_button_sizes(bbar);
    bbar_calc_button_positions(bbar);
    bbar_draw(bbar, IMAGE_STATE_CURRENT, MODE_MASK);
}
