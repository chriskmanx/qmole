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

static const char cvs_ident[] = "$Id: scrollbar.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <X11/cursorfont.h>

#include "buttons.h"
#include "command.h"
#include "draw.h"
#include "e.h"
#include "events.h"
#include "startup.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "scrollbar.h"
#include "term.h"
#include "windows.h"

event_dispatcher_data_t scrollbar_event_data;
scrollbar_t scrollbar = {
    None, None, None, None,
    0, 1,
    0, 1,
    0,
    SCROLLBAR_DEFAULT_TYPE,
    0,
    SHADOW,
    SB_WIDTH, 0,
    0, 0,
    0, 0
};

#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
short scroll_arrow_delay;
#endif
static GC gc_scrollbar;
static short last_top = 0, last_bot = 0;

#ifdef XTERM_SCROLLBAR
static GC gc_stipple, gc_border;
static unsigned char xterm_sb_bits[] = { 0xaa, 0x0a, 0x55, 0x05 };      /* 12x2 bitmap */
#endif
#if defined(MOTIF_SCROLLBAR) || defined(NEXT_SCROLLBAR)
static GC gc_top, gc_bottom;
#endif /* MOTIF_SCROLLBAR || NEXT_SCROLLBAR */

void
scrollbar_event_init_dispatcher(void)
{

    MEMSET(&scrollbar_event_data, 0, sizeof(event_dispatcher_data_t));

    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, EnterNotify, sb_handle_enter_notify);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, LeaveNotify, sb_handle_leave_notify);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, FocusIn, sb_handle_focus_in);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, FocusOut, sb_handle_focus_out);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, GraphicsExpose, sb_handle_expose);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, Expose, sb_handle_expose);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, ButtonPress, sb_handle_button_press);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, ButtonRelease, sb_handle_button_release);
    EVENT_DATA_ADD_HANDLER(scrollbar_event_data, MotionNotify, sb_handle_motion_notify);

    event_data_add_mywin(&scrollbar_event_data, scrollbar.win);
    event_data_add_mywin(&scrollbar_event_data, scrollbar.up_win);
    event_data_add_mywin(&scrollbar_event_data, scrollbar.dn_win);
    event_data_add_mywin(&scrollbar_event_data, scrollbar.sa_win);

    event_data_add_parent(&scrollbar_event_data, TermWin.vt);
    event_data_add_parent(&scrollbar_event_data, TermWin.parent);

}

unsigned char
sb_handle_enter_notify(event_t *ev)
{

    D_EVENTS(("sb_handle_enter_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);

    if (scrollbar_win_is_uparrow(ev->xany.window)) {
        scrollbar_draw_uparrow(IMAGE_STATE_SELECTED, 0);
    } else if (scrollbar_win_is_downarrow(ev->xany.window)) {
        scrollbar_draw_downarrow(IMAGE_STATE_SELECTED, 0);
    } else if (scrollbar_win_is_anchor(ev->xany.window)) {
        scrollbar_draw_anchor(IMAGE_STATE_SELECTED, 0);
    } else if (scrollbar_win_is_trough(ev->xany.window)) {
        scrollbar_draw_trough(IMAGE_STATE_SELECTED, 0);
    }
    return 1;
}

unsigned char
sb_handle_leave_notify(event_t *ev)
{

    D_EVENTS(("sb_handle_leave_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);

    if (scrollbar_win_is_uparrow(ev->xany.window)) {
        scrollbar_draw_uparrow(IMAGE_STATE_NORMAL, 0);
    } else if (scrollbar_win_is_downarrow(ev->xany.window)) {
        scrollbar_draw_downarrow(IMAGE_STATE_NORMAL, 0);
    } else if (scrollbar_win_is_anchor(ev->xany.window)) {
        scrollbar_draw_anchor(IMAGE_STATE_NORMAL, 0);
    } else if (scrollbar_win_is_trough(ev->xany.window)) {
        scrollbar_draw_trough(IMAGE_STATE_NORMAL, 0);
    }
    return 1;
}

unsigned char
sb_handle_focus_in(event_t *ev)
{

    D_EVENTS(("sb_handle_focus_in(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);

    return 1;
}

unsigned char
sb_handle_focus_out(event_t *ev)
{

    D_EVENTS(("sb_handle_focus_out(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);

    return 1;
}

unsigned char
sb_handle_expose(event_t *ev)
{

    XEvent unused_xevent;

    D_EVENTS(("sb_handle_expose(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);

    while (XCheckTypedWindowEvent(Xdisplay, ev->xany.window, Expose, &unused_xevent));
    while (XCheckTypedWindowEvent(Xdisplay, ev->xany.window, GraphicsExpose, &unused_xevent));

    if (scrollbar_win_is_uparrow(ev->xany.window)) {
        scrollbar_draw_uparrow(IMAGE_STATE_CURRENT, 0);
    } else if (scrollbar_win_is_downarrow(ev->xany.window)) {
        scrollbar_draw_downarrow(IMAGE_STATE_CURRENT, 0);
    } else if (scrollbar_win_is_anchor(ev->xany.window)) {
        scrollbar_draw_anchor(IMAGE_STATE_CURRENT, 0);
    } else if (scrollbar_win_is_trough(ev->xany.window)) {
        scrollbar_draw_trough(IMAGE_STATE_CURRENT, 0);
    }
    return 1;
}

unsigned char
sb_handle_button_press(event_t *ev)
{

    D_EVENTS(("sb_handle_button_press(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);

    button_state.bypass_keystate = (ev->xbutton.state & (Mod1Mask | ShiftMask));
    button_state.report_mode = (button_state.bypass_keystate ? 0 : ((PrivateModes & PrivMode_mouse_report) ? 1 : 0));
    scrollbar_cancel_motion();

#ifndef NO_SCROLLBAR_REPORT
    if (button_state.report_mode) {
        /* Mouse report disabled scrollbar.  Arrows send cursor key up/down, trough sends pageup/pagedown */
        if (scrollbar_win_is_uparrow(ev->xany.window))
            tt_printf((unsigned char *) "\033[A");
        else if (scrollbar_win_is_downarrow(ev->xany.window))
            tt_printf((unsigned char *) "\033[B");
        else {
            switch (ev->xbutton.button) {
                case Button2:
                    tt_printf((unsigned char *) "\014");
                    break;
                case Button1:
                    tt_printf((unsigned char *) "\033[6~");
                    break;
                case Button3:
                    tt_printf((unsigned char *) "\033[5~");
                    break;
            }
        }
    } else
#endif /* NO_SCROLLBAR_REPORT */
    {
        D_EVENTS(("ButtonPress event for window 0x%08x at %d, %d\n", ev->xany.window, ev->xbutton.x, ev->xbutton.y));
        D_EVENTS(("  up [0x%08x], down [0x%08x], anchor [0x%08x], trough [0x%08x]\n", scrollbar.up_win, scrollbar.dn_win,
                  scrollbar.sa_win, scrollbar.win));

        if (scrollbar_win_is_uparrow(ev->xany.window)) {
            scrollbar_draw_uparrow(IMAGE_STATE_CLICKED, 0);
#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
            scroll_arrow_delay = SCROLLBAR_INITIAL_DELAY;
#endif
            if (scr_page(UP, 1)) {
                scrollbar_set_uparrow_pressed(1);
            }
        } else if (scrollbar_win_is_downarrow(ev->xany.window)) {
            scrollbar_draw_downarrow(IMAGE_STATE_CLICKED, 0);
#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
            scroll_arrow_delay = SCROLLBAR_INITIAL_DELAY;
#endif
            if (scr_page(DN, 1)) {
                scrollbar_set_downarrow_pressed(1);
            }
        } else {
            if (scrollbar_win_is_anchor(ev->xany.window)) {
                scrollbar_set_anchor_pressed(1);
                scrollbar_draw_anchor(IMAGE_STATE_CLICKED, 0);
            }
            switch (ev->xbutton.button) {
                case Button2:
                    button_state.mouse_offset = scrollbar_anchor_height() / 2;  /* Align to center */
                    if (!scrollbar_win_is_anchor(ev->xany.window)) {
                        scr_move_to(scrollbar_position(ev->xbutton.y) - button_state.mouse_offset, scrollbar_scrollarea_height());
                    } else if (scrollbar.type == SCROLLBAR_XTERM) {
                        scr_move_to(scrollbar.anchor_top + ev->xbutton.y - button_state.mouse_offset,
                                    scrollbar_scrollarea_height());
                    }
                    scrollbar_set_motion(1);
                    break;

                case Button1:
                    button_state.mouse_offset = ((scrollbar_win_is_anchor(ev->xany.window)) ? (MAX(ev->xbutton.y, 1)) : (1));
                    /* drop */
                case Button3:
#if defined(MOTIF_SCROLLBAR) || defined(NEXT_SCROLLBAR)
                    if (scrollbar.type == SCROLLBAR_MOTIF || scrollbar.type == SCROLLBAR_NEXT) {
                        if (scrollbar_is_above_anchor(ev->xany.window, ev->xbutton.y)) {
                            scrollbar_draw_trough(IMAGE_STATE_CLICKED, 0);
                            scr_page(UP, TermWin.nrow - CONTEXT_LINES);
                        } else if (scrollbar_is_below_anchor(ev->xany.window, ev->xbutton.y)) {
                            scrollbar_draw_trough(IMAGE_STATE_CLICKED, 0);
                            scr_page(DN, TermWin.nrow - CONTEXT_LINES);
                        } else {
                            scrollbar_set_motion(1);
                        }
                    }
#endif /* MOTIF_SCROLLBAR || NEXT_SCROLLBAR */

#ifdef XTERM_SCROLLBAR
                    if (scrollbar.type == SCROLLBAR_XTERM) {
                        scr_page((ev->xbutton.button == Button1 ? DN : UP), TermWin.nrow - CONTEXT_LINES);
                    }
#endif /* XTERM_SCROLLBAR */
                    break;
            }
        }
    }

    return 1;
}

unsigned char
sb_handle_button_release(event_t *ev)
{

    Window root, child;
    int root_x, root_y, win_x, win_y;
    unsigned int mask;

    D_EVENTS(("sb_handle_button_release(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);
    button_state.mouse_offset = 0;
    button_state.report_mode = (button_state.bypass_keystate ? 0 : ((PrivateModes & PrivMode_mouse_report) ? 1 : 0));

    XQueryPointer(Xdisplay, scrollbar_get_win(), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
    scrollbar_cancel_motion();
    if (scrollbar_win_is_uparrow(child)) {
        scrollbar_draw_uparrow(IMAGE_STATE_SELECTED, 0);
    } else {
        scrollbar_draw_uparrow(IMAGE_STATE_NORMAL, 0);
    }
    if (scrollbar_win_is_downarrow(child)) {
        scrollbar_draw_downarrow(IMAGE_STATE_SELECTED, 0);
    } else {
        scrollbar_draw_downarrow(IMAGE_STATE_NORMAL, 0);
    }
    if (scrollbar_win_is_anchor(child)) {
        scrollbar_draw_anchor(IMAGE_STATE_SELECTED, 0);
    } else {
        scrollbar_draw_anchor(IMAGE_STATE_NORMAL, 0);
    }
    if (scrollbar_win_is_trough(child)) {
        scrollbar_draw_trough(IMAGE_STATE_SELECTED, 0);
    } else {
        scrollbar_draw_trough(IMAGE_STATE_NORMAL, 0);
    }
    return 1;
}

unsigned char
sb_handle_motion_notify(event_t *ev)
{

    D_EVENTS(("sb_handle_motion_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &scrollbar_event_data), 0);
    if ((PrivateModes & PrivMode_mouse_report) && !(button_state.bypass_keystate))
        return 1;

    D_EVENTS(("MotionNotify event for window 0x%08x\n", ev->xany.window));
    D_EVENTS(("  up [0x%08x], down [0x%08x], anchor [0x%08x], trough [0x%08x]\n", scrollbar.up_win, scrollbar.dn_win,
              scrollbar.sa_win, scrollbar.win));

    if ((scrollbar_win_is_trough(ev->xany.window) || scrollbar_win_is_anchor(ev->xany.window)) && scrollbar_is_moving()) {
        Window unused_root, unused_child;
        int unused_root_x, unused_root_y;
        unsigned int unused_mask;

        while (XCheckTypedWindowEvent(Xdisplay, scrollbar.win, MotionNotify, ev));
        XQueryPointer(Xdisplay, scrollbar.win, &unused_root, &unused_child, &unused_root_x, &unused_root_y, &(ev->xbutton.x),
                      &(ev->xbutton.y), &unused_mask);
        scr_move_to(scrollbar_position(ev->xbutton.y) - button_state.mouse_offset, scrollbar_scrollarea_height());
        refresh_count = refresh_limit = 0;
        scr_refresh(refresh_type);
        scrollbar_anchor_update_position(button_state.mouse_offset);
    }
    return 1;
}

unsigned char
scrollbar_dispatch_event(event_t *ev)
{
    if (scrollbar_event_data.handlers[ev->type]) {
        return ((scrollbar_event_data.handlers[ev->type]) (ev));
    }
    return (0);
}

/******************************************************************************/

void
scrollbar_draw_uparrow(unsigned char image_state, unsigned char force_modes)
{

    D_SCROLLBAR(("scrollbar_draw_uparrow(%u, 0x%02x)\n", (unsigned int) image_state, (unsigned int) force_modes));
    if (image_state != IMAGE_STATE_CURRENT) {
        if ((image_state == IMAGE_STATE_NORMAL) && (images[image_up].current != images[image_up].norm)) {
            images[image_up].current = images[image_up].norm;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_SELECTED) && (images[image_up].current != images[image_up].selected)) {
            images[image_up].current = images[image_up].selected;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_CLICKED) && (images[image_up].current != images[image_up].clicked)) {
            images[image_up].current = images[image_up].clicked;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_DISABLED) && (images[image_up].current != images[image_up].disabled)) {
            images[image_up].current = images[image_up].disabled;
            force_modes = MODE_MASK;
        }
    }
    if (!image_mode_is(image_up, MODE_MASK)) {
        /* Solid mode.  Redraw every time since it's cheap. */
        if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING)) {
            XSetWindowBackground(Xdisplay, scrollbar.up_win, PixColors[bgColor]);
            XClearWindow(Xdisplay, scrollbar.up_win);
        } else {
            XSetForeground(Xdisplay, gc_scrollbar, images[image_up].current->bg);
            XFillRectangle(Xdisplay, scrollbar.up_win, gc_scrollbar, 0, 0, scrollbar_arrow_width(), scrollbar_arrow_height());
        }
        XSetForeground(Xdisplay, gc_top, get_top_shadow_color(images[image_up].current->bg, ""));
        XSetForeground(Xdisplay, gc_bottom, get_bottom_shadow_color(images[image_up].current->bg, ""));
        if (image_state == IMAGE_STATE_CLICKED) {
            scrollbar_set_uparrow_pressed(1);
            draw_uparrow_clicked(scrollbar.up_win, gc_top, gc_bottom, 0, 0, scrollbar_arrow_width() - 1, scrollbar_get_shadow());
        } else {
            scrollbar_set_uparrow_pressed(0);
            draw_uparrow_raised(scrollbar.up_win, gc_top, gc_bottom, 0, 0, scrollbar_arrow_width() - 1, scrollbar_get_shadow());
        }
        return;
    }
    if (!((images[image_up].mode & MODE_MASK) & (force_modes))) {
        return;
    }
    render_simage(images[image_up].current, scrollbar.up_win, scrollbar_arrow_width(), scrollbar_arrow_height(), image_up, 0);
}

unsigned char
scrollbar_move_uparrow(void)
{
    static int last_x = -1, last_y = -1, last_w = -1, last_h = -1;
    int x, y, w, h;

    D_SCROLLBAR(("scrollbar_move_uparrow()\n"));
    x = scrollbar_get_shadow();
    y = scrollbar_up_loc();
    w = scrollbar_arrow_width();
    h = scrollbar_arrow_height();
    if ((last_x == x) && (last_y == y) && (last_w == w) && (last_h == h)) {
        D_SCROLLBAR((" -> No move required, returning 0.\n"));
        return 0;
    }
    D_SCROLLBAR((" -> XMoveResizeWindow(Xdisplay, 0x%08x, %d, %d, %d, %d)\n", scrollbar.up_win, x, y, w, h));
    XMoveResizeWindow(Xdisplay, scrollbar.up_win, x, y, w, h);
    last_x = x;
    last_y = y;
    last_w = w;
    last_h = h;
    return 1;
}

void
scrollbar_draw_downarrow(unsigned char image_state, unsigned char force_modes)
{

    D_SCROLLBAR(("scrollbar_draw_downarrow(%u, 0x%02x)\n", (unsigned int) image_state, (unsigned int) force_modes));
    if (image_state != IMAGE_STATE_CURRENT) {
        if ((image_state == IMAGE_STATE_NORMAL) && (images[image_down].current != images[image_down].norm)) {
            images[image_down].current = images[image_down].norm;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_SELECTED) && (images[image_down].current != images[image_down].selected)) {
            images[image_down].current = images[image_down].selected;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_CLICKED) && (images[image_down].current != images[image_down].clicked)) {
            images[image_down].current = images[image_down].clicked;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_DISABLED) && (images[image_down].current != images[image_down].disabled)) {
            images[image_down].current = images[image_down].disabled;
            force_modes = MODE_MASK;
        }
    }
    if (!image_mode_is(image_down, MODE_MASK)) {
        /* Solid mode.  Redraw every time since it's cheap. */
        if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING)) {
            XSetWindowBackground(Xdisplay, scrollbar.dn_win, PixColors[bgColor]);
            XClearWindow(Xdisplay, scrollbar.dn_win);
        } else {
            XSetForeground(Xdisplay, gc_scrollbar, images[image_down].current->bg);
            XFillRectangle(Xdisplay, scrollbar.dn_win, gc_scrollbar, 0, 0, scrollbar_arrow_width(), scrollbar_arrow_height());
        }
        XSetForeground(Xdisplay, gc_top, get_top_shadow_color(images[image_down].current->bg, ""));
        XSetForeground(Xdisplay, gc_bottom, get_bottom_shadow_color(images[image_down].current->bg, ""));
        if (image_state == IMAGE_STATE_CLICKED) {
            scrollbar_set_downarrow_pressed(1);
            draw_downarrow_clicked(scrollbar.dn_win, gc_top, gc_bottom, 0, 0, scrollbar_arrow_width() - 1, scrollbar_get_shadow());
        } else {
            scrollbar_set_downarrow_pressed(0);
            draw_downarrow_raised(scrollbar.dn_win, gc_top, gc_bottom, 0, 0, scrollbar_arrow_width() - 1, scrollbar_get_shadow());
        }
        return;
    }
    if (!((images[image_down].mode & MODE_MASK) & (force_modes))) {
        return;
    }
    render_simage(images[image_down].current, scrollbar.dn_win, scrollbar_arrow_width(), scrollbar_arrow_height(), image_down, 0);
}

unsigned char
scrollbar_move_downarrow(void)
{
    static int last_x = -1, last_y = -1, last_w = -1, last_h = -1;
    int x, y, w, h;

    D_SCROLLBAR(("scrollbar_move_downarrow()\n"));
    x = scrollbar_get_shadow();
    y = scrollbar_dn_loc();
    w = scrollbar_arrow_width();
    h = scrollbar_arrow_height();
    if ((last_x == x) && (last_y == y) && (last_w == w) && (last_h == h)) {
        D_SCROLLBAR((" -> No move required, returning 0.\n"));
        return 0;
    }
    D_SCROLLBAR((" -> XMoveResizeWindow(Xdisplay, 0x%08x, %d, %d, %d, %d)\n", scrollbar.dn_win, x, y, w, h));
    XMoveResizeWindow(Xdisplay, scrollbar.dn_win, x, y, w, h);
    last_x = x;
    last_y = y;
    last_w = w;
    last_h = h;
    return 1;
}

void
scrollbar_draw_anchor(unsigned char image_state, unsigned char force_modes)
{

    D_SCROLLBAR(("scrollbar_draw_anchor(%u, 0x%02x)\n", (unsigned int) image_state, (unsigned int) force_modes));
    if (image_state != IMAGE_STATE_CURRENT) {
        if ((image_state == IMAGE_STATE_NORMAL) && (images[image_sa].current != images[image_sa].norm)) {
            images[image_sa].current = images[image_sa].norm;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_SELECTED) && (images[image_sa].current != images[image_sa].selected)) {
            images[image_sa].current = images[image_sa].selected;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_CLICKED) && (images[image_sa].current != images[image_sa].clicked)) {
            images[image_sa].current = images[image_sa].clicked;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_DISABLED) && (images[image_sa].current != images[image_sa].disabled)) {
            images[image_sa].current = images[image_sa].disabled;
            force_modes = MODE_MASK;
        }
        if ((image_state == IMAGE_STATE_NORMAL) && (images[image_st].current != images[image_st].norm)) {
            images[image_st].current = images[image_st].norm;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_SELECTED) && (images[image_st].current != images[image_st].selected)) {
            images[image_st].current = images[image_st].selected;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_CLICKED) && (images[image_st].current != images[image_st].clicked)) {
            images[image_st].current = images[image_st].clicked;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_DISABLED) && (images[image_st].current != images[image_st].disabled)) {
            images[image_st].current = images[image_st].disabled;
            force_modes = MODE_MASK;
        }
    }
    if (!image_mode_is(image_sa, MODE_MASK)) {
        /* Solid mode.  Redraw every time since it's cheap. */
#ifdef XTERM_SCROLLBAR
        if (scrollbar.type == SCROLLBAR_XTERM) {
            int x = ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)) ? 1 : 0);

            XSetForeground(Xdisplay, gc_stipple, images[image_sa].current->bg);
            XFillRectangle(Xdisplay, scrollbar.sa_win, gc_stipple, x + 1, 0, scrollbar_anchor_width() - x - 1,
                           scrollbar_anchor_height());
            XClearWindow(Xdisplay, scrollbar.sa_win);
        }
#endif /* XTERM_SCROLLBAR */
#if defined(MOTIF_SCROLLBAR) || defined(NEXT_SCROLLBAR)
        if (scrollbar.type == SCROLLBAR_MOTIF || scrollbar.type == SCROLLBAR_NEXT) {
            if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING)) {
                XSetWindowBackground(Xdisplay, scrollbar.sa_win, PixColors[bgColor]);
                XClearWindow(Xdisplay, scrollbar.sa_win);
            } else {
                XSetForeground(Xdisplay, gc_scrollbar, images[image_sa].current->bg);
                XFillRectangle(Xdisplay, scrollbar.sa_win, gc_scrollbar, 0, 0, scrollbar_anchor_width(), scrollbar_anchor_height());
            }
            XSetForeground(Xdisplay, gc_top, get_top_shadow_color(images[image_sa].current->bg, ""));
            XSetForeground(Xdisplay, gc_bottom, get_bottom_shadow_color(images[image_sa].current->bg, ""));
            if (scrollbar_anchor_is_pressed()) {
                draw_shadow(scrollbar.sa_win, gc_bottom, gc_top, 0, 0, scrollbar_anchor_width(), scrollbar_anchor_height(),
                            scrollbar_get_shadow());
            } else {
                draw_shadow(scrollbar.sa_win, gc_top, gc_bottom, 0, 0, scrollbar_anchor_width(), scrollbar_anchor_height(),
                            scrollbar_get_shadow());
            }
        }
#endif
        return;
    }
    if (!((images[image_sa].mode & MODE_MASK) & (force_modes))) {
        return;
    }
    if (scrollbar_anchor_height() > 1) {
        unsigned char thumb;
        Pixmap pmap;

        thumb = (images[image_st].current->iml) ? 1 : 0;
        render_simage(images[image_sa].current, scrollbar.sa_win, scrollbar_anchor_width(), scrollbar_anchor_height(), image_sa,
                      thumb);
        pmap = images[image_sa].current->pmap->pixmap;
        /* Draw the thumb if there is one. */
        if (thumb) {
            unsigned short tw = 0, th = 0;
            imlib_t *iml = images[image_st].current->iml, *siml = images[image_sa].current->iml;

            if (image_mode_is(image_st, MODE_IMAGE) && iml->im) {
#ifdef PIXMAP_SUPPORT
                imlib_context_set_image(iml->im);
                tw = imlib_image_get_width();
                th = imlib_image_get_height();
#endif
            } else if (siml->bevel) {
                tw = scrollbar_anchor_width() - (siml->bevel->edges->left + siml->bevel->edges->right);
                th = scrollbar_anchor_width() - (siml->bevel->edges->top + siml->bevel->edges->bottom);
            } else if (siml->border) {
                tw = scrollbar_anchor_width() - (siml->border->left + siml->border->right);
                th = scrollbar_anchor_width() - (siml->border->top + siml->border->bottom);
            } else if (iml->bevel) {
                tw = iml->bevel->edges->left + iml->bevel->edges->right + 4;
                th = iml->bevel->edges->top + iml->bevel->edges->bottom + 4;
            }
            UPPER_BOUND(tw, scrollbar_anchor_width());
            UPPER_BOUND(th, scrollbar_anchor_height() >> 1);
            D_SCROLLBAR(("Thumb width/height has been calculated at %hux%hu.\n", tw, th));
            if ((tw > 0) && (th > 0)) {
                paste_simage(images[image_st].current, image_st, scrollbar.sa_win, pmap, (scrollbar_anchor_width() - tw) >> 1,
                             (scrollbar_anchor_height() - th) >> 1, tw, th);
                XSetWindowBackgroundPixmap(Xdisplay, scrollbar.sa_win, pmap);
                XClearWindow(Xdisplay, scrollbar.sa_win);
                IMLIB_FREE_PIXMAP(pmap);
                images[image_sa].current->pmap->pixmap = None;
            }
        }
    }
}

unsigned char
scrollbar_move_anchor(void)
{
    static int last_x = -1, last_y = -1, last_w = -1, last_h = -1;
    int x, y, w, h;

    D_SCROLLBAR(("Last values:  %d, %d, %d, %d\n", last_x, last_y, last_w, last_h));
    x = scrollbar_get_shadow();
    y = scrollbar.anchor_top;
    w = scrollbar_anchor_width();
    h = scrollbar_anchor_height();
    if ((last_x == x) && (last_y == y) && (last_w == w) && (last_h == h)) {
        D_SCROLLBAR((" -> No move required, returning 0.\n"));
        return 0;
    }
    D_SCROLLBAR((" -> XMoveResizeWindow(Xdisplay, 0x%08x, %d, %d, %d, %d)\n", scrollbar.sa_win, x, y, w, h));
    XMoveResizeWindow(Xdisplay, scrollbar.sa_win, x, y, w, h);
    last_x = x;
    last_y = y;
    last_w = w;
    last_h = h;
    return 1;
}

void
scrollbar_draw_trough(unsigned char image_state, unsigned char force_modes)
{

    D_SCROLLBAR(("scrollbar_draw_trough(%u, 0x%02x)\n", (unsigned int) image_state, (unsigned int) force_modes));
    if (image_state != IMAGE_STATE_CURRENT) {
        if ((image_state == IMAGE_STATE_NORMAL) && (images[image_sb].current != images[image_sb].norm)) {
            images[image_sb].current = images[image_sb].norm;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_SELECTED) && (images[image_sb].current != images[image_sb].selected)) {
            images[image_sb].current = images[image_sb].selected;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_CLICKED) && (images[image_sb].current != images[image_sb].clicked)) {
            images[image_sb].current = images[image_sb].clicked;
            force_modes = MODE_MASK;
        } else if ((image_state == IMAGE_STATE_DISABLED) && (images[image_sb].current != images[image_sb].disabled)) {
            images[image_sb].current = images[image_sb].disabled;
            force_modes = MODE_MASK;
        }
    }
    if (!image_mode_is(image_sb, MODE_MASK)) {
        /* Solid mode.  Redraw every time since it's cheap. */
        if ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING)) || (scrollbar.type == SCROLLBAR_XTERM)) {
            XSetWindowBackground(Xdisplay, scrollbar.win, PixColors[bgColor]);
            XClearWindow(Xdisplay, scrollbar.win);
        } else {
            XSetForeground(Xdisplay, gc_scrollbar, images[image_sb].current->bg);
            XFillRectangle(Xdisplay, scrollbar.win, gc_scrollbar, 0, 0, scrollbar_trough_width(), scrollbar_trough_height());
            XSetForeground(Xdisplay, gc_top, get_top_shadow_color(images[image_sb].current->bg, ""));
            XSetForeground(Xdisplay, gc_bottom, get_bottom_shadow_color(images[image_sb].current->bg, ""));
            draw_shadow(scrollbar.win, gc_bottom, gc_top, 0, 0, scrollbar_trough_width(), scrollbar_trough_height(),
                        scrollbar_get_shadow());
        }
        return;
    }
    if (!((images[image_sb].mode & MODE_MASK) & (force_modes))) {
        return;
    }
    render_simage(images[image_sb].current, scrollbar.win, scrollbar_trough_width(), scrollbar_trough_height(), image_sb, 0);
}

void
scrollbar_init(int width, int height)
{

    Cursor cursor;
    long mask;

    D_SCROLLBAR(("Initializing all scrollbar elements.\n"));

    Attributes.background_pixel = images[image_sb].norm->bg;
    Attributes.border_pixel = images[image_sb].norm->bg;
    Attributes.override_redirect = TRUE;
    Attributes.save_under = TRUE;
    cursor = XCreateFontCursor(Xdisplay, XC_left_ptr);
    mask = KeyPressMask | ExposureMask | EnterWindowMask | LeaveWindowMask | ButtonPressMask | ButtonReleaseMask
        | Button1MotionMask | Button2MotionMask | Button3MotionMask;
    scrollbar_calc_size(width, height);
    scrollbar.anchor_top = scrollbar.scrollarea_start;
    scrollbar.anchor_bottom = scrollbar.scrollarea_end;

    /* Create the scrollbar trough window.  It will be the parent to the other windows. */
    scrollbar.win =
        XCreateWindow(Xdisplay, TermWin.parent,
                      ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)) ? (width - scrollbar_trough_width()) : (0)),
                      bbar_calc_docked_height(BBAR_DOCKED_TOP), scrollbar_trough_width(), height, 0, Xdepth, InputOutput,
                      CopyFromParent, CWOverrideRedirect | CWBackPixel | CWBorderPixel | CWColormap, &Attributes);
    XDefineCursor(Xdisplay, scrollbar.win, cursor);
    XSelectInput(Xdisplay, scrollbar.win, mask);
    XStoreName(Xdisplay, scrollbar.win, "Eterm Scrollbar");
    D_SCROLLBAR(("Created scrollbar window 0x%08x\n", scrollbar.win));

    /* Now the up arrow window. */
    scrollbar.up_win =
        XCreateWindow(Xdisplay, scrollbar.win, scrollbar_get_shadow(), scrollbar_up_loc(), scrollbar_arrow_width(),
                      scrollbar_arrow_height(), 0, Xdepth, InputOutput, CopyFromParent, CWOverrideRedirect | CWColormap,
                      &Attributes);
    XSelectInput(Xdisplay, scrollbar.up_win, mask);
    XStoreName(Xdisplay, scrollbar.up_win, "Eterm Scrollbar Up Arrow");
    D_SCROLLBAR(("Created scrollbar up arrow window 0x%08x\n", scrollbar.up_win));

    /* The down arrow window */
    scrollbar.dn_win =
        XCreateWindow(Xdisplay, scrollbar.win, scrollbar_get_shadow(), scrollbar_dn_loc(), scrollbar_arrow_width(),
                      scrollbar_arrow_height(), 0, Xdepth, InputOutput, CopyFromParent, CWOverrideRedirect | CWColormap,
                      &Attributes);
    XSelectInput(Xdisplay, scrollbar.dn_win, mask);
    XStoreName(Xdisplay, scrollbar.up_win, "Eterm Scrollbar Down Arrow");
    D_SCROLLBAR(("Created scrollbar down arrow window 0x%08x\n", scrollbar.dn_win));

    /* The anchor window */
    scrollbar.sa_win =
        XCreateWindow(Xdisplay, scrollbar.win, scrollbar_get_shadow(), scrollbar.anchor_top, scrollbar_anchor_width(),
                      scrollbar_anchor_height(), 0, Xdepth, InputOutput, CopyFromParent,
                      CWOverrideRedirect | CWSaveUnder | CWColormap, &Attributes);
    XSelectInput(Xdisplay, scrollbar.sa_win, mask);
    XMapWindow(Xdisplay, scrollbar.sa_win);
    XStoreName(Xdisplay, scrollbar.up_win, "Eterm Scrollbar Anchor");
    D_SCROLLBAR(("Created scrollbar anchor window 0x%08x\n", scrollbar.sa_win));

    if (scrollbar_get_type() != SCROLLBAR_XTERM) {
        scrollbar_map_arrows();
    }
    event_register_dispatcher(scrollbar_dispatch_event, scrollbar_event_init_dispatcher);

    scrollbar_drawing_init();
    scrollbar_draw(IMAGE_STATE_CURRENT, MODE_MASK);
}

unsigned char
scrollbar_mapping(unsigned char show)
{

    unsigned char change = 0;

    D_SCROLLBAR(("scrollbar_mapping(%d)\n", show));

    if (show && !scrollbar_is_visible()) {
        D_SCROLLBAR((" -> Mapping scrollbar window.  Returning 1.\n"));
        scrollbar_set_visible(1);
        XMapWindow(Xdisplay, scrollbar.win);
        change = 1;
    } else if (!show && scrollbar_is_visible()) {
        D_SCROLLBAR((" -> Unmapping scrollbar window.  Returning 1.\n"));
        scrollbar_set_visible(0);
        XUnmapWindow(Xdisplay, scrollbar.win);
        change = 1;
    } else {
        D_SCROLLBAR((" -> No action required.  Returning 0.\n"));
    }
    return change;
}

void
scrollbar_reset(void)
{
    D_SCROLLBAR(("scrollbar_reset()\n"));
    last_top = last_bot = 0;
    scrollbar.init = 0;
}

void
scrollbar_calc_size(int width, int height)
{
    D_SCROLLBAR(("scrollbar_calc_size(%d, %d), type == %u\n", width, height, scrollbar_get_type()));
#ifdef ESCREEN
    if (TermWin.screen && TermWin.screen->backend != NS_MODE_NONE) {
        UPPER_BOUND(height, Height2Pixel(TermWin.nrow));
    }
#endif
    scrollbar.scrollarea_start = 0;
    scrollbar.scrollarea_end = height;
    scrollbar.up_arrow_loc = 0;
    scrollbar.down_arrow_loc = 0;
#ifdef MOTIF_SCROLLBAR
    if (scrollbar.type == SCROLLBAR_MOTIF) {
        /* arrows are as high as wide - leave 1 pixel gap */
        scrollbar.scrollarea_start += scrollbar_arrow_height() + scrollbar_get_shadow() + 1;
        scrollbar.scrollarea_end -= scrollbar_arrow_height() + scrollbar_get_shadow() + 1;
        scrollbar.up_arrow_loc = scrollbar_get_shadow();
        scrollbar.down_arrow_loc = scrollbar.scrollarea_end + 1;
    }
#endif
#ifdef NEXT_SCROLLBAR
    if (scrollbar.type == SCROLLBAR_NEXT) {
        scrollbar.scrollarea_start = scrollbar_get_shadow();
        scrollbar.scrollarea_end -= (scrollbar.width * 2 + (scrollbar_get_shadow()? scrollbar_get_shadow() : 1) + 2);
        scrollbar.up_arrow_loc = scrollbar.scrollarea_end + 1;
        scrollbar.down_arrow_loc = scrollbar.scrollarea_end + scrollbar.width + 2;
    }
#endif
    scrollbar.height = height - (2 * scrollbar_get_shadow());
    scrollbar.win_width = scrollbar.width + (2 * scrollbar_get_shadow());
    scrollbar.win_height = height;
    D_X11((" -> New scrollbar width/height == %hux%hu, win_width/height == %hux%hu\n", scrollbar.width, scrollbar.height,
           scrollbar.win_width, scrollbar.win_height));
    D_X11((" -> New scroll area start/end == %hu - %hu, up_arrow_loc == %hu, down_arrow_loc == %hu\n", scrollbar.scrollarea_start,
           scrollbar.scrollarea_end, scrollbar.up_arrow_loc, scrollbar.down_arrow_loc));
}

void
scrollbar_resize(int width, int height)
{
    if (!scrollbar_is_visible()) {
        return;
    }

    D_SCROLLBAR(("scrollbar_resize(%d, %d)\n", width, height));
    scrollbar_calc_size(width, height);
    D_SCROLLBAR((" -> XMoveResizeWindow(Xdisplay, 0x%08x, %d, y, %d, %d)\n", scrollbar.win,
                 ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)) ? (width - scrollbar_trough_width()) : (0)),
                 scrollbar_trough_width(), scrollbar_trough_height()));
    XMoveResizeWindow(Xdisplay, scrollbar.win,
                      ((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)) ? (width - scrollbar_trough_width()) : (0)),
                      bbar_calc_docked_height(BBAR_DOCKED_TOP), scrollbar_trough_width(), scrollbar_trough_height());
    scrollbar_draw_trough(IMAGE_STATE_CURRENT, MODE_MASK);
    scrollbar_reposition_and_draw(MODE_MASK);
    scrollbar.init = 0;
}

void
scrollbar_change_type(unsigned int type)
{
    D_SCROLLBAR(("scrollbar_change_type(0x%02x):  Current scrollbar type is 0x%02x\n", type, scrollbar_get_type()));
    if (scrollbar_get_type() == type) {
        /* Nothing to do. */
        return;
    }
#ifdef XTERM_SCROLLBAR
    if (scrollbar.type == SCROLLBAR_XTERM) {
        scrollbar_map_arrows();
    }
#endif
#ifdef MOTIF_SCROLLBAR
    if (scrollbar.type == SCROLLBAR_MOTIF) {
        /* arrows are as high as wide - leave 1 pixel gap */
        scrollbar.scrollarea_start -= scrollbar_arrow_height() + scrollbar_get_shadow() + 1;
        scrollbar.scrollarea_end += scrollbar_arrow_height() + scrollbar_get_shadow() + 1;
    }
#endif
#ifdef NEXT_SCROLLBAR
    if (scrollbar.type == SCROLLBAR_NEXT) {
        scrollbar.scrollarea_start = 0;
        scrollbar.scrollarea_end += (scrollbar.width * 2 + (scrollbar_get_shadow()? scrollbar_get_shadow() : 1) + 2);
    }
#endif

    scrollbar_reset();
    scrollbar.type = type;

#ifdef XTERM_SCROLLBAR
    if (scrollbar.type == SCROLLBAR_XTERM) {
        scrollbar_unmap_arrows();
    }
#endif
#ifdef MOTIF_SCROLLBAR
    if (type == SCROLLBAR_MOTIF) {
        scrollbar.scrollarea_start += scrollbar_arrow_height() + scrollbar_get_shadow() + 1;
        scrollbar.scrollarea_end -= scrollbar_arrow_height() + scrollbar_get_shadow() + 1;
        scrollbar.up_arrow_loc = scrollbar_get_shadow();
        scrollbar.down_arrow_loc = scrollbar.scrollarea_end + 1;
    }
#endif
#ifdef NEXT_SCROLLBAR
    if (type == SCROLLBAR_NEXT) {
        scrollbar.scrollarea_start = scrollbar_get_shadow();
        scrollbar.scrollarea_end -= (scrollbar.width * 2 + (scrollbar_get_shadow()? scrollbar_get_shadow() : 1) + 2);
        scrollbar.up_arrow_loc = scrollbar.scrollarea_end + 1;
        scrollbar.down_arrow_loc = scrollbar.scrollarea_end + scrollbar.width + 2;
    }
#endif
    scrollbar_reposition_and_draw(MODE_MASK);
}

void
scrollbar_change_width(unsigned short width)
{
    D_SCROLLBAR(("scrollbar_change_width(%hu):  Current width is %hu\n", width, scrollbar_get_width()));
    if (width == 0) {
        width = SB_WIDTH;
    }
    if (width == scrollbar.width) {
        /* Nothing to do, so return. */
        return;
    }
    scrollbar_reset();
    scrollbar.width = width;
    parent_resize();
}

void
scrollbar_drawing_init(void)
{

    XGCValues gcvalue;

    D_SCROLLBAR(("Called.\n"));
#ifdef XTERM_SCROLLBAR
    gcvalue.stipple = XCreateBitmapFromData(Xdisplay, scrollbar.win, (char *) xterm_sb_bits, 12, 2);
    if (!gcvalue.stipple) {
        libast_print_error("Unable to create xterm scrollbar bitmap.\n\n");
        if (scrollbar_get_type() == SCROLLBAR_XTERM) {
            scrollbar_set_type(SCROLLBAR_MOTIF);
        }
    } else {
        gcvalue.fill_style = FillOpaqueStippled;
        gcvalue.foreground = PixColors[fgColor];
        gcvalue.background = PixColors[bgColor];
        gc_stipple = LIBAST_X_CREATE_GC(GCForeground | GCBackground | GCFillStyle | GCStipple, &gcvalue);
        gcvalue.foreground = PixColors[borderColor];
        gc_border = LIBAST_X_CREATE_GC(GCForeground, &gcvalue);
    }
#endif /* XTERM_SCROLLBAR */

#if defined(MOTIF_SCROLLBAR) || defined(NEXT_SCROLLBAR)
    gcvalue.foreground = images[image_sb].norm->bg;
    gc_scrollbar = LIBAST_X_CREATE_GC(GCForeground, &gcvalue);
    gcvalue.foreground = PixColors[topShadowColor];
    gc_top = LIBAST_X_CREATE_GC(GCForeground, &gcvalue);
    gcvalue.foreground = PixColors[bottomShadowColor];
    gc_bottom = LIBAST_X_CREATE_GC(GCForeground, &gcvalue);
#endif /* MOTIF_SCROLLBAR || NEXT_SCROLLBAR */
}

unsigned char
scrollbar_set_focus(short has_focus)
{

    static short focus = -1;
    XGCValues gcvalue;

    D_SCROLLBAR(("scrollbar_set_focus(%hd):  focus == %hd\n", has_focus, focus));
    if (focus != has_focus) {
        focus = has_focus;
        gcvalue.foreground = (focus ? (images[image_sb].norm->bg) : (images[image_sb].disabled->bg));
        XChangeGC(Xdisplay, gc_scrollbar, GCForeground, &gcvalue);
        gcvalue.foreground = PixColors[focus ? topShadowColor : unfocusedTopShadowColor];
        XChangeGC(Xdisplay, gc_top, GCForeground, &gcvalue);
        gcvalue.foreground = PixColors[focus ? bottomShadowColor : unfocusedBottomShadowColor];
        XChangeGC(Xdisplay, gc_bottom, GCForeground, &gcvalue);
        return (1);
    }
    return (0);
}

unsigned char
scrollbar_anchor_update_position(short mouseoffset)
{

    int top = (TermWin.nscrolled - TermWin.view_start);
    int bot = top + (TermWin.nrow - 1);
    int len = MAX((TermWin.nscrolled + (TermWin.nrow - 1)), 1);

    D_SCROLLBAR(("scrollbar_anchor_update_position(%hd):  top == %d, bot == %d, len == %d\n", mouseoffset, top, bot, len));
    scrollbar.anchor_top = (scrollbar.scrollarea_start + (top * scrollbar_scrollarea_height()) / len);
    scrollbar.anchor_bottom = (scrollbar.scrollarea_start + (bot * scrollbar_scrollarea_height()) / len);

    if (rs_min_anchor_size && scrollbar.type != SCROLLBAR_XTERM) {
        if ((scrollbar_scrollarea_height() > rs_min_anchor_size) && (scrollbar_anchor_height() < rs_min_anchor_size)) {

            int grab_point = scrollbar.anchor_top + mouseoffset;

            if (grab_point - mouseoffset < scrollbar.scrollarea_start) {
                scrollbar.anchor_top = scrollbar.scrollarea_start;
                scrollbar.anchor_bottom = rs_min_anchor_size + scrollbar.scrollarea_start;
            } else if (scrollbar.anchor_top + rs_min_anchor_size > scrollbar.scrollarea_end) {
                scrollbar.anchor_top = scrollbar.scrollarea_end - rs_min_anchor_size;
                scrollbar.anchor_bottom = scrollbar.scrollarea_end;
            } else {
                scrollbar.anchor_top = grab_point - mouseoffset;
                scrollbar.anchor_bottom = scrollbar.anchor_top + rs_min_anchor_size;
            }
            if (scrollbar.anchor_bottom == scrollbar.scrollarea_end) {
                scr_move_to(scrollbar.scrollarea_end, scrollbar_scrollarea_height());
                scr_refresh(DEFAULT_REFRESH);
            }
        }
    }
    if ((scrollbar.anchor_top == last_top) && (scrollbar.anchor_bottom == last_bot) && (scrollbar.init)) {
        return 0;
    }
    if (scrollbar_move_anchor()) {
        scrollbar_draw_anchor(IMAGE_STATE_CURRENT, MODE_MASK);
    }

    last_top = scrollbar.anchor_top;
    last_bot = scrollbar.anchor_bottom;
    return 1;
}

void
scrollbar_draw(unsigned char image_state, unsigned char force_modes)
{
    D_SCROLLBAR(("scrollbar_draw(%d, 0x%02x)\n", image_state, force_modes));
    scrollbar_draw_trough(image_state, force_modes);
    scrollbar_draw_anchor(image_state, force_modes);
    scrollbar_draw_uparrow(image_state, force_modes);
    scrollbar_draw_downarrow(image_state, force_modes);
    scrollbar.init = 1;
}

void
scrollbar_reposition_and_draw(unsigned char force_modes)
{
    D_SCROLLBAR(("scrollbar_reposition_and_draw(0x%02x)\n", force_modes));
    if (scrollbar_move_uparrow()) {
        scrollbar_draw_uparrow(IMAGE_STATE_CURRENT, force_modes);
    }
    if (scrollbar_move_downarrow()) {
        scrollbar_draw_downarrow(IMAGE_STATE_CURRENT, force_modes);
    }
    if (!scrollbar_anchor_update_position(1)) {
        scrollbar_draw_anchor(IMAGE_STATE_CURRENT, force_modes);
    }
    scrollbar.init = 1;
}

void
scrollbar_reposition_and_always_draw(void)
{
    D_SCROLLBAR(("scrollbar_reposition_and_always_draw()\n"));
    scrollbar_draw_trough(IMAGE_STATE_CURRENT, MODE_MASK);
    scrollbar_move_uparrow();
    scrollbar_draw_uparrow(IMAGE_STATE_CURRENT, MODE_MASK);
    scrollbar_move_downarrow();
    scrollbar_draw_downarrow(IMAGE_STATE_CURRENT, MODE_MASK);
    scrollbar_anchor_update_position(1);
    scrollbar_draw_anchor(IMAGE_STATE_CURRENT, MODE_MASK);
    scrollbar.init = 1;
}

unsigned char
scrollbar_show(short mouseoffset)
{
    unsigned char force_update = 0;

    if (!scrollbar_is_visible()) {
        return 0;
    }

    D_SCROLLBAR(("scrollbar_show(%hd)\n", mouseoffset));

    force_update = scrollbar_set_focus(TermWin.focus);
    if (!(scrollbar.init)) {
        force_update++;
    }
    if (mouseoffset) {
        force_update += scrollbar_anchor_update_position(mouseoffset);
    }
    scrollbar_draw_trough(IMAGE_STATE_CURRENT, (force_update) ? (MODE_TRANS | MODE_VIEWPORT) : (MODE_MASK));
    scrollbar_draw_uparrow(IMAGE_STATE_CURRENT, (force_update) ? (MODE_TRANS | MODE_VIEWPORT) : (MODE_MASK));
    scrollbar_draw_downarrow(IMAGE_STATE_CURRENT, (force_update) ? (MODE_TRANS | MODE_VIEWPORT) : (MODE_MASK));
    scrollbar.init = 1;
    return 1;
}
