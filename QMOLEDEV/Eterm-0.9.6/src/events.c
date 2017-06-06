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

static const char cvs_ident[] = "$Id: events.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>

#include "startup.h"
#include "actions.h"
#include "buttons.h"
#include "command.h"
#include "e.h"
#include "events.h"
#include "font.h"
#include "menus.h"
#include "options.h"
#include "pixmap.h"
#include "profile.h"
#include "screen.h"
#include "scrollbar.h"
#include "term.h"
#include "windows.h"
#ifdef ESCREEN
#  include "screamcfg.h"
#endif

unsigned char paused = 0;
event_master_t event_master;
event_dispatcher_data_t primary_data;
mouse_button_state_t button_state = { 0, 0, 0, 0, 0, 0, 0, 0 };

/********** The Event Handling Subsystem **********/
void
event_init_subsystem(event_dispatcher_t primary_dispatcher, event_dispatcher_init_t init)
{

    /* Initialize the subsystem with the main event dispatcher */
    event_master.num_dispatchers = 1;
    event_master.dispatchers = (event_dispatcher_t *) MALLOC(sizeof(event_dispatcher_t));
    event_master.dispatchers[0] = (event_dispatcher_t) primary_dispatcher;
    (init) ();                  /* Initialize the dispatcher's data */
}

void
event_register_dispatcher(event_dispatcher_t func, event_dispatcher_init_t init)
{

    /* Add a secondary event dispatcher */
    event_master.num_dispatchers++;
    event_master.dispatchers =
        (event_dispatcher_t *) REALLOC(event_master.dispatchers, sizeof(event_dispatcher_t) * event_master.num_dispatchers);
    event_master.dispatchers[event_master.num_dispatchers - 1] = (event_dispatcher_t) func;
    (init) ();                  /* Initialize the dispatcher's data */
}

void
event_dispatch(event_t *event)
{

    register unsigned char i;
    register unsigned char handled;

    /* No debugging stuff here.  If you need it, take it out when done.  This should be ultra-fast. -- mej */
    for (i = 0; i < event_master.num_dispatchers; i++) {
        handled = (event_master.dispatchers[i]) (event);
        if (handled) {
            break;
        }
    }
}

void
event_data_add_mywin(event_dispatcher_data_t *data, Window win)
{

    ASSERT(data != NULL);

    if (data->num_my_windows > 0) {
        (data->num_my_windows)++;
        data->my_windows = (Window *) REALLOC(data->my_windows, sizeof(Window) * data->num_my_windows);
        data->my_windows[data->num_my_windows - 1] = win;
    } else {
        data->num_my_windows = 1;
        data->my_windows = (Window *) MALLOC(sizeof(Window));
        data->my_windows[0] = win;
    }
}

void
event_data_add_parent(event_dispatcher_data_t *data, Window win)
{

    ASSERT(data != NULL);

    if (data->num_my_parents > 0) {
        (data->num_my_parents)++;
        data->my_parents = (Window *) REALLOC(data->my_parents, sizeof(Window) * data->num_my_parents);
        data->my_parents[data->num_my_parents - 1] = win;
    } else {
        data->num_my_parents = 1;
        data->my_parents = (Window *) MALLOC(sizeof(Window));
        data->my_parents[0] = win;
    }
}

void
event_init_primary_dispatcher(void)
{

    MEMSET(&primary_data, 0, sizeof(event_dispatcher_data_t));

    EVENT_DATA_ADD_HANDLER(primary_data, KeyPress, handle_key_press);
    EVENT_DATA_ADD_HANDLER(primary_data, PropertyNotify, handle_property_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, DestroyNotify, handle_destroy_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, ClientMessage, handle_client_message);
    EVENT_DATA_ADD_HANDLER(primary_data, MappingNotify, handle_mapping_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, VisibilityNotify, handle_visibility_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, EnterNotify, handle_enter_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, LeaveNotify, handle_leave_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, FocusIn, handle_focus_in);
    EVENT_DATA_ADD_HANDLER(primary_data, FocusOut, handle_focus_out);
    EVENT_DATA_ADD_HANDLER(primary_data, ConfigureNotify, handle_configure_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, SelectionClear, handle_selection_clear);
    EVENT_DATA_ADD_HANDLER(primary_data, SelectionNotify, handle_selection_notify);
    EVENT_DATA_ADD_HANDLER(primary_data, SelectionRequest, handle_selection_request);
    EVENT_DATA_ADD_HANDLER(primary_data, GraphicsExpose, handle_expose);
    EVENT_DATA_ADD_HANDLER(primary_data, Expose, handle_expose);
    EVENT_DATA_ADD_HANDLER(primary_data, ButtonPress, handle_button_press);
    EVENT_DATA_ADD_HANDLER(primary_data, ButtonRelease, handle_button_release);
    EVENT_DATA_ADD_HANDLER(primary_data, MotionNotify, handle_motion_notify);

    event_data_add_mywin(&primary_data, TermWin.parent);
    event_data_add_mywin(&primary_data, TermWin.vt);

#ifdef PIXMAP_SUPPORT
    if (desktop_window != None) {
        event_data_add_parent(&primary_data, desktop_window);
    }
#endif
}

unsigned char
event_win_is_mywin(register event_dispatcher_data_t *data, Window win)
{

    register unsigned short i;

    ASSERT_RVAL(data != NULL, 0);

    for (i = 0; i < data->num_my_windows; i++) {
        if (data->my_windows[i] == win) {
            return 1;
        }
    }
    return 0;
}

unsigned char
event_win_is_parent(register event_dispatcher_data_t *data, Window win)
{

    register unsigned short i;

    ASSERT_RVAL(data != NULL, 0);

    for (i = 0; i < data->num_my_parents; i++) {
        if (data->my_parents[i] == win) {
            return 1;
        }
    }
    return 0;
}

unsigned char
handle_key_press(event_t *ev)
{
    XWMHints *wm_hints;
#ifdef COUNT_X_EVENTS
    static unsigned long keypress_cnt = 0;
#endif

    PROF_INIT(handle_key_press);
    D_EVENTS(("handle_key_press(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));
#if UNUSED_BLOCK
    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);
#endif

    COUNT_EVENT(keypress_cnt);
    if (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_NO_INPUT))) {
        lookup_key(ev);
    }
    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_URG_ALERT)) {
        wm_hints = XGetWMHints(Xdisplay, TermWin.parent);
        wm_hints->flags &= ~XUrgencyHint;
        XSetWMHints(Xdisplay, TermWin.parent, wm_hints);
        XFree(wm_hints);
    }
    PROF_DONE(handle_key_press);
    PROF_TIME(handle_key_press);
    return 1;
}

unsigned char
handle_property_notify(event_t *ev)
{
    D_EVENTS(("handle_property_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

#ifdef PIXMAP_OFFSET
    if (background_is_trans()) {
        Window win;
        Pixmap pmap;

        if ((ev->xany.window == TermWin.parent) || (ev->xany.window == Xroot)) {
            D_EVENTS(("On %s.  prop (_WIN_WORKSPACE) == 0x%08x, ev->xproperty.atom == 0x%08x\n",
                      ((ev->xany.window == Xroot) ? "the root window" : "TermWin.parent"), (int) props[PROP_DESKTOP],
                      (int) ev->xproperty.atom));
            if (ev->xproperty.atom == props[PROP_DESKTOP]) {
                win = get_desktop_window();
                if (win == (Window) 1) {
                    /* The desktop window is unchanged.  Ignore this event. */
                    return 1;
                }
                /* The desktop window has changed somehow. */
                if (desktop_window == None) {
                    free_desktop_pixmap();
                    FOREACH_IMAGE(if (image_mode_is(idx, MODE_TRANS)) {
                                  image_set_mode(idx, MODE_IMAGE); image_allow_mode(idx, ALLOW_IMAGE);}
                    );
                    return 1;
                }
                pmap = get_desktop_pixmap();
                if (pmap == (Pixmap) 1) {
                    /* Pixmap is unchanged */
                    return 1;
                }
                redraw_images_by_mode(MODE_TRANS | MODE_VIEWPORT);
                return 1;
            }
        }
        if (ev->xany.window == desktop_window) {
            D_EVENTS(("On desktop_window [0x%08x].  prop (_XROOTPMAP_ID) == %d, ev->xproperty.atom == %d\n",
                      (int) desktop_window, (int) props[PROP_TRANS_PIXMAP], (int) ev->xproperty.atom));
            if (ev->xproperty.atom == props[PROP_TRANS_PIXMAP]) {
                pmap = get_desktop_pixmap();
                if (pmap == (Pixmap) 1) {
                    /* Pixmap is unchanged */
                    return 1;
                }
                redraw_images_by_mode(MODE_TRANS | MODE_VIEWPORT);
                return 1;
            }
        }
    }
#endif
    if ((ev->xany.window == Xroot) && (image_mode_any(MODE_AUTO))) {
        D_EVENTS(("On the root window.  prop (ENLIGHTENMENT_COMMS) == %d, ev->xproperty.atom == %d\n", (int) props[PROP_ENL_COMMS],
                  (int) ev->xproperty.atom));
        if ((props[PROP_ENL_COMMS] != None) && (ev->xproperty.atom == props[PROP_ENL_COMMS])) {
            if ((enl_ipc_get_win()) != None) {
#ifdef PIXMAP_SUPPORT
                redraw_images_by_mode(MODE_AUTO);
#endif
            }
        }
    }
    if (ev->xany.window == TermWin.vt) {
        D_EVENTS(("PropertyNotify on term window for atom %d, state %d.  Selection atoms are %d and %d.\n", ev->xproperty.atom,
                  ev->xproperty.state, props[PROP_SELECTION_DEST], props[PROP_SELECTION_INCR]));
        if (ev->xproperty.atom == props[PROP_SELECTION_DEST] && ev->xproperty.state == PropertyNewValue) {
            D_SELECT(("Fetching next part of incremental selection.\n"));
            selection_fetch(ev->xproperty.window, ev->xproperty.atom, True);
        }
    }
    return 1;
}

unsigned char
handle_destroy_notify(event_t *ev)
{
    D_EVENTS(("handle_destroy_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    if (ev->xdestroywindow.window == ipc_win) {
        D_EVENTS((" -> IPC window 0x%08x changed/destroyed.  Clearing ipc_win.\n", ipc_win));
        XSelectInput(Xdisplay, ipc_win, None);
        ipc_win = None;
        (void) check_image_ipc(1);
        return 1;
    } else if (XEVENT_IS_MYWIN(ev, &primary_data)) {
        /* One of our main windows was deleted.  Exit cleanly. */
        D_EVENTS((" -> Primary window destroyed.  Terminating.\n"));
        exit(0);
        ASSERT_NOTREACHED_RVAL(1);
    }
    return 0;
}

unsigned char
handle_client_message(event_t *ev)
{

    D_EVENTS(("handle_client_message(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

    if (ev->xclient.format == 32 && ev->xclient.data.l[0] == (signed) props[PROP_DELETE_WINDOW])
        exit(EXIT_SUCCESS);
    if (ev->xclient.format == 8 && ev->xclient.message_type == props[PROP_ENL_MSG]) {
        char buff[13];
        unsigned char i;

        for (i = 0; i < 12; i++) {
            buff[i] = ev->xclient.data.b[i + 8];
        }
        buff[12] = 0;
        D_ENL(("Discarding unexpected Enlightenment IPC message:  \"%s\"\n", buff));
        return 1;
    }
#ifdef OFFIX_DND
    /* OffiX Dnd (drag 'n' drop) protocol */
    if (ev->xclient.message_type == props[PROP_DND_PROTOCOL]
        && ((ev->xclient.data.l[0] == DndFile) || (ev->xclient.data.l[0] == DndDir) || (ev->xclient.data.l[0] == DndLink))) {
        /* Get DnD data */
        Atom ActualType;
        int ActualFormat;
        unsigned char *data;
        unsigned long Size, RemainingBytes;

        XGetWindowProperty(Xdisplay, Xroot, props[PROP_DND_SELECTION], 0L, 1000000L, False, AnyPropertyType, &ActualType,
                           &ActualFormat, &Size, &RemainingBytes, &data);
        if (data) {
            XChangeProperty(Xdisplay, Xroot, XA_CUT_BUFFER0, XA_STRING, 8, PropModeReplace, data, strlen(data));
            selection_paste(XA_CUT_BUFFER0);
            XSetInputFocus(Xdisplay, Xroot, RevertToNone, CurrentTime);
            XFree(data);
        }
        return 1;
    }
#endif /* OFFIX_DND */
    if ((XInternAtom(Xdisplay, "_FVWM_COLORTUNER", True) == ev->xclient.message_type) && ev->xclient.send_event) {
        if (ev->xclient.data.l[0] >= 0 && ev->xclient.data.l[0] <= 31) {
            PixColors[(int) ev->xclient.data.l[0]] = ev->xclient.data.l[1];

            if ((int) ev->xclient.data.l[0] == bgColor) {
                XFocusChangeEvent fev;
                event_t *pfev;

                fev.type = FocusIn;
                fev.send_event = 1;
                fev.display = Xdisplay;
                fev.window = ev->xany.window;
                pfev = (event_t *) & fev;
                handle_focus_in(pfev);
                redraw_image(image_bg);
            }
            scr_touch();
            scr_refresh(refresh_type);
        }
        return 1;
    }
    return 1;
}

unsigned char
handle_mapping_notify(event_t *ev)
{

    D_EVENTS(("handle_mapping_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    XRefreshKeyboardMapping(&(ev->xmapping));
    get_modifiers();
    return 1;
}

unsigned char
handle_visibility_notify(event_t *ev)
{

    D_EVENTS(("handle_visibility_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);
    switch (ev->xvisibility.state) {
        case VisibilityUnobscured:
            D_X11(("Window completely visible.  Using FAST_REFRESH.\n"));
            refresh_type = FAST_REFRESH;
            break;
        case VisibilityPartiallyObscured:
            D_X11(("Window partially hidden.  Using SLOW_REFRESH.\n"));
            refresh_type = SLOW_REFRESH;
            break;
        default:
            D_X11(("Window completely hidden.  Using NO_REFRESH.\n"));
            refresh_type = NO_REFRESH;
            break;
    }
    return 1;
}

unsigned char
handle_enter_notify(event_t *ev)
{

    D_EVENTS(("handle_enter_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

    if (ev->xany.window == TermWin.vt) {
        if (images[image_bg].norm != images[image_bg].selected) {
            images[image_bg].current = images[image_bg].selected;
            redraw_image(image_bg);
        }
        return 1;
    }
    return 0;
}

unsigned char
handle_leave_notify(event_t *ev)
{

    D_EVENTS(("handle_leave_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

    if (ev->xany.window == TermWin.vt) {
        if (images[image_bg].norm != images[image_bg].selected) {
            images[image_bg].current = images[image_bg].norm;
            redraw_image(image_bg);
        }
        return 1;
    }
    return 0;
}

unsigned char
handle_focus_in(event_t *ev)
{
    XWMHints *wm_hints;

    D_EVENTS(("handle_focus_in(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

    if (!TermWin.focus) {
        Window unused_root, child;
        int unused_root_x, unused_root_y;
        unsigned int unused_mask;

        TermWin.focus = 1;
        XQueryPointer(Xdisplay, TermWin.parent, &unused_root, &child, &unused_root_x, &unused_root_y, &(ev->xbutton.x),
                      &(ev->xbutton.y), &unused_mask);
        if (child == TermWin.vt) {
            if (images[image_bg].current != images[image_bg].selected) {
                images[image_bg].current = images[image_bg].selected;
                redraw_image(image_bg);
            }
        } else {
            if (images[image_bg].current != images[image_bg].norm) {
                images[image_bg].current = images[image_bg].norm;
                redraw_image(image_bg);
            }
        }
        if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP)) {
            map_scrollbar(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR));
        } else {
            scrollbar_set_focus(TermWin.focus);
            scrollbar_draw(IMAGE_STATE_NORMAL, MODE_SOLID);
        }
        bbar_draw_all(IMAGE_STATE_NORMAL, MODE_SOLID);
#ifdef USE_XIM
        if (xim_input_context)
            XSetICFocus(xim_input_context);
#endif
        if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_URG_ALERT)) {
            wm_hints = XGetWMHints(Xdisplay, TermWin.parent);
            wm_hints->flags &= ~XUrgencyHint;
            XSetWMHints(Xdisplay, TermWin.parent, wm_hints);
            XFree(wm_hints);
        }
    }
    return 1;
}

unsigned char
handle_focus_out(event_t *ev)
{

    D_EVENTS(("handle_focus_out(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);
    if (TermWin.focus) {
        TermWin.focus = 0;
        if (images[image_bg].current != images[image_bg].disabled) {
            images[image_bg].current = images[image_bg].disabled;
            redraw_image(image_bg);
        }
        if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP)) {
            map_scrollbar(0);
        } else {
            scrollbar_set_focus(TermWin.focus);
            scrollbar_draw(IMAGE_STATE_DISABLED, MODE_SOLID);
        }
        bbar_draw_all(IMAGE_STATE_DISABLED, MODE_SOLID);
#ifdef USE_XIM
        if (xim_input_context)
            XUnsetICFocus(xim_input_context);
#endif
    }
    return 1;
}

unsigned char
handle_configure_notify(event_t *ev)
{
    D_EVENTS(("handle_configure_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

    while (XCheckTypedWindowEvent(Xdisplay, ev->xany.window, ConfigureNotify, ev)) {
        D_EVENTS(("New event:  Window 0x%08x, %dx%d at %d, %d\n", ev->xany.window, ev->xconfigure.width, ev->xconfigure.height,
                  ev->xconfigure.x, ev->xconfigure.y));
    }
    if (ev->xany.window == TermWin.parent) {
        int x = ev->xconfigure.x, y = ev->xconfigure.y;
        unsigned int width = ev->xconfigure.width, height = ev->xconfigure.height;

        D_EVENTS((" -> TermWin.parent is %ldx%ld at (%d, %d).  Internal cache data shows %dx%d at (%hd, %hd).  send_event is %d\n",
                  width, height, x, y, szHint.width, szHint.height, TermWin.x, TermWin.y, ev->xconfigure.send_event));
        /* If the font change count is non-zero, this event is telling us we resized ourselves. */
        if (font_chg > 0) {
            font_chg--;
        }
        if ((width != (unsigned int) szHint.width) || (height != (unsigned int) szHint.height)) {
            /* We were resized externally.  Handle the resize. */
            D_EVENTS((" -> External resize detected.\n"));
            handle_resize(width, height);
#ifdef USE_XIM
            xim_set_status_position();
#endif
            /* A resize requires the additional handling of a move */
            if (ev->xconfigure.send_event) {
                handle_move(x, y);
            }
        } else if (((x != TermWin.x) || (y != TermWin.y)) && (ev->xconfigure.send_event)) {
            /* There was an external move, but no resize.  Handle the move. */
            D_EVENTS((" -> External move detected.\n"));
            handle_move(x, y);
        } else {
            D_EVENTS((" -> Bogus ConfigureNotify detected, ignoring.\n"));
        }
    }
    return 1;
}

unsigned char
handle_selection_clear(event_t *ev)
{

    D_EVENTS(("handle_selection_clear(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    selection_clear();
    return 1;
}

unsigned char
handle_selection_notify(event_t *ev)
{

    D_EVENTS(("handle_selection_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    selection_fetch(ev->xselection.requestor, ev->xselection.property, True);
    return 1;
}

unsigned char
handle_selection_request(event_t *ev)
{

    D_EVENTS(("handle_selection_request(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    selection_send(&(ev->xselectionrequest));
    return 1;
}

unsigned char
handle_expose(event_t *ev)
{
    PROF_INIT(handle_expose);
    D_EVENTS(("handle_expose(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);
    if (ev->xany.window == TermWin.vt
#ifdef PIXMAP_SUPPORT
        && buffer_pixmap == None
#endif
        ) {
        if (refresh_type == NO_REFRESH) {
            refresh_type = FAST_REFRESH;
        }
        scr_expose(ev->xexpose.x, ev->xexpose.y, ev->xexpose.width, ev->xexpose.height);
    } else {

        XEvent unused_xevent;

        while (XCheckTypedWindowEvent(Xdisplay, ev->xany.window, Expose, &unused_xevent));
        while (XCheckTypedWindowEvent(Xdisplay, ev->xany.window, GraphicsExpose, &unused_xevent));
    }
    PROF_DONE(handle_expose);
    PROF_TIME(handle_expose);
    return 1;
}

unsigned char
handle_button_press(event_t *ev)
{

    D_EVENTS(("handle_button_press(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

    if (action_dispatch(ev, 0)) {
        button_state.ignore_release = 1;
        return 1;
    }

    button_state.bypass_keystate = (ev->xbutton.state & (Mod1Mask | ShiftMask));
    button_state.report_mode = (button_state.bypass_keystate ? 0 : ((PrivateModes & PrivMode_mouse_report) ? 1 : 0));

    if (ev->xany.window == TermWin.vt) {
        if (ev->xbutton.subwindow == None) {
            if (button_state.report_mode) {
                if (PrivateModes & PrivMode_MouseX10) {
                    /* no state info allowed */
                    ev->xbutton.state = 0;
                }
#ifdef MOUSE_REPORT_DOUBLECLICK
                if (ev->xbutton.button == Button1) {
                    if (ev->xbutton.time - button_state.button_press < MULTICLICK_TIME)
                        button_state.clicks++;
                    else
                        button_state.clicks = 1;
                }
#else
                button_state.clicks = 1;
#endif /* MOUSE_REPORT_DOUBLECLICK */
                mouse_report(&(ev->xbutton));
            } else {
                switch (ev->xbutton.button) {
                    case Button1:
                        if ((button_state.last_button_press == 1)
                            && (ev->xbutton.time - button_state.button_press < MULTICLICK_TIME)) {
                            button_state.clicks++;
                        } else {
                            button_state.clicks = 1;
                        }
                        selection_click(button_state.clicks, ev->xbutton.x, ev->xbutton.y);
                        button_state.last_button_press = 1;
                        break;

                    case Button3:
                        if ((button_state.last_button_press == 3)
                            && (ev->xbutton.time - button_state.button_press < MULTICLICK_TIME)) {
                            selection_rotate(ev->xbutton.x, ev->xbutton.y);
                        } else {
                            selection_extend(ev->xbutton.x, ev->xbutton.y, 1);
                        }
                        button_state.last_button_press = 3;
                        break;
#ifdef MOUSEWHEEL
                        /* This section activates the following bindings:
                         *
                         * Mousewheel Up               -- Scroll up 1 page
                         * Ctrl + Mousewheel Up        -- Scroll up 5 pages
                         * Shift + Mousewheel Up       -- Scroll up 1 line
                         * Alt + Mousewheel Up         -- Send PgUp to tty
                         * Alt + Ctrl + Mousewheel Up  -- Send 5 PgUp's to tty
                         * Alt + Shift + Mousewheel Up -- Send Up Arrow to tty
                         *
                         * Mousewheel Down               -- Scroll down 1 page
                         * Ctrl + Mousewheel Down        -- Scroll down 5 pages
                         * Shift + Mousewheel Down       -- Scroll down 1 line
                         * Alt + Mousewheel Down         -- Send PgDn to tty
                         * Alt + Ctrl + Mousewheel Down  -- Send 5 PgDn's to tty
                         * Alt + Shift + Mousewheel Down -- Send Down Arrow to tty
                         *
                         * Note that the number of lines which constitute a "page" is equal to the number
                         * of text rows in the terminal window.  The context lines are subtracted out *after*
                         * the conversion is done.  In other words, scrolling 5 pages means scrolling
                         * (5 * LINES_PER_PAGE) - CONTEXT_LINES
                         *    _not_
                         * (LINES_PER_PAGE - CONTEXT_LINES) * 5
                         *
                         * This is also true for the scroll() function in script.c.
                         */
                    case Button4:
                        if (action_check_modifiers(MOD_CTRL, ev->xbutton.state)) {
                            scr_page(UP, (TermWin.nrow * 5) - CONTEXT_LINES);
                        } else if (action_check_modifiers(MOD_SHIFT, ev->xbutton.state)) {
                            scr_page(UP, 1);
                        } else if (action_check_modifiers(MOD_ALT, ev->xbutton.state)) {
                            tt_write("\033[5~", 4);
                        } else if (action_check_modifiers((MOD_ALT | MOD_SHIFT), ev->xbutton.state)) {
                            tt_write("\033[A", 3);
                        } else if (action_check_modifiers((MOD_ALT | MOD_CTRL), ev->xbutton.state)) {
                            tt_write("\033[5~\033[5~\033[5~\033[5~\033[5~", 20);
                        } else {
                            scr_page(UP, TermWin.nrow - CONTEXT_LINES);
                        }
                        button_state.last_button_press = 4;
                        break;
                    case Button5:
                        if (action_check_modifiers(MOD_CTRL, ev->xbutton.state)) {
                            scr_page(DN, (TermWin.nrow * 5) - CONTEXT_LINES);
                        } else if (action_check_modifiers(MOD_SHIFT, ev->xbutton.state)) {
                            scr_page(DN, 1);
                        } else if (action_check_modifiers(MOD_ALT, ev->xbutton.state)) {
                            tt_write("\033[6~", 4);
                        } else if (action_check_modifiers((MOD_ALT | MOD_SHIFT), ev->xbutton.state)) {
                            tt_write("\033[B", 3);
                        } else if (action_check_modifiers((MOD_ALT | MOD_CTRL), ev->xbutton.state)) {
                            tt_write("\033[6~\033[6~\033[6~\033[6~\033[6~", 20);
                        } else {
                            scr_page(DN, TermWin.nrow - CONTEXT_LINES);
                        }
                        button_state.last_button_press = 5;
                        break;
#endif
                    default:
                        break;
                }
            }
            button_state.button_press = ev->xbutton.time;
            return (1);
        }
    }
    return 0;
}

unsigned char
handle_button_release(event_t *ev)
{
    D_EVENTS(("handle_button_release(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    if (button_state.ignore_release == 1) {
        button_state.ignore_release = 0;
        return 0;
    }

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);
    button_state.mouse_offset = 0;
    button_state.report_mode = (button_state.bypass_keystate ? 0 : ((PrivateModes & PrivMode_mouse_report) ? 1 : 0));

    if (ev->xany.window == TermWin.vt) {
        if (ev->xbutton.subwindow == None) {
            if (button_state.report_mode) {
                switch (PrivateModes & PrivMode_mouse_report) {
                    case PrivMode_MouseX10:
                        break;

                    case PrivMode_MouseX11:
                        ev->xbutton.state = button_state.bypass_keystate;
                        ev->xbutton.button = AnyButton;
                        mouse_report(&(ev->xbutton));
                        break;
                }
                return (1);
            }
            /*
             * dumb hack to compensate for the failure of click-and-drag
             * when overriding mouse reporting
             */
            if ((PrivateModes & PrivMode_mouse_report) && (button_state.bypass_keystate) && (ev->xbutton.button == Button1)
                && (clickOnce())) {
                selection_extend(ev->xbutton.x, ev->xbutton.y, 0);
            }
            switch (ev->xbutton.button) {
                case Button1:
                case Button3:
                    selection_make(ev->xbutton.time);
                    break;

                case Button2:
                    selection_paste(XA_PRIMARY);
                    break;
                default:
                    break;
            }
        }
    }
    return 0;
}

unsigned char
handle_motion_notify(event_t *ev)
{
#ifdef COUNT_X_EVENTS
    static unsigned long motion_cnt = 0;
#endif

    PROF_INIT(handle_motion_notify);
    D_EVENTS(("handle_motion_notify(ev [%8p] on window 0x%08x)\n", ev, ev->xany.window));

    COUNT_EVENT(motion_cnt);

    REQUIRE_RVAL(XEVENT_IS_MYWIN(ev, &primary_data), 0);

#ifdef HAVE_TWIN
    /* It's really silly that Twin uses mouse drag codes that are
     * different from the ones that xterm uses.
     */
    if ((PrivateModes & PrivMode_mouse_report) && !(button_state.bypass_keystate)) {
        twin_mouse_drag_report(&(ev->xbutton));
        return 1;
    }
#endif

    if (ev->xany.window == TermWin.vt) {
        if (ev->xbutton.state & (Button1Mask | Button3Mask)) {
            Window unused_root, unused_child;
            int unused_root_x, unused_root_y;
            unsigned int unused_mask;

            while (XCheckTypedWindowEvent(Xdisplay, TermWin.vt, MotionNotify, ev));
            XQueryPointer(Xdisplay, TermWin.vt, &unused_root, &unused_child, &unused_root_x, &unused_root_y, &(ev->xbutton.x),
                          &(ev->xbutton.y), &unused_mask);
#ifdef MOUSE_THRESHOLD
            /* deal with a `jumpy' mouse */
            if ((ev->xmotion.time - button_state.button_press) > MOUSE_THRESHOLD)
#endif
                selection_extend((ev->xbutton.x), (ev->xbutton.y), (ev->xbutton.state & Button3Mask));
        }
        PROF_DONE(handle_motion_notify);
        PROF_TIME(handle_motion_notify);
        return 1;
    }
    PROF_DONE(handle_motion_notify);
    PROF_TIME(handle_motion_notify);
    return 1;
}

unsigned char
process_x_event(event_t *ev)
{
#ifdef COUNT_X_EVENTS
    static unsigned long event_cnt = 0;
#endif

    COUNT_EVENT(event_cnt);
#if 0
    D_EVENTS(("process_x_event(ev [%8p] %s on window 0x%08x)\n", ev, event_type_to_name(ev->xany.type), ev->xany.window));
#endif
    if (primary_data.handlers[ev->type]) {
        return ((primary_data.handlers[ev->type]) (ev));
    }
    return (0);
}

XErrorHandler
xerror_handler(Display * display, XErrorEvent * event)
{

    char err_string[2048];

    strcpy(err_string, "");
    XGetErrorText(display, event->error_code, err_string, sizeof(err_string));
    libast_print_error("XError in function %s, resource 0x%08x (request %d.%d):  %s (error %d)\n",
                request_code_to_name(event->request_code), (int) event->resourceid, event->request_code, event->minor_code,
                err_string, event->error_code);
#if DEBUG > DEBUG_X11
    if (DEBUG_LEVEL >= DEBUG_X11) {
        dump_stack_trace();
    }
#endif
    libast_print_error("Attempting to continue...\n");
    return 0;
}
