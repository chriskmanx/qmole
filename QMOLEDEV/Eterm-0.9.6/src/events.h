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

#ifndef _EVENTS_H_
#define _EVENTS_H_

#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

/************ Macros and Definitions ************/
#if 0
# define XEVENT_IS_MYWIN(ev)       (((ev)->xany.window == TermWin.parent) \
				      || ((ev)->xany.window == TermWin.vt) \
				      || ((ev)->xany.window == menuBar.win) \
				      || (scrollbar_uparrow_is_pixmapped() && ((ev)->xany.window == scrollbar.up_win)) \
				      || (scrollbar_downarrow_is_pixmapped() && ((ev)->xany.window == scrollbar.dn_win)) \
				      || (scrollbar_anchor_is_pixmapped() && ((ev)->xany.window == scrollbar.sa_win)) \
				      || ((ev)->xany.window == scrollbar.win))
# define XEVENT_IS_PARENT(ev)      (((ev)->xany.window == TermWin.wm_parent) \
                                      || ((ev)->xany.window == TermWin.wm_grandparent))
#endif

#define XEVENT_IS_MYWIN(ev, data) event_win_is_mywin(data, (ev)->xany.window)
#define XEVENT_IS_PARENT(ev, data) event_win_is_parent(data, (ev)->xany.window)

#define clickOnce() (button_state.clicks <= 1)
#ifdef COUNT_X_EVENTS
# define COUNT_EVENT(x)  do {(x)++; D_EVENTS(("%s == %ld\n", #x, x));} while (0)
#else
# define COUNT_EVENT(x)  NOP
#endif
#ifdef PROFILE_X_EVENTS
# define P_EVENT_TIME(type, start, stop)  libast_dprintf(type ":  %ld microseconds\n", P_CMPTIMEVALS_USEC((start), (stop)))
#else
# define P_EVENT_TIME(type, start, stop)  NOP
#endif
#define EVENT_DATA_ADD_HANDLER(data, type, handler) (data).handlers[(type)] = handler

/************ Structures ************/
typedef XEvent event_t;
typedef unsigned char (*event_dispatcher_t)(event_t *);
typedef unsigned char (*event_handler_t)(event_t *);
typedef void (*event_dispatcher_init_t)(void);
typedef struct {
  event_dispatcher_t *dispatchers;
  unsigned char num_dispatchers;
} event_master_t;
typedef struct {
  event_handler_t handlers[LASTEvent];
  unsigned char num_my_windows;
  Window *my_windows;
  unsigned char num_my_parents;
  Window *my_parents;
} event_dispatcher_data_t;

typedef struct {
  unsigned short clicks, bypass_keystate, report_mode, mouse_offset;
  Time button_press, last_button_press, activate_time;
  unsigned char ignore_release;
} mouse_button_state_t;

/************ Variables ************/
extern unsigned char paused;
extern event_master_t event_master;
extern mouse_button_state_t button_state;

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void event_init_subsystem(event_dispatcher_t, event_dispatcher_init_t);
extern void event_register_dispatcher(event_dispatcher_t, event_dispatcher_init_t);
extern void event_dispatch(event_t *);
extern void event_data_add_mywin(event_dispatcher_data_t *, Window);
extern void event_data_add_parent(event_dispatcher_data_t *, Window);
extern void event_init_primary_dispatcher(void);
extern unsigned char event_win_is_mywin(event_dispatcher_data_t *, Window);
extern unsigned char event_win_is_parent(event_dispatcher_data_t *, Window);
extern unsigned char handle_key_press(event_t *);
extern unsigned char handle_property_notify(event_t *);
extern unsigned char handle_destroy_notify(event_t *);
extern unsigned char handle_reparent_notify(event_t *);
extern unsigned char handle_client_message(event_t *);
extern unsigned char handle_mapping_notify(event_t *);
extern unsigned char handle_leave_notify(event_t *);
extern unsigned char handle_visibility_notify(event_t *);
extern unsigned char handle_enter_notify(event_t *);
extern unsigned char handle_leave_notify(event_t *);
extern unsigned char handle_focus_in(event_t *);
extern unsigned char handle_focus_out(event_t *);
extern unsigned char handle_configure_notify(event_t *);
extern unsigned char handle_selection_clear(event_t *);
extern unsigned char handle_selection_notify(event_t *);
extern unsigned char handle_selection_request(event_t *);
extern unsigned char handle_expose(event_t *);
extern unsigned char handle_button_press(event_t *);
extern unsigned char handle_button_release(event_t *);
extern unsigned char handle_motion_notify(event_t *);
extern unsigned char process_x_event(event_t *);
extern XErrorHandler xerror_handler(Display *, XErrorEvent *);

_XFUNCPROTOEND

#endif	/* _EVENTS_H_ */
