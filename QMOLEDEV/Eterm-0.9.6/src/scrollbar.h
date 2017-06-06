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

#ifndef _SCROLLBAR_H
#define _SCROLLBAR_H

#include <X11/Xfuncproto.h>
#include <ctype.h>
#include "events.h"
#include "pixmap.h"

/************ Macros and Definitions ************/
/* Scrollbar types we support */
#define SCROLLBAR_MOTIF   1
#define SCROLLBAR_XTERM   2
#define SCROLLBAR_NEXT    3

/* Scrollbar states */
#define SCROLLBAR_STATE_VISIBLE               (1UL << 0)
#define SCROLLBAR_STATE_MOVING                (1UL << 1)
#define SCROLLBAR_STATE_UP_CLICKED            (1UL << 2)
#define SCROLLBAR_STATE_DOWN_CLICKED          (1UL << 3)
#define SCROLLBAR_STATE_ANCHOR_CLICKED        (1UL << 4)
#define SCROLLBAR_STATE_MOTION_MASK           (SCROLLBAR_STATE_UP_CLICKED | SCROLLBAR_STATE_DOWN_CLICKED | SCROLLBAR_STATE_ANCHOR_CLICKED | SCROLLBAR_STATE_MOVING)

/* Scrollbar state macros */
#define scrollbar_is_visible()                (scrollbar.state & SCROLLBAR_STATE_VISIBLE)
#define scrollbar_is_moving()                 (scrollbar.state & SCROLLBAR_STATE_MOVING)
#define scrollbar_uparrow_is_pressed()        (scrollbar.state & SCROLLBAR_STATE_UP_CLICKED)
#define scrollbar_downarrow_is_pressed()      (scrollbar.state & SCROLLBAR_STATE_DOWN_CLICKED)
#define scrollbar_arrow_is_pressed()          (scrollbar.state & (SCROLLBAR_STATE_UP_CLICKED | SCROLLBAR_STATE_DOWN_CLICKED))
#define scrollbar_anchor_is_pressed()         (scrollbar.state & SCROLLBAR_STATE_ANCHOR_CLICKED)
#define scrollbar_cancel_motion()             (scrollbar.state &= ~(SCROLLBAR_STATE_MOTION_MASK))
#define scrollbar_set_visible(bit)            ((bit) ? (scrollbar.state |= SCROLLBAR_STATE_VISIBLE) : (scrollbar.state &= ~(SCROLLBAR_STATE_VISIBLE)))
#define scrollbar_set_motion(bit)             ((bit) ? (scrollbar.state |= SCROLLBAR_STATE_MOVING) : (scrollbar.state &= ~(SCROLLBAR_STATE_MOVING)))
#define scrollbar_set_uparrow_pressed(bit)    ((bit) ? (scrollbar.state |= SCROLLBAR_STATE_UP_CLICKED) : (scrollbar.state &= ~(SCROLLBAR_STATE_UP_CLICKED)))
#define scrollbar_set_downarrow_pressed(bit)  ((bit) ? (scrollbar.state |= SCROLLBAR_STATE_DOWN_CLICKED) : (scrollbar.state &= ~(SCROLLBAR_STATE_DOWN_CLICKED)))
#define scrollbar_set_anchor_pressed(bit)     ((bit) ? (scrollbar.state |= SCROLLBAR_STATE_ANCHOR_CLICKED) : (scrollbar.state &= ~(SCROLLBAR_STATE_ANCHOR_CLICKED)))

/* The various scrollbar elements */
#define scrollbar_win_is_trough(w)            (scrollbar_is_visible() && (w) == scrollbar.win)
#define scrollbar_win_is_uparrow(w)           ((w) == scrollbar.up_win)
#define scrollbar_win_is_downarrow(w)         ((w) == scrollbar.dn_win)
#define scrollbar_win_is_anchor(w)            ((w) == scrollbar.sa_win)
#define scrollbar_is_pixmapped()              (images[image_sb].mode & MODE_MASK)
#define scrollbar_uparrow_is_pixmapped()      (images[image_up].mode & MODE_MASK)
#define scrollbar_downarrow_is_pixmapped()    (images[image_down].mode & MODE_MASK)
#define scrollbar_anchor_is_pixmapped()       (images[image_sa].mode & MODE_MASK)

/* Scrollbar dimensions */
#define scrollbar_scrollarea_height()         (scrollbar.scrollarea_end - scrollbar.scrollarea_start)
#define scrollbar_anchor_width()              ((scrollbar.type == SCROLLBAR_XTERM) ? (scrollbar.win_width) : (scrollbar.width))
#define scrollbar_anchor_height()             (MAX((scrollbar.anchor_bottom - scrollbar.anchor_top), 2))
#define scrollbar_trough_width()              (scrollbar.win_width)
#define scrollbar_trough_height()             (scrollbar.win_height)
#define scrollbar_arrow_width()               (scrollbar.width)
#define scrollbar_arrow_height()              (scrollbar.width)

/* Scrollbar positions */
#define scrollbar_is_above_anchor(w, y)       (!scrollbar_win_is_anchor(w) && ((y) <= scrollbar.anchor_top))
#define scrollbar_is_below_anchor(w, y)       (!scrollbar_win_is_anchor(w) && ((y) >= scrollbar.anchor_bottom))
#define scrollbar_position(y)                 ((y) - scrollbar.scrollarea_start)
#define scrollbar_up_loc()                    (scrollbar.up_arrow_loc)
#define scrollbar_dn_loc()                    (scrollbar.down_arrow_loc)

/* Scrollbar operations */
#if 0
#define map_scrollbar(show)                   do {PrivMode(show, PrivMode_scrollbar); if (scrollbar_mapping(show)) {scr_touch(); parent_resize();}} while (0)
#else
#define map_scrollbar(show)                   do {PrivMode(show, PrivMode_scrollbar); if (scrollbar_mapping(show)) {parent_resize();}} while (0)
#endif
#define scrollbar_map_arrows()                do {XMapWindow(Xdisplay, scrollbar.up_win); XMapWindow(Xdisplay, scrollbar.dn_win);} while (0)
#define scrollbar_unmap_arrows()              do {XUnmapWindow(Xdisplay, scrollbar.up_win); XUnmapWindow(Xdisplay, scrollbar.dn_win);} while (0)
#define scrollbar_get_shadow()                ((scrollbar.type == SCROLLBAR_XTERM) ? (0) : (scrollbar.shadow))
#define scrollbar_set_shadow(s)               (scrollbar.shadow = (s))
#define scrollbar_get_type()                  (scrollbar.type)
#define scrollbar_set_type(t)                 (scrollbar.type = (t))
#define scrollbar_get_width()                 (scrollbar.width)
#define scrollbar_set_width(w)                (scrollbar.width = (w))
#define scrollbar_get_win()                   (scrollbar.win)
#define scrollbar_get_uparrow_win()           (scrollbar.up_win)
#define scrollbar_get_downarrow_win()         (scrollbar.dn_win)
#define scrollbar_get_anchor_win()            (scrollbar.sa_win)

/************ Structures ************/
typedef struct {
  Window win, up_win, dn_win, sa_win;
  short scrollarea_start, scrollarea_end;
  short anchor_top, anchor_bottom;
  unsigned char state;
  unsigned int type:2;
  unsigned int init:1;
  unsigned int shadow:5;
  unsigned short width, height;
  unsigned short win_width, win_height;
  short up_arrow_loc, down_arrow_loc;
} scrollbar_t;

/************ Variables ************/
extern scrollbar_t scrollbar;
#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
extern short scroll_arrow_delay;
#endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void scrollbar_event_init_dispatcher(void);
extern unsigned char sb_handle_configure_notify(event_t *);
extern unsigned char sb_handle_enter_notify(event_t *);
extern unsigned char sb_handle_leave_notify(event_t *);
extern unsigned char sb_handle_focus_in(event_t *);
extern unsigned char sb_handle_focus_out(event_t *);
extern unsigned char sb_handle_expose(event_t *);
extern unsigned char sb_handle_button_press(event_t *);
extern unsigned char sb_handle_button_release(event_t *);
extern unsigned char sb_handle_motion_notify(event_t *);
extern unsigned char scrollbar_dispatch_event(event_t *);
extern void scrollbar_draw_uparrow(unsigned char image_state, unsigned char force_modes);
extern unsigned char scrollbar_move_uparrow(void);
extern void scrollbar_draw_downarrow(unsigned char image_state, unsigned char force_modes);
extern unsigned char scrollbar_move_downarrow(void);
extern void scrollbar_draw_anchor(unsigned char image_state, unsigned char force_modes);
extern unsigned char scrollbar_move_anchor(void);
extern void scrollbar_draw_trough(unsigned char image_state, unsigned char force_modes);
extern void scrollbar_init(int, int);
extern unsigned char scrollbar_mapping(unsigned char);
extern void scrollbar_reset(void);
extern void scrollbar_calc_size(int width, int height);
extern void scrollbar_resize(int, int);
extern void scrollbar_change_type(unsigned int);
extern void scrollbar_change_width(unsigned short);
extern void scrollbar_drawing_init(void);
extern unsigned char scrollbar_set_focus(short has_focus);
extern unsigned char scrollbar_anchor_update_position(short mouseoffset);
extern void scrollbar_draw(unsigned char image_state, unsigned char force_modes);
extern void scrollbar_reposition_and_draw(unsigned char force_modes);
extern void scrollbar_reposition_and_always_draw(void);
extern unsigned char scrollbar_show(short);

_XFUNCPROTOEND

#endif	/* _SCROLLBAR_H */
