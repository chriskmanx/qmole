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

#ifndef _BUTTONS_H
# define _BUTTONS_H

# include <X11/Xfuncproto.h>
# include "actions.h"
# include "events.h"
# include "menus.h"

/************ Macros and Definitions ************/
#define BBAR_DOCKED_TOP               (1 << 0)
#define BBAR_DOCKED_BOTTOM            (1 << 1)
#define BBAR_DOCKED                   (BBAR_DOCKED_TOP | BBAR_DOCKED_BOTTOM)
#define BBAR_UNDOCKED                 (~BBAR_DOCKED)
#define BBAR_VISIBLE                  (1 << 2)

#define bbar_is_docked(bbar)          (bbar->state & BBAR_DOCKED)
#define bbar_is_top_docked(bbar)      (bbar->state & BBAR_DOCKED_TOP)
#define bbar_is_bottom_docked(bbar)   (bbar->state & BBAR_DOCKED_BOTTOM)
#define bbar_set_docked(bbar, d)      do {bbar->state &= ~BBAR_DOCKED; bbar->state |= (d);} while (0)
#define bbar_redock(bbar)             bbar_dock(bbar, bbar_is_docked(bbar));
#define bbar_is_visible(bbar)         (bbar->state & BBAR_VISIBLE)
#define bbar_set_visible(bbar, v)     ((v) ? (bbar->state |= BBAR_VISIBLE) : (bbar->state &= ~BBAR_VISIBLE))
#define bbar_get_width(bbar)          (bbar->w)
#define bbar_get_height(bbar)         (bbar->h)
#ifdef __GNUC__
# define bbar_total_height()           __extension__ ({__typeof__(bbar_total_h) bth = (bbar_total_h != -1) ? (bbar_total_h) : (bbar_calc_total_height()); \
                                                       D_BBAR(("bbar_total_height() returning %d\n", bth)); bth;})
# define bbar_reset_total_height()     __extension__ ({D_BBAR(("bbar_reset_total_height()\n")); bbar_total_h = -1; bbar_total_h;})
#else
# define bbar_total_height()           ((bbar_total_h != -1) ? (bbar_total_h) : (bbar_calc_total_height()))
# define bbar_reset_total_height()     (bbar_total_h = -1)
#endif

#define FOREACH_BUTTONBAR(x)           do {buttonbar_t *bbar; for (bbar = buttonbar; bbar; bbar = bbar->next) { x } } while (0)

/************ Structures ************/
typedef struct button_struct {
  simage_t *icon;
  action_type_t type;
  union {
    menu_t *menu;
    char *script;
    char *string;
  } action;
  char *text;
  unsigned short len;
  unsigned short x, y, w, h;
  unsigned short text_x, text_y;
  unsigned short icon_x, icon_y, icon_w, icon_h;
#ifdef ESCREEN
  int flags;
#endif
  struct button_struct *next;
} button_t;

typedef struct buttonbar_struct {
  Window win;
  Pixmap bg;
  unsigned short x, y, w, h;
  GC gc;
  unsigned char state;
  XFontStruct *font;
#ifdef MULTI_CHARSET
  XFontSet fontset;
#endif
  unsigned short fwidth, fheight;
  event_dispatcher_data_t event_data;
  unsigned char image_state;
  button_t *buttons, *rbuttons, *current;
  struct buttonbar_struct *next;
} buttonbar_t;

/************ Variables ************/
extern buttonbar_t *buttonbar;
extern long bbar_total_h;
#ifdef ESCREEN
extern button_t *drag;
#endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern buttonbar_t *bbar_create(void);
extern void bbar_free(buttonbar_t *);
extern void bbar_init(buttonbar_t *, int);
extern void bbar_event_init_dispatcher(void);
extern unsigned char bbar_handle_enter_notify(event_t *);
extern unsigned char bbar_handle_leave_notify(event_t *);
extern unsigned char bbar_handle_button_press(event_t *);
extern unsigned char bbar_handle_button_release(event_t *);
extern unsigned char bbar_handle_motion_notify(event_t *);
extern unsigned char bbar_dispatch_event(event_t *);
extern buttonbar_t *find_bbar_by_window(Window);
extern void bbar_add(buttonbar_t *bbar);
extern unsigned short bbar_calc_height(buttonbar_t *bbar);
extern void bbar_calc_button_sizes(buttonbar_t *bbar);
extern void bbar_calc_button_positions(buttonbar_t *bbar);
extern void button_calc_size(buttonbar_t *bbar, button_t *button);
extern void button_calc_rel_coords(buttonbar_t *bbar, button_t *button);
extern void bbar_add_button(buttonbar_t *bbar, button_t *button);
extern void bbar_add_rbutton(buttonbar_t *bbar, button_t *button);
extern unsigned char bbar_set_font(buttonbar_t *bbar, const char *fontname);
extern button_t *find_button_by_text(buttonbar_t *bbar, char *text);
extern button_t *find_button_by_index(buttonbar_t *bbar, long);
extern button_t *find_button_by_coords(buttonbar_t *bbar, int x, int y);
extern button_t *button_create(char *text);
extern void button_free(button_t *);
extern unsigned char button_set_text(button_t *button, const char *text);
extern unsigned char button_set_icon(button_t *button, simage_t *icon);
extern unsigned char button_set_action(button_t *button, action_type_t type, char *action);
extern void bbar_select_button(buttonbar_t *bbar, button_t *button);
extern void bbar_deselect_button(buttonbar_t *bbar, button_t *button);
extern void bbar_click_button(buttonbar_t *bbar, button_t *button);
extern void button_check_action(buttonbar_t *bbar, button_t *button, unsigned char press, Time t);
extern unsigned char bbar_show(buttonbar_t *bbar, unsigned char visible);
extern void bbar_show_all(signed char visible);
extern void bbar_resize(buttonbar_t *bbar, int w);
extern void bbar_resize_all(int width);
extern void bbar_dock(buttonbar_t *bbar, unsigned char dock);
extern void bbar_draw(buttonbar_t *bbar, unsigned char image_state, unsigned char force_modes);
extern void bbar_draw_all(unsigned char image_state, unsigned char force_modes);
extern void bbar_calc_positions(void);
extern unsigned long bbar_calc_total_height(void);
extern unsigned long bbar_calc_docked_height(unsigned char);
extern void bbar_redraw(buttonbar_t *bbar);
extern buttonbar_t *bbar_insert_button(buttonbar_t *bbar, button_t *button, int after, int addright);

_XFUNCPROTOEND

#endif	/* _BUTTONS_H */
