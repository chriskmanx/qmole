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

#ifndef _ACTIONS_H_
#define _ACTIONS_H_

#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

#include "events.h"
#include "menus.h"

/************ Macros and Definitions ************/
typedef enum {
  ACTION_NONE = 0,
  ACTION_STRING,
  ACTION_ECHO,
  ACTION_SCRIPT,
  ACTION_MENU
} action_type_t;

#define KEYSYM_NONE    (0UL)

#define MOD_NONE           (0UL)
#define MOD_CTRL           (1UL << 0)
#define MOD_SHIFT          (1UL << 1)
#define MOD_LOCK           (1UL << 2)
#define MOD_META           (1UL << 3)
#define MOD_ALT            (1UL << 4)
#define MOD_MOD1           (1UL << 5)
#define MOD_MOD2           (1UL << 6)
#define MOD_MOD3           (1UL << 7)
#define MOD_MOD4           (1UL << 8)
#define MOD_MOD5           (1UL << 9)
#define MOD_ANY            (1UL << 10)

#define BUTTON_NONE        (0)
#define BUTTON_ANY         (0xff)

#define LOGICAL_XOR(a, b)  !(((a) && (b)) || (!(a) && !(b)))

#define SHOW_MODS(m)       ((m & MOD_CTRL) ? 'C' : 'c'), ((m & MOD_SHIFT) ? 'S' : 's'), ((m & MOD_META) ? 'M' : 'm'), ((m & MOD_ALT) ? 'A' : 'a')
#define SHOW_X_MODS(m)     ((m & ControlMask) ? 'C' : 'c'), ((m & ShiftMask) ? 'S' : 's'), ((m & MetaMask) ? 'M' : 'm'), ((m & AltMask) ? 'A' : 'a')
#define MOD_FMT            "%c%c%c%c"

/************ Structures ************/
typedef struct action_struct action_t;
typedef unsigned char (*action_handler_t) (event_t *, action_t *);
struct action_struct {
  unsigned short mod;
  unsigned char button; 
  KeySym keysym;
  action_type_t type;
  action_handler_t handler;
  union {
    char *string;
    char *script;
    menu_t *menu;
  } param;
  struct action_struct *next;
};

/************ Variables ************/
extern action_t *action_list;

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern unsigned char action_handle_string(event_t *ev, action_t *action);
extern unsigned char action_handle_echo(event_t *ev, action_t *action);
extern unsigned char action_handle_script(event_t *ev, action_t *action);
extern unsigned char action_handle_menu(event_t *ev, action_t *action);
extern action_t *action_find_match(unsigned short mod, unsigned char button, KeySym keysym);
extern unsigned char action_check_button(unsigned char button, int x_button);
extern unsigned char action_check_keysym(KeySym keysym, KeySym x_keysym);
extern unsigned char action_check_modifiers(unsigned short mod, int x_mod);
extern unsigned char action_dispatch(event_t *ev, KeySym keysym);
extern void action_add(unsigned short mod, unsigned char button, KeySym keysym, action_type_t type, void *param);

_XFUNCPROTOEND

#endif	/* _ACTIONS_H_ */
