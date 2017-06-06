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

#ifndef _MENUS_H
# define _MENUS_H

# include <X11/Xfuncproto.h>
# include "events.h"
# include "pixmap.h"

/************ Macros and Definitions ************/
#define MENUITEM_SEP		(1UL << 0)
#define MENUITEM_SUBMENU	(1UL << 1)
#define MENUITEM_STRING		(1UL << 2)
#define MENUITEM_ECHO		(1UL << 3)
#define MENUITEM_SCRIPT		(1UL << 4)
#define MENUITEM_ALERT		(1UL << 5)
#define MENUITEM_LITERAL	(1UL << 6)

#define MENU_STATE_IS_MAPPED	(1UL << 0)
#define MENU_STATE_IS_CURRENT	(1UL << 1)
#define MENU_STATE_IS_DRAGGING	(1UL << 2)
#define MENU_STATE_IS_SUBMENU	(1UL << 3)
#define MENU_STATE_IS_FOCUSED	(1UL << 4)

/* Constants */
#define MENU_HGAP	4
#define MENU_VGAP	4
#define MENU_CLICK_TIME	200
#define NO_CURRENT_ITEM ((unsigned short) -1)

#define menu_is_pixmapped()          ((images[image_menu].current->iml->im) && (images[image_menu].mode & MODE_MASK))
#define menu_submenu_is_pixmapped()  ((images[image_submenu].current->iml->im) && (images[image_submenu].mode & MODE_MASK))
#define menuitem_get_current(m)      (((m)->curitem != NO_CURRENT_ITEM) ? ((m)->items[(m)->curitem]) : (NULL))
#define menuitem_set_current(m, i)   ((m)->curitem = (i))
#define menuitem_clear_current(m)    ((m)->curitem = NO_CURRENT_ITEM)

/************ Structures ************/
typedef struct menu_t_struct menu_t;

typedef struct {
  simage_t *icon;
  unsigned char type, state;
  union {
    menu_t *submenu;
    char *string;
    char *script;
    char *alert;
  } action;
  char *text, *rtext;
  unsigned short len, rlen;
  unsigned short x, y, w, h;
} menuitem_t;

struct menu_t_struct {
  char *title;
  Window win, swin;
  Pixmap bg;
  unsigned short x, y, w, h;
  GC gc;
  unsigned char state;
  XFontStruct *font;
#ifdef MULTI_CHARSET
  XFontSet fontset;
#endif
  unsigned short fwidth, fheight;
  unsigned short numitems, curitem;
  menuitem_t **items;
};

typedef struct {
  unsigned char nummenus;
  menu_t **menus;
} menulist_t;

/************ Variables ************/
extern menulist_t *menu_list;
#ifdef ESCREEN
extern event_dispatcher_data_t menu_event_data;
#endif


/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void menu_init(void);
extern void menu_event_init_dispatcher(void);
extern unsigned char menu_handle_enter_notify(event_t *);
extern unsigned char menu_handle_leave_notify(event_t *);
extern unsigned char menu_handle_focus_in(event_t *);
extern unsigned char menu_handle_focus_out(event_t *);
extern unsigned char menu_handle_expose(event_t *);
extern unsigned char menu_handle_button_press(event_t *);
extern unsigned char menu_handle_button_release(event_t *);
extern unsigned char menu_handle_motion_notify(event_t *);
extern unsigned char menu_dispatch_event(event_t *);
extern menulist_t *menulist_add_menu(menulist_t *, menu_t *);
extern void menulist_clear(menulist_t *);
extern menu_t *menu_create(char *);
extern void menu_delete(menu_t *);
extern unsigned char menu_set_title(menu_t *, const char *);
extern unsigned char menu_set_font(menu_t *, const char *);
extern unsigned char menu_add_item(menu_t *, menuitem_t *);
extern unsigned char menu_is_child(menu_t *, menu_t *);
extern menu_t *find_menu_by_title(menulist_t *, char *);
extern menu_t *find_menu_by_window(menulist_t *, Window);
extern menuitem_t *find_item_by_coords(menu_t *, int, int);
extern unsigned short find_item_in_menu(menu_t *, menuitem_t *);
extern void menuitem_change_current(menuitem_t *);
extern menuitem_t *menuitem_create(char *);
extern void menuitem_delete(menuitem_t *);
extern unsigned char menuitem_set_text(menuitem_t *, const char *);
extern unsigned char menuitem_set_icon(menuitem_t *, simage_t *);
extern unsigned char menuitem_set_action(menuitem_t *, unsigned char, char *);
extern unsigned char menuitem_set_rtext(menuitem_t *, char *);

extern void menu_reset(menu_t *);
extern void menu_reset_all(menulist_t *);
extern void menu_reset_tree(menu_t *);
extern void menu_reset_submenus(menu_t *);
extern void menuitem_select(menu_t *);
extern void menuitem_deselect(menu_t *);
extern void menu_display_submenu(menu_t *, menuitem_t *);
extern void menu_move(menu_t *, unsigned short, unsigned short);
extern void menu_draw(menu_t *);
extern void menu_display(int, int, menu_t *);
extern void menu_action(menuitem_t *);
extern void menu_invoke(int, int, Window, menu_t *, Time);
extern void menu_invoke_by_title(int, int, Window, char *, Time);
extern int menu_tab(void *, char *[], int, char *, size_t, size_t);
extern int menu_dialog(void *, char *, int, char **, int (*)(void *, char *, size_t, size_t));

_XFUNCPROTOEND

#endif	/* _MENUS_H */
