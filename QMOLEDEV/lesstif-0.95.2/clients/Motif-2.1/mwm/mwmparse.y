%{
/**
 *
 * $Id: mwmparse.y,v 1.1 2004/08/28 19:27:39 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#include "LTconfig.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#else
#error "you lose (I don't know how to fix this)"
#endif
#include <X11/Xfuncs.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include "mwm.h"


extern int yydebug;
extern int yylex(void);

static int num_items;
static MenuRoot *cur_menu;
static ScreenInfo *pscr;
static Boolean skip_test;
static Boolean button_bind_match;
static Boolean button_bind_found;
static Boolean key_bind_match;
static Boolean key_bind_found;

/*
 * this must be here for the parser
 */
static int lineno;

%}
%token  ALT_TOK
	APP_TOK
	BACK_TOK
	BORDER_TOK
	BTN1_CLICK2_TOK
	BTN1_CLICK_TOK
	BTN1_DOWN_TOK
	BTN1_UP_TOK
	BTN2_CLICK2_TOK
	BTN2_CLICK_TOK
	BTN2_DOWN_TOK
	BTN2_UP_TOK
	BTN3_CLICK2_TOK
	BTN3_CLICK_TOK
	BTN3_DOWN_TOK
	BTN3_UP_TOK
	BTN4_CLICK2_TOK
	BTN4_CLICK_TOK
	BTN4_DOWN_TOK
	BTN4_UP_TOK
	BTN5_CLICK2_TOK
	BTN5_CLICK_TOK
	BTN5_DOWN_TOK
	BTN5_UP_TOK
	BUTTONS_TOK
	CTRL_TOK
	FBEEP_TOK
	FCIRCLE_DOWN_TOK
	FCIRCLE_UP_TOK
	FEXEC_TOK
	FFOCUS_COLOR_TOK
	FFOCUS_KEY_TOK
	FKILL_TOK
	FLOWER_TOK
	FMAXIMIZE_TOK
	FMENU_TOK
	FMINIMIZE_TOK
	FMOVE_TOK
	FNEXT_CMAP_TOK
	FNEXT_KEY_TOK
	FNOP_TOK
	FNORMALIZE_TOK
	FNORMANDRAISE_TOK
	FPACK_ICONS_TOK
	FPASS_KEYS_TOK
	FPOST_WMENU_TOK
	FPREV_CMAP_TOK
	FPREV_KEY_TOK
	FQUIT_MWM_TOK
	FRAISE_LOWER_TOK
	FRAISE_TOK
	FRAME_TOK
	FREE_FAMILY_TOK
	FREFRESH_TOK
	FREFRESH_WIN_TOK
	FRESIZE_TOK
	FRESTART_TOK
	FRESTOREANDRAISE_TOK
	FRESTORE_TOK
	FSCREEN_TOK
	FSEND_MSG_TOK
	FSEPARATOR_TOK
	FSET_BEHAVIOR_TOK
	FTITLE_TOK
	FWINDOWLIST_TOK
	FDESK_TOK
	FTOGGLE_PAGE_TOK
	FGOTO_PAGE_TOK
	ICON_TOK
	KEY_TOK
	KEYS_TOK
	LOCK_TOK
	MENU_TOK
	MENUB_TOK
	MINIMIZEB_TOK
	MAXIMIZEB_TOK
	MOD1_TOK
	MOD2_TOK
	MOD3_TOK
	MOD4_TOK
	MOD5_TOK
	NEXT_TOK
	PREV_TOK
	ROOT_TOK
	SHIFT_TOK
	TITLE_TOK
	TRANSIENT_TOK
	WINDOW_TOK
	WITHIN_TOK
	STRING_TOK

%type <string>
	STRING_TOK

%type <number>
	FCIRCLE_DOWN_TOK
	FCIRCLE_UP_TOK
	FEXEC_TOK
	FFOCUS_COLOR_TOK
	FFOCUS_KEY_TOK
	FKILL_TOK
	FLOWER_TOK
	FMAXIMIZE_TOK
	FMENU_TOK
	FMINIMIZE_TOK
	FMOVE_TOK
	FNEXT_CMAP_TOK
	FNEXT_KEY_TOK
	FNOP_TOK
	FNORMALIZE_TOK
	FNORMANDRAISE_TOK
	FPACK_ICONS_TOK
	FPASS_KEYS_TOK
	FPOST_WMENU_TOK
	FPREV_CMAP_TOK
	FPREV_KEY_TOK
	FQUIT_MWM_TOK
	FRAISE_LOWER_TOK
	FRAISE_TOK
	FRAME_TOK
	FREE_FAMILY_TOK
	FREFRESH_TOK
	FREFRESH_WIN_TOK
	FRESIZE_TOK
	FRESTART_TOK
	FRESTOREANDRAISE_TOK
	FRESTORE_TOK
	FSCREEN_TOK
	FSEND_MSG_TOK
	FSEPARATOR_TOK
	FSET_BEHAVIOR_TOK
	FTITLE_TOK
	ICON_TOK

%type <string>
	bitmap_file
	string

%type <label>
	label

%type <number>
	context
	object

%type <function>
	function

%type <modifiers>
	modifier_name
	modifier_list

%type <hotkey>
	key

%union {
    char	*string;
    int		number;
    KeySym	key;
    struct {
	int func;
	char *arg;
    } function;
    struct {
	int type;
	char *string;
    } label;
    long	modifiers;
    struct {
	int button;
	int event;
	int count;
	int modifiers;
    } button;
    struct {
	int modifiers;
	KeySym key;
    } hotkey;
};

%%
res_file:		res_file binding
	|
;

binding:		button_bindings
	|		key_bindings
	|		menu_bindings
;

button_bindings:	BUTTONS_TOK string
			{
			    if (!skip_test) {
				if (button_bind_found == False) {
				    if (strcmp($2, pscr->button_bindings) == 0)
					button_bind_match = True;
				    else
					button_bind_match = False;
				}
				else if (strcmp($2, pscr->button_bindings) == 0) {
				    yyerror("Ignoring duplicate button bindings");
				    button_bind_match = False;
				}
				else
				    button_bind_match = False;
			    }
			    else
				button_bind_match = True;
			}
			'{' button_list '}'
;

button_list:		button_list button_binding
	|		error
			{
			    yyerror("Invalid button binding");
			    yyerrok;
			    yyclearin;
			}
	|
;

button_binding:		button context function
			{
			    int contexts = $<number>2;
			    int mods, func;
			    MenuItem *mi = NULL;
			    MouseButton *temp = NULL, *ptr;

			    if (!button_bind_match)
				break;

			    mods = $<button>1.modifiers;
			    if ((contexts & C_WINDOW) &&
				(((mods == 0) || mods == AnyModifier))) {
			        pscr->buttons2grab &= ~($<button>1.button);
			    }
			
			    func = $<function>3.func;

			    if ((func == F_EXEC) ||
				(func == F_RESTART) ||
			        (func == F_CIRCULATE_UP) ||
				(func == F_CIRCULATE_DOWN) ||
			        (func == F_WARP)) {
			        mi = (MenuItem *)XtMalloc(sizeof(MenuItem));
			
			        mi->next = (MenuItem *) NULL;
			        mi->prev = (MenuItem *) NULL;
			        if ((func == F_EXEC) || (func == F_RESTART)) {
			            mi->item = XtNewString("DOIT");
			            mi->action = $<function>3.arg;
			        }
			        else {
			            mi->item = XtNewString("DONT DOIT");
			            mi->action = $<function>3.arg;
			        }
				mi->item2 = "";
			        mi->state = 0;
			        mi->func = func;
			        mi->strlen = strlen(mi->item);
				mi->strlen2 = 0;
			        mi->val1 = 0;
			        mi->val2 = 0;
			        mi->val1_unit = 1;
			        mi->val2_unit = 1;
			    }

			    temp = pscr->buttons;
			    pscr->buttons =
				(MouseButton *)XtMalloc(sizeof(MouseButton));
			    pscr->buttons->func = func;
			    if (func == F_POPUP)
				pscr->buttons->menu = (MenuRoot *)$<function>3.arg;
			    else if (func == F_W_POPUP)
				pscr->buttons->menu = (MenuRoot *)DEFAULT_WIN_MENU_NAME;
			    else
				pscr->buttons->menu = NULL;
			    pscr->buttons->item = mi;
			    pscr->buttons->button =
				ffs($<button>1.button) - 8;
			    pscr->buttons->context = contexts;
			    pscr->buttons->modifier = mods;
			    pscr->buttons->mask = $<button>1.event;
			    pscr->buttons->count = $<button>1.count;
			    pscr->buttons->val1 = 0;
			    pscr->buttons->val2 = 0;
			    pscr->buttons->val1_unit = pscr->d_width;
			    pscr->buttons->val2_unit = pscr->d_height;

			    /* if duplicate, skip second */
			    for (ptr = temp;
				 ptr != NULL;
				 ptr = ptr->next) {
                              /* If everything is the same except for having
                                  func's F_POPUP and F_WINDOWLIST, the
                                  second is a duplicate */
                               if (((ptr->func == F_POPUP || 
                                     ptr->func == F_WINDOWLIST) && 
                                    (pscr->buttons->func == F_POPUP || 
                                     pscr->buttons->func == F_WINDOWLIST)) &&
				    ptr->modifier == pscr->buttons->modifier &&
				    ptr->mask == pscr->buttons->mask &&
                                   ptr->count == pscr->buttons->count &&
                                   ptr->context == pscr->buttons->context &&
                                   ptr->button  == pscr->buttons->button) {

                                   XtFree((char *)pscr->buttons);
                                   pscr->buttons = temp;
                                   break;
                               }

                               if (ptr->func == pscr->buttons->func &&
                                   ptr->modifier == pscr->buttons->modifier &&
                                   ptr->mask == pscr->buttons->mask &&
                                   ptr->count == pscr->buttons->count &&
                                   ptr->button == pscr->buttons->button) {
				    ptr->context |= pscr->buttons->context;
				    XtFree((char *)pscr->buttons);
				    pscr->buttons = temp;
				    break;
				}
			    }
			    if (ptr == NULL)
				pscr->buttons->next = temp;
			}
;

key_bindings:		KEYS_TOK string
			{
			    if (!skip_test) {
				if (key_bind_found == False) {
				    if (strcmp($2, pscr->key_bindings) == 0)
					key_bind_match = True;
				    else
					key_bind_match = False;
				}
				else if (strcmp($2, pscr->key_bindings) == 0) {
				    yyerror("Ignoring duplicate key bindings");
					key_bind_match = False;
				}
				else
				    key_bind_match = False;
			    }
			    else
				key_bind_match = True;
			}
			'{' key_list '}'
;

key_list:		key_list key_binding
	|		error
			{
			    yyerror("Invalid key binding");
			    yyerrok;
			    yyclearin;
			}
	|
;

key_binding:		key context function
			{
			    FuncKey        *tmp = NULL;
			    KeySym          keysym;
			    KeyCode         keycode;
			    int             i, min, max;
			    int func, contexts;
			    char *ptr;

			    if (!key_bind_match)
				break;

			    ptr = $<function>3.arg;
			    func = $<function>3.func;
			    contexts = $<number>2;

			    /*
			     * Don't let a 0 keycode go through, since that
			     * means AnyKey to the XGrabKey call in GrabKeys().
			     */
			    keysym = $<hotkey>1.key;
			    if (keysym == 0)
				break;
			    if ((keycode = XKeysymToKeycode(dpy, $<hotkey>1.key)) == 0)
				break;

			    XDisplayKeycodes(dpy, &min, &max);
			    for (i = min; i <= max; i++) {
				if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
				    tmp = (FuncKey *)XtMalloc(sizeof(FuncKey));
				    tmp->next = pscr->keys;
				    pscr->keys = tmp;

				    tmp->name = "HOTKEY";
				    tmp->keycode = i;
				    tmp->cont = contexts;
				    tmp->mods = $<hotkey>1.modifiers;
				    tmp->func = func;
				    if (func == F_W_POPUP)
					tmp->action = DEFAULT_WIN_MENU_NAME;
				    else
					tmp->action = ptr;
				    tmp->val1 = 0;
				    tmp->val2 = 0;
				    tmp->val1_unit = pscr->d_width;
				    tmp->val2_unit = pscr->d_height;
				    tmp->menu = NULL;
				}
			    }
			}
;

menu_bindings:		MENU_TOK string
			{
			    num_items = 0;
			    cur_menu = MENU_Create($<string>2);
			}
			'{' item_list '}'
			{
			    MENU_Add(pscr, cur_menu);
			}
;

item_list:		item_list item_binding
	|		error
			{
			    yyerror("Invalid menu item");
			    yyerrok;
			    yyclearin;
			}
	|
;

item_binding:		label mnemonic accelerator function
			{
			    MenuItem *tmp;
			    int width, width2;
			    FuncKey        *tmpk = NULL;
			    KeySym          keysym;
			    KeyCode         keycode;
			    int             i, min, max;
			    int func, contexts;
			    char *ptr, *acc;

			    tmp = (MenuItem *)XtCalloc(1, sizeof(MenuItem));
			    if (cur_menu->first == NULL) {
				cur_menu->first = tmp;
				tmp->prev = NULL;
				tmp->next = NULL;
			    }
			    else {
				cur_menu->last->next = tmp;
				tmp->prev = cur_menu->last;
			    }
			    cur_menu->last = tmp;
			    cur_menu->items++;

			    if ($<function>4.func == F_NOP)
				tmp->item = "";
			    else
				tmp->item = $<label>1.string;
			    tmp->item2 = "";
			    MENU_FindHotKey(tmp, $<key>2);
			    if (strcmp(tmp->item, "no-label") == 0)
				tmp->strlen = 0;
			    else
				tmp->strlen = strlen(tmp->item);

			    tmp->action = $<function>4.arg;
			    tmp->next = NULL;
			    tmp->state = 0;
			    tmp->func = $<function>4.func;
			    tmp->val1 = 0;
			    tmp->val2 = 0;
			    tmp->val1_unit = pscr->d_width;
			    tmp->val2_unit = pscr->d_height;
			
			    width = XTextWidth(pscr->components[MWM_MENU].font,
					       tmp->item, tmp->strlen);
			    if (tmp->func == F_POPUP)
			        width += 15;
			    if (width <= 0)
			        width = 1;
			    if (width > cur_menu->width)
			        cur_menu->width = width;

			    ptr = $<function>4.arg;
			    func = $<function>4.func;
			    contexts = C_WINDOW|C_ICON|C_FRAME|C_TITLE|C_LALL|C_RALL;

			    /*
			     * Don't let a 0 keycode go through, since that
			     * means AnyKey to the XGrabKey call in GrabKeys().
			     */
			    keysym = $<hotkey>3.key;
			    if (keysym == 0)
				break;
			    if ((keycode = XKeysymToKeycode(dpy, $<hotkey>3.key)) == 0)
				break;

			    acc = MENU_AcceleratorString(pscr,
							 $<hotkey>3.key,
							 $<hotkey>3.modifiers);
			    if (strlen(acc)) {
				tmp->item2 = XtNewString(acc);
				tmp->strlen2 = strlen(acc);
				width2 = XTextWidth(pscr->components[MWM_MENU].font,
						    tmp->item2, tmp->strlen2);
				if (width2 > cur_menu->width2)
				    cur_menu->width2 = width2;
			    }

			    XDisplayKeycodes(dpy, &min, &max);
			    for (i = min; i <= max; i++) {
				if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
				    tmpk = (FuncKey *)XtMalloc(sizeof(FuncKey));
				    tmpk->next = pscr->keys;
				    pscr->keys = tmpk;

				    tmpk->name = "HOTKEY";
				    tmpk->keycode = i;
				    tmpk->cont = contexts;
				    tmpk->mods = $<hotkey>3.modifiers;
				    tmpk->func = func;
				    tmpk->action = ptr;
				    tmpk->val1 = 0;
				    tmpk->val2 = 0;
				    tmpk->val1_unit = pscr->d_width;
				    tmpk->val2_unit = pscr->d_height;
				    tmpk->menu = NULL;
				}
			    }
			}
;

accelerator:		key
			{ $<hotkey>$ = $<hotkey>1; }
	|
			{ $<hotkey>$.modifiers = 0; $<hotkey>$.key = 0; }
;

context:		context '|' object
			{ $<number>$ = $1 | $3; }
	|		object
			{ $<number>$ = $1; }
;

object:			ROOT_TOK
			{ $<number>$ = C_ROOT; }
	|		ICON_TOK
			{ $<number>$ = C_ICON; }
	|		MENUB_TOK
			{ $<number>$ = C_MENUB; }
	|		MINIMIZEB_TOK
			{ $<number>$ = C_MINIMIZEB; }
	|		MAXIMIZEB_TOK
			{ $<number>$ = C_MAXIMIZEB; }
	|		WINDOW_TOK
			{ $<number>$ = C_WINDOW|C_FRAME|C_TITLE|C_LALL|C_RALL|C_ICON; }
	|		TITLE_TOK
			{ $<number>$ = C_TITLE; }
	|		FRAME_TOK
			{ $<number>$ = C_FRAME|C_TITLE|C_LALL|C_RALL; }
	|		BORDER_TOK
			{ $<number>$ = C_FRAME; }
	|		APP_TOK
			{ $<number>$ = C_WINDOW; }
	|		error
			{ yyerror("Invalid context"); $<number>$ = 0; }
;

button:			modifier_list '<' button_event_name '>'
			{
			    $<button>$ = $<button>3;
			    $<button>$.modifiers = $1;
			}
	|		'<' button_event_name '>'
			{
			    $<button>$ = $<button>2;
			    $<button>$.modifiers = 0;
			}
;

key:			modifier_list '<' KEY_TOK '>' STRING_TOK
			{
			    $<hotkey>$.modifiers = $<modifiers>1;
			    $<hotkey>$.key = XStringToKeysym($<string>5);
			}
	|		'<' KEY_TOK '>' STRING_TOK
			{
			    $<hotkey>$.modifiers = 0;
			    $<hotkey>$.key = XStringToKeysym($<string>4);
			}
;

modifier_list:		modifier_list modifier_name
			{ $<modifiers>$ = $1 | $2; }
	|		modifier_name
			{ $<modifiers>$ = $1; }
;

modifier_name:		CTRL_TOK
			{ $<modifiers>$ = ControlMask; }
	|		SHIFT_TOK
			{ $<modifiers>$ = ShiftMask; }
	|		ALT_TOK
			{ $<modifiers>$ = pscr->alt_mask; }
	|		LOCK_TOK
			{ $<modifiers>$ = LockMask; }
	|		MOD1_TOK
			{ $<modifiers>$ = Mod1Mask; }
	|		MOD2_TOK
			{ $<modifiers>$ = Mod2Mask; }
	|		MOD3_TOK
			{ $<modifiers>$ = Mod3Mask; }
	|		MOD4_TOK
			{ $<modifiers>$ = Mod4Mask; }
	|		MOD5_TOK
			{ $<modifiers>$ = Mod5Mask; }
;

button_event_name:	BTN1_DOWN_TOK
			{
			    $<button>$.button = Button1Mask;
			    $<button>$.event = ButtonPressMask;
			    $<button>$.count = 0;
			}
	|		BTN1_UP_TOK
			{
			    $<button>$.button = Button1Mask;
			    $<button>$.event = ButtonReleaseMask;
			    $<button>$.count = 0;
			}
	|		BTN1_CLICK_TOK
			{
			    $<button>$.button = Button1Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 1;
			}
	|		BTN1_CLICK2_TOK
			{
			    $<button>$.button = Button1Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 2;
			}
	|		BTN2_DOWN_TOK
			{
			    $<button>$.button = Button2Mask;
			    $<button>$.event = ButtonPressMask;
			    $<button>$.count = 0;
			}
	|		BTN2_UP_TOK
			{
			    $<button>$.button = Button2Mask;
			    $<button>$.event = ButtonReleaseMask;
			    $<button>$.count = 0;
			}
	|		BTN2_CLICK_TOK
			{
			    $<button>$.button = Button2Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 1;
			}
	|		BTN2_CLICK2_TOK
			{
			    $<button>$.button = Button2Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 2;
			}
	|		BTN3_DOWN_TOK
			{
			    $<button>$.button = Button3Mask;
			    $<button>$.event = ButtonPressMask;
			    $<button>$.count = 0;
			}
	|		BTN3_UP_TOK
			{
			    $<button>$.button = Button3Mask;
			    $<button>$.event = ButtonReleaseMask;
			    $<button>$.count = 0;
			}
	|		BTN3_CLICK_TOK
			{
			    $<button>$.button = Button3Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 1;
			}
	|		BTN3_CLICK2_TOK
			{
			    $<button>$.button = Button3Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 2;
			}
	|		BTN4_DOWN_TOK
			{
			    $<button>$.button = Button4Mask;
			    $<button>$.event = ButtonPressMask;
			    $<button>$.count = 0;
			}
	|		BTN4_UP_TOK
			{
			    $<button>$.button = Button4Mask;
			    $<button>$.event = ButtonReleaseMask;
			    $<button>$.count = 0;
			}
	|		BTN4_CLICK_TOK
			{
			    $<button>$.button = Button4Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 1;
			}
	|		BTN4_CLICK2_TOK
			{
			    $<button>$.button = Button4Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 2;
			}
	|		BTN5_DOWN_TOK
			{
			    $<button>$.button = Button5Mask;
			    $<button>$.event = ButtonPressMask;
			    $<button>$.count = 0;
			}
	|		BTN5_UP_TOK
			{
			    $<button>$.button = Button5Mask;
			    $<button>$.event = ButtonReleaseMask;
			    $<button>$.count = 0;
			}
	|		BTN5_CLICK_TOK
			{
			    $<button>$.button = Button5Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 1;
			}
	|		BTN5_CLICK2_TOK
			{
			    $<button>$.button = Button5Mask;
			    $<button>$.event = ButtonPressMask|ButtonReleaseMask;
			    $<button>$.count = 2;
			}
	|		error
			{
			    yyerror("Invalid button event name");
			    $<button>$.button = 0;
			    $<button>$.event = 0;
			    $<button>$.count = 0;
			}
;

label:			string
			{ $<label>$.type = IS_STRING; $<label>$.string = $1; }
	|		bitmap_file
			{ $<label>$.type = IS_BITMAP; $<label>$.string = $1; }
	|		error
			{
			    yyerror("Invalid label");
			    $<label>$.type = IS_STRING;
			    $<label>$.string = "";
			}
;

bitmap_file:		'@' string
			{ $<string>$ = $2; }
;

mnemonic:		'_' string
			{
			    if (strlen($<string>2) != 1) {
				yyerror("Invalid mnemonic specification");
				$<key>$ = 0;
			    }
			    else {
				$<key>$ = XStringToKeysym($<string>2);
			    }
			}
	|
			{ $<key>$ = 0; }
;

function:		FBEEP_TOK
			{
			    $<function>$.func = F_BEEP;
			    $<function>$.arg = "";
			}
	|		FCIRCLE_DOWN_TOK optional_arg
			{
			    $<function>$.func = F_CIRCULATE_DOWN;
			    $<function>$.arg = $<string>2;
			}
	|		FCIRCLE_UP_TOK optional_arg
			{
			    $<function>$.func = F_CIRCULATE_UP;
			    $<function>$.arg = $<string>2;
			}
	|		FEXEC_TOK string
			{
			    $<function>$.func = F_EXEC;
			    $<function>$.arg = $<string>2;
			}
	|		'!' string
			{
			    $<function>$.func = F_EXEC;
			    $<function>$.arg = $<string>2;
			}
	|		FFOCUS_COLOR_TOK
			{
			    $<function>$.func = F_FOCUS_COLOR;
			    $<function>$.arg = "";
			}
	|		FFOCUS_KEY_TOK
			{
			    $<function>$.func = F_FOCUS_KEY;
			    $<function>$.arg = "";
			}
	|		FKILL_TOK
			{
			    $<function>$.func = F_CLOSE;
			    $<function>$.arg = "";
			}
	|		FLOWER_TOK optional_arg
			{
			    $<function>$.func = F_LOWER;
			    $<function>$.arg = $<string>2;
			}
	|		FMAXIMIZE_TOK
			{
			    $<function>$.func = F_MAXIMIZE;
			    $<function>$.arg = "";
			}
	|		FMENU_TOK string
			{
			    $<function>$.func = F_POPUP;
			    $<function>$.arg = $<string>2;
			}
	|		FMINIMIZE_TOK
			{
			    $<function>$.func = F_ICONIFY;
			    $<function>$.arg = "";
			}
	|		FMOVE_TOK
			{
			    $<function>$.func = F_MOVE;
			    $<function>$.arg = "";
			}
	|		FNEXT_CMAP_TOK
			{
			    $<function>$.func = F_NEXT_CMAP;
			    $<function>$.arg = "";
			}
	|		FNEXT_KEY_TOK optional_arg
			{
			    $<function>$.func = F_NEXT_KEY;
			    $<function>$.arg = $<string>2;
			}
	|		FNOP_TOK
			{
			    $<function>$.func = F_NOP;
			    $<function>$.arg = "";
			}
	|		FNORMALIZE_TOK
			{
			    $<function>$.func = F_NORMALIZE;
			    $<function>$.arg = "";
			}
	|		FNORMANDRAISE_TOK
			{
			    $<function>$.func = F_NORM_AND_RAISE;
			    $<function>$.arg = "";
			}
	|		FPACK_ICONS_TOK
			{
			    $<function>$.func = F_PACK_ICONS;
			    $<function>$.arg = "";
			}
	|		FPASS_KEYS_TOK
			{
			    $<function>$.func = F_PASS_KEYS;
			    $<function>$.arg = "";
			}
	|		FPOST_WMENU_TOK
			{
			    $<function>$.func = F_W_POPUP;
			    $<function>$.arg = "";
			}
	|		FPREV_CMAP_TOK
			{
			    $<function>$.func = F_PREV_CMAP;
			    $<function>$.arg = "";
			}
	|		FPREV_KEY_TOK optional_arg
			{
			    $<function>$.func = F_PREV_KEY;
			    $<function>$.arg = $<string>2;
			}
	|		FQUIT_MWM_TOK
			{
			    $<function>$.func = F_QUIT;
			    $<function>$.arg = "";
			}
	|		FRAISE_TOK optional_arg
			{
			    $<function>$.func = F_RAISE;
			    $<function>$.arg = $<string>2;
			}
	|		FRAISE_LOWER_TOK optional_arg
			{
			    $<function>$.func = F_RAISELOWER;
			    $<function>$.arg = $<string>2;
			}
	|		FREFRESH_TOK
			{
			    $<function>$.func = F_REFRESH;
			    $<function>$.arg = "";
			}
	|		FREFRESH_WIN_TOK
			{
			    $<function>$.func = F_REFRESH_WIN;
			    $<function>$.arg = "";
			}
	|		FRESIZE_TOK
			{
			    $<function>$.func = F_RESIZE;
			    $<function>$.arg = "";
			}
	|		FRESTORE_TOK
			{
			    $<function>$.func = F_ICONIFY;
			    $<function>$.arg = "";
			}
	|		FRESTOREANDRAISE_TOK
			{
			    $<function>$.func = F_RESTORE_AND_RAISE;
			    $<function>$.arg = "";
			}
	|		FRESTART_TOK optional_arg
			{
			    $<function>$.func = F_RESTART;
			    $<function>$.arg = $<string>2;
			}
	|		FSCREEN_TOK optional_arg
			{
			    $<function>$.func = F_SCREEN;
			    $<function>$.arg = $<string>2;
			}
	|		FSEND_MSG_TOK string
			{
			    $<function>$.func = F_SEND_MSG;
			    $<function>$.arg = $<string>2;
			}
	|		FSEPARATOR_TOK
			{
			    $<function>$.func = F_NOP;
			    $<function>$.arg = "";
			}
	|		FSET_BEHAVIOR_TOK
			{
			    $<function>$.func = F_SET_BEHAVIOR;
			    $<function>$.arg = "";
			}
	|		FTITLE_TOK
			{
			    $<function>$.func = F_TITLE;
			    $<function>$.arg = "";
			}
	|		FWINDOWLIST_TOK
			{
			    $<function>$.func = F_WINDOWLIST;
			    $<function>$.arg = "";
			}
	|		error
			{
			    yyerror("Invalid mnemonic, accelerator, or function");
			    $<function>$.func = F_NOP;
			    $<function>$.arg = "";
			}
;

optional_arg:		arg
			{ $<string>$ = $<string>1; }
	|
			{ $<string>$ = ""; }
;

arg:			'-' string
			{ $<string>$ = $<string>2; }
	|		ROOT_TOK
			{ $<string>$ = "ROOT"; }
	|		WINDOW_TOK
			{ $<string>$ = "WINDOW"; }
	|		TRANSIENT_TOK
			{ $<string>$ = "TRANSIENT"; }
	|		ICON_TOK
			{ $<string>$ = "ICON"; }
	|		WITHIN_TOK
			{ $<string>$ = "WITHIN"; }
	|		FREE_FAMILY_TOK
			{ $<string>$ = "FREE_FAMILY"; }
	|		NEXT_TOK
			{ $<string>$ = "NEXT"; }
	|		PREV_TOK
			{ $<string>$ = "PREV"; }
	|		BACK_TOK
			{ $<string>$ = "BACK"; }
;
string:			STRING_TOK
			{ $<string>$ = $<string>1; }
;
%%

/*
 * these to variables are used by the parser to control input
 */
#define MAX_UNGET	1024
static const char *curpos;
static const char *endpos;
static const char *input_buf;
static char unget[MAX_UNGET];
static int upos = 0;

static char *_MwmRootMenu = DEFAULT_MWM_ROOT_MENU;
static char *_MwmKeyBindings = DEFAULT_MWM_KEY_BINDINGS;
static char *_MwmWindowMenu = DEFAULT_MWM_WINDOW_MENU;
static char *_MwmBehaviorKey = MWM_BEHAVIOR_KEY_BINDINGS;
static char *_MwmBuiltinButtonBindings = BUILTIN_MWM_BUTTON_BINDINGS;
static char *_MwmBuiltinMenuButtonBindings = BUILTIN_MENU_BUTTON_BINDINGS;
static char *_MwmBuiltinKillButtonBindings = BUILTIN_KILL_BUTTON_BINDINGS;
static char *_MwmBuiltinIconButtonBindings = BUILTIN_ICON_BUTTON_BINDINGS;

void
yyerror(const char *fmt, ...) {
    va_list arg_list;

    va_start(arg_list, fmt);
    vfprintf(stderr, fmt, arg_list);
    va_end(arg_list);
    fprintf(stderr, " at line %d\n", lineno);
}


char
mwm_getc(void) {
    char c;

    if (upos) {
	upos--;
	c = unget[upos];
	if (c == '\n')
	    lineno++;
	return c;
    }
    if (curpos >= endpos)
	return 0;
    else if (*curpos == 0) {
	curpos++;
	return 0;
    }
    c = *curpos;
    curpos++;
    if (c == '\n')
	lineno++;
    return c;
}


void
mwm_putc(char c) {
    printf("OUTPUT: %c\n", c);
}


void
mwm_unputc(char c) {
    if (upos == MAX_UNGET) {
	yyerror("Lexer can't back up anymore.\n");
	return;
    }
    if (c == '\n')
	lineno--;
    unget[upos] = c;
    upos++;
}


int
PARSE_buf(ScreenInfo *scr, char *buffer) {
    pscr = scr;
    lineno = 1;
    curpos = input_buf = buffer;
    endpos = input_buf + strlen(input_buf);
    upos = 0;
    return yyparse();
}


static void
do_standard(ScreenInfo *scr)
{
    skip_test = True;

    PARSE_buf(scr, _MwmRootMenu);
    PARSE_buf(scr, _MwmWindowMenu);
    PARSE_buf(scr, _MwmKeyBindings);
    PARSE_buf(scr, _MwmBehaviorKey);

    /* a certain amount seems to be expected as builtin */
    PARSE_buf(scr, _MwmBuiltinButtonBindings);
    if (Mwm.w_menu_button_click)
	PARSE_buf(scr, _MwmBuiltinMenuButtonBindings);
    if (Mwm.w_menu_button_click_2)
	PARSE_buf(scr, _MwmBuiltinKillButtonBindings);
    if (Mwm.icon_click)
	PARSE_buf(scr, _MwmBuiltinIconButtonBindings);

    PROP_SetBehavior(scr, False);
}


static void
do_custom(ScreenInfo *scr)
{
    char *configfile, *ptr;
    int fd;

    configfile=find_config_file();
    if (configfile==NULL)
    {
	yyerror("Cannot find configuration file.  "
		"Using builtin defaults.\n", configfile);
	do_standard(scr);
	return;
    }
    
#ifdef __EMX__
    if ((fd = open(configfile, O_RDONLY|O_TEXT)) < 0)
#else
    if ((fd = open(configfile, O_RDONLY)) < 0)
#endif
    {
	yyerror("Cannot open configuration file '%s'.  "
		"Using builtin defaults.\n", configfile);
	do_standard(scr);
    }
    else {
	int bytes_read, statrc;
	struct stat st;

	if (debugging)
 	   fprintf(stderr, "Reading configuration from '%s'\n", configfile);

	statrc=stat(configfile, &st);
	ptr = XtMalloc(st.st_size + 1);
	bytes_read=read(fd, ptr, st.st_size);
	close(fd);

	skip_test = False;
	ptr[bytes_read] = '\0';
	PARSE_buf(scr, ptr);

	/* a certain amount seems to be expected as builtin */
	skip_test = True;

	PARSE_buf(scr, _MwmBuiltinButtonBindings);
	if (Mwm.w_menu_button_click)
	PARSE_buf(scr, _MwmBuiltinMenuButtonBindings);
	if (Mwm.w_menu_button_click_2)
	PARSE_buf(scr, _MwmBuiltinKillButtonBindings);
	if (Mwm.icon_click)
	PARSE_buf(scr, _MwmBuiltinIconButtonBindings);

	PROP_SetBehavior(scr, True);

	XtFree(ptr);
    }

    XtFree(configfile);
}

void
PARSE_mwmrc(ScreenInfo *scr) {
    int flag;

    key_bind_found = False;
    button_bind_found = False;
    key_bind_match = False;
    button_bind_match = False;

    if ((flag = PROP_GetBehavior(scr)) & MWM_INFO_STARTUP_CUSTOM) {
	if (debugging)
	    printf("parsing custom file.\n");
	do_custom(scr);
    }
    else if (flag & MWM_INFO_STARTUP_STANDARD) {
	if (debugging)
	    printf("parsing standard bindings.\n");
	do_standard(scr);
    }
    else {
	if (debugging)
	    printf("parsing custom file by default.\n");
	do_custom(scr);
    }

    return;
}
