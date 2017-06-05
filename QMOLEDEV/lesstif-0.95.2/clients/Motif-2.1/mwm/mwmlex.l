%{
/**
 *
 * $Id: mwmlex.l,v 1.1 2004/08/28 19:27:39 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#include <LTconfig.h>

#include <string.h>

#include <X11/Intrinsic.h>

#include "mwmparse.h"
#include "mwm.h"


/* for flex */
#ifdef FLEX_SCANNER
static void
lex_getc(char *b, int *r, int m)
{
    *b = mwm_getc();
    if (*b)
	*r = 1;
    else
	*r = 0;
}
#define YY_INPUT(b,r,m) lex_getc(b,&r,m)
#else
#undef input
#define input	mwm_getc
#undef output
#define output(c)
#undef unput
#define unput(c) mwm_unputc(c)
#endif
#undef ECHO
#define ECHO

static char *dupquot(char *str);
static char *dupstr(char *str);
%}
%option caseless
%e 1500
%p 4000
%a 3500

qstring		\"[^"]*\"
string		[A-Za-z0-9][A-Za-z0-9_\-\.]*
ws		[ \t\n\r]+
punct		[\{\}<>|@_\!-]
comment		^[\!].*
pcomment	\#.*
%%
{ws}			{ /* dump whitespace */ }
{comment}		{ /* dump comments */ }
{pcomment}		{ /* dump comments */ }
"within"		{ return WITHIN_TOK; }
"freeFamily"		{ return FREE_FAMILY_TOK; }
"next"			{ return NEXT_TOK; }
"prev"			{ return PREV_TOK; }
"back"			{ return BACK_TOK; }
"Menu"			{ return MENU_TOK; }
"Buttons"		{ return BUTTONS_TOK; }
"Keys"			{ return KEYS_TOK; }
"Key"			{ return KEY_TOK; }
"icon"			{ return ICON_TOK; }
"window"		{ return WINDOW_TOK; }
"transient"		{ return TRANSIENT_TOK; }
"root"			{ return ROOT_TOK; }
"frame"			{ return FRAME_TOK; }
"title"			{ return TITLE_TOK; }
"border"		{ return BORDER_TOK; }
"menub"			{ return MENUB_TOK; }
"minimizeb"		{ return MINIMIZEB_TOK; }
"maximizeb"		{ return MAXIMIZEB_TOK; }
"Ctrl"			{ return CTRL_TOK; }
"Shift"			{ return SHIFT_TOK; }
"Meta"			{ return ALT_TOK; }
"Alt"			{ return ALT_TOK; }
"Lock"			{ return LOCK_TOK; }
"Mod1"			{ return MOD1_TOK; }
"Mod2"			{ return MOD2_TOK; }
"Mod3"			{ return MOD3_TOK; }
"Mod4"			{ return MOD4_TOK; }
"Mod5"			{ return MOD5_TOK; }
"Btn1Down"		{ return BTN1_DOWN_TOK; }
"Btn1Up"		{ return BTN1_UP_TOK; }
"Btn1Click"		{ return BTN1_CLICK_TOK; }
"Btn1Click2"		{ return BTN1_CLICK2_TOK; }
"Btn2Down"		{ return BTN2_DOWN_TOK; }
"Btn2Up"		{ return BTN2_UP_TOK; }
"Btn2Click"		{ return BTN2_CLICK_TOK; }
"Btn2Click2"		{ return BTN2_CLICK2_TOK; }
"Btn3Down"		{ return BTN3_DOWN_TOK; }
"Btn3Up"		{ return BTN3_UP_TOK; }
"Btn3Click"		{ return BTN3_CLICK_TOK; }
"Btn3Click2"		{ return BTN3_CLICK2_TOK; }
"Btn4Down"		{ return BTN4_DOWN_TOK; }
"Btn4Up"		{ return BTN4_UP_TOK; }
"Btn4Click"		{ return BTN4_CLICK_TOK; }
"Btn4Click2"		{ return BTN4_CLICK2_TOK; }
"Btn5Down"		{ return BTN5_DOWN_TOK; }
"Btn5Up"		{ return BTN5_UP_TOK; }
"Btn5Click"		{ return BTN5_CLICK_TOK; }
"Btn5Click2"		{ return BTN5_CLICK2_TOK; }
"f.beep"		{ return FBEEP_TOK; }
"f.circle_down"		{ return FCIRCLE_DOWN_TOK; }
"f.circle_up"		{ return FCIRCLE_UP_TOK; }
"f.exec"		{ return FEXEC_TOK; }
"f.focus_color"		{ return FFOCUS_COLOR_TOK; }
"f.focus_key"		{ return FFOCUS_KEY_TOK; }
"f.kill"		{ return FKILL_TOK; }
"f.lower"		{ return FLOWER_TOK; }
"f.maximize"		{ return FMAXIMIZE_TOK; }
"f.menu"		{ return FMENU_TOK; }
"f.minimize"		{ return FMINIMIZE_TOK; }
"f.move"		{ return FMOVE_TOK; }
"f.next_cmap"		{ return FNEXT_CMAP_TOK; }
"f.next_key"		{ return FNEXT_KEY_TOK; }
"f.nop"			{ return FNOP_TOK; }
"f.normalize"		{ return FNORMALIZE_TOK; }
"f.normalize_and_raise"	{ return FNORMANDRAISE_TOK; }
"f.pack_icons"		{ return FPACK_ICONS_TOK; }
"f.pass_keys"		{ return FPASS_KEYS_TOK; }
"f.post_wmenu"		{ return FPOST_WMENU_TOK; }
"f.prev_cmap"		{ return FPREV_CMAP_TOK; }
"f.prev_key"		{ return FPREV_KEY_TOK; }
"f.quit_mwm"		{ return FQUIT_MWM_TOK; }
"f.raise"		{ return FRAISE_TOK; }
"f.raise_lower"		{ return FRAISE_LOWER_TOK; }
"f.refresh"		{ return FREFRESH_TOK; }
"f.refresh_win"		{ return FREFRESH_WIN_TOK; }
"f.resize"		{ return FRESIZE_TOK; }
"f.restart"		{ return FRESTART_TOK; }
"f.restore"		{ return FRESTORE_TOK; }
"f.restore_and_raise"	{ return FRESTOREANDRAISE_TOK; }
"f.screen"		{ return FSCREEN_TOK; }
"f.send_msg"		{ return FSEND_MSG_TOK; }
"f.separator"		{ return FSEPARATOR_TOK; }
"f.set_behavior"	{ return FSET_BEHAVIOR_TOK; }
"f.title"		{ return FTITLE_TOK; }
"f.window_list"		{ return FWINDOWLIST_TOK; }
"f.desk"		{ return FDESK_TOK; }
"f.toggle_page"		{ return FTOGGLE_PAGE_TOK; }
"f.goto_page"		{ return FGOTO_PAGE_TOK; }
{qstring}		{ yylval.string = dupquot(yytext); return STRING_TOK; }
{string}		{ yylval.string = dupstr(yytext); return STRING_TOK; }
{punct}			{ return yytext[0]; }
%%
#ifdef yywrap
#undef yywrap
#endif
int
yywrap()
{
    return 1;
}

static char *
dupquot(char *string) {
    char *tmp;

    tmp = XtMalloc(strlen(string) - 1);
    memcpy(tmp, string + 1, strlen(string) - 2);
    tmp[strlen(string) - 2] = 0;
    return tmp;
}

static char *
dupstr(char *string) {
    char *tmp;

    tmp = XtMalloc(strlen(string) + 1);
    memcpy(tmp, string, strlen(string));
    tmp[strlen(string)] = 0;
    return tmp;
}
