/*--------------------------------*-C-*---------------------------------*
 * File:	xdefaults.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1994        Robert Nation <nation@rocket.sanders.lockheed.com>
 * Copyright (c) 1997,1998   mj olesen <olesen@me.queensu.ca>
 * Copyright (c) 2004        Terry Griffin <griffint@pobox.com>
 * Copyright (c) 2005        Grant McDorman <grmcdorman@users.sourceforge.net>
 * Copyright (c) 2004-2005   Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: xdefaults.c,v 1.141 2005/06/20 20:31:03 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif



/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
#ifndef NO_RESOURCES
# ifdef KEYSYM_RESOURCE
Bool  rxvt_define_key (XrmDatabase*, XrmBindingList, XrmQuarkList, XrmRepresentation*, XrmValue*, XPointer);
int   rxvt_parse_keysym (rxvt_t*, const char*, const char*);
# endif
void  rxvt_parse_hotkeys (rxvt_t*, int, char*);
void  rxvt_delete_default_hotkeys (rxvt_t*);
# ifndef USE_XGETDEFAULT
void  rxvt_get_xdefaults (rxvt_t*, FILE*, const char*);
# else
char* get_xdefault_resource (XrmDatabase database, const char *name, char *subClass, char *fullClass, char *resource);
# endif
#endif
/*--------------------------------------------------------------------*
 *         END   `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/


static const char *const xnames[3] = {
	".mrxvtrc",
	".Xdefaults",
	".Xresources",
};

/*{{{ monolithic option/resource structure: */
/*
 * `string' options MUST have a usage argument
 * `switch' and `boolean' options have no argument
 * if there's no desc(ription), it won't appear in rxvt_usage()
 */

/* INFO() - descriptive information only */
#define INFO(opt, arg, desc)					\
	{0, -1, NULL, (opt), (arg), (desc), 0}

/* STRG() - command-line option, with/without resource */
#define STRG(rsp, kw, opt, arg, desc, multiple)				\
	{0, (rsp), (kw), (opt), (arg), (desc), (multiple)}

/* RSTRG() - resource/long-option */
#define RSTRG(rsp, kw, arg, multiple)					\
	{0, (rsp), (kw), NULL, (arg), NULL, (multiple)}

/* BOOL() - regular boolean `-/+' flag */
#define BOOL(rsp, kw, opt, flag, desc, multiple)				\
	{(Opt_Boolean|(flag)), (rsp), (kw), (opt), NULL, (desc), (multiple)}

/* SWCH() - `-' flag */
#define SWCH(opt, flag, desc, multiple)					\
	{(flag), -1, NULL, (opt), NULL, (desc), (multiple)}

/* convenient macros */
#define optList_STRLEN(i)						\
	(optList[i].flag ? 0 : (optList[i].arg ? STRLEN(optList[i].arg) : 1))
#define optList_isBool(i)					\
	(optList[i].flag & Opt_Boolean)
#define optList_isReverse(i)				\
	(optList[i].flag & Opt_Reverse)
#define optList_isMultiple(i)				\
	(optList[i].multiple)
#define optList_size()						\
	(sizeof(optList) / sizeof(optList[0]))

static const struct {
	const unsigned long flag;	/* Option flag */
	const int		doff;		/* data offset */
	const char*		kw;			/* keyword */
	const char*		opt;		/* option */
	const char*		arg;		/* argument */
	const char*		desc;		/* description */
	const char		multiple;	/* multiple values for VTs */
} optList[] = {
#ifdef BACKGROUND_IMAGE
	STRG(Rs_backgroundPixmap, "vt%d.Pixmap", "vt%d.pixmap", "file[;geom]", "background image for a tab", 1),
#endif
	STRG(Rs_tabtitle, "vt%d.tabTitle", "vt%d.tt", "string", "title name for tab", 1),
	STRG(Rs_saveLines, "vt%d.saveLines", "vt%d.sl", "number", "number of scrolled lines to save for tab", 1),
	STRG(Rs_command, "vt%d.command", "vt%d.e", "string", "command to execute for a tab", 1),
	STRG(Rs_color + TOTAL_COLORS, "vt%d.foreground",
		"vt%d.fg", "color", "foreground color for a tab", 1),
	STRG(Rs_color + TOTAL_COLORS + MAX_PAGES, "vt%d.background",
		"vt%d.bg", "color", "background color for a tab", 1),

#ifdef BACKGROUND_IMAGE
	/* Default pixmap for all tabs */
	STRG(Rs_backgroundPixmapAll, "Pixmap", "pixmap", "file[;geom]", "background image for all tabs", 1),
#endif
	/* Default tab title for all tabs */
	STRG(Rs_tabtitleAll, "tabTitle", "tt", "string", "title name for all tabs", 0),
	/* Default save lines for all tabs */
	STRG(Rs_saveLinesAll, "saveLines", "sl", "number", "number of scrolled lines to save for all tabs", 0),
	BOOL(Rs2_syncTabTitle, "syncTabTitle", "stt",
		Opt2_syncTabTitle, "synchronizing terminal title with tab title", 0),
	BOOL(Rs2_hideTabbar, "hideTabbar", "ht",
		Opt2_hideTabbar, "hiding tabbar on initialization", 0),
	BOOL(Rs2_bottomTabbar, "bottomTabbar", "bt",
		Opt2_bottomTabbar, "showing tabbar at bottom", 0),
	BOOL(Rs2_hideButtons, "hideButtons", "hb",
		Opt2_hideButtons, "hide buttons on tabbar", 0),
	BOOL(Rs2_syncTabIcon, "syncTabIcon", "sti",
		Opt2_syncTabIcon, "synchronizing icon name with tab title", 0),
	BOOL(Rs2_veryBold, "veryBoldFont", "vbf",
		Opt2_veryBold, "showing color text with bold font", 0),


	STRG(Rs_display_name, NULL, "d", NULL, NULL, 0),
	STRG(Rs_display_name, NULL, "display", "string", "X server to contact", 0),
	STRG(Rs_term_name, "termName", "tn", "string", "the TERM environment variable", 0),
	STRG(Rs_title, "title", "t", "string", "title name for window", 0),
	STRG(Rs_geometry, NULL, "g", NULL, NULL, 0),
	STRG(Rs_geometry, "geometry", "geometry", "geometry", "size (in characters) and position", 0),
	SWCH("C", Opt_console, "intercept console messages", 0),
	SWCH("iconic", Opt_iconic, "start iconic", 0),
	SWCH("ic", Opt_iconic, NULL, 0),
	BOOL(Rs_reverseVideo, "reverseVideo", "rv", Opt_reverseVideo, "reverse video", 0),
	BOOL(Rs_loginShell, "loginShell", "ls", Opt_loginShell, "login shell", 0),
	BOOL(Rs_jumpScroll, "jumpScroll", "j", Opt_jumpScroll, "jump scrolling", 0),

#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
	STRG(Rs_backgroundFade, "backgroundFade", "bgfade", "%",
		"fade background image or transparent background by %x", 0),
# ifdef TINTING_SUPPORT
	STRG(Rs_color + Color_tint, "tintColor", "tint", "color", "tint color", 0),
	STRG(Rs_shade, "shading", "shade", "%", "shade background by %x when tinting", 0),
# endif
#endif

#ifdef OFF_FOCUS_FADING
	STRG(Rs_fade, "fading", "fade", "%", "make colors x% darker when losing focus", 0),
#endif

#ifdef HAVE_SCROLLBARS
	BOOL(Rs_scrollBar, "scrollBar", "sb", Opt_scrollBar, "scrollbar", 0),
	BOOL(Rs_scrollBar_right, "scrollbarRight", "sr",
		Opt_scrollBar_right, "scrollbar right", 0),
	BOOL(Rs_scrollBar_floating, "scrollbarFloating", "st",
		Opt_scrollBar_floating, "scrollbar without a trough", 0),
	STRG(Rs_scrollBar_thickness, "scrollbarThickness", "sbt", "number",
		"scrollbar thickness/width in pixels", 0),
	STRG(Rs_scrollBar_style, "scrollbarStyle", "ss", "mode",
		"scrollbar style = plain|xterm|rxvt|next|sgi", 0),
	STRG(Rs_scrollBar_align, "scrollbarAlign", "sa", "mode", 
		"scrollbar alignment = top|bottom", 0),
# ifdef BACKGROUND_IMAGE
	STRG(Rs_scrollbarPixmap, "scrollbarPixmap", "sbpixmap",
		"file[;geom]", "scrollbar background image", 0),
# endif
#endif
	BOOL(Rs_scrollTtyOutputInhibit, "scrollTtyOutputInhibit", "si",
		Opt_scrollTtyOutputInhibit, "scroll-on-tty-output inhibit", 0),
	BOOL(Rs_scrollTtyKeypress, "scrollTtyKeypress", "sk",
		Opt_scrollTtyKeypress, "scroll-on-keypress", 0),
	BOOL(Rs_scrollWithBuffer, "scrollWithBuffer", "sw",
		Opt_scrollWithBuffer, "scroll-with-buffer", 0),

	STRG(Rs_opacity, "opacity", "o", "%",
		"transluscent window (true transparent) opaque degree", 0),
	STRG(Rs_opacityDegree, "opacityDegree", "od", "%",
		"transluscent window opaque degree interval", 0),

#ifdef TRANSPARENT
	BOOL(Rs_transparent, "transparent", "tr", Opt_transparent, "transparent", 0),
	BOOL(Rs_transparent_all, "transparentforce", "trf",
		Opt_transparent_all, "forcefully transparent", 0),
	SWCH("tr", Opt_transparent, NULL, 0),
# ifdef HAVE_SCROLLBARS
	BOOL(Rs_transparent_scrollbar, "transparentScrollbar", "trs",
		Opt_transparent_scrollbar, "transparent scrollbar", 0),
# endif
# ifdef HAVE_MENUBAR
	BOOL(Rs_transparent_menubar, "transparentMenubar", "trm",
		Opt_transparent_menubar, "transparent menubar", 0),
# endif
	BOOL(Rs_transparent_tabbar, "transparentTabbar", "trt",
		Opt_transparent_tabbar, "transparent tabbar", 0),
#endif	/* TRANSPARENT */

#ifdef BACKGROUND_IMAGE
	STRG(Rs_tabbarPixmap, "tabbarPixmap", "tbpixmap", "file[;geom]", "tabbar background image", 0),
	BOOL(Rs_tabPixmap, "tabUsePixmap", "tupixmap", Opt_tabPixmap, "use tabbar background image for tabs", 0),
	STRG(Rs_appIcon, "appIcon", "ic", "file[;geom]", "application icon file", 1),
#endif	/* BACKGROUND_IMAGE */

	BOOL(Rs_utmpInhibit, "utmpInhibit", "ut", Opt_utmpInhibit,
		"utmp inhibit - do not log to utmp", 0),
	STRG(Rs_confFile, NULL, "cf", "file",
		"X resource configuration file instead of ~/.mrxvtrc", 0),
	STRG(Rs_confFileSave, "confFileSave", "cfs", "file",
		"X resource configuration file to save the configuration", 0),

#ifndef NO_BELL
	BOOL(Rs_visualBell, "visualBell", "vb",
		Opt_visualBell, "visual bell", 0),
# if ! defined(NO_MAPALERT) && defined(MAPALERT_OPTION)
	BOOL(Rs_mapAlert, "mapAlert", NULL, Opt_mapAlert, NULL, 0),
# endif
#endif
#ifdef META8_OPTION
	BOOL(Rs_meta8, "meta8", "m8", Opt_meta8, "meta8", 0),
#endif
#ifdef MOUSE_WHEEL
	BOOL(Rs_mouseWheelScrollPage, "mouseWheelScrollPage", "mp",
		Opt_mouseWheelScrollPage, "mouse wheel scrolling a page", 0),
#endif
#ifdef MULTICHAR_SET
	BOOL(Rs_mc_hack, "multibyte_cursor", "mcc", Opt_mc_hack,
		"Multibyte character cursor movement", 0),
#endif
#ifndef NO_FRILLS
	BOOL(Rs_tripleclickwords, "tripleclickwords", "tcw", Opt_tripleclickwords, "triple click word selection", 0),
#endif
	STRG(Rs_color + Color_bg, "background", "bg", "color", "background color", 0),
	STRG(Rs_color + Color_fg, "foreground", "fg", "color", "foreground color", 0),
	STRG(Rs_color + Color_ufbg, "ufBackground", "ufbg", "color", "unfocused background color", 0),
#ifdef TEXT_SHADOW
	STRG(Rs_textShadow, "textShadow", "ts", "color", "text shadow color", 0),
	STRG(Rs_textShadowMode, "textShadowMode", "tsm", "mode",
		"shadow mode = "
		"top|bottom|left|right|topleft|topright|botleft|botright", 0),
#endif
	STRG(Rs_tabfg, "tabForeground", "tabfg", "color", "tabbar active tab foreground color", 0),
	STRG(Rs_tabbg, "tabBackground", "tabbg", "color", "tabbar and active tab background color", 0),
	STRG(Rs_itabfg, "itabForeground", "itabfg", "color", "tabbar inactive tab foreground color", 0),
	STRG(Rs_itabbg, "itabBackground", "itabbg", "color", "tabbar inactive tab background color", 0),
	RSTRG(Rs_color + minCOLOR + 0, "color0", "color", 0),
	RSTRG(Rs_color + minCOLOR + 1, "color1", "color", 0),
	RSTRG(Rs_color + minCOLOR + 2, "color2", "color", 0),
	RSTRG(Rs_color + minCOLOR + 3, "color3", "color", 0),
	RSTRG(Rs_color + minCOLOR + 4, "color4", "color", 0),
	RSTRG(Rs_color + minCOLOR + 5, "color5", "color", 0),
	RSTRG(Rs_color + minCOLOR + 6, "color6", "color", 0),
	RSTRG(Rs_color + minCOLOR + 7, "color7", "color", 0),
#ifndef NO_BRIGHTCOLOR
	RSTRG(Rs_color + minBrightCOLOR + 0, "color8", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 1, "color9", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 2, "color10", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 3, "color11", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 4, "color12", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 5, "color13", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 6, "color14", "color", 0),
	RSTRG(Rs_color + minBrightCOLOR + 7, "color15", "color", 0),
#endif				/* NO_BRIGHTCOLOR */
#ifndef NO_BOLD_UNDERLINE_REVERSE
	RSTRG(Rs_color + Color_BD, "colorBD", "color", 0),
	RSTRG(Rs_color + Color_UL, "colorUL", "color", 0),
	RSTRG(Rs_color + Color_RV, "colorRV", "color", 0),
#endif				/* ! NO_BOLD_UNDERLINE_REVERSE */
#ifdef KEEP_SCROLLCOLOR
	RSTRG(Rs_color + Color_scroll, "scrollColor", "color", 0),
	RSTRG(Rs_color + Color_trough, "troughColor", "color", 0),
#endif				/* KEEP_SCROLLCOLOR */
#ifdef OPTION_HC
	STRG(Rs_color + Color_HC, "highlightColor", "hc", "color", "highlight color", 0),
#endif
#ifndef NO_CURSORCOLOR
	STRG(Rs_color + Color_cursor, "cursorColor", "cr", "color", "cursor color", 0),
	RSTRG(Rs_color + Color_cursor2, "cursorColor2", "color", 0),
#endif				/* NO_CURSORCOLOR */
	STRG(Rs_color + Color_pointer, "pointerColor", "pr", "color", "pointer color", 0),
	STRG(Rs_color + Color_border, "borderColor", "bd", "color", "border color", 0),

#if defined (BACKGROUND_IMAGE) || (MENUBAR_MAX)
	RSTRG(Rs_path, "path", "search path", 0),
#endif				/* defined (BACKGROUND_IMAGE) || (MENUBAR_MAX) */
#if (MENUBAR_MAX)
	STRG(Rs_menu, "menu", "menu",
		"filename[;tag]", "menubar definition file", 0),
#endif
#ifdef HAVE_MENUBAR
	BOOL(Rs_showMenu, "showMenu", "showmenu", Opt_showMenu, "show menubar", 0),
# ifdef BACKGROUND_IMAGE
	STRG(Rs_menubarPixmap, "menubarPixmap", "mbpixmap",
		"file[;geom]", "menubar background image", 0),
# endif
#endif

#ifndef NO_BOLDFONT
	STRG(Rs_boldFont, "boldFont", "fb", "fontname", "bold text font", 0),
#endif

	STRG(Rs_font + 0, "font", "fn", "fontname", "normal text font", 0),
#if MAX_NFONTS > 1
	RSTRG(Rs_font + 1, "font1", "fontname", 0),
#endif
#if MAX_NFONTS > 2
	RSTRG(Rs_font + 2, "font2", "fontname", 0),
#endif
#if MAX_NFONTS > 3
	RSTRG(Rs_font + 3, "font3", "fontname", 0),
#endif
#if MAX_NFONTS > 4
	RSTRG(Rs_font + 4, "font4", "fontname", 0),
#endif
#if MAX_NFONTS > 5
	RSTRG(Rs_font + 5, "font5", "fontname", 0),
#endif
#if MAX_NFONTS > 6
	RSTRG(Rs_font + 6, "font6", "fontname", 0),
#endif
#if MAX_NFONTS > 7
	RSTRG(Rs_font + 7, "font7", "fontname", 0),
#endif
#ifdef MULTICHAR_SET
	STRG(Rs_mfont + 0, "mfont", "fm", "fontname", "multichar font", 0),
# if MAX_NFONTS > 1
	RSTRG(Rs_mfont + 1, "mfont1", "fontname", 0),
# endif
# if MAX_NFONTS > 2
	RSTRG(Rs_mfont + 2, "mfont2", "fontname", 0),
# endif
# if MAX_NFONTS > 3
	RSTRG(Rs_mfont + 3, "mfont3", "fontname", 0),
# endif
# if MAX_NFONTS > 4
	RSTRG(Rs_mfont + 4, "mfont4", "fontname", 0),
# endif
# if MAX_NFONTS > 5
	RSTRG(Rs_mfont + 5, "mfont5", "fontname", 0),
# endif
# if MAX_NFONTS > 6
	RSTRG(Rs_mfont + 6, "mfont6", "fontname", 0),
# endif
# if MAX_NFONTS > 7
	RSTRG(Rs_mfont + 7, "mfont7", "fontname", 0),
# endif
#endif				/* MULTICHAR_SET */

#ifdef XFT_SUPPORT
	BOOL(Rs_xft, "xft", "xft", Opt_xft, "use freetype font", 0),
	STRG(Rs_xftfont, "xftFont", "xftfn", "fontname", "freetype font", 0),
# ifdef MULTICHAR_SET
	STRG(Rs_xftmfont, "xftmFont", "xftfm", "fontname", "freetype multichar font", 0),
	STRG(Rs_xftmsz, "xftmSize", "xftmsz", "number",
		"freetype multichar font size", 0),
	BOOL(Rs2_xftNomFont, "xftNomFont", "xftnfm", Opt2_xftNomFont,
		"use freetype font as freetype mfont", 0),
	BOOL(Rs2_xftSlowOutput, "xftSlow", "xftslow", Opt2_xftSlowOutput,
		"multichar string display in slow mode for better visual effect", 0),
# endif
	BOOL(Rs2_xftaa, "xftAntialias", "xftaa", Opt2_xftAntialias,
		"antialias of freetype font", 0),
	BOOL(Rs2_xftht, "xftHinting", "xftht", Opt2_xftHinting,
		"hinting of freetype font", 0),
	BOOL(Rs2_xftah, "xftAutoHint", "xftah", Opt2_xftAutoHint,
		"autohint of freetype font", 0),
	BOOL(Rs2_xftga, "xftGlobalAdvance", "xftga", Opt2_xftGlobalAdvance,
		"global advance of freetype font", 0),
	STRG(Rs_xftwt, "xftWeight", "xftwt", "style",
		"weight style = light|medium|bold", 0),
	STRG(Rs_xftst, "xftSlant", "xftst", "style",
		"slant style = roman|italic|oblique", 0),
	STRG(Rs_xftsz, "xftSize", "xftsz", "number",
		"freetype font size", 0),
	STRG(Rs_xftwd, "xftWidth", "xftwd", "style",
		"freetype font width = "
		"ultracondensed|condensed|normal|expanded|ultraexpended", 0),
	STRG(Rs_xftrgb, "xftRGBA", "xftrgb", "style",
		"freetype font sub-pixel order = rgb|bgr|vrgb|vbgr|none", 0),
#endif

	BOOL(Rs2_tabShell, "tabShell", "sh",
		Opt2_tabShell, "running shell command for all tabs", 0),
	BOOL(Rs2_cmdAllTabs, "cmdAllTabs", "at",
		Opt2_cmdAllTabs, "running -e command for all tabs", 0),
	BOOL(Rs2_cmdInitTabs, "cmdInitTabs", "it",
		Opt2_cmdInitTabs, "loading tab command only on initialization", 0),
	BOOL(Rs2_protectSecondary, "protectSecondary", "ps",
		Opt2_protectSecondary, "protecting tab that uses the secondary screen from being closed", 0),

#ifdef MULTICHAR_SET
	STRG(Rs_multichar_encoding, "multichar_encoding", "km", "mode",
		"multichar encoding mode = eucj|sjis|big5|gb|gbk|kr|noenc", 0),
#endif				/* MULTICHAR_SET */

#ifdef USE_XIM
	STRG(Rs_inputMethod, "inputMethod", "im", "name", "name of input method", 0),
	STRG(Rs_preeditType, "preeditType", "pt", "style",
		"input style = OverTheSpot|OffTheSpot|Root", 0),
#endif				/* USE_XIM */

#ifdef THAI
	BOOL(Rs_thai, "thai", "thai", Opt_thai, "enable thai support", 0),
#endif

#ifdef GREEK_SUPPORT
	STRG(Rs_greek_keyboard, "greek_keyboard", "grk", "mode",
		"greek keyboard mapping mode = iso|ibm", 0),
	RSTRG(Rs_greektoggle_key, "greektoggle_key", "keysym", 0),
#endif

	STRG(Rs_name, "clientName", "name", "string",
		"client instance, icon, and title strings", 0),
	STRG(Rs_title, NULL, "T", NULL, NULL, 0),	/* short form */
	STRG(Rs_iconName, "iconName", "n", "string",
		"icon name for window", 0),
	BOOL(Rs2_borderLess, "borderLess", "bl", Opt2_borderLess,
		"borderless window", 0),
	BOOL(Rs2_overrideRedirect, "overrideRedirect", "or",
		Opt2_overrideRedirect, "override_redirect flag", 0),
	STRG(Rs_bellCommand, "bellCommand", "blc",
		"string", "command to execute instead of beeping", 0),
	BOOL(Rs2_holdExit, "holdExit", "hold",
		Opt2_holdExit, "hold after terminal exits", 0),
	STRG(Rs_holdExitText, "holdExitText", "het",
		"string", "text to show while holding the terminal", 0),
	STRG(Rs_desktop, "desktop", "desktop",
		"number", "desktop to place the program", 0),
	BOOL(Rs2_broadcast, "broadcast", "bcst",
		Opt2_broadcast, "broadcast input to all terminals", 0),

#ifndef NO_FRILLS
	STRG(Rs_ext_bwidth, "externalBorder", "w", "number",
		"external border in pixels", 0),
	STRG(Rs_ext_bwidth, NULL, "bw", NULL, NULL, 0),
	STRG(Rs_ext_bwidth, NULL, "borderwidth", NULL, NULL, 0),
	STRG(Rs_int_bwidth, "internalBorder", "b", "number",
		"internal border in pixels", 0),
#endif
#ifndef NO_LINESPACE
	STRG(Rs_lineSpace, "lineSpace", "lsp", "number", "number of extra pixels between rows", 0),
#endif

#ifdef POINTER_BLANK
	BOOL(Rs_pointerBlank, "pointerBlank", "pb", Opt_pointerBlank, 
		"blank pointer", 0),
	RSTRG(Rs_pointerBlankDelay, "pointerBlankDelay", "number", 0),
#endif
#ifdef CURSOR_BLINK
	BOOL(Rs_cursorBlink, "cursorBlink", "bc", Opt_cursorBlink, "blinking cursor", 0),
	STRG(Rs_cursorBlinkInterval, "cursorBlinkInterval", "bci",
		"number", "cursor blinking interval (ms)", 0),
#endif

#ifndef NO_BACKSPACE_KEY
	RSTRG(Rs_backspace_key, "backspaceKey", "string", 0),
#endif

#ifndef NO_DELETE_KEY
	RSTRG(Rs_delete_key, "deleteKey", "string", 0),
#endif

	RSTRG(Rs_selectstyle, "selectStyle",
		"select style mode = old|oldmode", 0),

#ifdef PRINTPIPE
	RSTRG(Rs_print_pipe, "printPipe", "string", 0),
#endif

#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
	RSTRG(Rs_bigfont_key, "bigFontKey", "keysym", 0),
	RSTRG(Rs_smallfont_key, "smallFontKey", "keysym", 0),
#endif

	/*
	** Begin of hotkey resources. It MUST be synchronized to the
	** definition of HK_FUNCS!!!
	*/
	RSTRG(Rs_hotkey + HKF_DUMMY, "hotkey.Dummy", "string", 0),
	RSTRG(Rs_hotkey + HKF_CHANGE_TITLE, "hotkey.ChangeTitle", "string", 0),
	RSTRG(Rs_hotkey + HKF_NEW_TAB, "hotkey.NewTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_KILL_TAB, "hotkey.KillTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_PREV_TAB, "hotkey.PrevTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_NEXT_TAB, "hotkey.NextTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_PREV_ATAB, "hotkey.PrevActiveTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_1, "hotkey.Tab1", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_2, "hotkey.Tab2", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_3, "hotkey.Tab3", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_4, "hotkey.Tab4", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_5, "hotkey.Tab5", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_6, "hotkey.Tab6", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_7, "hotkey.Tab7", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_8, "hotkey.Tab8", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_9, "hotkey.Tab9", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_10, "hotkey.Tab10", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_11, "hotkey.Tab11", "string", 0),
	RSTRG(Rs_hotkey + HKF_TAB_12, "hotkey.Tab12", "string", 0),
	RSTRG(Rs_hotkey + HKF_LMOVE_TAB, "hotkey.LeftMoveTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_RMOVE_TAB, "hotkey.RightMoveTab", "string", 0),
	RSTRG(Rs_hotkey + HKF_DUMP_SCREEN, "hotkey.DumpScreen", "string", 0),
	RSTRG(Rs_hotkey + HKF_INC_OPACITY, "hotkey.IncOpacity", "string", 0),
	RSTRG(Rs_hotkey + HKF_DEC_OPACITY, "hotkey.DecOpacity", "string", 0),
	RSTRG(Rs_hotkey + HKF_TRANSPARENCY, "hotkey.Transparency", "string", 0),
	RSTRG(Rs_hotkey + HKF_HIDE_TABBAR, "hotkey.HideTabbar", "string", 0),
	RSTRG(Rs_hotkey + HKF_HIDE_SCROLLBAR, "hotkey.HideScrollbar", "string", 0),
	RSTRG(Rs_hotkey + HKF_HIDE_MENUBAR, "hotkey.HideMenubar", "string", 0),
	RSTRG(Rs_hotkey + HKF_HIDE_BUTTON, "hotkey.HideButton", "string", 0),
	RSTRG(Rs_hotkey + HKF_VERYBOLD, "hotkey.VeryBold", "string", 0),
	RSTRG(Rs_hotkey + HKF_HOLD_EXIT, "hotkey.HoldExit", "string", 0),
	RSTRG(Rs_hotkey + HKF_BROADCAST, "hotkey.Broadcast", "string", 0),
	RSTRG(Rs_hotkey + HKF_SMALL_FONT, "hotkey.SmallFont", "string", 0),
	RSTRG(Rs_hotkey + HKF_LARGE_FONT, "hotkey.LargeFont", "string", 0),
	RSTRG(Rs_hotkey + HKF_SCROLL_UP, "hotkey.ScrollUp", "string", 0),
	RSTRG(Rs_hotkey + HKF_SCROLL_DOWN, "hotkey.ScrollDown", "string", 0),
	RSTRG(Rs_hotkey + HKF_SCROLL_PGUP, "hotkey.ScrollPageUp", "string", 0),
	RSTRG(Rs_hotkey + HKF_SCROLL_PGDOWN, "hotkey.ScrollPageDown", "string", 0),
	RSTRG(Rs_hotkey + HKF_SAVE_CONFIG, "hotkey.SaveConfig", "string", 0),
	RSTRG(Rs_hotkey + HKF_COPY_SEL, "hotkey.CopySel", "string", 0),
	RSTRG(Rs_hotkey + HKF_PASTE_SEL, "hotkey.PasteSel", "string", 0),
	/* end of hotkey resources */
	BOOL(Rs2_disableHotkeys, "disableHotkeys", "dh",
		Opt2_disableHotkeys, "disabling all keyboard shortcuts", 0),
	BOOL(Rs2_disableDefaultHotkeys, "disableDefaultHotkeys", "ddh",
		Opt2_disableDefaultHotkeys, "disabling default keyboard shortcuts", 0),
	BOOL(Rs2_linuxHomeEndKey, "linuxHomeEndKey", "lk",
		Opt2_linuxHomeEndKey, "enable Linux console Home/End keys", 0),

	STRG(Rs_modifier, "modifier", "mod", "modifier",
		"meta modifier = alt|meta|hyper|super|mod1|...|mod5", 0),
	INFO("xrm", "string", "X resource"),

#ifdef CUTCHAR_RESOURCE
	RSTRG(Rs_cutchars, "cutChars", "string", 0),
#endif	/* CUTCHAR_RESOURCE */

#ifdef ACS_ASCII
	RSTRG(Rs_acs_chars, "acsChars", "string", 0),
#endif	/* ACS_ASCII */

	RSTRG(Rs_answerbackstring, "answerbackString", "string", 0),

#ifdef HAVE_X11_SM_SMLIB_H
	BOOL(Rs2_enableSessionMgt, "sessionMgt", "sm",
		Opt2_enableSessionMgt, "enabling X session management", 0),
	STRG(Rs_smClientID, "smClientID", "sid", "string",
		"client id of mrxvt for X session management", 0),
#endif	/* HAVE_X11_SM_SMLIB_H */

	/* Initial number of terminals */
	STRG(Rs_init_term_num, "initTermNumber", "tnum", "number",
		"Initial number of tabs/terminals", 0),
	INFO("e", "command arg ...", "command to execute")
};

#undef INFO
#undef STRG
#undef RSTRG
#undef SWCH
#undef BOOL
/*}}} */

static const char releasestring[] = "Mrxvt v" VERSION "\n";
static const char optionsstring[] = "Options: "
#if defined(BACKGROUND_IMAGE)
# ifdef HAVE_LIBXPM
	"XPM,"
# endif
# ifdef USE_JPEG
	"Jpeg,"
# endif
# ifdef USE_JPEG
	"PNG,"
# endif
#endif
#if defined(TRANSPARENT)
	"transparent,"
#endif
#if defined(OFF_FOCUS_FADING)
	"fade,"
#endif
#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
# if defined(TINTING_SUPPORT)
	"tint,"
# endif
#endif
#if defined(TEXT_SHADOW)
	"textshadow,"
#endif
#if defined(UTMP_SUPPORT)
	"utmp,"
#endif
#if defined(HAVE_MENUBAR)
	"menubar,"
#endif
#if defined(USE_XIM)
	"XIM,"
#endif
#if defined(MULTICHAR_SET)
	"multichar_languages,"
#endif
	"scrollbars="
#if !defined(HAVE_SCROLLBARS)
	"NONE"
#else
# if defined(RXVT_SCROLLBAR)
	"rxvt"
#  if defined(NEXT_SCROLLBAR) || defined(XTERM_SCROLLBAR) || defined(SGI_SCROLLBAR) || defined(PLAIN_SCROLLBAR)
	"+"
#  endif
# endif	/* RXVT_SCROLLBAR */
# if defined(NEXT_SCROLLBAR)
	"NeXT"
#  if defined(XTERM_SCROLLBAR) || defined(SGI_SCROLLBAR) || defined(PLAIN_SCROLLBAR)
	"+"
#  endif
# endif	/* NEXT_SCROLLBAR */
# if defined(XTERM_SCROLLBAR)
	"xterm"
#  if defined(SGI_SCROLLBAR) || defined(PLAIN_SCROLLBAR)
    "+"
#  endif
# endif	/* XTERM_SCROLLBAR */
# if defined(SGI_SCROLLBAR)
	"sgi"
#  if defined(PLAIN_SCROLLBAR)
	"+"
#  endif
# endif	/* SGI_SCROLLBAR */
# if defined(PLAIN_SCROLLBAR)
	"plain"
# endif	/* PLAIN_SCROLLBAR */
#endif	/* HAVE_SCROLLBARS */
	","
#ifdef XFT_SUPPORT
	"xft,"
#endif
#ifdef THAI
	"thai,"
#endif
#if defined(GREEK_SUPPORT)
	"Greek,"
#endif
#if defined(NO_BACKSPACE_KEY)
	"no_backspace,"
#endif
#if defined(NO_DELETE_KEY)
	"no_delete,"
#endif
#ifdef OUR_STRINGS
	"strings,"
#endif
#if !defined(NO_FRILLS)
	"frills,"
#endif
#if !defined(NO_LINESPACE)
	"linespace,"
#endif
#if defined(PREFER_24BIT)
	"24bit,"
#endif
#if defined(SELECTION_SCROLLING)
	"selectionscrolling,"
#endif
#if defined(SMART_RESIZE)
	"smart-resize,"
#endif
#if defined(TTY_256COLOR)
	"256colour,"
#endif
#if defined(CURSOR_BLINK)
	"cursorBlink,"
#endif
#if defined(POINTER_BLANK)
	"pointerBlank,"
#endif
#ifdef HAVE_X11_SM_SMLIB_H
	"session management,"
#endif
#if defined(NO_RESOURCES)
	"NoResources"
#else
# if defined(USE_XGETDEFAULT)
	"XGetDefaults"
# else
	".Xdefaults"
# endif
#endif
	"\nUsage: ";		/* Usage */


#define INDENT 24


/*{{{ usage: */
/*----------------------------------------------------------------------*/
/* EXTPROTO */
void
rxvt_usage(int type)
{
	unsigned int	i, col;

	write(STDOUT_FILENO, releasestring, sizeof(releasestring) - 1);
	write(STDOUT_FILENO, optionsstring, sizeof(optionsstring) - 1);
	write(STDOUT_FILENO, APL_NAME, sizeof(APL_NAME) - 1);

	switch (type) {
	case 0:			/* brief listing */
		fprintf(stdout, " [-help] [--help]\n");
		for (col = 1, i = 0; i < optList_size(); i++)
			if (optList[i].desc != NULL) {
				int			 len = 0;

				if (!optList_isBool(i)) {
					len = optList_STRLEN(i);
					if (len > 0)
						len++;	/* account for space */
				}
				assert(optList[i].opt != NULL);

				len += 4 + STRLEN(optList[i].opt) +
					(optList_isBool(i) ? 2: 0);
				col += len;
				if (col > 79) {	/* assume regular width */
					putc('\n', stdout);
					col = 1 + len;
				}
				fprintf(stdout, " [-%s%s", (optList_isBool(i) ?
					"/+" : ""), optList[i].opt);
				if (optList_STRLEN(i))
					fprintf(stdout, " %s]", optList[i].arg);
				else
					fprintf(stdout, "]");
			}
		break;

	case 1:			/* full command-line listing */
		fprintf(stdout, " [options] [-e command args]\n\n"
			"where options include:\n");
		for (i = 0; i < optList_size(); i++)
			if (optList[i].desc != NULL) {
				assert(optList[i].opt != NULL);

				fprintf(stdout, "  %s%s %-*s%s%s\n",
					(optList_isBool(i) ? "-/+" : "-"), optList[i].opt,
					(INDENT - STRLEN(optList[i].opt)
					 + (optList_isBool(i) ? 0 : 2)),
					(optList[i].arg ? optList[i].arg : ""),
					(optList_isBool(i) ? "turn on/off " : ""),
					optList[i].desc);
			}
		fprintf(stdout, "\n  --help to list long-options");
		break;

	case 2:			/* full resource listing */
		fprintf(stdout,
			" [options] [-e command args]\n\n"
			"where resources (long-options) include:\n");

		for (i = 0; i < optList_size(); i++)
			if (optList[i].kw != NULL)
				fprintf(stdout, "  %s: %*s%s\n",
					optList[i].kw,
					(INDENT - STRLEN(optList[i].kw)), "", /* XXX */
					(optList_isBool(i) ? "boolean" : optList[i].arg));
#ifdef KEYSYM_RESOURCE
		fprintf(stdout, "  " "keysym.sym" ": %*s%s\n",
			(INDENT - sizeof("keysym.sym") + 1), "", /* XXX */
			"keysym");
#endif
		fprintf(stdout, "\n  -help to list options");
		break;
	}	/* switch */

	fprintf(stdout, "\n\n");
	exit(EXIT_FAILURE);
	/* NOTREACHED */
}
/*}}} */



extern hotkeys_handler_t   hk_handlers[NUM_HKFUNCS];

/* EXTPROTO */
int
rxvt_save_options (rxvt_t* r, const char* filename)
{
	int		i;
	FILE*	pf = fopen (filename, "w");

	if (NULL == pf)
		return 0;
	
	for (i = 0; i < optList_size(); i ++)	{
		if (NULL == optList[i].kw)
			continue;
		if (-1 == optList[i].doff)
			continue;

		if (optList[i].multiple &&
			rxvt_str_match(optList[i].kw, "vt%d."))	{
			register int	j;

			for (j = 0; j < MAX_PAGES; j ++)	{
				if (r->h->rs[optList[i].doff + j])
					fprintf (pf, "%s*vt%d*%s:\t\t\t%s\n", APL_NAME,
						j, optList[i].kw + STRLEN("vt%d."),
						r->h->rs[optList[i].doff +j]);
			}
		}
		else if (optList_isBool(i)) {
			int		bval;
			char*	OnOff[2] = {"False", "True"};

			/* do not save internal features */
			if (Rs2_disableDefaultHotkeys == optList[i].doff)
				continue;

			if (optList[i].doff < Rs_options2)
				bval = (r->Options & optList[i].flag) ? 1 : 0;
			else
				bval = (r->Options2 & optList[i].flag) ? 1 : 0;
			if (optList_isReverse(i))
				bval = !bval;
			fprintf (pf, "%s*%s:\t\t\t%s\n", APL_NAME,
				optList[i].kw, OnOff[bval]);
		}
		else if (r->h->rs[optList[i].doff])	{
			fprintf (pf, "%s*%s:\t\t\t%s\n", APL_NAME,
				optList[i].kw, r->h->rs[optList[i].doff]);
		}
	}

	/* parse all hotkeys */
	for (i = 0; i < MAX_HOTKEYS; i ++)	{
		char		buf[64] = "";
		hotkeys_t*	phk;

		if ((r->Options2 & Opt2_disableHotkeys) && 0 == i)
			phk = &(r->hotkeys[MAX_HOTKEYS]);
		else
			phk = &(r->hotkeys[i]);
		if (HKF_DUMMY == phk->func)
			break;		/* find last valid entry */
		if (HK_IS_INTERNAL (phk->flag))
			continue;	/* ignore internal keyboards */

		if (HK_IS_CTRL (phk->flag))
			STRCAT (buf, "ctrl+");
		if (HK_IS_META (phk->flag))
			STRCAT (buf, "meta+");
		if (HK_IS_SHFT (phk->flag))
			STRCAT (buf, "shift+");
		if (HK_IS_PRIMARY (phk->flag))
			STRCAT (buf, "primary+");
		STRNCAT (buf, XKeysymToString (phk->keysym),
			sizeof(buf) - 1 - STRLEN(buf));
		buf[sizeof(buf) - 1] = (char) 0;

		/* find the first hotkey resource entry */
		fprintf (pf, "%s*%s:\t\t\t%s\n", APL_NAME,
			hk_handlers[phk->func].res, buf);
	}

	fclose (pf);
	return 1;
}


/* INTPROTO */
void
rxvt_delete_default_hotkeys (rxvt_t* r)
{
	register int	i, beg;

	for (i = 0, beg = 0; i < MAX_HOTKEYS; i ++)	{
		if (HKF_DUMMY == r->hotkeys[i].func)
			break;	/* find last valid hotkey */

		if (HK_IS_INTERNAL (r->hotkeys[i].flag))
			continue;	/* skip internal hotkey */
		else /* this is not an internal hotkey, move it forward */
			r->hotkeys[beg++] = r->hotkeys[i];
	}

	/* clear the empty slots */
	for (; beg < i; beg ++)	{
		r->hotkeys[beg].func = HKF_DUMMY;
		r->hotkeys[beg].flag = 0;
		r->hotkeys[beg].keysym = 0;
	}
}


/*{{{ get command-line options before getting resources */
/* EXTPROTO */
void
rxvt_get_options(rxvt_t *r, int argc, const char *const *argv)
{
	int			 i, bad_option = 0;
	static const char On[3] = "ON", Off[4] = "OFF";

	for (i = 1; i < argc; i++) {
		unsigned int	entry, longopt = 0;
		const char		*flag, *opt;
		int				multiple;

		opt = argv[i];
		multiple = 0;	/* initialize multiple to 0 by default */

		DBG_MSG(1, (stderr, "argv[%d] = %s: ", i, opt));
		if (*opt == '-') {
			flag = On;
			if (*++opt == '-')
				longopt = *opt++;	/* long option */
		}
		else if (*opt == '+') {
			flag = Off;
			if (*++opt == '+')
				longopt = *opt++;	/* long option */
		}
		else {
			bad_option = 1;
			rxvt_print_error("bad option \"%s\"", opt);
			continue;
		}

		if (!STRCMP(opt, "help"))
			rxvt_usage(longopt ? 2 : 1);
		if (!STRCMP(opt, "h"))
			rxvt_usage(0);

		/* feature: always try to match long-options */
		for (entry = 0; entry < optList_size(); entry++)	{
			char			buflong[256];
			char			bufshort[128];

			/* initialize it to empty string */
			buflong[0] = (char) 0;
			bufshort[0] = (char) 0;

			if (optList[entry].multiple &&
				rxvt_str_match (opt, "vt"))	{
				multiple = atoi (opt+2);
				if (multiple < 0 || multiple >= MAX_PAGES)	{
					entry = optList_size();
					break;	/* out of range, jump to bad option */
				}
				snprintf (buflong, sizeof(buflong)-1,
					optList[entry].kw, multiple);
				buflong[sizeof(buflong)-1] = (char) 0;
				snprintf (bufshort, sizeof(bufshort)-1,
					optList[entry].opt, multiple);
				bufshort[sizeof(bufshort)-1] = (char) 0;
			}
			else if (optList[entry].kw)	{
				STRNCPY (buflong, optList[entry].kw, sizeof(buflong)-1);
				buflong[sizeof(buflong)-1] = (char) 0;
				if (optList[entry].opt)	{
					STRNCPY (bufshort, optList[entry].opt,
						sizeof(bufshort)-1);
					bufshort[sizeof(bufshort)-1] = (char) 0;
				}
			}
			else if (optList[entry].opt)	{
				/* here NULL == optList[entry].kw */
				STRNCPY (bufshort, optList[entry].opt,
					sizeof(bufshort)-1);
				bufshort[sizeof(bufshort)-1] = (char) 0;
			}

			if ((optList[entry].kw && !STRCMP(opt, buflong)) ||
				(!longopt && optList[entry].opt &&
				 !STRCASECMP(opt, bufshort)))
				break;
		}

		if (entry < optList_size()) {
			if (optList_isReverse(entry))
				flag = (flag == On) ? Off : On;
			if (optList_STRLEN(entry)) {	/* string value */
				const char	 *str = argv[++i];

				DBG_MSG(2, (stderr, "string (%s,%s) = ",
					optList[entry].opt ? optList[entry].opt : "nil",
					optList[entry].kw ? optList[entry].kw : "nil"));
				if (flag == On && str && (optList[entry].doff != -1)) {
					DBG_MSG(2, (stderr, "\"%s\"\n", str));
					r->h->rs[optList[entry].doff+multiple] = str;
					/*
					 * special cases are handled in main.c:main() to
					 * allow X resources to set these values before
					 * we settle for default values
					 */
				}
#ifdef DEBUG_VERBOSE
				else
					DBG_MSG(2, (stderr, "???\n"));
#endif
			}
			else {		/* boolean value */
				DBG_MSG(2, (stderr, "boolean (%s,%s) = %s\n",
					optList[entry].opt, optList[entry].kw, flag));
				if ((optList[entry].doff+multiple) < Rs_options2)	{
					if (flag == On)
						r->Options |= (optList[entry].flag);
					else
						r->Options &= ~(optList[entry].flag);
				}
				else	{
					if (flag == On)
						r->Options2 |= (optList[entry].flag);
					else
						r->Options2 &= ~(optList[entry].flag);
				}

				if ((optList[entry].doff+multiple) != -1)
					r->h->rs[optList[entry].doff+multiple] = flag;
			}
		}
		else
#ifdef KEYSYM_RESOURCE
			/* if (!STRNCMP(opt, "keysym.", sizeof("keysym.") - 1)) */
			if (rxvt_str_match(opt, "keysym.")) {
				const char	 *str = argv[++i];

				if (str != NULL)
					rxvt_parse_keysym(r, opt+sizeof("keysym.")-1, str);
		}
		else
#endif
		{
			/*
			 * various old-style options, just ignore
			 * Obsolete since about Jan 96,
			 * so they can probably eventually be removed
			 */
			const char	 *msg = "bad";

			if (longopt) {
				opt--;
				bad_option = 1;
			}
			else if (!STRCMP(opt, "7") || !STRCMP(opt, "8")
#ifdef GREEK_SUPPORT
			   /* obsolete 12 May 1996 (v2.17) */
			   || !rxvt_str_match(opt, "grk")
#endif
			)
				msg = "obsolete";
			else
				bad_option = 1;

			rxvt_print_error("%s option \"%s\"", msg, --opt);
		}
	}

	if (bad_option)
		rxvt_usage(0);

	/* clear default hotkeys */
	if (r->Options2 & Opt2_disableDefaultHotkeys)
		rxvt_delete_default_hotkeys (r);
}

/*}}} */


#ifndef NO_RESOURCES
/*----------------------------------------------------------------------*/

# ifdef KEYSYM_RESOURCE
/*
 * Define key from XrmEnumerateDatabase.
 *   quarks will be something like
 *	  "rxvt" "keysym" "0xFF01"
 *   value will be a string
 */
/* ARGSUSED */
/* INTPROTO */
Bool
rxvt_define_key(XrmDatabase *database __attribute__((unused)), XrmBindingList bindings __attribute__((unused)), XrmQuarkList quarks, XrmRepresentation *type __attribute__((unused)), XrmValue *value, XPointer closure __attribute__((unused)))
{
	int			 last;
	rxvt_t		 *r = rxvt_get_r();

	/* look for last quark in list */
	for (last = 0; quarks[last] != NULLQUARK; last++)
		;
	last--;
	rxvt_parse_keysym(r, XrmQuarkToString(quarks[last]),
		(char *)value->addr);
	return False;
}


/*
 * look for something like this (XK_Delete)
 * rxvt*keysym.0xFFFF: "\177"
 *
 * arg will be
 *	  NULL for ~/.Xdefaults and
 *	  non-NULL for command-line options (need to allocate)
 */
#define NEWARGLIM	500	/* `reasonable' size */
/* INTPROTO */
int
rxvt_parse_keysym(rxvt_t *r, const char *str, const char *arg)
{
	int			n, sym;
	char		*key_string, *newarg = NULL;
	char		newargstr[NEWARGLIM];

	if (arg == NULL) {
		if ((n = rxvt_str_match(str, "keysym.")) == 0)
			return 0;
		str += n;		/* skip `keysym.' */
	}

	/* some scanf() have trouble with a 0x prefix */
	if (isdigit((int) str[0])) {
		if (str[0] == '0' && toupper((int) str[1]) == 'X')
			str += 2;

		if (arg) {
			if (sscanf(str, (STRCHR(str, ':') ? "%x:" : "%x"), &sym) != 1)
			return -1;
		}
		else {
			if (sscanf(str, "%x:", &sym) != 1)
				return -1;

			/* cue to ':', it's there since sscanf() worked */
			STRNCPY(newargstr, STRCHR(str, ':') + 1, NEWARGLIM - 1);
			newargstr[NEWARGLIM - 1] = '\0';
			newarg = newargstr;
		}
	}
	else {
		/*
		 * convert keysym name to keysym number
		 */
		STRNCPY(newargstr, str, NEWARGLIM - 1);
		newargstr[NEWARGLIM - 1] = '\0';
		if (arg == NULL) {
			if ((newarg = STRCHR(newargstr, ':')) == NULL)
				return -1;
			*newarg++ = '\0';	/* terminate keysym name */
		}
		if ((sym = XStringToKeysym(newargstr)) == None)
			return -1;
	}

	/* we only do extended keys */
	if (sym < 0xFF00 || sym > 0xFFFF)
		return -1;
	sym &= 0xFF;
	if (r->h->Keysym_map[sym] != NULL)	/* already set ? */
		return -1;

	if (newarg == NULL) {
		STRNCPY(newargstr, arg, NEWARGLIM - 1);
		newargstr[NEWARGLIM - 1] = '\0';
		newarg = newargstr;
	}
	rxvt_str_trim(newarg);
	if (*newarg == '\0' || (n = rxvt_str_escaped(newarg)) == 0)
		return -1;
	MIN_IT(n, 255);
	assert (n > 0);	/* possible integer overflow */
	key_string = rxvt_malloc((n + 1) * sizeof(char));

	key_string[0] = n;
	STRNCPY(key_string + 1, newarg, n);
	r->h->Keysym_map[sym] = (unsigned char *)key_string;

	return 1;
}

# endif				/* KEYSYM_RESOURCE */


/* INTPROTO */
void
rxvt_parse_hotkeys (rxvt_t* r, int res, char* oldkeystr)
{
	short			flag = 0;
	short			func;
	KeySym			keysym;
	char*			keystr;
	char*			ptr;
	register int	i;


	assert (res >= Rs_hotkey && res <= _Rs_hotkey);
	assert (NULL != oldkeystr);

	func = res - Rs_hotkey;

	/* convert keystr to lower case */
	keystr = STRDUP (oldkeystr);
	for (ptr = keystr; *ptr; ptr++)
		*ptr = (char) tolower ((int) *ptr);

	if (NULL != STRSTR (keystr, "ctrl"))
		HK_SET_CTRL (flag);
	if (NULL != STRSTR (keystr, "meta") ||
		NULL != STRSTR (keystr, "alt"))
		HK_SET_META (flag);
	if (NULL != STRSTR (keystr, "shft") ||
		NULL != STRSTR (keystr, "shift"))
		HK_SET_SHFT (flag);
	if (NULL != STRSTR (keystr, "primary"))
		HK_SET_PRIMARY (flag);

	if (!flag) /* no ctrl/meta/shft key */
		goto TheEnd;

	ptr = STRRCHR (oldkeystr, (int) '+');
	if (NULL == ptr)		/* no '+' is specified */
		goto TheEnd;

	ptr ++;	/* skip '+' */
	if ((char) 0 == *ptr)	/* no keysym is specified */
		goto TheEnd;

	/* get the keysym */
	keysym = XStringToKeysym (ptr);

	if (None == keysym)		/* no valid keysym is specified */
		goto TheEnd;

	/* ignore Shift+ASCII printable non-space char */
	if (!HK_IS_CTRL(flag) &&
		!HK_IS_META(flag) &&
		HK_IS_SHFT(flag) &&
		keysym < 128 &&
		isgraph (keysym))
		goto TheEnd;

	/* when shift is pressed, convert keysym to upper case */
	if (HK_IS_SHFT(flag))
		keysym = toupper (keysym);

	for (i = 0; i < MAX_HOTKEYS; i ++)	{
		if (keysym == r->hotkeys[i].keysym &&
			flag == (r->hotkeys[i].flag & HK_MASK))	{
			/* replace an existing hotkey */
			r->hotkeys[i].func = func;
			r->hotkeys[i].flag = flag;	/* clear flag HK_INTERNAL */
			break;
		}
		if (HKF_DUMMY == r->hotkeys[i].func)	{
			/* replace a dummy hotkey */
			r->hotkeys[i].func = func;
			r->hotkeys[i].flag = flag;
			r->hotkeys[i].keysym = keysym;
			break;
		}
	}

	/*
	** if func is a dummy one, we are actually removing an
	** existing entry, so move everything after that forward
	*/
	if (HKF_DUMMY == func)	{
		for (; i < MAX_HOTKEYS - 1; i ++)	{
			r->hotkeys[i] = r->hotkeys[i+1];
			/* reach the area of dummy functions */
			if (HKF_DUMMY == r->hotkeys[i].func)
				break;
		}
	}

TheEnd:
	/* free duplicate string */
	free (keystr);
}


# ifndef USE_XGETDEFAULT
/*{{{ rxvt_get_xdefaults() */
/*
 * the matching algorithm used for memory-save fake resources
 */
/* INTPROTO */
void
rxvt_get_xdefaults(rxvt_t *r, FILE *stream, const char *name)
{
	unsigned int	len;
	char TAINTED *	str;
	char			buffer[256];


	DBG_MSG(1, (stderr, "rxvt_get_xdefaults (%s)\n", name));

	if (stream == NULL)
		return;
	len = STRLEN(name);

	while ((str = fgets(buffer, sizeof(buffer), stream)) != NULL) {
		unsigned int	entry, n;

		while (*str && isspace((int) *str))
			str++;		/* leading whitespace */

		if ((str[len] != '*' && str[len] != '.')
			|| (len && STRNCMP(str, name, len)))
			continue;
		str += (len + 1);	/* skip `name*' or `name.' */

		{
			/* replace '*' with '.', but stop at ':'! bug reported
			** by afo@zlug.org */
			char*	ptr = str;
			while (*ptr && *ptr != ':')	{
				if ('*' == *ptr)
					*ptr = '.';
				ptr ++;
			}
		}

# ifdef KEYSYM_RESOURCE
		if (!rxvt_parse_keysym(r, str, NULL))	{
# endif				/* KEYSYM_RESOURCE */
			for (entry = 0; entry < optList_size(); entry++) {
				/* const char*	kw = optList[entry].kw; */
				char	kw[256];
				int		multiple = 0;	/* default is no offset */

				if (optList[entry].kw == NULL)
					continue;
				STRNCPY (kw, optList[entry].kw, sizeof(kw)-1);
				kw[sizeof(kw)-1] = (char) 0;

				if (optList[entry].multiple &&
					rxvt_str_match(str, "vt"))	{
					char	buf[256];

					multiple = atoi (str+2);
					if (multiple < 0 || multiple >= MAX_PAGES)
						continue;	/* out of range */
					snprintf (buf, sizeof(buf)-1, kw, multiple);
					buf[sizeof(buf)-1] = (char) 0;
					STRNCPY (kw, buf, sizeof(kw)-1);
					kw[sizeof(kw)-1] = (char) 0;
				}

				n = STRLEN(kw);
				if (str[n] == ':' && rxvt_str_match(str, kw)) {
					/* skip `keyword:' */
					str += (n + 1);
					rxvt_str_trim(str);
					n = STRLEN(str);

					if (n &&
						optList[entry].doff+multiple >= Rs_hotkey &&
						optList[entry].doff+multiple <= _Rs_hotkey)	{
						/* parse hotkey resource */
						assert (0 == multiple);
						rxvt_parse_hotkeys (r, optList[entry].doff,str);
					}
					else
					if (n && !r->h->rs[optList[entry].doff+multiple]) {
						/* not already set */
						int		s;
						char*	p = STRDUP(str);

						r->h->rs[optList[entry].doff+multiple] = p;
						if (optList_isBool(entry)) {
							s = STRCASECMP(str, "true") == 0 ||
								STRCASECMP(str, "yes") == 0 ||
								STRCASECMP(str, "on") == 0 ||
								STRCASECMP(str, "1") == 0;
							if (optList_isReverse(entry))
								s = !s;

							if ((optList[entry].doff+multiple) <
								Rs_options2) {
								if (s)
									r->Options |= (optList[entry].flag);
								else
									r->Options &= ~(optList[entry].flag);
							}
							else	{
								if (s)
									r->Options2 |= (optList[entry].flag);
								else
									r->Options2 &= ~(optList[entry].flag);
							}
						}
					}
					break;
				}	/* for (entry = 0...) */
# ifdef KEYSYM_RESOURCE
			}
# endif
		}
	}

	rewind(stream);
}

/*}}} */
# endif				/* ! USE_XGETDEFAULT */
#endif				/* NO_RESOURCES */


/*{{{ read the resources files */
/*
 * using XGetDefault() or the hand-rolled replacement
 */
#ifdef USE_XGETDEFAULT
/* INTPROTO */
char* 
get_xdefault_resource(XrmDatabase database, const char *name, char *subClass, char *fullClass, char *resource)
{
	char*	resourceClass;
	char*	str_class;
	char*	str_name;
	char*	generic_return_str_type = NULL;
	char*	return_str_type = NULL;
	int		len, i;
	Bool	foundGeneric;
	Bool	found;
	XrmValue		return_value;

	static char*	invalidPrefix = "! invalid name !";
	int				prefix_len = STRLEN(invalidPrefix);
	/* find the maximal prefix_len */
	if (STRLEN(name) > prefix_len) {
		prefix_len = STRLEN(name);
	}
	if (STRLEN(subClass) > prefix_len) {
		prefix_len = STRLEN(subClass);
	}
	if (STRLEN(fullClass) > prefix_len) {
		prefix_len = STRLEN(fullClass);
	}

	/* allocate memory for str_class and str_name */
	len = prefix_len + STRLEN(resource) + 2;
	str_class = rxvt_malloc(len);
	str_name = rxvt_malloc(len);

	snprintf(str_name, len-1, "%s.%s", name, resource);
	str_name[len-1] = (char) 0;

	/* translate resource to class */
	resourceClass = rxvt_malloc(STRLEN(resource) + 1);
	resourceClass[0] = toupper(resource[0]);
	for (i = 1; i < STRLEN(resource); i++) {
		if (resource[i - 1] == '.') {
			resourceClass[i] = toupper(resource[i]);
		}
		else {
			resourceClass[i] = resource[i];
		}
	}
	resourceClass[STRLEN(resource)] = 0;

	/*
	** We need to get resources with two different class names. If
	** both subClass and fullClass exist, then subClass takes
	** precedence.
	**
	** The original algorithm for this was to get null-prefix resource
	** (i.e. *resource) as well as the subClass resource. If they
	** matched, then the fullClass was used. Unfortunately this has
	** a weakness; with the following database:
	**   *resource: value1
	**   subClass.resource: value1
	**   fullClass.resource: value2
	** the value used will be 'value2' instead of 'value1'. This is
	** incorrect.
	**
	** However, we can cheat. XrmGetResource returns string pointers
	** that are actually pointing to the internal string in the
	** database. Thus, if the *pointer* returned when looking for
	** the generic resource is the same as that for the subClass,
	** then they found the same thing.
	**
	** Note that XGetDefault *cannot* be used, as we may be getting
	** multi-level resources, something that it does not support.
	*/
	snprintf(str_class, len-1, "%s.%s", invalidPrefix, resourceClass);
	str_name[len-1] = (char) 0;
	foundGeneric = XrmGetResource(database, str_name, str_class, &generic_return_str_type, &return_value);

	snprintf(str_class, len-1, "%s.%s", subClass, resourceClass);
	str_name[len-1] = (char) 0;
	found = XrmGetResource(database, str_name, str_class, &return_str_type, &return_value);

	if (!found ||
		(foundGeneric && found && generic_return_str_type == return_str_type) ) {
		/* Subclass returned nothing or the same physical thing as the the generic. Try fullClass. */
		snprintf(str_class, len-1, "%s.%s", fullClass, resourceClass);
		str_name[len-1] = (char) 0;
		found = XrmGetResource(database, str_name, str_class, &return_str_type, &return_value);
	}

	/* free memory */
	free(str_class);
	free(str_name);

	return found ? (char *) return_value.addr : NULL;
}
#endif	/* USE_XGETDEFAULT */


/* ARGSUSED */
/* EXTPROTO */
void
rxvt_extract_resources(rxvt_t *r, Display *display __attribute__((unused)), const char *name)
{
#ifndef NO_RESOURCES

# if defined XAPPLOADDIR
#  if defined(HAVE_XSETLOCALE) || defined(HAVE_SETLOCALE)
	/* Compute the path of the possibly available localized Rxvt file */ 
	char		   *localepath = NULL;

	if (r->h->locale != NULL) {	/* XXX: must limit length of string */
		localepath = rxvt_malloc(256); 
		sprintf(localepath, XAPPLOADDIRLOCALE "/" APL_SUBCLASS,
			(int)(258-sizeof(XAPPLOADDIRLOCALE)-sizeof(APL_SUBCLASS)),
			r->h->locale);	/* 258 = 255 + 4 (-.*s) - 1 (/) */
	}

	{
#  endif
# endif

# ifdef USE_XGETDEFAULT
	/* get resources using the X library function */
	int			 entry;

#  ifdef XrmEnumOneLevel
	char*		displayResource, *xe;
	XrmName		name_prefix[3];
	XrmClass	class_prefix[3];
	XrmDatabase	database, rdb1;
	char		fname[1024];

	XrmInitialize();
	database = NULL;

	/* Get any Xserver defaults */
	displayResource = XResourceManagerString(display);
	if (displayResource != NULL)
		database = XrmGetStringDatabase(displayResource);

#   ifdef HAVE_EXTRA_XRESOURCE_FILES
	/* Add in ~/.Xdefaults or ~/.Xresources */
	{
		register int	i;
		char*			ptr;

		if ((ptr = (char *)getenv("HOME")) == NULL)
			ptr = ".";

		for (i = 0; i < (sizeof(xnames) / sizeof(xnames[0])); i++) {
			snprintf(fname, sizeof(fname)-1, "%-.*s/%s",
				sizeof(fname) - STRLEN(xnames[i]) - 2,
				ptr, xnames[i]);
			fname[sizeof(fname)-1] = (char) 0;
			if ((rdb1 = XrmGetFileDatabase(fname)) != NULL) {
				XrmMergeDatabases(rdb1, &database);
#	ifndef HAVE_BOTH_XRESOURCE_FILES
				break;
#	endif
			}
		}	/* for */
	}
#   endif

	/* Add in XENVIRONMENT file */
	if ((xe = (char *)getenv("XENVIRONMENT")) != NULL &&
		(rdb1 = XrmGetFileDatabase(xe)) != NULL)
		XrmMergeDatabases(rdb1, &database);

	/* Add in Rxvt file */
#   if defined(HAVE_XSETLOCALE) || defined(HAVE_SETLOCALE)
	if (localepath == NULL || (rdb1 = XrmGetFileDatabase(localepath)) == NULL)
#   endif
		rdb1 = XrmGetFileDatabase(XAPPLOADDIR "/" APL_SUBCLASS);
	if (rdb1 != NULL)
		XrmMergeDatabases(rdb1, &database);

	/* Add in $XAPPLRESDIR/Rxvt only; not bothering with
	** XUSERFILESEARCHPATH */
	if ((xe = (char *)getenv("XAPPLRESDIR")) != NULL) {
		snprintf(fname, sizeof(fname)-1, "%-.*s/" APL_SUBCLASS,
			sizeof(fname) - sizeof(APL_SUBCLASS) - 2, xe);
		fname[sizeof(fname)-1] = (char) 0;
		if ((rdb1 = XrmGetFileDatabase(fname)) != NULL)
			XrmMergeDatabases(rdb1, &database);
	}

	XrmSetDatabase(display, database);
#  endif

	/*
	** Query resources for options that affect us
	*/
	for (entry = 0; entry < optList_size(); entry++) {
		int				s;
		char*			p;
		register int	i;
		register int	loop = optList[entry].multiple ? MAX_PAGES : 1;

		if (optList[entry].kw == NULL)
			continue;

		for (i = 0; i < loop; i ++)	{
			char	buf[256];

			if (optList[entry].multiple)
				snprintf (buf, sizeof(buf)-1, optList[entry].kw, i);
			else
				STRNCPY (buf, optList[entry].kw, sizeof(buf)-1);
			buf[sizeof(buf)-1] = (char) 0;

			if (r->h->rs[optList[entry].doff+i] != NULL)
				continue;		/* previously set */

			p = get_xdefault_resource(database, name, APL_SUBCLASS, APL_CLASS, buf);
			if (p) {
				if (0 == STRNCASECMP (buf, "hotkey.", 7))	{
					assert (0 == i);	/* meaning 0 == multiple */
					rxvt_parse_hotkeys (r, optList[entry].doff, p);
					continue;
				}

				r->h->rs[optList[entry].doff+i] = p;

				if (optList_isBool(entry)) {
					s = STRCASECMP(p, "true") == 0 ||
						STRCASECMP(p, "yes") == 0 ||
						STRCASECMP(p, "on") == 0 ||
						STRCASECMP(p, "1") == 0;
					if (optList_isReverse(entry))
						s = !s;
					if ((optList[entry].doff+i) < Rs_options2)	{
						if (s)
							r->Options |= (optList[entry].flag);
						else
							r->Options &= ~(optList[entry].flag);
					}
					else	{
						if (s)
							r->Options2 |= (optList[entry].flag);
						else
							r->Options2 &= ~(optList[entry].flag);
					}
				}
			}	/* if (p) */
		}	/* for (i...) */
	}	/* for (entry...) */

/*
 * [R5 or later]: enumerate the resource database
 */
#  ifdef XrmEnumOneLevel
#   ifdef KEYSYM_RESOURCE
	name_prefix[0] = XrmStringToName(name);
	name_prefix[1] = XrmStringToName("keysym");
	name_prefix[2] = NULLQUARK;
	class_prefix[0] = XrmStringToName(APL_SUBCLASS);
	class_prefix[1] = XrmStringToName("Keysym");
	class_prefix[2] = NULLQUARK;
	/* XXX: Need to check sizeof(rxvt_t) == sizeof(XPointer) */
	XrmEnumerateDatabase(XrmGetDatabase(display), name_prefix,
		class_prefix, XrmEnumOneLevel, rxvt_define_key, NULL);
	name_prefix[0] = XrmStringToName(APL_CLASS);
	name_prefix[1] = XrmStringToName("keysym");
	class_prefix[0] = XrmStringToName(APL_CLASS);
	class_prefix[1] = XrmStringToName("Keysym");
	/* XXX: Need to check sizeof(rxvt_t) == sizeof(XPointer) */
	XrmEnumerateDatabase(XrmGetDatabase(display), name_prefix,
		class_prefix, XrmEnumOneLevel, rxvt_define_key, NULL);
#   endif
#  endif

# else				/* USE_XGETDEFAULT */
	/* get resources the hard way, but save lots of memory */
	FILE		   *fd = NULL;
	char		   *home;


	/* open user supplied config file first */
	if (r->h->rs[Rs_confFile])
		fd = fopen (r->h->rs[Rs_confFile], "r");

	if (NULL == fd && NULL != (home = getenv("HOME"))) {
		unsigned int	i, len = STRLEN(home) + 2;
		char*			f = NULL;

		/* possible integer overflow? */
		assert (len > 0);
		for (i = 0; i < (sizeof(xnames) / sizeof(xnames[0])); i++) {
			/* possible integer overflow? */
			assert (len + STRLEN(xnames[i]) > 0);
			f = rxvt_realloc(f, (len + STRLEN(xnames[i])) * sizeof(char));

			sprintf(f, "%s/%s", home, xnames[i]);

			if ((fd = fopen(f, "r")) != NULL)
				break;
		}
		free(f);
	}

	/*
	** The normal order to match resources is the following:
	** @ global resources (partial match, ~/.Xdefaults)
	** @ application file resources (XAPPLOADDIR/Rxvt)
	** @ class resources (~/.Xdefaults)
	** @ private resources (~/.Xdefaults)
	**
	** However, for the hand-rolled resources, the matching algorithm
	** checks if a resource string value has already been allocated
	** and won't overwrite it with (in this case) a less specific
	** resource value.
	**
	** This avoids multiple allocation.  Also, when we've called this
	** routine command-line string options have already been applied
	** so we needn't to allocate for those resources.
	**
	** So, search in resources from most to least specific.
	**
	** Also, use a special sub-class so that we can use either or
	** both of "XTerm" and "Rxvt" as class names.
	*/

	rxvt_get_xdefaults(r, fd, name);
	rxvt_get_xdefaults(r, fd, APL_SUBCLASS);

#  if defined(XAPPLOADDIR) && defined(USE_XAPPLOADDIR)
	{
		FILE*	ad = NULL;

#   if defined(HAVE_XSETLOCALE) || defined(HAVE_SETLOCALE)
		if (localepath == NULL || (ad = fopen(localepath, "r")) == NULL)
#   endif
			ad = fopen(XAPPLOADDIR "/" APL_SUBCLASS, "r");
		if (ad != NULL) {
			rxvt_get_xdefaults(r, ad, APL_SUBCLASS);
			rxvt_get_xdefaults(r, ad, "");
			fclose(ad);
		}
	}
#  endif			/* XAPPLOADDIR */

	rxvt_get_xdefaults(r, fd, APL_CLASS);
	rxvt_get_xdefaults(r, fd, "");	/* partial match */
	if (fd != NULL)
		fclose(fd);
# endif				/* USE_XGETDEFAULT */

# if defined XAPPLOADDIR
#  if defined(HAVE_XSETLOCALE) || defined(HAVE_SETLOCALE)
	}

	/* Free the path of the possibly available localized Rxvt file */ 
	free(localepath);
#  endif
# endif

#endif				/* NO_RESOURCES */


	/*
	** Clear the boolean and reverse flags from Options and Options2.
	** Otherwise this will cause trouble when we want to save the
	** options. In that case, the boolean flag is set for each boolean
	** options. Then if we compare Options(2) to the flag, we always
	** get TRUE!
	*/
	r->Options &= ~(Opt_Boolean | Opt_Reverse);
	r->Options2 &= ~(Opt_Boolean | Opt_Reverse);


	/*
	** even without resources, at least do this setup for command-line
	** options and command-line long options
	*/
#ifdef MULTICHAR_SET
	if (r->h->rs[Rs_multichar_encoding])
		rxvt_set_multichar_encoding(r, r->h->rs[Rs_multichar_encoding]);
	else
		rxvt_set_multichar_encoding(r, MULTICHAR_ENCODING);
#endif

#ifdef GREEK_SUPPORT
	/* this could be a function in grkelot.c */
	/* void set_greek_keyboard (const char * str); */
	if (r->h->rs[Rs_greek_keyboard]) {
		if (!STRCMP(r->h->rs[Rs_greek_keyboard], "iso"))
			greek_setmode(GREEK_ELOT928);	/* former -grk9 */
		else if (!STRCMP(r->h->rs[Rs_greek_keyboard], "ibm"))
			greek_setmode(GREEK_IBM437);	/* former -grk4 */
	}
	{
		KeySym		  sym;

		if (r->h->rs[Rs_greektoggle_key] &&
			((sym = XStringToKeysym(r->h->rs[Rs_greektoggle_key]))!=0))
			r->h->ks_greekmodeswith = sym;
	}
#endif				/* GREEK_SUPPORT */

#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
	{
		KeySym		  sym;

		if (r->h->rs[Rs_bigfont_key] &&
			((sym = XStringToKeysym(r->h->rs[Rs_bigfont_key])) != 0))
			r->h->ks_bigfont = sym;
		if (r->h->rs[Rs_smallfont_key] &&
			((sym = XStringToKeysym(r->h->rs[Rs_smallfont_key])) != 0))
			r->h->ks_smallfont = sym;
	}
#endif

	/*
	** It does not hurt (performance?) to re-delete hotkeys because
	** a careful user may use X resource instead of command line
	** option to disable all default hotkeys. This is an undocumented
	** feature because its behaviour actually is different from
	** command line option: if the user defines too many hotkeys,
	** some of the definitions may be ineffective because the hotkey
	** array is full under the situation that he tries to disable
	** default hotkeys from X resource.
	*/
	if (r->Options2 & Opt2_disableDefaultHotkeys)
		rxvt_delete_default_hotkeys (r);
	if (r->Options2 & Opt2_disableHotkeys)
		rxvt_toggle_hotkeys (r, 0);
}

/*}}} */
/*----------------------- end-of-file (C source) -----------------------*/
