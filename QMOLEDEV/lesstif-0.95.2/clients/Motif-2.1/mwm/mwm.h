/* $Id: mwm.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 *
 * Copyright (C) 1995-2002 LessTif Development Team
 *
 ***********************************************************************/


#ifndef _MWM_H
#define _MWM_H

#include <limits.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

/*
 * path components
 */
#define MWM_SHELL_NAME		"MWMSHELL"
#define SHELL_NAME		"SHELL"
#define DEFAULT_SHELL		"/bin/sh"
#define DEFAULT_SCREEN		"DISPLAY=:%s.%d"
#define DEFAULT_DISPLAY		"DISPLAY=%s:%s.%d"
#define DEFAULT_DISPLAY_STRING	"DISPLAY=%s"
#define HOME_MWMRC		".mwmrc"	/* precede with $HOME */
#define SYSTEM_MWMRC		"system.mwmrc"	/* precede with MWM_DDIR */
#define HOME_BINDINGS_FILE	".motifbind"
#define MWM_VERSION		"Lesstif mwm Release"
#define OPT_MULTISCREEN		"-multiscreen"
#define OPT_SCREENS		"-screens"


#ifndef MWM_DDIR
#define MWM_DDIR "/usr/lib/X11/mwm"
#endif

#ifndef XINCL_PATH
#define XINCL_PATH "/usr/X11/include"
#endif


#ifdef __EMX__
#define MWM_ICONDIR	"/XFree86/include/X11/bitmaps;/XFree86/include/X11/pixmaps"
/* amai: this is an ugly hack ... */
#ifdef MWM_DDIR
#undef MWM_DDIR
#endif
#define MWM_DDIR (char*)__XOS2RedirRoot("/XFree86/lib/X11/mwm")
#else /* !__EMX__ */
#define MWM_ICONDIR		XINCL_PATH"/X11/bitmaps:"XINCL_PATH"/X11/pixmaps"
#endif


/*
 * builtin defaults, when a configuration file can't be found
 * Don't change the WIN_MENU_NAME unless you change it in both places
 */
#define DEFAULT_WIN_MENU_NAME		"DefaultWindowMenu"
#define DEFAULT_BUTTON_BINDING_NAME	"DefaultButtonBindings"
#define DEFAULT_KEY_BINDING_NAME	"DefaultKeyBindings"

#define DEFAULT_MWM_WINDOW_MENU \
    "Menu DefaultWindowMenu {\n" \
	"Restore    Alt<Key>F5     f.restore\n" \
	"Move       Alt<Key>F7     f.move\n" \
	"Size       Alt<Key>F8     f.resize\n" \
	"Minimize   Alt<Key>F9     f.minimize\n" \
	"Maximize   Alt<Key>F10    f.maximize\n" \
	"Lower      Alt<Key>F3     f.lower\n" \
	"no-label                  f.separator\n" \
	"Close      Alt<Key>F4     f.kill\n" \
    "}"

#define DEFAULT_MWM_ROOT_MENU \
    "Menu DefaultRootMenu {\n" \
	"\"Root Menu\"		f.title\n" \
	"\"New Window\"		f.exec		\"xterm &\"\n" \
	"\"Shuffle Up\"		f.circle_up\n" \
	"\"Shuffle Down\"	f.circle_down\n" \
	"\"Refresh\"		f.refresh\n" \
	"\"Pack Icons\"		f.pack_icons\n" \
	"no-label		f.separator\n" \
	"\"Restart...\"		f.restart\n" \
	"\"Quit...\"		f.quit_mwm\n" \
    "}\n"

#define DEFAULT_MWM_KEY_BINDINGS \
    "Keys DefaultKeyBindings {\n" \
	"Shift<Key>Escape          window|icon        f.post_wmenu\n" \
	"Alt<Key>space             window|icon        f.post_wmenu\n" \
	"Alt<Key>Tab               root|icon|window   f.next_key\n" \
	"Alt Shift<Key>Tab         root|icon|window   f.prev_key\n" \
	"Alt<Key>Escape            root|icon|window   f.circle_down\n" \
	"Alt Shift<Key>Escape      root|icon|window   f.circle_up\n" \
	"Alt Shift Ctrl<Key>exclam root|icon|window   f.set_behavior\n" \
	"Alt Ctrl<Key>1            root|icon|window   f.set_behavior\n" \
	"Alt<Key>F6                window             f.next_key transient\n" \
	"Alt Shift<Key>F6          window             f.prev_key transient\n" \
	"Shift<Key>F10             icon               f.post_wmenu\n" \
    "}"

#define DEFAULT_MWM_BUTTON_BINDINGS \
    "Buttons DefaultButtonBindings {\n" \
	"<Btn1Down>     icon|frame      f.raise\n" \
	"<Btn3Down>     icon|frame      f.post_wmenu\n" \
	"<Btn3Down>     root            f.menu DefaultRootMenu\n" \
    "}"

/*
 * this next one is for toggling behavior
 */
#define MWM_BEHAVIOR_KEY_BINDINGS \
    "Keys  MwmBehaviorButtonBindings {\n" \
	"Alt Shift Ctrl<Key>exclam   root|icon|window   f.set_behavior\n" \
	"Alt Ctrl<Key>1              root|icon|window   f.set_behavior\n" \
    "}"

/*
 * this one is for builtin behaviors that always exist
 */
#define BUILTIN_MWM_BUTTON_BINDINGS \
    "Buttons BuiltinButtonBindings {\n" \
	"<Btn1Down>	root		f.window_list\n" \
	"<Btn3Down>	root		f.menu DefaultRootMenu\n" \
	"<Btn1Down>	border		f.resize\n" \
	"<Btn1Down>	icon|title	f.move\n"\
	"<Btn3Down>	icon|frame	f.post_wmenu\n" \
	"<Btn1Down>	menub		f.post_wmenu\n" \
	"<Btn1Down>	minimizeb	f.minimize\n" \
	"<Btn1Down>	maximizeb	f.maximize\n" \
    "}"

#define BUILTIN_MENU_BUTTON_BINDINGS \
    "Buttons MenuButtonBinding {" \
	"<Btn1Click>	menub		f.post_wmenu\n" \
    "}"

#define BUILTIN_KILL_BUTTON_BINDINGS \
    "Buttons KillButtonBinding {" \
	"<Btn1Click2>	menub		f.kill\n" \
    "}"

#define BUILTIN_ICON_BUTTON_BINDINGS \
    "Buttons IconButtonBinding {" \
	"<Btn1Click>	icon		f.post_wmenu\n" \
	"<Btn1Click2>	icon		f.restore\n" \
    "}"

typedef struct Size {
    Dimension       width;
    Dimension       height;
} Size;

typedef XRectangle Geometry;

/*
 * decoration components have resources, too...
 */
typedef struct ComponentInfo {
    int             type;

    Pixel           background;
    Pixmap          background_pixmap;
    Pixel           bottom_shadow_color;
    Pixmap          bottom_shadow_pixmap;
    XmFontList      font_list;
    Pixel           foreground;
    Boolean         save_under;
    Pixel           top_shadow_color;
    Pixmap          top_shadow_pixmap;

    GC              normal_GC;
    GC              grayed_GC;
    GC              top_GC;
    GC              bot_GC;
    XFontStruct    *font;
    int             f_height;
    int             f_y;

    /* frame and icon components have a few more */
    Pixel           active_background;
    Pixmap          active_background_pixmap;
    Pixel           active_bottom_shadow_color;
    Pixmap          active_bottom_shadow_pixmap;
    Pixel           active_foreground;
    Pixel           active_top_shadow_color;
    Pixmap          active_top_shadow_pixmap;

    GC              active_GC;
    GC              active_top_GC;
    GC              active_bot_GC;
} ComponentInfo;

/*
 * component types
 */
enum {
    MWM_MENU,
    MWM_FEEDBACK,
    MWM_PAGER,
    MWM_ICON,
    MWM_TITLE_A,
    MWM_RESIZE_H,
    MWM_BORDER,
    MWM_MAXIMIZE_B,
    MWM_MINIMIZE_B,
    MWM_MENU_B,
    MWM_MAX_COMPONENTS
};

/*
 * for focus policies.  Other valus are in Xm/Xm.h
 */
enum {
    XmKEYBOARD = 2
};

/*
 * for icon decorations
 */
#define XmICON_ACTIVELABEL	(1L << 0)
#define XmICON_IMAGE		(1L << 1)
#define XmICON_LABEL		(1L << 2)

/*
 * for icon placement
 */
#define XmICONS_TOP		(1L << 0)
#define XmICONS_BOTTOM		(1L << 1)
#define XmICONS_LEFT		(1L << 2)
#define XmICONS_RIGHT		(1L << 3)
#define XmICONS_TIGHT		(1L << 4)

/*
 * for icon placement
 */
#define MWM_FEEDBACK_ALL	(1L << 0)
#define MWM_FEEDBACK_BEHAVIOR	(1L << 1)
#define MWM_FEEDBACK_KILL	(1L << 2)
#define MWM_FEEDBACK_MOVE	(1L << 3)
#define MWM_FEEDBACK_PLACEMENT	(1L << 4)
#define MWM_FEEDBACK_QUIT	(1L << 5)
#define MWM_FEEDBACK_RESIZE	(1L << 6)
#define MWM_FEEDBACK_RESTART	(1L << 7)

enum {
    XmUSE_PPOSITION_ON,
    XmUSE_PPOSITION_OFF,
    XmUSE_PPOSITION_NON_ZERO
};

#ifndef WithdrawnState
#define WithdrawnState 0
#endif

/*
 * frame width
 */
#define PAN_FRAME_THICKNESS 2

/*
 * the maximum number of mouse buttons mwm knows about
 *
 * don't think that upping this to 5 will make everything
 * hunky-dory with 5 button mouses
 */
#define MAX_BUTTONS 3

#define HEIGHT_EXTRA 4		/* Extra height for texts in popus */
#define HEIGHT_EXTRA_TITLE 4	/* Extra height for underlining title */
#define HEIGHT_SEPARATOR 4	/* Height of separator lines */

#define SCROLL_REGION 2		/* region around screen edge that */
				/* triggers scrolling */

/*
 * menu label types
 */
enum {
   IS_STRING,
   IS_BITMAP
};

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#define NULLSTR ((char *) NULL)

/*
 * contexts for button presses
 */
#define C_NO_CONTEXT	0x0000
#define C_WINDOW	0x0001
#define C_TITLE		0x0002
#define C_ICON		0x0004
#define C_ROOT		0x0008
#define C_FRAME		0x0010
#define C_MENUB		0x0020
#define C_MAXIMIZEB	0x0040
#define C_MINIMIZEB	0x0080
#define C_RALL		(C_MINIMIZEB|C_MAXIMIZEB)
#define C_LALL		(C_MENUB)
#define C_ALL		(C_WINDOW|C_TITLE|C_ICON|C_ROOT|C_FRAME| \
			 C_MINIMIZEB|C_MAXIMIZEB|C_MENUB)

/*
 * window flags definitions 
 */
#define STICKY		0x00000001	/* Does window stick to glass? */
#define CIRCULATESKIP	0x00000002
#define STARTICONIC	0x00000004
#define WINDOWLISTSKIP	0x00000008
#define MAPPED		0x00000010	/* is it mapped? */
#define ICONIFIED	0x00000020	/* is it an icon now? */
#define TRANSIENT	0x00000040	/* is it a transient window? */
#define RAISED		0x00000080	/* if a sticky window, does it need to be raised */
#define VISIBLE		0x00000100	/* is the window fully visible */
#define ICON_OURS	0x00000200	/* is the icon window supplied by the app? */
#define XPM_FLAG	0x00000400	/* is the icon window an xpm? */
#define PIXMAP_OURS	0x00000800	/* is the icon pixmap ours to free? */
#define SHAPED_ICON	0x00001000	/* is the icon shaped? */
#define MAXIMIZED	0x00002000	/* is the window maximized? */
#define WM_TAKES_FOCUS	0x00004000	/* takes focus */
#define WM_DELS_WINDOW	0x00008000	/* accepts DEL_WINDOW message */
#define WM_SAVE_SELF	0x00010000	/* accepts DEL_WINDOW message */
#define ICON_MOVED	0x00020000	/* has the icon been moved by the user? */
#define ICON_UNMAPPED   0x00040000	/* was the icon unmapped, though window is iconified (Transients) */
#define MAP_PENDING	0x00080000	/* Sent an XMapWindow, but didn't receive a MapNotify yet. */
#define MWM_MESSAGES	0x00100000	/* has a MWM_MESSAGES property */

/*
 * flags to suppress/enable title bar buttons
 */
#define BUTTON1		0x0001
#define BUTTON2		0x0002
#define BUTTON3		0x0004
#define BUTTON4		0x0008
#define BUTTON5		0x0010
#define BUTTON6		0x0020
#define BUTTON7		0x0040
#define BUTTON8		0x0080
#define BUTTON9		0x0100
#define BUTTON10	0x0200

#define MAXPOPUPS 50

typedef struct MenuItem {
    struct MenuItem *next;	/* next menu item */
    struct MenuItem *prev;	/* prev menu item */
    char           *item;	/* the character string displayed on left */
    char           *item2;	/* the character string displayed on right */
    char           *action;	/* action to be performed */
    short           x;		/* x coordinate for text (item) */
    short           x2;		/* x coordinate for text (item2) */
    short           y_offset;	/* y coordinate for item */
    short           y_height;	/* y height for item */
    short           func;	/* twm built in function */
    long            val1;	/* values needed for F_SCROLL */
    long            val2;
    long            val1_unit;	/* units for val1, val2 */
    long            val2_unit;	/* pixels (unit=1) or percent of screen 
				 * (unit = Scr.d_width/d_height */
    short           state;	/* video state, 0 = normal, 1 = reversed */
    short           strlen;	/* strlen(item) */
    short           strlen2;	/* strlen(item2) */
    short           hotkey;	/* Hot key offset (pete@tecc.co.uk).
				   0 - No hot key
				   +ve - offset to hot key char in item
				   -ve - offset to hot key char in item2
				   (offsets have 1 added, so +1 or -1
				   refer to the *first* character)
				 */
    struct MenuRoot *menu;	/* sub-menu */
} MenuItem;

typedef struct MenuRoot {
    struct MenuItem *first;	/* first item in menu */
    struct MenuItem *last;	/* last item in menu */
    struct MenuRoot *next;	/* next in list of root menus */
    char           *name;	/* name of root */
    Window          w;		/* the window of the menu */
    short           height;	/* height of the menu */
    short           width;	/* width of the menu for 1st col */
    short           width2;	/* width of the menu for 2nd col */
    short           items;	/* number of items in the menu */
    Bool            in_use;
} MenuRoot;

typedef struct MouseButton {
    struct MouseButton *next;
    int             func;	/* the function number */
    MenuRoot       *menu;	/* menu if func is F_POPUP */
    MenuItem       *item;	/* action to perform if func != F_POPUP */
    int             button;
    int             context;
    int             modifier;
    int             mask;
    int             count;
    int             val1;
    int             val2;
    long            val1_unit;	/* units for val1, val2 */
    long            val2_unit;	/* pixels (unit=1) or percent of screen 
				 * (unit = Scr.d_width/d_height */
} MouseButton;

typedef struct FuncKey {
    struct FuncKey *next;	/* next in the list of function keys */
    char           *name;	/* key name */
    KeyCode         keycode;	/* X keycode */
    int             cont;	/* context */
    int             mods;	/* modifiers */
    int             func;	/* function to perform */
    MenuRoot       *menu;	/* menu if func is F_POPUP */
    char           *action;	/* action string (if any) */
    int             val1;	/* values needed for F_SCROLL */
    int             val2;
    long            val1_unit;	/* units for val1, val2 */
    long            val2_unit;	/* pixels (unit=1) or percent of screen  */
} FuncKey;

#define MENU_ERROR	-1
#define MENU_NOP	0
#define MENU_DONE	1
#define SUBMENU_DONE	2

#define SIZE_HINDENT 5
#define SIZE_VINDENT 3
#define MAX_WINDOW_WIDTH	32767
#define MAX_WINDOW_HEIGHT	32767

/*
 * for each window that is on the display, one of these structures
 * is allocated and linked into a list 
 */
typedef struct MwmWindow {
    struct MwmWindow *next;	/* next mwm window */
    struct MwmWindow *prev;	/* prev mwm window */

    struct MwmWindow *ancestor;	/* family lineage order */
    struct MwmWindow *child;	/* family lineage order */

    struct MwmWindow *focus_in_tree;	/* focus in family */

    long            client_decoration;
    long            client_functions;
    Boolean         focus_auto_raise;
    String          icon_image;
    Pixel           icon_image_background;
    Pixel           icon_image_bottom_shadow_color;
    Pixmap          icon_image_bottom_shadow_pixmap;
    Pixel           icon_image_foreground;
    Pixel           icon_image_top_shadow_color;
    Pixmap          icon_image_top_shadow_pixmap;
    Pixel           matte_background;
    Pixel           matte_bottom_shadow_color;
    Pixmap          matte_bottom_shadow_pixmap;
    Pixel           matte_foreground;
    Pixel           matte_top_shadow_color;
    Pixmap          matte_top_shadow_pixmap;
    Dimension       matte_width;
    Size            maximum_client_size;
    Boolean         use_client_icon;
    unsigned char   use_p_position;
    String          window_menu;

    char           *name;	/* name of the window */

    Window          w;		/* the child window */
    Window          frame;	/* the frame window */
    Window          parent;	/* Ugly Ugly Ugly - it looks like you
				 * HAVE to reparent the app window into
				 * a window whose size = app window,
				 * or else you can't keep xv and matlab
				 * happy at the same time! */
    Window          title;	/* the title bar window */
    Window          sides[4];
    Window          corners[4];	/* Corner pieces */
    Window          menub;
    Window          minimizeb;
    Window          maximizeb;
    Window          icon_w;	/* the icon window */
    Window          icon_pixmap_w;	/* the icon window */
    Window          icon_borders[4];
    Window          icon_frame;
    Window          transientfor;
    Window          pager_view;
    Window          shield;	/* for application modality & refresh */

    int             wShaped;	/* is this a shaped window */

    int             frame_x;	/* x position of frame */
    int             frame_y;	/* y position of frame */
    int             frame_width;	/* width of frame */
    int             frame_height;	/* height of frame */

    int             boundary_width;
    int             corner_width;
    int             old_bw;	/* border width before reparenting */
    int             bw;

    int             title_x;
    int             title_y;
    int             title_height;	/* height of the title bar */
    int             title_width;	/* width of the title bar */

    int             icon_x_loc;		/* icon window x coordinate */
    int             icon_xl_loc;	/* icon label window x coordinate */
    int             icon_y_loc;		/* icon window y coordiante */
    int             icon_w_width;	/* width of the icon window */
    int             icon_w_height;	/* height of the icon window */
    int             icon_t_width;	/* width of the icon title window */
    int             icon_p_width;	/* width of the icon pixmap window */
    int             icon_p_height;	/* height of the icon pixmap window */
    int             icon_border_width;  /* does this icon have borders? */
    Pixmap          icon_pixmap;	/* pixmap for the icon */
    int             icon_depth;		/* Drawable depth for the icon */
    Pixmap          icon_mask_pixmap;	/* pixmap for the icon mask */

    XWindowAttributes attr;		/* the child window attributes */
    XSizeHints      hints;		/* normal hints */
    XWMHints       *wmhints;		/* WM hints */
    XClassHint      classhint;

    int             Desk;		/* Tells which desktop this window is on */
    int             FocusDesk;		/* Where (if at all) was it focussed */
    int             DeIconifyDesk;	/* Desk to deiconify to, for StubbornIcons */

    unsigned long   flags;
    char           *icon_bitmap_file;
    char           *icon_label;
    char           *icon_active_label;

    int             orig_x;	/* unmaximized x coordinate */
    int             orig_y;	/* unmaximized y coordinate */
    int             orig_wd;	/* unmaximized window width */
    int             orig_ht;	/* unmaximized window height */

    int             xdiff, ydiff;	/* used to restore window position on exit */
    MwmHints       *mwm_hints;
    char           *mwm_menu;
    Atom           *mwm_messages;
    unsigned long   num_messages;
    int             functions;
    int             decorations;
    Window         *cmap_windows;	/* Colormap windows property */
    int             number_cmap_windows;	/* Should generally be 0 */
    int             focus_sequence;

    unsigned long   buttons;

    MenuRoot        *custom_menu;
} MwmWindow;

/*
 * windows without titles
 */
#define NO_NAME		"Untitled"

/*
 * Cursor types
 */
#define POSITION_CURS	0	/* upper Left corner cursor */
#define TITLE_CURS	1	/* title-bar cursor */
#define DEFAULT_CURS	2	/* cursor for apps to inherit */
#define SYS_CURS	3	/* sys-menu and iconify boxes cursor */
#define MOVE_CURS	4	/* resize cursor */
#define WAIT_CURS	5	/* wait a while cursor */
#define MENU_CURS	6	/* menu cursor */
#define SELECT_CURS	7	/* dot cursor for f.move, etc. from menus */
#define DESTROY_CURS	8	/* skull and cross bones, f.destroy */
#define TOP_CURS	9
#define RIGHT_CURS	10
#define BOTTOM_CURS	11
#define LEFT_CURS	12
#define TOP_LEFT_CURS	13
#define TOP_RIGHT_CURS	14
#define BOT_LEFT_CURS	15
#define BOT_RIGHT_CURS	16
#define SYS_MODAL_CURS	17
#define HOURGLASS_CURS	18
#define MAX_CURSORS	19

/*
 * Maximum number of icon boxes that are allowed
 */
#define MAX_BOXES 4

typedef struct {
    Window          win;
    int             isMapped;
} PanFrame;

typedef struct ScreenInfo {
    String          button_bindings;
    Boolean         clean_text;
    Boolean         fade_normal_icon;
    Geometry        feedback_geometry;
    Dimension       frame_border_width;
    Geometry        icon_box_geometry;
    String          icon_box_name;
    String          icon_box_sb_display_policy;
    XmString        icon_box_title;
    long            icon_decoration;
    Size            icon_image_maximum;
    Size            icon_image_minimum;
    unsigned char   icon_placement;
    Dimension       icon_placement_margin;
    String          key_bindings;
    Boolean         limit_resize;
    Size            maximum_maximum_size;
    Boolean         move_opaque;
    Dimension       resize_border_width;
    Boolean         resize_cursors;
    long            transient_decoration;
    long            transient_functions;
    Boolean         use_icon_box;

    /* instance vars */
    unsigned long   screen;		/* screen number */
    String	    screen_name;

    int             d_depth;		/* copy of DefaultDepth(dpy, screen) */
    int             d_width;		/* copy of DisplayWidth(dpy, screen) */
    int             d_height;		/* copy of DisplayHeight(dpy, screen) */

    Window          root_win;		/* the root window */
    Window          size_win;		/* the resize dimensions window */
    Window          no_focus_win;	/* focus window when nobody has it */
    Window          pager_win;		/* pager window */
    Window          pager_child_win;
    Window          pressed_win;	/* the decoration window pressed */
    Window          restart_win;
    Window          quit_win;
    Window          toggle_win;
    Window          shield_win;

    MwmWindow       mwm_root;		/* the head of the mwm window list */
    MwmWindow       *mwm_colormap;	/* mwm colormap window */
    MwmWindow       *mwm_pager;		/* the pager window */
    MwmWindow       *mwm_pushed;	/* saved window to install when pushes
					 * drops to zero */
    MwmWindow       *mwm_highlight;	/* the mwm window that is highlighted 
					 * except for networking delays, this
					 * is the window which has the focus */
    MwmWindow       *mwm_focus;		/* Last window which Mwm gave the focus
					 * to; NOT the window that really has
					 *the focus */
    MwmWindow       *mwm_last_focus;	/* Window which had focus before mwm
					 * stole it to do moves/menus/etc. */
    MwmWindow       *mwm_last_raised;	/* Last window which was raised. Used
					 * for raise lower func. */
    MwmWindow       *mwm_grabbing;
    MwmWindow       *mwm_event;		/* for choosing highlight window on a
					 * button or key event */
    MwmWindow       *top;		/* for stacking order */
    MwmWindow       *bottom;		/* for stacking order */

    Colormap        last_cmap;
    int             alt_mask;

    int             smart_placement;

    int             root_pushes;	/* current push level to install root
					   colormap windows */
    int             event_context;

    Cursor          cursors[MAX_CURSORS];

    GC              resize_GC;
    GC              matte_ts_GC;
    GC              matte_bs_GC;

    PanFrame        panner_top,
		    panner_left,
		    panner_right,
		    panner_bottom;

    ComponentInfo   components[MWM_MAX_COMPONENTS];

    unsigned        virt_scale;	/* Panner scale factor */
    int             virt_x_max;	/* Max location for top left of virt desk */
    int             virt_y_max;
    int             virt_x;		/* X Dimension of virtual desktop */
    int             virt_y;		/* Y Dimension of virtual desktop */
    int		    edge_scroll_x;
    int		    edge_scroll_y;
    int		    pager_x;
    int		    pager_y;

    MouseButton    *buttons;
    FuncKey        *keys;
    MenuRoot      **popups;
    int             num_popups;
    int             max_popups;

    char           *DefaultIcon;	/* Icon to use when no other icons are found */

    unsigned char   buttons2grab;	/* buttons to grab in click to focus mode */
    unsigned long   flags;
    int             icon_boxes[MAX_BOXES][4];
    int             num_icon_boxes;

    int             ScrollResistance;	/* resistance to scrolling in desktop */
    int             MoveResistance;	/* res to moving windows over viewport edge */
    int             OpaqueSize;
    int             current_desk;	/* The current desktop number */
    char            *IconPath;
    char            *PixmapPath;
} ScreenInfo;

/*
 * for the flags value - these used to be seperate Bool's
 */
#define CirculateSkipIcons	0x0001
#define StubbornIcons		0x0002
#define StubbornPlacement	0x0004
#define StubbornIconPlacement	0x0008
#define OpaqueResize		0x0010

/*
 * management info for the whole window manager
 */
typedef struct MwmInternalInfo {
    Boolean         auto_key_focus;
    Time            auto_raise_delay;
    String          bitmap_directory;
    Boolean         client_auto_place;
    unsigned char   colormap_focus_policy;
    String          config_file;
    Boolean         deiconify_key_focus;
    Time            double_click_time;
    Boolean         enable_warp;
    Boolean         enforce_key_focus;
    Boolean         icon_auto_place;
    Boolean         icon_click;
    Boolean         interactive_placement;
    unsigned char   keyboard_focus_policy;
    Boolean         lower_on_iconify;
    Dimension       move_threshold;
    Boolean         multi_screen;
    Boolean         pass_buttons;
    Boolean         pass_selection_buttons;
    Boolean         position_is_frame;
    Boolean         position_on_screen;
    Time            quit_timeout;
    Boolean         raise_key_focus;
    String          screens;
    long            show_feedback;
    Boolean         startup_key_focus;
    Boolean         w_menu_button_click;
    Boolean         w_menu_button_click_2;
    Boolean	    use_pager;
    int		    edge_scroll_x;
    int		    edge_scroll_y;
    int             pager_x;
    int             pager_y;
    int             virtual_x;
    int             virtual_y;
    Boolean         smart_placement;

    /* instance vars */
    ScreenInfo	    **screen_info;
    int		    number_of_screens;
    int             click_time;		/* Max button-click delay */
} MwmInternalInfo;

/*
 * mwm functions
 */
#define F_NOP			0
#define F_BEEP			1
#define F_CHANGE_WINDOWS_DESK   2
#define F_CIRCULATE_DOWN        3
#define F_CIRCULATE_UP          4
#define F_CLOSE                 5
#define F_DESK                  6
#define F_EXEC			7	/* string */
#define F_FOCUS                 8
#define F_FOCUS_COLOR		9
#define F_FOCUS_KEY		10
#define F_GOTO_PAGE             11
#define F_ICONIFY		12
#define F_LOWER			13
#define F_MAXIMIZE              14
#define F_MOVE			15
#define F_MOVECURSOR            16
#define F_NEXT_CMAP		17
#define F_NEXT_KEY		18
#define F_NORMALIZE		19
#define F_NORM_AND_RAISE	20
#define F_PACK_ICONS		21
#define F_PASS_KEYS		22
#define F_POPUP			23	/* string */
#define F_PREV_CMAP		24
#define F_PREV_KEY		25
#define F_QUIT			26
#define F_RAISE			27
#define F_RAISE_IT              28
#define F_RAISELOWER            29
#define F_RESIZE		30
#define F_RESTART               31
#define F_REFRESH		32
#define F_REFRESH_WIN		33
#define F_RESTORE_AND_RAISE	34
#define F_SCREEN		35
#define F_SCROLL                36	/* scroll the virtual desktop */
#define F_SEND_MSG		37
#define F_SET_BEHAVIOR		38
#define F_STICK                 39
#define F_TITLE			40
#define F_TOGGLE_PAGE           41
#define F_WARP                  42
#define F_WINDOWLIST            43
#define F_W_POPUP		44	/* string */

/*
 * error handler
 */
#define REDIRECT	01
#define GENERAL	02

/*
 * gotta know which way
 */
#define UP 1
#define DOWN 0

/*
 * prototypes
 */
/* Panner functions */
extern void	PAN_Initialize(ScreenInfo *scr);
extern void	PAN_Raise(ScreenInfo *scr);
extern void	PAN_CheckBounds(ScreenInfo *scr);
extern void PAN_PanDesktop(ScreenInfo *scr, int, int,
			       int *, int *, int *, int *, Boolean, XEvent *);
extern Boolean  PAN_IsPannerWindow(ScreenInfo *scr, Window win);

/* Desktop functions */
extern void	DT_SaveState(ScreenInfo *scr);
extern void	DT_ChangeDesks(ScreenInfo *scr, int val1, int val2);
extern void	DT_WindowChangingDesks(ScreenInfo *scr, MwmWindow *t, int val1);

/* Pager functions */
extern void PAGER_Clear(ScreenInfo *scr);
extern void PAGER_Redraw(ScreenInfo *scr);
extern void PAGER_SwitchPage(ScreenInfo *scr, Bool, Bool, XEvent *event);
extern void PAGER_UpdateViewPort(ScreenInfo *scr);
extern void PAGER_UpdateView(ScreenInfo *scr, MwmWindow * t);
extern void	PAGER_Initialize(ScreenInfo *scr, Position x, Position y);
extern void PAGER_Update(ScreenInfo *scr, XEvent *event);
extern void PAGER_MoveViewPort(ScreenInfo *scr, int newx, int newy, Boolean);

/* Parser functions */
extern void	PARSE_mwmrc(ScreenInfo *scr);
extern int	PARSE_buf(ScreenInfo *scr, char *buf);

/* Resource functions */
extern void	RES_Initialize(void);
extern void	RES_GetClientDefaults(ScreenInfo *scr, MwmWindow *win, char *name_s, char *class_s);
extern void	RES_GetScreenDefaults(ScreenInfo *scr);
extern void	RES_GetComponentDefaults(ScreenInfo *scr);

/* Miscellaneous functions */
extern Boolean	MISC_Grab(ScreenInfo *scr, int);
extern void MISC_Ungrab(ScreenInfo *scr);
extern void	MISC_WaitForButtonsUp(ScreenInfo *scr);
extern int  MISC_FlushExpose(Window w);
extern void MISC_SetTimer(int);
extern Boolean  MISC_StashEventTime(XEvent * ev);
extern Time	MISC_FetchEventTime(void);
extern void MISC_SetFocusSequence(ScreenInfo *scr);
extern void MISC_KeyboardShortcut(ScreenInfo *scr, XEvent *, int);
extern void	MISC_AddToTree(ScreenInfo *scr, MwmWindow *win);
extern void	MISC_RemoveFromTree(ScreenInfo *scr, MwmWindow *win);
extern void	MISC_PrintTree(ScreenInfo *scr);
extern MwmWindow *MISC_RootOfTree(MwmWindow *win);
extern void	MISC_FixupTransients(ScreenInfo *scr);
extern void	MISC_DestroyChildren(ScreenInfo *scr, MwmWindow *win);

/* Cursor functions */
extern void	CURS_Initialize(ScreenInfo *scr);

/* Property functions */
extern void	PROP_Initialize(void);
extern void	PROP_SetBehavior(ScreenInfo *scr, Boolean custom);
extern void	PROP_ClearBehavior(ScreenInfo *scr);
extern void	PROP_SetPriorityColors(ScreenInfo *scr);
extern Boolean	PROP_CheckDesktop(ScreenInfo *scr);
extern void PROP_SetState(MwmWindow *, int);
extern void	PROP_SendClientMessage(Window w, Atom a, Time timestamp);
extern void	PROP_GetMwmHints(MwmWindow *win);
extern void	PROP_GetMwmMenu(MwmWindow *win);
extern void	PROP_GetMwmMessages(MwmWindow *win);
extern void	PROP_GetWmIconName(MwmWindow *win);
extern void	PROP_GetWindowSizeHints(MwmWindow *win);
extern void	PROP_GetWmProtocols(MwmWindow *win);
extern int	PROP_GetBehavior(ScreenInfo *scr);
extern void	PROP_GetWmColormapWindows(MwmWindow *tmp);
extern Boolean	PROP_VerifyMwmMessage(MwmWindow *w, Atom message);
extern void	PROP_SendMwmMessage(Window w, Atom message, Time timestamp);

/* Screen functions */
extern Boolean	SCREEN_Initialize(ScreenInfo *scr);
extern ScreenInfo *SCREEN_EventToStruct(XEvent *event);

/* Window functions */
extern void	WIN_CaptureWindows(ScreenInfo *scr);
extern void	WIN_ReleaseWindows(ScreenInfo *scr);
extern void	WIN_MapWindow(ScreenInfo *scr, Window win);
extern void WIN_SetFocus(ScreenInfo *scr, Window, MwmWindow *);
extern void	WIN_ChangeFocus(ScreenInfo *scr, MwmWindow *t,
				int DeIconifyOnly);
extern void WIN_RestoreWithdrawn(ScreenInfo *scr, MwmWindow *, Boolean);
extern MwmWindow *WIN_WindowToStruct(ScreenInfo *scr, Window target);	
extern void WIN_Raise(ScreenInfo *scr, MwmWindow * t);
extern void WIN_Lower(ScreenInfo *scr, MwmWindow * t);
extern void	WIN_ConstrainWindow(ScreenInfo *scr, MwmWindow *win,
				    int *widthp, int *heightp);
extern void	WIN_DrawOutline(ScreenInfo *scr, Window win,
				int x, int y, int wd, int ht);
extern void WIN_FreeNames(MwmWindow * tmp, Bool nukename, Bool nukeicon);
extern void	WIN_DestroyWindow(ScreenInfo *scr, MwmWindow *Tmp_win);
extern void	WIN_SetFocusInTree(MwmWindow *win);

/* ColorMap functions */
extern void	COLOR_InstallWindowColorMap(ScreenInfo *scr, MwmWindow *win);
extern void	COLOR_PushRootColorMap(ScreenInfo *scr);
extern void	COLOR_PopRootColorMap(ScreenInfo *scr);

/* Decoration functions */
extern void DEC_DrawDecorations(ScreenInfo *, MwmWindow *,
				    Bool, Bool, Bool, Window);
extern void DEC_DrawTitleBar(ScreenInfo *scr, MwmWindow *, Bool, Bool);
extern void DEC_DrawShadows(MwmWindow *, Window, int, int, int, int,
				GC, GC);
extern void DEC_DrawBorder(MwmWindow *, Window, int, int, int, int,
			       GC, GC);
extern void DEC_CreateDecorations(ScreenInfo *scr, MwmWindow *);
extern void DEC_ConfigureDecorations(ScreenInfo *scr, MwmWindow *,
					 int, int, int, int, Boolean);
extern void DEC_SelectDecorations(ScreenInfo *scr, MwmWindow *t);
extern void DEC_ReselectDecorations(ScreenInfo *scr, MwmWindow *t);
extern void DEC_SetShape(MwmWindow *, int);

/* Event functions */
extern void	EVENT_Initialize(void);
extern int	EVENT_GetContext(ScreenInfo *scr, MwmWindow *t, XEvent *e, Window *w);
extern int	EVENT_Next(XEvent *event);
extern void	EVENT_Dispatch(XEvent *event);

/* Function functions */
extern void FUNC_Execute(ScreenInfo *scr, int, char *, Window,
			     MwmWindow *, XEvent *, unsigned long,
			     long, long, int, int, MenuRoot *);

/* Icon functions */
extern void ICON_CreateWindow(ScreenInfo *scr, MwmWindow *tmp,
				  int def_x, int def_y);
extern void ICON_DrawWindow(ScreenInfo *scr, MwmWindow *);
extern void ICON_UpdateWindow(ScreenInfo *scr, MwmWindow *, Boolean force);
extern void ICON_AutoPlace(ScreenInfo *scr, MwmWindow *);
extern void ICON_Iconify(ScreenInfo *scr, MwmWindow *, int, int);
extern void ICON_DeIconify(ScreenInfo *scr, MwmWindow *);

/* Menu functions */
extern MenuRoot *MENU_Create(const char *name);
extern void	MENU_FindHotKey(MenuItem *it, KeySym key);
extern void	MENU_Add(ScreenInfo *scr, MenuRoot *menu);
extern void	MENU_Remove(ScreenInfo *scr, MenuRoot *menu);
extern void	MENU_AddItem(ScreenInfo *scr, MenuRoot *menu,
			     char *item, char *item2,
			     char *action, int func,
			     long func_val_1, long func_val_2,
			     char unit_1, char unit_2);
extern void	MENU_LinkUp(ScreenInfo *scr);
extern void MENU_Realize(ScreenInfo *scr, MenuRoot *);
extern void MENU_RealizeMenus(ScreenInfo *scr);
extern int  MENU_PopupMenu(ScreenInfo *scr, MenuRoot * menu);
extern int  MENU_WinMenu(ScreenInfo *scr, MenuRoot *menu,
			     MwmWindow *win, Boolean button, Boolean icon);
extern void	MENU_Destroy(MenuRoot *menu);
extern void MENU_DestroyMenus(ScreenInfo *scr);
extern void MENU_Reset(void);
extern void MENU_BuildWindowMenu(ScreenInfo *scr, MwmWindow *win);
extern void MENU_DestroyWindowMenu(ScreenInfo *scr, MwmWindow *win);
extern char	*MENU_AcceleratorString(ScreenInfo *scr, KeySym key,
					int modifiers);

/* Movement functions */
extern void MOVE_EventLoop(ScreenInfo *scr, MwmWindow *, int, int, int, int,
			       int *, int *, Boolean, Boolean);
extern void	MOVE_Interactive(ScreenInfo *scr, Window *win, MwmWindow *tmp_win,
				 int *FinalX, int *FinalY, XEvent *eventp);

/* Resize functions */
extern void	RESIZE_EventLoop(ScreenInfo *scr, Window w, MwmWindow *tmp_win,
				 int val1, int val2,
				 int val1_unit, int val2_unit);

/* exit functions */
extern void	MWM_SetErrorHandler(int which);
extern void MWM_Done(int, const char *);

/* Debugging functions */
extern const char	 *_MwmPrintC(int x);
extern const char	 *_MwmPrintF(int x);

/* Resource default functions */
extern void	_WmMultiClickTimeDefault(Widget w, int offset, XrmValue *val);
extern void	_WmFocusAutoRaiseDefault(Widget w, int offset, XrmValue *val);
extern void	_WmDefaultBorderWidth(Widget w, int offset, XrmValue *val);
extern void	_WmDefaultResizeBorderWidth(Widget widget, int offset,
					    XrmValue *val);
extern void	_WmIconImageBDefault(Widget w, int offset, XrmValue *val);
extern void	_WmIconImageBSCDefault(Widget w, int offset, XrmValue *val);
extern void	_WmIconImageBSPDefault(Widget w, int offset, XrmValue *val);
extern void	_WmIconImageFDefault(Widget w, int offset, XrmValue *val);
extern void	_WmIconImageTSCDefault(Widget w, int offset, XrmValue *val);
extern void	_WmIconImageTSPDefault(Widget w, int offset, XrmValue *val);
extern void	_WmMatteBDefault(Widget w, int offset, XrmValue *val);
extern void	_WmMatteBSCDefault(Widget w, int offset, XrmValue *val);
extern void	_WmMatteBSPDefault(Widget w, int offset, XrmValue *val);
extern void	_WmMatteFDefault(Widget w, int offset, XrmValue *val);
extern void	_WmMatteTSCDefault(Widget w, int offset, XrmValue *val);
extern void	_WmMatteTSPDefault(Widget w, int offset, XrmValue *val);
extern void	_WmBackgroundDefault(Widget w, int offset, XrmValue *val);
extern void	_WmBackgroundPixmapDefault(Widget w, int offset,
					   XrmValue *val);
extern void	_WmBottomShadowColorDefault(Widget w, int offset,
					    XrmValue *val);
extern void	_WmBottomShadowPixmapDefault(Widget w, int offset,
					     XrmValue *val);
extern void	_WmForegroundDefault(Widget w, int offset, XrmValue *val);
extern void	_WmTopShadowColorDefault(Widget w, int offset, XrmValue *val);
extern void	_WmTopShadowPixmapDefault(Widget w, int offset, XrmValue *val);
extern void	_WmABackgroundDefault(Widget w, int offset, XrmValue *val);
extern void	_WmAForegroundDefault(Widget w, int offset, XrmValue *val);
extern void	_WmABottomShadowColorDefault(Widget w, int offset,
					     XrmValue *val);
extern void	_WmATopShadowColorDefault(Widget w, int offset, XrmValue *val);
extern void	_WmABackgroundPixmapDefault(Widget w, int offset,
					    XrmValue *val);
extern void	_WmATopShadowPixmapDefault(Widget w, int offset,
					   XrmValue *val);

/* Parsing functions */
extern char	mwm_getc(void);
extern void	mwm_putc(char c);
extern void	mwm_unputc(char c);
extern void	yyerror(const char *fmt, ...);

/* Misc utility functions */
extern int SetEnvironment(const char *key, const char *value);
void ReapChildren(void);
extern char *find_config_file(void);

/*
 * global data
 */
extern MwmInternalInfo Mwm;
extern XContext MwmContext;
extern XContext MenuContext;
extern Display *dpy;
extern int fd_width, x_fd;
extern Widget   toplevel;
extern char     NoName[];
extern Window   JunkRoot, JunkChild;
extern int      JunkX, JunkY;
extern unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;
extern void    *working_base;
extern ScreenInfo *rscr;

/* menus.c: */
extern int menuFromFrameOrWindowOrTitlebar;
extern int Stashed_X, Stashed_Y;

/* move.c: */
extern Bool NeedToResizeToo;

/* mwm.c: */
extern volatile int alarmed;
extern Bool debugging;
extern char **g_argv;

/* pager.c: */
extern Bool DoHandlePageing;
extern Bool pagerOn;
extern Bool EnablePagerRedraw;
extern Bool DoHandlePageing;



extern Atom     XA_MIT_PRIORITY_COLORS;
extern Atom     XA_WM_CHANGE_STATE;
extern Atom     XA_WM_STATE;
extern Atom     XA_WM_COLORMAP_WINDOWS;
extern Atom     XA_WM_PROTOCOLS;
extern Atom     XA_WM_TAKE_FOCUS;
extern Atom     XA_WM_SAVE_YOURSELF;
extern Atom     XA_WM_DELETE_WINDOW;
extern Atom     XA_WM_DESKTOP;
extern Atom     XA_MWM_CLIENT;
extern Atom     XA_MWM_HINTS;
extern Atom     XA_MWM_MESSAGES;
extern Atom     XA_MWM_MENU;
extern Atom     XA_MWM_INFO;


/* some utility #defines */
#ifdef PATH_MAX
# define MAX_PATH_LEN PATH_MAX
#else
# define MAX_PATH_LEN	2048 /* this is OS dependent, but this should catch most */
#endif


#endif /* _MWM_H */
