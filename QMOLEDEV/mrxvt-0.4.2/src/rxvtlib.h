/*--------------------------------*-H-*---------------------------------*
 * File:	rxvtlib.h
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2004        Terry Griffin <griffint@pobox.com>
 * Copyright (c) 2005        Teun Burgers <burgers@ecn.nl>
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
** $Id: rxvtlib.h,v 1.137 2005/06/24 01:54:07 cvs Exp $
*/

#ifndef __RXVTLIB_H__
#define __RXVTLIB_H__


/*
 * If we haven't pulled in typedef's like  RINT16T  then do them ourself
 * type of (normal and unsigned) basic sizes
 */
#if (SIZEOF_INT_P == 8)
/* we have 8 byte pointers on 64bit systems */
# if (SIZEOF_INT == 8)
typedef int				intp_t;
typedef unsigned int	u_intp_t;
# elif (SIZEOF_LONG == 8)
typedef long			intp_t;
typedef unsigned long	u_intp_t;
# else
#  error No 8 byte integer type available
# endif
#else
/* If we have <inttypes.h>, use *intptr_t instead of *INT32T. This
** eliminates some problems on 64-bit systems. Reported by David
** Mosberger (http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=312710).
** Patch is applied after slight modification. :-) */
# ifdef HAVE_INTTYPES_H
typedef intptr_t		intp_t;
typedef uintptr_t		u_intp_t;
# else
/* whatever normal size corresponds to a integer pointer */
typedef RINT32T			intp_t;
/* whatever normal size corresponds to a unsigned integer pointer */
typedef RUINT32T		u_intp_t;
# endif	/* HAVE_INTTYPES_H */
#endif
/* type of unicode_t */
typedef RUINT32T		unicode_t;


/*****************************************************************************
 *                                 SECTION 2                                 *
 *                      DO NOT TOUCH ANYTHING BELOW HERE                     *
 *****************************************************************************/

struct rxvt_vars;		/* defined later on */
struct rxvt_hidden;		/* not defined here */

typedef struct {
    RINT32T         row;
    RINT32T         col;
} row_col_t;

typedef unsigned char text_t;
#if defined(TTY_256COLOR) || defined(MULTICHAR_SET)
# define rend_t		RUINT32T
#else
# define rend_t		RUINT16T
#endif

/*
 * TermWin elements limits
 *  ncol      : 1 <= ncol       <= MAX(RINT16T)
 *  nrow      : 1 <= nrow       <= MAX(RINT16T)
 *  saveLines : 0 <= saveLines  <= MAX(RINT16T)
 *  nscrolled : 0 <= nscrolled  <= saveLines
 *  view_start: 0 <= view_start <= nscrolled
 */

typedef struct {
	RUINT16T		fwidth;	/* font width  [pixels] */
	RUINT16T		fheight;	/* font height [pixels] */
	RUINT16T		propfont;	/* font proportional flags */
	RUINT16T		ncol;	/* window columns [characters] */
	RUINT16T		nrow;	/* window rows [characters] */
	RUINT16T		mapped; /* TermWin is mapped? */
	RUINT16T		int_bwidth; /* internal border width */
	RUINT16T		ext_bwidth; /* external border width */
    RUINT16T		saveLines;	/* number of lines to save for all tabs */
#ifndef NO_LINESPACE
	RUINT16T		lineSpace;	/* space between rows */
#endif

	char			focus;	/* window is focused? */
	char			enter;	/* pointer is in window? */

	Window			parent;	/* parent window */
	GC				gc;		/* GC for drawing text */
	XFontStruct*	font;	/* main font structure */
#ifndef NO_BOLDFONT
	XFontStruct*	bfont;	/* bold font */
#endif
#ifdef MULTICHAR_SET
	XFontStruct*	mfont;	/* Multichar font structure */
#endif
#ifdef USE_XIM
	XFontSet		fontset;
#endif
#ifdef XFT_SUPPORT
	XftPattern*		xftpattern;
	XftFont*		xftfont;
# ifdef MULTICHAR_SET
	XftPattern*		xftmpattern;
	XftFont*		xftmfont;
	int				xftmsize;
#  ifdef HAVE_ICONV_H
	iconv_t			xfticonv;
#  endif
# endif
# ifndef NO_BOLDFONT
	XftPattern*		xftbpattern;
	XftFont*		xftbfont;
	char			bf_switched;/* bold and normal font switched? */
# endif
	char			xftfnmono;	/* font is mono? */
	char			xftmono;	/* font and mfont are mono? */
	int				xftsize;
#endif	/* XFT_SUPPORT */

# define PARENT_NUMBER		(6)
	int				opacity;	/* transluscent window opaque degree */
	int				opacity_degree;	/* opaque change degree */

#ifdef TRANSPARENT
	Pixmap			pixmap;	/* background image from XROOT, only
							** used for transparent window */
	Window			parenttree[PARENT_NUMBER];
#endif

#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
	int				bgfade;	/* fade percentage */
# ifdef TINTING_SUPPORT
	int				shade;	/* tinting shade percentage */
# endif
#endif

#ifdef OFF_FOCUS_FADING
	int				fade;	/* off-focus fading percentage */
#endif

#ifdef TEXT_SHADOW
    enum {
		SHADOW_NONE = 0,
		SHADOW_LEFT,
		SHADOW_RIGHT,
		SHADOW_TOP,
		SHADOW_BOTTOM,
		SHADOW_TOPLEFT,
		SHADOW_BOTRIGHT,
		SHADOW_TOPRIGHT,
		SHADOW_BOTLEFT,
	} shadow_mode;
	unsigned long	shadow;	/* text shadow color */
# ifdef XFT_SUPPORT
    XftColor		xftshadow;
# endif
#endif

#ifdef HAVE_X11_SM_SMLIB_H
	SmcConn			sm_conn;
	IceConn			ice_conn;
	int				ice_fd;
	char*			sm_client_id;
#endif
} TermWin_t;



/*
 * screen accounting:
 * screen_t elements
 *   text:      Contains all text information including the scrollback buffer.
 *              Each line is length TermWin.ncol
 *   tlen:      The length of the line or -1 for wrapped lines.
 *   rend:      Contains rendition information: font, bold, colour, etc.
 * * Note: Each line for both text and rend are only allocated on demand, and
 *         text[x] is allocated <=> rend[x] is allocated  for all x.
 *   row:       Cursor row position                   : 0 <= row < TermWin.nrow
 *   col:       Cursor column position                : 0 <= col < TermWin.ncol
 *   tscroll:   Scrolling region top row inclusive    : 0 <= row < TermWin.nrow
 *   bscroll:   Scrolling region bottom row inclusive : 0 <= row < TermWin.nrow
 *
 * selection_t elements
 *   clicks:    1, 2 or 3 clicks - 4 indicates a special condition of 1 where
 *              nothing is selected
 *   beg:       row/column of beginning of selection  : never past mark
 *   mark:      row/column of initial click           : never past end
 *   end:       row/column of one character past end of selection
 * * Note: -TermWin.nscrolled <= beg.row <= mark.row <= end.row < TermWin.nrow
 * * Note: col == -1 ==> we're left of screen
 *
 * Layout of text/rend information in the screen_t text/rend structures:
 *   Rows [0] ... [TermWin.saveLines - 1]
 *     scrollback region : we're only here if TermWin.view_start != 0
 *   Rows [TermWin.saveLines] ... [TermWin.saveLines + TermWin.nrow - 1]
 *     normal `unscrolled' screen region
 */
typedef struct {
    text_t**		text;		/* _all_ the text */
    RINT16T*		tlen;		/* length of each text line */
    rend_t**		rend;		/* rendition, uses RS_ flags */
    row_col_t       cur;		/* cursor position on the screen */
    RUINT16T		tscroll;	/* top of settable scroll region */
    RUINT16T		bscroll;	/* bottom of settable scroll region */
    RUINT16T		charset;	/* character set number [0..3] */
    unsigned int	flags;		/* see below */
    row_col_t		s_cur;		/* saved cursor position */
    RUINT16T		s_charset;	/* saved character set number [0..3] */
    char			s_charset_char;
    rend_t			s_rstyle;	/* saved rendition style */
} screen_t;


typedef struct {
    unsigned char*	text;	/* selected text */
    RUINT32T		len;	/* length of selected text */
    enum {
		SELECTION_CLEAR = 0,/* nothing selected */
		SELECTION_INIT,		/* marked a point */
		SELECTION_BEGIN,	/* started a selection */
		SELECTION_CONT,		/* continued selection */
		SELECTION_DONE		/* selection put in CUT_BUFFER0 */
    } op;					/* current operation */
	short			vt;		/* VT that has the selection */
    short           screen;	/* screen being used */
    short           clicks;	/* number of clicks */
    row_col_t       beg;	/* beginning of selection <= mark */
    row_col_t       mark;	/* point of initial click <= end */
    row_col_t       end;	/* one character past end point */
} selection_t;

typedef enum {
    OLD_SELECT, OLD_WORD_SELECT, NEW_SELECT
} sstyle_t;

/* ------------------------------------------------------------------------- */

/* screen_t flags */
#define Screen_Relative		(1<<0)	/* relative origin mode flag */
#define Screen_VisibleCursor	(1<<1)	/* cursor visible? */
#define Screen_Autowrap		(1<<2)	/* auto-wrap flag */
#define Screen_Insert		(1<<3)	/* insert mode (vs. overstrike) */
#define Screen_WrapNext		(1<<4)	/* need to wrap for next char? */
#define Screen_DefaultFlags	(Screen_VisibleCursor|Screen_Autowrap)

/* rxvt_vars.Options */
#define	Opt_console					(1LU<<0)
#define Opt_loginShell				(1LU<<1)
#define Opt_iconic					(1LU<<2)
#define Opt_visualBell				(1LU<<3)
#define Opt_mapAlert				(1LU<<4)
#define Opt_reverseVideo			(1LU<<5)
#define Opt_utmpInhibit				(1LU<<6)
#define Opt_scrollBar				(1LU<<7)
#define Opt_scrollBar_right			(1LU<<8)
#define Opt_scrollBar_floating		(1LU<<9)
#define Opt_meta8					(1LU<<10)
#define Opt_scrollTtyOutputInhibit	(1LU<<11)
#define Opt_scrollTtyKeypress		(1LU<<12)
#define Opt_transparent				(1LU<<13)
#define Opt_transparent_all			(1LU<<14)
#define Opt_mc_hack					(1LU<<15)
#define Opt_tripleclickwords		(1LU<<16)
#define Opt_scrollWithBuffer		(1LU<<17)
#define Opt_jumpScroll				(1LU<<18)
#define Opt_mouseWheelScrollPage	(1LU<<19)
#define Opt_pointerBlank			(1LU<<20)
#define Opt_cursorBlink				(1LU<<21)
#ifdef HAVE_SCROLLBARS
# define Opt_transparent_scrollbar	(1LU<<22)
#endif
#ifdef HAVE_MENUBAR
# define Opt_transparent_menubar	(1LU<<23)
# define Opt_showMenu				(1LU<<24)
#endif
#define Opt_transparent_tabbar		(1LU<<25)
#define Opt_tabPixmap				(1LU<<26)
#ifdef XFT_SUPPORT
# define Opt_xft					(1LU<<27)
#endif
#ifdef THAI
# define Opt_thai					(1LU<<28)
#endif
#define DEFAULT_OPTIONS		\
	(Opt_scrollBar | Opt_jumpScroll)

/* rxvt_vars.Options2 */
#define Opt2_protectSecondary		(1LU<<0)
#define Opt2_cmdInitTabs			(1LU<<1)
#define Opt2_cmdAllTabs   			(1LU<<2)
#ifdef XFT_SUPPORT
# ifdef MULTICHAR_SET
#  define Opt2_xftNomFont			(1LU<<3)
#  define Opt2_xftSlowOutput		(1LU<<4)
# endif
# define Opt2_xftAntialias			(1LU<<5)
# define Opt2_xftHinting			(1LU<<6)
# define Opt2_xftAutoHint			(1LU<<7)
# define Opt2_xftGlobalAdvance		(1LU<<8)
#endif
#define Opt2_syncTabTitle			(1LU<<9)
#define Opt2_syncTabIcon			(1LU<<10)
#define Opt2_hideTabbar				(1LU<<11)
#define Opt2_bottomTabbar			(1LU<<12)
#define Opt2_borderLess				(1LU<<13)
#define Opt2_overrideRedirect		(1LU<<14)
#define Opt2_holdExit				(1LU<<15)
#define Opt2_broadcast				(1LU<<16)
#define Opt2_hideButtons			(1LU<<17)
#define Opt2_veryBold				(1LU<<18)
#define Opt2_disableHotkeys			(1LU<<19)
#define Opt2_disableDefaultHotkeys	(1LU<<20)
#ifdef HAVE_X11_SM_SMLIB_H
# define Opt2_enableSessionMgt		(1LU<<21)
#endif
#define Opt2_tabShell				(1LU<<22)
#define Opt2_linuxHomeEndKey		(1LU<<23)
#define DEFAULT_OPTIONS2	\
	(Opt2_veryBold)

/* place holder used for parsing command-line options */
#define Opt_Reverse					(1LU<<30)
#define Opt_Boolean					(1LU<<31)


#define PROPFONT_NORMAL				(1<<0)
#define PROPFONT_BOLD				(1<<1)
#define PROPFONT_MULTI				(1<<2)

/* ------------------------------------------------------------------------- */

#ifdef HAVE_MENUBAR
typedef struct {
    short           state;
    Window          win;
	GC				gc;
# ifdef BACKGROUND_IMAGE
	Pixmap			pixmap;
# endif
	unsigned long	fg;
	unsigned long	bg;
	unsigned long	topshadow;
	unsigned long	botshadow;
} menuBar_t;
#endif


#ifdef HAVE_SCROLLBARS
typedef struct {
    char            state;	/* scrollbar state */
    char            init;	/* scrollbar has been initialised */
    short           beg;	/* slider sub-window begin height */
    short           end;	/* slider sub-window end height */
    short           top;	/* slider top position */
    short           bot;	/* slider bottom position */
    short           style;	/* style: rxvt, xterm, next */
    short           width;	/* scrollbar width */
    Window          win;
    int             (*update)(struct rxvt_vars *, int, int, int, int);

	GC				gc;
# ifdef BACKGROUND_IMAGE
	Pixmap			pixmap;
# endif
# ifdef RXVT_SCROLLBAR
	unsigned long	rxvt_fg;
	unsigned long	rxvt_bg;
	unsigned long	rxvt_topshadow;
	unsigned long	rxvt_botshadow;
# endif
# ifdef XTERM_SCROLLBAR
	unsigned long	xterm_fg;
	unsigned long	xterm_bg;
	unsigned long	xterm_shadow;
# endif
# ifdef PLAIN_SCROLLBAR
	unsigned long	plain_fg;
	unsigned long	plain_bg;
# endif
# ifdef NEXT_SCROLLBAR
	unsigned long	next_fg;	/* black */
	unsigned long	next_bg;	/* gray */
	unsigned long	next_white;
	unsigned long	next_dark;
	GC				next_stippleGC;
    Pixmap			next_dimple, 
					next_upArrow,
					next_upArrowHi,
					next_downArrow,
					next_downArrowHi;
# endif
# ifdef SGI_SCROLLBAR
	unsigned long	sgi_fg;		/* medium */
	unsigned long	sgi_bg;		/* light */
	unsigned long	sgi_black;
	unsigned long	sgi_white;
	unsigned long	sgi_lmedium;
	unsigned long	sgi_dark;
	unsigned long	sgi_vdark;
	GC				sgi_stippleGC;
    Pixmap			sgi_dimple, 
					sgi_upArrow,
					sgi_upArrowHi,
					sgi_upArrowLow,
					sgi_downArrow,
					sgi_downArrowHi,
					sgi_downArrowLow;
# endif
} scrollBar_t;
#endif	/* HAVE_SCROLLBARS */


typedef struct {
	char		state;	/* tabbar state */

	short		ltab;	/* last tab */
	short		atab;	/* active tab */
	short		ptab;	/* previous active tab */
	short		fvtab;	/* first visible tab */
	short		lvtab;	/* last visible tab */

	Window		win;
#ifdef BACKGROUND_IMAGE
	Pixmap		pixmap;	/* tab background image */
#endif
	GC			gc;		/* tab background/foreground, grey25/black */
	unsigned long	fg;		/* foreground, black */
	unsigned long	bg;		/* background, grey25 */
	unsigned long	ifg;	/* inactive tab foreground, black */
	unsigned long	ibg;	/* inactive tab background, grey */
	char			rsfg;	/* fg resource has changed */
	char			rsbg;	/* bg resource has changed */
	char			rsifg;	/* ifg resource has changed */
	char			rsibg;	/* ibg resource has changed */
	unsigned long	frame;		/* tab frame, white */
	unsigned long	delimit;	/* delimit, dark grey */
#ifdef XFT_SUPPORT
	XftDraw*		xftwin;	/* XFT window */
	XftColor		xftfg;	/* foreground */
	XftColor		xftifg;	/* background */
#endif
} tabBar_t;


#ifdef BACKGROUND_IMAGE
typedef struct {
	short           w, h, x, y;
	Pixmap          pixmap; 
} bgPixmap_t;
#endif


/* To suppress compile warning without xpm library */
#ifdef BACKGROUND_IMAGE
# ifndef HAVE_LIBXPM
#  define XpmCloseness		(0)
#  define XpmColormap		(0)
#  define XpmVisual			(0)
#  define XpmDepth			(0)
#  define XpmSize			(0)
#  define XpmReturnPixels	(0)
typedef struct {
	unsigned long	valuemask;
	Visual*			visual;
	Colormap		colormap;
	unsigned int	depth;
	unsigned int	width;
	unsigned int	height;
	unsigned int	closeness;
} XpmAttributes;
# endif	/* HAVE_LIBXPM */
#endif	/* BACKGROUND_IMAGE */


typedef enum {
	TERMENV_XTERM = 0,
	TERMENV_RXVT,
	TERMENV_VT102,
	TERMENV_VT100,
	TERMENV_ANSI,
	TERMENV_DUMB,
} termenv_t;

struct term_t;
typedef struct {
	/*
	** Index to vts. If it's -1, then this term_t structure is not
	** used. Otherwise, it is used by pointer vts[vts_idx]. This
	** is to improve destroy performance so that we only need to
	** do (i = page..ltab) vts[i] = vts[i+1] instead of vterm[i] =
	** vterm[i+1].
	*/
	short			vts_idx;

	/* moved from TermWin_t */
    RUINT16T		saveLines;	/* number of lines to save */
    RUINT16T		num_scr;	/* number of lines scrolled */
    RUINT16T		nscrolled;	/* number of line actually scrolled */
    RUINT16T		view_start;	/* scrollback view starts here */
    RUINT16T		mapped;		/* window state mapped? */
	RUINT16T		init_screen;	/* screen structure initialized? */

    Window          vt; /* terminal window */
#ifdef XFT_SUPPORT
	XftDraw*		xftvt;
#endif
#ifdef BACKGROUND_IMAGE
    Pixmap			pixmap;		/* background image, NOT used by */
	bgPixmap_t		bg;			/* transparent window!!! */
	XpmAttributes   xpm_attr;	/* original attr of image */
#endif

	/* Apparently, people like fg/bg colors for individual terminal */
	unsigned long*	p_fg;
	unsigned long*	p_bg;
#ifdef XFT_SUPPORT
    XftColor*		p_xftfg;
    XftColor*		p_xftbg;
#endif

	/* moved from rxvt_t */
    text_t**		drawn_text;	/* text drawn on screen (characters) */
    rend_t**		drawn_rend;	/* text drawn on screen (rendition) */
    text_t**		buf_text;
    rend_t**		buf_rend;
    screen_t        screen;
#if NSCREENS
    screen_t        swap;
#endif
	/* move from hidden */
	rend_t			rstyle;

	RUINT16T		prev_ncol; /* previous columns */
	RUINT16T		prev_nrow; /* previous rows */
	/* moved from tab_t */
	short			tab_width; /* tab width */
	char UNTAINTED *	tab_title; /* tab title */

	/* moved from rxvt_t */
    int             cmd_fd;	/* pty file descriptor; connected to command */
    int             tty_fd;	/* tty file descriptor; connected to child */
	/* moved from hidden */
	int				current_screen;
	int				hidden_pointer; /* pointer is hidden? */
	pid_t			cmd_pid;
	char*			ttydev;
#ifndef RESET_TTY_TO_COMMON_DEFAULTS
	struct stat     ttyfd_stat; /* original status of our tty */
#endif
#ifndef NO_SETOWNER_TTYDEV
	unsigned char	next_tty_action;
#endif

	/* moved from hidden */
	unsigned long	PrivateModes;
	unsigned long	SavedModes;

#ifdef UTMP_SUPPORT
#ifndef UTEMPTER_SUPPORT
# ifdef HAVE_STRUCT_UTMP
    struct utmp     ut;
# endif
# if defined(HAVE_STRUCT_UTMPX) && !defined(HAVE_STRUCT_UTMP)
    struct utmpx    utx;
# endif
# if (defined(HAVE_STRUCT_UTMP) && defined(HAVE_UTMP_PID)) || defined(HAVE_STRUCT_UTMPX)
    char            ut_id[5];
# endif
    int             utmp_pos;
#endif	/* UTEMPTER_SUPPORT */
	unsigned char	next_utmp_action;
#endif

	char**			command_argv;
	int				command_argc;

	/* move from rxvt_hidden */
	ttymode_t		tio;
	unsigned int	ttymode;
	char			rvideo;
#ifdef MULTICHAR_SET
	char			chstat,
	/* set ==> we only got half a glyph */
					lost_multi,
	/* set ==> currently using 2 bytes per glyph */
					multi_byte;
#endif
	char			charsets[4];

	/* need to hold the terminal?
	**   hold == 0: not hold
	**   hold == 1: hold
	**   hold >  1: can destroy the terminal now
	*/
	char			hold;
	/* the terminal is dead or alive? */
	char			dead;
	/* the terminal is highlighted? */
	char			highlight;

	/* the terminal TERM type */
	termenv_t		termenv;

	clock_t			checksum;	/* unique id of this terminal */

	/* write out buffer */
	unsigned char*	v_buffer;	/* pointer to physical buffer */
	unsigned char*	v_bufstr;	/* beginning of area to write */
	unsigned char*	v_bufptr;	/* end of area to write */
	unsigned char*	v_bufend;	/* end of physical buffer */

	/* command input buffering */
	unsigned char*	cmdbuf_ptr;
	unsigned char*	cmdbuf_endp;
	unsigned char	cmdbuf_base[BUFSIZ];
} term_t;



typedef enum {
	HKF_DUMMY = 0,			/* dummy hotkey, not used */
	HKF_CHANGE_TITLE,		/* change tab title */
	HKF_NEW_TAB,			/* create new tab */
	HKF_KILL_TAB,			/* kill current tab */
	HKF_PREV_TAB,			/* activate previous tab */
	HKF_NEXT_TAB,			/* activate next tab */
	HKF_PREV_ATAB,			/* activate previous active tab */
	HKF_TAB_1,				/* activate tab 1 */
	HKF_TAB_2,				/* activate tab 2 */
	HKF_TAB_3,				/* activate tab 3 */
	HKF_TAB_4,				/* activate tab 4 */
	HKF_TAB_5,				/* activate tab 5 */
	HKF_TAB_6,				/* activate tab 6 */
	HKF_TAB_7,				/* activate tab 7 */
	HKF_TAB_8,				/* activate tab 8 */
	HKF_TAB_9,				/* activate tab 9 */
	HKF_TAB_10,				/* activate tab 10 */
	HKF_TAB_11,				/* activate tab 11 */
	HKF_TAB_12,				/* activate tab 12 */
	HKF_LMOVE_TAB,			/* move active tab to left */
	HKF_RMOVE_TAB,			/* move active tab to right */
	HKF_DUMP_SCREEN,		/* dump screen of current tab */
	HKF_INC_OPACITY,		/* increase opacity */
	HKF_DEC_OPACITY,		/* decrease opacity */
	HKF_TRANSPARENCY,		/* toggle transparency */
	HKF_HIDE_TABBAR,		/* hide/show tabbar */
	HKF_HIDE_SCROLLBAR,		/* hide/show scrollbar */
	HKF_HIDE_MENUBAR,		/* hide/show menubar */
	HKF_HIDE_BUTTON,		/* hide/show tabbar buttons */
	HKF_VERYBOLD,			/* toggle verybold font */
	HKF_HOLD_EXIT,			/* toggle hold exit */
	HKF_BROADCAST,			/* toggle input broadcasting */
	HKF_SMALL_FONT,			/* use smaller font */
	HKF_LARGE_FONT,			/* use large font */
	HKF_SCROLL_UP,			/* scroll up one line */
	HKF_SCROLL_DOWN,		/* scroll down one line */
	HKF_SCROLL_PGUP,		/* scroll up one page */
	HKF_SCROLL_PGDOWN,		/* scroll down one page */
	HKF_SAVE_CONFIG,		/* save configuration */
	HKF_COPY_SEL,			/* copy selection to clipboard */
	HKF_PASTE_SEL,			/* paste selection */
	NUM_HKFUNCS,
} hk_funcs_t;	/* hotkey functions */


#define HK_CTRL		((unsigned short) (1<<0))
#define HK_META		((unsigned short) (1<<1))
#define HK_SHFT		((unsigned short) (1<<2))
/* whether the hotkey should only work on primary screen */
#define HK_PRIMARY	((unsigned short) (1<<14))
/* whether the hotkey is an internal defined one */
#define HK_INTERNAL	((unsigned short) (1<<15))
#define HK_MASK		(HK_CTRL|HK_META|HK_SHFT)

#define HK_SET_CTRL(F)		((F) |= HK_CTRL)
#define HK_SET_META(F)		((F) |= HK_META)
#define HK_SET_SHFT(F)		((F) |= HK_SHFT)
#define HK_SET_PRIMARY(F)	((F) |= HK_PRIMARY)
#define HK_SET_INTERNAL(F)	((F) |= HK_INTERNAL)

#define HK_IS_CTRL(F)		((F) & HK_CTRL)
#define HK_IS_META(F)		((F) & HK_META)
#define HK_IS_SHFT(F)		((F) & HK_SHFT)
#define HK_IS_PRIMARY(F)	((F) & HK_PRIMARY)
#define HK_IS_INTERNAL(F)	((F) & HK_INTERNAL)

typedef struct hotkeys {
	unsigned short	func;			/* hotkey function */
	unsigned short	flag;			/* meta-key flags */
	KeySym			keysym;			/* key symbol pressed */
} hotkeys_t;

typedef struct hotkeys_handler {
	unsigned short	func;			/* hotkey function */
	int				(*handler) ();	/* function handler */
	char*			res;			/* X resource name */
} hotkeys_handler_t;

extern hotkeys_handler_t   hk_handlers[];


/*
** Maximal number of hotkeys. Now we make it a fix number, which is
** 4 times of hotkey functions. It may waste certain memory, but is
** easy to handle. BTW, who wants to define 4 hotkeys for just one
** function? ;-)
*/
#define MAX_HOTKEYS		((unsigned long) (NUM_HKFUNCS << 2))


typedef struct rxvt_vars {
	/*
	** These ``hidden'' items are not for public consumption and
	** must not be accessed externally
	*/
    struct rxvt_hidden *h;

	/*
	** Exposed items
	**   Changes to structure here require library version number change
	*/
    TermWin_t       TermWin;
#ifdef HAVE_SCROLLBARS
    scrollBar_t     scrollBar;
#endif
#ifdef HAVE_MENUBAR
    menuBar_t       menuBar;
#endif
	tabBar_t		tabBar;
	Display*		Xdisplay;
	unsigned long   Options;
	unsigned long   Options2;
	XSizeHints      szHint;

	/* hotkeys */
	hotkeys_t*		hotkeys;

	Colormap        Xcmap;
#ifdef OFF_FOCUS_FADING
	/* TOTAL_COLORS + 2 * MAX_PAGES */
	unsigned long*	PixColorsUnfocus;
	/* PixColorsUnfocus and PixColor has been switched */
	char			color_switched;
#endif
	/* Bg and UfBg has been switched */
	char			ufbg_switched;
	/* TOTAL_COLORS + 2 * MAX_PAGES */
	unsigned long*	PixColors;
#ifdef XFT_SUPPORT
    XftColor*		XftColors;	/* number of colors + 2 * NPAGES */
#endif
	short			numPixColors;	/* TOTAL_COLORS */

	Cursor			term_pointer; /* cursor for vt window */
	int				Xdepth;
	int				sb_shadow;	/* scrollbar shadow width */
	int				Xfd;		/* file descriptor of X connection */

	/* term_t structures and pointers */
	term_t			vterm[MAX_PAGES];
	term_t*			vts[MAX_PAGES];
	/* number of children that have died */
	short			vt_died;

	int				num_fds;	/* number of fd to monitor */
	selection_t     selection;
	sstyle_t		selection_style;
	int				numlock_state;
	char*			tabstop;	/* per location: 1 == tab-stop */
	enum enc_label  encoding_method;

	char**			global_argv;
	int				global_argc;
} rxvt_t;


typedef enum {
	HIDE_MENUBAR = 0,
	SHOW_MENUBAR,
	HIDE_TABBAR,
	SHOW_TABBAR,
	HIDE_SCROLLBAR,
	SHOW_SCROLLBAR,
	RESIZE_FONT,
	X_CONFIGURE,
} resize_reason_t;


/* MACROS for colors of individual terminals */
#define VTFG(R, P)		\
	((R)->PixColors[TOTAL_COLORS + (P)])
#define VTBG(R, P)		\
	((R)->PixColors[TOTAL_COLORS + MAX_PAGES + (P)])
#ifdef XFT_SUPPORT
# define VTXFTFG(R, P)		\
	((R)->XftColors[TOTAL_COLORS + (P)])
# define VTXFTBG(R, P)		\
	((R)->XftColors[TOTAL_COLORS + MAX_PAGES + (P)])
#endif	/* XFT_SUPPORT */
#define ISSET_VTFG(R, P)	\
	(NULL != ((R)->h->rs[Rs_color + TOTAL_COLORS + (P)]))
#define ISSET_VTBG(R, P)	\
	(NULL != ((R)->h->rs[Rs_color + TOTAL_COLORS + MAX_PAGES + (P)]))


/* MACROS for tab/page number */
#define ATAB(R)		((R)->tabBar.atab)
#define LTAB(R)		((R)->tabBar.ltab)
#define FVTAB(R)	((R)->tabBar.fvtab)
#define LVTAB(R)	((R)->tabBar.lvtab)
#define PTAB(R)		((R)->tabBar.ptab)

#define APAGE(R)	((R)->tabBar.atab)
#define LPAGE(R)	((R)->tabBar.ltab)
#define FVPAGE(R)	((R)->tabBar.fvtab)
#define LVPAGE(R)	((R)->tabBar.lvtab)
#define PPAGE(R)	((R)->tabBar.ptab)

/* MACROS for vts structure */
#define AVTS(R)		((R)->vts[(R)->tabBar.atab])
#define LVTS(R)		((R)->vts[(R)->tabBar.ltab])
#define PVTS(R, P)	((R)->vts[(P)])

#define SEL(R)		((R)->selection)

#define ASCR(R)		((R)->vts[(R)->tabBar.atab]->screen)
#define PSCR(R, P)	((R)->vts[(P)]->screen)


/*****************************************************************************
 *                                PROTOTYPES                                 *
 *****************************************************************************/
void		rxvt_main_loop(rxvt_t *);
rxvt_t*		rxvt_init (int, const char *const *);

#endif		/* __RXVTLIB_H__ */
/*----------------------- end-of-file (H source) -----------------------*/
