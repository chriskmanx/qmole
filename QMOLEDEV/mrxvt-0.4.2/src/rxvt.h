/*--------------------------------*-H-*---------------------------------*
 * File:	rxvt.h
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2003-2004   Marc Lehmann <pcg@goof.com>
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
** $Id: rxvt.h,v 1.157 2005/07/15 05:05:55 cvs Exp $
*/

#ifndef __RXVT_H__		/* include once only */
#define __RXVT_H__

#include "../config.h"
#include "feature.h"


/*
 *****************************************************************************
 * SYSTEM HACKS
 *****************************************************************************
 */
/* Consistent defines - please report on the necessity
 * @ Unixware: defines (__svr4__)
 */
#if defined (SVR4) && !defined (OS_SVR4)
# define OS_SVR4
#endif
 /*
#if defined (sun) && !defined (__sun__)
# define __sun__
#endif
*/

#ifdef _SCO_DS
# define ALL_NUMERIC_PTYS
#endif


#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif

#ifdef HAVE_STDARG_H
# include <stdarg.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#if defined(HAVE_STRING_H) && !defined(OUR_STRINGS)
# include <string.h>
#endif

#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#ifdef HAVE_WCHAR_H
/* Ugly hack for OpenBSD. There should be better way... */
# ifdef OS_OPENBSD
typedef unsigned int    _our_wint_t;
typedef struct {
	int __count;
	union {
		_our_wint_t	__wch;
		char		__wchb[4]
	} __value;
} mbstate_t;
# endif	/* OS_OPENBSD */
# include <wchar.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if defined(OS_SOLARIS) && defined(__SVR4)
# include <sys/strredir.h>
#endif

#ifdef OS_SOLARIS
# include <sys/byteorder.h>
#endif

#ifdef OS_SOLARIS
# include <sys/int_types.h>
# undef HAVE_SYS_IOCTL_H
#endif

#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif

#ifdef HAVE_SYS_SOCKIO_H
# include <sys/sockio.h>
#endif

#if defined(PTYS_ARE_PTMX) && defined(HAVE_SYS_STROPTS_H)
# include <sys/stropts.h>	/* for I_PUSH */
#endif

#if defined(PTYS_ARE_PTMX) && !defined(OS_CYGWIN)
# include <sys/resource.h>
# define _NEW_TTY_CTRL		/* get proper definition in <termios.h> */
#endif

#ifdef HAVE_TIME_H
# include <time.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#ifdef HAVE_SYS_STRREDIR_H
# include <sys/strredir.h>
#endif

#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif

#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif

#ifdef HAVE_NET_IF_H
# include <net/if.h>
#endif

#ifdef HAVE_NET_IF_ARP_H
# include <net/if_arp.h>
#endif

#ifdef HAVE_PTY_H
# include <pty.h>
#endif

#ifdef OS_QNX
# include <process.h>
# include <sys/utsname.h>
# define ut_name	ut_user
#endif

#ifdef TTY_GID_SUPPORT
# include <grp.h>
#endif

#ifdef HAVE_TERMIOS_H
# include <termios.h>
typedef struct termios ttymode_t;
#else
# ifdef HAVE_SGTTY_H
#  include <sgtty.h>
# endif
typedef struct {
    struct sgttyb   sg;
    struct tchars   tc;
    struct ltchars  lc;
    int             line;
    int             local;
} ttymode_t;
#endif


#ifdef HAVE_LIBX11
# include <X11/Xlib.h>
# include <X11/X.h>
# include <X11/Xutil.h>
# include <X11/Xlibint.h>
# include <X11/cursorfont.h>
# include <X11/keysym.h>
# include <X11/keysymdef.h>
# include <X11/Xmd.h>
# include <X11/Xatom.h>
# include <X11/Xresource.h>
typedef struct {
	CARD32	flags;
	CARD32	functions;
	CARD32	decorations;
	INT32	input_mode;
	CARD32	status;
} MWMHints;
# ifdef HAVE_X11_SM_SMLIB_H
#  include <X11/SM/SMlib.h>
# endif
#endif

#ifndef HAVE_XPOINTER
typedef char*		XPointer;
#endif


#ifdef HAVE_LIBXPM
# include <X11/xpm.h>
#endif

#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
# ifdef USE_JPEG
#  include <jpeglib.h>
# endif
# ifdef USE_PNG
#  ifdef OS_OPENBSD
#   include <libpng/png.h>
#  else
#   include <png.h>
#  endif	/* OS_OPENBSD */
# endif	/* USE_PNG */
#endif

/* Xft.h seems to conflict with png.h. So we must include it
** after pnt.h */
#ifdef HAVE_LIBX11
# ifdef XFT_SUPPORT
#  include <X11/Xft/Xft.h>
# endif
# ifdef HAVE_ICONV_H
#  include <iconv.h>
# endif
#endif

/*
** It seems Mac OS X's fontconfig does not have following definitions
*/
#ifdef XFT_SUPPORT
# ifndef FC_WIDTH
#  define FC_WIDTH					"width"
#  define FC_WIDTH_ULTRACONDENSED	(50)
#  define FC_WIDTH_EXTRACONDENSED	(63)
#  define FC_WIDTH_CONDENSED		(75)
#  define FC_WIDTH_SEMICONDENSED	(87)
#  define FC_WIDTH_NORMAL			(100)
#  define FC_WIDTH_SEMIEXPANDED		(113)
#  define FC_WIDTH_EXPANDED			(125)
#  define FC_WIDTH_EXTRAEXPANDED	(150)
#  define FC_WIDTH_ULTRAEXPANDED	(200)
# endif
#endif


#ifdef HAVE_XSETLOCALE
# define X_LOCALE
# include <X11/Xlocale.h>
#else
# ifdef HAVE_LOCALE_H
#  include <locale.h>
# endif
#endif


#ifdef HAVE_NL_LANGINFO
# include <langinfo.h>
#endif


#ifdef UTMP_SUPPORT
# ifdef UTEMPTER_SUPPORT
#  include <utempter.h>
# endif
# ifdef HAVE_UTMP_H
#  include <utmp.h>
#  ifdef HAVE_SETUTENT
#   define RXVT_UTMP_SYSV
#  endif
# endif
# if defined(HAVE_UTMPX_H) && !defined(HAVE_UTMP_H)
#  include <utmpx.h>
#  define RXVT_UTMP_SYSV
# endif
# ifdef HAVE_LASTLOG_H
#  include <lastlog.h>
# endif
#endif


#if defined(UTMP_SUPPORT) || defined(HAVE_GETPWUID)
# include <pwd.h>
#endif


#ifdef UTMP_SUPPORT
# if defined(HAVE_UTMPX_H) && !defined(HAVE_UTMP_H)
#  define UTMP_FILENAME	UTMPX_FILE
# endif
# if !defined(UTMP_FILE) && defined(_PATH_UTMP)
#  define UTMP_FILE     _PATH_UTMP
# endif
# if !defined(UTMP_FILENAME) && defined(UTMP_FILE)
#  define UTMP_FILENAME UTMP_FILE
# endif
# define RXVT_UTMP_FILE	UTMP_FILENAME
#endif

#ifdef WTMP_SUPPORT
# if defined(HAVE_WTMPX_H) && !defined(HAVE_WTMP_H)
#  define WTMP_FILENAME	WTMPX_FILE
# endif
# if !defined(WTMP_FILE) && defined(_PATH_WTMP)
#  define WTMP_FILE     _PATH_WTMP
# endif
# if !defined(WTMP_FILENAME) && defined(WTMP_FILE)
#  define WTMP_FILENAME WTMP_FILE
# endif
# define RXVT_WTMP_FILE	WTMP_FILENAME
#endif

#ifdef LASTLOG_SUPPORT
# ifdef OS_SOLARIS
#  define LASTLOG_FILE		"/var/adm/lastlog"
# endif
# ifdef OS_OSF
#  define LASTLOG_FILE		"/var/adm/lastlog"
# endif
# if !defined(LASTLOG_FILE) && defined(_PATH_LASTLOG)
#  define LASTLOG_FILE     _PATH_LASTLOG
# endif
# if !defined(LASTLOG_FILENAME) && defined(LASTLOG_FILE)
#  define LASTLOG_FILENAME LASTLOG_FILE
# endif
# define RXVT_LASTLOG_FILE	LASTLOG_FILENAME
#endif

#ifdef UTMP_SUPPORT
# ifdef RXVT_UTMP_SYSV
#  ifndef USER_PROCESS
#   define USER_PROCESS		(7)
#  endif
#  ifndef DEAD_PROCESS
#   define DEAD_PROCESS		(8)
#  endif
# endif
#endif



#ifdef NO_SECONDARY_SCREEN
# define NSCREENS		0
#else
# define NSCREENS		1
#endif


/*
** Let's try some interesting static program analysis tools.
** Refer to http://www.cs.umd.edu/~jfoster/cqual/
**
** Tainted defines the type of data that is fetchted from user input,
** which in turn is unsafe without checking.
** Untainted defines the type of data that is safe.
*/
#ifdef __GNUC__
# define $tainted
# define $untainted
# define TAINTED	$tainted
# define UNTAINTED	$untainted
#else
# define TAINTED
# define UNTAINTED
#endif


/*
*********************************************************
** The following are internal includes
*********************************************************
*/
#include "encoding.h"
#include "rxvtlib.h"


#ifdef GREEK_SUPPORT
# include "grkelot.h"
#endif

#ifdef HAVE_MENUBAR
# include "menubar.h"
#endif

#include "command.h"
#include "init.h"


#ifndef STDIN_FILENO
# define STDIN_FILENO	0
# define STDOUT_FILENO	1
# define STDERR_FILENO	2
#endif

#if defined(HAVE_GRANTPT) && defined(HAVE_UNLOCKPT)
# if defined(PTYS_ARE_GETPT) || defined(PTYS_ARE_PTMX)
#  define NO_SETOWNER_TTYDEV
# endif
#endif
#if defined(OS_CYGWIN) || defined(PTYS_ARE_OPENPTY)
# define NO_SETOWNER_TTYDEV
#endif

/*
 *****************************************************************************
 * STRUCTURES AND TYPEDEFS
 *****************************************************************************
 */
struct rxvt_vars;		/* Later REDEFINED and typedef'd to rxvt_t */
struct rxvt_hidden;


/* Motif window hints, MwmHints.flags */
#define MWM_HINTS_FUNCTIONS     (1L << 0)
#define MWM_HINTS_DECORATIONS   (1L << 1)
#define MWM_HINTS_INPUT_MODE    (1L << 2)
#define MWM_HINTS_STATUS        (1L << 3)
/* bit definitions for MwmHints.functions */
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)
/* bit definitions for MwmHints.decorations */
#define MWM_DECOR_ALL           (1L << 0)
#define MWM_DECOR_BORDER        (1L << 1)
#define MWM_DECOR_RESIZEH       (1L << 2)
#define MWM_DECOR_TITLE         (1L << 3)
#define MWM_DECOR_MENU          (1L << 4)
#define MWM_DECOR_MINIMIZE      (1L << 5)
#define MWM_DECOR_MAXIMIZE      (1L << 6)
/* bit definitions for MwmHints.inputMode */
#define MWM_INPUT_MODELESS                  0
#define MWM_INPUT_PRIMARY_APPLICATION_MODAL 1
#define MWM_INPUT_SYSTEM_MODAL              2
#define MWM_INPUT_FULL_APPLICATION_MODAL    3
#define PROP_MWM_HINTS_ELEMENTS             5


/* Sanitize menubar info */
#ifndef HAVE_MENUBAR
# undef MENUBAR_MAX
#endif
#ifndef MENUBAR_MAX
# define MENUBAR_MAX	0
#endif

/* If we're using either the rxvt scrollbar or menu bars, keep the
 * scrollColor resource.
 */
#if defined(RXVT_SCROLLBAR) || defined(HAVE_MENUBAR)
# define KEEP_SCROLLCOLOR 1
#else
# undef KEEP_SCROLLCOLOR
#endif


/*
 * the 'essential' information for reporting Mouse Events
 * pared down from XButtonEvent
 */
struct mouse_event {
    int             clicks;
    Time            time;	/* milliseconds */
    unsigned int    state;	/* key or button mask */
    unsigned int    button;	/* detail */
};

#ifndef min
# define min(a,b)	(((a) < (b)) ? (a) : (b))
# define max(a,b)	(((a) > (b)) ? (a) : (b))
#endif

#define MAX_IT(current, other)	if ((other) > (current)) (current) = (other)
#define MIN_IT(current, other)	if ((other) < (current)) (current) = (other)
#define SWAP_IT(one, two, typeof)					\
    do {								\
	typeof          swapittmp;					\
	(swapittmp) = (one); (one) = (two); (two) = (swapittmp);	\
    } while (/* CONSTCOND */ 0)
#define BOUND_POSITIVE_INT16(val)			\
    (RINT16T)((val) <= 0				\
	      ? 0					\
	      : min((val), (((RUINT16T)-1)>>1)))

/*
 *****************************************************************************
 * NORMAL DEFINES
 *****************************************************************************
 */

#if defined (NO_OLD_SELECTION) && defined(NO_NEW_SELECTION)
# error if you disable both selection styles, how can you select, silly?
#endif

#define APL_CLASS		"XTerm"		/* class name */
#define APL_SUBCLASS	"Mrxvt"	/* also check resources under this name */
#define APL_NAME		"mrxvt"	/* normal name */

/* COLORTERM, TERM environment variables */
#define COLORTERMENV	"rxvt"
#ifdef BACKGROUND_IMAGE
# define COLORTERMENVFULL COLORTERMENV "-xpm"
#else
# define COLORTERMENVFULL COLORTERMENV
#endif
#ifndef TERMENV
# define TERMENV		"xterm"
#endif

#define TABTITLEENV		"MRXVT_TABTITLE="


#if defined (NO_MOUSE_REPORT) && !defined (NO_MOUSE_REPORT_SCROLLBAR)
# define NO_MOUSE_REPORT_SCROLLBAR
#endif

#ifdef NO_RESOURCES
# undef USE_XGETDEFAULT
#endif

/* now look for other badly set stuff */

#if !defined (EACCESS) && defined(EAGAIN)
# define EACCESS EAGAIN
#endif

#ifndef EXIT_SUCCESS		/* missing from <stdlib.h> */
# define EXIT_SUCCESS		0	/* exit function success */
# define EXIT_FAILURE		1	/* exit function failure */
#endif

#define menuBar_esc			(10)
#define scrollBar_esc		(30)
#define MENUBAR_MARGIN		(2)	/* margin below text */

/* width of scrollBar, menuBar shadow, must be 1 or 2 */
#ifdef HALFSHADOW
# define SHADOW				(1)
#else
# define SHADOW				(2)
#endif

#define R_SB_ALIGN_CENTRE	(0)
#define R_SB_ALIGN_TOP		(1)
#define R_SB_ALIGN_BOTTOM	(2)

#define R_SB_UNKNOWN		(-1)
#define R_SB_RXVT			(0)
#define R_SB_NEXT			(1)
#define R_SB_XTERM			(2)
#define R_SB_SGI			(3)
#define R_SB_PLAIN			(4)

#define SB_WIDTH_PLAIN		(15)
#define SB_WIDTH_XTERM		(15)
#define SB_WIDTH_RXVT		(10)
#define SB_WIDTH_NEXT		(17)
#define SB_WIDTH_SGI		(16)


#ifdef NEXT_SCROLLBAR
/*
** NeXT scrollbar definitions
*/
#define NEXT_SB_WIDTH			(SB_WIDTH_NEXT)
#define NEXT_SB_PAD				(1)
#define NEXT_SB_BD_WIDTH		(1)
#define NEXT_BEVEL_ULEFT_WIDTH	(1)
#define NEXT_BEVEL_LRIGHT_WIDTH	(2)
#define NEXT_SB_LPAD			(NEXT_SB_PAD + NEXT_SB_BD_WIDTH)
#define NEXT_SB_MARGIN			(NEXT_SB_PAD<<1)
#define NEXT_SB_BTN_WIDTH		(NEXT_SB_WIDTH - NEXT_SB_MARGIN - NEXT_SB_BD_WIDTH)
/* button height */
#define NEXT_SB_BTN_HEIGHT		(NEXT_SB_BTN_WIDTH)
/* single button height */
#define NEXT_SB_SBTN_HEIGHT		(NEXT_SB_BTN_HEIGHT + NEXT_SB_PAD)
/* double button height */
#define NEXT_SB_DBTN_HEIGHT		(NEXT_SB_SBTN_HEIGHT<<1)
/* total button height */
#define NEXT_SB_TBTN_HEIGHT		(NEXT_SB_DBTN_HEIGHT + NEXT_SB_PAD)
#define NEXT_BEVEL_X			(NEXT_SB_LPAD)
#define NEXT_BTN_FACE_X			(NEXT_BEVEL_X + NEXT_BEVEL_ULEFT_WIDTH)
#define NEXT_SB_MIN_HEIGHT		(NEXT_SB_BTN_WIDTH - (NEXT_SB_PAD<<1))
 /*
  *    +-------------+
  *    |             | <---< NEXT_SB_PAD
  *    | ::::::::::: |
  *    | ::::::::::: |
  *   '''''''''''''''''
  *   ,,,,,,,,,,,,,,,,,
  *    | ::::::::::: |
  *    | ::::::::::: |
  *    |  +---------------< NEXT_BEVEL_ULEFT_WIDTH
  *    |  | :::::::: |
  *    |  V :::: vv-------< NEXT_BEVEL_LRIGHT_WIDTH
  *    | +---------+ |
  *    | | ......%%| |
  *    | | ......%%| |
  *    | | ..()..%%| |
  *    | | ......%%| |
  *    | | %%%%%%%%| |
  *    | +---------+ | <.........................
  *    |             | <---< NEXT_SB_PAD         :
  *    | +---------+ | <-+..........            :---< NEXT_SB_TBTN_HEIGHT
  *    | | ......%%| |   |         :            :
  *    | | ../\..%%| |   |---< NEXT_SB_BTN_HEIGHT :
  *    | | %%%%%%%%| |   |         :            :
  *    | +---------+ | <-+         :            :
  *    |             |             :            :
  *    | +---------+ | <-+         :---< NEXT_SB_DBTN_HEIGHT
  *    | | ......%%| |   |         :            :
  *    | | ..\/..%%| |   |         :            :
  *    | | %%%%%%%%| |   |---< NEXT_SB_SBTN_HEIGHT
  *    | +---------+ |   |         :            :
  *    |             |   |         :            :
  *    +-------------+ <-+.........:............:
  *    ^^|_________| :
  *    ||     |      :
  *    ||     +---< NEXT_SB_BTN_WIDTH
  *    ||            :
  *    |+------< NEXT_SB_PAD
  *    |:            :
  *    +----< NEXT_SB_BD_WIDTH
  *     :            :
  *     :............:
  *           |
  *           +---< NEXT_SB_WIDTH
  */
#endif	/* NEXT_SCROLLBAR */


#ifdef SGI_SCROLLBAR
/*
** SGI IRIX scrollbar definitions
*/
#define SGI_ARROW_SOURCE_WIDTH	(14)
#define SGI_ARROW_SOURCE_HEIGHT	(14)
#define SGI_BEVEL_HI_WIDTH		(1)
#define SGI_BEVEL_LO_WIDTH		(1)
#define SGI_BEVEL_SIZE			(SGI_BEVEL_HI_WIDTH+SGI_BEVEL_LO_WIDTH)

#define SGI_ARROW_WIDTH			(SB_WIDTH_SGI-(SGI_BEVEL_SIZE<<1))
#define SGI_ARROW_HEIGHT		(SGI_ARROW_SOURCE_HEIGHT)

#define SGI_SB_BUTTON_HEIGHT	((SGI_BEVEL_SIZE<<1)+SGI_ARROW_WIDTH)
#define SGI_SB_BUTTONS_HEIGHT	(SGI_SB_BUTTON_HEIGHT<<1)
#endif



#define NO_REFRESH			(0)	/* Window not visible at all! */
#define FAST_REFRESH		(1<<0)	/* Fully exposed window */
#define SLOW_REFRESH		(1<<1)	/* Partially exposed window */
#define SMOOTH_REFRESH		(1<<2)	/* Do sync'ing to make it smooth */
#define REFRESH_BOUNDS		(1<<3)

#define IGNORE			0
#define SAVE			's'
#define RESTORE			'r'

/* special (internal) prefix for font commands */
#define FONT_CMD		'#'
#define FONT_DN			"#-"
#define FONT_UP			"#+"

/* flags for rxvt_scr_gotorc() */
#define C_RELATIVE		1	/* col movement is relative */
#define R_RELATIVE		2	/* row movement is relative */
#define RELATIVE		(R_RELATIVE|C_RELATIVE)

/* modes for rxvt_scr_insdel_chars(), rxvt_scr_insdel_lines() */
#define INSERT			-1	/* don't change these values */
#define DELETE			+1
#define ERASE			+2

/* modes for rxvt_scr_page() - scroll page. used by scrollbar window */
enum page_dirn {
    UP,
    DN,
    NO_DIR
};

/* arguments for rxvt_scr_change_screen() */
enum {
    PRIMARY = 0,
    SECONDARY
};

enum {
    SBYTE = 0,
    WBYTE
};


#define RS_None			0	/* Normal */

#if defined(TTY_256COLOR)
/* have at least 32 bits to use */
# define RS_fgMask			0x000001FFu	/* 512 colors */
# define RS_bgMask			0x0003FE00u	/* 512 colors */
# define RS_Bold			0x00040000u	/* bold */
# define RS_Blink			0x00080000u	/* blink */
# define RS_RVid			0x00100000u	/* reverse video */
# define RS_Uline			0x00200000u	/* underline */
# define RS_acsFont			0x00400000u	/* ACS graphics char set */
# define RS_ukFont			0x00800000u	/* UK character set */
#else
/* may only have 16 bits to use so squash them in */
# define RS_fgMask			0x0000001Fu	/* 32 colors */
# define RS_bgMask			0x000003E0u	/* 32 colors */
# define RS_Bold			0x00000400u	/* bold */
# define RS_Blink			0x00000800u	/* blink */
# define RS_RVid			0x00001000u	/* reverse video */
# define RS_Uline			0x00002000u	/* underline */
# define RS_acsFont			0x00004000u	/* ACS graphics char set */
# define RS_ukFont			0x00008000u	/* UK character set */
#endif

#ifdef MULTICHAR_SET
# define RS_multi0			0x10000000u	/* only multibyte characters */
# define RS_multi1			0x20000000u	/* multibyte 1st byte */
/* multibyte 2nd byte */
# define RS_multi2			(RS_multi0|RS_multi1)
/* multibyte mask */
# define RS_multiMask		(RS_multi0|RS_multi1)
# define IS_MULTI1(R)		(((R) & RS_multiMask) == RS_multi1)
# define IS_MULTI2(R)		(((R) & RS_multiMask) == RS_multi2)
#else
# define RS_multiMask		(0)
# define IS_MULTI1(r)		(0)
# define IS_MULTI2(r)		(0)
#endif


#define UNICODE_MASK 0x1fffffUL

#if UNICODE3
# define COMPOSE_LO 0x40000000UL
# define COMPOSE_HI 0x400fffffUL
# define IS_COMPOSE(n) ((int32_t)(n) >= COMPOSE_LO)
#else
# define COMPOSE_LO 0xd800UL
# define COMPOSE_HI 0xf8ffUL // dfff should be safer, but...
# define IS_COMPOSE(n) (COMPOSE_LO <= (n) && (n) <= COMPOSE_HI)
#endif


#define RS_fontMask			(RS_acsFont|RS_ukFont)
#define RS_baseattrMask		(RS_Bold|RS_Blink|RS_RVid|RS_Uline)
#define RS_attrMask			(RS_baseattrMask|RS_fontMask|RS_multiMask)

#define Sel_none			0	/* Not waiting */
#define Sel_normal			0x01	/* normal selection */
#define Sel_incr			0x02	/* incremental selection */
#define Sel_direct			0x00
#define Sel_Primary			0x01
#define Sel_Secondary		0x02
#define Sel_Clipboard		0x03
#define Sel_whereMask		0x0f
#define Sel_CompoundText	0x10	/* last request was Compound */

enum {
    C0_NUL = 0x00,
            C0_SOH, C0_STX, C0_ETX, C0_EOT, C0_ENQ, C0_ACK, C0_BEL,
    C0_BS , C0_HT , C0_LF , C0_VT , C0_FF , C0_CR , C0_SO , C0_SI ,
    C0_DLE, C0_DC1, C0_DC2, D0_DC3, C0_DC4, C0_NAK, C0_SYN, C0_ETB,
    C0_CAN, C0_EM , C0_SUB, C0_ESC, C0_IS4, C0_IS3, C0_IS2, C0_IS1
}; 
#define CHAR_ST			0x9c	/* 0234 */


/*
** Xwsh escape sequences: ESC P Ps .y Pt ESC
*/
#define Xwsh_title				(1)
#define Xwsh_iconName			(3)
#define Xwsh_textColor			(4)
#define Xwsh_pageColor			(5)
#define Xwsh_selTextColor		(6)
#define Xwsh_selPageColor		(7)
#define Xwsh_cursorTextColor	(8)
#define Xwsh_cursorPageColor	(9)
#define Xwsh_halfIntColor		(10)
#define Xwsh_boldIntColor		(11)
#define Xwsh_bindStrKeyVal		(101) /* not implemented */
#define Xwsh_bindStrKeyFunc		(103) /* not implemented */


/*
** XTerm Operating System Commands: ESC ] Ps;Pt (ST|BEL)
** colour extensions by Christian W. Zuckschwerdt <zany@triq.net>
*/
#define XTerm_name			(0)
#define XTerm_iconName		(1)
#define XTerm_title			(2)
#define XTerm_Color			(4)  /* change colors */
#define XTerm_Color_cursor	(12) /* change actual 'Cursor' color */
#define XTerm_Color_pointer	(13) /* change actual 'Pointer' color */
#define XTerm_Color_RV		(17) /* change actual 'Highlight' color */
#define XTerm_Color_BD		(18) /* change actual 'Bold' color */
#define XTerm_Color_UL		(19) /* change actual 'Underline' color */
#define XTerm_logfile		(46) /* not implemented */
#define XTerm_font			(50)


/*
** rxvt extensions of XTerm OSCs: ESC ] Ps;Pt (ST|BEL)
*/
#define XTerm_Menu			(10) /* set menu item */
#define XTerm_Pixmap		(20) /* new bg pixmap */
#define XTerm_restoreFG		(39) /* change default fg color */
#define XTerm_restoreBG		(49) /* change default bg color */
#define XTerm_dumpscreen	(55) /* dump scrollback and all screen */


/*
** mrxvt extensions of XTerm OSCs: ESC ] Ps;Pt (ST|BEL)
** Example: echo "\e]61;newtitle\a"
*/
#define Xterm_tab			(61) /* change tab title */
#define Xterm_tabterm		(62) /* change tab and terminal title */
#define Xterm_newtab		(63) /* create a new tab with title */
#define Xterm_prevtab		(64) /* switch to previous tab */
#define Xterm_nexttab		(65) /* switch to next tab */
#define Xterm_tint			(66) /* change tinting color */
#define Xterm_shade			(67) /* change shade level */
#define Xterm_encode		(68) /* change encoding */
#define Xterm_hide			(69) /* hide/show tabbar */
#define Xterm_opacity		(70) /* set opacity level */
#define Xterm_tabbtn		(71) /* hide/show tabbar buttons */
#define Xterm_tabfg			(72) /* change active tab fg */
#define Xterm_tabbg			(73) /* change tabbar/active tab bg */
#define Xterm_itabfg		(74) /* change inactive tab fg */
#define Xterm_itabbg		(75) /* change inactive tab bg */
#define Xterm_trans			(76) /* toggle transparency */
#define Xterm_moveleft		(77) /* move active tab to left */
#define Xterm_moveright		(78) /* move active tab to right */
#define Xterm_verybold		(79) /* toggle bold font for color text */
#define Xterm_hotkeys		(80) /* toggle hotkeys */
#define Xterm_saveconfig	(81) /* save configuration */
#define Xterm_bgfade		(82) /* set bgfade degree */
#define Xterm_termenv		(83) /* set TERMENV type */



/* Words starting with `Color_' are colours.  Others are counts */
/*
** The following comment is mostly obsolete since pixcolor_set was
** expanded:
** We're currently upto 30 colours.  Only 2 more available.  The
** PixColor and rendition colour usage should probably be decoupled
** on the unnecessary items, e.g. Color_pointer, but won't bother
** until we need to.  Also, be aware of usage in pixcolor_set
*/

enum colour_list {
    Color_fg = 0,
    Color_bg,
    minCOLOR,			/* 2 */
    Color_Black = minCOLOR,
    Color_Red3,
    Color_Green3,
    Color_Yellow3,
    Color_Blue3,
    Color_Magenta3,
    Color_Cyan3,
    maxCOLOR,			/* minCOLOR + 7 */
#ifndef NO_BRIGHTCOLOR
    Color_AntiqueWhite = maxCOLOR,
    minBrightCOLOR,		/* maxCOLOR + 1 */
    Color_Grey25 = minBrightCOLOR,
    Color_Red,
    Color_Green,
    Color_Yellow,
    Color_Blue,
    Color_Magenta,
    Color_Cyan,
    maxBrightCOLOR,		/* minBrightCOLOR + 7 */
    Color_White = maxBrightCOLOR,
#else
    Color_White = maxCOLOR,
#endif
#ifdef TTY_256COLOR
    min256COLOR = Color_White + 1,
    max256COLOR = minCOLOR + 255,
#endif
#ifndef NO_CURSORCOLOR
    Color_cursor,
    Color_cursor2,
#endif
    Color_pointer,
    Color_border,
    Color_ufbg,
#ifndef NO_BOLD_UNDERLINE_REVERSE
    Color_BD,
    Color_UL,
    Color_RV,
#endif
#ifdef OPTION_HC
    Color_HC,
#endif
#ifdef KEEP_SCROLLCOLOR
    Color_scroll,
    Color_trough,
#endif
#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
# ifdef TINTING_SUPPORT
	Color_tint,
# endif
#endif	/* BACKGROUND_IMAGE || TRANSPARENT */
    NRS_COLORS,			/* */
#ifdef KEEP_SCROLLCOLOR
    Color_topShadow = NRS_COLORS,
    Color_bottomShadow,
    TOTAL_COLORS		/* upto 31 */
#else
    TOTAL_COLORS = NRS_COLORS	/* */
#endif
};

#ifdef TTY_256COLOR
# define Color_Bits	9
# define NPIXCLR_SETS	9	/* (256 + 14) bits / 32 bits */
#else
# define Color_Bits	5
# define NPIXCLR_SETS	1	/* (16 + 14) bits / 32 bits */
#endif
#define NPIXCLR_BITS	32

#define DEFAULT_RSTYLE		(RS_None | (Color_fg) | (Color_bg<<Color_Bits))



/*
 * Resource list
 */
enum {
    Rs_display_name = 0,
    Rs_term_name,
    Rs_iconName,
    Rs_geometry,
    Rs_reverseVideo,
    Rs_color,
    _Rs_color = Rs_color + TOTAL_COLORS + 2 * MAX_PAGES - 1,
    Rs_font,
    _Rs_font = Rs_font + MAX_NFONTS - 1,
#ifdef MULTICHAR_SET
    Rs_mfont,
    _Rs_mfont = Rs_mfont + MAX_NFONTS - 1,
    Rs_multichar_encoding,
    Rs_mc_hack,
#endif
    Rs_name,
    Rs_title,
    Rs_tabtitleAll,
    Rs_tabtitle,
    _Rs_tabtitle = Rs_tabtitle + MAX_PAGES - 1,
#if defined (BACKGROUND_IMAGE) || (MENUBAR_MAX)
    Rs_path,
#endif

#ifdef BACKGROUND_IMAGE
    Rs_backgroundPixmapAll,/* terminal background pixmap for all tabs */
    Rs_backgroundPixmap,/* terminal background pixmap for each tab */
    _Rs_backgroundPixmap = Rs_backgroundPixmap + MAX_PAGES - 1,
	Rs_tabbarPixmap,	/* tabbar background pixmap */
	Rs_tabPixmap,		/* use tabbar bg pixmap for tabs */
	Rs_appIcon,			/* use pixmap as application icon */
#endif

#if (MENUBAR_MAX)
    Rs_menu,
#endif
#ifdef HAVE_MENUBAR
    Rs_showMenu,
# ifdef BACKGROUND_IMAGE
	Rs_menubarPixmap,
# endif
#endif

#ifndef NO_BOLDFONT
    Rs_boldFont,
#endif
#ifdef GREEK_SUPPORT
    Rs_greek_keyboard,
    Rs_greektoggle_key,
#endif
#ifdef XFT_SUPPORT
	Rs_xft,		/* Use XFT? */
	Rs_xftfont,	/* Font name, family */
# ifdef MULTICHAR_SET
	Rs_xftmfont,/* Multichar font name, family */
	Rs_xftmsz,	/* Multichar font size */
# endif
	Rs_xftwt,	/* Font weight */
	Rs_xftst,	/* Font slant */
	Rs_xftsz,	/* Font size */
	Rs_xftwd,	/* Font width style */
	Rs_xftrgb,	/* Font sub-pixel order */
#endif
#ifdef THAI
	Rs_thai,
#endif
    Rs_loginShell,
    Rs_jumpScroll,
#ifdef HAVE_SCROLLBARS
    Rs_scrollBar,
    Rs_scrollBar_right,
    Rs_scrollBar_floating,
    Rs_scrollBar_align,
# ifdef BACKGROUND_IMAGE
	Rs_scrollbarPixmap,
# endif
#endif	/* HAVE_SCROLLBARS */
    Rs_scrollBar_style,
    Rs_scrollTtyOutputInhibit,
    Rs_scrollTtyKeypress,
    Rs_scrollWithBuffer,
    Rs_saveLinesAll,
    Rs_saveLines,
    _Rs_saveLines = Rs_saveLines + MAX_PAGES - 1,

	Rs_tabfg,	/* active tab foreground */
	Rs_tabbg,	/* active tab background */
	Rs_itabfg,	/* inactive tab foreground */
	Rs_itabbg,	/* inactive tab background */

#ifdef OFF_FOCUS_FADING
	Rs_fade,	/* fade percentage */
#endif

#ifdef TEXT_SHADOW
	Rs_textShadow,		/* text shadow color */
	Rs_textShadowMode,	/* text shadow color mode */
#endif

#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
	Rs_backgroundFade,	/* fade percentage */
# ifdef TINTING_SUPPORT
	Rs_shade,	/* shade percentage */
	Rs_tint,	/* tinting color */
# endif
#endif

    Rs_utmpInhibit,
    Rs_visualBell,
	Rs_bellCommand,
	Rs_holdExitText,
	Rs_desktop,
#if ! defined(NO_MAPALERT) && defined(MAPALERT_OPTION)
    Rs_mapAlert,
#endif
#ifdef META8_OPTION
    Rs_meta8,
#endif
#ifdef MOUSE_WHEEL
    Rs_mouseWheelScrollPage,
#endif
#ifndef NO_BACKSPACE_KEY
    Rs_backspace_key,
#endif
#ifndef NO_DELETE_KEY
    Rs_delete_key,
#endif
    Rs_selectstyle,
#ifdef PRINTPIPE
    Rs_print_pipe,
#endif
#ifdef USE_XIM
    Rs_preeditType,
    Rs_inputMethod,
#endif
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
    Rs_bigfont_key,
    Rs_smallfont_key,
#endif
	Rs_opacity,			/* transluscent window opacity degree */
	Rs_opacityDegree,	/* opacity change degree */
#ifdef TRANSPARENT
    Rs_transparent,
    Rs_transparent_all,
# ifdef HAVE_SCROLLBARS
    Rs_transparent_scrollbar,
# endif
# ifdef HAVE_MENUBAR
    Rs_transparent_menubar,
# endif
    Rs_transparent_tabbar,
#endif
#ifndef NO_FRILLS
    Rs_ext_bwidth,
    Rs_int_bwidth,
#endif
    Rs_scrollBar_thickness,
#ifndef NO_LINESPACE
    Rs_lineSpace,
#endif
    Rs_cutchars,
#ifdef ACS_ASCII
    Rs_acs_chars,
#endif
    Rs_modifier,
	Rs_confFile,
	Rs_confFileSave,
    Rs_answerbackstring,
    Rs_tripleclickwords,
    Rs_cursorBlink,
    Rs_cursorBlinkInterval,
    Rs_pointerBlank,
    Rs_pointerBlankDelay,
	Rs_command,
	_Rs_command = Rs_command + MAX_PAGES - 1,
	Rs_smClientID,
	Rs_init_term_num,

	/*
	**********************************************************
	** hotkey resources 
	**********************************************************
	*/
	Rs_hotkey,
	_Rs_hotkey = Rs_hotkey + NUM_HKFUNCS - 1,

	/*
	**********************************************************
	** beginning of the secondary options
	**********************************************************
	*/
	Rs_options2,
	Rs2_protectSecondary,
	Rs2_tabShell,
	Rs2_cmdInitTabs,
	Rs2_cmdAllTabs,
#ifdef XFT_SUPPORT
# ifdef MULTICHAR_SET
	Rs2_xftNomFont,		/* do not use mfont */
	Rs2_xftSlowOutput,	/* slow mode output */
# endif
	Rs2_xftaa,			/* antialias */
	Rs2_xftht,			/* hinting */
	Rs2_xftah,			/* autohint */
	Rs2_xftga,			/* global advance */
	Rs2_xftwd,			/* width */
#endif
	Rs2_syncTabTitle,
	Rs2_syncTabIcon,
	Rs2_hideTabbar,
	Rs2_bottomTabbar,
	Rs2_hideButtons,
	Rs2_borderLess,
	Rs2_overrideRedirect,
	Rs2_holdExit,
	Rs2_broadcast,
	Rs2_veryBold,
	Rs2_disableHotkeys,
	Rs2_disableDefaultHotkeys,
#ifdef HAVE_X11_SM_SMLIB_H
	Rs2_enableSessionMgt,
#endif
	Rs2_linuxHomeEndKey,
    NUM_RESOURCES
} ;

enum {
    TIMEOUT_INCR = 0,
    NUM_TIMEOUTS
} ;


/*
** MUST sync with init.c:xa_names
*/
enum {
    XA_COMPOUND_TEXT = 0,
    XA_UTF8_STRING,
    XA_TEXT,
    XA_MULTIPLE,
    XA_TARGETS,
    XA_TIMESTAMP,
    XA_VT_SELECTION,
    XA_INCR,
    XA_WMDELETEWINDOW,
	XA_NET_WM_DESKTOP,
	XA_WIN_WORKSPACE,
	XA_NET_WM_NAME,
	XA_NET_WM_ICON_NAME,
	XA_WM_CLIENT_LEADER,
	XA_NET_WM_WINDOW_OPACITY,
#ifndef NO_FRILLS
	XA_NET_WM_PID,
#endif
#ifdef HAVE_X11_SM_SMLIB_H
	XA_SM_CLIENT_ID,
#endif
#ifdef USE_XIM
	XA_WM_LOCALE_NAME,
#endif
#ifdef TRANSPARENT
    XA_XROOTPMAPID,
#endif
#ifdef OFFIX_DND		/* OffiX Dnd (drag 'n' drop) support */
    XA_DNDPROTOCOL,
    XA_DNDSELECTION,
#endif				/* OFFIX_DND */
    XA_CLIPBOARD,
    NUM_XA
} ;


/*
 * number of graphics points
 * divisible by 2 (num lines)
 * divisible by 4 (num rect)
 */
#define	NGRX_PTS	1000

/* DEC private modes */
#define PrivMode_132			(1LU<<0)
#define PrivMode_132OK			(1LU<<1)
#define PrivMode_rVideo			(1LU<<2)
#define PrivMode_relOrigin		(1LU<<3)
#define PrivMode_Screen			(1LU<<4)
#define PrivMode_Autowrap		(1LU<<5)
#define PrivMode_aplCUR			(1LU<<6)
#define PrivMode_aplKP			(1LU<<7)
#define PrivMode_HaveBackSpace	(1LU<<8)
#define PrivMode_BackSpace		(1LU<<9)
#define PrivMode_ShiftKeys		(1LU<<10)
#define PrivMode_VisibleCursor	(1LU<<11)
#define PrivMode_MouseX10		(1LU<<12)
#define PrivMode_MouseX11		(1LU<<13)
#define PrivMode_scrollBar		(1LU<<14)
#define PrivMode_menuBar		(1LU<<15)
#define PrivMode_TtyOutputInh	(1LU<<16)
#define PrivMode_Keypress		(1LU<<17)
#define PrivMode_smoothScroll	(1LU<<18)
#define PrivMode_vt52			(1LU<<19)
/* too annoying to implement X11 highlight tracking */
/* #define PrivMode_MouseX11Track       (1LU<<18) */

#define PrivMode_mouse_report	(PrivMode_MouseX10|PrivMode_MouseX11)
#define PrivMode(test,bit,page)					\
    if (test)									\
		r->vts[page]->PrivateModes |= (bit);	\
    else										\
		r->vts[page]->PrivateModes &= ~(bit)

#ifdef ALLOW_132_MODE
# define PrivMode_Default						 \
(PrivMode_Autowrap|PrivMode_aplKP|PrivMode_ShiftKeys|PrivMode_VisibleCursor|PrivMode_132OK)
#else
# define PrivMode_Default						 \
(PrivMode_Autowrap|PrivMode_ShiftKeys|PrivMode_VisibleCursor)
/*
(PrivMode_Autowrap|PrivMode_aplKP|PrivMode_ShiftKeys|PrivMode_VisibleCursor)
*/
#endif


#define XSCREEN			DefaultScreen(r->Xdisplay)
#define XROOT			DefaultRootWindow(r->Xdisplay)

#ifdef PREFER_24BIT
# define XDEPTH			(r->Xdepth)
# define XCMAP			(r->Xcmap)
# define XVISUAL		(r->h->Xvisual)
#else
# ifdef DEBUG_DEPTH
#  define XDEPTH		DEBUG_DEPTH
# else
#  define XDEPTH		DefaultDepth(r->Xdisplay, XSCREEN)
#  define XCMAP			DefaultColormap(r->Xdisplay, XSCREEN)
#  define XVISUAL		DefaultVisual(r->Xdisplay, XSCREEN)
# endif
#endif


#define IMBUFSIZ		128	/* input modifier buffer sizes */
#ifndef BUFSIZ
# define BUFSIZ			4096
#endif
#define KBUFSZ			512	/* size of keyboard mapping buffer */

/*
 *****************************************************************************
 * MACRO DEFINES
 *****************************************************************************
 */

/* convert pixel dimensions to row/column values.  Everything as RINT32T */
#define Pixel2Width(x)		((RINT32T)(x) / (RINT32T)r->TermWin.fwidth)
#define Pixel2Height(y)		((RINT32T)(y) / (RINT32T)r->TermWin.fheight)
#define Width2Pixel(n)		((RINT32T)(n) * (RINT32T)r->TermWin.fwidth)
#define Height2Pixel(n)		((RINT32T)(n) * (RINT32T)r->TermWin.fheight)
#define Pixel2Col(x)		Pixel2Width((RINT32T)(x) - (RINT32T)r->TermWin.int_bwidth)
#define Pixel2Row(y)		Pixel2Height((RINT32T)(y) - (RINT32T)r->TermWin.int_bwidth)
#define Col2Pixel(col)		((RINT32T)Width2Pixel(col) + (RINT32T)r->TermWin.int_bwidth)
#define Row2Pixel(row)		((RINT32T)Height2Pixel(row) + (RINT32T)r->TermWin.int_bwidth)

/*
#define TermWin_TotalWidth()	((RINT32T)r->TermWin.width  + 2 * (RINT32T)r->TermWin.int_bwidth)
#define TermWin_TotalHeight()	((RINT32T)r->TermWin.height + 2 * (RINT32T)r->TermWin.int_bwidth)
*/

/* how to build & extract colors and attributes */
#define GET_BASEFG(x)		(((x) & RS_fgMask))
#define GET_BASEBG(x)		(((x) & RS_bgMask)>>Color_Bits)
#ifndef NO_BRIGHTCOLOR
# define GET_FGCOLOR(x)						\
    ((((x) & RS_Bold) == 0					\
      || GET_BASEFG(x) < minCOLOR				\
      || GET_BASEFG(x) >= minBrightCOLOR)			\
     ? GET_BASEFG(x)						\
     : (GET_BASEFG(x) + (minBrightCOLOR - minCOLOR)))
# define GET_BGCOLOR(x)						\
    ((((x) & RS_Blink) == 0					\
      || GET_BASEBG(x) < minCOLOR				\
      || GET_BASEBG(x) >= minBrightCOLOR)			\
     ? GET_BASEBG(x)						\
     : (GET_BASEBG(x) + (minBrightCOLOR - minCOLOR)))
#else
# define GET_FGCOLOR(x)		GET_BASEFG(x)
# define GET_BGCOLOR(x)		GET_BASEBG(x)
#endif

#define GET_ATTR(x)		(((x) & RS_attrMask))
#define GET_BGATTR(x)							\
    (((x) & RS_RVid) ? (((x) & (RS_attrMask & ~RS_RVid))		\
			| (((x) & RS_fgMask)<<Color_Bits))		\
		     : ((x) & (RS_attrMask | RS_bgMask)))
#define SET_FGCOLOR(x,fg)	(((x) & ~RS_fgMask)  | (fg))
#define SET_BGCOLOR(x,bg)	(((x) & ~RS_bgMask)  | ((bg)<<Color_Bits))
#define SET_ATTR(x,a)		(((x) & ~RS_attrMask)| (a))

#define SET_PIXCOLOR(h, x)		((h)->pixcolor_set[(x) / NPIXCLR_BITS] |= (1 << ((x) % NPIXCLR_BITS)))
#define ISSET_PIXCOLOR(h, x)	((h)->pixcolor_set[(x) / NPIXCLR_BITS] & (1 << ((x) % NPIXCLR_BITS)))

#define scrollbar_isMotion()	(r->scrollBar.state == 'm')
#define scrollbar_isUp()		(r->scrollBar.state == 'U')
#define scrollbar_isDn()		(r->scrollBar.state == 'D')
#define scrollbar_isUpDn()		isupper ((int) r->scrollBar.state)

#define scrollbar_setIdle()		r->scrollBar.state = (char) 1
#define scrollbar_setMotion()	r->scrollBar.state = 'm'
#define scrollbar_setUp()		r->scrollBar.state = 'U'
#define scrollbar_setDn()		r->scrollBar.state = 'D'


#define scrollbarnext_dnval()		(r->scrollBar.end + (r->scrollBar.width + 1))
#define scrollbarnext_upButton(y)	((y) > r->scrollBar.end \
					 && (y) <= scrollbarnext_dnval())
#define scrollbarnext_dnButton(y)	((y) > scrollbarnext_dnval())

#define scrollbarrxvt_upButton(y)	((y) < r->scrollBar.beg)
#define scrollbarrxvt_dnButton(y)	((y) > r->scrollBar.end)


#define scrollbarsgi_dnval()		(r->scrollBar.end + 1)
#define scrollbarsgi_upButton(y)	((y) < r->scrollBar.beg)
#define scrollbarsgi_dnButton(y)	((y) > r->scrollBar.end)

#ifdef NEXT_SCROLLBAR
# define SCROLLNEXT_MINHEIGHT		(NEXT_SB_MIN_HEIGHT)
#else
# define SCROLLNEXT_MINHEIGHT		(0)
#endif	/* NEXT_SCROLLBAR */
#ifdef SGI_SCROLLBAR
# define SCROLLSGI_MINHEIGHT		(10)
#else
# define SCROLLSGI_MINHEIGHT		(0)
#endif	/* SGI_SCROLLBAR */
#define SCROLL_MINHEIGHT			(10)
#define scrollbar_minheight()	(	\
	(r->scrollBar.style == R_SB_NEXT) ? SCROLLNEXT_MINHEIGHT :	\
	(r->scrollBar.style == R_SB_SGI) ? SCROLLSGI_MINHEIGHT :	\
		SCROLL_MINHEIGHT)
#define scrollbar_above_slider(y)	((y) < r->scrollBar.top)
#define scrollbar_below_slider(y)	((y) > r->scrollBar.bot)
#define scrollbar_position(y)		((y) - r->scrollBar.beg)
#define scrollbar_size()		(r->scrollBar.end - r->scrollBar.beg \
					 - scrollbar_minheight())



#ifdef BACKGROUND_IMAGE
# define XPMClearArea(a, b, c, d, e, f, g)	XClearArea((a), (b), (c), (d), (e), (f), (g))
#else
# define XPMClearArea(a, b, c, d, e, f, g)
#endif

#ifndef STRICT_FONT_CHECKING
# define rxvt_get_fontwidest(font)	((font)->max_bounds.width)
#endif

#define rxvt_Gr_ButtonPress(x,y)	rxvt_Gr_ButtonReport (r, 'P',(x),(y))
#define rxvt_Gr_ButtonRelease(x,y)	rxvt_Gr_ButtonReport (r, 'R',(x),(y))


/*
 *****************************************************************************
 * VARIABLES
 *****************************************************************************
 */
struct rxvt_hidden {
#ifdef __GNUC__
	unsigned char	want_refresh:1,
					want_full_refresh:1,
					am_transparent:1,
					am_pixmap_trans:1, 
					hate_those_clicks:1,
					num_scr_allow:1,
					bypass_keystate:1;
	unsigned char	hidden_cursor:1,
					hidden_pointer:1;
#else
	unsigned char	want_refresh,
# ifdef TRANSPARENT
					/* awaiting full screen refresh */
					want_full_refresh,
# endif
# if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
					/* is a transparent term */
					am_transparent,
					/* transparency w/known root pixmap */
					am_pixmap_trans,
# endif
					/* a.k.a. keep mark position */
					hate_those_clicks,
					num_scr_allow,
					bypass_keystate;
# ifdef CURSOR_BLINK
	unsigned char	hidden_cursor;
# endif
# ifdef POINTER_BLANK
	unsigned char	hidden_pointer;
# endif
#endif	/* !__GNUC__ */


    unsigned char   refresh_type,
#ifdef META8_OPTION
					meta_char,	/* Alt-key prefix */
#endif
					scrollbar_align,
					selection_wait,
					selection_type;

#ifdef GREEK_SUPPORT
					/* greek keyboard mode */
	short			greek_mode;
    KeySym			ks_greekmodeswith;
#endif

	/* screen: previous number of columns and rows */
    RUINT16T		prev_ncol,
					prev_nrow;
	RUINT32T		pixcolor_set[NPIXCLR_SETS];

#ifdef SELECTION_SCROLLING
	int				scroll_selection_delay,
					scroll_selection_lines;
	enum page_dirn	scroll_selection_dir;
	int				selection_save_x,
					selection_save_y,
					selection_save_state,
					pending_scroll_selection;
#endif

					/* Hops - csr offset in thumb/slider to */
					/*   give proper Scroll behaviour */
    int				csrO,
#ifndef NO_SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
					scroll_arrow_delay,
#endif
#if defined(MOUSE_WHEEL) && defined(MOUSE_SLIP_WHEELING)
					mouse_slip_wheel_delay,
					mouse_slip_wheel_speed,
#endif
					refresh_count,
					refresh_limit,
					fnum,	/* logical font number */
					last_bot,	/* scrollbar last bottom position */
					last_top,	/* scrollbar last top position */
					last_state,	/* scrollbar last state */
					scroller_len,
					currmaxcol,
#ifdef HAVE_MENUBAR
					menu_readonly,	/* okay to alter menu? */
					Arrows_x,
#endif
#if (MENUBAR_MAX > 1)
					Nbars,
#endif
					window_vt_x,
					window_vt_y,
# ifdef POINTER_BLANK
					pointerBlankDelay,
# endif
					allowedxerror;

    unsigned int    ModMetaMask,
					ModNumLockMask;
#ifndef NO_BRIGHTCOLOR
	unsigned long	colorfgbg;
#endif
	unsigned long	global_fg;
	unsigned long	global_bg;
#ifdef XFT_SUPPORT
	XftColor		global_xftfg;
	XftColor		global_xftbg;
#endif

	gid_t			ttygid;

#ifdef PREFER_24BIT
    Visual*			Xvisual;
#endif

    Atom			xa[NUM_XA];

    Time            selection_time,
                    selection_request_time;

    Cursor          bar_pointer;
#ifdef POINTER_BLANK
    Cursor          blank_pointer;
#endif

#ifndef NO_BACKSPACE_KEY
    const char*		key_backspace;
#endif
#ifndef NO_DELETE_KEY
    const char*		key_delete;
#endif
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
    KeySym			ks_bigfont, ks_smallfont;
#endif
#ifdef USE_XIM
    XIC				Input_Context;
    XIMStyle		input_style;
    int				event_type;
#endif
    struct mouse_event	MEvent;
    XComposeStatus	compose;
    row_col_t		oldcursor;
#ifdef MULTICHAR_SET
    int				oldcursormulti;
#endif
    void			(*multichar_decode)(unsigned char *str, int len);
#ifdef HAVE_MENUBAR
    menu_t*			ActiveMenu;		/* currently active menu */
	menu_t*			BuildMenu;		/* the menu currently being built */
    bar_t*			CurrentBar;
# if !(MENUBAR_MAX > 1)
    bar_t			BarList;
# endif				/* (MENUBAR_MAX > 1) */
#endif
#ifdef CURSOR_BLINK
	long			blinkInterval;
    struct timeval	lastcursorchange;
#endif
#ifdef POINTER_BLANK
    struct timeval	lastmotion;
#endif
    struct timeval	timeout[NUM_TIMEOUTS];

	/*
	** these three don't need to be kept but do so to placate some
	** mem checkers
	*/
	char*			env_windowid; /* environmental variable WINDOWID */
	char*			env_display;  /* environmental variable DISPLAY */
	char*			env_term;     /* environmental variable TERM */
	char*			env_tabtitle; /* environmental variable MRXVT_TABTITLE*/
	char*			env_colorfgbg;
	char*			buffer;
	char*			locale;
	/*
	unsigned char*	v_buffer;
	unsigned char*	v_bufstr;
	unsigned char*	v_bufptr;
	unsigned char*	v_bufend;
	*/
	char*			newfont[MAX_NFONTS];
#ifdef KEYSYM_RESOURCE
    unsigned char*	Keysym_map[256];
#endif
    const char*		rs[NUM_RESOURCES];
	/* command input buffering */
	/*
    unsigned char*	cmdbuf_ptr, *cmdbuf_endp;
    unsigned char	cmdbuf_base[BUFSIZ];
	*/
    unsigned char	kbuf[KBUFSZ];
};

#ifndef __attribute__
# ifdef __GNUC__
#  if (__GNUC__ == 2 && __GNUC_MINOR__ < 5) || (__GNUC__ < 2)
#   define __attribute__(x)
#  endif
# endif
# define __attribute__(x)
#endif

/*
 *****************************************************************************
 * PROTOTYPES
 *****************************************************************************
 */
#define __PROTO(p)	p
#include "protos.h"

#ifdef OUR_STRINGS
# define MEMSET(x, y, z)		ma_memset((x), (y), (size_t)(z))
# define MEMCPY(x, y, z)		ma_memcpy((void *)(x), (const void *)(y), (z))
# define MEMMOVE(x, y, z)		ma_memmove((void *)(x), (const void *)(y), (z))
# define STRCASECMP(x, y)		ma_strcasecmp((x), (y))
# define STRNCASECMP(x, y, z)	ma_strncasecmp((x), (y), (z))
# define STRCPY(x, y)			ma_strcpy((char *)(x), (const char *)(y))
# define STRNCPY(x, y, z)		ma_strncpy((char *)(x), (const char *)(y), (z))
# define STRCMP(x, y)			ma_strcmp((const char *)(x), (const char *)(y))
# define STRNCMP(x, y, z)		ma_strncmp((const char *)(x), (const char *)(y), (z))
# define STRCAT(x, y)			ma_strcat((char *)(x), (const char *)(y))
# define STRNCAT(x, y, z)		ma_strncat((char *)(x), (const char *)(y), (z))
# define STRDUP(x)				ma_strdup((const char *)(x))
# define STRNDUP(x, z)			ma_strndup((const char TAINTED *)(x), (size_t) (z))
# define STRLEN(x)				ma_strlen((const char *)(x))
# define STRCHR(x, y)			ma_strchr((const char *)(x), (int)(y))
# define STRRCHR(x, y)			ma_strrchr((const char *)(x), (int)(y))

#else /* OUR_STRINGS */

# define MEMSET(x, y, z)		memset((x), (y), (size_t)(z))
# define MEMCPY(x, y, z)		memcpy((void *)(x), (const void *)(y), (z))
# define MEMMOVE(x, y, z)		memmove((void *)(x), (const void *)(y), (z))
# define STRCASECMP(x, y)		strcasecmp((x), (y))
# define STRNCASECMP(x, y, z)	strncasecmp((x), (y), (z))
# define STRCPY(x, y)			strcpy((char *)(x), (const char *)(y))
# define STRNCPY(x, y, z)		strncpy((char *)(x), (const char *)(y), (z))
# define STRCMP(x, y)			strcmp((const char *)(x), (const char *)(y))
# define STRNCMP(x, y, z)		strncmp((const char *)(x), (const char *)(y), (z))
# define STRCAT(x, y)			strcat((char *)(x), (const char *)(y))
# define STRLEN(x)				strlen((const char *)(x))
# define STRNCAT(x, y, z)		strncat((char *)(x), (const char *)(y), (z))

# ifdef HAVE_STRDUP
#  define STRDUP(x)				strdup((const char *)(x))
# else
#  define STRDUP(x)				ma_strdup((const char *)(x))
# endif

# ifdef HAVE_STRNDUP
#  define STRNDUP(x, z)			strndup((const char TAINTED *)(x), (size_t) (z))
# else
#  define STRNDUP(x, z)			ma_strndup((const char TAINTED *)(x), (size_t) (z))
# endif

# ifdef HAVE_STRCHR
#  define STRCHR(x, y)			strchr((const char *)(x), (int)(y))
# else
#  define STRCHR(x, y)			ma_strchr((const char *)(x), (int)(y))
# endif

# ifdef HAVE_STRRCHR
#  define STRRCHR(x, y)			strrchr((const char *)(x), (int)(y))
# else
#  define STRRCHR(x, y)			ma_strrchr((const char *)(x), (int)(y))
# endif
#endif	/* OUR_STRINGS */

# define STRSTR(x, y)			strstr((const char *)(x), (const char*)(y))


#define TWIN_WIDTH(R)	((R)->szHint.width)
#define TWIN_HEIGHT(R)	((R)->szHint.height)
#define VT_WIDTH(R)		((R)->szHint.width - \
		(R)->szHint.base_width + 2*(R)->TermWin.int_bwidth)
#define VT_HEIGHT(R)	((R)->szHint.height - \
		(R)->szHint.base_height + 2*(R)->TermWin.int_bwidth)
/*
#define TWIN_WIDTH(R)	\
	((R)->szHint.width - 2*(R)->TermWin.int_bwidth)
#define TWIN_HEIGHT(R)	\
	((R)->szHint.height - 2*(R)->TermWin.int_bwidth)
#define VT_WIDTH(R)		\
	((R)->szHint.width - (R)->szHint.base_width)
#define VT_HEIGHT(R)	\
	((R)->szHint.height - (R)->szHint.base_height)
*/


#ifndef XTERM_COLOR_CHANGE
# define rxvt_set_window_color(r, idx, color)	()
#endif

#ifdef DEBUG_malloc
# include "dmalloc.h"		/* This comes last */
#endif

#ifdef DEBUG
/* # define DEBUG_VERBOSE */
#endif

#endif				/* __RXVT_H__ */
/*----------------------- end-of-file (H source) -----------------------*/
