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
#ifndef _COMMAND_H_
# define _COMMAND_H_
# include <signal.h>
# include <limits.h>
# include <X11/X.h>
# include <X11/Xfuncproto.h>
# include <X11/Xproto.h>
# include <X11/keysym.h>

# ifdef HAVE_X11_LOCALE_H
#  include <X11/Xlocale.h>
# endif
# include <locale.h>

# include "options.h"
# include "system.h"  /* For RETSIGTYPE */

/************ Macros and Definitions ************/
#if !defined(SIGSYS)
# if defined(SIGUNUSED)
#  define SIGSYS SIGUNUSED
# else
#  define SIGSYS ((int) 0)
# endif
#endif

#ifdef OFFIX_DND
# define DndFile	2
# define DndDir		5
# define DndLink	7
#endif

# define scrollbar_esc	30

/* Motif window hints */
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

/* DEC private modes */
# define PrivMode_132		(1LU<<0)
# define PrivMode_132OK		(1LU<<1)
# define PrivMode_rVideo	(1LU<<2)
# define PrivMode_relOrigin	(1LU<<3)
# define PrivMode_Screen	(1LU<<4)
# define PrivMode_Autowrap	(1LU<<5)
# define PrivMode_aplCUR	(1LU<<6)
# define PrivMode_aplKP		(1LU<<7)
# define PrivMode_BackSpace	(1LU<<8)
# define PrivMode_ShiftKeys	(1LU<<9)
# define PrivMode_VisibleCursor	(1LU<<10)
# define PrivMode_MouseX10	(1LU<<11)
# define PrivMode_MouseX11	(1LU<<12)
/* too annoying to implement X11 highlight tracking */
/* #define PrivMode_MouseX11Track	(1LU<<13) */
# define PrivMode_scrollbar	(1LU<<14)
# define PrivMode_menuBar	(1LU<<15)

#define PrivMode_mouse_report	(PrivMode_MouseX10|PrivMode_MouseX11)
#define PrivMode(test,bit) do {\
if (test) PrivateModes |= (bit); else PrivateModes &= ~(bit);} while (0)

#define PrivMode_Default (PrivMode_Autowrap|PrivMode_ShiftKeys|PrivMode_VisibleCursor)

#ifdef HAVE_TERMIOS_H
# ifdef TCSANOW			/* POSIX */
#  define GET_TERMIOS(fd,tios)	tcgetattr(fd, tios)
#  define SET_TERMIOS(fd,tios)	do {cfsetospeed(tios, BAUDRATE); cfsetispeed(tios, BAUDRATE); tcsetattr(fd, TCSANOW, tios);} while (0)
# else
#  ifdef TIOCSETA
#   define GET_TERMIOS(fd,tios)	ioctl(fd, TIOCGETA, tios)
#   define SET_TERMIOS(fd,tios)	do {tios->c_cflag |= BAUDRATE; ioctl(fd, TIOCSETA, tios);} while (0)
#  else
#   define GET_TERMIOS(fd,tios)	ioctl(fd, TCGETS, tios)
#   define SET_TERMIOS(fd,tios)	do {tios->c_cflag |= BAUDRATE; ioctl(fd, TCSETS, tios);} while (0)
#  endif
# endif
# define SET_TTYMODE(fd,tios)	SET_TERMIOS(fd, tios)
#else
# define SET_TTYMODE(fd,tt) do {tt->sg.sg_ispeed = tt->sg.sg_ospeed = BAUDRATE; ioctl(fd, TIOCSETP, &(tt->sg)); ioctl(fd, TIOCSETC, &(tt->tc)); \
                                ioctl(fd, TIOCSLTC, &(tt->lc)); ioctl(fd, TIOCSETD, &(tt->line)); ioctl(fd, TIOCLSET, &(tt->local)); \
                               } while (0)
#endif /* HAVE_TERMIOS_H */

#ifdef B38400
# define BAUDRATE	B38400
#else
# ifdef B19200
#  define BAUDRATE	B19200
# else
#  define BAUDRATE	B9600
# endif
#endif

/* Disable special character functions */
#ifdef _POSIX_VDISABLE
# define VDISABLE	_POSIX_VDISABLE
#else
# define VDISABLE	255
#endif

/* system default characters if defined and reasonable */
#ifndef CINTR
# define CINTR		'\003'	/* ^C */
#endif
#ifndef CQUIT
# define CQUIT		'\034'	/* ^\ */
#endif
#ifndef CERASE
# ifdef linux
#  define CERASE	'\177'	/* ^? */
# else
#  define CERASE	'\010'	/* ^H */
# endif
#endif
#ifndef CKILL
# define CKILL		'\025'	/* ^U */
#endif
#ifndef CEOF
# define CEOF		'\004'	/* ^D */
#endif
#ifndef CSTART
# define CSTART		'\021'	/* ^Q */
#endif
#ifndef CSTOP
# define CSTOP		'\023'	/* ^S */
#endif
#ifndef CSUSP
# define CSUSP		'\032'	/* ^Z */
#endif
#ifndef CDSUSP
# define CDSUSP		'\031'	/* ^Y */
#endif
#ifndef CRPRNT
# define CRPRNT		'\022'	/* ^R */
#endif
#ifndef CFLUSH
# define CFLUSH		'\017'	/* ^O */
#endif
#ifndef CWERASE
# define CWERASE	'\027'	/* ^W */
#endif
#ifndef CLNEXT
# define CLNEXT		'\026'	/* ^V */
#endif
#ifndef CSTATUS
# define CSTATUS	'\024'	/* ^T */
#endif
#ifndef VDISCRD
# ifdef VDISCARD
#  define VDISCRD	VDISCARD
# endif
#endif
#ifndef VWERSE
# ifdef VWERASE
#  define VWERSE	VWERASE
# endif
#endif

#define KBUFSZ		64	/* size of keyboard mapping buffer */
#define STRING_MAX	512	/* max string size for process_xterm_seq() */
#define ESC_ARGS	32	/* max # of args for esc sequences */

/* a large REFRESH_PERIOD causes problems with `cat' */
#ifndef REFRESH_PERIOD
# define REFRESH_PERIOD	3
#endif

#ifndef MULTICLICK_TIME
# define MULTICLICK_TIME		500
#endif
#ifndef SCROLLBAR_INITIAL_DELAY
# define SCROLLBAR_INITIAL_DELAY	40
#endif
#ifndef SCROLLBAR_CONTINUOUS_DELAY
# define SCROLLBAR_CONTINUOUS_DELAY	2
#endif

/* time factor to slow down a `jumpy' mouse */
#define MOUSE_THRESHOLD		50
#define CONSOLE		"/dev/console"	/* console device */

/* key-strings: if only these keys were standardized <sigh> */
#ifndef KS_HOME
# define KS_HOME "\033[7~"	/* Home */
#endif
#ifndef KS_END
# define KS_END  "\033[8~"	/* End */
#endif
#ifdef NO_DELETE_KEY
# undef KS_DELETE		/* use X server definition */
#else
# ifndef KS_DELETE
#  define KS_DELETE	"\033[3~"	/* Delete = Execute */
# endif
#endif

#define VT100_ANS	"\033[?1;2C"	/* vt100 answerback */
#define LINUX_ANS	"\033[?6;5C"	/* linux answerback */
#ifndef ESCZ_ANSWER
# define ESCZ_ANSWER	VT100_ANS	/* obsolete ANSI ESC[c */
#endif

#if defined(linux) && defined(N_TTY_BUF_SIZE)
# define CMD_BUF_SIZE N_TTY_BUF_SIZE
#else
# ifndef CMD_BUF_SIZE
#  define CMD_BUF_SIZE 4096
# endif
#endif

#if !defined(EACCESS) && defined(EAGAIN)
# define EACCESS EAGAIN
#endif

#define PTYCHAR1 "pqrstuvwxyz"
#define PTYCHAR2 "0123456789abcdefghijklmnopqrstuvwxyz"

#if RETSIGTYPE != void
# define SIG_RETURN(x) return ((RETSIGTYPE) x)
#else
# define SIG_RETURN(x) return
#endif

#define CHARS_READ()      (cmdbuf_ptr < cmdbuf_endp)
#define CHARS_BUFFERED()  (count != CMD_BUF_SIZE)
#define RETURN_CHAR()     do { \
                            unsigned char c = *cmdbuf_ptr++; \
                            refreshed = 0; \
                            if (c < 32) D_VT(("RETURN_CHAR():  \'%s\' (%d 0x%02x %03o)\n", get_ctrl_char_name(c), c, c, c)); \
                            else D_VT(("RETURN_CHAR():  \'%c\' (%d 0x%02x %03o)\n", c, c, c, c)); \
                            return (c); \
                          } while (0)

#ifdef REFRESH_DELAY
# define REFRESH_DELAY_USEC (1000000/25)
#endif

#if defined(linux)
# ifdef PTY_BUF_SIZE		/* From <linux/tty.h> */
#  define MAX_PTY_WRITE PTY_BUF_SIZE
# endif
#endif

/* NOTE: _POSIX_MAX_INPUT is defined _through_ <limits.h> at least for
 * the following systems: HP-UX 10.20, AIX (no idea about the version),
 * OSF1/alpha 4.0, Linux (probably any Linux system).
 */
#ifndef MAX_PTY_WRITE
# ifdef _POSIX_VERSION
#  ifdef _POSIX_MAX_INPUT
#   define MAX_PTY_WRITE _POSIX_MAX_INPUT
#  else
#   define MAX_PTY_WRITE 255	/* POSIX minimum MAX_INPUT */
#  endif
# endif
#endif

#ifndef MAX_PTY_WRITE
# define MAX_PTY_WRITE 128	/* 1/2 POSIX minimum MAX_INPUT */
#endif

/************ Structures ************/
/* Motif window hints */
# ifdef LONG64
typedef struct _mwmhints {
  CARD64 flags;
  CARD64 functions;
  CARD64 decorations;
  INT64  input_mode;
  CARD64 status;
} MWMHints;
# else
typedef struct _mwmhints {
  CARD32 flags;
  CARD32 functions;
  CARD32 decorations;
  INT32  input_mode;
  CARD32 status;
} MWMHints;
# endif

# ifdef HAVE_TERMIOS_H
typedef struct termios ttymode_t;
# else
typedef struct {  /* sgtty interface */
  struct sgttyb sg;
  struct tchars tc;
  struct ltchars lc;
  int line;
  int local;
} ttymode_t;
# endif

/************ Variables ************/
extern int my_ruid, my_rgid, my_euid, my_egid;
extern int pipe_fd;
extern char initial_dir[PATH_MAX+1];
extern unsigned long PrivateModes;
extern int refresh_count, refresh_limit, refresh_type;
extern pid_t cmd_pid;
#ifdef USE_XIM
extern XIC xim_input_context;	/* input context */
#endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

#ifdef HAVE_U_STACK_TRACE
extern void U_STACK_TRACE(void);
#endif
extern char *ptsname();
extern void privileges(int);
extern char *sig_to_str(int);
extern const char *event_type_to_name(int);
extern const char *request_code_to_name(int);
extern const char *get_ctrl_char_name(char);
extern void dump_stack_trace(void);
extern void install_handlers(void);
extern void clean_exit(void);
extern int get_pty(void);
extern int get_tty(void);
extern XFontSet create_fontset(const char *, const char *);
#if defined(USE_XIM) || defined(MULTI_CHARSET)
extern void xim_set_status_position(void);
extern void xim_get_position(XPoint *pos);
extern void xim_set_fontset(void);
extern void init_locale(void);
#else
# define init_locale() ((void)0)
#endif
extern int escreen_init(char **);
extern int run_command(char **);
extern void init_command(char **);
extern void tt_winsize(int);
extern void tt_resize(void);
extern unsigned int cmd_write(const unsigned char *, unsigned int);
#ifdef BACKGROUND_CYCLING_SUPPORT
extern RETSIGTYPE check_pixmap_change(int);
#endif
extern unsigned char cmd_getc(void);
extern void cmd_ungetc(void);
extern void tt_write(const unsigned char *, unsigned int);
extern void tt_printf(const unsigned char *, ...);
extern void main_loop(void);
extern int v_doPending(void);
extern void v_writeBig(int, char *, int);

_XFUNCPROTOEND

#endif	/* _COMMAND_H_ */
