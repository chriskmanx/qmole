/*--------------------------------*-C-*---------------------------------*
 * File:	main.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1992        John Bovey <jdb@ukc.ac.uk>
 * Copyright (c) 1994        Robert Nation <nation@rocket.sanders.lockheed.com>
 * Copyright (c) 1995        Garrett D'Amore <garrett@netcom.com>
 * Copyright (c) 1997        mj olesen <olesen@me.QueensU.CA>
 * Copyright (c) 1997,1998   Oezguer Kesim <kesim@math.fu-berlin.de>
 * Copyright (c) 1998-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2000        Xianping Ge <xge@ics.uci.edu>
 * Copyright (c) 2003-2004   Marc Lehmann <pcg@goof.com>
 * Copyright (c) 2005        Burgers A.R. <burgers@ecn.nl>
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
** $Id: main.c,v 1.145 2005/08/31 05:30:41 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"



#ifdef DEBUG_VERBOSE
# define DEBUG_LEVEL 1
# define DEBUG_X
#else 
# define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
# define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
# define DBG_MSG(d,x)
#endif



/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
void rxvt_clean_commands       (rxvt_t* r, int command_number);
void rxvt_free_hidden          (rxvt_t*);
void rxvt_font_up_down         (rxvt_t*, int, int);
int  rxvt_get_font_widest      (XFontStruct*);
void rxvt_set_colorfgbg        (rxvt_t*);
void rxvt_resize_sub_windows   (rxvt_t*);
void rxvt_recalc_szhint        (rxvt_t*, resize_reason_t reason, unsigned int* p_w, unsigned int* p_h);
#ifdef USE_XIM
void rxvt_IM_set_size          (rxvt_t*, XRectangle*);
void rxvt_IM_set_color         (rxvt_t*, unsigned long*, unsigned long*);
Bool rxvt_IM_is_running        (rxvt_t*);
void rxvt_IM_set_preedit_area  (rxvt_t*, XRectangle*, XRectangle*, XRectangle*);
void rxvt_IM_destroy_callback  (XIM, XPointer, XPointer);
Bool rxvt_IM_get_IC            (rxvt_t*);
#endif
void rxvt_set_r                (rxvt_t*);
#ifdef XFT_SUPPORT
void rxvt_init_font_fixed      (rxvt_t*);
# ifndef NO_BOLDFONT
void rxvt_init_bfont_xft       (rxvt_t*, XftPattern*);
# endif
# ifdef MULTICHAR_SET
int  rxvt_init_mfont_xft       (rxvt_t*, XftPattern*, const char*);
# endif
#endif	/* XFT_SUPPORT */
/*--------------------------------------------------------------------*
 *         END   `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/



/*----------------------------------------------------------------------*/
const char**	cmd_argv;
/*----------------------------------------------------------------------*/

/* INTPROTO */
void
rxvt_free_commands (rxvt_t* r, int command_number)
{
	register int	i;

	/* reset all tab commands to NULL */
	for (i = 0; i < MAX_PAGES; i ++)	{
		/* rs[ANYTHING] is static string, shouldn't use free to
		** release. So we simply reset them to NULL */
		r->h->rs[Rs_command+i] = NULL;
	}

	/* free all command argv */
	for (i = 0; i < command_number; i ++)	{
		if (PVTS(r, i)->command_argv &&
			PVTS(r, i)->command_argc)	{
			register int	j;

	        for (j = 0; j < PVTS(r, i)->command_argc; j ++)
	            free (PVTS(r, i)->command_argv[j]);
	        free (PVTS(r, i)->command_argv);
	        PVTS(r, i)->command_argv = NULL;
	        PVTS(r, i)->command_argc = 0;
		}
	}
}



/* rxvt_init() */
/* LIBPROTO */
rxvt_t			*
rxvt_init(int argc, const char *const *argv)
{
	register int	i;
	register int	itnum; /* initial terminal number */
	rxvt_t*			r;

	r = (rxvt_t *)rxvt_calloc(1, sizeof(rxvt_t));
	rxvt_set_r(r);		/* only assignment to _rxvt_vars */
	if (rxvt_init_vars(r) < 0) {
		free(r);
		return NULL;
	}
	/* save global argc and argv */
	r->global_argc = argc;
	r->global_argv = (char**) argv;

	rxvt_init_secondary(r);
	rxvt_init_hotkeys (r);
	cmd_argv = rxvt_init_resources(r, argc, argv);

	rxvt_create_show_windows(r, argc, argv);

#ifdef TRANSPARENT
	if (r->Options & Opt_transparent) {
		XSelectInput(r->Xdisplay, XROOT, PropertyChangeMask);
		rxvt_check_our_parents(r);
	}
#endif

	rxvt_init_env(r);
	rxvt_init_command(r, cmd_argv);
	rxvt_init_screen (r);

	/*
	** Initialize the pages
	*/
	if (!r->h->rs[Rs_init_term_num])
		itnum = 1;
	else	{
		itnum = atoi (r->h->rs[Rs_init_term_num]);
		itnum = max (1, itnum);
		itnum = min (itnum, MAX_PAGES);
	}
	for (i = 0; i < itnum; i ++)
		rxvt_append_page (r, NULL);
	rxvt_activate_page (r, 0);

	/*
	** If commands are loaded only on init, free command resources
	** now. The future commands will only get NULL string as their
	** commands, and will fall back to default shell. This simplify
	** the logic to choose commands for a new terminal.
	*/
	if (r->Options2 & Opt2_cmdInitTabs)
		rxvt_free_commands (r, itnum);

	/* Initialize xlocale after VT is created */
	rxvt_init_xlocale(r);

	return r;
}

/* ------------------------------------------------------------------------- *
 *							SIGNAL HANDLING & EXIT HANDLER						*
 * ------------------------------------------------------------------------- */
/*
 * Catch a SIGCHLD signal and exit if the direct child has died
 */
/* ARGSUSED */
/* EXTPROTO */
RETSIGTYPE
rxvt_Child_signal(int sig __attribute__((unused)))
{
	int					pid, save_errno;
	rxvt_t*				r;

	/* enable signal reentry, it's ok here */
	signal(SIGCHLD, rxvt_Child_signal);


	save_errno = errno;
	do {
		errno = 0;
	} while ((pid = waitpid(-1, NULL, WNOHANG)) == -1 &&
			errno == EINTR);

	DBG_MSG(1,(stderr, "signal: child %d died\n", (int) pid));
	r = rxvt_get_r();
	if (pid != -1)	{
		register int	i;

		for (i = 0; i <= LTAB(r); i ++)
			if (pid == PVTS(r, i)->cmd_pid)
				break;

		if (i <= LTAB(r))	{
			DBG_MSG(1,(stderr, "Dead child %d is tab %d\n", (int) pid, i));
			/* one more vt died */
			r->vt_died ++;
			/* update child members */
			PVTS(r, i)->dead = 1;
			if (r->Options2 & Opt2_holdExit)
				PVTS(r, i)->hold = 1;
		}
		else	{
			errno = save_errno;
		}
	}
}


/*
 * Catch a fatal signal and tidy up before quitting
 */
/* EXTPROTO */
RETSIGTYPE
rxvt_Exit_signal(int sig)
{
#ifdef UTMP_SUPPORT
	register int	i;
#endif
	rxvt_t*			r;

	DBG_MSG(1,(stderr, "Receive signal %d\n", (int) sig));
	signal(sig, SIG_DFL);

	r = rxvt_get_r();

#ifdef UTMP_SUPPORT
	for (i = 0; i <= LTAB(r); i ++)	{
		rxvt_privileges (RESTORE);
		rxvt_cleanutent (r, i);
		rxvt_privileges (IGNORE);
	}
#endif

	/* resend signal to default handler */
	/* kill (getpid (), sig); */
	rxvt_clean_exit (r);
}


/* INTPROTO */
void
rxvt_free_hidden (rxvt_t* r)
{
#ifdef DEBUG
	if (None != r->h->bar_pointer)	{
		XFreeCursor (r->Xdisplay, r->h->bar_pointer);
		r->h->bar_pointer = None;
	}
# ifdef POINTER_BLANK
	if (None != r->h->blank_pointer)	{
		XFreeCursor (r->Xdisplay, r->h->blank_pointer);
		r->h->blank_pointer = None;
	}
# endif
#endif	/* DEBUG */

#ifdef USE_XIM
	if (r->h->Input_Context)    {
		XDestroyIC (r->h->Input_Context);
		r->h->Input_Context = NULL; 
	}
#endif
}


/* EXTPROTO */
void
rxvt_clean_exit (rxvt_t* r)
{
	register int	i;

	/* restore default SIGCHLD signal handler */
	signal (SIGCHLD, SIG_DFL);

	rxvt_free_hidden (r);

#ifdef HAVE_X11_SM_SMLIB_H
	if (r->Options2 & Opt2_enableSessionMgt)
		rxvt_session_exit (r);
#endif

	/* now kill all child processes, zsh puts them into background
	** if we do not do so */
	for (i = 0; i <= LTAB(r); i ++)
		kill (PVTS(r, i)->cmd_pid, SIGHUP);

#ifdef DEBUG	/* Only free X resources in debug mode */
	/* Destroy windows before other X resources */
	if (None != r->TermWin.parent)	{
		XDestroySubwindows (r->Xdisplay, r->TermWin.parent);
		XDestroyWindow (r->Xdisplay, r->TermWin.parent);
		r->TermWin.parent = None;
	}

# ifdef HAVE_SCROLLBARS
	rxvt_scrollbar_clean_exit (r);
# endif

# ifdef HAVE_MENUBAR
	rxvt_menubar_clean_exit (r);
# endif

	rxvt_tabbar_clean_exit (r);

	if (NULL != r->TermWin.font)
		XFreeFont (r->Xdisplay, r->TermWin.font);
# ifndef NO_BOLDFONT
	if (NULL != r->TermWin.bfont &&
		r->TermWin.font != r->TermWin.bfont)	{
		XFreeFont (r->Xdisplay, r->TermWin.bfont);
		r->TermWin.bfont = NULL;
	}
# endif
# ifdef MULTICHAR_SET
	if (NULL != r->TermWin.mfont &&
		r->TermWin.font != r->TermWin.mfont)	{
		XFreeFont (r->Xdisplay, r->TermWin.mfont);
		r->TermWin.mfont = NULL;
	}
# endif
	r->TermWin.font = NULL;		/* clear font */

# ifdef XFT_SUPPORT
	if (NULL != r->TermWin.xftfont)
		XftFontClose (r->Xdisplay, r->TermWin.xftfont);
#  ifndef NO_BOLDFONT
	if (NULL != r->TermWin.xftbfont &&
		r->TermWin.xftfont != r->TermWin.xftbfont)	{
		XftFontClose (r->Xdisplay, r->TermWin.xftbfont);
		r->TermWin.xftbfont = NULL;
	}
#  endif
#  ifdef MULTICHAR_SET
	if (NULL != r->TermWin.xftmfont &&
		r->TermWin.xftfont != r->TermWin.xftmfont)	{
		XftFontClose (r->Xdisplay, r->TermWin.xftmfont);
		r->TermWin.xftmfont = NULL;
	}
#  endif
	r->TermWin.xftfont = NULL;	/* clear font */
# endif

	if (None != r->term_pointer)	{
		XFreeCursor (r->Xdisplay, r->term_pointer);
		r->term_pointer = None;
	}
	if (None != r->TermWin.gc)	{
		XFreeGC (r->Xdisplay, r->TermWin.gc);
		r->TermWin.gc = None;
	}
# ifdef TRANSPARENT
	if (None != r->TermWin.pixmap)	{
		XFreePixmap (r->Xdisplay, r->TermWin.pixmap);
		r->TermWin.pixmap = None;
	}
# endif
	XCloseDisplay (r->Xdisplay);
	r->Xdisplay = NULL;

	free (r->tabstop);			r->tabstop = NULL;
	free (r->PixColors);		r->PixColors = NULL;
# ifdef OFF_FOCUS_FADING
	free (r->PixColorsUnfocus);	r->PixColorsUnfocus = NULL;
# endif
# ifdef XFT_SUPPORT
	free (r->XftColors);		r->XftColors = NULL;
# endif
	free (r->h);				r->h = NULL;
	free (r);					r = NULL;

#endif	/* DEBUG */

	exit(EXIT_SUCCESS);
}


/* ------------------------------------------------------------------------- *
 *							MEMORY ALLOCATION WRAPPERS						*
 * ------------------------------------------------------------------------- */
/* EXTPROTO */
void*
rxvt_malloc(size_t size)
{
	void*		p;

	/* see AC_FUNC_MALLOC macro in autoconf documentation */
	if (0 == size)
		size = 1;

	p = malloc(size);
	if (p)
		return p;

	fprintf(stderr, APL_NAME ": memory allocation failure.  Aborting");
	exit(EXIT_FAILURE);
	/* NOTREACHED */
}


/* EXTPROTO */
void*
rxvt_calloc(size_t number, size_t size)
{
	void*		p;

	p = calloc(number, size);
	if (p)
		return p;

	fprintf(stderr, APL_NAME ": memory allocation failure.  Aborting");
	exit(EXIT_FAILURE);
	 /* NOTREACHED */
}


/* EXTPROTO */
void*
rxvt_realloc(void *ptr, size_t size)
{
	void*		p;

	if (ptr)
		p = realloc(ptr, size);
	else
		p = malloc(size);
	if (p)
		return p;

	fprintf(stderr, APL_NAME ": memory allocation failure.  Aborting");
	exit(EXIT_FAILURE);
	/* NOTREACHED */
}


/* ------------------------------------------------------------------------- *
 *							PRIVILEGED OPERATIONS							*
 * ------------------------------------------------------------------------- */

#if (defined(HAVE_SETEUID) || defined(HAVE_SETREUID)) && !defined(OS_CYGWIN)
static uid_t		g_euid;
static gid_t		g_egid;
#endif

/* take care of suid/sgid super-user (root) privileges */
/* EXTPROTO */
void
rxvt_privileges(int mode)
{
#if !defined(OS_CYGWIN)
# if !defined(HAVE_SETEUID) && defined(HAVE_SETREUID)
/* setreuid() is the poor man's setuid(), seteuid() */
#  define seteuid(a)	setreuid(-1, (a))
#  define setegid(a)	setregid(-1, (a))
#  define HAVE_SETEUID
# endif
# ifdef HAVE_SETEUID
	switch (mode) {
	case IGNORE:
	/*
	 * change effective uid/gid - not real uid/gid - so we can switch
	 * back to root later, as required
	 */
		seteuid(getuid());
		setegid(getgid());
		break;
	case SAVE:
		g_euid = geteuid();
		g_egid = getegid();
		break;
	case RESTORE:
		seteuid(g_euid);
		setegid(g_egid);
		break;
	}
# else
	switch (mode) {
	case IGNORE:
		if (setuid(getuid()) < 0)
			exit (EXIT_FAILURE);
		if (setgid(getgid()) < 0)
			exit (EXIT_FAILURE);
		/* FALLTHROUGH */
	case SAVE:
		/* FALLTHROUGH */
	case RESTORE:
		break;
	}
# endif
#endif
}


#ifdef UTMP_SUPPORT
/* EXTPROTO */
void
rxvt_privileged_utmp(rxvt_t* r, int page, char action)
{
	DBG_MSG(1,(stderr, "rxvt_privileged_utmp %d (%c); waiting for: %c (pid: %d)\n", page, action, PVTS(r, page)->next_utmp_action, (int) getpid()));

	if (PVTS(r, page)->next_utmp_action != action ||
		(action != SAVE && action != RESTORE) ||
		(r->Options & Opt_utmpInhibit) ||
		PVTS(r, page)->ttydev == NULL ||
		*(PVTS(r, page)->ttydev) == (char) 0)
		return;

	rxvt_privileges(RESTORE);
	if (action == SAVE) {
		PVTS(r, page)->next_utmp_action = RESTORE;
		rxvt_makeutent(r, page, PVTS(r, page)->ttydev, r->h->rs[Rs_display_name]);
	}
	else {		/* action == RESTORE */
		PVTS(r, page)->next_utmp_action = IGNORE;
		rxvt_cleanutent(r, page);
	}
	rxvt_privileges(IGNORE);
}
#endif


#ifndef NO_SETOWNER_TTYDEV
/* EXTPROTO */
void
rxvt_privileged_ttydev(rxvt_t* r, int page, char action)
{
	DBG_MSG(1,(stderr, "rxvt_privileged_ttydev %d (r, %c); waiting for: %c (pid: %d)\n", page, action, PVTS(r, page)->next_tty_action, getpid()));
	if (PVTS(r, page)->next_tty_action != action ||
		(action != SAVE && action != RESTORE) ||
		PVTS(r, page)->ttydev == NULL ||
		*(PVTS(r, page)->ttydev) == (char) 0)
		return;

	rxvt_privileges(RESTORE);

	if (action == SAVE) {
		PVTS(r, page)->next_tty_action = RESTORE;
# ifndef RESET_TTY_TO_COMMON_DEFAULTS
/* store original tty status for restoration rxvt_clean_exit() -- rgg 04/12/95 */
		if (lstat(PVTS(r, page)->ttydev, &h->ttyfd_stat) < 0)	/* you lose out */
			PVTS(r, page)->next_tty_action = IGNORE;
		else
# endif
		{
			/* fail silently */
			chown(PVTS(r, page)->ttydev, getuid(), r->h->ttygid);
			chmod(PVTS(r, page)->ttydev, PVTS(r, page)->ttymode);
# ifdef HAVE_REVOKE
			revoke(PVTS(r, page)->ttydev);
# endif
		}
	}
	else {			/* action == RESTORE */
		PVTS(r, page)->next_tty_action = IGNORE;
# ifndef RESET_TTY_TO_COMMON_DEFAULTS
		chmod(PVTS(r, page)->ttydev, PVTS(r, page)->ttyfd_stat.st_mode);
		chown(PVTS(r, page)->ttydev, PVTS(r, page)->ttyfd_stat.st_uid, PVTS(r, page)->ttyfd_stat.st_gid);
# else
		chmod(PVTS(r, page)->ttydev,
			(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH));
		chown(PVTS(r, page)->ttydev, 0, 0);
# endif
	}

	rxvt_privileges(IGNORE);

# ifndef RESET_TTY_TO_COMMON_DEFAULTS
	DBG_MSG(1,(stderr, "%s \"%s\": mode %03o, uid %d, gid %d\n", action == RESTORE ? "Restoring" : (action == SAVE ? "Saving" : "UNKNOWN ERROR for"), PVTS(r, page)->ttydev, PVTS(r, page)->ttyfd_stat.st_mode, PVTS(r, page)->ttyfd_stat.st_uid, PVTS(r, page)->ttyfd_stat.st_gid));
# endif
}
#endif


/*----------------------------------------------------------------------*/
/*
 * Tell the teletype handler what size the window is.
 * Called after a window size change.
 */
/* EXTPROTO */
void
rxvt_tt_winsize(int fd, unsigned short col, unsigned short row, pid_t pid)
{
	struct winsize  ws;

	if (fd < 0)
	return;
	ws.ws_col = col;
	ws.ws_row = row;
	ws.ws_xpixel = ws.ws_ypixel = 0;
#ifndef DEBUG
	(void)ioctl(fd, TIOCSWINSZ, &ws);
#else
	if (ioctl(fd, TIOCSWINSZ, &ws) < 0) {
		DBG_MSG(1,(stderr, "Failed to send TIOCSWINSZ to fd %d\n", fd));
	}
# ifdef SIGWINCH
	else if (pid)		/* force through to the command */
		kill(pid, SIGWINCH);
# endif
#endif	/* DEBUG */
}



#define IDX2FNUM(i)		((FONT0_IDX + i) % MAX_NFONTS)
#define FNUM2IDX(f)		((FONT0_IDX + f) % MAX_NFONTS)


#ifdef XFT_SUPPORT

# ifndef NO_BOLDFONT
/* INTPROTO */
void
rxvt_init_bfont_xft (rxvt_t* r, XftPattern* xpold)
{
	XftResult		fr;
	XftPattern*		xp;
# ifdef DEBUG
	FT_Face			face;
# endif


	if (r->h->rs[Rs_xftwt] && !STRCASECMP(r->h->rs[Rs_xftwt], "bold"))
		return ;

	xp = XftPatternDuplicate (xpold);
	if (NULL == xp)	
		return ;

	/* set font weight */
	XftPatternDel (xp, XFT_WEIGHT);
	XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_BOLD);

	r->TermWin.xftbpattern = XftFontMatch (r->Xdisplay, XSCREEN, xp, &fr);

	if (NULL != r->TermWin.xftbpattern)	{
		r->TermWin.xftbfont = XftFontOpenPattern (r->Xdisplay, r->TermWin.xftbpattern);
		if (NULL == r->TermWin.xftbfont)	{
			/* fall back to normal font */
			XftPatternDestroy (r->TermWin.xftbpattern);
			r->TermWin.xftbpattern = NULL;
		}
# ifdef DEBUG
		else	{
			face = XftLockFace (r->TermWin.xftbfont);
			XftUnlockFace (r->TermWin.xftbfont);
		}
# endif
	}

	XftPatternDestroy (xp);
}
# endif	/* NO_BOLDFONT */


# ifdef MULTICHAR_SET
/* INTPROTO */
int
rxvt_init_mfont_xft (rxvt_t* r, XftPattern* xp, const char* ofname)
{
	XftResult		fr;
	int				len, olen;		/* font name length */
	char*			mfname;			/* mfont name to open */
	char*			omfname = NULL;	/* actually opened mfont name */
	int				width, height;
# ifdef DEBUG
	FT_Face			face;
# endif

	/* temporary XftPattern */
	assert (NULL != xp);
	/* actually opened normal font name */
	assert (NULL != ofname);

	/*
	** Now try to open freetype mfont
	*/
	DBG_MSG(2,(stderr, "load freetype mfont\n"));

	/* font family */
	mfname = (char*) r->h->rs[Rs_xftmfont];
	if (NULL == mfname)
		mfname = rxvt_fallback_mfont_xft (r);

	/*
	** If we should not use mfont, then we always use normal font
	*/
	if (r->Options2 & Opt2_xftNomFont)	{
		r->TermWin.xftmpattern = r->TermWin.xftpattern;
		r->TermWin.xftmfont = r->TermWin.xftfont;
		return 1;
	}

	/*
	** shortcut:
	** mfont is the same as font, just alias mfont to font
	*/
	if (0 == STRCASECMP (ofname, mfname))	{
		r->TermWin.xftmpattern = r->TermWin.xftpattern;
		r->TermWin.xftmfont = r->TermWin.xftfont;
		return 1;
	}

	/* font family */
	XftPatternDel (xp, XFT_FAMILY);
	XftPatternAddString (xp, XFT_FAMILY, mfname);

	XftPatternDel (xp, XFT_SIZE);
	/* this seems to be optimal for simsun font */
	XftPatternAddInteger (xp, XFT_SIZE, r->h->rs[Rs_xftmsz] ?
		(double) r->TermWin.xftmsize :
		(double) (r->TermWin.xftfont->height - 1));

	/* font pattern */
	r->TermWin.xftmpattern = XftFontMatch (r->Xdisplay, XSCREEN, xp, &fr);
	if (NULL == r->TermWin.xftmpattern)
		return 0;

	/* globaladvance */
	if (r->Options2 & Opt2_xftGlobalAdvance)	{
		XftPatternDel (r->TermWin.xftmpattern, FC_GLOBAL_ADVANCE);
		XftPatternAddBool (r->TermWin.xftmpattern, FC_GLOBAL_ADVANCE, FcTrue);
	}

#  ifdef DEBUG_VERBOSE
	FcPatternPrint (r->TermWin.xftmpattern);
#  endif

	XftPatternGetString (r->TermWin.xftmpattern, XFT_FAMILY, 0, &omfname);
	assert (NULL != omfname);	/* shouldn't be NULL */
	len = STRLEN(mfname);
	olen = STRLEN(omfname);
	if (STRNCASECMP (omfname, mfname, (len < olen ? len : olen)))
		rxvt_print_error ("Cannot open mfont %s, use mfont %s instead.",
			mfname, omfname);

	DBG_MSG(1, (stderr, "create xftmpattern = 0x%x on mfont %d\n",
		(unsigned int) r->TermWin.xftmpattern, r->h->rs[Rs_xftmsz] ?
		r->TermWin.xftmsize : r->TermWin.xftfont->height-1));
	r->TermWin.xftmfont = XftFontOpenPattern (r->Xdisplay, r->TermWin.xftmpattern);
	
	if (NULL == r->TermWin.xftmfont)
		goto Failure;

# ifdef DEBUG
	face = XftLockFace (r->TermWin.xftmfont);
	XftUnlockFace (r->TermWin.xftmfont);
# endif

	width = r->TermWin.xftmfont->max_advance_width;
	if ((width & 0x01) == 1)	/* in case width is not even */
		r->TermWin.xftmono = 0;
	else
	if (STRCASECMP (ofname, omfname) &&
		(r->TermWin.fwidth != (width >> 1)))
		r->TermWin.xftmono = 0;
	else
	if (r->Options2 & Opt2_xftSlowOutput)
		r->TermWin.xftmono = 0;
	DBG_MSG(1, (stderr, "xftmono is %d\n", r->TermWin.xftmono));
	MAX_IT (r->TermWin.fwidth, (width >> 1));

	height = r->TermWin.xftmfont->ascent + r->TermWin.xftmfont->descent;
# ifndef NO_LINESPACE
	height += r->TermWin.lineSpace;
# endif
	MAX_IT (r->TermWin.fheight, height);

	return 1;


Failure:
	if (r->TermWin.xftmpattern)	{
		XftPatternDestroy (r->TermWin.xftmpattern);
		r->TermWin.xftmpattern = NULL;
	}
	return 0;
}
# endif	/* MULTICHAR_SET */



/* EXTPROTO */
int
rxvt_init_font_xft (rxvt_t* r)
{
	XftResult		fr;
	XftPattern*		xp;
	XGlyphInfo		ext1, ext2;
	int				len, olen;		/* font name length */
	char*			fname;			/* font name to open */
	char*			ofname = NULL;	/* actually opened font name */
# ifdef DEBUG
	FT_Face			face;
# endif


	DBG_MSG(2,(stderr, "rxvt_init_font_xft\n"));


	DBG_MSG(2,(stderr, "load freetype font\n"));
	xp = XftPatternCreate ();
	if (NULL == xp)
		return 0;

	/* font family */
	fname = (char*) r->h->rs[Rs_xftfont];
	if (NULL == fname)
		fname = "Luxi Mono";
	XftPatternAddString (xp, XFT_FAMILY, fname);

	/* No spacing between lines */
	XftPatternAddBool (xp, XFT_MINSPACE, FcFalse);

	/* antialias */
	if (r->Options2 & Opt2_xftAntialias)
		XftPatternAddBool (xp, XFT_ANTIALIAS, FcTrue);
	else
		XftPatternAddBool (xp, XFT_ANTIALIAS, FcFalse);

	/* hinting */
	if (r->Options2 & Opt2_xftHinting)
		XftPatternAddBool (xp, FC_HINTING, FcTrue);
	else
		XftPatternAddBool (xp, FC_HINTING, FcFalse);

	/* autohint */
	if (r->Options2 & Opt2_xftAutoHint)
		XftPatternAddBool (xp, FC_AUTOHINT, FcTrue);
	else
		XftPatternAddBool (xp, FC_AUTOHINT, FcFalse);

	/* font size, we always set it. if it's not set by the user, we
	** have chosen a default value (12) for it */
	XftPatternAddDouble (xp, XFT_SIZE, (double) r->TermWin.xftsize);
	/* Enforce Size/Pixel_Size = 1.0 */
	XftPatternAddDouble (xp, XFT_SCALE, (double) 1.0);

	/* font width */
	if (r->h->rs[Rs_xftwd])	{
		if (0 == STRCASECMP (r->h->rs[Rs_xftwd], "ultracondensed"))
			XftPatternAddInteger (xp, FC_WIDTH, FC_WIDTH_ULTRACONDENSED);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwd], "condensed"))
			XftPatternAddInteger (xp, FC_WIDTH, FC_WIDTH_CONDENSED);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwd], "normal"))
			XftPatternAddInteger (xp, FC_WIDTH, FC_WIDTH_NORMAL);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwd], "expanded"))
			XftPatternAddInteger (xp, FC_WIDTH, FC_WIDTH_EXPANDED);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwd], "ultraexpanded"))
			XftPatternAddInteger (xp, FC_WIDTH, FC_WIDTH_ULTRAEXPANDED);
		else
			XftPatternAddInteger (xp, FC_WIDTH, FC_WIDTH_NORMAL);
	}

	/* font weight */
	if (r->h->rs[Rs_xftwt])	{
		if (0 == STRCASECMP (r->h->rs[Rs_xftwt], "light"))
			XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_LIGHT);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwt], "medium"))
			XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_MEDIUM);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwt], "demibold"))
			XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_DEMIBOLD);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwt], "bold"))
			XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_BOLD);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftwt], "black"))
			XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_BLACK);
		else	/* default is medium */
			XftPatternAddInteger (xp, XFT_WEIGHT, XFT_WEIGHT_MEDIUM);
	}

	/* font slant */
	if (r->h->rs[Rs_xftst])	{
		if (0 == STRCASECMP (r->h->rs[Rs_xftst], "roman"))
			XftPatternAddInteger (xp, XFT_SLANT, XFT_SLANT_ROMAN);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftst], "italic"))
			XftPatternAddInteger (xp, XFT_SLANT, XFT_SLANT_ITALIC);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftst], "oblique"))
			XftPatternAddInteger (xp, XFT_SLANT, XFT_SLANT_OBLIQUE);
		else	/* default is roman */
			XftPatternAddInteger (xp, XFT_SLANT, XFT_SLANT_ROMAN);
	}

	/* font rgba */
	if (r->h->rs[Rs_xftrgb])	{
		if (0 == STRCASECMP (r->h->rs[Rs_xftrgb], "rgb"))
			XftPatternAddInteger (xp, XFT_RGBA, XFT_RGBA_RGB);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftrgb], "bgr"))
			XftPatternAddInteger (xp, XFT_RGBA, XFT_RGBA_BGR);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftrgb], "vrgb"))
			XftPatternAddInteger (xp, XFT_RGBA, XFT_RGBA_VRGB);
		else
		if (0 == STRCASECMP (r->h->rs[Rs_xftrgb], "vbgr"))
			XftPatternAddInteger (xp, XFT_RGBA, XFT_RGBA_VBGR);
		else
			XftPatternAddInteger (xp, XFT_RGBA, XFT_RGBA_NONE);
	}

	/* Only accept mono fonts */
	/*
	XftPatternAddInteger (xp, XFT_SPACING, XFT_MONO);
	*/

	r->TermWin.xftpattern = XftFontMatch (r->Xdisplay, XSCREEN, xp, &fr);

	if (NULL == r->TermWin.xftpattern)
		goto Failure;

	/* globaladvance */
	if (r->Options2 & Opt2_xftGlobalAdvance)	{
		XftPatternDel (r->TermWin.xftpattern, FC_GLOBAL_ADVANCE);
		XftPatternAddBool (r->TermWin.xftpattern, FC_GLOBAL_ADVANCE, FcTrue);
	}

# ifdef DEBUG_VERBOSE
	FcPatternPrint (r->TermWin.xftpattern);
# endif
	XftPatternGetString (r->TermWin.xftpattern, XFT_FAMILY, 0, &ofname);
	assert (NULL != ofname);	/* shouldn't be NULL */
	len = STRLEN(fname);
	olen = STRLEN(ofname);
	if (STRNCASECMP (ofname, fname, (len < olen ? len : olen)))
		rxvt_print_error ("Cannot open font %s, use font %s instead.",
			fname, ofname);

	DBG_MSG(1, (stderr, "create xftpattern = 0x%x on font %d\n",
		(unsigned int) r->TermWin.xftpattern, r->TermWin.xftsize));
	r->TermWin.xftfont = XftFontOpenPattern (r->Xdisplay, r->TermWin.xftpattern);
	
	if (NULL == r->TermWin.xftfont)	
		goto Failure;

	r->TermWin.fwidth = r->TermWin.xftfont->max_advance_width;
	r->TermWin.fheight = r->TermWin.xftfont->ascent + r->TermWin.xftfont->descent;
# ifndef NO_LINESPACE
	r->TermWin.fheight += r->TermWin.lineSpace;
# endif

#ifdef DEBUG
	face = XftLockFace (r->TermWin.xftfont);
	XftUnlockFace (r->TermWin.xftfont);
# endif

	/*
	** Do not trust the font width
	*/
	XftTextExtents8 (r->Xdisplay, r->TermWin.xftfont, "W", 1, &ext1);
	XftTextExtents8 (r->Xdisplay, r->TermWin.xftfont, "i", 1, &ext2);
	if (ext1.xOff == ext2.xOff)
		r->TermWin.xftfnmono = r->TermWin.xftmono = 1;
	else
		r->TermWin.xftfnmono = r->TermWin.xftmono = 0;
	DBG_MSG(1, (stderr, "xftfnmono is %d\n", r->TermWin.xftfnmono));


# ifndef NO_BOLDFONT
	/*
	rxvt_init_bfont_xft (r, xp);
	*/
# endif

# ifdef MULTICHAR_SET
	if (!rxvt_init_mfont_xft (r, xp, ofname))
		goto Failure;
# endif	/* MULTICHAR_SET */

	XftPatternDestroy (xp); xp = NULL;

	return 1;


Failure:
	if (xp)	{
		XftPatternDestroy (xp); xp = NULL;
	}
	if (r->TermWin.xftpattern)	{
		XftPatternDestroy (r->TermWin.xftpattern);
		r->TermWin.xftpattern = NULL;
	}
	return 0;
}


/*----------------------------------------------------------------------*/
/* rxvt_init_font_fixed () - initialize fixed font */
/* INTPROTO */
void
rxvt_init_font_fixed (rxvt_t* r)
{
	XFontStruct*	xfont;


	DBG_MSG(1,(stderr, " load font (fixed)\n"));
	xfont = XLoadQueryFont (r->Xdisplay, "fixed");
	if (NULL == xfont)	{
		rxvt_print_error("fatal error, aborting...");
		exit(EXIT_FAILURE);
	}

	r->TermWin.font = xfont;
#ifndef NO_BOLDFONT
	/*
	r->TermWin.bfont = r->TermWin.xftbfont? xfont : NULL;
	*/
	r->TermWin.bfont = NULL;
#endif
#ifdef MULTICHAR_SET
	r->TermWin.mfont = xfont;
#endif
}
#endif	/* XFT_SUPPORT */


/* rxvt_init_font_x11 () - initialize font */
/* EXTPROTO */
void
rxvt_init_font_x11 (rxvt_t *r)
{
	char*			msg = "can't load font \"%s\"";
	XFontStruct*	xfont;
#ifndef NO_BOLDFONT
	XFontStruct*	bfont;
	int				ckfont;
#endif
	int				fh = 0, fw = 0;
	int				idx = 0;			/* index into rs_font[] */


	DBG_MSG(1,(stderr, "rxvt_init_font_x11 \n"));


#ifdef XFT_SUPPORT
	/* Only load fixed font if we use freetype font */
	if ((r->Options & Opt_xft) && r->TermWin.xftfont)	{
		rxvt_init_font_fixed (r);
		return;
	}
#endif


	r->h->fnum = FONT0_IDX;
	idx = FNUM2IDX(r->h->fnum);

	/* OK, now it's time to load the default font */
	DBG_MSG(1,(stderr, " load font (%s)\n", r->h->rs[Rs_font+idx]));
	xfont = XLoadQueryFont (r->Xdisplay, r->h->rs[Rs_font+idx]);
	if (NULL == xfont) {
		/* failed to load font */
		rxvt_print_error(msg, r->h->rs[Rs_font+idx]);

		/* try to load fixed font */
		r->h->rs[Rs_font+idx] = "fixed";
		DBG_MSG(1,(stderr, " load font (%s)\n", r->h->rs[Rs_font+idx]));
		xfont = XLoadQueryFont(r->Xdisplay, r->h->rs[Rs_font+idx]);
		if (NULL == xfont) {
			/* still failed to load font */
			rxvt_print_error(msg, r->h->rs[Rs_font+idx]);

			/* cannot load any font, fatal error, abort the program */
			goto Abort;
		}
	}
	/* Font loading succeeded */
	if (NULL != xfont)	{
		r->TermWin.font = xfont;
	}


	/* set the font sizes */
	fw = rxvt_get_font_widest (r->TermWin.font);
	fh = r->TermWin.font->ascent + r->TermWin.font->descent;
#ifndef NO_LINESPACE
	fh += r->TermWin.lineSpace;
#endif

	if (fw == r->TermWin.font->min_bounds.width)
		/* Mono-spaced (fixed width) font */
		r->TermWin.propfont &= ~PROPFONT_NORMAL;
	else
		/* Proportional font */
		r->TermWin.propfont |= PROPFONT_NORMAL;

#ifndef NO_BOLDFONT
	ckfont = !(fw == r->TermWin.fwidth && fh == r->TermWin.fheight);
#endif

	r->TermWin.fwidth = fw;
	r->TermWin.fheight = fh;


#ifndef NO_BOLDFONT
	bfont = NULL;
	if (ckfont)	{
		/* try to load boldFont, fail silently */
		if (NULL != r->h->rs[Rs_boldFont])	{
			DBG_MSG(1,(stderr, " load bfont (%s)\n", r->h->rs[Rs_boldFont]));
			bfont = XLoadQueryFont (r->Xdisplay, r->h->rs[Rs_boldFont]);
		}

		if (NULL != bfont)	{
			/* Loading bold font succeeded */
			fw = rxvt_get_font_widest (bfont);
			fh = bfont->ascent + bfont->descent;
#ifndef NO_LINESPACE
			fh += r->TermWin.lineSpace;
#endif
			if (fw <= r->TermWin.fwidth && fh <= r->TermWin.fheight) {
				r->TermWin.bfont = bfont;
				if (fw == r->TermWin.fwidth)
					r->TermWin.propfont &= ~PROPFONT_NORMAL;
				else
					r->TermWin.propfont |= PROPFONT_NORMAL;
			}
			else	{
				XFreeFont (r->Xdisplay, bfont);
			}
		}
	}
#endif /* NO_BOLDFONT */


#ifdef MULTICHAR_SET
	/* load font or substitute */
	DBG_MSG(1,(stderr, " load mfont (%s)\n", r->h->rs[Rs_mfont+idx]));
	xfont = XLoadQueryFont(r->Xdisplay, r->h->rs[Rs_mfont+idx]);
	if (NULL == xfont) {
		char*	ptr;

		/* failed to load font */
		rxvt_print_error(msg, r->h->rs[Rs_mfont+idx]);

		ptr = rxvt_fallback_mfont_x11 (r);
		DBG_MSG(1,(stderr, " load mfont (%s)\n", ptr));
		xfont = XLoadQueryFont(r->Xdisplay, ptr);
		if (NULL != xfont)
			r->h->rs[Rs_mfont+idx] = ptr;
		else	{
			/* still failed to load font */
			rxvt_print_error(msg, ptr);
			/* cannot load any mfont, fatal error, abort the program */
			goto Abort;
		}
	}
	if (NULL != xfont)	{
		r->TermWin.mfont = xfont;
	}
#endif	/* MULTICHAR_SET */

	/* Succeeded to load font, return now */
	return ;

Abort:
	rxvt_print_error("fatal error, aborting...");
	exit(EXIT_FAILURE);
}


#ifdef XFT_SUPPORT
/* EXTPROTO */
int
rxvt_change_font_xft (rxvt_t* r, const char* fontname)
{
	XftPattern*	xp;
	XftFont*	xf;
# ifdef MULTICHAR_SET
	XftPattern*	mxp;
	XftFont*	mxf;
# endif
	int			resize, oldsize = r->TermWin.xftsize;


	assert (fontname);
	DBG_MSG(2,(stderr, "rxvt_change_font_xft (%s)\n", fontname));

	/* we only accept FONT_CMD now for XFT font ;-) */
	if (FONT_CMD != fontname[0])
		return 0;
	if ((char) 0 != fontname[1] &&
		'+' != fontname[1] &&
		'-' != fontname[1] &&
		!isdigit((int) fontname[1]))
		return 0;
	if (('+' == fontname[1]) && ((char) 0 == fontname[2]))
		resize = +1;
	else
	if (('-' == fontname[1]) && ((char) 0 == fontname[2]))
		resize = -1;
	else
		resize = atoi (fontname+1);
	r->TermWin.xftsize += resize;
	/* adjust for minimal font size */
	if (r->TermWin.xftsize < MIN_XFT_FONT_SIZE)
		r->TermWin.xftsize = MIN_XFT_FONT_SIZE;
	/* no change of font size */
	if (r->TermWin.xftsize == oldsize)
		return 0;
# ifdef MULTICHAR_SET
	if (r->h->rs[Rs_xftmsz])	{
		r->TermWin.xftmsize += resize;
		if (r->TermWin.xftmsize < MIN_XFT_FONT_SIZE)
			r->TermWin.xftmsize = MIN_XFT_FONT_SIZE;
	}
# endif

	/*
	** Now reload xft font with new size using rxvt_init_font_xft.
	** We can reuse it since we have only changed the font size.
	** Before doing so, let us backup the old xft info in case
	** we cannot load new xft font. In that case, we can still
	** fallback to the old font.
	*/
	xp = r->TermWin.xftpattern;	r->TermWin.xftpattern = NULL;
	xf = r->TermWin.xftfont;	r->TermWin.xftfont = NULL;
# ifdef MULTICHAR_SET
	mxp = r->TermWin.xftmpattern;	r->TermWin.xftmpattern = NULL;
	mxf = r->TermWin.xftmfont;		r->TermWin.xftmfont = NULL;
# endif
	if (!rxvt_init_font_xft (r))	{
		/* fallback to old font */
		r->TermWin.xftpattern = xp;
		r->TermWin.xftfont = xf;
# ifdef MULTICHAR_SET
		r->TermWin.xftmpattern = mxp;
		r->TermWin.xftmfont = mxf;
# endif
		return 0;
	}

	/*
	** It is safe now to free old XftPattern, but apparently there is
	** some problems if we change the font size in reverse direction,
	** e.g., increase font size then decrease it, or decrease font
	** size then increase it. It can crash the terminal.
	**
	** 12-19-2004: Is it because the pattern is got via XftFontMatch,
	** which actually returns a *static* pattern that we should not
	** manually destroy? Anyway, to avoid the crash, let us comment
	** out the XftPatternDestroy calls below. I hope there is a more
	** clear documentation about these Xft functions!!!
	*/
	/*
	DBG_MSG(1, (stderr, "destroy xftpattern = 0x%x\n", (unsigned int) xp));
	FcPatternPrint (xp);
	FcPatternPrint (r->TermWin.xftpattern);
	XftPatternDestroy (xp);
# ifdef MULTICHAR_SET
	if (xp != mxp)
		XftPatternDestroy (mxp);
# endif
	*/

	return 1;
}
#endif	/* XFT_SUPPORT */


/*----------------------------------------------------------------------*/
/* rxvt_change_font_x11 () - Switch to a new font */
/*
 * init = 1   - initialize
 *
 * fontname == FONT_UP  - switch to bigger font
 * fontname == FONT_DN  - switch to smaller font
 */
/* EXTPROTO */
int
rxvt_change_font_x11 (rxvt_t* r, const char *fontname)
{
	char*			msg = "can't load font \"%s\"";
	XFontStruct*	xfont;
#ifndef NO_BOLDFONT
	XFontStruct*	bfont;
	int				ckfont;
#endif
	int				fh = 0, fw = 0;
	int				idx = 0;			/* index into rs_font[] */


	assert (fontname);
	DBG_MSG(1,(stderr, "rxvt_change_font_x11 (%s)\n", fontname));


	switch (fontname[0]) {
	/* special (internal) prefix for font commands */
	case FONT_CMD: /* FONT_CMD =='#' */
		idx = atoi (fontname + 1);
		switch (fontname[1]) {
		case '+':	/* corresponds to FONT_UP */
			r->h->fnum += (idx ? idx : 1);	/* "#+" or "#+3"? */
			r->h->fnum %= MAX_NFONTS;
			break;

		case '-':	/* corresponds to FONT_DN */
			r->h->fnum += (idx ? idx : -1);	/* "#-" or "#-3"? */
			r->h->fnum %= MAX_NFONTS;
			break;

		default:
			/* input is not a logical font number */
			if (fontname[1] != '\0' &&
				!isdigit((int) fontname[1]))
				return 0;
			/*
			** input logical font number too big, but don't worry,
			** we will handle it gracefully ;-)
			*/
			r->h->fnum = IDX2FNUM(idx);
			break;
		}
		fontname = NULL;
		break;

	default:
		/* search for existing fontname */
		for (idx = 0; idx < MAX_NFONTS; idx++) {
			if (!STRCMP (r->h->rs[Rs_font+idx], fontname)) {
				r->h->fnum = IDX2FNUM(idx);
				fontname = NULL;
				break;
			}
		}
		break;
	}
	/* re-position around the normal font */
	if (r->h->fnum < 0)
		r->h->fnum += (-(r->h->fnum / MAX_NFONTS - 1) * MAX_NFONTS);
	idx = FNUM2IDX(r->h->fnum);


	/*
	** If fontname != NULL, it's some new font not in the rs_font.
	** We try to load it and replace font in rs_font if succeed.
	*/
	if (NULL != fontname)	{
		xfont = XLoadQueryFont(r->Xdisplay, fontname);
		if (xfont)	{
			/* load new font succeessfully */
			char* ptr = STRDUP (fontname);
			if (ptr) {
				if (r->h->newfont[idx] != NULL)	
					free (r->h->newfont[idx]);
				r->h->newfont[idx] = ptr;
				r->h->rs[Rs_font+idx] = r->h->newfont[idx];
			}
			else	{
				assert (0);	/* shouldn't happen */
			}
			/* Free it by now */
			XFreeFont (r->Xdisplay, xfont);
		}
	}


	/*
	** OK, now it's time to load font or substitute
	*/
	DBG_MSG(1,(stderr, " load font (%s)\n", r->h->rs[Rs_font+idx]));
	xfont = XLoadQueryFont (r->Xdisplay, r->h->rs[Rs_font+idx]);
	if (!xfont) {
		/* failed to load font */
		rxvt_print_error(msg, r->h->rs[Rs_font+idx]);

		/* try to load fixed font */
		r->h->rs[Rs_font+idx] = "fixed";
		DBG_MSG(1,(stderr, " load font (%s)\n", r->h->rs[Rs_font+idx]));
		xfont = XLoadQueryFont(r->Xdisplay, r->h->rs[Rs_font+idx]);
		if (!xfont) {
			/* still failed to load font */
			rxvt_print_error(msg, r->h->rs[Rs_font+idx]);
			return 0;
		}
	}
	/* Font loading succeeded */
	if (xfont)	{
		/*
		** if the previous font exists, replace it with new font.
		** otherwise, keep the old font, and do nothing
		*/
		if (r->TermWin.font)
			XFreeFont (r->Xdisplay, r->TermWin.font);
		r->TermWin.font = xfont;
	}


	/* set the font sizes */
	fw = rxvt_get_font_widest (r->TermWin.font);
	fh = r->TermWin.font->ascent + r->TermWin.font->descent;
#ifndef NO_LINESPACE
	fh += r->TermWin.lineSpace;
#endif

	if (fw == r->TermWin.font->min_bounds.width)
		/* Mono-spaced (fixed width) font */
		r->TermWin.propfont &= ~PROPFONT_NORMAL;
	else
		/* Proportional font */
		r->TermWin.propfont |= PROPFONT_NORMAL;

#ifndef NO_BOLDFONT
	ckfont = !(fw == r->TermWin.fwidth && fh == r->TermWin.fheight);
#endif

#ifdef XFT_SUPPORT
	/* Set font size when XFT is not enabled */
	if (!((r->Options & Opt_xft) && r->TermWin.xftfont))
#endif
	{
		r->TermWin.fwidth = fw;
		r->TermWin.fheight = fh;
	}


#ifndef NO_BOLDFONT
	bfont = NULL;
	if (ckfont)	{
		/* try to load boldFont, fail silently */
		if (NULL == r->TermWin.bfont &&
			NULL != r->h->rs[Rs_boldFont])	{
			DBG_MSG(1,(stderr, " load bfont (%s)\n", r->h->rs[Rs_boldFont]));
			bfont = XLoadQueryFont (r->Xdisplay, r->h->rs[Rs_boldFont]);
		}

		if (bfont)	{
			/* Loading bold font succeeded */
			fw = rxvt_get_font_widest (bfont);
			fh = bfont->ascent + bfont->descent;
#ifndef NO_LINESPACE
			fh += r->TermWin.lineSpace;
#endif
			if (fw <= r->TermWin.fwidth && fh <= r->TermWin.fheight) {
				if (r->TermWin.bfont)
					XFreeFont (r->Xdisplay, r->TermWin.bfont);
				r->TermWin.bfont = bfont;
				if (fw == r->TermWin.fwidth)
					r->TermWin.propfont &= ~PROPFONT_NORMAL;
				else
					r->TermWin.propfont |= PROPFONT_NORMAL;
			}
			else	{
				XFreeFont (r->Xdisplay, bfont);
			}
		}
	}
#endif /* NO_BOLDFONT */


#ifdef MULTICHAR_SET
	/* load font or substitute */
	DBG_MSG(1,(stderr, " load mfont (%s)\n", r->h->rs[Rs_mfont+idx]));
	xfont = XLoadQueryFont(r->Xdisplay, r->h->rs[Rs_mfont+idx]);
	if (!xfont) {
		char*	ptr;

		/* failed to load font */
		rxvt_print_error(msg, r->h->rs[Rs_mfont+idx]);

		/* try to load default font */
		ptr = rxvt_fallback_mfont_x11 (r);
		DBG_MSG(1,(stderr, " load mfont (%s)\n", ptr));
		xfont = XLoadQueryFont(r->Xdisplay, ptr);
		if (xfont)
			r->h->rs[Rs_mfont+idx] = ptr;
		else	{
			/* still failed to load font */
			rxvt_print_error(msg, ptr);
			return 0;
		}
	}
	if (xfont)	{
		/*
		** if the previous font exists, replace it with new font.
		** otherwise, keep the old font, and do nothing
		*/
		if (r->TermWin.mfont)
			XFreeFont (r->Xdisplay, r->TermWin.mfont);
		r->TermWin.mfont = xfont;
	}
#endif	/* MULTICHAR_SET */

	return 1;
}


/* INTPROTO */
void
rxvt_font_up_down(rxvt_t* r, int n, int direction)
{
	const char	 *p;
	int				initial, j;

	for (j = 0; j < n; j++) {
	initial = r->h->fnum;
	for (;;) {
		r->h->fnum += direction;
		if (r->h->fnum == MAX_NFONTS || r->h->fnum == -1) {
		r->h->fnum = initial;
		return;
		}
		p = r->h->rs[Rs_font + FNUM2IDX(r->h->fnum)];
		if (p != NULL && STRLEN(p) > 1)
		break;
	}
	}
}
#undef IDX2FNUM
#undef FNUM2IDX


/* INTPROTO */
int
rxvt_get_font_widest(XFontStruct *f)
{
	int				i, cw, fw = 0;

	if (f->min_bounds.width == f->max_bounds.width)
		return f->min_bounds.width;
	if (f->per_char == NULL)
		return f->max_bounds.width;
	for (i = f->max_char_or_byte2 - f->min_char_or_byte2; --i >= 0;) {
		cw = f->per_char[i].width;
		MAX_IT(fw, cw);
	}
	return fw;
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
#ifdef X_HAVE_UTF8_STRING
/* INTPROTO */
void
rxvt_set_utf8_property (rxvt_t* r, Atom prop, Window win, const char* str)
{
#ifdef HAVE_WCHAR_H
	wchar_t*	ws = rxvt_mbstowcs (str);
	char*		s = rxvt_wcstoutf8 (ws);

	XChangeProperty (r->Xdisplay, win, prop,
		r->h->xa[XA_UTF8_STRING], 8, PropModeReplace,
		(unsigned char*) s, STRLEN (s));

	free (s);
	free (ws);
#endif	/* HAVE_WCHAR_H */
}
#endif	/* X_HAVE_UTF8_STRING */


/* xterm sequences - title, iconName, color (exptl) */
/* EXTPROTO */
void
rxvt_set_win_title (rxvt_t* r, Window win, const char* str)
{
	XChangeProperty (r->Xdisplay, win, XA_WM_NAME,
		XA_STRING, 8, PropModeReplace,
		(unsigned char*) str, STRLEN (str));
#ifdef X_HAVE_UTF8_STRING
	rxvt_set_utf8_property (r, r->h->xa[XA_NET_WM_NAME],
		r->TermWin.parent, str);
#endif
}


/* EXTPROTO */
void
rxvt_set_term_title (rxvt_t* r, const unsigned char *str)
{
#ifdef SMART_WINDOW_TITLE
	XTextProperty	prop;

	if (XGetWMName (r->Xdisplay, r->TermWin.parent, &prop) == 0)
		prop.value = NULL;
	if (NULL == prop.value || STRCMP(prop.value, str))
#endif
	{
		rxvt_set_win_title (r, r->TermWin.parent, str);
	}
}


/* EXTPROTO */
void
rxvt_set_icon_name (rxvt_t* r, const unsigned char *str)
{
	XTextProperty	prop;

#ifdef SMART_WINDOW_TITLE
	if (XGetWMIconName(r->Xdisplay, r->TermWin.parent, &prop) == 0)
		prop.value = NULL;
	if (NULL == prop.value || STRCMP(prop.value, str))
#endif
	{
		XChangeProperty (r->Xdisplay, r->TermWin.parent,
			XA_WM_ICON_NAME, XA_STRING, 8, PropModeReplace,
			(unsigned char*) str, STRLEN (str));
#ifdef X_HAVE_UTF8_STRING
		rxvt_set_utf8_property (r, r->h->xa[XA_NET_WM_ICON_NAME],
			r->TermWin.parent, str);
#endif
	}
}


#ifdef XTERM_COLOR_CHANGE
/* EXTPROTO */
void
rxvt_set_window_color(rxvt_t* r, int idx, const char *color)
{
	XColor			xcol;
	int				ufbg_switched;
#ifdef OFF_FOCUS_FADING
	int				color_switched;
#endif
	int				color_set;
	register int	i;


	if (color == NULL || *color == '\0')
		return;

	color_set = ISSET_PIXCOLOR(r->h, idx);
	/*
	** Restore colors now. Remember to restore ufbg color before
	** PixColors
	*/
	ufbg_switched = rxvt_restore_ufbg_color (r);
#ifdef OFF_FOCUS_FADING
	color_switched = rxvt_restore_pix_color (r);
#endif

	/* handle color aliases */
	if (isdigit((int) *color)) {
		i = atoi(color);
		if (i >= 8 && i <= 15) {	/* bright colors */
			i -= 8;
# ifndef NO_BRIGHTCOLOR
			r->PixColors[idx] = r->PixColors[minBrightCOLOR + i];
			SET_PIXCOLOR(r->h, idx);
			goto Done;
# endif
		}
		if (i >= 0 && i <= 7) {	/* normal colors */
			r->PixColors[idx] = r->PixColors[minCOLOR + i];
			SET_PIXCOLOR(r->h, idx);
			goto Done;
		}
	}
	if (!rxvt_parse_alloc_color(r, &xcol, color))	{
		/* restore to original state and return */
#ifdef OFF_FOCUS_FADING
		if (color_switched)
			rxvt_switch_pix_color (r);
#endif
		if (ufbg_switched)
			rxvt_switch_ufbg_color (r);
		return;
	}

/* XStoreColor (r->Xdisplay, XCMAP, XColor*); */

/*
 * FIXME: should free colors here, but no idea how to do it so instead,
 * so just keep gobbling up the colormap
 */
# if 0
	for (i = Color_Black; i <= Color_White; i++)
	if (r->PixColors[idx] == r->PixColors[i])
		break;
	if (i > Color_White) {
	/* fprintf (stderr, "XFreeColors: r->PixColors [%d] = %lu\n", idx, r->PixColors [idx]); */
	XFreeColors(r->Xdisplay, XCMAP, (r->PixColors + idx), 1,
			DisplayPlanes(r->Xdisplay, XSCREEN));
	}
# endif

	r->PixColors[idx] = xcol.pixel;

#ifdef OFF_FOCUS_FADING
	if (r->h->rs[Rs_fade])
		r->PixColorsUnfocus[idx] = rxvt_fade_color (r, xcol.pixel);
#endif
#ifdef XFT_SUPPORT
	if (color_set)	{
		XftColorFree (r->Xdisplay, XVISUAL, XCMAP, &(r->XftColors[idx]));
	}
	rxvt_alloc_xft_color (r, r->PixColors[idx], &(r->XftColors[idx]));
#endif
	SET_PIXCOLOR(r->h, idx);

Done:
	/*
	** Ok, now return to original state before restoring colors.
	** Remember to change PixColors before ufbg color
	*/
#ifdef OFF_FOCUS_FADING
	if (color_switched)
		rxvt_switch_pix_color (r);
#endif
	if (ufbg_switched)
		rxvt_switch_ufbg_color (r);

	/* background color has changed */
	if ((idx == Color_bg && ufbg_switched) ||
		(idx == Color_ufbg && ufbg_switched)) {
#ifdef TRANSPARENT
		if (!(r->Options & Opt_transparent))
#endif
		{
			for (i = 0; i <= LTAB(r); i ++)	{
#ifdef BACKGROUND_IMAGE
				if (None == AVTS(r)->pixmap)
#endif
				{
					XSetWindowBackground(r->Xdisplay, PVTS(r, i)->vt,
						r->PixColors[Color_bg]);
				}
			}	/* for */
		}
	}

	/* handle Color_BD, scrollbar background, etc. */
	rxvt_set_colorfgbg(r);
	rxvt_recolour_cursor(r);

#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
# ifdef TINTING_SUPPORT
	if (idx == Color_tint)	{
#  ifdef TRANSPARENT
		if (r->Options & Opt_transparent)
			/* reset background */
			rxvt_check_our_parents (r);
		else
#  endif
#  ifdef BACKGROUND_IMAGE
		{	/* reset background */
			for (i = 0; i <= LTAB(r); i ++)
				rxvt_resize_pixmap (r, i);
		}
#  endif
		{	/* empty body to suppress compile error */	}
	}
# endif	/* TINTING_SUPPORT */
#endif	/* TRANSPARENT || BACKGROUND_IMAGE */

	/* the only reasonable way to enforce a clean update */
	/*
	for (i = 0; i <= LTAB(r); i ++)
		rxvt_scr_poweron(r, i);
	*/
	/*
	** Poweron is not a good idea since other terminal may be using
	** secondary screen, like vim. We should just refresh the screen.
	**
	** And we only need to refresh the current screen. When we switch
	** to other terminals, their screen will be automatically updated.
	*/
	rxvt_scr_clear (r, ATAB(r));
	rxvt_scr_touch (r, ATAB(r), True);
}

#else
# define rxvt_set_window_color(r, idx,color)	((void)0)
#endif		/* XTERM_COLOR_CHANGE */


/* EXTPROTO */
void
rxvt_recolour_cursor(rxvt_t *r)
{
	XColor			xcol[2];

	xcol[0].pixel = r->PixColors[Color_pointer];
	xcol[1].pixel = r->PixColors[Color_bg];
	XQueryColors(r->Xdisplay, XCMAP, xcol, 2);
	XRecolorCursor(r->Xdisplay, r->term_pointer, &(xcol[0]), &(xcol[1]));
}


/*----------------------------------------------------------------------*/
/*
 * find if fg/bg matches any of the normal (low-intensity) colors
 */
/* INTPROTO */
void
rxvt_set_colorfgbg(rxvt_t *r)
{
	unsigned int	i;
	const char	 *xpmb = "\0";
	char			fstr[sizeof("default") + 1], bstr[sizeof("default") + 1];

	r->h->env_colorfgbg = rxvt_malloc(sizeof("COLORFGBG=default;default;bg")
						+ 1);
	STRCPY(fstr, "default");
	STRCPY(bstr, "default");
	for (i = Color_Black; i <= Color_White; i++)
		if (r->PixColors[Color_fg] == r->PixColors[i]) {
			sprintf(fstr, "%d", (i - Color_Black));
			break;
		}
	for (i = Color_Black; i <= Color_White; i++)
		if (r->PixColors[Color_bg] == r->PixColors[i]) {
			sprintf(bstr, "%d", (i - Color_Black));
#ifdef BACKGROUND_IMAGE
			xpmb = "default;";
#endif
			break;
		}
	sprintf(r->h->env_colorfgbg, "COLORFGBG=%s;%s%s", fstr, xpmb, bstr);
	putenv(r->h->env_colorfgbg);

#ifndef NO_BRIGHTCOLOR
	r->h->colorfgbg = DEFAULT_RSTYLE;
	for (i = minCOLOR; i <= maxCOLOR; i++) {
	if (r->PixColors[Color_fg] == r->PixColors[i]
# ifndef NO_BOLD_UNDERLINE_REVERSE
		&& r->PixColors[Color_fg] == r->PixColors[Color_BD]
# endif				/* ! NO_BOLD_UNDERLINE_REVERSE */
		/* if we wanted boldFont to have precedence */
# if 0				/* ifndef NO_BOLDFONT */
		&& r->TermWin.bfont == NULL
# endif				/* NO_BOLDFONT */
		)
		r->h->colorfgbg = SET_FGCOLOR(r->h->colorfgbg, i);
	if (r->PixColors[Color_bg] == r->PixColors[i])
		r->h->colorfgbg = SET_BGCOLOR(r->h->colorfgbg, i);
	}
#endif	/* NO_BRIGHTCOLOR */
}

/*----------------------------------------------------------------------*/
/*
 * Colour determination for low colour displays, routine from
 *	 Hans de Goede <hans@highrise.nl>
 */


#ifdef XFT_SUPPORT
/* EXTPROTO */
int
rxvt_alloc_xft_color (rxvt_t* r, unsigned long pixel, XftColor* xftcolor)       {
	XColor          xcol;
	XRenderColor    render; 

	assert (xftcolor);

	xcol.pixel = pixel;
	if (!XQueryColor (r->Xdisplay, XCMAP, &xcol))
		return 0;

	render.red   = xcol.red;
	render.green = xcol.green;
	render.blue  = xcol.blue;
	render.alpha = 0xFFFF; 
	return (XftColorAllocValue (r->Xdisplay, XVISUAL, XCMAP, &render, xftcolor));
}
#endif  /* XFT_SUPPORT */


/* EXTPROTO */
int
rxvt_parse_alloc_color(rxvt_t* r, XColor *screen_in_out, const char *colour)
{
	int				res = 0;

	if (!XParseColor(r->Xdisplay, XCMAP, colour, screen_in_out))
		rxvt_print_error("can't determine colour: %s", colour);
	else
		res = rxvt_alloc_color(r, screen_in_out, colour);
	return res;
}


/* EXTPROTO */
int
rxvt_alloc_color(rxvt_t* r, XColor *screen_in_out, const char *colour)
{
	int				res;

	if ((res = XAllocColor(r->Xdisplay, XCMAP, screen_in_out)))
		return res;

	/* try again with closest match */
	if (XDEPTH >= 4 && XDEPTH <= 8) {
		int				i, numcol;
		int				best_pixel = 0;
		unsigned long   best_diff, diff;
		XColor			*colors;

#define rSQR(x)		((x)*(x))

		best_diff = 0;
		numcol = 0x01 << XDEPTH;
		if ((colors = rxvt_malloc(numcol * sizeof(XColor)))) {
			for (i = 0; i < numcol; i++)
			colors[i].pixel = i;

			XQueryColors(r->Xdisplay, XCMAP, colors, numcol);
			for (i = 0; i < numcol; i++) {
				diff = rSQR(screen_in_out->red - colors[i].red)
					+ rSQR(screen_in_out->green - colors[i].green)
					+ rSQR(screen_in_out->blue - colors[i].blue);
				if (i == 0 || diff < best_diff) {
					best_pixel = colors[i].pixel;
					best_diff = diff;
				}
			}
			*screen_in_out = colors[best_pixel];
			free(colors);
			res = XAllocColor(r->Xdisplay, XCMAP, screen_in_out);
		}
	}
	if (res == 0)
		rxvt_print_error("can't allocate colour: %s", colour);

	return res;
}



/* -------------------------------------------------------------------- *
 * -						X INPUT METHOD ROUTINES						- *
 * -------------------------------------------------------------------- */
#ifdef USE_XIM
/* INTPROTO */
void
rxvt_IM_set_size(rxvt_t* r, XRectangle *size)
{
	size->x = r->TermWin.int_bwidth;
	size->y = r->TermWin.int_bwidth;
	size->width = Width2Pixel(r->TermWin.ncol);
	size->height = Height2Pixel(r->TermWin.nrow);
}

/* INTPROTO */
void
rxvt_IM_set_color(rxvt_t* r, unsigned long *fg, unsigned long *bg)
{
	*fg = r->PixColors[Color_fg];
	*bg = r->PixColors[Color_bg];
}

/* Checking whether input method is running. */
/* INTPROTO */
Bool
rxvt_IM_is_running(rxvt_t *r)
{
	char				*p;
	Atom			atom;
	Window			win;
	char			server[IMBUFSIZ];

	/* get current locale modifier */
	DBG_MSG(2, (stderr, "rxvt_IM_is_running ()\n"));
	if ((p = XSetLocaleModifiers(NULL)) != NULL) {
		STRCPY(server, "@server=");
		STRNCAT(server, &(p[4]), IMBUFSIZ - 9);	/* skip "@im=" */
		if ((p = STRCHR(server + 1, '@')) != NULL)	/* first one only */
			*p = '\0';

		atom = XInternAtom(r->Xdisplay, server, False);
		win = XGetSelectionOwner (r->Xdisplay, atom);
		if (win != None)
			return True;
	}
	return False;
}


/* EXTPROTO */
void
rxvt_IM_send_spot (rxvt_t *r)
{
	XPoint			spot;
	XVaNestedList   preedit_attr;

	if (r->h->Input_Context == NULL ||
		!r->TermWin.focus ||
		!(r->h->input_style & XIMPreeditPosition) ||
		!(r->h->event_type == KeyPress ||
		r->h->event_type == Expose ||
		r->h->event_type == NoExpose ||
		r->h->event_type == SelectionNotify ||
		r->h->event_type == ButtonRelease ||
		r->h->event_type == FocusIn) ||
		!rxvt_IM_is_running(r))
		return;

	rxvt_setPosition(r, &spot);

	preedit_attr = XVaCreateNestedList(0, XNSpotLocation, &spot, NULL);
	XSetICValues(r->h->Input_Context, XNPreeditAttributes, preedit_attr, NULL);
	XFree(preedit_attr);
}


/* EXTPROTO */
void
rxvt_IM_set_fontset (rxvt_t* r, int idx)
{
	char				*string;
	long			length;
	XFontSet		prev_fontset;
	int				success = 0;

	if (idx < 0 || idx >= MAX_NFONTS)
		return;
	DBG_MSG(1,(stderr, "rxvt_setTermFontSet()\n"));
	prev_fontset = r->TermWin.fontset;
	r->TermWin.fontset = NULL;

	length = 0;
	if (r->h->rs[Rs_font + idx])
		length += STRLEN(r->h->rs[Rs_font + idx]) + 1;
# ifdef MULTICHAR_SET
	if (r->h->rs[Rs_mfont + idx])
		length += STRLEN(r->h->rs[Rs_mfont + idx]) + 1;
# endif
	/* possible integer overflow? */
	assert (length >= 0 && length+1 > 0);
	if (length == 0 || (string = rxvt_malloc(length + 1)) == NULL)
		r->TermWin.fontset = NULL;
	else {
		int				missing_charsetcount;
		char			**missing_charsetlist, *def_string;

		string[0] = '\0';
		if (r->h->rs[Rs_font + idx]) {
			STRCAT(string, r->h->rs[Rs_font + idx]);
			STRCAT(string, ",");
		}
# ifdef MULTICHAR_SET
		if (r->h->rs[Rs_mfont + idx]) {
			STRCAT(string, r->h->rs[Rs_mfont + idx]);
			STRCAT(string, ",");
		}
# endif
		string[STRLEN(string) - 1] = '\0';
		r->TermWin.fontset = XCreateFontSet(r->Xdisplay, string,
						&missing_charsetlist,
						&missing_charsetcount,
						&def_string);
		free(string);
		if (r->TermWin.fontset != NULL)
			success = 1;
	}

	if (success) {
		if (prev_fontset != NULL)
				XFreeFontSet(r->Xdisplay, prev_fontset);
	}
	else
		r->TermWin.fontset = prev_fontset;
}

/* INTPROTO */
void
rxvt_IM_set_preedit_area(rxvt_t* r, XRectangle *preedit_rect, XRectangle *status_rect, XRectangle *needed_rect)
{
	int				mbh = 0, vtx = 0;

#ifdef HAVE_SCROLLBARS
	if (!(r->Options & Opt_scrollBar_right))
		vtx += rxvt_scrollbar_width(r);
#endif

#ifdef HAVE_MENUBAR
	mbh += rxvt_menubar_height(r);
#endif

	mbh += rxvt_tabbar_height(r);
#ifndef NO_LINESPACE
	mbh -= r->TermWin.lineSpace;
#endif

	preedit_rect->x = needed_rect->width + vtx;
	preedit_rect->y = Height2Pixel(r->TermWin.nrow - 1) + mbh;

	preedit_rect->width = Width2Pixel(r->TermWin.ncol + 1) - needed_rect->width
					+ vtx;
	preedit_rect->height = Height2Pixel(1);

	status_rect->x = vtx;
	status_rect->y = Height2Pixel(r->TermWin.nrow - 1) + mbh;

	status_rect->width = needed_rect->width ? needed_rect->width
						: Width2Pixel(r->TermWin.ncol + 1);
	status_rect->height = Height2Pixel(1);
}

/* ARGSUSED */
/* INTPROTO */
void
rxvt_IM_destroy_callback(XIM xim __attribute__((unused)), XPointer client_data __attribute__((unused)), XPointer call_data __attribute__((unused)))
{
	rxvt_t			*r = rxvt_get_r();

	r->h->Input_Context = NULL;
	/* To avoid Segmentation Fault in C locale: Solaris only? */
	if (STRCMP(r->h->locale, "C"))
		XRegisterIMInstantiateCallback(r->Xdisplay, NULL, NULL, NULL,
			rxvt_IM_init_callback, NULL);
}

/*
 * X manual pages and include files don't match on some systems:
 * some think this is an XIDProc and others an XIMProc so we can't
 * use the first argument - need to update this to be nice for
 * both types via some sort of configure detection
 */
/* ARGSUSED */
/* EXTPROTO */
void
rxvt_IM_init_callback (Display *unused __attribute__((unused)), XPointer client_data __attribute__((unused)), XPointer call_data __attribute__((unused)))
{
	int				i, found, had_im;
	const char	 *p;
	char			**s;
	rxvt_t			*r = rxvt_get_r();
	char			buf[IMBUFSIZ];

	DBG_MSG(1,(stderr, "rxvt_IMInstantiateCallback()\n"));
	if (r->h->Input_Context)
		return;

	found = had_im = 0;
	p = r->h->rs[Rs_inputMethod];
	if (p && *p) {
		had_im = 1;
		s = rxvt_splitcommastring(p);
		for (i = 0; s[i]; i++) {
			if (*s[i]) {
				STRCPY(buf, "@im=");
				STRNCAT(buf, s[i], IMBUFSIZ - 5);
				if ((p = XSetLocaleModifiers(buf)) != NULL &&
					*p &&
					(rxvt_IM_get_IC(r) == True)) {
					found = 1;
					break;
				}
			}
		}
		for (i = 0; s[i]; i++)
			free(s[i]);
		free(s);
	}
	if (found)
		return;

	/* try with XMODIFIERS env. var. */
	if ((p = XSetLocaleModifiers("")) != NULL && *p) {
		rxvt_IM_get_IC(r);
		return;
	}

	/* try with no modifiers base IF the user didn't specify an IM */
	if (!had_im &&
		(p = XSetLocaleModifiers("@im=none")) != NULL &&
		*p &&
		rxvt_IM_get_IC(r) == True)
		return;
}

/*
 * Try to open a XIM with the current modifiers, then see if we can
 * open a suitable preedit type
 */
/* INTPROTO */
Bool
rxvt_IM_get_IC(rxvt_t *r)
{
	int				i, j, found;
	XIM				xim;
	XPoint			spot;
	XRectangle		rect, status_rect, needed_rect;
	unsigned long   fg, bg;
	const char	 *p;
	char			**s;
	XIMStyles		*xim_styles;
	XVaNestedList   preedit_attr, status_attr;
	XIMCallback	 ximcallback;
	struct rxvt_hidden *h = r->h;

	DBG_MSG(1,(stderr, "rxvt_IM_get_IC()\n"));
	xim = XOpenIM(r->Xdisplay, NULL, NULL, NULL);
	if (xim == NULL)	{
		DBG_MSG(1,(stderr, "Unalbe to open IM\n"));
		return False;
	}

	xim_styles = NULL;
	if (XGetIMValues(xim, XNQueryInputStyle, &xim_styles, NULL) ||
		!xim_styles || !xim_styles->count_styles) {
		XCloseIM(xim);
		return False;
	}

	p = h->rs[Rs_preeditType] ? h->rs[Rs_preeditType]
					: "OverTheSpot,OffTheSpot,Root";
	s = rxvt_splitcommastring(p);
	for (i = found = 0; !found && s[i]; i++) {
		if (!STRCMP(s[i], "OverTheSpot"))
			h->input_style = (XIMPreeditPosition | XIMStatusNothing);
		else if (!STRCMP(s[i], "OffTheSpot"))
			h->input_style = (XIMPreeditArea | XIMStatusArea);
		else if (!STRCMP(s[i], "Root"))
			h->input_style = (XIMPreeditNothing | XIMStatusNothing);

		for (j = 0; j < xim_styles->count_styles; j++)
			if (h->input_style == xim_styles->supported_styles[j]) {
				found = 1;
				break;
			}
	}
	for (i = 0; s[i]; i++)
		free(s[i]);
	free(s);
	XFree(xim_styles);

	if (!found) {
		XCloseIM(xim);
		return False;
	}

	ximcallback.callback = rxvt_IM_destroy_callback;

	/* XXX: not sure why we need this (as well as IC one below) */
	XSetIMValues(xim, XNDestroyCallback, &ximcallback, NULL);

	preedit_attr = status_attr = NULL;

	if (h->input_style & XIMPreeditPosition) {
		rxvt_IM_set_size(r, &rect);
		rxvt_setPosition(r, &spot);
		rxvt_IM_set_color(r, &fg, &bg);

		preedit_attr = XVaCreateNestedList(0, XNArea, &rect,
							XNSpotLocation, &spot,
							XNForeground, fg,
							XNBackground, bg,
							XNFontSet, r->TermWin.fontset,
							NULL);
	}
	else if (h->input_style & XIMPreeditArea) {
		rxvt_IM_set_color(r, &fg, &bg);

		/*
		 * The necessary width of preedit area is unknown
		 * until create input context.
		 */
		needed_rect.width = 0;

		rxvt_IM_set_preedit_area(r, &rect, &status_rect, &needed_rect);

		preedit_attr = XVaCreateNestedList(0, XNArea, &rect,
							XNForeground, fg,
							XNBackground, bg,
							XNFontSet, r->TermWin.fontset,
							NULL);
		status_attr = XVaCreateNestedList(0, XNArea, &status_rect,
						XNForeground, fg,
						XNBackground, bg,
						XNFontSet, r->TermWin.fontset, NULL);
	}
	h->Input_Context = XCreateIC(xim, XNInputStyle, h->input_style,
					XNClientWindow, r->TermWin.parent,
					XNFocusWindow, r->TermWin.parent,
					XNDestroyCallback, &ximcallback,
					preedit_attr ? XNPreeditAttributes : NULL,
					preedit_attr,
					status_attr ? XNStatusAttributes : NULL,
					status_attr, NULL);
	if (preedit_attr)
		XFree(preedit_attr);
	if (status_attr)
		XFree(status_attr);
	if (h->Input_Context == NULL) {
		rxvt_print_error("failed to create input context");
		XCloseIM(xim);
		return False;
	}
	if (h->input_style & XIMPreeditArea)
		rxvt_IM_set_status_pos (r);
	DBG_MSG(1,(stderr, "rxvt_IM_get_IC() - successful connection\n"));
	return True;
}


/* EXTPROTO */
void
rxvt_IM_set_status_pos (rxvt_t *r)
{
	XRectangle		preedit_rect, status_rect, *needed_rect;
	XVaNestedList   preedit_attr, status_attr;

	if (r->h->Input_Context == NULL ||
		!r->TermWin.focus ||
		!(r->h->input_style & XIMPreeditArea) ||
		!rxvt_IM_is_running(r))
	return;

	/* Getting the necessary width of preedit area */
	status_attr = XVaCreateNestedList(0, XNAreaNeeded, &needed_rect, NULL);
	XGetICValues(r->h->Input_Context, XNStatusAttributes, status_attr, NULL);
	XFree(status_attr);

	rxvt_IM_set_preedit_area(r, &preedit_rect, &status_rect, needed_rect);

	preedit_attr = XVaCreateNestedList(0, XNArea, &preedit_rect, NULL);
	status_attr = XVaCreateNestedList(0, XNArea, &status_rect, NULL);

	XSetICValues(r->h->Input_Context,
			XNPreeditAttributes, preedit_attr,
			XNStatusAttributes, status_attr, NULL);

	XFree(preedit_attr);
	XFree(status_attr);
}
#endif				/* USE_XIM */

/*----------------------------------------------------------------------*/
static rxvt_t  *_rxvt_vars = NULL;

/* EXTPROTO */
rxvt_t*
rxvt_get_r(void)
{
	return _rxvt_vars;
}
/* INTPROTO */
void
rxvt_set_r(rxvt_t *r)
{
	_rxvt_vars = r;
}

/*----------------------- end-of-file (C source) -----------------------*/
