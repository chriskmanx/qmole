/*--------------------------------*-C-*---------------------------------*
 * File:	ptytty.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1999-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
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
** $Id: ptytty.c,v 1.13 2004/12/25 21:08:57 cvs Exp $
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


/* ------------------------------------------------------------------------- *
 *                  GET PSEUDO TELETYPE - MASTER AND SLAVE                   *
 * ------------------------------------------------------------------------- */
/*
 * Returns pty file descriptor, or -1 on failure 
 * If successful, ttydev is set to the name of the slave device.
 * fd_tty _may_ also be set to an open fd to the slave device
 */
/* EXTPROTO */
int
rxvt_get_pty(int *fd_tty, char **ttydev)
{
    int		pfd;

	*ttydev = NULL;

#ifdef PTYS_ARE_OPENPTY
	{
		char	tty_name[sizeof "/dev/pts/????\0"];

		if (openpty(&pfd, fd_tty, tty_name, NULL, NULL) != -1) {
			*ttydev = STRDUP(tty_name);
			return pfd;
		}
	}
#endif

#ifdef PTYS_ARE__GETPTY
	{
		char*	ptr = _getpty(&pfd, O_RDWR|O_NDELAY|O_NOCTTY, 0622, 0);
		if (ptr)
			*ttydev = STRDUP(ptr);
	    if (ptr)
			return pfd;
	}
#endif

#ifdef PTYS_ARE_GETPTY
	{
		char*	ptydev;

		while ((ptydev = getpty()) != NULL)
			if ((pfd = open(ptydev, O_RDWR | O_NOCTTY, 0)) >= 0) {
				*ttydev = STRDUP(ptydev);
				return pfd;
			}
	}
#endif

#if defined(HAVE_GRANTPT) && defined(HAVE_UNLOCKPT)
# if defined(PTYS_ARE_GETPT) || defined(PTYS_ARE_PTMX)
    {
		extern char    *ptsname();

#  ifdef PTYS_ARE_GETPT
		extern int		getpt ();
		pfd = getpt();
#  else
		pfd = open("/dev/ptmx", O_RDWR | O_NOCTTY, 0);
#  endif
		if (pfd >= 0) {
			extern int grantpt (int);
			extern int unlockpt (int);
		    if (grantpt(pfd) == 0 &&	/* change slave permissions */
				unlockpt(pfd) == 0) {	/* slave now unlocked */
				char*	ptr = ptsname(pfd);	/* get slave's name */
				if (ptr)
					*ttydev = STRDUP(ptr);
				return pfd;
		    }
			close(pfd);
		}
    }
# endif
#endif

#ifdef PTYS_ARE_PTC
    if ((pfd = open("/dev/ptc", O_RDWR | O_NOCTTY, 0)) >= 0) {
		char*	ptr = ttyname(pfd);
		if (ptr)
			*ttydev = STRDUP(ptr);
		return pfd;
    }
#endif

#ifdef PTYS_ARE_CLONE
    if ((pfd = open("/dev/ptym/clone", O_RDWR | O_NOCTTY, 0)) >= 0) {
		char*	ptr = ptsname(pfd);
		if (ptr)
			*ttydev = STRDUP(ptr);
		return pfd;
    }
#endif

#ifdef PTYS_ARE_NUMERIC
    {
		int		idx;
		char*	c1;
		char*	c2;
		char	pty_name[] = "/dev/ptyp???";
		char	tty_name[] = "/dev/ttyp???";

		c1 = &(pty_name[sizeof(pty_name) - 4]);
		c2 = &(tty_name[sizeof(tty_name) - 4]);
		for (idx = 0; idx < 256; idx++) {
		    sprintf(c1, "%d", idx);
		    sprintf(c2, "%d", idx);
		    if (access(tty_name, F_OK) < 0) {
				idx = 256;
				break;
		    }
		    if ((pfd = open(pty_name, O_RDWR | O_NOCTTY, 0)) >= 0) {
				if (access(tty_name, R_OK | W_OK) == 0) {
				    *ttydev = STRDUP(tty_name);
				    return pfd;
				}
				close(pfd);
		    }
		}
    }
#endif
#ifdef PTYS_ARE_SEARCHED
    {
		const char*	c1;
		const char*	c2;
		char		pty_name[] = "/dev/pty??";
		char		tty_name[] = "/dev/tty??";

# ifndef PTYCHAR1
#  define PTYCHAR1	"pqrstuvwxyz"
# endif
# ifndef PTYCHAR2
#  define PTYCHAR2	"0123456789abcdef"
# endif
		for (c1 = PTYCHAR1; *c1; c1++) {
		    pty_name[(sizeof(pty_name) - 3)] =
		        tty_name[(sizeof(pty_name) - 3)] = *c1;
		    for (c2 = PTYCHAR2; *c2; c2++) {
				pty_name[(sizeof(pty_name) - 2)] =
				    tty_name[(sizeof(pty_name) - 2)] = *c2;
				if ((pfd = open(pty_name, O_RDWR | O_NOCTTY, 0)) >= 0) {
				    if (access(tty_name, R_OK | W_OK) == 0) {
						*ttydev = STRDUP(tty_name);
						return pfd;
				    }
				    close(pfd);
				}
		    }
		}
    }
#endif
    return -1;
}

/*----------------------------------------------------------------------*/
/*
 * Returns tty file descriptor, or -1 on failure 
 */
/* EXTPROTO */
int
rxvt_get_tty(const char *ttydev)
{
    return open(ttydev, O_RDWR | O_NOCTTY, 0);
}

/*----------------------------------------------------------------------*/
/*
 * Make our tty a controlling tty so that /dev/tty points to us
 */
/* EXTPROTO */
int
rxvt_control_tty(int fd_tty, const char *ttydev)
{
#ifndef __QNX__
    int             fd;

    DBG_MSG(1,(stderr, "rxvt_control_tty(): pid: %d, tty fd: %d, dev: %s\n", (int) getpid(), fd_tty, ttydev));
/* ---------------------------------------- */
# ifdef HAVE_SETSID
    setsid();
# endif
# if defined(HAVE_SETPGID)
    setpgid(0, 0);
# elif defined(HAVE_SETPGRP)
    setpgrp(0, 0);
# endif
/* ---------------------------------------- */
# ifdef TIOCNOTTY
    fd = open("/dev/tty", O_RDWR | O_NOCTTY);
    DBG_MSG(1,(stderr, "rxvt_control_tty(): Voiding tty associations: previous=%s\n", fd < 0 ? "no" : "yes"));
    if (fd >= 0) {
		ioctl(fd, TIOCNOTTY, NULL);	/* void tty associations */
		close(fd);
    }
# endif
/* ---------------------------------------- */
    fd = open("/dev/tty", O_RDWR | O_NOCTTY);
    DBG_MSG(1,(stderr, "rxvt_control_tty(): /dev/tty has controlling tty? %s\n", fd < 0 ? "no (good)" : "yes (bad)"));
    if (fd >= 0)
		close(fd);		/* ouch: still have controlling tty */
/* ---------------------------------------- */
#if defined(PTYS_ARE_PTMX) && defined(I_PUSH)
/*
 * Push STREAMS modules:
 *    ptem: pseudo-terminal hardware emulation module.
 *    ldterm: standard terminal line discipline.
 *    ttcompat: V7, 4BSD and XENIX STREAMS compatibility module.
 *
 * After we push the STREAMS modules, the first open() on the slave side
 * (i.e. the next section between the dashes giving us "tty opened OK")
 * should make the "ptem" (or "ldterm" depending upon either which OS
 * version or which set of manual pages you have) module give us a
 * controlling terminal.  We must already have close()d the master side
 * fd in this child process before we push STREAMS modules on because the
 * documentation is really unclear about whether it is any close() on
 * the master side or the last close() - i.e. a proper STREAMS dismantling
 * close() - on the master side which causes a hang up to be sent
 * through - Geoff Wing
 */
# ifdef HAVE_ISASTREAM
    if (isastream(fd_tty) == 1)
# endif
    {
		DBG_MSG(1,(stderr, "rxvt_control_tty(): Pushing STREAMS modules\n"));
		ioctl(fd_tty, I_PUSH, "ptem");
		ioctl(fd_tty, I_PUSH, "ldterm");
		ioctl(fd_tty, I_PUSH, "ttcompat");
    }
#endif
/* ---------------------------------------- */
# if defined(TIOCSCTTY)
    fd = ioctl(fd_tty, TIOCSCTTY, NULL);
    DBG_MSG(1,(stderr, "rxvt_control_tty(): ioctl(..,TIOCSCTTY): %d\n", fd));
# elif defined(TIOCSETCTTY)
    fd = ioctl(fd_tty, TIOCSETCTTY, NULL);
    DBG_MSG(1,(stderr, "rxvt_control_tty(): ioctl(..,TIOCSETCTTY): %d\n", fd));
# else
    fd = open(ttydev, O_RDWR);
    DBG_MSG(1,(stderr, "rxvt_control_tty(): tty open%s\n", fd < 0 ? " failure" : "ed OK"));
    if (fd >= 0)
	close(fd);
# endif
/* ---------------------------------------- */
    fd = open("/dev/tty", O_WRONLY);
    DBG_MSG(1,(stderr, "rxvt_control_tty(): do we have controlling tty now: %s\n", fd < 0 ? "no (fatal)" : "yes (good)"));
    if (fd < 0)
		return -1;		/* fatal */
    close(fd);
/* ---------------------------------------- */
    DBG_MSG(1,(stderr, "rxvt_control_tty(): tcgetpgrp(): %d  getpgrp(): %d\n", (int) tcgetpgrp(fd_tty), (int) getpgrp()));
/* ---------------------------------------- */
#endif				/* ! __QNX__ */
    return 0;
}
/*----------------------- end-of-file (C source) -----------------------*/
