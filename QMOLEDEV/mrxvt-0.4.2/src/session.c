/*--------------------------------*-C-*---------------------------------*
 * File:	session.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1999         Felix Bellaby <felix@pooh.u-net.com>
 * Copyright (c) 2004         Jingmin Zhou <jimmyzhou@users.sourceforge.net>
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
** $Id: session.c,v 1.18 2005/01/31 00:12:37 cvs Exp $
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


#ifdef HAVE_X11_SM_SMLIB_H

#define GnomePriority	"_GSM_Priority"


/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
static void callback_die                (SmcConn, SmPointer);
static void fetch_window_position       (rxvt_t*, int*, int*);
static void callback_save_yourself      (SmcConn, SmPointer, int, Bool, int, Bool);
static void callback_shutdown_cancelled (SmcConn, SmPointer);
static void callback_save_complete      (SmcConn, SmPointer);
static void ice_io_error_handler        (IceConn);
static void ice_connection_watch        (IceConn, IcePointer, Bool, IcePointer*);
/*--------------------------------------------------------------------*
 *         END `INTERNAL' ROUTINE PROTOTYPES                          *
 *--------------------------------------------------------------------*/

/* INTPROTO */
static void
callback_die (SmcConn smc_conn, SmPointer client_data)
{
	rxvt_t*		r = rxvt_get_r ();

	DBG_MSG(1, (stderr, "SessionMgr: received die\n"));

	if (NULL != smc_conn)	{
		SmcCloseConnection (smc_conn, 0, NULL);
		r->TermWin.sm_conn = NULL;
	}
}


/* INTPROTO */
static void
fetch_window_position (rxvt_t* r, int* px, int* py)
{
	int				k;
	unsigned int	n;
	Window			wintree[PARENT_NUMBER];
	Window			root;
	Window*			list;
	XWindowAttributes	attr;

	assert (NULL != px);
	assert (NULL != py);
	/* set default position to r->TermWin.parent's position */
	*px = r->szHint.x;
	*py = r->szHint.y;

	/*
	** look for parent tree of top level window because the window
	** manager may embed our window into some frame windows
	*/
	wintree[0] = r->TermWin.parent;
	for (k = 1; k < PARENT_NUMBER; k++)	{
		XQueryTree (r->Xdisplay, wintree[k-1], &root,
			&(wintree[k]), &list, &n);
		XFree (list);
		if (wintree[k] == XROOT)
			break;
	}

	if (PARENT_NUMBER == k || 1 == k)
		return ;
	k --;
	XGetWindowAttributes (r->Xdisplay, wintree[k], &attr);
	*px = attr.x;
	*py = attr.y;
}


/* INTPROTO */
static void
callback_save_yourself (SmcConn smc_conn, SmPointer client_data, int save_style, Bool shutdown, int interact_style, Bool fast)
{
	rxvt_t*		r = rxvt_get_r ();

	DBG_MSG(1, (stderr, "SessionMgr: received save_yourself\n"));

	if (NULL != smc_conn)	{
		struct {
			SmPropValue program[1];
			SmPropValue user[1];
			SmPropValue hint[1];
			SmPropValue priority[1];
			SmPropValue restart[32];
		} vals;

		SmProp prop[] = {
			{SmProgram,          SmARRAY8,       1, vals.program },
			{SmUserID,           SmARRAY8,       1, vals.user    },
			{SmRestartStyleHint, SmCARD8,        1, vals.hint    },
			{GnomePriority,      SmCARD8,        1, vals.priority},
			{SmCloneCommand,     SmLISTofARRAY8, 0, vals.restart },
			{SmRestartCommand,   SmLISTofARRAY8, 0, vals.restart },
		};

		SmProp*	props[] = { 
			&prop[0], 
			&prop[1], 
			&prop[2],
			&prop[3], 
			&prop[4],
			&prop[5],
		};

		char			priority = 5;
		char			restart_style = SmRestartIfRunning;
		struct passwd*	pw = NULL;
		int				n = 0, i;
		char			tnum[32];
		char			desktop[32];
		char			geometry[128];
		int				x, y;
		char			posx[32], posy[32];


		vals.program->value  = (r->global_argv)[0];
		vals.program->length = STRLEN(vals.program->value);

#ifdef HAVE_GETPWUID
		pw = getpwuid (getuid());
#endif
		vals.user->value  = pw ? pw->pw_name : "";
		vals.user->length = STRLEN(vals.user->value);

		vals.hint->value  = &restart_style;
		vals.hint->length = 1;

		vals.priority->value  = &priority;
		vals.priority->length = 1;

		sprintf (tnum, "%d", LTAB(r)+1); /* start from 0! */
		sprintf (desktop, "%d", (int) rxvt_get_desktop (r));
		sprintf (geometry, "%dx%d", r->TermWin.ncol, r->TermWin.nrow);
		fetch_window_position (r, &x, &y);
		sprintf (posx, (x >= 0) ? "+%d":"%d", x);
		sprintf (posy, (y >= 0) ? "+%d":"%d", y);
		STRCAT (geometry, posx);
		STRCAT (geometry, posy);

		vals.restart[n++].value = (r->global_argv)[0];
		vals.restart[n++].value = "-tnum";
		vals.restart[n++].value = tnum;
		vals.restart[n++].value = "-desktop";
		vals.restart[n++].value = desktop;
		vals.restart[n++].value = "-geometry";
		vals.restart[n++].value = geometry;
#ifdef MULTICHAR_SET
		vals.restart[n++].value = "-km";
		vals.restart[n++].value = rxvt_encoding_name (r);
#endif

		prop[4].num_vals = n;

		vals.restart[n++].value = "-sm";
		vals.restart[n++].value = "-sid";
		vals.restart[n++].value = r->TermWin.sm_client_id;

		prop[5].num_vals = n;

		for (i = 0; i < n; i++)
			vals.restart[i].length = STRLEN(vals.restart[i].value);

		SmcSetProperties(smc_conn, sizeof(props)/sizeof(SmProp*), props);
		SmcSaveYourselfDone (smc_conn, 1);
	}
}


/* INTPROTO */
static void 
callback_shutdown_cancelled (SmcConn smc_conn, SmPointer client_data)
{
	DBG_MSG(1, (stderr, "SessionMgr: received shutdown_cancelled\n"));
	/* We are not really interested in this message. */
}


/* INTPROTO */
static void 
callback_save_complete (SmcConn smc_conn, SmPointer client_data)
{
	DBG_MSG(1, (stderr, "SessionMgr: received save_complete\n"));
	/* We are not really interested in this message. */
}


/* INTPROTO */
static void 
ice_io_error_handler (IceConn connection)
{
	DBG_MSG(1, (stderr, "SessionMgr: received ice io_error\n"));
	/* The less we do here the better - the default handler does an
		exit(1) instead of closing the losing connection. */
}    


/* INTPROTO */
static void 
ice_connection_watch (IceConn connection, IcePointer client_data, Bool opening, IcePointer* watch_data)
{
	rxvt_t*		r = rxvt_get_r ();

	if (opening)	{
		DBG_MSG(1, (stderr, "SessionMgr: new ice connection\n"));
		r->TermWin.ice_conn = connection;
		r->TermWin.ice_fd = IceConnectionNumber(connection);

		/* Make sure ice_fd is not passed to child */
		if (-1 != r->TermWin.ice_fd)
			fcntl(r->TermWin.ice_fd, F_SETFD, FD_CLOEXEC);
	}
	else	{
		DBG_MSG(1, (stderr, "SessionMgr: close ice connection\n"));
		r->TermWin.ice_conn = NULL;
		r->TermWin.ice_fd = -1;
	}
}


/* EXTPROTO */
void 
rxvt_process_ice_msgs (rxvt_t* r)
{
	IceProcessMessagesStatus status;

	DBG_MSG(1, (stderr, "SessionMgr: received ice msgs\n"));
	assert (NULL != r->TermWin.ice_conn);
	assert (NULL != r->TermWin.sm_conn);
	status = IceProcessMessages(r->TermWin.ice_conn, NULL, NULL);

	if (status == IceProcessMessagesIOError) {
		DBG_MSG(1, (stderr, "SessionMgr: ICE IO error\n"));

		IceSetShutdownNegotiation (r->TermWin.ice_conn, False);
		IceCloseConnection (r->TermWin.ice_conn);
    }
}


/* EXTPROTO */
void 
rxvt_session_init (rxvt_t* r)
{
	SmcCallbacks        callbacks;
	char				error_string_ret[4096] = "";
	char*				client_id = NULL; 
	char*				prev_client_id; 


	DBG_MSG(1, (stderr, "SessionMgr: init\n"));

	if (NULL != r->TermWin.sm_conn)	{
		DBG_MSG(1, (stderr, "SessionMgr: duplicate session init\n"));
		return ;
	}

	if (NULL == getenv("SESSION_MANAGER")) {
		DBG_MSG(1, (stderr, "SessionMgr: session manager is not running\n"));
		return ;
	}

	/* Initialize ice handler */
	IceSetIOErrorHandler (ice_io_error_handler);
	IceAddConnectionWatch (ice_connection_watch, NULL);

	/* Initialize callbacks */
	callbacks.save_yourself.callback = callback_save_yourself;
	callbacks.die.callback = callback_die;
	callbacks.save_complete.callback = callback_save_complete;
	callbacks.shutdown_cancelled.callback = callback_shutdown_cancelled;
	callbacks.save_yourself.client_data =
		callbacks.die.client_data =
		callbacks.save_complete.client_data =
		callbacks.shutdown_cancelled.client_data = (SmPointer) NULL;

	/* previous client id must be NULL if it is not specified */
	prev_client_id = (char*) r->h->rs[Rs_smClientID];

	r->TermWin.sm_conn = SmcOpenConnection(NULL, NULL,
					SmProtoMajor, SmProtoMinor,
					SmcSaveYourselfProcMask | SmcSaveCompleteProcMask |
					SmcDieProcMask | SmcShutdownCancelledProcMask,
					&callbacks, prev_client_id, &client_id,
					sizeof(error_string_ret), error_string_ret);

	if (NULL == r->TermWin.sm_conn)	{
		DBG_MSG(1, (stderr, "SessionMgr: connection failed with error %s\n", error_string_ret));
		return ;
	}

	if (NULL == client_id)	{
		/* well, prev_client_id shouldn't be NULL */
		r->TermWin.sm_client_id = STRDUP (prev_client_id);
	}
	else {
		r->TermWin.sm_client_id = STRDUP (client_id);
		free (client_id);
	}
}


/* EXTPROTO */
void 
rxvt_session_exit (rxvt_t* r)
{
	DBG_MSG(1, (stderr, "SessionMgr: exit\n"));

	if (NULL == r->TermWin.sm_conn)
		return ;

	SmcCloseConnection (r->TermWin.sm_conn, 0, NULL);
	r->TermWin.sm_conn  = NULL;
}

#endif /* HAVE_X11_SM_SMLIB_H */ 

/*----------------------- end-of-file (C source) -----------------------*/
