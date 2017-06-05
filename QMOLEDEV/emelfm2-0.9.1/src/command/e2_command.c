/* $Id: e2_command.c 2953 2013-11-18 10:38:34Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/command/e2_command.c
@brief functions for handling e2 commands
*/
/**
\page commline the command line

ToDo

\section alias command aliases

ToDo
*/
/**
\page commands executing external commands

ToDo - description of how this works

\section alias alias operation

ToDo
*/


#include "e2_command.h"
#include <string.h>
#include <sys/wait.h>
#include <signal.h>
#include <pwd.h>
#include <pthread.h>
#include "e2_task.h"
#include "e2_filelist.h"
#include "e2_alias.h"
#include "e2_fs.h"

#define CMDNAMELEN 30

extern pthread_mutex_t task_mutex;
static gint waitflags;	//waitpid() argument, tailored for user's system
static gint block_count;	//refcount for [un]blocking child signals
const gchar *shellcmd;	//the command interpreter to use for external shell
//list of utf8 strings, each like "name-string=value-string", (not quoted), no whitespace surrounding "="
static GList *variables;

#ifdef E2_NEW_COMMAND
//static void _e2_command_sigpoll_handler (gint num, siginfo_t *info, void *context);
//static gpointer _e2_command_watch (E2_TaskRuntime *rt);
#else
static gboolean _e2_command_watch (GIOChannel *ioc, GIOCondition cond,
	E2_TaskRuntime *rt, gboolean error);
#endif
static void _e2_command_sigchld_handler (gint num, siginfo_t *info, void *context);

//CHECKME need SIGPIPE management ??

/**
@brief set handler for SIGCHLD signals
@return
*/
static void _e2_command_set_sigchild_handler (void)
{
	struct sigaction sigdata;
	sigdata.sa_sigaction = _e2_command_sigchld_handler;
	sigemptyset (&sigdata.sa_mask);
	sigdata.sa_flags = SA_RESTART | SA_SIGINFO; //SA_NOCLDSTOP not allowed;
	sigaction (SIGCHLD, &sigdata, NULL);
}
/**
@brief block incoming SIGCHLD signals
@return
*/
void e2_command_block_childsignal (void)
{
	if (++block_count == 1)
	{
		sigset_t set;	//, oldset;
		sigemptyset (&set);
		sigaddset (&set, SIGCHLD);
		sigprocmask (SIG_BLOCK, &set, NULL);	//&oldset);
	}
}
/**
@brief unblock incoming SIGCHLD signals
@return
*/
void e2_command_unblock_childsignal (void)
{
	if (--block_count < 0)
	{
		printd (WARN, "child signal block refcount error");
		block_count = 0;
	}
	if (block_count == 0)
	{
		sigset_t set;	//, oldset;
		//re-install the signal handler (some systems need this?) WRONG
//		_e2_command_set_sigchild_handler ();
		//and unblock it
		sigemptyset (&set);
		sigaddset (&set, SIGCHLD);
		sigprocmask (SIG_UNBLOCK, &set, NULL);	//&oldset);
	}
}
/**
@brief handle SIGCHILD signals
Note that opened pipes etc will come here, even though they're not logged as
children
Remember that this may be called before all data from the child is flushed!
@param num the signal number (17)
@param info pointer to data struct with details about signal, or NULL
@param context UNUSED

@return
*/
static void _e2_command_sigchld_handler (gint num, siginfo_t *info, void *context)
{
	printd (DEBUG, "signal handler (signal num:%d)", num);
	pid_t endpid;
	gint exitstatus;

	if (info != NULL)
	{
		//normally, the child's data is provided in signal data
		endpid = info->si_pid;
//		exitstatus = info->si_status;
		waitpid (endpid, &exitstatus, waitflags);	//must reap
	}
	else
	{
rewait:
		//get stopped, restarted and finished child(ren), if any
		//(any error will abort this loop, ECHILD (10) is normal loop terminator)
		endpid = waitpid (-1, &exitstatus, waitflags);
		printd (DEBUG, "waitpid() for any children returned %d", endpid);
	}

	if (endpid > 0)
	{
		E2_TaskRuntime *rt = e2_task_find_running_task ((glong) endpid);
		if (!(rt == NULL || rt->action))
		{
			printd (DEBUG, "ending after waitpid returned %d", endpid);
			pthread_mutex_lock (&task_mutex);
			if (WIFEXITED (exitstatus))
			{
				rt->pid = -2L;	//prevent further matches for this rt
				rt->status = E2_TASK_COMPLETED;
				rt->ex.command.exit = WEXITSTATUS (exitstatus);
				printd (DEBUG, "signal handler detected '%s' ended normally with exit code %d",
					rt->ex.command.command, WEXITSTATUS (exitstatus));
				printd (DEBUG, "child status stored");
			}
			else if (WIFSTOPPED (exitstatus))
			{
# ifdef WCONTINUED	//linux >= 2.6.10
				rt->status = E2_TASK_PAUSED;
# endif
				printd (DEBUG, "continuing after %d stopped by signal %d",
					(gint)endpid, WSTOPSIG (exitstatus));
			}
			else if (WIFSIGNALED (exitstatus))
			{
				gint signal = WTERMSIG (exitstatus);
				//CHECKME which other signals are acceptable ?
				if (signal == SIGTTOU || signal == SIGTTIN)
				{
# ifdef WCONTINUED	//linux >= 2.6.10
					rt->status = E2_TASK_PAUSED;
# endif
					//CHECKME do a select() and print messages after SIGTTOU/SIGTTIN signal ?
					printd (DEBUG, "continuing after %d stopped by signal %d",
						(gint)endpid, signal);
				}
				else
				{
					rt->pid = -2L;	//prevent further matches for this rt
					rt->status = E2_TASK_INCOMPLETE;
					rt->ex.command.exit = signal;
					printd (DEBUG, "child-signal handler detected command '%s' terminated by signal %d (%s)",
						rt->ex.command.command, signal, g_strsignal (signal));
					printd (DEBUG, "child status (%d) stored", signal);
				}
			}
# ifdef WCONTINUED
			else if (WIFCONTINUED (exitstatus))
			{
				rt->status = E2_TASK_RUNNING;
				printd (DEBUG, "process %d restarted", endpid);
			}
# endif
			pthread_mutex_unlock (&task_mutex);
#ifdef E2_SU_ACTIONS
			if (!e2_task_revert_user (rt->flags))
				rt->status = E2_TASK_INCOMPLETE;
#endif
		}
#ifdef E2_SU_ACTIONS
		else if (rt != NULL && rt->action)
		{
			if (!e2_task_revert_user (rt->flags))
				rt->status = E2_TASK_INCOMPLETE;
		}
#endif
		else	//no matching running-command data (could be a sync command (old version))
		{
			printd (WARN, "received signal from unrecorded child process (%d)", (gint) endpid);
			//in this handler, current signal is blocked, pass it on for other handlers
			//kill (endpid, num);	//CHECKME
		}
		if (info == NULL && rt != NULL && !rt->action)
			goto rewait;
	} //end of endpid > 0
	else if (endpid < 0)
	{
		if (errno == EINTR)
		{
			printd (DEBUG, "re-checking after waitpid was interrupted");
			goto rewait;
		}
		else if (errno != ECHILD)
		{
			printd (DEBUG, "aborting after waitpid pid error %d (%s)", errno, g_strerror (errno));
			//FIXME find which task it is and record error exit
//			rt->status = E2_TASK_INCOMPLETE;
//			rt->ex.command.exit = errno;
		}
	}

	//re-install the signal handler (some systems need this?)
	_e2_command_set_sigchild_handler ();
}
/**
@brief print @a buffer contents
The printing is segmented into into \n-separated  or \r-separated lines, to
allow proper processing of \r and to prevent too-chunky scrolling.
The final line may or may not have a trailing '\n' or '\r'
@param buffer character-buffer pointer
@param error TRUE to print error stream
@param rt pointer to task data struct

@return
*/
static void _e2_command_display (gchar *buffer, gboolean error, E2_TaskRuntime *rt)
{
	gchar *line = buffer;
	while (TRUE)
	{
		gchar *rsep, *nsep;
		//always ascii \r, \n, don't need utf8 search
		//most text will be in conventional lines, try for those first
		nsep = strchr (line, '\n');
		if (nsep != NULL)
		{
			gchar nsave;
			nsep++;
			nsave = *nsep;
			*nsep = '\0';

			while (TRUE)
			{
				rsep = strchr (line, '\r');
				if (rsep != NULL)
				{
					gchar rsave = 0;	//assignment for warning prevention only
					if (rsep != nsep - 1)	//this is not a \r\n sequence
					{
						rsep++;
						rsave = *rsep;
					}
					*rsep = '\0';
					WAIT_FOR_EVENTS_UNLOCKED //maybe this will help sporadic crash with fast output
					CLOSEBGL
					if (error)
						e2_output_print (rt->current_tab, line, rt->pidstr, FALSE,
							E2_ERRORTAGS, NULL);
					else
						e2_output_print (rt->current_tab, line, rt->pidstr, FALSE, NULL);
					OPENBGL
					if (rsep != nsep - 1)	//this is not a \r\n sequence
					{
						*rsep = rsave;
						line = rsep;
					}
				}
				else
					break;
			}

			WAIT_FOR_EVENTS_UNLOCKED //maybe this will help sporadic crash with fast output
			CLOSEBGL
			if (error)
				e2_output_print (rt->current_tab, line, rt->pidstr, FALSE,
					E2_ERRORTAGS, NULL);
			else
				e2_output_print (rt->current_tab, line, rt->pidstr, FALSE, NULL);
			OPENBGL

			*nsep = nsave;
			line = nsep;
		}
		else
		{
			if (*line != '\0')
			{
				while (TRUE)
				{
					rsep = strchr (line, '\r');
					if (rsep != NULL)
					{
						gchar rsave;
						rsep++;
						rsave = *rsep;
						*rsep = '\0';
						WAIT_FOR_EVENTS_UNLOCKED //maybe this will help sporadic crash with fast output
						CLOSEBGL
						if (error)
							e2_output_print (rt->current_tab, line, rt->pidstr, FALSE,
								E2_ERRORTAGS, NULL);
						else
							e2_output_print (rt->current_tab, line, rt->pidstr, FALSE, NULL);
						OPENBGL
						*rsep = rsave;
						line = rsep;
					}
					else
						break;
				}

				WAIT_FOR_EVENTS_UNLOCKED //maybe this will help sporadic crash with fast output
				CLOSEBGL
				if (error)
					e2_output_print (rt->current_tab, line, rt->pidstr, FALSE,
						E2_ERRORTAGS, NULL);
				else
					e2_output_print (rt->current_tab, line, rt->pidstr, FALSE, NULL);
				OPENBGL
			}
			break;
		}
	}
}

#ifdef E2_NEW_COMMAND //=======================
/*DOES NOT WORK PROPERLY YET
select() returns prematurely for linux 2.4
fork again and read from grandchild, like glib ?
use pseudo forkpty ?
sync commands print nothing until command ends
additional thread for sync output to sync child ?
event reinstatement after <Ctrl>c checks with sync child
SIGPOLL handler instead of polling in watch func? maybe only works when detached from session terminal ?
*/
//piping from here to child stdin not properly supported, not least because the
//output fd is always selectable, which causes irrelevant returning from select()
//#define OUTPUT_POLL

enum { INWARDS, OUTWARDS };	//pipe fd enumerator

/* In Linux versions before 2.6.11, the capacity of a pipe (== PIPE_BUF) was the
same as the system page size (e.g., 4096 bytes on x86). Since Linux 2.6.11, the
pipe capacity is 65536 bytes. Other OS's have different values */
#define READBUFSIZE 1024
//this is READBUFSIZE, less space for adding a trailing \0, then rounded down to multiple of 8
#define READMAX 1016

//#include <fcntl.h>
//#include <stropts.h> //for ioctl

/* *
@brief set handler for SIGPOLL/SIGIO signals
@return
*/
/*static void _e2_command_set_sigpoll_handler (void)
{
	struct sigaction sigdata;
	sigdata.sa_sigaction = _e2_command_sigpoll_handler;
	sigemptyset (&sigdata.sa_mask);
	sigdata.sa_flags = SA_RESTART | SA_SIGINFO;
	sigaction (SIGPOLL, &sigdata, NULL);
} */
/**
@brief tolerantly transfer data from file descriptor @a fd into @a buffer
The buffer pointed to by @a buffer must be big enough to hold @a count bytes
@param fd number of file descriptor to read
@param buffer character-buffer pointer
@param count maximum no. of bytes to read or write

@return the no. of bytes read, -1 in case of some errors
*/
static ssize_t _e2_command_piperead (gint fd, gchar *buffer, gint count)
{
	ssize_t i, n = 0;
	while (n < count)
	{
		i = read (fd, buffer + n, count - n);	//vfs?? e2_fs_file_blockread;
		if (i >= 0)
			return (i+n);
		else
		{
			switch (errno)
			{
				//some errors we don't care about
				case EINTR:
				case EAGAIN:
#ifdef ERESTART
				case ERESTART:
#endif
					n += i;
					break;
				default:
					return -1;
					break;
			}
		}
	}
	return count;
}
/*
//FOR DEBUGGING
static void _e2_command_sigpoll_handler (gint num, siginfo_t *info, void *context)
{
	printd (DEBUG, "signal handler (signal num:%d)", num);

	if (info != NULL)
	{
		gint code = info->si_code;
		switch (code)
		{
			case POLL_IN:	//data input available
				break;
			case POLL_OUT:	//output buffers available
				break;
			case POLL_MSG:	//input message available
				break;
			case POLL_ERR:	//I/O error
				break;
			case POLL_PRI:	//high priority input available
				break;
			case POLL_HUP:	//device disconnected
				break;
		}
		glong band = info->si_band;	//band event for POLL_IN, POLL_OUT, or POLL_MSG
	}
/ *
	gint fd = 0;
	gint owner = fcntl (fd, F_GETOWN);
	gint oldflags = fcntl (fd, F_GETFL);
	fcntl (fd, F_SETFL, oldflags | O_NONBLOCK | O_ASYNC);

	gint yesplease = 1;
	ioctl (fd, FIOASYNC, &yesplease);
* /
}
*/
#define E2CMDTYPEB
/**
@brief watch for and forward to output pane any stdout or stderr text from running command
This is a thread function.
Assumes BGL is off
@param rt pointer to command data struct

@return the pointerised exit code of the command, > NULL if error
*/
static gpointer _e2_command_watch (E2_TaskRuntime *rt)
{
#ifdef E2CMDTYPEB
	gboolean outread, errread;
#endif
	struct timespec wait;
	struct timespec *waitptr;
	gchar *buffer;
#ifdef USE_GLIB2_10
	buffer = g_slice_alloc (READBUFSIZE);
#else
	buffer = g_try_malloc (READBUFSIZE);
#endif
#if (CHECKALLOCATEDWARNT)
	CHECKALLOCATEDWARNT (buffer, return (GINT_TO_POINTER(1)););
#else
	if (buffer == NULL)
		return (GINT_TO_POINTER(1));
#endif

	e2_utils_block_thread_signals ();	//block all allowed signals to this thread (what about SIGPIPE?)

	E2_CommandTaskData *cmddata = &rt->ex.command;
	printd (DEBUG, "child output fd = %d, child error fd = %d, max monitorable fd = %d",
		cmddata->child_stdout_fd, cmddata->child_stderr_fd, FD_SETSIZE);
	//FIXME handle assigned fd > FD_SETSIZE
	gint maxfd = MAX(cmddata->child_stdout_fd, cmddata->child_stderr_fd);
#ifdef OUTPUT_POLL
	if (cmddata->child_stdin_fd > maxfd)
		maxfd = cmddata->child_stdin_fd;
#endif
	maxfd++;

	//as the pipes will be selected at least when the child closes it's ok to
	//block until select succeeds NO -  BAD to block when a command ends immediately
	//and note, ouput pipe (if used) is always selectable
	waitptr = &wait; //NULL;
#ifdef E2CMDTYPEB
	outread = errread = TRUE;	//we want to pselect() the fd's at least once
#endif

	while (TRUE)
	{
		gint selcount;
		guint maxwait;
		ssize_t bytecount;
#ifndef E2CMDTYPEB
		gboolean outread, errread;
#endif
		fd_set in_fds;
#ifdef OUTPUT_POLL
		fd_set out_fds;
#endif
		maxwait = 1;	//initial poll-timeout is 1 sec.
reselect:
/*#ifdef E2CMDTYPEB
		//FIXME handle case where outread and errread are both FALSE;
		if (!(outread || errread))
		{
			selcount = 0;
			goto ?
		}
#endif */
		FD_ZERO (&in_fds);
//FIXME find a way to make these tests work without stuffing up the completion reporting
#ifdef E2CMDTYPEB
		if (outread)	//don't select fd's that are closed
#endif
			FD_SET (cmddata->child_stdout_fd, &in_fds);
#ifdef E2CMDTYPEB
		if (errread)
#endif
			FD_SET (cmddata->child_stderr_fd, &in_fds);
#ifdef OUTPUT_POLL
		FD_ZERO (&out_fds);
# ifdef E2CMDTYPEB
		if (?)
# endif
			FD_SET (cmddata->child_stdin_fd, &out_fds);
#endif
		//timer data undefined after last use, re-init if used now
		if (waitptr != NULL)
		{
			wait.tv_sec = maxwait; //0 for immediate return
//			wait.tv_usec = 0;
			wait.tv_nsec = 0;
		}

#ifndef E2CMDTYPEB
		outread = errread = FALSE;	//ensure var's are initialised
#endif
		errno = 0;
#ifdef OUTPUT_POLL
		selcount = pselect (maxfd, &in_fds, &out_fds, NULL, waitptr, NULL);
#else
		selcount = pselect (maxfd, &in_fds, NULL, NULL, waitptr, NULL);
#endif
		if (selcount > 0)
		{
//			printd (DEBUG, "pipe selection");
			//at end-of-command, both input fd's report EOF (0 bytes ready to read)
			if (FD_ISSET (cmddata->child_stdout_fd, &in_fds))
			{
//				printd (DEBUG, "child stdout ready");
				//read input, starting with normal pipe
#ifndef E2CMDTYPEB
				outread = FALSE;	//in case this one fails
#endif
				while ((bytecount =
					_e2_command_piperead (cmddata->child_stdout_fd, buffer, READMAX)) > 0)
				{
//					printd (DEBUG, "child stdout buffer read");
#ifndef E2CMDTYPEB
					outread = TRUE;
#endif
					*(buffer+bytecount) = '\0';
					_e2_command_display (buffer, FALSE, rt);
					if (bytecount != READMAX)
						break;
				}
#ifdef E2CMDTYPEB
				outread = (bytecount != 0);	//finished when 0 bytes read
#endif
#ifdef DEBUG_MESSAGES
				if (bytecount == 0)		//EOF
				{
					printd (DEBUG, "child stdout EOF");
				}
				else if (!outread)
				{
					//pipe closed or closing
					printd (DEBUG, "actually, child stdout pipe NOT ready: %s", strerror(errno));
				}
#endif
			}
//#ifdef E2CMDTYPEB
//			else
//				outread = TRUE;	//ensure reselection happens
//#endif

			//to minimise race-risk, check again for stderr if stdout was processed
			if (!FD_ISSET (cmddata->child_stderr_fd, &in_fds) && outread)
			{
//				printd (DEBUG, "re-check child stderr ready");
				wait.tv_sec = 0;
//				wait.tv_usec = 0;
				wait.tv_nsec = 0;
				FD_ZERO (&in_fds);
				FD_SET (cmddata->child_stderr_fd, &in_fds);
				TEMP_FAILURE_RETRY (pselect (cmddata->child_stderr_fd+1, &in_fds, NULL, NULL, &wait, NULL));
			}
			if (FD_ISSET (cmddata->child_stderr_fd, &in_fds))
			{
//				printd (DEBUG, "child stderr ready");
				//read input, starting with error pipe
#ifndef E2CMDTYPEB
				errread = FALSE;	//in case this one fails
#endif
				while ((bytecount =
					_e2_command_piperead (cmddata->child_stderr_fd, buffer, READMAX)) > 0)
				{
//					printd (DEBUG, "child stderr buffer read");
#ifndef E2CMDTYPEB
					errread = TRUE;
#endif
					*(buffer+bytecount) = '\0';
					_e2_command_display (buffer, TRUE, rt);
					if (bytecount != READMAX)
						break;
				}
#ifdef E2CMDTYPEB
				errread = (bytecount != 0);	//finished when 0 bytes read
#endif
#ifdef DEBUG_MESSAGES
				if (bytecount == 0)
				{
					printd (DEBUG, "child stderr EOF");
				}
				else if (!errread)
				{
					//pipe closed or closing ??
					printd (DEBUG, "actually, child stderr NOT ready: %s", strerror(errno));
				}
#endif
			}
//#ifdef E2CMDTYPEB
//			else
//				errread = TRUE;	//ensure reselection happens
//#endif
#ifdef OUTPUT_POLL
//FIXME grab any commandline input and pass direct to running sync command
//MAYBE only if child is stopped ?
//FIXME child-pipe is ALWAYS ready, including at at end-of-command
//reading a line with \0 termination seems not to work
//multi reads can close the fd ?
			if (FD_ISSET (cmddata->child_stdin_fd, &out_fds))
			{
				printd (DEBUG, "child ready for input");
/*				CLOSEBGL
				e2_output_print (rt->current_tab, "Child wants input",
					rt->pidstr, TRUE, E2_ERRORTAGS, NULL);
				OPENBGL
				if (0)
				{	//there is something to send ...
					child = fdopen (cmddata->child_stderr_fd, "w");
					do
					{
						//DO SOMETHING TO GET MESSAGE INTO BUFFER
					} while (fputs (buf, child) != EOF);
					fclose (child);
				}
*/
			}
#endif
		}
		else	//selcount <= 0
			if (selcount == -1 && errno == EINTR)
		{
			usleep (1000);	//wait awhile for any interrrupt processing
			goto reselect;
		}
		else if (selcount < 0)
		{
			printd (DEBUG, "pipe selection error %d (%s)", errno, g_strerror (errno));
//			outread = errread = FALSE;	//ensure no more selects
//			selcount = 0;	//fake value to trigger loop exit
		}

/*		CHECKME possible race around SIGCHILD signal
		>> last flush of the pipes here ?
		pthread_mutex_lock (&task_mutex);
		gboolean done = (rt->status >= E2_TASK_COMPLETED);
		pthread_mutex_unlock (&task_mutex);
*/
		gboolean done = !(outread || errread);
#ifdef OUTPUT_POLL
		|| ? FIXME output too
#endif
		if (done
//#ifdef E2CMDTYPEB
//				|| selcount == 0 || !(outread || errread)
//#endif
			)
		{
			usleep (10000);	//allow for some cleanup
#ifndef E2CMDTYPEB
			if (selcount == 0 || !(outread || errread))
			{
#endif
				printd (DEBUG, "watch function cleanup after end of process %s", rt->pidstr);
				close (cmddata->child_stdin_fd);
				close (cmddata->child_stdout_fd);
				close (cmddata->child_stderr_fd);
				if ((rt->flags & E2_RUN_SHOW) && e2_option_bool_get ("fileop-show"))
				{
#ifdef E2CMDTYPEB
					//wait at most 5 secs for completion code to be logged
					guint i = 0;
					while (!done && i < 250)
					{
						usleep (20000);
						pthread_mutex_lock (&task_mutex);
						done = (rt->status >= E2_TASK_COMPLETED);
						pthread_mutex_unlock (&task_mutex);
						i++;
					}
					if (i < 250)
					{
#endif
						gchar *shorter = e2_utils_str_shorten (cmddata->command, 60, E2_DOTS_MIDDLE);
						gchar *message = g_strdup_printf ("%s>%s (%s) %s '%d'\n",
							(rt->flags & E2_RUN_EXT) ? "sh" : "", shorter,
							rt->pidstr, _("returned"), cmddata->exit);
						g_free (shorter);
						CLOSEBGL
						e2_output_print (rt->current_tab, message, rt->pidstr,
							TRUE, "small", (cmddata->exit == 0) ? "posit" : "negat", NULL);
						OPENBGL
						g_free (message);
#ifdef E2CMDTYPEB
					}
					else
						printd (WARN, "timeout - failed to detect child completion");
#endif
				}
#ifdef E2_OUTPUTSTYLES
				e2_output_clear_styles (rt->current_tab, rt->pidstr);
#endif
		//CHECKME sometimes (how?) we can end this func without breaking here
				printd (DEBUG, "Exiting command select loop");
				break;
#ifndef E2CMDTYPEB
			}
			else	//CHECKME maybe not really needed ?
			{
				printd (DEBUG, "scanning child pipes again after watch function detected process %s completion flag", rt->pidstr);
				//do one last quick scan in case of race between SIGCHILD and pipes interrogation
				maxwait = 0;
				waitptr = &wait;
				goto reselect;
			}
#endif
		}
#ifdef DEBUG_MESSAGES
		else
		{
//			if (selcount == 0)
//				printd (DEBUG, "timeout - no fd's ready for processing");
			if (!outread)
				printd (DEBUG, "flag outread FALSE");
			if (!errread)
				printd (DEBUG, "flag errread FALSE");
		}
#endif
	//CHECKME can we do this without so many loops ? SIGPOLL ?
	//CHECKME for sync commands, omit WNOHANG ?
/*
		if (maxwait < 10)
		{	//this is the end of the first pipes-scan
			maxwait = 10;	//revert to slower polling after 1st pass
//			cmddata->pollcount = -1;	//flag that the pipes have been polled now,
									//(so it's ok for timer to re-enable main-thread SIGIO)
			//ostensibly this should have no impact on main-thread signals, but
			//for some kernels it does seem to speed up termination
			//note that this will probably stuff up the blockage refcount!
//			e2_command_unblock_childsignal (); //allow any child's status change to be noticed
//			printd (DEBUG, "SIGCHILD re-enabled in watch function");
		}
THIS RELIES ON select() returning regularly ...
		if (cmddata->sync)
		{
			GdkDisplay *display =
			gdk_display_manager_get_default_display (gdk_display_manager_get());
			GdkEvent *event, *event2;
			//check for cancellation request
			//callbacks do not work, we must poll the events queue
			if ((event = gdk_display_get_event (display)) != NULL)	//peek won't work
			{
				if (event->type == GDK_KEY_PRESS)
				{
					TRANSLATE keyval
					if (((GdkEventKey*)event)->keyval == GDK_Control_L
						|| ((GdkEventKey*)event)->keyval == GDK_Control_R)
					{
						event2 = event;
						if ((event = gdk_display_get_event (display)) != NULL
							 && ((GdkEventKey*)event)->keyval == GDK_c
							 && ((GdkEventKey*)event)->state & GDK_CONTROL_MASK)
						{
							printd (DEBUG, "SIGTERM sent to child process group");
							kill (-pid, SIGTERM);
						}
						else
						{
							if (event != NULL)
							{
								gdk_display_put_event (display, event);
								gdk_event_free (event);
							}
							gdk_display_put_event (display, event2);
							gdk_event_free (event2);
						}
					}
					else if (((GdkEventKey*)event)->keyval == GDK_c
						&& (((GdkEventKey*)event)->state & GDK_CONTROL_MASK)
						//&& ((GdkEventKey*)event)->window == ?
					)
					{
						printd (DEBUG, "SIGTERM sent to child process group");
						kill (-pid, SIGTERM);
					}
//FIXME reinstating irrelevant events blocks handling of relevant ones
					else
						gdk_display_put_event (display, event);
				}
				else
					gdk_display_put_event (display, event);

				gdk_event_free (event);
			}
		}
*/
	}
#ifdef USE_GLIB2_10
	g_slice_free1 (READBUFSIZE, buffer);
#else
	g_free (buffer);
#endif

#ifdef E2_SU_ACTIONS
	if (!e2_task_revert_user (rt->flags))
		rt->status = E2_TASK_INCOMPLETE;
#endif

//	printd (DEBUG, "End command watch");
	return (GINT_TO_POINTER (rt->ex.command.exit));
}

/**
@brief run command @a command
Args[0] is either just the application/command name, or an absolute path ending
with such name.
Assumes BGL is closed upon entry.
@param command the entire command string, used in messages, UTF-8
@param args pointer to NULL-terminated command & arguments array, each member localised
@param cwd path of directory to use as CWD for the command, UTF-8
@param flags indicators of how the command is to be handled e.g.
 whether to show @a command and its pid in the output pane,
 whether command is to be run synchronously

@return the exit code of the command, > 0 if error
*/
static gint _e2_command_fork (gchar *command, gchar **args, const gchar *cwd,
	E2_RunFlags flags)
{
#ifdef E2_VFS
	VPATH ddata;
#endif
	//there are no embedded tests for command existence, so we need to do that
	if (!g_path_is_absolute (args[0]))
	{
#ifdef E2_VFSTMP
		if supporting get-and-exec, then local $PATH may be irrelevant
		need relevant PlaceInfo to sort this out, might not be curr_view->spacedata->workplace
		cwd needs to be a VPATH*
#endif
		gchar *path = g_find_program_in_path (args[0]);
		if (path != NULL)
		{
			g_free (args[0]);
			args[0] = path;
		}
		else
		{
			gchar *utf = F_DISPLAYNAME_FROM_LOCALE (args[0]);
			gchar *msg = g_strdup_printf (_("Command '%s' - %s"), utf, g_strerror (errno));
			e2_output_print_error (msg, TRUE);
			F_FREE (utf, args[0]);
			return 1;
		}
	}

	gchar *message;
	pid_t pid;
/*
//#include <termios.h>

	struct termios shell_tmodes;
	pid_t shell_pgid;
	gboolean interactive_shell = isatty (STDIN_FILENO);
	if (interactive_shell)
	{
		//loop until we are in the foreground
		while (tcgetpgrp (STDIN_FILENO) != (shell_pgid = getpgrp ()))
			kill (-shell_pgid, SIGTTIN);

/ *		//ignore interactive and job-control signals
		signal (SIGINT, SIG_IGN);
		signal (SIGQUIT, SIG_IGN);
		signal (SIGTSTP, SIG_IGN);
		signal (SIGTTIN, SIG_IGN);
		signal (SIGTTOU, SIG_IGN);
		signal (SIGCHLD, SIG_IGN);
* /
		//put ourselves in our own process group
		//NOTE conform e2_task_cleanup() to kill the group
		shell_pgid = getpid ();
		if (setpgid (shell_pgid, shell_pgid) < 0)
		{
			printd (DEBUG,"Could not put shell into its own process group");
			return FALSE;
		}
		//grab control of the terminal
		tcsetpgrp (STDIN_FILENO, shell_pgid);
		//save default terminal attributes for shell
		tcgetattr (STDIN_FILENO, &shell_tmodes);
	}
*/
	//setup to write to child's stdin & read child's stdout, stderr
	gint stdin_pipe[2];
	gint stdout_pipe[2];
	gint stderr_pipe[2];
	if (pipe (stdin_pipe) < 0)	//no need to worry about VFS
	{
		goto launch_error;
	}
	if (pipe (stdout_pipe) < 0)
	{
		close (stdin_pipe[INWARDS]);
		close (stdin_pipe[OUTWARDS]);
		goto launch_error;
	}
	if (pipe (stderr_pipe) < 0)
    {
		close (stdin_pipe[INWARDS]);
		close (stdin_pipe[OUTWARDS]);
		close (stdout_pipe[INWARDS]);
		close (stdout_pipe[OUTWARDS]);
		goto launch_error;
	}
	E2_TaskRuntime *rt;
	//keep trying this until data set fn not busy
	//real pid is set later
	while ((rt = e2_task_set_data (0,
		(flags & E2_RUN_SYNC) ? E2_TASKTYPE_SYNC : E2_TASKTYPE_ASYNC,
		command)) == NULL);
	if (rt == GINT_TO_POINTER (1))
	{
		close (stdin_pipe[INWARDS]);
		close (stdin_pipe[OUTWARDS]);
		close (stdout_pipe[INWARDS]);
		close (stdout_pipe[OUTWARDS]);
		close (stderr_pipe[INWARDS]);
		close (stderr_pipe[OUTWARDS]);
		goto launch_error;
	}

	e2_command_block_childsignal ();
	rt->flags = flags;

	OPENBGL	//BGL management happens downstream

	if ((pid = fork ()) == 0) //child process
	{	

#ifdef E2_SU_ACTIONS
		gchar **envp = NULL;
		if (flags & E2_RUN_AS)
		{
			if (!e2_task_become_user (flags, (gpointer*)&envp))
			{
				printd (ERROR, "Could not change to alternate user");
				//FIXME warn user about this
				_exit (2);
			}
		}
#endif
		close (stdin_pipe[OUTWARDS]);
		close (stdout_pipe[INWARDS]);
		close (stderr_pipe[INWARDS]);

/* see below, without an abort
		pid = getpid ();	//get real pid
		if (setpgid (pid, pid) < 0)
		{
			printd (DEBUG,"Could not put child into its own process group");
			//FIXME warn user about this
//			_exit (2);
		}
*/
		//arrange stdio via parent
/* seems to do nothing
	Setting the O_ASYNC flag for the read end of a pipe causes a signal
	(SIGIO by default) to be generated when new input becomes available on the pipe.
	On Linux, O_ASYNC is supported for pipes only since kernel 2.6.
	If the pipe is full, then a write(2) will block or fail, depending on whether
	the O_NONBLOCK flag is set
		gint flags = fcntl (stdout_pipe[OUTWARDS], F_GETFL);
		if (flags != -1)
			fcntl (stdout_pipe[OUTWARDS], F_SETFL, flags | O_NONBLOCK | O_ASYNC);
		flags = fcntl (stderr_pipe[OUTWARDS], F_GETFL);
		if (flags != -1)
			fcntl (stderr_pipe[OUTWARDS], F_SETFL, flags | O_NONBLOCK | O_ASYNC);
*/
		//these fd assignments redirect child's stdin, stdout, stderr streams too
		TEMP_FAILURE_RETRY (dup2 (stdin_pipe[INWARDS], STDIN_FILENO));
		//redirect output to parent
		TEMP_FAILURE_RETRY (dup2 (stdout_pipe[OUTWARDS], STDOUT_FILENO));
		TEMP_FAILURE_RETRY (dup2 (stderr_pipe[OUTWARDS], STDERR_FILENO));
//close on exit flags ?
		//setup crash if the parent exits and we later write to a pipe
		signal (SIGPIPE, SIG_DFL);

		e2_command_unblock_childsignal ();

/*	   //CHECKME make new session (no terminal), with child as its leader
		if ((pid = setsid ()) < 0)
		{
			printd (WARN,"Could not put child into a new session");
			//FIXME warn user about this
		}
*/
		pid = getpid ();	//get real pid
		if (setpgid (pid, pid) < 0)
		{
			printd (WARN,"Could not put child into its own process group");
			//FIXME warn user about this
		}

#ifdef E2_VFS
		if (curr_view->spacedata != NULL)
		{
#ifdef E2_VFSTMP
	if supporting get-and-exec, and getting is slow, some cd-race might occur
	need relevant PlaceInfo which might not be curr_view->spacedata->workplace
#endif
			//there's no native analog for the vdir, so go here...
			//all command args need to have been modified accordingly, by the command interpreter
			ddata.path = curr_view->spacedata->workplace;
			ddata.spacedata = NULL;
			if (!e2_fs_chdir_local (&ddata E2_ERR_NONE()))
				execvp (args[0], args);	//should not return
		}
		else	//native execution
#endif
		{
			if (cwd == NULL)
				cwd = curr_view->dir;
			if (e2_fs_chdir ((gchar *)cwd E2_ERR_NONE()))
			{
//				printd (DEBUG, "Child will run '%s'", args[0]);
#ifdef E2_SU_ACTIONS
				if (envp != NULL)
					execve (args[0], args, envp);
				else
#endif
					execvp (args[0], args);	//should not return
			}
		}
		_exit (1);
		//end of child process
	}
	if (pid > 0) //in parent process, pid is child
	{
		rt->status = E2_TASK_RUNNING;
		rt->pid = (glong) pid; //remember the child
		rt->pidstr = g_strdup_printf ("%ld", rt->pid);
		E2_CommandTaskData *cmddata = &rt->ex.command;
		close (stdin_pipe[INWARDS]);
		cmddata->child_stdin_fd = stdin_pipe[OUTWARDS];	//setup for later sending to child
		cmddata->child_stdout_fd = stdout_pipe[INWARDS];	//setup for later receiving from child
		close (stdout_pipe[OUTWARDS]);
		cmddata->child_stderr_fd = stderr_pipe[INWARDS];	//setup for later receiving from child
		close (stderr_pipe[OUTWARDS]);
//		FILE *child_stdin_stream = fdopen (stdin_pipe[OUTWARDS], "w");
//		FILE *child_stdout_stream = fdopen (stdout_pipe[INWARDS], "r");
//		FILE *child_stderr_stream = fdopen (stderr_pipe[INWARDS], "r");
//		cmddata->pollcount = 0;	//not ready to enable SIGCHILD yet NOT USED NOW
#ifdef DEBUG_MESSAGES
		if (flags & E2_RUN_SYNC)
			printd (DEBUG, "synchronously run command %s", args[0]);
		else
			printd (DEBUG, "asynchronously run command %s", args[0]);
#endif
		if ((flags & E2_RUN_SHOW) && e2_option_bool_get ("fileop-show"))
		{
			message = g_strconcat ((flags & E2_RUN_EXT) ? "sh>" : ">", command, NULL);
			CLOSEBGL
			e2_output_print (rt->current_tab,
				message, rt->pidstr, FALSE, "bold", "cmand", NULL);
			g_free (message);
			message = g_strdup_printf ("  (%s)\n", rt->pidstr);
			e2_output_print (rt->current_tab,
				message, rt->pidstr, FALSE, "small", "cmand", NULL);
			OPENBGL
			g_free (message);
//			WAIT_FOR_EVENTS_UNLOCKED;
		}

	//	e2_command_unblock_childsignal (); //allow child exit to be noticed SEE TIMER
	/* seems to do nothing
		Setting the O_ASYNC flag for the read end of a pipe causes a signal
		(SIGIO by default) to be generated when new input becomes available on the pipe.
		On Linux, O_ASYNC is supported for pipes only since kernel 2.6.

		//make pipes non-blocking, so we can read big buffers without having
		//to use fionread() to prevent blocking
		gint flags = fcntl (stdout_pipe[INWARDS], F_GETFL);
		if (flags != -1)
			fcntl (stdout_pipe[INWARDS], F_SETFL, flags | O_NONBLOCK | O_ASYNC);
		flags = fcntl (stderr_pipe[INWARDS], F_GETFL);
		if (flags != -1)
			fcntl (stderr_pipe[INWARDS], F_SETFL, flags | O_NONBLOCK | O_ASYNC);
	*/
	/*	prefer to poll pipes at least once before unblocking SIGCHILD in this thread
		(ostensibly it's not valid to unblock SIGCHILD in another thread where watching
		happens, but for some kernels that does seem to help, so unblocking also happens
		in _e2_command_watch())
		So for async at least we use a timer which checks a flag set in _e2_command_watch()
		CHECKME can timer persist past rt destruction at session end ? etc ?
		Timer interval is compromise between prompt cleanup when there is little or no
		initial input, and the amount of interruption of the initial input loop when
		that involves a lot of printing - so we use a 2-stage timer
	*/
		pthread_t wthreadID;
		if (flags & E2_RUN_SYNC)
		{
			if (pthread_create (&wthreadID, NULL,
				(gpointer(*)(gpointer))_e2_command_watch, rt))
			{
				CLOSEBGL //do this before unblocking ?
				e2_command_unblock_childsignal ();
			}
			else
			{
//BAD HERE		CLOSEBGL
				e2_command_unblock_childsignal ();
				//FIXME allow command output to be printed while waiting for completion
				//probably by running as for async, but blocking start of any other action
				// or command (except send to child)
				pthread_join (wthreadID, NULL);
				CLOSEBGL
				return cmddata->exit;
			}
		}
		else	//async execution
		{
			pthread_attr_t attr;
			pthread_attr_init (&attr);
			pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);

			gint res = pthread_create (&wthreadID, &attr,
				(gpointer(*)(gpointer))_e2_command_watch, rt);

			pthread_attr_destroy (&attr);

			CLOSEBGL
			e2_command_unblock_childsignal ();
			if (res == 0)
				return 0;
		}
	}
	else //pid < 0, fork error
	{
		CLOSEBGL
		close (stdin_pipe[INWARDS]);
		close (stdin_pipe[OUTWARDS]);
		close (stdout_pipe[INWARDS]);
		close (stdout_pipe[OUTWARDS]);
		close (stderr_pipe[INWARDS]);
		close (stderr_pipe[OUTWARDS]);
		e2_command_unblock_childsignal ();
		rt->status = E2_TASK_FAILED;
		rt->ex.command.exit = 128;
	}

launch_error:
#ifdef E2_VFS
# ifdef E2_VFSTMP
	if supporting get-and-exec, and getting is slow, some cd-race might occur
	need relevant PlaceInfo which might not be curr_view->spacedata->workplace
# endif
	ddata.path = args[0];
	ddata.spacedata = curr_view->spacedata;
#endif
	OPENBGL	//downstream does mutex management
	e2_fs_error_simple (_("Error while launching '%s'"),
#ifdef E2_VFS
		&ddata);
#else
		args[0]);
#endif
	CLOSEBGL
	return 1;
}
/**
@brief pipe utf-8 string @a message to process @a pid
The specified process must be a child of the current e2 instance
@param pid the id of the destination process, or 0 for the last-created child
@param message, utf8 string, to send to process @a pid
@return TRUE if the string was sent without error
*/
static gboolean _e2_command_send_to_pid (glong pid, gchar *message)
{
	printd (DEBUG, "_e2_command_send_to_pid (pid:%d,messge:%s)", pid, message);
	gchar *msg2 = NULL;
	E2_TaskRuntime *rt;

	if (pid == 0)
	{
		rt = e2_task_find_last_running_child (FALSE);
		if (rt == NULL)
			msg2 = g_strdup (_("Cannot find last child process"));
		else
			pid = rt->pid;	//in case of error message
	}
	else
	{
		rt = e2_task_find_running_task (pid);
		if (rt == NULL)
			msg2 = g_strdup_printf (_("Cannot find child process with pid %ld"), pid);
	}
	if (rt != NULL && rt->action)
		msg2 = g_strdup_printf (_("Cannot communicate to process %ld"), pid);

	if (msg2 != NULL)
	{
		e2_output_print_error (msg2, TRUE);
		return FALSE;
	}

	//CHECKME required message encoding ?
	msg2 = g_strconcat (message, "\n", NULL);
	size_t size = strlen (msg2);
	ssize_t sent = write (rt->ex.command.child_stdin_fd, msg2, size);
	if (sent != size)	//FIXME handle -1 error or keep trying to write it all
	{
		g_free (msg2);
		msg2 = _("Failed writing to child");
		e2_output_print (rt->current_tab, msg2, rt->pidstr, TRUE, E2_ERRORTAGS, NULL);
		e2_utils_beep ();
		return FALSE;
	}
	else if (rt->flags & E2_RUN_SHOW)
		//report what has been sent FIXME what if it's a password ?
		e2_output_print (rt->current_tab, msg2, rt->pidstr, FALSE, NULL);

	g_free (msg2);
	return TRUE;
}
#else //ndef E2_NEW_COMMAND =======================
/**
@brief process non-error-iochannel data
@param ioc the channel passing the data
@param cond flags indicating the condition(s) that have been satisfied
@param rt pointer to data struct for the command to which the channel applies
@return FALSE if the event source should be removed
*/
static gboolean _e2_command_watch_std (GIOChannel *ioc, GIOCondition cond,
	E2_TaskRuntime *rt)
{
	return _e2_command_watch (ioc, cond, rt, FALSE);
}
/**
@brief process error-iochannel data
@param ioc the channel passing the data
@param cond flags indicating the condition(s) that have been satisfied
@param rt pointer to data struct for the command to which the channel applies
@return FALSE if the event source should be removed
*/
static gboolean _e2_command_watch_err (GIOChannel *ioc, GIOCondition cond,
	E2_TaskRuntime *rt)
{
	return _e2_command_watch (ioc, cond, rt, TRUE);
}
/**
@brief process iochannel data

Data are grabbed in parcels up to 4095 bytes, which are then separated
into lines (\n), and those lines are sent to the output printer
Will shut the channel down upon error

@param ioc the channel event source
@param cond flags indicating the condition(s) that have been satisfied
@param rt pointer to data struct for the command to which the channel applies
@param error TRUE if this call relates to the error channel
@return FALSE if the event source should be removed
*/
static gboolean _e2_command_watch (GIOChannel *ioc, GIOCondition cond,
	E2_TaskRuntime *rt, gboolean error)
{
	printd (DEBUG, "_e2_command_watch (ioc:,cond:%x,rt:,%s channel)", cond, (error) ? "stderr":"stdout");
	E2_CommandTaskData *cmddata = &rt->ex.command;
	if (cond & (G_IO_IN | G_IO_PRI))
	{
		gchar buf[5000];	//space (rounded) to read 4096 and add trailing \0
		//buf[0] = buf[4096] = '\0';
		gsize len = 0;

//		e2_command_block_childsignal ();

		GIOStatus ret;
reread:
		ret = g_io_channel_read_chars (ioc, buf, 4096 * sizeof(gchar), &len, NULL);
		if (ret == G_IO_STATUS_NORMAL && len > 0)	//len==0 is also a proxy for ret == G_IO_STATUS_EOF
		{
			*(buf + len) = '\0';
			//line-by-line processing needed to parse and handle CR's BS's etc
			_e2_command_display (buf, error, rt);

//			e2_command_unblock_childsignal ();
			return TRUE;
		}
		else if (ret == G_IO_STATUS_AGAIN)
			goto reread;

//		e2_command_unblock_childsignal ();
	}

	if (cond & (G_IO_ERR | G_IO_HUP | G_IO_NVAL))
	{
		gint fd;
		if (error)
		{	//shutting down child's error channel normally with flag G_IO_HUP
//			printd (DEBUG, "stderr iochannel cleanup");
//			rt->status = E2_TASK_COMPLETED;
			//possible race here - SIGCHILD handler may be called after this
			//so store a dummy exit value if that handler has not logged the real value
			if ((cond & (G_IO_ERR | G_IO_NVAL))
				&& rt->status != E2_TASK_COMPLETED)
			{
				rt->status = E2_TASK_INCOMPLETE;	//ensure this
				cmddata->exit = 1;
//				printd (DEBUG, "child status 1 stored");
			}
//			else
//				printd (DEBUG, "child status NOT stored here");

			fd = g_io_channel_unix_get_fd (ioc);
			g_io_channel_shutdown (ioc, TRUE, NULL);
//			g_io_channel_unref (ioc); the return FALSE has this effect
			e2_fs_safeclose (fd);
			//shutdown the output channel
//			if (cmddata->to_child != NULL)
//			{
				fd = g_io_channel_unix_get_fd (cmddata->to_child);
				g_io_channel_shutdown (cmddata->to_child, FALSE, NULL);
				g_io_channel_unref (cmddata->to_child); //like source-removal
				e2_fs_safeclose (fd);
//			}
		}
		else
		{	//shutting down child's stdout channel normally with flag G_IO_HUP
//			printd (DEBUG, "stdout iochannel cleanup");
//			e2_command_block_childsignal ();

			fd = g_io_channel_unix_get_fd (ioc);
			//shutdown the error input channel
			g_io_channel_shutdown (ioc, TRUE, NULL);
//			g_io_channel_unref (ioc); the return FALSE has this effect
			e2_fs_safeclose (fd);

//irrelevant for *NIX g_spawn_close_pid (rt->pid);
			//possible race here - SIGCHILD handler may be called after this
			//so store a dummy exit value if that handler has not logged the real value
			if (!(cond & (G_IO_ERR | G_IO_NVAL))
				&& rt->status != E2_TASK_COMPLETED)
			{
				cmddata->exit = 0;
//				printd (DEBUG, "child status 0 stored");
			}
//			else
//				printd (DEBUG, "child status NOT stored here");

			if ((rt->flags & E2_RUN_SHOW) && e2_option_bool_get ("fileop-show"))
			{
				gchar *shorter = e2_utils_str_shorten (cmddata->command, 60, E2_DOTS_MIDDLE);
				gchar *message = g_strdup_printf ("%s%s (%s) %s '%d'\n",
					(rt->flags & E2_RUN_EXT) ? "sh>" : ">", shorter,
					rt->pidstr, _("returned"), cmddata->exit);
				g_free (shorter);
				WAIT_FOR_EVENTS_UNLOCKED //maybe this will help sporadic crash with fast output
				CLOSEBGL
				e2_output_print (rt->current_tab, message, rt->pidstr, TRUE, "small",
					(cmddata->exit == 0) ? "posit" : "negat", NULL);
				OPENBGL
				g_free (message);
				printd (DEBUG, "child status printed");
			}
			else
			{	//this is a hack to get small amounts of command-output text to
				//display when there's no completion message
				WAIT_FOR_EVENTS_UNLOCKED //maybe this will help sporadic crash with fast output
				CLOSEBGL
				e2_output_print (rt->current_tab, "", rt->pidstr, FALSE, NULL);
				OPENBGL
			}
//			e2_command_unblock_childsignal ();
#ifdef E2_OUTPUTSTYLES
			e2_output_clear_styles (rt->current_tab, rt->pidstr);
#endif
		}
		return FALSE;	//removes the source
	}
	return TRUE;	//should never get to here
}
/* *
@brief for a sync command, get the real pid before the child command is executed
@param rt pointer to task data struct for the command
@return
*/
/*not called in sync command, dunno why
static void _e2_command_set_childdata (E2_TaskRuntime *rt)
{
	if (1)	//(rt->ex.command<is-sync>)
	{
		rt->pid = (glong) getpid ();
		printd (DEBUG, "command child pid is %ld", rt->pid);
		//FIXME setup to watch descendants stdout, stderr
	}
} */
/**
@brief set up channel for receiving data from a child process
This is called twice for each async child (for child's stdout and stderr channels)
@param fd descriptor for the channel (from g_spawn_async_with_pipes())
@param cond flags setting the mode of channel operation
@param func the function to be called when @a cond is satisfied
@param rt pointer to data struct for the command to which the channel applies
@return the channel
*/
static GIOChannel *_e2_command_setup_watch_channel
	(gint fd, GIOCondition cond, GIOFunc func, E2_TaskRuntime *rt)
{
	GIOChannel *ioc = g_io_channel_unix_new (fd);
	// Set IOChannel encoding to none, to make it fit for binary data
	//(which seems to mean that _any_ encoding will work)
	g_io_channel_set_encoding (ioc, NULL, NULL);
	g_io_channel_set_buffered (ioc, FALSE);
	g_io_channel_set_close_on_unref (ioc, TRUE);
	g_io_add_watch_full (ioc, e2_option_int_get ("command-watch-priority"),
		cond, (GIOFunc) func, rt, NULL);
	//undo reference added by g_io_add_watch ()
	g_io_channel_unref (ioc);
	return ioc;
}
/*
static gboolean _e2_command_watch_out (GIOChannel *ioc, GIOCondition cond,
	E2_TaskRuntime *rt)
{
	printd (DEBUG, "command can be written to");
	return TRUE;
} */
/**
@brief asynchronously run shell command
This sets the executed command's CWD to curr_view->dir
Expects BGL closed
@param command the command string, used in messages, utf8
@param args pointer to command arguments array, each localised
@param cwd path of directory to use as CWD for the command, utf8
@param flags indicators of how the command is to be handled e.g.
 whether to show @a command and its pid in the output pane,
 whether command is to be run in external shell

@return the pid of process running the command, or 0 if there's an error
*/
//FIXME when this sort of mechanism is nested in a child process, that process hangs
#if E2_DEBUG_LEVEL > 2
GIOChannel *stdoutchannel;
GIOChannel *stderrchannel;
#endif
static gint _e2_command_run_async (gchar *command, gchar **args, const gchar *cwd,
	E2_RunFlags flags)
{
	printd (DEBUG, "asynchronously run command %s", command);

#ifdef E2_SU_ACTIONS
	gchar **envp = NULL;
	if (flags & E2_RUN_AS)
	{
		if (!e2_task_become_user (flags, (gpointer*)&envp))
		{
			printd (ERROR, "Could not change to alternate user");
			//FIXME warn user about this
			return 0;
		}
	}
#endif

	e2_command_block_childsignal ();	//prevent feedback from child until ready to get it

	if (cwd == NULL)
		cwd = curr_view->dir;
	gchar *local;
#ifdef E2_VFS
	//for a non-native place, there is no analog for curr_view->dir, so go elsewhere
	//all args need to have been conformed, by the command interpreter
	local = (curr_view->spacedata == NULL) ?
		F_FILENAME_TO_LOCALE (cwd) : curr_view->spacedata->workplace;
#else
	local = F_FILENAME_TO_LOCALE (cwd);
#endif
	GPid pid = 0;
	gint stdinfd = 0, stdoutfd = 0, stderrfd = 0;	//file descriptors to write to and read from child
	GError *error = NULL;
	g_spawn_async_with_pipes (local, args,
#ifdef E2_SU_ACTIONS
		envp,
#else
		NULL,
#endif
		G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
		NULL, NULL, &pid, &stdinfd, &stdoutfd, &stderrfd, &error);
#ifdef E2_VFS
	if (curr_view->spacedata == NULL)
#endif
		F_FREE (local, cwd);

	//CHECKME revert CWD to curr_view->dir if that still exists ?

	if (error != NULL)
	{
		gchar *message = g_strconcat ((flags & E2_RUN_EXT) ? "sh>" : ">", command, " ", NULL);
		//always use the current tab at the start of the command
		e2_output_print (&app.tab, message, NULL, TRUE, "bold", "cmand", NULL);
		g_free (message);
		message = g_strconcat (error->message, ".", NULL);
		e2_output_print_error (message, TRUE);
		g_error_free (error);
		e2_command_unblock_childsignal ();
		return 0;
	}

	E2_TaskRuntime *rt;
	//keep trying this until data set fn not busy
	while ((rt = e2_task_set_data ((glong)pid, E2_TASKTYPE_ASYNC, command)) == NULL);

	if (rt == GINT_TO_POINTER (1))
	{
		//CHECKME cleanup the spawn ?
		e2_command_unblock_childsignal ();
		return 0;
	}
	rt->flags = flags;

	//channel for getting the child's stdout
#if E2_DEBUG_LEVEL > 2
	stdoutchannel =
#endif
	_e2_command_setup_watch_channel (stdoutfd,
		G_IO_IN | G_IO_PRI | G_IO_HUP,
		(GIOFunc)_e2_command_watch_std, rt);
	//channel for getting the child's stderr
#if E2_DEBUG_LEVEL > 2
	stderrchannel =
#endif
	_e2_command_setup_watch_channel (stderrfd,
		G_IO_IN | G_IO_PRI | G_IO_ERR | G_IO_HUP | G_IO_NVAL,
		(GIOFunc)_e2_command_watch_err, rt);

	//channel for sending to child stdin
	//FIXME do this only if/when needed ?
	GIOChannel *ioc = g_io_channel_unix_new (stdinfd);
	g_io_channel_set_encoding (ioc, NULL, NULL);	//binary (=any) encoding
	g_io_channel_set_buffered (ioc, FALSE);
	g_io_channel_set_close_on_unref (ioc, TRUE);
//	g_io_add_watch_full (ioc, e2_option_int_get ("command-watch-priority"),
//		G_IO_OUT, (GIOFunc) _e2_command_watch_out, rt, NULL);
	//undo reference added by g_io_add_watch ()
//	g_io_channel_unref (ioc);
	rt->ex.command.to_child = ioc;

	if ((flags & E2_RUN_SHOW) && e2_option_bool_get ("fileop-show"))
	{
		gchar *message = g_strconcat ((flags & E2_RUN_EXT) ? "sh>" : ">", command, NULL);
		e2_output_print (rt->current_tab, message, rt->pidstr, FALSE,
			"bold", "cmand", NULL);
		g_free (message);
		message = g_strdup_printf ("  (%d)\n", pid);
		e2_output_print (rt->current_tab, message, rt->pidstr, FALSE,
			"small", "cmand", NULL);
		g_free (message);
	}

	usleep (100000);	//there may be some immediate messages to process before we want SIGCHILD
	e2_command_unblock_childsignal ();

	return pid;
}
/**
@brief run command @a command synchronously (block until it's finished)
This sets the executed command's CWD to curr_view->dir
Expects BGL to be closed
@param command the command string, used in messages, UTF-8
@param args pointer to command arguments array, each localised
@param cwd path of directory to use as CWD for the command, UTF-8
@param flags indicators of how the command is to be handled e.g.
 whether to show @a command and its pid in the output pane,
 whether command is to be run in external shell

@return the exit code of the command, > 0 if error
*/
static gint _e2_command_run_sync (gchar *command, gchar **args, const gchar *cwd,
	E2_RunFlags flags)
{
	printd (DEBUG, "synchronously run command %s", command);
	//we dont get real pid's from static commands, so we fake by using -ve ones
	//in simple sequence
	static glong fakepid = 0;
	fakepid--;

	gchar *message;
	E2_TaskRuntime *rt;

	//keep trying this until data set fn not busy
	//the child-signal handler will not be able to find this data, due to fake pid
	while ((rt = e2_task_set_data (fakepid, E2_TASKTYPE_SYNC, command)) == NULL);

	if (rt == GINT_TO_POINTER (1))
	{
		//CHECKME warn user ?
		fakepid++;
		return 1;
	}
	//but mark-names starting with "-" are confusing so substitute "s"
	*(rt->pidstr) = 's';
	rt->flags = flags;

	if ((flags & E2_RUN_SHOW) && e2_option_bool_get ("fileop-show"))
	{
		gchar *message = g_strconcat ((flags & E2_RUN_EXT) ? "sh>" : ">", command, NULL);
		e2_output_print (rt->current_tab, message, rt->pidstr, FALSE,
			"bold", "cmand", NULL);
		g_free (message);
		message = g_strdup_printf ("  (%s)\n", rt->pidstr);
		e2_output_print (rt->current_tab, message, rt->pidstr, FALSE,
			"small", "cmand", NULL);
		g_free (message);
		WAIT_FOR_EVENTS;
	}

	if (cwd == NULL)
		cwd = curr_view->dir;
//	GError *error;
	gboolean success;
	gint exit;
	gchar *sout = NULL, *serr = NULL;
	gchar *local;
#ifdef E2_SU_ACTIONS
	gchar **envp = NULL;
	if (flags & E2_RUN_AS)
	{
		if (!e2_task_become_user (flags, (gpointer*)&envp))
		{
			printd (ERROR, "Could not change to alternate user");
			success = FALSE;
			rt->status = E2_TASK_FAILED;
			rt->ex.command.exit = 2;
		}
	}
#endif

#ifdef E2_VFS
	//for a non-native place, there is no analog for curr_view->dir, so go elsewhere
	//all args need to have been conformed, by the command interpreter
	local = (curr_view->spacedata == NULL) ?
		F_FILENAME_TO_LOCALE (cwd) : curr_view->spacedata->workplace;
#else
	local = F_FILENAME_TO_LOCALE (cwd);
#endif
	e2_command_block_childsignal ();

	rt->status = E2_TASK_RUNNING;
	success = g_spawn_sync (local, args,
#ifdef E2_SU_ACTIONS
		envp,
#else
		NULL,
#endif
		G_SPAWN_SEARCH_PATH,
//DOES NOTHING		(GSpawnChildSetupFunc) _e2_command_set_childdata, rt,
		NULL, NULL,
		&sout, &serr, &exit, NULL);	//&error);
	//no rt cleanup after error
	rt->status = (success) ? E2_TASK_COMPLETED : E2_TASK_FAILED;

	//CHECKME revert CWD to curr_view->dir if that still exists ?
	//FIXME this delay doesn't work properly
	usleep (100000);	//there may be some immediate messages to process before we want SIGCHILD
	e2_command_unblock_childsignal ();

	rt->ex.command.exit = exit;

#ifdef E2_SU_ACTIONS
	if (flags & E2_RUN_NOTAS)
	{
		if (!e2_task_revert_user (flags))
		{
			printd (ERROR, "Could not revert to normal user");
			//FIXME warn user about this
//			serr = g_strdup ();
		}
	}
#endif

	if (success && (flags & E2_RUN_SHOW) && sout != NULL && *sout != '\0')
	{
		e2_output_print (rt->current_tab, sout, rt->pidstr, FALSE, NULL);
	}
	if (success && serr != NULL && *serr != '\0')
	{
		e2_output_print_error (serr, FALSE);
	}
	if (sout != NULL)
		g_free (sout);
	if (serr != NULL)
		g_free (serr);
#ifdef E2_VFS
	if (curr_view->spacedata == NULL)
#endif
		F_FREE (local, cwd);

	if (success && (flags & E2_RUN_SHOW) && e2_option_bool_get ("fileop-show"))
	{
		gchar *shorter = e2_utils_str_shorten (command, 60, E2_DOTS_MIDDLE);
		message = g_strdup_printf ("%s%s (%s) %s '%d'\n",
			(flags & E2_RUN_EXT) ? "sh>" : ">", shorter, rt->pidstr,
			_("returned"), WEXITSTATUS (exit));
		g_free (shorter);
		e2_output_print (rt->current_tab, message, rt->pidstr, TRUE, "small",
			(WEXITSTATUS (exit) == 0) ? "posit" : "negat", NULL);
		g_free (message);
	}
	if (!success)
	{
		message = g_strdup_printf (_("Strange error: could not run '%s'"), command);
		e2_output_print_error (message, TRUE);
	}
#ifdef E2_OUTPUTSTYLES
	e2_output_clear_styles (rt->current_tab, rt->pidstr);
#endif
	return WEXITSTATUS (exit);
}
/**
@brief pipe utf-8 string @a message to process @a pid
The specified process must be a child of the current e2 instance
@param pid the id of the destination process, or 0 for the last-created child
@param message utf string to send to process @a pid
@return TRUE if the string was sent without error
*/
static gboolean _e2_command_send_to_pid (glong pid, gchar *message)
{
	gchar *errmsg;
	printd (DEBUG, "e2_command_send_to_pid (pid:%d,messge:%s)", pid, message);
	E2_TaskRuntime *rt;
	rt = (pid == 0) ?
		e2_task_find_last_running_child (FALSE):
		e2_task_find_running_task (pid);
	if (rt == NULL || rt->action)
	{
		errmsg = g_strdup_printf (_("The process with pid %ld is not our child"), pid);
		e2_output_print_error (errmsg, TRUE);
		return FALSE;
	}

	gsize size;
	GIOStatus stat;
	GError *error;
retry:
	error = NULL;
	//CHECKME what if the process doesn't understand UTF-8 ?
	stat = g_io_channel_write_chars (rt->ex.command.to_child, message, -1, &size, &error);
	if (stat == G_IO_STATUS_AGAIN)
	{
		if (error != NULL)
			g_error_free (error);
		goto retry;
	}
	if (error != NULL)
	{
		errmsg = g_strdup_printf (_("Failed writing to child: %s"), error->message);
		printd (WARN, errmsg);
		e2_output_print (rt->current_tab, errmsg, rt->pidstr, TRUE, E2_ERRORTAGS, NULL);
		g_error_free (error);
		g_free (errmsg);
		return FALSE;
	}
retry2:
	error = NULL;
	stat = g_io_channel_write_chars (rt->ex.command.to_child, "\n", 1, &size, &error);
	if (stat == G_IO_STATUS_AGAIN)
	{
		if (error != NULL)
			g_error_free (error);
		goto retry2;
	}
	if (error != NULL)
	{
		errmsg = g_strdup_printf (_("Failed writing to child: %s"), error->message);
		printd (WARN, errmsg);
		e2_output_print (rt->current_tab, errmsg, rt->pidstr, TRUE, E2_ERRORTAGS, NULL);
		g_error_free (error);
		g_free (errmsg);
		return FALSE;
	}
	else if (rt->flags & E2_RUN_SHOW)
	{	//signal what has been sent
		e2_output_print (rt->current_tab, message, rt->pidstr, TRUE, NULL);
	}
	return TRUE;
}
#endif //def E2_NEW_COMMAND ========================
/**
@brief kill child process with id @a pid
@param pid the id of the process we're looking for
@return TRUE if a child process with that pid was found and killed
*/
gboolean e2_command_kill_child (guint pid)
{
	E2_TaskRuntime *rt = e2_task_find_running_task ((glong)pid);
	if (!(rt == NULL || rt->action))
	{	//it's one of ours
		if (!kill ((pid_t) pid, SIGTERM))	//or SIGKILL
		{
			rt->pid = -2L;	//no more matching
			rt->status = E2_TASK_ABORTED;
			return TRUE;
		}
		e2_output_print_strerrno ();
	}
	return FALSE;
}
/**
@brief check whether process with id @a pid is running
@param pid the id of the process we're looking for
@return TRUE if a process with that pid is running
*/
gboolean e2_command_find_process (guint pid)
{
	pid_t result = waitpid (pid, NULL, WNOHANG);
	return (result == 0);	//will be -1 if finished before, result if finished now
}
/**
@brief update all relevant child foreground-tab pointers after output pane tab change
@param currenttab pointer to data for tab to be replaced
@param replacetab pointer to data for tab to be substituted for @a currenttab
@return
*/
void e2_command_retab_children (E2_OutputTabRuntime *currenttab,
	E2_OutputTabRuntime *replacetab)
{
	E2_TaskRuntime *rt;
	GList *member;
#ifndef E2_NEW_COMMAND
	e2_command_block_childsignal ();
#endif
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt != NULL
			&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
		{
			if (rt->current_tab == currenttab)
				rt->current_tab = rt->background_tab;
			else if (rt->background_tab == replacetab)
				rt->current_tab = currenttab;
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
#ifndef E2_NEW_COMMAND
	e2_command_unblock_childsignal ();
#endif
}
/**
@brief update tab ptrs of children using a deleted tab
@param currenttab the deleted tab's data struct
@param replacetab the replacement tab's data struct
@return
*/
void e2_command_retab2_children (E2_OutputTabRuntime *currenttab,
	E2_OutputTabRuntime *replacetab)
{
	E2_TaskRuntime *rt;
	GList *member;
#ifndef E2_NEW_COMMAND
	e2_command_block_childsignal ();
#endif
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt != NULL
			&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED)
			&& rt->background_tab == currenttab)
		{
			rt->background_tab = replacetab;
			rt->current_tab = replacetab;
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
#ifndef E2_NEW_COMMAND
	e2_command_unblock_childsignal ();
#endif
}
/**
@brief clear pending commands from queue
This is an action, results shown in the current output tab
@param from UNUSED the activated widget
@param art UNUSED runtime data sent to this action
@return TRUE if anything pending is removed
*/
gboolean e2_command_clear_pending (gpointer from, E2_ActionRuntime *art)
{
	E2_TaskRuntime *rt;
	GList *member;
	gboolean retval = FALSE;
#ifndef E2_NEW_COMMAND
	e2_command_block_childsignal ();
#endif
	//to avoid iteration issues, a 2-stage process ...
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt->status <= E2_TASK_QUEUED)	//Q or none
		{
			//rt->pidstr not set yet for queued items
			if (rt->action)
			{
				g_free (rt->ex.action.currdir);
				g_free (rt->ex.action.othrdir);
				if (rt->ex.action.names != NULL)
					e2_fileview_clean_selected (rt->ex.action.names);
				if (rt->ex.action.rt_data != NULL)
					g_free (rt->ex.action.rt_data);
			}
			else
			{
				g_free (rt->ex.command.command);
				g_free (rt->ex.command.currdir);
#ifdef E2_COMMANDQ
				g_free (rt->ex.command.othrdir);
				if (rt->ex.command.names != NULL)	//active pane selected items array
				{
//#ifndef USE_GLIB2_22
					g_ptr_array_foreach (rt->ex.command.names, (GFunc)g_free, NULL);
//#endif
					g_ptr_array_free (rt->ex.command.names, TRUE);
				}
				if (rt->ex.command.othernames != NULL)	//inactive pane selected items array
				{
//#ifndef USE_GLIB2_22
					g_ptr_array_foreach (rt->ex.command.othernames, (GFunc)g_free, NULL);
//#endif
					g_ptr_array_free (rt->ex.command.othernames, TRUE);
				}
# ifdef E2_VFSTMP
	//FIXME clear "fstab" data for 2 panes if command is queued and needs CWD or has %fFdDpP
# endif
#endif
			}
			DEALLOCATE (E2_TaskRuntime, rt);
			member->data = NULL;
			retval = TRUE;
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
	pthread_mutex_lock (&task_mutex);
	app.taskhistory = g_list_remove_all (app.taskhistory, NULL);
	pthread_mutex_unlock (&task_mutex);
#ifndef E2_NEW_COMMAND
	e2_command_unblock_childsignal ();
#endif
	return retval;
}
/**
@brief list queued pending commands
This is an action, results shown in the current output tab
@param from UNUSED the activated widget
@param art UNUSED runtime data sent to this action
@return TRUE if there is any pending command
*/
static gboolean _e2_command_list_pending (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "e2_command_list_pending (data:)");
	//find start of waiting item(s) in Q
	E2_TaskRuntime *rt;
	GList *member;
	//quick check for something relevant
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt->status <= E2_TASK_QUEUED)
			break;
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
	if (member == NULL)
	{
		e2_output_print (&app.tab, _("nothing is waiting"), NULL, TRUE, NULL);
		return FALSE;
	}
	gchar *srcdir, *shortdir, *message;
	message = _("task");
	e2_output_print (&app.tab, message, NULL, TRUE, "bold", "uline", NULL);
#ifndef E2_NEW_COMMAND
	e2_command_block_childsignal ();
#endif
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt != NULL && rt->status <= E2_TASK_QUEUED)
		{
			if (rt->action)
			{
				E2_ActionTaskData *atask = &rt->ex.action;
				srcdir = F_FILENAME_FROM_LOCALE (atask->currdir);
				shortdir = e2_utils_str_shorten (srcdir, CMDNAMELEN, E2_DOTS_START);
				const gchar *subject;
				if (g_str_has_prefix (atask->action->name, _A(6)))
				{	//this was a "file-action"
					subject =
					(atask->rt_data == NULL || *((gchar *)atask->rt_data) == '\0') ?
						_("<selected items>") : (gchar *)atask->rt_data;
				}
				else
					subject = "";	//results in a redundant space
				message = g_strdup_printf ("%s %s @ %s",
					atask->action->name, subject, shortdir);
				F_FREE (srcdir, atask->currdir);
			}
			else
			{
				shortdir =
				e2_utils_str_shorten (rt->ex.command.currdir, CMDNAMELEN, E2_DOTS_START);
				gchar *shortcmd =
				e2_utils_str_shorten (rt->ex.command.command, CMDNAMELEN, E2_DOTS_END);
				message = g_strdup_printf ("%s @ %s", shortcmd, shortdir);
				g_free (shortcmd);
			}
			e2_output_print (&app.tab, message, NULL, TRUE, NULL);
			g_free (shortdir);
			g_free (message);
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
#ifndef E2_NEW_COMMAND
	e2_command_unblock_childsignal ();
#endif
	e2_output_print_end (&app.tab, FALSE);

	return TRUE;
}
/**
@brief count the active child processes of this session
This counts child processes, if @a countcmds is TRUE, and actions (queued or
not) in the task history list and marked as currently-running, or (if
@a countpaused is TRUE, paused
@param countcmds TRUE to include all active commands in the count
@param countpaused TRUE to include paused actions in the count
@return the number of currently-running commands and actions
*/
guint e2_command_count_running_tasks (gboolean countcmds, gboolean countpaused)
{
	guint i = 0;
	E2_TaskRuntime *rt;
	GList *member;
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt != NULL
			&& (rt->action || (countcmds && !rt->action))
			&& (rt->status == E2_TASK_RUNNING || (countpaused && (rt->status == E2_TASK_PAUSED))))
			i++;
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
	return i;
}
/**
@brief list the active child processes (commands and actions) of the current session
This is an action, results shown in the current tab
@param from UNUSED the activated widget
@param art UNUSED runtime data sent to this action
@return TRUE if there are any children
*/
static gboolean _e2_command_list_children (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "_e2_command_list_children (data:)");
	if (e2_command_count_running_tasks (TRUE, TRUE) == 0)
	{
		e2_output_print (&app.tab, _("nothing is running"), NULL, TRUE, NULL);
		return FALSE;
	}
	gchar *srcdir, *shortdir, *message;
	message = _(" (pid)   task    directory");
	e2_output_print (&app.tab, message, NULL, TRUE, "bold", "uline", NULL);

	E2_TaskRuntime *rt;
	GList *member;
#ifndef E2_NEW_COMMAND
	e2_command_block_childsignal ();
#endif
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt != NULL
			&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
		{
			if (rt->action)
			{
				E2_ActionTaskData *atask = &rt->ex.action;
				srcdir= F_FILENAME_FROM_LOCALE (atask->currdir);
				shortdir = e2_utils_str_shorten (srcdir, CMDNAMELEN, E2_DOTS_START);
				const gchar *subject;
				if (g_str_has_prefix (atask->action->name, _A(6)))
				{	//this was a "file-action"
					subject =
					(atask->rt_data == NULL || *((gchar *)atask->rt_data) == '\0') ?
						_("<selected items>") : (gchar *)atask->rt_data;
				}
				else
					subject = "";	//results in a redundant space
				message = g_strdup_printf ("(%6s) %s %s @ %s", rt->pidstr,
					atask->action->name, subject, shortdir);
				F_FREE (srcdir, atask->currdir);
			}
			else
			{
				shortdir = e2_utils_str_shorten (rt->ex.command.currdir,
					CMDNAMELEN, E2_DOTS_START);
				message = g_strdup_printf ("(%6s) %s @ %s", rt->pidstr,
					rt->ex.command.command, shortdir);
			}

			e2_output_print (&app.tab, message, NULL, TRUE, NULL);
			g_free (shortdir);
			g_free (message);
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
#ifndef E2_NEW_COMMAND
	e2_command_unblock_childsignal ();
#endif
	e2_output_print_end (&app.tab, FALSE);

	return TRUE;
}
/**
@brief list the command history of the current session
This is an action, results shown in the current output tab
@param from UNUSED the activated widget
@param art UNUSED data sent to this action
@return TRUE if there is a history
*/
static gboolean _e2_command_list_history (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "e2_command_list_history (data:)");
	gchar *srcdir, *shortdir, *message;
	message = _("task @ directory || result");
	e2_output_print (&app.tab, message, NULL, TRUE, "bold", "uline", NULL);

	E2_TaskRuntime *rt;
	GList *member;
#ifndef E2_NEW_COMMAND
	e2_command_block_childsignal ();
#endif
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *) member->data;
		if (rt != NULL && rt->status >= E2_TASK_COMPLETED)
		{
			if (rt->action)
			{
				E2_ActionTaskData *atask = &rt->ex.action;
				srcdir = F_FILENAME_FROM_LOCALE (atask->currdir);
				shortdir = e2_utils_str_shorten (srcdir, CMDNAMELEN, E2_DOTS_START);
				const gchar *subject;
				if (g_str_has_prefix (atask->action->name, _A(6)))
				{	//this was a "file-action"
					subject =
					(atask->rt_data == NULL || *((gchar *)atask->rt_data) == '\0') ?
						_("<selected items>") : (gchar *)atask->rt_data;
				}
				else
					subject = "";	//this results in a redundant space in the line
				message = g_strdup_printf ("%s %s @ %s || %s",
					atask->action->name, subject, shortdir,
					(atask->result) ? _("OK") : _("error"));
				F_FREE (srcdir, atask->currdir);
			}
			else
			{
				E2_CommandTaskData *ctask = &rt->ex.command;
				shortdir = e2_utils_str_shorten ((gchar *)ctask->currdir,
					CMDNAMELEN, E2_DOTS_START);
				gchar *shortcmd =
				e2_utils_str_shorten (rt->ex.command.command, CMDNAMELEN, E2_DOTS_END);
				if (ctask->exit == 0)
					message = g_strdup_printf (
					"%s @ %s || %s", shortcmd, shortdir, _("OK"));
				else
					message = g_strdup_printf (
					"%s @ %s || %s %d", shortcmd, shortdir, _("error"), ctask->exit);
				g_free (shortcmd);
			}

			e2_output_print (&app.tab, message, NULL, TRUE, NULL);
			g_free (shortdir);
			g_free (message);
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
#ifndef E2_NEW_COMMAND
	e2_command_unblock_childsignal ();
#endif
	e2_output_print_end (&app.tab, FALSE);

	return TRUE;
}
/**
@brief set or display value of @a variable stored in variables list
If @a value is "" or NULL, any matching variable is removed from the list
If @a var_name is "", any variable whose value is @a value is displayed.
If @a var_name and @a value are both "", all variables are displayed.
@param var_name utf8 variable-name string
@param value utf8 variable-value string
@return
*/
static void _e2_command_handle_variable_value (gchar *var_name, gchar *value)
{
	gchar *this;
	GList *member;
	g_strstrip (var_name);
	if (*var_name == '\0')	//display
	{
		if (*value == '\0')
		{	//just "="
			for (member = variables; member != NULL; member = member->next)
				e2_output_print (&app.tab, (gchar *)member->data, NULL, TRUE, NULL);
			e2_output_print_end (&app.tab, FALSE);
		}
		else
		{
			this = g_strconcat ("=", value, NULL);
			for (member = variables; member != NULL; member = member->next)
			{
				if (g_str_has_suffix ((gchar*)member->data, this))
				{
					e2_output_print (&app.tab, (gchar *)member->data, NULL, TRUE, NULL);
					break;
				}
			}
			g_free (this);
		}
	}
	else	//set or clear
	{
		gchar *prefix = g_strconcat (var_name, "=", NULL);
		if (*value != '\0')
		{
			gchar *v = e2_utils_pass_whitespace (value);
			if (v == NULL)
				v = value;

			//remove double-quotes but not single ones
			if (*v == '"')
			{
				g_strchomp (v);	//too bad about intended trailing whitespace
				gint len = strlen (v) - sizeof(gchar);
				if (*(v + len ) == '"')
				{
					len -= sizeof(gchar);
					memmove (value, v + sizeof(gchar), len);
					*(value + len) = '\0';
				}
			}
		}

		for (member = variables; member != NULL; member = member->next)
		{
			this = (gchar *)member->data;
			if (g_str_has_prefix (this, prefix))
			{
				g_free (this);
				if (*value == '\0')
					variables = g_list_delete_link (variables, member);
				else
					member->data = g_strconcat (prefix, value, NULL);
				return;
			}
		}
		this = g_strconcat (prefix, value, NULL);
		variables = g_list_append (variables, this);
	}
}
/**
@brief get value of @a var_name if that's a stored variable
@param var_name utf string which may be, or may have as its prefix, a stored variable
@param tailstore store for pointer to 'unconsumed' remainder of @a var_name, or NULL to match whole of @a var_name
@return pointer to value string, or NULL if none found
*/
const gchar *e2_command_get_variable_value (gchar *var_name, const gchar **tailstore)
{
	GList *member;
	for (member = variables; member != NULL; member = member->next)
	{
		const gchar *this, *assigner;

		this = (const gchar *)member->data;
		assigner = strchr (this, '='); //always succeeds
		if (strncmp (var_name, this, (assigner-this)) == 0)
		{
			if (tailstore != NULL)
			{
				*tailstore = var_name + (assigner-this); //maybe empty
				return (++assigner);
			}
		}
	}
	return NULL;
}
/**
@brief check whether @a command has any "modifiers" and skip leading whitespace
This may be called more than once for any command
@param command UTF-8 command string
@param macros pointer to flag, set FALSE if leading '!' found
@param flags pointer to other bit-flags, to be updated according to contents of @a command
@return integer, the byte-offset into @a command where the next char to be processed is
*/
static gint _e2_command_run_checks (gchar *command, gboolean *macros,
	E2_RunFlags *flags)
{
	gint retval = 0;
	gboolean done = FALSE;
	E2_RunFlags _flags = *flags;

	//checks here are all for single-char characters, no need for utf functions
	while (!done)
	{
		switch (command[0])
		{
			case ' ':	//skip leading whitespace
			case '\t':
				break;
			case '!':
				*macros = FALSE;
				break;
			case '>':
				_flags |= E2_RUN_EXT;
				break;
			case '|':
				_flags |= E2_RUN_SYNC;
				break;
			default:
				done = TRUE;
				break;
		}
		if (!done)
		{
			retval++;
			command++;
		}
	}

	if ((_flags & E2_RUN_EXT) == 0)
	{	//force external shell for commands that we know must go to shell (no cd's)
		//(redirection chars etc)
		//this will be corrected later for <pseudo-actions>
		gchar *s = command;
		if (*s == 'c')
		{
			s++;
			if (*s == 'd')
			{
				s++;
				if (*s == ' ' || *s == '\t')
				{
					*flags = _flags;
					return retval;
				}
			}
		}
		if ((s = e2_utils_bare_strchr (command, '>')) != NULL
			&& s > command && *(s+1) != '\0')
			_flags |= E2_RUN_EXT;
		else if ((s = e2_utils_bare_strchr (command, '|')) != NULL
			&& s > command && *(s+1) != '\0')
			_flags |= E2_RUN_EXT;
		else if ((s = e2_utils_bare_strchr (command, '<')) != NULL
			&& s > command && *(s+1) != '\0')
			_flags |= E2_RUN_EXT;
		else if ((s = e2_utils_bare_strchr (command, '`')) != NULL
				&& (s = e2_utils_bare_strchr (s+1, '`')) != NULL)
			_flags |= E2_RUN_EXT;
		else if ((s = e2_utils_bare_strchr (command, '&')) != NULL)
		{
			gchar *p = s + 1;
			if (*p == '&')
				p++;
			if (e2_utils_pass_whitespace (p) != NULL)
				_flags |= E2_RUN_EXT;
			else
			{	//trailing &[&]
				_flags &= ~E2_RUN_SHOW;
				*s = '\0';
			}
		}
	}
	*flags = _flags;
	return retval;
}
/**
@brief run a single command @a raw, after interpreting its contents
Expects BGL to be closed
This function does not set or use CWD, but downstream functions for commands
(not actions) use curr_view->dir
Command modifiers (!>&) are interpreted.
$... variables are interpreted
%... macros are expanded unless the "no-expand" modifier '!' is present at
the start of @a raw.
Any 'pid:' directive is interpreted. The 'pid' string must describe a base 10 integer.
Any "\" that doesn't follows another "\" is simply removed.
Any "~" followed by "/" is interpreted as $HOME, as is a "~" by itself.
If * or ? is in any argument, that will be expanded, if possible, into a
series of matching item-names in the active directory.
"cd" commands that are not run in a separate shell just change the
dir shown in the active pane. In that case the new dir must be
separated by one or more ' ' chars (ie no tabs), or "cd" alone is
interpreted as "cd $HOME".
Command arguments for must be separated by one or more ' ' chars (ie no tabs)
NOTE used to return pid of process running the command (if any)
@param raw utf-8 command string
@param cwd utf8 path string, maybe with trailer, to use as (native) CWD when running @a command, NULL for curr_view->dir
@param range flags for the scope of the command
@param from the widget where @a raw was initiated, used for any action in @a raw

@return 0 for success, >0 for error
*/
static gint _e2_command_run_single (gchar *raw, const gchar *cwd,
	E2_CommandRange range, gpointer from)
{
	//FIXME more consistent returned value
	//CHECKME should variables be volatilized for threaded usage ?
#ifdef E2_VFSTMP
//BIG FIXME handling command arguments etc when active pane is vdir
#endif
	static gchar *prior_cmd = NULL;	//for preventing recursive alias substitution
	gchar *command1, *command2, *freeme, *freeme2;
	gint result;
	gboolean free1 = FALSE;
	gboolean expand_macros = TRUE;	//default
	E2_RunFlags flags = E2_RUN_SHOW; //ditto

	//find how the command is to be run
	command1 = raw + _e2_command_run_checks (raw, &expand_macros, &flags);
	//(E2_RUN_SYNC and E2_RUN_SHOW flags will be overridden later, if (range & E2_COMMAND_RANGE_FILE_ACTION)

	//if command begins with a variable, decode that, so we can check for command or action etc
	if (*command1 == '$'
		&& (prior_cmd == NULL || strstr (command1, prior_cmd) == NULL))
	{
		command2 = e2_utils_find_whitespace (command1 + sizeof(gchar));
		freeme2 = (command2 == NULL) ?
			g_strdup (command1) : g_strndup (command1, command2 - command1);

		if (e2_utils_get_variable (&freeme2))
		{
			command1 = (command2 == NULL) ?
				freeme2 : g_strconcat (freeme2, command2, NULL);

			if (e2_utils_bare_strchr (command1, ';') != NULL)
			{	//now it's a joined command
				//it may have the variable that's just been replaced,
				//in which case we must prevent a recursive loop
				if (prior_cmd != NULL)
					g_free (prior_cmd);
				prior_cmd = g_strdup (freeme2);

				result = e2_command_run (command1, range, from
#ifdef E2_COMMANDQ
				, TRUE
#endif
				);
				g_free (freeme2);
				if (command1 != freeme2)
					g_free (command1);
				g_free (prior_cmd);
				prior_cmd = NULL;
				return result;
			}
			if (freeme2 != command1)
			{
				g_free (freeme2);
				freeme2 = command1;
			}
			//check command operation mode again, in case variable changed it
			command1 = freeme2 + _e2_command_run_checks (freeme2,
				&expand_macros, &flags);
		}
		//freeme2 is newly-allocated, command1 is inside freeme2
	}
	else
		freeme2 = NULL;

	//replace any alias if so desired
	if (//CHECKME range != E2_COMMAND_RANGE_FILE_ACTION //this is not an action on selected items
		//&& expand_macros	//no instruction to not expand macros
		//&&
		e2_option_bool_get ("command-use-aliases")	//alias use is expected
		&& (prior_cmd == NULL || strstr (command1, prior_cmd) == NULL)	//ok to recurse aliases
	)
	{
		freeme = e2_alias_apply (command1, &app.aliases);
		//alias may have substituted a joined command
		if (e2_utils_bare_strchr (freeme, ';') != NULL)	//if always ascii ;, don't need g_utf8_strchr()
		{
			//one or more elements of the joined command may have the alias that's
			//just been replaced, in which case we must prevent a recursive loop
			command2 = e2_utils_find_whitespace (command1);
			gint len = (command2 == NULL) ? strlen (command1) : (command2-command1);
			if (prior_cmd != NULL)
				g_free (prior_cmd);
			prior_cmd = g_strndup (command1, len);
			//re-apply any modifier(s)
			if ((flags & (E2_RUN_EXT | E2_RUN_SYNC)) || !expand_macros)
			{
				command2 = strstr (raw, command1);
				if (command2 > raw)
				{
					gchar c = *command2;
					*command2 = '\0';
					freeme2 = e2_utils_strcat (raw, freeme);
					*command2 = c;
					g_free (freeme);
					freeme = freeme2;
					freeme2 = NULL; //don't free this later
				}
			}
			if ((flags & E2_RUN_SHOW) == 0)
			{
				freeme2 = freeme;
				freeme = e2_utils_strcat (freeme, "&");
			}
			//go restart the command process
			result = e2_command_run (freeme, range, from
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			g_free (freeme);
			if (freeme2 != NULL)
				g_free (freeme2);
			g_free (prior_cmd);
			prior_cmd = NULL;
			return result;
		}
		//check command operation mode again, in case alias changed it
		command1 = freeme + _e2_command_run_checks (freeme,
			&expand_macros, &flags);
	}
	else
		freeme = NULL;

	E2_Action *action = e2_action_check (command1);
	gboolean command_is_action = (action != NULL);
	if (command_is_action)
	{
		if (action->exclude & E2_ACTION_EXCLUDE_ACCEL)
		{
			gchar *msg = g_strdup_printf (_("Cannot run '%s'"), command1);
			e2_output_print_error (msg, TRUE);
			if (freeme != NULL)
				g_free (freeme);
			if (freeme2 != NULL)
				g_free (freeme2);
			return 1;
		}
		flags &= ~E2_RUN_EXT;	//over-ride any explicit or applied flag
		//set the correct action-range according to whether the action name
		//begins with "file" and has no argument
		if (g_str_has_prefix (command1, _A(6)))
		{
			command2 = e2_utils_find_whitespace (command1);
			if (command2 == NULL || e2_utils_pass_whitespace (command2) == NULL)
				//CHECKME special-case file.du with no RANGE_FILE_ACTION ?
				range |= E2_COMMAND_RANGE_FILE_ACTION;
		}
		else if (range & E2_COMMAND_RANGE_FILE_ACTION)
		{
			range &= ~E2_COMMAND_RANGE_FILE_ACTION;
			range |= E2_COMMAND_RANGE_DEFAULT;
		}
	}
//#ifdef E2_NEW_COMMAND
	else	//for commands, relative (and non-absolute?) paths in the command need to be interpreted
	{
		if (g_str_has_prefix (command1, "./") || g_str_has_prefix (command1, "../"))
		{
			command2 = e2_utils_find_whitespace (command1);
			if (command2 != NULL)
				*command2 = '\0';

			gchar *s1 = e2_utils_translate_relative_path (
#ifdef E2_VFSTMP
			//FIXME which CWD ?
			OR should we "get" and run an executable in an active-pane vdir
			or some dir realtive to that?
#else
			curr_view->dir,
#endif
			command1);
			//strip trailer from the expansion
			*(s1 + strlen (s1) - sizeof(gchar)) = '\0';

			if (command2 != NULL)
			{
				*command2 = ' ';	//any whitespace will do
				command2 = g_strconcat (s1, command2, NULL);
				g_free (s1);
			}
			else
				command2 = s1;

			command1 = command2;
			free1 = TRUE;
		}

		if (range & E2_COMMAND_RANGE_FILE_ACTION)
		{
/*			CHECKME is this of any real use ? it's intended mainly for actions
			if (e2_option_bool_get ("fileop-background"))
				flags &= ~E2_RUN_SYNC;
			else
				flags |= E2_RUN_SYNC;
*/
			//override any show flag in raw or alias
			if (e2_option_bool_get ("fileop-show"))
				flags |= E2_RUN_SHOW;
			else
				flags &= ~E2_RUN_SHOW;
		}
	}
//#endif

	//if appropriate, arrange to append the selected item(s),
	//as the (or additional) command argument(s)
	if (range & E2_COMMAND_RANGE_FILE_ACTION	//this is an action on selected items
		&& expand_macros	//no instruction to not expand macros
		&& (!command_is_action || g_str_has_prefix (command1, _A(6))) //command or file action
		//there is no filelist macro already in the command
//		//FIXME support macros other than with english letters
		&& ((command2 = strchr (command1, '%')) == NULL
			||(strstr (command2, "%f") == NULL
			&& strstr (command2, "%F") == NULL
			&& strstr (command2, "%p") == NULL
			&& strstr (command2, "%P") == NULL
#ifdef E2_BADQUOTES
			&& strstr (command2, "%e") == NULL
			&& strstr (command2, "%E") == NULL
#endif
			  )
		  )
	   )
		command2 = g_strconcat (command1, " %f", NULL);	//CHECKME %p ?
	else
		command2 = g_strdup (command1);

	if (freeme != NULL)
		g_free (freeme);
	if (freeme2 != NULL)
		g_free (freeme2);
//#ifdef E2_NEW_COMMAND
	if (free1 && command1 != freeme)
		g_free (command1);
//#endif

	//newly-allocated command2 is now the focus ..
	command1 = command2; //vars & wildcards processing moved below

	gboolean command_is_cd;
	//if command is: cd [%]%[f|e|p], when updir '../' line is selected, that item
	//will not be included in reported selected item[s], so, before doing macros ...
	//CHECKME also do this for commands other than "cd"
	if (g_str_has_prefix (command1, "cd"))
	{
		command2 = command1 + 2;
		if (*command2 == '\0')
			command_is_cd = TRUE;
		else if (*command2 == ' ' || *command2 == '\t')
		{
			command_is_cd = TRUE;
			//FIXME support macros other than with english letters
			if (strstr (command2, "%f") != NULL || strstr (command2, "%p") != NULL
#ifdef E2_BADQUOTES
			|| strstr (command2, "%e") != NULL
#endif
				)
			{
				FileInfo *info = e2_fileview_get_selected_first_local (curr_view, TRUE);
				if (info != NULL //something selected
					&& !strcmp (info->filename, ".."))
				{
					g_free (command1);
					command1 = g_strdup ("cd ..");	//no translation
					expand_macros = FALSE;	//avoid error message
				}
			}
		}
		else
			command_is_cd = FALSE;
	}
	else
		command_is_cd = FALSE;
	//expand macros, if wanted
	if (expand_macros)
	{
		command2 = e2_utils_expand_macros (command1, NULL);
		if (command2 == NULL)
		{
			e2_output_print_error (_("Failed to expand macros"), FALSE); //current tab
			result = 1;
			goto cleanup;
		}
		else if (command2 == GINT_TO_POINTER(1))
		{	//the user cancelled in a prompt macro
			result = 1;
			goto cleanup;
		}
		g_free (command1);
		command1 = command2;
	}

	//replace ~ and $variable[s]
	//CHECKME replace only internal vars in non-action argument[s], saves some
	//duplication & some external[s] might change when command process is forked
	freeme = command1;
	command1 = e2_utils_replace_vars (freeme, command_is_cd);
	g_free (freeme);

	if ((flags & E2_RUN_EXT) == 0)
	{
		//want to process external variable in current shell ?
		if (g_str_has_prefix (command1, "set"))
		{
			command2 = e2_utils_pass_whitespace (command1 + 3);
			if (command2 != NULL && command2 > command1 + 3)
			{
				command2 = e2_utf8_to_locale (command2);
				if (command2 != NULL)
				{
					gchar *value = e2_utils_find_whitespace (command2);
					if (value != NULL)
					{
						*value = '\0';
						value = e2_utils_pass_whitespace (value + 1);
						if (value != NULL && *value == '=')
							value = e2_utils_pass_whitespace (value + 1);
					}
					else
					{
						value = strchr (command2, '=');
						if (value != NULL)
						{
							*value = '\0';
							value = e2_utils_pass_whitespace (value + 1);
						}
					}

					if (value != NULL)
					{
						gchar *clean = e2_utils_unquote_string (value);
						if (G_LIKELY(clean != NULL))
						{
							value = e2_utf8_unescape (clean, ';'); //maybe these escaped in unquoted value
							g_free (clean);
							if (G_LIKELY(value != NULL))
							{
								result = (g_setenv (command2, value, TRUE)) ? 0 : 1;
								g_free (value); //but not NULL
							}
						}
						else
							value = NULL;
					}
					if (value == NULL)
					{
						g_unsetenv (command2);
						result = 0;
					}
					g_free (command2);
				}
				else
					result = 1;

				goto cleanup;
			}
			else
			{
				flags |= E2_RUN_EXT;	//no internal bare 'set' capability
			}
		}
		else //want to process any internal variable ?
			if ((command2 = e2_utils_bare_strchr (command1, '=')) != NULL)
		{
			*command2 = '\0';
			//ensure not "=" in a command parameter
			gchar *s = e2_utils_find_whitespace (command1);
			if (s != NULL)
				s = e2_utils_pass_whitespace (s+1);
			if (s == NULL)
			{
				_e2_command_handle_variable_value (command1, command2+1);
				return 0;
			}
			*command2 = '=';
		}
	}

	//interpret any 'pid:' directive
/*	the 'pid' string must describe a base 10 integer, no
	bigger than the max integer for the current system

	i.e. should be 5 bytes long (16385) for 32-bit systems,
	10 bytes for 64-bit systems
#if sizeof(gint) == 4
# define E2_PID_SIZE 5
#else
# define E2_PID_SIZE 10
#endif
*/
	command2 = NULL;
	if (range & E2_COMMAND_RANGE_TOCHILD)
	{
		freeme2 = e2_utf8_unescape (command1, ' ');
		result = (_e2_command_send_to_pid (0, freeme2)) ? 0 : 1;
		g_free (freeme2);
		//don't want transmitted message in command history
		if (GTK_IS_ENTRY (from))
		{
			GtkWidget *parent =
#ifdef USE_GTK2_14
				gtk_widget_get_parent (GTK_WIDGET(from));
#else
				GTK_WIDGET(from)->parent;
#endif
#ifdef USE_GTK3_0
			if (GTK_IS_COMBO_BOX (parent))
#else
			if (GTK_IS_COMBO_BOX_ENTRY (parent))
#endif
				e2_combobox_clear_value (parent, command1, TRUE);
		}
		goto cleanup;
	}
	if (//(range & E2_COMMAND_RANGE_FILE_ACTION) == 0	//CHECKME why excluded ?
		//&&
		(command2 = strchr (command1, ':')) != NULL	//if always ascii ':', don't need g_utf8_strchr()
	)
	{
		if (command2 > command1)	// there was something ahead of the ':'
		{
			*command2 = '\0';
			gchar *end;
			glong i = strtol (command1, &end, 10);
			if (end == command2)
			{
				//the whole string was used in conversion, so it was a valid
				//number and nothing else
				freeme2 = e2_utf8_unescape (command2 + 1, ' ');
				printd (DEBUG, "sending '%s' to process '%s'", freeme2, command1);
				result = (_e2_command_send_to_pid (i, freeme2)) ? 0 : 1;	//+1 to skip the '\0'
				g_free (freeme2);
				//don't want transmitted message in command-history
				if (GTK_IS_ENTRY (from))
				{
					GtkWidget *parent =
#ifdef USE_GTK2_14
						gtk_widget_get_parent (GTK_WIDGET(from));
#else
						GTK_WIDGET(from)->parent;
#endif
#ifdef USE_GTK3_0
					if (GTK_IS_COMBO_BOX (parent))
#else
					if (GTK_IS_COMBO_BOX_ENTRY (parent))
#endif
					{
						*command2 = ':';
						e2_combobox_clear_value (parent, command1, TRUE);
					}
				}
				goto cleanup;
			}
			*command2 = ':';
		}
	}

#ifdef E2_SU_ACTIONS
	if (range & (E2_COMMAND_RANGE_USER | E2_COMMAND_RANGE_NOTUSER))
	{
		//embed context-id in flags
		flags |= range & (0xff << E2_COMMAND_RANGE_SHIFT);
		if (range & E2_COMMAND_RANGE_USER)
			flags |= E2_RUN_AS; //tell downstream to process user-context
		if (range & E2_COMMAND_RANGE_NOTUSER)
			flags |= E2_RUN_NOTAS;
	}
#endif
	//"cd" commands that are not run in a separate shell just change the dir in
	//the active pane. Note that 'cd <unquoted path containing &>' will incorrectly
	//have run_in_shell = TRUE
	if (command_is_cd && (flags & E2_RUN_EXT) == 0)
	{
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_USER)
		{
			if (!e2_task_become_user (flags, NULL))
			{
				result = E2_TASK_FAILED;
				goto cleanup;
			}
		}
#endif
		gint len = strlen (command1);
		if (len > 3) //'cd <path>'
		{
			command2 = e2_utf8_unescape (command1 + 3, ' ');
			e2_pane_change_dir (NULL, command2);	//command1 is UTF-8 or ASCII
			g_free (command2);
		}
		else //'cd' or 'cd '
			e2_pane_change_dir (NULL, "~");
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_NOTUSER)
		{
			if (!e2_task_revert_user (flags))
			{
				result = E2_TASK_INCOMPLETE;
				goto cleanup;
			}
		}
#endif
		result = 0;
		goto cleanup;
	}
	else if (command_is_action)
	{
		gchar *gap = e2_utils_find_whitespace (command1);
		if (gap != NULL)
		{
			*gap = '\0';
			gap = e2_utils_pass_whitespace (gap+1);
			if (gap != NULL)
			{
				if (expand_macros)
				{
					command2 = e2_utils_replace_wildcards (gap);
					free1 = (command2 != gap);
				}
				else
				{
					command2 = gap;
					free1 = FALSE;
				}
				gap = e2_utf8_unescape (command2, ' ');	//actions don't expect escaped whitespace
				if (free1)
					g_free (command2);
			}
		}
		//now gap points to args, or NULL
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_USER)
		{
			gboolean TODO_change_environment;
			if (!e2_task_become_user (flags, NULL))
			{
				result = E2_TASK_FAILED;
				goto cleanup;
			}
		}
#endif
		OPENBGL
		result = (e2_action_run_simple_from (command1, gap, from)) ? 0 : 1;
		CLOSEBGL
		if (gap != NULL)
			g_free (gap);
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_NOTUSER)
		{
			if (!e2_task_revert_user (flags))
				result = E2_TASK_INCOMPLETE;
		}
#endif
		goto cleanup;
	}

	//CHECKME handle commands via Q processor thread ?
	gchar **args;
	gint argc;
	if (flags & E2_RUN_EXT)
	{
		argc = 3;
		//construct freeable vector of args
		args = NEW (gchar*, 4);
		args[0] = g_strdup (shellcmd);
		args[1] = g_strdup ("-c");
		args[2] = g_strdup (command1);
		args[3] = NULL;
	}
	else
	{	//run in same shell
		command2 = e2_utils_replace_wildcards (command1);
		GError *error = NULL;
		//NB the glib 2.6 API doco is wrong about how this works !!
		g_shell_parse_argv (command2, &argc, &args, &error);
		if (command1 != command2)
			g_free (command2);
		if (error != NULL)
		{
			gchar *msg = g_strdup_printf (_("Failed parsing command '%s' - %s"),
				command1, error->message);
			e2_output_print_error (msg, TRUE);
			g_error_free (error);
			result = 1;
			goto cleanup;
		}
	}

//#ifndef E2_FILES_UTF8ONLY
//	if (!app.utf8_filenames)
//	if (app.pane1.view.converted || app.pane2.view.converted)
	//user might enter a command with ANY ENCODING, regardless of current dirs
//	{
		//localise the actual command strings
		gchar *local;
		gchar **tmp = args;
		while (*tmp != NULL)
		{	//we always take a copy and strfreev later
			local = D_FILENAME_TO_LOCALE (*tmp);
			g_free (*tmp);
			*tmp = local;
			tmp++;
		}
//	}
//#endif

	e2_utf8_set_name_conversion_if_requested ();

#ifdef E2_NEW_COMMAND
	result = _e2_command_fork (command1, args, cwd, flags);
#else
	result = (flags & E2_RUN_SYNC) ?
		_e2_command_run_sync (command1, args, cwd, flags):
		_e2_command_run_async (command1, args, cwd, flags);
#endif
//	if (free)
		g_free (command1);
	g_strfreev (args);

#ifdef E2_NEW_COMMAND
	return result;	//((result == 0) ? 0 : 1); //exit code, 0 = ok
#else
	if (flags & E2_RUN_SYNC)
		return result;	//exit code, 0 = ok
	else
		return ((result == 0) ? 1 : 0);	//pid, >0 = ok
#endif

cleanup:
	g_free (command1);
	return result;
}
/**
@brief action to send a string to last-started child process, if any
Various checks are performed, to decide whether it's ok to send the command.
The string may be empty.
@param from the entry which has the string to send
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2_command_send_to_lastchild (gpointer from, E2_ActionRuntime *art)
{
	gchar *msg;
	//get the command string
	const gchar *command_raw = gtk_entry_get_text (GTK_ENTRY (from));
	//quick exit
	if (command_raw == NULL)	//cannot happen ?
		return FALSE;
//	if (strchr (command_raw, ';') != NULL)
//		goto error;
	if (e2_task_find_last_running_child (FALSE) == NULL)
		goto error;
	gchar *command = g_strdup (command_raw);
	//assume that an action string is not really meant to send to running command
	if (e2_action_check (command) != NULL)
	{
		g_free (command);
		goto error;
	}
//	tochild = TRUE;
	//this is an action, can only return T/F, so we lose the result of the command
	_e2_command_run_single (command, NULL,
		E2_COMMAND_RANGE_DEFAULT | E2_COMMAND_RANGE_TOCHILD, from);
//	tochild = FALSE;
	//we don't want any history, just clear the text
	gtk_entry_set_text (GTK_ENTRY (from), "");
	g_free (command);
	return TRUE;

error:
	msg =
	g_strdup_printf (_("Cannot send \"%s\" to a child process"), command_raw);
	e2_output_print_error (msg, TRUE);
	return FALSE;
}

  /****************/
 /**** public ****/
/****************/

/**
@brief run command @a command after interpreting its contents, and with CWD @a workdir

@a command may be a single command or a series joined by ';' (any ';' inside
single- or double-parentheses is ignored).
Normally, commands are run with CWD set to curr_view->dir. Here, we borrow that
string temporarily.
The change of CWD will probably not persist for the duration of command execution
if the command is asynchronous.
Downstream expects BGL closed.

@param command allocated UTF-8 string containing the command(s)
@param workdir UTF-8 path string, maybe with trailer, to use as (native) CWD
 when running @a command, NULL for curr_view->dir
@param range flags for the scope of the command (may be over-ridden, for actions)
@param from pointer to widget where @a command was initiated, used for any action in @a command
#ifdef E2_COMMANDQ
? TRUE if ?
#endif

@return integer return code from last command executed, 0 = success, > 0 on error
*/
gint e2_command_run_at (gchar *command, const gchar *workdir, E2_CommandRange range,
	gpointer from)
{
//E2_VFSTMP workdir probably needs to become a VPATH*
	//CHECKME should these variables be volatilized for threaded usage ?
	if ((command == NULL) || (*command == '\0'))
		return 1;
	g_strchug (command);	//command arg may be a path with intended trailing whitespace
	if (*command == '\0')
		return 1;
	printd (DEBUG, "e2_command_run_at (command:%s,range:%d)", command, range);

//FIXME umount commands sometimes should get eject appended - but what about aliases ?
	//parse & handle joined commands here
	//in particular, ignore separators inside parentheses
	if (strchr (command, ';') == NULL)	//no joined commands
	{
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_USER)
			range |= E2_COMMAND_RANGE_NOTUSER; //revert user after this command
#endif
		return _e2_command_run_single (command, workdir, range, from);
	}

	//don't split joined commands if ">>" modifier-prefix is provided
	gchar *s = command;
	gboolean run_in_shell = FALSE, done = FALSE;
	while (*s != '\0')
	{
		switch (*s++)
		{
			case ' ':
			case '\t':
			case '!':
			case '|':
				break;
			case '>':
				if (*s == '>')
					run_in_shell = TRUE;
			default:
				done = TRUE;
				break;
		}
		if (done)
			break;
	}
	if (run_in_shell)
	{
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_USER)
			range |= E2_COMMAND_RANGE_NOTUSER; //revert user after this command
#endif
		return _e2_command_run_single (command, workdir, range, from);
	}

#ifdef E2_SU_ACTIONS
	gboolean first = TRUE, revert = FALSE;
#endif
	gint result, retval = 0;
	gchar *p;
	s = command;
	while ((p = e2_utils_bare_strchr (s, ';')) != NULL)
	{
		*p = '\0';
		if (e2_utils_pass_whitespace (p+1) == NULL) //stupid trailing ';' !
			break;
		//process this command
#ifdef E2_SU_ACTIONS
		if (range & E2_COMMAND_RANGE_USER)
		{
			if (first)
				first = FALSE;
			else
			{
				range &= ~E2_COMMAND_RANGE_USER;
				revert = TRUE;
			}
		}
#endif
		result = _e2_command_run_single (s, workdir, range, from);
		if (retval == 0) retval = result;
#ifdef E2_SU_ACTIONS
		if (revert)
		{
			range |= E2_COMMAND_RANGE_USER;
			revert = FALSE;
		}
#endif
		s = p + 1;	//resume from the next spot
	}

	//process the last (or only) command
#ifdef E2_SU_ACTIONS
	if (range & E2_COMMAND_RANGE_USER)
		range |= E2_COMMAND_RANGE_NOTUSER; //revert user after this command
#endif
	result = _e2_command_run_single (s, workdir, range, from);
	if (retval == 0) retval = result;
	return retval;
}
/* *
NOW a #define
@brief run command @a command, after interpreting its contents

@a command may be a single command or a series joined by ';'
(any ';' inside single- or double-parentheses is ignored)
NOTE used to return command's pid
@param command utf string containing the command(s)
@param range flags for the scope of the command (may be over-ridden, for actions)
@param from pointer to widget where @a command was initiated

@return integer return code from last command executed, 0 = success, > 0 on error
*/
/*gint e2_command_run (gchar *command, E2_CommandRange range, gpointer from)
{
	return (e2_command_run_at (command, NULL, range, from));
} */
/**
@brief display commands help document
The document name is hard-coded with the help-file name
defined at build-time
@return
*/
void e2_command_output_help (void)
{	//check whether there's still an alias for the help command
	//(and BTW get a copy of the string, which needs to be freeable for e2_command_run)
	gchar *help_cmd = _("help");
	gchar *command = e2_alias_apply (help_cmd, &app.aliases);
	if (!strcmp (help_cmd, command))
	{	//we can't find what the user has set for a help command
		//so use the default instead
		g_free (command);
		command = g_strconcat(_A(6),".",_A(111)," ",
			e2_option_str_get ("usage-help-doc"), " [commands]",NULL);	//file.view_at _I( helpfile titles not (yet?) translated
	}
	e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, curr_tab->scroll
#ifdef E2_COMMANDQ
	, FALSE
#endif
	);
	g_free (command);
}
/**
@brief register command-related actions
@return
*/
void e2_command_actions_register (void)
{
	//not action stuff, but this is a convenient spot for other init
	e2_cache_list_register ("internal-variables", &variables);

//#ifndef E2_NEW_COMMAND
	_e2_command_set_sigchild_handler ();
//#endif
//#ifdef E2_NEW_COMMAND
//	_e2_command_set_sigpoll_handler ();
//#endif

	struct passwd *pw;
	pw = getpwuid (getuid ());
	if (pw != NULL)
	{
		gchar *baseshell = g_path_get_basename (pw->pw_shell);
		shellcmd = g_find_program_in_path (baseshell); //LEAK
		g_free (baseshell);
		if (shellcmd == NULL)
			shellcmd = "/bin/sh";
	}
	else
		shellcmd = "/bin/sh";

	//setup waitpid() parameter, with check for glibc/kernel disconnect on WCONTINUED
	waitflags = WNOHANG
#ifdef WCONTINUED
			| WCONTINUED
#endif
			| WUNTRACED;
#ifdef WCONTINUED
	if (waitpid (-1, NULL, waitflags) < 0 && errno == EINVAL)
	{
		printd (DEBUG, "this kernel does not support WCONTINUED");
		waitflags = WNOHANG | WUNTRACED;
	}
#endif

#ifdef __linux__
# ifdef __WALL
//	waitflags = waitflags | __WALL | __WNOTHREAD; causes failure
	waitflags |= __WALL;
# endif
#endif
	E2_Action actions[] =
	{
	{g_strconcat(_A(2),".",_A(61),NULL), _e2_command_list_children,    FALSE,E2_ACTION_TYPE_ITEM,0, NULL, NULL},
	{g_strconcat(_A(8),".",_A(61),NULL), _e2_command_list_history,     FALSE,E2_ACTION_TYPE_ITEM,0, NULL, NULL},
	{g_strconcat(_A(15),".",_A(61),NULL),_e2_command_list_pending,     FALSE,E2_ACTION_TYPE_ITEM,0, NULL, NULL},
	{g_strconcat(_A(15),".",_A(36),NULL),e2_command_clear_pending,     FALSE,E2_ACTION_TYPE_ITEM,0, NULL, NULL},
	{g_strconcat(_A(1),".",_A(84),NULL), _e2_command_send_to_lastchild,FALSE,E2_ACTION_TYPE_ITEM,E2_ACTION_EXCLUDE_TOOLBAR | E2_ACTION_EXCLUDE_MENU, NULL, NULL},
	};
	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
}
/**
@brief register command-related options
@return
*/
void e2_command_options_register (void)
{
	//no screen rebuilds needed after any change to these options
	gchar *group_name = g_strconcat(_C(6),":",_C(26),NULL); //_("commands:misc"
	e2_option_str_register ("command-xterm", group_name, _("x terminal emulator:"),
		_("This is the external command/application that will be be run when emelFM2 is asked to open a terminal"),
		NULL, "xterm",
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("use-external-viewer", group_name, _("use external file-viewer"),
		_("If activated, the command entered below will be run to view file content, instead of launching the internal viewer"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC);
	e2_option_str_register ("command-viewer", group_name, _("viewer command:"),
		_("This is a command to run an external application for viewing file content.\n"
		"The first selected item will be supplied as the first argument"),    //CHECKME = this is wrong ??
		"use-external-viewer", "gview",
		E2_OPTION_FLAG_BASIC);
	e2_option_bool_register ("use-external-editor", group_name, _("use external file-editor"),
		_("If activated, the command entered below will be run to edit file content, instead of launching the internal editor"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC);
	e2_option_str_register ("command-editor", group_name, _("editor command:"),
		_("This is a command to run an external application for editing file content.\n"
		"The first selected item will be supplied as the first argument"),    //CHECKME = this is wrong ??
		"use-external-editor", "leafpad",
		E2_OPTION_FLAG_BASIC);
	e2_option_bool_register ("use-external-encoder", group_name, _("use external encoding converter"),
		_("If activated, the command entered below will be run, instead of using the internal conversion functions, to convert file character encoding when needed"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED);
	gchar *defcmd = g_strconcat ("enca -L %{(languages)@", _("File encoding:"),"} -x UTF-8 <%p", NULL);
	e2_option_str_register ("command-encoder", group_name, _("converter command:"),
		_("A command which runs an external application to convert text encoding to UTF-8"),
		"use-external-encoder", defcmd,
		E2_OPTION_FLAG_ADVANCED);
	g_free (defcmd);
	e2_option_bool_register ("command-use-aliases", group_name, _("use aliases"),
		_("This is a general switch to turn on/off alias handling for commands"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("transparent-dir-links", group_name, _("interpret 'relative' paths"),
		_("This enables correct interpretation of paths containing '..' etc. These might be typed in, or attached to a button, say"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
#ifndef E2_NEW_COMMAND
	e2_option_int_register ("command-watch-priority", group_name, _("watch priority"),
		_("The watch priority of commands influences how fast program\n"
		"output is read from i/o channels. A too-high priority might\n"
		"decrease gui responsiveness and break automatic scrolling.\n"
		"(note: negative values mean high priority, positive values mean low priority)"),
		NULL, 300, -200, 400,
		E2_OPTION_FLAG_ADVANCED);
#endif
	//these options relate to actions, not commands
	group_name = g_strconcat(_C(6),":",_C(14),NULL);  //_("commands:file operations"
	e2_option_bool_register ("task-timeout-checks", group_name, _("stop after timeout"),
	_("If activated, each file operation will be terminated if not finished within the time-interval set below"),
	NULL, FALSE,
	E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_int_register ("task-timeout-interval", group_name, _("timeout interval"),
	_("The interval (seconds) allowed to complete any file operation"),
	"task-timeout-checks", 20, 1, 3600,
	E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("confirm-delete", group_name, _("confirm any delete"),
		_("If activated, you will be asked for confirmation before actually deleting anything"), NULL, TRUE,
		E2_OPTION_FLAG_BASIC);
	e2_option_bool_register ("confirm-overwrite", group_name, _("confirm any overwrite"),
		_("If activated, you will be asked for confirmation before actually overwriting anything"), NULL, TRUE,
		E2_OPTION_FLAG_BASIC);
	e2_option_bool_register ("relative-symlinks", group_name, _("relative symlinks"),
		_("This gives each created symlink a relative path to its source, like '../../<otherbranch>/<file>', instead of a full path referenced to /"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
}
