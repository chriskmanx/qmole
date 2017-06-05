/* $Id: e2_fs_FAM_inotify.c 2746 2013-09-19 22:59:03Z tpgww $

Copyright (C) 2005-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/filesystem/e2_fs_FAM_inotify.c
@brief functions related to file-alteration monitoring using inotify on linux
*/
/**
\page inotify inotify operations

A connection to the kernel's inotify apparatus is established at session-start,
and closed at session-end. Some associated setup and cleanup happens, respectivley.

Whenever a filelist-directory is opened in the application, a "watch" is set
for that directory (except if it is already watched in the other pane). Similarly,
if the appropriate option is in effect, a watch is set on the application
config-data file.

Feedback from the kernel about any watch is detected by periodic polling (every
QPOLL_MSEC) of the kernel's event-reports-queue length. We don't use select() or
poll() because we don't want to block (in a separate thread), and we do want to
be able to abort/suspend without delay. After QUIET_SECONDS without any report,
the poll-timer is turned off, to be restarted by any subsequent gtk event on the
main window (TODO or any new inotify report).

All such feedback is logged as quickly as possible, and later processed according
to the context. A watch-specific timer is used to defer such processing.

When a report is receieved, and the corresponding timer is inactive, then we start
the timer with a short interval (IN_SHORTMSEC) to allow accumulation of any
further reports (which often come in blocks).

When the short timer ends, if refresh is enabled, initiate refresh for the subject,
clean up inotify data, and restart the timer with a longer interval (IN_LONGMSEC)
during which further reports will be logged but not processed. This is to prevent
rapid un-necessary repeating of refreshes.

When the long timer ends, if refresh is enabled, and if report(s) have been logged,
process them, clean up inotify data, and restart the long timer. If no report
has been logged, do not restart, pending further report.

When a timer ends and refresh is disabled, do not immediately clean up reports/
inotify data.

When refresh is enabled or requested, if short timer is running, do nothing. If
long timer is running, stop it, and treat as for end of long timer, as above.

Count(s) of logged event-reports of are accessed only during timer callbacks,
which should not overlap. So no protection against contemporary access is provided.
*/

#include "emelfm2.h"
#ifdef E2_FAM_INOTIFY

#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <string.h>
#include "e2_filelist.h"

//#define EXTRA_MESSAGES

//glibc 2.4 omits this ?
#ifndef IN_MASK_ADD
# define IN_MASK_ADD 0x20000000
#endif
/*
flags used when normal refreshing is in effect.
when monitoring a dir we ignore these inotify events:
IN_CLOSE_WRITE this should be covered by IN_MODIFY
IN_CLOSE_NOWRITE
IN_OPEN we're monitoring dirs and config file and don't care about opening
and we always get these, so don't need to ask
IN_UNMOUNT
IN_Q_OVERFLOW
IN_IGNORED (item no longer watched, likely due to monitor-cancellation, or
item deletion (or replacement))
*/
#define INDIR_FLAGS \
IN_ACCESS | IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | \
IN_CREATE | IN_DELETE | IN_DELETE_SELF | IN_MOVE_SELF
/* flags used when limited refreshing is in effect (i.e. no IN_ACCESS) */
#define INDIR_SHORT_FLAGS \
IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | \
IN_CREATE | IN_DELETE | IN_DELETE_SELF | IN_MOVE_SELF
/* flags used to supplement INDIR_SHORT_FLAGS to get back to full operation */
#define INDIR_XTRA_FLAGS IN_ACCESS

/*for the config file we're interested only in events that would trigger a
reload of that file, ie modification or creation. Not deletion, as a replacement
may not yet be available.
If something renames the file (whether or not it then creates a replacement version),
the renamed file will continue to be monotored unless IN_MOVED* are checked.
However, in normal course of events, the file is deleted and then replaced, the
deletion causes a IN_IGNORED report to be generated, and monitoring ceases to be
effective on the monitored inode - so no IN_CREATE report is generated, normally.
*/
#define INCFG_FLAGS \
IN_MODIFY | IN_MOVE_SELF

//timer interval between inotify queue polls
#define QPOLL_MSEC 350
//short timer interval (between any 'initial' event-report and refresh-request)
//a bit longer than QPOLL_MSEC
#define IN_SHORTMSEC 400
//long timer interval (between any refresh-request and another one, if needed)
#define IN_LONGMSEC 3000

/* Size (bytes) of kernel-space reports-queue which triggers transcription to
user-space. A queue can hold 16384 reports, each at least 16,and possibly up to
(16+MAX_NAME), bytes. So, to avoid queue overflow, we conservatively need to
transcribe before the size reaches 256k. However, we probably worry about memory
paging before the queue gets to that size. For now, use 128k */
#define INQUEUE_SIZE 131072

/* Size (bytes) of local buffer for transcribing inotify reports. When monitoring
a dir, each reported inotify_event struct ends with a gchar stub, for the item
name (without its path) So each such report is of indeterminate length. If there
is a name, it will have a trailing \0 and then be padded out to a multiple of 16
bytes (informative but irrelevant..).
NOTE that each relevant bit-flag for a reported item is sent as a separate report,
so copying 1000 items, say, would trigger 4000 reports !! but that's ok as the
buffer is re-cycled */
#define READ_BUFFER_SIZE 8192
/* Sometimes (e.g. for /tmp) there's a flood of reports and we need to curtail
the report-reading process so that a refresh can be done. This sets the approx.
max no. of received reports before triggering a refresh. Hitting this limit will
usually result in more queued reports to process after the dir in question ceases
to be monitored, but such reports are ignored.
*/
#define IN_BATCHLIMIT 1000

/*Max. no of sequential loops to check for anything pending in the inotify
report queue */
#define MAX_CHECKS 5

/* When looping to check for queued pending reports, we stop iterating if the
incremental size of the queue is less than GROWTH_FACTOR * 2^n, where n is the
0-based loop counter. This is essentially the approach taken by beagle */
#define GROWTH_FACTOR 32

//delay between sequential inotify checks
#define PAUSE_MICROSECONDS 2000

//the connection to inotify
static gint inotify_fd = 0;

//for deferring processing after any report is received from inotify
static guint target0_timerid = 0;
static guint target1_timerid = 0;
static guint target2_timerid = 0;
//for logging whether respective timers are in long or short phase
static volatile gint target0_timer_interval = 0;
static volatile gint target1_timer_interval = 0;
static volatile gint target2_timer_interval = 0;
//for distinguishing between types of IN_IGNORED reports
static gboolean suspend_IGNORE = FALSE;

extern volatile gint p1dirty, p2dirty, cfgdirty;
extern time_t last_work_time;

//array-columns enumerator
enum { REPORT_WD, REPORT_COUNT, REPORT_ROWS };
//FAMreq value for blocking use of a watch value, must be < 0
#define WD_BUSY -2
//array REPORT_WD value for an error
#define WD_ERR -1
//array REPORT_WD value for an unused row, intentionally the same as value from
//a failed inotify function-call
#define WD_UNUSED -1
/*store for logging reports and associated stuff for up to 3 watches
(1 or 2 pane-dirs and config file NOTE pane1 not necessarily target/row 0, pane2 not nec. target/row 1)
Rows are used as per the enum above*/
static gint watch_reports[3][REPORT_ROWS] = {{WD_UNUSED}, {WD_UNUSED}, {WD_UNUSED}};
//cached strings for each row of watch_reports array. Contains the path of
//respective watched item, used for checking path is still relevant when timer
//finishes. For speed, mManipulated with glibc funcs, not glib equivalents.
static gchar *watch_paths [3];
//local store for event-reports transferred from inotify queue
static gpointer inotify_buffer = NULL;
//path of monitored config-file, cached to avoid repeated create+free in timer callback
static gchar *config_file;

/* FIXME find a way for inotify to always trigger a queue-read to clear the
   inotify queue before it's full */

/**
@brief timer callback after any 'short' or 'long' interval between refresh requests
If event-report(s) have been logged, and if refresh is enabled, process the
event(s), clean up inotify data, and start or continue the long timer. If no
report has been logged, abort timer pending further reporting.
@param user_data pointerised enumerator 0..2, for row in watch_reports array

@return TRUE if this is a long timer callback, and refresh has been initiated while here
*/
static gboolean _e2_fs_FAM_inotify_timeout (gpointer user_data)
{
#ifdef EXTRA_MESSAGES
	const gchar *period = "un-assigned";
#endif
	gboolean more = FALSE; //default
	guint target_id = GPOINTER_TO_UINT (user_data);
	printd (DEBUG, "In _e2_fs_FAM_inotify_timeout, for target %u", target_id);
	gchar *target_path = (watch_reports[target_id][REPORT_WD] != WD_UNUSED) ?
		watch_paths[target_id] : NULL;
	if (G_LIKELY (target_path != NULL))
	{
		if (watch_reports[target_id][REPORT_COUNT] > 0) //report(s) available
		{
#ifdef EXTRA_MESSAGES
			printd (DEBUG, " target %d (watch %d) watch-reports count = %d",
				target_id, watch_reports[target_id][REPORT_WD], watch_reports[target_id][REPORT_COUNT]);
#endif
			if (strcmp (target_path, app.pane1.path) == 0)
			{
#ifdef E2_REFRESH_DEBUG
				printd (DEBUG,"dirty set TRUE for pane 1");
#endif
				g_atomic_int_set (&p1dirty, 1);	//set shared flag to trigger refresh
				more = TRUE;
			}
			//both panes may be the same
			if (strcmp (target_path, app.pane2.path) == 0)
			{
#ifdef E2_REFRESH_DEBUG
				printd (DEBUG,"dirty set TRUE for pane 2");
#endif
				g_atomic_int_set (&p2dirty, 1);
				more = TRUE;
			}
			if (!more //if a monitored dir now has same path as config file, no config update is possible
				&& config_file != NULL //config monitoring is happening
				&& strcmp (target_path, config_file) == 0)
			{
#ifdef E2_REFRESH_DEBUG
				printd (DEBUG,"dirty set TRUE for config");
#endif
				g_atomic_int_set (&cfgdirty, 1);	//log this for the config-monitor process to handle
				more = TRUE;
			}

			if (more)
			{
				last_work_time = time (NULL);	//refresh suspension threshold, in case downstream processing is deferred
				e2_filelist_check_dirty (GINT_TO_POINTER (1)); //start a refresh ASAP
			}
#ifdef EXTRA_MESSAGES
			printd (DEBUG, "clear reports-count for %s", watch_paths[target_id]);
#endif
//			printall ("after requesting refresh for %s (watch %d), reports-count (%d) is zeroed",
//				watch_paths [target_id],
//				watch_reports[target_id][REPORT_WD], watch_reports[target_id][REPORT_COUNT]);
			E2_BLOCK
			watch_reports[target_id][REPORT_COUNT] = 0;
			E2_UNBLOCK
		}
	}
	else //target_path == NULL, i.e. cached data is for some item not monitored now
		if (watch_reports[target_id][REPORT_WD] != WD_UNUSED)
	{
		printd (DEBUG, "OOPS no cached path for target");
		//clear data
		E2_BLOCK
		watch_reports[target_id][REPORT_WD] = WD_UNUSED;
		watch_reports[target_id][REPORT_COUNT] = 0;
		E2_UNBLOCK
	}

	switch (target_id)
	{
		case 0:
			if (more)
			{
				if (target0_timer_interval < IN_LONGMSEC)
				{
					target0_timer_interval = IN_LONGMSEC;
					//start long timer, and later return FALSE to kill this one
					target0_timerid = g_timeout_add (IN_LONGMSEC,
						_e2_fs_FAM_inotify_timeout, GUINT_TO_POINTER (0));
					more = FALSE;
				}
#ifdef EXTRA_MESSAGES
				period = "long";
#endif
			}
			else
			{	//no more reports to process, this timer will be killed
//				printd (DEBUG, "clear timer parameters");
				target0_timer_interval = 0;
				target0_timerid = 0;
#ifdef EXTRA_MESSAGES
				period = "cancelled";
#endif
			}
			break;
		case 1:
			if (more)
			{
				if (target1_timer_interval < IN_LONGMSEC)
				{
					target1_timer_interval = IN_LONGMSEC;
					//start long timer, and later return FALSE to kill this one
					target1_timerid = g_timeout_add (IN_LONGMSEC,
						_e2_fs_FAM_inotify_timeout, GUINT_TO_POINTER (1));
					more = FALSE;
				}
#ifdef EXTRA_MESSAGES
				period = "long";
#endif
			}
			else
			{
				target1_timer_interval = 0;
				target1_timerid = 0;
#ifdef EXTRA_MESSAGES
				period = "cancelled";
#endif
			}
			break;
		case 2:
			if (more)
			{
				if (target2_timer_interval < IN_LONGMSEC)
				{
					target2_timer_interval = IN_LONGMSEC;
					//start long timer, and later return FALSE to kill this one
					target2_timerid = g_timeout_add (IN_LONGMSEC,
						_e2_fs_FAM_inotify_timeout, GUINT_TO_POINTER (2));
					more = FALSE;
				}
#ifdef EXTRA_MESSAGES
				period = "long";
#endif
			}
			else
			{
				target2_timer_interval = 0;
				target2_timerid = 0;
#ifdef EXTRA_MESSAGES
				period = "cancelled";
#endif
			}
			break;
		default:
			more = FALSE;
			break;
	}
#ifdef EXTRA_MESSAGES
	printd (DEBUG, " inotify %s timer will %s", period, (more)? "continue":"stop now");
#endif
	return more; //continue long timer if a refresh was requested
}
/**
@brief cleanup cached data for @a wd
If @a path is non-NULL, this stops after cearing the first match of both @a path
and @a wd in the watch_reports table. If @a path is NULL, data for all instances
of @a wd are cleared.
@param path UTF-8 path string, for comparison with watch_paths[]
@param wd watch-descriptor number
@return
*/
static void _e2_fs_FAM_clean_local (const gchar *path, gint wd)
{
	E2_BLOCK
	gint i;
	for (i=0; i<3; i++)
	{
		if (watch_reports[i][REPORT_WD] == wd)
		{
			if (path == NULL || strcmp (path, watch_paths[i]) == 0)
			{
				watch_reports[i][REPORT_WD] = WD_UNUSED;
				free (watch_paths[i]);
				watch_paths[i] = NULL;
				if (path != NULL)
					break;
			}
		}
	}
	E2_UNBLOCK
}
/**
@brief get any inotify reports, and record the essential data for later processing.
This is the callback func for timer app.timers[DIRTYCHECK_T].
It  must not do any processing per se - that takes too long and risks loss of reports
@param userdata UNUSED pointer specified when the timer was set up
@return TRUE, or FALSE to suspend timer after extended period of inactivity
*/
static gboolean _e2_fs_FAM_inotify_read (gpointer userdata)
{
	//counter used to reduce frequency of access-reports
	static guint access_counter = 0;

	//check a few times for pending queued inotify-reports
	guint count, pending, prev_pending = 0;

	for (count = 0; count < MAX_CHECKS; count++)
	{
		//check current size of the event-queue
		if (ioctl (inotify_fd, FIONREAD, &pending) == -1)
		{
			pending = 0;	//ensure we exit immediately
			break;
		}

		//stop checking now if the reports-queue is big enough
		if (pending > INQUEUE_SIZE)
			break;

		//or stop checking now if queue accretion-rate is
		//not still growing fast enough
		//(i.e. < GROWTH_FACTOR * 2^n extra queued bytes in loop n)
		//CHECKME merits of this ??
		if ((pending - prev_pending) < (guint)(GROWTH_FACTOR * (1 << count)))
			break;

		prev_pending = pending;

		//short pause before checking again
		g_usleep (PAUSE_MICROSECONDS);
	}
	if (pending == 0)
	{	//nothing to get
		if (time (NULL) >= last_work_time + QUIET_SECONDS)
		{
			printd (DEBUG, "Suspend operation of inotify Q-poll timer");
			app.timers[DIRTYCHECK_T] = 0;
			//arrange to restart upon activity
			g_signal_connect (G_OBJECT (app.main_window), "event",
				G_CALLBACK (e2_filelist_repoll), GUINT_TO_POINTER (DIRTYCHECK_T));
			return FALSE;
		}
		return TRUE;
	}

	//transcribe change-reports to user-space
	void *store = inotify_buffer;
	gsize store_size = READ_BUFFER_SIZE;
	//must stop getting data if we can't fit another report header
	gsize buffer_limit = READ_BUFFER_SIZE - sizeof(struct inotify_event);
	gsize buffer_bytes;
	struct inotify_event *report = inotify_buffer;
	gboolean batchflag = FALSE; //for aborting after lots of reports
	gint items = 0;	//for debug reporting
	//loop to read all the queued inotify reports
	do
	{
		//read into full or partial buffer
		//do NOT use e2_fs_read(), that will block until reports are sufficient
		//to fill the buffer
		buffer_bytes = read (inotify_fd, store, store_size);
		if (buffer_bytes == 0)
			break;
		else if (buffer_bytes == -1)
		{
			printd (WARN, "_e2_fs_inotify_read: error reading inotify data");
			return TRUE;
		}

		gsize buffer_ptr = 0;
		//get the real no. of bytes in the buffer
		buffer_bytes += (store - inotify_buffer);
		//loop until we run out of data, or out of room
		while (buffer_ptr < buffer_bytes && buffer_ptr <= buffer_limit)
		{	//we always scan from the real start of the buffer
			report = (struct inotify_event *) (inotify_buffer + buffer_ptr);
			buffer_ptr += sizeof (struct inotify_event) + report->len;
			//buffer_ptr may now be <,=.> buffer_bytes
			if (buffer_ptr > buffer_bytes)	//if this happens, would be > READ_BUFFER_SIZE
				break;	//can't read all of this report's data yet

#ifdef EXTRA_MESSAGES
			if (report->len > 0)
			{	//there is a name provided
				if (report->mask & IN_ISDIR)
					printd (DEBUG, "inotify reported a directory: %s", report->name);
				else
					printd (DEBUG, "inotify reported wd %d mask 0x%x for item %s",
						report->wd, report->mask, report->name);
			}
			else
			{
				if (report->mask & IN_ISDIR)
					printd (DEBUG, "inotify reported a directory");
				else
					printd (DEBUG, "inotify reported wd %d mask 0x%x", report->wd, report->mask);
			}
#endif
			if (report->mask & IN_Q_OVERFLOW)
			{	//OOPS we lost something, don't know what ..
				//just trigger a refresh for everything
				register gint i;
				E2_BLOCK
				for (i=0; i<3; i++)
					watch_reports[i][REPORT_COUNT]++;
				E2_UNBLOCK
				printd (DEBUG, "inotify reported queue overflow");
			}
			else if (report->mask & IN_IGNORED)
			{
#ifdef EXTRA_MESSAGES
				printd (DEBUG, "IN_IGNORED report(s) received for wd %d", report->wd);
#endif
				gint cfgwatch = g_atomic_int_get (&app.FAMreq);
				if (report->wd == cfgwatch)
				{	//config file deleted, or its monitoring cancelled
					if (suspend_IGNORE)
					{ //this follows a manual suspension of monitoring
#ifdef EXTRA_MESSAGES
						printd (DEBUG, " and ignored");
#endif
						suspend_IGNORE = FALSE;
					}
					else
					{ //this is a normal timer callback, assumed a deletion
						_e2_fs_FAM_clean_local (config_file, cfgwatch); //clean up cached data
						if (e2_fs_FAM_monitor_config ()) //try to setup monitoring of new file
						{
							//log a change for new wd
							cfgwatch = g_atomic_int_get (&app.FAMreq); //watch-value different now
							E2_BLOCK
							register gint i, r = cfgwatch;
							for (i=0; i<3; i++)
							{
								if (watch_reports[i][REPORT_WD] == r)
								{
#ifdef EXTRA_MESSAGES
									printd (DEBUG, " and report logged (for config file)");
#endif
									watch_reports[i][REPORT_COUNT] = 1;
									break;
								}
							}
							E2_UNBLOCK
						}
					}
				}
			}
			else // !IN_Q_OVERFLOW and !IN_IGNORED)
			{
#ifdef EXTRA_MESSAGES
				printd (DEBUG, " watch %d mask = 0x%x", report->wd, report->mask);
#endif
				/*we don't care about any item name, because we only want to
				record changes to a monitored dir or the config file, not any item
				in a dir. So we just do a quick & clean report-count ref
				This also ignores any reports in the queue but relating to a
				watch that has been cancelled */
//				g_hash_table_replace (report_hash, (gpointer)report->wd, NULL);	//(gpointer)event->mask);
//				items++;
				//log the report
				E2_BLOCK
				register gint i, r = report->wd;
				for (i=0; i<3; i++)
				{
//#ifdef EXTRA_MESSAGES
//					printd (DEBUG, " watch %d reports count before this one = %d",
//						watch_reports[i][REPORT_WD], watch_reports[i][REPORT_COUNT]);
//#endif
					if (watch_reports[i][REPORT_WD] == r)
					{
						if (report->mask & IN_ACCESS)
						{
							if ((report->mask & !IN_ACCESS) > 0
									|| access_counter == 0)
								watch_reports[i][REPORT_COUNT]++;
						}
						else
							watch_reports[i][REPORT_COUNT]++;
						if (watch_reports[i][REPORT_COUNT] > IN_BATCHLIMIT)
							batchflag = TRUE;
						items++;
#ifdef EXTRA_MESSAGES
						printd (DEBUG, "%d reports logged for wd %d",
							watch_reports[i][REPORT_COUNT], report->wd);
#endif
						break;
					}
				}
				E2_UNBLOCK
			}
			if (batchflag)
				break;
		}
		//decide how to play the next read ...
		if (buffer_ptr <= buffer_limit)
		//buffer not full, this time, so nothing more to read
			break;
		else if (buffer_ptr == buffer_bytes)
		{
			//the last-parsed report exactly-filled the buffer
			//so next read uses whole buffer
			store = inotify_buffer;
			store_size = READ_BUFFER_SIZE;
		}
		else
		{
			//(buffer_ptr > buffer_limit or buffer_ptr <> buffer_bytes)
			//  in practice this means buffer_ptr > buffer_limit, as the buffer will be full
			//move partial-report to start of buffer
			//CHECKME does it matter when report not updated, i.e.
			//buffer_ptr > buffer_limit or buffer_ptr >= buffer_bytes
			gsize remnant = READ_BUFFER_SIZE + inotify_buffer - (void *)report;
			memmove (inotify_buffer, report, remnant);
			//begin next read after the end of moved stuff
			store = inotify_buffer + remnant;
			store_size = READ_BUFFER_SIZE - remnant;
		}
	} while (buffer_bytes > 0 && !batchflag);

//	if (items > 0)
//		printd (DEBUG, "inotify reported %d events from %d scans", items, count+1);
	if (++access_counter == 5)	//report on accesses every 5th check
		access_counter = 0;

	/* when report(s) received, and the corresponding timer is inactive, start
	   the timer with a short interval */
	if (watch_reports[0][REPORT_COUNT] > 0 &&
		g_atomic_int_compare_and_exchange (&target0_timer_interval, 0, IN_SHORTMSEC)) //flag that short delay is happening
	{
		//timer-callback data = pointerised enumerator of array row
		target0_timerid = g_timeout_add (IN_SHORTMSEC,
			_e2_fs_FAM_inotify_timeout, GUINT_TO_POINTER (0));
	}
	if (watch_reports[1][REPORT_COUNT] > 0 &&
		g_atomic_int_compare_and_exchange (&target1_timer_interval, 0, IN_SHORTMSEC))
	{
		target1_timerid = g_timeout_add (IN_SHORTMSEC,
			_e2_fs_FAM_inotify_timeout, GUINT_TO_POINTER (1));
	}
	if (watch_reports[2][REPORT_COUNT] > 0 &&
		g_atomic_int_compare_and_exchange (&target2_timer_interval, 0, IN_SHORTMSEC))
	{
		target2_timerid = g_timeout_add (IN_SHORTMSEC,
			_e2_fs_FAM_inotify_timeout, GUINT_TO_POINTER (2));
	}

	return TRUE;
}
/**
@brief register @a path with inotify
This assumes @a path will not be registered more than once at any time.
The first-available row in watch_reports array is used, so watch_reports[0] is
not necessarily for pane1, etc.
This func can be called from within a threaded cd
@param path UTF-8 string with path of item to be monitored
@param mask flag(s) indicating the types of reports wanted
@return the watch descriptor established by inotify, <0 for an error
*/
static gint _e2_fs_FAM_inotify_monitor_item (gchar *path, guint mask)
{
	gchar *local = F_FILENAME_TO_LOCALE (path);
	gint wd = inotify_add_watch (inotify_fd, local, (u_int32_t) mask);
	F_FREE (local, path);

	if (wd >= 0)
	{
		//setup the reports log
		gint i;
		E2_BLOCK
		for (i=0; i<3; i++)
		{
			if (watch_reports[i][REPORT_WD] == WD_UNUSED)
			{
				watch_reports[i][REPORT_WD] = wd;
				watch_reports[i][REPORT_COUNT] = 0;
				watch_paths[i] = strdup (path);
				printd (DEBUG, "Added inotify watch %d for %s", wd, path);
//				printall ("Added inotify watch %d for %s", wd, path);
				break;
			}
		}
		E2_UNBLOCK
#ifdef EXTRA_MESSAGES
		if (i == 3)
			printd (WARN, "Failed to add inotify watch %d for %s\n%s", wd, path, "No space in reports-table");
		printd (DEBUG, "Tabled watches now are %d, %d, %d",
			watch_reports[0][REPORT_WD], watch_reports[1][REPORT_WD], watch_reports[2][REPORT_WD]);
#endif
//		printall ("Tabled watches now are %d, %d, %d",
//			watch_reports[0][REPORT_WD], watch_reports[1][REPORT_WD], watch_reports[2][REPORT_WD]);
	}
	else
		printd (WARN, "Failed to add inotify watch for %s\n%s", path, strerror (errno));

	return wd;
}
/**
@brief change the scope of inotify-monitoring for @a path
Called during threaded refresh.
@param path UTF-8 string with path of item to be changed, to be compared with rt->path's
@param mask bitflags for changed monitoring scope
@return
*/
static void _e2_fs_FAM_inotify_remonitor_item (gchar *path, guint mask)
{
	gint *req, *oreq;
	gint old, new, other;

#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#endif
again:
	if (strcmp (path, curr_pane->path) == 0)
	{
		req = &curr_pane->FAMreq;
		oreq = &other_pane->FAMreq;
	}
	else if (strcmp (path, other_pane->path) == 0)
	{
		req = &other_pane->FAMreq;
		oreq = &curr_pane->FAMreq;
	}
	else
		return;

	if ((old = g_atomic_int_get (req)) == WD_BUSY ||
		(other = g_atomic_int_get (oreq)) == WD_BUSY)
	{
		usleep (2000); //threaded, blocking is ok
		goto again;
	}
	if (G_LIKELY (old != WD_UNUSED))
		g_atomic_int_set (req, WD_BUSY); //block other threads from messing with it
	else
		return;

	gchar *local = F_FILENAME_TO_LOCALE (path);
	new = inotify_add_watch (inotify_fd, local, (u_int32_t) mask);
	F_FREE (local, path);

	if (new == old)
		g_atomic_int_set (req, old); //unblock it
	else
	{
		//conform the reports array
		gint i;
		E2_BLOCK
		for (i=0; i<3; i++)
		{
			if (watch_reports[i][REPORT_WD] == old)
			{
				watch_reports[i][REPORT_WD] = new;
				if (new < 0)
				{
					watch_reports[i][REPORT_COUNT] = 0;
					free (watch_paths[i]);
					watch_paths[i] = NULL;
				}
			}
		}
		E2_UNBLOCK
#ifdef DEBUG_MESSAGES
		printd (DEBUG, "inotify watch number changed from %d to %d when reports scope changed",
			old, new);
# ifdef EXTRA_MESSAGES
		printd (DEBUG, "Tabled watches now are %d, %d, %d",
			watch_reports[0][REPORT_WD], watch_reports[1][REPORT_WD], watch_reports[2][REPORT_WD]);
# endif
#endif
//		printall ("After changing monotor scope for %s\n  tabled watches now are %d, %d, %d",
//		    path,
//			watch_reports[0][REPORT_WD], watch_reports[1][REPORT_WD], watch_reports[2][REPORT_WD]);

		//alter watch data CHECKME also if new < 0?
		g_atomic_int_set (req, new);
		if (other == old)
			g_atomic_int_set (oreq, new);
//		printall ("  and FAMreq's are %d, %d", curr_pane->FAMreq, other_pane->FAMreq);
	}
}
/**
@brief de-register @a path from inotify
This func can be called from within a threaded refresh.
Do NOT call this when the same watch applies to more than one directory.
@param path UTF-8 string with path of item no longer to be monitored
@param wd watch descriptor number that is to be cancelled
@return TRUE if successfully cancelled
*/
static gboolean _e2_fs_FAM_inotify_demonitor_item (gchar *path, gint wd)
{
	gboolean retval;
	if (inotify_rm_watch (inotify_fd, wd) >= 0)
	{
		printd (DEBUG, "Removed inotify %d watch for %s", wd, path);
		retval = TRUE;
	}
	else
	{	//error may be because monitored item (e.g. config file or unmounted dir) is gone now
		printd (DEBUG, "%s error when trying to remove inotify watch %d for %s", strerror (errno), wd, path);
		retval = FALSE;
	}

	//no need to flush the reports queue here
	_e2_fs_FAM_clean_local (path, wd);
#ifdef EXTRA_MESSAGES
	printd (DEBUG, "Tabled watches now are %d, %d, %d",
		watch_reports[0][REPORT_WD], watch_reports[1][REPORT_WD], watch_reports[2][REPORT_WD]);
#endif
//	printall ("After cancelling watch for %s\n  tabled watches now are %d, %d, %d",
//	    path,
//		watch_reports[0][REPORT_WD], watch_reports[1][REPORT_WD], watch_reports[2][REPORT_WD]);

	return retval;
}
/**
@brief Initialize inotify monitoring.

@return TRUE if initialization succeeded
*/
static gboolean _e2_fs_FAM_inotify_init (void)
{
	inotify_fd = inotify_init ();

	if (inotify_fd < 0)
	{
		printd (WARN, "Could not initialize inotify");
		return FALSE;
	}
	//prevent blocking during Q reads
	gint flags = fcntl (inotify_fd, F_GETFL);
#ifdef EXTRA_MESSAGES
	printd (DEBUG, "inotify fd flags are 0x%x", flags);
#endif
	if (flags != EBADF)
		fcntl (inotify_fd, F_SETFL, flags | O_NONBLOCK);

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "start Q-poll timer @ init");
#endif
	/* Use one of the 'main' timers so its suspension can be managed like others
	   This one would be used for 'default' polling if kernel-fam were not available
	   Some cleanup code assumes the same timer for both modes - do not change it
	   BUT shutting this down will also block config updating */
	app.timers[DIRTYCHECK_T] = g_timeout_add (QPOLL_MSEC, _e2_fs_FAM_inotify_read, NULL);

	return TRUE;
}
/**
@brief cleanup as part of ending inotify monitoring.

@return
*/
static void _e2_fs_FAM_inotify_abandon (void)
{
	if (app.timers[DIRTYCHECK_T] != 0)
	{
		g_source_remove (app.timers[DIRTYCHECK_T]);
		app.timers[DIRTYCHECK_T] = 0;
	}
	if (target0_timerid != 0)
	{
		g_source_remove (target0_timerid);
//		target0_timerid = 0;
	}
	if (target1_timerid != 0)
	{
		g_source_remove (target1_timerid);
//		target1_timerid = 0;
	}
	if (target2_timerid != 0)
	{
		g_source_remove (target2_timerid);
//		target2_timerid = 0;
	}

	//cancel all monitoring that's in force
	guint i;
	for (i=0; i<3; i++)
	{
		if (watch_reports[i][REPORT_WD] > 0)
		{
			inotify_rm_watch (inotify_fd, watch_reports[i][REPORT_WD]);
//			watch_reports[i][REPORT_WD] = WD_UNUSED;
		}
		if (watch_paths[i] != NULL)
		{
			free (watch_paths[i]);
//			watch_paths[i] = NULL;
		}
	}
	//any queued pending reports are cleaned by kernel
	//do not close inotify_fd, it's not a valid file-descriptor

	g_free (inotify_buffer);	//cleanup session reports-buffer
	inotify_buffer = NULL;
}

  /**********************/
 /** public functions **/
/**********************/

/**
@brief establish inotify connection
TODO error message assumes BGL closed

@return
*/
void e2_fs_FAM_connect (void)
{
	if (G_LIKELY (inotify_buffer == NULL))
	{
		inotify_buffer = g_try_malloc (READ_BUFFER_SIZE);	//cleared during disconnection
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (inotify_buffer, app.monitor_type = E2_MONITOR_DEFAULT; return;)
#else
		if (inotify_buffer == NULL)
		{
			printd (WARN, "e2_fs_FAM_connect: unable to create inotify read-buffer");
			app.monitor_type = E2_MONITOR_DEFAULT;
			return;
		}
#endif
	}

	if (_e2_fs_FAM_inotify_init ())
		app.monitor_type = E2_MONITOR_FAM;
	else
	{
		app.monitor_type = E2_MONITOR_DEFAULT;
		g_free (inotify_buffer);
		inotify_buffer = NULL;
	}
}
/**
@brief terminate inotify connection

Call at session-end

@return
*/
void e2_fs_FAM_disconnect (void)
{
	if (app.monitor_type == E2_MONITOR_FAM)
		_e2_fs_FAM_inotify_abandon ();
	if (config_file != NULL)
	{
		g_free (config_file);
		config_file = NULL;
	}
}
/**
@brief put inotify monitoring to 'sleep' i.e. suspend polling activity
@return
*/
void e2_fs_FAM_suspend (void)
{
	if (app.timers[DIRTYCHECK_T] != 0)
	{
		g_source_remove (app.timers[DIRTYCHECK_T]);
		app.timers[DIRTYCHECK_T] = 0;
	}
}
/**
@brief wake up inotify monitoring
@return
*/
void e2_fs_FAM_resume (void)
{
	if (app.timers[DIRTYCHECK_T] == 0)
		app.timers[DIRTYCHECK_T] = g_timeout_add (QPOLL_MSEC, _e2_fs_FAM_inotify_read, NULL);
}
/**
@brief change monitored directory from @a oldir to @a rt ->view.dir
Called during threaded cd.
If @a olddir not also in the other pane, cancel old-dir monitoring.
If the dir for @a rt (new-dir) is not also in other pane, start monitoring new-dir.
If new-dir is not monitored (FAM error) the FAM request for the pane is set to WD_UNUSED

@a olddir needs trailing '/', for valid comparing with rt->view.dir etc

@param olddir UTF-8 string with absolute path of dir to stop monitoring
@param rt data struct for the pane to which the cd applies, including the new dir in rt->view.dir

@return
*/
void e2_fs_FAM_change (gchar *olddir, E2_PaneRuntime *rt)
{
	if (G_LIKELY (app.monitor_type == E2_MONITOR_FAM))
	{	//inotify connection set up
		E2_PaneRuntime *ort;
		gint old, new, other;
again:
		ort = (rt == curr_pane) ? other_pane : curr_pane;
		//atomic access cuz' other threads may want to use FAMreq(s)
		if ((old = g_atomic_int_get (&rt->FAMreq)) == WD_BUSY ||
			(other = g_atomic_int_get (&ort->FAMreq)) == WD_BUSY)
		{
			usleep (2000);
			goto again;
		}

		g_atomic_int_set (&rt->FAMreq, WD_BUSY); //block other threads from messing with it
		//if both panes are now using the same watch (FAMreq), or this pane is
		//unwatched, we can't cancel the watch for olddir
		if (!(old == other || old == WD_UNUSED))
			_e2_fs_FAM_inotify_demonitor_item (olddir, old);
//#ifdef EXTRA_MESSAGES
//		else
//			printd (DEBUG, "skip demonitor, old wd = %d, other wd = %d", old, other);
//#endif
//		else
//			printall ("During CD, skip demonitor, old wd = %d, other wd = %d", old, other);

		//if both panes will be using the same watch, no need to start a new
		//watch, just replicate data
		if (strcmp (rt->view.dir, ort->view.dir) == 0 && other > 0)
		{
			new = other;
//#ifdef EXTRA_MESSAGES
//			printd (DEBUG, "Set pane FAMreq = %d from other pane", new);
//#endif
		}
		else
		{
			new = _e2_fs_FAM_inotify_monitor_item (rt->view.dir, INDIR_FLAGS);
//#ifdef EXTRA_MESSAGES
//			printd (DEBUG, "Set pane FAMreq = %d from new watch", new);
//#endif
		}
		g_atomic_int_set (&rt->FAMreq, new);
//		printall ("After inotify setup during CD, FAMreq's are %d, %d",
//			curr_pane->FAMreq, other_pane->FAMreq);
	}
}
/**
@brief finish or suspend monitoring of dir related to @a rt
@return TRUE if the connection was successfully removed
*/
gboolean e2_fs_FAM_cancel_monitor_dir (E2_PaneRuntime *rt)
{
	gboolean result;
	gint old = g_atomic_int_get (&rt->FAMreq);
	if (old > 0)
	{
		result = _e2_fs_FAM_inotify_demonitor_item (rt->path, old);
		if (result)
		{
			g_atomic_int_set (&rt->FAMreq, WD_UNUSED);
/*#ifdef DEBUG_MESSAGES
			printd (DEBUG, "FAM cancel for %s succeeded", path);
#endif */
		}
/*#ifdef DEBUG_MESSAGES
		else
			printd (WARN, "FAM cancel for %s failed", path);
#endif
*/
	}
	else
		result = FALSE;
	return result;
}
/**
@brief reduce the scope of event-monitoring for @a path
Essentially this abandons IN_ACCESS reporting, normally used for a directory.
Any prior reports logged for the dir are not affected.
@param path UTF-8 string with path of dir whose watch is to be altered
@return
*/
void e2_fs_FAM_less_monitor_dir (gchar *path)
{
#ifdef EXTRA_MESSAGES
	printd (DEBUG, "Cancel ACCESS event-reports for %s", path);
#endif
	//there's no 'remove mask' API, so we reset the watch
	_e2_fs_FAM_inotify_remonitor_item (path, INDIR_SHORT_FLAGS);
}
/**
@brief resume normal scope of event-monitoring for @a path
This is needed after e2_fs_FAM_less_monitor_dir(), normally used for a directory.
Essentially this resumes IN_ACCESS reporting.
Any prior event-reports for the dir are not affected.
@param path UTF-8 string with path of dir whose watch is to be altered
@return
*/
void e2_fs_FAM_more_monitor_dir (gchar *path)
{
#ifdef EXTRA_MESSAGES
	printd (DEBUG, "Resume ACCESS event-reports for %s", path);
#endif
	_e2_fs_FAM_inotify_remonitor_item (path, INDIR_XTRA_FLAGS | IN_MASK_ADD);
}
/**
@brief begin or resume monitoring of config file

@return TRUE if the connection was successfully established
*/
gboolean e2_fs_FAM_monitor_config (void)
{
	if (config_file != NULL)
		g_free (config_file);
	config_file = g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);

	gint new = _e2_fs_FAM_inotify_monitor_item (config_file, INCFG_FLAGS);
	//atomic access cuz' other threads may want to use the variable
	g_atomic_int_set (&app.FAMreq, new);

	return (new > 0);
}
/**
@brief finish or suspend monitoring of config file

@return TRUE if the connection was successfully removed
*/
gboolean e2_fs_FAM_cancel_monitor_config (void)
{
	gboolean result;
	suspend_IGNORE = TRUE; //guide for handling IGNORED reports, cleared by the reader
//	printd (DEBUG, "goto _e2_fs_FAM_inotify_demonitor_item() for %s", config_file);
	//this spits a non-fatal error if called after the config file has been deleted,
	//including deletion as part of a replacement
	gint old = g_atomic_int_get (&app.FAMreq);
	if (old > 0)
	{
		result = _e2_fs_FAM_inotify_demonitor_item (config_file, old);
		if (result)
		{
			g_atomic_int_set (&app.FAMreq, WD_UNUSED);
//			printd (DEBUG, "FAM cancel for %s succeeded", config_file);
		}
/*#ifdef DEBUG_MESSAGES
		else
			printd (WARN, "FAM cancel for %s failed", config_file);
#endif
*/
	}
	else
		result = FALSE;
	return result;
}

#ifdef EXTRA_MESSAGES
# undef EXTRA_MESSAGES
#endif

#endif //def E2_FAM_INOTIFY
