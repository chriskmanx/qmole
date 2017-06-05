/* $Id: e2_main.c 3059 2014-02-14 23:38:41Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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
@file src/e2_main.c
@brief main function

This file contains the main function and some local startup and shutdown helpers.
*/

/**
\mainpage emelFM2 API

\attention This API-documentation is under construction!\n It is planned to provide additional information, to make it easier to read and understand the code and hopefully to inspire you to enhance it.

\section intro introduction

Welcome to emelFM2-doxygen. If you don't know doxygen well, click on
everything you're interested in. Even within the source documentation you
will find lots of links. It's quite fun.\n\n
On this page you'll find all the important aspects of coding for emelFM2.
There are three parts:\n
\arg \ref conventions
\arg \ref internals
\arg \ref visible\n

If you're interested in developing plugins, go to \ref plugin_writing

\section conventions conventions

emelFM2 code should follow some layout and naming conventions:
\arg use tabs, not spaces, to indent code. Preferred tab size is 4.
\arg stick to a consistent coding style
 - vertically-aligned matching braces
 - a space between text and left-brackets, after commas, and around operators
 - "//" C++ style single-line comments
 - in general, break lines at or about column 80

\code
void routine (void)
{
	if (function (a, b, c))
	{
		//do something
		x = a + 1;
		printf (_("Break lines after 80 characters, in general, like the following.\n))
		rt->set = e2_option_tree_register ("option_name", strdup ("group_name"), _("option_label"),
			NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL);
		printf (_("or _perhaps_ NOT after 80, if the line is just a string that needs to be internationalised like this.\n"));
	}
}
\endcode
\arg files that are part of emelFM2 have a name starting with e2_, or e2p_ for a plugin
\arg emelFM2's own header files can included with just "" even if they are not in the same directory
\arg the name of emelFM2's own functions starts with e2_, e2p_, _e2_, or _e2p_. The "_" variants are for static (private) functions, those prefixes should not be used for functions called from other parts of the program. The "p" variants apply in plugins. In general, the second part of the function name matches the file that contains the function. Sometimes it's a condensed form of that name. Functions imported from other code are probably best left with
their original name.
\arg emelFM2's own constants should begin with E2 or E2_

Code should be liberally commented, to assist others in reviewing and enhancing things.
In particular, functions should be documented in a form which doxygen understands.\n\n
Strings which are displayed to the user should be internationalised, like _("Tell me")

\section internals internals

This section explains how emelFM2 works.
\arg \ref options
\arg \ref cache
\arg \ref actions
\arg \ref commands
\arg \ref bindings
\arg \ref filesystem
\arg \ref plugins

\section visible user-interface

This section is about parts of emelFM2 you can see and interact with. It provides
development-related information which goes beyond, and should be read in conjuntion
with, user-guidance in the files
 - USAGE (general help)
 - CONFIGURATION (configuration help mainly for use via a configuration dialog)
 - ACTIONS (a listing of action-names, each with parameters and brief description)\n

The following items are covered:
\arg \ref panes
\arg \ref output
\arg \ref toolbar
\arg \ref commline
\arg \ref menu
\arg \ref othermenu
\arg \ref status
\arg \ref dialogs
*/

#include "emelfm2.h"
#include <string.h>
#include <unistd.h>
#include <locale.h>
#include <signal.h>
#include <pthread.h>
#include "e2_dialog.h"
#include "e2_filelist.h"
#include "e2_filetype.h"
#include "e2_plugins.h"
#include "e2_complete.h"
#include "e2_task.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"
#include "../plugins/e2p_upgrade.h"

extern volatile gint cfg_refresh_refcount;
extern GList *open_history;
#ifdef USE_GLIB2_10
extern GHashTable *actions_hash;
#endif
extern time_t last_work_time;

#ifdef WAIT_POLL
pthread_mutex_t wait_mutex; // = PTHREAD_MUTEX_INITIALIZER;	if no recursion
#define WAIT_LOCK pthread_mutex_lock (&wait_mutex);
#define WAIT_UNLOCK pthread_mutex_unlock (&wait_mutex);
#endif

static void _e2_main_system_shutdown (gint num); //__attribute__ ((noreturn));

/**
@brief local management of UI-backend-access mutex
These allow a conservative approach to locking without risk of deadlock
@return
*/
void e2_main_close_uilock (void)
{
	//a single thread can't write to screen more than once at a time,
	//so deadlocks just reflect "unnecessary" re-locks, and can be ignored
#ifdef DEBUG_MESSAGES
	if (pthread_mutex_lock (&display_mutex) == EDEADLK)
		printd (DEBUG, "attempted BGL re-lock");
#else
	pthread_mutex_lock (&display_mutex);
#endif
}

void e2_main_open_uilock (void)
{
#ifdef DEBUG_MESSAGES
	if (pthread_mutex_unlock (&display_mutex) == EPERM)
		printd (DEBUG, "bad attempt to open BGL");
#else
	pthread_mutex_unlock (&display_mutex);
#endif
}
/* *
@brief aborted-thread cleanup function
@param data pointerised boolean, non-NULL to re-close BGL
*/
/*void e2_main_cleanup_uilock (gpointer data)
{
	printd (DEBUG, "e2_main_cleanup_uilock");
	gboolean FIXME;
	if (data != NULL)
	{
		//e2_main_close_uilock ();
		CLOSEBGL_IF_OPEN
	}
	printd (DEBUG, "e2_main_cleanup_uilock FINISH");
}
*/
/**
@brief re-initialize BGL
@param free TRUE to kill the current mutex first

@return
*/
void e2_main_init_uilock (gboolean free)
{
//PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP isn't always available to init mutex
	pthread_mutexattr_t attr;
	if (free)
		pthread_mutex_destroy (&display_mutex);
	pthread_mutexattr_init (&attr);
#ifdef DEBUG_MESSAGES
	pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_ERRORCHECK);
#else
	pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
#endif
	pthread_mutex_init (&display_mutex, &attr);
	pthread_mutexattr_destroy (&attr);
}
/**
@brief idle and then timer callback to do non-time-critical things at session start
This waits until after both filelists are initialised, as determined by both
view->dir strings being not empty (""). Then does various things: start refreshing
filelists, etc
@param data NULL specified when idle was established, non-NULL for timer

@return TRUE until the filelists have been established
*/
static gboolean _e2_main_defer_init (gpointer data)
{
	if (app.pane1.view.dir == NULL || *app.pane1.view.dir == '\0'
	 || app.pane2.view.dir == NULL || *app.pane2.view.dir == '\0'
	 || g_atomic_int_get (&app.pane1.view.listcontrols.cd_working)
	 || g_atomic_int_get (&app.pane2.view.listcontrols.cd_working))
	{
		if (data != NULL)	//not first first callback
			return TRUE;
		//start a timer to check completion
		//app.timers[?] = CHECKME no need to force shutdown ?
		g_timeout_add (100, (GSourceFunc) (_e2_main_defer_init),
			GINT_TO_POINTER (100));
			return FALSE;
	}

	if (curr_pane == &app.pane1)
	{
		if (e2_cl_options.force_path)
		{
			CLOSEBGL
			e2_pane_activate_other (); //focus the specified dir in pane 2
			OPENBGL
		}
	}
	else //async setup of initial dirs results in pane2 completion after pane 1
	{
		if (!e2_cl_options.force_path)
		{
			CLOSEBGL
			e2_pane_activate_other (); //focus pane 1
			OPENBGL
		}
	}

	//prevent further pane-divider-position overrides in the window_show callback
	e2_cl_options.force_path = FALSE;
	g_free (e2_cl_options.pane2_path);
	e2_cl_options.pane2_path = NULL;
	g_free (e2_cl_options.pane1_path);
	e2_cl_options.pane1_path = NULL;

#ifdef E2_FAM
	//hookup to a file monitor if possible
	e2_fs_FAM_connect ();
#endif
	last_work_time = time (NULL);	//actually, only needed when auto-refreshing
	if (e2_option_bool_get ("auto-refresh"))
		//initiate filelist dirty-checks
		e2_filelist_start_refresh_checks ();
	else
	{
		//must cleanup the flags in case of later use
		app.pane1.view.listcontrols.refresh_refcount = 0;
		app.pane2.view.listcontrols.refresh_refcount = 0;
	}
	if (e2_option_bool_get ("auto-refresh-config"))
		//downstream uses change-monitor-type, so do this after FAM_connect()
		e2_option_start_config_checks ();
	else
		cfg_refresh_refcount = 0;

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "all change-polling initialised after filelists filled");
#endif

	//set the 'real' initial conversion funcs
	e2_utf8_set_name_conversion (app.pane1.view.convert || app.pane2.view.convert);

#ifdef E2_HAL
	e2_hal_init ();
#endif
#ifdef E2_DEVKIT
	e2_devkit_init ();
#endif
	return FALSE;
}
/**
 @brief spit out a warning if the current locale-information looks dodgy

 @param locale single or ";"-joined series of locale identifier strings

 @return
*/
static void _e2_main_check_locale_broken (const gchar *locale)
 {
	gboolean broken = FALSE;
	if (!strcmp (locale, "C"))
		broken = TRUE;
	else
	{
		gchar **split = g_strsplit (locale, ";", -1);
		gint i = 0;
		while (split[i] != NULL)
		{
			if (g_str_has_prefix (split[0], "LC_CTYPE") &&
				g_str_has_suffix (split[0], "=C"))
			{
				broken = TRUE;
				break;
			}
			i++;
		}
	}
	if (broken)
 	{
 		printf (_("Your current locale is '%s'.\n"), locale);
		printf (
		  _("You have set the environment variable G_BROKEN_FILENAMES, which\n"
 			"causes GTK+ to convert filename encoding, from the one specified\n"
			"by the system locale, to UTF-8.\n"
			"However, you have not set a system locale. Please do so, by setting\n"
			"the environment variable LANG or LC_CTYPE!\n"));
 		if (!e2_cl_options.ignore_problems)
 		{
			printf (
			  _("(Note: There is a command line option -i/--ignore-problems, but use it\n"
				"at your own risk!)\n"));
 			exit (1);
 		} else
			printf (
			  _("%s will ignore locale problems when reading filenames because\n"
 				"--ignore-problems/-i has been set. This might result in segfaults and all\n"
 				"kind of problems. You really should set a system locale with the\n"
				"LANG or LC_CTYPE environment variable.\n"), PROGNAME);
 	}
}
/**
@brief check locale

@return
*/
static void _e2_main_check_locale (void)
{
//	printd (DEBUG, "check_locale ()");
	if (g_getenv ("G_BROKEN_FILENAMES") != NULL)
	{
		const gchar *locale = setlocale(LC_ALL, (gchar *) NULL);
		if (locale != NULL)
		{
			printd (NOTICE, "current locale is '%s'", locale);
			_e2_main_check_locale_broken (locale);
		}
		else
			printf ("Your locale setup is broken!\n");//no point in translating this
	}
}

/* *
@brief blind log handler used when gtk messages-logging is suppressed

@return
*/
//static void _e2_main_blind_log_handler () { }
/**
@brief handle messages from glib or gtk

@return
*/
static void _e2_main_message_handler (const gchar *log_domain,
	GLogLevelFlags log_level, const gchar *message, gpointer user_data)
{
	if (e2_cl_options.suppress_gtk_log)
		g_log_default_handler (log_domain, log_level, message, user_data);
	else
	{
#ifdef DEBUG_MESSAGES
		printd (WARN, "%s message: %s", log_domain, message);
#else
		//mimic default message printer without aborting on fatal error
		gchar *fmt = (log_level < G_LOG_LEVEL_WARNING) ?
			"\n** %s: %s **\n\n" : "%s: %s\n";
		printf (fmt, log_domain, message);
#endif
		g_log (log_domain, log_level, "%s", message);
	}
}

#ifndef WAIT_POLL
//GMainContext *localctx = NULL; //CHECKME cleared ?
#endif

/**
@brief create new local wait loop
@param mainwait TRUE if wait is initiated from default GMainContext
@return the loop data struct or NULL in case of error
*/
E2_MainLoop *e2_main_loop_new (gboolean mainwait)
{
#ifdef WAIT_POLL
	E2_MainLoop *loopdata = ALLOCATE0 (E2_MainLoop);
	CHECKALLOCATEDWARN (loopdata, return NULL;)
	//without a threadID yet, an (unlikely) immediate abort will not find this
	//loopdata in the list, hence leak
	loopdata->maincontext = mainwait;
#else
	E2_MainLoop *loopdata = MALLOCATE (E2_MainLoop);
	CHECKALLOCATEDWARN (loopdata, return NULL;)
/*	GMainContext *ctx;
	if (mainwait)
		ctx = DEFAULT_CONTEXT;
	else
		ctx = g_main_context_new (); FIXME leak
	GMainLoop *loop = g_main_loop_new (ctx, FALSE);
*/
	GMainLoop *loop = g_main_loop_new (DEFAULT_CONTEXT, FALSE);
	if (loop == NULL)
	{
		DEMALLOCATE (E2_MainLoop, loopdata);
		return NULL;
	}
	loopdata->loop = loop;
	loopdata->threadID = 0;
#endif
	app.mainloops = g_slist_prepend (app.mainloops, loopdata);
	return loopdata;
}
#ifdef WAIT_POLL
/**
@brief cleanup after finishing or aborting a wait 'loop'
@param loopdata pointer to loop data struct

@return
*/
static void _e2_main_loop_clean_loop (E2_MainLoop *loopdata)
{
	printd (DEBUG, "In %s, loopdata: %x thread %u", __PRETTY_FUNCTION__, loopdata, pthread_self());
	DEALLOCATE (E2_MainLoop, loopdata);
	app.mainloops = g_slist_remove (app.mainloops, loopdata);
//	printd (DEBUG, "End %s, loopdata: %x", __PRETTY_FUNCTION__, loopdata);
}
#endif
/**
@brief restore BGL after finishing or aborting a wait 'loop'
Expects BGL open/off.
@param loopdata pointer to loop data struct

@return
*/
static void _e2_main_loop_clean_BGL (E2_MainLoop *loopdata)
{
	printd (DEBUG, "Start %s, loopdata: %x thread %u", __PRETTY_FUNCTION__, loopdata, pthread_self());
	while (g_main_context_iteration (DEFAULT_CONTEXT, FALSE)); //CHECKME useful ?
	CLOSEBGL
//	printd (DEBUG, "_e2_main_loop_clean_BGL, thread %u, BGL back on", pthread_self());
	gdk_flush (); //stops UI freezes ? (gtk does this)
//	printd (DEBUG, "End %s, loopdata: %x", __PRETTY_FUNCTION__, loopdata);
}

#ifdef WAIT_POLL
# define LOOP_WAIT_USEC 50000 //20Hz polling should be responsive enough
#endif
/**
@brief block until wait is ended (as indicated by flag in @a loopdata) then cleanup
Function is abort-safe. Refreshing may be disabled - if so, it's reverted while here
BGL is expected to be on/closed
@param loopdata pointer to loop data struct

@return
*/
void e2_main_loop_run (E2_MainLoop *loopdata)
{
	//a full-blown mainloop here is overkill?, and may?? compromise BGL mananagment
#ifdef WAIT_POLL
	gboolean wait;
#endif
	loopdata->threadID = pthread_self ();	//for aborting if that happens
	gint saverefcount;

	gdk_flush ();
	pthread_cleanup_push ((gpointer)e2_filelist_reset_refresh, NULL);
		//enable refresh
		//BAD if cfg_refresh_refcount adjusted individually ?
		saverefcount = g_atomic_int_get (&cfg_refresh_refcount); //proxy for all refcounters
		if (saverefcount > 0)
		{
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "enable refresh, e2_main_loop_run");
#endif
			e2_filelist_enable_refresh ();
		}
#ifdef WAIT_POLL
		pthread_cleanup_push ((gpointer)_e2_main_loop_clean_loop, loopdata);
#endif
			pthread_cleanup_push ((gpointer)_e2_main_loop_clean_BGL, loopdata);
				OPENBGL
//				printd (DEBUG, "e2_main_loop_run, BGL off");
#ifdef WAIT_POLL
//				pthread_cleanup_push ((gpointer)_e2_main_loop_clean, loopdata);
				printd (DEBUG, "starting wait-flag poll loop");
				/* We can't block the parent thread, it's needed to update the UI
				   including processing the user's input to end this loop. But we
				   don't want to hog too much resources while sitting here waiting */
				do
				{
//					FIXME the maincontext flag is not set correctly for dialogs that do local
//					locking e.g. ownership and permissions
					usleep (LOOP_WAIT_USEC); //may be aborted during this

					//"un-related" events may need to be processed e.g. if an abort-confirmation dialog is popped up
//					WAIT_FOR_EVENTS_UNLOCKED
#if 0 //def DEBUG_MESSAGES
					if (g_main_context_iteration (DEFAULT_CONTEXT, FALSE))
					{
						while (g_main_context_iteration (DEFAULT_CONTEXT, FALSE));
						printd (DEBUG, "mainloop source(s) were dispatched");
					}
#else
					while (g_main_context_iteration (DEFAULT_CONTEXT, FALSE));
#endif

					WAIT_LOCK
					wait = !loopdata->finished;
					WAIT_UNLOCK
				} while (wait);
				printd (DEBUG, "finished wait-flag poll loop");
#else
				pthread_cleanup_push ((gpointer)e2_main_loop_quit, loopdata);
				g_main_loop_run (loopdata->loop);
				pthread_cleanup_pop (0);	//don't cleanup loop data this way (that re-enters the quitter!)
#endif
//				_e2_main_loop_clean_BGL (loopdata);	//pop(1) BAD when aborting ?
				CLOSEBGL
				printd (DEBUG, "BGL back on");
			pthread_cleanup_pop (0);	//BGL back on

#ifdef WAIT_POLL
		_e2_main_loop_clean_loop (loopdata); //pop(1) BAD when aborting ?
//		printd (DEBUG, "goto thread cleanup of cleanup loop");
		pthread_cleanup_pop (0);	//cleanup loop WAS 1, BAD when aborting
#endif
//		printd (DEBUG, "after thread cleanup of cleanup loop");

	if (saverefcount > 0)
	{
#ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "disable refresh, e2_main_loop_run");
#endif
		e2_filelist_disable_refresh ();
	}
	pthread_cleanup_pop (0);
//	usleep (25000);
//	WAIT_FOR_EVENTS
}

/**
@brief quit a wait loop
BGL is expected to be on/closed
@param loopdata pointer to loop data struct

@return
*/
void e2_main_loop_quit (E2_MainLoop *loopdata)
{
#ifdef WAIT_POLL
//	printd (DEBUG, "e2_main_loop_quit");
	WAIT_LOCK
	loopdata->finished = TRUE;
	e2_utils_fake_event ();
	WAIT_UNLOCK
	//allow the downstream cleanup to happen
	usleep (LOOP_WAIT_USEC);
//	printd (DEBUG, "e2_main_loop_quit ENDS");
#else
	if (loopdata->loop != NULL)
	{
		//ASAP prevent other attempts to stop this loop
		GMainLoop *tmp = loopdata->loop;
		loopdata->loop = NULL;
		GMainContext *ctx = g_main_loop_get_context (tmp);
		gboolean mainctx = (ctx == g_main_context_default ());
		if (g_main_loop_is_running (tmp));
		{
			if (mainctx)
			{
				OPENBGL
				while (g_main_context_pending (ctx)) //INTERFERES with other mainloops
					g_main_context_iteration (ctx, FALSE);
				CLOSEBGL
			}
			g_main_loop_quit (tmp);
			usleep (5000);	//allow some post-block activity
		}
		g_main_loop_unref (tmp);
//		if (!mainctx)
//			g_main_context_unref (ctx);
	}
	DEMALLOCATE (E2_MainLoop, loopdata);
	app.mainloops = g_slist_remove (app.mainloops, loopdata);
#endif
}
/**
@brief abort any wait whose watcher is @a ID, and cleanup
@param ID thread ID to match

@return TRUE if something was found
*/
gboolean e2_main_loop_abort (pthread_t ID)
{
	printd (DEBUG, "e2_main_loop_abort, ID: %u", ID);
	gboolean killed = FALSE;
#ifdef WAIT_POLL
	gboolean wait = FALSE;
#endif
	GSList *member = app.mainloops;
	while (member != NULL)
	{
		E2_MainLoop *loopdata = (E2_MainLoop *)member->data;
		member = member->next;	//in case the list is modified by loop quit
		if (loopdata->threadID == ID)
		{
			killed = TRUE;
#ifdef WAIT_POLL
			wait = wait || !loopdata->maincontext;
#endif
			//to avoid racing with e2_main_loop_run() we don't kill loopdata here
			e2_main_loop_quit (loopdata);
			//may be > 1 wait per thread, don't finish yet
		}
	}
#ifdef WAIT_POLL
	//for non-main threads (which may be killed by upstream) allow the downstream
	//cleanup to happen
	if (wait)
		usleep (LOOP_WAIT_USEC);
#endif
	return killed;
}

  /****************/
 /***** main *****/
/****************/

/**
@brief main function

@param argc number of command-line arguments
@param argv array of command-line arguments

@return nothing, directly
*/
gint main (gint argc, gchar *argv[])
{
	//threads needed
	pthread_mutexattr_t attr;
	//setup mutex to protect threaded access to cd functionality
	pthread_mutexattr_init (&attr);
	pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init (&list_mutex, &attr);
	pthread_mutex_init (&history_mutex, &attr);
#ifdef WAIT_POLL
	pthread_mutex_init (&wait_mutex, &attr);
#endif
	pthread_mutexattr_destroy (&attr);

#ifndef USE_GLIB2_32
	g_thread_init (NULL);
#endif
	e2_main_init_uilock (FALSE);
# ifndef LOCAL_BGL
#ifdef USE_GTK3_6
#warning GTK 3.6 deprecates use of an application-specific display mutex. No reasonable workaround is available.
#endif
	//these are deprecated for gtk 3.6+
	gdk_threads_set_lock_functions (e2_main_close_uilock, e2_main_open_uilock);
	gdk_threads_init ();	//setup gdk mutex
# endif
#ifdef ENABLE_NLS
	//before gtk_init(), ensure that _() in recent gtk's works cleanly
	textdomain (BINNAME);
	printd (DEBUG, "setting locale base dir to '%s'", LOCALE_DIR);
	bindtextdomain (BINNAME, LOCALE_DIR);
	bind_textdomain_codeset (BINNAME, "UTF-8");
#endif
	//gtk initialization
	printd (DEBUG, "gtk init");
	gtk_init (&argc, &argv);
#ifndef USE_GTK2_12TIPS
	//log runtime version
	app.gtkversion = gtk_major_version * 10000 + gtk_minor_version * 100 + gtk_micro_version;
#endif
	g_set_application_name (_("emelFM2"));

#if 0
	gchar *parms[4] = { argv[0], "-c", "/home/maker/tmp", NULL };
	e2_cl_option_process (3, parms);
#else
	e2_cl_option_process (argc, argv);
#endif
	_e2_main_check_locale ();
//#ifndef E2_FILES_UTF8ONLY
//	e2_fs_check_coding ();	//setup for path/filename conversion if needed
//#else
	//set worst-case for commands etc, pending filelists loading which sets real state
	e2_utf8_set_name_conversion (TRUE);
	app.pane1.view.convert = app.pane2.view.convert = TRUE;
//#endif
	//send all messages to the handler
	GLogLevelFlags levels = G_LOG_LEVEL_MASK | G_LOG_FLAG_RECURSION | G_LOG_FLAG_FATAL;
	g_log_set_handler ("Glib", levels, _e2_main_message_handler, NULL);
	g_log_set_handler ("Gtk", levels, _e2_main_message_handler, NULL);

	//some local init stuff
	e2_action_setup_labels ();  //need this before option_init, to avoid crashes when there is no config file
	e2_option_setup_labels ();
	//after labels & before options setup, setup data for referencing toolbars
	e2_toolbar_data_create ();

	e2_option_init ();  //setup default options (before cache or config read)

	if (!e2_cl_options.original)
	{
		gboolean confdir_ok = e2_option_set_config_dir ();  //set name for config dir, make it if not there already
		e2_cache_init (confdir_ok);  //read and process cache file (if any) (before options init)
		gboolean confile_ok = (confdir_ok) ?
			e2_option_file_read (e2_cl_options.config_dir): //read & process saved options (if any)
			FALSE;
		//read & process shared options-data (if wanted)
		if (e2_cl_options.sharedconfig_dir != NULL)
			if (e2_option_file_read (e2_cl_options.sharedconfig_dir))
				confile_ok = TRUE;

		if (confile_ok)
		{
			e2_option_date_style (); //customise date-format option
			//ASAP after config settled
			if (strcmp (app.cfgfile_version, VERSION RELEASE) < 0)
			{
				E2P_InitData data;
				if (e2_plugins_open_module (PLUGINS_DIR G_DIR_SEPARATOR_S UPGRADE_PNAME, &data))	//localised path
				{
					//do any upgrades
					UpgradeIface *u = UPGRADE_IFACE ((*data.init) (E2P_SETUP));
					if (!(*u->update_config)())
						printd (ERROR, "Upgrade failed");
					g_module_close (data.module); //no cleanup needed, just dump it
				}
				else
					printd (ERROR, "Can't find upgrade plugin "UPGRADE_PNAME", so can't upgrade the config file");
			}
			gchar **values = e2_list_to_strv (e2_cl_options.option_overrides);
			e2_option_read_array (values);
			g_strfreev (values);
		}
	//	else
	//		app.keytrans = TRUE; //translate the newly-created keybindings, later on
	}
	//after config & updates, install default tree options where needed
	e2_option_tree_install_defaults ();

	//before plugins loaded, before bars/menus created
#ifdef E2_ICONCACHE
	e2_icons_cache_init ();
#endif
	e2_actions_init (); //setup action data stores & register most actions
	e2_complete_init (); //"like" actions

#ifndef USE_GTK2_12TIPS
	app.tooltips = gtk_tooltips_new ();
#endif

	//check if all plugin data is in config already
//	E2_OptionSet *set = e2_option_get ("plugins");  //internal name
//	gboolean defer_plugs = set->ex.tree.synced;
//	if (!defer_plugs)
	//if this is a fresh start,
	//do this before bars/menus created
	e2_plugins_load_configured (); // load plugins specified in the config data
	//some of these are used during pane creation
	app.dir_history = g_hash_table_new_full (
#ifdef E2_VFS
		g_direct_hash, g_direc_equal, NULL,
#else
		g_str_hash, g_str_equal, g_free,
#endif
		e2_fileview_clean1_history
	);

	//create and show the main window before the filelist creation (in case there's a big delay)
	e2_window_create (&app.window);
#ifndef E2_STATUS_DEMAND
	e2_window_enable_status_update (-1);
#endif
	e2_button_setup_labels ();	//maybe needed by dialog when opening dir
	e2_utils_translate_keys (TRUE, FALSE);	//setup for specific keycode management
	//all widgets are in place, register bindings in case needed in dialog
	e2_keybinding_register_all ();
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_register_all (); //includes gestures if relevant
#endif

	//set big refresh-counters, to block refresh-things before real start of monitoring
	app.pane1.view.listcontrols.refresh_refcount = 10;
	app.pane2.view.listcontrols.refresh_refcount = 10;
	cfg_refresh_refcount = 10;
	//threaded cd's may start at any time, including after main loop is running
	//and cd completion depends on speed of dir loading, so these funcs won't
	//set respective view.dir's synchronously
#ifdef E2_FAM
	//NOTE FAM mechanism not set up until after initial filelists created, but
	//because of values below and/or app.monitor_type starts as E2_MONITOR_DEFAULT,
	//nothing is done by backend before setup
	//signal unknown state
	app.pane1.FAMreq = -1;
	app.pane2.FAMreq = -1;
#endif
	e2_pane_goto_accessible_path (&app.pane2);
	//do pane 1 last, possibly making it the default at session start
	//(active pane is changed if needed in _e2_main_defer_init())
	e2_pane_goto_accessible_path (&app.pane1);
	//update window title to initial directory if wanted
	e2_window_set_title_path (app.main_window, &app.pane1.view);
	e2_window_set_title_path (app.main_window, &app.pane2.view);

	gint pos = gtk_paned_get_position (GTK_PANED (app.window.output_paned));
	if (pos == 0)
	{
		gchar *command = g_strconcat(_A(1),".",_A(50),NULL);  //_("command.focus")
		e2_action_run_simple_from (command, NULL, app.main_window);
		g_free (command);
	}

	e2_utils_update_gtk_settings ();

	e2_filetype_add_all ();	//process filetypes into data structure

	e2_cache_list_register ("open-history", &open_history); //history for 'open with' command
	//if no cache data from last usage, we don't want an unpleasant config dialog size
	e2_cache_int_register ("config-width", &app.cfg_alloc.width, 300);
	e2_cache_int_register ("config-height", &app.cfg_alloc.height, 400);
	e2_option_set_trash_dir ();

	GSList *tmp;
	for (tmp = e2_cl_options.startup_commands; tmp != NULL; tmp = g_slist_next (tmp))
		e2_command_run (tmp->data, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
		, FALSE
#endif
	);
	//if instructed, detach from controlling terminal and run in background
	if (e2_cl_options.detached
#ifdef DEBUG_MESSAGES
		&& (e2_cl_options.debug_level < 5)
#endif
	)
	{
#if 1
		if (daemon (1, 1) != 0)	//this func not available for System V-based OS
		{
			printd (WARN, "daemon creation failed, running normally instead");
		}
#else
		//FIXME manual daemon setup, as per www.netzmafia.de/skripten/unix/linux-daemon-howto.html
#endif
	}

	//trap signals that might be (but probably aren't) emitted when a
	//session-manager requests a shutdown/"save-yourself"
	//CHECKME only work if not running as daemon ?
	struct sigaction sigdata;
	sigdata.sa_handler = _e2_main_system_shutdown;
//	sigemptyset (&sigdata.sa_mask);	//don't block any signal while the handler is busy
	sigfillset (&sigdata.sa_mask);	//block all allowed signals while the handller is busy
	sigdata.sa_flags = 0;
//	sigaction (SIGKILL, &sigdata, NULL);	//CHECKME save this for stop without saving ?
	sigaction (SIGABRT, &sigdata, NULL);
	sigaction (SIGQUIT, &sigdata, NULL);
	sigaction (SIGTSTP, &sigdata, NULL);
	sigaction (SIGHUP, &sigdata, NULL);
	sigaction (SIGINT, &sigdata, NULL);	//this from a <Ctrl>c in a terminal
	sigaction (SIGTERM, &sigdata, NULL);

/*	//SIGPIPE ignorance maybe needed to ensure thread-safety ?
	//(but we don't want to pass that on to child processes)
//	sigemptyset (&sigdata.sa_mask);	//don't block any signal while the handler is busy
	sigaction (SIGPIPE, NULL, &sigdata);
	sigdata.sa_handler = SIG_IGN;
	sigaction (SIGPIPE, &sigdata, NULL);
*/
#ifdef E2_VFSTMP
	//session startup always uses local fs first, to prevent potential long
	//delay when app window is being created
	//now we can arrange to reinstate vfs state if appropriate
//now done in plugin init	g_idle_add ((GSourceFunc) e2_vfs_initialise, NULL);
#endif

	//start change polling etc after both initial filelists are completed
	g_idle_add ((GSourceFunc) _e2_main_defer_init, NULL);

	//get all actions, for help document
//	e2_action_list_all ();

	CLOSEBGL //gtk_main() opens lock before running loop
	gtk_main ();
	OPENBGL

	e2_main_closedown (TRUE, TRUE, TRUE); //should never happen
	exit (0); //ditto
}

  /************************/
 /*** departure lounge ***/
/************************/

/**
@brief session-end cleanups
Expects BGL on/closed, for any dialog or error message here or downstream
@param compulsory TRUE to prevent the user from cancelling
@param saveconfig TRUE to write config and cache files
@param doexit TRUE to do a normal program exit

@return FALSE if the user cancels or @a doexit is FALSE, else no return
*/
gboolean e2_main_closedown (gboolean compulsory, gboolean saveconfig, gboolean doexit)
{
	if (!compulsory)
	{
		if (e2_option_bool_get ("session-end-warning"))
		{
			//only count running child commands if they are in line for killing
			guint running = e2_command_count_running_tasks
				(!e2_option_bool_get ("command-persist"), TRUE);
			if (running > 0)
			{
				//can't use standard warning dialog, both its labels are unsuitable
				gchar *prompt = g_strdup_printf (_("%u process(es) are running"), running);
				GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_WARNING,
					prompt, _("confirm"), DUMMY_RESPONSE_CB, NULL);
				g_free (prompt);

				E2_Button yes_btn = { _("_Continue"), STOCK_NAME_EXECUTE,
					_("Continue working"), E2_BTN_DEFAULT | E2_BTN_TIPPED, 0,
					E2_BUTTON_YES.response };
				E2_Button no_btn = { _("_Quit"), STOCK_NAME_QUIT,
					NULL, 0, 0, E2_BUTTON_NO.response };

				DialogButtons choice = e2_dialog_show (dialog, app.main_window,
					E2_DIALOG_BLOCKED | E2_DIALOG_FREE,
					&yes_btn, &no_btn, NULL);

				if (choice != CANCEL)
					return FALSE;
			}
		}
	}

	guint i;
	for (i = 0; i < MAX_TIMERS; i++)
	{
		if (app.timers[i] > 0)
			g_source_remove (app.timers[i]);
	}
	//CHECKME other non-static timers e.g. keybinding timers, edit blink timers etc

	e2_task_cleanup (FALSE, pthread_self());	//cleanup action/command processing

#ifdef E2_FAM
	e2_fs_FAM_disconnect ();
#endif
#ifdef E2_HAL
	e2_hal_disconnect ();
#endif
#ifdef E2_DEVKIT
	e2_devkit_disconnect ();
#endif
#ifdef E2_ADD_STOCKS
	e2_icons_clear_stocks ();
#endif
#ifdef E2_ICONCACHE
	e2_icons_cache_clear ();
#endif
	//before unloading plugins, in case they have config options
	if (saveconfig && !e2_cl_options.original)
	{
		e2_option_file_write (NULL);
#ifdef USE_GTK2_18
		//main window dimensions for cache
		//FIXME handle maximised window
		gtk_widget_get_allocation (app.main_window, &app.main_alloc);
#endif
		if (e2_option_bool_get ("cache-history"))
		{	//limit the extent of cached history
			e2_pane_trim_history (&app.pane1);
			e2_pane_trim_history (&app.pane2);
		}
		e2_cache_file_write ();
		printd (DEBUG, "touch config dir");
		e2_fs_touch_config_dir (); //signal to the world that changes saved
	}
	e2_plugins_unload_all (TRUE);	//backup caches, cleanup etc
/*#ifdef E2_VFS
	if (vfs.loaded)
	{	//unloading does cacheing and proper cleanups
		Plugin *p = e2_plugins_get_installed ("vfs"VERSION);
		e2_plugins_unload1 (p, TRUE);
	}
#endif */
	if (app.pane1.hook_change_dir.is_setup)
		g_hook_list_clear (&app.pane1.hook_change_dir);
	if (app.pane2.hook_change_dir.is_setup)
		g_hook_list_clear (&app.pane2.hook_change_dir);
	if (app.pane1.view.hook_refresh.is_setup)
		g_hook_list_clear (&app.pane1.view.hook_refresh);
	if (app.pane2.view.hook_refresh.is_setup)
		g_hook_list_clear (&app.pane2.view.hook_refresh);
#ifdef USE_GLIB2_10
	e2_plugins_clean ();	//needs actions_hash
	GList *member;
	//need to deallocate the glib slices, at least
	//NOTE this must be after all cleanups involving actions
	//printd (DEBUG, "clean actions");
	//e2_actions_clean ()
	g_hash_table_destroy (actions_hash);
	e2_cache_clean ();
	e2_complete_clear ();
	e2_alias_clean ();
	e2_command_line_clean_all ();
	for (member = app.taskhistory; member != NULL; member = member->next)
	{
		//also free the data in the rt ??
		DEALLOCATE (E2_TaskRuntime, (E2_TaskRuntime *) member->data);
	}
	e2_command_clear_pending (NULL, NULL);
	//output tabs
	for (member = app.tabslist; member != NULL; member = member->next)
		DEALLOCATE (E2_OutputTabRuntime, (E2_OutputTabRuntime *) member->data);

	e2_toolbar_data_clean ();	//toolbar & related data
	e2_keybinding_clean (); //keybinding runtimes
	if (app.keysnative != NULL)
		g_hash_table_destroy (app.keysnative);
	if (app.keyslocal != NULL)
		g_hash_table_destroy (app.keyslocal);
/* FIXME deallocate other slices:
	//anything from e2_fileview_get_selected()
	//PlaceInfo's handled by unloading the vfs plugin
	//FileInfo's tied to treeview lines are cleaned already ?? HOW?
//unref all optiontree stores ?
	//view & edit dialog history items (if sliced!)
	//edit dialog undo items
	//ETC
*/
#endif
	//these might be in some backup place, for VFS
	//dirline histories ?
	//directory history list items
//	printd (DEBUG, "clear dirs hash");
	g_hash_table_destroy (app.dir_history);
	g_list_free (app.pane1.opendirs);	//list data are owned by app.dir_history
	g_list_free (app.pane2.opendirs);	//ditto
	e2_fileview_clear_filter_patterns (&app.pane1.view);
	e2_fileview_clear_filter_patterns (&app.pane2.view);
/*	printd (DEBUG, "clean bookmarks");
	e2_bookmark_clean ();
*/
	//NOTE this must be after all cleanups involving options
	g_hash_table_destroy (options_hash);
	e2_cl_option_clear ();

	if (doexit)
	{
		guint lvl = gtk_main_level ();
		while (lvl-- > 0)
			gtk_main_quit ();

		pthread_mutex_destroy (&list_mutex);
		pthread_mutex_destroy (&history_mutex);
#ifdef WAIT_POLL
		pthread_mutex_destroy (&wait_mutex);
#endif
		OPENBGL //open lock closed around gtk_main()
		pthread_mutex_destroy (&display_mutex);
		printd (DEBUG, "exit(0) from lounge");
		exit (0);
	}
	return FALSE;
}
/**
@brief user-initiated shutdown action

@param from the button, menu item etc which was activated
@param art action runtime data

@return FALSE if the user cancelled, or else it does not return at all
*/
gboolean e2_main_user_shutdown (gpointer from, E2_ActionRuntime *art)
{
	e2_main_closedown (FALSE, TRUE, TRUE);
	return FALSE;
}
/**
@brief perform system-initiated shutdown in repsonse to a shutdown signal
This is performed the "proper" way, so as to not block signalling to related
processes
@param num the signal number

@return
*/
static void _e2_main_system_shutdown (gint num)
{
	printd (DEBUG, "system shutdown initiated by signal %d", num);
	CLOSEBGL	//downstream errors expect BGL closed
	e2_main_closedown (TRUE, (num != SIGINT && num != SIGKILL), FALSE);
	OPENBGL
	//in this handler, current signal is blocked, unblock it for this use
	struct sigaction sigdata;
	sigdata.sa_handler = SIG_DFL;
	sigemptyset (&sigdata.sa_mask);
	sigdata.sa_flags = 0;
	sigaction (num, &sigdata, NULL);
	sigaddset (&sigdata.sa_mask, num);
	sigprocmask (SIG_UNBLOCK, &sigdata.sa_mask, NULL);
	kill (getpid(), num);
}
