/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-keyring-daemon.c - main keyring daemon code.

   Copyright (C) 2003 Red Hat, Inc

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Alexander Larsson <alexl@redhat.com>
   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gkd-glue.h"
#include "gkd-main.h"
#include "gkd-capability.h"
#include "gkd-pkcs11.h"
#include "gkd-util.h"

#include "control/gkd-control.h"

#include "dbus/gkd-dbus.h"

#include "egg/egg-cleanup.h"
#include "egg/egg-error.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-unix-credentials.h"

#include "login/gkd-login.h"

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <locale.h>
#include <syslog.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <glib.h>
#include <glib/gi18n.h>

#include <gcrypt.h>

/* preset file descriptors */
#define  STDIN   0
#define  STDOUT  1
#define  STDERR  2

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t;
#endif

#define GKD_COMP_KEYRING    "keyring"
#define GKD_COMP_PKCS11     "pkcs11"
#define GKD_COMP_SECRETS    "secrets"
#define GKD_COMP_SSH        "ssh"
#define GKD_COMP_GPG        "gpg"

/* -----------------------------------------------------------------------------
 * COMMAND LINE
 */

/* All the components to run on startup if not specified on command line */
#ifdef WITH_SSH
#	ifdef WITH_GPG
#		define DEFAULT_COMPONENTS  GKD_COMP_PKCS11 "," GKD_COMP_SECRETS "," GKD_COMP_SSH "," GKD_COMP_GPG
#	else
#		define DEFAULT_COMPONENTS  GKD_COMP_PKCS11 "," GKD_COMP_SECRETS "," GKD_COMP_SSH
#	endif
#else
#	ifdef WITH_GPG
#		define DEFAULT_COMPONENTS  GKD_COMP_PKCS11 "," GKD_COMP_SECRETS  "," GKD_COMP_GPG
#	else
#		define DEFAULT_COMPONENTS  GKD_COMP_PKCS11 "," GKD_COMP_SECRETS
#	endif
#endif

/*
 * If --login is used and then daemon is not initialized within LOGIN_TIMEOUT
 * seconds, then we exit. See on_login_timeout() below.
 */

#define LOGIN_TIMEOUT 120

static gchar* run_components = DEFAULT_COMPONENTS;
static gboolean pkcs11_started = FALSE;
static gboolean secrets_started = FALSE;
static gboolean ssh_started = FALSE;
static gboolean gpg_started = FALSE;
static gboolean dbus_started = FALSE;

static gboolean run_foreground = FALSE;
static gboolean run_daemonized = FALSE;
static gboolean run_version = FALSE;
static gboolean run_for_login = FALSE;
static gboolean run_for_start = FALSE;
static gboolean run_for_replace = FALSE;
static gchar* login_password = NULL;
static gchar* control_directory = NULL;
static guint timeout_id = 0;
static gboolean initialization_completed = FALSE;
static gboolean sig_thread_valid = FALSE;
static pthread_t sig_thread;

static GOptionEntry option_entries[] = {
	{ "start", 's', 0, G_OPTION_ARG_NONE, &run_for_start,
	  "Start a dameon or initialize an already running daemon." },
	{ "replace", 'r', 0, G_OPTION_ARG_NONE, &run_for_replace,
	  "Replace the daemon for this desktop login environment." },
	{ "foreground", 'f', 0, G_OPTION_ARG_NONE, &run_foreground,
	  "Run in the foreground", NULL },
	{ "daemonize", 'd', 0, G_OPTION_ARG_NONE, &run_daemonized,
	  "Run as a daemon", NULL },
	{ "login", 'l', 0, G_OPTION_ARG_NONE, &run_for_login,
	  "Run for a user login. Read login password from stdin", NULL },
	{ "components", 'c', 0, G_OPTION_ARG_STRING, &run_components,
	  "The optional components to run", DEFAULT_COMPONENTS },
	{ "control-directory", 'C', 0, G_OPTION_ARG_FILENAME, &control_directory,
	  "The directory for sockets and control data", NULL },
	{ "version", 'V', 0, G_OPTION_ARG_NONE, &run_version,
	  "Show the version number and exit.", NULL },
	{ NULL }
};

static void
parse_arguments (int *argc, char** argv[])
{
	GError *err = NULL;
	GOptionContext *context;

	context = g_option_context_new ("- The Gnome Keyring Daemon");
	g_option_context_add_main_entries (context, option_entries, GETTEXT_PACKAGE);

	if (!g_option_context_parse (context, argc, argv, &err)) {
		g_printerr ("gnome-keyring-daemon: %s", egg_error_message (err));
		g_clear_error (&err);
	}

	if (!run_components || !run_components[0]) {
		run_components = DEFAULT_COMPONENTS;
	} else {
		run_components = g_strdup (run_components);
		egg_cleanup_register (g_free, run_components);
	}

	/* Check the arguments */
	if (run_for_login && run_for_start) {
		g_printerr ("gnome-keyring-daemon: The --start option is incompatible with --login");
		run_for_login = FALSE;
	}

	if (run_for_login && run_for_replace) {
		g_printerr ("gnome-keyring-daemon: The --replace option is incompatible with --login");
		run_for_login = FALSE;
	}

	if (run_for_start && run_for_replace) {
		g_printerr ("gnome-keyring-daemon: The --replace option is incompatible with --start");
		run_for_start = FALSE;
	}

	g_option_context_free (context);
}

/* -----------------------------------------------------------------------------
 * MEMORY
 */

static gboolean do_warning = TRUE;
#define WARNING  "couldn't allocate secure memory to keep passwords " \
                 "and or keys from being written to the disk"

#define ABORTMSG "The GNOME_KEYRING_PARANOID environment variable was set. " \
                 "Exiting..."


/*
 * These are called from gkr-secure-memory.c to provide appropriate
 * locking for memory between threads
 */

G_LOCK_DEFINE_STATIC (memory_mutex);

void
egg_memory_lock (void)
{
	G_LOCK (memory_mutex);
}

void
egg_memory_unlock (void)
{
	G_UNLOCK (memory_mutex);
}

void*
egg_memory_fallback (void *p, size_t sz)
{
	const gchar *env;

	/* We were asked to free memory */
	if (!sz) {
		g_free (p);
		return NULL;
	}

	/* We were asked to allocate */
	if (!p) {
		if (do_warning) {
			g_message (WARNING);
			do_warning = FALSE;
		}

		env = g_getenv ("GNOME_KEYRING_PARANOID");
		if (env && *env)
			g_error (ABORTMSG);

		return g_malloc0 (sz);
	}

	/*
	 * Reallocation is a bit of a gray area, as we can be asked
	 * by external libraries (like libgcrypt) to reallocate a
	 * non-secure block into secure memory. We cannot satisfy
	 * this request (as we don't know the size of the original
	 * block) so we just try our best here.
	 */

	return g_realloc (p, sz);
}

/* -----------------------------------------------------------------------------
 * LOGS
 */

static void
log_handler (const gchar *log_domain, GLogLevelFlags log_level,
             const gchar *message, gpointer user_data)
{
	int level;

	/* Note that crit and err are the other way around in syslog */

	switch (G_LOG_LEVEL_MASK & log_level) {
	case G_LOG_LEVEL_ERROR:
		level = LOG_CRIT;
		break;
	case G_LOG_LEVEL_CRITICAL:
		level = LOG_ERR;
		break;
	case G_LOG_LEVEL_WARNING:
		level = LOG_WARNING;
		break;
	case G_LOG_LEVEL_MESSAGE:
		level = LOG_NOTICE;
		break;
	case G_LOG_LEVEL_INFO:
		level = LOG_INFO;
		break;
	case G_LOG_LEVEL_DEBUG:
		level = LOG_DEBUG;
		break;
	default:
		level = LOG_ERR;
		break;
	}

	/* Log to syslog first */
	if (log_domain)
		syslog (level, "%s: %s", log_domain, message);
	else
		syslog (level, "%s", message);

	/* And then to default handler for aborting and stuff like that */
	g_log_default_handler (log_domain, log_level, message, user_data);
}

static void
printerr_handler (const gchar *string)
{
	/* Print to syslog and stderr */
	syslog (LOG_WARNING, "%s", string);
	fprintf (stderr, "%s", string);
}

static void
prepare_logging ()
{
	GLogLevelFlags flags = G_LOG_FLAG_FATAL | G_LOG_LEVEL_ERROR |
	                       G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING |
	                       G_LOG_LEVEL_MESSAGE | G_LOG_LEVEL_INFO;

	openlog ("gnome-keyring-daemon", LOG_PID, LOG_AUTH);

	g_log_set_handler (NULL, flags, log_handler, NULL);
	g_log_set_handler ("Glib", flags, log_handler, NULL);
	g_log_set_handler ("Gtk", flags, log_handler, NULL);
	g_log_set_handler ("Gnome", flags, log_handler, NULL);
	g_log_set_default_handler (log_handler, NULL);
	g_set_printerr_handler (printerr_handler);
}

/* -----------------------------------------------------------------------------
 * SIGNALS
 */

static sigset_t signal_set;
static gint signal_quitting = 0;

static gpointer
signal_thread (gpointer user_data)
{
	GMainLoop *loop = user_data;
	int sig, err;

	for (;;) {
		err = sigwait (&signal_set, &sig);
		if (err != EINTR && err != 0) {
			g_warning ("couldn't wait for signals: %s", g_strerror (err));
			return NULL;
		}

		switch (sig) {
		case SIGPIPE:
			/* Ignore */
			break;
		case SIGHUP:
		case SIGTERM:
			g_atomic_int_set (&signal_quitting, 1);
			g_main_loop_quit (loop);
			return NULL;
		default:
			g_warning ("received unexpected signal when waiting for signals: %d", sig);
			break;
		}
	}

	g_assert_not_reached ();
	return NULL;
}

static void
setup_signal_handling (GMainLoop *loop)
{
	int res;

	/*
	 * Block these signals for this thread, and any threads
	 * started up after this point (so essentially all threads).
	 *
	 * We also start a signal handling thread which uses signal_set
	 * to catch the various signals we're interested in.
	 */

	sigemptyset (&signal_set);
	sigaddset (&signal_set, SIGPIPE);
	sigaddset (&signal_set, SIGHUP);
	sigaddset (&signal_set, SIGTERM);
	pthread_sigmask (SIG_BLOCK, &signal_set, NULL);

	res = pthread_create (&sig_thread, NULL, signal_thread, loop);
	if (res == 0) {
		sig_thread_valid = TRUE;
	} else {
		g_warning ("couldn't startup thread for signal handling: %s",
		           g_strerror (res));
	}
}

void
gkd_main_quit (void)
{
	/*
	 * Send a signal to terminate our signal thread,
	 * which in turn runs stops the main loop and that
	 * starts the shutdown process.
	 */

	if (sig_thread_valid)
		pthread_kill (sig_thread, SIGTERM);
	else
		raise (SIGTERM);
}

static void
cleanup_signal_handling (void)
{
	/* The thread is not joinable, so cleans itself up */
	if (!g_atomic_int_get (&signal_quitting))
		g_warning ("gkr_daemon_quit() was not used to shutdown the daemon");
}

/* -----------------------------------------------------------------------------
 * STARTUP
 */

static int
sane_dup2 (int fd1, int fd2)
{
	int ret;

 retry:
	ret = dup2 (fd1, fd2);
	if (ret < 0 && errno == EINTR)
		goto retry;

	return ret;
}

static gchar*
read_login_password (int fd)
{
	/* We only accept a max of 8K as the login password */
	#define MAX_LENGTH 8192
	#define MAX_BLOCK 256

	/*
	 * When --login is specified then the login password is passed
	 * in on stdin. All data (including newlines) are part of the
	 * password. A zero length password is no password.
	 */

	gchar *buf = egg_secure_alloc (MAX_BLOCK);
	gchar *ret = NULL;
	int r, len = 0;

	for (;;) {
		r = read (fd, buf, sizeof (buf));
		if (r < 0) {
			if (errno == EAGAIN)
				continue;
			egg_secure_free (ret);
			egg_secure_free (buf);
			return NULL;

		} else if (r == 0 || len > MAX_LENGTH) {
			break;

		} else {
			ret = egg_secure_realloc (ret, len + r + 1);
			memset (ret + len, 0, r + 1);
			len = len + r;
			strncat (ret, buf, r);
		}
	}

	egg_secure_free (buf);
	return ret;
}

static void
cleanup_and_exit (int code)
{
	egg_cleanup_perform ();
	exit (code);
}

static void
clear_login_password (void)
{
	if(login_password)
		egg_secure_strfree (login_password);
	login_password = NULL;
}

static void
print_environment (pid_t pid)
{
	const gchar **env;
	for (env = gkd_util_get_environment (); *env; ++env)
		printf ("%s\n", *env);
	if (pid)
		printf ("GNOME_KEYRING_PID=%d\n", (gint)pid);
}

static gboolean
initialize_daemon_at (const gchar *directory)
{
	gchar **ourenv, **daemonenv, **e;

	/* Exchange environment variables, and try to initialize daemon */
	ourenv = gkd_util_build_environment (GKD_UTIL_IN_ENVIRONMENT);
	daemonenv = gkd_control_initialize (directory, run_components,
	                                    (const gchar**)ourenv);
	g_strfreev (ourenv);

	/* Initialization failed, start this process up as a daemon */
	if (!daemonenv)
		return FALSE;

	/* Setup all the environment variables we were passed */
	for (e = daemonenv; *e; ++e)
		gkd_util_push_environment_full (*e);
	g_strfreev (daemonenv);

	return TRUE;
}

static gboolean
replace_daemon_at (const gchar *directory)
{
	g_free (control_directory);
	control_directory = g_strdup (directory);
	return gkd_control_quit (directory);
}

typedef gboolean (*DiscoverFunc) (const gchar *control_directory);

static gboolean
discover_other_daemon (DiscoverFunc callback, gboolean acquire)
{
	const gchar *control_env;
	gchar *control = NULL;
	gboolean acquired = FALSE;
	gboolean ret;

	/* A pre-specified directory to control at, don't try anything else */
	if (control_directory)
		return (callback) (control_directory);

	/* An environment variable from an already running daemon */
	control_env = g_getenv (GKD_UTIL_ENV_CONTROL);
	if (control_env && control_env[0]) {
		if ((callback)(control_env))
			return TRUE;
	}

	/* See if we can contact a daemon running, that didn't set an env variable */
	if (acquire && !gkd_dbus_singleton_acquire (&acquired))
		return FALSE;

	/* We're the main daemon */
	if (acquired)
		return FALSE;

	control = gkd_dbus_singleton_control ();
	if (control) {
		ret = (callback) (control);
		g_free (control);
		if (ret == TRUE)
			return TRUE;
	}

	return FALSE;
}

static void
fork_and_print_environment (void)
{
	int status;
	pid_t pid;
	int fd, i;

	if (run_foreground) {
		print_environment (getpid ());
		return;
	}

	pid = fork ();

	if (pid != 0) {

		/* Here we are in the initial process */

		if (run_daemonized) {

			/* Initial process, waits for intermediate child */
			if (pid == -1)
				exit (1);

			waitpid (pid, &status, 0);
			if (WEXITSTATUS (status) != 0)
				exit (WEXITSTATUS (status));

		} else {
			/* Not double forking, we know the PID */
			print_environment (pid);
		}

		/* The initial process exits successfully */
		exit (0);
	}

	if (run_daemonized) {

		/* Double fork if need to daemonize properly */
		pid = fork ();

		if (pid != 0) {

			/* Here we are in the intermediate child process */

			/*
			 * This process exits, so that the final child will inherit
			 * init as parent to avoid zombies
			 */
			if (pid == -1)
				exit (1);

			/* We've done two forks. Now we know the PID */
			print_environment (pid);

			/* The intermediate child exits */
			exit (0);
		}

	}

	/* Here we are in the resulting daemon or background process. */

	for (i = 0; i < 3; ++i) {
		fd = open ("/dev/null", O_RDONLY);
		sane_dup2 (fd, i);
		close (fd);
	}
}

static gboolean
gkr_daemon_startup_steps (const gchar *components)
{
	g_assert (components);

	/*
	 * Startup that must run before forking.
	 * Note that we set initialized flags early so that two
	 * initializations don't overlap
	 */

#ifdef WITH_SSH
	if (strstr (components, GKD_COMP_SSH)) {
		if (ssh_started) {
			g_message ("The SSH agent was already initialized");
		} else {
			ssh_started = TRUE;
			if (!gkd_daemon_startup_ssh ()) {
				ssh_started = FALSE;
				return FALSE;
			}
		}
	}
#endif

#ifdef WITH_GPG
	if (strstr (components, GKD_COMP_GPG)) {
		if (gpg_started) {
			g_message ("The GPG agent was already initialized");
		} else {
			gpg_started = TRUE;
			if (!gkd_daemon_startup_gpg ()) {
				gpg_started = FALSE;
				return FALSE;
			}
		}
	}
#endif

	return TRUE;
}

static gboolean
gkr_daemon_initialize_steps (const gchar *components)
{
	g_assert (components);

	/*
	 * Startup that can run after forking.
	 * Note that we set initialized flags early so that two
	 * initializations don't overlap
	 */

	if (!initialization_completed) {
		initialization_completed = TRUE;
		if (timeout_id)
			g_source_remove (timeout_id);

		/* Initialize new style PKCS#11 components */
		if (!gkd_pkcs11_initialize ())
			return FALSE;

		/*
		 * Unlock the login keyring if we were given a password on STDIN.
		 * If it does not exist. We create it.
		 */
		if (login_password) {
			if (!gkd_login_unlock (login_password))
				g_message ("failed to unlock login keyring on startup");
			egg_secure_strclear (login_password);
		}

		dbus_started = TRUE;
		if (!gkd_dbus_setup ())
			dbus_started = FALSE;
	}

	/* The Secret Service API */
	if (strstr (components, GKD_COMP_SECRETS) || strstr (components, GKD_COMP_KEYRING)) {
		if (secrets_started) {
			g_message ("The Secret Service was already initialized");
		} else {
			if (!dbus_started) {
				dbus_started = TRUE;
				if (!gkd_dbus_setup ())
					dbus_started = FALSE;
			}
			if (dbus_started) {
				secrets_started = TRUE;
				if (!gkd_dbus_secrets_startup ()) {
					secrets_started = FALSE;
					return FALSE;
				}
			}
		}
	}

	/* The PKCS#11 remoting */
	if (strstr (components, GKD_COMP_PKCS11)) {
		if (pkcs11_started) {
			g_message ("The PKCS#11 component was already initialized");
		} else {
			pkcs11_started = TRUE;
			if (!gkd_pkcs11_startup_pkcs11 ()) {
				pkcs11_started = FALSE;
				return FALSE;
			}
		}
	}

	return TRUE;
}

void
gkd_main_complete_initialization (const gchar *components)
{
	g_assert (components);

	/*
	 * Sometimes we don't initialize the full daemon right on
	 * startup. When run with --login is one such case.
	 */

	gkr_daemon_startup_steps (components);
	gkr_daemon_initialize_steps (components);
}

static gboolean
on_login_timeout (gpointer data)
{
	if (!initialization_completed)
		cleanup_and_exit (0);
	return FALSE;
}

int
main (int argc, char *argv[])
{
	GMainLoop *loop;

	/*
	 * The gnome-keyring startup is not as simple as I wish it could be.
	 *
	 * It's often started in the primordial stages of a session, where
	 * there's no DBus, and no proper X display. This is the strange world
	 * of PAM.
	 *
	 * When started with the --login option, we do as little initialization
	 * as possible. We expect a login password on the stdin, and unlock
	 * or create the login keyring.
	 *
	 * Then later we expect gnome-keyring-dameon to be run again with the
	 * --start option. This second gnome-keyring-daemon will hook the
	 * original daemon up with environment variables necessary to initialize
	 * itself and bring it into the session. This second daemon usually exits.
	 *
	 * Without either of these options, we follow a more boring and
	 * predictable startup.
	 */

	/*
	 * Before we do ANYTHING, we drop privileges so we don't become
	 * a security issue ourselves.
	 */
	gkd_capability_obtain_capability_and_drop_privileges ();

#ifdef WITH_TESTABLE
	g_setenv ("DBUS_FATAL_WARNINGS", "1", FALSE);
	if (!g_getenv ("G_DEBUG"))
		g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING);
#endif

	g_type_init ();
	g_thread_init (NULL);

#ifdef HAVE_LOCALE_H
	/* internationalisation */
	setlocale (LC_ALL, "");
#endif

#ifdef HAVE_GETTEXT
	bindtextdomain (GETTEXT_PACKAGE, GNOMELOCALEDIR);
	textdomain (GETTEXT_PACKAGE);
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif

	egg_libgcrypt_initialize ();

	/* Send all warning or error messages to syslog */
	prepare_logging ();

	parse_arguments (&argc, &argv);

	/* The --version option. This is machine parseable output */
	if (run_version) {
		g_print ("gnome-keyring-daemon: %s\n", VERSION);
		g_print ("testing: %s\n",
#ifdef WITH_TESTABLE
		         "enabled");
#else
		         "disabled");
#endif
		exit (0);
	}

	/* The --start option */
	if (run_for_start) {
		if (discover_other_daemon (initialize_daemon_at, TRUE)) {
			/*
			 * Another daemon was initialized, print out environment
			 * for any callers, and quit or go comatose.
			 */
			print_environment (0);
			if (run_foreground)
				while (sleep(0x08000000) == 0);
			cleanup_and_exit (0);
		}

	/* The --replace option */
	} else if (run_for_replace) {
		discover_other_daemon (replace_daemon_at, FALSE);
		if (control_directory)
			g_message ("replacing daemon at: %s", control_directory);
	}

	/* Initialize the main directory */
	gkd_util_init_master_directory (control_directory);

	/* Initialize our daemon main loop and threading */
	loop = g_main_loop_new (NULL, FALSE);

	/* Initialize our control socket */
	if (!gkd_control_listen ())
		return FALSE;

	/* The --login option. Delayed initialization */
	if (run_for_login) {
		login_password = read_login_password (STDIN);
		atexit (clear_login_password);
		timeout_id = g_timeout_add_seconds (LOGIN_TIMEOUT, (GSourceFunc) on_login_timeout, NULL);

	/* Not a login daemon. Startup stuff now.*/
	} else {
		/* These are things that can run before forking */
		if (!gkr_daemon_startup_steps (run_components))
			cleanup_and_exit (1);
	}

	/* The whole forking and daemonizing dance starts here. */
	fork_and_print_environment();

	setup_signal_handling (loop);

	/* Prepare logging a second time, since we may be in a different process */
	prepare_logging();

	/* Remainder initialization after forking, if initialization not delayed */
	if (!run_for_login) {
		gkr_daemon_initialize_steps (run_components);
	}

	g_main_loop_run (loop);

	/* This wraps everything up in order */
	egg_cleanup_perform ();

	/* Wrap up signal handling here */
	cleanup_signal_handling ();

	g_free (control_directory);

	return 0;
}
