/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 by the Claws Mail Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif

#ifdef CRASH_DIALOG

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>

#if HAVE_SYS_UTSNAME_H
#	include <sys/utsname.h>
#endif

#if defined(__GNU_LIBRARY__) && !defined(__UCLIBC__)
#	include <gnu/libc-version.h>
#endif

#include "main.h"
#include "claws.h"
#include "crash.h"
#include "utils.h"
#include "filesel.h"
#include "version.h"
#include "prefs_common.h"
#include "manage_window.h"

/*
 * NOTE: the crash dialog is called when claws is not 
 * initialized, so do not assume settings are available.
 * for example, loading / creating pixmaps seems not 
 * to be possible.
 */

/***/

static GtkWidget	*crash_dialog_show		(const gchar *text, 
							 const gchar *debug_output);
static void		 crash_create_debugger_file	(void);
static void		 crash_save_crash_log		(GtkButton *, const gchar *);
static void		 crash_create_bug_report	(GtkButton *, const gchar *);
static void		 crash_debug			(unsigned long crash_pid, 
							 gchar   *exe_image,
							 GString *debug_output);
static const gchar	*get_compiled_in_features	(void);
static const gchar	*get_lib_version		(void);
static const gchar	*get_operating_system		(void);
static gboolean		 is_crash_dialog_allowed	(void);
static void		 crash_handler			(int sig);
static void		 crash_cleanup_exit		(void);

/***/

static const gchar *DEBUG_SCRIPT = "thread all apply\nbt full\nkill\nq";

/***/

/*!
 *\brief	install crash handlers
 */
void crash_install_handlers(void)
{
#if CRASH_DIALOG 
	sigset_t mask;

	if (!is_crash_dialog_allowed()) return;

	sigemptyset(&mask);

#ifdef SIGSEGV
	signal(SIGSEGV, crash_handler);
	sigaddset(&mask, SIGSEGV);
#endif
	
#ifdef SIGFPE
	signal(SIGFPE, crash_handler);
	sigaddset(&mask, SIGFPE);
#endif

#ifdef SIGILL
	signal(SIGILL, crash_handler);
	sigaddset(&mask, SIGILL);
#endif

#ifdef SIGABRT
	signal(SIGABRT, crash_handler);
	sigaddset(&mask, SIGABRT);
#endif

	sigprocmask(SIG_UNBLOCK, &mask, 0);
#endif /* CRASH_DIALOG */	
}

/***/

/*!
 *\brief	crash dialog entry point 
 */
void crash_main(const char *arg) 
{
#if CRASH_DIALOG 
	gchar *text;
	gchar **tokens;
	unsigned long pid;
	GString *output;

	crash_create_debugger_file();
	tokens = g_strsplit(arg, ",", 0);

	pid = atol(tokens[0]);
	text = g_strdup_printf(_("Claws Mail process (%ld) received signal %ld"),
			       pid, atol(tokens[1]));

	output = g_string_new("");     
	crash_debug(pid, tokens[2], output);

	/*
	 * try to get the settings
	 */
	prefs_common_read_config();

	crash_dialog_show(text, output->str);
	g_string_free(output, TRUE);
	g_free(text);
	g_strfreev(tokens);
#endif /* CRASH_DIALOG */	
}

/*!
 *\brief	show crash dialog
 *
 *\param	text Description
 *\param	debug_output Output text by gdb
 *
 *\return	GtkWidget * Dialog widget
 */
static GtkWidget *crash_dialog_show(const gchar *text, const gchar *debug_output)
{
	GtkWidget *window1;
	GtkWidget *vbox1;
	GtkWidget *hbox1;
	GtkWidget *label1;
	GtkWidget *frame1;
	GtkWidget *scrolledwindow1;
	GtkWidget *text1;
	GtkWidget *hbuttonbox3;
	GtkWidget *hbuttonbox4;
	GtkWidget *button3;
	GtkWidget *button4;
	GtkWidget *button5;
	gchar	  *crash_report;
	GtkTextBuffer *buffer;
	GtkTextIter iter;

	window1 = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "crash");
	gtk_container_set_border_width(GTK_CONTAINER(window1), 5);
	gtk_window_set_title(GTK_WINDOW(window1), _("Claws Mail has crashed"));
	gtk_window_set_position(GTK_WINDOW(window1), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window1), TRUE);
	gtk_window_set_default_size(GTK_WINDOW(window1), 460, 272);


	vbox1 = gtk_vbox_new(FALSE, 2);
	gtk_widget_show(vbox1);
	gtk_container_add(GTK_CONTAINER(window1), vbox1);

	hbox1 = gtk_hbox_new(FALSE, 4);
	gtk_widget_show(hbox1);
	gtk_box_pack_start(GTK_BOX(vbox1), hbox1, FALSE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(hbox1), 4);

	label1 = gtk_label_new
	    (g_strdup_printf(_("%s.\nPlease file a bug report and include the information below."), text));
	gtk_widget_show(label1);
	gtk_box_pack_start(GTK_BOX(hbox1), label1, TRUE, TRUE, 0);
	gtk_misc_set_alignment(GTK_MISC(label1), 7.45058e-09, 0.5);

	frame1 = gtk_frame_new(_("Debug log"));
	gtk_widget_show(frame1);
	gtk_box_pack_start(GTK_BOX(vbox1), frame1, TRUE, TRUE, 0);

	scrolledwindow1 = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scrolledwindow1);
	gtk_container_add(GTK_CONTAINER(frame1), scrolledwindow1);
	gtk_container_set_border_width(GTK_CONTAINER(scrolledwindow1), 3);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwindow1),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

	text1 = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text1), FALSE);
	gtk_widget_show(text1);
	gtk_container_add(GTK_CONTAINER(scrolledwindow1), text1);
	
	crash_report = g_strdup_printf(
		"Claws Mail version %s\n"
		"GTK+ version %d.%d.%d / GLib %d.%d.%d\n"
		"Locale: %s (charset: %s)\n"
		"Features:%s\n"
		"Operating system: %s\n"
		"C Library: %s\n--\n%s",
		VERSION,
		gtk_major_version, gtk_minor_version, gtk_micro_version,
		glib_major_version, glib_minor_version, glib_micro_version,
		conv_get_current_locale(), conv_get_locale_charset_str(),
		get_compiled_in_features(),
		get_operating_system(),
		get_lib_version(),
		debug_output);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text1));
	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_insert(buffer, &iter, crash_report, -1);

	hbuttonbox3 = gtk_hbutton_box_new();
	gtk_widget_show(hbuttonbox3);
	gtk_box_pack_start(GTK_BOX(vbox1), hbuttonbox3, FALSE, FALSE, 0);

	hbuttonbox4 = gtk_hbutton_box_new();
	gtk_widget_show(hbuttonbox4);
	gtk_box_pack_start(GTK_BOX(vbox1), hbuttonbox4, FALSE, FALSE, 0);

	button3 = gtk_button_new_with_label(_("Close"));
	gtk_widget_show(button3);
	gtk_container_add(GTK_CONTAINER(hbuttonbox4), button3);
	gtkut_widget_set_can_default(button3, TRUE);

	button4 = gtk_button_new_with_label(_("Save..."));
	gtk_widget_show(button4);
	gtk_container_add(GTK_CONTAINER(hbuttonbox4), button4);
	gtkut_widget_set_can_default(button4, TRUE);

	button5 = gtk_button_new_with_label(_("Create bug report"));
	gtk_widget_show(button5);
	gtk_container_add(GTK_CONTAINER(hbuttonbox4), button5);
	gtkut_widget_set_can_default(button5, TRUE);
	
	g_signal_connect(G_OBJECT(window1), "delete_event",
			 G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(button3),   "clicked",
			 G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(button4), "clicked",
			 G_CALLBACK(crash_save_crash_log), crash_report);
	g_signal_connect(G_OBJECT(button5), "clicked",
			 G_CALLBACK(crash_create_bug_report), NULL);

	MANAGE_WINDOW_SIGNALS_CONNECT(window1);

	gtk_widget_show(window1);

	gtk_main();
	return window1;
}


/*!
 *\brief	create debugger script file in claws directory.
 *		all the other options (creating temp files) looked too 
 *		convoluted.
 */
static void crash_create_debugger_file(void)
{
	gchar *filespec = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, DEBUGGERRC, NULL);
	
	str_write_to_file(DEBUG_SCRIPT, filespec);
	g_free(filespec);
}

/*!
 *\brief	saves crash log to a file
 */
static void crash_save_crash_log(GtkButton *button, const gchar *text)
{
	time_t timer;
	struct tm *lt;
	char buf[100];
	struct tm buft;
	gchar *filename;

	timer = time(NULL);
	lt = localtime_r(&timer, &buft);
	strftime(buf, sizeof buf, "claws-crash-log-%Y-%m-%d-%H-%M-%S.txt", lt);
	if (NULL != (filename = filesel_select_file_save(_("Save crash information"), buf))
	&&  *filename)
		str_write_to_file(text, filename);
	g_free(filename);	
}

/*!
 *\brief	create bug report (goes to Claws Mail bug tracker)	
 */
static void crash_create_bug_report(GtkButton *button, const gchar *data)
{
	open_uri(BUGZILLA_URI, prefs_common_get_uri_cmd());
}

/*!
 *\brief	launches debugger and attaches it to crashed claws
 */
static void crash_debug(unsigned long crash_pid, 
			gchar *exe_image,
			GString *debug_output)
{
	int choutput[2];
	pid_t pid;

	pipe(choutput);

	if (0 == (pid = fork())) {
		char *argp[10];
		char **argptr = argp;
		gchar *filespec = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, DEBUGGERRC, NULL);

		setgid(getgid());
		setuid(getuid());

		/*
		 * setup debugger to attach to crashed claws
		 */
		*argptr++ = "gdb"; 
		*argptr++ = "--nw";
		*argptr++ = "--nx";
		*argptr++ = "--quiet";
		*argptr++ = "--batch";
		*argptr++ = "-x";
		*argptr++ = filespec;
		*argptr++ = exe_image;
		*argptr++ = g_strdup_printf("%ld", crash_pid);
		*argptr   = NULL;

		/*
		 * redirect output to write end of pipe
		 */
		close(1);
		dup(choutput[1]);
		close(choutput[0]);
		if (-1 == execvp("gdb", argp)) 
			g_print("error execvp\n");
	} else {
		char buf[100];
		int r;
	
		waitpid(pid, NULL, 0);

		/*
		 * make it non blocking
		 */
		if (-1 == fcntl(choutput[0], F_SETFL, O_NONBLOCK))
			g_print("set to non blocking failed\n");

		/*
		 * get the output
		 */
		do {
			r = read(choutput[0], buf, sizeof buf - 1);
			if (r > 0) {
				buf[r] = 0;
				g_string_append(debug_output, buf);
			}
		} while (r > 0);
		
		close(choutput[0]);
		close(choutput[1]);
		
		/*
		 * kill the process we attached to
		 */
		kill(crash_pid, SIGCONT); 
	}
}

/***/

/*!
 *\brief	features
 */
static const gchar *get_compiled_in_features(void)
{
	return g_strdup_printf("%s",
#if INET6
		   " IPv6"
#endif
#if HAVE_ICONV
		   " iconv"
#endif
#if HAVE_LIBCOMPFACE
		   " compface"
#endif
#if USE_GNUTLS
		   " GnuTLS"
#endif
#if USE_LDAP
		   " LDAP"
#endif
#if USE_JPILOT
		   " JPilot"
#endif
#if USE_ENCHANT
		   " GNU/aspell"
#endif
#if HAVE_LIBETPAN
		   " libetpan"
#endif
#if HAVE_LIBSM
		   " libSM"
#endif
	"");
}

/***/

/*!
 *\brief	library version
 */
static const gchar *get_lib_version(void)
{
#if defined(__UCLIBC__)
	return g_strdup_printf("uClibc %i.%i.%i", __UCLIBC_MAJOR__, __UCLIBC_MINOR__, __UCLIBC_SUBLEVEL__);
#elif defined(__GNU_LIBRARY__)
	return g_strdup_printf("GNU libc %s", gnu_get_libc_version());
#else
	return g_strdup(_("Unknown"));
#endif
}

/***/

/*!
 *\brief	operating system
 */
static const gchar *get_operating_system(void)
{
#if HAVE_SYS_UTSNAME_H
	struct utsname utsbuf;
	uname(&utsbuf);
	return g_strdup_printf("%s %s (%s)",
			       utsbuf.sysname,
			       utsbuf.release,
			       utsbuf.machine);
#else
	return g_strdup(_("Unknown"));
	
#endif
}

/***/

/*!
 *\brief	see if the crash dialog is allowed (because some
 *		developers may prefer to run claws-mail under gdb...)
 */
static gboolean is_crash_dialog_allowed(void)
{
	return !getenv("CLAWS_NO_CRASH");
}

/*!
 *\brief	this handler will probably evolve into 
 *		something better.
 */
static void crash_handler(int sig)
{
	pid_t pid;
	static volatile unsigned long crashed_ = 0;

	/*
	 * let's hope argv0 aren't trashed.
	 * both are defined in main.c.
	 */
	extern gchar *argv0;


	/*
	 * besides guarding entrancy it's probably also better 
	 * to mask off signals
	 */
	if (crashed_) return;

	crashed_++;

#ifdef SIGTERM
	if (sig == SIGTERM) 
		clean_quit(NULL);
#endif

	/*
	 * gnome ungrabs focus, and flushes gdk. mmmh, good idea.
	 */
	gdk_pointer_ungrab(GDK_CURRENT_TIME);
	gdk_keyboard_ungrab(GDK_CURRENT_TIME);
	gdk_flush();

	if (0 == (pid = fork())) {
		char buf[50];
		char *args[5];
	
		/*
		 * probably also some other parameters (like GTK+ ones).
		 * also we pass the full startup dir and the real command
		 * line typed in (argv0)
		 */
		args[0] = argv0; 
		args[1] = "--debug";
		args[2] = "--crash";
		sprintf(buf, "%d,%d,%s", getppid(), sig, argv0);
		args[3] = buf;
		args[4] = NULL;

		chdir(claws_get_startup_dir());
		setgid(getgid());
		setuid(getuid());
		execvp(argv0, args);
	} else {
		waitpid(pid, NULL, 0);
		crash_cleanup_exit();
		_exit(253);
	}

	_exit(253);
}

/*!
 *\brief	put all the things here we can do before
 *		letting the program die
 */
static void crash_cleanup_exit(void)
{
	const char *filename = claws_get_socket_name();
	claws_unlink(filename);
}

#endif
