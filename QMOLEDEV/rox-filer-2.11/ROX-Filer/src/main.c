/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* main.c - parses command-line options and parameters, plus some global
 * 	    housekeeping.
 *
 * New to the code and feeling lost? Read global.h now.
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <libxml/parser.h>

#ifdef HAVE_GETOPT_LONG
#  include <getopt.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdkx.h>		/* For rox_x_error */

#include "global.h"

#include "main.h"
#include "log.h"
#include "support.h"
#include "gui_support.h"
#include "filer.h"
#include "display.h"
#include "mount.h"
#include "menu.h"
#include "dnd.h"
#include "options.h"
#include "choices.h"
#include "type.h"
#include "pixmaps.h"
#include "dir.h"
#include "diritem.h"
#include "action.h"
#include "i18n.h"
#include "remote.h"
#include "pinboard.h"
#include "run.h"
#include "toolbar.h"
#include "bind.h"
#include "panel.h"
#include "session.h"
#include "minibuffer.h"
#include "xtypes.h"
#include "bulk_rename.h"
#include "gtksavebox.h"

int number_of_windows = 0;	/* Quit when this reaches 0 again... */
int to_wakeup_pipe = -1;	/* Write here to get noticed */

/* Information about the ROX-Filer process */
uid_t euid;
gid_t egid;
int ngroups;			/* Number of supplemental groups */
gid_t *supplemental_groups = NULL;

/* Message to display at the top of each filer window */
const gchar *show_user_message = NULL;

int home_dir_len;
const char *home_dir, *app_dir;

GtkTooltips *tooltips = NULL;

#define COPYING								\
	     N_("Copyright (C) 2005 Thomas Leonard.\n"			\
		"ROX-Filer comes with ABSOLUTELY NO WARRANTY,\n"	\
		"to the extent permitted by law.\n"			\
		"You may redistribute copies of ROX-Filer\n"		\
		"under the terms of the GNU General Public License.\n"	\
		"For more information about these matters, "		\
		"see the file named COPYING.\n")

#ifdef HAVE_GETOPT_LONG
#  define USAGE   N_("Try `ROX-Filer/AppRun --help' for more information.\n")
#  define SHORT_ONLY_WARNING ""
#else
#  define USAGE   N_("Try `ROX-Filer/AppRun -h' for more information.\n")
#  define SHORT_ONLY_WARNING	\
		_("NOTE: Your system does not support long options - \n" \
		"you must use the short versions instead.\n\n")
#endif

#define BUGS_TO "<rox-devel@lists.sourceforge.net>"

#define HELP N_("Usage: ROX-Filer/AppRun [OPTION]... [FILE]...\n"	\
       "Open each directory or file listed, or the current working\n"	\
       "directory if no arguments are given.\n\n"			\
       "  -b, --border=PANEL	open PANEL as a border panel\n"	\
       "  -B, --bottom=PANEL	open PAN as a bottom-edge panel\n"	\
       "  -c, --client-id=ID	used for session management\n"		\
       "  -d, --dir=DIR		open DIR as directory (not application)\n"  \
       "  -D, --close=DIR	close DIR and its subdirectories\n"     \
       "  -h, --help		display this help and exit\n"		\
       "  -l, --left=PANEL	open PAN as a left-edge panel\n"	\
       "  -m, --mime-type=FILE	print MIME type of FILE and exit\n" \
       "  -n, --new		start new copy; for debugging the filer\n"  \
       "  -p, --pinboard=PIN	use pinboard PIN as the pinboard\n"	\
       "  -r, --right=PANEL	open PAN as a right-edge panel\n"	\
       "  -R, --RPC		invoke method call read from stdin\n"	\
       "  -s, --show=FILE	open a directory showing FILE\n"	\
       "  -S, --rox-session	use default panel and pinboard options, and -n\n"\
       "  -t, --top=PANEL	open PANEL as a top-edge panel\n"	\
       "  -u, --user		show user name in each window \n"	\
       "  -U, --url=URL		open file or directory in URI form\n"   \
       "  -v, --version		display the version information and exit\n"   \
       "  -x, --examine=FILE	FILE has changed - re-examine it\n"	\
       "\nReport bugs to %s.\n"		\
       "Home page (including updated versions): http://rox.sourceforge.net/\n")

#define SHORT_OPS "c:d:t:b:l:r:B:op:s:hvnux:m:D:RSU:"

#ifdef HAVE_GETOPT_LONG
static struct option long_opts[] =
{
	{"dir", 1, NULL, 'd'},
	{"top", 1, NULL, 't'},
	{"bottom", 1, NULL, 'B'},
	{"border", 1, NULL, 'b'},
	{"left", 1, NULL, 'l'},
	{"override", 0, NULL, 'o'},
	{"pinboard", 1, NULL, 'p'},
	{"right", 1, NULL, 'r'},
	{"help", 0, NULL, 'h'},
	{"version", 0, NULL, 'v'},
	{"user", 0, NULL, 'u'},
	{"new", 0, NULL, 'n'},
	{"RPC", 0, NULL, 'R'},
	{"show", 1, NULL, 's'},
	{"rox-session", 0, NULL, 'S'},
	{"examine", 1, NULL, 'x'},
	{"close", 1, NULL, 'D'},
	{"mime-type", 1, NULL, 'm'},
	{"client-id", 1, NULL, 'c'},
	{"url", 1, NULL, 'u'},
	{NULL, 0, NULL, 0},
};
#endif

/* Take control of panels away from WM? */
Option o_override_redirect;

/* Options used when we are called by ROX-Session */
enum {
	SESSION_PANEL_ONLY,
	SESSION_PINBOARD_ONLY,
	SESSION_BOTH,
};
Option o_session_panel_or_pin;
Option o_session_panel_name;	/* Now a comma-separated list */
Option o_session_pinboard_name;

/* Always start a new filer, even if one seems to be already running */
gboolean new_copy = FALSE;

/* Maps child PIDs to Callback pointers */
static GHashTable *death_callbacks = NULL;
static gboolean child_died_flag = FALSE;

Option o_dnd_no_hostnames;

/* Static prototypes */
static void show_features(void);
static void soap_add(xmlNodePtr body,
			   xmlChar *function,
			   const xmlChar *arg1_name, const xmlChar *arg1_value,
			   const xmlChar *arg2_name, const xmlChar *arg2_value);
static void soap_reply(xmlDocPtr reply, gboolean rpc_mode);
static void child_died(int signum);
static void child_died_callback(void);
static void wake_up_cb(gpointer data, gint source, GdkInputCondition condition);
static void xrandr_size_change(GdkScreen *screen, gpointer user_data);
static void add_default_panel_and_pinboard(xmlNodePtr body);
static GList *build_launch(Option *option, xmlNode *node, guchar *label);
static GList *build_make_script(Option *option, xmlNode *node, guchar *label);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* The value that goes with an option */
#define VALUE (*optarg == '=' ? optarg + 1 : optarg)

static int rox_x_error(Display *display, XErrorEvent *error)
{
	gchar buf[64];

	XGetErrorText(display, error->error_code, buf, 63);

	g_warning ("The program '%s' received an X Window System error.\n"
			"This probably reflects a bug in the program.\n"
			"The error was '%s'.\n"
			"  (Details: serial %ld error_code %d request_code %d minor_code %d)\n"
			"  (Note to programmers: normally, X errors are reported asynchronously;\n"
			"   that is, you will receive the error a while after causing it.\n"
			"   To debug your program, run it with the --sync command line\n"
			"   option to change this behavior. You can then get a meaningful\n"
			"   backtrace from your debugger.)",
			g_get_prgname (),
			buf,
			error->serial, 
			error->error_code, 
			error->request_code,
			error->minor_code);

	/* Try to cope with BadWindow errors */
	if (error->error_code == BadWindow || error->error_code == BadDrawable)
	{
		g_warning(_("We got a BadWindow error from the X server. "
			    "This might be due to this GTK bug (during drag-and-drop?):\n"
			    "http://bugzilla.gnome.org/show_bug.cgi?id=152151\n"
			    "Trying to continue..."));
		return 0;
	}

	abort();
}

/* Parses the command-line to work out what the user wants to do.
 * Tries to send the request to an already-running copy of the filer.
 * If that fails, it initialises all the other modules and executes the
 * request itself.
 */
int main(int argc, char **argv)
{
	int		 wakeup_pipe[2];
	int		 i;
	struct sigaction act;
	guchar		*tmp, *dir;
	gchar *client_id = NULL;
	gboolean	show_user = FALSE;
	gboolean	rpc_mode = FALSE;
	xmlDocPtr	rpc, soap_rpc = NULL, reply;
	xmlNodePtr	body;
	int		fd, ofd0=-1;

	/* Relocate stdin. We do need it (-R), but it can cause problems if
	 * a child process wants a password, etc...
	 * Do this BEFORE opening anything (e.g., the X connection), in
	 * case fd 0 isn't open at this point.
	 */
	fd = open("/dev/null", O_RDONLY);
	if (fd > 0)
	{
		ofd0=dup(0);
		close(0);
		dup2(fd, 0);
		close(fd);
	}

	home_dir = g_get_home_dir();
	home_dir_len = strlen(home_dir);
	app_dir = g_strdup(getenv("APP_DIR"));

	/* Get internationalisation up and running. This requires the
	 * choices system, to discover the user's preferred language.
	 */
	choices_init();
	options_init();
	i18n_init();
	xattr_init();

	if (!app_dir)
	{
		g_warning("APP_DIR environment variable was unset!\n"
			"Use the AppRun script to invoke ROX-Filer...\n");
		app_dir = g_get_current_dir();
	}
#ifdef HAVE_UNSETENV 
	else
	{
		/* Don't pass it on to our child processes... */
		unsetenv("APP_DIR");
	}
#endif

	/* Sometimes we want to take special action when a child
	 * process exits. This hash table is used to convert the
	 * child's PID to the callback function.
	 */
	death_callbacks = g_hash_table_new(NULL, NULL);

	/* Find out some information about ourself */
	euid = geteuid();
	egid = getegid();
	ngroups = getgroups(0, NULL);
	if (ngroups < 0)
		ngroups = 0;
	else if (ngroups > 0)
	{
		supplemental_groups = g_malloc(sizeof(gid_t) * ngroups);
		getgroups(ngroups, supplemental_groups);
	}

	if (argc == 2 && strcmp(argv[1], "-v") == 0)
	{
		/* This is used by install.sh to test if the filer
		 * compiled OK. Do this test before gtk_init so that
		 * we don't need an X server to install.
		 */
		g_print("ROX-Filer %s\n", VERSION);
		g_print(_(COPYING));
		show_features();
		return EXIT_SUCCESS;
	}

	option_add_int(&o_override_redirect, "override_redirect", FALSE);

	option_add_int(&o_session_panel_or_pin, "session_panel_or_pin",
		       SESSION_BOTH);
	option_add_string(&o_session_panel_name, "session_panel_name",
			  "Default");
	option_add_string(&o_session_pinboard_name, "session_pinboard_name",
			  "Default");
	option_register_widget("launch", build_launch);
	option_register_widget("make-script", build_make_script);

#ifdef UNIT_TESTS
	bulk_rename_tests();
#endif

	/* The idea here is to convert the command-line arguments
	 * into a SOAP RPC.
	 * We attempt to invoke the call on an already-running copy of
	 * the filer if possible, or execute it ourselves if not.
	 */
	rpc = soap_new(&body);

	/* Note: must do this before checking our options,
	 * otherwise we report an error for Gtk's options.
	 */
	gtk_init(&argc, &argv);
	/* Set a default style for the collection widget */
	gtk_rc_parse_string("style \"rox-default-collection-style\" {\n"
		"  bg[NORMAL] = \"#f3f3f3\"\n"
		"  fg[NORMAL] = \"#000000\"\n"
		"  bg[INSENSITIVE] = \"#bfbfbf\"\n"
		"  fg[INSENSITIVE] = \"#000000\"\n"
		"}\n"
		"style \"rox-default-pinboard-style\" {\n"
		"  bg[NORMAL] = \"#666666\"\n"
		"}\n"
		"widget \"rox-pinboard\" style : gtk "
		"\"rox-default-pinboard-style\"\n"

		"class \"Collection\" style : gtk "
		"\"rox-default-collection-style\"\n");

	g_signal_connect(gdk_screen_get_default(), "size-changed",
			 G_CALLBACK(xrandr_size_change), NULL);

	/* Process each option in turn */
	while (1)
	{
		int	c;
#ifdef HAVE_GETOPT_LONG
		int	long_index;
		c = getopt_long(argc, argv, SHORT_OPS,
				long_opts, &long_index);
#else
		c = getopt(argc, argv, SHORT_OPS);
#endif

		if (c == EOF)
			break;		/* No more options */
		
		switch (c)
		{
			case 'n':
				new_copy = TRUE;
				break;
			case 'o':
				info_message(_("The -o argument is no longer "
					"used. You can turn on override "
					"redirect from the Options box "
					"instead."));
				break;
			case 'v':
				g_print("ROX-Filer %s\n", VERSION);
				g_print("%s", _(COPYING));
				show_features();
				return EXIT_SUCCESS;
			case 'h':
			        g_print(_(HELP), BUGS_TO);
				g_print("%s", _(SHORT_ONLY_WARNING));
				return EXIT_SUCCESS;
			case 'D':
			case 'd':
		        case 'x':
				/* Argument is a path */
				if (c == 'd' && VALUE[0] == '/')
					tmp = g_strdup(VALUE);
				else
					tmp = pathdup(VALUE);
				soap_add(body,
					c == 'D' ? "CloseDir" :
					c == 'd' ? "OpenDir" :
					c == 'x' ? "Examine" : "Unknown",
					"Filename", tmp,
					NULL, NULL);
				g_free(tmp);
				break;
			case 's':
				tmp = g_path_get_dirname(VALUE);
				
				if (tmp[0] == '/')
					dir = NULL;
				else
					dir = pathdup(tmp);

				soap_add(body, "Show",
					"Directory", dir ? dir : tmp,
					"Leafname", g_basename(VALUE));
				g_free(tmp);
				g_free(dir);
				break;
			case 'l':
			case 'r':
			case 't':
			case 'B':
				/* Argument is a leaf (or starts with /) */
				soap_add(body, "Panel", "Name", VALUE,
					 "Side", c == 'l' ? "Left" :
						 c == 'r' ? "Right" :
						 c == 't' ? "Top" :
						 c == 'B' ? "Bottom" :
						 "Unkown");
				break;
			case 'b':
				/* Argument is a leaf (or starts with /) */
				if (*VALUE)
					soap_add(body, "Panel", "Name", VALUE,
							NULL, NULL);
				else
					soap_add(body, "Panel",
							"Side", "Bottom",
							NULL, NULL);
				break;
			case 'p':
				soap_add(body, "Pinboard",
						"Name", VALUE, NULL, NULL);
				break;
			case 'u':
				show_user = TRUE;
				break;
		        case 'm':
			{
				MIME_type *type;
				type_init();
				diritem_init();
				pixmaps_init();
				type = type_get_type(VALUE);
				printf("%s/%s\n", type->media_type,
						type->subtype);
				return EXIT_SUCCESS;
			}
			case 'c':
				client_id = g_strdup(VALUE);
				break;
			case 'R':
				/* Reconnect stdin */
				if(ofd0>-1) {
					close(0);
					dup2(ofd0, 0);
				}
				soap_rpc = xmlParseFile("-");
				if (!soap_rpc)
					g_error("Invalid XML in RPC");
				/* Disconnect stdin again */
				fd = open("/dev/null", O_RDONLY);
				if (fd > 0)
				{
					close(0);
					dup2(fd, 0);
					close(fd);
				}
				/* Want to print return uninterpreted */
				rpc_mode=TRUE;
				
				break;

			case 'S':
				new_copy = TRUE;
				add_default_panel_and_pinboard(body);
				session_auto_respawn = TRUE;
				break;

		        case 'U':
				soap_add(body, "RunURI",
						"URI", VALUE, NULL, NULL);
				break;

			default:
				printf(_(USAGE));
				return EXIT_FAILURE;
		}
	}

	tooltips = gtk_tooltips_new();

	if (euid == 0 || show_user)
		show_user_message = g_strdup_printf(_("Running as user '%s'"), 
						    user_name(euid));
	
	/* Add each remaining (non-option) argument to the list of files
	 * to run.
	 */
	i = optind;
	while (i < argc)
	{
		tmp = pathdup(argv[i++]);

		soap_add(body, "Run", "Filename", tmp, NULL, NULL);

		g_free(tmp);
	}

	if (soap_rpc)
	{
		if (body->xmlChildrenNode)
			g_error("Can't use -R with other options - sorry!");
		xmlFreeDoc(rpc);
		body = NULL;
		rpc = soap_rpc;
	}
	else if (!body->xmlChildrenNode)
	{
		/* The user didn't request any action. Open the current
		 * directory.
		 */
		guchar	*dir;

		dir = g_get_current_dir();
		soap_add(body, "OpenDir", "Filename", dir, NULL, NULL);
		g_free(dir);
	}

	option_add_int(&o_dnd_no_hostnames, "dnd_no_hostnames", 1);

	/* Try to send the request to an already-running copy of the filer */
	gui_support_init();
	if (remote_init(rpc, new_copy))
		return EXIT_SUCCESS;	/* It worked - exit */

	/* Put ourselves into the background (so 'rox' always works the
	 * same, whether we're already running or not).
	 * Not for -n, though (helps when debugging).
	 */
	if (!new_copy)
	{
		pid_t child;

		child = fork();
		if (child > 0)
			_exit(0);	/* Parent exits */
		/* Otherwise we're the child (or an error occurred - ignore
		 * it!).
		 */
	}

	/* Initialize the rest of the filer... */

	pixmaps_init();

	log_init();
	dnd_init();
	bind_init();
	dir_init();
	diritem_init();
	menu_init();
	minibuffer_init();
	filer_init();
	toolbar_init();
	display_init();
	mount_init();
	type_init();
	action_init();

	pinboard_init();
	panel_init();

	/* Let everyone update */
	options_notify();

	/* When we get a signal, we can't do much right then. Instead,
	 * we send a char down this pipe, which causes the main loop to
	 * deal with the event next time we're idle.
	 */
	pipe(wakeup_pipe);
	close_on_exec(wakeup_pipe[0], TRUE);
	close_on_exec(wakeup_pipe[1], TRUE);
	gdk_input_add_full(wakeup_pipe[0], GDK_INPUT_READ, wake_up_cb,
			NULL, NULL);
	to_wakeup_pipe = wakeup_pipe[1];

	/* If the pipe is full then we're going to get woken up anyway... */
	set_blocking(to_wakeup_pipe, FALSE);

	/* Let child processes die */
	act.sa_handler = child_died;
	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_NOCLDSTOP;
	sigaction(SIGCHLD, &act, NULL);

	/* Ignore SIGPIPE - check for EPIPE errors instead */
	act.sa_handler = SIG_IGN;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGPIPE, &act, NULL);

	/* Set up session managament if available */
	session_init(client_id);
	g_free(client_id);

	/* See if we need to migrate the Choices directories*/
	choices_migrate();

	/* Finally, execute the request */
	reply = run_soap(rpc);
	xmlFreeDoc(rpc);
	soap_reply(reply, rpc_mode);

	/* Try to find out why we crash with GTK 2.4 */
	XSetErrorHandler(rox_x_error);

	/* Enter the main loop, processing events until all our windows
	 * are closed.
	 */
	if (number_of_windows > 0)
		gtk_main();

	return EXIT_SUCCESS;
}

/* Register a function to be called when process number 'child' dies. */
void on_child_death(gint child, CallbackFn callback, gpointer data)
{
	Callback	*cb;

	g_return_if_fail(callback != NULL);

	cb = g_new(Callback, 1);

	cb->callback = callback;
	cb->data = data;

	g_hash_table_insert(death_callbacks, GINT_TO_POINTER(child), cb);
}

void one_less_window(void)
{
	if (--number_of_windows < 1)
		gtk_main_quit();
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void show_features(void)
{
	g_print("\n");
	g_print(_("Compiled with GTK version %s\n"), GTK_VERSION);
	g_print(_("Running with GTK version %d.%d.%d\n"),
				gtk_major_version,
				gtk_minor_version,
				gtk_micro_version);
	g_print("\n-- %s --\n\n", _("features set at compile time"));
	g_print("%s... %s\n", _("Large File Support"),
#ifdef LARGE_FILE_SUPPORT
		_("Yes")
#else
		_("No")
#endif
		);
	g_print("%s... %s\n", _("Inotify support"),
#ifdef USE_INOTIFY
		_("Yes")
#else
		_("No")
#endif
	       );
	g_print("%s... %s\n", _("Dnotify support"),
#ifdef USE_DNOTIFY
		_("Yes")
#else
		_("No")
#endif
	       );
	g_print("%s... %s\n", _("Binary compatibility"),
#if defined(HAVE_APSYMBOLS_H) || defined(HAVE_APBUILD_APSYMBOLS_H)
		_("Yes (can run with older glibc versions)")
#else
		_("No (apsymbols.h not found)")
#endif
	       );

	g_print("%s... %s\n", _("Extended attribute support"),
		xattr_supported(NULL)? _("Yes"): _("No"));
}

static void soap_add(xmlNodePtr body,
			   xmlChar *function,
			   const xmlChar *arg1_name, const xmlChar *arg1_value,
			   const xmlChar *arg2_name, const xmlChar *arg2_value)
{
	xmlNodePtr node;
	xmlNs *rox;

	rox = xmlSearchNsByHref(body->doc, body, ROX_NS);
	
	node = xmlNewChild(body, rox, function, NULL);

	if (arg1_name)
	{
		xmlNewTextChild(node, rox, arg1_name, arg1_value);
		if (arg2_name)
			xmlNewTextChild(node, rox, arg2_name, arg2_value);
	}
}

static void soap_reply(xmlDocPtr reply, gboolean rpc_mode)
{
	gboolean print=TRUE;

	if(!reply)
		return;

	if(!rpc_mode) {
		gchar **errs=extract_soap_errors(reply);

		if(errs) {
			int i;
			
			print=FALSE;

			for(i=0; errs[i]; i++)
				fprintf(stderr, "%s\n", errs[i]);

			g_strfreev(errs);
		}
	}

	/* Write the result, if any, to stdout */
	if(print)
		save_xml_file(reply, "-");
	xmlFreeDoc(reply);
}

/* This is called as a signal handler; simply ensures that
 * child_died_callback() will get called later.
 */
static void child_died(int signum)
{
	child_died_flag = TRUE;
	write(to_wakeup_pipe, "\0", 1);	/* Wake up! */
}

static void child_died_callback(void)
{
	int	    	status;
	gint	    	child;

	child_died_flag = FALSE;

	/* Find out which children exited and allow them to die */
	do
	{
		Callback	*cb;

		child = waitpid(-1, &status, WNOHANG);

		if (child == 0 || child == -1)
			return;

		cb = g_hash_table_lookup(death_callbacks,
				GINT_TO_POINTER(child));
		if (cb)
		{
			cb->callback(cb->data);
			g_hash_table_remove(death_callbacks,
					GINT_TO_POINTER(child));
		}

	} while (1);
}

#define BUFLEN 40
/* When data is written to_wakeup_pipe, this gets called from the event
 * loop some time later. Useful for getting out of signal handlers, etc.
 */
static void wake_up_cb(gpointer data, gint source, GdkInputCondition condition)
{
	char buf[BUFLEN];

	read(source, buf, BUFLEN);
	
	if (child_died_flag)
		child_died_callback();
#ifdef USE_DNOTIFY
	if (dnotify_wakeup_flag)
		dnotify_wakeup();
#endif
}

static void xrandr_size_change(GdkScreen *screen, gpointer user_data)
{
	gui_store_screen_geometry(screen);

	panel_update_size();
	pinboard_update_size();
}

static void add_default_panel_and_pinboard(xmlNodePtr body)
{
	const char *name;

	if (o_session_panel_or_pin.int_value != SESSION_PANEL_ONLY)
	{
		name=o_session_pinboard_name.value;
		if (!name[0])
			name="Default";
		soap_add(body, "Pinboard","Name", name, NULL, NULL);
	}
					
	if (o_session_panel_or_pin.int_value != SESSION_PINBOARD_ONLY)
	{
		gboolean use_old_option = TRUE;
		GIOChannel *fp = NULL;
		GError *err = NULL;
		char *line = NULL;
		gsize term;
		char *filename = choices_find_xdg_path_load("panels",
				"ROX-Filer", "rox.sourceforge.net");

		if (filename)
			fp = g_io_channel_new_file(filename, "r", &err);
		while (fp && g_io_channel_read_line(fp, &line, NULL, &term, &err) ==
				G_IO_STATUS_NORMAL)
		{
			if (line && (line[term] = 0, line[0]))
			{
				soap_add(body, "Panel", "Name", line, NULL, NULL);
				use_old_option = FALSE;
			}
		}
		if (err)
		{
			g_critical(_("Unable to read '%s': %s"),
					filename, err->message);
			g_error_free(err);
		}
		if (fp)
			g_io_channel_shutdown(fp, FALSE, NULL);
		if (use_old_option)
		{
			name = o_session_panel_name.value;
			if (!name[0])
				name="Default";
			soap_add(body, "Panel", "Name", name, NULL, NULL);
		}
		g_free(filename);
	}
}

static GtkWidget *launch_button_new(const char *label, const char *uri,
				    const char *appname)
{
	GtkWidget *button;
	GClosure *closure;
	const gchar *slash;
	gchar *tip;

	button = button_new_mixed(GTK_STOCK_PREFERENCES, label);
	closure = g_cclosure_new(G_CALLBACK(launch_uri),
					g_strdup(uri),
					(GClosureNotify) g_free);
	g_signal_connect_closure(button, "clicked", closure, FALSE);
	if(appname) {
		g_object_set_data_full(G_OBJECT(button), "appname",
				       g_strdup(appname),
				       (GDestroyNotify) g_free);
	}
	
	allow_right_click(button);

	slash = strrchr(uri, '/');
	if (!slash)
		slash = uri - 1;
	tip = g_strdup_printf(
			_("Left-click to run %s.\n"
			  "Right-click for a list of versions."),
			slash + 1);

	gtk_tooltips_set_tip(tooltips, button, tip, NULL);

	g_free(tip);

	return button;
}

static GList *build_launch(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *align;
	char *uri;
	char *appname;

	g_return_val_if_fail(option == NULL, NULL);
	g_return_val_if_fail(label != NULL, NULL);

	uri = xmlGetProp(node, "uri");
	appname = xmlGetProp(node, "appname");

	g_return_val_if_fail(uri != NULL, NULL);

	align = gtk_alignment_new(0, 0.5, 0, 0);

	gtk_container_add(GTK_CONTAINER(align),
			  launch_button_new(_(label), uri, appname));

	g_free(uri);
	if(appname)
	  g_free(appname);

	return g_list_append(NULL, align);
}

/* Call back from save box to create a rox script */
static gint new_script_cb(GObject *savebox,
			 const gchar *path, gpointer data)
{
       FILE *fp;

       fp = fopen(path, "w");

       if (fp == NULL)
       {
               report_error(_("Error creating '%s': %s"),
                               path, g_strerror(errno));
	       return GTK_XDS_SAVE_ERROR;
        }

       fprintf(fp, "#!/bin/sh\n");
       fprintf(fp, "exec %s/AppRun \"$@\"\n", app_dir);

       fclose(fp);
       chmod(path, 0755);

       dir_check_this(path);

       return GTK_XDS_SAVED;
}

/* Option button to create the rox script clicked */
static void make_script_clicked(GtkWidget *button, gpointer udata)
{
	const gchar *filename;
	const gchar *action;
	GtkWidget   *savebox;
	MaskedPixmap *image;

	/* Default to saving in current filer window */
	if(window_with_focus)
		filename=make_path(window_with_focus->sym_path, "rox");
	else
		filename="rox";
	action = _("Start script");
	image = type_to_icon(application_x_shellscript);

	/* Create a save box to save the script */
	savebox = gtk_savebox_new(_("Save"));
	gtk_savebox_set_action(GTK_SAVEBOX(savebox), GDK_ACTION_COPY);
	g_signal_connect(savebox, "save_to_file",
				G_CALLBACK(new_script_cb), NULL);

	gtk_window_set_title(GTK_WINDOW(savebox), _("Start script"));

	gtk_savebox_set_pathname(GTK_SAVEBOX(savebox), filename);
	gtk_savebox_set_icon(GTK_SAVEBOX(savebox), image->pixbuf);
	g_object_unref(image);
				
	gtk_widget_show(savebox);
}

/* Build option button to create rox script */
static GList *build_make_script(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *align;
	GtkWidget *button;
	gchar     *tip;

	g_return_val_if_fail(option == NULL, NULL);
	g_return_val_if_fail(label != NULL, NULL);

	align = gtk_alignment_new(0, 0.5, 0, 0);

	button = gtk_button_new_with_label(_(label));
	g_signal_connect(button, "clicked", G_CALLBACK(make_script_clicked),
			 NULL);
	
	tip = _("Click to save a script to run ROX-Filer.\n"
		"If you are using Zero Install you should use 0alias "
		"instead.");
	gtk_tooltips_set_tip(tooltips, button, tip, NULL);

	gtk_container_add(GTK_CONTAINER(align), button);

	return g_list_append(NULL, align);
}
