/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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
#  include "config.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef G_OS_UNIX
#  include <signal.h>
#  include <errno.h>
#  include <fcntl.h>
#endif
#ifdef HAVE_LIBSM
#include <X11/SM/SMlib.h>
#endif

#include "wizard.h"
#ifdef HAVE_STARTUP_NOTIFICATION
# define SN_API_NOT_YET_FROZEN
# include <libsn/sn-launchee.h>
# include <gdk/gdkx.h>
#endif

#ifdef HAVE_DBUS_GLIB
#include <dbus/dbus-glib.h>
#endif
#ifdef HAVE_NETWORKMANAGER_SUPPORT
#include <NetworkManager.h>
#endif
#ifdef HAVE_VALGRIND
#include <valgrind.h>
#endif

#include "claws.h"
#include "main.h"
#include "mainwindow.h"
#include "folderview.h"
#include "image_viewer.h"
#include "summaryview.h"
#include "prefs_common.h"
#include "prefs_account.h"
#include "prefs_actions.h"
#include "prefs_ext_prog.h"
#include "prefs_fonts.h"
#include "prefs_image_viewer.h"
#include "prefs_message.h"
#include "prefs_receive.h"
#include "prefs_msg_colors.h"
#include "prefs_quote.h"
#include "prefs_spelling.h"
#include "prefs_summaries.h"
#include "prefs_themes.h"
#include "prefs_other.h"
#include "prefs_logging.h"
#include "prefs_send.h"
#include "prefs_wrapping.h"
#include "prefs_compose_writing.h"
#include "prefs_display_header.h"
#include "account.h"
#include "procmsg.h"
#include "inc.h"
#include "import.h"
#include "manage_window.h"
#include "alertpanel.h"
#include "statusbar.h"
#include "addressbook.h"
#include "compose.h"
#include "folder.h"
#include "setup.h"
#include "utils.h"
#include "gtkutils.h"
#include "socket.h"
#include "log.h"
#include "prefs_toolbar.h"
#include "plugin.h"
#include "mh_gtk.h"
#include "imap_gtk.h"
#include "news_gtk.h"
#include "matcher.h"
#include "tags.h"
#include "hooks.h"
#include "menu.h"
#include "quicksearch.h"

#ifdef HAVE_LIBETPAN
#include "imap-thread.h"
#include "nntp-thread.h"
#endif
#include "stock_pixmap.h"
#ifdef USE_GNUTLS
#  include "ssl.h"
#endif

#include "version.h"

#include "crash.h"

#include "timing.h"

#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-banner.h>
#include <hildon/hildon-program.h>
#else
#include <hildon-widgets/hildon-banner.h>
#include <hildon-widgets/hildon-program.h>
#endif
#include <libosso.h>
#include <libgnomevfs/gnome-vfs-volume.h>
#include <libgnomevfs/gnome-vfs-volume-monitor.h>
#include <libgnomevfs/gnome-vfs-utils.h>

#define OSSO_NAME    "claws_mail"
#define OSSO_SERVICE "com.nokia."OSSO_NAME
#define OSSO_OBJECT  "/com/nokia/"OSSO_NAME
#define OSSO_IFACE   "com.nokia."OSSO_NAME

typedef struct _AppData AppData;
struct _AppData {
    HildonProgram *program;
    HildonWindow *window;
    osso_context_t *osso_context;
};

static GnomeVFSVolumeMonitor *volmon;
#endif

#ifdef HAVE_NETWORKMANAGER_SUPPORT
/* Went offline due to NetworkManager */
static gboolean went_offline_nm;
#endif

#if !defined(NM_CHECK_VERSION)
#define NM_CHECK_VERSION(x,y,z) 0
#endif

#ifdef HAVE_DBUS_GLIB
static DBusGProxy *awn_proxy = NULL;
#endif

gchar *prog_version;
gchar *argv0;

#ifdef MAEMO
HildonProgram *hildon_program;
#endif

#ifdef HAVE_STARTUP_NOTIFICATION
static SnLauncheeContext *sn_context = NULL;
static SnDisplay *sn_display = NULL;
#endif

static gint lock_socket = -1;
static gint lock_socket_tag = 0;
static gchar *x_display = NULL;
typedef enum 
{
	ONLINE_MODE_DONT_CHANGE,
 	ONLINE_MODE_ONLINE,
	ONLINE_MODE_OFFLINE
} OnlineMode;

static struct RemoteCmd {
	gboolean receive;
	gboolean receive_all;
	gboolean compose;
	const gchar *compose_mailto;
	GPtrArray *attach_files;
	gboolean search;
	const gchar *search_folder;
	const gchar *search_type;
	const gchar *search_request;
	gboolean search_recursive;
	gboolean status;
	gboolean status_full;
	GPtrArray *status_folders;
	GPtrArray *status_full_folders;
	gboolean send;
	gboolean crash;
	int online_mode;
	gchar   *crash_params;
	gboolean exit;
	gboolean subscribe;
	const gchar *subscribe_uri;
	const gchar *target;
} cmd;

static void parse_cmd_opt(int argc, char *argv[]);

static gint prohibit_duplicate_launch	(void);
static gchar * get_crashfile_name	(void);
static gint lock_socket_remove		(void);
static void lock_socket_input_cb	(gpointer	   data,
					 gint		   source,
					 GdkInputCondition condition);

static void open_compose_new		(const gchar	*address,
					 GPtrArray	*attach_files);

static void send_queue			(void);
static void initial_processing		(FolderItem *item, gpointer data);
static void quit_signal_handler         (int sig);
static void install_basic_sighandlers   (void);
#if (defined linux && defined SIGIO)
static void install_memory_sighandler   (void);
#endif
static void exit_claws			(MainWindow *mainwin);

#ifdef HAVE_NETWORKMANAGER_SUPPORT
static void networkmanager_state_change_cb(DBusGProxy *proxy, gchar *dev,
																					 gpointer data);
#endif

#define MAKE_DIR_IF_NOT_EXIST(dir) \
{ \
	if (!is_dir_exist(dir)) { \
		if (is_file_exist(dir)) { \
			alertpanel_warning \
				(_("File '%s' already exists.\n" \
				   "Can't create folder."), \
				 dir); \
			return 1; \
		} \
		if (make_dir(dir) < 0) \
			return 1; \
	} \
}

static MainWindow *static_mainwindow;

#ifdef MAEMO
static osso_context_t *static_osso_context;

void exit_event_handler(gboolean die_now, gpointer data)
{
	AppData *appdata;
	appdata = (AppData *) data;
	/* Do whatever application needs to do before exiting */
	exit_claws(static_mainwindow);
	hildon_banner_show_information(GTK_WIDGET(appdata->window), NULL,
                                   _("Exiting..."));

}

/* Callback for hardware D-BUS events */
void hw_event_handler(osso_hw_state_t *state, gpointer data)
{
	AppData *appdata;
	appdata = (AppData *) data;

	if (state->shutdown_ind) {
		exit_claws(static_mainwindow);
		hildon_banner_show_information(GTK_WIDGET(appdata->window), NULL,
			_("Exiting..."));
	}
}

/* Callback for normal D-BUS messages */
gint dbus_req_handler(const gchar * interface, const gchar * method,
                      GArray * arguments, gpointer data,
                      osso_rpc_t * retval)
{
    AppData *appdata;
    appdata = (AppData *) data;

    if (!strcmp(method, "top_application")) {
	    osso_rpc_free_val(retval);
	    return OSSO_OK;
    }
    osso_system_note_infoprint(appdata->osso_context, method, retval);
    osso_rpc_free_val(retval);

    return OSSO_OK;
}
#endif
static gboolean emergency_exit = FALSE;

#ifdef HAVE_STARTUP_NOTIFICATION
static void sn_error_trap_push(SnDisplay *display, Display *xdisplay)
{
	gdk_error_trap_push();
}

static void sn_error_trap_pop(SnDisplay *display, Display *xdisplay)
{
	gdk_error_trap_pop();
}

static void startup_notification_complete(gboolean with_window)
{
	Display *xdisplay;
	GtkWidget *hack = NULL;

	if (with_window) {
		/* this is needed to make the startup notification leave,
		 * if we have been launched from a menu.
		 * We have to display a window, so let it be very little */
		hack = gtk_window_new(GTK_WINDOW_POPUP);
		gtk_window_move(GTK_WINDOW(hack), 0, 0);
		gtk_widget_set_size_request(hack, 1, 1);
		gtk_widget_show(hack);
	}

	xdisplay = GDK_DISPLAY_XDISPLAY(gdk_display_get_default());
	sn_display = sn_display_new(xdisplay,
				sn_error_trap_push,
				sn_error_trap_pop);
	sn_context = sn_launchee_context_new_from_environment(sn_display,
						 DefaultScreen(xdisplay));

	if (sn_context != NULL)	{
		sn_launchee_context_complete(sn_context);
		sn_launchee_context_unref(sn_context);
		sn_display_unref(sn_display);
	}
	if (with_window) {
		gtk_widget_destroy(hack);
	}
}
#endif /* HAVE_STARTUP_NOTIFICATION */

static void claws_gtk_idle(void) 
{
	while(gtk_events_pending()) {
		gtk_main_iteration();
	}
	g_usleep(50000);
}

static gboolean sc_starting = FALSE;

static gboolean defer_check_all(void *data)
{
	gboolean autochk = GPOINTER_TO_INT(data);

	inc_all_account_mail(static_mainwindow, autochk, 
			prefs_common.newmail_notify_manu);

	if (sc_starting) {
		sc_starting = FALSE;
		main_window_set_menu_sensitive(static_mainwindow);
		toolbar_main_set_sensitive(static_mainwindow);
	}
	return FALSE;
}

static gboolean defer_check(void *data)
{
	inc_mail(static_mainwindow, prefs_common.newmail_notify_manu);

	if (sc_starting) {
		sc_starting = FALSE;
		main_window_set_menu_sensitive(static_mainwindow);
		toolbar_main_set_sensitive(static_mainwindow);
	}
	return FALSE;
}

static gboolean defer_jump(void *data)
{
	if (cmd.receive_all) {
		defer_check_all(GINT_TO_POINTER(FALSE));
	} else if (prefs_common.chk_on_startup) {
		defer_check_all(GINT_TO_POINTER(TRUE));
	} else if (cmd.receive) {
		defer_check(NULL);
	} 
	mainwindow_jump_to(data, FALSE);
	if (sc_starting) {
		sc_starting = FALSE;
		main_window_set_menu_sensitive(static_mainwindow);
		toolbar_main_set_sensitive(static_mainwindow);
	}
	return FALSE;
}

static void chk_update_val(GtkWidget *widget, gpointer data)
{
        gboolean *val = (gboolean *)data;
	*val = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}

static gboolean migrate_old_config(const gchar *old_cfg_dir, const gchar *new_cfg_dir, const gchar *oldversion)
{
	gchar *message = g_strdup_printf(_("Configuration for %s found.\n"
			 "Do you want to migrate this configuration?"), oldversion);
	gchar *message2 = g_strdup_printf(_("\n\nYour Sylpheed filtering rules can be converted by a\n"
			     "script available at %s."), TOOLS_URI);

	if (!strcmp(oldversion, "Sylpheed"))
		message = g_strconcat(message, message2, NULL);
	g_free(message2);

	gint r = 0;
	GtkWidget *window = NULL;
	GtkWidget *keep_backup_chk;
	CLAWS_TIP_DECL();
	gboolean backup = TRUE;

	keep_backup_chk = gtk_check_button_new_with_label (_("Keep old configuration"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(keep_backup_chk), TRUE);
	CLAWS_SET_TIP(keep_backup_chk,
			     _("Keeping a backup will allow you to go back to an "
			       "older version, but may take a while if you have "
			       "cached IMAP or News data, and will take some extra "
			       "room on your disk."));

	g_signal_connect(G_OBJECT(keep_backup_chk), "toggled", 
			G_CALLBACK(chk_update_val), &backup);

	if (alertpanel_full(_("Migration of configuration"), message,
		 	GTK_STOCK_NO, "+" GTK_STOCK_YES, NULL, FALSE,
			keep_backup_chk, ALERT_QUESTION, G_ALERTDEFAULT) != G_ALERTALTERNATE) {
		return FALSE;
	}
	
	/* we can either do a fast migration requiring not any extra disk
	 * space, or a slow one that copies the old configuration and leaves
	 * it in place. */
	if (backup) {
backup_mode:
		window = label_window_create(_("Copying configuration... This may take a while..."));
		GTK_EVENTS_FLUSH();
		
		r = copy_dir(old_cfg_dir, new_cfg_dir);
		label_window_destroy(window);
		
		/* if copy failed, we'll remove the partially copied
		 * new directory */
		if (r != 0) {
			alertpanel_error(_("Migration failed!"));
			remove_dir_recursive(new_cfg_dir);
		} else {
			if (!backup) {
				/* fast mode failed, but we don't want backup */
				remove_dir_recursive(old_cfg_dir);
			}
		}
	} else {
		window = label_window_create(_("Migrating configuration..."));
		GTK_EVENTS_FLUSH();
		
		r = g_rename(old_cfg_dir, new_cfg_dir);
		label_window_destroy(window);
		
		/* if g_rename failed, we'll try to copy */
		if (r != 0) {
			FILE_OP_ERROR(new_cfg_dir, "g_rename failed, trying copy\n");
			goto backup_mode;
		}
	}
	return (r == 0);
}

static int migrate_common_rc(const gchar *old_rc, const gchar *new_rc)
{
	FILE *oldfp, *newfp;
	gchar *plugin_path, *old_plugin_path, *new_plugin_path;
	gchar buf[BUFFSIZE];
	gboolean err = FALSE;

	oldfp = g_fopen(old_rc, "r");
	if (!oldfp)
		return -1;
	newfp = g_fopen(new_rc, "w");
	if (!newfp) {
		fclose(oldfp);
		return -1;
	}
	
	plugin_path = g_strdup(get_plugin_dir());
	new_plugin_path = g_strdup(plugin_path);
	
	if (strstr(plugin_path, "/claws-mail/")) {
		gchar *end = g_strdup(strstr(plugin_path, "/claws-mail/")+strlen("/claws-mail/"));
		*(strstr(plugin_path, "/claws-mail/")) = '\0';
		old_plugin_path = g_strconcat(plugin_path, "/sylpheed-claws/", end, NULL);
		g_free(end);
	} else {
		old_plugin_path = g_strdup(new_plugin_path);
	}
	debug_print("replacing %s with %s\n", old_plugin_path, new_plugin_path);
	while (fgets(buf, sizeof(buf), oldfp)) {
		if (strncmp(buf, old_plugin_path, strlen(old_plugin_path))) {
			err |= (fputs(buf, newfp) == EOF);
		} else {
			debug_print("->replacing %s", buf);
			debug_print("  with %s%s", new_plugin_path, buf+strlen(old_plugin_path));
			err |= (fputs(new_plugin_path, newfp) == EOF);
			err |= (fputs(buf+strlen(old_plugin_path), newfp) == EOF);
		}
	}
	g_free(plugin_path);
	g_free(new_plugin_path);
	g_free(old_plugin_path);
	fclose(oldfp);
	if (fclose(newfp) == EOF)
		err = TRUE;
	
	return (err ? -1:0);
}

#ifdef HAVE_LIBSM
static void
sc_client_set_value (MainWindow *mainwin,
		  gchar       *name,
		  char        *type,
		  int          num_vals,
		  SmPropValue *vals)
{
	SmProp *proplist[1];
	SmProp prop;

	prop.name = name;
	prop.type = type;
	prop.num_vals = num_vals;
	prop.vals = vals;

	proplist[0]= &prop;
	if (mainwin->smc_conn)
		SmcSetProperties ((SmcConn) mainwin->smc_conn, 1, proplist);
}

static void sc_die_callback (SmcConn smc_conn, SmPointer client_data)
{
	clean_quit(NULL);
}

static void sc_save_complete_callback(SmcConn smc_conn, SmPointer client_data)
{
}

static void sc_shutdown_cancelled_callback (SmcConn smc_conn, SmPointer client_data)
{
	MainWindow *mainwin = (MainWindow *)client_data;
	if (mainwin->smc_conn)
		SmcSaveYourselfDone ((SmcConn) mainwin->smc_conn, TRUE);
}

static void sc_save_yourself_callback (SmcConn   smc_conn,
			       SmPointer client_data,
			       int       save_style,
			       gboolean  shutdown,
			       int       interact_style,
			       gboolean  fast) {

	MainWindow *mainwin = (MainWindow *)client_data;
	if (mainwin->smc_conn)
		SmcSaveYourselfDone ((SmcConn) mainwin->smc_conn, TRUE);
}

static IceIOErrorHandler sc_ice_installed_handler;

static void sc_ice_io_error_handler (IceConn connection)
{
	if (sc_ice_installed_handler)
		(*sc_ice_installed_handler) (connection);
}
static gboolean sc_process_ice_messages (GIOChannel   *source,
		      GIOCondition  condition,
		      gpointer      data)
{
	IceConn connection = (IceConn) data;
	IceProcessMessagesStatus status;

	status = IceProcessMessages (connection, NULL, NULL);

	if (status == IceProcessMessagesIOError) {
		IcePointer context = IceGetConnectionContext (connection);

		if (context && GTK_IS_OBJECT (context)) {
		guint disconnect_id = g_signal_lookup ("disconnect", G_OBJECT_TYPE (context));

		if (disconnect_id > 0)
			g_signal_emit (context, disconnect_id, 0);
		} else {
			IceSetShutdownNegotiation (connection, False);
			IceCloseConnection (connection);
		}
	}

	return TRUE;
}

static void new_ice_connection (IceConn connection, IcePointer client_data, Bool opening,
		    IcePointer *watch_data)
{
	guint input_id;

	if (opening) {
		GIOChannel *channel;
		/* Make sure we don't pass on these file descriptors to any
		exec'ed children */
		fcntl(IceConnectionNumber(connection),F_SETFD,
		fcntl(IceConnectionNumber(connection),F_GETFD,0) | FD_CLOEXEC);

		channel = g_io_channel_unix_new (IceConnectionNumber (connection));
		input_id = g_io_add_watch (channel,
		G_IO_IN | G_IO_HUP | G_IO_ERR | G_IO_PRI,
		sc_process_ice_messages,
		connection);
		g_io_channel_unref (channel);

		*watch_data = (IcePointer) GUINT_TO_POINTER (input_id);
	} else {
		input_id = GPOINTER_TO_UINT ((gpointer) *watch_data);
		g_source_remove (input_id);
	}
}

static void sc_session_manager_connect(MainWindow *mainwin)
{
	static gboolean connected = FALSE;
	SmcCallbacks      callbacks;
	gchar            *client_id;
	IceIOErrorHandler default_handler;

	if (connected)
		return;
	connected = TRUE;


	sc_ice_installed_handler = IceSetIOErrorHandler (NULL);
	default_handler = IceSetIOErrorHandler (sc_ice_io_error_handler);

	if (sc_ice_installed_handler == default_handler)
		sc_ice_installed_handler = NULL;

	IceAddConnectionWatch (new_ice_connection, NULL);
      
      
      	callbacks.save_yourself.callback      = sc_save_yourself_callback;
	callbacks.die.callback                = sc_die_callback;
	callbacks.save_complete.callback      = sc_save_complete_callback;
	callbacks.shutdown_cancelled.callback = sc_shutdown_cancelled_callback;

	callbacks.save_yourself.client_data =
		callbacks.die.client_data =
		callbacks.save_complete.client_data =
		callbacks.shutdown_cancelled.client_data = (SmPointer) mainwin;
	if (g_getenv ("SESSION_MANAGER")) {
		gchar error_string_ret[256] = "";

		mainwin->smc_conn = (gpointer)
			SmcOpenConnection (NULL, mainwin,
				SmProtoMajor, SmProtoMinor,
				SmcSaveYourselfProcMask | SmcDieProcMask |
				SmcSaveCompleteProcMask |
				SmcShutdownCancelledProcMask,
				&callbacks,
				NULL, &client_id,
				256, error_string_ret);

		if (error_string_ret[0] || mainwin->smc_conn == NULL)
			g_warning ("While connecting to session manager:\n%s.",
				error_string_ret);
		else {
			SmPropValue *vals;
			vals = g_new (SmPropValue, 1);
			vals[0].length = strlen(argv0);
			vals[0].value = argv0;
			sc_client_set_value (mainwin, SmCloneCommand, SmLISTofARRAY8, 1, vals);
			sc_client_set_value (mainwin, SmRestartCommand, SmLISTofARRAY8, 1, vals);
			sc_client_set_value (mainwin, SmProgram, SmARRAY8, 1, vals);

			vals[0].length = strlen(g_get_user_name()?g_get_user_name():"");
			vals[0].value = g_strdup(g_get_user_name()?g_get_user_name():"");
			sc_client_set_value (mainwin, SmUserID, SmARRAY8, 1, vals);
		}
	}
}
#endif

static gboolean sc_exiting = FALSE;
static gboolean show_at_startup = TRUE;
static gboolean claws_crashed_bool = FALSE;

gboolean claws_crashed(void) {
	return claws_crashed_bool;
}

void main_set_show_at_startup(gboolean show)
{
	show_at_startup = show;
}

#ifdef MAEMO
static void main_vol_mount_cb(GnomeVFSVolumeMonitor *vfs, GnomeVFSVolume *vol, MainWindow *mainwin)
{
	gchar *uri = gnome_vfs_volume_get_activation_uri (vol);
	gchar *mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
	g_free (uri);
	if (mount_path) {
		if(!strcmp(mount_path, prefs_common.data_root)) {
			gtk_widget_set_sensitive(mainwin->window, TRUE);
			inc_unlock();
		}
	}
	g_free(mount_path);
}
static void main_vol_unmount_cb(GnomeVFSVolumeMonitor *vfs, GnomeVFSVolume *vol, MainWindow *mainwin)
{
	gchar *uri = gnome_vfs_volume_get_activation_uri (vol);
	gchar *mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
	g_free (uri);
	if (mount_path) {
		if(!strcmp(mount_path, prefs_common.data_root)) {
			gtk_widget_set_sensitive(mainwin->window, FALSE);
			inc_lock();
		}
	}
	g_free(mount_path);
}
#endif

#ifdef G_OS_WIN32
static FILE* win32_debug_fp=NULL;
static guint win32_log_handler_app_id;
static guint win32_log_handler_glib_id;
static guint win32_log_handler_gtk_id;

static void win32_print_stdout(const gchar *string)
{
	if (win32_debug_fp) {
		fprintf(win32_debug_fp, string);
		fflush(win32_debug_fp);
	}
}

static void win32_print_stderr(const gchar *string)
{
	if (win32_debug_fp) {
		fprintf(win32_debug_fp, string);
		fflush(win32_debug_fp);
	}
}

static void win32_log(const gchar *log_domain, GLogLevelFlags log_level, const gchar* message, gpointer user_data)
{
	if (win32_debug_fp) {
		const gchar* type;

		switch(log_level & G_LOG_LEVEL_MASK)
		{
			case G_LOG_LEVEL_ERROR:
				type="error";
				break;
			case G_LOG_LEVEL_CRITICAL:
				type="critical";
				break;
			case G_LOG_LEVEL_WARNING:
				type="warning";
				break;
			case G_LOG_LEVEL_MESSAGE:
				type="message";
				break;
			case G_LOG_LEVEL_INFO:
				type="info";
				break;
			case G_LOG_LEVEL_DEBUG:
				type="debug";
				break;
			default:
				type="N/A";
		}
		if (log_domain)
			fprintf(win32_debug_fp, "%s: %s: %s", log_domain, type, message);
		else
			fprintf(win32_debug_fp, "%s: %s", type, message);
		fflush(win32_debug_fp);
	}
}

static void win32_open_log(void)
{
	gchar *logfile = g_strconcat(g_get_tmp_dir(), G_DIR_SEPARATOR_S, "claws-win32.log", NULL);
	gchar *oldlogfile = g_strconcat(g_get_tmp_dir(), G_DIR_SEPARATOR_S, "claws-win32.log.bak", NULL);

	if (is_file_exist(logfile)) {
		if (rename_force(logfile, oldlogfile) < 0)
			FILE_OP_ERROR(logfile, "rename");
	}
	win32_debug_fp = g_fopen(logfile, "w");
	g_free(logfile);
	g_free(oldlogfile);
	if (win32_debug_fp)
	{
		g_set_print_handler(win32_print_stdout);
		g_set_printerr_handler(win32_print_stdout);
		win32_log_handler_app_id = g_log_set_handler(NULL, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                     | G_LOG_FLAG_RECURSION, win32_log, NULL);
		win32_log_handler_glib_id = g_log_set_handler("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                     | G_LOG_FLAG_RECURSION, win32_log, NULL);
		win32_log_handler_gtk_id = g_log_set_handler("Gtk", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                     | G_LOG_FLAG_RECURSION, win32_log, NULL);
	}
}

static void win32_close_log(void)
{
	if (win32_debug_fp)
	{
		g_log_remove_handler("", win32_log_handler_app_id);
		g_log_remove_handler("GLib", win32_log_handler_glib_id);
		g_log_remove_handler("Gtk", win32_log_handler_gtk_id);
		fclose(win32_debug_fp);
		win32_debug_fp=NULL;
	}
}		
#endif

static void main_dump_features_list(gboolean show_debug_only)
/* display compiled-in features list */
{
	if (show_debug_only && !debug_get_mode())
		return;

	if (show_debug_only)
		debug_print("runtime GTK+ %d.%d.%d / GLib %d.%d.%d\n",
			   gtk_major_version, gtk_minor_version, gtk_micro_version,
			   glib_major_version, glib_minor_version, glib_micro_version);
	else
		g_print("runtime GTK+ %d.%d.%d / GLib %d.%d.%d\n",
			   gtk_major_version, gtk_minor_version, gtk_micro_version,
			   glib_major_version, glib_minor_version, glib_micro_version);
	if (show_debug_only)
		debug_print("buildtime GTK+ %d.%d.%d / GLib %d.%d.%d\n",
			   GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION,
			   GLIB_MAJOR_VERSION, GLIB_MINOR_VERSION, GLIB_MICRO_VERSION);
	else
		g_print("buildtime GTK+ %d.%d.%d / GLib %d.%d.%d\n",
			   GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION,
			   GLIB_MAJOR_VERSION, GLIB_MINOR_VERSION, GLIB_MICRO_VERSION);
	
	if (show_debug_only)
		debug_print("Compiled-in features:\n");
	else
		g_print("Compiled-in features:\n");
#if HAVE_LIBCOMPFACE
	if (show_debug_only)
		debug_print(" compface\n");
	else
		g_print(" compface\n");
#endif
#if USE_ENCHANT
	if (show_debug_only)
		debug_print(" aspell\n");
	else
		g_print(" aspell\n");
#endif
#if USE_GNUTLS
	if (show_debug_only)
		debug_print(" gnutls\n");
	else
		g_print(" gnutls\n");
#endif
#if INET6
	if (show_debug_only)
		debug_print(" ipv6\n");
	else
		g_print(" ipv6\n");
#endif
#if HAVE_ICONV
	if (show_debug_only)
		debug_print(" iconv\n");
	else
		g_print(" iconv\n");
#endif
#if USE_JPILOT
	if (show_debug_only)
		debug_print(" jpilot\n");
	else
		g_print(" jpilot\n");
#endif
#if USE_LDAP
	if (show_debug_only)
		debug_print(" ldap\n");
	else
		g_print(" ldap\n");
#endif
#if HAVE_LIBETPAN
	if (show_debug_only)
		debug_print(" libetpan %d.%d\n", LIBETPAN_VERSION_MAJOR, LIBETPAN_VERSION_MINOR);
	else
		g_print(" libetpan %d.%d\n", LIBETPAN_VERSION_MAJOR, LIBETPAN_VERSION_MINOR);
#endif
#if HAVE_LIBSM
	if (show_debug_only)
		debug_print(" libsm\n");
	else
		g_print(" libsm\n");
#endif
#if HAVE_NETWORKMANAGER_SUPPORT
	if (show_debug_only)
		debug_print(" NetworkManager\n");
	else
		g_print(" NetworkManager\n");
#endif
}

#ifdef HAVE_DBUS_GLIB
static guint dbus_item_hook_id = -1;
static guint dbus_folder_hook_id = -1;

static void uninstall_dbus_status_handler(void)
{
	if(awn_proxy)
		g_object_unref(awn_proxy);
	awn_proxy = NULL;
	if (dbus_item_hook_id != -1)
		hooks_unregister_hook(FOLDER_ITEM_UPDATE_HOOKLIST, dbus_item_hook_id);
	if (dbus_folder_hook_id != -1)
		hooks_unregister_hook(FOLDER_UPDATE_HOOKLIST, dbus_folder_hook_id);
}

static void dbus_update(FolderItem *removed_item)
{
	guint new, unread, unreadmarked, marked, total;
	guint replied, forwarded, locked, ignored, watched;
	gchar *buf;
	GError *error = NULL;

	folder_count_total_msgs(&new, &unread, &unreadmarked, &marked, &total,
				&replied, &forwarded, &locked, &ignored,
				&watched);
	if (removed_item) {
		total -= removed_item->total_msgs;
		new -= removed_item->new_msgs;
		unread -= removed_item->unread_msgs;
	}

	if (new > 0) {
		buf = g_strdup_printf("%d", new);
		dbus_g_proxy_call(awn_proxy, "SetInfoByName", &error,
			G_TYPE_STRING, "claws-mail",
			G_TYPE_STRING, buf,
			G_TYPE_INVALID, G_TYPE_INVALID);
		g_free(buf);
		
	} else {
		dbus_g_proxy_call(awn_proxy, "UnsetInfoByName", &error, G_TYPE_STRING,
			"claws-mail", G_TYPE_INVALID, G_TYPE_INVALID);
	}
	if (error) {
		debug_print("%s\n", error->message);
		g_error_free(error);
	}
}

static gboolean dbus_status_update_folder_hook(gpointer source, gpointer data)
{
	FolderUpdateData *hookdata;
	hookdata = source;
	if (hookdata->update_flags & FOLDER_REMOVE_FOLDERITEM)
		dbus_update(hookdata->item);
	else
		dbus_update(NULL);

	return FALSE;
}

static gboolean dbus_status_update_item_hook(gpointer source, gpointer data)
{
	dbus_update(NULL);

	return FALSE;
}

static void install_dbus_status_handler(void)
{
	GError *tmp_error = NULL;
	DBusGConnection *connection = dbus_g_bus_get(DBUS_BUS_SESSION, &tmp_error);
	
	if(!connection) {
		/* If calling code doesn't do error checking, at least print some debug */
		debug_print("Failed to open connection to session bus: %s\n",
				 tmp_error->message);
		g_error_free(tmp_error);
		return;
	}
	awn_proxy = dbus_g_proxy_new_for_name(connection,
			"com.google.code.Awn",
			"/com/google/code/Awn",
			"com.google.code.Awn");
	dbus_item_hook_id = hooks_register_hook (FOLDER_ITEM_UPDATE_HOOKLIST, dbus_status_update_item_hook, NULL);
	if (dbus_item_hook_id == -1) {
		g_warning(_("Failed to register folder item update hook"));
		uninstall_dbus_status_handler();
		return;
	}

	dbus_folder_hook_id = hooks_register_hook (FOLDER_UPDATE_HOOKLIST, dbus_status_update_folder_hook, NULL);
	if (dbus_folder_hook_id == -1) {
		g_warning(_("Failed to register folder update hook"));
		uninstall_dbus_status_handler();
		return;
	}
}
#endif

int main(int argc, char *argv[])
{
#ifdef MAEMO
	osso_context_t *osso_context;
	osso_return_t result;
#endif
#ifdef HAVE_DBUS_GLIB
	DBusGConnection *connection;
	GError *error;
#endif
#ifdef HAVE_NETWORKMANAGER_SUPPORT
	DBusGProxy *nm_proxy;
#endif
	gchar *userrc;
	MainWindow *mainwin;
	FolderView *folderview;
	GdkPixbuf *icon;
	gboolean crash_file_present = FALSE;
	gint num_folder_class = 0;
	gboolean asked_for_migration = FALSE;
	gboolean start_done = TRUE;
	GtkUIManager *gui_manager = NULL;
	GSList *plug_list = NULL;
	gboolean never_ran = FALSE;
	START_TIMING("startup");

	sc_starting = TRUE;

#ifdef G_OS_WIN32
	win32_open_log();
#endif
	if (!claws_init(&argc, &argv)) {
#ifdef G_OS_WIN32
		win32_close_log();
#endif
		return 0;
	}

	main_dump_features_list(TRUE);

	prog_version = PROG_VERSION;
	argv0 = g_strdup(argv[0]);

	parse_cmd_opt(argc, argv);

	prefs_prepare_cache();

#ifdef CRASH_DIALOG
	if (cmd.crash) {
		gtk_set_locale();
		gtk_init(&argc, &argv);
		crash_main(cmd.crash_params);
#ifdef G_OS_WIN32
		win32_close_log();
#endif
		return 0;
	}
	crash_install_handlers();
#endif
	install_basic_sighandlers();
#if (defined linux && defined SIGIO)
	install_memory_sighandler();
#endif
	sock_init();

	/* check and create unix domain socket for remote operation */
	lock_socket = prohibit_duplicate_launch();
	if (lock_socket < 0) {
#ifdef HAVE_STARTUP_NOTIFICATION
		if(gtk_init_check(&argc, &argv))
			startup_notification_complete(TRUE);
#endif
		return 0;
	}

	if (cmd.status || cmd.status_full || cmd.search) {
		puts("0 Claws Mail not running.");
		lock_socket_remove();
		return 0;
	}
	
	if (cmd.exit)
		return 0;
	if (!g_thread_supported())
		g_thread_init(NULL);

	gtk_set_locale();
	gtk_init(&argc, &argv);

#ifdef G_OS_WIN32
	gtk_settings_set_string_property(gtk_settings_get_default(),
			"gtk-theme-name",
			"MS-Windows",
			"XProperty");
#endif

#ifdef HAVE_NETWORKMANAGER_SUPPORT
	went_offline_nm = FALSE;
	nm_proxy = NULL;
#endif
#ifdef HAVE_DBUS_GLIB
	error = NULL;
	connection = dbus_g_bus_get(DBUS_BUS_SYSTEM, &error);

	if(!connection) {
		debug_print("Failed to open connection to system bus: %s\n", error->message);
		g_error_free(error);
	}
	else {
#ifdef HAVE_NETWORKMANAGER_SUPPORT
		nm_proxy = dbus_g_proxy_new_for_name(connection,
			"org.freedesktop.NetworkManager",
			"/org/freedesktop/NetworkManager",
			"org.freedesktop.NetworkManager");
		if (nm_proxy) {
#if NM_CHECK_VERSION(0,8,992)
			dbus_g_proxy_add_signal(nm_proxy, "StateChanged", G_TYPE_UINT, G_TYPE_INVALID);
			dbus_g_proxy_connect_signal(nm_proxy, "StateChanged",
				G_CALLBACK(networkmanager_state_change_cb),
				NULL,NULL);
#else
			dbus_g_proxy_add_signal(nm_proxy, "StateChange", G_TYPE_UINT, G_TYPE_INVALID);
			dbus_g_proxy_connect_signal(nm_proxy, "StateChange",
				G_CALLBACK(networkmanager_state_change_cb),
				NULL,NULL);
#endif
		}
#endif
		install_dbus_status_handler();
	}
#endif


#ifdef MAEMO
	osso_context = osso_initialize(OSSO_SERVICE, "2.8.1", TRUE, NULL);
	if (osso_context == NULL) {
		return OSSO_ERROR;
	}
	hildon_program = HILDON_PROGRAM(hildon_program_get_instance());
	static_osso_context = osso_context;
#endif	
	gtk_widget_set_default_colormap(
		gdk_screen_get_system_colormap(
			gdk_screen_get_default()));

	gui_manager = gtkut_create_ui_manager();

	/* Create container for all the menus we will be adding */
	MENUITEM_ADDUI("/", "Menus", NULL, GTK_UI_MANAGER_MENUBAR);

	if (!g_thread_supported()) {
		g_error(_("g_thread is not supported by glib.\n"));
	}

	/* check that we're not on a too recent/old gtk+ */
#if GTK_CHECK_VERSION(2, 9, 0)
	if (gtk_check_version(2, 9, 0) != NULL) {
		alertpanel_error(_("Claws Mail has been compiled with "
				   "a more recent GTK+ library than is "
				   "currently available. This will cause "
				   "crashes. You need to upgrade GTK+ or "
				   "recompile Claws Mail."));
#ifdef G_OS_WIN32
		win32_close_log();
#endif
		exit(1);
	}
#else
	if (gtk_check_version(2, 9, 0) == NULL) {
		alertpanel_error(_("Claws Mail has been compiled with "
				   "an older GTK+ library than is "
				   "currently available. This will cause "
				   "crashes. You need to recompile "
				   "Claws Mail."));
#ifdef G_OS_WIN32
		win32_close_log();
#endif
		exit(1);
	}
#endif	

#ifdef G_OS_WIN32
	CHDIR_EXEC_CODE_RETURN_VAL_IF_FAIL(get_home_dir(), 1, win32_close_log(););
#else
	CHDIR_RETURN_VAL_IF_FAIL(get_home_dir(), 1);
#endif
	
	/* no config dir exists. See if we can migrate an old config. */
	if (!is_dir_exist(RC_DIR)) {
		prefs_destroy_cache();
		gboolean r = FALSE;
		
		/* if one of the old dirs exist, we'll ask if the user 
		 * want to migrates, and r will be TRUE if he said yes
		 * and migration succeeded, and FALSE otherwise.
		 */
		if (is_dir_exist(OLD_GTK2_RC_DIR)) {
			r = migrate_old_config(OLD_GTK2_RC_DIR, RC_DIR, _("Sylpheed-Claws 2.6.0 (or older)"));
			asked_for_migration = TRUE;
		} else if (is_dir_exist(OLDER_GTK2_RC_DIR)) {
			r = migrate_old_config(OLDER_GTK2_RC_DIR, RC_DIR, _("Sylpheed-Claws 1.9.15 (or older)"));
			asked_for_migration = TRUE;
		} else if (is_dir_exist(OLD_GTK1_RC_DIR)) {
			r = migrate_old_config(OLD_GTK1_RC_DIR, RC_DIR, _("Sylpheed-Claws 1.0.5 (or older)"));
			asked_for_migration = TRUE;
		} else if (is_dir_exist(SYLPHEED_RC_DIR)) {
			r = migrate_old_config(SYLPHEED_RC_DIR, RC_DIR, "Sylpheed");
			asked_for_migration = TRUE;
		}
		
		/* If migration failed or the user didn't want to do it,
		 * we create a new one (and we'll hit wizard later). 
		 */
		if (r == FALSE && !is_dir_exist(RC_DIR)) {
#ifdef G_OS_UNIX
			if (copy_dir(SYSCONFDIR "/skel/.claws-mail", RC_DIR) < 0) {
#endif
				if (!is_dir_exist(RC_DIR) && make_dir(RC_DIR) < 0) {
#ifdef G_OS_WIN32
					win32_close_log();
#endif
					exit(1);
				}
#ifdef G_OS_UNIX
			}
#endif
		}
	}
	

	if (!is_file_exist(RC_DIR G_DIR_SEPARATOR_S COMMON_RC) &&
	    is_file_exist(RC_DIR G_DIR_SEPARATOR_S OLD_COMMON_RC)) {
	    	/* post 2.6 name change */
		migrate_common_rc(RC_DIR G_DIR_SEPARATOR_S OLD_COMMON_RC,
			  RC_DIR G_DIR_SEPARATOR_S COMMON_RC);
	}

	if (!cmd.exit)
		plugin_load_all("Common");

	userrc = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, "gtkrc-2.0", NULL);
	gtk_rc_parse(userrc);
	g_free(userrc);

	userrc = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, MENU_RC, NULL);
	gtk_accel_map_load (userrc);
	g_free(userrc);

#ifdef G_OS_WIN32
	CHDIR_EXEC_CODE_RETURN_VAL_IF_FAIL(get_rc_dir(), 1, win32_close_log(););
#else
	CHDIR_RETURN_VAL_IF_FAIL(get_rc_dir(), 1);
#endif

	MAKE_DIR_IF_NOT_EXIST(get_mail_base_dir());
	MAKE_DIR_IF_NOT_EXIST(get_imap_cache_dir());
	MAKE_DIR_IF_NOT_EXIST(get_news_cache_dir());
	MAKE_DIR_IF_NOT_EXIST(get_mime_tmp_dir());
	MAKE_DIR_IF_NOT_EXIST(get_tmp_dir());
	MAKE_DIR_IF_NOT_EXIST(UIDL_DIR);

	crash_file_present = is_file_exist(get_crashfile_name());
	/* remove temporary files */
	remove_all_files(get_tmp_dir());
	remove_all_files(get_mime_tmp_dir());

	if (!cmd.crash && crash_file_present)
		claws_crashed_bool = TRUE;

	if (is_file_exist("claws.log")) {
		if (rename_force("claws.log", "claws.log.bak") < 0)
			FILE_OP_ERROR("claws.log", "rename");
	}
	set_log_file(LOG_PROTOCOL, "claws.log");

	if (is_file_exist("filtering.log")) {
		if (rename_force("filtering.log", "filtering.log.bak") < 0)
			FILE_OP_ERROR("filtering.log", "rename");
	}
	set_log_file(LOG_DEBUG_FILTERING, "filtering.log");

#ifdef G_OS_WIN32
	CHDIR_EXEC_CODE_RETURN_VAL_IF_FAIL(get_home_dir(), 1, win32_close_log(););
#else
	CHDIR_RETURN_VAL_IF_FAIL(get_home_dir(), 1);
#endif

	folder_system_init();
	prefs_common_read_config();

	prefs_themes_init();
	prefs_fonts_init();
	prefs_ext_prog_init();
	prefs_wrapping_init();
	prefs_compose_writing_init();
	prefs_msg_colors_init();
	image_viewer_init();
	prefs_image_viewer_init();
	prefs_quote_init();
	prefs_summaries_init();
	prefs_message_init();
	prefs_other_init();
	prefs_logging_init();
	prefs_receive_init();
	prefs_send_init();
	tags_read_tags();
#ifdef USE_ENCHANT
	gtkaspell_checkers_init();
	prefs_spelling_init();
#endif

	sock_set_io_timeout(prefs_common.io_timeout_secs);
	prefs_actions_read_config();
	prefs_display_header_read_config();
	/* prefs_filtering_read_config(); */
	addressbook_read_file();

	gtkut_widget_init();
	stock_pixbuf_gdk(NULL, STOCK_PIXMAP_CLAWS_MAIL_ICON, &icon);
	gtk_window_set_default_icon(icon);

	folderview_initialize();

	mh_gtk_init();
	imap_gtk_init();
	news_gtk_init();

	mainwin = main_window_create();

#ifdef HAVE_NETWORKMANAGER_SUPPORT
	networkmanager_state_change_cb(nm_proxy,NULL,mainwin);
#endif

#ifdef MAEMO
	AppData *appdata;
	appdata = g_new0(AppData, 1);
	appdata->program = hildon_program;
	appdata->window = HILDON_WINDOW(mainwin->window);
	appdata->osso_context = osso_context;
	result = osso_rpc_set_cb_f(appdata->osso_context, 
                OSSO_SERVICE, 
                OSSO_OBJECT, 
                OSSO_IFACE,
                dbus_req_handler, appdata);
	if (result != OSSO_OK) {
		return OSSO_ERROR;
	}

#ifndef CHINOOK
	/* Add handler for Exit D-BUS messages */
	result = osso_application_set_exit_cb(appdata->osso_context,
	                                        exit_event_handler,
	                                        (gpointer) appdata);
	if (result != OSSO_OK) {
		return OSSO_ERROR;
	}
#endif
	osso_hw_set_event_cb( appdata->osso_context,
				NULL, hw_event_handler, (gpointer) appdata );
#endif
	manage_window_focus_in(mainwin->window, NULL, NULL);
	folderview = mainwin->folderview;

	gtk_cmclist_freeze(GTK_CMCLIST(mainwin->folderview->ctree));
	folder_item_update_freeze();

	/* register the callback of unix domain socket input */
	lock_socket_tag = claws_input_add(lock_socket,
					GDK_INPUT_READ | GDK_INPUT_EXCEPTION,
					lock_socket_input_cb,
					mainwin, TRUE);


	prefs_account_init();
	account_read_config_all();

#ifdef HAVE_LIBETPAN
	imap_main_init(prefs_common.skip_ssl_cert_check);
	imap_main_set_timeout(prefs_common.io_timeout_secs);
	nntp_main_init(prefs_common.skip_ssl_cert_check);
#endif	
	/* If we can't read a folder list or don't have accounts,
	 * it means the configuration's not done. Either this is
	 * a brand new install, either a failed/refused migration.
	 * So we'll start the wizard.
	 */
	if (folder_read_list() < 0) {
		prefs_destroy_cache();
		
		/* if run_wizard returns FALSE it's because it's
		 * been cancelled. We can't do much but exit.
		 * however, if the user was asked for a migration,
		 * we remove the newly created directory so that
		 * he's asked again for migration on next launch.*/
		if (!run_wizard(mainwin, TRUE)) {
			if (asked_for_migration)
				remove_dir_recursive(RC_DIR);
#ifdef G_OS_WIN32
			win32_close_log();
#endif
			exit(1);
		}
		main_window_reflect_prefs_all_now();
		folder_write_list();
		never_ran = TRUE;
	}

	if (!account_get_list()) {
		prefs_destroy_cache();
		if (!run_wizard(mainwin, FALSE)) {
			if (asked_for_migration)
				remove_dir_recursive(RC_DIR);
#ifdef G_OS_WIN32
			win32_close_log();
#endif
			exit(1);
		}
		if(!account_get_list()) {
			exit_claws(mainwin);
#ifdef G_OS_WIN32
			win32_close_log();
#endif
			exit(1);
		}
		never_ran = TRUE;
	}

	
	toolbar_main_set_sensitive(mainwin);
	main_window_set_menu_sensitive(mainwin);

	/* if crashed, show window early so that the user
	 * sees what's happening */
	if (claws_crashed())
		main_window_popup(mainwin);

	account_set_missing_folder();
	folder_set_missing_folders();
	folderview_set(folderview);

	prefs_matcher_read_config();
	quicksearch_set_search_strings(mainwin->summaryview->quicksearch);

	/* make one all-folder processing before using claws */
	main_window_cursor_wait(mainwin);
	folder_func_to_all_folders(initial_processing, (gpointer *)mainwin);

	/* if claws crashed, rebuild caches */
	if (claws_crashed()) {
		GTK_EVENTS_FLUSH();
		debug_print("Claws Mail crashed, checking for new messages in local folders\n");
		folder_item_update_thaw();
		folderview_check_new(NULL);
		folder_clean_cache_memory_force();
		folder_item_update_freeze();
	}
	/* make the crash-indicator file */
	str_write_to_file("foo", get_crashfile_name());

	inc_autocheck_timer_init(mainwin);

	/* ignore SIGPIPE signal for preventing sudden death of program */
#ifdef G_OS_UNIX
	signal(SIGPIPE, SIG_IGN);
#endif
	if (cmd.online_mode == ONLINE_MODE_OFFLINE) {
		main_window_toggle_work_offline(mainwin, TRUE, FALSE);
	}
	if (cmd.online_mode == ONLINE_MODE_ONLINE) {
		main_window_toggle_work_offline(mainwin, FALSE, FALSE);
	}

	if (cmd.status_folders) {
		g_ptr_array_free(cmd.status_folders, TRUE);
		cmd.status_folders = NULL;
	}
	if (cmd.status_full_folders) {
		g_ptr_array_free(cmd.status_full_folders, TRUE);
		cmd.status_full_folders = NULL;
	}

	claws_register_idle_function(claws_gtk_idle);

	prefs_toolbar_init();

	num_folder_class = g_list_length(folder_get_list());

	plugin_load_all("GTK2");

	if (g_list_length(folder_get_list()) != num_folder_class) {
		debug_print("new folders loaded, reloading processing rules\n");
		prefs_matcher_read_config();
	}
	
	if ((plug_list = plugin_get_unloaded_list()) != NULL) {
		GSList *cur;
		gchar *list = NULL;
		gint num_plugins = 0;
		for (cur = plug_list; cur; cur = cur->next) {
			Plugin *plugin = (Plugin *)cur->data;
			gchar *tmp = g_strdup_printf("%s\n%s",
				list? list:"",
				plugin_get_name(plugin));
			g_free(list);
			list = tmp;
			num_plugins++;
		}
		main_window_cursor_normal(mainwin);
		main_window_popup(mainwin);
		alertpanel_warning(ngettext(
				     "The following plugin failed to load. "
				     "Check the Plugins configuration "
				     "for more information:\n%s",
				     "The following plugins failed to load. "
				     "Check the Plugins configuration "
				     "for more information:\n%s", 
				     num_plugins), 
				     list);
		main_window_cursor_wait(mainwin);
		g_free(list);
		g_slist_free(plug_list);
	}

	if (never_ran) {
		prefs_common_write_config();
	 	plugin_load_standard_plugins ();
	}
	/* if not crashed, show window now */
	if (!claws_crashed()) {
		/* apart if something told not to show */
		if (show_at_startup)
			main_window_popup(mainwin);
	}

	if (!folder_have_mailbox()) {
		prefs_destroy_cache();
		main_window_cursor_normal(mainwin);
		if (folder_get_list() != NULL) {
			alertpanel_error(_("Claws Mail has detected a configured "
				   "mailbox, but it is incomplete. It is "
				   "possibly due to a failing IMAP account. Use "
				   "\"Rebuild folder tree\" on the mailbox parent "
				   "folder's context menu to try to fix it."));
		} else {
			alertpanel_error(_("Claws Mail has detected a configured "
				   "mailbox, but could not load it. It is "
				   "probably provided by an out-of-date "
				   "external plugin. Please reinstall the "
				   "plugin and try again."));
			exit_claws(mainwin);
#ifdef G_OS_WIN32
			win32_close_log();
#endif
			exit(1);
		}
	}
	
	static_mainwindow = mainwin;

#ifdef MAEMO
	if (prefs_common.data_root != NULL && *prefs_common.data_root != '\0') {
		GnomeVFSVolume *vol = NULL;
		gchar *uri, *mount_path;

		volmon = gnome_vfs_get_volume_monitor();
		vol = gnome_vfs_volume_monitor_get_volume_for_path(volmon, prefs_common.data_root);

		uri = gnome_vfs_volume_get_activation_uri (vol);
		mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
		g_free(uri);

		if (vol == NULL || !gnome_vfs_volume_is_mounted(vol) 
		    || strcmp(mount_path, prefs_common.data_root)) {
			alertpanel_error(_("Claws Mail can not start without its data volume (%s)."), 
				prefs_common.data_root);
			g_free(mount_path);
			gnome_vfs_volume_unref(vol);
			exit_claws(mainwin);
			exit(1);
		}
		g_free(mount_path);
		gnome_vfs_volume_unref(vol);
		g_signal_connect(G_OBJECT(volmon), 
				"volume-mounted", G_CALLBACK(main_vol_mount_cb), mainwin);
		g_signal_connect(G_OBJECT(volmon), 
				"volume-unmounted", G_CALLBACK(main_vol_unmount_cb), mainwin);
	}
#endif

#ifdef HAVE_STARTUP_NOTIFICATION
	startup_notification_complete(FALSE);
#endif
#ifdef HAVE_LIBSM
	sc_session_manager_connect(mainwin);
#endif

	folder_item_update_thaw();
	gtk_cmclist_thaw(GTK_CMCLIST(mainwin->folderview->ctree));
	main_window_cursor_normal(mainwin);

	if (!cmd.target && prefs_common.goto_last_folder_on_startup &&
	    folder_find_item_from_identifier(prefs_common.last_opened_folder) != NULL &&
	    !claws_crashed()) {
		cmd.target = prefs_common.last_opened_folder;
	}

	if (cmd.receive_all && !cmd.target) {
		start_done = FALSE;
		g_timeout_add(1000, defer_check_all, GINT_TO_POINTER(FALSE));
	} else if (prefs_common.chk_on_startup && !cmd.target) {
		start_done = FALSE;
		g_timeout_add(1000, defer_check_all, GINT_TO_POINTER(TRUE));
	} else if (cmd.receive && !cmd.target) {
		start_done = FALSE;
		g_timeout_add(1000, defer_check, NULL);
	} 
	gtk_widget_grab_focus(folderview->ctree);

	if (cmd.compose) {
		open_compose_new(cmd.compose_mailto, cmd.attach_files);
	}
	if (cmd.attach_files) {
		ptr_array_free_strings(cmd.attach_files);
		g_ptr_array_free(cmd.attach_files, TRUE);
		cmd.attach_files = NULL;
	}
	if (cmd.subscribe) {
		folder_subscribe(cmd.subscribe_uri);
	}

	if (cmd.send) {
		send_queue();
	}
	
	if (cmd.target) {
		start_done = FALSE;
		g_timeout_add(500, defer_jump, (gpointer)cmd.target);
	}

	prefs_destroy_cache();
	
	compose_reopen_exit_drafts();

	if (start_done) {
		sc_starting = FALSE;
		main_window_set_menu_sensitive(mainwin);
		toolbar_main_set_sensitive(mainwin);
	}
	END_TIMING();

	gtk_main();

#ifdef MAEMO
	osso_deinitialize(osso_context);
#endif
#ifdef HAVE_NETWORKMANAGER_SUPPORT
	if(nm_proxy)
		g_object_unref(nm_proxy);
#endif
#ifdef HAVE_DBUS_GLIB
	uninstall_dbus_status_handler();
	if(connection)
		dbus_g_connection_unref(connection);
#endif
#ifdef G_OS_WIN32
	win32_close_log();
#endif
	utils_free_regex();
	exit_claws(mainwin);

	return 0;
}

static void save_all_caches(FolderItem *item, gpointer data)
{
	if (!item->cache) {
		return;
	}

	if (item->opened) {
		folder_item_close(item);
	}

	folder_item_free_cache(item, TRUE);
}

static void exit_claws(MainWindow *mainwin)
{
	gchar *filename;
	gboolean have_connectivity;

	sc_exiting = TRUE;

	debug_print("shutting down\n");
	inc_autocheck_timer_remove();

#ifdef HAVE_NETWORKMANAGER_SUPPORT
	if (prefs_common.work_offline && went_offline_nm)
		prefs_common.work_offline = FALSE;
#endif

	/* save prefs for opened folder */
	if(mainwin->folderview->opened) {
		FolderItem *item;

		item = gtk_cmctree_node_get_row_data(GTK_CMCTREE(mainwin->folderview->ctree), mainwin->folderview->opened);
		summary_save_prefs_to_folderitem(mainwin->folderview->summaryview, item);
		prefs_common.last_opened_folder = folder_item_get_identifier(item);
	}

	/* save all state before exiting */
	folder_func_to_all_folders(save_all_caches, NULL);
	folder_write_list();

	main_window_get_size(mainwin);
	main_window_get_position(mainwin);

	prefs_common_write_config();
	account_write_config_all();
	addressbook_export_to_file();

	filename = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, MENU_RC, NULL);
	gtk_accel_map_save(filename);
	g_free(filename);

	/* delete temporary files */
	remove_all_files(get_tmp_dir());
	remove_all_files(get_mime_tmp_dir());

	close_log_file(LOG_PROTOCOL);
	close_log_file(LOG_DEBUG_FILTERING);

#ifdef HAVE_NETWORKMANAGER_SUPPORT
	have_connectivity = networkmanager_is_online(NULL); 
#else
	have_connectivity = TRUE;
#endif
#ifdef HAVE_LIBETPAN
	imap_main_done(have_connectivity);
	nntp_main_done(have_connectivity);
#endif
	/* delete crashfile */
	if (!cmd.crash)
		claws_unlink(get_crashfile_name());

	lock_socket_remove();

#ifdef HAVE_LIBSM
	if (mainwin->smc_conn)
		SmcCloseConnection ((SmcConn)mainwin->smc_conn, 0, NULL);
	mainwin->smc_conn = NULL;
#endif

	main_window_destroy_all();
	
	plugin_unload_all("GTK2");

	prefs_toolbar_done();

	addressbook_destroy();

	prefs_themes_done();
	prefs_fonts_done();
	prefs_ext_prog_done();
	prefs_wrapping_done();
	prefs_compose_writing_done();
	prefs_msg_colors_done();
	prefs_image_viewer_done();
	image_viewer_done();
	prefs_quote_done();
	prefs_summaries_done();
	prefs_message_done();
	prefs_other_done();
	prefs_receive_done();
	prefs_logging_done();
	prefs_send_done();
	tags_write_tags();
#ifdef USE_ENCHANT       
	prefs_spelling_done();
	gtkaspell_checkers_quit();
#endif
	plugin_unload_all("Common");
	claws_done();
}

static void parse_cmd_opt(int argc, char *argv[])
{
	gint i;

	for (i = 1; i < argc; i++) {
		if (!strncmp(argv[i], "--receive-all", 13)) {
			cmd.receive_all = TRUE;
		} else if (!strncmp(argv[i], "--receive", 9)) {
			cmd.receive = TRUE;
		} else if (!strncmp(argv[i], "--compose", 9)) {
			const gchar *p = (i+1 < argc)?argv[i+1]:NULL;

			cmd.compose = TRUE;
			cmd.compose_mailto = NULL;
			if (p && *p != '\0' && *p != '-') {
				if (!strncmp(p, "mailto:", 7)) {
					cmd.compose_mailto = p + 7;
				} else {
					cmd.compose_mailto = p;
				}
				i++;
			}
		} else if (!strncmp(argv[i], "--subscribe", 11)) {
			const gchar *p = (i+1 < argc)?argv[i+1]:NULL;
			if (p && *p != '\0' && *p != '-') {
				cmd.subscribe = TRUE;
				cmd.subscribe_uri = p;
			}
		} else if (!strncmp(argv[i], "--attach", 8)) {
			const gchar *p = (i+1 < argc)?argv[i+1]:NULL;
			gchar *file = NULL;

			while (p && *p != '\0' && *p != '-') {
				if (!cmd.attach_files) {
					cmd.attach_files = g_ptr_array_new();
				}
				if ((file = g_filename_from_uri(p, NULL, NULL)) != NULL) {
					if (!is_file_exist(file)) {
						g_free(file);
						file = NULL;
					}
				}
				if (file == NULL && *p != G_DIR_SEPARATOR) {
					file = g_strconcat(claws_get_startup_dir(),
							   G_DIR_SEPARATOR_S,
							   p, NULL);
				} else if (file == NULL) {
					file = g_strdup(p);
				}
				g_ptr_array_add(cmd.attach_files, file);
				i++;
				p = (i+1 < argc)?argv[i+1]:NULL;
			}
		} else if (!strncmp(argv[i], "--send", 6)) {
			cmd.send = TRUE;
		} else if (!strncmp(argv[i], "--version-full", 14) ||
			   !strncmp(argv[i], "-V", 2)) {
			g_print("Claws Mail version " VERSION "\n");
			main_dump_features_list(FALSE);
			exit(0);
		} else if (!strncmp(argv[i], "--version", 9) ||
			   !strncmp(argv[i], "-v", 2)) {
			g_print("Claws Mail version " VERSION "\n");
			exit(0);
 		} else if (!strncmp(argv[i], "--status-full", 13)) {
 			const gchar *p = (i+1 < argc)?argv[i+1]:NULL;
 
 			cmd.status_full = TRUE;
 			while (p && *p != '\0' && *p != '-') {
 				if (!cmd.status_full_folders) {
 					cmd.status_full_folders =
 						g_ptr_array_new();
				}
 				g_ptr_array_add(cmd.status_full_folders,
 						g_strdup(p));
 				i++;
 				p = (i+1 < argc)?argv[i+1]:NULL;
 			}
  		} else if (!strncmp(argv[i], "--status", 8)) {
 			const gchar *p = (i+1 < argc)?argv[i+1]:NULL;
 
  			cmd.status = TRUE;
 			while (p && *p != '\0' && *p != '-') {
 				if (!cmd.status_folders)
 					cmd.status_folders = g_ptr_array_new();
 				g_ptr_array_add(cmd.status_folders,
 						g_strdup(p));
 				i++;
 				p = (i+1 < argc)?argv[i+1]:NULL;
 			}
		} else if (!strncmp(argv[i], "--search", 8)) {
			cmd.search_folder    = (i+1 < argc)?argv[i+1]:NULL;
			cmd.search_type      = (i+2 < argc)?argv[i+2]:NULL;
			cmd.search_request   = (i+3 < argc)?argv[i+3]:NULL;
			const char* rec      = (i+4 < argc)?argv[i+4]:NULL;
			cmd.search_recursive = TRUE;
			if (rec && (tolower(*rec)=='n' || tolower(*rec)=='f' || *rec=='0'))
				cmd.search_recursive = FALSE;
			if (cmd.search_folder && cmd.search_type && cmd.search_request)
				cmd.search = TRUE;
		} else if (!strncmp(argv[i], "--online", 8)) {
			cmd.online_mode = ONLINE_MODE_ONLINE;
		} else if (!strncmp(argv[i], "--offline", 9)) {
			cmd.online_mode = ONLINE_MODE_OFFLINE;
		} else if (!strncmp(argv[i], "--help", 6) ||
			   !strncmp(argv[i], "-h", 2)) {
			gchar *base = g_path_get_basename(argv[0]);
			g_print(_("Usage: %s [OPTION]...\n"), base);

			g_print("%s\n", _("  --compose [address]    open composition window"));
			g_print("%s\n", _("  --subscribe [uri]      subscribe to the given URI if possible"));
			g_print("%s\n", _("  --attach file1 [file2]...\n"
			          "                         open composition window with specified files\n"
			          "                         attached"));
			g_print("%s\n", _("  --receive              receive new messages"));
			g_print("%s\n", _("  --receive-all          receive new messages of all accounts"));
			g_print("%s\n", _("  --search folder type request [recursive]"));
			g_print("%s\n", _("                         searches mail"));
			g_print("%s\n", _("                         folder ex.: \"#mh/Mailbox/inbox\" or \"Mail\""));
			g_print("%s\n", _("                         type: s[ubject],f[rom],t[o],e[xtended],m[ixed] or g: tag"));
			g_print("%s\n", _("                         request: search string"));
			g_print("%s\n", _("                         recursive: false if arg. starts with 0, n, N, f or F"));

			g_print("%s\n", _("  --send                 send all queued messages"));
 			g_print("%s\n", _("  --status [folder]...   show the total number of messages"));
 			g_print("%s\n", _("  --status-full [folder]...\n"
 			                  "                         show the status of each folder"));
			g_print("%s\n", _("  --select folder[/msg]  jumps to the specified folder/message\n" 
			                  "                         folder is a folder id like 'folder/sub_folder'"));
			g_print("%s\n", _("  --online               switch to online mode"));
			g_print("%s\n", _("  --offline              switch to offline mode"));
			g_print("%s\n", _("  --exit --quit -q       exit Claws Mail"));
			g_print("%s\n", _("  --debug                debug mode"));
			g_print("%s\n", _("  --help -h              display this help and exit"));
			g_print("%s\n", _("  --version -v           output version information and exit"));
			g_print("%s\n", _("  --version-full -V      output version and built-in features information and exit"));
			g_print("%s\n", _("  --config-dir           output configuration directory"));
			g_print("%s\n", _("  --alternate-config-dir [dir]\n"
			                  "                         use specified configuration directory"));

			g_free(base);
			exit(1);
		} else if (!strncmp(argv[i], "--crash", 7)) {
			cmd.crash = TRUE;
			cmd.crash_params = g_strdup((i+1 < argc)?argv[i+1]:NULL);
			i++;
		} else if (!strncmp(argv[i], "--config-dir", sizeof "--config-dir" - 1)) {
			g_print(RC_DIR "\n");
			exit(0);
		} else if (!strncmp(argv[i], "--alternate-config-dir", sizeof "--alternate-config-dir" - 1) && i+1 < argc) {
			set_rc_dir(argv[i+1]);
		} else if (!strncmp(argv[i], "--exit", 6) ||
			   !strncmp(argv[i], "--quit", 6) ||
			   !strncmp(argv[i], "-q", 2)) {
			cmd.exit = TRUE;
		} else if (!strncmp(argv[i], "--select", 8) && i+1 < argc) {
			cmd.target = argv[i+1];
		} else if (i == 1 && argc == 2) {
			/* only one parameter. Do something intelligent about it */
			if ((strstr(argv[i], "@")||!strncmp(argv[i], "mailto:", 7)) && !strstr(argv[i], "://")) {
				const gchar *p = argv[i];

				cmd.compose = TRUE;
				cmd.compose_mailto = NULL;
				if (p && *p != '\0' && *p != '-') {
					if (!strncmp(p, "mailto:", 7)) {
						cmd.compose_mailto = p + 7;
					} else {
						cmd.compose_mailto = p;
					}
				}
			} else if (!strncmp(argv[i], "file://", 7)) {
				cmd.target = argv[i];
			} else if (!strncmp(argv[i], "?attach=file://", strlen("?attach=file://"))) {
				cmd.compose = TRUE;
				cmd.compose_mailto = argv[i];
			} else if (strstr(argv[i], "://")) {
				const gchar *p = argv[i];
				if (p && *p != '\0' && *p != '-') {
					cmd.subscribe = TRUE;
					cmd.subscribe_uri = p;
				}
			} else if (!strcmp(argv[i], "--sync")) {
				/* gtk debug */
			} else if (is_dir_exist(argv[i]) || is_file_exist(argv[i])) {
				cmd.target = argv[i];
			} else {
				g_print(_("Unknown option\n"));
				exit(1);
			}
		}
	}

	if (cmd.attach_files && cmd.compose == FALSE) {
		cmd.compose = TRUE;
		cmd.compose_mailto = NULL;
	}
}

static void initial_processing(FolderItem *item, gpointer data)
{
	MainWindow *mainwin = (MainWindow *)data;
	gchar *buf;

	cm_return_if_fail(item);
	buf = g_strdup_printf(_("Processing (%s)..."), 
			      item->path 
			      ? item->path 
			      : _("top level folder"));
	g_free(buf);

	
	if (item->prefs->enable_processing) {
		item->processing_pending = TRUE;
		folder_item_apply_processing(item);
		item->processing_pending = FALSE;
	}

	STATUSBAR_POP(mainwin);
}

static gboolean draft_all_messages(void)
{
	GList *compose_list = NULL;
	
	compose_clear_exit_drafts();
	compose_list = compose_get_compose_list();
	while (compose_list != NULL) {
		Compose *c = (Compose*)compose_list->data;
		if (!compose_draft(c, COMPOSE_DRAFT_FOR_EXIT))
			return FALSE;
		compose_list = compose_get_compose_list();
	}
	return TRUE;
}
gboolean clean_quit(gpointer data)
{
	static gboolean firstrun = TRUE;

	if (!firstrun) {
		return FALSE;
	}
	firstrun = FALSE;

	/*!< Good idea to have the main window stored in a 
	 *   static variable so we can check that variable
	 *   to see if we're really allowed to do things
	 *   that actually the spawner is supposed to 
	 *   do (like: sending mail, composing messages).
	 *   Because, really, if we're the spawnee, and
	 *   we touch GTK stuff, we're hosed. See the 
	 *   next fixme. */

	/* FIXME: Use something else to signal that we're
	 * in the original spawner, and not in a spawned
	 * child. */
	if (!static_mainwindow) {
		return FALSE;
	}
		
	draft_all_messages();
	emergency_exit = TRUE;
	exit_claws(static_mainwindow);
	exit(0);

	return FALSE;
}

void app_will_exit(GtkWidget *widget, gpointer data)
{
	MainWindow *mainwin = data;
	
	if (gtk_main_level() == 0) {
		debug_print("not even started\n");
		return;
	}
	if (sc_exiting == TRUE) {
		debug_print("exit pending\n");
		return;
	}
	sc_exiting = TRUE;
	debug_print("exiting\n");
	if (compose_get_compose_list()) {
		if (!draft_all_messages()) {
			main_window_popup(mainwin);
			sc_exiting = FALSE;
			return;
		}
	}

	if (prefs_common.warn_queued_on_exit && procmsg_have_queued_mails_fast()) {
		if (alertpanel(_("Queued messages"),
			       _("Some unsent messages are queued. Exit now?"),
			       GTK_STOCK_CANCEL, GTK_STOCK_OK, NULL)
		    != G_ALERTALTERNATE) {
			main_window_popup(mainwin);
		    	sc_exiting = FALSE;
			return;
		}
		manage_window_focus_in(mainwin->window, NULL, NULL);
	}

	sock_cleanup();
#ifdef HAVE_VALGRIND
	if (RUNNING_ON_VALGRIND) {
		summary_clear_list(mainwin->summaryview);
	}
#endif
	if (folderview_get_selected_item(mainwin->folderview))
		folder_item_close(folderview_get_selected_item(mainwin->folderview));
	gtk_main_quit();
}

gboolean claws_is_exiting(void)
{
	return sc_exiting;
}

gboolean claws_is_starting(void)
{
	return sc_starting;
}

/*
 * CLAWS: want this public so crash dialog can delete the
 * lock file too
 */
gchar *claws_get_socket_name(void)
{
	static gchar *filename = NULL;
	const gchar *socket_dir = NULL;
	
	if (rc_dir_is_alt())
		socket_dir = get_rc_dir();
	else
		socket_dir = g_get_tmp_dir();
	if (filename == NULL) {
		filename = g_strdup_printf("%s%cclaws-mail-%d",
					   socket_dir, G_DIR_SEPARATOR,
#if HAVE_GETUID
					   getuid());
#else
					   0);						
#endif
	}

	return filename;
}

static gchar *get_crashfile_name(void)
{
	static gchar *filename = NULL;

	if (filename == NULL) {
		filename = g_strdup_printf("%s%cclaws-crashed",
					   get_tmp_dir(), G_DIR_SEPARATOR);
	}

	return filename;
}

static gint prohibit_duplicate_launch(void)
{
	gint uxsock;
#ifdef G_OS_UNIX
	gchar *path;

	path = claws_get_socket_name();
	uxsock = fd_connect_unix(path);
	
	if (x_display == NULL)
		x_display = g_strdup(g_getenv("DISPLAY"));

	if (uxsock < 0) {
		claws_unlink(path);
		return fd_open_unix(path);
	}
#else
        HANDLE hmutex;

        hmutex = CreateMutexA(NULL, FALSE, "ClawsMail");
        if (!hmutex) {
                debug_print("cannot create Mutex\n");
                return -1;
        }
        if (GetLastError() != ERROR_ALREADY_EXISTS) {
                uxsock = fd_open_inet(50216);
                if (uxsock < 0)
                        return 0;
                return uxsock;
        }

        uxsock = fd_connect_inet(50216);
        if (uxsock < 0)
                return -1;
#endif
	/* remote command mode */

	debug_print("another Claws Mail instance is already running.\n");

	if (cmd.receive_all) {
		fd_write_all(uxsock, "receive_all\n", 12);
	} else if (cmd.receive) {
		fd_write_all(uxsock, "receive\n", 8);
	} else if (cmd.compose && cmd.attach_files) {
		gchar *str, *compose_str;
		gint i;

		if (cmd.compose_mailto) {
			compose_str = g_strdup_printf("compose_attach %s\n",
						      cmd.compose_mailto);
		} else {
			compose_str = g_strdup("compose_attach\n");
		}

		fd_write_all(uxsock, compose_str, strlen(compose_str));
		g_free(compose_str);

		for (i = 0; i < cmd.attach_files->len; i++) {
			str = g_ptr_array_index(cmd.attach_files, i);
			fd_write_all(uxsock, str, strlen(str));
			fd_write_all(uxsock, "\n", 1);
		}

		fd_write_all(uxsock, ".\n", 2);
	} else if (cmd.compose) {
		gchar *compose_str;

		if (cmd.compose_mailto) {
			compose_str = g_strdup_printf
				("compose %s\n", cmd.compose_mailto);
		} else {
			compose_str = g_strdup("compose\n");
		}

		fd_write_all(uxsock, compose_str, strlen(compose_str));
		g_free(compose_str);
	} else if (cmd.subscribe) {
		gchar *str = g_strdup_printf("subscribe %s\n", cmd.subscribe_uri);
		fd_write_all(uxsock, str, strlen(str));
		g_free(str);
	} else if (cmd.send) {
		fd_write_all(uxsock, "send\n", 5);
	} else if (cmd.online_mode == ONLINE_MODE_ONLINE) {
		fd_write(uxsock, "online\n", 6);
	} else if (cmd.online_mode == ONLINE_MODE_OFFLINE) {
		fd_write(uxsock, "offline\n", 7);
 	} else if (cmd.status || cmd.status_full) {
  		gchar buf[BUFFSIZE];
 		gint i;
 		const gchar *command;
 		GPtrArray *folders;
 		gchar *folder;
 
 		command = cmd.status_full ? "status-full\n" : "status\n";
 		folders = cmd.status_full ? cmd.status_full_folders :
 			cmd.status_folders;
 
 		fd_write_all(uxsock, command, strlen(command));
 		for (i = 0; folders && i < folders->len; ++i) {
 			folder = g_ptr_array_index(folders, i);
 			fd_write_all(uxsock, folder, strlen(folder));
 			fd_write_all(uxsock, "\n", 1);
 		}
 		fd_write_all(uxsock, ".\n", 2);
 		for (;;) {
 			fd_gets(uxsock, buf, sizeof(buf));
 			if (!strncmp(buf, ".\n", 2)) break;
 			fputs(buf, stdout);
 		}
	} else if (cmd.exit) {
		fd_write_all(uxsock, "exit\n", 5);
	} else if (cmd.target) {
		gchar *str = g_strdup_printf("select %s\n", cmd.target);
		fd_write_all(uxsock, str, strlen(str));
		g_free(str);
	} else if (cmd.search) {
		gchar buf[BUFFSIZE];
		gchar *str =
			g_strdup_printf("search %s\n%s\n%s\n%c\n",
							cmd.search_folder, cmd.search_type, cmd.search_request,
							(cmd.search_recursive==TRUE)?'1':'0');
		fd_write_all(uxsock, str, strlen(str));
		g_free(str);
		for (;;) {
			fd_gets(uxsock, buf, sizeof(buf));
			if (!strncmp(buf, ".\n", 2)) break;
			fputs(buf, stdout);
		}
	} else {
#ifndef G_OS_WIN32
		gchar buf[BUFSIZ];
		fd_write_all(uxsock, "get_display\n", 12);
		memset(buf, 0, sizeof(buf));
		fd_gets(uxsock, buf, sizeof(buf));
		if (strcmp2(buf, x_display)) {
			g_print("Claws Mail is already running on display %s.\n",
				buf);
		} else {
			fd_close(uxsock);
			uxsock = fd_connect_unix(path);
			fd_write_all(uxsock, "popup\n", 6);
		}
#else
		fd_write_all(uxsock, "popup\n", 6);
#endif
	}

	fd_close(uxsock);
	return -1;
}

static gint lock_socket_remove(void)
{
#ifdef G_OS_UNIX
	gchar *filename;
#endif
	if (lock_socket < 0) {
		return -1;
	}

	if (lock_socket_tag > 0) {
		g_source_remove(lock_socket_tag);
	}
	fd_close(lock_socket);

#ifdef G_OS_UNIX
	filename = claws_get_socket_name();
	claws_unlink(filename);
#endif

	return 0;
}

static GPtrArray *get_folder_item_list(gint sock)
{
	gchar buf[BUFFSIZE];
	FolderItem *item;
	GPtrArray *folders = NULL;

	for (;;) {
		fd_gets(sock, buf, sizeof(buf));
		if (!strncmp(buf, ".\n", 2)) {
			break;
		}
		strretchomp(buf);
		if (!folders) {
			folders = g_ptr_array_new();
		}
		item = folder_find_item_from_identifier(buf);
		if (item) {
			g_ptr_array_add(folders, item);
		} else {
			g_warning("no such folder: %s\n", buf);
		}
	}

	return folders;
}

static void lock_socket_input_cb(gpointer data,
				 gint source,
				 GdkInputCondition condition)
{
	MainWindow *mainwin = (MainWindow *)data;
	gint sock;
	gchar buf[BUFFSIZE];
	/* re-use the same quicksearch (& avoid matcher_list mem.leaks) */
	static QuickSearch *quicksearch = NULL;

	sock = fd_accept(source);
	fd_gets(sock, buf, sizeof(buf));

	if (!strncmp(buf, "popup", 5)) {
		main_window_popup(mainwin);
	} else if (!strncmp(buf, "get_display", 11)) {
		fd_write_all(sock, x_display, strlen(x_display));
	} else if (!strncmp(buf, "receive_all", 11)) {
		inc_all_account_mail(mainwin, FALSE,
				     prefs_common.newmail_notify_manu);
	} else if (!strncmp(buf, "receive", 7)) {
		inc_mail(mainwin, prefs_common.newmail_notify_manu);
	} else if (!strncmp(buf, "compose_attach", 14)) {
		GPtrArray *files;
		gchar *mailto;

		mailto = g_strdup(buf + strlen("compose_attach") + 1);
		files = g_ptr_array_new();
		while (fd_gets(sock, buf, sizeof(buf)) > 0) {
			strretchomp(buf);
			if (!strcmp2(buf, "."))
				break;
			g_ptr_array_add(files, g_strdup(buf));
		}
		open_compose_new(mailto, files);
		ptr_array_free_strings(files);
		g_ptr_array_free(files, TRUE);
		g_free(mailto);
	} else if (!strncmp(buf, "compose", 7)) {
		open_compose_new(buf + strlen("compose") + 1, NULL);
	} else if (!strncmp(buf, "subscribe", 9)) {
		main_window_popup(mainwin);
		folder_subscribe(buf + strlen("subscribe") + 1);
	} else if (!strncmp(buf, "send", 4)) {
		send_queue();
	} else if (!strncmp(buf, "online", 6)) {
		main_window_toggle_work_offline(mainwin, FALSE, FALSE);
	} else if (!strncmp(buf, "offline", 7)) {
		main_window_toggle_work_offline(mainwin, TRUE, FALSE);
 	} else if (!strncmp(buf, "status-full", 11) ||
 		   !strncmp(buf, "status", 6)) {
 		gchar *status;
 		GPtrArray *folders;
 
 		folders = get_folder_item_list(sock);
 		status = folder_get_status
 			(folders, !strncmp(buf, "status-full", 11));
 		fd_write_all(sock, status, strlen(status));
 		fd_write_all(sock, ".\n", 2);
 		g_free(status);
 		if (folders) g_ptr_array_free(folders, TRUE);
	} else if (!strncmp(buf, "select ", 7)) {
		const gchar *target = buf+7;
		mainwindow_jump_to(target, TRUE);
	} else if (!strncmp(buf, "search ", 7)) {
		FolderItem* folderItem = NULL;
		GSList *messages = NULL;
		gchar *folder_name, *request;
		QuickSearchType searchType = QUICK_SEARCH_EXTENDED;
		gboolean recursive;

		if (quicksearch==NULL)
			quicksearch = quicksearch_new_nogui();
		
		folder_name = g_strdup(buf+7);
		strretchomp(folder_name);

		if (fd_gets(sock, buf, sizeof(buf)) <= 0) {
			g_free(folder_name);
			folder_name=NULL;
		}
		searchType = quicksearch_type(buf);
		if (fd_gets(sock, buf, sizeof(buf)) <= 0) {
			g_free(folder_name);
			folder_name=NULL;
		}
		request = g_strdup(buf);
		strretchomp(request);

		recursive = TRUE;
		if (fd_gets(sock, buf, sizeof(buf)) > 0)
			if (buf[0]=='0')
				recursive = FALSE;

		debug_print("search: %s %i %s %i\n",folder_name,searchType,request,recursive);

		if (folder_name)
			folderItem = folder_find_item_from_identifier(folder_name);
		if (folder_name && folderItem == NULL) {
			debug_print("Unknow folder item : '%s', searching folder\n",folder_name);
			Folder* folder = folder_find_from_path(folder_name);
			if (folder != NULL)
				folderItem = FOLDER_ITEM(folder->node->data);
			else
				debug_print("Unknown folder: '%s'\n",folder_name);
		} else {
			debug_print("%s %s\n",folderItem->name, folderItem->path);
        }
		if (folderItem != NULL) {
			quicksearch_set(quicksearch, searchType, request);
			quicksearch_set_recursive(quicksearch, recursive);
			search_msgs_in_folders(&messages, quicksearch, folderItem);
		} else {
			g_print("Folder '%s' not found.\n'", folder_name);
		}

		GSList *cur;
		for (cur=messages; cur != NULL; cur = cur->next) {
			MsgInfo* msg = (MsgInfo *)cur->data;
			gchar *file = procmsg_get_message_file_path(msg);
			fd_write_all(sock, file, strlen(file));
			fd_write_all(sock, "\n", 1);
			g_free(file);
		}
		fd_write_all(sock, ".\n", 2);

		if (messages != NULL)
			procmsg_msg_list_free(messages);
		g_free(folder_name);
		g_free(request);
	} else if (!strncmp(buf, "exit", 4)) {
		if (prefs_common.clean_on_exit && !prefs_common.ask_on_clean) {
			procmsg_empty_all_trash();
                }
		app_will_exit(NULL, mainwin);
	}
	fd_close(sock);

}

static void open_compose_new(const gchar *address, GPtrArray *attach_files)
{
	gchar *addr = NULL;

	if (address) {
		Xstrdup_a(addr, address, return);
		g_strstrip(addr);
	}

	compose_new(NULL, addr, attach_files);
}

static void send_queue(void)
{
	GList *list;
	gchar *errstr = NULL;
	gboolean error = FALSE;
	for (list = folder_get_list(); list != NULL; list = list->next) {
		Folder *folder = list->data;

		if (folder->queue) {
			gint res = procmsg_send_queue
				(folder->queue, prefs_common.savemsg,
				&errstr);

			if (res) {
				folder_item_scan(folder->queue);
			}
			
			if (res < 0)
				error = TRUE;
		}
	}
	if (errstr) {
		alertpanel_error_log(_("Some errors occurred "
				"while sending queued messages:\n%s"), errstr);
		g_free(errstr);
	} else if (error) {
		alertpanel_error_log("Some errors occurred "
				"while sending queued messages.");
	}
}

static void quit_signal_handler(int sig)
{
	debug_print("Quitting on signal %d\n", sig);

	g_timeout_add(0, clean_quit, NULL);
}

static void install_basic_sighandlers()
{
#ifndef G_OS_WIN32
	sigset_t    mask;
	struct sigaction act;

	sigemptyset(&mask);

#ifdef SIGTERM
	sigaddset(&mask, SIGTERM);
#endif
#ifdef SIGINT
	sigaddset(&mask, SIGINT);
#endif
#ifdef SIGHUP
	sigaddset(&mask, SIGHUP);
#endif

	act.sa_handler = quit_signal_handler;
	act.sa_mask    = mask;
	act.sa_flags   = 0;

#ifdef SIGTERM
	sigaction(SIGTERM, &act, 0);
#endif
#ifdef SIGINT
	sigaction(SIGINT, &act, 0);
#endif	
#ifdef SIGHUP
	sigaction(SIGHUP, &act, 0);
#endif	

	sigprocmask(SIG_UNBLOCK, &mask, 0);
#endif /* !G_OS_WIN32 */
}

#if (defined linux && defined SIGIO)
static int mem_notify_fd = 0;

static gboolean clean_caches(gpointer unused)
{
	if (static_mainwindow && static_mainwindow->lock_count > 0)
		return TRUE;
	debug_print("/dev/mem_notify: callback: Freeing some memory now!\n");
	folder_clean_cache_memory_force();
	return FALSE;
}

static void memory_signal_handler(int sig)
{
	debug_print("/dev/mem_notify: Kernel says we should free up some memory!\n");
	g_timeout_add(10, clean_caches, NULL); 
}

static void install_memory_sighandler()
{
	sigset_t    mask;
	struct sigaction act;
	int flags;

	mem_notify_fd = g_open("/dev/mem_notify", O_RDONLY|O_NONBLOCK, 0);
	if (mem_notify_fd == -1) {
		debug_print("/dev/mem_notify not available (%s)\n", 
			strerror(errno));
		return;
	}
	
	fcntl(mem_notify_fd, F_SETOWN, getpid());
	flags = fcntl(mem_notify_fd, F_GETFL);
	fcntl(mem_notify_fd, flags|FASYNC);

	sigemptyset(&mask);

	sigaddset(&mask, SIGIO);

	act.sa_handler = memory_signal_handler;
	act.sa_mask    = mask;
	act.sa_flags   = 0;

	sigaction(SIGIO, &act, 0);

	sigprocmask(SIG_UNBLOCK, &mask, 0);

	debug_print("/dev/mem_notify: installed handler\n");
}
#endif /* linux && SIGIO */

#ifdef MAEMO
osso_context_t *get_osso_context(void)
{
	return static_osso_context;
}
#endif


#ifdef HAVE_NETWORKMANAGER_SUPPORT
static void networkmanager_state_change_cb(DBusGProxy *proxy, gchar *dev,
					 gpointer data)
{
	MainWindow *mainWin;

	mainWin = NULL;
	if (static_mainwindow)
		mainWin = static_mainwindow;
	else if (data)
		mainWin = (MainWindow*)data;
	
	if (!prefs_common.use_networkmanager)
		return;

	if (mainWin) {
		GError *error;
		gboolean online;

		error = NULL;		
		online = networkmanager_is_online(&error);
		if(!error) {
			if(online && went_offline_nm) {
				went_offline_nm = FALSE;
				main_window_toggle_work_offline(mainWin, FALSE, FALSE);
				debug_print("NetworkManager: Went online\n");
				log_message(LOG_PROTOCOL, _("NetworkManager: network is online.\n"));
			}
			else if(!online) {
				went_offline_nm = TRUE;
				main_window_toggle_work_offline(mainWin, TRUE, FALSE);
				debug_print("NetworkManager: Went offline\n");
				log_message(LOG_PROTOCOL, _("NetworkManager: network is offline.\n"));
			}
		}
		else {
			debug_print("Failed to get online information from NetworkManager: %s\n",
							 error->message);
			g_error_free(error);
		}
	}
	else
		debug_print("NetworkManager: Cannot change connection state because "
						 "main window does not exist\n");
}

/* Returns true (and sets error appropriately, if given) in case of error */
gboolean networkmanager_is_online(GError **error)
{
	DBusGConnection *connection;
	DBusGProxy *proxy;
	GError *tmp_error = NULL;
	gboolean retVal;
	guint32 state;

	if (!prefs_common.use_networkmanager)
		return TRUE;

	tmp_error = NULL;
	proxy = NULL;
	connection = dbus_g_bus_get(DBUS_BUS_SYSTEM, &tmp_error);

	if(!connection) {
		/* If calling code doesn't do error checking, at least print some debug */
		if((error == NULL) || (*error == NULL))
			debug_print("Failed to open connection to system bus: %s\n",
							 tmp_error->message);
		g_propagate_error(error, tmp_error);
		return TRUE;
	}

	proxy = dbus_g_proxy_new_for_name(connection,
			"org.freedesktop.NetworkManager",
			"/org/freedesktop/NetworkManager",
			"org.freedesktop.NetworkManager");

	retVal = dbus_g_proxy_call(proxy,"state",&tmp_error, G_TYPE_INVALID,
			G_TYPE_UINT, &state, G_TYPE_INVALID);

	if(proxy)
		g_object_unref(proxy);
	if(connection)
		dbus_g_connection_unref(connection);

	if(!retVal) {
		/* If calling code doesn't do error checking, at least print some debug */
		if((error == NULL) || (*error == NULL))
			debug_print("Failed to get state info from NetworkManager: %s\n",
							 tmp_error->message);
		g_propagate_error(error, tmp_error);
		return TRUE;
	}
#if NM_CHECK_VERSION(0,8,992)
    	return (state == NM_STATE_CONNECTED_LOCAL ||
		state == NM_STATE_CONNECTED_SITE ||
		state == NM_STATE_CONNECTED_GLOBAL ||
		state == NM_STATE_UNKNOWN);
#else
    	return (state == NM_STATE_CONNECTED ||
		state == NM_STATE_UNKNOWN);
#endif
}
#endif
