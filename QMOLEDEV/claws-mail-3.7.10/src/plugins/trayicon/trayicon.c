/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2011 the Claws Mail Team
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

#include <stdio.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "common/claws.h"
#include "common/version.h"
#include "plugin.h"
#include "utils.h"
#include "hooks.h"
#include "folder.h"
#include "mainwindow.h"
#include "gtkutils.h"
#include "menu.h"
#include "toolbar.h"
#include "prefs_common.h"
#include "main.h"
#include "alertpanel.h"
#include "account.h"
#include "gtk/manage_window.h"

#include "trayicon_prefs.h"

#include "stock_pixmap.h"

#define PLUGIN_NAME (_("Trayicon"))

static guint item_hook_id;
static guint folder_hook_id;
static guint offline_hook_id;
static guint account_hook_id;
static guint close_hook_id;
static guint iconified_hook_id;
static guint theme_hook_id;

static GdkPixbuf *newmail_pixbuf[2] = {NULL, NULL};
static GdkPixbuf *unreadmail_pixbuf[2] = {NULL, NULL};
static GdkPixbuf *newmarkedmail_pixbuf[2] = {NULL, NULL};
static GdkPixbuf *unreadmarkedmail_pixbuf[2] = {NULL, NULL};
static GdkPixbuf *nomail_pixbuf[2] = {NULL, NULL};

static GtkStatusIcon *trayicon;
static GtkWidget *focused_widget = NULL;

static GtkWidget *traymenu_popup;
static gboolean updating_menu = FALSE;

typedef enum
{
	TRAYICON_NEW,
	TRAYICON_NEWMARKED,
	TRAYICON_UNREAD,
	TRAYICON_UNREADMARKED,
	TRAYICON_NOTHING
} TrayIconType;

static void trayicon_get_all_cb	    (GtkAction *action, gpointer data);
static void trayicon_compose_cb	    (GtkAction *action, gpointer data);
static void trayicon_compose_acc_cb (GtkMenuItem *menuitem, gpointer data );
static void trayicon_addressbook_cb (GtkAction *action, gpointer data);
static void trayicon_exit_cb	    (GtkAction *action, gpointer data);
static void trayicon_toggle_offline_cb	(GtkAction *action, gpointer data);

static GtkActionEntry trayicon_popup_menu_entries[] =
{
	{"TrayiconPopup",			NULL, "TrayiconPopup" },
	{"TrayiconPopup/GetMail",		NULL, N_("_Get Mail"), NULL, NULL, G_CALLBACK(trayicon_get_all_cb) },
	{"TrayiconPopup/---",			NULL, "---", NULL, NULL, G_CALLBACK(trayicon_compose_cb) },
	{"TrayiconPopup/Email",			NULL, N_("_Email"), NULL, NULL, G_CALLBACK(trayicon_compose_cb) },
	{"TrayiconPopup/EmailAcc",		NULL, N_("E_mail from account"), NULL, NULL, NULL },
	{"TrayiconPopup/OpenAB",		NULL, N_("Open A_ddressbook"), NULL, NULL, G_CALLBACK(trayicon_addressbook_cb) },
	{"TrayiconPopup/Exit",			NULL, N_("E_xit Claws Mail"), NULL, NULL, G_CALLBACK(trayicon_exit_cb) },
};

static GtkToggleActionEntry trayicon_popup_toggle_menu_entries[] =
{
	{"TrayiconPopup/ToggleOffline",		NULL, N_("_Work Offline"), NULL, NULL, G_CALLBACK(trayicon_toggle_offline_cb) },
};

static gboolean trayicon_set_accounts_hook(gpointer source, gpointer data)
{
	GList *cur_ac;
	GtkWidget *menu, *submenu;
	GtkWidget *menuitem;
	PrefsAccount *ac_prefs;

	GList *account_list = account_get_list();

	menu = gtk_ui_manager_get_widget(gtkut_ui_manager(), "/Menus/TrayiconPopup/EmailAcc");
	gtk_widget_show(menu);

	gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu), NULL);
	submenu = gtk_menu_new();
	for (cur_ac = account_list; cur_ac != NULL; cur_ac = cur_ac->next) {
		ac_prefs = (PrefsAccount *)cur_ac->data;

		menuitem = gtk_menu_item_new_with_label
			(ac_prefs->account_name ? ac_prefs->account_name
			 : _("Untitled"));
		gtk_widget_show(menuitem);
		gtk_menu_shell_append(GTK_MENU_SHELL(submenu), menuitem);
		g_signal_connect(G_OBJECT(menuitem), "activate",
				 G_CALLBACK(trayicon_compose_acc_cb),
				 ac_prefs);
	}
	gtk_widget_show(submenu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu), submenu);
	return FALSE;
}

static void set_trayicon_pixbuf(TrayIconType icontype)
{
	GdkPixbuf *pixbuf = NULL;
	static GdkPixbuf *last_pixbuf = NULL;

	switch(icontype) {
	case TRAYICON_NEW:
		pixbuf = newmail_pixbuf[prefs_common.work_offline];
		break;
	case TRAYICON_NEWMARKED:
		pixbuf = newmarkedmail_pixbuf[prefs_common.work_offline];
		break;
	case TRAYICON_UNREAD:
		pixbuf = unreadmail_pixbuf[prefs_common.work_offline];
		break;
	case TRAYICON_UNREADMARKED:
		pixbuf = unreadmarkedmail_pixbuf[prefs_common.work_offline];
		break;
	default:
		pixbuf = nomail_pixbuf[prefs_common.work_offline];
		break;
	}

	if (pixbuf == last_pixbuf) {
		return;
	}

	gtk_status_icon_set_from_pixbuf(GTK_STATUS_ICON(trayicon), pixbuf);

	last_pixbuf = pixbuf;
}

static void update(FolderItem *removed_item)
{
	guint new, unread, unreadmarked, marked, total;
	guint replied, forwarded, locked, ignored, watched;
	gchar *buf;
	TrayIconType icontype = TRAYICON_NOTHING;

	folder_count_total_msgs(&new, &unread, &unreadmarked, &marked, &total,
				&replied, &forwarded, &locked, &ignored,
				&watched);
	if (removed_item) {
		total -= removed_item->total_msgs;
		new -= removed_item->new_msgs;
		unread -= removed_item->unread_msgs;
	}

	buf = g_strdup_printf(_("New %d, Unread: %d, Total: %d"), new, unread, total);

#if !(GTK_CHECK_VERSION(2,16,0))
	gtk_status_icon_set_tooltip(trayicon, buf);
#else
	gtk_status_icon_set_tooltip_text(trayicon, buf);
#endif
	g_free(buf);

	if (new > 0 && unreadmarked > 0) {
		icontype = TRAYICON_NEWMARKED;
	} else if (new > 0) {
		icontype = TRAYICON_NEW;
	} else if (unreadmarked > 0) {
		icontype = TRAYICON_UNREADMARKED;
	} else if (unread > 0) {
		icontype = TRAYICON_UNREAD;
	}

	set_trayicon_pixbuf(icontype);
}

static gboolean folder_item_update_hook(gpointer source, gpointer data)
{
	update(NULL);

	return FALSE;
}

static gboolean folder_update_hook(gpointer source, gpointer data)
{
	FolderUpdateData *hookdata;
	hookdata = source;
	if (hookdata->update_flags & FOLDER_REMOVE_FOLDERITEM)
		update(hookdata->item);
	else
		update(NULL);

	return FALSE;
}

static gboolean offline_update_hook(gpointer source, gpointer data)
{
	update(NULL);
	return FALSE;
}

static gboolean trayicon_close_hook(gpointer source, gpointer data)
{
	if (source) {
		gboolean *close_allowed = (gboolean*)source;

		if (trayicon_prefs.close_to_tray) {
			MainWindow *mainwin = mainwindow_get_mainwindow();

			*close_allowed = FALSE;
			focused_widget = gtk_window_get_focus(GTK_WINDOW(mainwin->window));
			
			if (gtkut_widget_get_visible(GTK_WIDGET(mainwin->window)))
				main_window_hide(mainwin);
		}
	}
	return FALSE;
}

static gboolean trayicon_got_iconified_hook(gpointer source, gpointer data)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();

	if (trayicon_prefs.hide_when_iconified
			&& gtkut_widget_get_visible(GTK_WIDGET(mainwin->window))
			&& !gtk_window_get_skip_taskbar_hint(GTK_WINDOW(mainwin->window))) {
		focused_widget = gtk_window_get_focus(GTK_WINDOW(mainwin->window));
		gtk_window_set_skip_taskbar_hint(GTK_WINDOW(mainwin->window), TRUE);
	}
	return FALSE;
}

static void fix_folderview_scroll(MainWindow *mainwin)
{
	static gboolean fix_done = FALSE;

	if (fix_done)
		return;

	gtk_widget_queue_resize(mainwin->folderview->ctree);

	fix_done = TRUE;
}

static gboolean click_cb(GtkWidget * widget,
		         GdkEventButton * event, gpointer user_data)
{
	MainWindow *mainwin;

	if (event == NULL) {
		return TRUE;
	}

	mainwin = mainwindow_get_mainwindow();

	switch (event->button) {
	case 1:
		if (gtkut_widget_get_visible(GTK_WIDGET(mainwin->window))) {
			if ((gdk_window_get_state(GTK_WIDGET(mainwin->window)->window)&GDK_WINDOW_STATE_ICONIFIED)
					|| mainwindow_is_obscured()) {
				gtk_window_deiconify(GTK_WINDOW(mainwin->window));
				gtk_window_set_skip_taskbar_hint(GTK_WINDOW(mainwin->window), FALSE);
				main_window_show(mainwin);
				gtk_window_present(GTK_WINDOW(mainwin->window));
				gtk_window_set_focus(GTK_WINDOW(mainwin->window), focused_widget);
			} else {
				focused_widget = gtk_window_get_focus(GTK_WINDOW(mainwin->window));
				main_window_hide(mainwin);
			}
		} else {
			gtk_window_deiconify(GTK_WINDOW(mainwin->window));
			gtk_window_set_skip_taskbar_hint(GTK_WINDOW(mainwin->window), FALSE);
			main_window_show(mainwin);
			gtk_window_present(GTK_WINDOW(mainwin->window));
			fix_folderview_scroll(mainwin);
			gtk_window_set_focus(GTK_WINDOW(mainwin->window), focused_widget);
        }
		break;
	case 3:
		/* tell callbacks to skip any event */
		updating_menu = TRUE;
		/* initialize checkitem according to current offline state */
		cm_toggle_menu_set_active("TrayiconPopup/ToggleOffline", prefs_common.work_offline);
		cm_menu_set_sensitive("TrayiconPopup/GetMail", mainwin->lock_count == 0);
		updating_menu = FALSE;

		gtk_menu_popup( GTK_MENU(traymenu_popup), NULL, NULL, NULL, NULL,
		       event->button, event->time );
		break;
	default:
		return TRUE;
	}
	return TRUE;
}

static void create_trayicon(void);

static gboolean trayicon_update_theme(gpointer source, gpointer data)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_NOMAIL, &nomail_pixbuf[0]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_UNREADMAIL, &unreadmail_pixbuf[0]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_NEWMAIL, &newmail_pixbuf[0]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_UNREADMARKEDMAIL, &unreadmarkedmail_pixbuf[0]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_NEWMARKEDMAIL, &newmarkedmail_pixbuf[0]);

	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_NOMAIL_OFFLINE, &nomail_pixbuf[1]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_UNREADMAIL_OFFLINE, &unreadmail_pixbuf[1]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_NEWMAIL_OFFLINE, &newmail_pixbuf[1]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_UNREADMARKEDMAIL_OFFLINE, &unreadmarkedmail_pixbuf[1]);
	stock_pixbuf_gdk(GTK_WIDGET(mainwin->window), STOCK_PIXMAP_TRAY_NEWMARKEDMAIL_OFFLINE, &newmarkedmail_pixbuf[1]);

	update(NULL);

	return FALSE;
}

static void create_trayicon()
{
	GtkActionGroup *action_group;
	trayicon = gtk_status_icon_new();
#if GTK_CHECK_VERSION(2,18,0)
	gtk_status_icon_set_title(GTK_STATUS_ICON(trayicon), _("Claws Mail"));
#endif
	g_signal_connect(G_OBJECT(trayicon), "button-press-event",
		G_CALLBACK(click_cb), NULL);

	action_group = cm_menu_create_action_group("TrayiconPopup", trayicon_popup_menu_entries,
			G_N_ELEMENTS(trayicon_popup_menu_entries), (gpointer)NULL);
	gtk_action_group_add_toggle_actions(action_group, trayicon_popup_toggle_menu_entries,
			G_N_ELEMENTS(trayicon_popup_toggle_menu_entries), (gpointer)NULL);

	MENUITEM_ADDUI("/Menus", "TrayiconPopup", "TrayiconPopup", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "GetMail", "TrayiconPopup/GetMail", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "Separator1", "TrayiconPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "Email", "TrayiconPopup/Email", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "EmailAcc", "TrayiconPopup/EmailAcc", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "Separator2", "TrayiconPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "OpenAB", "TrayiconPopup/OpenAB", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "Separator3", "TrayiconPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "ToggleOffline", "TrayiconPopup/ToggleOffline", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "Separator4", "TrayiconPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI("/Menus/TrayiconPopup", "Exit", "TrayiconPopup/Exit", GTK_UI_MANAGER_MENUITEM)

	traymenu_popup = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
				gtk_ui_manager_get_widget(gtkut_ui_manager(), "/Menus/TrayiconPopup")) );

	trayicon_update_theme(NULL, NULL);
}

int plugin_init(gchar **error)
{
	if (!check_plugin_version(MAKE_NUMERIC_VERSION(2,9,2,72),
				VERSION_NUMERIC, PLUGIN_NAME, error))
		return -1;

	item_hook_id = hooks_register_hook (FOLDER_ITEM_UPDATE_HOOKLIST, folder_item_update_hook, NULL);
	if (item_hook_id == -1) {
		*error = g_strdup(_("Failed to register folder item update hook"));
		goto err_out_item;
	}

	folder_hook_id = hooks_register_hook (FOLDER_UPDATE_HOOKLIST, folder_update_hook, NULL);
	if (folder_hook_id == -1) {
		*error = g_strdup(_("Failed to register folder update hook"));
		goto err_out_folder;
	}

	offline_hook_id = hooks_register_hook (OFFLINE_SWITCH_HOOKLIST, offline_update_hook, NULL);
	if (offline_hook_id == -1) {
		*error = g_strdup(_("Failed to register offline switch hook"));
		goto err_out_offline;
	}

	account_hook_id = hooks_register_hook (ACCOUNT_LIST_CHANGED_HOOKLIST, trayicon_set_accounts_hook, NULL);
	if (account_hook_id == -1) {
		*error = g_strdup(_("Failed to register account list changed hook"));
		goto err_out_account;
	}

	close_hook_id = hooks_register_hook (MAIN_WINDOW_CLOSE, trayicon_close_hook, NULL);
	if (close_hook_id == -1) {
		*error = g_strdup(_("Failed to register close hook"));
		goto err_out_close;
	}

	iconified_hook_id = hooks_register_hook (MAIN_WINDOW_GOT_ICONIFIED, trayicon_got_iconified_hook, NULL);
	if (iconified_hook_id == -1) {
		*error = g_strdup(_("Failed to register got iconified hook"));
		goto err_out_iconified;
	}

	theme_hook_id = hooks_register_hook(THEME_CHANGED_HOOKLIST, trayicon_update_theme, NULL);
	if (theme_hook_id == -1) {
		*error = g_strdup(_("Failed to register theme change hook"));
		goto err_out_theme;
	}

	create_trayicon();
	trayicon_set_accounts_hook(NULL, NULL);

	trayicon_prefs_init();

	if (trayicon_prefs.hide_at_startup && claws_is_starting()) {
		MainWindow *mainwin = mainwindow_get_mainwindow();

		if (gtkut_widget_get_visible(GTK_WIDGET(mainwin->window)))
			main_window_hide(mainwin);
		main_set_show_at_startup(FALSE);
	}

	return 0;

err_out_theme:
	hooks_unregister_hook(MAIN_WINDOW_GOT_ICONIFIED, iconified_hook_id);
err_out_iconified:
	hooks_unregister_hook(MAIN_WINDOW_CLOSE, close_hook_id);
err_out_close:
	hooks_unregister_hook(ACCOUNT_LIST_CHANGED_HOOKLIST, account_hook_id);
err_out_account:
	hooks_unregister_hook(OFFLINE_SWITCH_HOOKLIST, offline_hook_id);
err_out_offline:
	hooks_unregister_hook(FOLDER_UPDATE_HOOKLIST, folder_hook_id);
err_out_folder:
	hooks_unregister_hook(FOLDER_ITEM_UPDATE_HOOKLIST, item_hook_id);
err_out_item:
	return -1;
}

gboolean plugin_done(void)
{
	trayicon_prefs_done();

	hooks_unregister_hook(FOLDER_ITEM_UPDATE_HOOKLIST, item_hook_id);
	hooks_unregister_hook(FOLDER_UPDATE_HOOKLIST, folder_hook_id);
	hooks_unregister_hook(OFFLINE_SWITCH_HOOKLIST, offline_hook_id);
	hooks_unregister_hook(ACCOUNT_LIST_CHANGED_HOOKLIST, account_hook_id);
	hooks_unregister_hook(MAIN_WINDOW_CLOSE, close_hook_id);
	hooks_unregister_hook(MAIN_WINDOW_GOT_ICONIFIED, iconified_hook_id);
	hooks_unregister_hook(THEME_CHANGED_HOOKLIST, theme_hook_id);

	if (claws_is_exiting())
		return TRUE;

	gtk_status_icon_set_visible(trayicon, FALSE);
	g_object_unref(G_OBJECT(trayicon));
	trayicon = NULL;

	while (gtk_events_pending()) {
		gtk_main_iteration();
	}
	return TRUE;
}

const gchar *plugin_name(void)
{
	return PLUGIN_NAME;
}

const gchar *plugin_desc(void)
{
	return _("This plugin places a mailbox icon in the system tray that "
	         "indicates if you have new or unread mail.\n"
	         "\n"
	         "The mailbox is empty if you have no unread mail, otherwise "
	         "it contains a letter. A tooltip shows new, unread and total "
	         "number of messages.");
}

const gchar *plugin_type(void)
{
	return "GTK2";
}

const gchar *plugin_licence(void)
{
	return "GPL3+";
}

const gchar *plugin_version(void)
{
	return VERSION;
}


/* popup menu callbacks */
static void trayicon_get_all_cb( GtkAction *action, gpointer data )
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	inc_all_account_mail_cb(mainwin, 0, NULL);
}

static void trayicon_compose_cb( GtkAction *action, gpointer data )
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	compose_mail_cb(mainwin, 0, NULL);
}

static void trayicon_compose_acc_cb( GtkMenuItem *menuitem, gpointer data )
{
	compose_new((PrefsAccount *)data, NULL, NULL);
}

static void trayicon_addressbook_cb( GtkAction *action, gpointer data )
{
	addressbook_open(NULL);
}

static void trayicon_toggle_offline_cb( GtkAction *action, gpointer data )
{
	/* toggle offline mode if menu checkitem has been clicked */
	if (!updating_menu) {
		MainWindow *mainwin = mainwindow_get_mainwindow();
		main_window_toggle_work_offline(mainwin, !prefs_common.work_offline, TRUE);
	}
}

static void app_exit_cb(MainWindow *mainwin, guint action, GtkWidget *widget)
{
	if (prefs_common.clean_on_exit) {
		if (!main_window_empty_trash(mainwin, prefs_common.ask_on_clean, TRUE))
			return;
	}

	if (prefs_common.confirm_on_exit) {
		if (alertpanel(_("Exit"), _("Exit Claws Mail?"),
			       GTK_STOCK_CANCEL, GTK_STOCK_OK,
			       NULL) != G_ALERTALTERNATE) {
			return;
		}
		manage_window_focus_in(mainwin->window, NULL, NULL);
	}

	app_will_exit(NULL, mainwin);
}

static void trayicon_exit_cb( GtkAction *action, gpointer data )
{
	MainWindow *mainwin = mainwindow_get_mainwindow();

	if (mainwin->lock_count == 0) {
		app_exit_cb(mainwin, 0, NULL);
	}
}

struct PluginFeature *plugin_provides(void)
{
	static struct PluginFeature features[] = 
		{ {PLUGIN_NOTIFIER, N_("Trayicon")},
		  {PLUGIN_NOTHING, NULL}};
	return features;
}
