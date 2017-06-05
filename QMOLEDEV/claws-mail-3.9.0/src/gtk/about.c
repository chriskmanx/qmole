/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#if HAVE_SYS_UTSNAME_H
#  include <sys/utsname.h>
#endif
#include <errno.h>

#include "about.h"
#include "gtkutils.h"
#include "stock_pixmap.h"
#include "prefs_common.h"
#include "utils.h"
#include "version.h"
#include "authors.h"
#include "codeconv.h"
#include "menu.h"
#include "textview.h"
#include "main.h"

extern SessionStats session_stats;
static GtkTextBuffer *stats_text_buffer;

static GtkWidget *window;
static gchar* uri_hover = NULL;
static GtkTextIter uri_hover_start_iter;
static GtkTextIter uri_hover_end_iter;
static GdkCursor *hand_cursor = NULL;
static GdkCursor *text_cursor = NULL;

static void about_create(void);
static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event);
static gboolean about_textview_uri_clicked(GtkTextTag *tag, GObject *obj,
					GdkEvent *event, GtkTextIter *iter,
					GtkWidget *textview);
static gboolean about_textview_motion_notify(GtkWidget *widget,
					GdkEventMotion *event,
					GtkWidget *textview);
static gboolean about_textview_leave_notify(GtkWidget *widget,
					GdkEventCrossing *event,
					GtkWidget *textview);
static void about_size_allocate_cb(GtkWidget *widget,
				   	GtkAllocation *allocation);
static void about_textview_uri_update(GtkWidget *textview, gint x, gint y);
static void about_update_stats(void);

static GtkWidget *link_popupmenu;


void about_show(void)
{
	if (!window)
		about_create();
	else {
		about_update_stats();
		gtk_window_present(GTK_WINDOW(window));
	}
	
}

static GtkWidget *about_create_child_page_info(void)
{
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	GdkColor uri_color;
	gchar buf[1024];
	GtkTextTag *tag;
#if HAVE_SYS_UTSNAME_H
	struct utsname utsbuf;
#endif

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);

	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);
	gtk_widget_add_events(text, GDK_LEAVE_NOTIFY_MASK);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, 0);

	/* textview link style (based upon main prefs) */
	gtkut_convert_int_to_gdk_color(prefs_common.uri_col,
				(GdkColor*)&uri_color);
	tag = gtk_text_buffer_create_tag(buffer, "link",
				"foreground-gdk", &uri_color,
				"wrap-mode", GTK_WRAP_NONE,
				NULL);
	gtk_text_buffer_create_tag(buffer, "link-hover",
				"foreground-gdk", &uri_color,
				"underline", PANGO_UNDERLINE_SINGLE,
				NULL);

	gtk_text_buffer_insert(buffer, &iter, _(
				"Claws Mail is a lightweight, fast and "
				"highly-configurable email client.\n\n"
				"For further information visit the Claws Mail "
				"website:\n"), -1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, HOMEPAGE_URI, -1,
				"link", NULL);
	gtk_text_buffer_insert(buffer, &iter, _("\n\n"
				"Claws Mail is free software released "
				"under the GPL. If you wish to donate "
				"to the Claws Mail project you can do "
				"so at:\n"), -1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, DONATE_URI, -1,
				"link", NULL);

	gtk_text_buffer_create_tag(buffer, "indented-list-item",
				"indent", 8,
				NULL);
	gtk_text_buffer_create_tag(buffer, "underlined-list-title",
				"underline", PANGO_UNDERLINE_SINGLE,
				NULL);
#ifdef GENERIC_UMPC
	gtk_text_buffer_insert(buffer, &iter, _(
				"\n\nCopyright (C) 1999-2012\nThe Claws Mail Team\n"
				" and Hiroyuki Yamamoto"), -1);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("\n\nSystem Information\n")), -1,
			"underlined-list-title", NULL);

#if HAVE_SYS_UTSNAME_H
	uname(&utsbuf);
	g_snprintf(buf, sizeof(buf),
		   _("GTK+ %d.%d.%d / GLib %d.%d.%d\n"
		     "Locale: %s (charset: %s)\n"
		     "Operating System: %s %s (%s)"),
		   gtk_major_version, gtk_minor_version, gtk_micro_version,
		   glib_major_version, glib_minor_version, glib_micro_version,
		   conv_get_current_locale(), conv_get_locale_charset_str(),
		   utsbuf.sysname, utsbuf.release, utsbuf.machine);
#elif defined(G_OS_WIN32)
	g_snprintf(buf, sizeof(buf),
		   _("GTK+ %d.%d.%d / GLib %d.%d.%d\n"
		     "Locale: %s (charset: %s)\n"
		     "Operating System: %s"),
		   gtk_major_version, gtk_minor_version, gtk_micro_version,
		   glib_major_version, glib_minor_version, glib_micro_version,
		   conv_get_current_locale(), conv_get_locale_charset_str(),
		   "Win32");
#else
	g_snprintf(buf, sizeof(buf),
		   _("GTK+ %d.%d.%d / GLib %d.%d.%d\n"
		     "Locale: %s (charset: %s)\n"
		     "Operating System: unknown"),
		   gtk_major_version, gtk_minor_version, gtk_micro_version,
		   glib_major_version, glib_minor_version, glib_micro_version,
		   conv_get_current_locale(), conv_get_locale_charset_str());
#endif

	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, buf, -1,
						 "indented-list-item", NULL);

	gtk_text_buffer_insert(buffer, &iter, "\n", -1);

	g_signal_connect(G_OBJECT(tag), "event",
				G_CALLBACK(about_textview_uri_clicked), text);
	g_signal_connect(G_OBJECT(text), "motion-notify-event",
				G_CALLBACK(about_textview_motion_notify), text);
	g_signal_connect(G_OBJECT(text), "leave-notify-event",
				G_CALLBACK(about_textview_leave_notify), text);

	return scrolledwin;
}

static GtkWidget *about_create_child_page_authors(void)
{
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	gint i;

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);

	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);
	gtk_widget_add_events(text, GDK_LEAVE_NOTIFY_MASK);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, 0);

	/* init formatting tag: indentation for list items */
	gtk_text_buffer_create_tag(buffer, "indented-list-item",
				"indent", 8,
				NULL);
	gtk_text_buffer_create_tag(buffer, "underlined-list-title",
				"underline", PANGO_UNDERLINE_SINGLE,
				NULL);

	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("The Claws Mail Team")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; TEAM_LIST[i] != NULL; i++) {
		if (g_utf8_validate(TEAM_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, TEAM_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(TEAM_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("Previous team members")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; EX_TEAM_LIST[i] != NULL; i++) {
		if (g_utf8_validate(EX_TEAM_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, EX_TEAM_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(EX_TEAM_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("The translation team")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; TRANS_TEAM_LIST[i] != NULL; i++) {
		if (g_utf8_validate(TRANS_TEAM_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, TRANS_TEAM_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(TRANS_TEAM_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("Documentation team")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; DOC_TEAM_LIST[i] != NULL; i++) {
		if (g_utf8_validate(DOC_TEAM_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, DOC_TEAM_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(DOC_TEAM_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("Logo")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; LOGO_LIST[i] != NULL; i++) {
		if (g_utf8_validate(LOGO_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, LOGO_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(LOGO_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("Icons")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; ICONS_LIST[i] != NULL; i++) {
		if (g_utf8_validate(ICONS_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, ICONS_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(ICONS_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (_("Contributors")), -1,
			"underlined-list-title", NULL);
	gtk_text_buffer_insert(buffer, &iter, "\n", 1);

	for (i = 0; CONTRIBS_LIST[i] != NULL; i++) {
		if (g_utf8_validate(CONTRIBS_LIST[i], -1, NULL))
			gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, CONTRIBS_LIST[i], -1,
					"indented-list-item", NULL);
		else {
			gchar *conv = conv_codeset_strdup(CONTRIBS_LIST[i], CS_ISO_8859_1, CS_UTF_8);
			if (conv)
				gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, conv, -1,
						"indented-list-item", NULL);
			g_free(conv);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);
	}

	return scrolledwin;
}

static GtkWidget *about_create_child_page_features(void)
{
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	GdkPixbuf *active_pixbuf;
	GdkPixbuf *inactive_pixbuf;

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);

	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);
	gtk_widget_add_events(text, GDK_LEAVE_NOTIFY_MASK);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, 0);

	gtk_text_buffer_insert(buffer, &iter, _("Compiled-in Features\n"), -1);

	gtk_text_buffer_create_tag(buffer, "bold", "weight", PANGO_WEIGHT_BOLD,
				   NULL);

	stock_pixbuf_gdk(window, STOCK_PIXMAP_CHECKBOX_ON, &active_pixbuf);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_CHECKBOX_OFF, &inactive_pixbuf);

#if HAVE_LIBCOMPFACE
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" compface "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("compface|adds support for the X-Face header\n"), -1);

#if USE_ENCHANT
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" Enchant "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("Enchant|adds support for spell checking\n"), -1);

#if USE_GNUTLS
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" GnuTLS "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("GnuTLS|adds support for encrypted connections to servers\n"), -1);

#if INET6
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" IPv6 "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("IPv6|adds support for IPv6 addresses, the new Internet "
			    "addressing protocol\n"), -1);

#if HAVE_ICONV
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" iconv "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("iconv|allows converting to and from different character sets\n"), -1);

#if USE_JPILOT
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" JPilot "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("JPilot|adds support for PalmOS addressbooks\n"), -1);

#if USE_LDAP
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" LDAP "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("LDAP|adds support for LDAP shared addressbooks\n"), -1);

#if HAVE_LIBETPAN
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" libetpan "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("libetpan|adds support for IMAP and NNTP servers\n"), -1);

#if HAVE_LIBSM
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" libSM "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter, 
		(gchar *)Q_("libSM|adds support for session handling\n"), -1);

#if HAVE_NETWORKMANAGER_SUPPORT
	gtk_text_buffer_insert_pixbuf(buffer, &iter, active_pixbuf);
#else
	gtk_text_buffer_insert_pixbuf(buffer, &iter, inactive_pixbuf);
#endif
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, (" NetworkManager "), -1,
						 "bold", NULL);
	gtk_text_buffer_insert(buffer, &iter,
		(gchar *)Q_("NetworkManager|adds support for detection of network connection changes\n"), -1);

	return scrolledwin;
}

static GtkWidget *about_create_child_page_license(void)
{
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	GdkColor uri_color;
	GtkTextTag *tag;

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);

	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, 0);

	gtk_text_buffer_insert(buffer, &iter,
		_("This program is free software; you can redistribute it and/or modify "
		  "it under the terms of the GNU General Public License as published by "
		  "the Free Software Foundation; either version 3, or (at your option) "
		  "any later version.\n\n"), -1);

	gtk_text_buffer_insert(buffer, &iter,
		_("This program is distributed in the hope that it will be useful, "
		  "but WITHOUT ANY WARRANTY; without even the implied warranty of "
		  "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. "
		  "See the GNU General Public License for more details.\n\n"), -1);

	/* textview link style (based upon main prefs) */
	gtkut_convert_int_to_gdk_color(prefs_common.uri_col,
			(GdkColor*)&uri_color);

	tag = gtk_text_buffer_create_tag(buffer, "link",
		"foreground-gdk", &uri_color,
		NULL);
	gtk_text_buffer_create_tag(buffer, "link-hover",
		"foreground-gdk", &uri_color,
		"underline", PANGO_UNDERLINE_SINGLE,
		NULL);

	gtk_text_buffer_insert(buffer, &iter,
		_("You should have received a copy of the GNU General Public License "
		  "along with this program. If not, see <"), -1);
	gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, 
		"http://www.gnu.org/licenses/", -1,
		"link", NULL);
	gtk_text_buffer_insert(buffer, &iter, _(">. \n\n"), -1);

	g_signal_connect(G_OBJECT(tag), "event",
				G_CALLBACK(about_textview_uri_clicked), text);
	g_signal_connect(G_OBJECT(text), "motion-notify-event",
			 G_CALLBACK(about_textview_motion_notify), text);
	g_signal_connect(G_OBJECT(text), "leave-notify-event",
				G_CALLBACK(about_textview_leave_notify), text);

	return scrolledwin;
}

static gboolean release_notes_available(void)
{
	gboolean ret = FALSE;
	gchar *path = NULL;

	path = g_strconcat(DOCDIR, G_DIR_SEPARATOR_S, RELEASE_NOTES_FILE, NULL);
	ret = (is_file_exist(path));
	g_free(path);

	return ret;
}

static GtkWidget *about_create_child_page_release_notes(void)
{
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	gchar *path, buf[1024];
	FILE *fp;

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
			GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
			GTK_SHADOW_IN);
	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, 0);

	path = g_strconcat(DOCDIR, G_DIR_SEPARATOR_S, RELEASE_NOTES_FILE, NULL);
	if ((fp = g_fopen(path, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
		g_free(path);
		return scrolledwin;
	}
	g_free(path);

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		const gchar *src_codeset = conv_get_locale_charset_str();
		const gchar *dest_codeset = CS_UTF_8;
		gchar *tmp;

		tmp = conv_codeset_strdup(buf, src_codeset, dest_codeset);
		if (!tmp) {
			g_warning("Failed to convert character set of action configuration\n");
			tmp = g_strdup(buf);
		}

		gtk_text_buffer_insert(buffer, &iter, tmp, -1);
		g_free(tmp);
	}
	fclose(fp);

	return scrolledwin;
}

static GtkWidget *about_create_child_page_session_stats(void)
{
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextIter iter;

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
			GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
			GTK_SHADOW_IN);
	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);

	stats_text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));

	gtk_text_buffer_get_iter_at_offset(stats_text_buffer, &iter, 0);
	gtk_text_buffer_create_tag(stats_text_buffer, "indented-list-item",
				"indent", 8,
				NULL);
	gtk_text_buffer_create_tag(stats_text_buffer, "underlined-list-title",
				"underline", PANGO_UNDERLINE_SINGLE,
				NULL);
	gtk_text_buffer_create_tag(stats_text_buffer, "bold", "weight", PANGO_WEIGHT_BOLD,
				   NULL);

	about_update_stats();

	return scrolledwin;
}

static void about_update_stats(void)
{
	if (stats_text_buffer != NULL)
	{
		GtkTextIter start, end, iter;
		gchar buf[1024];

		gtk_text_buffer_get_start_iter(stats_text_buffer, &start);
		gtk_text_buffer_get_end_iter(stats_text_buffer, &end);
		gtk_text_buffer_delete(stats_text_buffer, &start, &end);

		gtk_text_buffer_get_iter_at_offset(stats_text_buffer, &iter, 0);

		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter,
				(_("Session statistics\n")), -1,
				"underlined-list-title", NULL);

		if (prefs_common.date_format) {
			struct tm *lt;
			gint len = 100;
			gchar date[len];

			lt = localtime(&session_stats.time_started);
			fast_strftime(date, len, prefs_common.date_format, lt);
			g_snprintf(buf, sizeof(buf), _("Started: %s\n"),
						lt ? date : ctime(&session_stats.time_started));
		} else
			g_snprintf(buf, sizeof(buf), _("Started: %s\n"),
						ctime(&session_stats.time_started));
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter, buf, -1,
				"indented-list-item", NULL);

		gtk_text_buffer_insert(stats_text_buffer, &iter, "\n", 1);
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter,
				(_("Incoming traffic\n")), -1,
				"underlined-list-title", NULL);

		g_snprintf(buf, sizeof(buf), _("Received messages: %d\n"),
					session_stats.received);
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter, buf, -1,
				"indented-list-item", "bold", NULL);

		gtk_text_buffer_insert(stats_text_buffer, &iter, "\n", 1);
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter,
				(_("Outgoing traffic\n")), -1,
				"underlined-list-title", NULL);

		g_snprintf(buf, sizeof(buf), _("New/redirected messages: %d\n"),
					session_stats.sent);
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter, buf, -1,
				"indented-list-item", NULL);

		g_snprintf(buf, sizeof(buf), _("Replied messages: %d\n"),
					session_stats.replied);
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter, buf, -1,
				"indented-list-item", NULL);

		g_snprintf(buf, sizeof(buf), _("Forwarded messages: %d\n"),
					session_stats.forwarded);
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter, buf, -1,
				"indented-list-item", NULL);

		g_snprintf(buf, sizeof(buf), _("Total outgoing messages: %d\n"),
					(session_stats.sent + session_stats.replied +
					 session_stats.forwarded));
		gtk_text_buffer_insert_with_tags_by_name(stats_text_buffer, &iter, buf, -1,
				"indented-list-item", "bold", NULL);
	} 
}

static void about_create(void)
{
	GtkWidget *vbox1;
	GtkWidget *image;	
 	GtkWidget *vbox2;
	GtkWidget *label;
	GtkWidget *button;
	GtkWidget *scrolledwin;
	GtkWidget *notebook;
	GtkWidget *table;
	char *markup;
	GtkWidget *confirm_area;
	GtkWidget *close_button;
	static GdkGeometry geometry;

	stats_text_buffer = NULL;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "about");
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER_ALWAYS);
	gtk_window_set_title(GTK_WINDOW(window), _("About Claws Mail"));
	gtk_container_set_border_width(GTK_CONTAINER(window), 8);
	gtk_widget_set_size_request(window, -1, -1);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(about_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(about_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(gtk_widget_hide_on_delete), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	
	if (!geometry.min_width) {
		geometry.min_width = 450;
		geometry.min_height = 500;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_window_set_default_size(GTK_WINDOW(window), prefs_common.aboutwin_width,
				    prefs_common.aboutwin_height);	
	
	gtk_widget_realize(window);

	vbox1 = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(window), vbox1);

	table = gtk_table_new (2, 1, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox1), table,
			FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (table), 8);
	gtk_table_set_row_spacings (GTK_TABLE (table), 8);
	gtk_table_set_col_spacings (GTK_TABLE (table), 8);

	image = stock_pixmap_widget(window, STOCK_PIXMAP_CLAWS_MAIL_LOGO);
	gtk_table_attach (GTK_TABLE (table), image, 0, 1, 0, 1,
			(GtkAttachOptions) (GTK_EXPAND),
			(GtkAttachOptions) (0), 0, 0);

	vbox2 = gtk_vbox_new (FALSE, 4);
	gtk_table_attach (GTK_TABLE (table), vbox2, 1, 2, 0, 1,
			(GtkAttachOptions) (GTK_EXPAND),
			(GtkAttachOptions) (0), 0, 0);

	label = gtk_label_new("");
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(vbox2), label, FALSE, FALSE, 0);
	markup = g_markup_printf_escaped
		("<span weight=\"bold\" size=\"xx-large\">Claws Mail</span>\nversion %s",
		 VERSION);
	gtk_label_set_markup(GTK_LABEL(label), markup);
	g_free(markup);

	button = gtkut_get_link_btn(window, HOMEPAGE_URI, " "HOMEPAGE_URI" ");
	gtk_box_pack_start(GTK_BOX(vbox2), button, FALSE, FALSE, 0);
#ifndef GENERIC_UMPC
	label = gtk_label_new
		(_("Copyright (C) 1999-2012\nThe Claws Mail Team\n"
		 "and Hiroyuki Yamamoto"));
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox2), label, FALSE, FALSE, 0);
#endif
	notebook = gtk_notebook_new();
	gtk_widget_set_size_request(notebook, -1, 220);
	gtk_widget_show(notebook);

	if ((scrolledwin = about_create_child_page_info()) != NULL) {
		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
				scrolledwin,
				gtk_label_new_with_mnemonic(_("_Info")));
	}

	if ((scrolledwin = about_create_child_page_authors()) != NULL) {
		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
				scrolledwin,
				gtk_label_new_with_mnemonic(_("_Authors")));
	}

	if ((scrolledwin = about_create_child_page_features()) != NULL) {
		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
				scrolledwin,
				gtk_label_new_with_mnemonic(_("_Features")));
	}

	if ((scrolledwin = about_create_child_page_license()) != NULL) {
		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
				scrolledwin,
				gtk_label_new_with_mnemonic(_("_License")));
	}

	if (release_notes_available() &&
			(scrolledwin = about_create_child_page_release_notes()) != NULL) {

		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
				scrolledwin,
				gtk_label_new_with_mnemonic(_("_Release Notes")));
	}

	if ((scrolledwin = about_create_child_page_session_stats()) != NULL) {
		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
				scrolledwin,
				gtk_label_new_with_mnemonic(_("_Statistics")));
	}

	gtk_box_pack_start(GTK_BOX(vbox1), notebook, TRUE, TRUE, 0);

	gtkut_stock_button_set_create(&confirm_area, &close_button, GTK_STOCK_CLOSE,
				      NULL, NULL, NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox1), confirm_area, FALSE, FALSE, 4);
	gtk_widget_grab_default(close_button);
	gtk_widget_grab_focus(close_button);
	g_signal_connect_closure
		(G_OBJECT(close_button), "clicked",
		 g_cclosure_new_swap(G_CALLBACK(gtk_widget_hide_on_delete),
				     window, NULL), FALSE);

	gtk_widget_show_all(window);
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event)
{
	if (event && event->keyval == GDK_KEY_Escape)
		gtk_widget_hide(window);
	return FALSE;
}

static void about_size_allocate_cb(GtkWidget *widget,
				   GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.aboutwin_width = allocation->width;
	prefs_common.aboutwin_height = allocation->height;
}


static gboolean about_textview_uri_clicked(GtkTextTag *tag, GObject *obj,
					GdkEvent *event, GtkTextIter *iter,
					GtkWidget *textview)
{
	GtkTextIter start_iter, end_iter;
	GdkEventButton *bevent;
	gchar *link = NULL;

	if (!event || !tag) {
		return FALSE;
	}

	if (event->type != GDK_BUTTON_PRESS && event->type != GDK_2BUTTON_PRESS
		&& event->type != GDK_BUTTON_RELEASE) {
		return FALSE;
	}

	/* get link text from tag */
	if (get_tag_range(iter, tag, &start_iter,
				   &end_iter) == FALSE) {
		return FALSE;
	}
	link = gtk_text_iter_get_text(&start_iter, &end_iter);
	if (link == NULL) {
		return FALSE;
	}

	bevent = (GdkEventButton *) event;
	if (bevent->button == 1 && event->type == GDK_BUTTON_RELEASE) {
		GtkTextBuffer *buffer;

		/* we shouldn't follow a link if the user has selected something */
		buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
		gtk_text_buffer_get_selection_bounds(buffer, &start_iter, &end_iter);
		if (gtk_text_iter_get_offset(&start_iter) != gtk_text_iter_get_offset(&end_iter)) {
			return FALSE;
		}
		/* open link and do *not* return TRUE so that
		   further gtk processing of the signal is done */
		open_uri(link, prefs_common_get_uri_cmd());

	} else {
		if (bevent->button == 3 && event->type == GDK_BUTTON_PRESS) {
			link_popupmenu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
				gtk_ui_manager_get_widget(gtkut_ui_manager(), "/Menus/TextviewPopupLink")));

			g_object_set_data(
					G_OBJECT(link_popupmenu),
					"raw_url", link);
			gtk_menu_popup(GTK_MENU(link_popupmenu), 
					NULL, NULL, NULL, NULL, 
					bevent->button, bevent->time);

			return TRUE;
		}
	}
	return FALSE;
}

static gboolean about_textview_motion_notify(GtkWidget *widget,
					GdkEventMotion *event,
					GtkWidget *textview)
{
	about_textview_uri_update(textview, event->x, event->y);
	gdk_window_get_pointer(gtk_widget_get_window(widget), NULL, NULL, NULL);

	return FALSE;
}

static gboolean about_textview_leave_notify(GtkWidget *widget,
					GdkEventCrossing *event,
					GtkWidget *textview)
{
	about_textview_uri_update(textview, -1, -1);

	return FALSE;
}

static void about_textview_uri_update(GtkWidget *textview, gint x, gint y)
{
	GtkTextBuffer *buffer;
	GtkTextIter start_iter, end_iter;
	gchar *uri = NULL;
	gboolean same;
	
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));

	if (x != -1 && y != -1) {
		gint bx, by;
		GtkTextIter iter;
		GSList *tags;
		GSList *cur;
	    
		gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(textview), 
				GTK_TEXT_WINDOW_WIDGET,
				x, y, &bx, &by);
		gtk_text_view_get_iter_at_location(GTK_TEXT_VIEW(textview),
				&iter, bx, by);

		tags = gtk_text_iter_get_tags(&iter);
		for (cur = tags; cur != NULL; cur = cur->next) {
			GtkTextTag *tag = cur->data;
			char *name;

			g_object_get(G_OBJECT(tag), "name", &name, NULL);
			if (strcmp(name, "link") == 0
			    && get_tag_range(&iter, tag, &start_iter, &end_iter)) {
				uri = gtk_text_iter_get_text(&start_iter, &end_iter);
			}
			g_free(name);

			if (uri) {
				break;
			}
		}
		g_slist_free(tags);
	}

	/* compare previous hovered link and this one
	   (here links must be unique in text buffer otherwise ClickableText structures should be
	   used as in textview.c) */
	same = (uri != NULL && uri_hover != NULL
		&& strcmp((char*)uri, (char*)uri_hover) == 0);

	if (same == FALSE) {
		GdkWindow *window;

		if (uri_hover) {
			gtk_text_buffer_remove_tag_by_name(buffer,
					"link-hover",
					&uri_hover_start_iter,
					&uri_hover_end_iter);
		}
		    
		uri_hover = uri;
		if (uri) {
			uri_hover_start_iter = start_iter;
			uri_hover_end_iter = end_iter;

			gtk_text_buffer_apply_tag_by_name(buffer,
					"link-hover",
					&start_iter,
					&end_iter);
		}
		
		window = gtk_text_view_get_window(GTK_TEXT_VIEW(textview),
						GTK_TEXT_WINDOW_TEXT);
		if (!hand_cursor)
			hand_cursor = gdk_cursor_new(GDK_HAND2);
		if (!text_cursor)
			text_cursor = gdk_cursor_new(GDK_XTERM);
		gdk_window_set_cursor(window, uri ? hand_cursor : text_cursor);
	}
}
