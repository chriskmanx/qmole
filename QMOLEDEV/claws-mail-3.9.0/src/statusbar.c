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

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <stdarg.h>

#include "mainwindow.h"
#include "statusbar.h"
#include "gtkutils.h"
#include "utils.h"
#include "log.h"
#include "hooks.h"

#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-banner.h>
#else
#include <hildon-widgets/hildon-banner.h>
#endif
#endif


#define BUFFSIZE 1024

static GList *statusbar_list = NULL;
gint statusbar_puts_all_hook_id = -1;

GtkWidget *statusbar_create(void)
{
	GtkWidget *statusbar;
	GtkWidget *child;
	GtkWidget *parent;
	GtkWidget *hbox;

	statusbar = gtk_statusbar_new();
	gtk_widget_set_size_request(statusbar, 1, -1);
	statusbar_list = g_list_append(statusbar_list, statusbar);
#if !GTK_CHECK_VERSION(3, 0, 0)
	gtk_statusbar_set_has_resize_grip(GTK_STATUSBAR(statusbar), 
					  FALSE);
#else
	gtk_window_set_has_resize_grip(GTK_WINDOW(statusbar), 
					  FALSE);
#endif
	gtk_container_set_border_width(GTK_CONTAINER(statusbar), 1);
	child = gtk_statusbar_get_message_area(GTK_STATUSBAR(statusbar));
	parent = gtk_widget_get_parent(child);
	gtk_container_remove(GTK_CONTAINER(parent), g_object_ref(child));
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(parent), hbox);
	gtk_widget_show(hbox);
	gtk_box_pack_start(GTK_BOX(hbox), child, TRUE, TRUE, 0);
	g_object_unref(child);	

	return statusbar;
}

void statusbar_puts(GtkStatusbar *statusbar, const gchar *str)
{
	gint cid;
	gchar *buf;
	gchar *tmp;

	tmp = g_strdup(str);
	strretchomp(tmp);
	buf = trim_string(tmp, 76);
	g_free(tmp);

	cid = gtk_statusbar_get_context_id(statusbar, "Standard Output");
	gtk_statusbar_pop(statusbar, cid);
	gtk_statusbar_push(statusbar, cid, buf);
	gtkut_widget_draw_now(GTK_WIDGET(statusbar));

	g_free(buf);
}

void statusbar_puts_all(const gchar *str)
{
	GList *cur;

	for (cur = statusbar_list; cur != NULL; cur = cur->next)
		statusbar_puts(GTK_STATUSBAR(cur->data), str);
}

void statusbar_print(GtkStatusbar *statusbar, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE];

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);

	statusbar_puts(statusbar, buf);
}

#ifdef MAEMO
static GSList *banner_texts = NULL;
static GtkWidget *banner = NULL;
void statuswindow_print_all(const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE];
	GList *cur;

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);

	for (cur = statusbar_list; cur != NULL; cur = cur->next)
		statusbar_puts(GTK_STATUSBAR(cur->data), buf);
	if (mainwindow_get_mainwindow()) {
		if (banner != NULL) {
			gchar *last_text = (gchar *)banner_texts->data;
			if (!strcmp2(last_text, buf))
				return;
		}
		statusbar_pop_all();
		if (banner == NULL) {
			banner = hildon_banner_show_animation(
				mainwindow_get_mainwindow()->window,
				NULL,
				buf);
			g_object_ref(banner);
			banner_texts = g_slist_prepend(banner_texts, g_strdup(buf));
		} else {
			hildon_banner_set_text(HILDON_BANNER(banner), buf);
			banner_texts = g_slist_prepend(banner_texts, g_strdup(buf));
		}
	}
}

void statuswindow_pop_all(void)
{
	GList *cur;
	gint cid;

	for (cur = statusbar_list; cur != NULL; cur = cur->next) {
		cid = gtk_statusbar_get_context_id(GTK_STATUSBAR(cur->data),
						   "Standard Output");
		gtk_statusbar_pop(GTK_STATUSBAR(cur->data), cid);
	}
	if (banner && banner_texts) {
		gchar *old_text = (gchar *)banner_texts->data;
		gchar *prev_text = NULL;
		banner_texts = g_slist_remove(banner_texts, old_text);	
		g_free(old_text);
		if (banner_texts) {
			prev_text = (gchar *)banner_texts->data;
			hildon_banner_set_text(HILDON_BANNER(banner), prev_text);
		} else {
			gtk_widget_destroy(banner);
			g_object_unref(banner);
			banner = NULL;
		}
	}
}
#endif

void statusbar_print_all(const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE];
	GList *cur;

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);

	for (cur = statusbar_list; cur != NULL; cur = cur->next)
		statusbar_puts(GTK_STATUSBAR(cur->data), buf);
}

void statusbar_pop_all(void)
{
	GList *cur;
	gint cid;

	for (cur = statusbar_list; cur != NULL; cur = cur->next) {
		cid = gtk_statusbar_get_context_id(GTK_STATUSBAR(cur->data),
						   "Standard Output");
		gtk_statusbar_pop(GTK_STATUSBAR(cur->data), cid);
	}
}

static gboolean statusbar_puts_all_hook (gpointer source, gpointer data)
{
	LogText *logtext = (LogText *) source;

	cm_return_val_if_fail(logtext != NULL, TRUE);
	cm_return_val_if_fail(logtext->text != NULL, TRUE);

	statusbar_pop_all();
	if (logtext->type == LOG_NORMAL) {
		statusbar_puts_all(logtext->text + LOG_TIME_LEN);
	} else if (logtext->type == LOG_MSG) {
		statusbar_puts_all(logtext->text);
	}

	return FALSE;
}

void statusbar_verbosity_set(gboolean verbose)
{
	if (verbose && (statusbar_puts_all_hook_id == -1)) {
		statusbar_puts_all_hook_id =
			hooks_register_hook(LOG_APPEND_TEXT_HOOKLIST, statusbar_puts_all_hook, NULL);
	} else if (!verbose && (statusbar_puts_all_hook_id != -1)) {
		hooks_unregister_hook(LOG_APPEND_TEXT_HOOKLIST, statusbar_puts_all_hook_id);
		statusbar_puts_all_hook_id = -1;
		statusbar_pop_all();
	}
}

void statusbar_progress_all (gint done, gint total, gint step) 
{
	GtkProgressBar *progressbar = GTK_PROGRESS_BAR(
					mainwindow_get_mainwindow()->progressbar);
	gchar buf[32];
	
	if (total && done % step == 0) {
#ifdef GENERIC_UMPC
		/* use a more compact format */
		const gchar *format = "%d/%d";
#else
		const gchar *format = "%d / %d";
#endif
		g_snprintf(buf, sizeof(buf), format, done, total);
		gtk_progress_bar_set_text(progressbar, buf);
		gtk_progress_bar_set_fraction(progressbar,
			 (total == 0) ? 0 : (gfloat)done / (gfloat)total);
		if (!gtk_widget_get_visible(GTK_WIDGET(progressbar)))
			gtk_widget_show(GTK_WIDGET(progressbar));
	} else if (total == 0) {
		gtk_progress_bar_set_text(progressbar, "");
		gtk_progress_bar_set_fraction(progressbar, 0.0);
		gtk_widget_hide(GTK_WIDGET(progressbar));
	}
}
