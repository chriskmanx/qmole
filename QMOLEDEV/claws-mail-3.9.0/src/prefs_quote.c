/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2012 Colin Leroy <colin@colino.net> & The Claws Mail Team
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

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "prefs_common.h"
#include "prefs_gtk.h"
#include "prefs_template.h"
#include "alertpanel.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"

#include "manage_window.h"
#include "quote_fmt.h"

typedef struct _QuotePage
{
	PrefsPage page;

	GtkWidget *window;

	GtkWidget *checkbtn_compose_with_format;
	GtkWidget *entry_subject;
	GtkWidget *text_format;
	GtkWidget *entry_quotemark;
	GtkWidget *text_quotefmt;
	GtkWidget *entry_fw_quotemark;
	GtkWidget *text_fw_quotefmt;
	GtkWidget *btn_quotedesc;
} QuotePage;

QuotePage *prefs_quote;

static void prefs_quote_set_default_new_msg_fmt(void)
{
	cm_return_if_fail(prefs_quote->text_format != NULL);

	pref_set_textview_from_pref(GTK_TEXT_VIEW(prefs_quote->text_format),
		_("Hello,\\n"));
}

static void prefs_quote_set_default_reply_fmt(void)
{
	cm_return_if_fail(prefs_quote->text_quotefmt != NULL);

	pref_set_textview_from_pref(GTK_TEXT_VIEW(prefs_quote->text_quotefmt),
		_("On %d\\n%f wrote:\\n\\n%q"));
}

static void prefs_quote_set_default_forward_fmt(void)
{
	cm_return_if_fail(prefs_quote->text_fw_quotefmt != NULL);

	pref_set_textview_from_pref(GTK_TEXT_VIEW(prefs_quote->text_fw_quotefmt),
		_("\\n\\nBegin forwarded message:\\n\\n"
		"?d{Date: %d\\n}?f{From: %f\\n}?t{To: %t\\n}?c{Cc: %c\\n}"
		"?n{Newsgroups: %n\\n}?s{Subject: %s\\n}\\n\\n%M"));
}

static void prefs_quote_create_widget(PrefsPage *_page, GtkWindow *window, 
			       	  gpointer data)
{
	QuotePage *prefs_quote = (QuotePage *) _page;
	
	GtkWidget *vbox;
	GtkWidget *vbox2;
	GtkWidget *notebook;

	vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox);

	notebook = gtk_notebook_new();
	gtk_widget_show(notebook);
	gtk_box_pack_start(GTK_BOX(vbox), notebook, TRUE, TRUE, 0);

	/* new message */
	vbox2 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox2);
	gtk_container_set_border_width (GTK_CONTAINER (vbox2), VBOX_BORDER);
	
	quotefmt_create_new_msg_fmt_widgets(
				window,
				vbox2,
				&prefs_quote->checkbtn_compose_with_format,
				NULL,
				&prefs_quote->entry_subject,
				&prefs_quote->text_format,
				TRUE, prefs_quote_set_default_new_msg_fmt);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox2, gtk_label_new(_("Compose")));

	/* reply */
	vbox2 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox2);
	gtk_container_set_border_width (GTK_CONTAINER (vbox2), VBOX_BORDER);
	
	quotefmt_create_reply_fmt_widgets(
				window,
				vbox2,
				NULL,
				NULL,
				&prefs_quote->entry_quotemark,
				&prefs_quote->text_quotefmt,
				TRUE, prefs_quote_set_default_reply_fmt);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox2, gtk_label_new(_("Reply")));

	/* forward */
	vbox2 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox2);
	gtk_container_set_border_width (GTK_CONTAINER (vbox2), VBOX_BORDER);
	
	quotefmt_create_forward_fmt_widgets(
				window,
				vbox2,
				NULL,
				NULL,
				&prefs_quote->entry_fw_quotemark,
				&prefs_quote->text_fw_quotefmt,
				TRUE, prefs_quote_set_default_forward_fmt);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox2, gtk_label_new(_("Forward")));

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prefs_quote->checkbtn_compose_with_format),
			prefs_common.compose_with_format);
	pref_set_entry_from_pref(GTK_ENTRY(prefs_quote->entry_subject),
			prefs_common.compose_subject_format);
	if (prefs_common.compose_body_format)
		pref_set_textview_from_pref(GTK_TEXT_VIEW(prefs_quote->text_format),
				prefs_common.compose_body_format);
	else
		prefs_quote_set_default_new_msg_fmt();

	gtk_entry_set_text(GTK_ENTRY(prefs_quote->entry_quotemark), 
			prefs_common.quotemark?prefs_common.quotemark:"");
	if (prefs_common.quotefmt)
		pref_set_textview_from_pref(GTK_TEXT_VIEW(prefs_quote->text_quotefmt),
				prefs_common.quotefmt);
	else
		prefs_quote_set_default_reply_fmt();

	gtk_entry_set_text(GTK_ENTRY(prefs_quote->entry_fw_quotemark), 
			prefs_common.fw_quotemark?prefs_common.fw_quotemark:"");
	if (prefs_common.fw_quotefmt)
		pref_set_textview_from_pref(GTK_TEXT_VIEW(prefs_quote->text_fw_quotefmt),
				prefs_common.fw_quotefmt);
	else
		prefs_quote_set_default_forward_fmt();

	prefs_quote->window		= GTK_WIDGET(window);
	prefs_quote->page.widget = vbox;
}

static void prefs_quote_save(PrefsPage *_page)
{
	QuotePage *page = (QuotePage *) _page;
	
	g_free(prefs_common.compose_subject_format); 
	prefs_common.compose_subject_format = NULL;
	g_free(prefs_common.compose_body_format); 
	prefs_common.compose_body_format = NULL;
	g_free(prefs_common.quotefmt); 
	prefs_common.quotefmt = NULL;
	g_free(prefs_common.fw_quotefmt); 
	prefs_common.fw_quotefmt = NULL;
	g_free(prefs_common.quotemark); 
	prefs_common.quotemark = NULL;
	g_free(prefs_common.fw_quotemark); 
	prefs_common.fw_quotemark = NULL;
	
	prefs_common.compose_with_format =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_compose_with_format));
	prefs_common.compose_subject_format = pref_get_pref_from_entry(
			GTK_ENTRY(page->entry_subject));
	prefs_common.compose_body_format = pref_get_pref_from_textview(
			GTK_TEXT_VIEW(page->text_format));
	quotefmt_check_new_msg_formats(prefs_common.compose_with_format,
								   NULL,
								   prefs_common.compose_subject_format,
								   prefs_common.compose_body_format);

	prefs_common.quotemark = gtk_editable_get_chars(
			GTK_EDITABLE(page->entry_quotemark), 0, -1);
	prefs_common.quotefmt = pref_get_pref_from_textview(
			GTK_TEXT_VIEW(page->text_quotefmt));
	quotefmt_check_reply_formats(TRUE,
								 NULL,
								 prefs_common.quotemark,
								 prefs_common.quotefmt);

	prefs_common.fw_quotemark = gtk_editable_get_chars(
			GTK_EDITABLE(page->entry_fw_quotemark), 0, -1);
	prefs_common.fw_quotefmt = pref_get_pref_from_textview(
			GTK_TEXT_VIEW(page->text_fw_quotefmt));
	quotefmt_check_forward_formats(TRUE,
								   NULL,
								   prefs_common.fw_quotemark,
								   prefs_common.fw_quotefmt);
}

static void prefs_quote_destroy_widget(PrefsPage *_page)
{
}

void prefs_quote_init(void)
{
	QuotePage *page;
	static gchar *path[3];

	path[0] = _("Compose");
	path[1] = _("Templates");
	path[2] = NULL;

	page = g_new0(QuotePage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_quote_create_widget;
	page->page.destroy_widget = prefs_quote_destroy_widget;
	page->page.save_page = prefs_quote_save;
	page->page.weight = 185.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_quote = page;
}

void prefs_quote_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_quote);
	g_free(prefs_quote);
}
