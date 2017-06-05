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
#include "prefs_display_header.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"

#include "manage_window.h"

typedef struct _MessagePage
{
	PrefsPage page;

	GtkWidget *window;

	GtkWidget *checkbtn_disphdrpane;
	GtkWidget *checkbtn_disphdr;
	GtkWidget *checkbtn_dispxface;

	GtkWidget *checkbtn_html;
	GtkWidget *checkbtn_html_plugin;
	GtkWidget *checkbtn_promote_html_part;
	GtkWidget *spinbtn_linespc;

	GtkWidget *checkbtn_smoothscroll;
	GtkWidget *spinbtn_scrollstep;
	GtkWidget *checkbtn_halfpage;
	GtkWidget *checkbtn_hide_quoted;

	GtkWidget *checkbtn_attach_desc;
	GtkWidget *entry_quote_chars;
} MessagePage;

static void disphdr_pane_toggled(GtkToggleButton *toggle_btn, GtkWidget *widget)
{
	gboolean is_active;

	is_active = gtk_toggle_button_get_active(toggle_btn);

	gtk_widget_set_sensitive(widget, !is_active);
}

static void prefs_message_create_widget(PrefsPage *_page, GtkWindow *window, 
			       	  gpointer data)
{
	MessagePage *prefs_message = (MessagePage *) _page;
	
	GtkWidget *vbox1;
	GtkWidget *vbox2;
	GtkWidget *hbox1;
	GtkWidget *checkbtn_disphdrpane;
	GtkWidget *checkbtn_disphdr;
	GtkWidget *checkbtn_dispxface;

	GtkWidget *button_edit_disphdr;
	GtkWidget *checkbtn_html;
	GtkWidget *checkbtn_html_plugin;
	GtkWidget *checkbtn_promote_html_part;
	GtkWidget *hbox_linespc;
	GtkWidget *label_linespc;
	GtkAdjustment *spinbtn_linespc_adj;
	GtkWidget *spinbtn_linespc;

	GtkWidget *frame;
	GtkWidget *vbox_scr;
	GtkWidget *checkbtn_smoothscroll;
	GtkWidget *hbox_scr;
	GtkWidget *label_scr;
	GtkAdjustment *spinbtn_scrollstep_adj;
	GtkWidget *spinbtn_scrollstep;
	GtkWidget *checkbtn_halfpage;
	GtkWidget *checkbtn_hide_quoted;

	GtkWidget *checkbtn_attach_desc;
	
	GtkWidget *frame_quote;
	GtkWidget *hbox2;
	GtkWidget *vbox_quote;
	GtkWidget *entry_quote_chars;
	GtkWidget *label_quote_chars;

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	vbox2 = gtkut_get_options_frame(vbox1, &frame, _("Headers"));

	PACK_CHECK_BUTTON(vbox2, checkbtn_disphdrpane,
			  _("Display header pane above message view"));

#if HAVE_LIBCOMPFACE
	PACK_CHECK_BUTTON(vbox2, checkbtn_dispxface,
			  _("Display (X-)Face in message view"));
#else
	PACK_CHECK_BUTTON(vbox2, checkbtn_dispxface,
			  _("Display Face in message view"));
#endif

	gtk_widget_set_sensitive(checkbtn_dispxface, 
		!prefs_common.display_header_pane);

	g_signal_connect(G_OBJECT(checkbtn_disphdrpane), "toggled",
			 G_CALLBACK(disphdr_pane_toggled), checkbtn_dispxface);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox1, FALSE, TRUE, 0);

	PACK_CHECK_BUTTON(hbox1, checkbtn_disphdr,
			  _("Display headers in message view"));

	button_edit_disphdr = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	gtk_widget_show (button_edit_disphdr);
	gtk_box_pack_start (GTK_BOX (hbox1), button_edit_disphdr,
			  FALSE, TRUE, 0);
	g_signal_connect (G_OBJECT (button_edit_disphdr), "clicked",
			  G_CALLBACK (prefs_display_header_open),
			  NULL);

	SET_TOGGLE_SENSITIVITY(checkbtn_disphdr, button_edit_disphdr);

	vbox2 = gtkut_get_options_frame(vbox1, &frame, _("HTML messages"));

	PACK_CHECK_BUTTON(vbox2, checkbtn_html,
			  _("Render HTML messages as text"));

	PACK_CHECK_BUTTON(vbox2, checkbtn_html_plugin,
			  _("Render HTML-only messages with plugin if possible"));
	
	PACK_CHECK_BUTTON(vbox2, checkbtn_promote_html_part,
			  _("Select the HTML part of multipart/alternative messages"));

	hbox1 = gtk_hbox_new (FALSE, 32);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, TRUE, 0);

	hbox_linespc = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (hbox1), hbox_linespc, FALSE, TRUE, 0);

	label_linespc = gtk_label_new (_("Line space"));
	gtk_widget_show (label_linespc);
	gtk_box_pack_start (GTK_BOX (hbox_linespc), label_linespc,
			    FALSE, FALSE, 0);

	spinbtn_linespc_adj = GTK_ADJUSTMENT(gtk_adjustment_new (2, 0, 16, 1, 1, 0));
	spinbtn_linespc = gtk_spin_button_new
		(GTK_ADJUSTMENT (spinbtn_linespc_adj), 1, 0);
	gtk_widget_show (spinbtn_linespc);
	gtk_box_pack_start (GTK_BOX (hbox_linespc), spinbtn_linespc,
			    FALSE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_linespc, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_linespc), TRUE);

	label_linespc = gtk_label_new (_("pixel(s)"));
	gtk_widget_show (label_linespc);
	gtk_box_pack_start (GTK_BOX (hbox_linespc), label_linespc,
			    FALSE, FALSE, 0);
	gtk_widget_show_all (hbox1);

	vbox_scr = gtkut_get_options_frame(vbox1, &frame, _("Scroll"));

	PACK_CHECK_BUTTON(vbox_scr, checkbtn_halfpage, _("Half page"));

	hbox1 = gtk_hbox_new (FALSE, 32);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox_scr), hbox1, FALSE, TRUE, 0);

	PACK_CHECK_BUTTON(hbox1, checkbtn_smoothscroll, _("Smooth scroll"));

	hbox_scr = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox_scr);
	gtk_box_pack_start (GTK_BOX (hbox1), hbox_scr, FALSE, FALSE, 0);

	label_scr = gtk_label_new (_("Step"));
	gtk_widget_show (label_scr);
	gtk_box_pack_start (GTK_BOX (hbox_scr), label_scr, FALSE, FALSE, 0);

	spinbtn_scrollstep_adj = GTK_ADJUSTMENT(gtk_adjustment_new (1, 1, 100, 1, 10, 0));
	spinbtn_scrollstep = gtk_spin_button_new
		(GTK_ADJUSTMENT (spinbtn_scrollstep_adj), 1, 0);
	gtk_widget_show (spinbtn_scrollstep);
	gtk_box_pack_start (GTK_BOX (hbox_scr), spinbtn_scrollstep,
			    FALSE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_scrollstep, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_scrollstep),
				     TRUE);

	label_scr = gtk_label_new (_("pixel(s)"));
	gtk_widget_show (label_scr);
	gtk_box_pack_start (GTK_BOX (hbox_scr), label_scr, FALSE, FALSE, 0);

	SET_TOGGLE_SENSITIVITY (checkbtn_smoothscroll, hbox_scr)

	PACK_CHECK_BUTTON(vbox1, checkbtn_attach_desc,
			  _("Show attachment descriptions (rather than names)"));

	/* quote chars */
	PACK_FRAME (vbox1, frame_quote, _("Quotation"));

	vbox_quote = gtk_vbox_new (FALSE, VSPACING_NARROW);
	gtk_widget_show (vbox_quote);
	gtk_container_add (GTK_CONTAINER (frame_quote), vbox_quote);
	gtk_container_set_border_width (GTK_CONTAINER (vbox_quote), 8);

	hbox1 = gtk_hbox_new (FALSE, 32);
	gtk_widget_show (hbox1);
	PACK_CHECK_BUTTON(vbox_quote, checkbtn_hide_quoted, _("Collapse quoted text on double click"));
	gtk_box_pack_start (GTK_BOX (vbox_quote), hbox1, FALSE, FALSE, 0);

	hbox2 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox2);
	gtk_box_pack_start (GTK_BOX (hbox1), hbox2, FALSE, FALSE, 0);

	label_quote_chars = gtk_label_new (_("Treat these characters as quotation marks: "));
	gtk_widget_show (label_quote_chars);
	gtk_box_pack_start (GTK_BOX (hbox2), label_quote_chars, FALSE, FALSE, 0);

	entry_quote_chars = gtk_entry_new ();
	gtk_widget_show (entry_quote_chars);
	gtk_box_pack_start (GTK_BOX (hbox2), entry_quote_chars,
			    FALSE, FALSE, 0);
	gtk_widget_set_size_request (entry_quote_chars, 64, -1);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_disphdrpane),
		prefs_common.display_header_pane);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_dispxface),
		prefs_common.display_xface);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_disphdr),
		prefs_common.display_header);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_html),
		prefs_common.render_html);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_html_plugin),
		prefs_common.invoke_plugin_on_html);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_promote_html_part),
		prefs_common.promote_html_part);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_smoothscroll),
		prefs_common.enable_smooth_scroll);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_hide_quoted),
		prefs_common.hide_quoted);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_halfpage),
		prefs_common.scroll_halfpage);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_attach_desc),
		prefs_common.attach_desc);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spinbtn_linespc),
		prefs_common.line_space);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spinbtn_scrollstep),
		prefs_common.scroll_step);
	gtk_entry_set_text(GTK_ENTRY(entry_quote_chars), 
			prefs_common.quote_chars?prefs_common.quote_chars:"");
		
	prefs_message->window = GTK_WIDGET(window);
	prefs_message->checkbtn_disphdrpane = checkbtn_disphdrpane;
	prefs_message->checkbtn_dispxface = checkbtn_dispxface;
	prefs_message->checkbtn_disphdr = checkbtn_disphdr;
	prefs_message->checkbtn_html = checkbtn_html;
	prefs_message->checkbtn_html_plugin = checkbtn_html_plugin;
	prefs_message->checkbtn_promote_html_part = checkbtn_promote_html_part;
	prefs_message->spinbtn_linespc = spinbtn_linespc;
	prefs_message->checkbtn_smoothscroll = checkbtn_smoothscroll;
	prefs_message->checkbtn_hide_quoted = checkbtn_hide_quoted;
	prefs_message->spinbtn_scrollstep = spinbtn_scrollstep;
	prefs_message->checkbtn_halfpage = checkbtn_halfpage;
	prefs_message->checkbtn_attach_desc = checkbtn_attach_desc;
	prefs_message->entry_quote_chars = entry_quote_chars;
	
	prefs_message->page.widget = vbox1;
}

static void prefs_message_save(PrefsPage *_page)
{
	MessagePage *page = (MessagePage *) _page;

	prefs_common.display_header_pane = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_disphdrpane));
	prefs_common.display_xface = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_dispxface));
	prefs_common.display_header = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_disphdr));
	prefs_common.render_html = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_html));
	prefs_common.invoke_plugin_on_html = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_html_plugin));
	prefs_common.promote_html_part = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_promote_html_part));
	prefs_common.enable_smooth_scroll = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_smoothscroll));
	prefs_common.scroll_halfpage = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_halfpage));
	prefs_common.hide_quoted = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_hide_quoted));
	prefs_common.attach_desc = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_attach_desc));
	prefs_common.line_space = gtk_spin_button_get_value_as_int(
		GTK_SPIN_BUTTON(page->spinbtn_linespc));
	prefs_common.scroll_step = gtk_spin_button_get_value_as_int(
		GTK_SPIN_BUTTON(page->spinbtn_scrollstep));

	g_free(prefs_common.quote_chars); 
	prefs_common.quote_chars = gtk_editable_get_chars(
			GTK_EDITABLE(page->entry_quote_chars), 0, -1);
	remove_space(prefs_common.quote_chars);

	main_window_reflect_prefs_all_real(FALSE);
}

static void prefs_message_destroy_widget(PrefsPage *_page)
{
}

MessagePage *prefs_message;

void prefs_message_init(void)
{
	MessagePage *page;
	static gchar *path[3];

	path[0] = _("Message View");
	path[1] = _("Text Options");
	path[2] = NULL;

	page = g_new0(MessagePage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_message_create_widget;
	page->page.destroy_widget = prefs_message_destroy_widget;
	page->page.save_page = prefs_message_save;
	page->page.weight = 170.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_message = page;
}

void prefs_message_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_message);
	g_free(prefs_message);
}
