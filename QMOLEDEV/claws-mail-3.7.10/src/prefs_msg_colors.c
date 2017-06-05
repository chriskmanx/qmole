/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2004-2011 Hiroyuki Yamamoto & The Claws Mail Team
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

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "prefs_common.h"
#include "prefs_gtk.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"

#include "manage_window.h"
#include "mainwindow.h"
#include "colorlabel.h"

#define SAFE_STRING(str) \
	(str) ? (str) : ""

static struct MessageColorButtons {
	GtkWidget *btn_quote_level1;
	GtkWidget *btn_quote_level2;
	GtkWidget *btn_quote_level3;
	GtkWidget *btn_quote_level1_bg;
	GtkWidget *btn_quote_level2_bg;
	GtkWidget *btn_quote_level3_bg;
	GtkWidget *btn_uri;
	GtkWidget *btn_tgt_folder;
	GtkWidget *btn_signature;
	GtkWidget *btn_color_new;
	/* custom colors */
	GtkWidget *custom_color[COLORLABELS];
} color_buttons;

typedef struct _MsgColorsPage
{
	PrefsPage page;

	GtkWidget *window;
	
	GtkWidget *checkbtn_enable_colors;
	GtkWidget *checkbtn_enable_bgcolors;
	GtkWidget *checkbtn_recycle_colors;

	/* custom colors */
	GtkWidget *entry_custom_colorlabel[COLORLABELS];
} MsgColorsPage;

static GtkWidget *color_dialog;

static void quote_color_set_dialog		(GtkWidget	*widget,
						 gpointer	 data);
static void quote_colors_set_dialog_ok		(GtkWidget	*widget,
						 gpointer	 data);
static void quote_colors_set_dialog_cancel	(GtkWidget	*widget,
						 gpointer	 data);
static gboolean quote_colors_set_dialog_key_pressed	(GtkWidget	*widget,
						 GdkEventKey	*event,
						 gpointer	 data);
static void set_button_bg_color			(GtkWidget	*widget,
						 gint		 color);
static void prefs_msg_colors_reset_custom_colors(GtkWidget *widget,
						 gpointer	 data);

static void prefs_msg_colors_create_widget(PrefsPage *_page, GtkWindow *window, 
			       	    gpointer data)
{
	MsgColorsPage *prefs_msg_colors = (MsgColorsPage *) _page;
	
	GtkWidget *notebook;
	GtkWidget *vbox1;
	GtkWidget *vbox2;
	GtkWidget *checkbtn_enable_colors;
	GtkWidget *label_quote_level1;
	GtkWidget *label_quote_level2;
	GtkWidget *label_quote_level3;
	GtkWidget *label_quote_color1;
	GtkWidget *label_quote_color2;
	GtkWidget *label_quote_color3;
	GtkWidget *checkbtn_enable_bgcolors;
	GtkWidget *label_quote_bgcolor1;
	GtkWidget *label_quote_bgcolor2;
	GtkWidget *label_quote_bgcolor3;
	GtkWidget *lable_uri;
	GtkWidget *label_signature;
	GtkWidget *label_tgt_folder;
	GtkWidget *checkbtn_recycle_colors;
	GtkWidget *hbox;
	GtkWidget *label_color_new;
	GtkWidget *frame_msg;
	GtkWidget *frame_folder;
	GtkWidget *frame_quote;
	GtkWidget *vbox3;
	GtkWidget *hbox_quote;
	GtkWidget *vbox_quotefg;
	GtkWidget *vbox_quotebg;
	/* custom colors */
	GtkWidget *hbox_custom_colors;
	GtkWidget *vbox_custom_colors;
	GtkWidget *vbox_custom_colors1;
	GtkWidget *vbox_custom_colors2;
 	GtkWidget *hbox_reset_custom_colors;
	GtkWidget *btn_reset_custom_colors;
	GtkWidget *hbox_custom_color[COLORLABELS];
	GtkWidget *entry_custom_colorlabel[COLORLABELS];
	gint c;
	gchar *tooltip_btn_text = NULL;
	gchar *tooltip_entry_text = NULL;
	CLAWS_TIP_DECL();

	notebook = gtk_notebook_new();
	gtk_widget_show(notebook);
	
	vbox1 = gtk_vbox_new (FALSE, VBOX_BORDER);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox1,
				 gtk_label_new(_("Other")));

	vbox2 = gtkut_get_options_frame(vbox1, &frame_msg, _("Message view"));

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);

	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);
	PACK_CHECK_BUTTON (hbox, checkbtn_enable_colors,
			   _("Enable coloration of message text"));	

	hbox_quote = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox_quote);
	vbox_quotefg = gtk_vbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (vbox_quotefg);
	vbox_quotebg = gtk_vbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (vbox_quotebg);
	vbox3 = gtkut_get_options_frame(vbox2, &frame_quote, _("Quote"));
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, frame_quote);
	
	gtk_box_pack_start (GTK_BOX (vbox3), hbox_quote, FALSE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (hbox_quote), vbox_quotefg, FALSE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (hbox_quote), vbox_quotebg, FALSE, TRUE, 0);

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotefg), hbox, FALSE, TRUE, 0);

	PACK_CHECK_BUTTON (hbox, checkbtn_recycle_colors,
			   _("Cycle quote colors"));
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, checkbtn_recycle_colors);

	CLAWS_SET_TIP(checkbtn_recycle_colors,
			     _("If there are more than 3 quote levels, the colors will be reused"));

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotefg), hbox, FALSE, TRUE, 0);

	label_quote_level1 = gtk_label_new (_("1st Level"));
	gtk_widget_show(label_quote_level1);
  	gtk_box_pack_start (GTK_BOX(hbox), label_quote_level1, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_quote_level1);

	label_quote_color1 = gtk_label_new (_("Text"));
	gtk_widget_show(label_quote_color1);
  	gtk_box_pack_end (GTK_BOX(hbox), label_quote_color1, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_quote_color1);
		
	color_buttons.btn_quote_level1 = gtk_button_new();
	gtk_widget_show(color_buttons.btn_quote_level1);
	gtk_widget_set_size_request (color_buttons.btn_quote_level1, 30, 20);
  	gtk_box_pack_end (GTK_BOX(hbox), color_buttons.btn_quote_level1, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, color_buttons.btn_quote_level1);

	CLAWS_SET_TIP(color_buttons.btn_quote_level1,
			     Q_("Tooltip|Pick color for 1st level text"));

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotefg), hbox, FALSE, TRUE, 0);

	label_quote_level2 = gtk_label_new (_("2nd Level"));
	gtk_widget_show(label_quote_level2);
  	gtk_box_pack_start (GTK_BOX(hbox), label_quote_level2, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_quote_level2);

	label_quote_color2 = gtk_label_new (_("Text"));
	gtk_widget_show(label_quote_color2);
  	gtk_box_pack_end (GTK_BOX(hbox), label_quote_color2, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_quote_color2);

	color_buttons.btn_quote_level2 = gtk_button_new();
	gtk_widget_show(color_buttons.btn_quote_level2);
	gtk_widget_set_size_request (color_buttons.btn_quote_level2, 30, 20);
  	gtk_box_pack_end (GTK_BOX(hbox), color_buttons.btn_quote_level2, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, color_buttons.btn_quote_level2);

	CLAWS_SET_TIP(color_buttons.btn_quote_level2,
			     Q_("Tooltip|Pick color for 2nd level text"));

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotefg), hbox, FALSE, TRUE, 0);

	label_quote_level3 = gtk_label_new (_("3rd Level"));
	gtk_widget_show(label_quote_level3);
  	gtk_box_pack_start (GTK_BOX(hbox), label_quote_level3, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_quote_level3);

	label_quote_color3 = gtk_label_new (_("Text"));
	gtk_widget_show(label_quote_color3);
  	gtk_box_pack_end (GTK_BOX(hbox), label_quote_color3, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_quote_color3);

	color_buttons.btn_quote_level3 = gtk_button_new();
	gtk_widget_show(color_buttons.btn_quote_level3);
	gtk_widget_set_size_request (color_buttons.btn_quote_level3, 30, 20);
  	gtk_box_pack_end (GTK_BOX(hbox), color_buttons.btn_quote_level3, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, color_buttons.btn_quote_level3);

	CLAWS_SET_TIP(color_buttons.btn_quote_level3,
			     Q_("Tooltip|Pick color for 3rd level text"));

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotebg), hbox, FALSE, TRUE, 0);

	PACK_CHECK_BUTTON (hbox, checkbtn_enable_bgcolors,
			   _("Enable coloration of text background"));
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, checkbtn_enable_bgcolors);

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotebg), hbox, FALSE, TRUE, 0);
	
	color_buttons.btn_quote_level1_bg = gtk_button_new();
	gtk_widget_show(color_buttons.btn_quote_level1_bg);
	gtk_widget_set_size_request (color_buttons.btn_quote_level1_bg, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_quote_level1_bg, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_bgcolors, color_buttons.btn_quote_level1_bg);

	CLAWS_SET_TIP(color_buttons.btn_quote_level1_bg,
			     Q_("Tooltip|Pick color for 1st level text background"));

	label_quote_bgcolor1 = gtk_label_new (_("Background"));
	gtk_widget_show(label_quote_bgcolor1);
  	gtk_box_pack_start (GTK_BOX(hbox), label_quote_bgcolor1, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_bgcolors, label_quote_bgcolor1);

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotebg), hbox, FALSE, TRUE, 0);
	
	color_buttons.btn_quote_level2_bg = gtk_button_new();
	gtk_widget_show(color_buttons.btn_quote_level2_bg);
	gtk_widget_set_size_request (color_buttons.btn_quote_level2_bg, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_quote_level2_bg, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_bgcolors, color_buttons.btn_quote_level2_bg);

	CLAWS_SET_TIP(color_buttons.btn_quote_level2_bg,
			     Q_("Tooltip|Pick color for 2nd level text background"));

	label_quote_bgcolor2 = gtk_label_new (_("Background"));
	gtk_widget_show(label_quote_bgcolor2);
  	gtk_box_pack_start (GTK_BOX(hbox), label_quote_bgcolor2, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_bgcolors, label_quote_bgcolor2);
	
	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, hbox);
	gtk_box_pack_start (GTK_BOX (vbox_quotebg), hbox, FALSE, TRUE, 0);
	
	color_buttons.btn_quote_level3_bg = gtk_button_new();
	gtk_widget_show(color_buttons.btn_quote_level3_bg);
	gtk_widget_set_size_request (color_buttons.btn_quote_level3_bg, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_quote_level3_bg, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_bgcolors, color_buttons.btn_quote_level3_bg);

	CLAWS_SET_TIP(color_buttons.btn_quote_level3_bg,
			     Q_("Tooltip|Pick color for 3rd level text background"));

	label_quote_bgcolor3 = gtk_label_new (_("Background"));
	gtk_widget_show(label_quote_bgcolor3);
  	gtk_box_pack_start (GTK_BOX(hbox), label_quote_bgcolor3, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_bgcolors, label_quote_bgcolor3);
	
	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);

	color_buttons.btn_uri = gtk_button_new();
	gtk_widget_show(color_buttons.btn_uri);
	gtk_widget_set_size_request (color_buttons.btn_uri, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_uri, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, color_buttons.btn_uri);

	CLAWS_SET_TIP(color_buttons.btn_uri,
			     Q_("Tooltip|Pick color for links"));

	lable_uri = gtk_label_new (_("URI link"));
	gtk_widget_show(lable_uri);
  	gtk_box_pack_start (GTK_BOX(hbox), lable_uri, FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, lable_uri);

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);

	color_buttons.btn_signature = gtk_button_new();
	gtk_widget_show(color_buttons.btn_signature);
	gtk_widget_set_size_request (color_buttons.btn_signature, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_signature, 
			    FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, color_buttons.btn_signature);

	CLAWS_SET_TIP(color_buttons.btn_signature,
			     Q_("Tooltip|Pick color for signatures"));

	label_signature = gtk_label_new (_("Signatures"));
	gtk_widget_show(label_signature);
  	gtk_box_pack_start (GTK_BOX(hbox), label_signature, FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_enable_colors, label_signature);

	vbox2 = gtkut_get_options_frame(vbox1, &frame_folder, _("Folder list"));

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);

	color_buttons.btn_tgt_folder = gtk_button_new();
	gtk_widget_show(color_buttons.btn_tgt_folder);
	gtk_widget_set_size_request (color_buttons.btn_tgt_folder, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_tgt_folder, 
			    FALSE, FALSE, 0);

	CLAWS_SET_TIP(color_buttons.btn_tgt_folder,
			     _("Pick color for Target folder. "
			       "Target folder is used when the option 'Execute immediately "
			       "when moving or deleting messages' is turned off"));

	label_tgt_folder = gtk_label_new (_("Target folder"));
	gtk_widget_show(label_tgt_folder);
  	gtk_box_pack_start (GTK_BOX(hbox), label_tgt_folder, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, FALSE, 0);

	color_buttons.btn_color_new = gtk_button_new();
	gtk_widget_show (color_buttons.btn_color_new);
	gtk_widget_set_size_request (color_buttons.btn_color_new, 30, 20);
  	gtk_box_pack_start (GTK_BOX(hbox), color_buttons.btn_color_new,
			    FALSE, FALSE, 0);

	CLAWS_SET_TIP(color_buttons.btn_color_new,
			     _("Pick color for folders containing new messages"));

	label_color_new = gtk_label_new (_("Folder containing new messages"));
 	gtk_widget_show(label_color_new);
 	gtk_box_pack_start (GTK_BOX(hbox), label_color_new, FALSE, FALSE, 0);

	/* custom colors */
	vbox_custom_colors = gtk_vbox_new (FALSE, VSPACING_NARROW);
	gtk_widget_show (vbox_custom_colors);
	gtk_container_set_border_width (GTK_CONTAINER (vbox_custom_colors), VBOX_BORDER);
	gtk_notebook_prepend_page(GTK_NOTEBOOK(notebook), vbox_custom_colors,
				 gtk_label_new(_("Color labels")));

	hbox_custom_colors = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_custom_colors);
	gtk_box_pack_start(GTK_BOX (vbox_custom_colors), hbox_custom_colors,
				   FALSE, TRUE, 0);

	vbox_custom_colors1 = gtk_vbox_new (FALSE, VSPACING_NARROW);
	gtk_widget_show (vbox_custom_colors1);
	gtk_box_pack_start (GTK_BOX (hbox_custom_colors), vbox_custom_colors1, FALSE, FALSE, 0);

	vbox_custom_colors2 = gtk_vbox_new (FALSE, VSPACING_NARROW);
	gtk_widget_show (vbox_custom_colors2);
	gtk_box_pack_start (GTK_BOX (hbox_custom_colors), vbox_custom_colors2, FALSE, FALSE, 0);

	for (c = 0; c < (COLORLABELS>>1)+(COLORLABELS&1); c++) {
		/* TRANSLATORS: 'color %d' refers to the filtering/processing 
		   rule name and should not be translated */
		tooltip_btn_text = g_strdup_printf(Q_("Tooltip|Pick color for 'color %d'"), c+1);

		/* TRANSLATORS: 'color %d' refers to the filtering/processing 
		   rule name and should not be translated */
		tooltip_entry_text = g_strdup_printf(_("Set label for 'color %d'"), c+1);

		hbox_custom_color[c] = gtk_hbox_new(FALSE, 8);
		gtk_widget_show(hbox_custom_color[c]);
		gtk_box_pack_start(GTK_BOX (vbox_custom_colors1), hbox_custom_color[c],
				   FALSE, TRUE, 0);

		color_buttons.custom_color[c] = gtk_button_new();
		gtk_widget_show(color_buttons.custom_color[c]);
		gtk_widget_set_size_request(color_buttons.custom_color[c], 30, 20);
  		gtk_box_pack_start(GTK_BOX (hbox_custom_color[c]), color_buttons.custom_color[c],
				   FALSE, FALSE, 0);

		CLAWS_SET_TIP(color_buttons.custom_color[c],
			     	     tooltip_btn_text);

		entry_custom_colorlabel[c] = gtk_entry_new();
		gtk_widget_show (entry_custom_colorlabel[c]);
  		gtk_box_pack_start(GTK_BOX (hbox_custom_color[c]), entry_custom_colorlabel[c],
				   FALSE, FALSE, 0);
		CLAWS_SET_TIP(entry_custom_colorlabel[c],
			     	     tooltip_entry_text);
	}

	for (c = (COLORLABELS>>1)+(COLORLABELS&1); c < COLORLABELS; c++) {
		/* TRANSLATORS: 'color %d' refers to the filtering/processing 
		   rule name and should not be translated */
		tooltip_btn_text = g_strdup_printf(Q_("Tooltip|Pick color for 'color %d'"), c+1);

		/* TRANSLATORS: 'color %d' refers to the filtering/processing 
		   rule name and should not be translated */
		tooltip_entry_text = g_strdup_printf(_("Set label for 'color %d'"), c+1);

		hbox_custom_color[c] = gtk_hbox_new(FALSE, 8);
		gtk_widget_show(hbox_custom_color[c]);
		gtk_box_pack_start(GTK_BOX (vbox_custom_colors2), hbox_custom_color[c],
				   FALSE, TRUE, 0);

		color_buttons.custom_color[c] = gtk_button_new();
		gtk_widget_show(color_buttons.custom_color[c]);
		gtk_widget_set_size_request(color_buttons.custom_color[c], 30, 20);
  		gtk_box_pack_start(GTK_BOX (hbox_custom_color[c]), color_buttons.custom_color[c],
				   FALSE, FALSE, 0);
		CLAWS_SET_TIP(color_buttons.custom_color[c],
			     	     tooltip_btn_text);

		entry_custom_colorlabel[c] = gtk_entry_new();
		gtk_widget_show (entry_custom_colorlabel[c]);
  		gtk_box_pack_start(GTK_BOX (hbox_custom_color[c]), entry_custom_colorlabel[c],
				   FALSE, FALSE, 0);
		CLAWS_SET_TIP(entry_custom_colorlabel[c],
			     	     tooltip_entry_text);
	}

	g_free(tooltip_btn_text);
	g_free(tooltip_entry_text);

	hbox_reset_custom_colors = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show (hbox_reset_custom_colors);
	gtk_box_pack_start(GTK_BOX (vbox_custom_colors), hbox_reset_custom_colors,
			   FALSE, FALSE, 0);

	btn_reset_custom_colors = gtk_button_new_with_label(_(" Use default "));
	gtk_widget_show(btn_reset_custom_colors);
	gtk_box_pack_start(GTK_BOX(hbox_reset_custom_colors), btn_reset_custom_colors,
		FALSE, FALSE, 0);

	g_signal_connect(G_OBJECT(color_buttons.btn_quote_level1), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "LEVEL1");
	g_signal_connect(G_OBJECT(color_buttons.btn_quote_level2), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "LEVEL2");
	g_signal_connect(G_OBJECT(color_buttons.btn_quote_level3), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "LEVEL3");
	g_signal_connect(G_OBJECT(color_buttons.btn_quote_level1_bg), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "LEVEL1BG");
	g_signal_connect(G_OBJECT(color_buttons.btn_quote_level2_bg), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "LEVEL2BG");
	g_signal_connect(G_OBJECT(color_buttons.btn_quote_level3_bg), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "LEVEL3BG");
	g_signal_connect(G_OBJECT(color_buttons.btn_uri), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "URI");
	g_signal_connect(G_OBJECT(color_buttons.btn_tgt_folder), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "TGTFLD");
	g_signal_connect(G_OBJECT(color_buttons.btn_signature), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "SIGNATURE");
	g_signal_connect(G_OBJECT(color_buttons.btn_color_new), "clicked",
			 G_CALLBACK(quote_color_set_dialog), "NEW");
	/* custom colors */
	for (c = 0; c < COLORLABELS; c++) {
		g_signal_connect(G_OBJECT(color_buttons.custom_color[c]), "clicked",
				 G_CALLBACK(quote_color_set_dialog), GINT_TO_POINTER(c));
	}

	g_signal_connect(G_OBJECT(btn_reset_custom_colors), "clicked",
			 G_CALLBACK(prefs_msg_colors_reset_custom_colors), prefs_msg_colors);

	set_button_bg_color(color_buttons.btn_quote_level1,
			    prefs_common.quote_level1_col);
	set_button_bg_color(color_buttons.btn_quote_level2,
			    prefs_common.quote_level2_col);
	set_button_bg_color(color_buttons.btn_quote_level3,
			    prefs_common.quote_level3_col);
	set_button_bg_color(color_buttons.btn_quote_level1_bg,
			    prefs_common.quote_level1_bgcol);
	set_button_bg_color(color_buttons.btn_quote_level2_bg,
			    prefs_common.quote_level2_bgcol);
	set_button_bg_color(color_buttons.btn_quote_level3_bg,
			    prefs_common.quote_level3_bgcol);
	set_button_bg_color(color_buttons.btn_uri,
			    prefs_common.uri_col);
	set_button_bg_color(color_buttons.btn_tgt_folder,
			    prefs_common.tgt_folder_col);
	set_button_bg_color(color_buttons.btn_signature,
			    prefs_common.signature_col);
	set_button_bg_color(color_buttons.btn_color_new,
			    prefs_common.color_new);
	/* custom colors */
	for (c = 0; c < COLORLABELS; c++) {
		set_button_bg_color(color_buttons.custom_color[c],
				    prefs_common.custom_colorlabel[c].color);
		gtk_entry_set_text(GTK_ENTRY (entry_custom_colorlabel[c]), 
				   gettext(SAFE_STRING (prefs_common.custom_colorlabel[c].label)));
	}

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_colors),
				     prefs_common.enable_color);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_bgcolors),
				     prefs_common.enable_bgcolor);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_recycle_colors),
				     prefs_common.recycle_quote_colors);

	prefs_msg_colors->checkbtn_enable_colors 	= checkbtn_enable_colors;
	prefs_msg_colors->checkbtn_enable_bgcolors 	= checkbtn_enable_bgcolors;
	prefs_msg_colors->checkbtn_recycle_colors	= checkbtn_recycle_colors;
	/* custom colors */
	for (c = 0; c < COLORLABELS; c++) {
		prefs_msg_colors->entry_custom_colorlabel[c] = entry_custom_colorlabel[c];
	}
	gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 0);
		
	prefs_msg_colors->page.widget = notebook;
}

static void quote_color_set_dialog(GtkWidget *widget, gpointer data)
{
	gchar *type = (gchar *)data;
	gchar *title = NULL;
	GdkColor color;
	gint rgbvalue = 0;
	GtkColorSelectionDialog *dialog;
	gint c;

	/* custom colors */
	/* leave the extra space at the end of the title, this is for translators' convenience */
	for (c = 0; c < COLORLABELS; c++) {
		if (GPOINTER_TO_INT(type) == c) {
			/* TRANSLATORS: 'color %d' refers to the filtering/processing 
			   rule name and should not be translated */
			title = g_strdup_printf(Q_("Dialog title|Pick color for 'color %d'"), c+1);
			rgbvalue = prefs_common.custom_colorlabel[c].color;
			break;
		}
	}
	/* other colors */
	if (c == COLORLABELS) {
		if(g_ascii_strcasecmp(type, "LEVEL1") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for 1st level text"));
			rgbvalue = prefs_common.quote_level1_col;
		} else if(g_ascii_strcasecmp(type, "LEVEL2") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for 2nd level text"));
			rgbvalue = prefs_common.quote_level2_col;
		} else if(g_ascii_strcasecmp(type, "LEVEL3") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for 3rd level text"));
			rgbvalue = prefs_common.quote_level3_col;
		} else if(g_ascii_strcasecmp(type, "LEVEL1BG") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for 1st level text background"));
			rgbvalue = prefs_common.quote_level1_bgcol;
		} else if(g_ascii_strcasecmp(type, "LEVEL2BG") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for 2nd level text background"));
			rgbvalue = prefs_common.quote_level2_bgcol;
		} else if(g_ascii_strcasecmp(type, "LEVEL3BG") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for 3rd level text background"));
			rgbvalue = prefs_common.quote_level3_bgcol;
		} else if(g_ascii_strcasecmp(type, "URI") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for links"));
			rgbvalue = prefs_common.uri_col;
		} else if(g_ascii_strcasecmp(type, "TGTFLD") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for target folder"));
			rgbvalue = prefs_common.tgt_folder_col;
		} else if(g_ascii_strcasecmp(type, "SIGNATURE") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for signatures"));
			rgbvalue = prefs_common.signature_col;
		} else if(g_ascii_strcasecmp(type, "NEW") == 0) {
			title = g_strdup(Q_("Dialog title|Pick color for folder"));
			rgbvalue = prefs_common.color_new;
		} else {
			/* Should never be called */
			g_warning("Unrecognized datatype '%s' in quote_color_set_dialog\n", type);
			return;
		}
	}

	color_dialog = gtk_color_selection_dialog_new(title);
	g_free(title);
	gtk_window_set_position(GTK_WINDOW(color_dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(color_dialog), TRUE);
	gtk_window_set_resizable(GTK_WINDOW(color_dialog), FALSE);
	manage_window_set_transient(GTK_WINDOW(color_dialog));

	g_signal_connect(G_OBJECT(GTK_COLOR_SELECTION_DIALOG(color_dialog)->cancel_button),
			 "clicked", G_CALLBACK(quote_colors_set_dialog_cancel), data);
	g_signal_connect(G_OBJECT(GTK_COLOR_SELECTION_DIALOG(color_dialog)->ok_button),
			 "clicked", G_CALLBACK(quote_colors_set_dialog_ok), data);
	g_signal_connect(G_OBJECT(color_dialog), "key_press_event",
			 G_CALLBACK(quote_colors_set_dialog_key_pressed),data);

	/* preselect the previous color in the color selection dialog */

	gtkut_convert_int_to_gdk_color(rgbvalue, &color);

	dialog = GTK_COLOR_SELECTION_DIALOG(color_dialog);
	gtk_color_selection_set_current_color
		(GTK_COLOR_SELECTION(dialog->colorsel), &color);

	gtk_widget_show(color_dialog);
}

static void quote_colors_set_dialog_ok(GtkWidget *widget, gpointer data)
{
	GtkColorSelection *colorsel = (GtkColorSelection *)
						((GtkColorSelectionDialog *)color_dialog)->colorsel;
	GdkColor color;
	gint rgbvalue;
	gchar *type = (gchar *)data;
	gint c;

	gtk_color_selection_get_current_color(colorsel, &color);
	rgbvalue = gtkut_convert_gdk_color_to_int(&color);


	/* custom colors */
	for (c = 0; c < COLORLABELS; c++) {
		if (GPOINTER_TO_INT(type) == c) {
			prefs_common.custom_colorlabel[c].color = rgbvalue;
			set_button_bg_color(color_buttons.custom_color[c], rgbvalue);
			break;
		}
	}
	/* other colors */
	if (c == COLORLABELS) {
		if (g_ascii_strcasecmp(type, "LEVEL1") == 0) {
			prefs_common.quote_level1_col = rgbvalue;
			set_button_bg_color(color_buttons.btn_quote_level1, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "LEVEL2") == 0) {
			prefs_common.quote_level2_col = rgbvalue;
			set_button_bg_color(color_buttons.btn_quote_level2, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "LEVEL3") == 0) {
			prefs_common.quote_level3_col = rgbvalue;
			set_button_bg_color(color_buttons.btn_quote_level3, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "LEVEL1BG") == 0) {
			prefs_common.quote_level1_bgcol = rgbvalue;
			set_button_bg_color(color_buttons.btn_quote_level1_bg, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "LEVEL2BG") == 0) {
			prefs_common.quote_level2_bgcol = rgbvalue;
			set_button_bg_color(color_buttons.btn_quote_level2_bg, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "LEVEL3BG") == 0) {
			prefs_common.quote_level3_bgcol = rgbvalue;
			set_button_bg_color(color_buttons.btn_quote_level3_bg, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "URI") == 0) {
			prefs_common.uri_col = rgbvalue;
			set_button_bg_color(color_buttons.btn_uri, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "TGTFLD") == 0) {
			prefs_common.tgt_folder_col = rgbvalue;
			set_button_bg_color(color_buttons.btn_tgt_folder, rgbvalue);
			folderview_set_target_folder_color(prefs_common.tgt_folder_col);
		} else if (g_ascii_strcasecmp(type, "SIGNATURE") == 0) {
			prefs_common.signature_col = rgbvalue;
			set_button_bg_color(color_buttons.btn_signature, rgbvalue);
		} else if (g_ascii_strcasecmp(type, "NEW") == 0) {
			prefs_common.color_new = rgbvalue;
			set_button_bg_color(color_buttons.btn_color_new, rgbvalue);
		} else {
			g_printerr("Unrecognized datatype '%s' in quote_color_set_dialog_ok\n", type);
		}
	}

	gtk_widget_destroy(color_dialog);
}

static void quote_colors_set_dialog_cancel(GtkWidget *widget, gpointer data)
{
	gtk_widget_destroy(color_dialog);
}

static gboolean quote_colors_set_dialog_key_pressed(GtkWidget *widget,
						GdkEventKey *event,
						gpointer data)
{
	if (event) {
		switch (event->keyval) {
			case GDK_Escape:
				gtk_button_clicked(GTK_BUTTON(GTK_COLOR_SELECTION_DIALOG
							(widget)->cancel_button));
				return TRUE;
			case GDK_Return: 
			case GDK_KP_Enter:
				/* NOTE: changing focus makes widget accept all currently 
				 * changed settings! */
				gtk_widget_grab_focus
					(GTK_COLOR_SELECTION_DIALOG
						(widget)->ok_button);
				/* call ok handler */						
				gtk_button_clicked(GTK_BUTTON
					(GTK_COLOR_SELECTION_DIALOG
						(widget)->ok_button));
				return TRUE;
			default:
				break;
		}
	}
	return FALSE;
}

static void set_button_bg_color(GtkWidget *widget, gint rgbvalue)
{
	GtkStyle *newstyle;
	GdkColor color;

	gtkut_convert_int_to_gdk_color(rgbvalue, &color);
	newstyle = gtk_style_copy(gtk_widget_get_default_style());
	newstyle->bg[GTK_STATE_NORMAL]   = color;
	newstyle->bg[GTK_STATE_PRELIGHT] = color;
	newstyle->bg[GTK_STATE_ACTIVE]   = color;

	gtk_widget_set_style(GTK_WIDGET(widget), newstyle);
}

static void prefs_msg_colors_save(PrefsPage *_page)
{
	MsgColorsPage *page = (MsgColorsPage *) _page;
	gint c;

	prefs_common.enable_color = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_enable_colors));
	prefs_common.enable_bgcolor = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_enable_bgcolors));
	prefs_common.recycle_quote_colors =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_recycle_colors));

	/* custom colors */
	for (c = 0; c < COLORLABELS; c++) {
		g_free(prefs_common.custom_colorlabel[c].label);
		prefs_common.custom_colorlabel[c].label =
			gtk_editable_get_chars(GTK_EDITABLE(page->entry_custom_colorlabel[c]), 0, -1);
	}
	colorlabel_update_colortable_from_prefs();

	main_window_reflect_prefs_all();
	main_window_reflect_prefs_custom_colors(mainwindow_get_mainwindow());
}

static void prefs_msg_colors_reset_custom_colors(GtkWidget *widget, gpointer data)
{
#define CL(x)		(((gulong) (x) >> (gulong) 8) & 0xFFUL)	
#define CR(r, g, b)	((CL(r) << (gulong) 16) | \
			 (CL(g) << (gulong)  8) | \
			 (CL(b)))
	MsgColorsPage *page = (MsgColorsPage *) data;
	GdkColor color;
	gint c;

	for (c = 0; c < COLORLABELS; c++) {
		color = colorlabel_get_default_color(c);
		prefs_common.custom_colorlabel[c].color =
							(gint)CR(color.red, color.green, color.blue);
		set_button_bg_color(color_buttons.custom_color[c],
							prefs_common.custom_colorlabel[c].color);
		gtk_entry_set_text(GTK_ENTRY (page->entry_custom_colorlabel[c]),
							gettext(SAFE_STRING (colorlabel_get_color_default_text(c))));
	}

#undef CR
#undef CL
}

static void prefs_msg_colors_destroy_widget(PrefsPage *_page)
{
}

MsgColorsPage *prefs_msg_colors;

void prefs_msg_colors_init(void)
{
	MsgColorsPage *page;
	static gchar *path[3];

	path[0] = _("Display");
	path[1] = _("Colors");
	path[2] = NULL;

	page = g_new0(MsgColorsPage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_msg_colors_create_widget;
	page->page.destroy_widget = prefs_msg_colors_destroy_widget;
	page->page.save_page = prefs_msg_colors_save;
	page->page.weight = 165.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_msg_colors = page;
}

void prefs_msg_colors_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_msg_colors);
	g_free(prefs_msg_colors);
}

