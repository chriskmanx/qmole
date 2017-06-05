/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 Hiroyuki Yamamoto & the Claws Mail team
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

#if USE_ENCHANT

#include "defs.h"

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "utils.h"
#include "prefs_common.h"
#include "prefs_gtk.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"
#include "gtk/filesel.h"
#include "gtk/colorsel.h"
#include "gtk/combobox.h"

typedef struct _SpellingPage
{
	PrefsPage page;

	GtkWidget *window;		/* do not modify */

	GtkWidget *automatic_frame;
	GtkWidget *dictionary_frame;
	
	GtkWidget *enable_aspell_checkbtn;
	GtkWidget *recheck_when_changing_dict_checkbtn;
	GtkWidget *check_while_typing_checkbtn;
	GtkWidget *use_alternate_checkbtn;

	GtkWidget *default_dict_label;
	GtkWidget *default_dict_combo;

	GtkWidget *default_alt_dict_label;
	GtkWidget *default_alt_dict_combo;

	GtkWidget *both_dict_check;

	GtkWidget *misspelled_label;
	GtkWidget *misspelled_colorbtn;
	GtkWidget *misspelled_useblack_label;

	gint	   misspell_col;
} SpellingPage;

static void prefs_spelling_colorsel(GtkWidget *widget,
				    gpointer data)
{
	SpellingPage *spelling = (SpellingPage *) data;
	gint rgbcolor;

	rgbcolor = colorsel_select_color_rgb(_("Pick color for misspelled word"), 
					     spelling->misspell_col);
	gtkut_set_widget_bgcolor_rgb(spelling->misspelled_colorbtn, rgbcolor);
	spelling->misspell_col = rgbcolor;
}

#define SAFE_STRING(str) \
	(str) ? (str) : ""

static void prefs_spelling_create_widget(PrefsPage *_page, GtkWindow *window, gpointer data)
{
	SpellingPage *prefs_spelling = (SpellingPage *) _page;

	GtkWidget *vbox1, *vbox2;

	GtkWidget *enable_aspell_checkbtn;
	GtkWidget *check_while_typing_checkbtn;
	GtkWidget *recheck_when_changing_dict_checkbtn;
	GtkWidget *use_alternate_checkbtn;

	GtkWidget *automatic_frame;
	GtkWidget *dictionary_frame;

	GtkWidget *table;

	GtkWidget *default_dict_label;
	GtkWidget *default_dict_combo;

	GtkWidget *default_alt_dict_label;
	GtkWidget *default_alt_dict_combo;
	GtkWidget *both_dict_check;
#ifdef WIN32
	GtkWidget *get_dictionaries_btn;
#endif

	GtkWidget *misspelled_label;
	GtkWidget *misspelled_hbox;
	GtkWidget *misspelled_colorbtn;

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (vbox1), vbox2, FALSE, FALSE, 0);

	enable_aspell_checkbtn = gtk_check_button_new_with_label(
			_("Enable spell checker"));
	gtk_widget_show(enable_aspell_checkbtn);
	gtk_box_pack_start(GTK_BOX(vbox2), enable_aspell_checkbtn, TRUE, TRUE, 0);

	use_alternate_checkbtn = gtk_check_button_new_with_label(
			_("Enable alternate dictionary"));
	gtk_widget_show(use_alternate_checkbtn);
	gtk_box_pack_start(GTK_BOX(vbox2), use_alternate_checkbtn, TRUE, TRUE, 0);

	CLAWS_SET_TIP(use_alternate_checkbtn, 
			_("Faster switching with last used dictionary"));

	vbox2 = gtkut_get_options_frame(vbox1, &automatic_frame, _("Automatic spell checking"));
	
	check_while_typing_checkbtn = gtk_check_button_new_with_label(
			_("Check while typing"));
	gtk_widget_show(check_while_typing_checkbtn);
	gtk_box_pack_start(GTK_BOX(vbox2), check_while_typing_checkbtn, TRUE, TRUE, 0);

	recheck_when_changing_dict_checkbtn = gtk_check_button_new_with_label(
			_("Re-check message when changing dictionary"));
	gtk_widget_show(recheck_when_changing_dict_checkbtn);
	gtk_box_pack_start(GTK_BOX(vbox2), recheck_when_changing_dict_checkbtn, TRUE, TRUE, 0);
	
	vbox2 = gtkut_get_options_frame(vbox1, &dictionary_frame, _("Dictionary"));
	
	table = gtk_table_new(6, 4, FALSE);
	gtk_widget_show(table);
	gtk_container_set_border_width(GTK_CONTAINER(table), 8);
 	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
 	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	gtk_box_pack_start(GTK_BOX(vbox2), table, TRUE, TRUE, 0);

	default_dict_label = gtk_label_new(_("Default dictionary"));
	gtk_widget_show(default_dict_label);
	gtk_table_attach(GTK_TABLE (table), default_dict_label, 0, 1, 0, 1,
                    	 (GtkAttachOptions) (GTK_FILL),
                    	 (GtkAttachOptions) (0), 0, 2);
	gtk_label_set_justify(GTK_LABEL(default_dict_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(default_dict_label), 1, 0.5);
	
	default_dict_combo = gtkaspell_dictionary_combo_new(TRUE);
	gtk_widget_set_size_request(default_dict_combo, 180, -1);
	gtk_table_attach (GTK_TABLE (table), default_dict_combo, 1, 2, 0, 1,
			  GTK_SHRINK, 0, 0, 0);

	default_alt_dict_label = gtk_label_new(_("Default alternate dictionary"));
	gtk_widget_show(default_alt_dict_label);
	gtk_table_attach(GTK_TABLE (table), default_alt_dict_label, 0, 1, 1, 2,
                    	 (GtkAttachOptions) (GTK_FILL),
                    	 (GtkAttachOptions) (0), 0, 2);
	gtk_label_set_justify(GTK_LABEL(default_alt_dict_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(default_alt_dict_label), 1, 0.5);
	
	default_alt_dict_combo = gtkaspell_dictionary_combo_new(FALSE);
	gtk_widget_set_size_request(default_alt_dict_combo, 180, -1);
	gtk_table_attach (GTK_TABLE (table), default_alt_dict_combo, 1, 2, 1, 2,
			  GTK_SHRINK, 0, 0, 0);

	both_dict_check = gtk_check_button_new_with_label(
				_("Check with both dictionaries"));
	gtk_widget_show(both_dict_check);
	gtk_table_attach (GTK_TABLE (table), both_dict_check, 1, 2, 2, 3,
			  GTK_SHRINK, 0, 0, 0);

#ifdef WIN32
	get_dictionaries_btn = gtkut_get_link_btn(GTK_WIDGET(window), 
				DICTS_URI, _("Get more dictionaries..."));

	gtk_widget_show(get_dictionaries_btn);
	gtk_table_attach (GTK_TABLE (table), get_dictionaries_btn, 1, 2, 3, 4,
			  GTK_SHRINK, 0, 0, 0);
#endif
	misspelled_hbox = gtk_hbox_new(FALSE, 10);
	gtk_widget_show(misspelled_hbox);
	gtk_box_pack_start(GTK_BOX(vbox1), misspelled_hbox, FALSE, FALSE, 0);
		
	misspelled_label = gtk_label_new(_("Misspelled word color"));
	gtk_widget_show(misspelled_label);
	gtk_box_pack_start(GTK_BOX(misspelled_hbox), misspelled_label,
		FALSE, FALSE, 0);
	gtk_label_set_justify(GTK_LABEL(misspelled_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(misspelled_label), 1, 0.5);

	misspelled_colorbtn = gtk_button_new_with_label("");
	gtk_widget_show(misspelled_colorbtn);
	gtk_box_pack_start(GTK_BOX(misspelled_hbox), misspelled_colorbtn,
		FALSE, FALSE, 0);
	gtk_widget_set_size_request(misspelled_colorbtn, 30, 20);
	CLAWS_SET_TIP(misspelled_colorbtn,
			     _("Pick color for misspelled word. "
			       "Use black to underline"));

	SET_TOGGLE_SENSITIVITY(enable_aspell_checkbtn, automatic_frame);
	SET_TOGGLE_SENSITIVITY(enable_aspell_checkbtn, dictionary_frame);
	SET_TOGGLE_SENSITIVITY(enable_aspell_checkbtn, misspelled_label);
	SET_TOGGLE_SENSITIVITY(enable_aspell_checkbtn, misspelled_colorbtn);
	SET_TOGGLE_SENSITIVITY(enable_aspell_checkbtn, use_alternate_checkbtn);
	SET_TOGGLE_SENSITIVITY(use_alternate_checkbtn, default_alt_dict_label);
	SET_TOGGLE_SENSITIVITY(use_alternate_checkbtn, default_alt_dict_combo);
	SET_TOGGLE_SENSITIVITY(use_alternate_checkbtn, both_dict_check);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(enable_aspell_checkbtn),
			prefs_common.enable_aspell);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(both_dict_check),
			prefs_common.use_both_dicts);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_while_typing_checkbtn),
			prefs_common.check_while_typing);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(recheck_when_changing_dict_checkbtn),
			prefs_common.recheck_when_changing_dict);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(use_alternate_checkbtn),
			prefs_common.use_alternate);
	if (prefs_common.dictionary && 
	    strrchr(prefs_common.dictionary, '/')) {
		gchar *tmp = g_strdup(strrchr(prefs_common.dictionary, '/')+1);
		g_free(prefs_common.dictionary);
		prefs_common.dictionary = tmp;
	}
	if (prefs_common.alt_dictionary &&
	    strrchr(prefs_common.alt_dictionary, '/')) {
		gchar *tmp = g_strdup(strrchr(prefs_common.alt_dictionary, '/')+1);
		g_free(prefs_common.alt_dictionary);
		prefs_common.alt_dictionary = tmp;
	}
	if (prefs_common.dictionary &&
	    strchr(prefs_common.dictionary, '-')) {
		*(strchr(prefs_common.dictionary, '-')) = '\0';
	}
	if (prefs_common.alt_dictionary &&
	    strchr(prefs_common.alt_dictionary, '-')) {
		*(strchr(prefs_common.alt_dictionary, '-')) = '\0';
	}
	gtkaspell_set_dictionary_menu_active_item(GTK_COMBO_BOX(default_dict_combo),
						prefs_common.dictionary);

	gtkaspell_set_dictionary_menu_active_item(GTK_COMBO_BOX(default_alt_dict_combo),
						prefs_common.alt_dictionary);

	g_signal_connect(G_OBJECT(misspelled_colorbtn), "clicked",
			 G_CALLBACK(prefs_spelling_colorsel), prefs_spelling);

	prefs_spelling->misspell_col = prefs_common.misspelled_col;
	gtkut_set_widget_bgcolor_rgb(misspelled_colorbtn, prefs_spelling->misspell_col);

	prefs_spelling->window			= GTK_WIDGET(window);
	prefs_spelling->automatic_frame =	automatic_frame;
	prefs_spelling->dictionary_frame =	dictionary_frame;
	prefs_spelling->enable_aspell_checkbtn	= enable_aspell_checkbtn;
	prefs_spelling->check_while_typing_checkbtn
		= check_while_typing_checkbtn;
	prefs_spelling->recheck_when_changing_dict_checkbtn
		= recheck_when_changing_dict_checkbtn;
	prefs_spelling->use_alternate_checkbtn	= use_alternate_checkbtn;
	prefs_spelling->default_dict_label	= default_dict_label;
	prefs_spelling->default_dict_combo	= default_dict_combo;
	prefs_spelling->default_alt_dict_label	= default_alt_dict_label;
	prefs_spelling->default_alt_dict_combo	= default_alt_dict_combo;
	prefs_spelling->misspelled_label	= misspelled_label;
	prefs_spelling->misspelled_colorbtn	= misspelled_colorbtn;
	prefs_spelling->both_dict_check	= both_dict_check;

	prefs_spelling->page.widget = vbox1;
}

static void prefs_spelling_save(PrefsPage *_page)
{
	SpellingPage *spelling = (SpellingPage *) _page;

	prefs_common.enable_aspell =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(spelling->enable_aspell_checkbtn));
	prefs_common.check_while_typing =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(spelling->check_while_typing_checkbtn));
	prefs_common.recheck_when_changing_dict =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(spelling->recheck_when_changing_dict_checkbtn));
	prefs_common.use_alternate =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(spelling->use_alternate_checkbtn));
	prefs_common.use_both_dicts =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(spelling->both_dict_check));

	g_free(prefs_common.dictionary);
	prefs_common.dictionary = 
		gtkaspell_get_dictionary_menu_active_item(
				GTK_COMBO_BOX(spelling->default_dict_combo));

	g_free(prefs_common.alt_dictionary);
	prefs_common.alt_dictionary = 
		gtkaspell_get_dictionary_menu_active_item(
				GTK_COMBO_BOX(spelling->default_alt_dict_combo));

	prefs_common.misspelled_col = spelling->misspell_col;
}

static void prefs_spelling_destroy_widget(PrefsPage *_page)
{
	/* SpellingPage *spelling = (SpellingPage *) _page; */

}

SpellingPage *prefs_spelling;

void prefs_spelling_init(void)
{
	SpellingPage *page;
	static gchar *path[3];
	const gchar* language = NULL;
	
	path[0] = _("Compose");
	path[1] = _("Spell Checking");
	path[2] = NULL;

	page = g_new0(SpellingPage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_spelling_create_widget;
	page->page.destroy_widget = prefs_spelling_destroy_widget;
	page->page.save_page = prefs_spelling_save;
	page->page.weight = 180.0;

	prefs_gtk_register_page((PrefsPage *) page);
	prefs_spelling = page;
	
	language = g_getenv("LANG");
	if (language == NULL)
		language = "en";
	else if (!strcmp(language, "POSIX") || !strcmp(language, "C"))
		language = "en";
	
	if (!prefs_common.dictionary)
		prefs_common.dictionary = g_strdup_printf("%s",
						language);
	if (!strlen(prefs_common.dictionary)
	||  !strcmp(prefs_common.dictionary,"(None"))
		prefs_common.dictionary = g_strdup_printf("%s",
						language);
	if (strcasestr(prefs_common.dictionary,".utf"))
		*(strcasestr(prefs_common.dictionary,".utf")) = '\0';
	if (strstr(prefs_common.dictionary,"@"))
		*(strstr(prefs_common.dictionary,"@")) = '\0';
}

void prefs_spelling_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_spelling);
	g_free(prefs_spelling);
}

#endif /* USE_ENCHANT */
