/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <gtk/gtk.h>

#if !GTK_CHECK_VERSION(2, 6, 0)
#	ifdef HAVE_CONFIG_H
#		include "config.h"
#	endif
#	include "i18n.h"
void add_about_stock(void)
{
	GtkStockItem *stock;
	GtkIconFactory *icon_factory;
	GtkIconSet *icon_set;
	
	stock = g_malloc(sizeof(GtkStockItem));
	stock->stock_id = "my-gtk-about";
//	stock->label = _("_About");
	stock->label = "About";
	stock->modifier = 0;
	stock->keyval = 0;
	stock->translation_domain = PACKAGE;
	gtk_stock_add(stock, 1);
	
	icon_factory = gtk_icon_factory_new();
	icon_set = gtk_icon_factory_lookup_default(GTK_STOCK_HELP);
	gtk_icon_factory_add(icon_factory, stock->stock_id, icon_set);
	gtk_icon_set_unref(icon_set);
	gtk_icon_factory_add_default(icon_factory);
	g_object_unref(icon_factory);
}

typedef struct {
	GtkWidget *dialog;
	gchar **authors;
	gchar **documenters;
	gchar *translator_credits;
} Credits;

static void add_credits_page(GtkWidget *notebook,
	const gchar *title, gchar **people)
{
	GtkWidget *label, *sw;
	GString *string;
	gint i;
	gchar *tmp;
		
	label = gtk_label_new(NULL);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.0);
	gtk_misc_set_padding(GTK_MISC(label), 8, 8);
	
	sw = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw),
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(sw), label);
	gtk_viewport_set_shadow_type(GTK_VIEWPORT(GTK_BIN(sw)->child),
		GTK_SHADOW_NONE);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
		sw, gtk_label_new(title));
	
	string = g_string_new(NULL);
	for (i = 0; people[i]; i++) {
		tmp = g_markup_escape_text(people[i], -1);
		g_string_append(string, tmp);
		g_free(tmp);
		if (people[i + 1])
			g_string_append_c(string, '\n');
	}
	
	gtk_label_set_markup(GTK_LABEL(label), string->str);
	g_string_free(string, TRUE);
}

static void display_credits_dialog(GtkWidget *button, Credits *credits)
{
	GtkWidget *dialog, *notebook;
	
	if (credits->dialog != NULL) {
		gtk_window_present(GTK_WINDOW(credits->dialog));
		return;
	}
	
	dialog = gtk_dialog_new_with_buttons(_("Credits"),
		GTK_WINDOW(gtk_widget_get_toplevel(button)),
		GTK_DIALOG_DESTROY_WITH_PARENT,
#	if GTK_CHECK_VERSION(2, 4, 0)
		GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
#	else
		GTK_STOCK_OK, GTK_RESPONSE_CLOSE,
#	endif
		NULL);
	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);
#	if GTK_CHECK_VERSION(2, 4, 0)
	gtk_dialog_set_has_separator(GTK_DIALOG(dialog), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 5);
	gtk_box_set_spacing(GTK_BOX(GTK_DIALOG(dialog)->vbox), 2);
#	endif
	
	credits->dialog = dialog;
	gtk_window_set_default_size(GTK_WINDOW(dialog), 360, 260);
	gtk_window_set_modal(GTK_WINDOW(dialog),
		gtk_window_get_modal(GTK_WINDOW(gtk_widget_get_toplevel(button))));
	g_signal_connect(G_OBJECT(dialog), "response",
		G_CALLBACK(gtk_widget_destroy), dialog);
	g_signal_connect(G_OBJECT(dialog), "destroy",
		G_CALLBACK(gtk_widget_destroyed), &(credits->dialog));
	
	notebook = gtk_notebook_new();
#	if GTK_CHECK_VERSION(2, 4, 0)
	gtk_container_set_border_width(GTK_CONTAINER(notebook), 5);
#	else
	gtk_container_set_border_width(GTK_CONTAINER(notebook), 8);
#	endif
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox),
		notebook, TRUE, TRUE, 0);
	if (credits->authors != NULL)
		add_credits_page(notebook,
			_("Written by"), credits->authors);
	if (credits->documenters[0] != NULL)
		add_credits_page(notebook,
			_("Documented by"), credits->documenters);
	if (credits->translator_credits != NULL) {
		gchar *translators[2];
		translators[0] = credits->translator_credits;
		translators[1] = NULL;
		add_credits_page(notebook,
			_("Translated by"), translators);
	}
	
	gtk_widget_show_all(dialog);
}

static GtkWidget *my_gtk_about_new(
	const gchar *name,
	const gchar *version,
	const gchar *copyright,
	const gchar *comments,
	const gchar **authors,
	const gchar **documenters,
	const gchar *translator_credits,
	GdkPixbuf *logo_pixbuf)
{
	GtkWidget *dialog;
	GtkWidget *vbox;
	GtkWidget *button;
	GtkWidget *logo_image;
	GtkWidget *name_label;
	GtkWidget *comments_label;
	GtkWidget *copyright_label;
	gchar *title;
	gchar *str;
	static Credits *credits = NULL;
	
	if (credits == NULL) {
		credits = g_malloc(sizeof(Credits));
		credits->dialog = NULL;
		credits->authors = g_strdupv((gchar **)authors);
		credits->documenters = g_strdupv((gchar **)documenters);
		credits->translator_credits = g_strdup(translator_credits);
	}
	
	dialog = gtk_dialog_new();
#	if GTK_CHECK_VERSION(2, 4, 0)
	gtk_dialog_set_has_separator(GTK_DIALOG(dialog), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 5);
	gtk_box_set_spacing(GTK_BOX(GTK_DIALOG(dialog)->vbox), 5);
#	endif
	gtk_dialog_add_button(GTK_DIALOG(dialog),
#	if GTK_CHECK_VERSION(2, 4, 0)
		GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE);
#	else
		GTK_STOCK_OK, GTK_RESPONSE_CLOSE);
#	endif
	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);
	g_signal_connect(G_OBJECT(dialog), "response",
		G_CALLBACK(gtk_widget_destroy), dialog);
#	if GTK_CHECK_VERSION(2, 4, 0)
	button = gtk_button_new_with_mnemonic(_("C_redits"));
#	else
	button = gtk_button_new_with_mnemonic(_("_Credits"));
#	endif
	gtk_box_pack_end(GTK_BOX(GTK_DIALOG(dialog)->action_area),
		button, FALSE, TRUE, 0);
	gtk_button_box_set_child_secondary(
		GTK_BUTTON_BOX(GTK_DIALOG(dialog)->action_area), button, TRUE);
	g_signal_connect(button, "clicked",
		G_CALLBACK(display_credits_dialog), credits);
	gtk_widget_show(button);
	
	title = g_strdup_printf(_("About %s"), name);
	gtk_window_set_title(GTK_WINDOW(dialog), title);
	g_free(title);
	
	vbox = gtk_vbox_new(FALSE, 8);
#	if GTK_CHECK_VERSION(2, 4, 0)
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);
#	else
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 8);
#	endif
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), vbox, TRUE, TRUE, 0);
	
	logo_image = gtk_image_new();
	gtk_image_set_from_pixbuf(GTK_IMAGE(logo_image), logo_pixbuf);
	gtk_box_pack_start(GTK_BOX(vbox), logo_image, FALSE, FALSE, 0);
 	
	name_label = gtk_label_new(NULL);
	gtk_label_set_selectable(GTK_LABEL(name_label), TRUE);
	gtk_label_set_justify(GTK_LABEL(name_label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(vbox), name_label, FALSE, FALSE, 0);
	
	comments_label = gtk_label_new(NULL);
	gtk_label_set_selectable(GTK_LABEL(comments_label), TRUE);
	gtk_label_set_justify(GTK_LABEL(comments_label), GTK_JUSTIFY_CENTER);
	gtk_label_set_line_wrap(GTK_LABEL(comments_label), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), comments_label, FALSE, FALSE, 0);
	
	copyright_label = gtk_label_new(NULL);
	gtk_label_set_selectable(GTK_LABEL(copyright_label), TRUE);
	gtk_label_set_justify(GTK_LABEL(comments_label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(vbox), copyright_label, FALSE, FALSE, 0);
	
	str = g_strdup_printf(
		"<span size=\"xx-large\" weight=\"bold\">%s %s</span>", name, version);
	gtk_label_set_markup(GTK_LABEL(name_label), str);
	g_free(str);
	gtk_label_set_markup(GTK_LABEL(comments_label), comments);
	str = g_strdup_printf("<span size=\"small\">%s</span>", copyright);
	gtk_label_set_markup(GTK_LABEL(copyright_label), str);
	g_free(str);
	
	gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE);
	gtk_widget_show_all(vbox);
	
	return dialog;
}

GtkWidget *create_about_dialog(
	const gchar *name,
	const gchar *version,
	const gchar *copyright,
	const gchar *comments,
	const gchar **authors,
	const gchar **documenters,
	const gchar *translator_credits,
	GdkPixbuf *logo)
{
	GtkWidget *about;
	
	about = my_gtk_about_new(
		name,
		version,
		copyright,
		comments,
		authors,
		documenters,
		translator_credits,
		logo);
	
	return about;
}
#endif

