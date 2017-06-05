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

#include "leafpad.h"
#include <gdk/gdkkeysyms.h>

static GtkWidget *menu_item_save;
static GtkWidget *menu_item_cut;
static GtkWidget *menu_item_copy;
static GtkWidget *menu_item_paste;
static GtkWidget *menu_item_delete;

static GtkItemFactoryEntry menu_items[] =
{
	{ N_("/_File"), NULL,
		NULL, 0, "<Branch>" },
	{ N_("/File/_New"), "<control>N",
		G_CALLBACK(on_file_new), 0, "<StockItem>", GTK_STOCK_NEW },
	{ N_("/File/_Open..."), "<control>O",
		G_CALLBACK(on_file_open), 0, "<StockItem>", GTK_STOCK_OPEN },
	{ N_("/File/_Save"), "<control>S",
		G_CALLBACK(on_file_save), 0, "<StockItem>", GTK_STOCK_SAVE },
	{ N_("/File/Save _As..."), "<shift><control>S",
		G_CALLBACK(on_file_save_as), 0, "<StockItem>", GTK_STOCK_SAVE_AS },
	{ "/File/---", NULL,
		NULL, 0, "<Separator>" },
#ifdef ENABLE_PRINT
#	if GTK_CHECK_VERSION(2, 10, 0)
	{ N_("/File/Print Pre_view"), "<shift><control>P",
		G_CALLBACK(on_file_print_preview), 0, "<StockItem>", GTK_STOCK_PRINT_PREVIEW },
#	endif
	{ N_("/File/_Print..."), "<control>P",
		G_CALLBACK(on_file_print), 0, "<StockItem>", GTK_STOCK_PRINT },
#	if GTK_CHECK_VERSION(2, 10, 0)
	{ "/File/---", NULL,
		NULL, 0, "<Separator>" },
#	endif
#endif
	{ N_("/File/_Quit"), "<control>Q",
		G_CALLBACK(on_file_quit), 0, "<StockItem>", GTK_STOCK_QUIT },
	{ N_("/_Edit"),	 NULL,
		NULL, 0, "<Branch>" },
	{ N_("/Edit/_Undo"), "<control>Z",
		G_CALLBACK(on_edit_undo), 0, "<StockItem>", GTK_STOCK_UNDO },
	{ N_("/Edit/_Redo"), "<shift><control>Z",
		G_CALLBACK(on_edit_redo), 0, "<StockItem>", GTK_STOCK_REDO },
	{ "/Edit/---", NULL,
		NULL, 0, "<Separator>" },
	{ N_("/Edit/Cu_t"), "<control>X",
		G_CALLBACK(on_edit_cut), 0, "<StockItem>", GTK_STOCK_CUT },
	{ N_("/Edit/_Copy"), "<control>C",
		G_CALLBACK(on_edit_copy), 0, "<StockItem>", GTK_STOCK_COPY },
	{ N_("/Edit/_Paste"), "<control>V",
		G_CALLBACK(on_edit_paste), 0, "<StockItem>", GTK_STOCK_PASTE },
	{ N_("/Edit/_Delete"), NULL,
		G_CALLBACK(on_edit_delete), 0, "<StockItem>", GTK_STOCK_DELETE },
	{ "/Edit/---", NULL,
		NULL, 0, "<Separator>" },
	{ N_("/Edit/Select _All"), "<control>A",
		G_CALLBACK(on_edit_select_all), 0 },
	{ N_("/_Search"),	 NULL,
		NULL, 0, "<Branch>" },
	{ N_("/Search/_Find..."), "<control>F",
		G_CALLBACK(on_search_find), 0, "<StockItem>", GTK_STOCK_FIND },
	{ N_("/Search/Find _Next"), "<control>G",
		G_CALLBACK(on_search_find_next), 0 },
	{ N_("/Search/Find _Previous"), "<shift><control>G",
		G_CALLBACK(on_search_find_previous), 0 },
	{ N_("/Search/_Replace..."), "<control>H",
		G_CALLBACK(on_search_replace), 0, "<StockItem>", GTK_STOCK_FIND_AND_REPLACE },
	{ "/Search/---", NULL,
		NULL, 0, "<Separator>" },
	{ N_("/Search/_Jump To..."), "<control>J",
		G_CALLBACK(on_search_jump_to), 0, "<StockItem>", GTK_STOCK_JUMP_TO },
	{ N_("/_Options"), NULL,
		NULL, 0, "<Branch>" },
	{ N_("/Options/_Font..."), NULL,
		G_CALLBACK(on_option_font), 0, "<StockItem>", GTK_STOCK_SELECT_FONT },
	{ N_("/Options/_Word Wrap"), NULL,
		G_CALLBACK(on_option_word_wrap), 0, "<CheckItem>" },
	{ N_("/Options/_Line Numbers"), NULL,
		G_CALLBACK(on_option_line_numbers), 0, "<CheckItem>" },
	{ "/Options/---", NULL,
		NULL, 0, "<Separator>" },
	{ N_("/Options/_Auto Indent"), NULL,
		G_CALLBACK(on_option_auto_indent), 0, "<CheckItem>" },
	{ N_("/_Help"), NULL,
		NULL, 0, "<Branch>" },
	{ N_("/Help/_About"), NULL,
#if GTK_CHECK_VERSION(2, 6, 0)
		G_CALLBACK(on_help_about), 0, "<StockItem>", GTK_STOCK_ABOUT },
#else
		G_CALLBACK(on_help_about), 0, "<StockItem>", "my-gtk-about" },
#endif
};

static gint nmenu_items = sizeof(menu_items) / sizeof(GtkItemFactoryEntry);

static gchar *menu_translate(const gchar *path, gpointer data)
{
	gchar *str;
	
	str = (gchar *)_(path);
	
	return str;
}

void menu_sensitivity_from_modified_flag(gboolean is_text_modified)
{
	gtk_widget_set_sensitive(menu_item_save,   is_text_modified);
}

void menu_sensitivity_from_selection_bound(gboolean is_bound_exist)
{
	gtk_widget_set_sensitive(menu_item_cut,    is_bound_exist);
	gtk_widget_set_sensitive(menu_item_copy,   is_bound_exist);
	gtk_widget_set_sensitive(menu_item_delete, is_bound_exist);
}

//void menu_sensitivity_from_clipboard(gboolean is_clipboard_exist)
void menu_sensitivity_from_clipboard(void)
{
//g_print("clip board checked.\n");
	gtk_widget_set_sensitive(menu_item_paste,
		gtk_clipboard_wait_is_text_available(
			gtk_clipboard_get(GDK_SELECTION_CLIPBOARD)));
}

GtkWidget *create_menu_bar(GtkWidget *window)
{
	GtkAccelGroup *accel_group;
	GtkItemFactory *ifactory;
	gboolean flag_emacs = FALSE;
	
	gchar *key_theme = NULL;
	GtkSettings *settings = gtk_settings_get_default();
	if (settings) {
		g_object_get(settings, "gtk-key-theme-name", &key_theme, NULL);
		if (key_theme) {
			if (!g_ascii_strcasecmp(key_theme, "Emacs"))
				flag_emacs = TRUE;
			g_free(key_theme);
		}
	}
	
	accel_group = gtk_accel_group_new();
	ifactory = gtk_item_factory_new(GTK_TYPE_MENU_BAR, "<main>", accel_group);
	gtk_item_factory_set_translate_func(ifactory, menu_translate, NULL, NULL);
	gtk_item_factory_create_items(ifactory, nmenu_items, menu_items, NULL);
	gtk_window_add_accel_group(GTK_WINDOW(window), accel_group);
	
	/* hidden keybinds */
	gtk_accel_group_connect(
		accel_group, GDK_W, GDK_CONTROL_MASK, 0,
		g_cclosure_new_swap(G_CALLBACK(on_file_close), NULL, NULL));
	gtk_accel_group_connect(
		accel_group, GDK_T, GDK_CONTROL_MASK, 0,
		g_cclosure_new_swap(G_CALLBACK(on_option_always_on_top), NULL, NULL));
	gtk_widget_add_accelerator(
		gtk_item_factory_get_widget(ifactory, "/Edit/Redo"),
		"activate", accel_group, GDK_Y, GDK_CONTROL_MASK, 0);
	gtk_widget_add_accelerator(
		gtk_item_factory_get_widget(ifactory, "/Search/Find Next"),
		"activate", accel_group, GDK_F3, 0, 0);
	gtk_widget_add_accelerator(
		gtk_item_factory_get_widget(ifactory, "/Search/Find Previous"),
		"activate", accel_group, GDK_F3, GDK_SHIFT_MASK, 0);
	gtk_widget_add_accelerator(
		gtk_item_factory_get_widget(ifactory, "/Search/Replace..."),
		"activate", accel_group, GDK_R, GDK_CONTROL_MASK, 0);
	
	/* initialize sensitivities */
	gtk_widget_set_sensitive(
		gtk_item_factory_get_widget(ifactory, "/Search/Find Next"),
		FALSE);
	gtk_widget_set_sensitive(
		gtk_item_factory_get_widget(ifactory, "/Search/Find Previous"),
		FALSE);
	
	menu_item_save   = gtk_item_factory_get_widget(ifactory, "/File/Save");
	menu_item_cut    = gtk_item_factory_get_widget(ifactory, "/Edit/Cut");
	menu_item_copy   = gtk_item_factory_get_widget(ifactory, "/Edit/Copy");
	menu_item_paste  = gtk_item_factory_get_widget(ifactory, "/Edit/Paste");
	menu_item_delete = gtk_item_factory_get_widget(ifactory, "/Edit/Delete");
	menu_sensitivity_from_selection_bound(FALSE);
	
	return gtk_item_factory_get_widget(ifactory, "<main>");
}
