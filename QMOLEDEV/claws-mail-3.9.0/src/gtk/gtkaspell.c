/* gtkaspell - a spell-checking addon for GtkText
 * Copyright (c) 2000 Evan Martin (original code for ispell).
 * Copyright (c) 2002 Melvin Hadasht.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */
/*
 * Stuphead: (C) 2000,2001 Grigroy Bakunov, Sergey Pinaev
 * Adapted for Sylpheed (Claws) (c) 2001-2002 by Hiroyuki Yamamoto & 
 * The Claws Mail Team.
 * Adapted for pspell (c) 2001-2002 Melvin Hadasht
 * Adapted for GNU/aspell (c) 2002 Melvin Hadasht
 */
 
#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_ENCHANT

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include <fcntl.h>
#include <time.h>
#include <dirent.h>

#include <glib.h>
#include <glib/gi18n.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include "utils.h"
#include "alertpanel.h"
#include "gtkaspell.h"
#include "gtk/gtkutils.h"
#include "gtk/combobox.h"

#define ASPELL_FASTMODE       1
#define ASPELL_NORMALMODE     2
#define ASPELL_BADSPELLERMODE 3

/* size of the text buffer used in various word-processing routines. */
#define BUFSIZE 1024

/* number of suggestions to display on each menu. */
#define MENUCOUNT 15

enum {
	SET_GTKASPELL_NAME	= 0,
	SET_GTKASPELL_FULLNAME	= 1,
	SET_GTKASPELL_SIZE
};

typedef struct _GtkAspellCheckers {
	GSList		*checkers;
	GSList		*dictionary_list;
	gchar		*error_message;
} GtkAspellCheckers;

/******************************************************************************/

static GtkAspellCheckers *gtkaspellcheckers;

/* Error message storage */
static void gtkaspell_checkers_error_message	(gchar	*message);

/* Callbacks */
static gboolean key_press_cb			(GtkWidget    *text_view,
						 GdkEventKey  *event,
                                                 GtkAspell      *gtkaspell);
static void entry_insert_cb			(GtkTextBuffer	*textbuf,
						 GtkTextIter	*iter,
						 gchar		*newtext, 
						 gint		len,
					 	 GtkAspell	*gtkaspell);
static void entry_delete_cb			(GtkTextBuffer	*textbuf,
						 GtkTextIter	*startiter,
						 GtkTextIter	*enditer,
						 GtkAspell	*gtkaspell);
/*static gint button_press_intercept_cb		(GtkTextView	*gtktext,
						 GdkEvent	*e, 
						 GtkAspell	*gtkaspell);
*/
static void button_press_intercept_cb(GtkTextView *gtktext,
   			GtkMenu *menu, GtkAspell *gtkaspell);
			
/* Checker creation */
static GtkAspeller* gtkaspeller_new		(Dictionary	*dict);
static GtkAspeller* gtkaspeller_real_new	(Dictionary	*dict);
static GtkAspeller* gtkaspeller_delete		(GtkAspeller	*gtkaspeller);
static GtkAspeller* gtkaspeller_real_delete	(GtkAspeller	*gtkaspeller);

/* Checker configuration */
static EnchantDict 	*set_dictionary   		(EnchantBroker *broker, 
							 Dictionary *dict);
static void 		set_use_both_cb     		(GtkMenuItem *w, 
							 GtkAspell *gtkaspell);

/* Checker actions */
static gboolean check_at			(GtkAspell	*gtkaspell, 
						 int		 from_pos);
static gboolean check_at_cb			(gpointer	data);
static GList* misspelled_suggest	 	(GtkAspell	*gtkaspell, 
						 gchar		*word);
static gboolean find_misspelled_cb		(gpointer 	data,
						 gboolean 	forward);
static void add_word_to_session_cb		(GtkWidget	*w, 
						 gpointer	 data);
static void add_word_to_personal_cb		(GtkWidget	*w, 
						 gpointer	 data);
static void replace_with_create_dialog_cb	(GtkWidget	*w,
						 gpointer	 data);
static void replace_with_supplied_word_cb	(GtkWidget	*w, 
						 GtkAspell	*gtkaspell);
static void replace_word_cb			(GtkWidget	*w, 
						 gpointer	data); 
static void replace_real_word			(GtkAspell	*gtkaspell, 
						 const gchar	*newword);
static void replace_real_word_cb		(gpointer 	data,
						const gchar	*newword);
static void check_with_alternate_cb		(GtkWidget	*w,
						 gpointer	 data);
static void toggle_check_while_typing_cb	(GtkWidget	*w, 
						 gpointer	 data);

/* Menu creation */
static GSList*	make_sug_menu			(GtkAspell	*gtkaspell);
static GSList * populate_submenu		(GtkAspell	*gtkaspell);
GSList*	gtkaspell_make_config_menu		(GtkAspell	*gtkaspell);
static void set_menu_pos			(GtkMenu	*menu, 
						 gint		*x, 
						 gint		*y, 
						 gboolean	*push_in,
						 gpointer	 data);
/* Other menu callbacks */
static gboolean aspell_key_pressed		(GtkWidget *widget,
				  		 GdkEventKey *event,
				  		 GtkAspell *gtkaspell);
static void change_dict_cb			(GtkWidget	*w, 
						 GtkAspell	*gtkaspell);
static void switch_to_alternate_cb		(GtkWidget	*w, 
						 gpointer	 data);

/* Misc. helper functions */
static void	 	set_point_continue		(GtkAspell *gtkaspell);
static void 		continue_check			(gpointer *gtkaspell);
static gboolean 	iswordsep			(gunichar c);
static gunichar		get_text_index_whar		(GtkAspell *gtkaspell, 
							 int pos);
static gboolean 	get_word_from_pos		(GtkAspell *gtkaspell, 
							 gint pos, 
							 char* buf,
							 gint buflen,
							 gint *pstart, 
							 gint *pend);
static void 		allocate_color			(GtkAspell *gtkaspell,
							 gint rgbvalue);
static void 		change_color			(GtkAspell *gtkaspell, 
			 				 gint start, 
							 gint end, 
							 gchar *newtext,
							 GdkColor *color);
static gint 		compare_dict			(Dictionary *a, 
							 Dictionary *b);
static void 		dictionary_delete		(Dictionary *dict);
static Dictionary *	dictionary_dup			(const Dictionary *dict);
static void 		reset_theword_data		(GtkAspell *gtkaspell);
static void 		free_checkers			(gpointer elt, 
							 gpointer data);

static void destroy_menu(GtkWidget *widget, gpointer user_data);	

/******************************************************************************/
static gint get_textview_buffer_charcount(GtkTextView *view);

static void 		gtkaspell_free_dictionary_list	(GSList *list);
static GSList*		gtkaspell_get_dictionary_list	(gint refresh);

static void 		gtkaspell_uncheck_all		(GtkAspell *gtkaspell);

static gint get_textview_buffer_charcount(GtkTextView *view)
{
	GtkTextBuffer *buffer;

	cm_return_val_if_fail(view, 0);

	buffer = gtk_text_view_get_buffer(view);
	cm_return_val_if_fail(buffer, 0);

	return gtk_text_buffer_get_char_count(buffer);
}
static gint get_textview_buffer_offset(GtkTextView *view)
{
	GtkTextBuffer * buffer;
	GtkTextMark * mark;
	GtkTextIter iter;

	cm_return_val_if_fail(view, 0);

	buffer = gtk_text_view_get_buffer(view);
	cm_return_val_if_fail(buffer, 0);

	mark = gtk_text_buffer_get_insert(buffer);
	cm_return_val_if_fail(mark, 0);

	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

	return gtk_text_iter_get_offset(&iter);
}
static void set_textview_buffer_offset(GtkTextView *view, gint offset)
{
	GtkTextBuffer *buffer;
	GtkTextIter iter;

	cm_return_if_fail(view);

	buffer = gtk_text_view_get_buffer(view);
	cm_return_if_fail(buffer);

	gtk_text_buffer_get_iter_at_offset(buffer, &iter, offset);
	gtk_text_buffer_place_cursor(buffer, &iter);
}
/******************************************************************************/

void gtkaspell_checkers_init(void)
{
	gtkaspellcheckers 		   = g_new(GtkAspellCheckers, 1);
	gtkaspellcheckers->checkers        = NULL;
	gtkaspellcheckers->dictionary_list = NULL;
	gtkaspellcheckers->error_message   = NULL;
}
	
void gtkaspell_checkers_quit(void)
{
	GSList *checkers;
	GSList *dict_list;

	if (gtkaspellcheckers == NULL) 
		return;

	if ((checkers  = gtkaspellcheckers->checkers)) {
		debug_print("Aspell: number of running checkers to delete %d\n",
				g_slist_length(checkers));

		g_slist_foreach(checkers, free_checkers, NULL);
		g_slist_free(checkers);
		gtkaspellcheckers->checkers = NULL;
	}

	if ((dict_list = gtkaspellcheckers->dictionary_list)) {
		debug_print("Aspell: number of dictionaries to delete %d\n",
				g_slist_length(dict_list));

		gtkaspell_free_dictionary_list(dict_list);
		gtkaspellcheckers->dictionary_list = NULL;
	}

	g_free(gtkaspellcheckers->error_message);
	gtkaspellcheckers->error_message = NULL;
	return;
}

static void gtkaspell_checkers_error_message (gchar *message)
{
	gchar *tmp;
	if (gtkaspellcheckers->error_message) {
		tmp = g_strdup_printf("%s\n%s", 
				      gtkaspellcheckers->error_message,
				      message);
		g_free(message);
		g_free(gtkaspellcheckers->error_message);
		gtkaspellcheckers->error_message = tmp;
	} else 
		gtkaspellcheckers->error_message = message;
	
	
}

const char *gtkaspell_checkers_strerror(void)
{
	cm_return_val_if_fail(gtkaspellcheckers, "");
	return gtkaspellcheckers->error_message;
}

void gtkaspell_checkers_reset_error(void)
{
	cm_return_if_fail(gtkaspellcheckers);
	
	g_free(gtkaspellcheckers->error_message);
	
	gtkaspellcheckers->error_message = NULL;
}

GtkAspell *gtkaspell_new(const gchar *dictionary, 
			 const gchar *alt_dictionary, 
			 const gchar *encoding, /* unused */
			 gint  misspelled_color,
			 gboolean check_while_typing,
			 gboolean recheck_when_changing_dict,
			 gboolean use_alternate,
			 gboolean use_both_dicts,
			 GtkTextView *gtktext,
			 GtkWindow *parent_win,
			 void (dict_changed_cb)(void *data),
			 void (*spell_menu_cb)(void *data),
			 void *data)
{
	Dictionary 	*dict;
	GtkAspell 	*gtkaspell;
	GtkAspeller 	*gtkaspeller;
	GtkTextBuffer *buffer;

	cm_return_val_if_fail(gtktext, NULL);
	if (!dictionary || !*dictionary) {
		gtkaspell_checkers_error_message(
				g_strdup(_("No dictionary selected.")));
		return NULL;
	}

	buffer = gtk_text_view_get_buffer(gtktext);
	
	dict 	       = g_new0(Dictionary, 1);
	if (strrchr(dictionary, '/')) {
		dict->fullname = g_strdup(strrchr(dictionary, '/')+1);
		dict->dictname = g_strdup(strrchr(dictionary, '/')+1);
	} else {
		dict->fullname = g_strdup(dictionary);
		dict->dictname = g_strdup(dictionary);
	}

	if (strchr(dict->fullname, '-')) {
		*(strchr(dict->fullname, '-')) = '\0';
		*(strchr(dict->dictname, '-')) = '\0';
	}
	gtkaspeller    = gtkaspeller_new(dict); 
	dictionary_delete(dict);

	if (!gtkaspeller) {
		gtkaspell_checkers_error_message(
				g_strdup_printf(_("Couldn't initialize %s speller."), dictionary));
		return NULL;
	}
	
	gtkaspell = g_new0(GtkAspell, 1);

	gtkaspell->gtkaspeller	      = gtkaspeller;

	if (use_alternate && alt_dictionary && *alt_dictionary) {
		Dictionary 	*alt_dict;
		GtkAspeller 	*alt_gtkaspeller;

		alt_dict 	       = g_new0(Dictionary, 1);
		if (strrchr(alt_dictionary, '/')) {
			alt_dict->fullname = g_strdup(strrchr(alt_dictionary, '/')+1);
			alt_dict->dictname = g_strdup(strrchr(alt_dictionary, '/')+1);
		} else {
			alt_dict->fullname = g_strdup(alt_dictionary);
			alt_dict->dictname = g_strdup(alt_dictionary);
		}
		if (strchr(alt_dict->fullname, '-')) {
			*(strchr(alt_dict->fullname, '-')) = '\0';
			*(strchr(alt_dict->dictname, '-')) = '\0';
		}

		alt_gtkaspeller    = gtkaspeller_new(alt_dict);
		dictionary_delete(alt_dict);

		if (!alt_gtkaspeller) {
			gtkaspell_checkers_error_message(
				g_strdup_printf(_("Couldn't initialize %s speller."), dictionary));
			gtkaspeller_delete(gtkaspeller);
			g_free(gtkaspell);
			return NULL;
		}

		gtkaspell->alternate_speller  = alt_gtkaspeller;
	} else {
		gtkaspell->alternate_speller  = NULL;
	}

	gtkaspell->theword[0]	      = 0x00;
	gtkaspell->start_pos	      = 0;
	gtkaspell->end_pos	      = 0;
	gtkaspell->orig_pos	      = -1;
	gtkaspell->end_check_pos      = -1;
	gtkaspell->misspelled	      = -1;
	gtkaspell->check_while_typing = check_while_typing;
	gtkaspell->recheck_when_changing_dict = recheck_when_changing_dict;
	gtkaspell->continue_check     = NULL;
	gtkaspell->replace_entry      = NULL;
	gtkaspell->gtktext	      = gtktext;
	gtkaspell->max_sug	      = -1;
	gtkaspell->suggestions_list   = NULL;
	gtkaspell->use_alternate      = use_alternate;
	gtkaspell->use_both_dicts     = use_both_dicts;
	gtkaspell->parent_window      = GTK_WIDGET(parent_win);
	gtkaspell->dict_changed_cb = dict_changed_cb;
	gtkaspell->menu_changed_cb = spell_menu_cb;
	gtkaspell->menu_changed_data = data;

	allocate_color(gtkaspell, misspelled_color);

	g_signal_connect(G_OBJECT(gtktext), "key_press_event",
			       G_CALLBACK(key_press_cb), gtkaspell);
	g_signal_connect_after(G_OBJECT(buffer), "insert-text",
			       G_CALLBACK(entry_insert_cb), gtkaspell);
	g_signal_connect_after(G_OBJECT(buffer), "delete-range",
		               G_CALLBACK(entry_delete_cb), gtkaspell);
	/*g_signal_connect(G_OBJECT(gtktext), "button-press-event",
			 G_CALLBACK(button_press_intercept_cb),
			 gtkaspell);*/
	g_signal_connect(G_OBJECT(gtktext), "populate-popup",
			 G_CALLBACK(button_press_intercept_cb), gtkaspell);
	
	debug_print("Aspell: created gtkaspell %p\n", gtkaspell);

	return gtkaspell;
}

void gtkaspell_delete(GtkAspell *gtkaspell) 
{
	GtkTextView *gtktext = gtkaspell->gtktext;
	
        g_signal_handlers_disconnect_by_func(G_OBJECT(gtktext),
					     G_CALLBACK(key_press_cb),
					     gtkaspell);
        g_signal_handlers_disconnect_by_func(G_OBJECT(gtktext),
					     G_CALLBACK(entry_insert_cb),
					     gtkaspell);
	g_signal_handlers_disconnect_by_func(G_OBJECT(gtktext),
					     G_CALLBACK(entry_delete_cb),
					     gtkaspell);
	g_signal_handlers_disconnect_by_func(G_OBJECT(gtktext),
					     G_CALLBACK(button_press_intercept_cb),
					     gtkaspell);

	gtkaspell_uncheck_all(gtkaspell);
	
	gtkaspeller_delete(gtkaspell->gtkaspeller);

	if (gtkaspell->alternate_speller)
		gtkaspeller_delete(gtkaspell->alternate_speller);

	if (gtkaspell->suggestions_list)
		gtkaspell_free_suggestions_list(gtkaspell);

	debug_print("Aspell: deleting gtkaspell %p\n", gtkaspell);

	g_free(gtkaspell);

	gtkaspell = NULL;
}

void gtkaspell_dict_changed(GtkAspell *gtkaspell)
{
	if(!gtkaspell || !gtkaspell->dict_changed_cb ||
			!gtkaspell->menu_changed_data)
		return;

	gtkaspell->dict_changed_cb(gtkaspell->menu_changed_data);
}

static gboolean key_press_cb			(GtkWidget    *text_view,
						 GdkEventKey  *event,
                                                 GtkAspell    *gtkaspell)
{
	gint pos;

	cm_return_val_if_fail(gtkaspell->gtkaspeller->speller, FALSE);

	if (!gtkaspell->check_while_typing)
		return FALSE;

	switch (event->keyval) {
		case GDK_KEY_Home:
		case GDK_KEY_Left:
		case GDK_KEY_Up:
		case GDK_KEY_Right:
		case GDK_KEY_Down:
		case GDK_KEY_Page_Up:
		case GDK_KEY_Page_Down:
		case GDK_KEY_End:
		case GDK_KEY_Begin:
			pos = get_textview_buffer_offset(GTK_TEXT_VIEW(text_view));
			if (pos > 0)
				check_at(gtkaspell, pos - 1);
			else
				check_at(gtkaspell, pos);
			break;
		default:
			break;
	}

	return FALSE;
}

static void entry_insert_cb(GtkTextBuffer *textbuf,
			    GtkTextIter *iter,
			    gchar *newtext,
			    gint len,
			    GtkAspell *gtkaspell)
{
	guint pos;

	cm_return_if_fail(gtkaspell->gtkaspeller->speller);

	if (!gtkaspell->check_while_typing)
		return;

	pos = gtk_text_iter_get_offset(iter);
	
	if (iswordsep(g_utf8_get_char(newtext))) {
		/* did we just end a word? */
		if (pos >= 2)
			check_at(gtkaspell, pos - 2);

		/* did we just split a word? */
		if (pos < gtk_text_buffer_get_char_count(textbuf))
			check_at(gtkaspell, pos + 1);
	} else {
		/* check as they type, *except* if they're typing at the end (the most
                 * common case).
                 */
		if (pos < gtk_text_buffer_get_char_count(textbuf) &&
		    !iswordsep(get_text_index_whar(gtkaspell, pos))) {
			check_at(gtkaspell, pos - 1);
		}
	}
}

static void entry_delete_cb(GtkTextBuffer *textbuf,
			    GtkTextIter *startiter,
			    GtkTextIter *enditer,
			    GtkAspell *gtkaspell)
{
	int origpos;
	gint start;
    
	cm_return_if_fail(gtkaspell->gtkaspeller->speller);

	if (!gtkaspell->check_while_typing)
		return;

	start = gtk_text_iter_get_offset(startiter);
	origpos = get_textview_buffer_offset(gtkaspell->gtktext);
	if (start) {
		check_at(gtkaspell, start - 1);
		check_at(gtkaspell, start);
	}

	set_textview_buffer_offset(gtkaspell->gtktext, origpos);
	/* this is to *UNDO* the selection, in case they were holding shift
         * while hitting backspace. */
	/* needed with textview ??? */
	/* gtk_editable_select_region(GTK_EDITABLE(gtktext), origpos, origpos); */
}

void gtkaspell_make_context_menu(GtkMenu *menu, GtkAspell *gtkaspell)
{
	GtkMenuItem *menuitem;
	GSList *spell_menu = NULL;
	GSList *items;
	gboolean suggest = FALSE;

	if (gtkaspell->misspelled && 
	    misspelled_suggest(gtkaspell, gtkaspell->theword)) {
		spell_menu = make_sug_menu(gtkaspell);
		suggest = TRUE;
	} 
	if (!spell_menu) 
		spell_menu = gtkaspell_make_config_menu(gtkaspell);
	
	menuitem = GTK_MENU_ITEM(gtk_separator_menu_item_new());
	gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), GTK_WIDGET(menuitem));
	gtk_widget_show(GTK_WIDGET(menuitem));

	spell_menu = g_slist_reverse(spell_menu);
	for (items = spell_menu;
	     items; items = items->next) {
		menuitem = GTK_MENU_ITEM(items->data);
		gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), GTK_WIDGET(menuitem));
		gtk_widget_show(GTK_WIDGET(menuitem));
	}
	g_slist_free(spell_menu);
	
	g_signal_connect(G_OBJECT(menu), "deactivate",
				 G_CALLBACK(destroy_menu),
				 gtkaspell);
	if (suggest)
		g_signal_connect(G_OBJECT(menu),
			"key_press_event",
		       	G_CALLBACK(aspell_key_pressed),
		       	gtkaspell);
}

static void set_position_cb(gpointer data, gint pos)
{
	GtkAspell *gtkaspell = (GtkAspell *) data;
	set_textview_buffer_offset(gtkaspell->gtktext, pos);
}

void gtkaspell_context_set(GtkAspell *gtkaspell)
{
	gtkaspell->ctx.set_position	= set_position_cb;
	gtkaspell->ctx.set_menu_pos	= set_menu_pos;
	gtkaspell->ctx.find_misspelled	= find_misspelled_cb;
	gtkaspell->ctx.check_word	= check_at_cb;
	gtkaspell->ctx.replace_word	= replace_real_word_cb;
	gtkaspell->ctx.data		= (gpointer) gtkaspell;
}

static void button_press_intercept_cb(GtkTextView *gtktext,
   			GtkMenu *menu, GtkAspell *gtkaspell)
{
	gtktext = gtkaspell->gtktext;
	gtkaspell->orig_pos = get_textview_buffer_offset(gtktext);
	gtkaspell->misspelled = check_at(gtkaspell, gtkaspell->orig_pos);

	gtkaspell_context_set(gtkaspell);	
	gtkaspell_make_context_menu(menu, gtkaspell);
}
/* Checker creation */
static GtkAspeller *gtkaspeller_new(Dictionary *dictionary)
{
	GtkAspeller	*gtkaspeller = NULL;
	GtkAspeller	*tmp;
	Dictionary	*dict;

	cm_return_val_if_fail(gtkaspellcheckers, NULL);

	cm_return_val_if_fail(dictionary, NULL);

	if (dictionary->dictname == NULL)
		gtkaspell_checkers_error_message(
				g_strdup(_("No dictionary selected.")));
	
	cm_return_val_if_fail(dictionary->fullname, NULL);

	dict = dictionary_dup(dictionary);

	tmp = g_new0(GtkAspeller, 1);
	tmp->dictionary = dict;

	g_free(tmp);

	if ((gtkaspeller = gtkaspeller_real_new(dict)) != NULL) {
		gtkaspellcheckers->checkers = g_slist_append(
				gtkaspellcheckers->checkers,
				gtkaspeller);

		debug_print("Aspell: Created a new gtkaspeller %p\n",
				gtkaspeller);
	} else {
		dictionary_delete(dict);

		debug_print("Aspell: Could not create spell checker.\n");
	}

	debug_print("Aspell: number of existing checkers %d\n", 
			g_slist_length(gtkaspellcheckers->checkers));

	return gtkaspeller;
}

static GtkAspeller *gtkaspeller_real_new(Dictionary *dict)
{
	GtkAspeller		*gtkaspeller;
	EnchantBroker 		*broker;
	EnchantDict		*speller;
	
	cm_return_val_if_fail(gtkaspellcheckers, NULL);
	cm_return_val_if_fail(dict, NULL);

	gtkaspeller = g_new(GtkAspeller, 1);
	
	gtkaspeller->dictionary = dict;

	broker = enchant_broker_init();

	if (!broker) {
		gtkaspell_checkers_error_message(
				g_strdup(_("Couldn't initialize Enchant broker.")));
		g_free(gtkaspeller);
		return NULL;
	}
	if ((speller = set_dictionary(broker, dict)) == NULL) {
		gtkaspell_checkers_error_message(
				g_strdup_printf(_("Couldn't initialize %s dictionary:"), dict->fullname));
		gtkaspell_checkers_error_message(
				g_strdup(enchant_broker_get_error(broker)));
		g_free(gtkaspeller);
		return NULL;
	}
	gtkaspeller->speller = speller;
	gtkaspeller->broker = broker;

	return gtkaspeller;
}

static GtkAspeller *gtkaspeller_delete(GtkAspeller *gtkaspeller)
{
	cm_return_val_if_fail(gtkaspellcheckers, NULL);
	
	gtkaspellcheckers->checkers = 
		g_slist_remove(gtkaspellcheckers->checkers, 
				gtkaspeller);

	debug_print("Aspell: Deleting gtkaspeller %p.\n", 
			gtkaspeller);

	gtkaspeller_real_delete(gtkaspeller);

	debug_print("Aspell: number of existing checkers %d\n", 
			g_slist_length(gtkaspellcheckers->checkers));

	return gtkaspeller;
}

static GtkAspeller *gtkaspeller_real_delete(GtkAspeller *gtkaspeller)
{
	cm_return_val_if_fail(gtkaspeller,          NULL);
	cm_return_val_if_fail(gtkaspeller->speller, NULL);

	enchant_broker_free_dict(gtkaspeller->broker, gtkaspeller->speller);
	enchant_broker_free(gtkaspeller->broker);

	dictionary_delete(gtkaspeller->dictionary);

	debug_print("Aspell: gtkaspeller %p deleted.\n", 
		    gtkaspeller);

	g_free(gtkaspeller);

	return NULL;
}

/*****************************************************************************/
/* Checker configuration */

static EnchantDict *set_dictionary(EnchantBroker *broker, Dictionary *dict)
{
	cm_return_val_if_fail(broker, FALSE);
	cm_return_val_if_fail(dict,   FALSE);

	return enchant_broker_request_dict(broker, dict->dictname );
}

static void set_use_both_cb(GtkMenuItem *w, GtkAspell *gtkaspell)
{
	gtkaspell->use_both_dicts = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(w));
	gtkaspell_dict_changed(gtkaspell);

	if (gtkaspell->menu_changed_cb)
		gtkaspell->menu_changed_cb(gtkaspell->menu_changed_data);
}
  
/* misspelled_suggest() - Create a suggestion list for  word  */
static GList *misspelled_suggest(GtkAspell *gtkaspell, gchar *word) 
{
	GList                 *list = NULL;
	char **suggestions;
	size_t num_sug, i;
	cm_return_val_if_fail(word, NULL);

	if (*word == 0)
		return NULL;

	gtkaspell_free_suggestions_list(gtkaspell);

	suggestions = enchant_dict_suggest(gtkaspell->gtkaspeller->speller, word, strlen(word), &num_sug);
	list = g_list_append(list, g_strdup(word)); 
	if (suggestions == NULL || num_sug == 0) {
		gtkaspell->max_sug          = -1;
		gtkaspell->suggestions_list = list;
		return list;
	}
	for (i = 0; i < num_sug; i++)
		list = g_list_append(list, g_strdup((gchar *)suggestions[i]));

	gtkaspell->max_sug          = num_sug - 1;
	gtkaspell->suggestions_list = list;
	enchant_dict_free_string_list(gtkaspell->gtkaspeller->speller, suggestions);
	return list;
}

/* misspelled_test() - Just test if word is correctly spelled */  
int gtkaspell_misspelled_test(GtkAspell *gtkaspell, char *word) 
{
	gint result = 0;
	cm_return_val_if_fail(word, 0);

	if (*word == 0)
		return 0;

	result = enchant_dict_check(gtkaspell->gtkaspeller->speller, word, strlen(word));

	if (result && gtkaspell->use_both_dicts && gtkaspell->alternate_speller) {
		gtkaspell_use_alternate_dict(gtkaspell);
		result = enchant_dict_check(gtkaspell->gtkaspeller->speller, word, strlen(word));
		gtkaspell_use_alternate_dict(gtkaspell);
	}
	return (result && strcasecmp(word, "sylpheed") && 
		strcasecmp(word, "claws-mail"));
}


static gboolean iswordsep(gunichar c) 
{
	return (g_unichar_isspace(c) || g_unichar_ispunct(c)) && c != (gunichar)'\'';
}

static gunichar get_text_index_whar(GtkAspell *gtkaspell, int pos) 
{
	GtkTextView *view = gtkaspell->gtktext;
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(view);
	GtkTextIter start, end;
	gchar *utf8chars;
	gunichar a = '\0';

	gtk_text_buffer_get_iter_at_offset(buffer, &start, pos);
	gtk_text_buffer_get_iter_at_offset(buffer, &end, pos+1);

	utf8chars = gtk_text_iter_get_text(&start, &end);
	a = g_utf8_get_char(utf8chars);
	g_free(utf8chars);

	return a;
}

/* get_word_from_pos () - return the word pointed to. */
/* Handles correctly the quotes. */
static gboolean get_word_from_pos(GtkAspell *gtkaspell, gint pos, 
                                  char* buf, gint buflen,
                                  gint *pstart, gint *pend) 
{

	/* TODO : when correcting a word into quotes, change the color of */
	/* the quotes too, as may be they were highlighted before. To do  */
	/* this, we can use two others pointers that points to the whole    */
	/* word including quotes. */

	gint start;
	gint end;
		  
	gunichar c;
	GtkTextView *gtktext;
	
	gtktext = gtkaspell->gtktext;
	if (iswordsep(get_text_index_whar(gtkaspell, pos))) 
		return FALSE;
	
	/* The apostrophe character is somtimes used for quotes 
	 * So include it in the word only if it is not surrounded 
	 * by other characters. 
	 */
	 
	for (start = pos; start >= 0; --start) {
		c = get_text_index_whar(gtkaspell, start);
		if (c == (gunichar)'\'') {
			if (start > 0) {
				if (g_unichar_isspace(get_text_index_whar(gtkaspell,
								 start - 1))
				||  g_unichar_ispunct(get_text_index_whar(gtkaspell,
								 start - 1))
				||  g_unichar_isdigit(get_text_index_whar(gtkaspell,
								 start - 1))) {
					/* start_quote = TRUE; */
					break;
				}
			}
			else {
				/* start_quote = TRUE; */
				break;
			}
		}
		else if (g_unichar_isspace(c) || g_unichar_ispunct(c) || g_unichar_isdigit(c))
				break;
	}

	start++;

	for (end = pos; end < get_textview_buffer_charcount(gtktext); end++) {
		c = get_text_index_whar(gtkaspell, end); 
		if (c == (gunichar)'\'') {
			if (end < get_textview_buffer_charcount(gtktext)) {
				if (g_unichar_isspace(get_text_index_whar(gtkaspell,
								 end + 1))
				||  g_unichar_ispunct(get_text_index_whar(gtkaspell,
								 end + 1))
				||  g_unichar_isdigit(get_text_index_whar(gtkaspell,
								 end + 1))) {
					/* end_quote = TRUE; */
					break;
				}
			}
			else {
				/* end_quote = TRUE; */
				break;
			}
		}
		else if (g_unichar_isspace(c) || g_unichar_ispunct(c) || g_unichar_isdigit(c))
				break;
	}
						
	if (pstart) 
		*pstart = start;
	if (pend) 
		*pend = end;

	if (buf) {
		if (end - start < buflen) {
			GtkTextIter iterstart, iterend;
			gchar *tmp;
			GtkTextBuffer *buffer = gtk_text_view_get_buffer(gtktext);
			gtk_text_buffer_get_iter_at_offset(buffer, &iterstart, start);
			gtk_text_buffer_get_iter_at_offset(buffer, &iterend, end);
			tmp = gtk_text_buffer_get_text(buffer, &iterstart, &iterend, FALSE);
			strncpy(buf, tmp, buflen-1);
			buf[buflen-1]='\0';
			g_free(tmp);
		} else
			return FALSE;
	}

	return TRUE;
}

static gboolean check_at(GtkAspell *gtkaspell, gint from_pos) 
{
	gint	      start, end;
	char buf[GTKASPELLWORDSIZE];

	cm_return_val_if_fail(from_pos >= 0, FALSE);
    
	if (!get_word_from_pos(gtkaspell, from_pos, buf, sizeof(buf), 
			       &start, &end))
		return FALSE;

	if (gtkaspell_misspelled_test(gtkaspell, buf)) {
		strncpy(gtkaspell->theword, (gchar *)buf, GTKASPELLWORDSIZE - 1);
		gtkaspell->theword[GTKASPELLWORDSIZE - 1] = 0;
		gtkaspell->start_pos  = start;
		gtkaspell->end_pos    = end;
		gtkaspell_free_suggestions_list(gtkaspell);

		change_color(gtkaspell, start, end, (gchar *)buf, &(gtkaspell->highlight));
		return TRUE;
	} else {
		change_color(gtkaspell, start, end, (gchar *)buf, NULL);
		return FALSE;
	}
}

static gboolean check_at_cb(gpointer data)
{
	GtkAspell *gtkaspell = (GtkAspell *)data;
	return check_at(gtkaspell, gtkaspell->start_pos);
}

static gboolean find_misspelled_cb(gpointer data, gboolean forward)
{
	GtkAspell *gtkaspell = (GtkAspell *)data;
	gboolean misspelled;
	gint pos;
	gint minpos;
	gint maxpos;
	gint direc = -1;
	
	minpos = 0;
	maxpos = gtkaspell->end_check_pos;

	if (forward) {
		minpos = -1;
		direc = 1;
		maxpos--;
	} 

	pos = get_textview_buffer_offset(gtkaspell->gtktext);
	gtkaspell->orig_pos = pos;
	while (iswordsep(get_text_index_whar(gtkaspell, pos)) &&
	       pos > minpos && pos <= maxpos)
		pos += direc;
	while (!(misspelled = check_at(gtkaspell, pos)) &&
	       pos > minpos && pos <= maxpos) {

		while (!iswordsep(get_text_index_whar(gtkaspell, pos)) &&
		       pos > minpos && pos <= maxpos)
			pos += direc;

		while (iswordsep(get_text_index_whar(gtkaspell, pos)) &&
		       pos > minpos && pos <= maxpos)
			pos += direc;
	}
	
	return misspelled;
}

gboolean gtkaspell_check_next_prev(GtkAspell *gtkaspell, gboolean forward)
{
	gboolean misspelled = gtkaspell->ctx.find_misspelled(gtkaspell->ctx.data,
							forward);
	if (misspelled) {
		GSList *list, *cur;
		GtkWidget *menu;
		misspelled_suggest(gtkaspell, gtkaspell->theword);

		if (forward)
			gtkaspell->orig_pos = gtkaspell->end_pos;

		gtkaspell->ctx.set_position(gtkaspell->ctx.data, gtkaspell->end_pos);
		
		/* only execute when in textview context */
		if (gtkaspell == (GtkAspell *)gtkaspell->ctx.data) {
			/* scroll line to window center */
			gtk_text_view_scroll_to_mark(gtkaspell->gtktext,
				gtk_text_buffer_get_insert(
					gtk_text_view_get_buffer(gtkaspell->gtktext)),
					0.0, TRUE, 0.0,	0.5);
			/* let textview recalculate coordinates (set_menu_pos) */
			while (gtk_events_pending ())
				gtk_main_iteration ();
		}

		list = make_sug_menu(gtkaspell);
		menu = gtk_menu_new();
		for (cur = list; cur; cur = cur->next)
			gtk_menu_shell_append(GTK_MENU_SHELL(menu), GTK_WIDGET(cur->data));
		g_slist_free(list);
		gtk_menu_popup(GTK_MENU(menu), NULL, NULL,
				gtkaspell->ctx.set_menu_pos,
				gtkaspell->ctx.data,
				0, GDK_CURRENT_TIME);
		g_signal_connect(G_OBJECT(menu), "deactivate",
					 G_CALLBACK(destroy_menu),
					 gtkaspell);
		g_signal_connect(G_OBJECT(menu),
			"key_press_event",
		       	G_CALLBACK(aspell_key_pressed),
		       	gtkaspell);


	} else {
		reset_theword_data(gtkaspell);

		alertpanel_notice(_("No misspelled word found."));
		gtkaspell->ctx.set_position(gtkaspell->ctx.data,
					gtkaspell->orig_pos);
	}

	return misspelled;
}

void gtkaspell_check_backwards(GtkAspell *gtkaspell)
{
	gtkaspell->continue_check = NULL;
	gtkaspell->end_check_pos =
		get_textview_buffer_charcount(gtkaspell->gtktext);
	gtkaspell_context_set(gtkaspell);
	gtkaspell_check_next_prev(gtkaspell, FALSE);
}

void gtkaspell_check_forwards_go(GtkAspell *gtkaspell)
{

	gtkaspell->continue_check = NULL;
	gtkaspell->end_check_pos =
		get_textview_buffer_charcount(gtkaspell->gtktext);
	gtkaspell_context_set(gtkaspell);
	gtkaspell_check_next_prev(gtkaspell, TRUE);
}

void gtkaspell_check_all(GtkAspell *gtkaspell)
{	
	GtkTextView *gtktext;
	gint start, end;
	GtkTextBuffer *buffer;
	GtkTextIter startiter, enditer;

	cm_return_if_fail(gtkaspell);
	cm_return_if_fail(gtkaspell->gtktext);

	gtktext = gtkaspell->gtktext;
	buffer = gtk_text_view_get_buffer(gtktext);
	gtk_text_buffer_get_selection_bounds(buffer, &startiter, &enditer);
	start = gtk_text_iter_get_offset(&startiter);
	end = gtk_text_iter_get_offset(&enditer);

	if (start == end) {
		start = 0;
		end = gtk_text_buffer_get_char_count(buffer);
	} else if (start > end) {
		gint tmp;

		tmp   = start;
		start = end;
		end   = tmp;
	}

	set_textview_buffer_offset(gtktext, start);

	gtkaspell->continue_check = continue_check;
	gtkaspell->end_check_pos  = end;

	gtkaspell_context_set(gtkaspell);
	gtkaspell->misspelled = gtkaspell_check_next_prev(gtkaspell, TRUE);
}	

static void continue_check(gpointer *data)
{
	GtkAspell *gtkaspell = (GtkAspell *) data;
	gint pos = get_textview_buffer_offset(gtkaspell->gtktext);
	if (pos < gtkaspell->end_check_pos && gtkaspell->misspelled)
		gtkaspell->misspelled = gtkaspell_check_next_prev(gtkaspell, TRUE);
	else
		gtkaspell->continue_check = NULL;
}

void gtkaspell_highlight_all(GtkAspell *gtkaspell) 
{
	guint     origpos;
	guint     pos = 0;
	guint     len;
	GtkTextView *gtktext;

	cm_return_if_fail(gtkaspell->gtkaspeller->speller);	

	gtktext = gtkaspell->gtktext;

	len = get_textview_buffer_charcount(gtktext);

	origpos = get_textview_buffer_offset(gtktext);

	while (pos < len) {
		while (pos < len &&
		       iswordsep(get_text_index_whar(gtkaspell, pos)))
			pos++;
		while (pos < len &&
		       !iswordsep(get_text_index_whar(gtkaspell, pos)))
			pos++;
		if (pos > 0)
			check_at(gtkaspell, pos - 1);
	}
	set_textview_buffer_offset(gtktext, origpos);
}

static void replace_with_supplied_word_cb(GtkWidget *w, GtkAspell *gtkaspell) 
{
	char *newword;
	GdkEvent *e= (GdkEvent *) gtk_get_current_event();

	newword = gtk_editable_get_chars(GTK_EDITABLE(gtkaspell->replace_entry),
					 0, -1);

	if (strcmp(newword, gtkaspell->theword)) {
		gtkaspell->ctx.replace_word(gtkaspell->ctx.data, newword);

		if ((e->type == GDK_KEY_PRESS &&
		    ((GdkEventKey *) e)->state & GDK_CONTROL_MASK)) {
			enchant_dict_store_replacement(gtkaspell->gtkaspeller->speller, 
					gtkaspell->theword, strlen(gtkaspell->theword),
					newword, strlen(newword));
		}
		gtkaspell->replace_entry = NULL;
	}

	g_free(newword);

	if (w && GTK_IS_DIALOG(w)) {
		gtk_widget_destroy(w);
	}

	set_point_continue(gtkaspell);
}


static void replace_word_cb(GtkWidget *w, gpointer data)
{
	const char *newword;
	GtkAspell *gtkaspell = (GtkAspell *) data;
	GdkEvent *e= (GdkEvent *) gtk_get_current_event();

	newword = gtk_label_get_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN((w)))));

	gtkaspell->ctx.replace_word(gtkaspell->ctx.data, newword);

	if ((e->type == GDK_KEY_PRESS && 
	    ((GdkEventKey *) e)->state & GDK_CONTROL_MASK) ||
	    (e->type == GDK_BUTTON_RELEASE && 
	     ((GdkEventButton *) e)->state & GDK_CONTROL_MASK)) {
		enchant_dict_store_replacement(gtkaspell->gtkaspeller->speller, 
				gtkaspell->theword, strlen(gtkaspell->theword),
				newword, strlen(newword));
	}

	gtk_menu_shell_deactivate(GTK_MENU_SHELL(w->parent));

	set_point_continue(gtkaspell);
}

void gtkaspell_block_check(GtkAspell *gtkaspell)
{
	GtkTextView *gtktext;
	
	if (gtkaspell == NULL)
		return;
		
	gtktext = gtkaspell->gtktext;
	g_signal_handlers_block_by_func(G_OBJECT(gtktext),
					 G_CALLBACK(key_press_cb),
					 gtkaspell);
	g_signal_handlers_block_by_func(G_OBJECT(gtktext),
					 G_CALLBACK(entry_insert_cb),
					 gtkaspell);
	g_signal_handlers_block_by_func(G_OBJECT(gtktext),
					 G_CALLBACK(entry_delete_cb),
					 gtkaspell);
}

void gtkaspell_unblock_check(GtkAspell *gtkaspell)
{
	GtkTextView *gtktext;

	if (gtkaspell == NULL)
		return;
		
	gtktext = gtkaspell->gtktext;
	g_signal_handlers_unblock_by_func(G_OBJECT(gtktext),
					 G_CALLBACK(key_press_cb),
					 gtkaspell);
	g_signal_handlers_unblock_by_func(G_OBJECT(gtktext),
					 G_CALLBACK(entry_insert_cb),
					 gtkaspell);
	g_signal_handlers_unblock_by_func(G_OBJECT(gtktext),
					 G_CALLBACK(entry_delete_cb),
					 gtkaspell);
}

static void replace_real_word(GtkAspell *gtkaspell, const gchar *newword)
{
	int		oldlen, newlen;
	gint		origpos;
	gint		pos;
	GtkTextView	*gtktext;
	GtkTextBuffer	*textbuf;
	GtkTextIter	startiter, enditer;
    
	if (!newword) return;

	gtktext = gtkaspell->gtktext;
	textbuf = gtk_text_view_get_buffer(gtktext);

	origpos = gtkaspell->orig_pos;
	pos     = origpos;
	oldlen  = gtkaspell->end_pos - gtkaspell->start_pos;

	newlen = strlen(newword); /* FIXME: multybyte characters? */

	gtkaspell_block_check(gtkaspell);

	gtk_text_buffer_get_iter_at_offset(textbuf, &startiter,
					   gtkaspell->start_pos);
	gtk_text_buffer_get_iter_at_offset(textbuf, &enditer,
					   gtkaspell->end_pos);
	g_signal_emit_by_name(G_OBJECT(textbuf), "delete-range",
			      &startiter, &enditer, gtkaspell);
	g_signal_emit_by_name(G_OBJECT(textbuf), "insert-text",
			      &startiter, newword, newlen, gtkaspell);

	gtkaspell_unblock_check(gtkaspell);

	/* Put the point and the position where we clicked with the mouse
	 * It seems to be a hack, as I must thaw,freeze,thaw the widget
	 * to let it update correctly the word insertion and then the
	 * point & position position. If not, SEGV after the first replacement
	 * If the new word ends before point, put the point at its end.
	 */

	if (origpos - gtkaspell->start_pos < oldlen &&
	    origpos - gtkaspell->start_pos >= 0) {
		/* Original point was in the word.
		 * Let it there unless point is going to be outside of the word
		 */
		if (origpos - gtkaspell->start_pos >= newlen) {
			pos = gtkaspell->start_pos + newlen;
		}
	}
	else if (origpos >= gtkaspell->end_pos) {
		/* move the position according to the change of length */
		pos = origpos + newlen - oldlen;
	}

	gtkaspell->end_pos = gtkaspell->start_pos + strlen(newword); /* FIXME: multibyte characters? */

	if (get_textview_buffer_charcount(gtktext) < pos)
		pos = get_textview_buffer_charcount(gtktext);
	gtkaspell->orig_pos = pos;

	set_textview_buffer_offset(gtktext, gtkaspell->orig_pos);
}

static void replace_real_word_cb(gpointer data, const gchar *newword)
{
	replace_real_word((GtkAspell *)data, newword);
}

/* Accept this word for this session */
static void add_word_to_session_cb(GtkWidget *w, gpointer data)
{
   	GtkAspell *gtkaspell = (GtkAspell *) data; 

	enchant_dict_add_to_session(gtkaspell->gtkaspeller->speller, gtkaspell->theword, strlen(gtkaspell->theword));

	gtkaspell->ctx.check_word(gtkaspell->ctx.data);
	gtkaspell_dict_changed(gtkaspell);

	gtk_menu_shell_deactivate(GTK_MENU_SHELL(GTK_WIDGET(w)->parent));

	set_point_continue(gtkaspell);
}

/* add_word_to_personal_cb() - add word to personal dict. */
static void add_word_to_personal_cb(GtkWidget *w, gpointer data)
{
   	GtkAspell *gtkaspell = (GtkAspell *) data; 

	enchant_dict_add_to_pwl(gtkaspell->gtkaspeller->speller, gtkaspell->theword, strlen(gtkaspell->theword));

	gtkaspell->ctx.check_word(gtkaspell->ctx.data);
	gtkaspell_dict_changed(gtkaspell);
	
	gtk_menu_shell_deactivate(GTK_MENU_SHELL(GTK_WIDGET(w)->parent));
	set_point_continue(gtkaspell);
}

static void check_with_alternate_cb(GtkWidget *w, gpointer data)
{
	GtkAspell *gtkaspell = (GtkAspell *)data;
	gint misspelled;

	gtk_menu_shell_deactivate(GTK_MENU_SHELL(GTK_WIDGET(w)->parent));

	gtkaspell_use_alternate_dict(gtkaspell);
	misspelled = gtkaspell->ctx.check_word(gtkaspell->ctx.data);

	if (!gtkaspell->continue_check) {

		gtkaspell->misspelled = misspelled;

		if (gtkaspell->misspelled) {
			GtkWidget *menu;
			GSList *list, *cur;
			misspelled_suggest(gtkaspell, gtkaspell->theword);

			gtkaspell->ctx.set_position(gtkaspell->ctx.data,
					    gtkaspell->end_pos);

			list = make_sug_menu(gtkaspell);
			menu = gtk_menu_new();
			for (cur = list; cur; cur = cur->next)
				gtk_menu_shell_append(GTK_MENU_SHELL(menu), GTK_WIDGET(cur->data));
			g_slist_free(list);
			gtk_menu_popup(GTK_MENU(menu), NULL, NULL,
				       gtkaspell->ctx.set_menu_pos,
				       gtkaspell->ctx.data, 0,
				       GDK_CURRENT_TIME);
			g_signal_connect(G_OBJECT(menu), "deactivate",
					 G_CALLBACK(destroy_menu),
					 gtkaspell);
			g_signal_connect(G_OBJECT(menu),
				"key_press_event",
			       	G_CALLBACK(aspell_key_pressed),
			       	gtkaspell);
			return;
		}
	} else
		gtkaspell->orig_pos = gtkaspell->start_pos;

	set_point_continue(gtkaspell);
}
	
static gboolean replace_key_pressed(GtkWidget *widget,
				   GdkEventKey *event,
				   GtkAspell *gtkaspell)
{
	if (event && event->keyval == GDK_KEY_Escape) {
		gtk_widget_destroy(widget);
		return TRUE;
	} else if (event && event->keyval == GDK_KEY_Return) {
		replace_with_supplied_word_cb(widget, gtkaspell);
		return TRUE;
	}
	return FALSE;
}
	
static void replace_with_create_dialog_cb(GtkWidget *w, gpointer data)
{
	static PangoFontDescription *font_desc;
	GtkWidget *dialog;
	GtkWidget *label;
	GtkWidget *hbox;
	GtkWidget *vbox;
	GtkWidget *entry;
	GtkWidget *ok_button;
	GtkWidget *cancel_button;
	GtkWidget *confirm_area;
	GtkWidget *icon;
	gchar *utf8buf, *thelabel;
	gint xx, yy;
	GtkAspell *gtkaspell = (GtkAspell *) data;

	gdk_window_get_origin((GTK_WIDGET(w)->parent)->window, &xx, &yy);

	gtk_menu_shell_deactivate(GTK_MENU_SHELL(GTK_WIDGET(w)->parent));

	dialog = gtk_dialog_new();

	gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE);
	gtk_window_set_title(GTK_WINDOW(dialog),_("Replace unknown word"));
	gtk_window_move(GTK_WINDOW(dialog), xx, yy);

	g_signal_connect_swapped(G_OBJECT(dialog), "destroy",
				 G_CALLBACK(gtk_widget_destroy), 
				 G_OBJECT(dialog));

	gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (dialog)->vbox), 14);
	hbox = gtk_hbox_new (FALSE, 12);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox,
			    FALSE, FALSE, 0);

	utf8buf  = g_strdup(gtkaspell->theword);

	thelabel = g_strdup_printf(_("<span weight=\"bold\" "
					"size=\"larger\">Replace \"%s\" with: </span>"), 
				   utf8buf);
	
	icon = gtk_image_new_from_stock(GTK_STOCK_DIALOG_QUESTION,
        				GTK_ICON_SIZE_DIALOG); 
	gtk_misc_set_alignment (GTK_MISC (icon), 0.5, 0.0);
	gtk_box_pack_start (GTK_BOX (hbox), icon, FALSE, FALSE, 0);
	
	vbox = gtk_vbox_new (FALSE, 12);
	gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);
	gtk_widget_show (vbox);
	
	label = gtk_label_new(thelabel);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	if (!font_desc) {
		gint size;

		size = pango_font_description_get_size
			(label->style->font_desc);
		font_desc = pango_font_description_new();
		pango_font_description_set_weight
			(font_desc, PANGO_WEIGHT_BOLD);
		pango_font_description_set_size
			(font_desc, size * PANGO_SCALE_LARGE);
	}
	if (font_desc)
		gtk_widget_modify_font(label, font_desc);
	g_free(thelabel);
	
	entry = gtk_entry_new();
	gtkaspell->replace_entry = entry;
	gtk_entry_set_text(GTK_ENTRY(entry), utf8buf);
	gtk_editable_select_region(GTK_EDITABLE(entry), 0, -1);
	g_signal_connect(G_OBJECT(dialog),
			"key_press_event",
		       	G_CALLBACK(replace_key_pressed), gtkaspell);
	gtk_box_pack_start(GTK_BOX(vbox), entry, FALSE, FALSE, 0);
	g_free(utf8buf);  

	label = gtk_label_new(_("Holding down Control key while pressing "
				"Enter\nwill learn from mistake.\n"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	gtk_widget_show(label);

	gtkut_stock_button_set_create(&confirm_area,
				      &cancel_button, GTK_STOCK_CANCEL,
				      &ok_button, GTK_STOCK_OK,
				      NULL, NULL);

	gtk_box_pack_end(GTK_BOX(GTK_DIALOG(dialog)->action_area),
			 confirm_area, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(confirm_area), 5);

	g_signal_connect(G_OBJECT(ok_button), "clicked",
			 G_CALLBACK(replace_with_supplied_word_cb), 
			 gtkaspell);
	g_signal_connect_swapped(G_OBJECT(ok_button), "clicked",
				   G_CALLBACK(gtk_widget_destroy), 
				   G_OBJECT(dialog));

	g_signal_connect_swapped(G_OBJECT(cancel_button), "clicked",
				 G_CALLBACK(gtk_widget_destroy), 
				 G_OBJECT(dialog));

	gtk_widget_grab_focus(entry);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	gtk_widget_show_all(dialog);
}

static void gtkaspell_uncheck_all(GtkAspell * gtkaspell) 
{
	GtkTextView *gtktext;
	GtkTextBuffer *buffer;
	GtkTextIter startiter, enditer;
	
	gtktext = gtkaspell->gtktext;

	buffer = gtk_text_view_get_buffer(gtktext);
	gtk_text_buffer_get_iter_at_offset(buffer, &startiter, 0);
	gtk_text_buffer_get_iter_at_offset(buffer, &enditer,
				   get_textview_buffer_charcount(gtktext)-1);
	gtk_text_buffer_remove_tag_by_name(buffer, "misspelled",
					   &startiter, &enditer);
}

static void toggle_check_while_typing_cb(GtkWidget *w, gpointer data)
{
	GtkAspell *gtkaspell = (GtkAspell *) data;

	gtkaspell->check_while_typing = gtkaspell->check_while_typing == FALSE;

	if (!gtkaspell->check_while_typing)
		gtkaspell_uncheck_all(gtkaspell);
	if (gtkaspell->menu_changed_cb)
		gtkaspell->menu_changed_cb(gtkaspell->menu_changed_data);
}

static GSList *create_empty_dictionary_list(void)
{
	GSList *list = NULL;
	Dictionary *dict;

	dict = g_new0(Dictionary, 1);
	dict->fullname = g_strdup(_("None"));
	dict->dictname = NULL;

	return g_slist_append(list, dict);
}

static void list_dict_cb(const char * const lang_tag,
		 const char * const provider_name,
		 const char * const provider_desc,
		 const char * const provider_file,
		 void * data)
{
	GSList **list = (GSList **)data;
	Dictionary *dict = g_new0(Dictionary, 1);
	dict->fullname = g_strdup(lang_tag);
	dict->dictname = g_strdup(lang_tag);

	if (g_slist_find_custom(*list, dict, 
			(GCompareFunc) compare_dict) == NULL) {
		debug_print("Aspell: found dictionary %s %s\n", dict->fullname,
				dict->dictname);
		*list = g_slist_insert_sorted(*list, dict,
				(GCompareFunc) compare_dict);
	} else {
		dictionary_delete(dict);
	}
}

/* gtkaspell_get_dictionary_list() - returns list of dictionary names */
static GSList *gtkaspell_get_dictionary_list(gint refresh)
{
	GSList *list;
	EnchantBroker *broker;

	if (!gtkaspellcheckers)
		gtkaspell_checkers_init();

	if (gtkaspellcheckers->dictionary_list && !refresh)
		return gtkaspellcheckers->dictionary_list;
	else
		gtkaspell_free_dictionary_list(
				gtkaspellcheckers->dictionary_list);
	list = NULL;

	broker = enchant_broker_init();

	enchant_broker_list_dicts(broker, list_dict_cb, &list);

	enchant_broker_free(broker);

        if (list == NULL){
		
		debug_print("Aspell: error when searching for dictionaries: "
			      "No dictionary found.\n");
		list = create_empty_dictionary_list();
	}

	gtkaspellcheckers->dictionary_list = list;

	return list;
}

static void gtkaspell_free_dictionary_list(GSList *list)
{
	Dictionary *dict;
	GSList *walk;
	for (walk = list; walk != NULL; walk = g_slist_next(walk))
		if (walk->data) {
			dict = (Dictionary *) walk->data;
			dictionary_delete(dict);
		}				
	g_slist_free(list);
}

GtkTreeModel *gtkaspell_dictionary_store_new_with_refresh(gboolean refresh)
{
	GSList *dict_list, *tmp;
	GtkListStore *store;
	GtkTreeIter iter;
	Dictionary *dict;

	dict_list = gtkaspell_get_dictionary_list(refresh);
	cm_return_val_if_fail(dict_list, NULL);

	store = gtk_list_store_new(SET_GTKASPELL_SIZE,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   -1);
	
	for (tmp = dict_list; tmp != NULL; tmp = g_slist_next(tmp)) {
		dict = (Dictionary *) tmp->data;
		
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
				   SET_GTKASPELL_NAME, dict->dictname,
				   SET_GTKASPELL_FULLNAME, dict->fullname,
				   -1);
	}

	return GTK_TREE_MODEL(store);
}

GtkTreeModel *gtkaspell_dictionary_store_new(void)
{
	return gtkaspell_dictionary_store_new_with_refresh
		(TRUE);
}

GtkWidget *gtkaspell_dictionary_combo_new(const gboolean refresh)
{
	GtkWidget *combo;
	GtkCellRenderer *renderer;

	combo = gtk_combo_box_new_with_model(
			gtkaspell_dictionary_store_new_with_refresh(refresh));
	gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);   
	gtk_widget_show(combo);
	
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combo), renderer, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combo),renderer,
					"text",	SET_GTKASPELL_NAME, NULL);
	
	return combo;
} 

gchar *gtkaspell_get_dictionary_menu_active_item(GtkComboBox *combo)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	gchar *dict_fullname = NULL;
	
	cm_return_val_if_fail(GTK_IS_COMBO_BOX(combo), NULL);
	cm_return_val_if_fail(gtk_combo_box_get_active_iter(combo, &iter), NULL);
	
	model = gtk_combo_box_get_model(combo);
	if(model == NULL)
		return NULL;
	
	gtk_tree_model_get(model, &iter,
			   SET_GTKASPELL_FULLNAME, &dict_fullname,
			   -1);

        return dict_fullname;
}

gint gtkaspell_set_dictionary_menu_active_item(GtkComboBox *combo,
					       const gchar *dictionary)
{
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *dict_name = NULL;
	
	cm_return_val_if_fail(combo != NULL, 0);
	cm_return_val_if_fail(dictionary != NULL, 0);
	cm_return_val_if_fail(GTK_IS_COMBO_BOX(combo), 0);

	if((model = gtk_combo_box_get_model(combo)) == NULL)
		return 0;
	if((gtk_tree_model_get_iter_first(model, &iter)) == FALSE)
		return 0;
	
	do {
		gtk_tree_model_get(model, &iter,
				   SET_GTKASPELL_FULLNAME, &dict_name,
				   -1);
		
		if ((dict_name != NULL) && !strcmp2(dict_name, dictionary)) {
			gtk_combo_box_set_active_iter(combo, &iter);
			g_free(dict_name);
			return 1;
		}
		
		g_free(dict_name);
		
	} while ((gtk_tree_model_iter_next(model, &iter)) == TRUE);
	
	return 0;
}

void gtkaspell_use_alternate_dict(GtkAspell *gtkaspell)
{
	GtkAspeller *tmp;

	tmp = gtkaspell->gtkaspeller;
	gtkaspell->gtkaspeller = gtkaspell->alternate_speller;
	gtkaspell->alternate_speller = tmp;
}

static void destroy_menu(GtkWidget *widget,
			     gpointer user_data) {
	GtkAspell *gtkaspell = (GtkAspell *)user_data;

	if (gtkaspell->accel_group) {
		gtk_window_remove_accel_group(GTK_WINDOW(gtkaspell->parent_window), 
				gtkaspell->accel_group);
		gtkaspell->accel_group = NULL;
	}
}

static gboolean aspell_key_pressed(GtkWidget *widget,
				   GdkEventKey *event,
				   GtkAspell *gtkaspell)
{
	if (event && (isascii(event->keyval) || event->keyval == GDK_KEY_Return)) {
		gtk_accel_groups_activate(
				G_OBJECT(gtkaspell->parent_window),
				event->keyval, event->state);
	} else if (event && event->keyval == GDK_KEY_Escape) {
		destroy_menu(NULL, gtkaspell);
	}
	return FALSE;
}

/* Create a paged submenu with choice of available dictionaries */
static GtkWidget *make_dictionary_list_submenu(GtkAspell *gtkaspell)
{
	GtkWidget *menu, *curmenu, *moremenu, *item;
	int count = 2;
	Dictionary *dict;
	GSList *tmp;

	/* Dict list */
	if (gtkaspellcheckers->dictionary_list == NULL)
		gtkaspell_get_dictionary_list(FALSE);

	menu = gtk_menu_new();
	curmenu = menu;

	item = gtk_menu_item_new_with_label(_("Change to..."));
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);

	item = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);

	for (tmp = gtkaspellcheckers->dictionary_list; tmp != NULL; 
			tmp = g_slist_next(tmp)) {
		if (count == MENUCOUNT) {

			moremenu = gtk_menu_new();
			item = gtk_menu_item_new_with_label(_("More..."));
			gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), 
						  moremenu);

			gtk_menu_shell_append(GTK_MENU_SHELL(curmenu), item);
			curmenu = moremenu;
			count = 0;
		}
		dict = (Dictionary *) tmp->data;
		item = gtk_check_menu_item_new_with_label(dict->fullname);
		g_object_set_data(G_OBJECT(item), "dict_name",
				  dict->dictname); 
		if (strcmp2(dict->fullname,
		    gtkaspell->gtkaspeller->dictionary->fullname))
			gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), FALSE);
		else {
			gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(item),
						 FALSE);
		}
		g_signal_connect(G_OBJECT(item), "activate",
				 G_CALLBACK(change_dict_cb),
				 gtkaspell);
		gtk_menu_shell_append(GTK_MENU_SHELL(curmenu), item);

		count++;
	}

	gtk_widget_show_all(menu);
	return menu;
}

/* make_sug_menu() - Add menus to accept this word for this session 
 * and to add it to personal dictionary 
 */
static GSList *make_sug_menu(GtkAspell *gtkaspell) 
{
	GtkWidget 	*item, *submenu;
	char	*caption;
	GtkAccelGroup 	*accel;
	GList 		*l = gtkaspell->suggestions_list;
	gchar		*utf8buf;
	GSList *list = NULL;

	if (l == NULL)
		return NULL;

	accel = gtk_accel_group_new();

	if (gtkaspell->accel_group) {
		gtk_window_remove_accel_group(GTK_WINDOW(gtkaspell->parent_window), 
				gtkaspell->accel_group);
		gtkaspell->accel_group = NULL;
	}

	utf8buf  = g_strdup(l->data);
	caption = g_strdup_printf(_("\"%s\" unknown in dictionary '%s'"), 
				  utf8buf, 
				  gtkaspell->gtkaspeller->dictionary->dictname);
	item = gtk_menu_item_new_with_label(caption);
	submenu = make_dictionary_list_submenu(gtkaspell);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);
	g_free(utf8buf);
	gtk_widget_show(item);
	list = g_slist_append(list, item);
	gtk_misc_set_alignment(GTK_MISC(gtk_bin_get_child(GTK_BIN((item)))), 0.5, 0.5);
	g_free(caption);

	item = gtk_menu_item_new();
	gtk_widget_show(item);
	list = g_slist_append(list, item);

	item = gtk_menu_item_new_with_label(_("Accept in this session"));
	gtk_widget_show(item);
	list = g_slist_append(list, item);
        g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(add_word_to_session_cb), 
			 gtkaspell);
	gtk_widget_add_accelerator(item, "activate", accel, GDK_KEY_space,
				   GDK_CONTROL_MASK,
				   GTK_ACCEL_LOCKED | GTK_ACCEL_VISIBLE);

	item = gtk_menu_item_new_with_label(_("Add to personal dictionary"));
	gtk_widget_show(item);
	list = g_slist_append(list, item);
        g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(add_word_to_personal_cb), 
			 gtkaspell);
	gtk_widget_add_accelerator(item, "activate", accel, GDK_KEY_Return,
				   GDK_CONTROL_MASK,
				   GTK_ACCEL_LOCKED | GTK_ACCEL_VISIBLE);

        item = gtk_menu_item_new_with_label(_("Replace with..."));
	gtk_widget_show(item);
	list = g_slist_append(list, item);
        g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(replace_with_create_dialog_cb), 
			 gtkaspell);
	gtk_widget_add_accelerator(item, "activate", accel, GDK_KEY_R, 0,
				   GTK_ACCEL_LOCKED | GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(item, "activate", accel, GDK_KEY_R, 
				   GDK_CONTROL_MASK,
				   GTK_ACCEL_LOCKED);

	if (gtkaspell->use_alternate && gtkaspell->alternate_speller) {
		caption = g_strdup_printf(_("Check with %s"), 
			gtkaspell->alternate_speller->dictionary->dictname);
		item = gtk_menu_item_new_with_label(caption);
		g_free(caption);
		gtk_widget_show(item);
		list = g_slist_append(list, item);
		g_signal_connect(G_OBJECT(item), "activate",
				 G_CALLBACK(check_with_alternate_cb),
				 gtkaspell);
		gtk_widget_add_accelerator(item, "activate", accel, GDK_KEY_X, 0,
					   GTK_ACCEL_LOCKED | GTK_ACCEL_VISIBLE);
		gtk_widget_add_accelerator(item, "activate", accel, GDK_KEY_X, 
					   GDK_CONTROL_MASK,
					   GTK_ACCEL_LOCKED);
	}

	item = gtk_menu_item_new();
        gtk_widget_show(item);
        list = g_slist_append(list, item);

	l = l->next;
        if (l == NULL) {
		item = gtk_menu_item_new_with_label(_("(no suggestions)"));
		gtk_widget_show(item);
		list = g_slist_append(list, item);
        } else {
		GtkWidget *curmenu = NULL;
		gint count = 0;
		
		do {
			if (count == MENUCOUNT) {
				count -= MENUCOUNT;

				item = gtk_menu_item_new_with_label(_("More..."));
				gtk_widget_show(item);
				if (curmenu)
					gtk_menu_shell_append(GTK_MENU_SHELL(curmenu), item);
				else 
					list = g_slist_append(list, item);

				curmenu = gtk_menu_new();
				gtk_menu_item_set_submenu(GTK_MENU_ITEM(item),
							  curmenu);
			}

			utf8buf  = g_strdup(l->data);

			item = gtk_menu_item_new_with_label(utf8buf);
			g_free(utf8buf);
			gtk_widget_show(item);
			if (curmenu == NULL) {
				list = g_slist_append(list, item);
			} else {
				gtk_menu_shell_append(GTK_MENU_SHELL(curmenu), item);
			}
			g_signal_connect(G_OBJECT(item), "activate",
					 G_CALLBACK(replace_word_cb),
					 gtkaspell);

			if (curmenu == NULL && count < MENUCOUNT) {
				gtk_widget_add_accelerator(item, "activate",
							   accel,
							   GDK_KEY_A + count, 0,
							   GTK_ACCEL_LOCKED | 
							   GTK_ACCEL_VISIBLE);
				gtk_widget_add_accelerator(item, "activate", 
							   accel,
							   GDK_KEY_A + count, 
							   GDK_CONTROL_MASK,
							   GTK_ACCEL_LOCKED);
				}

			count++;

		} while ((l = l->next) != NULL);
	}

	gtk_window_add_accel_group
		(GTK_WINDOW(gtkaspell->parent_window),
		 accel);
	gtkaspell->accel_group = accel;

	return list;
}

static GSList *populate_submenu(GtkAspell *gtkaspell)
{
	GtkWidget *item, *submenu;
	gchar *dictname;
	GtkAspeller *gtkaspeller = NULL;
	GSList *list = NULL;

	if (!gtkaspell)
		return NULL;

	gtkaspeller = gtkaspell->gtkaspeller;
	dictname = g_strdup_printf(_("Dictionary: %s"),
				   gtkaspeller->dictionary->dictname);
	item = gtk_menu_item_new_with_label(dictname);
	gtk_misc_set_alignment(GTK_MISC(gtk_bin_get_child(GTK_BIN((item)))), 0.5, 0.5);
	g_free(dictname);
	submenu = make_dictionary_list_submenu(gtkaspell);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);
	gtk_widget_show(item);
	list = g_slist_append(list, item);

	item = gtk_menu_item_new();
        gtk_widget_show(item);
        list = g_slist_append(list, item);
		
	if (gtkaspell->use_alternate && gtkaspell->alternate_speller) {
		dictname = g_strdup_printf(_("Use alternate (%s)"), 
				gtkaspell->alternate_speller->dictionary->dictname);
		item = gtk_menu_item_new_with_label(dictname);
		g_free(dictname);
		g_signal_connect(G_OBJECT(item), "activate",
				 G_CALLBACK(switch_to_alternate_cb),
				 gtkaspell);
		gtk_widget_show(item);
		list = g_slist_append(list, item);
	}

	item = gtk_check_menu_item_new_with_label(_("Use both dictionaries"));
	if (gtkaspell->use_both_dicts) {
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), TRUE);
	} 
	g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(set_use_both_cb),
			 gtkaspell);
	gtk_widget_show(item);
	list = g_slist_append(list, item);
	
	item = gtk_menu_item_new();
        gtk_widget_show(item);
        list = g_slist_append(list, item);
	
	item = gtk_check_menu_item_new_with_label(_("Check while typing"));
	if (gtkaspell->check_while_typing)
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), TRUE);
	else	
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), FALSE);
	g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(toggle_check_while_typing_cb),
			 gtkaspell);
	gtk_widget_show(item);
	list = g_slist_append(list, item);

	return list;
}

GSList *gtkaspell_make_config_menu(GtkAspell *gtkaspell)
{
	return populate_submenu(gtkaspell);
}

static void set_menu_pos(GtkMenu *menu, gint *x, gint *y, 
			 gboolean *push_in, gpointer data)
{
	GtkAspell 	*gtkaspell = (GtkAspell *) data;
	gint 		 xx = 0, yy = 0;
	gint 		 sx,     sy;
	gint 		 wx,     wy;
	GtkTextView 	*text = GTK_TEXT_VIEW(gtkaspell->gtktext);
	GtkTextBuffer	*textbuf;
	GtkTextIter	 iter;
	GdkRectangle	 rect;
	GtkRequisition 	 r;

	textbuf = gtk_text_view_get_buffer(gtkaspell->gtktext);
	gtk_text_buffer_get_iter_at_mark(textbuf, &iter,
					 gtk_text_buffer_get_insert(textbuf));
	gtk_text_view_get_iter_location(gtkaspell->gtktext, &iter, &rect);
	gtk_text_view_buffer_to_window_coords(text, GTK_TEXT_WINDOW_TEXT,
					      rect.x, rect.y, 
					      &rect.x, &rect.y);

	gdk_window_get_origin(GTK_WIDGET(gtkaspell->gtktext)->window, &xx, &yy);

	sx = gdk_screen_width();
	sy = gdk_screen_height();

	gtk_widget_get_child_requisition(GTK_WIDGET(menu), &r);

	wx =  r.width;
	wy =  r.height;

	*x = rect.x + xx + 8;

	*y = rect.y + rect.height + yy;

	if (*x + wx > sx)
		*x = sx - wx;
	if (*y + wy > sy)
		*y = *y - wy - 10;
}

/* change the current dictionary of gtkaspell
   - if always_set_alt_dict is set, the alternate dict is unconditionally set to the former
     current dictionary (common use: from menu callbacks)
   - if always_set_alt_dict is NOT set, the alternate dict will be set to the former
     current dictionary only if there is no alternate dictionary already set
     (this is when we need to set the current dictionary then the alternate one
     when creating a compose window, from the account and folder settings)
*/
gboolean gtkaspell_change_dict(GtkAspell *gtkaspell, const gchar *dictionary,
							 gboolean always_set_alt_dict)
{
	Dictionary 	*dict;       
	GtkAspeller 	*gtkaspeller;

	cm_return_val_if_fail(gtkaspell, FALSE);
	cm_return_val_if_fail(dictionary, FALSE);
  
	dict = g_new0(Dictionary, 1);
	
	if (strrchr(dictionary, '/')) {
		dict->fullname = g_strdup(strrchr(dictionary, '/')+1);
		dict->dictname = g_strdup(strrchr(dictionary, '/')+1);
	} else {
		dict->fullname = g_strdup(dictionary);
		dict->dictname = g_strdup(dictionary);
	}

	if (dict->fullname && strchr(dict->fullname, '-')) {
		*(strchr(dict->fullname, '-')) = '\0';
		*(strchr(dict->dictname, '-')) = '\0';
	}

	if (!dict->fullname || !(*dict->fullname)) {
		dictionary_delete(dict);
		return FALSE;
	}
	gtkaspeller = gtkaspeller_new(dict);

	if (!gtkaspeller) {
		alertpanel_warning(_("The spell checker could not change dictionary.\n%s"), 
					  gtkaspellcheckers->error_message);
	} else {
		if (gtkaspell->use_alternate) {
			if (gtkaspell->alternate_speller) {
				if (always_set_alt_dict) {
					gtkaspeller_delete(gtkaspell->alternate_speller);
					gtkaspell->alternate_speller = gtkaspell->gtkaspeller;
				} else
					gtkaspeller_delete(gtkaspell->gtkaspeller);
			} else
				/* should never be reached as the dicts are always set
				   to a default value */
				gtkaspell->alternate_speller = gtkaspell->gtkaspeller;
		} else
			gtkaspeller_delete(gtkaspell->gtkaspeller);

		gtkaspell->gtkaspeller = gtkaspeller;
	}
	
	dictionary_delete(dict);

	return TRUE;	
}

/* change the alternate dictionary of gtkaspell (doesn't affect the default dictionary) */
gboolean gtkaspell_change_alt_dict(GtkAspell *gtkaspell, const gchar *alt_dictionary)
{
	Dictionary 	*dict;       
	GtkAspeller 	*gtkaspeller;

	cm_return_val_if_fail(gtkaspell, FALSE);
	cm_return_val_if_fail(alt_dictionary, FALSE);
  
	dict = g_new0(Dictionary, 1);
	if (strrchr(alt_dictionary, '/')) {
		dict->fullname = g_strdup(strrchr(alt_dictionary, '/')+1);
		dict->dictname = g_strdup(strrchr(alt_dictionary, '/')+1);
	} else {
		dict->fullname = g_strdup(alt_dictionary);
		dict->dictname = g_strdup(alt_dictionary);
	}

	if (dict->fullname && strchr(dict->fullname, '-')) {
		*(strchr(dict->fullname, '-')) = '\0';
		*(strchr(dict->dictname, '-')) = '\0';
	}

	if (!dict->fullname || !(*dict->fullname)) {
		dictionary_delete(dict);
		return FALSE;
	}

	gtkaspeller = gtkaspeller_new(dict);

	if (!gtkaspeller) {
		alertpanel_warning(_("The spell checker could not change the alternate dictionary.\n%s"), 
					  gtkaspellcheckers->error_message);
	} else {
		if (gtkaspell->alternate_speller)
			gtkaspeller_delete(gtkaspell->alternate_speller);
		gtkaspell->alternate_speller = gtkaspeller;
	}
	
	dictionary_delete(dict);

	return TRUE;	
}

/* Menu call backs */

/* change_dict_cb() - Menu callback : change dict */
static void change_dict_cb(GtkWidget *w, GtkAspell *gtkaspell)
{
	gchar		*fullname;
  
        fullname = (gchar *) g_object_get_data(G_OBJECT(w), "dict_name");
	
	if (!strcmp2(fullname, _("None")))
		return;

	gtkaspell_change_dict(gtkaspell, fullname, TRUE);
	gtkaspell_dict_changed(gtkaspell);

	if (gtkaspell->menu_changed_cb)
		gtkaspell->menu_changed_cb(gtkaspell->menu_changed_data);
}

static void switch_to_alternate_cb(GtkWidget *w,
				   gpointer data)
{
	GtkAspell *gtkaspell = (GtkAspell *) data;
	gtkaspell_use_alternate_dict(gtkaspell);
	gtkaspell_dict_changed(gtkaspell);
	
	if (gtkaspell->menu_changed_cb)
		gtkaspell->menu_changed_cb(gtkaspell->menu_changed_data);
}

/* Misc. helper functions */

static void set_point_continue(GtkAspell *gtkaspell)
{
	gtkaspell->ctx.set_position(gtkaspell->ctx.data, gtkaspell->orig_pos);

	if (gtkaspell->continue_check)
		gtkaspell->continue_check((gpointer *) gtkaspell->ctx.data);
}

static void allocate_color(GtkAspell *gtkaspell, gint rgbvalue)
{
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(gtkaspell->gtktext);
	GdkColor *color = &(gtkaspell->highlight);

	/* Shameless copy from Sylpheed's gtkutils.c */
	color->pixel = 0L;
	color->red   = (int) (((gdouble)((rgbvalue & 0xff0000) >> 16) / 255.0)
			* 65535.0);
	color->green = (int) (((gdouble)((rgbvalue & 0x00ff00) >>  8) / 255.0)
			* 65535.0);
	color->blue  = (int) (((gdouble) (rgbvalue & 0x0000ff)        / 255.0)
			* 65535.0);

	if (rgbvalue != 0)
		gtk_text_buffer_create_tag(buffer, "misspelled",
				   "foreground-gdk", color, NULL);
	else
		gtk_text_buffer_create_tag(buffer, "misspelled",
				   "underline", PANGO_UNDERLINE_ERROR, NULL);

}

static void change_color(GtkAspell * gtkaspell, 
			 gint start, gint end,
			 gchar *newtext,
                         GdkColor *color) 
{
	GtkTextView *gtktext;
	GtkTextBuffer *buffer;
	GtkTextIter startiter, enditer;

	if (start > end)
		return;
    
	gtktext = gtkaspell->gtktext;
    
	buffer = gtk_text_view_get_buffer(gtktext);
	gtk_text_buffer_get_iter_at_offset(buffer, &startiter, start);
	gtk_text_buffer_get_iter_at_offset(buffer, &enditer, end);
	if (color)
		gtk_text_buffer_apply_tag_by_name(buffer, "misspelled",
						  &startiter, &enditer);
	else {
		gtk_text_iter_forward_char(&enditer);
		gtk_text_buffer_remove_tag_by_name(buffer, "misspelled",
						   &startiter, &enditer);
	}
}

/* compare_dict () - compare 2 dict names */
static gint compare_dict(Dictionary *a, Dictionary *b)
{
	guint   aparts = 0,  bparts = 0;
	guint  	i;

	for (i=0; i < strlen(a->dictname); i++)
		if (a->dictname[i] == '-')
			aparts++;
	for (i=0; i < strlen(b->dictname); i++)
		if (b->dictname[i] == '-')
			bparts++;

	if (aparts != bparts) 
		return (aparts < bparts) ? -1 : +1;
	else {
		gint compare;
		compare = strcmp2(a->dictname, b->dictname);
		if (!compare)
			compare = strcmp2(a->fullname, b->fullname);
		return compare;
	}
}

static void dictionary_delete(Dictionary *dict)
{
	g_free(dict->fullname);
	g_free(dict->dictname);
	g_free(dict);
}

static Dictionary *dictionary_dup(const Dictionary *dict)
{
	Dictionary *dict2;

	dict2 = g_new(Dictionary, 1); 

	dict2->fullname = g_strdup(dict->fullname);
	dict2->dictname = g_strdup(dict->dictname);

	return dict2;
}

void gtkaspell_free_suggestions_list(GtkAspell *gtkaspell)
{
	GList *list;

	for (list = gtkaspell->suggestions_list; list != NULL;
	     list = list->next)
		g_free(list->data);

	g_list_free(gtkaspell->suggestions_list);
	
	gtkaspell->max_sug          = -1;
	gtkaspell->suggestions_list = NULL;
}

static void reset_theword_data(GtkAspell *gtkaspell)
{
	gtkaspell->start_pos     =  0;
	gtkaspell->end_pos       =  0;
	gtkaspell->theword[0]    =  0;
	gtkaspell->max_sug       = -1;

	gtkaspell_free_suggestions_list(gtkaspell);
}

static void free_checkers(gpointer elt, gpointer data)
{
	GtkAspeller *gtkaspeller = elt;

	cm_return_if_fail(gtkaspeller);

	gtkaspeller_real_delete(gtkaspeller);
}

gchar *gtkaspell_get_default_dictionary(GtkAspell *gtkaspell)
{
	if (gtkaspell && gtkaspell->gtkaspeller &&
			gtkaspell->gtkaspeller->dictionary)
		return gtkaspell->gtkaspeller->dictionary->dictname;
	else
		return NULL;
}
#endif
