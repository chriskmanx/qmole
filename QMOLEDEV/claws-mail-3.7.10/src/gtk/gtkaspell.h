/*
    Stuphead: (C) 2000,2001 Grigroy Bakunov, Sergey Pinaev
 */
/* gtkaspell - a spell-checking addon for GtkText
 * Copyright (c) 2001-2002 Melvin Hadasht
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
 * Adapted by the Claws Mail Team.
 */

/*
 *  Adapted for pspell (c) 2001-2002 Melvin Hadasht
 *  Adapted for GNU/aspell (c) 2002 Melvin Hadasht
 *
 */

#ifndef __GTKENCHANT_H__
#define __GTKENCHANT_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef USE_ENCHANT

#include <gtk/gtk.h>
#include <enchant.h>

#define GTKASPELLWORDSIZE 1024

typedef struct _Dictionary {
	gchar *fullname;
	gchar *dictname;
} Dictionary;

typedef struct _GtkAspeller {
	Dictionary	*dictionary;
	EnchantBroker 	*broker;
	EnchantDict 	*speller;
} GtkAspeller;

typedef void (*ContCheckFunc) (gpointer *gtkaspell);

struct _WidgetContext
{
	void		(*set_position)(gpointer data, gint pos);
	void		(*set_menu_pos)(GtkMenu *menu, gint *x, gint *y,
				gboolean *push_in, gpointer user_data);
	gboolean	(*find_misspelled)(gpointer data, gboolean forward);
	gboolean	(*check_word)(gpointer data);
	void		(*replace_word)(gpointer data, const gchar *newword);
	gpointer	*data; 
};

typedef struct _WidgetContext WidgetContext;

struct _GtkAspell
{
	GtkAspeller	*gtkaspeller;
	GtkAspeller	*alternate_speller;
	gchar 		 theword[GTKASPELLWORDSIZE];
	gint  		 start_pos;
	gint  		 end_pos;
        gint 		 orig_pos;
	gint		 end_check_pos;
	gboolean	 misspelled;
	gboolean	 check_while_typing;
	gboolean	 recheck_when_changing_dict;
	gboolean	 use_alternate;
	gboolean	 use_both_dicts;

	ContCheckFunc 	 continue_check; 

	GtkWidget	*replace_entry;
	GtkWidget 	*parent_window;

	gint		 max_sug;
	GList		*suggestions_list;

	GtkTextView	*gtktext;
	GdkColor 	 highlight;
	GtkAccelGroup	*accel_group;
	void		(*dict_changed_cb)(void *data);
	void 		(*menu_changed_cb)(void *data);
	void 		*menu_changed_data;
	
	WidgetContext	ctx;
};

typedef struct _GtkAspell GtkAspell;


void		gtkaspell_checkers_init		(void);

void		gtkaspell_checkers_quit		(void);

const char * 	gtkaspell_checkers_strerror	(void);

void 		gtkaspell_checkers_reset_error	(void);

GtkAspell*	gtkaspell_new			(const gchar *dictionary, 
						 const gchar *alt_dictionary, 
						 const gchar *encoding,
						 gint  misspelled_color,
						 gboolean check_while_typing,
						 gboolean recheck_when_changing_dict,
						 gboolean use_alternate,  
						 gboolean use_both_dicts,  
						 GtkTextView *gtktext,
						 GtkWindow *parent_win,
						 void (*dict_changed_cd)(void *data), 
						 void (*spell_menu_cb)(void *data),
						 void *data);

void 		gtkaspell_delete		(GtkAspell *gtkaspell); 


gboolean	gtkaspell_change_dict		(GtkAspell *gtkaspell,
    						 const gchar* dictionary,
							 gboolean always_set_alt_dict);

gboolean	gtkaspell_change_alt_dict	(GtkAspell *gtkaspell,
    						 const gchar* alt_dictionary);
void		gtkaspell_use_alternate_dict	(GtkAspell *gtkaspell);

gboolean 	gtkaspell_check_next_prev	(GtkAspell *gtkaspell,
						 gboolean forward);
void 		gtkaspell_check_forwards_go	(GtkAspell *gtkaspell);
void 		gtkaspell_check_backwards	(GtkAspell *gtkaspell);

void 		gtkaspell_check_all		(GtkAspell *gtkaspell);
void 		gtkaspell_highlight_all		(GtkAspell *gtkaspell);

GtkWidget*	gtkaspell_dictionary_combo_new	(const gboolean refresh);

GtkTreeModel*	gtkaspell_dictionary_store_new	(void);
GtkTreeModel*	gtkaspell_dictionary_store_new_with_refresh
							(gboolean     refresh);

gchar*		gtkaspell_get_dictionary_menu_active_item
							(GtkComboBox *combo);
gint		gtkaspell_set_dictionary_menu_active_item
							(GtkComboBox *combo, 
							 const gchar *dictionary);

GSList*		gtkaspell_make_config_menu		(GtkAspell	*gtkaspell);

gchar*		gtkaspell_get_default_dictionary	(GtkAspell *gtkaspell);

void		gtkaspell_make_context_menu		(GtkMenu	*menu,
							 GtkAspell	*gtkaspell);

int 		gtkaspell_misspelled_test		(GtkAspell *gtkaspell,
							 char *word);
void		gtkaspell_dict_changed			(GtkAspell *gtkaspell);
void		gtkaspell_context_set			(GtkAspell *gtkaspell);
void		gtkaspell_free_suggestions_list		(GtkAspell *gtkaspell);


#endif /* USE_ENCHANT */
#endif /* __GTKENCHANT_H__ */
