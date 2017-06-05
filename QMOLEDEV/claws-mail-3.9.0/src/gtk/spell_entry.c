/*
 * @file libsexy/sexy-icon-entry.c Entry widget
 *
 * @Copyright (C) 2004-2006 Christian Hammond.
 * Some of this code is from gtkspell, Copyright (C) 2002 Evan Martin.
 * Adapted for Claws Mail (c) 2009-2012 Pawel Pekala and the Claws Mail team
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

#ifdef USE_ENCHANT

#include <glib.h>
#include <glib/gi18n.h>

#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <string.h>
#include <glib.h>
#include <gtk/gtk.h>

#include "spell_entry.h"
#include "prefs_common.h"
#include "codeconv.h"
#include "defs.h"
#include "gtkutils.h"

static void claws_spell_entry_init		(ClawsSpellEntry *entry);
static void claws_spell_entry_editable_init	(GtkEditableClass *iface);
static void claws_spell_entry_finalize		(GObject *object);
#if !GTK_CHECK_VERSION(3, 0, 0)
static void claws_spell_entry_destroy		(GtkObject *object);
static gint claws_spell_entry_expose		(GtkWidget *widget,
						 GdkEventExpose *event);
#else
static void claws_spell_entry_destroy		(GtkWidget *object);
static gint claws_spell_entry_expose		(GtkWidget *widget,
						 cairo_t *cr);
#endif
static gint claws_spell_entry_button_press	(GtkWidget *widget,
						 GdkEventButton *event);
static gboolean claws_spell_entry_popup_menu	(GtkWidget *widget,
						 ClawsSpellEntry *entry);
static void claws_spell_entry_populate_popup	(ClawsSpellEntry *entry,
						 GtkMenu *menu,
						 gpointer data);
static void claws_spell_entry_changed		(GtkEditable *editable,
						 gpointer data);

struct _ClawsSpellEntryPriv
{
	PangoAttrList        *attr_list;
	gint                  mark_character;
	gchar               **words;
	gint                 *word_starts;
	gint                 *word_ends;
};

static GtkEntryClass *parent_class = NULL;


G_DEFINE_TYPE_EXTENDED(ClawsSpellEntry, claws_spell_entry, GTK_TYPE_ENTRY, 0, G_IMPLEMENT_INTERFACE(GTK_TYPE_EDITABLE, claws_spell_entry_editable_init)); 


static void claws_spell_entry_class_init(ClawsSpellEntryClass *klass)
{
	GObjectClass	*g_object_class;
#if !GTK_CHECK_VERSION(3, 0, 0)
	GtkObjectClass	*gtk_object_class;
#endif
	GtkWidgetClass	*widget_class;
	
	parent_class = g_type_class_peek_parent(klass);
	
	g_object_class = G_OBJECT_CLASS(klass);
	g_object_class->finalize = claws_spell_entry_finalize;
	
#if !GTK_CHECK_VERSION(3, 0, 0)
	gtk_object_class = GTK_OBJECT_CLASS(klass);
	gtk_object_class->destroy = claws_spell_entry_destroy;
#endif
	
	widget_class = GTK_WIDGET_CLASS(klass);
	widget_class->button_press_event = claws_spell_entry_button_press;
#if !GTK_CHECK_VERSION(3, 0, 0)
	widget_class->expose_event = claws_spell_entry_expose;
#else
	widget_class->draw = claws_spell_entry_expose;
	widget_class->destroy = claws_spell_entry_destroy;
#endif
	
	g_type_class_add_private(g_object_class,
			sizeof(struct _ClawsSpellEntryPriv));
}

static void claws_spell_entry_init(ClawsSpellEntry *entry)
{
	entry->gtkaspell = NULL;
	
	entry->priv = g_new0(ClawsSpellEntryPriv, 1);
	entry->priv->attr_list = pango_attr_list_new();
                                        
	g_signal_connect(G_OBJECT(entry), "popup-menu",
			G_CALLBACK(claws_spell_entry_popup_menu), entry);
	g_signal_connect(G_OBJECT(entry), "populate-popup",
			G_CALLBACK(claws_spell_entry_populate_popup), NULL);
	g_signal_connect(G_OBJECT(entry), "changed",
			G_CALLBACK(claws_spell_entry_changed), NULL);
}

static void claws_spell_entry_editable_init (GtkEditableClass *iface) {}

static void claws_spell_entry_finalize(GObject *object)
{
	ClawsSpellEntry *entry = CLAWS_SPELL_ENTRY(object);

	if (entry->priv->attr_list)
		pango_attr_list_unref(entry->priv->attr_list);
	if (entry->priv->words)
		g_strfreev(entry->priv->words);

	g_free(entry->priv->word_starts);
	g_free(entry->priv->word_ends);
	g_free(entry->priv);
	entry->priv = NULL;
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

#if !GTK_CHECK_VERSION(3, 0, 0)
static void claws_spell_entry_destroy(GtkObject *object)
{
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}
#else
static void claws_spell_entry_destroy(GtkWidget *object)
{
	GTK_WIDGET_CLASS(parent_class)->destroy(object);
}
#endif

GtkWidget *claws_spell_entry_new(void)
{
	return GTK_WIDGET( g_object_new(CLAWS_TYPE_SPELL_ENTRY, NULL) );
}

void claws_spell_entry_set_gtkaspell(ClawsSpellEntry *entry, GtkAspell *gtkaspell)
{
	cm_return_if_fail(CLAWS_IS_SPELL_ENTRY(entry));

	entry->gtkaspell = gtkaspell;
}

static gint gtk_entry_find_position (GtkEntry *entry, gint x)
{
	PangoLayout *layout;
	PangoLayoutLine *line;
	const gchar *text;
	gint cursor_index;
	gint index;
	gint pos;
	gboolean trailing;

	x = x + entry->scroll_offset;

	layout = gtk_entry_get_layout(entry);
	text = pango_layout_get_text(layout);
	cursor_index = g_utf8_offset_to_pointer(text, entry->current_pos) - text;

	line = pango_layout_get_lines(layout)->data;
	pango_layout_line_x_to_index(line, x * PANGO_SCALE, &index, &trailing);

	if (index >= cursor_index && entry->preedit_length) {
		if (index >= cursor_index + entry->preedit_length) {
			index -= entry->preedit_length;
		} else {
			index = cursor_index;
			trailing = FALSE;
		}
	}

	pos = g_utf8_pointer_to_offset (text, text + index);
	pos += trailing;

	return pos;
}

static void get_word_extents_from_position(ClawsSpellEntry *entry, gint *start,
					   gint *end, guint position)
{
	const gchar *text;
	gint i, bytes_pos;

	*start = -1;
	*end = -1;

	if (entry->priv->words == NULL)
		return;

	text = gtk_entry_get_text(GTK_ENTRY(entry));
	bytes_pos = (gint) (g_utf8_offset_to_pointer(text, position) - text);

	for (i = 0; entry->priv->words[i]; i++) {
		if (bytes_pos >= entry->priv->word_starts[i] &&
		    bytes_pos <= entry->priv->word_ends[i]) {
			*start = entry->priv->word_starts[i];
			*end   = entry->priv->word_ends[i];
			return;
		}
	}
}

static gchar *get_word(ClawsSpellEntry *entry, const int start, const int end)
{
	const gchar *text;
	gchar *word;
	
	if (start >= end)
		return NULL;

	text = gtk_entry_get_text(GTK_ENTRY(entry));
	word = g_new0(gchar, end - start + 2);
	g_strlcpy(word, text + start, end - start + 1);

	return word;
}

static void replace_word(ClawsSpellEntry *entry, const gchar *newword)
{
	gint cursor, start_pos, end_pos;
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(entry));

	start_pos = entry->gtkaspell->start_pos;
	end_pos = entry->gtkaspell->end_pos;
		
	cursor = gtk_editable_get_position(GTK_EDITABLE(entry));
	/* is the cursor at the end? If so, restore it there */
	if (g_utf8_strlen(text, -1) == cursor)
		cursor = -1;
	else if(cursor < entry->priv->mark_character ||
		cursor > entry->priv->mark_character)
			cursor = entry->priv->mark_character;

	gtk_editable_delete_text(GTK_EDITABLE(entry), start_pos, end_pos);
	gtk_editable_insert_text(GTK_EDITABLE(entry), newword, strlen(newword),
							 &start_pos);
	gtk_editable_set_position(GTK_EDITABLE(entry), cursor);
}


static gboolean word_misspelled(ClawsSpellEntry *entry, int start, int end)
{
	gchar *word;
	gboolean ret;

	word = get_word(entry, start, end);
	if (word == NULL || g_unichar_isdigit(word[0]))
		return FALSE;
		
	ret = gtkaspell_misspelled_test(entry->gtkaspell, word);

	g_free(word);
	return ret;
}

static gboolean is_word_end (GtkEntry *entry, const int offset)
{
	gchar *p = gtk_editable_get_chars(GTK_EDITABLE(entry), offset, offset+1);
	gunichar ch;
	
	ch = g_utf8_get_char(p);
	g_free(p);
	
	if (ch == '\0')
		return TRUE;

	if (ch == '\'') {
		p = gtk_editable_get_chars(GTK_EDITABLE(entry), offset+1, offset+2);
		ch = g_utf8_get_char(p);
		g_free(p);
		
		return (g_unichar_isspace(ch) || g_unichar_ispunct(ch)
			|| g_unichar_isdigit(ch));
	}
	
	return (g_unichar_isspace(ch) || g_unichar_ispunct(ch));
}

static void entry_strsplit_utf8(GtkEntry *entry, gchar ***set, gint **starts, gint **ends)
{
	PangoLayout   *layout;
	PangoLogAttr  *log_attrs;
	const gchar   *text;
	gint           n_attrs, n_strings, i, j;

	layout = gtk_entry_get_layout(GTK_ENTRY(entry));
	text = gtk_entry_get_text(GTK_ENTRY(entry));
	pango_layout_get_log_attrs(layout, &log_attrs, &n_attrs);

	/* Find how many words we have */
	n_strings = 0;
	for (i = 0; i < n_attrs; i++)
		if (log_attrs[i].is_word_start)
			n_strings++;

	*set    = g_new0(gchar *, n_strings + 1);
	*starts = g_new0(gint, n_strings);
	*ends   = g_new0(gint, n_strings);

	/* Copy out strings */
	for (i = 0, j = 0; i < n_attrs; i++) {
		if (log_attrs[i].is_word_start) {
			gint cend, bytes;
			gchar *start;

			/* Find the end of this string */
			cend = i;
			while (!is_word_end(entry, cend))
				cend++;

			/* Copy sub-string */
			start = g_utf8_offset_to_pointer(text, i);
			bytes = (gint) (g_utf8_offset_to_pointer(text, cend) - start);
			(*set)[j]    = g_new0(gchar, bytes + 1);
			(*starts)[j] = (gint) (start - text);
			(*ends)[j]   = (gint) (start - text + bytes);
			g_utf8_strncpy((*set)[j], start, cend - i);

			/* Move on to the next word */
			j++;
		}
	}

	g_free (log_attrs);
}

static void insert_misspelled_marker(ClawsSpellEntry *entry, guint start, guint end)
{
	guint16 red   = (guint16) (((gdouble)((prefs_common.misspelled_col & 
					0xff0000) >> 16) / 255.0) * 65535.0);
	guint16 green = (guint16) (((gdouble)((prefs_common.misspelled_col & 
					0x00ff00) >> 8) / 255.0) * 65535.0);
	guint16 blue  = (guint16) (((gdouble) (prefs_common.misspelled_col & 
					0x0000ff) / 255.0) * 65535.0);
	PangoAttribute *fcolor, *ucolor, *unline;
	
	if(prefs_common.misspelled_col != 0) {
		fcolor = pango_attr_foreground_new(red, green, blue);
		fcolor->start_index = start;
		fcolor->end_index = end;
		
		pango_attr_list_insert(entry->priv->attr_list, fcolor);
	} else {
		ucolor = pango_attr_underline_color_new (65535, 0, 0);
		unline = pango_attr_underline_new (PANGO_UNDERLINE_ERROR);

		ucolor->start_index = start;
		unline->start_index = start;

		ucolor->end_index = end;
		unline->end_index = end;

		pango_attr_list_insert (entry->priv->attr_list, ucolor);
		pango_attr_list_insert (entry->priv->attr_list, unline);
	}
}

static gboolean check_word(ClawsSpellEntry *entry, int start, int end)
{
	GtkAspell *gtkaspell = entry->gtkaspell;
	PangoAttrIterator *it;
	gint s, e;
	gboolean misspelled;
	gchar *text = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	gchar *word = NULL;

	/* Check to see if we've got any attributes at this position.
	 * If so, free them, since we'll readd it if the word is misspelled */
	it = pango_attr_list_get_iterator(entry->priv->attr_list);
	if (it == NULL)
		return FALSE;
	do {
		pango_attr_iterator_range(it, &s, &e);
		if (s == start) {
			GSList *attrs = pango_attr_iterator_get_attrs(it);
			g_slist_foreach(attrs, (GFunc) pango_attribute_destroy, NULL);
			g_slist_free(attrs);
		}
	} while (pango_attr_iterator_next(it));
	pango_attr_iterator_destroy(it);

	if ((misspelled = word_misspelled(entry, start, end))) {
		insert_misspelled_marker(entry, start, end);
	
		word = get_word(entry, start, end);
		strncpy(gtkaspell->theword, (gchar *)word, GTKASPELLWORDSIZE - 1);
		gtkaspell->theword[GTKASPELLWORDSIZE - 1] = 0;
		gtkaspell->start_pos  = g_utf8_pointer_to_offset(text, (text+start));
		gtkaspell->end_pos    = g_utf8_pointer_to_offset(text, (text+end));
		gtkaspell_free_suggestions_list(gtkaspell);
		g_free(word);
	}
	
	g_free(text);
	
	return misspelled;
}

void claws_spell_entry_recheck_all(ClawsSpellEntry *entry)
{
	GdkRectangle rect;
	PangoLayout *layout;
	int length, i;

	cm_return_if_fail(CLAWS_IS_SPELL_ENTRY(entry));
	cm_return_if_fail(entry->gtkaspell != NULL);

	if (entry->priv->words == NULL)
		return;

	/* Remove all existing pango attributes.  These will get readded as we check */
	pango_attr_list_unref(entry->priv->attr_list);
	entry->priv->attr_list = pango_attr_list_new();

	/* Loop through words */
	for (i = 0; entry->priv->words[i]; i++) {
		length = strlen(entry->priv->words[i]);
		if (length == 0)
			continue;
		check_word(entry, entry->priv->word_starts[i], entry->priv->word_ends[i]);
	}

	layout = gtk_entry_get_layout(GTK_ENTRY(entry));
	pango_layout_set_attributes(layout, entry->priv->attr_list);

	if (gtk_widget_get_realized(GTK_WIDGET(entry))) {
		rect.x = 0; rect.y = 0;
		rect.width  = GTK_WIDGET(entry)->allocation.width;
		rect.height = GTK_WIDGET(entry)->allocation.height;
		gdk_window_invalidate_rect(GTK_WIDGET(entry)->window, &rect, TRUE);
	}
}

#if !GTK_CHECK_VERSION(3, 0, 0)
static gint claws_spell_entry_expose(GtkWidget *widget, GdkEventExpose *event)
#else
static gint claws_spell_entry_expose(GtkWidget *widget, cairo_t *cr)
#endif
{
	ClawsSpellEntry *entry = CLAWS_SPELL_ENTRY(widget);
	GtkEntry *gtk_entry = GTK_ENTRY(widget);
	PangoLayout *layout;

	if (entry->gtkaspell != NULL) {
		layout = gtk_entry_get_layout(gtk_entry);
		pango_layout_set_attributes(layout, entry->priv->attr_list);
	}

#if !GTK_CHECK_VERSION(3, 0, 0)
	return GTK_WIDGET_CLASS(parent_class)->expose_event (widget, event);
#else
	return GTK_WIDGET_CLASS(parent_class)->draw (widget, cr);
#endif
}

static gint claws_spell_entry_button_press(GtkWidget *widget, GdkEventButton *event)
{
	ClawsSpellEntry *entry = CLAWS_SPELL_ENTRY(widget);
	GtkEntry *gtk_entry = GTK_ENTRY(widget);
	gint pos;

	pos = gtk_entry_find_position(gtk_entry, event->x);
	entry->priv->mark_character = pos;

	return GTK_WIDGET_CLASS(parent_class)->button_press_event (widget, event);
}

static gboolean claws_spell_entry_popup_menu(GtkWidget *widget, ClawsSpellEntry *entry)
{
	entry->priv->mark_character = gtk_editable_get_position (GTK_EDITABLE (entry));
	return FALSE;
}

static void set_position(gpointer data, gint pos)
{
	gtk_editable_set_position(GTK_EDITABLE(data), pos);
}

static gboolean find_misspelled_cb(gpointer data, gboolean forward)
{
	ClawsSpellEntry *entry = (ClawsSpellEntry *)data;
	GtkAspell *gtkaspell = entry->gtkaspell;
	gboolean misspelled = FALSE;
	gint cursor, minpos, maxpos, i, words_len = 0;
	gint start, end;
	gchar *text;
	
	if (entry->priv->words == NULL)
		return FALSE;

	gtkaspell->orig_pos = gtk_editable_get_position(GTK_EDITABLE(entry));
	text = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	cursor = g_utf8_offset_to_pointer(text, gtkaspell->orig_pos) - text;

	if (gtk_editable_get_selection_bounds(GTK_EDITABLE(entry), &start, &end)) {
		minpos = g_utf8_offset_to_pointer(text, start) - text;
		maxpos = g_utf8_offset_to_pointer(text, end) - text;
	} else {
		minpos = forward ? cursor : 0;
		maxpos = forward ? strlen(text)-1 : cursor;
	}
	g_free(text);

	while(entry->priv->words[words_len])
		words_len++;

	if (forward) {
		for(i=0; i < words_len; i++)
			if (entry->priv->word_ends[i] > minpos &&
			    (misspelled = check_word(entry,
			    		entry->priv->word_starts[i],
					entry->priv->word_ends[i])))
				break;
	} else {
		for(i=words_len-1; i >= 0; i--)
			if (entry->priv->word_starts[i] < maxpos &&
			    (misspelled = check_word(entry,
			    		entry->priv->word_starts[i],
					entry->priv->word_ends[i])))
				break;
	}
	
	return misspelled;
}

static gboolean check_word_cb(gpointer data)
{
	ClawsSpellEntry *entry = (ClawsSpellEntry *)data;
	gint start, end;
	
	get_word_extents_from_position(entry, &start, &end, entry->priv->mark_character);
	return check_word(entry, start, end);
}

static void replace_word_cb(gpointer data, const gchar *newword)
{
	replace_word((ClawsSpellEntry *) data, newword);
}

static void set_menu_pos(GtkMenu *menu, gint *x, gint *y, 
			 gboolean *push_in, gpointer data)
{
	ClawsSpellEntry *entry = (ClawsSpellEntry *) data;
	GtkAspell *gtkaspell = entry->gtkaspell;
	gint pango_offset, win_x, win_y, scr_x, scr_y, text_index, entry_x;
	gchar *text;
	GtkRequisition subject_rq;
	PangoLayout *layout = gtk_entry_get_layout(GTK_ENTRY(entry));
	PangoLayoutLine *line = pango_layout_get_lines(layout)->data;

	gtk_widget_get_child_requisition(GTK_WIDGET(entry), &subject_rq);
	
	/* screen -> compose window coords */
	gdk_window_get_origin(GTK_WIDGET(gtkaspell->parent_window)->window,
				&scr_x, &scr_y);

	/* compose window -> subject entry coords */
	gtk_widget_translate_coordinates(GTK_WIDGET(entry),
			gtkaspell->parent_window, 0, 0,	&win_x, &win_y);

	text = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	text_index = g_utf8_offset_to_pointer(text, gtkaspell->end_pos) - text;
	g_free(text);

	pango_offset = gtk_entry_text_index_to_layout_index(GTK_ENTRY(entry),
					text_index);
	pango_layout_line_index_to_x(line, pango_offset, TRUE, &entry_x);

	*x = scr_x + win_x + PANGO_PIXELS(entry_x) + 8;
	*y = scr_y + win_y + subject_rq.height;
}

void claws_spell_entry_context_set(ClawsSpellEntry *entry)
{
	cm_return_if_fail(CLAWS_IS_SPELL_ENTRY(entry));
	cm_return_if_fail(entry->gtkaspell != NULL);

	entry->gtkaspell->ctx.set_position	= set_position;
	entry->gtkaspell->ctx.set_menu_pos	= set_menu_pos;
	entry->gtkaspell->ctx.find_misspelled	= find_misspelled_cb;
	entry->gtkaspell->ctx.check_word	= check_word_cb;
	entry->gtkaspell->ctx.replace_word	= replace_word_cb;
        entry->gtkaspell->ctx.data		= (gpointer) entry;
}

static void claws_spell_entry_populate_popup(ClawsSpellEntry *entry, GtkMenu *menu,
						gpointer data)
{
	GtkAspell *gtkaspell = entry->gtkaspell;
	gint start, end;
	gchar *word, *text;
	
	if (gtkaspell == NULL)
        	return;
	
        get_word_extents_from_position(entry, &start, &end, entry->priv->mark_character);

        if ((word = get_word(entry, start, end)) != NULL) {
		strncpy(gtkaspell->theword, word, GTKASPELLWORDSIZE - 1);
		g_free(word);
	}

        gtkaspell->misspelled = word_misspelled(entry, start, end);

	text = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	gtkaspell->start_pos  = g_utf8_pointer_to_offset(text, (text+start));
	gtkaspell->end_pos    = g_utf8_pointer_to_offset(text, (text+end));
	g_free(text);

        claws_spell_entry_context_set(entry);
        gtkaspell_make_context_menu(menu, gtkaspell);
}

static void claws_spell_entry_changed(GtkEditable *editable, gpointer data)
{
	ClawsSpellEntry *entry = CLAWS_SPELL_ENTRY(editable);

	if (entry->gtkaspell == NULL)
		return;

	if (entry->priv->words) {
		g_strfreev(entry->priv->words);
		g_free(entry->priv->word_starts);
		g_free(entry->priv->word_ends);
	}
	entry_strsplit_utf8(GTK_ENTRY(entry), &entry->priv->words, 
			&entry->priv->word_starts, &entry->priv->word_ends);
	if(entry->gtkaspell->check_while_typing == TRUE)
        	claws_spell_entry_recheck_all(entry);
}

static void continue_check(gpointer *data)
{
	ClawsSpellEntry *entry = (ClawsSpellEntry *)data;
	GtkAspell *gtkaspell = entry->gtkaspell;
	gint pos = gtk_editable_get_position(GTK_EDITABLE(entry));
	
	if (gtkaspell->misspelled && pos < gtkaspell->end_check_pos)
		gtkaspell->misspelled = gtkaspell_check_next_prev(gtkaspell, TRUE);
	else
		gtkaspell->continue_check = NULL;
}

void claws_spell_entry_check_all(ClawsSpellEntry *entry)
{
	gint start, end;
	gchar *text;
	
	cm_return_if_fail(CLAWS_IS_SPELL_ENTRY(entry));
	cm_return_if_fail(entry->gtkaspell != NULL);
	
	if (!gtk_editable_get_selection_bounds(GTK_EDITABLE(entry), &start, &end)) {
		text = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);	
		
		start = 0;
		end = g_utf8_strlen(text, -1) - 1;
		
		g_free(text);
	}

	gtk_editable_set_position(GTK_EDITABLE(entry), start);
	entry->gtkaspell->continue_check = continue_check;
	entry->gtkaspell->end_check_pos	 = end;

	claws_spell_entry_context_set(entry);
	entry->gtkaspell->misspelled = 
			gtkaspell_check_next_prev(entry->gtkaspell, TRUE);
}

void claws_spell_entry_check_backwards(ClawsSpellEntry *entry)
{
	cm_return_if_fail(CLAWS_IS_SPELL_ENTRY(entry));
	cm_return_if_fail(entry->gtkaspell != NULL);
	
	entry->gtkaspell->continue_check = NULL;
	claws_spell_entry_context_set(entry);
	gtkaspell_check_next_prev(entry->gtkaspell, FALSE);
}

void claws_spell_entry_check_forwards_go(ClawsSpellEntry *entry)
{
	cm_return_if_fail(CLAWS_IS_SPELL_ENTRY(entry));
	cm_return_if_fail(entry->gtkaspell != NULL);

	entry->gtkaspell->continue_check = NULL;
	claws_spell_entry_context_set(entry);
	gtkaspell_check_next_prev(entry->gtkaspell, TRUE);
}

#endif  /* USE_ENCHANT */
