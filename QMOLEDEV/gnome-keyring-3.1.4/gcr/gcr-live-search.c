/*
 * Copyright (C) 2011 Collabora Ltd.
 * Copyright (C) 2010 Collabora Ltd.
 * Copyright (C) 2007-2010 Nokia Corporation.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Authors: Felix Kaser <felix.kaser@collabora.co.uk>
 *          Xavier Claessens <xavier.claessens@collabora.co.uk>
 *          Claudio Saavedra <csaavedra@igalia.com>
 *          Stef Walter <stefw@collabora.co.uk>
 */

/* Code borrowed from Empathy */

#include "config.h"

#include "gcr-live-search.h"
#include "gcr-marshal.h"

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include <string.h>

G_DEFINE_TYPE (GcrLiveSearch, _gcr_live_search, GTK_TYPE_BOX)

struct _GcrLiveSearchPrivate {
	GtkWidget *search_entry;
	GtkWidget *hook_widget;
	GPtrArray *stripped_words;
};

enum {
	PROP_0,
	PROP_HOOK_WIDGET,
	PROP_TEXT
};

enum {
	ACTIVATE,
	KEYNAV,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void on_hook_widget_destroy (GtkWidget *object, gpointer user_data);

static gunichar
stripped_char (gunichar ch)
{
	gunichar retval = 0;
	GUnicodeType utype;
	gunichar *decomp;
	gsize dlen;

	utype = g_unichar_type (ch);

	switch (utype) {
	case G_UNICODE_CONTROL:
	case G_UNICODE_FORMAT:
	case G_UNICODE_UNASSIGNED:
	case G_UNICODE_NON_SPACING_MARK:
	case G_UNICODE_COMBINING_MARK:
	case G_UNICODE_ENCLOSING_MARK:
		/* Ignore those */
		break;
	case G_UNICODE_PRIVATE_USE:
	case G_UNICODE_SURROGATE:
	case G_UNICODE_LOWERCASE_LETTER:
	case G_UNICODE_MODIFIER_LETTER:
	case G_UNICODE_OTHER_LETTER:
	case G_UNICODE_TITLECASE_LETTER:
	case G_UNICODE_UPPERCASE_LETTER:
	case G_UNICODE_DECIMAL_NUMBER:
	case G_UNICODE_LETTER_NUMBER:
	case G_UNICODE_OTHER_NUMBER:
	case G_UNICODE_CONNECT_PUNCTUATION:
	case G_UNICODE_DASH_PUNCTUATION:
	case G_UNICODE_CLOSE_PUNCTUATION:
	case G_UNICODE_FINAL_PUNCTUATION:
	case G_UNICODE_INITIAL_PUNCTUATION:
	case G_UNICODE_OTHER_PUNCTUATION:
	case G_UNICODE_OPEN_PUNCTUATION:
	case G_UNICODE_CURRENCY_SYMBOL:
	case G_UNICODE_MODIFIER_SYMBOL:
	case G_UNICODE_MATH_SYMBOL:
	case G_UNICODE_OTHER_SYMBOL:
	case G_UNICODE_LINE_SEPARATOR:
	case G_UNICODE_PARAGRAPH_SEPARATOR:
	case G_UNICODE_SPACE_SEPARATOR:
	default:
		ch = g_unichar_tolower (ch);
		decomp = g_unicode_canonical_decomposition (ch, &dlen);
		if (decomp != NULL) {
			retval = decomp[0];
			g_free (decomp);
		}
	}

	return retval;
}

static void
append_word (GPtrArray **word_array, GString **word)
{
	if (*word != NULL) {
		if (*word_array == NULL)
			*word_array = g_ptr_array_new_with_free_func (g_free);
		g_ptr_array_add (*word_array, g_string_free (*word, FALSE));
		*word = NULL;
	}
}

GPtrArray *
_gcr_live_search_strip_utf8_string (const gchar *string)
{
	GPtrArray *word_array = NULL;
	GString *word = NULL;
	const gchar *p;

	if (string == NULL || *string == '\0')
		return NULL;

	for (p = string; *p != '\0'; p = g_utf8_next_char (p)) {
		gunichar sc;

		/* Make the char lower-case, remove its accentuation marks, and ignore it
		 * if it is just unicode marks */
		sc = stripped_char (g_utf8_get_char (p));
		if (sc == 0)
			continue;

		/* If it is not alpha-num, it is separator between words */
		if (!g_unichar_isalnum (sc)) {
			append_word (&word_array, &word);
			continue;
		}

		/* It is alpha-num, append this char to current word, or start new word */
		if (word == NULL)
			word = g_string_new (NULL);
		g_string_append_unichar (word, sc);
	}

	append_word (&word_array, &word);

	return word_array;
}

static gboolean
live_search_match_prefix (const gchar *string, const gchar *prefix)
{
	const gchar *p;
	const gchar *prefix_p;
	gboolean next_word = FALSE;

	if (prefix == NULL || prefix[0] == 0)
		return TRUE;

	if (string == NULL || *string == '\0')
		return FALSE;

	prefix_p = prefix;
	for (p = string; *p != '\0'; p = g_utf8_next_char (p)) {
		gunichar sc;

		/* Make the char lower-case, remove its accentuation marks, and ignore it
		 * if it is just unicode marks */
		sc = stripped_char (g_utf8_get_char (p));
		if (sc == 0)
			continue;

		/* If we want to go to next word, ignore alpha-num chars */
		if (next_word && g_unichar_isalnum (sc))
			continue;
		next_word = FALSE;

		/* Ignore word separators */
		if (!g_unichar_isalnum (sc))
			continue;

		/* If this char does not match prefix_p, go to next word and start again
		 * from the beginning of prefix */
		if (sc != g_utf8_get_char (prefix_p)) {
			next_word = TRUE;
			prefix_p = prefix;
			continue;
		}

		/* prefix_p match, verify to next char. If this was the last of prefix,
		 * it means it completely machted and we are done. */
		prefix_p = g_utf8_next_char (prefix_p);
		if (*prefix_p == '\0')
			return TRUE;
	}

	return FALSE;
}

gboolean
_gcr_live_search_match_words (const gchar *string, GPtrArray *words)
{
	guint i;

	if (words == NULL)
		return TRUE;

	for (i = 0; i < words->len; i++)
		if (!live_search_match_prefix (string, g_ptr_array_index (words, i)))
			return FALSE;

	return TRUE;
}

static gboolean
fire_key_navigation_sig (GcrLiveSearch *self, GdkEventKey *event)
{
	gboolean ret;

	g_signal_emit (self, signals[KEYNAV], 0, event, &ret);
	return ret;
}

static gboolean
on_search_entry_key_pressed (GtkEntry *entry, GdkEventKey *event, gpointer user_data)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (user_data);

	/* if esc key pressed, hide the search */
	if (event->keyval == GDK_KEY_Escape) {
		gtk_widget_hide (GTK_WIDGET (self));
		return TRUE;
	}

	/* emit key navigation signal, so other widgets can respond to it properly */
	if (event->keyval == GDK_KEY_Up || event->keyval == GDK_KEY_Down
	    || event->keyval == GDK_KEY_Page_Up || event->keyval == GDK_KEY_Page_Down) {
		return fire_key_navigation_sig (self, event);
	}

	if (event->keyval == GDK_KEY_Home || event->keyval == GDK_KEY_End ||
	    event->keyval == GDK_KEY_space) {
		/* If the live search is visible, the entry should catch the Home/End
		 * and space events */
		if (!gtk_widget_get_visible (GTK_WIDGET (self))) {
			return fire_key_navigation_sig (self, event);
		}
	}

	return FALSE;
}

static void
on_search_entry_text_changed (GtkEntry *entry, gpointer user_data)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (user_data);
	const gchar *text;

	text = gtk_entry_get_text (entry);

	if (text == NULL || *text == '\0')
		gtk_widget_hide (GTK_WIDGET (self));
	else
		gtk_widget_show (GTK_WIDGET (self));

	if (self->pv->stripped_words != NULL)
		g_ptr_array_unref (self->pv->stripped_words);

	self->pv->stripped_words = _gcr_live_search_strip_utf8_string (text);

	g_object_notify (G_OBJECT (self), "text");
}

static void
on_search_entry_close_pressed (GtkEntry *entry, GtkEntryIconPosition icon_pos,
                               GdkEvent *event, gpointer user_data)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (user_data);
	gtk_widget_hide (GTK_WIDGET (self));
}

static gboolean
on_hook_widget_key_press_event (GtkWidget *widget, GdkEventKey *event,
                                gpointer user_data)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (user_data);
	GdkEvent *new_event;
	gboolean ret;

	/* dont forward this event to the entry, else the event is consumed by the
	 * entry and does not close the window */
	if (!gtk_widget_get_visible (GTK_WIDGET (self)) &&
	    event->keyval == GDK_KEY_Escape)
		return FALSE;

	/* do not show the search if CTRL and/or ALT are pressed with a key
	 * this is needed, because otherwise the CTRL + F accel would not work,
	 * because the entry consumes it */
	if (event->state & (GDK_MOD1_MASK | GDK_CONTROL_MASK) ||
	    event->keyval == GDK_KEY_Control_L ||
	    event->keyval == GDK_KEY_Control_R)
		return FALSE;

	/* dont forward the up/down and Page Up/Down arrow keys to the entry,
	 * they are needed for navigation in the treeview and are not needed in
	 * the search entry */
	if (event->keyval == GDK_KEY_Up || event->keyval == GDK_KEY_Down ||
	    event->keyval == GDK_KEY_Page_Up || event->keyval == GDK_KEY_Page_Down)
		return FALSE;

	if (event->keyval == GDK_KEY_Home || event->keyval == GDK_KEY_End ||
	    event->keyval == GDK_KEY_space) {
		/* Home/End and space keys have to be forwarded to the entry only if
		 * the live search is visible (to move the cursor inside the entry). */
		if (!gtk_widget_get_visible (GTK_WIDGET (self)))
			return FALSE;
	}

	/* realize the widget if it is not realized yet */
	gtk_widget_realize (self->pv->search_entry);
	if (!gtk_widget_has_focus (self->pv->search_entry)) {
		gtk_widget_grab_focus (self->pv->search_entry);
		gtk_editable_set_position (GTK_EDITABLE (self->pv->search_entry), -1);
	}

	/* forward the event to the search entry */
	new_event = gdk_event_copy ((GdkEvent *) event);
	ret = gtk_widget_event (self->pv->search_entry, new_event);
	gdk_event_free (new_event);

	return ret;
}

static void
on_search_entry_activate (GtkEntry *entry, GcrLiveSearch *self)
{
	g_signal_emit (self, signals[ACTIVATE], 0);
}

static void
live_search_release_hook_widget (GcrLiveSearch *self)
{
	/* remove old handlers if old source was not null */
	if (self->pv->hook_widget != NULL) {
		g_signal_handlers_disconnect_by_func (self->pv->hook_widget,
		                                      on_hook_widget_key_press_event, self);
		g_signal_handlers_disconnect_by_func (self->pv->hook_widget,
		                                      on_hook_widget_destroy, self);
		g_object_unref (self->pv->hook_widget);
		self->pv->hook_widget = NULL;
	}
}

static void
on_hook_widget_destroy (GtkWidget *object, gpointer user_data)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (user_data);

	/* unref the hook widget and hide search */
	gtk_widget_hide (GTK_WIDGET (self));
	live_search_release_hook_widget (self);
}

static void
live_search_dispose (GObject *obj)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (obj);

	live_search_release_hook_widget (self);

	G_OBJECT_CLASS (_gcr_live_search_parent_class)->dispose (obj);
}

static void
live_search_finalize (GObject *obj)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (obj);

	if (self->pv->stripped_words != NULL)
		g_ptr_array_unref (self->pv->stripped_words);

	G_OBJECT_CLASS (_gcr_live_search_parent_class)->finalize (obj);
}

static void
live_search_get_property (GObject *object, guint param_id,
                          GValue *value, GParamSpec *pspec)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (object);

	switch (param_id) {
	case PROP_HOOK_WIDGET:
		g_value_set_object (value, _gcr_live_search_get_hook_widget (self));
		break;
	case PROP_TEXT:
		g_value_set_string (value, _gcr_live_search_get_text (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	}
}

static void
live_search_set_property (GObject *object, guint param_id,
                          const GValue *value, GParamSpec *pspec)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (object);

	switch (param_id) {
	case PROP_HOOK_WIDGET:
		_gcr_live_search_set_hook_widget (self, g_value_get_object (value));
		break;
	case PROP_TEXT:
		_gcr_live_search_set_text (self, g_value_get_string (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
		break;
	};
}

static void
live_search_unmap (GtkWidget *widget)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (widget);

	GTK_WIDGET_CLASS (_gcr_live_search_parent_class)->unmap (widget);

	/* unmap can happen if a parent gets hidden, in that case we want to hide
	 * the live search as well, so when it gets mapped again, the live search
	 * won't be shown. */
	gtk_widget_hide (widget);

	gtk_entry_set_text (GTK_ENTRY (self->pv->search_entry), "");
	gtk_widget_grab_focus (self->pv->hook_widget);
}

static void
live_search_show (GtkWidget *widget)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (widget);

	if (!gtk_widget_has_focus (self->pv->search_entry))
		gtk_widget_grab_focus (self->pv->search_entry);

	GTK_WIDGET_CLASS (_gcr_live_search_parent_class)->show (widget);
}

static void
live_search_grab_focus (GtkWidget *widget)
{
	GcrLiveSearch *self = GCR_LIVE_SEARCH (widget);

	if (!gtk_widget_has_focus (self->pv->search_entry)) {
		gtk_widget_grab_focus (self->pv->search_entry);
		gtk_editable_set_position (GTK_EDITABLE (self->pv->search_entry), -1);
	}
}

static void
_gcr_live_search_class_init (GcrLiveSearchClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;
	GtkWidgetClass *widget_class = (GtkWidgetClass *) klass;
	GParamSpec *param_spec;

	object_class->finalize = live_search_finalize;
	object_class->dispose = live_search_dispose;
	object_class->get_property = live_search_get_property;
	object_class->set_property = live_search_set_property;

	widget_class->unmap = live_search_unmap;
	widget_class->show = live_search_show;
	widget_class->grab_focus = live_search_grab_focus;

	signals[ACTIVATE] = g_signal_new ("activate",
	                                  G_TYPE_FROM_CLASS (object_class),
	                                  G_SIGNAL_RUN_LAST,
	                                  0,
	                                  NULL, NULL,
	                                  g_cclosure_marshal_VOID__VOID,
	                                  G_TYPE_NONE, 0);

	signals[KEYNAV] = g_signal_new ("key-navigation",
	                                G_TYPE_FROM_CLASS (object_class),
	                                G_SIGNAL_RUN_LAST,
	                                0,
	                                g_signal_accumulator_true_handled, NULL,
	                                _gcr_marshal_BOOLEAN__BOXED,
	                                G_TYPE_BOOLEAN, 1, GDK_TYPE_EVENT | G_SIGNAL_TYPE_STATIC_SCOPE);

	param_spec = g_param_spec_object ("hook-widget", "Live Search Hook Widget",
	                                  "The live search catches key-press-events on this widget",
	                                  GTK_TYPE_WIDGET, G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
	g_object_class_install_property (object_class, PROP_HOOK_WIDGET, param_spec);

	param_spec = g_param_spec_string ("text", "Live Search Text",
	                                  "The text of the live search entry",
	                                  "", G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
	g_object_class_install_property (object_class, PROP_TEXT, param_spec);

	g_type_class_add_private (klass, sizeof (GcrLiveSearchPrivate));
}

static void
_gcr_live_search_init (GcrLiveSearch *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE ((self), GCR_TYPE_LIVE_SEARCH,
	                                        GcrLiveSearchPrivate);

	gtk_widget_set_no_show_all (GTK_WIDGET (self), TRUE);

	self->pv->search_entry = gtk_entry_new ();
	gtk_entry_set_icon_from_stock (GTK_ENTRY (self->pv->search_entry),
	                               GTK_ENTRY_ICON_SECONDARY, GTK_STOCK_CLOSE);
	gtk_entry_set_icon_activatable (GTK_ENTRY (self->pv->search_entry),
	                                GTK_ENTRY_ICON_SECONDARY, TRUE);
	gtk_entry_set_icon_sensitive (GTK_ENTRY (self->pv->search_entry),
	                              GTK_ENTRY_ICON_SECONDARY, TRUE);
	gtk_widget_show (self->pv->search_entry);

	gtk_box_pack_start (GTK_BOX (self), self->pv->search_entry, TRUE, TRUE, 0);

	g_signal_connect (self->pv->search_entry, "icon_release",
	                  G_CALLBACK (on_search_entry_close_pressed), self);
	g_signal_connect (self->pv->search_entry, "changed",
	                  G_CALLBACK (on_search_entry_text_changed), self);
	g_signal_connect (self->pv->search_entry, "key-press-event",
	                  G_CALLBACK (on_search_entry_key_pressed), self);
	g_signal_connect (self->pv->search_entry, "activate",
	                  G_CALLBACK (on_search_entry_activate), self);

	self->pv->hook_widget = NULL;
}

GtkWidget *
_gcr_live_search_new (GtkWidget *hook)
{
	g_return_val_if_fail (hook == NULL || GTK_IS_WIDGET (hook), NULL);

	return g_object_new (GCR_TYPE_LIVE_SEARCH,
	                     "hook-widget", hook,
	                     NULL);
}

/* public methods */

GtkWidget *
_gcr_live_search_get_hook_widget (GcrLiveSearch *self)
{
	g_return_val_if_fail (GCR_IS_LIVE_SEARCH (self), NULL);

	return self->pv->hook_widget;
}

void
_gcr_live_search_set_hook_widget (GcrLiveSearch *self, GtkWidget *hook)
{
	g_return_if_fail (GCR_IS_LIVE_SEARCH (self));
	g_return_if_fail (hook == NULL || GTK_IS_WIDGET (hook));

	/* release the actual widget */
	live_search_release_hook_widget (self);

	/* connect handlers if new source is not null */
	if (hook != NULL) {
		self->pv->hook_widget = g_object_ref (hook);
		g_signal_connect (self->pv->hook_widget, "key-press-event",
		                  G_CALLBACK (on_hook_widget_key_press_event),
		                  self);
		g_signal_connect (self->pv->hook_widget, "destroy",
		                  G_CALLBACK (on_hook_widget_destroy),
		                  self);
	}
}

const gchar *
_gcr_live_search_get_text (GcrLiveSearch *self)
{
	g_return_val_if_fail (GCR_IS_LIVE_SEARCH (self), NULL);

	return gtk_entry_get_text (GTK_ENTRY (self->pv->search_entry));
}

void
_gcr_live_search_set_text (GcrLiveSearch *self, const gchar *text)
{
	g_return_if_fail (GCR_IS_LIVE_SEARCH (self));
	g_return_if_fail (text != NULL);

	gtk_entry_set_text (GTK_ENTRY (self->pv->search_entry), text);
}

/**
 * _gcr_live_search_match:
 * @self: a #GcrLiveSearch
 * @string: a string where to search, must be valid UTF-8.
 *
 * Search if one of the words in @string string starts with the current text
 * of @self.
 *
 * Searching for "aba" in "Abasto" will match, searching in "Moraba" will not,
 * and searching in "A tool (abacus)" will do.
 *
 * The match is not case-sensitive, and regardless of the accentuation marks.
 *
 * Returns: %TRUE if a match is found, %FALSE otherwise.
 *
 **/
gboolean
_gcr_live_search_match (GcrLiveSearch *self, const gchar *string)
{
	g_return_val_if_fail (GCR_IS_LIVE_SEARCH (self), FALSE);

	return _gcr_live_search_match_words (string, self->pv->stripped_words);
}

gboolean
_gcr_live_search_match_string (const gchar *string, const gchar *prefix)
{
	GPtrArray *words;
	gboolean match;

	words = _gcr_live_search_strip_utf8_string (prefix);
	match = _gcr_live_search_match_words (string, words);
	if (words != NULL)
		g_ptr_array_unref (words);

	return match;
}

GPtrArray *
_gcr_live_search_get_words (GcrLiveSearch *self)
{
	return self->pv->stripped_words;
}
