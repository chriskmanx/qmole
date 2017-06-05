/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gtksourceundomanager.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 1998, 1999 Alex Roberts, Evan Lawrence
 * Copyright (C) 2000, 2001 Chema Celorio, Paolo Maggi
 * Copyright (C) 2002-2005  Paolo Maggi
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

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gtksourceundomanagerdefault.h"
#include "gtksourceundomanager.h"
#include "gtksourceview-i18n.h"

#define DEFAULT_MAX_UNDO_LEVELS		-1

/*
 * The old code which used a GSList and g_slist_nth_element
 * was way too slow for many operations (search/replace, hit Ctrl-Z,
 * see gedit freezes). GSList was replaced with a GPtrArray with
 * minimal changes to the code: to avoid insertions at the beginning
 * of the array it uses array beginning as the list end and vice versa;
 * hence bunch of ugly action_list_* functions.
 * FIXME: so, somebody please rewrite this stuff using some nice data
 * structures or something.
 */

typedef struct _GtkSourceUndoAction  			GtkSourceUndoAction;
typedef struct _GtkSourceUndoInsertAction		GtkSourceUndoInsertAction;
typedef struct _GtkSourceUndoDeleteAction		GtkSourceUndoDeleteAction;

typedef enum
{
	GTK_SOURCE_UNDO_ACTION_INSERT,
	GTK_SOURCE_UNDO_ACTION_DELETE
} GtkSourceUndoActionType;

/*
 * We use offsets instead of GtkTextIters because the last ones
 * require to much memory in this context without giving us any advantage.
 */

struct _GtkSourceUndoInsertAction
{
	gint   pos;
	gchar *text;
	gint   length;
	gint   chars;
};

struct _GtkSourceUndoDeleteAction
{
	gint   start;
	gint   end;
	gchar *text;
	gboolean forward;
};

struct _GtkSourceUndoAction
{
	GtkSourceUndoActionType action_type;

	union
	{
		GtkSourceUndoInsertAction  insert;
		GtkSourceUndoDeleteAction  delete;
	} action;

	gint order_in_group;

	/* It is TRUE whether the action can be merged with the following action. */
	guint mergeable : 1;

	/* It is TRUE whether the action is marked as "modified".
	 * An action is marked as "modified" if it changed the
	 * state of the buffer from "not modified" to "modified". Only the first
	 * action of a group can be marked as modified.
	 * There can be a single action marked as "modified" in the actions list.
	 */
	guint modified  : 1;
};

/* INVALID is a pointer to an invalid action */
#define INVALID ((void *) "IA")

enum
{
	INSERT_TEXT,
	DELETE_RANGE,
	BEGIN_USER_ACTION,
	MODIFIED_CHANGED,
	NUM_SIGNALS
};

struct _GtkSourceUndoManagerDefaultPrivate
{
	GtkTextBuffer *buffer;

	GPtrArray *actions;
	gint next_redo;

	gint actions_in_current_group;
	gint running_not_undoable_actions;
	gint num_of_groups;
	gint max_undo_levels;

	guint can_undo : 1;
	guint can_redo : 1;

	/* It is TRUE whether, while undoing an action of the current group (with order_in_group > 1),
	 * the state of the buffer changed from "not modified" to "modified".
	 */
	guint modified_undoing_group : 1;

	/* Pointer to the action (in the action list) marked as "modified".
	 * It is NULL when no action is marked as "modified".
	 * It is INVALID when the action marked as "modified" has been removed
	 * from the action list (freeing the list or resizing it) */
	GtkSourceUndoAction *modified_action;

	guint buffer_signals[NUM_SIGNALS];
};

/* Properties */
enum
{
	PROP_0,
	PROP_BUFFER,
	PROP_MAX_UNDO_LEVELS
};

static void insert_text_handler       (GtkTextBuffer             *buffer,
                                       GtkTextIter               *pos,
                                       const gchar               *text,
                                       gint                       length,
                                       GtkSourceUndoManagerDefault      *um);

static void delete_range_handler      (GtkTextBuffer             *buffer,
                                       GtkTextIter               *start,
                                       GtkTextIter               *end,
                                       GtkSourceUndoManagerDefault      *um);

static void begin_user_action_handler (GtkTextBuffer             *buffer,
                                       GtkSourceUndoManagerDefault      *um);

static void modified_changed_handler  (GtkTextBuffer             *buffer,
                                       GtkSourceUndoManagerDefault      *um);

static void free_action_list          (GtkSourceUndoManagerDefault      *um);

static void add_action                (GtkSourceUndoManagerDefault      *um,
                                       const GtkSourceUndoAction *undo_action);
static void free_first_n_actions      (GtkSourceUndoManagerDefault      *um,
                                       gint                       n);
static void check_list_size           (GtkSourceUndoManagerDefault      *um);

static gboolean merge_action          (GtkSourceUndoManagerDefault      *um,
                                       const GtkSourceUndoAction *undo_action);

static void gtk_source_undo_manager_iface_init (GtkSourceUndoManagerIface *iface);

G_DEFINE_TYPE_WITH_CODE (GtkSourceUndoManagerDefault, gtk_source_undo_manager_default, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_SOURCE_UNDO_MANAGER,
                                                gtk_source_undo_manager_iface_init))

static void
gtk_source_undo_manager_default_finalize (GObject *object)
{
	GtkSourceUndoManagerDefault *manager;

	manager = GTK_SOURCE_UNDO_MANAGER_DEFAULT (object);

	free_action_list (manager);
	g_ptr_array_free (manager->priv->actions, TRUE);

	G_OBJECT_CLASS (gtk_source_undo_manager_default_parent_class)->finalize (object);
}

static void
clear_undo (GtkSourceUndoManagerDefault *manager)
{
	free_action_list (manager);

	manager->priv->next_redo = -1;

	if (manager->priv->can_undo)
	{
		manager->priv->can_undo = FALSE;
		gtk_source_undo_manager_can_undo_changed (GTK_SOURCE_UNDO_MANAGER (manager));
	}

	if (manager->priv->can_redo)
	{
		manager->priv->can_redo = FALSE;
		gtk_source_undo_manager_can_redo_changed (GTK_SOURCE_UNDO_MANAGER (manager));
	}
}

static void
set_buffer (GtkSourceUndoManagerDefault *manager,
            GtkTextBuffer               *buffer)
{
	if (buffer == manager->priv->buffer)
	{
		return;
	}

	clear_undo (manager);

	if (manager->priv->buffer != NULL)
	{
		gint i;

		for (i = 0; i < NUM_SIGNALS; ++i)
		{
			g_signal_handler_disconnect (manager->priv->buffer,
			                             manager->priv->buffer_signals[i]);
		}

		g_object_unref (manager->priv->buffer);
		manager->priv->buffer = NULL;
	}

	if (buffer != NULL)
	{
		manager->priv->buffer = g_object_ref (buffer);

		manager->priv->buffer_signals[INSERT_TEXT] =
			g_signal_connect (buffer,
			                  "insert-text",
			                  G_CALLBACK (insert_text_handler),
			                  manager);

		manager->priv->buffer_signals[DELETE_RANGE] =
			g_signal_connect (buffer,
			                  "delete-range",
			                  G_CALLBACK (delete_range_handler),
			                  manager);

		manager->priv->buffer_signals[BEGIN_USER_ACTION] =
			g_signal_connect (buffer,
			                  "begin-user-action",
			                  G_CALLBACK (begin_user_action_handler),
			                  manager);

		manager->priv->buffer_signals[MODIFIED_CHANGED] =
			g_signal_connect (buffer,
			                  "modified-changed",
			                  G_CALLBACK (modified_changed_handler),
			                  manager);
	}
}

static void
set_max_undo_levels (GtkSourceUndoManagerDefault *manager,
                     gint                         max_undo_levels)
{
	gint old_levels;

	old_levels = manager->priv->max_undo_levels;
	manager->priv->max_undo_levels = max_undo_levels;

	if (max_undo_levels < 1)
		return;

	if (old_levels > max_undo_levels)
	{
		/* strip redo actions first */
		while (manager->priv->next_redo >= 0 &&
		       (manager->priv->num_of_groups > max_undo_levels))
		{
			free_first_n_actions (manager, 1);
			manager->priv->next_redo--;
		}

		/* now remove undo actions if necessary */
		check_list_size (manager);

		/* emit "can_undo" and/or "can_redo" if appropiate */
		if (manager->priv->next_redo < 0 && manager->priv->can_redo)
		{
			manager->priv->can_redo = FALSE;
			gtk_source_undo_manager_can_redo_changed (GTK_SOURCE_UNDO_MANAGER (manager));
		}

		if (manager->priv->can_undo &&
		    manager->priv->next_redo >= (gint)manager->priv->actions->len - 1)
		{
			manager->priv->can_undo = FALSE;
			gtk_source_undo_manager_can_undo_changed (GTK_SOURCE_UNDO_MANAGER (manager));
		}
	}
}

static void
gtk_source_undo_manager_default_dispose (GObject *object)
{
	GtkSourceUndoManagerDefault *manager;

	manager = GTK_SOURCE_UNDO_MANAGER_DEFAULT (object);

	if (manager->priv->buffer != NULL)
	{
		/* Clear the buffer */
		set_buffer (manager, NULL);
	}
}

static void
gtk_source_undo_manager_default_set_property (GObject      *object,
                                              guint         prop_id,
                                              const GValue *value,
                                              GParamSpec   *pspec)
{
	GtkSourceUndoManagerDefault *self = GTK_SOURCE_UNDO_MANAGER_DEFAULT (object);
	
	switch (prop_id)
	{
		case PROP_BUFFER:
			set_buffer (self, g_value_get_object (value));
		break;
		case PROP_MAX_UNDO_LEVELS:
			gtk_source_undo_manager_default_set_max_undo_levels (self,
			                                                     g_value_get_int (value));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
gtk_source_undo_manager_default_get_property (GObject    *object,
                                              guint       prop_id,
                                              GValue     *value,
                                              GParamSpec *pspec)
{
	GtkSourceUndoManagerDefault *self = GTK_SOURCE_UNDO_MANAGER_DEFAULT (object);
	
	switch (prop_id)
	{
		case PROP_BUFFER:
			g_value_set_object (value, self->priv->buffer);
		break;
		case PROP_MAX_UNDO_LEVELS:
			g_value_set_int (value, self->priv->max_undo_levels);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
gtk_source_undo_manager_default_class_init (GtkSourceUndoManagerDefaultClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = gtk_source_undo_manager_default_finalize;
	object_class->dispose = gtk_source_undo_manager_default_dispose;

	object_class->set_property = gtk_source_undo_manager_default_set_property;
	object_class->get_property = gtk_source_undo_manager_default_get_property;

	g_object_class_install_property (object_class,
	                                 PROP_BUFFER,
	                                 g_param_spec_object ("buffer",
	                                                      _("Buffer"),
	                                                      _("The text buffer to add undo support on"),
	                                                      GTK_TYPE_TEXT_BUFFER,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_MAX_UNDO_LEVELS,
	                                 g_param_spec_int ("max-undo-levels",
	                                                   _("Maximum Undo Levels"),
	                                                   _("Number of undo levels for "
							     "the buffer"),
	                                                   -1,
	                                                   G_MAXINT,
	                                                   DEFAULT_MAX_UNDO_LEVELS,
	                                                   G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_type_class_add_private (object_class, sizeof(GtkSourceUndoManagerDefaultPrivate));
}

static void
gtk_source_undo_manager_default_init (GtkSourceUndoManagerDefault *um)
{
	um->priv = G_TYPE_INSTANCE_GET_PRIVATE (um, GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT,
	                                        GtkSourceUndoManagerDefaultPrivate);

	um->priv->actions = g_ptr_array_new ();
}

static void
end_not_undoable_action_internal (GtkSourceUndoManagerDefault *manager)
{
	g_return_if_fail (manager->priv->running_not_undoable_actions > 0);

	--manager->priv->running_not_undoable_actions;
}

/* Interface implementations */
static void
gtk_source_undo_manager_begin_not_undoable_action_impl (GtkSourceUndoManager *manager)
{
	GtkSourceUndoManagerDefault *manager_default;

	manager_default = GTK_SOURCE_UNDO_MANAGER_DEFAULT (manager);
	++manager_default->priv->running_not_undoable_actions;
}

static void
gtk_source_undo_manager_end_not_undoable_action_impl (GtkSourceUndoManager *manager)
{
	GtkSourceUndoManagerDefault *manager_default;

	manager_default = GTK_SOURCE_UNDO_MANAGER_DEFAULT (manager);
	end_not_undoable_action_internal (manager_default);

	if (manager_default->priv->running_not_undoable_actions == 0)
	{
		clear_undo (manager_default);
	}
}

static gboolean
gtk_source_undo_manager_can_undo_impl (GtkSourceUndoManager *manager)
{
	return GTK_SOURCE_UNDO_MANAGER_DEFAULT (manager)->priv->can_undo;
}

static gboolean
gtk_source_undo_manager_can_redo_impl (GtkSourceUndoManager *manager)
{
	return GTK_SOURCE_UNDO_MANAGER_DEFAULT (manager)->priv->can_redo;
}

static void
set_cursor (GtkTextBuffer *buffer,
            gint           cursor)
{
	GtkTextIter iter;

	/* Place the cursor at the requested position */
	gtk_text_buffer_get_iter_at_offset (buffer, &iter, cursor);
	gtk_text_buffer_place_cursor (buffer, &iter);
}

static void
insert_text (GtkTextBuffer *buffer,
             gint           pos,
             const gchar   *text,
             gint           len)
{
	GtkTextIter iter;

	gtk_text_buffer_get_iter_at_offset (buffer, &iter, pos);
	gtk_text_buffer_insert (buffer, &iter, text, len);
}

static void
delete_text (GtkTextBuffer *buffer,
             gint           start,
             gint           end)
{
	GtkTextIter start_iter;
	GtkTextIter end_iter;

	gtk_text_buffer_get_iter_at_offset (buffer, &start_iter, start);

	if (end < 0)
		gtk_text_buffer_get_end_iter (buffer, &end_iter);
	else
		gtk_text_buffer_get_iter_at_offset (buffer, &end_iter, end);

	gtk_text_buffer_delete (buffer, &start_iter, &end_iter);
}

static gchar *
get_chars (GtkTextBuffer *buffer,
           gint           start,
           gint           end)
{
	GtkTextIter start_iter;
	GtkTextIter end_iter;

	gtk_text_buffer_get_iter_at_offset (buffer, &start_iter, start);

	if (end < 0)
		gtk_text_buffer_get_end_iter (buffer, &end_iter);
	else
		gtk_text_buffer_get_iter_at_offset (buffer, &end_iter, end);

	return gtk_text_buffer_get_slice (buffer, &start_iter, &end_iter, TRUE);
}

static GtkSourceUndoAction *
action_list_nth_data (GPtrArray *array,
                      gint       n)
{
	if (n < 0 || n >= (gint)array->len)
		return NULL;
	else
		return array->pdata[array->len - 1 - n];
}

static void
action_list_prepend (GPtrArray           *array,
                     GtkSourceUndoAction *action)
{
	g_ptr_array_add (array, action);
}

static GtkSourceUndoAction *
action_list_last_data (GPtrArray *array)
{
	if (array->len != 0)
		return array->pdata[0];
	else
		return NULL;
}

static void
action_list_delete_last (GPtrArray *array)
{
	if (array->len != 0)
	{
		memmove (&array->pdata[0], &array->pdata[1], (array->len - 1)*sizeof (gpointer));
		g_ptr_array_set_size (array, array->len - 1);
	}
}

static void
gtk_source_undo_manager_undo_impl (GtkSourceUndoManager *manager)
{
	GtkSourceUndoManagerDefault *manager_default;
	GtkSourceUndoAction *undo_action;
	gboolean modified = FALSE;
	gint cursor_pos = -1;

	manager_default = GTK_SOURCE_UNDO_MANAGER_DEFAULT (manager);

	g_return_if_fail (manager_default->priv->can_undo);

	manager_default->priv->modified_undoing_group = FALSE;

	gtk_source_undo_manager_begin_not_undoable_action (manager);

	do
	{
		undo_action = action_list_nth_data (manager_default->priv->actions,
		                                    manager_default->priv->next_redo + 1);

		g_return_if_fail (undo_action != NULL);

		/* undo_action->modified can be TRUE only if undo_action->order_in_group <= 1 */
		g_return_if_fail ((undo_action->order_in_group <= 1) ||
				  ((undo_action->order_in_group > 1) && !undo_action->modified));

		if (undo_action->order_in_group <= 1)
		{
			/* Set modified to TRUE only if the buffer did not change its state from
			 * "not modified" to "modified" undoing an action (with order_in_group > 1)
			 * in current group. */
			modified = (undo_action->modified &&
			            !manager_default->priv->modified_undoing_group);
		}

		switch (undo_action->action_type)
		{
			case GTK_SOURCE_UNDO_ACTION_DELETE:
				insert_text (manager_default->priv->buffer,
				             undo_action->action.delete.start,
				             undo_action->action.delete.text,
				             strlen (undo_action->action.delete.text));

				if (undo_action->action.delete.forward)
					cursor_pos = undo_action->action.delete.start;
				else
					cursor_pos = undo_action->action.delete.end;

				break;

			case GTK_SOURCE_UNDO_ACTION_INSERT:
				delete_text (manager_default->priv->buffer,
				             undo_action->action.insert.pos,
				             undo_action->action.insert.pos +
				             undo_action->action.insert.chars);

				cursor_pos = undo_action->action.insert.pos;
				break;

			default:
				/* Unknown action type. */
				g_return_if_reached ();
		}

		++manager_default->priv->next_redo;

	} while (undo_action->order_in_group > 1);

	if (cursor_pos >= 0)
		set_cursor (manager_default->priv->buffer, cursor_pos);

	if (modified)
	{
		--manager_default->priv->next_redo;
		gtk_text_buffer_set_modified (manager_default->priv->buffer, FALSE);
		++manager_default->priv->next_redo;
	}

	/* FIXME: why does this call the internal one ? */
	end_not_undoable_action_internal (manager_default);

	manager_default->priv->modified_undoing_group = FALSE;

	if (!manager_default->priv->can_redo)
	{
		manager_default->priv->can_redo = TRUE;
		gtk_source_undo_manager_can_redo_changed (manager);
	}

	if (manager_default->priv->next_redo >= (gint)manager_default->priv->actions->len - 1)
	{
		manager_default->priv->can_undo = FALSE;
		gtk_source_undo_manager_can_undo_changed (manager);
	}
}

static void
gtk_source_undo_manager_redo_impl (GtkSourceUndoManager *manager)
{
	GtkSourceUndoManagerDefault *manager_default;
	GtkSourceUndoAction *undo_action;
	gboolean modified = FALSE;
	gint cursor_pos = -1;

	manager_default = GTK_SOURCE_UNDO_MANAGER_DEFAULT (manager);

	g_return_if_fail (manager_default->priv->can_redo);

	undo_action = action_list_nth_data (manager_default->priv->actions,
	                                    manager_default->priv->next_redo);
	g_return_if_fail (undo_action != NULL);

	gtk_source_undo_manager_begin_not_undoable_action (manager);

	do
	{
		if (undo_action->modified)
		{
			g_return_if_fail (undo_action->order_in_group <= 1);
			modified = TRUE;
		}

		--manager_default->priv->next_redo;

		switch (undo_action->action_type)
		{
			case GTK_SOURCE_UNDO_ACTION_DELETE:
				delete_text (manager_default->priv->buffer,
				             undo_action->action.delete.start,
				             undo_action->action.delete.end);

				cursor_pos = undo_action->action.delete.start;

				break;

			case GTK_SOURCE_UNDO_ACTION_INSERT:
				cursor_pos = undo_action->action.insert.pos +
				             undo_action->action.insert.length;

				insert_text (manager_default->priv->buffer,
				             undo_action->action.insert.pos,
				             undo_action->action.insert.text,
				             undo_action->action.insert.length);

				break;

			default:
				/* Unknown action type */
				++manager_default->priv->next_redo;
				g_return_if_reached ();
		}

		if (manager_default->priv->next_redo < 0)
			undo_action = NULL;
		else
			undo_action = action_list_nth_data (manager_default->priv->actions,
			                                    manager_default->priv->next_redo);

	} while ((undo_action != NULL) && (undo_action->order_in_group > 1));

	if (cursor_pos >= 0)
		set_cursor (manager_default->priv->buffer, cursor_pos);

	if (modified)
	{
		++manager_default->priv->next_redo;
		gtk_text_buffer_set_modified (manager_default->priv->buffer, FALSE);
		--manager_default->priv->next_redo;
	}

	/* FIXME: why is this only internal ?*/
	end_not_undoable_action_internal (manager_default);

	if (manager_default->priv->next_redo < 0)
	{
		manager_default->priv->can_redo = FALSE;
		gtk_source_undo_manager_can_redo_changed (manager);
	}

	if (!manager_default->priv->can_undo)
	{
		manager_default->priv->can_undo = TRUE;
		gtk_source_undo_manager_can_undo_changed (manager);
	}
}

static void
gtk_source_undo_action_free (GtkSourceUndoAction *action)
{
	if (action == NULL)
		return;

	if (action->action_type == GTK_SOURCE_UNDO_ACTION_INSERT)
		g_free (action->action.insert.text);
	else if (action->action_type == GTK_SOURCE_UNDO_ACTION_DELETE)
		g_free (action->action.delete.text);
	else
		g_return_if_reached ();

	g_free (action);
}

static void
free_action_list (GtkSourceUndoManagerDefault *um)
{
	gint i;

	for (i = (gint)um->priv->actions->len - 1; i >= 0; i--)
	{
		GtkSourceUndoAction *action = um->priv->actions->pdata[i];

		if (action->order_in_group == 1)
			--um->priv->num_of_groups;

		if (action->modified)
			um->priv->modified_action = INVALID;

		gtk_source_undo_action_free (action);
	}

	/* Some arbitrary limit, to avoid wasting space */
	if (um->priv->actions->len > 2048)
	{
		g_ptr_array_free (um->priv->actions, TRUE);
		um->priv->actions = g_ptr_array_new ();
	}
	else
	{
		g_ptr_array_set_size (um->priv->actions, 0);
	}
}

static void
insert_text_handler (GtkTextBuffer               *buffer,
                     GtkTextIter                 *pos,
                     const gchar                 *text,
                     gint                         length,
                     GtkSourceUndoManagerDefault *manager)
{
	GtkSourceUndoAction undo_action;

	if (manager->priv->running_not_undoable_actions > 0)
		return;

	undo_action.action_type = GTK_SOURCE_UNDO_ACTION_INSERT;

	undo_action.action.insert.pos    = gtk_text_iter_get_offset (pos);
	undo_action.action.insert.text   = (gchar*) text;
	undo_action.action.insert.length = length;
	undo_action.action.insert.chars  = g_utf8_strlen (text, length);

	if ((undo_action.action.insert.chars > 1) || (g_utf8_get_char (text) == '\n'))

		undo_action.mergeable = FALSE;
	else
		undo_action.mergeable = TRUE;

	undo_action.modified = FALSE;

	add_action (manager, &undo_action);
}

static void
delete_range_handler (GtkTextBuffer               *buffer,
                      GtkTextIter                 *start,
                      GtkTextIter                 *end,
                      GtkSourceUndoManagerDefault *um)
{
	GtkSourceUndoAction undo_action;
	GtkTextIter insert_iter;

	if (um->priv->running_not_undoable_actions > 0)
		return;

	undo_action.action_type = GTK_SOURCE_UNDO_ACTION_DELETE;

	gtk_text_iter_order (start, end);

	undo_action.action.delete.start  = gtk_text_iter_get_offset (start);
	undo_action.action.delete.end    = gtk_text_iter_get_offset (end);

	undo_action.action.delete.text   = get_chars (
						buffer,
						undo_action.action.delete.start,
						undo_action.action.delete.end);

	/* figure out if the user used the Delete or the Backspace key */
	gtk_text_buffer_get_iter_at_mark (buffer, &insert_iter,
					  gtk_text_buffer_get_insert (buffer));
	if (gtk_text_iter_get_offset (&insert_iter) <= undo_action.action.delete.start)
		undo_action.action.delete.forward = TRUE;
	else
		undo_action.action.delete.forward = FALSE;

	if (((undo_action.action.delete.end - undo_action.action.delete.start) > 1) ||
	     (g_utf8_get_char (undo_action.action.delete.text  ) == '\n'))
		undo_action.mergeable = FALSE;
	else
		undo_action.mergeable = TRUE;

	undo_action.modified = FALSE;

	add_action (um, &undo_action);

	g_free (undo_action.action.delete.text);

}

static void
begin_user_action_handler (GtkTextBuffer               *buffer,
                           GtkSourceUndoManagerDefault *um)
{
	if (um->priv->running_not_undoable_actions > 0)
		return;

	um->priv->actions_in_current_group = 0;
}

static void
add_action (GtkSourceUndoManagerDefault *um,
            const GtkSourceUndoAction   *undo_action)
{
	GtkSourceUndoAction* action;

	if (um->priv->next_redo >= 0)
	{
		free_first_n_actions (um, um->priv->next_redo + 1);
	}

	um->priv->next_redo = -1;

	if (!merge_action (um, undo_action))
	{
		action = g_new (GtkSourceUndoAction, 1);
		*action = *undo_action;

		if (action->action_type == GTK_SOURCE_UNDO_ACTION_INSERT)
			action->action.insert.text = g_strndup (undo_action->action.insert.text, undo_action->action.insert.length);
		else if (action->action_type == GTK_SOURCE_UNDO_ACTION_DELETE)
			action->action.delete.text = g_strdup (undo_action->action.delete.text);
		else
		{
			g_free (action);
			g_return_if_reached ();
		}

		++um->priv->actions_in_current_group;
		action->order_in_group = um->priv->actions_in_current_group;

		if (action->order_in_group == 1)
			++um->priv->num_of_groups;

		action_list_prepend (um->priv->actions, action);
	}

	check_list_size (um);

	if (!um->priv->can_undo)
	{
		um->priv->can_undo = TRUE;
		gtk_source_undo_manager_can_undo_changed (GTK_SOURCE_UNDO_MANAGER (um));
	}

	if (um->priv->can_redo)
	{
		um->priv->can_redo = FALSE;
		gtk_source_undo_manager_can_redo_changed (GTK_SOURCE_UNDO_MANAGER (um));
	}
}

static void
free_first_n_actions (GtkSourceUndoManagerDefault *um,
                      gint                         n)
{
	gint i;

	if (um->priv->actions->len == 0)
		return;

	for (i = 0; i < n; i++)
	{
		GtkSourceUndoAction *action = um->priv->actions->pdata[um->priv->actions->len - 1];

		if (action->order_in_group == 1)
			--um->priv->num_of_groups;

		if (action->modified)
			um->priv->modified_action = INVALID;

		gtk_source_undo_action_free (action);

		g_ptr_array_set_size (um->priv->actions, um->priv->actions->len - 1);

		if (um->priv->actions->len == 0)
			return;
	}
}

static void
check_list_size (GtkSourceUndoManagerDefault *um)
{
	gint undo_levels;

	undo_levels = um->priv->max_undo_levels;

	if (undo_levels < 1)
		return;

	if (um->priv->num_of_groups > undo_levels)
	{
		GtkSourceUndoAction *undo_action;

		undo_action = action_list_last_data (um->priv->actions);

		do
		{
			if (undo_action->order_in_group == 1)
				--um->priv->num_of_groups;

			if (undo_action->modified)
				um->priv->modified_action = INVALID;

			gtk_source_undo_action_free (undo_action);

			action_list_delete_last (um->priv->actions);

			undo_action = action_list_last_data (um->priv->actions);
			g_return_if_fail (undo_action != NULL);

		} while ((undo_action->order_in_group > 1) ||
			 (um->priv->num_of_groups > undo_levels));
	}
}

/**
 * gtk_source_undo_manager_default_merge_action:
 * @um: a #GtkSourceUndoManagerDefault.
 * @undo_action: a #GtkSourceUndoAction.
 *
 * This function tries to merge the undo action at the top of
 * the stack with a new undo action. So when we undo for example
 * typing, we can undo the whole word and not each letter by itself.
 *
 * Return Value: %TRUE is merge was sucessful, %FALSE otherwise.
 **/
static gboolean
merge_action (GtkSourceUndoManagerDefault *um,
              const GtkSourceUndoAction   *undo_action)
{
	GtkSourceUndoAction *last_action;

	g_return_val_if_fail (GTK_IS_SOURCE_UNDO_MANAGER_DEFAULT (um), FALSE);
	g_return_val_if_fail (um->priv != NULL, FALSE);

	if (um->priv->actions->len == 0)
		return FALSE;

	last_action = action_list_nth_data (um->priv->actions, 0);

	if (!last_action->mergeable)
		return FALSE;

	if ((!undo_action->mergeable) ||
	    (undo_action->action_type != last_action->action_type))
	{
		last_action->mergeable = FALSE;
		return FALSE;
	}

	if (undo_action->action_type == GTK_SOURCE_UNDO_ACTION_DELETE)
	{
		if ((last_action->action.delete.forward != undo_action->action.delete.forward) ||
		    ((last_action->action.delete.start != undo_action->action.delete.start) &&
		     (last_action->action.delete.start != undo_action->action.delete.end)))
		{
			last_action->mergeable = FALSE;
			return FALSE;
		}

		if (last_action->action.delete.start == undo_action->action.delete.start)
		{
			gchar *str;

#define L  (last_action->action.delete.end - last_action->action.delete.start - 1)
#define g_utf8_get_char_at(p,i) g_utf8_get_char(g_utf8_offset_to_pointer((p),(i)))

			/* Deleted with the delete key */
			if ((g_utf8_get_char (undo_action->action.delete.text) != ' ') &&
			    (g_utf8_get_char (undo_action->action.delete.text) != '\t') &&
                            ((g_utf8_get_char_at (last_action->action.delete.text, L) == ' ') ||
			     (g_utf8_get_char_at (last_action->action.delete.text, L)  == '\t')))
			{
				last_action->mergeable = FALSE;
				return FALSE;
			}

			str = g_strdup_printf ("%s%s", last_action->action.delete.text,
				undo_action->action.delete.text);

			g_free (last_action->action.delete.text);
			last_action->action.delete.end += (undo_action->action.delete.end -
							   undo_action->action.delete.start);
			last_action->action.delete.text = str;
		}
		else
		{
			gchar *str;

			/* Deleted with the backspace key */
			if ((g_utf8_get_char (undo_action->action.delete.text) != ' ') &&
			    (g_utf8_get_char (undo_action->action.delete.text) != '\t') &&
                            ((g_utf8_get_char (last_action->action.delete.text) == ' ') ||
			     (g_utf8_get_char (last_action->action.delete.text) == '\t')))
			{
				last_action->mergeable = FALSE;
				return FALSE;
			}

			str = g_strdup_printf ("%s%s", undo_action->action.delete.text,
				last_action->action.delete.text);

			g_free (last_action->action.delete.text);
			last_action->action.delete.start = undo_action->action.delete.start;
			last_action->action.delete.text = str;
		}
	}
	else if (undo_action->action_type == GTK_SOURCE_UNDO_ACTION_INSERT)
	{
		gchar* str;

#define I (last_action->action.insert.chars - 1)

		if ((undo_action->action.insert.pos !=
		     	(last_action->action.insert.pos + last_action->action.insert.chars)) ||
		    ((g_utf8_get_char (undo_action->action.insert.text) != ' ') &&
		      (g_utf8_get_char (undo_action->action.insert.text) != '\t') &&
		     ((g_utf8_get_char_at (last_action->action.insert.text, I) == ' ') ||
		      (g_utf8_get_char_at (last_action->action.insert.text, I) == '\t')))
		   )
		{
			last_action->mergeable = FALSE;
			return FALSE;
		}

		str = g_strdup_printf ("%s%s", last_action->action.insert.text,
				undo_action->action.insert.text);

		g_free (last_action->action.insert.text);
		last_action->action.insert.length += undo_action->action.insert.length;
		last_action->action.insert.text = str;
		last_action->action.insert.chars += undo_action->action.insert.chars;

	}
	else
		/* Unknown action inside undo merge encountered */
		g_return_val_if_reached (TRUE);

	return TRUE;
}

static void
modified_changed_handler (GtkTextBuffer               *buffer,
                          GtkSourceUndoManagerDefault *manager)
{
	GtkSourceUndoAction *action;
	gint idx;

	if (manager->priv->actions->len == 0)
		return;

	idx = manager->priv->next_redo + 1;
	action = action_list_nth_data (manager->priv->actions, idx);

	if (gtk_text_buffer_get_modified (buffer) == FALSE)
	{
		if (action != NULL)
			action->mergeable = FALSE;

		if (manager->priv->modified_action != NULL)
		{
			if (manager->priv->modified_action != INVALID)
				manager->priv->modified_action->modified = FALSE;

			manager->priv->modified_action = NULL;
		}

		return;
	}

	if (action == NULL)
	{
		g_return_if_fail (manager->priv->running_not_undoable_actions > 0);

		return;
	}

	if (manager->priv->modified_action != NULL)
	{
		g_message ("%s: oops", G_STRLOC);
		return;
	}

	if (action->order_in_group > 1)
		manager->priv->modified_undoing_group  = TRUE;

	while (action->order_in_group > 1)
	{
		action = action_list_nth_data (manager->priv->actions, ++idx);
		g_return_if_fail (action != NULL);
	}

	action->modified = TRUE;
	manager->priv->modified_action = action;
}

static void
gtk_source_undo_manager_iface_init (GtkSourceUndoManagerIface *iface)
{
	iface->can_undo = gtk_source_undo_manager_can_undo_impl;
	iface->can_redo = gtk_source_undo_manager_can_redo_impl;

	iface->undo = gtk_source_undo_manager_undo_impl;
	iface->redo = gtk_source_undo_manager_redo_impl;

	iface->begin_not_undoable_action = gtk_source_undo_manager_begin_not_undoable_action_impl;
	iface->end_not_undoable_action = gtk_source_undo_manager_end_not_undoable_action_impl;
}

void
gtk_source_undo_manager_default_set_max_undo_levels (GtkSourceUndoManagerDefault *manager,
                                                     gint                         max_undo_levels)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER_DEFAULT (manager));

	set_max_undo_levels (manager, max_undo_levels);
	g_object_notify (G_OBJECT (manager), "max-undo-levels");
}
