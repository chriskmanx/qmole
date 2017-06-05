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

#include "gtksourceundomanager.h"
#include "gtksourceview-marshal.h"

/**
 * SECTION:undomanager
 * @short_description: Undo manager interface for #GtkSourceView
 * @title: GtkSourceUndoManager
 * @see_also: #GtkTextBuffer, #GtkSourceView
 *
 * The #GtkSourceUndoManager interface can be implemented to provide custom
 * undo management to a #GtkSourceBuffer. Use
 * #gtk_source_buffer_set_undo_manager to install a custom undo manager for
 * a particular source buffer.
 *
 * Use #gtk_source_undo_manager_can_undo_changed and
 * #gtk_source_undo_manager_can_redo_changed when respectively the undo state
 * or redo state of the undo stack has changed.
 *
 * Since: 2.10
 *
 */

/* Signals */
enum
{
	CAN_UNDO_CHANGED,
	CAN_REDO_CHANGED,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS] = {0,};

static gboolean
gtk_source_undo_manager_can_undo_default (GtkSourceUndoManager *manager)
{
	return FALSE;
}

static gboolean
gtk_source_undo_manager_can_redo_default (GtkSourceUndoManager *manager)
{
	return FALSE;
}

static void
gtk_source_undo_manager_undo_default (GtkSourceUndoManager *manager)
{
}

static void
gtk_source_undo_manager_redo_default (GtkSourceUndoManager *manager)
{
}

static void
gtk_source_undo_manager_begin_not_undoable_action_default (GtkSourceUndoManager *manager)
{
}

static void
gtk_source_undo_manager_end_not_undoable_action_default (GtkSourceUndoManager *manager)
{
}

static void
gtk_source_undo_manager_init (GtkSourceUndoManagerIface *iface)
{
	static gboolean initialized = FALSE;

	iface->can_undo = gtk_source_undo_manager_can_undo_default;
	iface->can_redo = gtk_source_undo_manager_can_redo_default;

	iface->undo = gtk_source_undo_manager_undo_default;
	iface->redo = gtk_source_undo_manager_redo_default;

	iface->begin_not_undoable_action = gtk_source_undo_manager_begin_not_undoable_action_default;
	iface->end_not_undoable_action = gtk_source_undo_manager_end_not_undoable_action_default;

	if (!initialized)
	{
		/**
		 * GtkSourceUndoManager::can-undo-changed:
		 * @manager: The #GtkSourceUndoManager
		 *
		 * Emitted when the ability to undo has changed.
		 *
		 * Since: 2.10
		 *
		 */
		signals[CAN_UNDO_CHANGED] =
			g_signal_new ("can-undo-changed",
			      G_TYPE_FROM_INTERFACE (iface),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceUndoManagerIface, can_undo_changed),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

		/**
		 * GtkSourceUndoManager::can-redo-changed:
		 * @manager: The #GtkSourceUndoManager
		 *
		 * Emitted when the ability to redo has changed.
		 *
		 * Since: 2.10
		 *
		 */
		signals[CAN_REDO_CHANGED] =
			g_signal_new ("can-redo-changed",
			      G_TYPE_FROM_INTERFACE (iface),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceUndoManagerIface, can_redo_changed),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

		initialized = TRUE;
	}
}

GType
gtk_source_undo_manager_get_type ()
{
	static GType gtk_source_undo_manager_type_id = 0;
	
	if (!gtk_source_undo_manager_type_id)
	{
		static const GTypeInfo g_define_type_info =
		{
			sizeof (GtkSourceUndoManagerIface),
			(GBaseInitFunc) gtk_source_undo_manager_init,
			NULL,
			NULL,
			NULL,
			NULL,
			0,
			0,
			NULL
		};
		
		gtk_source_undo_manager_type_id =
			g_type_register_static (G_TYPE_INTERFACE,
						"GtkSourceUndoManager",
						&g_define_type_info,
						0);

		g_type_interface_add_prerequisite (gtk_source_undo_manager_type_id,
		                                   G_TYPE_OBJECT);
	}
	
	return gtk_source_undo_manager_type_id;
}

/**
 * gtk_source_undo_manager_can_undo:
 * @manager: A #GtkSourceUndoManager
 *
 * Get whether there are undo operations available.
 *
 * Since: 2.10
 *
 * Returns: %TRUE if there are undo operations available, %FALSE otherwise
 *
 */
gboolean
gtk_source_undo_manager_can_undo (GtkSourceUndoManager *manager)
{
	g_return_val_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager), FALSE);
	return GTK_SOURCE_UNDO_MANAGER_GET_INTERFACE (manager)->can_undo (manager);
}

/**
 * gtk_source_undo_manager_can_redo:
 * @manager: A #GtkSourceUndoManager
 *
 * Get whether there are redo operations available.
 *
 * Since: 2.10
 *
 * Returns: %TRUE if there are redo operations available, %FALSE otherwise
 *
 */
gboolean
gtk_source_undo_manager_can_redo (GtkSourceUndoManager *manager)
{
	g_return_val_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager), FALSE);
	return GTK_SOURCE_UNDO_MANAGER_GET_INTERFACE (manager)->can_redo (manager);
}

/**
 * gtk_source_undo_manager_undo:
 * @manager: A #GtkSourceUndoManager
 *
 * Perform a single undo. Calling this function when there are no undo operations
 * available is an error. Use #gtk_source_undo_manager_can_undo to find out
 * if there are undo operations available.
 *
 * Since: 2.10
 *
 */
void
gtk_source_undo_manager_undo (GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager));
	GTK_SOURCE_UNDO_MANAGER_GET_INTERFACE (manager)->undo (manager);
}

/**
 * gtk_source_undo_manager_redo:
 * @manager: A #GtkSourceUndoManager
 *
 * Perform a single redo. Calling this function when there are no redo operations
 * available is an error. Use #gtk_source_undo_manager_can_redo to find out
 * if there are redo operations available.
 *
 * Since: 2.10
 *
 */
void
gtk_source_undo_manager_redo (GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager));
	GTK_SOURCE_UNDO_MANAGER_GET_INTERFACE (manager)->redo (manager);
}

/**
 * gtk_source_undo_manager_begin_not_undoable_action:
 * @manager: A #GtkSourceUndoManager
 *
 * Begin a not undoable action on the buffer. All changes between this call
 * and the call to #gtk_source_undo_manager_end_not_undoable_action cannot
 * be undone. This function should be re-entrant.
 *
 * Since: 2.10
 *
 */
void
gtk_source_undo_manager_begin_not_undoable_action (GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager));
	GTK_SOURCE_UNDO_MANAGER_GET_INTERFACE (manager)->begin_not_undoable_action (manager);
}

/**
 * gtk_source_undo_manager_end_not_undoable_action:
 * @manager: A #GtkSourceUndoManager
 *
 * Ends a not undoable action on the buffer.
 *
 * Since: 2.10
 *
 */
void
gtk_source_undo_manager_end_not_undoable_action (GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager));
	GTK_SOURCE_UNDO_MANAGER_GET_INTERFACE (manager)->end_not_undoable_action (manager);
}

/**
 * gtk_source_undo_manager_can_undo_changed:
 * @manager: A #GtkSourceUndoManager
 * 
 * Emits the #GtkSourceUndoManager::can-undo-changed signal.
 *
 * Since: 2.10
 *
 **/
void
gtk_source_undo_manager_can_undo_changed (GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager));

	g_signal_emit (manager, signals[CAN_UNDO_CHANGED], 0);
}

/**
 * gtk_source_undo_manager_can_redo_changed:
 * @manager: A #GtkSourceUndoManager
 * 
 * Emits the #GtkSourceUndoManager::can-redo-changed signal.
 *
 * Since: 2.10
 *
 **/
void
gtk_source_undo_manager_can_redo_changed (GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_UNDO_MANAGER (manager));

	g_signal_emit (manager, signals[CAN_REDO_CHANGED], 0);
}
