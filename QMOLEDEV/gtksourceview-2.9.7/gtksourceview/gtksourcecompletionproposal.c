/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/* gtksourcecompletionproposal.c
 * This file is part of gtksourcecompletion
 *
 * Copyright (C) 2007 - 2009 Jesús Barbero Rodríguez <chuchiperriman@gmail.com>
 * Copyright (C) 2009 - Jesse van den Kieboom <jessevdk@gnome.org>
 *
 * gtksourceview is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * gtksourceview is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/**
 * SECTION:completionproposal
 * @title: GtkSourceCompletionProposal
 * @short_description: Completion proposal object
 *
 * The proposal interface represents a completion item in the completion window.
 * It provides information on how to display the completion item and what action
 * should be taken when the completion item is activated.
 */

#include <gtksourceview/gtksourcecompletionproposal.h>

#include "gtksourceview-marshal.h"

/* Signals */
enum
{
	CHANGED,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS] = {0,};

static gchar *
gtk_source_completion_proposal_get_label_default (GtkSourceCompletionProposal *proposal)
{
	return NULL;
}

static gchar *
gtk_source_completion_proposal_get_markup_default (GtkSourceCompletionProposal *proposal)
{
	return NULL;
}

static gchar *
gtk_source_completion_proposal_get_text_default (GtkSourceCompletionProposal *proposal)
{
	return NULL;
}

static GdkPixbuf *
gtk_source_completion_proposal_get_icon_default (GtkSourceCompletionProposal *proposal)
{
	return NULL;
}

static gchar *
gtk_source_completion_proposal_get_info_default (GtkSourceCompletionProposal *proposal)
{
	return NULL;
}

static guint
gtk_source_completion_proposal_hash_default	(GtkSourceCompletionProposal *proposal)
{
	return g_direct_hash (proposal);
}

static gboolean
gtk_source_completion_proposal_equal_default (GtkSourceCompletionProposal *proposal,
                                              GtkSourceCompletionProposal *other)
{
	return g_direct_equal (proposal, other);
}

static void 
gtk_source_completion_proposal_init (GtkSourceCompletionProposalIface *iface)
{
	static gboolean initialized = FALSE;
	
	iface->get_label = gtk_source_completion_proposal_get_label_default;
	iface->get_markup = gtk_source_completion_proposal_get_markup_default;
	iface->get_text = gtk_source_completion_proposal_get_text_default;
	
	iface->get_icon = gtk_source_completion_proposal_get_icon_default;
	iface->get_info = gtk_source_completion_proposal_get_info_default;
	iface->hash = gtk_source_completion_proposal_hash_default;
	iface->equal = gtk_source_completion_proposal_equal_default;
	
	if (!initialized)
	{
		/**
		 * GtkSourceCompletionProposal::changed:
		 * @proposal: The #GtkSourceCompletionProposal
		 *
		 * Emitted when the proposal has changed. The completion popup
		 * will react to this by updating the shown information.
		 *
		 */
		signals[CHANGED] = 
			g_signal_new ("changed",
			      G_TYPE_FROM_INTERFACE (iface),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceCompletionProposalIface, changed),
			      NULL, 
			      NULL,
			      g_cclosure_marshal_VOID__VOID, 
			      G_TYPE_NONE,
			      0);

		initialized = TRUE;
	}
}

GType 
gtk_source_completion_proposal_get_type ()
{
	static GType gtk_source_completion_proposal_type_id = 0;
	
	if (!gtk_source_completion_proposal_type_id)
	{
		static const GTypeInfo g_define_type_info =
		{
			sizeof (GtkSourceCompletionProposalIface),
			(GBaseInitFunc) gtk_source_completion_proposal_init, 
			NULL,
			NULL,
			NULL,
			NULL,
			0,
			0,
			NULL
		};
		
		gtk_source_completion_proposal_type_id = 
			g_type_register_static (G_TYPE_INTERFACE,
						"GtkSourceCompletionProposal",
						&g_define_type_info,
						0);

		g_type_interface_add_prerequisite (gtk_source_completion_proposal_type_id,
		                                   G_TYPE_OBJECT);
	}
	
	return gtk_source_completion_proposal_type_id;
}

/**
 * gtk_source_completion_proposal_get_label:
 * @proposal: A #GtkSourceCompletionProposal
 *
 * Gets the label of @proposal. The label is shown in the list of proposals as
 * plain text. If you need any markup (such as bold or italic text), you have
 * to implement #gtk_source_completion_proposal_get_markup. The returned string
 * must be freed with g_free().
 *
 * Returns: A new string containing the label of @proposal.
 */
gchar *
gtk_source_completion_proposal_get_label (GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), NULL);	
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->get_label (proposal);
}

/**
 * gtk_source_completion_proposal_get_markup:
 * @proposal: A #GtkSourceCompletionProposal
 *
 * Gets the label of @proposal with markup. The label is shown in the list of 
 * proposals and may contain markup. This will be used instead of
 * #gtk_source_completion_proposal_get_label if implemented. The returned string
 * must be freed with g_free().
 *
 * Returns: A new string containing the label of @proposal with markup.
 */
gchar *
gtk_source_completion_proposal_get_markup (GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), NULL);	
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->get_markup (proposal);
}

/**
 * gtk_source_completion_proposal_get_text:
 * @proposal: A #GtkSourceCompletionProposal
 *
 * Gets the text of @proposal. The text that is inserted into
 * the text buffer when the proposal is activated by the default activation.
 * You are free to implement a custom activation handler in the provider and
 * not implement this function. The returned string must be freed with g_free().
 *
 * Returns: A new string containing the text of @proposal.
 */
gchar *
gtk_source_completion_proposal_get_text (GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), NULL);
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->get_text (proposal);
}

/**
 * gtk_source_completion_proposal_get_icon:
 * @proposal: A #GtkSourceCompletionProposal
 *
 * Gets the icon of @proposal.
 *
 * Returns: The icon of @proposal.
 */
GdkPixbuf *
gtk_source_completion_proposal_get_icon (GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), NULL);
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->get_icon (proposal);
}

/**
 * gtk_source_completion_proposal_get_info:
 * @proposal: A #GtkSourceCompletionProposal
 *
 * Gets extra information associated to the proposal. This information will be
 * used to present the user with extra, detailed information about the
 * selected proposal. The returned string must be freed with g_free().
 *
 * Returns: A new string containing extra information of @proposal or %NULL if
 *          no extra information is associated to @proposal.
 */
gchar *
gtk_source_completion_proposal_get_info (GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), NULL);
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->get_info (proposal);
}

/**
 * gtk_source_completion_proposal_hash:
 * @proposal: A #GtkSourceCompletionProposal
 * 
 * Get the hash value of @proposal. This is used to (together with
 * #gtk_source_completion_proposal_equal) to match proposals in the completion
 * model. By default, it uses a direct hash (#g_direct_hash).
 *
 * Returns: The hash value of @proposal
 *
 **/
guint
gtk_source_completion_proposal_hash	(GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), 0);
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->hash (proposal);
}

/**
 * gtk_source_completion_proposal_equal:
 * @proposal: A #GtkSourceCompletionProposal
 * @other: A #GtkSourceCompletionProposal
 * 
 * Get whether two proposal objects are the same.  This is used to (together 
 * with #gtk_source_completion_proposal_hash) to match proposals in the 
 * completion model. By default, it uses direct equality (#g_direct_equal).
 *
 * Returns: %TRUE if @proposal and @object are the same proposal
 *
 **/
gboolean
gtk_source_completion_proposal_equal (GtkSourceCompletionProposal *proposal,
                                      GtkSourceCompletionProposal *other)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), FALSE);
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (other), FALSE);
	
	return GTK_SOURCE_COMPLETION_PROPOSAL_GET_INTERFACE (proposal)->equal (proposal, other);
}

/**
 * gtk_source_completion_proposal_changed:
 * @proposal: A #GtkSourceCompletionProposal
 *
 * Emits the "changed" signal on @proposal. This should be called by
 * implementations whenever the name, icon or info of the proposal has
 * changed.
 */
void
gtk_source_completion_proposal_changed (GtkSourceCompletionProposal *proposal)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal));
	g_signal_emit (proposal, signals[CHANGED], 0);
}
