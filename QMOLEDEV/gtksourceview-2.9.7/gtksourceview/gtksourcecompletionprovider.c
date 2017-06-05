/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/* gtksourcecompletionprovider.c
 * This file is part of gtksourcecompletion
 *
 * Copyright (C) 2007 -2009 Jesús Barbero Rodríguez <chuchiperriman@gmail.com>
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
 * SECTION:completionprovider
 * @title: GtkSourceCompletionProvider
 * @short_description: Completion provider interface
 *
 * You must implement this interface to provide proposals to #GtkSourceCompletion
 * 
 */

#include <gtksourceview/gtksourcecompletionprovider.h>

/* Default implementations */
static gchar *
gtk_source_completion_provider_get_name_default (GtkSourceCompletionProvider *provider)
{
	g_return_val_if_reached (NULL);
}

static GdkPixbuf *
gtk_source_completion_provider_get_icon_default (GtkSourceCompletionProvider *provider)
{
	return NULL;
}

static void
gtk_source_completion_provider_populate_default (GtkSourceCompletionProvider *provider,
                                                 GtkSourceCompletionContext  *context)
{
	gtk_source_completion_context_add_proposals (context, provider, NULL, TRUE);
}

static GtkSourceCompletionActivation
gtk_source_completion_provider_get_activation_default (GtkSourceCompletionProvider *provider)
{
	return GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE |
	       GTK_SOURCE_COMPLETION_ACTIVATION_USER_REQUESTED;
}

static gboolean
gtk_source_completion_provider_match_default (GtkSourceCompletionProvider *provider,
                                              GtkSourceCompletionContext  *context)
{
	return TRUE;
}

static GtkWidget *
gtk_source_completion_provider_get_info_widget_default (GtkSourceCompletionProvider *provider,
                                                        GtkSourceCompletionProposal *proposal)
{
	return NULL;
}

static void
gtk_source_completion_provider_update_info_default (GtkSourceCompletionProvider *provider,
                                                    GtkSourceCompletionProposal *proposal,
                                                    GtkSourceCompletionInfo     *info)
{
}

static gboolean
gtk_source_completion_provider_get_start_iter_default (GtkSourceCompletionProvider *provider,
                                                       GtkSourceCompletionContext  *context,
                                                       GtkSourceCompletionProposal *proposal,
                                                       GtkTextIter                 *iter)
{
	return FALSE;
}

static gboolean
gtk_source_completion_provider_activate_proposal_default (GtkSourceCompletionProvider *provider,
                                                          GtkSourceCompletionProposal *proposal,
                                                          GtkTextIter                 *iter)
{
	return FALSE;
}

static gint
gtk_source_completion_provider_get_interactive_delay_default (GtkSourceCompletionProvider *provider)
{
	/* -1 means the default value in the completion object */
	return -1;
}

static gint
gtk_source_completion_provider_get_priority_default (GtkSourceCompletionProvider *provider)
{
	return 0;
}

static void 
gtk_source_completion_provider_base_init (GtkSourceCompletionProviderIface *iface)
{
	static gboolean initialized = FALSE;
	
	iface->get_name = gtk_source_completion_provider_get_name_default;
	iface->get_icon = gtk_source_completion_provider_get_icon_default;

	iface->populate = gtk_source_completion_provider_populate_default;

	iface->match = gtk_source_completion_provider_match_default;
	iface->get_activation = gtk_source_completion_provider_get_activation_default;
	
	iface->get_info_widget = gtk_source_completion_provider_get_info_widget_default;
	iface->update_info = gtk_source_completion_provider_update_info_default;
	
	iface->get_start_iter = gtk_source_completion_provider_get_start_iter_default;
	iface->activate_proposal = gtk_source_completion_provider_activate_proposal_default;

	iface->get_interactive_delay = gtk_source_completion_provider_get_interactive_delay_default;
	iface->get_priority = gtk_source_completion_provider_get_priority_default;

	if (!initialized)
	{
		initialized = TRUE;
	}
}

GType 
gtk_source_completion_provider_get_type ()
{
	static GType gtk_source_completion_provider_type_id = 0;

	if (!gtk_source_completion_provider_type_id)
	{
		static const GTypeInfo g_define_type_info = 
		{ 
			sizeof (GtkSourceCompletionProviderIface), 
			(GBaseInitFunc) gtk_source_completion_provider_base_init, 
			(GBaseFinalizeFunc) NULL, 
			(GClassInitFunc) NULL, 
			(GClassFinalizeFunc) NULL, 
			NULL, 
			0, 
			0, 
			(GInstanceInitFunc) NULL 
		};
						
		gtk_source_completion_provider_type_id = 
				g_type_register_static (G_TYPE_INTERFACE, 
							"GtkSourceCompletionProvider", 
							&g_define_type_info, 
							0);

		g_type_interface_add_prerequisite (gtk_source_completion_provider_type_id,
		                                   G_TYPE_OBJECT);
	}

	return gtk_source_completion_provider_type_id;
}

/**
 * gtk_source_completion_provider_get_name:
 * @provider: The #GtkSourceCompletionProvider
 *
 * Get the name of the provider. This should be a translatable name for
 * display to the user. For example: _("Document word completion provider"). The
 * returned string must be freed with g_free().
 *
 * Returns: A new string containing the name of the provider.
 */
gchar *
gtk_source_completion_provider_get_name (GtkSourceCompletionProvider *provider)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), NULL);
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_name (provider);
}

/**
 * gtk_source_completion_provider_get_icon:
 * @provider: The #GtkSourceCompletionProvider
 *
 * Get the icon of the provider.
 *
 * Returns: The icon to be used for the provider, or %NULL if the provider does
 *          not have a special icon.
 */
GdkPixbuf *
gtk_source_completion_provider_get_icon (GtkSourceCompletionProvider *provider)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), NULL);
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_icon (provider);
}

/**
 * gtk_source_completion_provider_populate:
 * @provider: The #GtkSourceCompletionProvider
 * @context: The #GtkSourceCompletionContext
 *
 * Populate @context with proposals from @provider
 *
 */
void
gtk_source_completion_provider_populate (GtkSourceCompletionProvider *provider,
                                         GtkSourceCompletionContext  *context)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider));
	GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->populate (provider, context);
}

/**
 * gtk_source_completion_provider_get_activation:
 * @provider: A #GtkSourceCompletionProvider
 * 
 * Get with what kind of activation the provider should be activated.
 *
 * Returns: a combination of #GtkSourceCompletionActivation.
 *
 **/
GtkSourceCompletionActivation
gtk_source_completion_provider_get_activation (GtkSourceCompletionProvider *provider)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), GTK_SOURCE_COMPLETION_ACTIVATION_NONE);
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_activation (provider);
}

/**
 * gtk_source_completion_provider_match:
 * @provider: The #GtkSourceCompletionProvider
 * @context: The #GtkSourceCompletionContext
 *
 * Get whether the provider match the context of completion detailed in
 * @context.
 *
 * Returns: %TRUE if @provider matches the completion context, %FALSE otherwise
 */
gboolean
gtk_source_completion_provider_match (GtkSourceCompletionProvider *provider,
                                      GtkSourceCompletionContext  *context)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), TRUE);
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->match (provider,
	                                                                       context);
}

/**
 * gtk_source_completion_provider_get_info_widget:
 * @provider: The #GtkSourceCompletionProvider
 * @proposal: The currently selected #GtkSourceCompletionProposal
 *
 * Get a customized info widget to show extra information of a proposal.
 * This allows for customized widgets on a proposal basis, although in general
 * providers will have the same custom widget for all their proposals and
 * @proposal can be ignored. The implementation of this function is optional. 
 * If implemented, #gtk_source_completion_provider_update_info MUST also be
 * implemented. If not implemented, the default 
 * #gtk_source_completion_proposal_get_info will be used to display extra
 * information about a #GtkSourceCompletionProposal.
 *
 * Returns: a custom #GtkWidget to show extra information about @proposal.
 */
GtkWidget *
gtk_source_completion_provider_get_info_widget (GtkSourceCompletionProvider *provider,
                                                GtkSourceCompletionProposal *proposal)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), NULL);
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), NULL);
	
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_info_widget (provider, proposal);
}

/**
 * gtk_source_completion_provider_update_info:
 * @provider: A #GtkSourceCompletionProvider
 * @proposal: A #GtkSourceCompletionProposal
 * @info: A #GtkSourceCompletionInfo
 *
 * Update extra information shown in @info for @proposal. This should be
 * implemented if your provider sets a custom info widget for @proposal.
 * This function MUST be implemented when 
 * #gtk_source_completion_provider_get_info_widget is implemented.
 */
void 
gtk_source_completion_provider_update_info (GtkSourceCompletionProvider *provider,
                                            GtkSourceCompletionProposal *proposal,
                                            GtkSourceCompletionInfo     *info)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider));
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal));
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_INFO (info));
	
	GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->update_info (provider, proposal, info);
}

/**
 * gtk_source_completion_provider_get_start_iter:
 * @provider: A #GtkSourceCompletionProvider
 * @proposal: A #GtkSourceCompletionProposal
 * @context: A #GtkSourceCompletionContext
 * @iter: A #GtkTextIter
 * 
 * Get the #GtkTextIter at which the completion for @proposal starts. When
 * implemented, the completion can use this information to position the
 * completion window accordingly when a proposal is selected in the completion
 * window.
 *
 * Returns: %TRUE if @iter was set for @proposal, %FALSE otherwise
 *
 **/
gboolean
gtk_source_completion_provider_get_start_iter (GtkSourceCompletionProvider *provider,
                                               GtkSourceCompletionContext  *context,
                                               GtkSourceCompletionProposal *proposal,
                                               GtkTextIter                 *iter)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), FALSE);
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_CONTEXT (context), FALSE);
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_start_iter (provider, 
	                                                                                context,
	                                                                                proposal,
	                                                                                iter);
}

/**
 * gtk_source_completion_provider_activate_proposal:
 * @provider: A #GtkSourceCompletionProvider
 * @proposal: A #GtkSourceCompletionProposal
 * @iter: A #GtkTextIter
 *
 * Activate @proposal at @iter. When this functions returns %FALSE, the default
 * activation of @proposal will take place which replaces the word at @iter
 * with the label of @proposal.
 *
 * Returns: %TRUE to indicate that the proposal activation has been handled,
 *          %FALSE otherwise.
 */
gboolean
gtk_source_completion_provider_activate_proposal (GtkSourceCompletionProvider *provider,
                                                  GtkSourceCompletionProposal *proposal,
                                                  GtkTextIter                 *iter)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), FALSE);
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposal), FALSE);
	
	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->activate_proposal (provider, 
	                                                                                   proposal,
	                                                                                   iter);
}

/**
 * gtk_source_completion_provider_get_interactive_delay:
 * @provider: A # GtkSourceCompletionProvider
 *
 * Get the delay in milliseconds before starting interactive completion for
 * this provider. A value of -1 indicates to use the default value as set
 * by #GtkSourceCompletion::auto-complete-delay.
 *
 * Returns: the interactive delay in milliseconds.
 *
 **/
gint
gtk_source_completion_provider_get_interactive_delay (GtkSourceCompletionProvider *provider)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), -1);

	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_interactive_delay (provider);
}

/**
 * gtk_source_completion_provider_get_priority:
 * @provider: A # GtkSourceCompletionProvider
 *
 * Get the provider priority. The priority determines the order in which
 * proposals appear in the completion popup. Higher priorities are sorted
 * before lower priorities. The default priority is 0.
 *
 * Returns: the provider priority.
 *
 **/
gint
gtk_source_completion_provider_get_priority (GtkSourceCompletionProvider *provider)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), 0);

	return GTK_SOURCE_COMPLETION_PROVIDER_GET_INTERFACE (provider)->get_priority (provider);
}
