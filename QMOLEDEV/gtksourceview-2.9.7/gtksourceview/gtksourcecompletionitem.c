/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionitem.c
 * This file is part of gtksourcecompletion
 *
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

#include <gtksourceview/gtksourcecompletionitem.h>

#include "gtksourcecompletionutils.h"
#include "gtksourceview-i18n.h"

#define GTK_SOURCE_COMPLETION_ITEM_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_TYPE_SOURCE_COMPLETION_ITEM, GtkSourceCompletionItemPrivate))

struct _GtkSourceCompletionItemPrivate
{
	gchar *label;
	gchar *markup;
	gchar *text;
	gchar *info;
	GdkPixbuf *icon;
};

/* Properties */
enum
{
	PROP_0,
	PROP_LABEL,
	PROP_MARKUP,
	PROP_TEXT,
	PROP_ICON,
	PROP_INFO
};

static void gtk_source_completion_proposal_iface_init (gpointer g_iface, gpointer iface_data);

G_DEFINE_TYPE_WITH_CODE (GtkSourceCompletionItem, 
			 gtk_source_completion_item, 
			 G_TYPE_OBJECT,
			 G_IMPLEMENT_INTERFACE (GTK_TYPE_SOURCE_COMPLETION_PROPOSAL,
			 			gtk_source_completion_proposal_iface_init))

static gchar *
gtk_source_completion_proposal_get_label_impl (GtkSourceCompletionProposal *self)
{
	return g_strdup (GTK_SOURCE_COMPLETION_ITEM (self)->priv->label);
}

static gchar *
gtk_source_completion_proposal_get_markup_impl (GtkSourceCompletionProposal *self)
{
	return g_strdup (GTK_SOURCE_COMPLETION_ITEM (self)->priv->markup);
}

static gchar *
gtk_source_completion_proposal_get_text_impl (GtkSourceCompletionProposal *self)
{
	return g_strdup (GTK_SOURCE_COMPLETION_ITEM (self)->priv->text);
}

static GdkPixbuf *
gtk_source_completion_proposal_get_icon_impl (GtkSourceCompletionProposal *self)
{
	return GTK_SOURCE_COMPLETION_ITEM (self)->priv->icon;
}

static gchar *
gtk_source_completion_proposal_get_info_impl (GtkSourceCompletionProposal *self)
{
	return g_strdup (GTK_SOURCE_COMPLETION_ITEM (self)->priv->info);
}

static void
gtk_source_completion_proposal_iface_init (gpointer g_iface, 
					   gpointer iface_data)
{
	GtkSourceCompletionProposalIface *iface = (GtkSourceCompletionProposalIface *)g_iface;
	
	/* Interface data getter implementations */
	iface->get_label = gtk_source_completion_proposal_get_label_impl;
	iface->get_markup = gtk_source_completion_proposal_get_markup_impl;
	iface->get_text = gtk_source_completion_proposal_get_text_impl;
	iface->get_icon = gtk_source_completion_proposal_get_icon_impl;
	iface->get_info = gtk_source_completion_proposal_get_info_impl;
}

static void
gtk_source_completion_item_finalize (GObject *object)
{
	GtkSourceCompletionItem *self = GTK_SOURCE_COMPLETION_ITEM(object);
	
	g_free (self->priv->label);
	g_free (self->priv->markup);
	g_free (self->priv->text);

	g_free (self->priv->info);
	
	if (self->priv->icon != NULL)
	{
		g_object_unref (self->priv->icon);
	}

	G_OBJECT_CLASS (gtk_source_completion_item_parent_class)->finalize (object);
}

static void
gtk_source_completion_item_get_property (GObject    *object,
					 guint       prop_id,
					 GValue     *value,
					 GParamSpec *pspec)
{
	GtkSourceCompletionItem *self;

	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_ITEM (object));

	self = GTK_SOURCE_COMPLETION_ITEM (object);

	switch (prop_id)
	{
		case PROP_LABEL:
			g_value_set_string (value, self->priv->label);
			break;
		case PROP_MARKUP:
			g_value_set_string (value, self->priv->markup);
			break;
		case PROP_TEXT:
			g_value_set_string (value, self->priv->text);
			break;
		case PROP_INFO:
			g_value_set_string (value, self->priv->info);
			break;
		case PROP_ICON:
			g_value_set_object (value, self->priv->icon);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
emit_changed (GtkSourceCompletionItem *item)
{
	gtk_source_completion_proposal_changed (GTK_SOURCE_COMPLETION_PROPOSAL (item));
}

static void
gtk_source_completion_item_set_property (GObject      *object,
					 guint         prop_id,
					 const GValue *value,
					 GParamSpec   *pspec)
{
	GtkSourceCompletionItem *self;

	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_ITEM (object));

	self = GTK_SOURCE_COMPLETION_ITEM (object);

	switch (prop_id)
	{
		case PROP_LABEL:
			g_free (self->priv->label);
			self->priv->label = g_value_dup_string (value);
			
			emit_changed (self);
			break;
		case PROP_MARKUP:
			g_free (self->priv->markup);
			self->priv->markup = g_value_dup_string (value);
			
			emit_changed (self);
			break;
		case PROP_TEXT:
			g_free (self->priv->text);
			self->priv->text = g_value_dup_string (value);
			break;
		case PROP_INFO:
			g_free (self->priv->info);
			self->priv->info = g_value_dup_string (value);
			
			emit_changed (self);
			break;
		case PROP_ICON:
			if (self->priv->icon != NULL)
			{
				g_object_unref (self->priv->icon);
			}
			
			self->priv->icon = GDK_PIXBUF (g_value_dup_object (value));
			emit_changed (self);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_completion_item_class_init (GtkSourceCompletionItemClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = gtk_source_completion_item_finalize;
	object_class->get_property = gtk_source_completion_item_get_property;
	object_class->set_property = gtk_source_completion_item_set_property;

	/**
	 * GtkSourceCompletionItem:label:
	 *
	 * Label to be shown for this proposal.
	 */
	g_object_class_install_property (object_class,
					 PROP_LABEL,
					 g_param_spec_string ("label",
							      _("Label"),
							      _("Label to be shown for this item"),
							      NULL,
							      G_PARAM_READWRITE));

	/**
	 * GtkSourceCompletionItem:markup:
	 *
	 * Label with markup to be shown for this proposal.
	 */
	g_object_class_install_property (object_class,
					 PROP_MARKUP,
					 g_param_spec_string ("markup",
							      _("Markup"),
							      _("Markup to be shown for this item"),
							      NULL,
							      G_PARAM_READWRITE));

	/**
	 * GtkSourceCompletionItem:text:
	 *
	 * Proposal text.
	 */
	g_object_class_install_property (object_class,
					 PROP_TEXT,
					 g_param_spec_string ("text",
							      _("Text"),
							      _("Item text"),
							      NULL,
							      G_PARAM_READWRITE));

	/**
	 * GtkSourceCompletionItem:icon:
	 *
	 * Icon to be shown for this proposal.
	 */
	g_object_class_install_property (object_class,
					 PROP_ICON,
					 g_param_spec_object ("icon",
							      _("Icon"),
							      _("Icon to be shown for this item"),
							      GDK_TYPE_PIXBUF,
							      G_PARAM_READWRITE));

	/**
	 * GtkSourceCompletionItem:info:
	 *
	 * Optional extra information to be shown for this proposal.
	 */
	g_object_class_install_property (object_class,
					 PROP_INFO,
					 g_param_spec_string ("info",
							      _("Info"),
							      _("Info to be shown for this item"),
							      NULL,
							      G_PARAM_READWRITE));

	g_type_class_add_private (object_class, sizeof(GtkSourceCompletionItemPrivate));
}

static void
gtk_source_completion_item_init (GtkSourceCompletionItem *self)
{
	self->priv = GTK_SOURCE_COMPLETION_ITEM_GET_PRIVATE (self);
}

/** 
 * gtk_source_completion_item_new:
 * @label: The item label
 * @text: The item text
 * @icon: The item icon
 * @info: The item extra information
 *
 * Create a new #GtkSourceCompletionItem with label @label, icon @icon and 
 * extra information @info. Both @icon and @info can be %NULL in which case 
 * there will be no icon shown and no extra information available.
 *
 * Returns: The new #GtkSourceCompletionItem.
 *
 */
GtkSourceCompletionItem *
gtk_source_completion_item_new (const gchar *label,
				const gchar *text,
				GdkPixbuf   *icon,
				const gchar *info)
{
	return g_object_new (GTK_TYPE_SOURCE_COMPLETION_ITEM, 
			     "label", label,
			     "text", text,
			     "icon", icon,
			     "info", info,
			     NULL);
}

/** 
 * gtk_source_completion_item_new_with_markup:
 * @markup: The item markup label
 * @text: The item text
 * @icon: The item icon
 * @info: The item extra information
 *
 * Create a new #GtkSourceCompletionItem with markup label @markup, icon 
 * @icon and extra information @info. Both @icon and @info can be %NULL in 
 * which case there will be no icon shown and no extra information available.
 *
 * Returns: The new #GtkSourceCompletionItem.
 *
 */
GtkSourceCompletionItem *
gtk_source_completion_item_new_with_markup (const gchar *markup,
                                            const gchar *text,
                                            GdkPixbuf   *icon,
                                            const gchar *info)
{
	return g_object_new (GTK_TYPE_SOURCE_COMPLETION_ITEM, 
			     "markup", markup,
			     "text", text,
			     "icon", icon,
			     "info", info,
			     NULL);
}

/** 
 * gtk_source_completion_item_new_from_stock:
 * @label: The item label
 * @text: The item text
 * @stock: The stock icon
 * @info: The item extra information
 *
 * Creates a new #GtkSourceCompletionItem from a stock item. If @label is %NULL, 
 * the stock label will be used.
 *
 * Returns: the newly constructed #GtkSourceCompletionItem.
 *
 */
GtkSourceCompletionItem *
gtk_source_completion_item_new_from_stock (const gchar *label,
					   const gchar *text,
					   const gchar *stock,
					   const gchar *info)
{
	GtkSourceCompletionItem *item;
	GdkPixbuf *icon;
	GtkIconTheme *theme;
	gint width;
	gint height;
	GtkStockItem stock_item;
	
	if (stock != NULL)
	{
		theme = gtk_icon_theme_get_default ();
	
		gtk_icon_size_lookup (GTK_ICON_SIZE_MENU, &width, &height);

		icon = gtk_icon_theme_load_icon (theme, 
						 stock, 
						 width, 
						 GTK_ICON_LOOKUP_USE_BUILTIN, 
						 NULL);

		if (label == NULL && gtk_stock_lookup (stock, &stock_item))
		{
			label = stock_item.label;
		}
	}
	else
	{
		icon = NULL;
	}
	
	item = gtk_source_completion_item_new (label, text, icon, info);
	
	if (icon != NULL)
	{
		g_object_unref (icon);
	}
	
	return item;
}
