/*
 * gtksourcecompletioncontext.c
 * This file is part of gtksourceview
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

#include "gtksourcecompletioncontext.h"
#include "gtksourceview-typebuiltins.h"
#include "gtksourcecompletionprovider.h"
#include "gtksourceview-i18n.h"
#include "gtksourcecompletion.h"

#define GTK_SOURCE_COMPLETION_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_TYPE_SOURCE_COMPLETION_CONTEXT, GtkSourceCompletionContextPrivate))

struct _GtkSourceCompletionContextPrivate
{
	GtkSourceCompletion *completion;

	GtkTextMark *mark;
	GtkSourceCompletionActivation activation;

	gulong mark_set_id;
};

/* Properties */
enum
{
	PROP_0,

	PROP_COMPLETION,
	PROP_ITER,
	PROP_ACTIVATION
};

/* Signals */
enum
{
	CANCELLED,
	NUM_SIGNALS
};

guint context_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (GtkSourceCompletionContext, gtk_source_completion_context, G_TYPE_INITIALLY_UNOWNED)

/* FIXME: we use this util to get the buffer from the completion
   but this object is not robust to a change of the buffer associated
   to the view. Context lifetime should be short enough to not really
   matter.
*/

static GtkTextBuffer *
get_buffer (GtkSourceCompletionContext *context)
{
	GtkSourceView *view = gtk_source_completion_get_view (context->priv->completion);
	return gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
}

static void
gtk_source_completion_context_dispose (GObject *object)
{
	GtkSourceCompletionContext *context;
	GtkTextBuffer *buffer;

	context = GTK_SOURCE_COMPLETION_CONTEXT (object);
	buffer = get_buffer (context);

	if (context->priv->mark_set_id)
	{
		g_signal_handler_disconnect (buffer, context->priv->mark_set_id);
		context->priv->mark_set_id = 0;
	}

	if (context->priv->completion)
	{
		g_object_unref (context->priv->completion);
		context->priv->completion = NULL;
	}

	if (context->priv->mark)
	{
		gtk_text_buffer_delete_mark (buffer, context->priv->mark);
		context->priv->mark = NULL;
	}

	G_OBJECT_CLASS (gtk_source_completion_context_parent_class)->dispose (object);
}

static void
gtk_source_completion_context_set_iter (GtkSourceCompletionContext *context,
                                        GtkTextIter                *iter)
{
	GtkTextBuffer *buffer = get_buffer (context);

	if (context->priv->mark == NULL)
	{
		context->priv->mark = gtk_text_buffer_create_mark (buffer, NULL, iter, FALSE);
	}
	else
	{
		gtk_text_buffer_move_mark (buffer, context->priv->mark, iter);
	}
}

static void
gtk_source_completion_context_set_property (GObject      *object,
                                            guint         prop_id,
                                            const GValue *value,
                                            GParamSpec   *pspec)
{
	GtkSourceCompletionContext *context = GTK_SOURCE_COMPLETION_CONTEXT (object);

	switch (prop_id)
	{
		case PROP_COMPLETION:
			context->priv->completion = g_value_dup_object (value);
			break;
		case PROP_ITER:
			gtk_source_completion_context_set_iter (context, (GtkTextIter *) g_value_get_boxed (value));
			break;
		case PROP_ACTIVATION:
			context->priv->activation = g_value_get_flags (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
	}
}

static void
gtk_source_completion_context_get_property (GObject    *object,
                                            guint       prop_id,
                                            GValue     *value,
                                            GParamSpec *pspec)
{
	GtkSourceCompletionContext *context = GTK_SOURCE_COMPLETION_CONTEXT (object);

	switch (prop_id)
	{
		case PROP_COMPLETION:
			g_value_set_object (value, context->priv->completion);
			break;
		case PROP_ITER:
			{
				GtkTextIter iter;
				gtk_source_completion_context_get_iter (context, &iter);
				g_value_set_boxed (value, &iter);
			}
			break;
		case PROP_ACTIVATION:
			g_value_set_flags (value, context->priv->activation);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
	}
}

static void
buffer_mark_set_cb (GtkTextBuffer              *buffer,
                    GtkTextIter                *iter,
                    GtkTextMark                *mark,
                    GtkSourceCompletionContext *context)
{
	if (mark == context->priv->mark)
	{
		g_object_notify (G_OBJECT (buffer), "iter");
	}
}

static GObject *
gtk_source_completion_context_constructor (GType                  type,
                                           guint                  n_construct_properties,
                                           GObjectConstructParam *construct_param)
{
	GObject *object;
	GtkTextBuffer *buffer;
	GtkSourceCompletionContext *context;

	object = G_OBJECT_CLASS (gtk_source_completion_context_parent_class)->constructor (type,
											   n_construct_properties,
									 		   construct_param);

	/* we need to connect after the completion property is set */
	context = GTK_SOURCE_COMPLETION_CONTEXT (object);
	buffer = get_buffer (context);
	context->priv->mark_set_id = g_signal_connect (buffer,
						       "mark-set",
						       G_CALLBACK (buffer_mark_set_cb),
						       context);

	return object;
}

static void
gtk_source_completion_context_class_init (GtkSourceCompletionContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->set_property = gtk_source_completion_context_set_property;
	object_class->get_property = gtk_source_completion_context_get_property;
	object_class->constructor = gtk_source_completion_context_constructor;
	object_class->dispose = gtk_source_completion_context_dispose;

	/**
	 * GtkSourceCompletionContext::cancelled:
	 *
	 * Emitted when the current population of proposals has been cancelled.
	 * Providers adding proposals asynchronously should connect to this signal
	 * to know when to cancel running proposal queries.
	 **/
	context_signals[CANCELLED] =
		g_signal_new ("cancelled",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		              G_STRUCT_OFFSET (GtkSourceCompletionContextClass, cancelled),
		              NULL, 
		              NULL,
		              g_cclosure_marshal_VOID__VOID, 
		              G_TYPE_NONE,
		              0);

	/**
	 * GtkSourceCompletionContext:completion:
	 *
	 * The #GtkSourceCompletion associated with the context.
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_COMPLETION,
	                                 g_param_spec_object ("completion",
	                                                      _("Completion"),
	                                                      _("The completion object to which the context belongs"),
	                                                      GTK_TYPE_SOURCE_COMPLETION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GtkSourceCompletionContext:iter:
	 *
	 * The #GtkTextIter at which the completion is invoked.
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_ITER,
	                                 g_param_spec_boxed ("iter",
	/* Translators: The GtkTextIter at which the completion was invoked */
	                                                      _("Iterator"),
	                                                      _("The GtkTextIter at which the completion was invoked"),
	                                                      GTK_TYPE_TEXT_ITER,
	                                                      G_PARAM_READWRITE));

	/**
	 * GtkSourceCompletionContext:activation:
	 *
	 * The completion activation
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_ACTIVATION,
	                                 g_param_spec_flags ("activation",
	                                                     _("Activation"),
	                                                     _("The type of activation"),
	                                                     GTK_TYPE_SOURCE_COMPLETION_ACTIVATION,
	                                                     GTK_SOURCE_COMPLETION_ACTIVATION_NONE,
	                                                     G_PARAM_READWRITE));

	g_type_class_add_private (object_class, sizeof(GtkSourceCompletionContextPrivate));
}

static void
gtk_source_completion_context_init (GtkSourceCompletionContext *context)
{
	context->priv = GTK_SOURCE_COMPLETION_CONTEXT_GET_PRIVATE (context);
}

/**
 * gtk_source_completion_context_add_proposals:
 * @context: A #GtkSourceCompletionContext
 * @provider: A #GtkSourceCompletionProvider
 * @proposals: The list of proposals to add
 * @finished: Whether the provider is finished adding proposals
 * 
 * Providers can use this function to add proposals to the completion. They
 * can do so asynchronously by means of the @finished argument. Providers must
 * ensure that they always call this function with @finished set to %TRUE
 * once each population (even if no proposals need to be added).
 *
 **/
void
gtk_source_completion_context_add_proposals (GtkSourceCompletionContext  *context,
                                             GtkSourceCompletionProvider *provider,
                                             GList                       *proposals,
                                             gboolean                     finished)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_CONTEXT (context));
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider));

	_gtk_source_completion_add_proposals (context->priv->completion,
	                                      context,
	                                      provider,
	                                      proposals,
	                                      finished);
}

/**
 * gtk_source_completion_context_get_iter:
 * @context: A #GtkSourceCompletionContext
 * @iter: A #GtkTextIter
 * 
 * Get the iter at which the completion was invoked. Providers can use this
 * to determine how and if to match proposals.
 *
 **/
void
gtk_source_completion_context_get_iter (GtkSourceCompletionContext *context,
                                        GtkTextIter                *iter)
{
	GtkTextBuffer *buffer;

	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_CONTEXT (context));

	buffer = get_buffer (context);

	if (context->priv->mark != NULL)
	{
		gtk_text_buffer_get_iter_at_mark (buffer, iter, context->priv->mark);
	}
	else
	{
		/* This should never happen: context should be always be created
		   providing a position iter */
		g_warning ("Completion context without mark");
	}
}

/**
 * gtk_source_completion_context_get_activation:
 * @context: A #GtkSourceCompletionContext
 *
 * Get the context activation
 *
 * Returns: The context activation
 */
GtkSourceCompletionActivation
gtk_source_completion_context_get_activation (GtkSourceCompletionContext *context)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_CONTEXT (context), FALSE);

	return context->priv->activation;
}

void
_gtk_source_completion_context_cancel (GtkSourceCompletionContext *context)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_CONTEXT (context));

	g_signal_emit (context, context_signals[CANCELLED], 0);
}

GtkSourceCompletionContext *
_gtk_source_completion_context_new (GtkSourceCompletion *completion, GtkTextIter *position)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION (completion), NULL);
	g_return_val_if_fail (position != NULL, NULL);

	return g_object_new (GTK_TYPE_SOURCE_COMPLETION_CONTEXT,
	                     "completion", completion,
	                     "iter", position,
	                      NULL);
}
