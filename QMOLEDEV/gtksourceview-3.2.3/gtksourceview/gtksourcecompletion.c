/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/* gtksourcecompletion.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2007 -2009 Jesús Barbero Rodríguez <chuchiperriman@gmail.com>
 * Copyright (C) 2009 - Jesse van den Kieboom <jessevdk@gnome.org>
 *
 * GtkSourceView is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GtkSourceView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/**
 * SECTION:completion
 * @title: GtkSourceCompletion
 * @short_description: Main Completion Object
 *
 */

#include <gdk/gdkkeysyms.h>
#include "gtksourcecompletionutils.h"
#include "gtksourceview-marshal.h"
#include <gtksourceview/gtksourcecompletion.h>
#include "gtksourceview-i18n.h"
#include "gtksourcecompletionmodel.h"
#include <string.h>
#include <gtksourceview/gtksourceview.h>
#include "gtksourcecompletion-private.h"
#include "gtksourcecompletioncontext.h"
#include "gtksourcecompletionui.h"
#include <stdarg.h>

#define WINDOW_WIDTH 350
#define WINDOW_HEIGHT 200

#define GTK_SOURCE_COMPLETION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object),\
						  GTK_SOURCE_TYPE_COMPLETION,           \
						  GtkSourceCompletionPrivate))

/*#define ENABLE_DEBUG*/
#undef ENABLE_DEBUG

#ifdef ENABLE_DEBUG
#define DEBUG(x) (x)
#else
#define DEBUG(x)
#endif

/* Signals */
enum
{
	SHOW,
	HIDE,
	POPULATE_CONTEXT,

	/* Actions */
	ACTIVATE_PROPOSAL,
	MOVE_CURSOR,
	MOVE_PAGE,

	LAST_SIGNAL
};

enum
{
	PROP_0,
	PROP_VIEW,
	PROP_REMEMBER_INFO_VISIBILITY,
	PROP_SELECT_ON_SHOW,
	PROP_SHOW_HEADERS,
	PROP_SHOW_ICONS,
	PROP_ACCELERATORS,

	PROP_AUTO_COMPLETE_DELAY,

	PROP_PROVIDER_PAGE_SIZE,
	PROP_PROPOSAL_PAGE_SIZE
};

enum
{
	TEXT_VIEW_KEY_PRESS,
	TEXT_VIEW_FOCUS_OUT,
	TEXT_VIEW_BUTTON_PRESS,
	TEXT_VIEW_EDITABLE,
	TEXT_BUFFER_BEGIN_SIGNALS,
	TEXT_BUFFER_DELETE_RANGE,
	TEXT_BUFFER_INSERT_TEXT,
	TEXT_BUFFER_MARK_SET,
	SOURCE_BUFFER_REDO,
	SOURCE_BUFFER_REDO_AFTER,
	SOURCE_BUFFER_UNDO,
	SOURCE_BUFFER_UNDO_AFTER,
	TEXT_BUFFER_PASTE_DONE,
	LAST_EXTERNAL_SIGNAL
};

struct _GtkSourceCompletionPrivate
{
	/* Widget and popup variables*/
	GtkWidget *window;
	GtkWidget *info_window;
	GtkWidget *info_button;
	GtkWidget *selection_label;
	GtkWidget *default_info;
	GtkWidget *selection_image;
	GtkWidget *hbox_info;
	GtkWidget *label_info;
	GtkWidget *image_info;
	GtkTreeViewColumn *tree_view_column_accelerator;
	GtkCellRenderer *cell_renderer_accelerator;
	GtkCellRenderer *cell_renderer_icon;

	GtkWidget *tree_view_proposals;
	GtkSourceCompletionModel *model_proposals;

	guint num_accelerators;

	/* Page size */
	guint proposal_page_size;
	guint provider_page_size;

	/* Completion management */
	GtkSourceView *view;

	GList *providers;
	GList *interactive_providers;

	GtkSourceCompletionContext *context;
	GList *active_providers;
	GList *running_providers;

	guint show_timed_out_id;
	guint auto_complete_delay;

	gint typing_line;
	gint typing_line_offset;

	gulong signals_ids[LAST_EXTERNAL_SIGNAL];

	gint min_auto_complete_delay;
	GList *auto_completion_selection;
	GtkSourceCompletionContext *auto_completion_context;

	gint block_count;

	guint remember_info_visibility : 1;
	guint info_visible : 1;
	guint select_on_show : 1;
	guint show_headers : 1;
	guint select_first : 1;
	guint show_icons : 1;
};

static guint signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE(GtkSourceCompletion, gtk_source_completion, G_TYPE_OBJECT)

static void update_completion (GtkSourceCompletion        *completion,
                               GList                      *providers,
                               GtkSourceCompletionContext *context);

static void show_info_cb (GtkWidget           *widget,
	                  GtkSourceCompletion *completion);

static gboolean
get_selected_proposal (GtkSourceCompletion          *completion,
                       GtkTreeIter                  *iter,
		       GtkSourceCompletionProvider **provider,
		       GtkSourceCompletionProposal **proposal)
{
	GtkTreeIter piter;
	GtkTreeModel *model;
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

	if (gtk_tree_selection_get_selected (selection, NULL, &piter))
	{
		model = GTK_TREE_MODEL (completion->priv->model_proposals);

		if (proposal)
		{
			gtk_tree_model_get (model, &piter,
					    GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROPOSAL,
					    proposal, -1);
		}

		if (provider)
		{
			gtk_tree_model_get (model, &piter,
					    GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROVIDER,
					    provider, -1);
		}

		if (iter != NULL)
		{
			*iter = piter;
		}

		return TRUE;
	}

	return FALSE;
}

static void
get_iter_at_insert (GtkSourceCompletion *completion,
                    GtkTextIter         *iter)
{
	GtkTextBuffer *buffer;

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view));
	gtk_text_buffer_get_iter_at_mark (buffer,
	                                  iter,
	                                  gtk_text_buffer_get_insert (buffer));
}

static void
completion_begin_block (GtkSourceCompletion *completion,
                        GtkSourceBuffer     *buffer)
{
	if (completion->priv->block_count == 0)
	{
		g_signal_handler_block (buffer,
		                        completion->priv->signals_ids[TEXT_BUFFER_INSERT_TEXT]);
		g_signal_handler_block (buffer,
		                        completion->priv->signals_ids[TEXT_BUFFER_DELETE_RANGE]);
	}

	++completion->priv->block_count;
}

static void
completion_end_block (GtkSourceCompletion *completion,
                      GtkSourceBuffer     *buffer)
{
	if (completion->priv->block_count == 0)
	{
		return;
	}

	if (--completion->priv->block_count == 0)
	{
		g_signal_handler_unblock (buffer,
		                          completion->priv->signals_ids[TEXT_BUFFER_INSERT_TEXT]);
		g_signal_handler_unblock (buffer,
		                          completion->priv->signals_ids[TEXT_BUFFER_DELETE_RANGE]);
	}
}

static gboolean
activate_current_proposal (GtkSourceCompletion *completion)
{
	gboolean activated;
	GtkTreeIter iter;
	GtkTextIter titer;
	GtkSourceCompletionProposal *proposal = NULL;
	GtkSourceCompletionProvider *provider = NULL;
	GtkTextBuffer *buffer;
	gboolean has_start;
	GtkTextIter start;

	if (!get_selected_proposal (completion, &iter, &provider, &proposal))
	{
		DEBUG({
			g_print ("Hiding because activated without proposal\n");
		});

		gtk_source_completion_hide (completion);
		return TRUE;
	}

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view));
	gtk_text_buffer_get_start_iter (buffer, &start);

	has_start = gtk_source_completion_provider_get_start_iter (provider,
	                                                           completion->priv->context,
	                                                           proposal,
	                                                           &start);

	/* First hide the completion because the activation might actually
	   activate another one, which we don't want to hide */
	DEBUG({
		g_print ("Hiding just before proposal activation\n");
	});

	gtk_source_completion_hide (completion);

	/* Get insert iter */

	get_iter_at_insert (completion, &titer);

	completion_begin_block (completion, GTK_SOURCE_BUFFER (buffer));

	activated = gtk_source_completion_provider_activate_proposal (provider, proposal, &titer);

	if (!activated)
	{
		gchar *text = gtk_source_completion_proposal_get_text (proposal);

		if (has_start)
		{
			/* Replace from 'start' to 'titer' */
			gtk_text_buffer_begin_user_action (buffer);
			gtk_text_buffer_delete (buffer, &start, &titer);
			gtk_text_buffer_insert (buffer, &start, text, -1);
			gtk_text_buffer_end_user_action (buffer);
		}
		else
		{
			gtk_source_completion_utils_replace_current_word (GTK_SOURCE_BUFFER (buffer),
					                                  text,
					                                  -1);
		}
		g_free (text);
	}

	completion_end_block (completion, GTK_SOURCE_BUFFER (buffer));

	g_object_unref (provider);
	g_object_unref (proposal);

	return TRUE;
}

typedef gboolean (*ProposalSelector)(GtkSourceCompletion *completion,
                                     GtkTreeModel        *model,
                                     GtkTreeIter         *iter,
                                     gboolean             hasselection,
                                     gpointer             userdata);

static gboolean
select_proposal (GtkSourceCompletion *completion,
                 ProposalSelector     selector,
                 gpointer             userdata)
{
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	GtkTreePath *path;
	GtkTreeModel *model;
	gboolean hasselection;

	if (!gtk_widget_get_visible (completion->priv->tree_view_proposals))
	{
		return FALSE;
	}

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

	if (gtk_tree_selection_get_mode (selection) == GTK_SELECTION_NONE)
	{
		return FALSE;
	}

	model = GTK_TREE_MODEL (completion->priv->model_proposals);

	hasselection = gtk_tree_selection_get_selected (selection, NULL, &iter);

	if (selector (completion, model, &iter, hasselection, userdata))
	{
		gtk_tree_selection_select_iter (selection, &iter);

		path = gtk_tree_model_get_path (model, &iter);
		gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (completion->priv->tree_view_proposals),
					      path,
					      NULL,
					      FALSE,
					      0,
					      0);

		gtk_tree_path_free (path);
	}

	/* Always return TRUE to consume the key press event */
	return TRUE;
}

static void
scroll_to_iter (GtkSourceCompletion *completion,
                GtkTreeIter         *iter)
{
	GtkTreePath *path;

	path = gtk_tree_model_get_path (GTK_TREE_MODEL (completion->priv->model_proposals),
	                                iter);

	gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (completion->priv->tree_view_proposals),
				      path,
				      NULL,
				      FALSE,
				      0,
				      0);
	gtk_tree_path_free (path);
}

static gboolean
selector_first (GtkSourceCompletion *completion,
                GtkTreeModel        *model,
                GtkTreeIter         *iter,
                gboolean             hasselection,
                gpointer             userdata)
{
	gboolean ret;
	gboolean hasfirst;
	GtkTreeIter first;

	ret = gtk_tree_model_get_iter_first (model, iter);
	hasfirst = ret;
	first = *iter;

	while (ret && gtk_source_completion_model_iter_is_header (
			GTK_SOURCE_COMPLETION_MODEL (model), iter))
	{
		ret = gtk_tree_model_iter_next (model, iter);
	}

	if (hasfirst && !ret)
	{
		scroll_to_iter (completion, &first);
	}

	return ret;
}

static gboolean
selector_last (GtkSourceCompletion *completion,
               GtkTreeModel        *model,
               GtkTreeIter         *iter,
               gboolean             hasselection,
               gpointer             userdata)
{
	gboolean ret;
	gboolean haslast;
	GtkTreeIter last;

	ret = gtk_source_completion_model_iter_last (GTK_SOURCE_COMPLETION_MODEL (model),
	                                             iter);

	haslast = ret;
	last = *iter;

	while (ret && gtk_source_completion_model_iter_is_header (
			GTK_SOURCE_COMPLETION_MODEL (model), iter))
	{
		ret = gtk_source_completion_model_iter_previous (GTK_SOURCE_COMPLETION_MODEL (model),
		                                                 iter);
	}

	if (haslast && !ret)
	{
		scroll_to_iter (completion, &last);
	}

	return ret;
}

static gboolean
selector_previous (GtkSourceCompletion *completion,
                   GtkTreeModel        *model,
                   GtkTreeIter         *iter,
                   gboolean             hasselection,
                   gpointer             userdata)
{
	gint num = GPOINTER_TO_INT (userdata);
	gboolean ret = FALSE;
	GtkTreeIter next;
	GtkTreeIter last;

	if (!hasselection)
	{
		return selector_last (completion, model, iter, hasselection, userdata);
	}

	next = *iter;
	last = *iter;

	while (num > 0 && gtk_source_completion_model_iter_previous (
				GTK_SOURCE_COMPLETION_MODEL (model), &next))
	{
		if (!gtk_source_completion_model_iter_is_header (GTK_SOURCE_COMPLETION_MODEL (model),
		                                                 &next))
		{
			ret = TRUE;
			*iter = next;
			--num;
		}

		last = next;
	}

	if (!ret)
	{
		scroll_to_iter (completion, &last);
	}

	return ret;
}

static gboolean
selector_next (GtkSourceCompletion *completion,
               GtkTreeModel        *model,
               GtkTreeIter         *iter,
               gboolean             hasselection,
               gpointer             userdata)
{
	gint num = GPOINTER_TO_INT (userdata);
	gboolean ret = FALSE;
	GtkTreeIter next;
	GtkTreeIter last;

	if (!hasselection)
	{
		return selector_first (completion, model, iter, hasselection, userdata);
	}

	next = *iter;
	last = *iter;

	while (num > 0 && gtk_tree_model_iter_next (model, &next))
	{
		if (!gtk_source_completion_model_iter_is_header (GTK_SOURCE_COMPLETION_MODEL (model),
		                                                 &next))
		{
			ret = TRUE;
			*iter = next;
			--num;
		}

		last = next;
	}

	if (!ret)
	{
		scroll_to_iter (completion, &last);
	}

	return ret;
}

static gboolean
select_first_proposal (GtkSourceCompletion *completion)
{
	return select_proposal (completion, selector_first, NULL);
}

static gboolean
select_last_proposal (GtkSourceCompletion *completion)
{
	return select_proposal (completion, selector_last, NULL);
}

static gboolean
select_previous_proposal (GtkSourceCompletion *completion,
			  gint                 rows)
{
	return select_proposal (completion, selector_previous, GINT_TO_POINTER (rows));
}

static gboolean
select_next_proposal (GtkSourceCompletion *completion,
		      gint                 rows)
{
	return select_proposal (completion, selector_next, GINT_TO_POINTER (rows));
}

static GtkSourceCompletionProvider *
get_visible_provider (GtkSourceCompletion *completion)
{
	GtkSourceCompletionModel *model = completion->priv->model_proposals;
	GList *visible;

	visible = gtk_source_completion_model_get_visible_providers (model);

	if (visible != NULL)
	{
		return GTK_SOURCE_COMPLETION_PROVIDER (visible->data);
	}
	else
	{
		return NULL;
	}
}

static void
get_num_visible_providers (GtkSourceCompletion *completion,
                           guint               *num,
                           guint               *current)
{
	GList *providers;
	GList *item;
	GtkSourceCompletionProvider *visible;

	visible = get_visible_provider (completion);

	*num = 0;
	*current = 0;

	providers = gtk_source_completion_model_get_providers (completion->priv->model_proposals);

	for (item = providers; item; item = g_list_next (item))
	{
		/* This works for now since we only show either all providers,
		   or a single one */
		if (item->data == visible)
		{
			*current = ++*num;
		}
		else
		{
			/* See if it has anything */
			if (gtk_source_completion_model_n_proposals (completion->priv->model_proposals,
			                                             GTK_SOURCE_COMPLETION_PROVIDER (item->data)))
			{
				++*num;
			}
		}
	}
}

static void
update_selection_label (GtkSourceCompletion *completion)
{
	guint pos;
	guint num;
	gchar *name;
	gchar *tmp;
	GtkSourceCompletionProvider *visible;

	visible = get_visible_provider (completion);

	get_num_visible_providers (completion, &num, &pos);

	if (visible == NULL)
	{
		/* Translators: "All" is used as a label in thestatus bar of the
		popup, telling that all completion pages are shown */
		name = g_strdup_printf("<b>%s</b>", _("All"));

		gtk_image_clear (GTK_IMAGE (completion->priv->selection_image));
	}
	else
	{
		gchar *temp_name = gtk_source_completion_provider_get_name (visible);
		name = g_markup_escape_text (temp_name, -1);
		g_free (temp_name);

		gtk_image_set_from_pixbuf (GTK_IMAGE (completion->priv->selection_image),
                           (GdkPixbuf *)gtk_source_completion_provider_get_icon (visible));
	}

	if (num > 1)
	{
		tmp = g_strdup_printf ("<small>%s (%d/%d)</small>", name, pos + 1, num + 1);
		gtk_label_set_markup (GTK_LABEL (completion->priv->selection_label),
		                      tmp);
		g_free (tmp);
	}
	else
	{
		tmp = g_strdup_printf ("<small>%s</small>", name);
		gtk_label_set_markup (GTK_LABEL (completion->priv->selection_label),
		                      tmp);
		g_free (tmp);
	}

	g_free (name);
}

static void
visible_provider_changed (GtkSourceCompletion *completion)
{
	GtkTreeSelection *selection;
	GtkTreeIter iter;

	update_selection_label (completion);

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

	if (gtk_tree_selection_get_selected (selection, NULL, &iter))
	{
		GtkTreePath *path;

 		path = gtk_tree_model_get_path (GTK_TREE_MODEL (completion->priv->model_proposals), &iter);

		gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (completion->priv->tree_view_proposals),
	                                      path,
	                                      NULL,
	                                      FALSE,
	                                      0,
	                                      0);
		gtk_tree_path_free (path);
	}
	else
	{
		gtk_tree_view_scroll_to_point (GTK_TREE_VIEW (completion->priv->tree_view_proposals),
	                                       0,
	                                       0);
	}
}

typedef GList * (*ListSelector)(GList *);

static gboolean
select_provider (GtkSourceCompletion *completion,
                 ListSelector         advance,
                 ListSelector         cycle_first,
                 ListSelector         cycle_last,
                 guint                num_advance)
{
	GList *first;
	GList *last;
	GList *orig;
	GList *current;
	GtkSourceCompletionProvider *provider;
	guint num;
	guint pos;
	GList *providers;
	GtkSourceCompletionProvider *visible;
	guint i;

	providers = gtk_source_completion_model_get_providers (completion->priv->model_proposals);
	visible = get_visible_provider (completion);

	get_num_visible_providers (completion, &num, &pos);

	if (num <= 1)
	{
		if (visible != NULL)
		{
			gtk_source_completion_model_set_visible_providers (
					completion->priv->model_proposals,
			                NULL);

			visible_provider_changed (completion);
			return TRUE;
		}

		return FALSE;
	}

	if (visible != NULL)
	{
		orig = g_list_find (providers, visible);
	}
	else
	{
		orig = NULL;
	}

	first = cycle_first ? cycle_first (providers) : NULL;
	last = cycle_last ? cycle_last (providers) : NULL;

	current = orig;

	for (i = 0; i < num_advance; ++i)
	{
		do
		{
			if (current == NULL)
			{
				current = first;
			}
			else if (current == last)
			{
				current = NULL;
			}
			else
			{
				current = advance (current);
			}

			if (current != NULL)
			{
				provider = GTK_SOURCE_COMPLETION_PROVIDER (current->data);

				if (gtk_source_completion_model_n_proposals (completion->priv->model_proposals,
					                                     provider) != 0)
				{
					break;
				}
			}
			else if (!gtk_source_completion_model_is_empty (completion->priv->model_proposals, TRUE))
			{
				break;
			}
		} while (orig != current);
	}

	if (orig == current)
	{
		return FALSE;
	}

	if (current != NULL)
	{
		GList *providers = g_list_append (NULL, current->data);

		gtk_source_completion_model_set_visible_providers (completion->priv->model_proposals,
		                                                   providers);
		g_list_free (providers);
		visible_provider_changed (completion);
	}
	else
	{
		gtk_source_completion_model_set_visible_providers (completion->priv->model_proposals,
		                                                   NULL);
		visible_provider_changed (completion);
	}

	return TRUE;
}

static GList *
wrap_g_list_next (GList *list)
{
	return g_list_next (list);
}

static GList *
wrap_g_list_previous (GList *list)
{
	return g_list_previous (list);
}

static gboolean
select_next_provider (GtkSourceCompletion *completion,
                      guint                num)
{
	return select_provider (completion,
	                        wrap_g_list_next,
	                        g_list_first,
	                        g_list_last,
	                        num);
}

static gboolean
select_previous_provider (GtkSourceCompletion *completion,
                          guint                num)
{
	return select_provider (completion,
	                        wrap_g_list_previous,
	                        g_list_last,
	                        g_list_first,
	                        num);
}

static gboolean
select_last_provider (GtkSourceCompletion *completion)
{
	return select_provider (completion, g_list_last, g_list_last, NULL, 1);
}

static GList *
select_first_provider_wrap (GList *list)
{
	/* Basicly, this means selecting 'All', which is just selection NULL */
	return NULL;
}

static gboolean
select_first_provider (GtkSourceCompletion *completion)
{
	return select_provider (completion, select_first_provider_wrap, NULL, NULL, 1);
}

static void
update_info_position (GtkSourceCompletion *completion)
{
	GdkScreen *screen;
	gint x, y;
	gint width, height;
	gint sw;
	gint info_width;

	gtk_window_get_position (GTK_WINDOW (completion->priv->window), &x, &y);
	gtk_window_get_size (GTK_WINDOW (completion->priv->window),
			     &width, &height);
	gtk_window_get_size (GTK_WINDOW (completion->priv->info_window), &info_width, NULL);

	screen = gtk_window_get_screen (GTK_WINDOW (completion->priv->window));
	sw = gdk_screen_get_width (screen);

	/* Determine on which side to place it */
	if (x + width + info_width >= sw)
	{
		x -= info_width;
	}
	else
	{
		x += width;
	}

	gtk_window_move (GTK_WINDOW (completion->priv->info_window), x, y);
}

static void
row_activated_cb (GtkTreeView         *tree_view,
		  GtkTreePath         *path,
		  GtkTreeViewColumn   *column,
		  GtkSourceCompletion *completion)
{
	DEBUG({
		g_print ("Activating current proposal from row-activated\n");
	});

	activate_current_proposal (completion);
}

static void
update_proposal_info_real (GtkSourceCompletion         *completion,
                           GtkSourceCompletionProvider *provider,
                           GtkSourceCompletionProposal *proposal)
{
	GtkWidget *info_widget;
	gboolean prov_update_info = FALSE;
	GtkSourceCompletionInfo *info_window;

	info_window = GTK_SOURCE_COMPLETION_INFO (completion->priv->info_window);

	if (proposal == NULL)
	{
		/* Set to default widget */
		info_widget = completion->priv->default_info;
		gtk_label_set_markup (GTK_LABEL (info_widget), _("No extra information available"));

		gtk_source_completion_info_set_widget (info_window,
		                                       info_widget);

		gtk_widget_hide (GTK_WIDGET (info_window));
		return;
	}
	else
	{
		info_widget = gtk_source_completion_provider_get_info_widget (provider,
		                                                              proposal);

		/* If there is no special custom widget, use the default */
		if (info_widget == NULL)
		{
			gint width;
			gchar *text;

			info_widget = completion->priv->default_info;
			text = gtk_source_completion_proposal_get_info (proposal);
			gtk_widget_set_size_request (info_widget, -1, -1);

			gtk_label_set_markup (GTK_LABEL (info_widget), text != NULL ? text : _("No extra information available"));

			g_free (text);

			gtk_widget_get_size_request (info_widget, &width, NULL);

			if (width > WINDOW_WIDTH)
			{
				gtk_widget_set_size_request (info_widget, width, -1);
			}
		}
		else
		{
			/* we need to ref the default info widget before removing it */
			if (gtk_source_completion_info_get_widget (info_window) == completion->priv->default_info)
			{
				g_object_ref (completion->priv->default_info);
			}

			prov_update_info = TRUE;
		}
	}

	gtk_source_completion_info_set_widget (info_window, info_widget);

	if (prov_update_info)
	{
		gtk_source_completion_provider_update_info (provider,
			                                    proposal,
			                                    info_window);
	}

	g_signal_handlers_block_by_func (info_window,
	                                 G_CALLBACK (show_info_cb),
	                                 completion);

	gtk_widget_show (GTK_WIDGET (info_window));

	g_signal_handlers_unblock_by_func (info_window,
	                                   G_CALLBACK (show_info_cb),
	                                   completion);
}

static void
update_proposal_info (GtkSourceCompletion *completion)
{
	GtkSourceCompletionProposal *proposal = NULL;
	GtkSourceCompletionProvider *provider;
	GtkTreeIter iter;

	if (get_selected_proposal (completion, &iter, &provider, &proposal))
	{
		update_proposal_info_real (completion, provider, proposal);

		g_object_unref (provider);
		g_object_unref (proposal);
	}
	else
	{
		update_proposal_info_real (completion, NULL, NULL);
	}
}

static void
update_window_position (GtkSourceCompletion *completion)
{
	GtkSourceCompletionProvider *provider;
	GtkSourceCompletionProposal *proposal;

	if (get_selected_proposal (completion, NULL, &provider, &proposal))
	{
		GtkTextIter iter;
		GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view));

		gtk_text_buffer_get_start_iter (buffer, &iter);

		if (gtk_source_completion_provider_get_start_iter (provider,
		                                                   completion->priv->context,
		                                                   proposal,
		                                                   &iter))
		{
			gtk_source_completion_utils_move_to_iter (GTK_WINDOW (completion->priv->window),
			                                          GTK_SOURCE_VIEW (completion->priv->view),
			                                          &iter);
		}

		g_object_unref (provider);
		g_object_unref (proposal);
	}
}

static void
selection_changed_cb (GtkTreeSelection    *selection,
		      GtkSourceCompletion *completion)
{
	if (!gtk_widget_get_visible (completion->priv->window))
	{
		return;
	}

	if (get_selected_proposal (completion, NULL, NULL, NULL))
	{
		completion->priv->select_first = FALSE;
	}
	else if (completion->priv->select_on_show)
	{
		completion->priv->select_first = TRUE;
	}

	/* Update the proposal info here */
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (completion->priv->info_button)))
	{
		update_proposal_info (completion);
	}

	/* Update window position if needed */
	update_window_position (completion);
}

static void
info_toggled_cb (GtkToggleButton     *widget,
		 GtkSourceCompletion *completion)
{
	if (gtk_toggle_button_get_active (widget))
	{
		gtk_widget_show (completion->priv->info_window);
	}
	else
	{
		gtk_widget_hide (completion->priv->info_window);
	}
}

static void
show_info_cb (GtkWidget           *widget,
	      GtkSourceCompletion *completion)
{
	g_return_if_fail (gtk_widget_get_visible (GTK_WIDGET (completion->priv->window)));

	update_info_position (completion);
	update_proposal_info (completion);

	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (completion->priv->info_button),
				      TRUE);
}

static void
show_info_after_cb (GtkWidget           *widget,
	            GtkSourceCompletion *completion)
{
	g_return_if_fail (gtk_widget_get_visible (GTK_WIDGET (completion->priv->window)));

	/* We do this here because GtkLabel does not properly handle
	 * can-focus = FALSE and selects all the text when it gets focus from
	 * showing the info window for the first time */
	gtk_label_select_region (GTK_LABEL (completion->priv->default_info), 0, 0);
}

static void
info_size_allocate_cb (GtkWidget           *widget,
                       GtkAllocation       *allocation,
                       GtkSourceCompletion *completion)
{
	/* Update window position */
	update_info_position (completion);
}

static gint
measure_accelerator_width (GtkWidget *widget)
{
	PangoLayout *layout;
	PangoRectangle rect;

	layout = gtk_widget_create_pango_layout (widget, NULL);
	pango_layout_set_markup (layout, "<small><b>0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n</b></small>", -1);

	pango_layout_get_pixel_extents (layout, &rect, NULL);
	g_object_unref (layout);

	return rect.width;
}

static gboolean
gtk_source_completion_configure_event (GtkWidget           *widget,
                                       GdkEventConfigure   *event,
                                       GtkSourceCompletion *completion)
{
	if (gtk_widget_get_visible (completion->priv->info_window))
	{
		update_info_position (completion);
	}

	return FALSE;
}

static void
set_column_width (GtkTreeView *tv,
                  gint         column,
                  gint         width)
{
	GtkTreeViewColumn *col = gtk_tree_view_get_column (tv, column);

	if (width <= 0)
	{
		return;
	}

	if (gtk_tree_view_column_get_fixed_width (col) != width)
	{
		gtk_tree_view_column_set_fixed_width (col, width);
	}
}

static void
update_column_sizes (GtkSourceCompletion *completion)
{
	gint cwidth;
	GtkTreeView *tv;
	GtkAllocation allocation;
	gint icon_width;
	gint icon_height;

	/* Resize tree view columns accordingly */
	if (completion->priv->num_accelerators > 0)
	{
		GtkStyleContext *context;
		gint xpad;
		gint separator;

		g_object_get (completion->priv->cell_renderer_accelerator,
			      "xpad", &xpad,
			      NULL);

		context = gtk_widget_get_style_context (completion->priv->tree_view_proposals);
		gtk_style_context_get_style (context,
					     "horizontal-separator", &separator,
					     NULL);

		cwidth = measure_accelerator_width (completion->priv->tree_view_proposals);
		cwidth += (xpad + separator) * 2;
	}
	else
	{
		cwidth = 0;
	}

	tv = GTK_TREE_VIEW (completion->priv->tree_view_proposals);
	gtk_widget_get_allocation (GTK_WIDGET (completion->priv->tree_view_proposals),
				   &allocation);

	set_column_width (tv, 0, allocation.width - cwidth);
	set_column_width (tv, 1, cwidth);

	gtk_tree_view_column_set_visible (completion->priv->tree_view_column_accelerator,
	                                  completion->priv->num_accelerators > 0);

	g_object_set (completion->priv->cell_renderer_icon,
	              "visible", completion->priv->show_icons,
	              NULL);

	gtk_icon_size_lookup (GTK_ICON_SIZE_MENU, &icon_width, &icon_height);
	gtk_cell_renderer_set_fixed_size (completion->priv->cell_renderer_icon,
		                          icon_width,
		                          icon_height);
}

static void
gtk_source_completion_size_allocate (GtkWidget           *widget,
                                     GtkAllocation       *allocation,
                                     GtkSourceCompletion *completion)
{
	update_column_sizes (completion);
}

static void
gtk_source_completion_style_updated (GtkWidget           *widget,
                                     GtkSourceCompletion *completion)
{
	update_column_sizes (completion);
}

static gboolean
view_focus_out_event_cb (GtkWidget     *widget,
                         GdkEventFocus *event,
                         gpointer       user_data)
{
	GtkSourceCompletion *completion = GTK_SOURCE_COMPLETION (user_data);

	if (gtk_widget_get_visible (completion->priv->window) &&
	    !gtk_widget_has_focus (completion->priv->window))
	{
		DEBUG({
			g_print ("Lost focus\n");
		});

		gtk_source_completion_hide (completion);
	}

	return FALSE;
}

static gboolean
view_button_press_event_cb (GtkWidget      *widget,
			    GdkEventButton *event,
			    gpointer        user_data)
{
	GtkSourceCompletion *completion = GTK_SOURCE_COMPLETION (user_data);

	if (gtk_widget_get_visible (completion->priv->window))
	{
		DEBUG({
			g_print ("Button press in the view\n");
		});

		gtk_source_completion_hide (completion);
	}

	return FALSE;
}

static void
gtk_source_completion_activate_proposal (GtkSourceCompletion *completion)
{
	DEBUG({
		g_print ("Activating from default proposal activate handler\n");
	});

	activate_current_proposal (completion);
}

static void
gtk_source_completion_move_cursor (GtkSourceCompletion *completion,
                                   GtkScrollStep        step,
                                   gint                 num)
{
	if (step == GTK_SCROLL_ENDS)
	{
		if (num > 0)
		{
			select_last_proposal (completion);
		}
		else
		{
			select_first_proposal (completion);
		}
	}
	else
	{
		if (step == GTK_SCROLL_PAGES)
		{
			num *= completion->priv->proposal_page_size;
		}

		if (num > 0)
		{
			select_next_proposal (completion, num);
		}
		else
		{
			select_previous_proposal (completion, -1 * num);
		}
	}
}

static void
gtk_source_completion_move_page (GtkSourceCompletion *completion,
                                 GtkScrollStep        step,
                                 gint                 num)
{
	if (step == GTK_SCROLL_ENDS)
	{
		if (num > 0)
		{
			select_last_provider (completion);
		}
		else
		{
			select_first_provider (completion);
		}
	}
	else
	{
		if (step == GTK_SCROLL_PAGES)
		{
			num *= completion->priv->provider_page_size;
		}

		if (num > 0)
		{
			select_next_provider (completion, num);
		}
		else
		{
			select_previous_provider (completion, -1 * num);
		}
	}
}

static gboolean
activate_by_accelerator (GtkSourceCompletion *completion,
                         gint                 num)
{
	GtkTreeIter iter;
	GtkTreeModel *model = GTK_TREE_MODEL (completion->priv->model_proposals);
	gint i = -1;

	num = num == 0 ? 9 : num - 1;

	if (num < 0 || num > completion->priv->num_accelerators)
	{
		return FALSE;
	}

	if (gtk_tree_model_get_iter_first (model, &iter))
	{
		do
		{
			if (!gtk_source_completion_model_iter_is_header (completion->priv->model_proposals,
			                                                 &iter))
			{
				++i;
			}
		} while (i < num && gtk_tree_model_iter_next (model, &iter));

		if (i == num)
		{
			gtk_tree_selection_select_iter (
				gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals)),
				&iter);

			activate_current_proposal (completion);
		}
	}
	return TRUE;
}

static gboolean
view_key_press_event_cb (GtkSourceView       *view,
			 GdkEventKey         *event,
			 GtkSourceCompletion *completion)
{
	GdkModifierType mod;
	GtkLabel *label_info;
	GtkBindingSet *binding_set;

	mod = gtk_accelerator_get_default_mod_mask () & event->state;

	if (!gtk_widget_get_visible (completion->priv->window))
	{
		return FALSE;
	}

	label_info = GTK_LABEL (completion->priv->label_info);

	/* Handle info button mnemonic */
	if (event->keyval == gtk_label_get_mnemonic_keyval (label_info) &&
	    mod == GDK_MOD1_MASK)
	{
		GtkToggleButton *button = GTK_TOGGLE_BUTTON (completion->priv->info_button);

		gtk_toggle_button_set_active (button,
		                              !gtk_toggle_button_get_active (button));
		return TRUE;
	}

	if (mod == GDK_MOD1_MASK &&
	    event->keyval >= GDK_KEY_0 && event->keyval <= GDK_KEY_9 &&
	    completion->priv->num_accelerators > 0)
	{
		if (activate_by_accelerator (completion, event->keyval - GDK_KEY_0))
		{
			return TRUE;
		}
	}

	binding_set = gtk_binding_set_by_class (GTK_SOURCE_COMPLETION_GET_CLASS (completion));

	if (gtk_binding_set_activate (binding_set,
	                              event->keyval,
	                              event->state,
	                              G_OBJECT (completion)))
	{
		return TRUE;
	}

	return FALSE;
}

static void
update_typing_offsets (GtkSourceCompletion *completion)
{
	GtkTextBuffer *buffer;
	GtkTextIter start;
	GtkTextIter end;
	gchar *word;

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view));
	word = gtk_source_completion_utils_get_word_iter (GTK_SOURCE_BUFFER (buffer),
							  NULL,
							  &start,
							  &end);
	g_free (word);

	completion->priv->typing_line = gtk_text_iter_get_line (&start);
	completion->priv->typing_line_offset = gtk_text_iter_get_line_offset (&start);
}

static GList *
select_providers (GtkSourceCompletion        *completion,
                  GList                      *providers,
                  GtkSourceCompletionContext *context)
{
	/* Select providers based on selection */
	GList *selection = NULL;

	if (providers == NULL)
	{
		providers = completion->priv->providers;
	}

	while (providers)
	{
		GtkSourceCompletionProvider *provider =
			GTK_SOURCE_COMPLETION_PROVIDER (providers->data);

		if (gtk_source_completion_provider_match (provider, context))
		{
			selection = g_list_prepend (selection, provider);
		}

		providers = g_list_next (providers);
	}

	return g_list_reverse (selection);
}

static gboolean
auto_completion_final (GtkSourceCompletion *completion)
{
	/* Store and set to NULL because update_completion will cancel the last
	   completion, which will also remove the timeout source which in turn
	   would free these guys */
	GtkSourceCompletionContext *context = completion->priv->auto_completion_context;
	GList *selection = completion->priv->auto_completion_selection;

	completion->priv->auto_completion_context = NULL;
	completion->priv->auto_completion_selection = NULL;

	update_completion (completion, selection, context);

	/* No need to free the context since it was a floating reference which
	   has been taken over by update_completion */
	g_list_free (selection);
	return FALSE;
}

static void
auto_completion_destroy (GtkSourceCompletion *completion)
{
	if (completion->priv->auto_completion_context != NULL)
	{
		g_object_unref (completion->priv->auto_completion_context);
		completion->priv->auto_completion_context = NULL;
	}

	g_list_free (completion->priv->auto_completion_selection);
	completion->priv->auto_completion_selection = NULL;
}

static gint
minimum_auto_complete_delay (GtkSourceCompletion *completion,
                             GList               *providers)
{
	gint min_delay = completion->priv->auto_complete_delay;

	while (providers)
	{
		GtkSourceCompletionProvider *provider = providers->data;
		gint delay = gtk_source_completion_provider_get_interactive_delay (provider);

		if (delay < 0)
		{
			delay = completion->priv->auto_complete_delay;
		}

		if (delay < min_delay)
		{
			min_delay = delay;
		}

		providers = g_list_next (providers);
	}

	return min_delay;
}

static gboolean
auto_completion_prematch (GtkSourceCompletion *completion)
{
	GtkTextIter iter;
	GtkSourceCompletionContext *context;
	gint delay;
	GList *selection;

	/* Do a prematch on the available interactive providers and determine
	   the minimum delay to the real selection that matches the current
	   context */

	completion->priv->show_timed_out_id = 0;

	if (gtk_widget_get_visible (completion->priv->window))
	{
		return FALSE;
	}

	/* Check if the user has changed the cursor position. If yes, we don't complete */
	get_iter_at_insert (completion, &iter);

	if ((gtk_text_iter_get_line (&iter) != completion->priv->typing_line))
	{
		return FALSE;
	}

	context = gtk_source_completion_create_context (completion, &iter);
	g_object_ref_sink (context);

	g_object_set (context,
	              "activation",
	              GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE,
	              NULL);

	g_signal_emit (completion, signals[POPULATE_CONTEXT], 0, context);

	selection = select_providers (completion,
	                              completion->priv->interactive_providers,
	                              context);

	if (selection == NULL)
	{
		g_object_unref (context);

		return FALSE;
	}

	/* Check the minimum delay on this set */
	delay = minimum_auto_complete_delay (completion, selection);
	completion->priv->auto_completion_context = context;
	completion->priv->auto_completion_selection = selection;

	if (delay > completion->priv->min_auto_complete_delay)
	{
		completion->priv->show_timed_out_id =
			g_timeout_add_full (G_PRIORITY_DEFAULT,
			                    delay - completion->priv->min_auto_complete_delay,
			                    (GSourceFunc)auto_completion_final,
			                    completion,
			                    (GDestroyNotify)auto_completion_destroy);
	}
	else
	{
		auto_completion_final (completion);
	}

	return FALSE;
}

static void
interactive_do_show (GtkSourceCompletion *completion)
{
	if (completion->priv->interactive_providers == NULL)
	{
		return;
	}

	update_typing_offsets (completion);

	if (completion->priv->show_timed_out_id != 0)
	{
		g_source_remove (completion->priv->show_timed_out_id);
	}

	/* Install first handler to do the match on the minimum auto complete
	   delay */
	completion->priv->show_timed_out_id =
		g_timeout_add (completion->priv->min_auto_complete_delay,
		               (GSourceFunc)auto_completion_prematch,
		               completion);
}

static void
update_interactive_completion (GtkSourceCompletion *completion,
                               GtkTextIter         *iter,
                               gboolean             start_completion)
{
	/* Only handle interactive completion in editable parts of the buffer */
	if (!gtk_text_iter_can_insert (iter, TRUE))
	{
		return;
	}

	if (completion->priv->context == NULL)
	{
		/* Schedule for interactive showing */
		if (start_completion)
		{
			interactive_do_show (completion);
		}
		else if (completion->priv->show_timed_out_id)
		{
			g_source_remove (completion->priv->show_timed_out_id);
			completion->priv->show_timed_out_id = 0;
		}
	}
	else if ((gtk_source_completion_context_get_activation (completion->priv->context) &
		 GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE) &&
		 gtk_text_iter_get_line (iter) != completion->priv->typing_line)
	{
		gtk_source_completion_hide (completion);
	}
	else
	{
		update_completion (completion,
				   completion->priv->active_providers,
				   completion->priv->context);
	}
}

static gboolean
buffer_delete_range_cb (GtkTextBuffer       *buffer,
                        GtkTextIter         *start,
                        GtkTextIter         *end,
                        GtkSourceCompletion *completion)
{
	update_interactive_completion (completion, start, FALSE);
	return FALSE;
}

static void
buffer_insert_text_cb (GtkTextBuffer       *buffer,
                       GtkTextIter         *location,
                       gchar               *text,
                       gint                 len,
                       GtkSourceCompletion *completion)
{
	update_interactive_completion (completion, location, TRUE);
}

static void
buffer_mark_set_cb (GtkTextBuffer       *buffer,
                    GtkTextIter         *iter,
                    GtkTextMark         *mark,
                    GtkSourceCompletion *completion)
{
	GtkTextIter it;

	if (mark != gtk_text_buffer_get_insert (buffer) ||
	    !completion->priv->active_providers)
	{
		return;
	}

	/* Check if the cursor is still on the completion line */
	gtk_source_completion_context_get_iter (completion->priv->context,
	                                        &it);

	if (!gtk_text_iter_equal (iter, &it))
	{
		gtk_source_completion_hide (completion);
		return;
	}

	update_completion (completion,
	                   completion->priv->active_providers,
	                   completion->priv->context);
}

static void
disconnect_view (GtkSourceCompletion *completion)
{
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view));
	gint i;

	for (i = 0; i < LAST_EXTERNAL_SIGNAL; ++i)
	{
		GObject *obj;

		if (i == TEXT_BUFFER_BEGIN_SIGNALS)
		{
			continue;
		}

		obj = i < TEXT_BUFFER_BEGIN_SIGNALS ?
		      G_OBJECT (completion->priv->view) :
		      G_OBJECT (buffer);

		g_signal_handler_disconnect (obj, completion->priv->signals_ids[i]);
	}
}

static void
buffer_paste_done_cb (GtkTextBuffer       *buffer,
                      GtkClipboard        *clipboard,
                      GtkSourceCompletion *completion)
{
	/* Cancel any interactive completion in progress after a paste */
	if (completion->priv->show_timed_out_id)
	{
		g_source_remove (completion->priv->show_timed_out_id);
		completion->priv->show_timed_out_id = 0;
	}
}

static void
text_view_editable_cb (GtkTextView         *view,
                       GParamSpec          *spec,
                       GtkSourceCompletion *completion)
{
	gboolean editable = gtk_text_view_get_editable (view);
	GtkSourceBuffer *buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (view));

	/* Block when view is not editable, and unblock if it is */
	if (!editable)
	{
		completion_begin_block (completion, buffer);
	}
	else
	{
		completion_end_block (completion, buffer);
	}
}

static void
connect_view (GtkSourceCompletion *completion)
{
	GtkTextBuffer *buffer;

	completion->priv->signals_ids[TEXT_VIEW_FOCUS_OUT] =
		g_signal_connect (completion->priv->view,
				  "focus-out-event",
				  G_CALLBACK (view_focus_out_event_cb),
				  completion);

	completion->priv->signals_ids[TEXT_VIEW_BUTTON_PRESS] =
		g_signal_connect (completion->priv->view,
				  "button-press-event",
				  G_CALLBACK (view_button_press_event_cb),
				  completion);

	completion->priv->signals_ids[TEXT_VIEW_KEY_PRESS] =
		g_signal_connect (completion->priv->view,
				  "key-press-event",
				  G_CALLBACK (view_key_press_event_cb),
				  completion);

	completion->priv->signals_ids[TEXT_VIEW_EDITABLE] =
		g_signal_connect (completion->priv->view,
		                  "notify::editable",
		                  G_CALLBACK (text_view_editable_cb),
		                  completion);

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view));

	completion->priv->signals_ids[TEXT_BUFFER_DELETE_RANGE] =
		g_signal_connect_after (buffer,
					"delete-range",
					G_CALLBACK (buffer_delete_range_cb),
					completion);

	completion->priv->signals_ids[TEXT_BUFFER_INSERT_TEXT] =
		g_signal_connect_after (buffer,
					"insert-text",
					G_CALLBACK (buffer_insert_text_cb),
					completion);

	completion->priv->signals_ids[TEXT_BUFFER_MARK_SET] =
		g_signal_connect_after (buffer,
					"mark-set",
					G_CALLBACK (buffer_mark_set_cb),
					completion);

	completion->priv->signals_ids[SOURCE_BUFFER_UNDO] =
		g_signal_connect_data (buffer,
		                       "undo",
		                       G_CALLBACK (completion_begin_block),
		                       completion,
		                       NULL,
		                       G_CONNECT_SWAPPED);

	completion->priv->signals_ids[SOURCE_BUFFER_UNDO_AFTER] =
		g_signal_connect_data (buffer,
		                       "undo",
		                       G_CALLBACK (completion_end_block),
		                       completion,
		                       NULL,
		                       G_CONNECT_SWAPPED | G_CONNECT_AFTER);

	completion->priv->signals_ids[SOURCE_BUFFER_REDO] =
		g_signal_connect_data (buffer,
		                       "redo",
		                       G_CALLBACK (completion_begin_block),
		                       completion,
		                       NULL,
		                       G_CONNECT_SWAPPED);

	completion->priv->signals_ids[SOURCE_BUFFER_REDO_AFTER] =
		g_signal_connect_data (buffer,
		                       "redo",
		                       G_CALLBACK (completion_end_block),
		                       completion,
		                       NULL,
		                       G_CONNECT_SWAPPED | G_CONNECT_AFTER);

	completion->priv->signals_ids[TEXT_BUFFER_PASTE_DONE] =
		g_signal_connect (buffer,
		                  "paste-done",
		                  G_CALLBACK (buffer_paste_done_cb),
		                  completion);
}

static void
cancel_completion (GtkSourceCompletion        *completion,
                   GtkSourceCompletionContext *context)
{
	if (completion->priv->show_timed_out_id)
	{
		g_source_remove (completion->priv->show_timed_out_id);
		completion->priv->show_timed_out_id = 0;
	}

	if (completion->priv->context == NULL)
	{
		if (context != NULL)
		{
			completion->priv->context = context;
		}
	}
	else
	{
		/* Inform providers of cancellation through the context */
		_gtk_source_completion_context_cancel (completion->priv->context);

		/* Let the model know we are cancelling the population */
		gtk_source_completion_model_cancel (completion->priv->model_proposals);

		if (completion->priv->context != context)
		{
			g_object_unref (completion->priv->context);
			completion->priv->context = NULL;
		}
		else if (context != NULL)
		{
			completion->priv->context = context;
		}

		g_list_free (completion->priv->running_providers);
		completion->priv->running_providers = NULL;
	}
}

static void
gtk_source_completion_dispose (GObject *object)
{
	GtkSourceCompletion *completion = GTK_SOURCE_COMPLETION (object);

	/* Cancel running completion */
	cancel_completion (completion, NULL);

	if (completion->priv->view != NULL)
	{
		disconnect_view (completion);
		g_object_unref (completion->priv->view);

		completion->priv->view = NULL;

		g_list_foreach (completion->priv->providers, (GFunc)g_object_unref, NULL);
	}

	g_list_free (completion->priv->active_providers);
	g_list_free (completion->priv->interactive_providers);

	G_OBJECT_CLASS (gtk_source_completion_parent_class)->dispose (object);
}

static void
gtk_source_completion_finalize (GObject *object)
{
	GtkSourceCompletion *completion = GTK_SOURCE_COMPLETION (object);

	if (completion->priv->show_timed_out_id != 0)
	{
		g_source_remove (completion->priv->show_timed_out_id);
	}

	g_list_free (completion->priv->providers);
	g_list_free (completion->priv->active_providers);

	G_OBJECT_CLASS (gtk_source_completion_parent_class)->finalize (object);
}

static void
update_min_auto_complete_delay (GtkSourceCompletion *completion)
{
	completion->priv->min_auto_complete_delay =
		minimum_auto_complete_delay (completion,
		                             completion->priv->interactive_providers);
}

static void
gtk_source_completion_get_property (GObject    *object,
				    guint       prop_id,
				    GValue     *value,
				    GParamSpec *pspec)
{
	GtkSourceCompletion *completion;

	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (object));

	completion = GTK_SOURCE_COMPLETION (object);

	switch (prop_id)
	{
		case PROP_VIEW:
			g_value_set_object (value, completion->priv->view);
			break;
		case PROP_REMEMBER_INFO_VISIBILITY:
			g_value_set_boolean (value, completion->priv->remember_info_visibility);
			break;
		case PROP_SELECT_ON_SHOW:
			g_value_set_boolean (value, completion->priv->select_on_show);
			break;
		case PROP_SHOW_HEADERS:
			g_value_set_boolean (value, completion->priv->show_headers);
			break;
		case PROP_SHOW_ICONS:
			g_value_set_boolean (value, completion->priv->show_icons);
			break;
		case PROP_ACCELERATORS:
			g_value_set_uint (value, completion->priv->num_accelerators);
			break;
		case PROP_AUTO_COMPLETE_DELAY:
			g_value_set_uint (value, completion->priv->auto_complete_delay);
			break;
		case PROP_PROPOSAL_PAGE_SIZE:
			g_value_set_uint (value, completion->priv->proposal_page_size);
			break;
		case PROP_PROVIDER_PAGE_SIZE:
			g_value_set_uint (value, completion->priv->provider_page_size);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_completion_set_property (GObject      *object,
				    guint         prop_id,
				    const GValue *value,
				    GParamSpec   *pspec)
{
	GtkSourceCompletion *completion;

	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (object));

	completion = GTK_SOURCE_COMPLETION (object);

	switch (prop_id)
	{
		case PROP_VIEW:
			/* On construction only */
			completion->priv->view = g_value_dup_object (value);
			connect_view (completion);
			break;
		case PROP_REMEMBER_INFO_VISIBILITY:
			completion->priv->remember_info_visibility = g_value_get_boolean (value);
			break;
		case PROP_SELECT_ON_SHOW:
			completion->priv->select_on_show = g_value_get_boolean (value);
			break;
		case PROP_SHOW_HEADERS:
			completion->priv->show_headers = g_value_get_boolean (value);

			if (completion->priv->model_proposals != NULL)
			{
				gtk_source_completion_model_set_show_headers (completion->priv->model_proposals,
				                                              completion->priv->show_headers);
			}
			break;
		case PROP_SHOW_ICONS:
			completion->priv->show_icons = g_value_get_boolean (value);

			update_column_sizes (completion);
			break;
		case PROP_ACCELERATORS:
			completion->priv->num_accelerators = g_value_get_uint (value);

			update_column_sizes (completion);
			break;
		case PROP_AUTO_COMPLETE_DELAY:
			completion->priv->auto_complete_delay = g_value_get_uint (value);
			update_min_auto_complete_delay (completion);
			break;
		case PROP_PROPOSAL_PAGE_SIZE:
			completion->priv->proposal_page_size = g_value_get_uint (value);
			break;
		case PROP_PROVIDER_PAGE_SIZE:
			completion->priv->provider_page_size = g_value_get_uint (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
check_first_selected (GtkSourceCompletion *completion)
{
	GtkTreeSelection *selection;
	GtkTreeIter piter;
	GtkTreeIter first;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL (completion->priv->model_proposals);
	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

	if (!completion->priv->select_first)
	{
		return;
	}

	if (!gtk_tree_model_get_iter_first (model, &first))
	{
		return;
	}

	piter = first;

	while (gtk_source_completion_model_iter_is_header (completion->priv->model_proposals, &piter))
	{
		if (!gtk_tree_model_iter_next (model, &piter))
		{
			return;
		}
	}

	gtk_tree_selection_select_iter (selection, &piter);

	gtk_tree_model_get_iter_first (model, &piter);
	scroll_to_iter (completion, &first);

	completion->priv->select_first = TRUE;
}

static void
on_row_inserted_cb (GtkTreeModel        *tree_model,
                    GtkTreePath         *path,
                    GtkTreeIter         *iter,
                    GtkSourceCompletion *completion)
{
	if (!gtk_widget_get_visible (completion->priv->window))
	{
		if (!completion->priv->remember_info_visibility)
		{
			completion->priv->info_visible = FALSE;
		}

		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (completion->priv->info_button),
					      completion->priv->info_visible);

		DEBUG({
			g_print ("Emitting show\n");
		});

		g_signal_emit (completion, signals[SHOW], 0);
	}
	else
	{
		DEBUG({
			g_print ("Already visible\n");
		});
	}

	check_first_selected (completion);
}

static void
on_row_deleted_cb (GtkTreeModel        *tree_model,
                   GtkTreePath         *path,
                   GtkSourceCompletion *completion)
{
	check_first_selected (completion);
}

static void
gtk_source_completion_hide_default (GtkSourceCompletion *completion)
{
	gtk_label_set_markup (GTK_LABEL (completion->priv->default_info), "");

	gtk_widget_hide (completion->priv->info_window);
	gtk_widget_hide (completion->priv->window);

	gtk_source_completion_model_clear (completion->priv->model_proposals);

	cancel_completion (completion, NULL);

	g_list_free (completion->priv->active_providers);
	completion->priv->active_providers = NULL;

	completion->priv->select_first = FALSE;

	completion->priv->info_visible =
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (completion->priv->info_button));
}

static void
gtk_source_completion_show_default (GtkSourceCompletion *completion)
{
	/* Move completion window */
	if (completion->priv->context)
	{
		GtkTextIter location;
		gtk_source_completion_context_get_iter (completion->priv->context,
		                                        &location);

		gtk_source_completion_utils_move_to_iter (GTK_WINDOW (completion->priv->window),
		                                          GTK_SOURCE_VIEW (completion->priv->view),
		                                          &location);
	}

	gtk_widget_show (GTK_WIDGET (completion->priv->window));
	gtk_widget_grab_focus (GTK_WIDGET (completion->priv->view));

	if (completion->priv->select_on_show)
	{
		select_first_proposal (completion);
	}
}

static void
gtk_source_completion_class_init (GtkSourceCompletionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GtkBindingSet *binding_set;

	g_type_class_add_private (klass, sizeof (GtkSourceCompletionPrivate));

	object_class->get_property = gtk_source_completion_get_property;
	object_class->set_property = gtk_source_completion_set_property;
	object_class->finalize = gtk_source_completion_finalize;
	object_class->dispose = gtk_source_completion_dispose;

	klass->show = gtk_source_completion_show_default;
	klass->hide = gtk_source_completion_hide_default;

	klass->move_cursor = gtk_source_completion_move_cursor;
	klass->move_page = gtk_source_completion_move_page;
	klass->activate_proposal = gtk_source_completion_activate_proposal;

	/**
	 * GtkSourceCompletion:view:
	 *
	 * The #GtkSourceView bound to the completion object.
	 *
	 */
	g_object_class_install_property (object_class,
					 PROP_VIEW,
					 g_param_spec_object ("view",
							      _("View"),
							      _("The GtkSourceView bound to the completion"),
							      GTK_SOURCE_TYPE_VIEW,
							      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GtkSourceCompletion:remember-info-visibility:
	 *
	 * Determines whether the visibility of the info window should be
	 * saved when the completion is hidden, and restored when the completion
	 * is shown again.
	 *
	 */
	g_object_class_install_property (object_class,
					 PROP_REMEMBER_INFO_VISIBILITY,
					 g_param_spec_boolean ("remember-info-visibility",
							      _("Remember Info Visibility"),
							      _("Remember the last info window visibility state"),
							      FALSE,
							      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
	/**
	 * GtkSourceCompletion:select-on-show:
	 *
	 * Determines whether the first proposal should be selected when the
	 * completion is first shown.
	 *
	 */
	g_object_class_install_property (object_class,
					 PROP_SELECT_ON_SHOW,
					 g_param_spec_boolean ("select-on-show",
							      _("Select on Show"),
							      _("Select first proposal when completion is shown"),
							      TRUE,
							      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion:show-headers:
	 *
	 * Determines whether provider headers should be shown in the proposal
	 * list if there is more than one provider with proposals.
	 *
	 */
	g_object_class_install_property (object_class,
					 PROP_SHOW_HEADERS,
					 g_param_spec_boolean ("show-headers",
							      _("Show Headers"),
							      _("Show provider headers when proposals from multiple providers are available"),
							      TRUE,
							      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion:show-icons:
	 *
	 * Determines whether provider and proposal icons should be shown in
	 * the completion popup.
	 *
	 */
	g_object_class_install_property (object_class,
					 PROP_SHOW_ICONS,
					 g_param_spec_boolean ("show-icons",
							      _("Show Icons"),
							      _("Show provider and proposal icons in the completion popup"),
							      TRUE,
							      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion:accelerators:
	 *
	 * Number of accelerators to show for the first proposals.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_ACCELERATORS,
	                                 g_param_spec_uint ("accelerators",
	                                                    _("Accelerators"),
	                                                    _("Number of proposal accelerators to show"),
	                                                    0,
	                                                    10,
	                                                    5,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion:auto-complete-delay:
	 *
	 * Determines the popup delay (in milliseconds) at which the completion
	 * will be shown for interactive completion.
	 *
	 */
	g_object_class_install_property (object_class,
					 PROP_AUTO_COMPLETE_DELAY,
					 g_param_spec_uint ("auto-complete-delay",
							    _("Auto Complete Delay"),
							    _("Completion popup delay for interactive completion"),
							    0,
							    G_MAXUINT,
							    250,
							    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion:provider-page-size:
	 *
	 * The scroll page size of the provider pages in the completion window.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_PROVIDER_PAGE_SIZE,
	                                 g_param_spec_uint ("provider-page-size",
	                                                    _("Provider Page Size"),
	                                                    _("Provider scrolling page size"),
	                                                    1,
	                                                    G_MAXUINT,
	                                                    5,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion:proposal-page-size:
	 *
	 * The scroll page size of the proposals in the completion window.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_PROPOSAL_PAGE_SIZE,
	                                 g_param_spec_uint ("proposal-page-size",
	                                                    _("Proposal Page Size"),
	                                                    _("Proposal scrolling page size"),
	                                                    1,
	                                                    G_MAXUINT,
	                                                    5,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * GtkSourceCompletion::show:
	 * @completion: The #GtkSourceCompletion who emits the signal
	 *
	 * Emitted when the completion window is shown. The default handler
	 * will actually show the window.
	 *
	 */
	signals[SHOW] =
		g_signal_new ("show",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceCompletionClass, show),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);


	/**
	 * GtkSourceCompletion::hide:
	 * @completion: The #GtkSourceCompletion who emits the signal
	 *
	 * Emitted when the completion window is hidden. The default handler
	 * will actually hide the window.
	 *
	 */
	signals[HIDE] =
		g_signal_new ("hide",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceCompletionClass, hide),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * GtkSourceCompletion::populate-context:
	 * @completion: The #GtkSourceCompletion who emits the signal
	 * @context: The #GtkSourceCompletionContext for the current completion
	 *
	 * Emitted just before starting to populate the completion with providers.
	 * You can use this signal to add additional attributes in the context.
	 *
	 */
	signals[POPULATE_CONTEXT] =
		g_signal_new ("populate-context",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		              G_STRUCT_OFFSET (GtkSourceCompletionClass, populate_context),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              GTK_SOURCE_TYPE_COMPLETION_CONTEXT);

	/* Actions */

	/**
	 * GtkSourceCompletion::move-cursor:
	 * @completion: The #GtkSourceCompletion who emits the signal
	 * @step: The #GtkScrollStep by which to move the cursor
	 * @num: The amount of steps to move the cursor
	 *
	 * The ::move-cursor signal is a keybinding signal which gets emitted when
	 * the user initiates a cursor movement.
	 *
	 * Applications should not connect to it, but may emit it with
	 * #g_signal_emit_by_name if they need to control the cursor
	 * programmatically.
	 *
	 */
	signals [MOVE_CURSOR] =
		g_signal_new ("move-cursor",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceCompletionClass, move_cursor),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__ENUM_INT,
			      G_TYPE_NONE,
			      2,
			      GTK_TYPE_SCROLL_STEP,
			      G_TYPE_INT);

	/**
	 * GtkSourceCompletion::move-page:
	 * @completion: The #GtkSourceCompletion who emits the signal
	 * @step: The #GtkScrollStep by which to move the page
	 * @num: The amount of steps to move the page
	 *
	 * The ::move-page signal is a keybinding signal which gets emitted when
	 * the user initiates a page movement (i.e. switches between provider
	 * pages).
	 *
	 * Applications should not connect to it, but may emit it with
	 * #g_signal_emit_by_name if they need to control the page selection
	 * programmatically.
	 *
	 */
	signals [MOVE_PAGE] =
		g_signal_new ("move-page",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceCompletionClass, move_page),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__ENUM_INT,
			      G_TYPE_NONE,
			      2,
			      GTK_TYPE_SCROLL_STEP,
			      G_TYPE_INT);

	/**
	 * GtkSourceCompletion::activate-proposal:
	 * @completion: The #GtkSourceCompletion who emits the signal
	 *
	 * The ::activate-proposal signal is a keybinding signal which gets
	 * emitted when the user initiates a proposal activation.
	 *
	 * Applications should not connect to it, but may emit it with
	 * #g_signal_emit_by_name if they need to control the proposal activation
	 * programmatically.
	 *
	 */
	signals [ACTIVATE_PROPOSAL] =
		g_signal_new ("activate-proposal",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceCompletionClass, activate_proposal),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/* Key bindings */
	binding_set = gtk_binding_set_by_class (klass);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Down,
				      0,
				      "move-cursor",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Page_Down,
				      0,
				      "move-cursor",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Up,
				      0,
				      "move-cursor",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Page_Up,
				      0,
				      "move-cursor",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Home,
				      0,
				      "move-cursor",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_End,
				      0,
				      "move-cursor",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Escape,
				      0,
				      "hide",
				      0);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Return,
				      0,
				      "activate-proposal",
				      0);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Tab,
				      0,
				      "activate-proposal",
				      0);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Left,
				      GDK_CONTROL_MASK,
				      "move-page",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Right,
				      GDK_CONTROL_MASK,
				      "move-page",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Home,
				      GDK_CONTROL_MASK,
				      "move-page",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_End,
				      GDK_CONTROL_MASK,
				      "move-page",
				      2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, 1);
}

static void
update_transient_for_info (GObject             *window,
                           GParamSpec          *spec,
                           GtkSourceCompletion *completion)
{
	gtk_window_set_transient_for (GTK_WINDOW (completion->priv->info_window),
				      gtk_window_get_transient_for (GTK_WINDOW (completion->priv->window)));

}

static void
render_proposal_icon_func (GtkTreeViewColumn   *column,
                           GtkCellRenderer     *cell,
                           GtkTreeModel        *model,
                           GtkTreeIter         *iter,
                           GtkSourceCompletion *completion)
{
	gboolean isheader;
	GdkPixbuf *icon;

	isheader = gtk_source_completion_model_iter_is_header (completion->priv->model_proposals,
	                                                       iter);

	if (isheader)
	{
		GtkStyleContext *context;
		GdkRGBA color;

		context = gtk_widget_get_style_context (completion->priv->tree_view_proposals);
		gtk_style_context_get_background_color (context,
		                                        GTK_STATE_FLAG_INSENSITIVE,
		                                        &color);

		g_object_set (cell,
		              "cell-background-rgba", &color,
		              NULL);
	}
	else
	{
		g_object_set (cell,
		              "cell-background-set", FALSE,
		              NULL);
	}

	gtk_tree_model_get (model,
	                    iter,
	                    GTK_SOURCE_COMPLETION_MODEL_COLUMN_ICON,
	                    &icon,
	                    -1);

	g_object_set (cell, "pixbuf", icon, NULL);

	if (icon)
	{
		g_object_unref (icon);
	}
}

static void
render_proposal_text_func (GtkTreeViewColumn   *column,
                           GtkCellRenderer     *cell,
                           GtkTreeModel        *model,
                           GtkTreeIter         *iter,
                           GtkSourceCompletion *completion)
{
	gchar *label;
	gchar *markup;
	GtkSourceCompletionProvider *provider;
	gboolean isheader;

	isheader = gtk_source_completion_model_iter_is_header (completion->priv->model_proposals,
		                                               iter);

	if (isheader)
	{
		gchar *name;
		GtkStyleContext *context;
		GdkRGBA color;
		GdkRGBA bgcolor;

		gtk_tree_model_get (model,
		                    iter,
		                    GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROVIDER,
		                    &provider,
		                    -1);

		name = gtk_source_completion_provider_get_name (provider);

		if (name != NULL)
		{
			gchar *escaped = g_markup_escape_text (name, -1);
			label = g_strdup_printf ("<b>%s</b>", escaped);
			g_free (escaped);
			g_free (name);
		}
		else
		{
			label = g_strdup_printf ("<b>%s</b>", _("Provider"));
		}

		context = gtk_widget_get_style_context (completion->priv->tree_view_proposals);
		gtk_style_context_get_color (context,
		                             GTK_STATE_FLAG_INSENSITIVE,
		                             &color);
		gtk_style_context_get_background_color (context,
		                                        GTK_STATE_FLAG_INSENSITIVE,
		                                        &bgcolor);
		g_object_set (cell,
		              "markup", label,
		              "foreground-rgba", &color,
		              "cell-background-rgba", &bgcolor,
		              NULL);

		g_free (label);
		g_object_unref (provider);
	}
	else
	{
		gtk_tree_model_get (model,
		                    iter,
		                    GTK_SOURCE_COMPLETION_MODEL_COLUMN_LABEL,
		                    &label,
		                    GTK_SOURCE_COMPLETION_MODEL_COLUMN_MARKUP,
		                    &markup,
		                    -1);

		if (!markup)
		{
			markup = g_markup_escape_text (label ? label : "", -1);
		}

		g_object_set (cell,
		              "markup", markup,
		              "cell-background-set", FALSE,
		              "foreground-set", FALSE,
		              NULL);

		g_free (label);
		g_free (markup);
	}
}

static gint
iter_accelerator (GtkSourceCompletion *completion,
                  GtkTreeIter         *iter)
{
	GtkTreeIter it;
	GtkTreeModel *model = GTK_TREE_MODEL (completion->priv->model_proposals);
	gint ret = 0;

	if (!gtk_tree_model_get_iter_first (model, &it))
	{
		return -1;
	}

	do
	{
		if (!gtk_source_completion_model_iter_is_header (completion->priv->model_proposals,
		                                                 &it))
		{
			if (gtk_source_completion_model_iter_equal (completion->priv->model_proposals,
			                                            iter,
			                                            &it))
			{
				return ret;
			}

			++ret;
		}
	} while (ret < completion->priv->num_accelerators && gtk_tree_model_iter_next (model, &it));

	return -1;
}

static void
render_proposal_accelerator_func (GtkTreeViewColumn   *column,
                                  GtkCellRenderer     *cell,
                                  GtkTreeModel        *model,
                                  GtkTreeIter         *iter,
                                  GtkSourceCompletion *completion)
{
	gboolean isheader;
	GtkStyleContext *context;
	GdkRGBA color;

	isheader = gtk_source_completion_model_iter_is_header (completion->priv->model_proposals,
	                                                       iter);

	context = gtk_widget_get_style_context (completion->priv->tree_view_proposals);

	if (isheader)
	{
		gtk_style_context_get_background_color (context,
		                                        GTK_STATE_FLAG_INSENSITIVE,
		                                        &color);

		g_object_set (cell,
		              "cell-background-rgba", &color,
		              "text", NULL,
		              NULL);
	}
	else
	{
		gtk_style_context_get_color (context,
		                             GTK_STATE_FLAG_INSENSITIVE,
		                             &color);

		gint accel = iter_accelerator (completion, iter);
		gchar *text = NULL;

		if (accel != -1)
		{
			text = g_strdup_printf ("<small><b>%d</b></small>",
						accel == 9 ? 0 : accel + 1);
		}

		g_object_set (cell,
			      "foreground-rgba", &color,
			      "cell-background-set", FALSE,
			      "markup", text,
			      NULL);
		g_free (text);
	}
}

static gboolean
selection_func (GtkTreeSelection    *selection,
                GtkTreeModel        *model,
                GtkTreePath         *path,
                gboolean             path_currently_selected,
                GtkSourceCompletion *completion)
{
	GtkTreeIter iter;

	gtk_tree_model_get_iter (model, &iter, path);

	if (gtk_source_completion_model_iter_is_header (completion->priv->model_proposals,
	                                                &iter))
	{
		return path_currently_selected;
	}
	else
	{
		return TRUE;
	}
}

static void
on_providers_changed (GtkSourceCompletionModel *model,
                      GtkSourceCompletion      *completion)
{
	update_selection_label (completion);
}

static void
info_button_style_updated (GtkWidget           *button,
                           GtkSourceCompletion *completion)
{
	GtkStyleContext *context;
	gint spacing;
	GtkSettings *settings;
	gboolean show_image;

	context = gtk_widget_get_style_context (button);

	gtk_style_context_get_style (context,
	                             "image-spacing", &spacing,
	                             NULL);

	gtk_box_set_spacing (GTK_BOX (completion->priv->hbox_info), spacing);

	settings = gtk_widget_get_settings (button);
	g_object_get (settings,
	              "gtk-button-images", &show_image,
	              NULL);

	gtk_widget_set_visible (completion->priv->image_info, show_image);
}

static void
on_begin_delete (GtkSourceCompletionModel *model,
                 GtkSourceCompletion      *completion)
{
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

	g_signal_handlers_block_by_func (completion->priv->model_proposals,
	                                 G_CALLBACK (on_row_deleted_cb),
	                                 completion);

	g_signal_handlers_block_by_func (selection,
	                                 G_CALLBACK (selection_changed_cb),
	                                 completion);
}

static void
on_end_delete (GtkSourceCompletionModel *model,
                 GtkSourceCompletion      *completion)
{
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

	g_signal_handlers_unblock_by_func (completion->priv->model_proposals,
	                                   G_CALLBACK (on_row_deleted_cb),
	                                   completion);

	g_signal_handlers_unblock_by_func (selection,
	                                   G_CALLBACK (selection_changed_cb),
	                                   completion);

	check_first_selected (completion);
}


static void
initialize_ui (GtkSourceCompletion *completion)
{
	GtkBuilder *builder;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
	GtkWidget *toggle_button_info;

	builder = gtk_builder_new ();

	gtk_builder_add_from_string (builder,
	                             gtk_source_completion_ui,
	                             -1,
	                             NULL);

	completion->priv->window =
		GTK_WIDGET (gtk_builder_get_object (builder,
	                                            "window_completion"));
	completion->priv->info_button =
		GTK_WIDGET (gtk_builder_get_object (builder,
	                                            "toggle_button_info"));
	completion->priv->selection_label =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "label_selection"));
	completion->priv->selection_image =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "image_selection"));
	completion->priv->tree_view_proposals =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "tree_view_completion"));
	completion->priv->label_info =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "label_info"));
	completion->priv->image_info =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "image_info"));
	completion->priv->hbox_info =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "hbox_info"));

	info_button_style_updated (completion->priv->info_button, completion);

	/* Tree view and model */
	completion->priv->model_proposals = gtk_source_completion_model_new ();
	gtk_source_completion_model_set_show_headers (completion->priv->model_proposals,
				                      completion->priv->show_headers);


	gtk_tree_view_set_model (GTK_TREE_VIEW (completion->priv->tree_view_proposals),
	                         GTK_TREE_MODEL (completion->priv->model_proposals));

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));
	gtk_tree_selection_set_select_function (selection,
	                                        (GtkTreeSelectionFunc)selection_func,
	                                        completion,
	                                        NULL);

	column = GTK_TREE_VIEW_COLUMN (gtk_builder_get_object (builder,
	                                                       "tree_view_column_proposal"));

	completion->priv->cell_renderer_icon = GTK_CELL_RENDERER (gtk_builder_get_object (builder,
	                                                                                  "cell_renderer_icon"));

	gtk_tree_view_column_set_cell_data_func (column,
	                                         completion->priv->cell_renderer_icon,
	                                         (GtkTreeCellDataFunc)render_proposal_icon_func,
	                                         completion,
	                                         NULL);

	gtk_tree_view_column_set_cell_data_func (column,
	                                         GTK_CELL_RENDERER (gtk_builder_get_object (builder,
	                                                                                    "cell_renderer_proposal")),
	                                         (GtkTreeCellDataFunc)render_proposal_text_func,
	                                         completion,
	                                         NULL);

	completion->priv->tree_view_column_accelerator =
		GTK_TREE_VIEW_COLUMN (gtk_builder_get_object (builder,
	                                                      "tree_view_column_accelerator"));

	completion->priv->cell_renderer_accelerator =
		GTK_CELL_RENDERER (gtk_builder_get_object (builder,
		                                           "cell_renderer_accelerator"));

	gtk_tree_view_column_set_visible (completion->priv->tree_view_column_accelerator,
	                                  completion->priv->num_accelerators > 0);

	gtk_tree_view_column_set_cell_data_func (completion->priv->tree_view_column_accelerator,
	                                         completion->priv->cell_renderer_accelerator,
	                                         (GtkTreeCellDataFunc)render_proposal_accelerator_func,
	                                         completion,
	                                         NULL);

	g_signal_connect_after (completion->priv->model_proposals,
	                        "row-inserted",
	                        G_CALLBACK (on_row_inserted_cb),
	                        completion);

	g_signal_connect_after (completion->priv->model_proposals,
	                        "row-deleted",
	                        G_CALLBACK (on_row_deleted_cb),
	                        completion);

	g_signal_connect_after (completion->priv->model_proposals,
	                        "begin-delete",
	                        G_CALLBACK (on_begin_delete),
	                        completion);

	g_signal_connect_after (completion->priv->model_proposals,
	                        "end-delete",
	                        G_CALLBACK (on_end_delete),
	                        completion);

	g_signal_connect (completion->priv->model_proposals,
	                  "providers-changed",
	                  G_CALLBACK (on_providers_changed),
	                  completion);

	g_signal_connect (completion->priv->tree_view_proposals,
			  "row-activated",
			  G_CALLBACK (row_activated_cb),
			  completion);

	g_signal_connect (selection,
			  "changed",
			  G_CALLBACK (selection_changed_cb),
			  completion);

	toggle_button_info =
		GTK_WIDGET (gtk_builder_get_object (builder,
		                                    "toggle_button_info"));

	g_signal_connect (toggle_button_info,
			  "toggled",
			  G_CALLBACK (info_toggled_cb),
			  completion);

	g_signal_connect (toggle_button_info,
			  "style-updated",
			  G_CALLBACK (info_button_style_updated),
			  completion);

	g_object_unref (builder);

	/* Info window */
	completion->priv->info_window = GTK_WIDGET (gtk_source_completion_info_new ());

	g_signal_connect (completion->priv->window,
	                  "notify::transient-for",
	                  G_CALLBACK (update_transient_for_info),
	                  completion);

	/* Default info widget */
	completion->priv->default_info = gtk_label_new (NULL);

	gtk_misc_set_alignment (GTK_MISC (completion->priv->default_info), 0.5, 0.5);
	gtk_label_set_selectable (GTK_LABEL (completion->priv->default_info), TRUE);
	gtk_label_set_line_wrap (GTK_LABEL (completion->priv->default_info), TRUE);

	gtk_widget_show (completion->priv->default_info);

	gtk_source_completion_info_set_widget (GTK_SOURCE_COMPLETION_INFO (completion->priv->info_window),
	                                       completion->priv->default_info);

	/* Connect signals */
	g_signal_connect_after (completion->priv->window,
				"configure-event",
				G_CALLBACK (gtk_source_completion_configure_event),
				completion);

	g_signal_connect_after (completion->priv->tree_view_proposals,
				"size-allocate",
				G_CALLBACK (gtk_source_completion_size_allocate),
				completion);

	g_signal_connect_after (completion->priv->tree_view_proposals,
				"style-updated",
				G_CALLBACK (gtk_source_completion_style_updated),
				completion);

	g_signal_connect (completion->priv->window,
			  "delete-event",
			  G_CALLBACK (gtk_widget_hide_on_delete),
			  NULL);

	g_signal_connect (completion->priv->info_window,
			  "before-show",
			  G_CALLBACK (show_info_cb),
			  completion);

	g_signal_connect (completion->priv->info_window,
			  "show",
			  G_CALLBACK (show_info_after_cb),
			  completion);

	g_signal_connect (completion->priv->info_window,
	                  "size-allocate",
	                  G_CALLBACK(info_size_allocate_cb),
	                  completion);

	gtk_widget_set_size_request (completion->priv->window,
	                             WINDOW_WIDTH,
	                             WINDOW_HEIGHT);
}

static void
gtk_source_completion_init (GtkSourceCompletion *completion)
{
	completion->priv = GTK_SOURCE_COMPLETION_GET_PRIVATE (completion);
	initialize_ui (completion);
}

static void
update_completion (GtkSourceCompletion        *completion,
                   GList                      *providers,
                   GtkSourceCompletionContext *context)
{
	GList *item;

	DEBUG({
		g_print ("Update completion: %d\n", g_list_length (providers));
	});

	update_typing_offsets (completion);

	if (gtk_widget_get_visible (completion->priv->info_window))
	{
		/* Move info window accordingly */
		update_info_position (completion);
	}

	/* Make sure to first cancel any running completion */
	cancel_completion (completion, context);

	completion->priv->running_providers = g_list_copy (providers);

	if (completion->priv->active_providers != providers)
	{
		g_list_free (completion->priv->active_providers);
		completion->priv->active_providers = g_list_copy (providers);
	}

	completion->priv->select_first =
		completion->priv->select_on_show &&
		(!get_selected_proposal (completion, NULL, NULL, NULL) || completion->priv->select_first);

	gtk_source_completion_model_begin (completion->priv->model_proposals,
	                                   completion->priv->active_providers);

	for (item = providers; item != NULL; item = g_list_next (item))
	{
		GtkSourceCompletionProvider *provider =
			GTK_SOURCE_COMPLETION_PROVIDER (item->data);

		DEBUG({
			gchar *temp_name = gtk_source_completion_provider_get_name (provider);
			g_print ("Populating provider: %s\n", temp_name);
			g_free (temp_name);
		});

		gtk_source_completion_provider_populate (provider, context);
	}
}

static void
populating_done (GtkSourceCompletion        *completion,
                 GtkSourceCompletionContext *context)
{
	if (gtk_source_completion_model_is_empty (completion->priv->model_proposals,
	                                          FALSE))
	{
		DEBUG({
			g_print ("Model is empty after populating\n");
		});

		/* No completion made, make sure to hide the window */
		gtk_source_completion_hide (completion);

		/* If the window is not visible, the completion was not really
		   cancelled */
		cancel_completion (completion, NULL);
	}
	else
	{
		update_selection_label (completion);

		if (completion->priv->select_on_show)
		{
			/* CHECK: does this actually work? */
			GtkTreeSelection *selection;

			selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (completion->priv->tree_view_proposals));

			if (gtk_tree_selection_count_selected_rows (selection) == 0)
			{
				GtkTreePath *path = gtk_tree_path_new_first ();
				gtk_tree_selection_select_path (selection, path);
				gtk_tree_path_free (path);
			}
		}
	}
}

void
_gtk_source_completion_add_proposals (GtkSourceCompletion         *completion,
                                      GtkSourceCompletionContext  *context,
                                      GtkSourceCompletionProvider *provider,
                                      GList                       *proposals,
                                      gboolean                     finished)
{
	GList *item;

	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (completion));
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_CONTEXT (context));
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_PROVIDER (provider));
	g_return_if_fail (completion->priv->context == context);

	item = g_list_find (completion->priv->running_providers, provider);
	g_return_if_fail (item != NULL);

	gtk_source_completion_model_append (completion->priv->model_proposals,
	                                    provider,
	                                    proposals);

	if (finished)
	{
		/* Let the model know this provider is done */
		gtk_source_completion_model_end (completion->priv->model_proposals,
		                                 provider);

		/* Remove provider from list of running providers */
		completion->priv->running_providers =
			g_list_delete_link (completion->priv->running_providers,
			                    item);

		if (completion->priv->running_providers == NULL)
		{
			populating_done (completion, context);
		}
	}
}

/**
 * gtk_source_completion_show:
 * @completion: a #GtkSourceCompletion.
 * @providers: (element-type GtkSource.CompletionProvider) (allow-none):
 * a list of #GtkSourceCompletionProvider, or %NULL.
 * @context: (transfer full): The #GtkSourceCompletionContext
 * with which to start the completion.
 *
 * Starts a new completion with the specified #GtkSourceCompletionContext and
 * a list of potential candidate providers for completion.
 *
 * Returns: %TRUE if it was possible to the show completion window.
 */
gboolean
gtk_source_completion_show (GtkSourceCompletion        *completion,
                            GList                      *providers,
                            GtkSourceCompletionContext *context)
{
	GList *selected_providers;

	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION (completion), FALSE);
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_CONTEXT (context), FALSE);

	/* Make sure to clear any active completion */
	DEBUG({
		g_print ("Hiding completion before new one\n");
	});

	gtk_source_completion_hide (completion);

	/* We need to take owenership of the context right before doing
	   anything so we don't leak it or get a crash emitting the signal */
	g_object_ref_sink (context);

	if (providers == NULL)
	{
		g_object_unref (context);

		return FALSE;
	}

	/* Populate the context */
	g_signal_emit (completion, signals[POPULATE_CONTEXT], 0, context);

	/* From the providers, select the ones that match the context */
	selected_providers = select_providers (completion, providers, context);

	if (selected_providers == NULL)
	{
		g_object_unref (context);

		DEBUG({
			g_print ("No providers for completion\n");
		});

		gtk_source_completion_hide (completion);
		return FALSE;
	}

	update_completion (completion, selected_providers, context);
	g_list_free (selected_providers);

	return TRUE;
}

/**
 * gtk_source_completion_get_providers:
 * @completion: a #GtkSourceCompletion.
 *
 * Get list of providers registered on @completion. The returned list is owned
 * by the completion and should not be freed.
 *
 * Returns: (element-type GtkSource.CompletionProvider) (transfer none):
 * list of #GtkSourceCompletionProvider.
 */
GList *
gtk_source_completion_get_providers (GtkSourceCompletion *completion)
{
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION (completion), NULL);
	return completion->priv->providers;
}

GQuark
gtk_source_completion_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark) == 0)
	{
		quark = g_quark_from_static_string ("gtk-source-completion-error-quark");
	}

	return quark;
}

/**
 * gtk_source_completion_new:
 * @view: a #GtkSourceView.
 *
 * Creates a new #GtkSourceCompletion associated with @view.
 *
 * Returns: a new #GtkSourceCompletion.
 */
GtkSourceCompletion *
gtk_source_completion_new (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), NULL);

	return g_object_new (GTK_SOURCE_TYPE_COMPLETION,
	                     "view", view,
	                     NULL);
}

/**
 * gtk_source_completion_add_provider:
 * @completion: a #GtkSourceCompletion.
 * @provider: a #GtkSourceCompletionProvider.
 * @error: (allow-none): a #GError.
 *
 * Add a new #GtkSourceCompletionProvider to the completion object. This will
 * add a reference @provider, so make sure to unref your own copy when you
 * no longer need it.
 *
 * Returns: %TRUE if @provider was successfully added, otherwise if @error
 *          is provided, it will be set with the error and %FALSE is returned.
 */
gboolean
gtk_source_completion_add_provider (GtkSourceCompletion          *completion,
				    GtkSourceCompletionProvider  *provider,
				    GError                      **error)
{
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION (completion), FALSE);
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_PROVIDER (provider), FALSE);

	if (g_list_find (completion->priv->providers, provider) != NULL)
	{
		if (error)
		{
			g_set_error (error,
			             GTK_SOURCE_COMPLETION_ERROR,
			             GTK_SOURCE_COMPLETION_ERROR_ALREADY_BOUND,
			             "Provider is already bound to this completion object");
		}

		return FALSE;
	}

	completion->priv->providers = g_list_append (completion->priv->providers,
	                                             g_object_ref (provider));

	if (gtk_source_completion_provider_get_activation (provider) &
	    GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE)
	{
		gint delay = gtk_source_completion_provider_get_interactive_delay (provider);

		completion->priv->interactive_providers =
				g_list_append (completion->priv->interactive_providers,
		                               provider);

		if (delay >= 0 && delay < completion->priv->min_auto_complete_delay)
		{
			completion->priv->min_auto_complete_delay = delay;
		}
		else if (delay < 0 && completion->priv->auto_complete_delay <
		                      completion->priv->min_auto_complete_delay)
		{
			completion->priv->min_auto_complete_delay = completion->priv->auto_complete_delay;
		}
	}

	if (error)
	{
		*error = NULL;
	}

	return TRUE;
}

/**
 * gtk_source_completion_remove_provider:
 * @completion: a #GtkSourceCompletion.
 * @provider: a #GtkSourceCompletionProvider.
 * @error: (allow-none): a #GError.
 *
 * Remove @provider from the completion.
 *
 * Returns: %TRUE if @provider was successfully removed, otherwise if @error
 *          is provided, it will be set with the error and %FALSE is returned.
 **/
gboolean
gtk_source_completion_remove_provider (GtkSourceCompletion          *completion,
				       GtkSourceCompletionProvider  *provider,
				       GError                      **error)
{
	GList *item;

	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION (completion), FALSE);
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_PROVIDER (provider), FALSE);

	item = g_list_find (completion->priv->providers, provider);

	if (item != NULL)
	{
		completion->priv->providers = g_list_remove_link (completion->priv->providers, item);

		if (gtk_source_completion_provider_get_activation (provider) &
		    GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE)
		{
			gint delay = gtk_source_completion_provider_get_interactive_delay (provider);

			completion->priv->interactive_providers =
					g_list_remove (completion->priv->interactive_providers,
			                               provider);

			if (delay == completion->priv->min_auto_complete_delay ||
			    (delay == -1 && completion->priv->min_auto_complete_delay ==
			                    completion->priv->auto_complete_delay))
			{
				update_min_auto_complete_delay (completion);
			}
		}

		g_object_unref (provider);

		if (error)
		{
			*error = NULL;
		}

		return TRUE;
	}
	else
	{
		if (error)
		{
			g_set_error (error,
			             GTK_SOURCE_COMPLETION_ERROR,
			             GTK_SOURCE_COMPLETION_ERROR_NOT_BOUND,
			             "Provider is not bound to this completion object");
		}

		return FALSE;
	}
}

/**
 * gtk_source_completion_hide:
 * @completion: a #GtkSourceCompletion.
 *
 * Hides the completion if it is active (visible).
 */
void
gtk_source_completion_hide (GtkSourceCompletion *completion)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (completion));

	DEBUG({
			g_print ("Emitting hide\n");
	});

	g_signal_emit (completion, signals[HIDE], 0);
}

/**
 * gtk_source_completion_get_info_window:
 * @completion: a #GtkSourceCompletion.
 *
 * The info widget is the window where the completion displays optional extra
 * information of the proposal.
 *
 * Returns: (transfer none): The #GtkSourceCompletionInfo window
 *                           associated with @completion.
 */
GtkSourceCompletionInfo *
gtk_source_completion_get_info_window (GtkSourceCompletion *completion)
{
	return GTK_SOURCE_COMPLETION_INFO (completion->priv->info_window);
}

/**
 * gtk_source_completion_get_view:
 * @completion: a #GtkSourceCompletion.
 *
 * The #GtkSourceView associated with @completion.
 *
 * Returns: (type GtkSource.View) (transfer none):
 * The #GtkSourceView associated with @completion.
 */
GtkSourceView *
gtk_source_completion_get_view (GtkSourceCompletion *completion)
{
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION (completion), NULL);

	return completion->priv->view;
}

/**
 * gtk_source_completion_create_context:
 * @completion: a #GtkSourceCompletion.
 * @position: (allow-none): a #GtkTextIter, or %NULL.
 *
 * Create a new #GtkSourceCompletionContext for @completion. The position at
 * which the completion using the new context will consider completion can
 * be provider by @position. If @position is %NULL, the current cursor
 * position will be used.
 *
 * Returns: (transfer full): a new #GtkSourceCompletionContext.
 * The reference being returned is a 'floating' reference,
 * so if you invoke #gtk_source_completion_show with this context
 * you don't need to unref it.
 */
GtkSourceCompletionContext *
gtk_source_completion_create_context (GtkSourceCompletion *completion,
                                      GtkTextIter         *position)
{
	GtkTextIter iter;

	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION (completion), NULL);

	if (position == NULL)
	{
		get_iter_at_insert (completion, &iter);
	}
	else
	{
		iter = *position;
	}

	return _gtk_source_completion_context_new (completion, &iter);
}

/**
 * gtk_source_completion_move_window:
 * @completion: a #GtkSourceCompletion.
 * @iter: a #GtkTextIter.
 *
 * Move the completion window to a specific iter.
 */
void
gtk_source_completion_move_window (GtkSourceCompletion *completion,
                                   GtkTextIter         *iter)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (completion));
	g_return_if_fail (iter != NULL);

	if (!gtk_widget_get_visible (completion->priv->window))
	{
		return;
	}

	gtk_source_completion_utils_move_to_iter (GTK_WINDOW (completion->priv->window),
	                                          GTK_SOURCE_VIEW (completion->priv->view),
	                                          iter);
}

/**
 * gtk_source_completion_block_interactive:
 * @completion: a #GtkSourceCompletion.
 *
 * Block interactive completion. This can be used to disable interactive
 * completion when inserting or deleting text from the buffer associated with
 * the completion. Use #gtk_source_completion_unblock_interactive to enable
 * interactive completion again.
 **/
void
gtk_source_completion_block_interactive (GtkSourceCompletion *completion)
{
	GtkSourceBuffer *buffer;

	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (completion));

	buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view)));
	completion_begin_block (completion, buffer);
}

/**
 * gtk_source_completion_unblock_interactive:
 * @completion: a #GtkSourceCompletion.
 *
 * Unblock interactive completion. This can be used after using
 * #gtk_source_completion_block_interactive to enable interactive completion
 * again.
 **/
void
gtk_source_completion_unblock_interactive (GtkSourceCompletion *completion)
{
	GtkSourceBuffer *buffer;

	g_return_if_fail (GTK_SOURCE_IS_COMPLETION (completion));

	buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (completion->priv->view)));
	completion_end_block (completion, buffer);
}
