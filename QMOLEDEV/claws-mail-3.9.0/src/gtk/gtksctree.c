/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Parts of this file:
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * Parts of this file from gtk/gtkctree.c and gtk/gtkclist.c:
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball, Josh MacDonald, 
 * Copyright (C) 1997-1998 Jay Painter <jpaint@serv.net><jpaint@gimp.org>  
 *
 * Parts of this file from gtkflist.c:
 * Copyright (C) 1999 The Free Software Foundation
 * Author: Federico Mena <federico@nuclecu.unam.mx>
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

#include <stdlib.h>

#include "gtksctree.h"
#include "claws-marshal.h"
#include "prefs_common.h"
#include "utils.h"
#include "gtkutils.h"

#define CLIST_UNFROZEN(clist)     (((GtkCMCList*) (clist))->freeze_count == 0)
#define CLIST_REFRESH(clist)    G_STMT_START { \
  if (CLIST_UNFROZEN (clist)) \
    GTK_CMCLIST_GET_CLASS (clist)->refresh ((GtkCMCList*) (clist)); \
} G_STMT_END
#define CELL_SPACING               1
#define CLIST_OPTIMUM_SIZE         64
#define COLUMN_INSET               3
#define PM_SIZE                    8
#define TAB_SIZE                   (PM_SIZE + 6)
#define ROW_TOP_YPIXEL(clist, row) (((clist)->row_height * (row)) + \
				    (((row) + 1) * CELL_SPACING) + \
				    (clist)->voffset)
#define ROW_FROM_YPIXEL(clist, y)  (((y) - (clist)->voffset) / \
                                    ((clist)->row_height + CELL_SPACING))
#define COLUMN_LEFT_XPIXEL(clist, col)  ((clist)->column[(col)].area.x \
                                    + (clist)->hoffset)
#define COLUMN_LEFT(clist, column) ((clist)->column[(column)].area.x)

enum {
	ROW_POPUP_MENU,
	EMPTY_POPUP_MENU,
	OPEN_ROW,
	START_DRAG,
	LAST_SIGNAL
};

static void gtk_sctree_class_init (GtkSCTreeClass *class);
static void gtk_sctree_init (GtkSCTree *sctree);

static gint gtk_sctree_button_press (GtkWidget *widget, GdkEventButton *event);
static gint gtk_sctree_button_release (GtkWidget *widget, GdkEventButton *event);
static gint gtk_sctree_motion (GtkWidget *widget, GdkEventMotion *event);
static void gtk_sctree_drag_begin (GtkWidget *widget, GdkDragContext *context);
static void gtk_sctree_drag_end (GtkWidget *widget, GdkDragContext *context);
static void gtk_sctree_drag_data_get (GtkWidget *widget, GdkDragContext *context,
				     GtkSelectionData *data, guint info, guint time);
static void gtk_sctree_drag_leave (GtkWidget *widget, GdkDragContext *context, guint time);
static gboolean gtk_sctree_drag_motion (GtkWidget *widget, GdkDragContext *context,
				       gint x, gint y, guint time);
static gboolean gtk_sctree_drag_drop (GtkWidget *widget, GdkDragContext *context,
				     gint x, gint y, guint time);
static void gtk_sctree_drag_data_received (GtkWidget *widget, GdkDragContext *context,
					  gint x, gint y, GtkSelectionData *data,
					  guint info, guint time);

static void gtk_sctree_clear (GtkCMCList *clist);
static void gtk_sctree_real_unselect_all (GtkCMCList *clist);
       
static void stree_sort (GtkCMCTree *ctree, GtkCMCTreeNode  *node, gpointer data);
void gtk_sctree_sort_node (GtkCMCTree *ctree, GtkCMCTreeNode *node);
void gtk_sctree_sort_recursive (GtkCMCTree *ctree, GtkCMCTreeNode *node);

static void gtk_sctree_link (GtkCMCTree *ctree,
			GtkCMCTreeNode  *node,
			GtkCMCTreeNode  *parent,
			GtkCMCTreeNode  *sibling,
			gboolean       update_focus_row);

static void gtk_sctree_unlink (GtkCMCTree      *ctree, 
			GtkCMCTreeNode  *node,
			gboolean       update_focus_row);

static void stree_update_level (GtkCMCTree      *ctree, 
			GtkCMCTreeNode  *node, 
			gpointer       data);

static GtkCMCTreeNode * gtk_sctree_last_visible (GtkCMCTree     *ctree,
					      GtkCMCTreeNode *node);
static void gtk_sctree_real_tree_expand            (GtkCMCTree      *ctree,
						 GtkCMCTreeNode  *node);
static void gtk_sctree_real_tree_collapse          (GtkCMCTree      *ctree,
						 GtkCMCTreeNode  *node);
static void
sreal_tree_move (GtkCMCTree     *ctree,
		GtkCMCTreeNode *node,
		GtkCMCTreeNode *new_parent, 
		GtkCMCTreeNode *new_sibling);

static GtkCMCTreeClass *parent_class;

static guint sctree_signals[LAST_SIGNAL];

/**
 * gtk_sctree_get_type:
 * @void: 
 * 
 * Creates the GtkSCTree class and its type information
 * 
 * Return value: The type ID for GtkSCTreeClass
 **/
GType
gtk_sctree_get_type (void)
{
	static GType sctree_type = 0;

	if (!sctree_type) {
		GTypeInfo sctree_info = {
			sizeof (GtkSCTreeClass),

			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,

			(GClassInitFunc) gtk_sctree_class_init,
			(GClassFinalizeFunc) NULL,
			NULL,	/* class_data */

			sizeof (GtkSCTree),
			0,	/* n_preallocs */
			(GInstanceInitFunc) gtk_sctree_init,
		};

		sctree_type = g_type_register_static (GTK_TYPE_CMCTREE, "GtkSCTree", &sctree_info, (GTypeFlags)0);
	}

	return sctree_type;
}

static void
gtk_sctree_change_focus_row_expansion (GtkCMCTree          *ctree,
			    GtkCMCTreeExpansionType action)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *node;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  if (gdk_display_pointer_is_grabbed (gtk_widget_get_display (GTK_WIDGET (ctree))) && 
      gtk_widget_has_grab (GTK_WIDGET(ctree)))
    return;
  
  if (!(node =
	GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row))) ||
      GTK_CMCTREE_ROW (node)->is_leaf || !(GTK_CMCTREE_ROW (node)->children))
    return;

  switch (action)
    {
    case GTK_CMCTREE_EXPANSION_EXPAND:
      if (GTK_SCTREE(ctree)->always_expand_recursively)
	      gtk_cmctree_expand_recursive (ctree, node);
      else
	      gtk_cmctree_expand (ctree, node);

      break;
    case GTK_CMCTREE_EXPANSION_EXPAND_RECURSIVE:
      gtk_cmctree_expand_recursive (ctree, node);
      break;
    case GTK_CMCTREE_EXPANSION_COLLAPSE:
      gtk_cmctree_collapse (ctree, node);
      break;
    case GTK_CMCTREE_EXPANSION_COLLAPSE_RECURSIVE:
      gtk_cmctree_collapse_recursive (ctree, node);
      break;
    case GTK_CMCTREE_EXPANSION_TOGGLE:
      if (GTK_SCTREE(ctree)->always_expand_recursively)
	      gtk_cmctree_toggle_expansion_recursive (ctree, node);
      else
	      gtk_cmctree_toggle_expansion (ctree, node);
      break;
    case GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE:
      gtk_cmctree_toggle_expansion_recursive (ctree, node);
      break;
    }
}

static void gtk_sctree_finalize(GObject *object)
{
	GtkSCTree *sctree = GTK_SCTREE(object);
	g_free(sctree->use_markup);
	sctree->use_markup = NULL;
	G_OBJECT_CLASS (parent_class)->finalize (object);
}

/* Standard class initialization function */
static void
gtk_sctree_class_init (GtkSCTreeClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GtkWidgetClass *widget_class;
	GtkCMCListClass *clist_class;
	GtkCMCTreeClass *ctree_class;

	widget_class = (GtkWidgetClass *) klass;
	clist_class = (GtkCMCListClass *) klass;
	ctree_class = (GtkCMCTreeClass *) klass;

	parent_class = g_type_class_peek (gtk_cmctree_get_type ());

	sctree_signals[ROW_POPUP_MENU] =
		g_signal_new ("row_popup_menu",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GtkSCTreeClass, row_popup_menu),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      GDK_TYPE_EVENT);
	sctree_signals[EMPTY_POPUP_MENU] =
		g_signal_new ("empty_popup_menu",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GtkSCTreeClass, empty_popup_menu),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      GDK_TYPE_EVENT);
	sctree_signals[OPEN_ROW] =
		g_signal_new ("open_row",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GtkSCTreeClass, open_row),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);
	sctree_signals[START_DRAG] =
		g_signal_new ("start_drag",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GtkSCTreeClass, start_drag),
			      NULL, NULL,
			      claws_marshal_VOID__INT_POINTER,
			      G_TYPE_NONE, 2,
			      G_TYPE_INT,
			      GDK_TYPE_EVENT);

	/* gtk_object_class_add_signals (object_class, sctree_signals, LAST_SIGNAL); */

	clist_class->clear = gtk_sctree_clear;
	clist_class->unselect_all = gtk_sctree_real_unselect_all;
        ctree_class->tree_collapse = gtk_sctree_real_tree_collapse;
	ctree_class->tree_expand = gtk_sctree_real_tree_expand;
	ctree_class->tree_move = sreal_tree_move;
	ctree_class->change_focus_row_expansion = gtk_sctree_change_focus_row_expansion;
	
	widget_class->button_press_event = gtk_sctree_button_press;
	widget_class->button_release_event = gtk_sctree_button_release;
	widget_class->motion_notify_event = gtk_sctree_motion;
	widget_class->drag_begin = gtk_sctree_drag_begin;
	widget_class->drag_end = gtk_sctree_drag_end;
	widget_class->drag_data_get = gtk_sctree_drag_data_get;
	widget_class->drag_leave = gtk_sctree_drag_leave;
	widget_class->drag_motion = gtk_sctree_drag_motion;
	widget_class->drag_drop = gtk_sctree_drag_drop;
	widget_class->drag_data_received = gtk_sctree_drag_data_received;
	
	gobject_class->finalize = gtk_sctree_finalize;
}

/* Standard object initialization function */
static void
gtk_sctree_init (GtkSCTree *sctree)
{
	sctree->anchor_row = NULL;

	/* GtkCMCTree does not specify pointer motion by default */
	gtk_widget_add_events (GTK_WIDGET (sctree), GDK_POINTER_MOTION_MASK);
	gtk_widget_add_events (GTK_WIDGET (sctree), GDK_POINTER_MOTION_MASK);
}

/* Get information the specified row is selected. */

static gboolean
row_is_selected(GtkSCTree *sctree, gint row)
{
	GtkCMCListRow *clist_row;
	clist_row =  g_list_nth (GTK_CMCLIST(sctree)->row_list, row)->data;
	return clist_row ? clist_row->state == GTK_STATE_SELECTED : FALSE;
}

/* Selects the rows between the anchor to the specified row, inclusive.  */
static void
select_range (GtkSCTree *sctree, gint row)
{
	gint prev_row;
	gint min, max;
	gint i;
	GList *node;
	if (sctree->anchor_row == NULL) {
		prev_row = row;
		sctree->anchor_row = gtk_cmctree_node_nth(GTK_CMCTREE(sctree), row);
	} else
		prev_row = g_list_position(GTK_CMCLIST(sctree)->row_list,
					   (GList *)sctree->anchor_row);

	if (row < prev_row) {
		min = row;
		max = prev_row;
		GTK_CMCLIST(sctree)->focus_row = max;
	} else {
		min = prev_row;
		max = row;
	}
	sctree->selecting_range++;
	
	if (max < min) {
		int t = min;
		min = max;
		max = t;
	}
	
	if (max - min > 10)
		gtk_cmclist_freeze(GTK_CMCLIST(sctree));

	node = g_list_nth((GTK_CMCLIST(sctree))->row_list, min);
	for (i = min; i < max; i++) {
		if (node && GTK_CMCTREE_ROW (node)->row.selectable) {
			g_signal_emit_by_name(G_OBJECT(sctree), "tree_select_row",
				node, -1);
		}
		node = node->next;
	}
	if (max - min > 10)
		gtk_cmclist_thaw(GTK_CMCLIST(sctree));


	sctree->selecting_range--;
	gtk_cmclist_select_row (GTK_CMCLIST (sctree), max, -1);
}

/* Handles row selection according to the specified modifier state */
/* in certain cases, we arrive here from a function knowing the GtkCMCTreeNode, and having
 * already slowly found row using g_list_position. In which case, _node will be non-NULL
 * to avoid this function having to slowly find it with g_list_nth. */
static void
select_row (GtkSCTree *sctree, gint row, gint col, guint state, GtkCMCTreeNode *_node)
{
	gboolean range, additive;
	cm_return_if_fail (sctree != NULL);
	cm_return_if_fail (GTK_IS_SCTREE (sctree));
    
	range = ((state & GDK_SHIFT_MASK) != 0) &&
		(GTK_CMCLIST(sctree)->selection_mode != GTK_SELECTION_SINGLE) &&
		(GTK_CMCLIST(sctree)->selection_mode != GTK_SELECTION_BROWSE);
	additive = ((state & GDK_CONTROL_MASK) != 0) &&
		   (GTK_CMCLIST(sctree)->selection_mode != GTK_SELECTION_SINGLE) &&
		   (GTK_CMCLIST(sctree)->selection_mode != GTK_SELECTION_BROWSE);

	if (!range && !additive && sctree->force_additive_sel)
		additive = TRUE;

	GTK_CMCLIST(sctree)->focus_row = row;

	if (!additive) {
		gtk_cmclist_unselect_all (GTK_CMCLIST (sctree));
	}

	if (!range) {
		GtkCMCTreeNode *node;

		node = _node ? _node : gtk_cmctree_node_nth (GTK_CMCTREE(sctree), row);

		/*No need to manage overlapped list*/
		if (additive) {
			if (row_is_selected(sctree, row))
				gtk_cmclist_unselect_row (GTK_CMCLIST (sctree), row, col);
			else
				g_signal_emit_by_name
					(G_OBJECT (sctree),
					 "tree_select_row", node, col);
		} else {
			g_signal_emit_by_name
				(G_OBJECT (sctree),
				 "tree_select_row", node, col);
		}
		sctree->anchor_row = node;
	} else
		select_range (sctree, row);
}

static gboolean
sctree_is_hot_spot (GtkSCTree     *sctree, 
		   GtkCMCTreeNode *node,
		   gint          row, 
		   gint          x, 
		   gint          y)
{
  GtkCMCTreeRow *tree_row;
  GtkCMCList *clist;
  GtkCMCTree *ctree;
  gint xl, xmax;
  gint yu;
  gint hotspot_size;
  
  cm_return_val_if_fail (GTK_IS_SCTREE (sctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);

  clist = GTK_CMCLIST (sctree);
  ctree = GTK_CMCTREE (sctree);

  if (!clist->column[ctree->tree_column].visible ||
      ctree->expander_style == GTK_CMCTREE_EXPANDER_NONE)
    return FALSE;

  tree_row = GTK_CMCTREE_ROW (node);

  hotspot_size = clist->row_height-2;
  if (hotspot_size > clist->column[ctree->tree_column].area.width - 2)
	hotspot_size = clist->column[ctree->tree_column].area.width - 2;

  if (!GTK_CMCLIST_ROW_HEIGHT_SET(GTK_CMCLIST(clist)))
     yu = (ROW_TOP_YPIXEL (clist, row) + (clist->row_height - hotspot_size) / 2 -
	(clist->row_height - 1) % 2);
  else
     yu = (ROW_TOP_YPIXEL (clist, row) + (clist->row_height/2 - hotspot_size) / 2 -
	(clist->row_height/2 - 1) % 2);

#ifndef GENERIC_UMPC
  if (clist->column[ctree->tree_column].justification == GTK_JUSTIFY_RIGHT)
    xl = clist->column[ctree->tree_column].area.x + 
	  clist->column[ctree->tree_column].area.width - 1 + clist->hoffset -
	  (tree_row->level - 1) * ctree->tree_indent - hotspot_size;
  else
    xl = clist->column[ctree->tree_column].area.x + clist->hoffset +
	  (tree_row->level - 1) * ctree->tree_indent;

  xmax = xl + hotspot_size;
#else
  if (clist->column[ctree->tree_column].justification == GTK_JUSTIFY_RIGHT) {
    xl = clist->column[ctree->tree_column].area.x + 
	  clist->column[ctree->tree_column].area.width - 1 + clist->hoffset -
	  (tree_row->level - 1) * ctree->tree_indent - hotspot_size;
    xmax = xl + hotspot_size;
  } else if (ctree->tree_column == 0) {
    xl = clist->column[ctree->tree_column].area.x + clist->hoffset;
    xmax = clist->column[ctree->tree_column].area.x + clist->hoffset +
	   (tree_row->level - 1) * ctree->tree_indent +
	   hotspot_size;
  } else {
    xl = clist->column[ctree->tree_column].area.x + clist->hoffset +
	  (tree_row->level - 1) * ctree->tree_indent;
    xmax = xl + hotspot_size;
  }
#endif
  return (x >= xl && x <= xmax && y >= yu && y <= yu + hotspot_size);
}

gboolean
gtk_sctree_is_hot_spot (GtkSCTree *ctree, 
		       gint      x, 
		       gint      y)
{
  GtkCMCTreeNode *node;
  gint column;
  gint row;
  
  cm_return_val_if_fail (GTK_IS_SCTREE (ctree), FALSE);

  if (gtk_cmclist_get_selection_info (GTK_CMCLIST (ctree), x, y, &row, &column))
    if ((node = GTK_CMCTREE_NODE(g_list_nth (GTK_CMCLIST (ctree)->row_list, row))))
      return sctree_is_hot_spot (ctree, node, row, x, y);

  return FALSE;
}

/* Our handler for button_press events.  We override all of GtkCMCList's broken
 * behavior.
 */
static gint
gtk_sctree_button_press (GtkWidget *widget, GdkEventButton *event)
{
	GtkSCTree *sctree;
	GtkCMCList *clist;
	gboolean on_row;
	gint row;
	gint col;
	gint retval;

	cm_return_val_if_fail (widget != NULL, FALSE);
	cm_return_val_if_fail (GTK_IS_SCTREE (widget), FALSE);
	cm_return_val_if_fail (event != NULL, FALSE);

	sctree = GTK_SCTREE (widget);
	clist = GTK_CMCLIST (widget);
	retval = FALSE;

	if (event->window != clist->clist_window)
		return (* GTK_WIDGET_CLASS (parent_class)->button_press_event) (widget, event);

	on_row = gtk_cmclist_get_selection_info (clist, event->x, event->y, &row, &col);

	if (on_row && !gtk_widget_has_focus(widget))
		gtk_widget_grab_focus (widget);

	if (gtk_sctree_is_hot_spot (GTK_SCTREE(sctree), event->x, event->y)) {
		GtkCMCTreeNode *node = gtk_cmctree_node_nth(GTK_CMCTREE(sctree), row);
		if (GTK_CMCTREE_ROW (node)->expanded)
			gtk_cmctree_collapse(GTK_CMCTREE(sctree), node);
		else if (GTK_SCTREE(sctree)->always_expand_recursively)
			gtk_cmctree_expand_recursive (GTK_CMCTREE(sctree), node);
		else
			gtk_cmctree_expand(GTK_CMCTREE(sctree), node);
		return TRUE;
	}

	switch (event->type) {
	case GDK_BUTTON_PRESS:
		if (event->button == 1 || event->button == 2) {
			if (event->button == 2)
				event->state &= ~(GDK_SHIFT_MASK | GDK_CONTROL_MASK);
			if (on_row) {
				/* Save the mouse info for DnD */
				sctree->dnd_press_button = event->button;
				sctree->dnd_press_x = event->x;
				sctree->dnd_press_y = event->y;

				/* Handle selection */
				if ((row_is_selected (sctree, row)
				     && !(event->state & (GDK_CONTROL_MASK | GDK_SHIFT_MASK)))
				    || ((event->state & GDK_CONTROL_MASK)
					&& !(event->state & GDK_SHIFT_MASK))) {
					sctree->dnd_select_pending = TRUE;
					sctree->dnd_select_pending_state = event->state;
					sctree->dnd_select_pending_row = row;
				} else {
					select_row (sctree, row, col, event->state, NULL);
				}
			} else {
				gtk_cmclist_unselect_all (clist);
			}

			retval = TRUE;
		} else if (event->button == 3) {
			/* Emit *_popup_menu signal*/
			if (on_row) {
				if (!row_is_selected(sctree,row))
					select_row (sctree, row, col, 0, NULL);
				g_signal_emit (G_OBJECT (sctree),
						 sctree_signals[ROW_POPUP_MENU],
						 0, event);
			} else {
				gtk_cmclist_unselect_all(clist);
				g_signal_emit (G_OBJECT (sctree),
						 sctree_signals[EMPTY_POPUP_MENU],
						 0, event);
			}
			retval = TRUE;
		}

		break;

	case GDK_2BUTTON_PRESS:
		if (event->button != 1)
			break;

		sctree->dnd_select_pending = FALSE;
		sctree->dnd_select_pending_state = 0;

		if (on_row)
			g_signal_emit (G_OBJECT (sctree),
				       sctree_signals[OPEN_ROW], 0);

		retval = TRUE;
		break;

	default:
		break;
	}

	return retval;
}

/* Our handler for button_release events.  We override all of GtkCMCList's broken
 * behavior.
 */
static gint
gtk_sctree_button_release (GtkWidget *widget, GdkEventButton *event)
{
	GtkSCTree *sctree;
	GtkCMCList *clist;
	gint on_row;
	gint row, col;
	gint retval;

	cm_return_val_if_fail (widget != NULL, FALSE);
	cm_return_val_if_fail (GTK_IS_SCTREE (widget), FALSE);
	cm_return_val_if_fail (event != NULL, FALSE);

	sctree = GTK_SCTREE (widget);
	clist = GTK_CMCLIST (widget);
	retval = FALSE;

	if (event->window != clist->clist_window)
		return (* GTK_WIDGET_CLASS (parent_class)->button_release_event) (widget, event);

	on_row = gtk_cmclist_get_selection_info (clist, event->x, event->y, &row, &col);

	if (!(event->button == 1 || event->button == 2))
		return FALSE;

	sctree->dnd_press_button = 0;
	sctree->dnd_press_x = 0;
	sctree->dnd_press_y = 0;

	if (on_row) {
		if (sctree->dnd_select_pending) {
			select_row (sctree, row, col, sctree->dnd_select_pending_state, NULL);
			sctree->dnd_select_pending = FALSE;
			sctree->dnd_select_pending_state = 0;
		}

		retval = TRUE;
	}

	return retval;
}

/* Our handler for motion_notify events.  We override all of GtkCMCList's broken
 * behavior.
 */
static gint
gtk_sctree_motion (GtkWidget *widget, GdkEventMotion *event)
{
	GtkSCTree *sctree;
	GtkCMCList *clist;

	cm_return_val_if_fail (widget != NULL, FALSE);
	cm_return_val_if_fail (GTK_IS_SCTREE (widget), FALSE);
	cm_return_val_if_fail (event != NULL, FALSE);

	sctree = GTK_SCTREE (widget);
	clist = GTK_CMCLIST (widget);

	if (event->window != clist->clist_window)
		return (* GTK_WIDGET_CLASS (parent_class)->motion_notify_event) (widget, event);

	if (!((sctree->dnd_press_button == 1 && (event->state & GDK_BUTTON1_MASK))
	      || (sctree->dnd_press_button == 2 && (event->state & GDK_BUTTON2_MASK))))
		return FALSE;

	/* This is the same threshold value that is used in gtkdnd.c */

#ifndef GENERIC_UMPC
#define THRESHOLD 3
#else
#define THRESHOLD 8
#endif
	if (MAX (ABS (sctree->dnd_press_x - event->x),
		 ABS (sctree->dnd_press_y - event->y)) <= THRESHOLD)
		return FALSE;

	/* Handle any pending selections */

	if (sctree->dnd_select_pending) {
		if (!row_is_selected(sctree,sctree->dnd_select_pending_row))
			select_row (sctree,
				    sctree->dnd_select_pending_row,
				    -1,
				    sctree->dnd_select_pending_state,
				    NULL);

		sctree->dnd_select_pending = FALSE;
		sctree->dnd_select_pending_state = 0;
	}

	g_signal_emit (G_OBJECT (sctree),
		       sctree_signals[START_DRAG],
		       0,
		       sctree->dnd_press_button,
		       event);
	return TRUE;
}

/* We override the drag_begin signal to do nothing */
static void
gtk_sctree_drag_begin (GtkWidget *widget, GdkDragContext *context)
{
	/* nothing */
}

/* We override the drag_end signal to do nothing */
static void
gtk_sctree_drag_end (GtkWidget *widget, GdkDragContext *context)
{
	/* nothing */
}

/* We override the drag_data_get signal to do nothing */
static void
gtk_sctree_drag_data_get (GtkWidget *widget, GdkDragContext *context,
				     GtkSelectionData *data, guint info, guint time)
{
	/* nothing */
}

/* We override the drag_leave signal to do nothing */
static void
gtk_sctree_drag_leave (GtkWidget *widget, GdkDragContext *context, guint time)
{
	/* nothing */
}

/* We override the drag_motion signal to do nothing */
static gboolean
gtk_sctree_drag_motion (GtkWidget *widget, GdkDragContext *context,
				   gint x, gint y, guint time)
{
	return FALSE;
}

/* We override the drag_drop signal to do nothing */
static gboolean
gtk_sctree_drag_drop (GtkWidget *widget, GdkDragContext *context,
				 gint x, gint y, guint time)
{
	return FALSE;
}

/* We override the drag_data_received signal to do nothing */
static void
gtk_sctree_drag_data_received (GtkWidget *widget, GdkDragContext *context,
					  gint x, gint y, GtkSelectionData *data,
					  guint info, guint time)
{
	/* nothing */
}

/* Our handler for the clear signal of the clist.  We have to reset the anchor
 * to null.
 */
static void
gtk_sctree_clear (GtkCMCList *clist)
{
	GtkSCTree *sctree;

	cm_return_if_fail (clist != NULL);
	cm_return_if_fail (GTK_IS_SCTREE (clist));

	sctree = GTK_SCTREE (clist);
	sctree->anchor_row = NULL;

	if (((GtkCMCListClass *)parent_class)->clear)
		(* ((GtkCMCListClass *)parent_class)->clear) (clist);
}

static void
gtk_sctree_real_unselect_all (GtkCMCList *clist)
{
	GtkSCTree *sctree;
	gboolean should_freeze = FALSE;

	cm_return_if_fail (clist != NULL);
	cm_return_if_fail (GTK_IS_SCTREE (clist));

	sctree = GTK_SCTREE (clist);

	if (sc_g_list_bigger(GTK_CMCLIST(sctree)->selection, 10)) {
		should_freeze = TRUE;
		sctree->selecting_range++;
		gtk_cmclist_freeze (GTK_CMCLIST (sctree));
	}

	if (((GtkCMCListClass *)parent_class)->unselect_all)
		(* ((GtkCMCListClass *)parent_class)->unselect_all) (clist);

	if (should_freeze) {
		gtk_cmclist_thaw (GTK_CMCLIST (sctree));
		sctree->selecting_range--;
	}
}

static void
gtk_sctree_column_auto_resize (GtkCMCList    *clist,
		    GtkCMCListRow *clist_row,
		    gint         column,
		    gint         old_width)
{
  /* resize column if needed for auto_resize */
  GtkRequisition requisition;

  if (!clist->column[column].auto_resize ||
      GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    return;

  if (clist_row)
    GTK_CMCLIST_GET_CLASS (clist)->cell_size_request (clist, clist_row,
						   column, &requisition);
  else
    requisition.width = 0;

  if (requisition.width > clist->column[column].width)
    gtk_cmclist_set_column_width (clist, column, requisition.width);
  else if (requisition.width < old_width &&
	   old_width == clist->column[column].width)
    {
      GList *list;
      GtkRequisition button_req;
      gint new_width;

      /* run a "gtk_cmclist_optimal_column_width" but break, if
       * the column doesn't shrink */
      if (GTK_CMCLIST_SHOW_TITLES (clist) && clist->column[column].button)
        {
	gtk_widget_get_requisition (clist->column[column].button, &button_req);
	new_width = (button_req.width -
		     (CELL_SPACING + (2 * COLUMN_INSET)));
        }
      else
	new_width = 0;

      for (list = clist->row_list; list; list = list->next)
	{
	  GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
	    (clist, GTK_CMCLIST_ROW (list), column, &requisition);
	  new_width = MAX (new_width, requisition.width);
	  if (new_width == clist->column[column].width)
	    break;
	}
      if (new_width < clist->column[column].width)
	gtk_cmclist_set_column_width (clist, column, new_width);
    }
}

static void
gtk_sctree_auto_resize_columns (GtkCMCList *clist)
{
  gint i;

  if (GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    return;

  for (i = 0; i < clist->columns; i++)
    gtk_sctree_column_auto_resize (clist, NULL, i, clist->column[i].width);
}

static void 
gtk_sctree_real_tree_collapse (GtkCMCTree     *ctree,
		    GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *work;
  GtkRequisition requisition;
  gboolean visible;
  gint level;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (!node || !GTK_CMCTREE_ROW (node)->expanded ||
      GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  clist = GTK_CMCLIST (ctree);

  GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
  
  GTK_CMCTREE_ROW (node)->expanded = FALSE;
  level = GTK_CMCTREE_ROW (node)->level;

  visible = gtk_cmctree_is_viewable (ctree, node);
  /* get cell width if tree_column is auto resized */
  if (visible && clist->column[ctree->tree_column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
      (clist, &GTK_CMCTREE_ROW (node)->row, ctree->tree_column, &requisition);

  /* unref/unset opened pixbuf */
  if (GTK_CMCELL_PIXTEXT 
      (GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf)
    {
      g_object_unref
	(GTK_CMCELL_PIXTEXT
	 (GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf);
      
      GTK_CMCELL_PIXTEXT
	(GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf = NULL;
    }

  /* set/ref closed pixbuf */
  if (GTK_CMCTREE_ROW (node)->pixbuf_closed)
    {
      GTK_CMCELL_PIXTEXT 
	(GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf = 
	g_object_ref (GTK_CMCTREE_ROW (node)->pixbuf_closed);
    }

  work = GTK_CMCTREE_ROW (node)->children;
  if (work)
    {
      gint tmp = 0;
      gint row;
      GList *list;

      while (work && GTK_CMCTREE_ROW (work)->level > level)
	{
	  work = GTK_CMCTREE_NODE_NEXT (work);
	  tmp++;
	}

      if (work)
	{
	  list = (GList *)node;
	  list->next = (GList *)work;
	  list = (GList *)GTK_CMCTREE_NODE_PREV (work);
	  list->next = NULL;
	  list = (GList *)work;
	  list->prev = (GList *)node;
	}
      else
	{
	  list = (GList *)node;
	  list->next = NULL;
	  clist->row_list_end = (GList *)node;
	}

      if (visible)
	{
	  /* resize auto_resize columns if needed */
	  gtk_sctree_auto_resize_columns (clist);

	  if (!GTK_SCTREE(clist)->sorting) {
		  row = g_list_position (clist->row_list, (GList *)node);
		  if (row < clist->focus_row)
		    clist->focus_row -= tmp;
	  }
	  clist->rows -= tmp;
	  CLIST_REFRESH (clist);
	}
    }
  else if (visible && clist->column[ctree->tree_column].auto_resize &&
	   !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    /* resize tree_column if needed */
    gtk_sctree_column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row, ctree->tree_column,
			requisition.width);
    
}


GtkWidget *gtk_sctree_new_with_titles (gint columns, gint tree_column, 
				       gchar *titles[])
{
	GtkWidget *widget;
                                                                                                            
	cm_return_val_if_fail (columns > 0, NULL);
	cm_return_val_if_fail (tree_column >= 0, NULL);
	
	if (tree_column >= columns) {
		g_warning("Wrong tree column");
		tree_column = 0;
		print_backtrace();
	}
	
	widget = gtk_widget_new (TYPE_GTK_SCTREE,
				 "n_columns", columns,
				 "tree_column", tree_column,
				 NULL);
	if (titles) {
		GtkCMCList *clist = GTK_CMCLIST (widget);
		guint i;

		for (i = 0; i < columns; i++)
			gtk_cmclist_set_column_title (clist, i, titles[i]);
		gtk_cmclist_column_titles_show (clist);
	}

	GTK_SCTREE(widget)->show_stripes = TRUE;
	GTK_SCTREE(widget)->always_expand_recursively = TRUE;
	GTK_SCTREE(widget)->force_additive_sel = FALSE;
	
	GTK_SCTREE(widget)->use_markup = g_new0(gboolean, columns);

	return widget;
}

void gtk_sctree_set_use_markup		    (GtkSCTree		*sctree,
					     int		 column,
					     gboolean		 markup)
{
	gint columns = 0;
	GValue value = { 0 };
	
	cm_return_if_fail(GTK_IS_SCTREE(sctree));

	g_value_init (&value, G_TYPE_INT);	
	g_object_get_property (G_OBJECT (sctree), "n-columns", &value);
	columns = g_value_get_int (&value);
	g_value_unset (&value);

	cm_return_if_fail(column < columns);

	sctree->use_markup[column] = markup;
}

void gtk_sctree_select (GtkSCTree *sctree, GtkCMCTreeNode *node)
{
	select_row(sctree, 
		   g_list_position(GTK_CMCLIST(sctree)->row_list, (GList *)node),
		   -1, 0, node);
}

void gtk_sctree_select_with_state (GtkSCTree *sctree, GtkCMCTreeNode *node, int state)
{
	select_row(sctree, 
		   g_list_position(GTK_CMCLIST(sctree)->row_list, (GList *)node),
		   -1, state, node);
}

void gtk_sctree_unselect_all (GtkSCTree *sctree)
{
	gtk_cmclist_unselect_all(GTK_CMCLIST(sctree));
	sctree->anchor_row = NULL;
}

void gtk_sctree_set_anchor_row (GtkSCTree *sctree, GtkCMCTreeNode *node)
{
	sctree->anchor_row = node;
}

void gtk_sctree_remove_node (GtkSCTree *sctree, GtkCMCTreeNode *node)
{
	if (sctree->anchor_row == node)
		sctree->anchor_row = NULL;
	gtk_cmctree_remove_node(GTK_CMCTREE(sctree), node);
}

void gtk_sctree_set_stripes(GtkSCTree  *sctree, gboolean show_stripes)
{
	sctree->show_stripes = show_stripes;
}

void gtk_sctree_set_recursive_expand(GtkSCTree  *sctree, gboolean rec_exp)
{
	sctree->always_expand_recursively = rec_exp;
}

/***********************************************************
 *             Tree sorting functions                      *
 ***********************************************************/

static void sink(GtkCMCList *clist, GPtrArray *numbers, gint root, gint bottom)
{
	gint j, k ;
	GtkCMCTreeNode *temp;

	j = 2 * root;
	k = j + 1;

	/* find the maximum element of numbers[root],
	   numbers[2*root] and numbers[2*root+1] */
	if (j <= bottom) {
		if (clist->compare( clist, GTK_CMCTREE_ROW (g_ptr_array_index(numbers, root)),
				    GTK_CMCTREE_ROW(g_ptr_array_index( numbers, j))) >= 0)
			j = root;
		if (k <= bottom)
			if (clist->compare( clist, GTK_CMCTREE_ROW (g_ptr_array_index(numbers, k)),
					    GTK_CMCTREE_ROW (g_ptr_array_index( numbers, j))) > 0)
				j = k;
		/* if numbers[root] wasn't the maximum element then
		   sink again */
		if (root != j) {
			temp = g_ptr_array_index( numbers,root);
			g_ptr_array_index( numbers, root) = g_ptr_array_index( numbers, j);
			g_ptr_array_index( numbers, j) = temp;
			sink( clist, numbers, j, bottom);
		}
	}
}

static void heap_sort(GtkCMCList *clist, GPtrArray *numbers, gint array_size)
{
	gint i;
	GtkCMCTreeNode *temp;
	
	/* build the Heap */
	for (i = (array_size / 2); i >= 1; i--)
		sink( clist, numbers, i, array_size);
	/* output the Heap */
	for (i = array_size; i >= 2; i--) {
		temp = g_ptr_array_index( numbers, 1);
		g_ptr_array_index( numbers, 1) = g_ptr_array_index( numbers, i);
		g_ptr_array_index( numbers, i) = temp;
		sink( clist, numbers, 1, i-1);
	}
}

static void
stree_sort (GtkCMCTree    *ctree,
	   GtkCMCTreeNode *node,
	   gpointer      data)
{
	GtkCMCTreeNode *list_start, *work, *next;
	GPtrArray *row_array, *viewable_array;
	GtkCMCList *clist;
	gint i;

	clist = GTK_CMCLIST (ctree);

	if (node)
		work = GTK_CMCTREE_ROW (node)->children;
	else
		work = GTK_CMCTREE_NODE (clist->row_list);

	row_array = g_ptr_array_new();
	viewable_array = g_ptr_array_new();

	if (work) {
		g_ptr_array_add( row_array, NULL);
		while (work) {
			/* add all rows to row_array */
			g_ptr_array_add( row_array, work);
			if (GTK_CMCTREE_ROW (work)->parent && gtk_cmctree_is_viewable( ctree, work))
				g_ptr_array_add( viewable_array, GTK_CMCTREE_ROW (work)->parent);
			next = GTK_CMCTREE_ROW (work)->sibling;
			gtk_sctree_unlink( ctree, work, FALSE);
			work = next;
		}

		heap_sort( clist, row_array, (row_array->len)-1);

		if (node)
			list_start = GTK_CMCTREE_ROW (node)->children;
		else
			list_start = GTK_CMCTREE_NODE (clist->row_list);

		if (clist->sort_type == GTK_SORT_ASCENDING) {
			for (i=(row_array->len)-1; i>=1; i--) {
				work = g_ptr_array_index( row_array, i);
				gtk_sctree_link( ctree, work, node, list_start, FALSE);
				list_start = work;
				/* insert work at the beginning of the list */
			}
		} else {
			for (i=1; i<row_array->len; i++) {
				work = g_ptr_array_index( row_array, i);
				gtk_sctree_link( ctree, work, node, list_start, FALSE);
				list_start = work;
				/* insert work at the beginning of the list */
			}
		}

		for (i=0; i<viewable_array->len; i++) {
			gtk_cmctree_expand( ctree, g_ptr_array_index( viewable_array, i));
		}
		
	}
	g_ptr_array_free( row_array, TRUE);
	g_ptr_array_free( viewable_array, TRUE);
}

void
gtk_sctree_sort_recursive (GtkCMCTree     *ctree, 
			  GtkCMCTreeNode *node)
{
	GtkCMCList *clist;
	GtkCMCTreeNode *focus_node = NULL;

	cm_return_if_fail (ctree != NULL);
	cm_return_if_fail (GTK_IS_CMCTREE (ctree));

	clist = GTK_CMCLIST (ctree);

	gtk_cmclist_freeze (clist);

	if (clist->selection_mode == GTK_SELECTION_EXTENDED) {
		GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
      
		g_list_free (clist->undo_selection);
		g_list_free (clist->undo_unselection);
		clist->undo_selection = NULL;
		clist->undo_unselection = NULL;
	}

	if (!node || (node && gtk_cmctree_is_viewable (ctree, node)))
		focus_node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row));
      
	GTK_SCTREE(ctree)->sorting = TRUE;

	gtk_cmctree_post_recursive (ctree, node, GTK_CMCTREE_FUNC (stree_sort), NULL);

	if (!node)
		stree_sort (ctree, NULL, NULL);

	GTK_SCTREE(ctree)->sorting = FALSE;

	if (focus_node) {
		clist->focus_row = g_list_position (clist->row_list,(GList *)focus_node);
		clist->undo_anchor = clist->focus_row;
	}

	gtk_cmclist_thaw (clist);
}

void
gtk_sctree_sort_node (GtkCMCTree     *ctree, 
		     GtkCMCTreeNode *node)
{
	GtkCMCList *clist;
	GtkCMCTreeNode *focus_node = NULL;

	cm_return_if_fail (ctree != NULL);
	cm_return_if_fail (GTK_IS_CMCTREE (ctree));

	clist = GTK_CMCLIST (ctree);

	gtk_cmclist_freeze (clist);

	if (clist->selection_mode == GTK_SELECTION_EXTENDED) {
		GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);

		g_list_free (clist->undo_selection);
		g_list_free (clist->undo_unselection);
		clist->undo_selection = NULL;
		clist->undo_unselection = NULL;
	}

	if (!node || (node && gtk_cmctree_is_viewable (ctree, node)))
		focus_node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row));

	GTK_SCTREE(ctree)->sorting = TRUE;

	stree_sort (ctree, node, NULL);

	GTK_SCTREE(ctree)->sorting = FALSE;

	if (focus_node) {
		clist->focus_row = g_list_position (clist->row_list,(GList *)focus_node);
		clist->undo_anchor = clist->focus_row;
	}

	gtk_cmclist_thaw (clist);
}

/************************************************************************/

static void
gtk_sctree_unlink (GtkCMCTree     *ctree, 
		  GtkCMCTreeNode *node,
                  gboolean      update_focus_row)
{
	GtkCMCList *clist;
	gint rows;
	gint level;
	gint visible;
	GtkCMCTreeNode *work;
	GtkCMCTreeNode *parent;
	GList *list;

	cm_return_if_fail (ctree != NULL);
	cm_return_if_fail (GTK_IS_CMCTREE (ctree));
	cm_return_if_fail (node != NULL);

	clist = GTK_CMCLIST (ctree);
  
	if (update_focus_row && clist->selection_mode == GTK_SELECTION_EXTENDED) {
		GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);

		g_list_free (clist->undo_selection);
		g_list_free (clist->undo_unselection);
		clist->undo_selection = NULL;
		clist->undo_unselection = NULL;
	}

	visible = gtk_cmctree_is_viewable (ctree, node);

	/* clist->row_list_end unlinked ? */
	if (visible && (GTK_CMCTREE_NODE_NEXT (node) == NULL ||
	   (GTK_CMCTREE_ROW (node)->children && gtk_cmctree_is_ancestor (ctree, node,
	    GTK_CMCTREE_NODE (clist->row_list_end)))))
		clist->row_list_end = (GList *) (GTK_CMCTREE_NODE_PREV (node));

	/* update list */
	rows = 0;
	level = GTK_CMCTREE_ROW (node)->level;
	work = GTK_CMCTREE_NODE_NEXT (node);
	while (work && GTK_CMCTREE_ROW (work)->level > level) {
		work = GTK_CMCTREE_NODE_NEXT (work);
		rows++;
	}

	if (visible) {
		clist->rows -= (rows + 1);

		if (update_focus_row) {
			gint pos;
			pos = g_list_position (clist->row_list, (GList *)node);
			if (pos + rows < clist->focus_row)
				clist->focus_row -= (rows + 1);
			else if (pos <= clist->focus_row) {
				if (!GTK_CMCTREE_ROW (node)->sibling)
					clist->focus_row = MAX (pos - 1, 0);
				else
					clist->focus_row = pos;
	      
				clist->focus_row = MIN (clist->focus_row, clist->rows - 1);
			}
			clist->undo_anchor = clist->focus_row;
		}
	}

	if (work) {
		list = (GList *)GTK_CMCTREE_NODE_PREV (work);
		list->next = NULL;
		list = (GList *)work;
		list->prev = (GList *)GTK_CMCTREE_NODE_PREV (node);
	}

	if (GTK_CMCTREE_NODE_PREV (node) &&
	    GTK_CMCTREE_NODE_NEXT (GTK_CMCTREE_NODE_PREV (node)) == node) {
		list = (GList *)GTK_CMCTREE_NODE_PREV (node);
		list->next = (GList *)work;
	}

	/* update tree */
	parent = GTK_CMCTREE_ROW (node)->parent;
	if (parent) {
		if (GTK_CMCTREE_ROW (parent)->children == node) {
			GTK_CMCTREE_ROW (parent)->children = GTK_CMCTREE_ROW (node)->sibling;
		}
		else {
			GtkCMCTreeNode *sibling;

			sibling = GTK_CMCTREE_ROW (parent)->children;
			while (GTK_CMCTREE_ROW (sibling)->sibling != node)
				sibling = GTK_CMCTREE_ROW (sibling)->sibling;
			GTK_CMCTREE_ROW (sibling)->sibling = GTK_CMCTREE_ROW (node)->sibling;
		}
	}
	else {
		if (clist->row_list == (GList *)node)
			clist->row_list = (GList *) (GTK_CMCTREE_ROW (node)->sibling);
		else {
			GtkCMCTreeNode *sibling;

			sibling = GTK_CMCTREE_NODE (clist->row_list);
			while (GTK_CMCTREE_ROW (sibling)->sibling != node)
				sibling = GTK_CMCTREE_ROW (sibling)->sibling;
			GTK_CMCTREE_ROW (sibling)->sibling = GTK_CMCTREE_ROW (node)->sibling;
		}
	}
}

static void
gtk_sctree_link (GtkCMCTree     *ctree,
		GtkCMCTreeNode *node,
		GtkCMCTreeNode *parent,
		GtkCMCTreeNode *sibling,
		gboolean      update_focus_row)
{
	GtkCMCList *clist;
	GList *list_end;
	GList *list;
	GList *work;
	gboolean visible = FALSE;
	gint rows = 0;
  
	if (sibling)
		cm_return_if_fail (GTK_CMCTREE_ROW (sibling)->parent == parent);
	cm_return_if_fail (node != NULL);
	cm_return_if_fail (node != sibling);
	cm_return_if_fail (node != parent);

	clist = GTK_CMCLIST (ctree);

	if (update_focus_row && clist->selection_mode == GTK_SELECTION_EXTENDED) {
		GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);

		g_list_free (clist->undo_selection);
		g_list_free (clist->undo_unselection);
		clist->undo_selection = NULL;
		clist->undo_unselection = NULL;
	}

	for (rows = 1, list_end = (GList *)node; list_end->next;
	     list_end = list_end->next)
		rows++;

	GTK_CMCTREE_ROW (node)->parent = parent;
	GTK_CMCTREE_ROW (node)->sibling = sibling;

	if (!parent || (parent && (gtk_cmctree_is_viewable (ctree, parent) &&
	    GTK_CMCTREE_ROW (parent)->expanded))) {
		visible = TRUE;
		clist->rows += rows;
	}

	if (parent)
		work = (GList *)(GTK_CMCTREE_ROW (parent)->children);
	else
		work = clist->row_list;

	if (sibling) {
		if (work != (GList *)sibling) {
			while (GTK_CMCTREE_ROW (work)->sibling != sibling)
				work = (GList *)(GTK_CMCTREE_ROW (work)->sibling);
			GTK_CMCTREE_ROW (work)->sibling = node;
		}

		if (sibling == GTK_CMCTREE_NODE (clist->row_list))
		clist->row_list = (GList *) node;
		if (GTK_CMCTREE_NODE_PREV (sibling) &&
		    GTK_CMCTREE_NODE_NEXT (GTK_CMCTREE_NODE_PREV (sibling)) == sibling) {
			list = (GList *)GTK_CMCTREE_NODE_PREV (sibling);
			list->next = (GList *)node;
		}

		list = (GList *)node;
		list->prev = (GList *)GTK_CMCTREE_NODE_PREV (sibling);
		list_end->next = (GList *)sibling;
		list = (GList *)sibling;
		list->prev = list_end;
		if (parent && GTK_CMCTREE_ROW (parent)->children == sibling)
			GTK_CMCTREE_ROW (parent)->children = node;
	}
	else {
		if (work) {
			/* find sibling */
			while (GTK_CMCTREE_ROW (work)->sibling)
			work = (GList *)(GTK_CMCTREE_ROW (work)->sibling);
			GTK_CMCTREE_ROW (work)->sibling = node;

			/* find last visible child of sibling */
			work = (GList *) gtk_sctree_last_visible (ctree,
			       GTK_CMCTREE_NODE (work));

			list_end->next = work->next;
			if (work->next)
				list = work->next->prev = list_end;
			work->next = (GList *)node;
			list = (GList *)node;
			list->prev = work;
		}
		else {
			if (parent) {
				GTK_CMCTREE_ROW (parent)->children = node;
				list = (GList *)node;
				list->prev = (GList *)parent;
				if (GTK_CMCTREE_ROW (parent)->expanded) {
					list_end->next = (GList *)GTK_CMCTREE_NODE_NEXT (parent);
					if (GTK_CMCTREE_NODE_NEXT(parent)) {
						list = (GList *)GTK_CMCTREE_NODE_NEXT (parent);
						list->prev = list_end;
					}
					list = (GList *)parent;
					list->next = (GList *)node;
				}
				else
					list_end->next = NULL;
			}
			else {
				clist->row_list = (GList *)node;
				list = (GList *)node;
				list->prev = NULL;
				list_end->next = NULL;
			}
		}
	}

	gtk_cmctree_pre_recursive (ctree, node, stree_update_level, NULL); 

	if (clist->row_list_end == NULL ||
	    clist->row_list_end->next == (GList *)node)
		clist->row_list_end = list_end;

	if (visible && update_focus_row) {
		gint pos;
		pos = g_list_position (clist->row_list, (GList *)node);
  
		if (pos <= clist->focus_row) {
			clist->focus_row += rows;
			clist->undo_anchor = clist->focus_row;
		}
	}
}

static void
stree_update_level (GtkCMCTree     *ctree, 
		   GtkCMCTreeNode *node, 
		   gpointer      data)
{
	if (!node)
		return;

	if (GTK_CMCTREE_ROW (node)->parent)
		GTK_CMCTREE_ROW (node)->level = 
		GTK_CMCTREE_ROW (GTK_CMCTREE_ROW (node)->parent)->level + 1;
	else
		GTK_CMCTREE_ROW (node)->level = 1;
}

static GtkCMCTreeNode *
gtk_sctree_last_visible (GtkCMCTree     *ctree,
			GtkCMCTreeNode *node)
{
	GtkCMCTreeNode *work;
  
	if (!node)
		return NULL;

	work = GTK_CMCTREE_ROW (node)->children;

	if (!work || !GTK_CMCTREE_ROW (node)->expanded)
		return node;

	while (GTK_CMCTREE_ROW (work)->sibling)
		work = GTK_CMCTREE_ROW (work)->sibling;

	return gtk_sctree_last_visible (ctree, work);
}

static void 
sset_node_info (GtkCMCTree     *ctree,
	       GtkCMCTreeNode *node,
	       const gchar  *text,
	       guint8        spacing,
	       GdkPixbuf    *pixbuf_closed,
	       GdkPixbuf    *pixbuf_opened,
	       gboolean      is_leaf,
	       gboolean      expanded)
{
  if (GTK_CMCTREE_ROW (node)->pixbuf_opened)
    {
      g_object_unref (GTK_CMCTREE_ROW (node)->pixbuf_opened);
    }
  if (GTK_CMCTREE_ROW (node)->pixbuf_closed)
    {
      g_object_unref (GTK_CMCTREE_ROW (node)->pixbuf_closed);
    }

  GTK_CMCTREE_ROW (node)->pixbuf_opened = NULL;
  GTK_CMCTREE_ROW (node)->pixbuf_closed = NULL;

  if (pixbuf_closed)
    {
      GTK_CMCTREE_ROW (node)->pixbuf_closed = g_object_ref (pixbuf_closed);
    }
  if (pixbuf_opened)
    {
      GTK_CMCTREE_ROW (node)->pixbuf_opened = g_object_ref (pixbuf_opened);
    }

  GTK_CMCTREE_ROW (node)->is_leaf  = is_leaf;
  GTK_CMCTREE_ROW (node)->expanded = (is_leaf) ? FALSE : expanded;

  if (GTK_CMCTREE_ROW (node)->expanded)
    gtk_cmctree_node_set_pixtext (ctree, node, ctree->tree_column,
				text, spacing, pixbuf_opened);
  else 
    gtk_cmctree_node_set_pixtext (ctree, node, ctree->tree_column,
				text, spacing, pixbuf_closed);
}

static GtkCMCTreeRow *
srow_new (GtkCMCTree *ctree)
{
  GtkCMCList *clist;
  GtkCMCTreeRow *ctree_row;
  int i;

  clist = GTK_CMCLIST (ctree);
#if GLIB_CHECK_VERSION(2,10,0)
  ctree_row = g_slice_new (GtkCMCTreeRow);
  ctree_row->row.cell = g_slice_alloc (sizeof (GtkCMCell) * clist->columns);
#else
  ctree_row = g_chunk_new (GtkCMCTreeRow, (GMemChunk *)clist->row_mem_chunk);
  ctree_row->row.cell = g_chunk_new (GtkCMCell, (GMemChunk *)clist->cell_mem_chunk);
#endif
  for (i = 0; i < clist->columns; i++)
    {
      ctree_row->row.cell[i].type = GTK_CMCELL_EMPTY;
      ctree_row->row.cell[i].vertical = 0;
      ctree_row->row.cell[i].horizontal = 0;
      ctree_row->row.cell[i].style = NULL;
    }

  GTK_CMCELL_PIXTEXT (ctree_row->row.cell[ctree->tree_column])->text = NULL;

  ctree_row->row.fg_set     = FALSE;
  ctree_row->row.bg_set     = FALSE;
  ctree_row->row.style      = NULL;
  ctree_row->row.selectable = TRUE;
  ctree_row->row.state      = GTK_STATE_NORMAL;
  ctree_row->row.data       = NULL;
  ctree_row->row.destroy    = NULL;

  ctree_row->level         = 0;
  ctree_row->expanded      = FALSE;
  ctree_row->parent        = NULL;
  ctree_row->sibling       = NULL;
  ctree_row->children      = NULL;
  ctree_row->pixbuf_closed = NULL;
  ctree_row->pixbuf_opened = NULL;
  
  return ctree_row;
}

static void
srow_delete (GtkCMCTree    *ctree,
	    GtkCMCTreeRow *ctree_row)
{
  GtkCMCList *clist;
  gint i;

  clist = GTK_CMCLIST (ctree);

  for (i = 0; i < clist->columns; i++)
    {
      GTK_CMCLIST_GET_CLASS (clist)->set_cell_contents
	(clist, &(ctree_row->row), i, GTK_CMCELL_EMPTY, NULL, 0, NULL);
      if (ctree_row->row.cell[i].style)
	{
	  if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
	    gtk_style_detach (ctree_row->row.cell[i].style);
	  g_object_unref (ctree_row->row.cell[i].style);
	}
    }

  if (ctree_row->row.style)
    {
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
	gtk_style_detach (ctree_row->row.style);
      g_object_unref (ctree_row->row.style);
    }

  if (ctree_row->pixbuf_closed)
    {
      g_object_unref (ctree_row->pixbuf_closed);
    }

  if (ctree_row->pixbuf_opened)
    {
      g_object_unref (ctree_row->pixbuf_opened);
    }

  if (ctree_row->row.destroy)
    {
      GDestroyNotify dnotify = ctree_row->row.destroy;
      gpointer ddata = ctree_row->row.data;

      ctree_row->row.destroy = NULL;
      ctree_row->row.data = NULL;

      dnotify (ddata);
    }

#if GLIB_CHECK_VERSION(2,10,0)  
  g_slice_free1 (sizeof (GtkCMCell) * clist->columns, ctree_row->row.cell);
  g_slice_free (GtkCMCTreeRow, ctree_row);
#else
  g_mem_chunk_free ((GMemChunk *)clist->cell_mem_chunk, ctree_row->row.cell);
  g_mem_chunk_free ((GMemChunk *)clist->row_mem_chunk, ctree_row);
#endif
}

static void
stree_delete_row (GtkCMCTree     *ctree, 
		 GtkCMCTreeNode *node, 
		 gpointer      data)
{
  srow_delete (ctree, GTK_CMCTREE_ROW (node));
  g_list_free_1 ((GList *)node);
}

static void 
gtk_sctree_real_tree_expand (GtkCMCTree     *ctree,
		  GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *work;
  GtkRequisition requisition;
  gboolean visible;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (!node || GTK_CMCTREE_ROW (node)->expanded || GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  clist = GTK_CMCLIST (ctree);
  
  GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);

  GTK_CMCTREE_ROW (node)->expanded = TRUE;

  visible = gtk_cmctree_is_viewable (ctree, node);
  /* get cell width if tree_column is auto resized */
  if (visible && clist->column[ctree->tree_column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
      (clist, &GTK_CMCTREE_ROW (node)->row, ctree->tree_column, &requisition);

  /* unref/unset closed pixbuf */
  if (GTK_CMCELL_PIXTEXT 
      (GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf)
    {
      g_object_unref
	(GTK_CMCELL_PIXTEXT
	 (GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf);
      
      GTK_CMCELL_PIXTEXT
	(GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf = NULL;
    }

  /* set/ref opened pixbuf */
  if (GTK_CMCTREE_ROW (node)->pixbuf_opened)
    {
      GTK_CMCELL_PIXTEXT 
	(GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->pixbuf = 
	g_object_ref (GTK_CMCTREE_ROW (node)->pixbuf_opened);
    }


  work = GTK_CMCTREE_ROW (node)->children;
  if (work)
    {
      GList *list = (GList *)work;
      gint *cell_width = NULL;
      gint tmp = 0;
      gint row;
      gint i;
      
      if (visible && !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
	{
	  cell_width = g_new0 (gint, clist->columns);
	  if (clist->column[ctree->tree_column].auto_resize)
	      cell_width[ctree->tree_column] = requisition.width;

	  while (work)
	    {
	      /* search maximum cell widths of auto_resize columns */
	      for (i = 0; i < clist->columns; i++)
		if (clist->column[i].auto_resize)
		  {
		    GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
		      (clist, &GTK_CMCTREE_ROW (work)->row, i, &requisition);
		    cell_width[i] = MAX (requisition.width, cell_width[i]);
		  }

	      list = (GList *)work;
	      work = GTK_CMCTREE_NODE_NEXT (work);
	      tmp++;
	    }
	}
      else
	while (work)
	  {
	    list = (GList *)work;
	    work = GTK_CMCTREE_NODE_NEXT (work);
	    tmp++;
	  }

      list->next = (GList *)GTK_CMCTREE_NODE_NEXT (node);

      if (GTK_CMCTREE_NODE_NEXT (node))
	{
	  GList *tmp_list;

	  if (clist->row_list_end == list)
	      clist->row_list_end = g_list_last(list);

	  tmp_list = (GList *)GTK_CMCTREE_NODE_NEXT (node);
	  tmp_list->prev = list;
	}
      else
	clist->row_list_end = list;

      list = (GList *)node;
      list->next = (GList *)(GTK_CMCTREE_ROW (node)->children);

      if (visible && !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
	{
	  /* resize auto_resize columns if needed */
	  for (i = 0; i < clist->columns; i++)
	    if (clist->column[i].auto_resize &&
		cell_width[i] > clist->column[i].width)
	      gtk_cmclist_set_column_width (clist, i, cell_width[i]);
	  g_free (cell_width);
	
	  if (!GTK_SCTREE(ctree)->sorting) {
		  /* update focus_row position */
		  row = g_list_position (clist->row_list, (GList *)node);
		  if (row < clist->focus_row)
		    clist->focus_row += tmp;
	  }
	  clist->rows += tmp;
	  CLIST_REFRESH (clist);
	}
    }
  else if (visible && clist->column[ctree->tree_column].auto_resize)
    /* resize tree_column if needed */
    gtk_sctree_column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row, ctree->tree_column,
			requisition.width);

}

GtkCMCTreeNode * 
gtk_sctree_insert_node (GtkCMCTree     *ctree,
		       GtkCMCTreeNode *parent, 
		       GtkCMCTreeNode *sibling,
		       gchar        *text[],
		       guint8        spacing,
		       GdkPixbuf    *pixbuf_closed,
		       GdkPixbuf    *pixbuf_opened,
		       gboolean      is_leaf,
		       gboolean      expanded)
{
  GtkCMCList *clist;
  GtkCMCTreeRow *new_row;
  GtkCMCTreeNode *node;
  GList *list;
  gint i;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  if (sibling)
    cm_return_val_if_fail (GTK_CMCTREE_ROW (sibling)->parent == parent, NULL);

  if (parent && GTK_CMCTREE_ROW (parent)->is_leaf)
    return NULL;

  clist = GTK_CMCLIST (ctree);

  /* create the row */
  new_row = srow_new (ctree);
  list = g_list_alloc ();
  list->data = new_row;
  node = GTK_CMCTREE_NODE (list);

  if (text)
    for (i = 0; i < clist->columns; i++)
      if (text[i] && i != ctree->tree_column)
	GTK_CMCLIST_GET_CLASS (clist)->set_cell_contents
	  (clist, &(new_row->row), i, GTK_CMCELL_TEXT, text[i], 0, NULL);

  sset_node_info (ctree, node, text ?
		 text[ctree->tree_column] : NULL, spacing, pixbuf_closed,
		 pixbuf_opened, is_leaf, expanded);

  /* sorted insertion */
  if (GTK_CMCLIST_AUTO_SORT (clist))
    {
      if (parent)
	sibling = GTK_CMCTREE_ROW (parent)->children;
      else
	sibling = GTK_CMCTREE_NODE (clist->row_list);

      while (sibling && clist->compare
	     (clist, GTK_CMCTREE_ROW (node), GTK_CMCTREE_ROW (sibling)) > 0)
	sibling = GTK_CMCTREE_ROW (sibling)->sibling;
    }

  gtk_sctree_link (ctree, node, parent, sibling, FALSE);

  if (text && !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist) &&
      gtk_cmctree_is_viewable (ctree, node))
    {
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].auto_resize)
	  gtk_sctree_column_auto_resize (clist, &(new_row->row), i, 0);
    }

  if (clist->rows == 1)
    {
      clist->focus_row = 0;
      if (clist->selection_mode == GTK_SELECTION_BROWSE)
	gtk_sctree_select (GTK_SCTREE(ctree), node);
    }


  CLIST_REFRESH (clist);

  return node;
}

GtkCMCTreeNode *
gtk_sctree_insert_gnode (GtkCMCTree          *ctree,
			GtkCMCTreeNode      *parent,
			GtkCMCTreeNode      *sibling,
			GNode             *gnode,
			GtkCMCTreeGNodeFunc  func,
			gpointer           data)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *cnode = NULL;
  GtkCMCTreeNode *child = NULL;
  GtkCMCTreeNode *new_child;
  GList *list;
  GNode *work;
  guint depth = 1;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  cm_return_val_if_fail (gnode != NULL, NULL);
  cm_return_val_if_fail (func != NULL, NULL);
  if (sibling)
    cm_return_val_if_fail (GTK_CMCTREE_ROW (sibling)->parent == parent, NULL);
  
  clist = GTK_CMCLIST (ctree);

  if (parent)
    depth = GTK_CMCTREE_ROW (parent)->level + 1;

  list = g_list_alloc ();
  list->data = srow_new (ctree);
  cnode = GTK_CMCTREE_NODE (list);

  gtk_cmclist_freeze (clist);

  sset_node_info (ctree, cnode, "", 0, NULL, NULL, TRUE, FALSE);

  if (!func (ctree, depth, gnode, cnode, data))
    {
      stree_delete_row (ctree, cnode, NULL);
      gtk_cmclist_thaw (clist);
      return NULL;
    }

  if (GTK_CMCLIST_AUTO_SORT (clist))
    {
      if (parent)
	sibling = GTK_CMCTREE_ROW (parent)->children;
      else
	sibling = GTK_CMCTREE_NODE (clist->row_list);

      while (sibling && clist->compare
	     (clist, GTK_CMCTREE_ROW (cnode), GTK_CMCTREE_ROW (sibling)) > 0)
	sibling = GTK_CMCTREE_ROW (sibling)->sibling;
    }

  gtk_sctree_link (ctree, cnode, parent, sibling, FALSE);

  for (work = g_node_last_child (gnode); work; work = work->prev)
    {
      new_child = gtk_sctree_insert_gnode (ctree, cnode, child,
					  work, func, data);
      if (new_child)
	child = new_child;
    }	
  
  gtk_cmclist_thaw (clist);

  return cnode;
}

static void
sreal_tree_move (GtkCMCTree     *ctree,
		GtkCMCTreeNode *node,
		GtkCMCTreeNode *new_parent, 
		GtkCMCTreeNode *new_sibling)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *work;
  gboolean visible = FALSE;

  cm_return_if_fail (ctree != NULL);
  cm_return_if_fail (node != NULL);
  cm_return_if_fail (!new_sibling || 
		    GTK_CMCTREE_ROW (new_sibling)->parent == new_parent);

  if (new_parent && GTK_CMCTREE_ROW (new_parent)->is_leaf)
    return;

  /* new_parent != child of child */
  for (work = new_parent; work; work = GTK_CMCTREE_ROW (work)->parent)
    if (work == node)
      return;

  clist = GTK_CMCLIST (ctree);

  visible = gtk_cmctree_is_viewable (ctree, node);

  if (clist->selection_mode == GTK_SELECTION_MULTIPLE)
    {
      GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
      
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
    }

  if (GTK_CMCLIST_AUTO_SORT (clist))
    {
      if (new_parent == GTK_CMCTREE_ROW (node)->parent)
	return;
      
      if (new_parent)
	new_sibling = GTK_CMCTREE_ROW (new_parent)->children;
      else
	new_sibling = GTK_CMCTREE_NODE (clist->row_list);

      while (new_sibling && clist->compare
	     (clist, GTK_CMCTREE_ROW (node), GTK_CMCTREE_ROW (new_sibling)) > 0)
	new_sibling = GTK_CMCTREE_ROW (new_sibling)->sibling;
    }

  if (new_parent == GTK_CMCTREE_ROW (node)->parent && 
      new_sibling == GTK_CMCTREE_ROW (node)->sibling)
    return;

  gtk_cmclist_freeze (clist);

  work = NULL;

  if (!GTK_SCTREE(ctree)->sorting && gtk_cmctree_is_viewable (ctree, node))
    work = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row));
      
  gtk_sctree_unlink (ctree, node, FALSE);
  gtk_sctree_link (ctree, node, new_parent, new_sibling, FALSE);
  
  if (!GTK_SCTREE(ctree)->sorting && work)
    {
      while (work &&  !gtk_cmctree_is_viewable (ctree, work))
	work = GTK_CMCTREE_ROW (work)->parent;
      clist->focus_row = g_list_position (clist->row_list, (GList *)work);
      clist->undo_anchor = clist->focus_row;
    }

  if (clist->column[ctree->tree_column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist) &&
      (visible || gtk_cmctree_is_viewable (ctree, node)))
    gtk_cmclist_set_column_width
      (clist, ctree->tree_column,
       gtk_cmclist_optimal_column_width (clist, ctree->tree_column));

  gtk_cmclist_thaw (clist);
}

void gtk_sctree_set_column_tooltip	    (GtkSCTree		*sctree,
					     int		 column,
					     const gchar 	*tip)
{
#if !(GTK_CHECK_VERSION(2,12,0))
	GtkTooltips *tips;
	if (!sctree->tooltips)
		sctree->tooltips = gtk_tooltips_new();
	tips = sctree->tooltips;
#endif

	CLAWS_SET_TIP(GTK_CMCLIST(sctree)->column[column].button,
			tip);
}

