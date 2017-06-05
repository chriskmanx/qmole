/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball, Josh MacDonald
 * Copyright (C) 1997-1998 Jay Painter <jpaint@serv.net><jpaint@gimp.org>
 *
 * GtkCMCTree widget for GTK+
 * Copyright (C) 1998 Lars Hamann and Stefan Jeske
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __GTK_CMCTREE_H__
#define __GTK_CMCTREE_H__

#include "gtkcmclist.h"

G_BEGIN_DECLS

#define GTK_TYPE_CMCTREE            (gtk_cmctree_get_type ())
#define GTK_CMCTREE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_CMCTREE, GtkCMCTree))
#define GTK_CMCTREE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_CMCTREE, GtkCMCTreeClass))
#define GTK_IS_CMCTREE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_CMCTREE))
#define GTK_IS_CMCTREE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_CMCTREE))
#define GTK_CMCTREE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_CMCTREE, GtkCMCTreeClass))

#define GTK_CMCTREE_ROW(_node_) ((GtkCMCTreeRow *)(((GList *)(_node_))->data))
#define GTK_CMCTREE_NODE(_node_) ((GtkCMCTreeNode *)((_node_)))
#define GTK_CMCTREE_NODE_NEXT(_nnode_) ((GtkCMCTreeNode *)(((GList *)(_nnode_))->next))
#define GTK_CMCTREE_NODE_PREV(_pnode_) ((GtkCMCTreeNode *)(((GList *)(_pnode_))->prev))
#define GTK_CMCTREE_FUNC(_func_) ((GtkCMCTreeFunc)(_func_))

#define GTK_TYPE_CMCTREE_NODE (gtk_cmctree_node_get_type ())

GType gtk_cmctree_pos_get_type (void) G_GNUC_CONST;
#define GTK_TYPE_CMCTREE_POS (gtk_cmctree_pos_get_type())
GType gtk_cmctree_line_style_get_type (void) G_GNUC_CONST;
#define GTK_TYPE_CMCTREE_LINE_STYLE (gtk_cmctree_line_style_get_type())
GType gtk_cmctree_expander_style_get_type (void) G_GNUC_CONST;
#define GTK_TYPE_CMCTREE_EXPANDER_STYLE (gtk_cmctree_expander_style_get_type())
GType gtk_cmctree_expansion_type_get_type (void) G_GNUC_CONST;
#define GTK_TYPE_CMCTREE_EXPANSION_TYPE (gtk_cmctree_expansion_type_get_type())

typedef enum
{
  GTK_CMCTREE_POS_BEFORE,
  GTK_CMCTREE_POS_AS_CHILD,
  GTK_CMCTREE_POS_AFTER
} GtkCMCTreePos;

typedef enum
{
  GTK_CMCTREE_LINES_NONE,
  GTK_CMCTREE_LINES_SOLID,
  GTK_CMCTREE_LINES_DOTTED,
  GTK_CMCTREE_LINES_TABBED
} GtkCMCTreeLineStyle;

typedef enum
{
  GTK_CMCTREE_EXPANDER_NONE,
  GTK_CMCTREE_EXPANDER_SQUARE,
  GTK_CMCTREE_EXPANDER_TRIANGLE,
  GTK_CMCTREE_EXPANDER_CIRCULAR
} GtkCMCTreeExpanderStyle;

typedef enum
{
  GTK_CMCTREE_EXPANSION_EXPAND,
  GTK_CMCTREE_EXPANSION_EXPAND_RECURSIVE,
  GTK_CMCTREE_EXPANSION_COLLAPSE,
  GTK_CMCTREE_EXPANSION_COLLAPSE_RECURSIVE,
  GTK_CMCTREE_EXPANSION_TOGGLE,
  GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE
} GtkCMCTreeExpansionType;

typedef struct _GtkCMCTree      GtkCMCTree;
typedef struct _GtkCMCTreeClass GtkCMCTreeClass;
typedef struct _GtkCMCTreeRow   GtkCMCTreeRow;
typedef struct _GtkCMCTreeNode  GtkCMCTreeNode;

typedef void (*GtkCMCTreeFunc) (GtkCMCTree     *ctree,
			      GtkCMCTreeNode *node,
			      gpointer      data);

typedef gboolean (*GtkCMCTreeGNodeFunc) (GtkCMCTree     *ctree,
                                       guint         depth,
                                       GNode        *gnode,
				       GtkCMCTreeNode *cnode,
                                       gpointer      data);

typedef gboolean (*GtkCMCTreeCompareDragFunc) (GtkCMCTree     *ctree,
                                             GtkCMCTreeNode *source_node,
                                             GtkCMCTreeNode *new_parent,
                                             GtkCMCTreeNode *new_sibling);

struct _GtkCMCTree
{
  GtkCMCList clist;
  
  GdkGC *lines_gc;
  
  gint tree_indent;
  gint tree_spacing;
  gint tree_column;

  guint line_style     : 2;
  guint expander_style : 2;
  guint show_stub      : 1;

  GtkCMCTreeCompareDragFunc drag_compare;
};

struct _GtkCMCTreeClass
{
  GtkCMCListClass parent_class;
  
  void (*tree_select_row)   (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *row,
			     gint          column);
  void (*tree_unselect_row) (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *row,
			     gint          column);
  void (*tree_expand)       (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *node);
  void (*tree_collapse)     (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *node);
  void (*tree_move)         (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *node,
			     GtkCMCTreeNode *new_parent,
			     GtkCMCTreeNode *new_sibling);
  void (*change_focus_row_expansion) (GtkCMCTree *ctree,
				      GtkCMCTreeExpansionType action);
};

struct _GtkCMCTreeRow
{
  GtkCMCListRow row;
  
  GtkCMCTreeNode *parent;
  GtkCMCTreeNode *sibling;
  GtkCMCTreeNode *children;
  
  GdkPixbuf *pixbuf_closed;
  GdkPixbuf *pixbuf_opened;
  
  guint16 level;
  
  guint is_leaf  : 1;
  guint expanded : 1;
};

struct _GtkCMCTreeNode {
  GList list;
};


/***********************************************************
 *           Creation, insertion, deletion                 *
 ***********************************************************/

GType gtk_cmctree_get_type                       (void);
GtkWidget * gtk_cmctree_new_with_titles            (gint          columns, 
						  gint          tree_column,
						  gchar        *titles[]);
GtkWidget * gtk_cmctree_new                        (gint          columns, 
						  gint          tree_column);
GtkCMCTreeNode * gtk_cmctree_insert_node             (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *parent, 
						  GtkCMCTreeNode *sibling,
						  gchar        *text[],
						  guint8        spacing,
						  GdkPixbuf    *pixbuf_closed,
						  GdkPixbuf    *pixbuf_opened,
						  gboolean      is_leaf,
						  gboolean      expanded);
void gtk_cmctree_remove_node                       (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);
GtkCMCTreeNode * gtk_cmctree_insert_gnode            (GtkCMCTree          *ctree,
						  GtkCMCTreeNode      *parent,
						  GtkCMCTreeNode      *sibling,
						  GNode             *gnode,
						  GtkCMCTreeGNodeFunc  func,
						  gpointer           data);
GNode * gtk_cmctree_export_to_gnode                (GtkCMCTree          *ctree,
						  GNode             *parent,
						  GNode             *sibling,
						  GtkCMCTreeNode      *node,
						  GtkCMCTreeGNodeFunc  func,
						  gpointer           data);

/***********************************************************
 *  Generic recursive functions, querying / finding tree   *
 *  information                                            *
 ***********************************************************/

void gtk_cmctree_post_recursive                    (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node,
						  GtkCMCTreeFunc  func,
						  gpointer      data);
void gtk_cmctree_post_recursive_to_depth           (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node,
						  gint          depth,
						  GtkCMCTreeFunc  func,
						  gpointer      data);
void gtk_cmctree_pre_recursive                     (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node,
						  GtkCMCTreeFunc  func,
						  gpointer      data);
void gtk_cmctree_pre_recursive_to_depth            (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node,
						  gint          depth,
						  GtkCMCTreeFunc  func,
						  gpointer      data);
gboolean gtk_cmctree_is_viewable                   (GtkCMCTree     *ctree, 
					          GtkCMCTreeNode *node);
GtkCMCTreeNode * gtk_cmctree_last                    (GtkCMCTree     *ctree,
					          GtkCMCTreeNode *node);
GtkCMCTreeNode * gtk_cmctree_find_node_ptr           (GtkCMCTree     *ctree,
					          GtkCMCTreeRow  *ctree_row);
GtkCMCTreeNode * gtk_cmctree_node_nth                (GtkCMCTree     *ctree,
						  guint         row);
gboolean gtk_cmctree_find                          (GtkCMCTree     *ctree,
					          GtkCMCTreeNode *node,
					          GtkCMCTreeNode *child);
gboolean gtk_cmctree_is_ancestor                   (GtkCMCTree     *ctree,
					          GtkCMCTreeNode *node,
					          GtkCMCTreeNode *child);
GtkCMCTreeNode * gtk_cmctree_find_by_row_data        (GtkCMCTree     *ctree,
					          GtkCMCTreeNode *node,
					          gpointer      data);
/* returns a GList of all GtkCMCTreeNodes with row->data == data. */
GList * gtk_cmctree_find_all_by_row_data           (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gpointer      data);
GtkCMCTreeNode * gtk_cmctree_find_by_row_data_custom (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gpointer      data,
						  GCompareFunc  func);
/* returns a GList of all GtkCMCTreeNodes with row->data == data. */
GList * gtk_cmctree_find_all_by_row_data_custom    (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gpointer      data,
						  GCompareFunc  func);
gboolean gtk_cmctree_is_hot_spot                   (GtkCMCTree     *ctree,
					          gint          x,
					          gint          y);

/***********************************************************
 *   Tree signals : move, expand, collapse, (un)select     *
 ***********************************************************/

void gtk_cmctree_move                              (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  GtkCMCTreeNode *new_parent, 
						  GtkCMCTreeNode *new_sibling);
void gtk_cmctree_expand                            (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_expand_recursive                  (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_expand_to_depth                   (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          depth);
void gtk_cmctree_collapse                          (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_collapse_recursive                (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_collapse_to_depth                 (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          depth);
void gtk_cmctree_toggle_expansion                  (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_toggle_expansion_recursive        (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_select                            (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);
void gtk_cmctree_select_recursive                  (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);
void gtk_cmctree_unselect                          (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);
void gtk_cmctree_unselect_recursive                (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);
void gtk_cmctree_real_select_recursive             (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node, 
						  gint          state);

/***********************************************************
 *           Analogons of GtkCMCList functions               *
 ***********************************************************/

void gtk_cmctree_node_set_text                     (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  const gchar  *text);
void gtk_cmctree_node_set_pixbuf                   (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  GdkPixbuf    *pixbuf);
void gtk_cmctree_node_set_pixtext                  (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  const gchar  *text,
						  guint8        spacing,
						  GdkPixbuf    *pixbuf);
void gtk_cmctree_set_node_info                     (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  const gchar  *text,
						  guint8        spacing,
						  GdkPixbuf    *pixbuf_closed,
						  GdkPixbuf    *pixbuf_opened,
						  gboolean      is_leaf,
						  gboolean      expanded);
void gtk_cmctree_node_set_shift                    (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  gint          vertical,
						  gint          horizontal);
void gtk_cmctree_node_set_selectable               (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gboolean      selectable);
gboolean gtk_cmctree_node_get_selectable           (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
GtkCMCellType gtk_cmctree_node_get_cell_type         (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column);
gboolean gtk_cmctree_node_get_text                 (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  gchar       **text);
gboolean gtk_cmctree_node_get_pixbuf               (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  GdkPixbuf   **pixbuf);
gboolean gtk_cmctree_node_get_pixtext              (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  gchar       **text,
						  guint8       *spacing,
						  GdkPixbuf   **pixbuf);
gboolean gtk_cmctree_get_node_info                 (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gchar       **text,
						  guint8       *spacing,
						  GdkPixbuf   **pixbuf_closed,
						  GdkPixbuf   **pixbuf_opened,
						  gboolean     *is_leaf,
						  gboolean     *expanded);
void gtk_cmctree_node_set_row_style                (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  GtkStyle     *style);
GtkStyle * gtk_cmctree_node_get_row_style          (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_node_set_cell_style               (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  GtkStyle     *style);
GtkStyle * gtk_cmctree_node_get_cell_style         (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column);
void gtk_cmctree_node_set_foreground               (GtkCMCTree       *ctree,
						  GtkCMCTreeNode   *node,
						  const GdkColor *color);
void gtk_cmctree_node_set_background               (GtkCMCTree       *ctree,
						  GtkCMCTreeNode   *node,
						  const GdkColor *color);
void gtk_cmctree_node_set_row_data                 (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gpointer      data);
void gtk_cmctree_node_set_row_data_full            (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gpointer      data,
						  GDestroyNotify destroy);
gpointer gtk_cmctree_node_get_row_data             (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);
void gtk_cmctree_node_moveto                       (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node,
						  gint          column,
						  gfloat        row_align,
						  gfloat        col_align);
GtkVisibility gtk_cmctree_node_is_visible          (GtkCMCTree     *ctree,
						  GtkCMCTreeNode *node);

/***********************************************************
 *             GtkCMCTree specific functions                 *
 ***********************************************************/

void gtk_cmctree_set_indent            (GtkCMCTree                *ctree, 
				      gint                     indent);
void gtk_cmctree_set_spacing           (GtkCMCTree                *ctree, 
				      gint                     spacing);
void gtk_cmctree_set_show_stub         (GtkCMCTree                *ctree, 
				      gboolean                 show_stub);
void gtk_cmctree_set_line_style        (GtkCMCTree                *ctree, 
				      GtkCMCTreeLineStyle        line_style);
void gtk_cmctree_set_expander_style    (GtkCMCTree                *ctree, 
				      GtkCMCTreeExpanderStyle    expander_style);
void gtk_cmctree_set_drag_compare_func (GtkCMCTree     	      *ctree,
				      GtkCMCTreeCompareDragFunc  cmp_func);

/***********************************************************
 *             Tree sorting functions                      *
 ***********************************************************/

void gtk_cmctree_sort_node                         (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);
void gtk_cmctree_sort_recursive                    (GtkCMCTree     *ctree, 
						  GtkCMCTreeNode *node);


#define gtk_cmctree_set_reorderable(t,r)                    gtk_cmclist_set_reorderable((GtkCMCList*) (t),(r))

/* GType for the GtkCMCTreeNode.  This is a boxed type, although it uses
 * no-op's for the copy and free routines.  It is defined in order to
 * provide type information for the signal arguments
 */
GType   gtk_cmctree_node_get_type                  (void) G_GNUC_CONST;

G_END_DECLS

#endif				/* __GTK_CMCTREE_H__ */
