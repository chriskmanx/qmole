/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball, Josh MacDonald, 
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

#include <config.h>
#include <stdlib.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "gtkcmctree.h"
#include "claws-marshal.h"
#include "utils.h"
#include "gtkutils.c"

#define PM_SIZE                    8
#define TAB_SIZE                   (PM_SIZE + 6)
#define CELL_SPACING               1
#define CLIST_OPTIMUM_SIZE         64
#define COLUMN_INSET               3
#define DRAG_WIDTH                 6

#define ROW_TOP_YPIXEL(clist, row) (((clist)->row_height * (row)) + \
				    (((row) + 1) * CELL_SPACING) + \
				    (clist)->voffset)
#define ROW_FROM_YPIXEL(clist, y)  (((y) - (clist)->voffset) / \
                                    ((clist)->row_height + CELL_SPACING))
#define COLUMN_LEFT_XPIXEL(clist, col)  ((clist)->column[(col)].area.x \
                                    + (clist)->hoffset)
#define COLUMN_LEFT(clist, column) ((clist)->column[(column)].area.x)

GType
gtk_cmctree_pos_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_CMCTREE_POS_BEFORE, "GTK_CMCTREE_POS_BEFORE", "before" },
      { GTK_CMCTREE_POS_AS_CHILD, "GTK_CMCTREE_POS_AS_CHILD", "as-child" },
      { GTK_CMCTREE_POS_AFTER, "GTK_CMCTREE_POS_AFTER", "after" },
      { 0, NULL, NULL }
    };
#if GLIB_CHECK_VERSION(2,10,0)
    etype = g_enum_register_static (g_intern_static_string ("GtkCMCTreePos"), values);
#else
    etype = g_enum_register_static ("GtkCMCTreePos", values);
#endif
  }
  return etype;
}
GType
gtk_cmctree_line_style_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_CMCTREE_LINES_NONE, "GTK_CMCTREE_LINES_NONE", "none" },
      { 0, NULL, NULL }
    };
#if GLIB_CHECK_VERSION(2,10,0)
    etype = g_enum_register_static (g_intern_static_string ("GtkCMCTreeLineStyle"), values);
#else
    etype = g_enum_register_static ("GtkCMCTreeLineStyle", values);
#endif
  }
  return etype;
}
GType
gtk_cmctree_expander_style_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_CMCTREE_EXPANDER_NONE, "GTK_CMCTREE_EXPANDER_NONE", "none" },
      { GTK_CMCTREE_EXPANDER_TRIANGLE, "GTK_CMCTREE_EXPANDER_TRIANGLE", "triangle" },
      { 0, NULL, NULL }
    };
#if GLIB_CHECK_VERSION(2,10,0)
    etype = g_enum_register_static (g_intern_static_string ("GtkCMCTreeExpanderStyle"), values);
#else
    etype = g_enum_register_static ("GtkCMCTreeExpanderStyle", values);
#endif
  }
  return etype;
}
GType
gtk_cmctree_expansion_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_CMCTREE_EXPANSION_EXPAND, "GTK_CMCTREE_EXPANSION_EXPAND", "expand" },
      { GTK_CMCTREE_EXPANSION_EXPAND_RECURSIVE, "GTK_CMCTREE_EXPANSION_EXPAND_RECURSIVE", "expand-recursive" },
      { GTK_CMCTREE_EXPANSION_COLLAPSE, "GTK_CMCTREE_EXPANSION_COLLAPSE", "collapse" },
      { GTK_CMCTREE_EXPANSION_COLLAPSE_RECURSIVE, "GTK_CMCTREE_EXPANSION_COLLAPSE_RECURSIVE", "collapse-recursive" },
      { GTK_CMCTREE_EXPANSION_TOGGLE, "GTK_CMCTREE_EXPANSION_TOGGLE", "toggle" },
      { GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE, "GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE", "toggle-recursive" },
      { 0, NULL, NULL }
    };
#if GLIB_CHECK_VERSION(2,10,0)
    etype = g_enum_register_static (g_intern_static_string ("GtkCMCTreeExpansionType"), values);
#else
    etype = g_enum_register_static ("GtkCMCTreeExpansionType", values);
#endif
  }
  return etype;
}


static inline gint
COLUMN_FROM_XPIXEL (GtkCMCList * clist,
		    gint x)
{
  gint i, cx;

  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].visible)
      {
	cx = clist->column[i].area.x + clist->hoffset;

	if (x >= (cx - (COLUMN_INSET + CELL_SPACING)) &&
	    x <= (cx + clist->column[i].area.width + COLUMN_INSET))
	  return i;
      }

  /* no match */
  return -1;
}

#define CLIST_UNFROZEN(clist)     (((GtkCMCList*) (clist))->freeze_count == 0)
#define CLIST_REFRESH(clist)    G_STMT_START { \
  if (CLIST_UNFROZEN (clist)) \
    GTK_CMCLIST_GET_CLASS (clist)->refresh ((GtkCMCList*) (clist)); \
} G_STMT_END


enum {
  ARG_0,
  ARG_N_COLUMNS,
  ARG_TREE_COLUMN,
  ARG_INDENT,
  ARG_SPACING,
  ARG_SHOW_STUB,
  ARG_LINE_STYLE,
  ARG_EXPANDER_STYLE
};


static void     gtk_cmctree_class_init    (GtkCMCTreeClass         *klass);
static void     gtk_cmctree_init          (GtkCMCTree              *ctree);
static GObject* gtk_cmctree_constructor   (GType                  type,
				         guint                  n_construct_properties,
				         GObjectConstructParam *construct_params);
static void gtk_cmctree_set_arg		(GObject *object,
				guint      arg_id,
				const GValue *value,
				GParamSpec *spec);
static void gtk_cmctree_get_arg      	(GObject *object,
				guint      arg_id,
				GValue *value,
				GParamSpec *spec);
static void gtk_cmctree_realize           (GtkWidget      *widget);
static void gtk_cmctree_unrealize         (GtkWidget      *widget);
static gint gtk_cmctree_button_press      (GtkWidget      *widget,
					 GdkEventButton *event);
static void ctree_attach_styles         (GtkCMCTree       *ctree,
					 GtkCMCTreeNode   *node,
					 gpointer        data);
static void ctree_detach_styles         (GtkCMCTree       *ctree,
					 GtkCMCTreeNode   *node, 
					 gpointer        data);
static void set_cell_contents           (GtkCMCList      *clist,
					 GtkCMCListRow   *clist_row,
					 gint           column,
					 GtkCMCellType    type,
					 const gchar   *text,
					 guint8         spacing,
					 GdkPixbuf     *pixbuf);
static void set_node_info               (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 const gchar   *text,
					 guint8         spacing,
					 GdkPixbuf     *pixbuf_closed,
					 GdkPixbuf     *pixbuf_opened,
					 gboolean       is_leaf,
					 gboolean       expanded);
static GtkCMCTreeRow *row_new             (GtkCMCTree      *ctree);
static void row_delete                  (GtkCMCTree      *ctree,
				 	 GtkCMCTreeRow   *ctree_row);
static void tree_delete                 (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
					 gpointer       data);
static void tree_delete_row             (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
					 gpointer       data);
static void real_clear                  (GtkCMCList      *clist);
static void tree_update_level           (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
					 gpointer       data);
static void tree_select                 (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
					 gpointer       data);
static void tree_unselect               (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
				         gpointer       data);
static void real_select_all             (GtkCMCList      *clist);
static void real_unselect_all           (GtkCMCList      *clist);
static void tree_expand                 (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node,
					 gpointer       data);
static void tree_collapse               (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node,
					 gpointer       data);
static void tree_collapse_to_depth      (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
					 gint           depth);
static void tree_toggle_expansion       (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 gpointer       data);
static void change_focus_row_expansion  (GtkCMCTree      *ctree,
				         GtkCMCTreeExpansionType expansion);
static void real_select_row             (GtkCMCList      *clist,
					 gint           row,
					 gint           column,
					 GdkEvent      *event);
static void real_unselect_row           (GtkCMCList      *clist,
					 gint           row,
					 gint           column,
					 GdkEvent      *event);
static void real_tree_select            (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 gint           column);
static void real_tree_unselect          (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 gint           column);
static void real_tree_expand            (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node);
static void real_tree_collapse          (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node);
static void real_tree_move              (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 GtkCMCTreeNode  *new_parent, 
					 GtkCMCTreeNode  *new_sibling);
static void real_row_move               (GtkCMCList      *clist,
					 gint           source_row,
					 gint           dest_row);
static void gtk_cmctree_link              (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 GtkCMCTreeNode  *parent,
					 GtkCMCTreeNode  *sibling,
					 gboolean       update_focus_row);
static void gtk_cmctree_unlink            (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node,
					 gboolean       update_focus_row);
static GtkCMCTreeNode * gtk_cmctree_last_visible (GtkCMCTree     *ctree,
					      GtkCMCTreeNode *node);
static gboolean ctree_is_hot_spot       (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node,
					 gint           row, 
					 gint           x, 
					 gint           y);
static void tree_sort                   (GtkCMCTree      *ctree,
					 GtkCMCTreeNode  *node,
					 gpointer       data);
static void fake_unselect_all           (GtkCMCList      *clist,
					 gint           row);
static GList * selection_find           (GtkCMCList      *clist,
					 gint           row_number,
					 GList         *row_list_element);
static void resync_selection            (GtkCMCList      *clist,
					 GdkEvent      *event);
static void real_undo_selection         (GtkCMCList      *clist);
static void select_row_recursive        (GtkCMCTree      *ctree, 
					 GtkCMCTreeNode  *node, 
					 gpointer       data);
static gint real_insert_row             (GtkCMCList      *clist,
					 gint           row,
					 gchar         *text[]);
static void real_remove_row             (GtkCMCList      *clist,
					 gint           row);
static void real_sort_list              (GtkCMCList      *clist);
static void cell_size_request           (GtkCMCList       *clist,
					 GtkCMCListRow    *clist_row,
					 gint            column,
					 GtkRequisition *requisition);
static void column_auto_resize          (GtkCMCList       *clist,
					 GtkCMCListRow    *clist_row,
					 gint            column,
					 gint            old_width);
static void auto_resize_columns         (GtkCMCList       *clist);


static gboolean check_drag               (GtkCMCTree         *ctree,
			                  GtkCMCTreeNode     *drag_source,
					  GtkCMCTreeNode     *drag_target,
					  GtkCMCListDragPos   insert_pos);
static void gtk_cmctree_drag_begin         (GtkWidget        *widget,
					  GdkDragContext   *context);
static gint gtk_cmctree_drag_motion        (GtkWidget        *widget,
					  GdkDragContext   *context,
					  gint              x,
					  gint              y,
					  guint             time);
static void gtk_cmctree_drag_data_received (GtkWidget        *widget,
					  GdkDragContext   *context,
					  gint              x,
					  gint              y,
					  GtkSelectionData *selection_data,
					  guint             info,
					  guint32           time);
static void remove_grab                  (GtkCMCList         *clist);
static void drag_dest_cell               (GtkCMCList         *clist,
					  gint              x,
					  gint              y,
					  GtkCMCListDestInfo *dest_info);


enum
{
  TREE_SELECT_ROW,
  TREE_UNSELECT_ROW,
  TREE_EXPAND,
  TREE_COLLAPSE,
  TREE_MOVE,
  CHANGE_FOCUS_ROW_EXPANSION,
  LAST_SIGNAL
};

static GtkCMCListClass *parent_class = NULL;
static GtkContainerClass *container_class = NULL;
static guint ctree_signals[LAST_SIGNAL] = {0};


GType
gtk_cmctree_get_type (void)
{
  static GType ctree_type = 0;

  if (!ctree_type)
    {
      static const GTypeInfo ctree_info =
      {
			sizeof (GtkCMCTreeClass),

			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,

			(GClassInitFunc) gtk_cmctree_class_init,
			(GClassFinalizeFunc) NULL,
			NULL,	/* class_data */

			sizeof (GtkCMCTree),
			0,	/* n_preallocs */
			(GInstanceInitFunc) gtk_cmctree_init,
      };

	ctree_type = g_type_register_static (GTK_TYPE_CMCLIST, "GtkCMCTree", &ctree_info, (GTypeFlags)0);
    }

  return ctree_type;
}

static gint
draw_cell_pixbuf (GdkWindow    *window,
		  GdkRectangle *clip_rectangle,
		  cairo_t      *cr,
		  GdkPixbuf    *pixbuf,
		  gint          x,
		  gint          y,
		  gint          width,
		  gint          height)
{
  gint xsrc = 0;
  gint ysrc = 0;

  if (!pixbuf || (width == 0 && height == 0))
	return x;

  if (x < clip_rectangle->x)
    {
      xsrc = clip_rectangle->x - x;
      width -= xsrc;
      x = clip_rectangle->x;
    }
  if (x + width > clip_rectangle->x + clip_rectangle->width)
    width = clip_rectangle->x + clip_rectangle->width - x;

  if (y < clip_rectangle->y)
    {
      ysrc = clip_rectangle->y - y;
      height -= ysrc;
      y = clip_rectangle->y;
    }

  if (y + height > clip_rectangle->y + clip_rectangle->height)
    height = clip_rectangle->y + clip_rectangle->height - y;

  gdk_cairo_set_source_pixbuf(cr, pixbuf, x, y);
  cairo_paint(cr);

  return x + MAX (width, 0);
}

static gint
draw_expander (GtkCMCTree     *ctree,
               GtkCMCTreeRow  *ctree_row,
	       GtkStyle     *style,
	       GdkRectangle *clip_rectangle,
	       cairo_t	    *cr,
	       gint          x)
{
  GtkCMCList *clist;
  gint justification_factor;
  gint y;

 if (ctree->expander_style == GTK_CMCTREE_EXPANDER_NONE)
   return x;

  clist = GTK_CMCLIST (ctree);
  if (clist->column[ctree->tree_column].justification == GTK_JUSTIFY_RIGHT)
    justification_factor = -1;
  else
    justification_factor = 1;
  if (!GTK_CMCLIST_ROW_HEIGHT_SET(GTK_CMCLIST(clist)))
      y = (clip_rectangle->y + (clip_rectangle->height - PM_SIZE) / 2 -
          (clip_rectangle->height + 1) % 2) + 1;
  else
      y = (clip_rectangle->y + (clip_rectangle->height/2 - PM_SIZE) / 2 -
          (clip_rectangle->height/2 + 1) % 2) + 1;

  if (!ctree_row->children)
    {
	  return x + justification_factor * (PM_SIZE + 3);
    }

  /* pixel offsets +/- 1 or +/- justification_factor here and there ..
   * to fill correctly, somewhat ... what do I do wrong?
   */
  gdk_cairo_set_source_color(cr, &gtk_widget_get_style(GTK_WIDGET(ctree))->fg[GTK_STATE_NORMAL]);
  if (ctree_row->expanded)
  {
    gint tmp3 = PM_SIZE / 2;
    gint tmp6 = PM_SIZE / 6;
    cairo_move_to(cr, x + justification_factor * (tmp3 + tmp6) + (PM_SIZE / 2), y + 1);
    cairo_rel_line_to(cr, 0, tmp3 + tmp6 + 1);
    cairo_rel_line_to(cr, -justification_factor * (tmp3 + tmp6) - justification_factor, -1);
  }
  else
  {
    gint tmp3 = PM_SIZE / 2;
    gint tmp6 = PM_SIZE / 6;
    cairo_move_to(cr, x + tmp6 - justification_factor + (PM_SIZE / 2), y + tmp6 - 1);
    cairo_rel_line_to(cr, justification_factor * tmp3, tmp3);
    cairo_rel_line_to(cr, -justification_factor * tmp3, tmp3);
  }
  cairo_fill(cr);

  x += justification_factor * (PM_SIZE + 3);

  return x;
}

static gint
get_offset(GtkCMCTree     *ctree,
		      GtkCMCTreeRow  *ctree_row,
		      gint            column,
		      GdkRectangle   *clip_rectangle)
{
  gint justify_right;
  justify_right = (GTK_CMCLIST (ctree)->column[column].justification == GTK_JUSTIFY_RIGHT);

  if (justify_right)
      return (clip_rectangle->x + clip_rectangle->width - 1 -
		ctree->tree_indent * (ctree_row->level - 1));

  return clip_rectangle->x + ctree->tree_indent * (ctree_row->level - 1);
}

 static void
get_cell_style (GtkCMCList     *clist,
		GtkCMCListRow  *clist_row,
		gint          state,
		gint          column,
		GtkStyle    **style)
{
  GtkStyle *gtkstyle;

  gtkstyle = gtk_widget_get_style (GTK_WIDGET (clist));

  if (clist_row->cell[column].style)
    {
      if (style)
	*style = clist_row->cell[column].style;
    }
  else if (clist_row->style)
    {
      if (style)
	*style = clist_row->style;
    }
  else
    {
      if (style)
	*style = gtkstyle;
    }
}

static gboolean filter_fg (PangoAttribute *attribute, gpointer data)
{
	const PangoAttrClass *klass = attribute->klass;
	if (klass->type == PANGO_ATTR_FOREGROUND)
		return TRUE;

	return FALSE;	
}

static PangoLayout *
create_cell_layout (GtkCMCList       *clist,
			       GtkCMCListRow    *clist_row,
			       gint            column)
{
  PangoLayout *layout;
  GtkStyle *style;
  GtkCMCell *cell;
  gchar *text;

  get_cell_style (clist, clist_row, GTK_STATE_NORMAL, column, &style);


  cell = &clist_row->cell[column];
  switch (cell->type)
    {
    case GTK_CMCELL_TEXT:
    case GTK_CMCELL_PIXTEXT:
      text = ((cell->type == GTK_CMCELL_PIXTEXT) ?
	      GTK_CMCELL_PIXTEXT (*cell)->text :
	      GTK_CMCELL_TEXT (*cell)->text);

      if (!text)
	return NULL;
      
      if (!GTK_SCTREE(clist)->use_markup[column]) {
	      layout = gtk_widget_create_pango_layout (GTK_WIDGET (clist),
						       ((cell->type == GTK_CMCELL_PIXTEXT) ?
							GTK_CMCELL_PIXTEXT (*cell)->text :
							GTK_CMCELL_TEXT (*cell)->text));
	      pango_layout_set_font_description (layout, style->font_desc);
      } else {
	      PangoContext *context = gtk_widget_get_pango_context (GTK_WIDGET(clist));
	      layout = pango_layout_new (context);
	      pango_layout_set_markup (layout, text, -1);
	      pango_layout_set_font_description (layout, style->font_desc);
	      if (clist_row->state == GTK_STATE_SELECTED) {
		      /* for selected row, we should remove any forced foreground color
		       * or it looks like shit */
		      PangoAttrList *list = pango_layout_get_attributes(layout);
		      PangoAttrList *rem = pango_attr_list_filter(list, filter_fg, NULL);
		      if (rem)
			      pango_attr_list_unref(rem);
	      }
      }
      
      return layout;
      
    default:
      return NULL;
    }
}


static void
draw_row (GtkCMCList     *clist,
	  GdkRectangle *area,
	  gint          row,
	  GtkCMCListRow  *clist_row)
{
  GtkWidget *widget;
  GtkStyle *style;
  GtkCMCTree  *ctree;
  GdkRectangle *crect;
  GdkRectangle row_rectangle;
  GdkRectangle cell_rectangle; 
  GdkRectangle clip_rectangle;
  GdkRectangle intersect_rectangle;
  gint last_column;
  gint offset = 0;
  gint state;
  gint i;
  static GdkColor greybg={0, 0, 0, 0};
  static gboolean color_change = TRUE;
  cairo_t *cr;
  GdkColor *fgcolor, *bgcolor;

  cm_return_if_fail (clist != NULL);
  widget = GTK_WIDGET (clist);
  style = clist_row->style ? clist_row->style : gtk_widget_get_style (widget);

  if (greybg.pixel == 0 &&
      greybg.red == 0 &&
      greybg.green == 0 &&
      greybg.blue == 0) {
	GdkColor normalbg = {0, 0xffff, 0xffff, 0xffff};
	if (style) {
		normalbg = style->base[GTK_STATE_NORMAL];
	}
	if (normalbg.red > 0x8888 && normalbg.green > 0x8888 && normalbg.blue > 0x8888) {
		greybg.pixel = normalbg.pixel;
		greybg.red = normalbg.red - prefs_common.stripes_color_offset;
		greybg.green = normalbg.green - prefs_common.stripes_color_offset;
		greybg.blue = normalbg.blue - prefs_common.stripes_color_offset;
	} else if (normalbg.red < 0x8888 && normalbg.green < 0x8888 && normalbg.blue < 0x8888) {
		greybg.pixel = normalbg.pixel;
		greybg.red = normalbg.red + prefs_common.stripes_color_offset;
		greybg.green = normalbg.green + prefs_common.stripes_color_offset;
		greybg.blue = normalbg.blue + prefs_common.stripes_color_offset;
	} else {
		color_change = FALSE;
	}
  }

  /* bail now if we arn't drawable yet */
  if (!gtk_widget_is_drawable (GTK_WIDGET(clist)) || row < 0 || row >= clist->rows)
    return;

  ctree  = GTK_CMCTREE  (clist);

  /* if the function is passed the pointer to the row instead of null,
   * it avoids this expensive lookup */
  if (!clist_row)
    clist_row = (g_list_nth (clist->row_list, row))->data;

  /* rectangle of the entire row */
  row_rectangle.x = 0;
  row_rectangle.y = ROW_TOP_YPIXEL (clist, row);
  row_rectangle.width = clist->clist_window_width;
  row_rectangle.height = clist->row_height;

  /* rectangle of the cell spacing above the row */
  cell_rectangle.x = 0;
  cell_rectangle.y = row_rectangle.y - CELL_SPACING;
  cell_rectangle.width = row_rectangle.width;
  cell_rectangle.height = CELL_SPACING;

  /* rectangle used to clip drawing operations, its y and height
   * positions only need to be set once, so we set them once here. 
   * the x and width are set withing the drawing loop below once per
   * column */
  clip_rectangle.y = row_rectangle.y;
  clip_rectangle.height = row_rectangle.height;

  if (prefs_common.use_stripes_everywhere && GTK_SCTREE(ctree)->show_stripes
      && color_change && row % 2) {
    bgcolor = &greybg;
  } else {
    bgcolor = &style->base[GTK_STATE_NORMAL];
  }
  state = clist_row->state;

  cr = gdk_cairo_create(clist->clist_window);
  
  if (clist_row->fg_set && state != GTK_STATE_SELECTED)
	fgcolor = &clist_row->foreground;
  else
	fgcolor = &style->fg[clist_row->state];
  /* draw the cell borders */
  if (area)
    {
      crect = &intersect_rectangle;

      if (gdk_rectangle_intersect (area, &cell_rectangle, crect)) {
        gdk_cairo_rectangle(cr, &cell_rectangle);
	gdk_cairo_set_source_color(cr, &style->base[GTK_STATE_NORMAL]);
	cairo_fill(cr);
	cairo_rectangle(cr, cell_rectangle.x, cell_rectangle.y + row_rectangle.height + 1,cell_rectangle.width,cell_rectangle.height);
	cairo_fill(cr);
      }
    }
  else
    {
      crect = &cell_rectangle;

      gdk_cairo_rectangle(cr, &cell_rectangle);
      gdk_cairo_set_source_color(cr, &style->base[GTK_STATE_NORMAL]);
      cairo_fill(cr);
      cairo_rectangle(cr, cell_rectangle.x, cell_rectangle.y + row_rectangle.height + 1,cell_rectangle.width,cell_rectangle.height);
      cairo_fill(cr);
    }

  /* the last row has to clear its bottom cell spacing too */
  if (clist_row == clist->row_list_end->data)
    {
      cell_rectangle.y += clist->row_height + CELL_SPACING;

      if (!area || gdk_rectangle_intersect (area, &cell_rectangle, crect))
	{
          gdk_cairo_rectangle(cr, crect);
	  gdk_cairo_set_source_color(cr, &style->base[GTK_STATE_NORMAL]);
	  cairo_fill(cr);
	}
    }	  

  for (last_column = clist->columns - 1;
       last_column >= 0 && !clist->column[last_column].visible; last_column--)
    ;

  /* iterate and draw all the columns (row cells) and draw their contents */
  for (i = 0; i < clist->columns; i++)
    {
      GtkStyle *style;
      PangoLayout *layout = NULL;
      PangoRectangle logical_rect;

      gint width;
      gint height;
      gint pixbuf_width;
      gint string_width;
      gint old_offset;

      if (!clist->column[i].visible)
	continue;

      get_cell_style (clist, clist_row, state, i, &style);

      /* calculate clipping region */
      clip_rectangle.x = clist->column[i].area.x + clist->hoffset;
      clip_rectangle.width = clist->column[i].area.width;

      cell_rectangle.x = clip_rectangle.x - COLUMN_INSET - CELL_SPACING;
      cell_rectangle.width = (clip_rectangle.width + 2 * COLUMN_INSET +
			      (1 + (i == last_column)) * CELL_SPACING);
      cell_rectangle.y = clip_rectangle.y;
      cell_rectangle.height = clip_rectangle.height;

      string_width = 0;
      pixbuf_width = 0;
      height = 0;

      if (area && !gdk_rectangle_intersect (area, &cell_rectangle,
					    &intersect_rectangle))
	{
	  if (i != ctree->tree_column)
	    continue;
	}
      else
	{
	  gdk_cairo_rectangle(cr, &cell_rectangle);
	  if (state == GTK_STATE_NORMAL)
		gdk_cairo_set_source_color(cr, bgcolor);
	  else
		gdk_cairo_set_source_color(cr, &style->base[state]);
	  cairo_fill(cr);
	  layout = create_cell_layout (clist, clist_row, i);
	  if (layout)
	    {
	      pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
	      width = logical_rect.width;
	    }
	  else
	    width = 0;

	  switch (clist_row->cell[i].type)
	    {
	    case GTK_CMCELL_PIXBUF:
	      pixbuf_width = gdk_pixbuf_get_width(GTK_CMCELL_PIXBUF (clist_row->cell[i])->pixbuf);
	      height = gdk_pixbuf_get_height(GTK_CMCELL_PIXBUF (clist_row->cell[i])->pixbuf);
	      width += pixbuf_width;
	      break;
	    case GTK_CMCELL_PIXTEXT:
	      if (GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf)
		{
		  pixbuf_width = gdk_pixbuf_get_width(GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf);
		  height = gdk_pixbuf_get_height(GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf);
		  width += pixbuf_width;
		}

	      if (GTK_CMCELL_PIXTEXT (clist_row->cell[i])->text &&
		  GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf)
		width +=  GTK_CMCELL_PIXTEXT (clist_row->cell[i])->spacing;

	      if (i == ctree->tree_column)
		width += (ctree->tree_indent *
			  ((GtkCMCTreeRow *)clist_row)->level);
	      break;
	    default:
	      break;
	    }

	  switch (clist->column[i].justification)
	    {
	    case GTK_JUSTIFY_LEFT:
	      offset = clip_rectangle.x + clist_row->cell[i].horizontal;
	      break;
	    case GTK_JUSTIFY_RIGHT:
	      offset = (clip_rectangle.x + clist_row->cell[i].horizontal +
			clip_rectangle.width - width);
	      break;
	    case GTK_JUSTIFY_CENTER:
	    case GTK_JUSTIFY_FILL:
	      offset = (clip_rectangle.x + clist_row->cell[i].horizontal +
			(clip_rectangle.width / 2) - (width / 2));
	      break;
	    };

	  if (i != ctree->tree_column)
	    {
	      int start_y = (clip_rectangle.height - height) / 2;
	      if (GTK_CMCLIST_ROW_HEIGHT_SET(GTK_CMCLIST(clist)))
		      start_y = (clip_rectangle.height/2 - height) / 2;

	      offset += clist_row->cell[i].horizontal;
	      switch (clist_row->cell[i].type)
		{
		case GTK_CMCELL_PIXBUF:
		  draw_cell_pixbuf
		    (clist->clist_window, &clip_rectangle, cr,
		     GTK_CMCELL_PIXBUF (clist_row->cell[i])->pixbuf,
		     offset,
		     clip_rectangle.y + clist_row->cell[i].vertical +
		     start_y,
		     pixbuf_width, height);
		  break;
		case GTK_CMCELL_PIXTEXT:
		  offset = draw_cell_pixbuf
		    (clist->clist_window, &clip_rectangle, cr,
		     GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf,
		     offset,
		     clip_rectangle.y + clist_row->cell[i].vertical +
		     start_y,
		     pixbuf_width, height);
		  offset += GTK_CMCELL_PIXTEXT (clist_row->cell[i])->spacing;

		  /* Fall through */
		case GTK_CMCELL_TEXT:
		  if (layout)
		    {
		      gint row_center_offset = (clist->row_height - logical_rect.height) / 2;
		      gdk_cairo_set_source_color(cr, fgcolor);
		      cairo_move_to(cr, offset, row_rectangle.y + row_center_offset + clist_row->cell[i].vertical);
		      pango_cairo_show_layout(cr, layout);
		      g_object_unref (G_OBJECT (layout));
		    }
		  break;
		default:
		  break;
		}
	      continue;
	    }
	}

      /* draw ctree->tree_column */
      cell_rectangle.y -= CELL_SPACING;
      cell_rectangle.height += CELL_SPACING;

      if (area && !gdk_rectangle_intersect (area, &cell_rectangle,
					    &intersect_rectangle))
	{
	  if (layout)
            g_object_unref (G_OBJECT (layout));
	  continue;
	}


      /* draw lines */
      offset = get_offset (ctree, (GtkCMCTreeRow *)clist_row, i,
				      &clip_rectangle);

      /* draw expander */
      offset = draw_expander (ctree, (GtkCMCTreeRow *)clist_row,
					style, &clip_rectangle, cr, offset);

      if (clist->column[i].justification == GTK_JUSTIFY_RIGHT)
	offset -= ctree->tree_spacing;
      else
	offset += ctree->tree_spacing;

      if (clist->column[i].justification == GTK_JUSTIFY_RIGHT)
	offset -= (pixbuf_width + clist_row->cell[i].horizontal);
      else
	offset += clist_row->cell[i].horizontal;

      old_offset = offset;
      offset = draw_cell_pixbuf (clist->clist_window, &clip_rectangle, cr,
				 GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf,
				 offset, 
				 clip_rectangle.y + clist_row->cell[i].vertical
				 + (clip_rectangle.height - height) / 2,
				 pixbuf_width, height);

      if (layout)
	{
	  gint row_center_offset = (clist->row_height - logical_rect.height) / 2;
	  
	  if (clist->column[i].justification == GTK_JUSTIFY_RIGHT)
	    {
	      offset = (old_offset - string_width);
	      if (GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf)
		offset -= GTK_CMCELL_PIXTEXT (clist_row->cell[i])->spacing;
	    }
	  else
	    {
	      if (GTK_CMCELL_PIXTEXT (clist_row->cell[i])->pixbuf)
		offset += GTK_CMCELL_PIXTEXT (clist_row->cell[i])->spacing;
	    }
	  
	  cairo_move_to(cr, offset, row_rectangle.y + row_center_offset + clist_row->cell[i].vertical);
	  gdk_cairo_set_source_color(cr, fgcolor);
	  pango_cairo_show_layout(cr, layout);
          g_object_unref (G_OBJECT (layout));
	}
    }
   /* draw focus rectangle */
  if (clist->focus_row == row &&
      gtk_widget_get_can_focus (widget) && gtk_widget_has_focus (widget)
       && state == GTK_STATE_SELECTED)
    {
      if (!area || gdk_rectangle_intersect (area, &row_rectangle,
					&intersect_rectangle))
	{
	    cairo_set_line_width(cr, 1.0);
	    cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	    gdk_cairo_set_source_color(cr, &style->fg[GTK_STATE_NORMAL]);
	    cairo_move_to (cr, row_rectangle.x, row_rectangle.y + 0.5);
	    cairo_line_to (cr, row_rectangle.x + row_rectangle.width, row_rectangle.y + 0.5);
	    cairo_move_to (cr, row_rectangle.x, row_rectangle.y + row_rectangle.height - 0.5);
	    cairo_line_to (cr, row_rectangle.x + row_rectangle.width, row_rectangle.y + row_rectangle.height - 0.5);
	    cairo_stroke(cr);
	}
     }
    cairo_destroy(cr);
}

static void
gtk_cmctree_class_init (GtkCMCTreeClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkCMCListClass *clist_class;
  GtkBindingSet *binding_set;

  gobject_class->constructor = gtk_cmctree_constructor;

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  container_class = (GtkContainerClass *) klass;
  clist_class = (GtkCMCListClass *) klass;

  parent_class = g_type_class_peek (GTK_TYPE_CMCLIST);
  container_class = g_type_class_peek (GTK_TYPE_CONTAINER);

  gobject_class->set_property = gtk_cmctree_set_arg;
  gobject_class->get_property = gtk_cmctree_get_arg;

  widget_class->realize = gtk_cmctree_realize;
  widget_class->unrealize = gtk_cmctree_unrealize;
  widget_class->button_press_event = gtk_cmctree_button_press;

  widget_class->drag_begin = gtk_cmctree_drag_begin;
  widget_class->drag_motion = gtk_cmctree_drag_motion;
  widget_class->drag_data_received = gtk_cmctree_drag_data_received;

  clist_class->select_row = real_select_row;
  clist_class->unselect_row = real_unselect_row;
  clist_class->row_move = real_row_move;
  clist_class->undo_selection = real_undo_selection;
  clist_class->resync_selection = resync_selection;
  clist_class->selection_find = selection_find;
  clist_class->click_column = NULL;
  clist_class->draw_row = draw_row;
  clist_class->clear = real_clear;
  clist_class->select_all = real_select_all;
  clist_class->unselect_all = real_unselect_all;
  clist_class->fake_unselect_all = fake_unselect_all;
  clist_class->insert_row = real_insert_row;
  clist_class->remove_row = real_remove_row;
  clist_class->sort_list = real_sort_list;
  clist_class->set_cell_contents = set_cell_contents;
  clist_class->cell_size_request = cell_size_request;

  klass->tree_select_row = real_tree_select;
  klass->tree_unselect_row = real_tree_unselect;
  klass->tree_expand = real_tree_expand;
  klass->tree_collapse = real_tree_collapse;
  klass->tree_move = real_tree_move;
  klass->change_focus_row_expansion = change_focus_row_expansion;

  g_object_class_install_property (gobject_class,
				ARG_N_COLUMNS,
				g_param_spec_uint ("n-columns",
				"N-Columns",
				"N-Columns",
				1,
				G_MAXINT,
				1,
				G_PARAM_READWRITE|G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
				ARG_TREE_COLUMN,
				g_param_spec_uint ("tree-column",
				"tree-column",
				"tree-column",
				0,
				G_MAXINT,
				0,
				G_PARAM_READWRITE|G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
				ARG_INDENT,
				g_param_spec_uint ("indent",
				"indent",
				"indent",
				1,
				G_MAXINT,
				1,
				G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
				ARG_SPACING,
				g_param_spec_uint ("spacing",
				"spacing",
				"spacing",
				1,
				G_MAXINT,
				1,
				G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
				ARG_SHOW_STUB,
				g_param_spec_boolean ("show-stub",
				"show-stub",
				"show-stub",
				TRUE,
				G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
				ARG_LINE_STYLE,
				g_param_spec_enum ("line-style",
				"line-style",
				"line-style",
				GTK_TYPE_CMCTREE_LINE_STYLE, 0,
				G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
				ARG_EXPANDER_STYLE,
				g_param_spec_enum ("expander-style",
				"expander-style",
				"expander-style",
				GTK_TYPE_CMCTREE_EXPANDER_STYLE, 0,
				G_PARAM_READWRITE));

  ctree_signals[TREE_SELECT_ROW] =
 		g_signal_new ("tree_select_row",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GtkCMCTreeClass, tree_select_row),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER_INT,
			      G_TYPE_NONE, 2,
			      GTK_TYPE_CMCTREE_NODE,
			      G_TYPE_INT);
  ctree_signals[TREE_UNSELECT_ROW] =
 		g_signal_new ("tree_unselect_row",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GtkCMCTreeClass, tree_unselect_row),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER_INT,
			      G_TYPE_NONE, 2,
			      GTK_TYPE_CMCTREE_NODE,
			      G_TYPE_INT);
  ctree_signals[TREE_EXPAND] =
 		g_signal_new ("tree_expand",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GtkCMCTreeClass, tree_expand),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      GTK_TYPE_CMCTREE_NODE);
  ctree_signals[TREE_COLLAPSE] =
 		g_signal_new ("tree_collapse",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GtkCMCTreeClass, tree_collapse),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1,
			      GTK_TYPE_CMCTREE_NODE);
  ctree_signals[TREE_MOVE] =
 		g_signal_new ("tree_move",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GtkCMCTreeClass, tree_move),
			      NULL, NULL,
			      claws_marshal_VOID__POINTER_POINTER_POINTER,
			      G_TYPE_NONE, 3,
			      GTK_TYPE_CMCTREE_NODE,GTK_TYPE_CMCTREE_NODE,GTK_TYPE_CMCTREE_NODE);
  ctree_signals[CHANGE_FOCUS_ROW_EXPANSION] =
 		g_signal_new ("change_focus_row_expansion",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkCMCTreeClass, change_focus_row_expansion),
			      NULL, NULL,
			      claws_marshal_VOID__ENUM,
			      G_TYPE_NONE, 1, GTK_TYPE_CMCTREE_EXPANSION_TYPE);

  binding_set = gtk_binding_set_by_class (klass);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_plus, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_EXPAND);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_plus, GDK_CONTROL_MASK,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_EXPAND_RECURSIVE);

  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Add, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_EXPAND);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Add, GDK_CONTROL_MASK,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_EXPAND_RECURSIVE);
  
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_minus, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_COLLAPSE);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_minus, GDK_CONTROL_MASK,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM,
				GTK_CMCTREE_EXPANSION_COLLAPSE_RECURSIVE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Subtract, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_COLLAPSE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Subtract, GDK_CONTROL_MASK,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM,
				GTK_CMCTREE_EXPANSION_COLLAPSE_RECURSIVE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_equal, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_TOGGLE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Equal, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_TOGGLE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Multiply, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_TOGGLE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_asterisk, 0,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM, GTK_CMCTREE_EXPANSION_TOGGLE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_KP_Multiply, GDK_CONTROL_MASK,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM,
				GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE);
  gtk_binding_entry_add_signal (binding_set,
				GDK_KEY_asterisk, GDK_CONTROL_MASK,
				"change_focus_row_expansion", 1,
				G_TYPE_ENUM,
				GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE);  
}

static void
gtk_cmctree_set_arg (GObject *object,
				guint      arg_id,
				const GValue *value,
				GParamSpec *spec)
{
  GtkCMCTree *ctree;
  GtkCMCList *clist;

  ctree = GTK_CMCTREE (object);
  clist = GTK_CMCLIST (ctree);

  switch (arg_id)
    {
    case ARG_N_COLUMNS: /* construct-only arg, only set at construction time */
#if !GLIB_CHECK_VERSION(2,10,0)
      cm_return_if_fail (clist->row_mem_chunk == NULL);
#endif
      clist->columns = MAX (1, g_value_get_uint (value));
#if !GLIB_CHECK_VERSION(2,10,0)
      clist->row_mem_chunk = g_mem_chunk_new ("ctree row mem chunk",
					      sizeof (GtkCMCTreeRow),
					      sizeof (GtkCMCTreeRow)
					      * CLIST_OPTIMUM_SIZE,
					      G_ALLOC_AND_FREE);
      clist->cell_mem_chunk = g_mem_chunk_new ("ctree cell mem chunk",
					       sizeof (GtkCMCell) * clist->columns,
					       sizeof (GtkCMCell) * clist->columns
					       * CLIST_OPTIMUM_SIZE,
					       G_ALLOC_AND_FREE);
#endif
      ctree->tree_column = CLAMP (ctree->tree_column, 0, clist->columns);
      break;
    case ARG_TREE_COLUMN: /* construct-only arg, only set at construction time */
      ctree->tree_column = g_value_get_uint (value);
#if !GLIB_CHECK_VERSION(2,10,0)
      if (clist->row_mem_chunk)
#endif
        ctree->tree_column = CLAMP (ctree->tree_column, 0, clist->columns);
      break;
    case ARG_INDENT:
      gtk_cmctree_set_indent (ctree, g_value_get_uint (value));
      break;
    case ARG_SPACING:
      gtk_cmctree_set_spacing (ctree, g_value_get_uint (value));
      break;
    case ARG_SHOW_STUB:
      gtk_cmctree_set_show_stub (ctree, g_value_get_boolean (value));
      break;
    case ARG_LINE_STYLE:
      gtk_cmctree_set_line_style (ctree, g_value_get_enum (value));
      break;
    case ARG_EXPANDER_STYLE:
      gtk_cmctree_set_expander_style (ctree, g_value_get_enum (value));
      break;
    default:
      break;
    }
}

static void
gtk_cmctree_get_arg (GObject *object,
				guint      arg_id,
				GValue *value,
				GParamSpec *spec)
{
  GtkCMCTree *ctree;

  ctree = GTK_CMCTREE (object);

  switch (arg_id)
    {
    case ARG_N_COLUMNS:
      g_value_set_uint(value, GTK_CMCLIST (ctree)->columns);
      break;
    case ARG_TREE_COLUMN:
      g_value_set_uint(value, ctree->tree_column);
      break;
    case ARG_INDENT:
      g_value_set_uint(value, ctree->tree_indent);
      break;
    case ARG_SPACING:
      g_value_set_uint(value, ctree->tree_spacing);
      break;
    case ARG_SHOW_STUB:
      g_value_set_boolean(value, ctree->show_stub);
      break;
    case ARG_LINE_STYLE:
      g_value_set_enum(value, ctree->line_style);
      break;
    case ARG_EXPANDER_STYLE:
      g_value_set_enum(value, ctree->expander_style);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, arg_id, spec);
      break;
    }
}

static void
gtk_cmctree_init (GtkCMCTree *ctree)
{
  GtkCMCList *clist;

  GTK_CMCLIST_SET_FLAG (ctree, CMCLIST_DRAW_DRAG_RECT);
  GTK_CMCLIST_SET_FLAG (ctree, CMCLIST_DRAW_DRAG_LINE);

  clist = GTK_CMCLIST (ctree);

  ctree->tree_indent    = 20;
  ctree->tree_spacing   = 5;
  ctree->tree_column    = 0;
  ctree->line_style     = GTK_CMCTREE_LINES_NONE;
  ctree->expander_style = GTK_CMCTREE_EXPANDER_TRIANGLE;
  ctree->drag_compare   = NULL;
  ctree->show_stub      = TRUE;

  clist->button_actions[0] |= GTK_CMBUTTON_EXPANDS;
}

static void
ctree_attach_styles (GtkCMCTree     *ctree,
		     GtkCMCTreeNode *node,
		     gpointer      data)
{
  GtkCMCList *clist;
  gint i;

  clist = GTK_CMCLIST (ctree);

  if (GTK_CMCTREE_ROW (node)->row.style)
    GTK_CMCTREE_ROW (node)->row.style =
      gtk_style_attach (GTK_CMCTREE_ROW (node)->row.style, clist->clist_window);

  if (GTK_CMCTREE_ROW (node)->row.fg_set || GTK_CMCTREE_ROW (node)->row.bg_set)
    {
      GdkColormap *colormap;

      colormap = gtk_widget_get_colormap (GTK_WIDGET (ctree));
      if (GTK_CMCTREE_ROW (node)->row.fg_set)
	gdk_colormap_alloc_color (colormap, &(GTK_CMCTREE_ROW (node)->row.foreground), TRUE, TRUE);
      if (GTK_CMCTREE_ROW (node)->row.bg_set)
	gdk_colormap_alloc_color (colormap, &(GTK_CMCTREE_ROW (node)->row.background), TRUE, TRUE);
    }

  for (i = 0; i < clist->columns; i++)
    if  (GTK_CMCTREE_ROW (node)->row.cell[i].style)
      GTK_CMCTREE_ROW (node)->row.cell[i].style =
	gtk_style_attach (GTK_CMCTREE_ROW (node)->row.cell[i].style,
			  clist->clist_window);
}

static void
ctree_detach_styles (GtkCMCTree     *ctree,
		     GtkCMCTreeNode *node,
		     gpointer      data)
{
  GtkCMCList *clist;
  gint i;

  clist = GTK_CMCLIST (ctree);

  if (GTK_CMCTREE_ROW (node)->row.style)
    gtk_style_detach (GTK_CMCTREE_ROW (node)->row.style);
  for (i = 0; i < clist->columns; i++)
    if  (GTK_CMCTREE_ROW (node)->row.cell[i].style)
      gtk_style_detach (GTK_CMCTREE_ROW (node)->row.cell[i].style);
}

static void
gtk_cmctree_realize (GtkWidget *widget)
{
  GtkCMCTree *ctree;
  GtkCMCList *clist;
  GtkCMCTreeNode *node;
  GtkCMCTreeNode *child;
  gint i;

  cm_return_if_fail (GTK_IS_CMCTREE (widget));

  GTK_WIDGET_CLASS (parent_class)->realize (widget);

  ctree = GTK_CMCTREE (widget);
  clist = GTK_CMCLIST (widget);

  node = GTK_CMCTREE_NODE (clist->row_list);
  for (i = 0; i < clist->rows; i++)
    {
      if (GTK_CMCTREE_ROW (node)->children && !GTK_CMCTREE_ROW (node)->expanded)
	for (child = GTK_CMCTREE_ROW (node)->children; child;
	     child = GTK_CMCTREE_ROW (child)->sibling)
	  gtk_cmctree_pre_recursive (ctree, child, ctree_attach_styles, NULL);
      node = GTK_CMCTREE_NODE_NEXT (node);
    }
}

static void
gtk_cmctree_unrealize (GtkWidget *widget)
{
  GtkCMCTree *ctree;
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (widget));

  GTK_WIDGET_CLASS (parent_class)->unrealize (widget);

  ctree = GTK_CMCTREE (widget);
  clist = GTK_CMCLIST (widget);

  if (gtk_widget_get_realized (widget))
    {
      GtkCMCTreeNode *node;
      GtkCMCTreeNode *child;
      gint i;

      node = GTK_CMCTREE_NODE (clist->row_list);
      for (i = 0; i < clist->rows; i++)
	{
	  if (GTK_CMCTREE_ROW (node)->children &&
	      !GTK_CMCTREE_ROW (node)->expanded)
	    for (child = GTK_CMCTREE_ROW (node)->children; child;
		 child = GTK_CMCTREE_ROW (child)->sibling)
	      gtk_cmctree_pre_recursive(ctree, child, ctree_detach_styles, NULL);
	  node = GTK_CMCTREE_NODE_NEXT (node);
	}
    }
}

static gint
gtk_cmctree_button_press (GtkWidget      *widget,
			GdkEventButton *event)
{
  GtkCMCTree *ctree;
  GtkCMCList *clist;
  gint button_actions;

  cm_return_val_if_fail (GTK_IS_CMCTREE (widget), FALSE);
  cm_return_val_if_fail (event != NULL, FALSE);

  ctree = GTK_CMCTREE (widget);
  clist = GTK_CMCLIST (widget);

  button_actions = clist->button_actions[event->button - 1];

  if (button_actions == GTK_CMBUTTON_IGNORED)
    return FALSE;

  if (event->window == clist->clist_window)
    {
      GtkCMCTreeNode *work;
      gint x;
      gint y;
      gint row;
      gint column;

      x = event->x;
      y = event->y;

      if (!gtk_cmclist_get_selection_info (clist, x, y, &row, &column))
	return FALSE;

      work = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, row));
	  
      if (button_actions & GTK_CMBUTTON_EXPANDS &&
	  (GTK_CMCTREE_ROW (work)->children && !GTK_CMCTREE_ROW (work)->is_leaf  &&
	   (event->type == GDK_2BUTTON_PRESS ||
	    ctree_is_hot_spot (ctree, work, row, x, y))))
	{
	  if (GTK_CMCTREE_ROW (work)->expanded)
	    gtk_cmctree_collapse (ctree, work);
	  else
	    gtk_cmctree_expand (ctree, work);

	  return TRUE;
	}
    }
  
  return GTK_WIDGET_CLASS (parent_class)->button_press_event (widget, event);
}

static GtkCMCTreeNode *
gtk_cmctree_last_visible (GtkCMCTree     *ctree,
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

  return gtk_cmctree_last_visible (ctree, work);
}

static void
gtk_cmctree_link (GtkCMCTree     *ctree,
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

  if (update_focus_row && clist->selection_mode == GTK_SELECTION_MULTIPLE)
    {
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
			     GTK_CMCTREE_ROW (parent)->expanded)))
    {
      visible = TRUE;
      clist->rows += rows;
    }

  if (parent)
    work = (GList *)(GTK_CMCTREE_ROW (parent)->children);
  else
    work = clist->row_list;

  if (sibling)
    {
      if (work != (GList *)sibling)
	{
	  while (GTK_CMCTREE_ROW (work)->sibling != sibling)
	    work = (GList *)(GTK_CMCTREE_ROW (work)->sibling);
	  GTK_CMCTREE_ROW (work)->sibling = node;
	}

      if (sibling == GTK_CMCTREE_NODE (clist->row_list))
	clist->row_list = (GList *) node;
      if (GTK_CMCTREE_NODE_PREV (sibling) &&
	  GTK_CMCTREE_NODE_NEXT (GTK_CMCTREE_NODE_PREV (sibling)) == sibling)
	{
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
  else
    {
      if (work)
	{
	  /* find sibling */
	  while (GTK_CMCTREE_ROW (work)->sibling)
	    work = (GList *)(GTK_CMCTREE_ROW (work)->sibling);
	  GTK_CMCTREE_ROW (work)->sibling = node;
	  
	  /* find last visible child of sibling */
	  work = (GList *) gtk_cmctree_last_visible (ctree,
						   GTK_CMCTREE_NODE (work));
	  
	  list_end->next = work->next;
	  if (work->next)
	    list = work->next->prev = list_end;
	  work->next = (GList *)node;
	  list = (GList *)node;
	  list->prev = work;
	}
      else
	{
	  if (parent)
	    {
	      GTK_CMCTREE_ROW (parent)->children = node;
	      list = (GList *)node;
	      list->prev = (GList *)parent;
	      if (GTK_CMCTREE_ROW (parent)->expanded)
		{
		  list_end->next = (GList *)GTK_CMCTREE_NODE_NEXT (parent);
		  if (GTK_CMCTREE_NODE_NEXT(parent))
		    {
		      list = (GList *)GTK_CMCTREE_NODE_NEXT (parent);
		      list->prev = list_end;
		    }
		  list = (GList *)parent;
		  list->next = (GList *)node;
		}
	      else
		list_end->next = NULL;
	    }
	  else
	    {
	      clist->row_list = (GList *)node;
	      list = (GList *)node;
	      list->prev = NULL;
	      list_end->next = NULL;
	    }
	}
    }

  gtk_cmctree_pre_recursive (ctree, node, tree_update_level, NULL); 

  if (clist->row_list_end == NULL ||
      clist->row_list_end->next == (GList *)node)
    clist->row_list_end = list_end;

  if (visible && update_focus_row)
    {
      gint pos;
	  
      pos = g_list_position (clist->row_list, (GList *)node);
  
      if (pos <= clist->focus_row)
	{
	  clist->focus_row += rows;
	  clist->undo_anchor = clist->focus_row;
	}
    }
}

static void
gtk_cmctree_unlink (GtkCMCTree     *ctree, 
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

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  clist = GTK_CMCLIST (ctree);
  
  if (update_focus_row && clist->selection_mode == GTK_SELECTION_MULTIPLE)
    {
      GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
      
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
    }

  visible = gtk_cmctree_is_viewable (ctree, node);

  /* clist->row_list_end unlinked ? */
  if (visible &&
      (GTK_CMCTREE_NODE_NEXT (node) == NULL ||
       (GTK_CMCTREE_ROW (node)->children &&
	gtk_cmctree_is_ancestor (ctree, node,
			       GTK_CMCTREE_NODE (clist->row_list_end)))))
    clist->row_list_end = (GList *) (GTK_CMCTREE_NODE_PREV (node));

  /* update list */
  rows = 0;
  level = GTK_CMCTREE_ROW (node)->level;
  work = GTK_CMCTREE_NODE_NEXT (node);
  while (work && GTK_CMCTREE_ROW (work)->level > level)
    {
      work = GTK_CMCTREE_NODE_NEXT (work);
      rows++;
    }

  if (visible)
    {
      clist->rows -= (rows + 1);

      if (update_focus_row)
	{
	  gint pos;
	  
	  pos = g_list_position (clist->row_list, (GList *)node);
	  if (pos + rows < clist->focus_row)
	    clist->focus_row -= (rows + 1);
	  else if (pos <= clist->focus_row)
	    {
	      if (!GTK_CMCTREE_ROW (node)->sibling)
		clist->focus_row = MAX (pos - 1, 0);
	      else
		clist->focus_row = pos;
	      
	      clist->focus_row = MIN (clist->focus_row, clist->rows - 1);
	    }
	  clist->undo_anchor = clist->focus_row;
	}
    }

  if (work)
    {
      list = (GList *)GTK_CMCTREE_NODE_PREV (work);
      list->next = NULL;
      list = (GList *)work;
      list->prev = (GList *)GTK_CMCTREE_NODE_PREV (node);
    }

  if (GTK_CMCTREE_NODE_PREV (node) &&
      GTK_CMCTREE_NODE_NEXT (GTK_CMCTREE_NODE_PREV (node)) == node)
    {
      list = (GList *)GTK_CMCTREE_NODE_PREV (node);
      list->next = (GList *)work;
    }

  /* update tree */
  parent = GTK_CMCTREE_ROW (node)->parent;
  if (parent)
    {
      if (GTK_CMCTREE_ROW (parent)->children == node)
	{
	  GTK_CMCTREE_ROW (parent)->children = GTK_CMCTREE_ROW (node)->sibling;
	  if (!GTK_CMCTREE_ROW (parent)->children)
	    gtk_cmctree_collapse (ctree, parent);
	}
      else
	{
	  GtkCMCTreeNode *sibling;

	  sibling = GTK_CMCTREE_ROW (parent)->children;
	  while (GTK_CMCTREE_ROW (sibling)->sibling != node)
	    sibling = GTK_CMCTREE_ROW (sibling)->sibling;
	  GTK_CMCTREE_ROW (sibling)->sibling = GTK_CMCTREE_ROW (node)->sibling;
	}
    }
  else
    {
      if (clist->row_list == (GList *)node)
	clist->row_list = (GList *) (GTK_CMCTREE_ROW (node)->sibling);
      else
	{
	  GtkCMCTreeNode *sibling;

	  sibling = GTK_CMCTREE_NODE (clist->row_list);
	  while (GTK_CMCTREE_ROW (sibling)->sibling != node)
	    sibling = GTK_CMCTREE_ROW (sibling)->sibling;
	  GTK_CMCTREE_ROW (sibling)->sibling = GTK_CMCTREE_ROW (node)->sibling;
	}
    }
}

static void
real_row_move (GtkCMCList *clist,
	       gint      source_row,
	       gint      dest_row)
{
  GtkCMCTree *ctree;
  GtkCMCTreeNode *node;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  if (GTK_CMCLIST_AUTO_SORT (clist))
    return;

  if (source_row < 0 || source_row >= clist->rows ||
      dest_row   < 0 || dest_row   >= clist->rows ||
      source_row == dest_row)
    return;

  ctree = GTK_CMCTREE (clist);
  node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, source_row));

  if (source_row < dest_row)
    {
      GtkCMCTreeNode *work; 

      dest_row++;
      work = GTK_CMCTREE_ROW (node)->children;

      while (work && GTK_CMCTREE_ROW (work)->level > GTK_CMCTREE_ROW (node)->level)
	{
	  work = GTK_CMCTREE_NODE_NEXT (work);
	  dest_row++;
	}

      if (dest_row > clist->rows)
	dest_row = clist->rows;
    }

  if (dest_row < clist->rows)
    {
      GtkCMCTreeNode *sibling;

      sibling = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, dest_row));
      gtk_cmctree_move (ctree, node, GTK_CMCTREE_ROW (sibling)->parent, sibling);
    }
  else
    gtk_cmctree_move (ctree, node, NULL, NULL);
}

static void
real_tree_move (GtkCMCTree     *ctree,
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
  if (gtk_cmctree_is_viewable (ctree, node))
    work = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row));
      
  gtk_cmctree_unlink (ctree, node, FALSE);
  gtk_cmctree_link (ctree, node, new_parent, new_sibling, FALSE);
  
  if (work)
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

static void
change_focus_row_expansion (GtkCMCTree          *ctree,
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
      gtk_cmctree_toggle_expansion (ctree, node);
      break;
    case GTK_CMCTREE_EXPANSION_TOGGLE_RECURSIVE:
      gtk_cmctree_toggle_expansion_recursive (ctree, node);
      break;
    }
}

static void 
real_tree_expand (GtkCMCTree     *ctree,
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

	  /* update focus_row position */
	  row = g_list_position (clist->row_list, (GList *)node);
	  if (row < clist->focus_row)
	    clist->focus_row += tmp;

	  clist->rows += tmp;
	  CLIST_REFRESH (clist);
	}
    }
  else if (visible && clist->column[ctree->tree_column].auto_resize)
    /* resize tree_column if needed */
    column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row, ctree->tree_column,
			requisition.width);
}

static void 
real_tree_collapse (GtkCMCTree     *ctree,
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
	  auto_resize_columns (clist);

	  row = g_list_position (clist->row_list, (GList *)node);
	  if (row < clist->focus_row)
	    clist->focus_row -= tmp;
	  clist->rows -= tmp;
	  CLIST_REFRESH (clist);
	}
    }
  else if (visible && clist->column[ctree->tree_column].auto_resize &&
	   !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    /* resize tree_column if needed */
    column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row, ctree->tree_column,
			requisition.width);
    
}

static void
column_auto_resize (GtkCMCList    *clist,
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
      gint new_width;

      /* run a "gtk_cmclist_optimal_column_width" but break, if
       * the column doesn't shrink */
      if (GTK_CMCLIST_SHOW_TITLES (clist) && clist->column[column].button)
        {
	GtkRequisition req;
	gtk_widget_get_requisition (clist->column[column].button, &req);
	new_width = (req.width -
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
auto_resize_columns (GtkCMCList *clist)
{
  gint i;

  if (GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    return;

  for (i = 0; i < clist->columns; i++)
    column_auto_resize (clist, NULL, i, clist->column[i].width);
}

static void
cell_size_request (GtkCMCList       *clist,
		   GtkCMCListRow    *clist_row,
		   gint            column,
		   GtkRequisition *requisition)
{
  GtkCMCTree *ctree;
  gint width;
  gint height;
  PangoLayout *layout;
  PangoRectangle logical_rect;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));
  cm_return_if_fail (requisition != NULL);

  ctree = GTK_CMCTREE (clist);

  layout = create_cell_layout (clist, clist_row, column);
  if (layout)
    {
      pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

      requisition->width = logical_rect.width;
      requisition->height = logical_rect.height;
      
      g_object_unref (G_OBJECT (layout));
    }
  else
    {
      requisition->width  = 0;
      requisition->height = 0;
    }

  switch (clist_row->cell[column].type)
    {
    case GTK_CMCELL_PIXTEXT:
      if (GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf)
	{
	  width = gdk_pixbuf_get_width(GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf);
	  height = gdk_pixbuf_get_height(GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf);
	  width += GTK_CMCELL_PIXTEXT (clist_row->cell[column])->spacing;
	}
      else
	width = height = 0;
	  
      requisition->width += width;
      requisition->height = MAX (requisition->height, height);
      
      if (column == ctree->tree_column)
	{
	  requisition->width += (ctree->tree_spacing + ctree->tree_indent *
				 (((GtkCMCTreeRow *) clist_row)->level - 1));
	  switch (ctree->expander_style)
	    {
	    case GTK_CMCTREE_EXPANDER_NONE:
	      break;
	    case GTK_CMCTREE_EXPANDER_TRIANGLE:
	      requisition->width += PM_SIZE + 3;
	      break;
	    }
	}
      break;
    case GTK_CMCELL_PIXBUF:
      width = gdk_pixbuf_get_width(GTK_CMCELL_PIXBUF (clist_row->cell[column])->pixbuf);
      height = gdk_pixbuf_get_height(GTK_CMCELL_PIXBUF (clist_row->cell[column])->pixbuf);
      requisition->width += width;
      requisition->height = MAX (requisition->height, height);
      break;
    default:
      break;
    }

  requisition->width  += clist_row->cell[column].horizontal;
  requisition->height += clist_row->cell[column].vertical;
}

static void
set_cell_contents (GtkCMCList    *clist,
		   GtkCMCListRow *clist_row,
		   gint         column,
		   GtkCMCellType  type,
		   const gchar *text,
		   guint8       spacing,
		   GdkPixbuf   *pixbuf)
{
  gboolean visible = FALSE;
  GtkCMCTree *ctree;
  GtkRequisition requisition;
  gchar *old_text = NULL;
  GdkPixbuf *old_pixbuf = NULL;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));
  cm_return_if_fail (clist_row != NULL);

  ctree = GTK_CMCTREE (clist);

  if (clist->column[column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    {
      GtkCMCTreeNode *parent;

      parent = ((GtkCMCTreeRow *)clist_row)->parent;
      if ((parent && GTK_CMCTREE_ROW (parent)->expanded &&
		      gtk_cmctree_is_viewable (ctree, parent)))
	{
	  visible = TRUE;
	  GTK_CMCLIST_GET_CLASS (clist)->cell_size_request (clist, clist_row,
							 column, &requisition);
	}
    }

  switch (clist_row->cell[column].type)
    {
    case GTK_CMCELL_EMPTY:
      break;
    case GTK_CMCELL_TEXT:
      old_text = GTK_CMCELL_TEXT (clist_row->cell[column])->text;
      break;
    case GTK_CMCELL_PIXBUF:
      old_pixbuf = GTK_CMCELL_PIXBUF (clist_row->cell[column])->pixbuf;
      break;
    case GTK_CMCELL_PIXTEXT:
      old_text = GTK_CMCELL_PIXTEXT (clist_row->cell[column])->text;
      old_pixbuf = GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf;
      break;
    case GTK_CMCELL_WIDGET:
      /* unimplemented */
      break;
      
    default:
      break;
    }

  clist_row->cell[column].type = GTK_CMCELL_EMPTY;
  if (column == ctree->tree_column && type != GTK_CMCELL_EMPTY)
    type = GTK_CMCELL_PIXTEXT;

  /* Note that pixbuf and mask were already ref'ed by the caller
   */
  switch (type)
    {
    case GTK_CMCELL_TEXT:
      if (text)
	{
	  clist_row->cell[column].type = GTK_CMCELL_TEXT;
	  GTK_CMCELL_TEXT (clist_row->cell[column])->text = g_strdup (text);
	}
      break;
    case GTK_CMCELL_PIXBUF:
      if (pixbuf)
	{
	  clist_row->cell[column].type = GTK_CMCELL_PIXBUF;
	  GTK_CMCELL_PIXBUF (clist_row->cell[column])->pixbuf = pixbuf;
	}
      break;
    case GTK_CMCELL_PIXTEXT:
      if (column == ctree->tree_column)
	{
	  clist_row->cell[column].type = GTK_CMCELL_PIXTEXT;
	  GTK_CMCELL_PIXTEXT (clist_row->cell[column])->spacing = spacing;
	  if (text)
	    GTK_CMCELL_PIXTEXT (clist_row->cell[column])->text = g_strdup (text);
	  else
	    GTK_CMCELL_PIXTEXT (clist_row->cell[column])->text = NULL;
	  if (pixbuf)
	    {
	      GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf = pixbuf;
	    }
	  else
	    {
	      GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf = NULL;
	    }
	}
      else if (text && pixbuf)
	{
	  clist_row->cell[column].type = GTK_CMCELL_PIXTEXT;
	  GTK_CMCELL_PIXTEXT (clist_row->cell[column])->text = g_strdup (text);
	  GTK_CMCELL_PIXTEXT (clist_row->cell[column])->spacing = spacing;
	  GTK_CMCELL_PIXTEXT (clist_row->cell[column])->pixbuf = pixbuf;
	}
      break;
    default:
      break;
    }
  
  if (visible && clist->column[column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    column_auto_resize (clist, clist_row, column, requisition.width);

  g_free (old_text);
  if (old_pixbuf)
    g_object_unref (old_pixbuf);
}

static void 
set_node_info (GtkCMCTree     *ctree,
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

static void
tree_delete (GtkCMCTree     *ctree, 
	     GtkCMCTreeNode *node, 
	     gpointer      data)
{
  tree_unselect (ctree,  node, NULL);
  row_delete (ctree, GTK_CMCTREE_ROW (node));
  g_list_free_1 ((GList *)node);
}

static void
tree_delete_row (GtkCMCTree     *ctree, 
		 GtkCMCTreeNode *node, 
		 gpointer      data)
{
  row_delete (ctree, GTK_CMCTREE_ROW (node));
  g_list_free_1 ((GList *)node);
}

static void
tree_update_level (GtkCMCTree     *ctree, 
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

static void
tree_select (GtkCMCTree     *ctree, 
	     GtkCMCTreeNode *node, 
	     gpointer      data)
{
  if (node && GTK_CMCTREE_ROW (node)->row.state != GTK_STATE_SELECTED &&
      GTK_CMCTREE_ROW (node)->row.selectable)
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_SELECT_ROW], 0,
		     node, -1);
}

static void
tree_unselect (GtkCMCTree     *ctree, 
	       GtkCMCTreeNode *node, 
	       gpointer      data)
{
  if (node && GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_SELECTED)
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_UNSELECT_ROW], 0,
		     node, -1);
}

static void
tree_expand (GtkCMCTree     *ctree, 
	     GtkCMCTreeNode *node, 
	     gpointer      data)
{
  if (node && !GTK_CMCTREE_ROW (node)->expanded)
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_EXPAND], 0,node);
}

static void
tree_collapse (GtkCMCTree     *ctree, 
	       GtkCMCTreeNode *node, 
	       gpointer      data)
{
  if (node && GTK_CMCTREE_ROW (node)->expanded)
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_COLLAPSE], 0,node);
}

static void
tree_collapse_to_depth (GtkCMCTree     *ctree, 
			GtkCMCTreeNode *node, 
			gint          depth)
{
  if (node && GTK_CMCTREE_ROW (node)->level == depth)
    gtk_cmctree_collapse_recursive (ctree, node);
}

static void
tree_toggle_expansion (GtkCMCTree     *ctree,
		       GtkCMCTreeNode *node,
		       gpointer      data)
{
  if (!node)
    return;

  if (GTK_CMCTREE_ROW (node)->expanded)
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_COLLAPSE], 0,node);
  else
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_EXPAND], 0,node);
}

static GtkCMCTreeRow *
row_new (GtkCMCTree *ctree)
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
row_delete (GtkCMCTree    *ctree,
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
real_select_row (GtkCMCList *clist,
		 gint      row,
		 gint      column,
		 GdkEvent *event)
{
  GList *node;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));
  
  if ((node = g_list_nth (clist->row_list, row)) &&
      GTK_CMCTREE_ROW (node)->row.selectable)
    g_signal_emit (G_OBJECT (clist), ctree_signals[TREE_SELECT_ROW],0,
		     node, column);
}

static void
real_unselect_row (GtkCMCList *clist,
		   gint      row,
		   gint      column,
		   GdkEvent *event)
{
  GList *node;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  if ((node = g_list_nth (clist->row_list, row)))
    g_signal_emit (G_OBJECT (clist), ctree_signals[TREE_UNSELECT_ROW],0,
		     node, column);
}

static void
tree_draw_node (GtkCMCTree     *ctree, 
               GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  
  clist = GTK_CMCLIST (ctree);

  if (CLIST_UNFROZEN (clist) && gtk_cmctree_is_viewable (ctree, node))
    {
      GtkCMCTreeNode *work;
      gint num = 0;

      work = GTK_CMCTREE_NODE (clist->row_list);
      while (work && work != node)
	{
	  work = GTK_CMCTREE_NODE_NEXT (work);
	  num++;
	}
      if (work && gtk_cmclist_row_is_visible (clist, num) != GTK_VISIBILITY_NONE)
	GTK_CMCLIST_GET_CLASS(ctree)->draw_row
	  (clist, NULL, num, GTK_CMCLIST_ROW ((GList *) node));
    }
}

static void
real_tree_select (GtkCMCTree     *ctree,
		  GtkCMCTreeNode *node,
		  gint          column)
{
  GtkCMCList *clist;
  GList *list;
  GtkCMCTreeNode *sel_row;
  gboolean node_selected;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (!node || GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_SELECTED ||
      !GTK_CMCTREE_ROW (node)->row.selectable)
    return;

  clist = GTK_CMCLIST (ctree);

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_SINGLE:
    case GTK_SELECTION_BROWSE:

      node_selected = FALSE;
      list = clist->selection;

      while (list)
	{
	  sel_row = list->data;
	  list = list->next;
	  
	  if (node == sel_row)
	    node_selected = TRUE;
	  else
	    g_signal_emit (G_OBJECT (ctree),
			     ctree_signals[TREE_UNSELECT_ROW], 0, sel_row, column);
	}

      if (node_selected)
	return;

    default:
      break;
    }

  GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_SELECTED;

  if (!clist->selection)
    {
      clist->selection = g_list_append (clist->selection, node);
      clist->selection_end = clist->selection;
    }
  else
    clist->selection_end = g_list_append (clist->selection_end, node)->next;

  tree_draw_node (ctree, node);
}

static void
real_tree_unselect (GtkCMCTree     *ctree,
		    GtkCMCTreeNode *node,
		    gint          column)
{
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (!node || GTK_CMCTREE_ROW (node)->row.state != GTK_STATE_SELECTED)
    return;

  clist = GTK_CMCLIST (ctree);

  if (clist->selection_end && clist->selection_end->data == node)
    clist->selection_end = clist->selection_end->prev;

  clist->selection = g_list_remove (clist->selection, node);
  
  GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_NORMAL;

  tree_draw_node (ctree, node);
}

static void
select_row_recursive (GtkCMCTree     *ctree, 
		      GtkCMCTreeNode *node, 
		      gpointer      data)
{
  if (!node || GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_SELECTED ||
      !GTK_CMCTREE_ROW (node)->row.selectable)
    return;

  GTK_CMCLIST (ctree)->undo_unselection = 
    g_list_prepend (GTK_CMCLIST (ctree)->undo_unselection, node);
  gtk_cmctree_select (ctree, node);
}

static void
real_select_all (GtkCMCList *clist)
{
  GtkCMCTree *ctree;
  GtkCMCTreeNode *node;
  
  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  ctree = GTK_CMCTREE (clist);

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_SINGLE:
    case GTK_SELECTION_BROWSE:
      return;

    case GTK_SELECTION_MULTIPLE:

      gtk_cmclist_freeze (clist);

      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
	  
      clist->anchor_state = GTK_STATE_SELECTED;
      clist->anchor = -1;
      clist->drag_pos = -1;
      clist->undo_anchor = clist->focus_row;

      for (node = GTK_CMCTREE_NODE (clist->row_list); node;
	   node = GTK_CMCTREE_NODE_NEXT (node))
	gtk_cmctree_pre_recursive (ctree, node, select_row_recursive, NULL);

      gtk_cmclist_thaw (clist);
      break;

    default:
      /* do nothing */
      break;
    }
}

static void
real_unselect_all (GtkCMCList *clist)
{
  GtkCMCTree *ctree;
  GtkCMCTreeNode *node;
  GList *list;
 
  cm_return_if_fail (GTK_IS_CMCTREE (clist));
  
  ctree = GTK_CMCTREE (clist);

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_BROWSE:
      if (clist->focus_row >= 0)
	{
	  gtk_cmctree_select
	    (ctree,
	     GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row)));
	  return;
	}
      break;

    case GTK_SELECTION_MULTIPLE:
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;

      clist->anchor = -1;
      clist->drag_pos = -1;
      clist->undo_anchor = clist->focus_row;
      break;

    default:
      break;
    }

  list = clist->selection;

  while (list)
    {
      node = list->data;
      list = list->next;
      gtk_cmctree_unselect (ctree, node);
    }
}

static gboolean
ctree_is_hot_spot (GtkCMCTree     *ctree, 
		   GtkCMCTreeNode *node,
		   gint          row, 
		   gint          x, 
		   gint          y)
{
  GtkCMCTreeRow *tree_row;
  GtkCMCList *clist;
  gint xl;
  gint yu;
  gint hotspot_size;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);

  clist = GTK_CMCLIST (ctree);

  if (!clist->column[ctree->tree_column].visible ||
      ctree->expander_style == GTK_CMCTREE_EXPANDER_NONE)
    return FALSE;

  tree_row = GTK_CMCTREE_ROW (node);

  hotspot_size = clist->row_height-2;
  if (hotspot_size > clist->column[ctree->tree_column].area.width - 2)
	hotspot_size = clist->column[ctree->tree_column].area.width - 2;

  yu = (ROW_TOP_YPIXEL (clist, row) + (clist->row_height - hotspot_size) / 2 -
	(clist->row_height - 1) % 2);

  if (clist->column[ctree->tree_column].justification == GTK_JUSTIFY_RIGHT)
    xl = (clist->column[ctree->tree_column].area.x + 
	  clist->column[ctree->tree_column].area.width - 1 + clist->hoffset -
	  (tree_row->level - 1) * ctree->tree_indent - hotspot_size);
  else
    xl = (clist->column[ctree->tree_column].area.x + clist->hoffset +
	  (tree_row->level - 1) * ctree->tree_indent);

  return (x >= xl && x <= xl + hotspot_size && y >= yu && y <= yu + hotspot_size);
}

/***********************************************************
 ***********************************************************
 ***                  Public interface                   ***
 ***********************************************************
 ***********************************************************/


/***********************************************************
 *           Creation, insertion, deletion                 *
 ***********************************************************/

static GObject*
gtk_cmctree_constructor (GType                  type,
		       guint                  n_construct_properties,
		       GObjectConstructParam *construct_properties)
{
  GObject *object = G_OBJECT_CLASS (parent_class)->constructor (type,
								n_construct_properties,
								construct_properties);

  return object;
}

GtkWidget*
gtk_cmctree_new_with_titles (gint         columns, 
			   gint         tree_column,
			   gchar       *titles[])
{
  GtkWidget *widget;

  cm_return_val_if_fail (columns > 0, NULL);
  cm_return_val_if_fail (tree_column >= 0 && tree_column < columns, NULL);

  widget = gtk_widget_new (GTK_TYPE_CMCTREE,
			   "n_columns", columns,
			   "tree_column", tree_column,
			   NULL);
  if (titles)
    {
      GtkCMCList *clist = GTK_CMCLIST (widget);
      guint i;

      for (i = 0; i < columns; i++)
	gtk_cmclist_set_column_title (clist, i, titles[i]);
      gtk_cmclist_column_titles_show (clist);
    }

  return widget;
}

GtkWidget *
gtk_cmctree_new (gint columns, 
	       gint tree_column)
{
  return gtk_cmctree_new_with_titles (columns, tree_column, NULL);
}

static gint
real_insert_row (GtkCMCList *clist,
		 gint      row,
		 gchar    *text[])
{
  GtkCMCTreeNode *parent = NULL;
  GtkCMCTreeNode *sibling;
  GtkCMCTreeNode *node;

  cm_return_val_if_fail (GTK_IS_CMCTREE (clist), -1);

  sibling = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, row));
  if (sibling)
    parent = GTK_CMCTREE_ROW (sibling)->parent;

  node = gtk_cmctree_insert_node (GTK_CMCTREE (clist), parent, sibling, text, 5,
				NULL, NULL, TRUE, FALSE);

  if (GTK_CMCLIST_AUTO_SORT (clist) || !sibling)
    return g_list_position (clist->row_list, (GList *) node);
  
  return row;
}

GtkCMCTreeNode * 
gtk_cmctree_insert_node (GtkCMCTree     *ctree,
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
  new_row = row_new (ctree);
  list = g_list_alloc ();
  list->data = new_row;
  node = GTK_CMCTREE_NODE (list);

  if (text)
    for (i = 0; i < clist->columns; i++)
      if (text[i] && i != ctree->tree_column)
	GTK_CMCLIST_GET_CLASS (clist)->set_cell_contents
	  (clist, &(new_row->row), i, GTK_CMCELL_TEXT, text[i], 0, NULL);

  set_node_info (ctree, node, text ?
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

  gtk_cmctree_link (ctree, node, parent, sibling, TRUE);

  if (text && !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist) &&
      gtk_cmctree_is_viewable (ctree, node))
    {
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].auto_resize)
	  column_auto_resize (clist, &(new_row->row), i, 0);
    }

  if (clist->rows == 1)
    {
      clist->focus_row = 0;
      if (clist->selection_mode == GTK_SELECTION_BROWSE)
	gtk_cmctree_select (ctree, node);
    }


  CLIST_REFRESH (clist);

  return node;
}

GtkCMCTreeNode *
gtk_cmctree_insert_gnode (GtkCMCTree          *ctree,
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
  list->data = row_new (ctree);
  cnode = GTK_CMCTREE_NODE (list);

  gtk_cmclist_freeze (clist);

  set_node_info (ctree, cnode, "", 0, NULL, NULL, TRUE, FALSE);

  if (!func (ctree, depth, gnode, cnode, data))
    {
      tree_delete_row (ctree, cnode, NULL);
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

  gtk_cmctree_link (ctree, cnode, parent, sibling, TRUE);

  for (work = g_node_last_child (gnode); work; work = work->prev)
    {
      new_child = gtk_cmctree_insert_gnode (ctree, cnode, child,
					  work, func, data);
      if (new_child)
	child = new_child;
    }	
  
  gtk_cmclist_thaw (clist);

  return cnode;
}

GNode *
gtk_cmctree_export_to_gnode (GtkCMCTree          *ctree,
			   GNode             *parent,
			   GNode             *sibling,
			   GtkCMCTreeNode      *node,
			   GtkCMCTreeGNodeFunc  func,
			   gpointer           data)
{
  GtkCMCTreeNode *work;
  GNode *gnode;
  gint depth;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  cm_return_val_if_fail (node != NULL, NULL);
  cm_return_val_if_fail (func != NULL, NULL);
  if (sibling)
    {
      cm_return_val_if_fail (parent != NULL, NULL);
      cm_return_val_if_fail (sibling->parent == parent, NULL);
    }

  gnode = g_node_new (NULL);
  depth = g_node_depth (parent) + 1;
  
  if (!func (ctree, depth, gnode, node, data))
    {
      g_node_destroy (gnode);
      return NULL;
    }

  if (parent)
    g_node_insert_before (parent, sibling, gnode);

  if (!GTK_CMCTREE_ROW (node)->is_leaf)
    {
      GNode *new_sibling = NULL;

      for (work = GTK_CMCTREE_ROW (node)->children; work;
	   work = GTK_CMCTREE_ROW (work)->sibling)
	new_sibling = gtk_cmctree_export_to_gnode (ctree, gnode, new_sibling,
						 work, func, data);

      g_node_reverse_children (gnode);
    }

  return gnode;
}
  
static void
real_remove_row (GtkCMCList *clist,
		 gint      row)
{
  GtkCMCTreeNode *node;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, row));

  if (node)
    gtk_cmctree_remove_node (GTK_CMCTREE (clist), node);
}

void
gtk_cmctree_remove_node (GtkCMCTree     *ctree, 
		       GtkCMCTreeNode *node)
{
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  gtk_cmclist_freeze (clist);

  if (node)
    {
      gtk_cmctree_unlink (ctree, node, TRUE);
      gtk_cmctree_post_recursive (ctree, node, GTK_CMCTREE_FUNC (tree_delete),
				NULL);
      if (clist->selection_mode == GTK_SELECTION_BROWSE && !clist->selection &&
	  clist->focus_row >= 0)
	gtk_cmclist_select_row (clist, clist->focus_row, -1);

      auto_resize_columns (clist);
    }
  else
    gtk_cmclist_clear (clist);

  gtk_cmclist_thaw (clist);
}

static void
real_clear (GtkCMCList *clist)
{
  GtkCMCTree *ctree;
  GtkCMCTreeNode *work;
  GtkCMCTreeNode *ptr;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  ctree = GTK_CMCTREE (clist);

  /* remove all rows */
  work = GTK_CMCTREE_NODE (clist->row_list);
  clist->row_list = NULL;
  clist->row_list_end = NULL;

  GTK_CMCLIST_SET_FLAG (clist, CMCLIST_AUTO_RESIZE_BLOCKED);
  while (work)
    {
      ptr = work;
      work = GTK_CMCTREE_ROW (work)->sibling;
      gtk_cmctree_post_recursive (ctree, ptr, GTK_CMCTREE_FUNC (tree_delete_row), 
				NULL);
    }
  GTK_CMCLIST_UNSET_FLAG (clist, CMCLIST_AUTO_RESIZE_BLOCKED);

  parent_class->clear (clist);
}


/***********************************************************
 *  Generic recursive functions, querying / finding tree   *
 *  information                                            *
 ***********************************************************/


void
gtk_cmctree_post_recursive (GtkCMCTree     *ctree, 
			  GtkCMCTreeNode *node,
			  GtkCMCTreeFunc  func,
			  gpointer      data)
{
  GtkCMCTreeNode *work;
  GtkCMCTreeNode *tmp;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (func != NULL);

  if (node)
    work = GTK_CMCTREE_ROW (node)->children;
  else
    work = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (work)
    {
      tmp = GTK_CMCTREE_ROW (work)->sibling;
      gtk_cmctree_post_recursive (ctree, work, func, data);
      work = tmp;
    }

  if (node)
    func (ctree, node, data);
}

void
gtk_cmctree_post_recursive_to_depth (GtkCMCTree     *ctree, 
				   GtkCMCTreeNode *node,
				   gint          depth,
				   GtkCMCTreeFunc  func,
				   gpointer      data)
{
  GtkCMCTreeNode *work;
  GtkCMCTreeNode *tmp;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (func != NULL);

  if (depth < 0)
    {
      gtk_cmctree_post_recursive (ctree, node, func, data);
      return;
    }

  if (node)
    work = GTK_CMCTREE_ROW (node)->children;
  else
    work = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  if (work && GTK_CMCTREE_ROW (work)->level <= depth)
    {
      while (work)
	{
	  tmp = GTK_CMCTREE_ROW (work)->sibling;
	  gtk_cmctree_post_recursive_to_depth (ctree, work, depth, func, data);
	  work = tmp;
	}
    }

  if (node && GTK_CMCTREE_ROW (node)->level <= depth)
    func (ctree, node, data);
}

void
gtk_cmctree_pre_recursive (GtkCMCTree     *ctree, 
			 GtkCMCTreeNode *node,
			 GtkCMCTreeFunc  func,
			 gpointer      data)
{
  GtkCMCTreeNode *work;
  GtkCMCTreeNode *tmp;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (func != NULL);

  if (node)
    {
      work = GTK_CMCTREE_ROW (node)->children;
      func (ctree, node, data);
    }
  else
    work = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (work)
    {
      tmp = GTK_CMCTREE_ROW (work)->sibling;
      gtk_cmctree_pre_recursive (ctree, work, func, data);
      work = tmp;
    }
}

void
gtk_cmctree_pre_recursive_to_depth (GtkCMCTree     *ctree, 
				  GtkCMCTreeNode *node,
				  gint          depth, 
				  GtkCMCTreeFunc  func,
				  gpointer      data)
{
  GtkCMCTreeNode *work;
  GtkCMCTreeNode *tmp;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (func != NULL);

  if (depth < 0)
    {
      gtk_cmctree_pre_recursive (ctree, node, func, data);
      return;
    }

  if (node)
    {
      work = GTK_CMCTREE_ROW (node)->children;
      if (GTK_CMCTREE_ROW (node)->level <= depth)
	func (ctree, node, data);
    }
  else
    work = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  if (work && GTK_CMCTREE_ROW (work)->level <= depth)
    {
      while (work)
	{
	  tmp = GTK_CMCTREE_ROW (work)->sibling;
	  gtk_cmctree_pre_recursive_to_depth (ctree, work, depth, func, data);
	  work = tmp;
	}
    }
}

gboolean
gtk_cmctree_is_viewable (GtkCMCTree     *ctree, 
		       GtkCMCTreeNode *node)
{ 
  GtkCMCTreeRow *work;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);

  work = GTK_CMCTREE_ROW (node);

  while (work && work->parent && GTK_CMCTREE_ROW (work->parent)->expanded)
    work = GTK_CMCTREE_ROW (work->parent);

  if (!work->parent)
    return TRUE;

  return FALSE;
}

GtkCMCTreeNode * 
gtk_cmctree_last (GtkCMCTree     *ctree,
		GtkCMCTreeNode *node)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);

  if (!node) 
    return NULL;

  while (GTK_CMCTREE_ROW (node)->sibling)
    node = GTK_CMCTREE_ROW (node)->sibling;
  
  if (GTK_CMCTREE_ROW (node)->children)
    return gtk_cmctree_last (ctree, GTK_CMCTREE_ROW (node)->children);
  
  return node;
}

GtkCMCTreeNode *
gtk_cmctree_find_node_ptr (GtkCMCTree    *ctree,
			 GtkCMCTreeRow *ctree_row)
{
  GtkCMCTreeNode *node;
  
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  cm_return_val_if_fail (ctree_row != NULL, NULL);
  
  if (ctree_row->parent)
    node = GTK_CMCTREE_ROW (ctree_row->parent)->children;
  else
    node = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (GTK_CMCTREE_ROW (node) != ctree_row)
    node = GTK_CMCTREE_ROW (node)->sibling;
  
  return node;
}

GtkCMCTreeNode *
gtk_cmctree_node_nth (GtkCMCTree *ctree,
		    guint     row)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);

  if ((row >= GTK_CMCLIST(ctree)->rows))
    return NULL;
 
  return GTK_CMCTREE_NODE (g_list_nth (GTK_CMCLIST (ctree)->row_list, row));
}

gboolean
gtk_cmctree_find (GtkCMCTree     *ctree,
		GtkCMCTreeNode *node,
		GtkCMCTreeNode *child)
{
  if (!child)
    return FALSE;

  if (!node)
    node = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (node)
    {
      if (node == child) 
	return TRUE;
      if (GTK_CMCTREE_ROW (node)->children)
	{
	  if (gtk_cmctree_find (ctree, GTK_CMCTREE_ROW (node)->children, child))
	    return TRUE;
	}
      node = GTK_CMCTREE_ROW (node)->sibling;
    }
  return FALSE;
}

gboolean
gtk_cmctree_is_ancestor (GtkCMCTree     *ctree,
		       GtkCMCTreeNode *node,
		       GtkCMCTreeNode *child)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);

  if (GTK_CMCTREE_ROW (node)->children)
    return gtk_cmctree_find (ctree, GTK_CMCTREE_ROW (node)->children, child);

  return FALSE;
}

GtkCMCTreeNode *
gtk_cmctree_find_by_row_data (GtkCMCTree     *ctree,
			    GtkCMCTreeNode *node,
			    gpointer      data)
{
  GtkCMCTreeNode *work;
  
  if (!node)
    node = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);
  
  while (node)
    {
      if (GTK_CMCTREE_ROW (node)->row.data == data) 
	return node;
      if (GTK_CMCTREE_ROW (node)->children &&
	  (work = gtk_cmctree_find_by_row_data 
	   (ctree, GTK_CMCTREE_ROW (node)->children, data)))
	return work;
      node = GTK_CMCTREE_ROW (node)->sibling;
    }
  return NULL;
}

GList *
gtk_cmctree_find_all_by_row_data (GtkCMCTree     *ctree,
				GtkCMCTreeNode *node,
				gpointer      data)
{
  GList *list = NULL;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);

  /* if node == NULL then look in the whole tree */
  if (!node)
    node = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (node)
    {
      if (GTK_CMCTREE_ROW (node)->row.data == data)
        list = g_list_append (list, node);

      if (GTK_CMCTREE_ROW (node)->children)
        {
	  GList *sub_list;

          sub_list = gtk_cmctree_find_all_by_row_data (ctree,
						     GTK_CMCTREE_ROW
						     (node)->children,
						     data);
          list = g_list_concat (list, sub_list);
        }
      node = GTK_CMCTREE_ROW (node)->sibling;
    }
  return list;
}

GtkCMCTreeNode *
gtk_cmctree_find_by_row_data_custom (GtkCMCTree     *ctree,
				   GtkCMCTreeNode *node,
				   gpointer      data,
				   GCompareFunc  func)
{
  GtkCMCTreeNode *work;

  cm_return_val_if_fail (func != NULL, NULL);

  if (!node)
    node = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (node)
    {
      if (!func (GTK_CMCTREE_ROW (node)->row.data, data))
	return node;
      if (GTK_CMCTREE_ROW (node)->children &&
	  (work = gtk_cmctree_find_by_row_data_custom
	   (ctree, GTK_CMCTREE_ROW (node)->children, data, func)))
	return work;
      node = GTK_CMCTREE_ROW (node)->sibling;
    }
  return NULL;
}

GList *
gtk_cmctree_find_all_by_row_data_custom (GtkCMCTree     *ctree,
				       GtkCMCTreeNode *node,
				       gpointer      data,
				       GCompareFunc  func)
{
  GList *list = NULL;

  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  cm_return_val_if_fail (func != NULL, NULL);

  /* if node == NULL then look in the whole tree */
  if (!node)
    node = GTK_CMCTREE_NODE (GTK_CMCLIST (ctree)->row_list);

  while (node)
    {
      if (!func (GTK_CMCTREE_ROW (node)->row.data, data))
        list = g_list_append (list, node);

      if (GTK_CMCTREE_ROW (node)->children)
        {
	  GList *sub_list;

          sub_list = gtk_cmctree_find_all_by_row_data_custom (ctree,
							    GTK_CMCTREE_ROW
							    (node)->children,
							    data,
							    func);
          list = g_list_concat (list, sub_list);
        }
      node = GTK_CMCTREE_ROW (node)->sibling;
    }
  return list;
}

gboolean
gtk_cmctree_is_hot_spot (GtkCMCTree *ctree, 
		       gint      x, 
		       gint      y)
{
  GtkCMCTreeNode *node;
  gint column;
  gint row;
  
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);

  if (gtk_cmclist_get_selection_info (GTK_CMCLIST (ctree), x, y, &row, &column))
    if ((node = GTK_CMCTREE_NODE(g_list_nth (GTK_CMCLIST (ctree)->row_list, row))))
      return ctree_is_hot_spot (ctree, node, row, x, y);

  return FALSE;
}


/***********************************************************
 *   Tree signals : move, expand, collapse, (un)select     *
 ***********************************************************/


void
gtk_cmctree_move (GtkCMCTree     *ctree,
		GtkCMCTreeNode *node,
		GtkCMCTreeNode *new_parent, 
		GtkCMCTreeNode *new_sibling)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);
  
  g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_MOVE], 0, node,
		   new_parent, new_sibling);
}

void
gtk_cmctree_expand (GtkCMCTree     *ctree,
		  GtkCMCTreeNode *node)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);
  
  if (GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_EXPAND], 0, node);
}

void 
gtk_cmctree_expand_recursive (GtkCMCTree     *ctree,
			    GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  gboolean thaw = FALSE;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  if (node && GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  if (CLIST_UNFROZEN (clist) && (!node || gtk_cmctree_is_viewable (ctree, node)))
    {
      gtk_cmclist_freeze (clist);
      thaw = TRUE;
    }

  gtk_cmctree_post_recursive (ctree, node, GTK_CMCTREE_FUNC (tree_expand), NULL);

  if (thaw)
    gtk_cmclist_thaw (clist);
}

void 
gtk_cmctree_expand_to_depth (GtkCMCTree     *ctree,
			   GtkCMCTreeNode *node,
			   gint          depth)
{
  GtkCMCList *clist;
  gboolean thaw = FALSE;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  if (node && GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  if (CLIST_UNFROZEN (clist) && (!node || gtk_cmctree_is_viewable (ctree, node)))
    {
      gtk_cmclist_freeze (clist);
      thaw = TRUE;
    }

  gtk_cmctree_post_recursive_to_depth (ctree, node, depth,
				     GTK_CMCTREE_FUNC (tree_expand), NULL);

  if (thaw)
    gtk_cmclist_thaw (clist);
}

void
gtk_cmctree_collapse (GtkCMCTree     *ctree,
		    GtkCMCTreeNode *node)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);
  
  if (GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_COLLAPSE], 0, node);
}

void 
gtk_cmctree_collapse_recursive (GtkCMCTree     *ctree,
			      GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  gboolean thaw = FALSE;
  gint i;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (node && GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  clist = GTK_CMCLIST (ctree);

  if (CLIST_UNFROZEN (clist) && (!node || gtk_cmctree_is_viewable (ctree, node)))
    {
      gtk_cmclist_freeze (clist);
      thaw = TRUE;
    }

  GTK_CMCLIST_SET_FLAG (clist, CMCLIST_AUTO_RESIZE_BLOCKED);
  gtk_cmctree_post_recursive (ctree, node, GTK_CMCTREE_FUNC (tree_collapse), NULL);
  GTK_CMCLIST_UNSET_FLAG (clist, CMCLIST_AUTO_RESIZE_BLOCKED);
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].auto_resize)
      gtk_cmclist_set_column_width (clist, i,
				  gtk_cmclist_optimal_column_width (clist, i));

  if (thaw)
    gtk_cmclist_thaw (clist);
}

void 
gtk_cmctree_collapse_to_depth (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *node,
			     gint          depth)
{
  GtkCMCList *clist;
  gboolean thaw = FALSE;
  gint i;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (node && GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  clist = GTK_CMCLIST (ctree);

  if (CLIST_UNFROZEN (clist) && (!node || gtk_cmctree_is_viewable (ctree, node)))
    {
      gtk_cmclist_freeze (clist);
      thaw = TRUE;
    }

  GTK_CMCLIST_SET_FLAG (clist, CMCLIST_AUTO_RESIZE_BLOCKED);
  gtk_cmctree_post_recursive_to_depth (ctree, node, depth,
				     GTK_CMCTREE_FUNC (tree_collapse_to_depth),
				     GINT_TO_POINTER (depth));
  GTK_CMCLIST_UNSET_FLAG (clist, CMCLIST_AUTO_RESIZE_BLOCKED);
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].auto_resize)
      gtk_cmclist_set_column_width (clist, i,
				  gtk_cmclist_optimal_column_width (clist, i));

  if (thaw)
    gtk_cmclist_thaw (clist);
}

void
gtk_cmctree_toggle_expansion (GtkCMCTree     *ctree,
			    GtkCMCTreeNode *node)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);
  
  if (GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  tree_toggle_expansion (ctree, node, NULL);
}

void 
gtk_cmctree_toggle_expansion_recursive (GtkCMCTree     *ctree,
				      GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  gboolean thaw = FALSE;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  
  if (node && GTK_CMCTREE_ROW (node)->is_leaf)
    return;

  clist = GTK_CMCLIST (ctree);

  if (CLIST_UNFROZEN (clist) && (!node || gtk_cmctree_is_viewable (ctree, node)))
    {
      gtk_cmclist_freeze (clist);
      thaw = TRUE;
    }
  
  gtk_cmctree_post_recursive (ctree, node,
			    GTK_CMCTREE_FUNC (tree_toggle_expansion), NULL);

  if (thaw)
    gtk_cmclist_thaw (clist);
}

void
gtk_cmctree_select (GtkCMCTree     *ctree, 
		  GtkCMCTreeNode *node)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  if (GTK_CMCTREE_ROW (node)->row.selectable)
    g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_SELECT_ROW], 0,
		     node, -1);
}

void
gtk_cmctree_unselect (GtkCMCTree     *ctree, 
		    GtkCMCTreeNode *node)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  g_signal_emit (G_OBJECT (ctree), ctree_signals[TREE_UNSELECT_ROW], 0,
		   node, -1);
}

void
gtk_cmctree_select_recursive (GtkCMCTree     *ctree, 
			    GtkCMCTreeNode *node)
{
  gtk_cmctree_real_select_recursive (ctree, node, TRUE);
}

void
gtk_cmctree_unselect_recursive (GtkCMCTree     *ctree, 
			      GtkCMCTreeNode *node)
{
  gtk_cmctree_real_select_recursive (ctree, node, FALSE);
}

void
gtk_cmctree_real_select_recursive (GtkCMCTree     *ctree, 
				 GtkCMCTreeNode *node, 
				 gint          state)
{
  GtkCMCList *clist;
  gboolean thaw = FALSE;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  if ((state && 
       (clist->selection_mode ==  GTK_SELECTION_BROWSE ||
	clist->selection_mode == GTK_SELECTION_SINGLE)) ||
      (!state && clist->selection_mode ==  GTK_SELECTION_BROWSE))
    return;

  if (CLIST_UNFROZEN (clist) && (!node || gtk_cmctree_is_viewable (ctree, node)))
    {
      gtk_cmclist_freeze (clist);
      thaw = TRUE;
    }

  if (clist->selection_mode == GTK_SELECTION_MULTIPLE)
    {
      GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
      
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
    }

  if (state)
    gtk_cmctree_post_recursive (ctree, node,
			      GTK_CMCTREE_FUNC (tree_select), NULL);
  else 
    gtk_cmctree_post_recursive (ctree, node,
			      GTK_CMCTREE_FUNC (tree_unselect), NULL);
  
  if (thaw)
    gtk_cmclist_thaw (clist);
}


/***********************************************************
 *           Analogons of GtkCMCList functions               *
 ***********************************************************/


void 
gtk_cmctree_node_set_text (GtkCMCTree     *ctree,
			 GtkCMCTreeNode *node,
			 gint          column,
			 const gchar  *text)
{
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return;
  
  clist = GTK_CMCLIST (ctree);

  GTK_CMCLIST_GET_CLASS (clist)->set_cell_contents
    (clist, &(GTK_CMCTREE_ROW (node)->row), column, GTK_CMCELL_TEXT,
     text, 0, NULL);

  tree_draw_node (ctree, node);
}

void 
gtk_cmctree_node_set_pixbuf (GtkCMCTree     *ctree,
			   GtkCMCTreeNode *node,
			   gint          column,
			   GdkPixbuf    *pixbuf)
{
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);
  cm_return_if_fail (pixbuf != NULL);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return;

  g_object_ref (pixbuf);

  clist = GTK_CMCLIST (ctree);

  GTK_CMCLIST_GET_CLASS (clist)->set_cell_contents
    (clist, &(GTK_CMCTREE_ROW (node)->row), column, GTK_CMCELL_PIXBUF,
     NULL, 0, pixbuf);

  tree_draw_node (ctree, node);
}

void 
gtk_cmctree_node_set_pixtext (GtkCMCTree     *ctree,
			    GtkCMCTreeNode *node,
			    gint          column,
			    const gchar  *text,
			    guint8        spacing,
			    GdkPixbuf    *pixbuf)
{
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);
  if (column != ctree->tree_column)
    cm_return_if_fail (pixbuf != NULL);
  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return;

  clist = GTK_CMCLIST (ctree);

  if (pixbuf)
    {
      g_object_ref (pixbuf);
    }

  GTK_CMCLIST_GET_CLASS (clist)->set_cell_contents
    (clist, &(GTK_CMCTREE_ROW (node)->row), column, GTK_CMCELL_PIXTEXT,
     text, spacing, pixbuf);

  tree_draw_node (ctree, node);
}

void 
gtk_cmctree_set_node_info (GtkCMCTree     *ctree,
			 GtkCMCTreeNode *node,
			 const gchar  *text,
			 guint8        spacing,
			 GdkPixbuf    *pixbuf_closed,
			 GdkPixbuf    *pixbuf_opened,
			 gboolean      is_leaf,
			 gboolean      expanded)
{
  gboolean old_leaf;
  gboolean old_expanded;
 
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  old_leaf = GTK_CMCTREE_ROW (node)->is_leaf;
  old_expanded = GTK_CMCTREE_ROW (node)->expanded;

  if (is_leaf && GTK_CMCTREE_ROW (node)->children)
    {
      GtkCMCTreeNode *work;
      GtkCMCTreeNode *ptr;
      
      work = GTK_CMCTREE_ROW (node)->children;
      while (work)
	{
	  ptr = work;
	  work = GTK_CMCTREE_ROW (work)->sibling;
	  gtk_cmctree_remove_node (ctree, ptr);
	}
    }

  set_node_info (ctree, node, text, spacing, pixbuf_closed,
		 pixbuf_opened, is_leaf, expanded);

  if (!is_leaf && !old_leaf)
    {
      GTK_CMCTREE_ROW (node)->expanded = old_expanded;
      if (expanded && !old_expanded)
	gtk_cmctree_expand (ctree, node);
      else if (!expanded && old_expanded)
	gtk_cmctree_collapse (ctree, node);
    }

  GTK_CMCTREE_ROW (node)->expanded = (is_leaf) ? FALSE : expanded;
  
  tree_draw_node (ctree, node);
}

void
gtk_cmctree_node_set_shift (GtkCMCTree     *ctree,
			  GtkCMCTreeNode *node,
			  gint          column,
			  gint          vertical,
			  gint          horizontal)
{
  GtkCMCList *clist;
  GtkRequisition requisition;
  gboolean visible = FALSE;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return;

  clist = GTK_CMCLIST (ctree);

  if (clist->column[column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    {
      visible = gtk_cmctree_is_viewable (ctree, node);
      if (visible)
	GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
	  (clist, &GTK_CMCTREE_ROW (node)->row, column, &requisition);
    }

  GTK_CMCTREE_ROW (node)->row.cell[column].vertical   = vertical;
  GTK_CMCTREE_ROW (node)->row.cell[column].horizontal = horizontal;

  if (visible)
    column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row,
			column, requisition.width);

  tree_draw_node (ctree, node);
}

static void
remove_grab (GtkCMCList *clist)
{
  if (gdk_display_pointer_is_grabbed (gtk_widget_get_display (GTK_WIDGET (clist))) && 
      gtk_widget_has_grab (GTK_WIDGET(clist)))
    {
      gtk_grab_remove (GTK_WIDGET (clist));
      gdk_display_pointer_ungrab (gtk_widget_get_display (GTK_WIDGET (clist)),
				  GDK_CURRENT_TIME);
    }

  if (clist->htimer)
    {
      g_source_remove (clist->htimer);
      clist->htimer = 0;
    }

  if (clist->vtimer)
    {
      g_source_remove (clist->vtimer);
      clist->vtimer = 0;
    }
}

void
gtk_cmctree_node_set_selectable (GtkCMCTree     *ctree,
			       GtkCMCTreeNode *node,
			       gboolean      selectable)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  if (selectable == GTK_CMCTREE_ROW (node)->row.selectable)
    return;

  GTK_CMCTREE_ROW (node)->row.selectable = selectable;

  if (!selectable && GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_SELECTED)
    {
      GtkCMCList *clist;

      clist = GTK_CMCLIST (ctree);

      if (clist->anchor >= 0 &&
	  clist->selection_mode == GTK_SELECTION_MULTIPLE)
	{
	  clist->drag_button = 0;
	  remove_grab (clist);

	  GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
	}
      gtk_cmctree_unselect (ctree, node);
    }      
}

gboolean
gtk_cmctree_node_get_selectable (GtkCMCTree     *ctree,
			       GtkCMCTreeNode *node)
{
  cm_return_val_if_fail (node != NULL, FALSE);

  return GTK_CMCTREE_ROW (node)->row.selectable;
}

GtkCMCellType 
gtk_cmctree_node_get_cell_type (GtkCMCTree     *ctree,
			      GtkCMCTreeNode *node,
			      gint          column)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), -1);
  cm_return_val_if_fail (node != NULL, -1);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return -1;

  return GTK_CMCTREE_ROW (node)->row.cell[column].type;
}

gboolean
gtk_cmctree_node_get_text (GtkCMCTree      *ctree,
			 GtkCMCTreeNode  *node,
			 gint           column,
			 gchar        **text)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return FALSE;

  if (GTK_CMCTREE_ROW (node)->row.cell[column].type != GTK_CMCELL_TEXT)
    return FALSE;

  if (text)
    *text = GTK_CMCELL_TEXT (GTK_CMCTREE_ROW (node)->row.cell[column])->text;

  return TRUE;
}

gboolean
gtk_cmctree_node_get_pixbuf (GtkCMCTree     *ctree,
			   GtkCMCTreeNode *node,
			   gint          column,
			   GdkPixbuf   **pixbuf)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return FALSE;

  if (GTK_CMCTREE_ROW (node)->row.cell[column].type != GTK_CMCELL_PIXBUF)
    return FALSE;

  if (pixbuf)
    *pixbuf = GTK_CMCELL_PIXBUF (GTK_CMCTREE_ROW (node)->row.cell[column])->pixbuf;

  return TRUE;
}

gboolean
gtk_cmctree_node_get_pixtext (GtkCMCTree      *ctree,
			    GtkCMCTreeNode  *node,
			    gint           column,
			    gchar        **text,
			    guint8        *spacing,
			    GdkPixbuf    **pixbuf)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);
  
  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return FALSE;
  
  if (GTK_CMCTREE_ROW (node)->row.cell[column].type != GTK_CMCELL_PIXTEXT)
    return FALSE;
  
  if (text)
    *text = GTK_CMCELL_PIXTEXT (GTK_CMCTREE_ROW (node)->row.cell[column])->text;
  if (spacing)
    *spacing = GTK_CMCELL_PIXTEXT (GTK_CMCTREE_ROW 
				 (node)->row.cell[column])->spacing;
  if (pixbuf)
    *pixbuf = GTK_CMCELL_PIXTEXT (GTK_CMCTREE_ROW 
				(node)->row.cell[column])->pixbuf;
  
  return TRUE;
}

gboolean
gtk_cmctree_get_node_info (GtkCMCTree      *ctree,
			 GtkCMCTreeNode  *node,
			 gchar        **text,
			 guint8        *spacing,
			 GdkPixbuf    **pixbuf_closed,
			 GdkPixbuf    **pixbuf_opened,
			 gboolean      *is_leaf,
			 gboolean      *expanded)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);
  cm_return_val_if_fail (node != NULL, FALSE);
  
  if (text)
    *text = GTK_CMCELL_PIXTEXT 
      (GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->text;
  if (spacing)
    *spacing = GTK_CMCELL_PIXTEXT 
      (GTK_CMCTREE_ROW (node)->row.cell[ctree->tree_column])->spacing;
  if (pixbuf_closed)
    *pixbuf_closed = GTK_CMCTREE_ROW (node)->pixbuf_closed;
  if (pixbuf_opened)
    *pixbuf_opened = GTK_CMCTREE_ROW (node)->pixbuf_opened;
  if (is_leaf)
    *is_leaf = GTK_CMCTREE_ROW (node)->is_leaf;
  if (expanded)
    *expanded = GTK_CMCTREE_ROW (node)->expanded;
  
  return TRUE;
}

void
gtk_cmctree_node_set_cell_style (GtkCMCTree     *ctree,
			       GtkCMCTreeNode *node,
			       gint          column,
			       GtkStyle     *style)
{
  GtkCMCList *clist;
  GtkRequisition requisition;
  gboolean visible = FALSE;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  clist = GTK_CMCLIST (ctree);

  if (column < 0 || column >= clist->columns)
    return;

  if (GTK_CMCTREE_ROW (node)->row.cell[column].style == style)
    return;

  if (clist->column[column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    {
      visible = gtk_cmctree_is_viewable (ctree, node);
      if (visible)
	GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
	  (clist, &GTK_CMCTREE_ROW (node)->row, column, &requisition);
    }

  if (GTK_CMCTREE_ROW (node)->row.cell[column].style)
    {
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
        gtk_style_detach (GTK_CMCTREE_ROW (node)->row.cell[column].style);
      g_object_unref (GTK_CMCTREE_ROW (node)->row.cell[column].style);
    }

  GTK_CMCTREE_ROW (node)->row.cell[column].style = style;

  if (GTK_CMCTREE_ROW (node)->row.cell[column].style)
    {
      g_object_ref (GTK_CMCTREE_ROW (node)->row.cell[column].style);
      
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
        GTK_CMCTREE_ROW (node)->row.cell[column].style =
	  gtk_style_attach (GTK_CMCTREE_ROW (node)->row.cell[column].style,
			    clist->clist_window);
    }

  if (visible)
    column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row, column,
			requisition.width);

  tree_draw_node (ctree, node);
}

GtkStyle *
gtk_cmctree_node_get_cell_style (GtkCMCTree     *ctree,
			       GtkCMCTreeNode *node,
			       gint          column)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  cm_return_val_if_fail (node != NULL, NULL);

  if (column < 0 || column >= GTK_CMCLIST (ctree)->columns)
    return NULL;

  return GTK_CMCTREE_ROW (node)->row.cell[column].style;
}

void
gtk_cmctree_node_set_row_style (GtkCMCTree     *ctree,
			      GtkCMCTreeNode *node,
			      GtkStyle     *style)
{
  GtkCMCList *clist;
  GtkRequisition requisition;
  gboolean visible;
  gint *old_width = NULL;
  gint i;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  clist = GTK_CMCLIST (ctree);

  if (GTK_CMCTREE_ROW (node)->row.style == style)
    return;
  
  visible = gtk_cmctree_is_viewable (ctree, node);
  if (visible && !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    {
      old_width = g_new (gint, clist->columns);
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].auto_resize)
	  {
	    GTK_CMCLIST_GET_CLASS (clist)->cell_size_request
	      (clist, &GTK_CMCTREE_ROW (node)->row, i, &requisition);
	    old_width[i] = requisition.width;
	  }
    }

  if (GTK_CMCTREE_ROW (node)->row.style)
    {
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
        gtk_style_detach (GTK_CMCTREE_ROW (node)->row.style);
      g_object_unref (GTK_CMCTREE_ROW (node)->row.style);
    }

  GTK_CMCTREE_ROW (node)->row.style = style;

  if (GTK_CMCTREE_ROW (node)->row.style)
    {
      g_object_ref (GTK_CMCTREE_ROW (node)->row.style);
      
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
        GTK_CMCTREE_ROW (node)->row.style =
	  gtk_style_attach (GTK_CMCTREE_ROW (node)->row.style,
			    clist->clist_window);
    }

  if (visible && !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    {
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].auto_resize)
	  column_auto_resize (clist, &GTK_CMCTREE_ROW (node)->row, i,
			      old_width[i]);
      g_free (old_width);
    }
  tree_draw_node (ctree, node);
}

GtkStyle *
gtk_cmctree_node_get_row_style (GtkCMCTree     *ctree,
			      GtkCMCTreeNode *node)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);
  cm_return_val_if_fail (node != NULL, NULL);

  return GTK_CMCTREE_ROW (node)->row.style;
}

void
gtk_cmctree_node_set_foreground (GtkCMCTree       *ctree,
			       GtkCMCTreeNode   *node,
			       const GdkColor *color)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  if (color)
    {
      GTK_CMCTREE_ROW (node)->row.foreground = *color;
      GTK_CMCTREE_ROW (node)->row.fg_set = TRUE;
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
	gdk_colormap_alloc_color (gtk_widget_get_colormap (GTK_WIDGET (ctree)),
			 &GTK_CMCTREE_ROW (node)->row.foreground, TRUE, TRUE);
    }
  else
    GTK_CMCTREE_ROW (node)->row.fg_set = FALSE;

  tree_draw_node (ctree, node);
}

void
gtk_cmctree_node_set_background (GtkCMCTree       *ctree,
			       GtkCMCTreeNode   *node,
			       const GdkColor *color)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  if (color)
    {
      GTK_CMCTREE_ROW (node)->row.background = *color;
      GTK_CMCTREE_ROW (node)->row.bg_set = TRUE;
      if (gtk_widget_get_realized (GTK_WIDGET(ctree)))
	gdk_colormap_alloc_color (gtk_widget_get_colormap (GTK_WIDGET (ctree)),
			 &GTK_CMCTREE_ROW (node)->row.background, TRUE, TRUE);
    }
  else
    GTK_CMCTREE_ROW (node)->row.bg_set = FALSE;

  tree_draw_node (ctree, node);
}

void
gtk_cmctree_node_set_row_data (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *node,
			     gpointer      data)
{
  gtk_cmctree_node_set_row_data_full (ctree, node, data, NULL);
}

void
gtk_cmctree_node_set_row_data_full (GtkCMCTree         *ctree,
				  GtkCMCTreeNode     *node,
				  gpointer          data,
				  GDestroyNotify  destroy)
{
  GDestroyNotify dnotify;
  gpointer ddata;
  
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (node != NULL);

  dnotify = GTK_CMCTREE_ROW (node)->row.destroy;
  ddata = GTK_CMCTREE_ROW (node)->row.data;
  
  GTK_CMCTREE_ROW (node)->row.data = data;
  GTK_CMCTREE_ROW (node)->row.destroy = destroy;

  if (dnotify)
    dnotify (ddata);
}

gpointer
gtk_cmctree_node_get_row_data (GtkCMCTree     *ctree,
			     GtkCMCTreeNode *node)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), NULL);

  return node ? GTK_CMCTREE_ROW (node)->row.data : NULL;
}

void
gtk_cmctree_node_moveto (GtkCMCTree     *ctree,
		       GtkCMCTreeNode *node,
		       gint          column,
		       gfloat        row_align,
		       gfloat        col_align)
{
  gint row = -1;
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  while (node && !gtk_cmctree_is_viewable (ctree, node))
    node = GTK_CMCTREE_ROW (node)->parent;

  if (node)
    row = g_list_position (clist->row_list, (GList *)node);
  
  gtk_cmclist_moveto (clist, row, column, row_align, col_align);
}

GtkVisibility 
gtk_cmctree_node_is_visible (GtkCMCTree     *ctree,
                           GtkCMCTreeNode *node)
{
  gint row;
  
  cm_return_val_if_fail (ctree != NULL, 0);
  cm_return_val_if_fail (node != NULL, 0);
  
  row = g_list_position (GTK_CMCLIST (ctree)->row_list, (GList*) node);
  return gtk_cmclist_row_is_visible (GTK_CMCLIST (ctree), row);
}


/***********************************************************
 *             GtkCMCTree specific functions                 *
 ***********************************************************/

void
gtk_cmctree_set_indent (GtkCMCTree *ctree, 
                      gint      indent)
{
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (indent >= 0);

  if (indent == ctree->tree_indent)
    return;

  clist = GTK_CMCLIST (ctree);
  ctree->tree_indent = indent;

  if (clist->column[ctree->tree_column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    gtk_cmclist_set_column_width
      (clist, ctree->tree_column,
       gtk_cmclist_optimal_column_width (clist, ctree->tree_column));
  else
    CLIST_REFRESH (ctree);
}

void
gtk_cmctree_set_spacing (GtkCMCTree *ctree, 
		       gint      spacing)
{
  GtkCMCList *clist;
  gint old_spacing;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));
  cm_return_if_fail (spacing >= 0);

  if (spacing == ctree->tree_spacing)
    return;

  clist = GTK_CMCLIST (ctree);

  old_spacing = ctree->tree_spacing;
  ctree->tree_spacing = spacing;

  if (clist->column[ctree->tree_column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    gtk_cmclist_set_column_width (clist, ctree->tree_column,
				clist->column[ctree->tree_column].width +
				spacing - old_spacing);
  else
    CLIST_REFRESH (ctree);
}

void
gtk_cmctree_set_show_stub (GtkCMCTree *ctree, 
			 gboolean  show_stub)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  show_stub = show_stub != FALSE;

  if (show_stub != ctree->show_stub)
    {
      GtkCMCList *clist;

      clist = GTK_CMCLIST (ctree);
      ctree->show_stub = show_stub;

      if (CLIST_UNFROZEN (clist) && clist->rows &&
	  gtk_cmclist_row_is_visible (clist, 0) != GTK_VISIBILITY_NONE)
	GTK_CMCLIST_GET_CLASS (clist)->draw_row
	  (clist, NULL, 0, GTK_CMCLIST_ROW (clist->row_list));
    }
}

void 
gtk_cmctree_set_line_style (GtkCMCTree          *ctree, 
			  GtkCMCTreeLineStyle  line_style)
{
}

void 
gtk_cmctree_set_expander_style (GtkCMCTree              *ctree, 
			      GtkCMCTreeExpanderStyle  expander_style)
{
  GtkCMCList *clist;
  GtkCMCTreeExpanderStyle old_style;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  if (expander_style == ctree->expander_style)
    return;

  clist = GTK_CMCLIST (ctree);

  old_style = ctree->expander_style;
  ctree->expander_style = expander_style;

  if (clist->column[ctree->tree_column].auto_resize &&
      !GTK_CMCLIST_AUTO_RESIZE_BLOCKED (clist))
    {
      gint new_width;

      new_width = clist->column[ctree->tree_column].width;
      switch (old_style)
	{
	case GTK_CMCTREE_EXPANDER_NONE:
	  break;
	case GTK_CMCTREE_EXPANDER_TRIANGLE:
	  new_width -= PM_SIZE + 3;
	  break;
	}

      switch (expander_style)
	{
	case GTK_CMCTREE_EXPANDER_NONE:
	  break;
	case GTK_CMCTREE_EXPANDER_TRIANGLE:
	  new_width += PM_SIZE + 3;
	  break;
	}

      gtk_cmclist_set_column_width (clist, ctree->tree_column, new_width);
    }

  if (gtk_widget_is_drawable (GTK_WIDGET(clist)))
    CLIST_REFRESH (clist);
}


/***********************************************************
 *             Tree sorting functions                      *
 ***********************************************************/


static void
tree_sort (GtkCMCTree     *ctree,
	   GtkCMCTreeNode *node,
	   gpointer      data)
{
  GtkCMCTreeNode *list_start;
  GtkCMCTreeNode *cmp;
  GtkCMCTreeNode *work;
  GtkCMCList *clist;

  clist = GTK_CMCLIST (ctree);

  if (node)
    list_start = GTK_CMCTREE_ROW (node)->children;
  else
    list_start = GTK_CMCTREE_NODE (clist->row_list);

  while (list_start)
    {
      cmp = list_start;
      work = GTK_CMCTREE_ROW (cmp)->sibling;
      while (work)
	{
	  if (clist->sort_type == GTK_SORT_ASCENDING)
	    {
	      if (clist->compare 
		  (clist, GTK_CMCTREE_ROW (work), GTK_CMCTREE_ROW (cmp)) < 0)
		cmp = work;
	    }
	  else
	    {
	      if (clist->compare 
		  (clist, GTK_CMCTREE_ROW (work), GTK_CMCTREE_ROW (cmp)) > 0)
		cmp = work;
	    }
	  work = GTK_CMCTREE_ROW (work)->sibling;
	}
      if (cmp == list_start)
	list_start = GTK_CMCTREE_ROW (cmp)->sibling;
      else
	{
	  gtk_cmctree_unlink (ctree, cmp, FALSE);
	  gtk_cmctree_link (ctree, cmp, node, list_start, FALSE);
	}
    }
}

void
gtk_cmctree_sort_recursive (GtkCMCTree     *ctree, 
			  GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *focus_node = NULL;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  gtk_cmclist_freeze (clist);

  if (clist->selection_mode == GTK_SELECTION_MULTIPLE)
    {
      GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
      
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
    }

  if (!node || (node && gtk_cmctree_is_viewable (ctree, node)))
    focus_node =
      GTK_CMCTREE_NODE (g_list_nth (clist->row_list, clist->focus_row));
      
  gtk_cmctree_post_recursive (ctree, node, GTK_CMCTREE_FUNC (tree_sort), NULL);

  if (!node)
    tree_sort (ctree, NULL, NULL);

  if (focus_node)
    {
      clist->focus_row = g_list_position (clist->row_list,(GList *)focus_node);
      clist->undo_anchor = clist->focus_row;
    }

  gtk_cmclist_thaw (clist);
}

static void
real_sort_list (GtkCMCList *clist)
{
  gtk_cmctree_sort_recursive (GTK_CMCTREE (clist), NULL);
}

void
gtk_cmctree_sort_node (GtkCMCTree     *ctree, 
		     GtkCMCTreeNode *node)
{
  GtkCMCList *clist;
  GtkCMCTreeNode *focus_node = NULL;

  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  clist = GTK_CMCLIST (ctree);

  gtk_cmclist_freeze (clist);

  if (clist->selection_mode == GTK_SELECTION_MULTIPLE)
    {
      GTK_CMCLIST_GET_CLASS (clist)->resync_selection (clist, NULL);
      
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
    }

  if (!node || (node && gtk_cmctree_is_viewable (ctree, node)))
    focus_node = GTK_CMCTREE_NODE
      (g_list_nth (clist->row_list, clist->focus_row));

  tree_sort (ctree, node, NULL);

  if (focus_node)
    {
      clist->focus_row = g_list_position (clist->row_list,(GList *)focus_node);
      clist->undo_anchor = clist->focus_row;
    }

  gtk_cmclist_thaw (clist);
}

/************************************************************************/

static void
fake_unselect_all (GtkCMCList *clist,
		   gint      row)
{
  GList *list;
  GList *focus_node = NULL;

  if (row >= 0 && (focus_node = g_list_nth (clist->row_list, row)))
    {
      if (GTK_CMCTREE_ROW (focus_node)->row.state == GTK_STATE_NORMAL &&
	  GTK_CMCTREE_ROW (focus_node)->row.selectable)
	{
	  GTK_CMCTREE_ROW (focus_node)->row.state = GTK_STATE_SELECTED;
	  
	  if (CLIST_UNFROZEN (clist) &&
	      gtk_cmclist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	    GTK_CMCLIST_GET_CLASS (clist)->draw_row (clist, NULL, row,
						  GTK_CMCLIST_ROW (focus_node));
	}  
    }

  clist->undo_selection = clist->selection;
  clist->selection = NULL;
  clist->selection_end = NULL;
  
  for (list = clist->undo_selection; list; list = list->next)
    {
      if (list->data == focus_node)
	continue;

      GTK_CMCTREE_ROW ((GList *)(list->data))->row.state = GTK_STATE_NORMAL;
      tree_draw_node (GTK_CMCTREE (clist), GTK_CMCTREE_NODE (list->data));
    }
}

static GList *
selection_find (GtkCMCList *clist,
		gint      row_number,
		GList    *row_list_element)
{
  return g_list_find (clist->selection, row_list_element);
}

static void
resync_selection (GtkCMCList *clist, GdkEvent *event)
{
  GtkCMCTree *ctree;
  GList *list;
  GtkCMCTreeNode *node;
  gint i;
  gint e;
  gint row;
  gboolean unselect;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  if (clist->selection_mode != GTK_SELECTION_MULTIPLE)
    return;

  if (clist->anchor < 0 || clist->drag_pos < 0)
    return;

  ctree = GTK_CMCTREE (clist);
  
  clist->freeze_count++;

  i = MIN (clist->anchor, clist->drag_pos);
  e = MAX (clist->anchor, clist->drag_pos);

  if (clist->undo_selection)
    {
      list = clist->selection;
      clist->selection = clist->undo_selection;
      clist->selection_end = g_list_last (clist->selection);
      clist->undo_selection = list;
      list = clist->selection;

      while (list)
	{
	  node = list->data;
	  list = list->next;
	  
	  unselect = TRUE;

	  if (gtk_cmctree_is_viewable (ctree, node))
	    {
	      row = g_list_position (clist->row_list, (GList *)node);
	      if (row >= i && row <= e)
		unselect = FALSE;
	    }
	  if (unselect && GTK_CMCTREE_ROW (node)->row.selectable)
	    {
	      GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_SELECTED;
	      gtk_cmctree_unselect (ctree, node);
	      clist->undo_selection = g_list_prepend (clist->undo_selection,
						      node);
	    }
	}
    }    

  if (clist->anchor < clist->drag_pos)
    {
      for (node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, i)); i <= e;
	   i++, node = GTK_CMCTREE_NODE_NEXT (node))
	if (GTK_CMCTREE_ROW (node)->row.selectable)
	  {
	    if (g_list_find (clist->selection, node))
	      {
		if (GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_NORMAL)
		  {
		    GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_SELECTED;
		    gtk_cmctree_unselect (ctree, node);
		    clist->undo_selection =
		      g_list_prepend (clist->undo_selection, node);
		  }
	      }
	    else if (GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_SELECTED)
	      {
		GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_NORMAL;
		clist->undo_unselection =
		  g_list_prepend (clist->undo_unselection, node);
	      }
	  }
    }
  else
    {
      for (node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list, e)); i <= e;
	   e--, node = GTK_CMCTREE_NODE_PREV (node))
	if (GTK_CMCTREE_ROW (node)->row.selectable)
	  {
	    if (g_list_find (clist->selection, node))
	      {
		if (GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_NORMAL)
		  {
		    GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_SELECTED;
		    gtk_cmctree_unselect (ctree, node);
		    clist->undo_selection =
		      g_list_prepend (clist->undo_selection, node);
		  }
	      }
	    else if (GTK_CMCTREE_ROW (node)->row.state == GTK_STATE_SELECTED)
	      {
		GTK_CMCTREE_ROW (node)->row.state = GTK_STATE_NORMAL;
		clist->undo_unselection =
		  g_list_prepend (clist->undo_unselection, node);
	      }
	  }
    }

  clist->undo_unselection = g_list_reverse (clist->undo_unselection);
  for (list = clist->undo_unselection; list; list = list->next)
    gtk_cmctree_select (ctree, list->data);

  clist->anchor = -1;
  clist->drag_pos = -1;

  if (!CLIST_UNFROZEN (clist))
    clist->freeze_count--;
}

static void
real_undo_selection (GtkCMCList *clist)
{
  GtkCMCTree *ctree;
  GList *work;

  cm_return_if_fail (GTK_IS_CMCTREE (clist));

  if (clist->selection_mode != GTK_SELECTION_MULTIPLE)
    return;

  if (!(clist->undo_selection || clist->undo_unselection))
    {
      gtk_cmclist_unselect_all (clist);
      return;
    }

  ctree = GTK_CMCTREE (clist);

  for (work = clist->undo_selection; work; work = work->next)
    if (GTK_CMCTREE_ROW (work->data)->row.selectable)
      gtk_cmctree_select (ctree, GTK_CMCTREE_NODE (work->data));

  for (work = clist->undo_unselection; work; work = work->next)
    if (GTK_CMCTREE_ROW (work->data)->row.selectable)
      gtk_cmctree_unselect (ctree, GTK_CMCTREE_NODE (work->data));

  if (gtk_widget_has_focus (GTK_WIDGET(clist)) &&
      clist->focus_row != clist->undo_anchor)
    {
      clist->focus_row = clist->undo_anchor;
      gtk_widget_queue_draw (GTK_WIDGET (clist));
    }
  else
    clist->focus_row = clist->undo_anchor;
  
  clist->undo_anchor = -1;
 
  g_list_free (clist->undo_selection);
  g_list_free (clist->undo_unselection);
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;

  if (ROW_TOP_YPIXEL (clist, clist->focus_row) + clist->row_height >
      clist->clist_window_height)
    gtk_cmclist_moveto (clist, clist->focus_row, -1, 1, 0);
  else if (ROW_TOP_YPIXEL (clist, clist->focus_row) < 0)
    gtk_cmclist_moveto (clist, clist->focus_row, -1, 0, 0);

}

void
gtk_cmctree_set_drag_compare_func (GtkCMCTree                *ctree,
				 GtkCMCTreeCompareDragFunc  cmp_func)
{
  cm_return_if_fail (GTK_IS_CMCTREE (ctree));

  ctree->drag_compare = cmp_func;
}

static gboolean
check_drag (GtkCMCTree        *ctree,
	    GtkCMCTreeNode    *drag_source,
	    GtkCMCTreeNode    *drag_target,
	    GtkCMCListDragPos  insert_pos)
{
  cm_return_val_if_fail (GTK_IS_CMCTREE (ctree), FALSE);

  if (drag_source && drag_source != drag_target &&
      (!GTK_CMCTREE_ROW (drag_source)->children ||
       !gtk_cmctree_is_ancestor (ctree, drag_source, drag_target)))
    {
      switch (insert_pos)
	{
	case GTK_CMCLIST_DRAG_NONE:
	  return FALSE;
	case GTK_CMCLIST_DRAG_AFTER:
	  if (GTK_CMCTREE_ROW (drag_target)->sibling != drag_source)
	    return (!ctree->drag_compare ||
		    ctree->drag_compare (ctree,
					 drag_source,
					 GTK_CMCTREE_ROW (drag_target)->parent,
					 GTK_CMCTREE_ROW (drag_target)->sibling));
	  break;
	case GTK_CMCLIST_DRAG_BEFORE:
	  if (GTK_CMCTREE_ROW (drag_source)->sibling != drag_target)
	    return (!ctree->drag_compare ||
		    ctree->drag_compare (ctree,
					 drag_source,
					 GTK_CMCTREE_ROW (drag_target)->parent,
					 drag_target));
	  break;
	case GTK_CMCLIST_DRAG_INTO:
	  if (!GTK_CMCTREE_ROW (drag_target)->is_leaf &&
	      GTK_CMCTREE_ROW (drag_target)->children != drag_source)
	    return (!ctree->drag_compare ||
		    ctree->drag_compare (ctree,
					 drag_source,
					 drag_target,
					 GTK_CMCTREE_ROW (drag_target)->children));
	  break;
	}
    }
  return FALSE;
}



/************************************/
static void
drag_dest_info_destroy (gpointer data)
{
  GtkCMCListDestInfo *info = data;

  g_free (info);
}

static void
drag_dest_cell (GtkCMCList         *clist,
		gint              x,
		gint              y,
		GtkCMCListDestInfo *dest_info)
{
  GtkStyle *style;
  GtkWidget *widget;
  guint border_width;

  widget = GTK_WIDGET (clist);
  style = gtk_widget_get_style (widget);

  dest_info->insert_pos = GTK_CMCLIST_DRAG_NONE;

  border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));
  y -= (border_width +
	style->ythickness + clist->column_title_area.height);
  dest_info->cell.row = ROW_FROM_YPIXEL (clist, y);

  if (dest_info->cell.row >= clist->rows)
    {
      dest_info->cell.row = clist->rows - 1;
      y = ROW_TOP_YPIXEL (clist, dest_info->cell.row) + clist->row_height;
    }
  if (dest_info->cell.row < -1)
    dest_info->cell.row = -1;
  
  x -= border_width + style->xthickness;

  dest_info->cell.column = COLUMN_FROM_XPIXEL (clist, x);

  if (dest_info->cell.row >= 0)
    {
      gint y_delta;
      gint h = 0;

      y_delta = y - ROW_TOP_YPIXEL (clist, dest_info->cell.row);
      
      if (GTK_CMCLIST_DRAW_DRAG_RECT(clist) &&
	  !GTK_CMCTREE_ROW (g_list_nth (clist->row_list,
				      dest_info->cell.row))->is_leaf)
	{
	  dest_info->insert_pos = GTK_CMCLIST_DRAG_INTO;
	  h = clist->row_height / 4;
	}
      else if (GTK_CMCLIST_DRAW_DRAG_LINE(clist))
	{
	  dest_info->insert_pos = GTK_CMCLIST_DRAG_BEFORE;
	  h = clist->row_height / 2;
	}

      if (GTK_CMCLIST_DRAW_DRAG_LINE(clist))
	{
	  if (y_delta < h)
	    dest_info->insert_pos = GTK_CMCLIST_DRAG_BEFORE;
	  else if (clist->row_height - y_delta < h)
	    dest_info->insert_pos = GTK_CMCLIST_DRAG_AFTER;
	}
    }
}

static void
gtk_cmctree_drag_begin (GtkWidget	     *widget,
		      GdkDragContext *context)
{
  GtkCMCList *clist;
  gboolean use_icons;

  cm_return_if_fail (GTK_IS_CMCTREE (widget));
  cm_return_if_fail (context != NULL);

  clist = GTK_CMCLIST (widget);

  use_icons = GTK_CMCLIST_USE_DRAG_ICONS (clist);
  GTK_CMCLIST_UNSET_FLAG (clist, CMCLIST_USE_DRAG_ICONS);
  GTK_WIDGET_CLASS (parent_class)->drag_begin (widget, context);

  if (use_icons)
    {
      GTK_CMCLIST_SET_FLAG (clist, CMCLIST_USE_DRAG_ICONS);
      gtk_drag_set_icon_default (context);
    }
}

static gint
gtk_cmctree_drag_motion (GtkWidget      *widget,
		       GdkDragContext *context,
		       gint            x,
		       gint            y,
		       guint           time)
{
  GtkCMCList *clist;
  GtkCMCTree *ctree;
  GtkCMCListDestInfo new_info;
  GtkCMCListDestInfo *dest_info;

  cm_return_val_if_fail (GTK_IS_CMCTREE (widget), FALSE);

  clist = GTK_CMCLIST (widget);
  ctree = GTK_CMCTREE (widget);

  dest_info = g_dataset_get_data (context, "gtk-clist-drag-dest");

  if (!dest_info)
    {
      dest_info = g_new (GtkCMCListDestInfo, 1);
	  
      dest_info->cell.row    = -1;
      dest_info->cell.column = -1;
      dest_info->insert_pos  = GTK_CMCLIST_DRAG_NONE;

      g_dataset_set_data_full (context, "gtk-clist-drag-dest", dest_info,
			       drag_dest_info_destroy);
    }

  drag_dest_cell (clist, x, y, &new_info);

  if (GTK_CMCLIST_REORDERABLE (clist))
    {
      GdkAtom atom = gdk_atom_intern_static_string ("gtk-clist-drag-reorder");
      GdkAtom found = gtk_drag_dest_find_target(widget, context, NULL);

      if (atom == found)
	{
	  GtkCMCTreeNode *drag_source;
	  GtkCMCTreeNode *drag_target;

	  drag_source = GTK_CMCTREE_NODE (g_list_nth (clist->row_list,
						    clist->click_cell.row));
	  drag_target = GTK_CMCTREE_NODE (g_list_nth (clist->row_list,
						    new_info.cell.row));

	  if (gtk_drag_get_source_widget (context) != widget ||
	      !check_drag (ctree, drag_source, drag_target,
			   new_info.insert_pos))
	    {
	      if (dest_info->cell.row < 0)
		{
		  gdk_drag_status (context, GDK_ACTION_DEFAULT, time);
		  return FALSE;
		}
	      return TRUE;
	    }

	  if (new_info.cell.row != dest_info->cell.row ||
	      (new_info.cell.row == dest_info->cell.row &&
	       dest_info->insert_pos != new_info.insert_pos))
	    {
	      dest_info->insert_pos  = new_info.insert_pos;
	      dest_info->cell.row    = new_info.cell.row;
	      dest_info->cell.column = new_info.cell.column;

	      clist->drag_highlight_row = dest_info->cell.row;
	      clist->drag_highlight_pos = dest_info->insert_pos;

	      gdk_drag_status (context,
		gdk_drag_context_get_suggested_action(context), time);
	    }
	  return TRUE;
	}
    }

  dest_info->insert_pos  = new_info.insert_pos;
  dest_info->cell.row    = new_info.cell.row;
  dest_info->cell.column = new_info.cell.column;
  return TRUE;
}

static void
gtk_cmctree_drag_data_received (GtkWidget        *widget,
			      GdkDragContext   *context,
			      gint              x,
			      gint              y,
			      GtkSelectionData *selection_data,
			      guint             info,
			      guint32           time)
{
  GtkCMCTree *ctree;
  GtkCMCList *clist;

  cm_return_if_fail (GTK_IS_CMCTREE (widget));
  cm_return_if_fail (context != NULL);
  cm_return_if_fail (selection_data != NULL);

  ctree = GTK_CMCTREE (widget);
  clist = GTK_CMCLIST (widget);

  if (GTK_CMCLIST_REORDERABLE (clist) &&
      gtk_drag_get_source_widget (context) == widget &&
      gtk_selection_data_get_target (selection_data) ==
      gdk_atom_intern_static_string ("gtk-clist-drag-reorder") &&
      gtk_selection_data_get_format (selection_data) == 8 &&
      gtk_selection_data_get_length (selection_data) == sizeof (GtkCMCListCellInfo))
    {
      GtkCMCListCellInfo *source_info;

      source_info = (GtkCMCListCellInfo *)(gtk_selection_data_get_data (selection_data));
      if (source_info)
	{
	  GtkCMCListDestInfo dest_info;
	  GtkCMCTreeNode *source_node;
	  GtkCMCTreeNode *dest_node;

	  drag_dest_cell (clist, x, y, &dest_info);
	  
	  source_node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list,
						    source_info->row));
	  dest_node = GTK_CMCTREE_NODE (g_list_nth (clist->row_list,
						  dest_info.cell.row));

	  if (!source_node || !dest_node)
	    return;

	  switch (dest_info.insert_pos)
	    {
	    case GTK_CMCLIST_DRAG_NONE:
	      break;
	    case GTK_CMCLIST_DRAG_INTO:
	      if (check_drag (ctree, source_node, dest_node,
			      dest_info.insert_pos))
		gtk_cmctree_move (ctree, source_node, dest_node,
				GTK_CMCTREE_ROW (dest_node)->children);
	      g_dataset_remove_data (context, "gtk-clist-drag-dest");
	      break;
	    case GTK_CMCLIST_DRAG_BEFORE:
	      if (check_drag (ctree, source_node, dest_node,
			      dest_info.insert_pos))
		gtk_cmctree_move (ctree, source_node,
				GTK_CMCTREE_ROW (dest_node)->parent, dest_node);
	      g_dataset_remove_data (context, "gtk-clist-drag-dest");
	      break;
	    case GTK_CMCLIST_DRAG_AFTER:
	      if (check_drag (ctree, source_node, dest_node,
			      dest_info.insert_pos))
		gtk_cmctree_move (ctree, source_node,
				GTK_CMCTREE_ROW (dest_node)->parent, 
				GTK_CMCTREE_ROW (dest_node)->sibling);
	      g_dataset_remove_data (context, "gtk-clist-drag-dest");
	      break;
	    }
	}
    }
}

GType
gtk_cmctree_node_get_type (void)
{
  static GType our_type = 0;
  
  if (our_type == 0)
    our_type = g_pointer_type_register_static ("GtkCMCTreeNode");

  return our_type;
}
