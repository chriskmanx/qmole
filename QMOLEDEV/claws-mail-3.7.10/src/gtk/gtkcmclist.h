/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball, Josh MacDonald
 * Copyright (C) 1997-1998 Jay Painter <jpaint@serv.net><jpaint@gimp.org>
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

#ifndef __GTK_CMCLIST_H__
#define __GTK_CMCLIST_H__

#include <gdk/gdk.h>
#include <gtk/gtk.h>


G_BEGIN_DECLS


/* clist flags */
enum {
  GTK_CMCLIST_IN_DRAG             = 1 <<  0,
  GTK_CMCLIST_ROW_HEIGHT_SET      = 1 <<  1,
  GTK_CMCLIST_SHOW_TITLES         = 1 <<  2,
  /* Unused */
  GTK_CMCLIST_ADD_MODE            = 1 <<  4,
  GTK_CMCLIST_AUTO_SORT           = 1 <<  5,
  GTK_CMCLIST_AUTO_RESIZE_BLOCKED = 1 <<  6,
  GTK_CMCLIST_REORDERABLE         = 1 <<  7,
  GTK_CMCLIST_USE_DRAG_ICONS      = 1 <<  8,
  GTK_CMCLIST_DRAW_DRAG_LINE      = 1 <<  9,
  GTK_CMCLIST_DRAW_DRAG_RECT      = 1 << 10
}; 

/* cell types */
typedef enum
{
  GTK_CMCELL_EMPTY,
  GTK_CMCELL_TEXT,
  GTK_CMCELL_PIXBUF,
  GTK_CMCELL_PIXTEXT,
  GTK_CMCELL_WIDGET
} GtkCMCellType;

typedef enum
{
  GTK_CMCLIST_DRAG_NONE,
  GTK_CMCLIST_DRAG_BEFORE,
  GTK_CMCLIST_DRAG_INTO,
  GTK_CMCLIST_DRAG_AFTER
} GtkCMCListDragPos;

typedef enum
{
  GTK_CMBUTTON_IGNORED = 0,
  GTK_CMBUTTON_SELECTS = 1 << 0,
  GTK_CMBUTTON_DRAGS   = 1 << 1,
  GTK_CMBUTTON_EXPANDS = 1 << 2
} GtkCMButtonAction;

#define GTK_TYPE_CMCLIST            (gtk_cmclist_get_type ())
#define GTK_CMCLIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_CMCLIST, GtkCMCList))
#define GTK_CMCLIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_CMCLIST, GtkCMCListClass))
#define GTK_IS_CMCLIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_CMCLIST))
#define GTK_IS_CMCLIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_CMCLIST))
#define GTK_CMCLIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_CMCLIST, GtkCMCListClass))


#define GTK_CMCLIST_FLAGS(clist)             (GTK_CMCLIST (clist)->flags)
#define GTK_CMCLIST_SET_FLAG(clist,flag)     (GTK_CMCLIST_FLAGS (clist) |= (GTK_ ## flag))
#define GTK_CMCLIST_UNSET_FLAG(clist,flag)   (GTK_CMCLIST_FLAGS (clist) &= ~(GTK_ ## flag))

#define GTK_CMCLIST_IN_DRAG(clist)           (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_IN_DRAG)
#define GTK_CMCLIST_ROW_HEIGHT_SET(clist)    (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_ROW_HEIGHT_SET)
#define GTK_CMCLIST_SHOW_TITLES(clist)       (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_SHOW_TITLES)
#define GTK_CMCLIST_ADD_MODE(clist)          (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_ADD_MODE)
#define GTK_CMCLIST_AUTO_SORT(clist)         (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_AUTO_SORT)
#define GTK_CMCLIST_AUTO_RESIZE_BLOCKED(clist) (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_AUTO_RESIZE_BLOCKED)
#define GTK_CMCLIST_REORDERABLE(clist)       (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_REORDERABLE)
#define GTK_CMCLIST_USE_DRAG_ICONS(clist)    (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_USE_DRAG_ICONS)
#define GTK_CMCLIST_DRAW_DRAG_LINE(clist)    (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_DRAW_DRAG_LINE)
#define GTK_CMCLIST_DRAW_DRAG_RECT(clist)    (GTK_CMCLIST_FLAGS (clist) & GTK_CMCLIST_DRAW_DRAG_RECT)

#define GTK_CMCLIST_ROW(_glist_) ((GtkCMCListRow *)((_glist_)->data))

/* pointer casting for cells */
#define GTK_CMCELL_TEXT(cell)     (((GtkCMCellText *) &(cell)))
#define GTK_CMCELL_PIXBUF(cell)   (((GtkCMCellPixbuf *) &(cell)))
#define GTK_CMCELL_PIXTEXT(cell)  (((GtkCMCellPixText *) &(cell)))
#define GTK_CMCELL_WIDGET(cell)   (((GtkCMCellWidget *) &(cell)))

typedef struct _GtkCMCList GtkCMCList;
typedef struct _GtkCMCListClass GtkCMCListClass;
typedef struct _GtkCMCListColumn GtkCMCListColumn;
typedef struct _GtkCMCListRow GtkCMCListRow;

typedef struct _GtkCMCell GtkCMCell;
typedef struct _GtkCMCellText GtkCMCellText;
typedef struct _GtkCMCellPixbuf GtkCMCellPixbuf;
typedef struct _GtkCMCellPixText GtkCMCellPixText;
typedef struct _GtkCMCellWidget GtkCMCellWidget;

typedef gint (*GtkCMCListCompareFunc) (GtkCMCList     *clist,
				     gconstpointer ptr1,
				     gconstpointer ptr2);

typedef struct _GtkCMCListCellInfo GtkCMCListCellInfo;
typedef struct _GtkCMCListDestInfo GtkCMCListDestInfo;

struct _GtkCMCListCellInfo
{
  gint row;
  gint column;
};

struct _GtkCMCListDestInfo
{
  GtkCMCListCellInfo cell;
  GtkCMCListDragPos  insert_pos;
};

struct _GtkCMCList
{
  GtkContainer container;
  
  guint16 flags;
  
#if !GLIB_CHECK_VERSION(2,10,0)
  GMemChunk *row_mem_chunk;
  GMemChunk *cell_mem_chunk;
#else
  gpointer reserved1;
  gpointer reserved2;
#endif
  guint freeze_count;
  
  /* allocation rectangle after the conatiner_border_width
   * and the width of the shadow border */
  GdkRectangle internal_allocation;
  
  /* rows */
  gint rows;
  gint row_height;
  GList *row_list;
  GList *row_list_end;
  
  /* columns */
  gint columns;
  GdkRectangle column_title_area;
  GdkWindow *title_window;
  
  /* dynamicly allocated array of column structures */
  GtkCMCListColumn *column;
  
  /* the scrolling window and its height and width to
   * make things a little speedier */
  GdkWindow *clist_window;
  gint clist_window_width;
  gint clist_window_height;
  
  /* offsets for scrolling */
  gint hoffset;
  gint voffset;
  
  /* border shadow style */
  GtkShadowType shadow_type;
  
  /* the list's selection mode (gtkenums.h) */
  GtkSelectionMode selection_mode;
  
  /* list of selected rows */
  GList *selection;
  GList *selection_end;
  
  GList *undo_selection;
  GList *undo_unselection;
  gint undo_anchor;
  
  /* mouse buttons */
  guint8 button_actions[5];

  guint8 drag_button;

  /* dnd */
  GtkCMCListCellInfo click_cell;

  /* scroll adjustments */
  GtkAdjustment *hadjustment;
  GtkAdjustment *vadjustment;
  
  /* xor GC for the vertical drag line */
  GdkGC *xor_gc;
  
  /* gc for drawing unselected cells */
  GdkGC *fg_gc;
  GdkGC *bg_gc;
  
  /* cursor used to indicate dragging */
  GdkCursor *cursor_drag;
  
  /* the current x-pixel location of the xor-drag line */
  gint x_drag;
  
  /* focus handling */
  gint focus_row;

  gint focus_header_column;
  
  /* dragging the selection */
  gint anchor;
  GtkStateType anchor_state;
  gint drag_pos;
  gint htimer;
  gint vtimer;
  
  GtkSortType sort_type;
  GtkCMCListCompareFunc compare;
  gint sort_column;

  gint drag_highlight_row;
  GtkCMCListDragPos drag_highlight_pos;
};

struct _GtkCMCListClass
{
  GtkContainerClass parent_class;
  
  void  (*set_scroll_adjustments) (GtkCMCList       *clist,
				   GtkAdjustment  *hadjustment,
				   GtkAdjustment  *vadjustment);
  void   (*refresh)             (GtkCMCList       *clist);
  void   (*select_row)          (GtkCMCList       *clist,
				 gint            row,
				 gint            column,
				 GdkEvent       *event);
  void   (*unselect_row)        (GtkCMCList       *clist,
				 gint            row,
				 gint            column,
				 GdkEvent       *event);
  void   (*row_move)            (GtkCMCList       *clist,
				 gint            source_row,
				 gint            dest_row);
  void   (*click_column)        (GtkCMCList       *clist,
				 gint            column);
  void   (*resize_column)       (GtkCMCList       *clist,
				 gint            column,
                                 gint            width);
  void   (*toggle_focus_row)    (GtkCMCList       *clist);
  void   (*select_all)          (GtkCMCList       *clist);
  void   (*unselect_all)        (GtkCMCList       *clist);
  void   (*undo_selection)      (GtkCMCList       *clist);
  void   (*start_selection)     (GtkCMCList       *clist);
  void   (*end_selection)       (GtkCMCList       *clist);
  void   (*extend_selection)    (GtkCMCList       *clist,
				 GtkScrollType   scroll_type,
				 gfloat          position,
				 gboolean        auto_start_selection);
  void   (*scroll_horizontal)   (GtkCMCList       *clist,
				 GtkScrollType   scroll_type,
				 gfloat          position);
  void   (*scroll_vertical)     (GtkCMCList       *clist,
				 GtkScrollType   scroll_type,
				 gfloat          position);
  void   (*toggle_add_mode)     (GtkCMCList       *clist);
  void   (*abort_column_resize) (GtkCMCList       *clist);
  void   (*resync_selection)    (GtkCMCList       *clist,
				 GdkEvent       *event);
  GList* (*selection_find)      (GtkCMCList       *clist,
				 gint            row_number,
				 GList          *row_list_element);
  void   (*draw_row)            (GtkCMCList       *clist,
				 GdkRectangle   *area,
				 gint            row,
				 GtkCMCListRow    *clist_row);
  void   (*draw_drag_highlight) (GtkCMCList        *clist,
				 GtkCMCListRow     *target_row,
				 gint             target_row_number,
				 GtkCMCListDragPos  drag_pos);
  void   (*clear)               (GtkCMCList       *clist);
  void   (*fake_unselect_all)   (GtkCMCList       *clist,
				 gint            row);
  void   (*sort_list)           (GtkCMCList       *clist);
  gint   (*insert_row)          (GtkCMCList       *clist,
				 gint            row,
				 gchar          *text[]);
  void   (*remove_row)          (GtkCMCList       *clist,
				 gint            row);
  void   (*set_cell_contents)   (GtkCMCList       *clist,
				 GtkCMCListRow    *clist_row,
				 gint            column,
				 GtkCMCellType     type,
				 const gchar    *text,
				 guint8          spacing,
				 GdkPixbuf      *pixbuf);
  void   (*cell_size_request)   (GtkCMCList       *clist,
				 GtkCMCListRow    *clist_row,
				 gint            column,
				 GtkRequisition *requisition);

};

struct _GtkCMCListColumn
{
  gchar *title;
  GdkRectangle area;
  
  GtkWidget *button;
  GdkWindow *window;
  
  gint width;
  gint min_width;
  gint max_width;
  GtkJustification justification;
  
  guint visible        : 1;  
  guint width_set      : 1;
  guint resizeable     : 1;
  guint auto_resize    : 1;
  guint button_passive : 1;
};

struct _GtkCMCListRow
{
  GtkCMCell *cell;
  GtkStateType state;
  
  GdkColor foreground;
  GdkColor background;
  
  GtkStyle *style;

  gpointer data;
  GDestroyNotify destroy;
  
  guint fg_set     : 1;
  guint bg_set     : 1;
  guint selectable : 1;
};

/* Cell Structures */
struct _GtkCMCellText
{
  GtkCMCellType type;
  
  gint16 vertical;
  gint16 horizontal;
  
  GtkStyle *style;

  gchar *text;
};

struct _GtkCMCellPixbuf
{
  GtkCMCellType type;
  
  gint16 vertical;
  gint16 horizontal;
  
  GtkStyle *style;

  GdkPixbuf *pixbuf;
};

struct _GtkCMCellPixText
{
  GtkCMCellType type;
  
  gint16 vertical;
  gint16 horizontal;
  
  GtkStyle *style;

  gchar *text;
  guint8 spacing;
  GdkPixbuf *pixbuf;
};

struct _GtkCMCellWidget
{
  GtkCMCellType type;
  
  gint16 vertical;
  gint16 horizontal;
  
  GtkStyle *style;

  GtkWidget *widget;
};

struct _GtkCMCell
{
  GtkCMCellType type;
  
  gint16 vertical;
  gint16 horizontal;
  
  GtkStyle *style;

  union {
    gchar *text;
    
    struct {
      GdkPixbuf *pixbuf;
    } pm;
    
    struct {
      gchar *text;
      guint8 spacing;
      GdkPixbuf *pixbuf;
    } pt;
    
    GtkWidget *widget;
  } u;
};

GType gtk_cmclist_get_type (void);

/* create a new GtkCMCList */
GtkWidget* gtk_cmclist_new             (gint   columns);
GtkWidget* gtk_cmclist_new_with_titles (gint   columns,
				      gchar *titles[]);

/* set adjustments of clist */
void gtk_cmclist_set_hadjustment (GtkCMCList      *clist,
				GtkAdjustment *adjustment);
void gtk_cmclist_set_vadjustment (GtkCMCList      *clist,
				GtkAdjustment *adjustment);

/* get adjustments of clist */
GtkAdjustment* gtk_cmclist_get_hadjustment (GtkCMCList *clist);
GtkAdjustment* gtk_cmclist_get_vadjustment (GtkCMCList *clist);

/* set the border style of the clist */
void gtk_cmclist_set_shadow_type (GtkCMCList      *clist,
				GtkShadowType  type);

/* set the clist's selection mode */
void gtk_cmclist_set_selection_mode (GtkCMCList         *clist,
				   GtkSelectionMode  mode);

/* enable clists reorder ability */
void gtk_cmclist_set_reorderable (GtkCMCList *clist,
				gboolean  reorderable);
void gtk_cmclist_set_use_drag_icons (GtkCMCList *clist,
				   gboolean  use_icons);
void gtk_cmclist_set_button_actions (GtkCMCList *clist,
				   guint     button,
				   guint8    button_actions);

/* freeze all visual updates of the list, and then thaw the list after
 * you have made a number of changes and the updates wil occure in a
 * more efficent mannor than if you made them on a unfrozen list
 */
void gtk_cmclist_freeze (GtkCMCList *clist);
void gtk_cmclist_thaw   (GtkCMCList *clist);

/* show and hide the column title buttons */
void gtk_cmclist_column_titles_show (GtkCMCList *clist);
void gtk_cmclist_column_titles_hide (GtkCMCList *clist);

/* set the column title to be a active title (responds to button presses, 
 * prelights, and grabs keyboard focus), or passive where it acts as just
 * a title
 */
void gtk_cmclist_column_title_active   (GtkCMCList *clist,
				      gint      column);
void gtk_cmclist_column_title_passive  (GtkCMCList *clist,
				      gint      column);
void gtk_cmclist_column_titles_active  (GtkCMCList *clist);
void gtk_cmclist_column_titles_passive (GtkCMCList *clist);

/* set the title in the column title button */
void gtk_cmclist_set_column_title (GtkCMCList    *clist,
				 gint         column,
				 const gchar *title);

/* returns the title of column. Returns NULL if title is not set */
gchar * gtk_cmclist_get_column_title (GtkCMCList *clist,
				    gint      column);

/* set a widget instead of a title for the column title button */
void gtk_cmclist_set_column_widget (GtkCMCList  *clist,
				  gint       column,
				  GtkWidget *widget);

/* returns the column widget */
GtkWidget * gtk_cmclist_get_column_widget (GtkCMCList *clist,
					 gint      column);

/* set the justification on a column */
void gtk_cmclist_set_column_justification (GtkCMCList         *clist,
					 gint              column,
					 GtkJustification  justification);

/* set visibility of a column */
void gtk_cmclist_set_column_visibility (GtkCMCList *clist,
				      gint      column,
				      gboolean  visible);

/* enable/disable column resize operations by mouse */
void gtk_cmclist_set_column_resizeable (GtkCMCList *clist,
				      gint      column,
				      gboolean  resizeable);

/* resize column automatically to its optimal width */
void gtk_cmclist_set_column_auto_resize (GtkCMCList *clist,
				       gint      column,
				       gboolean  auto_resize);

gint gtk_cmclist_columns_autosize (GtkCMCList *clist);

/* return the optimal column width, i.e. maximum of all cell widths */
gint gtk_cmclist_optimal_column_width (GtkCMCList *clist,
				     gint      column);

/* set the pixel width of a column; this is a necessary step in
 * creating a CList because otherwise the column width is chozen from
 * the width of the column title, which will never be right
 */
void gtk_cmclist_set_column_width (GtkCMCList *clist,
				 gint      column,
				 gint      width);

/* set column minimum/maximum width. min/max_width < 0 => no restriction */
void gtk_cmclist_set_column_min_width (GtkCMCList *clist,
				     gint      column,
				     gint      min_width);
void gtk_cmclist_set_column_max_width (GtkCMCList *clist,
				     gint      column,
				     gint      max_width);

/* change the height of the rows, the default (height=0) is
 * the hight of the current font.
 */
void gtk_cmclist_set_row_height (GtkCMCList *clist,
			       guint     height);

/* scroll the viewing area of the list to the given column and row;
 * row_align and col_align are between 0-1 representing the location the
 * row should appear on the screnn, 0.0 being top or left, 1.0 being
 * bottom or right; if row or column is -1 then then there is no change
 */
void gtk_cmclist_moveto (GtkCMCList *clist,
		       gint      row,
		       gint      column,
		       gfloat    row_align,
		       gfloat    col_align);

/* returns whether the row is visible */
GtkVisibility gtk_cmclist_row_is_visible (GtkCMCList *clist,
					gint      row);

/* returns the cell type */
GtkCMCellType gtk_cmclist_get_cell_type (GtkCMCList *clist,
				     gint      row,
				     gint      column);

/* sets a given cell's text, replacing its current contents */
void gtk_cmclist_set_text (GtkCMCList    *clist,
			 gint         row,
			 gint         column,
			 const gchar *text);

/* for the "get" functions, any of the return pointer can be
 * NULL if you are not interested
 */
gint gtk_cmclist_get_text (GtkCMCList  *clist,
			 gint       row,
			 gint       column,
			 gchar    **text);

/* sets a given cell's pixbuf, replacing its current contents */
void gtk_cmclist_set_pixbuf (GtkCMCList  *clist,
			   gint       row,
			   gint       column,
			   GdkPixbuf *pixbuf);

gint gtk_cmclist_get_pixbuf (GtkCMCList   *clist,
			   gint        row,
			   gint        column,
			   GdkPixbuf **pixbuf);

/* sets a given cell's pixbuf and text, replacing its current contents */
void gtk_cmclist_set_pixtext (GtkCMCList    *clist,
			    gint         row,
			    gint         column,
			    const gchar *text,
			    guint8       spacing,
			    GdkPixbuf   *pixbuf);

gint gtk_cmclist_get_pixtext (GtkCMCList   *clist,
			    gint        row,
			    gint        column,
			    gchar     **text,
			    guint8     *spacing,
			    GdkPixbuf **pixbuf);

/* sets the foreground color of a row, the color must already
 * be allocated
 */
void gtk_cmclist_set_foreground (GtkCMCList       *clist,
			       gint            row,
			       const GdkColor *color);

/* sets the background color of a row, the color must already
 * be allocated
 */
void gtk_cmclist_set_background (GtkCMCList       *clist,
			       gint            row,
			       const GdkColor *color);

/* set / get cell styles */
void gtk_cmclist_set_cell_style (GtkCMCList *clist,
			       gint      row,
			       gint      column,
			       GtkStyle *style);

GtkStyle *gtk_cmclist_get_cell_style (GtkCMCList *clist,
				    gint      row,
				    gint      column);

void gtk_cmclist_set_row_style (GtkCMCList *clist,
			      gint      row,
			      GtkStyle *style);

GtkStyle *gtk_cmclist_get_row_style (GtkCMCList *clist,
				   gint      row);

/* this sets a horizontal and vertical shift for drawing
 * the contents of a cell; it can be positive or negitive;
 * this is particulary useful for indenting items in a column
 */
void gtk_cmclist_set_shift (GtkCMCList *clist,
			  gint      row,
			  gint      column,
			  gint      vertical,
			  gint      horizontal);

/* set/get selectable flag of a single row */
void gtk_cmclist_set_selectable (GtkCMCList *clist,
			       gint      row,
			       gboolean  selectable);
gboolean gtk_cmclist_get_selectable (GtkCMCList *clist,
				   gint      row);

/* prepend/append returns the index of the row you just added,
 * making it easier to append and modify a row
 */
gint gtk_cmclist_prepend (GtkCMCList    *clist,
		        gchar       *text[]);
gint gtk_cmclist_append  (GtkCMCList    *clist,
			gchar       *text[]);

/* inserts a row at index row and returns the row where it was
 * actually inserted (may be different from "row" in auto_sort mode)
 */
gint gtk_cmclist_insert (GtkCMCList    *clist,
		       gint         row,
		       gchar       *text[]);

/* removes row at index row */
void gtk_cmclist_remove (GtkCMCList *clist,
		       gint      row);

/* sets a arbitrary data pointer for a given row */
void gtk_cmclist_set_row_data (GtkCMCList *clist,
			     gint      row,
			     gpointer  data);

/* sets a data pointer for a given row with destroy notification */
void gtk_cmclist_set_row_data_full (GtkCMCList         *clist,
			          gint              row,
			          gpointer          data,
				  GDestroyNotify  destroy);

/* returns the data set for a row */
gpointer gtk_cmclist_get_row_data (GtkCMCList *clist,
				 gint      row);

/* givin a data pointer, find the first (and hopefully only!)
 * row that points to that data, or -1 if none do
 */
gint gtk_cmclist_find_row_from_data (GtkCMCList *clist,
				   gpointer  data);

/* force selection of a row */
void gtk_cmclist_select_row (GtkCMCList *clist,
			   gint      row,
			   gint      column);

/* force unselection of a row */
void gtk_cmclist_unselect_row (GtkCMCList *clist,
			     gint      row,
			     gint      column);

/* undo the last select/unselect operation */
void gtk_cmclist_undo_selection (GtkCMCList *clist);

/* clear the entire list -- this is much faster than removing
 * each item with gtk_cmclist_remove
 */
void gtk_cmclist_clear (GtkCMCList *clist);

/* return the row column corresponding to the x and y coordinates,
 * the returned values are only valid if the x and y coordinates
 * are respectively to a window == clist->clist_window
 */
gint gtk_cmclist_get_selection_info (GtkCMCList *clist,
			     	   gint      x,
			     	   gint      y,
			     	   gint     *row,
			     	   gint     *column);

/* in multiple or extended mode, select all rows */
void gtk_cmclist_select_all (GtkCMCList *clist);

/* in all modes except browse mode, deselect all rows */
void gtk_cmclist_unselect_all (GtkCMCList *clist);

/* swap the position of two rows */
void gtk_cmclist_swap_rows (GtkCMCList *clist,
			  gint      row1,
			  gint      row2);

/* move row from source_row position to dest_row position */
void gtk_cmclist_row_move (GtkCMCList *clist,
			 gint      source_row,
			 gint      dest_row);

/* sets a compare function different to the default */
void gtk_cmclist_set_compare_func (GtkCMCList            *clist,
				 GtkCMCListCompareFunc  cmp_func);

/* the column to sort by */
void gtk_cmclist_set_sort_column (GtkCMCList *clist,
				gint      column);

/* how to sort : ascending or descending */
void gtk_cmclist_set_sort_type (GtkCMCList    *clist,
			      GtkSortType  sort_type);

/* sort the list with the current compare function */
void gtk_cmclist_sort (GtkCMCList *clist);

/* Automatically sort upon insertion */
void gtk_cmclist_set_auto_sort (GtkCMCList *clist,
			      gboolean  auto_sort);

/* Private function for clist, ctree */

PangoLayout *_gtk_cmclist_create_cell_layout (GtkCMCList       *clist,
					    GtkCMCListRow    *clist_row,
					    gint            column);


G_END_DECLS


#endif				/* __GTK_CMCLIST_H__ */
