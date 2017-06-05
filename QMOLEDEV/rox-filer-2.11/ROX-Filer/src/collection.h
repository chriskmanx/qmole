/*
 * The collection widget provides an area for displaying a collection of
 * objects (such as files). It allows the user to choose a selection of
 * them and provides signals to allow popping up menus, detecting double-clicks
 * etc.
 *
 * Thomas Leonard, <tal197@users.sourceforge.net>
 */


#ifndef __COLLECTION_H__
#define __COLLECTION_H__

#include <gdk/gdk.h>
#include <gtk/gtkwidget.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define COLLECTION(obj) GTK_CHECK_CAST((obj), collection_get_type(), Collection)
#define COLLECTION_CLASS(obj) G_TYPE_INSTANCE_GET_CLASS((obj), \
					collection_get_type(), CollectionClass)
#define IS_COLLECTION(obj) \
	G_TYPE_CHECK_INSTANCE_TYPE((obj), collection_get_type())

/* If the display gets mucked up then remember to fix it next time we get the
 * chance.
 */
enum
{
	PAINT_NORMAL,		/* Just redraw what we need to */
	PAINT_OVERWRITE,	/* Draw everything */
	PAINT_CLEAR,		/* Blank everything, then redraw */
};

typedef struct _Collection Collection;

/* Each item in a Collection has one of these, which stores its selected
 * state, data, and view_data.
 */
typedef struct _CollectionItem   CollectionItem;

typedef struct _CollectionClass  CollectionClass;
typedef void (*CollectionDrawFunc)(GtkWidget *widget,
			     	  CollectionItem *item,
			     	  GdkRectangle *area,
				  gpointer user_data);
typedef gboolean (*CollectionTestFunc)( Collection *collection,
					int point_x, int point_y,
			       		CollectionItem *item,
			       		int width, int height,
					gpointer user_data);
typedef void (*CollectionFreeFunc)(Collection *collection,
			     	   CollectionItem *item);

struct _CollectionItem
{
	gpointer	data;
	gpointer	view_data;
	gboolean	selected;
};

struct _Collection
{
	GtkWidget 	parent_widget;

	/* With 2.0, the collection is in a Viewport, and this is used only to
	 * force scrolling, etc.
	 */
	GtkAdjustment	*vadj;

	CollectionDrawFunc draw_item;
	CollectionTestFunc test_point;
	CollectionFreeFunc free_item;
	gpointer	cb_user_data;	/* Passed to above functions */

	gboolean	lasso_box;	/* Is the box drawn? */
	int		drag_box_x[2];	/* Index 0 is the fixed corner */
	int		drag_box_y[2];
	GdkGC		*xor_gc;

	CollectionItem	*items;
	gint		cursor_item;		/* -1 if not shown */
	gint		cursor_item_old;	/* May be -1 */
	gint		wink_item;		/* -1 if not active */
	gint		wink_on_map;		/* -1 if not waiting for map */
	gint		winks_left;		/* Flashes in this wink op */
	gint		wink_timeout;
	guint		columns;
	gboolean        vertical_order;		/* order elements vertically? */
	gint		number_of_items;	/* (often compared with -1) */
	guint		item_width, item_height;

	guint		number_selected;

	guint		array_size;

	gint		block_selection_changed;
};

struct _CollectionClass
{
	GtkWidgetClass 	parent_class;

	void 		(*gain_selection)(Collection 	*collection,
					  gint		time);
	void 		(*lose_selection)(Collection 	*collection,
					  gint		time);
	void		(*selection_changed)(Collection	*collection,
					     gint	time);
};

GType	collection_get_type   		(void);
GtkWidget *collection_new		(void);
void    collection_clear           	(Collection *collection);
void	collection_clear_except		(Collection *collection, gint item);
gint	collection_insert		(Collection *collection,
					 gpointer data,
					 gpointer view);
void    collection_remove          	(Collection *collection, gint item);
void    collection_unselect_item	(Collection *collection, gint item);
void    collection_select_item		(Collection *collection, gint item);
void 	collection_toggle_item		(Collection *collection, gint item);
void 	collection_select_all		(Collection *collection);
void 	collection_clear_selection	(Collection *collection);
void	collection_invert_selection	(Collection *collection);
void	collection_draw_item		(Collection *collection, gint item,
					 gboolean blank);
void 	collection_set_item_size	(Collection *collection,
					 int width, int height);
void 	collection_qsort		(Collection *collection,
					 int (*compar)(const void *,
						       const void *),
					 GtkSortType order);
int 	collection_find_item		(Collection *collection,
					 gpointer data,
					 int (*compar)(const void *,
						       const void *),
					 GtkSortType order);
int 	collection_get_item		(Collection *collection, int x, int y);
void 	collection_set_cursor_item	(Collection *collection, gint item,
					 gboolean may_scroll);
void 	collection_wink_item		(Collection *collection, gint item);
void 	collection_delete_if		(Collection *collection,
			  		 gboolean (*test)(gpointer item,
						          gpointer data),
			  		 gpointer data);
void 	collection_move_cursor		(Collection *collection,
					 int drow, int dcol);
void	collection_lasso_box		(Collection *collection, int x, int y);
void	collection_end_lasso		(Collection *collection,
					 GdkFunction fn);
void	collection_snap_size		(Collection *collection,
					 int rows, int cols);
void	collection_unblock_selection_changed(Collection *collection,
					   guint time,
					   gboolean emit);
void    collection_item_to_rowcol       (const Collection *collection,
					 int item, int *row, int *col);
int     collection_rowcol_to_item       (const Collection *collection,
					 int row, int col);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __COLLECTION_H__ */
