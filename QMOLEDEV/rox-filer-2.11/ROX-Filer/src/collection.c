/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * The collection widget provides an area for displaying a collection of
 * objects (such as files). It allows the user to choose a selection of
 * them and provides signals to allow popping up menus, detecting
 * double-clicks etc.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "config.h"

#include <stdlib.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "global.h"

#include "collection.h"

#define MIN_WIDTH 80
#define MIN_HEIGHT 60
#define MINIMUM_ITEMS 16

#define MAX_WINKS 5		/* Should be an odd number */

/* Macro to emit the "selection_changed" signal only if allowed */
#define EMIT_SELECTION_CHANGED(collection, time) \
	if (!collection->block_selection_changed) \
		g_signal_emit(collection, \
				collection_signals[SELECTION_CHANGED], 0, time)

enum
{
	PROP_0,
	PROP_VADJUSTMENT
};

/* Signals:
 *
 * void gain_selection(collection, time, user_data)
 * 	We've gone from no selected items to having a selection.
 * 	Time is the time of the event that caused the change, or
 * 	GDK_CURRENT_TIME if not known.
 *
 * void lose_selection(collection, time, user_data)
 * 	We've dropped to having no selected items.
 * 	Time is the time of the event that caused the change, or
 * 	GDK_CURRENT_TIME if not known.
 *
 * void selection_changed(collection, user_data)
 * 	The set of selected items has changed.
 * 	Time is the time of the event that caused the change, or
 * 	GDK_CURRENT_TIME if not known.
 */
enum
{
	GAIN_SELECTION,
	LOSE_SELECTION,
	SELECTION_CHANGED,
	LAST_SIGNAL
};

static guint collection_signals[LAST_SIGNAL] = { 0 };

static guint32 current_event_time = GDK_CURRENT_TIME;

static GtkWidgetClass *parent_class = NULL;

/* Static prototypes */
static void draw_one_item(Collection 	*collection,
			  int 		item,
			  GdkRectangle 	*area);
static void collection_class_init(GObjectClass *gclass, gpointer data);
static void collection_init(GTypeInstance *object, gpointer g_class);
static void collection_destroy(GtkObject *object);
static void collection_finalize(GObject *object);
static void collection_realize(GtkWidget *widget);
static void collection_map(GtkWidget *widget);
static void collection_size_request(GtkWidget 		*widget,
				    GtkRequisition 	*requisition);
static void collection_size_allocate(GtkWidget 		*widget,
			     GtkAllocation 	*allocation);
static void collection_set_adjustment(Collection 	*collection,
				      GtkAdjustment 	*vadj);
static void collection_get_property(GObject    *object,
				    guint      prop_id,
				    GValue     *value,
				    GParamSpec *pspec);
static void collection_set_property(GObject      *object,
				    guint        prop_id,
				    const GValue *value,
				    GParamSpec   *pspec);
static gint collection_expose(GtkWidget *widget, GdkEventExpose *event);
static void default_draw_item(GtkWidget *widget,
				CollectionItem *data,
				GdkRectangle *area,
				gpointer user_data);
static gboolean	default_test_point(Collection *collection,
				   int point_x, int point_y,
				   CollectionItem *data,
				   int width, int height,
				   gpointer user_data);
static gint collection_motion_notify(GtkWidget *widget,
				     GdkEventMotion *event);
static void add_lasso_box(Collection *collection);
static void abort_lasso(Collection *collection);
static void remove_lasso_box(Collection *collection);
static void draw_lasso_box(Collection *collection);
static void cancel_wink(Collection *collection);
static gint collection_key_press(GtkWidget *widget, GdkEventKey *event);
static void get_visible_limits(Collection *collection, int *first, int *last);
static void scroll_to_show(Collection *collection, int item);
static void collection_item_set_selected(Collection *collection,
                                         gint item,
                                         gboolean selected,
					 gboolean signal);
static gint collection_scroll_event(GtkWidget *widget, GdkEventScroll *event);
static int collection_get_rows(const Collection *collection);
static int collection_get_cols(const Collection *collection);


/* The number of rows, at least 1.  */
static inline int collection_get_rows(const Collection *collection)
{
	int rows = (collection->number_of_items + collection->columns - 1) /
		collection->columns;
	return MAX(rows, 1);
}

/* The number of columns _actually_ displayed, at least 1.  This
 * function is required in vertical_order layout-based manipulation
 * such as moving the cursor to detect the last column.  */
static inline int collection_get_cols(const Collection *collection)
{
	if (collection->vertical_order) 
	{
		int rows = collection_get_rows(collection);
		int cols = (collection->number_of_items + rows - 1) / rows;
		return MAX(1, cols);
	}
	else
		return collection->columns;
}

static void draw_focus_at(Collection *collection, GdkRectangle *area)
{
	GtkWidget    	*widget;
	GtkStateType	state;

	widget = GTK_WIDGET(collection);

	if (GTK_WIDGET_FLAGS(widget) & GTK_HAS_FOCUS)
		state = GTK_STATE_ACTIVE;
	else
		state = GTK_STATE_INSENSITIVE;

	gtk_paint_focus(widget->style,
			widget->window,
			state,
			NULL,
			widget,
			"collection",
			area->x, area->y,
			collection->item_width,
			area->height);
}

static void draw_one_item(Collection *collection, int item, GdkRectangle *area)
{
	if (item < collection->number_of_items)
	{
		collection->draw_item((GtkWidget *) collection,
				&collection->items[item],
				area, collection->cb_user_data);
	}
	
	if (item == collection->cursor_item)
		draw_focus_at(collection, area);
}

GType collection_get_type(void)
{
	static GType my_type = 0;

	if (!my_type)
	{
		static const GTypeInfo info =
		{
			sizeof(CollectionClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			(GClassInitFunc) collection_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(Collection),
			0,			/* n_preallocs */
			collection_init
		};

		my_type = g_type_register_static(gtk_widget_get_type(),
					"Collection", &info, 0);
	}

	return my_type;
}

typedef void (*FinalizeFn)(GObject *object);

static void collection_class_init(GObjectClass *gclass, gpointer data)
{
	CollectionClass *collection_class = (CollectionClass *) gclass;
	GtkObjectClass *object_class = (GtkObjectClass *) gclass;
	GtkWidgetClass *widget_class = (GtkWidgetClass *) gclass;

	parent_class = gtk_type_class(gtk_widget_get_type());

	object_class->destroy = collection_destroy;
	G_OBJECT_CLASS(object_class)->finalize =
		(FinalizeFn) collection_finalize;

	widget_class->realize = collection_realize;
	widget_class->expose_event = collection_expose;
	widget_class->size_request = collection_size_request;
	widget_class->size_allocate = collection_size_allocate;

	widget_class->key_press_event = collection_key_press;
	
	widget_class->motion_notify_event = collection_motion_notify;
	widget_class->map = collection_map;
	widget_class->scroll_event = collection_scroll_event;

	gclass->set_property = collection_set_property;
	gclass->get_property = collection_get_property;

	collection_class->gain_selection = NULL;
	collection_class->lose_selection = NULL;
	collection_class->selection_changed = NULL;

	collection_signals[GAIN_SELECTION] = g_signal_new("gain_selection",
					G_TYPE_FROM_CLASS(gclass),
					G_SIGNAL_RUN_LAST,
					G_STRUCT_OFFSET(CollectionClass,
							gain_selection),
					NULL, NULL,
					g_cclosure_marshal_VOID__INT,
					G_TYPE_NONE, 1,
					G_TYPE_INT);

	collection_signals[LOSE_SELECTION] = g_signal_new("lose_selection",
					G_TYPE_FROM_CLASS(gclass),
					G_SIGNAL_RUN_LAST,
					G_STRUCT_OFFSET(CollectionClass,
							lose_selection),
					NULL, NULL,
					g_cclosure_marshal_VOID__INT,
					G_TYPE_NONE, 1,
					G_TYPE_INT);

	collection_signals[SELECTION_CHANGED] = g_signal_new(
					"selection_changed",
					G_TYPE_FROM_CLASS(gclass),
					G_SIGNAL_RUN_LAST,
					G_STRUCT_OFFSET(CollectionClass,
							selection_changed),
					NULL, NULL,
					g_cclosure_marshal_VOID__INT,
					G_TYPE_NONE, 1,
					G_TYPE_INT);

	g_object_class_install_property(gclass,
		PROP_VADJUSTMENT,
		g_param_spec_object("vadjustment",
			"Vertical Adjustment",
			"The GtkAdjustment for the vertical position.",
			GTK_TYPE_ADJUSTMENT,
			G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void collection_init(GTypeInstance *instance, gpointer g_class)
{
	Collection *object = (Collection *) instance;

	g_return_if_fail(object != NULL);
	g_return_if_fail(IS_COLLECTION(object));

	GTK_WIDGET_SET_FLAGS(GTK_WIDGET(object), GTK_CAN_FOCUS);

	object->number_of_items = 0;
	object->number_selected = 0;
	object->block_selection_changed = 0;
	object->columns = 1;
	object->vertical_order = FALSE;
	object->item_width = 64;
	object->item_height = 64;
	object->vadj = NULL;

	object->items = g_new(CollectionItem, MINIMUM_ITEMS);
	object->cursor_item = -1;
	object->cursor_item_old = -1;
	object->wink_item = -1;
	object->wink_on_map = -1;
	object->array_size = MINIMUM_ITEMS;
	object->draw_item = default_draw_item;
	object->test_point = default_test_point;
	object->free_item = NULL;
}

GtkWidget* collection_new(void)
{
	return GTK_WIDGET(gtk_widget_new(collection_get_type(), NULL));
}

/* After this we are unusable, but our data (if any) is still hanging around.
 * It will be freed later with finalize.
 */
static void collection_destroy(GtkObject *object)
{
	Collection *collection;

	g_return_if_fail(object != NULL);
	g_return_if_fail(IS_COLLECTION(object));

	collection = COLLECTION(object);

	collection_clear(collection);

	if (collection->vadj)
	{
		g_object_unref(G_OBJECT(collection->vadj));
		collection->vadj = NULL;
	}

	if (GTK_OBJECT_CLASS(parent_class)->destroy)
		(*GTK_OBJECT_CLASS(parent_class)->destroy)(object);
}

/* This is the last thing that happens to us. Free all data. */
static void collection_finalize(GObject *object)
{
	Collection *collection;

	collection = COLLECTION(object);

	g_return_if_fail(collection->number_of_items == 0);

	g_free(collection->items);

	if (G_OBJECT_CLASS(parent_class)->finalize)
		G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void collection_map(GtkWidget *widget)
{
	Collection *collection = COLLECTION(widget);
	
	if (GTK_WIDGET_CLASS(parent_class)->map)
		(*GTK_WIDGET_CLASS(parent_class)->map)(widget);

	if (collection->wink_on_map >= 0)
	{
		collection_wink_item(collection, collection->wink_on_map);
		collection->wink_on_map = -1;
	}
}

static void collection_realize(GtkWidget *widget)
{
	Collection 	*collection;
	GdkWindowAttr 	attributes;
	gint 		attributes_mask;
	GdkGCValues	xor_values;
	GdkColor	*bg, *fg;

	g_return_if_fail(widget != NULL);
	g_return_if_fail(IS_COLLECTION(widget));
	g_return_if_fail(widget->parent != NULL);

	GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);
	collection = COLLECTION(widget);

	attributes.x = widget->allocation.x;
	attributes.y = widget->allocation.y;
	attributes.width = widget->allocation.width;
	attributes.height = widget->allocation.height;
	attributes.wclass = GDK_INPUT_OUTPUT;
	attributes.window_type = GDK_WINDOW_CHILD;
	attributes.event_mask = gtk_widget_get_events(widget) | 
		GDK_EXPOSURE_MASK |
		GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK |
		GDK_BUTTON1_MOTION_MASK | GDK_BUTTON2_MOTION_MASK |
		GDK_BUTTON3_MOTION_MASK;
	attributes.visual = gtk_widget_get_visual(widget);
	attributes.colormap = gtk_widget_get_colormap(widget);

	attributes_mask = GDK_WA_X | GDK_WA_Y |
				GDK_WA_VISUAL | GDK_WA_COLORMAP;
	widget->window = gdk_window_new(gtk_widget_get_parent_window(widget),
			&attributes, attributes_mask);

	widget->style = gtk_style_attach(widget->style, widget->window);

	gdk_window_set_user_data(widget->window, widget);
	gdk_window_set_background(widget->window,
			&widget->style->base[GTK_STATE_NORMAL]);

	bg = &widget->style->base[GTK_STATE_NORMAL];
	fg = &widget->style->text[GTK_STATE_NORMAL];
	xor_values.function = GDK_XOR;
	xor_values.foreground.pixel = fg->pixel ^ bg->pixel;
	collection->xor_gc = gdk_gc_new_with_values(widget->window,
					&xor_values,
					GDK_GC_FOREGROUND
					| GDK_GC_FUNCTION);
}

static void collection_size_request(GtkWidget *widget,
				GtkRequisition *requisition)
{
	Collection *collection = COLLECTION(widget);
	int	rows;

	/* We ask for the total size we need; our containing viewport
	 * will deal with scrolling.
	 */
	requisition->width = MIN_WIDTH;
	rows = collection_get_rows(collection);
	requisition->height = rows * collection->item_height;
}

static gboolean scroll_after_alloc(Collection *collection)
{
	if (collection->wink_item != -1)
		scroll_to_show(collection, collection->wink_item);
	else if (collection->cursor_item != -1)
		scroll_to_show(collection, collection->cursor_item);
	g_object_unref(G_OBJECT(collection));

	return FALSE;
}

static void collection_size_allocate(GtkWidget *widget,
				GtkAllocation *allocation)
{
	Collection 	*collection;
	int		old_columns;
	gboolean	cursor_visible = FALSE;

	g_return_if_fail(widget != NULL);
	g_return_if_fail(IS_COLLECTION(widget));
	g_return_if_fail(allocation != NULL);

	collection = COLLECTION(widget);

	if (collection->cursor_item != -1)
	{
		int	first, last;
		int	crow, ccol;
		
		collection_item_to_rowcol(collection, collection->cursor_item,
					  &crow, &ccol);

		get_visible_limits(collection, &first, &last);

		cursor_visible = crow >= first && crow <= last;
	}

	old_columns = collection->columns;

	widget->allocation = *allocation;

	collection->columns = allocation->width / collection->item_width;
	if (collection->columns < 1)
		collection->columns = 1;
	
	if (GTK_WIDGET_REALIZED(widget))
	{
		gdk_window_move_resize(widget->window,
				allocation->x, allocation->y,
				allocation->width, allocation->height);

		if (cursor_visible)
			scroll_to_show(collection, collection->cursor_item);
	}

	if (old_columns != collection->columns)
	{
		/* Need to go around again... */
		gtk_widget_queue_resize(widget);
	}
	else if (collection->wink_item != -1 || collection->cursor_item != -1)
	{
		/* Viewport resets the adjustments after the alloc */
		g_object_ref(G_OBJECT(collection));
		g_idle_add((GSourceFunc) scroll_after_alloc, collection);
	}
}

/* Return the area occupied by the item at (row, col) by filling
 * in 'area'.
 */
static void collection_get_item_area(Collection *collection,
					int row, int col,
					GdkRectangle *area)

{
	area->x = col * collection->item_width;
	area->y = row * collection->item_height;

	area->width = collection->item_width;
	area->height = collection->item_height;
	if (col == collection->columns - 1)
		area->width <<= 1;
}

static gint collection_expose(GtkWidget *widget, GdkEventExpose *event)
{
	Collection	*collection;
	GdkRectangle	item_area;
	int		row, col;
	int		item;
	int		start_row, last_row;
	int		start_col, last_col;
	int		phys_last_col;

	g_return_val_if_fail(widget != NULL, FALSE);
	g_return_val_if_fail(IS_COLLECTION(widget), FALSE);
	g_return_val_if_fail(event != NULL, FALSE);

	/* Note about 'detail' argument:
	 * - If set to "base", lighthouse theme will crash
	 * - If set to NULL, cleanice theme will crash
	 *
	 * Clear the background only if we have a background pixmap.
	 */
	if (widget->style->bg_pixmap[GTK_STATE_NORMAL])
		gtk_paint_flat_box(widget->style, widget->window, GTK_STATE_NORMAL, 
				   GTK_SHADOW_NONE, &event->area,
				   widget, "collection", 0, 0, -1, -1);

	collection = COLLECTION(widget);

	/* Calculate the ranges to plot */
	start_row = event->area.y / collection->item_height;
	last_row = (event->area.y + event->area.height - 1)
		   / collection->item_height;

	if (last_row >= collection_get_rows(collection))
		last_row = collection_get_rows(collection) - 1;

	start_col = event->area.x / collection->item_width;
	phys_last_col = (event->area.x + event->area.width - 1)
			/ collection->item_width;

	/* The right-most column may be wider than the others.
	 * Therefore, to redraw the area after the last 'real' column
	 * we may have to draw the right-most column.
	 */
	if (start_col >= collection->columns)
		start_col = collection->columns - 1;

	if (phys_last_col >= collection->columns)
		last_col = collection->columns - 1;
	else
		last_col = phys_last_col;


	for(row = start_row; row <= last_row; row++) 
		for(col = start_col; col <= last_col; col++) 
	{
			item = collection_rowcol_to_item(collection, row, col);	      
			if (item == 0 || item < collection->number_of_items) {
				collection_get_item_area(collection,
							 row, col, &item_area);
		draw_one_item(collection, item, &item_area);
		}
	}

	if (collection->lasso_box)
		draw_lasso_box(collection);

	return FALSE;
}

static void default_draw_item(GtkWidget *widget,
			      CollectionItem *item,
			      GdkRectangle *area,
			      gpointer user_data)
{
	gdk_draw_arc(widget->window,
			item->selected ? widget->style->white_gc
				       : widget->style->black_gc,
			TRUE,
			area->x, area->y,
		 	COLLECTION(widget)->item_width, area->height,
			0, 360 * 64);
}


static gboolean	default_test_point(Collection *collection,
				   int point_x, int point_y,
				   CollectionItem *item,
				   int width, int height,
				   gpointer user_data)
{
	float	f_x, f_y;

	/* Convert to point in unit circle */
	f_x = ((float) point_x / width) - 0.5;
	f_y = ((float) point_y / height) - 0.5;

	return (f_x * f_x) + (f_y * f_y) <= .25;
}

static void collection_set_property(GObject      *object,
				    guint        prop_id,
				    const GValue *value,
				    GParamSpec   *pspec)
{
	Collection *collection;

	collection = COLLECTION(object);

	switch (prop_id)
	{
		case PROP_VADJUSTMENT:
			collection_set_adjustment(collection,
						  g_value_get_object(value));
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object,
							  prop_id, pspec);
			break;
	}
}

static void collection_set_adjustment(Collection    *collection,
				      GtkAdjustment *vadj)
{
	if (vadj)
		g_return_if_fail(GTK_IS_ADJUSTMENT(vadj));
	else
		vadj = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,
							 0.0, 0.0,
							 0.0, 0.0, 0.0));

	if (collection->vadj == vadj)
		return;

	if (collection->vadj)
		g_object_unref(G_OBJECT(collection->vadj));

	collection->vadj = vadj;
	g_object_ref(G_OBJECT(collection->vadj));
	gtk_object_sink(GTK_OBJECT(collection->vadj));
}

static void collection_get_property(GObject    *object,
				    guint      prop_id,
				    GValue     *value,
				    GParamSpec *pspec)
{
	Collection *collection;

	collection = COLLECTION(object);

	switch (prop_id)
	{
		case PROP_VADJUSTMENT:
			g_value_set_object(value, G_OBJECT(collection->vadj));
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object,
							  prop_id, pspec);
			break;
	}
}

static void resize_arrays(Collection *collection, guint new_size)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(new_size >= collection->number_of_items);

	collection->items = g_realloc(collection->items,
					sizeof(CollectionItem) * new_size);
	collection->array_size = new_size;
}

static gint collection_key_press(GtkWidget *widget, GdkEventKey *event)
{
	Collection *collection;
	int	   item;
	int	   key;

	g_return_val_if_fail(widget != NULL, FALSE);
	g_return_val_if_fail(IS_COLLECTION(widget), FALSE);
	g_return_val_if_fail(event != NULL, FALSE);

	collection = (Collection *) widget;
	item = collection->cursor_item;

	key = event->keyval;
	if (event->state & (GDK_CONTROL_MASK | GDK_SHIFT_MASK))
	{
		if (key == GDK_Left || key == GDK_Right || \
				key == GDK_Up || key == GDK_Down)
			return TRUE;
		return FALSE;
	}
	
	switch (key)
	{
		case GDK_Left:
			collection_move_cursor(collection, 0, -1);
			break;
		case GDK_Right:
			collection_move_cursor(collection, 0, 1);
			break;
		case GDK_Up:
			collection_move_cursor(collection, -1, 0);
			break;
		case GDK_Down:
			collection_move_cursor(collection, 1, 0);
			break;
		case GDK_Home:
			collection_set_cursor_item(collection, 0, TRUE);
			break;
		case GDK_End:
			collection_set_cursor_item(collection,
				MAX((gint) collection->number_of_items - 1, 0),
				TRUE);
			break;
		case GDK_Page_Up:
		  {
		        int first, last;
		       	get_visible_limits(collection, &first, &last);
			collection_move_cursor(collection, first - last - 1, 0);
			break;
		  }
		case GDK_Page_Down:
		  {
		        int first, last;
		       	get_visible_limits(collection, &first, &last);
			collection_move_cursor(collection, last - first + 1, 0);
			break;
		  }
		default:
			return FALSE;
	}

	return TRUE;
}

/* Wheel mouse scrolling */
static gint collection_scroll_event(GtkWidget *widget, GdkEventScroll *event)
{
	Collection    	*collection;
	int		diff = 0;

	g_return_val_if_fail(widget != NULL, FALSE);
	g_return_val_if_fail(IS_COLLECTION(widget), FALSE);
	g_return_val_if_fail(event != NULL, FALSE);

	collection = COLLECTION(widget);

	if (event->direction == GDK_SCROLL_UP)
		diff = -1;
	else if (event->direction == GDK_SCROLL_DOWN)
		diff = 1;
	else
		return FALSE;

	if (diff)
	{
		int	old_value = collection->vadj->value;
		int	new_value = 0;
		gboolean box = collection->lasso_box;
		int	step = collection->vadj->page_increment / 2;

		new_value = CLAMP(old_value + diff * step, 0.0, 
				collection->vadj->upper
				- collection->vadj->page_size);
		diff = new_value - old_value;
		if (diff)
		{
			if (box)
			{
				remove_lasso_box(collection);
				collection->drag_box_y[0] -= diff;
			}
			gtk_adjustment_set_value(collection->vadj, new_value);
			if (box)
				add_lasso_box(collection);
		}
	}

	return TRUE;
}

/* 'from' and 'to' are pixel positions. 'step' is the size of each item.
 * Returns the index of the first item covered, and the number of items.
 */
static void get_range(int from, int to, int step, gint *pos, gint *len)
{
	int	margin = MIN(step / 4, 40);

	if (from > to)
	{
		int tmp = to;
		to = from;
		from = tmp;
	}

	from = (from + margin) / step;	/* First item */
	to = (to + step - margin) / step;	/* Last item (inclusive) */

	*pos = MAX(from, 0);
	*len = to - *pos;
}

/* Fills in the area with a rectangle corresponding to the current
 * size of the lasso box (units of items, not pixels).
 *
 * The box will only span valid columns, but the total number
 * of items is not taken into account (rows or cols).
 */
static void find_lasso_area(Collection *collection, GdkRectangle *area)
{
	int	cols = collection->columns;
	int	dx = collection->drag_box_x[0] - collection->drag_box_x[1];
	int	dy = collection->drag_box_y[0] - collection->drag_box_y[1];

	if (ABS(dx) < 8 && ABS(dy) < 8)
	{
		/* Didn't move far enough - ignore */
		area->x = area->y = 0;
		area->width = 0;
		area->height = 0;
		return;
	}

	get_range(collection->drag_box_x[0], collection->drag_box_x[1],
		  collection->item_width, &area->x, &area->width);

	if (area->x >= cols)
		area->width = 0;
	else if (area->x + area->width > cols)
		area->width = cols - area->x;

	get_range(collection->drag_box_y[0], collection->drag_box_y[1],
		  collection->item_height, &area->y, &area->height);
}

static void collection_process_area(Collection	 *collection,
				    GdkRectangle *area,
				    GdkFunction  fn,
				    guint32	 time)
{
	int		x, y;
	int             rows = collection_get_rows(collection);
	int             cols = collection->columns;
	guint32		stacked_time;
	int		item;
	gboolean	changed = FALSE;
	guint		old_selected;

	g_return_if_fail(fn == GDK_SET || fn == GDK_INVERT);

	old_selected = collection->number_selected;

	stacked_time = current_event_time;
	current_event_time = time;

	collection->block_selection_changed++;

	for (y = area->y; y < area->y + area->height && y < rows; y++)
		for (x = area->x; x < area->x + area->width && x < cols; x++)
	{
			item = collection_rowcol_to_item(collection, y, x);
			if (item < collection->number_of_items) {
			if (fn == GDK_INVERT)
					collection_item_set_selected(
					    collection, item, 
					    !collection-> items[item].selected,
					FALSE);
			else
					collection_item_set_selected(
						collection, item, TRUE, FALSE);

			changed = TRUE;
		}
	}

	if (collection->number_selected && !old_selected)
		g_signal_emit(collection,
				collection_signals[GAIN_SELECTION], 0,
				current_event_time);
	else if (!collection->number_selected && old_selected)
		g_signal_emit(collection,
				collection_signals[LOSE_SELECTION], 0,
				current_event_time);
	
	collection_unblock_selection_changed(collection,
					current_event_time, changed);
	current_event_time = stacked_time;
}

static gint collection_motion_notify(GtkWidget *widget,
				     GdkEventMotion *event)
{
	Collection    	*collection;
	gint		x, y;

	g_return_val_if_fail(widget != NULL, FALSE);
	g_return_val_if_fail(IS_COLLECTION(widget), FALSE);
	g_return_val_if_fail(event != NULL, FALSE);

	collection = COLLECTION(widget);

	if (!collection->lasso_box)
		return FALSE;

	if (event->window != widget->window)
		gdk_window_get_pointer(widget->window, &x, &y, NULL);
	else
	{
		x = event->x;
		y = event->y;
	}

	remove_lasso_box(collection);
	collection->drag_box_x[1] = x;
	collection->drag_box_y[1] = y;
	add_lasso_box(collection);
	return TRUE;
}

static void add_lasso_box(Collection *collection)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(collection->lasso_box == FALSE);

	collection->lasso_box = TRUE;
	draw_lasso_box(collection);
}

static void draw_lasso_box(Collection *collection)
{
	GtkWidget	*widget;
	int		x, y, width, height;
	
	widget = GTK_WIDGET(collection);

	x = MIN(collection->drag_box_x[0], collection->drag_box_x[1]);
	y = MIN(collection->drag_box_y[0], collection->drag_box_y[1]);
	width = abs(collection->drag_box_x[1] - collection->drag_box_x[0]);
	height = abs(collection->drag_box_y[1] - collection->drag_box_y[0]);

	/* XXX: A redraw bug sometimes leaves a one-pixel dot on the screen.
	 * As a quick hack, don't draw boxes that small for now...
	 */
	if (width || height)
		gdk_draw_rectangle(widget->window, collection->xor_gc, FALSE,
			x, y, width, height);
}

static void abort_lasso(Collection *collection)
{
	if (collection->lasso_box)
		remove_lasso_box(collection);
}

static void remove_lasso_box(Collection *collection)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(collection->lasso_box == TRUE);

	draw_lasso_box(collection);

	collection->lasso_box = FALSE;

	return;
}

/* Make sure that 'item' is fully visible (vertically), scrolling if not. */
static void scroll_to_show(Collection *collection, int item)
{
        int     first, last, row, col;

	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));

	collection_item_to_rowcol(collection, item, &row, &col);
	get_visible_limits(collection, &first, &last);

	if (row <= first)
	{
		gtk_adjustment_set_value(collection->vadj,
				row * collection->item_height);
	}
	else if (row >= last)
	{
		GtkWidget	*widget = (GtkWidget *) collection;
		gint 		height;

		if (GTK_WIDGET_REALIZED(widget))
		{
			height = collection->vadj->page_size;
			gtk_adjustment_set_value(collection->vadj,
				(row + 1) * collection->item_height - height);
		}
	}
}

/* Return the first and last rows which are [partly] visible. Does not
 * ensure that the rows actually exist (contain items).
 */
static void get_visible_limits(Collection *collection, int *first, int *last)
{
	GtkWidget	*widget = (GtkWidget *) collection;
	gint		scroll = 0, height;

	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(first != NULL && last != NULL);

	if (!GTK_WIDGET_REALIZED(widget))
	{
		*first = 0;
		*last = 0;
	}
	else
	{
		scroll = collection->vadj->value;
		height = collection->vadj->page_size;

		*first = MAX(scroll / collection->item_height, 0);
		*last = (scroll + height - 1) /collection->item_height;

		if (*last < *first)
			*last = *first;
	}
}

/* Cancel the current wink effect. */
static void cancel_wink(Collection *collection)
{
	gint	item;
	
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(collection->wink_item != -1);

	item = collection->wink_item;

	collection->wink_item = -1;
	g_source_remove(collection->wink_timeout);

	collection_draw_item(collection, item, TRUE);
}

/* Draw/undraw a box around collection->wink_item */
static void invert_wink(Collection *collection)
{
	GdkRectangle area;
	gint	row, col;

	g_return_if_fail(collection->wink_item >= 0);

	if (!GTK_WIDGET_REALIZED(GTK_WIDGET(collection)))
		return;

	collection_item_to_rowcol(collection, collection->wink_item, 
				  &row, &col);
	collection_get_item_area(collection, row, col, &area);

	gdk_draw_rectangle(((GtkWidget *) collection)->window,
			collection->xor_gc, FALSE,
			area.x, area.y,
			collection->item_width - 1,
			area.height - 1);
}

static gboolean wink_timeout(Collection *collection)
{
	gint	item;
	
	g_return_val_if_fail(collection != NULL, FALSE);
	g_return_val_if_fail(IS_COLLECTION(collection), FALSE);
	g_return_val_if_fail(collection->wink_item != -1, FALSE);

	item = collection->wink_item;

	if (collection->winks_left-- > 0)
	{
		invert_wink(collection);
		return TRUE;
	}

	collection->wink_item = -1;

	collection_draw_item(collection, item, TRUE);

	return FALSE;
}

/* Change the selected state of an item.
 * Send GAIN/LOSE signals if 'signal' is TRUE.
 * Send SELECTION_CHANGED unless blocked.
 * Updates number_selected and redraws the item.
 */
static void collection_item_set_selected(Collection *collection,
                                         gint item,
                                         gboolean selected,
					 gboolean signal)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(item >= 0 && item < collection->number_of_items);

	if (collection->items[item].selected == selected)
		return;

	collection->items[item].selected = selected;
	collection_draw_item(collection, item, TRUE);

	if (selected)
	{
		collection->number_selected++;
		if (signal && collection->number_selected == 1)
			g_signal_emit(collection,
					collection_signals[GAIN_SELECTION], 0,
					current_event_time);
	}
	else
	{
		collection->number_selected--;
		if (signal && collection->number_selected == 0)
			g_signal_emit(collection,
					collection_signals[LOSE_SELECTION], 0,
					current_event_time);
	}

	EMIT_SELECTION_CHANGED(collection, current_event_time);
}

/* Functions for managing collections */

/* Remove all objects from the collection */
void collection_clear(Collection *collection)
{
	collection_delete_if(collection, NULL, NULL);
}

/* Inserts a new item at the end. The new item is unselected, and its
 * number is returned.
 */
gint collection_insert(Collection *collection, gpointer data, gpointer view)
{
	int	item;
	
	/* g_return_val_if_fail(IS_COLLECTION(collection), -1); (slow) */

	item = collection->number_of_items;

	if (item >= collection->array_size)
		resize_arrays(collection, item + (item >> 1));

	collection->items[item].data = data;
	collection->items[item].view_data = view;
	collection->items[item].selected = FALSE;

	collection->number_of_items++;

	gtk_widget_queue_resize(GTK_WIDGET(collection));

	collection_draw_item(collection, item, FALSE);

	return item;
}

void collection_unselect_item(Collection *collection, gint item)
{
	collection_item_set_selected(collection, item, FALSE, TRUE);
}

void collection_select_item(Collection *collection, gint item)
{
	collection_item_set_selected(collection, item, TRUE, TRUE);
}

void collection_toggle_item(Collection *collection, gint item)
{
	collection_item_set_selected(collection, item,
			!collection->items[item].selected, TRUE);
}

/* Select all items in the collection */
void collection_select_all(Collection *collection)
{
	GtkWidget	*widget;
	int		item = 0;
	
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));

	widget = GTK_WIDGET(collection);

	if (collection->number_selected == collection->number_of_items)
		return;		/* Nothing to do */

	while (collection->number_selected < collection->number_of_items)
	{
		while (collection->items[item].selected)
			item++;

		collection->items[item].selected = TRUE;
		collection_draw_item(collection, item, TRUE);
		item++;
		
		collection->number_selected++;
	}

	g_signal_emit(collection, collection_signals[GAIN_SELECTION], 0,
			current_event_time);
	EMIT_SELECTION_CHANGED(collection, current_event_time);
}

/* Toggle all items in the collection */
void collection_invert_selection(Collection *collection)
{
	int		item;
	
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));

	if (collection->number_selected == 0)
	{
		collection_select_all(collection);
		return;
	}
	else if (collection->number_of_items == collection->number_selected)
	{
		collection_clear_selection(collection);
		return;
	}

	for (item = 0; item < collection->number_of_items; item++)
		collection->items[item].selected =
			!collection->items[item].selected;

	collection->number_selected = collection->number_of_items -
				      collection->number_selected;
	
	/* Have to redraw everything... */
	gtk_widget_queue_draw(GTK_WIDGET(collection));
	
	EMIT_SELECTION_CHANGED(collection, current_event_time);
}

/* Unselect all items except number item, which is selected (-1 to unselect
 * everything).
 */
void collection_clear_except(Collection *collection, gint item)
{
	GtkWidget	*widget;
	int		i = 0;
	int		end;		/* Selected items to end up with */

	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(item >= -1 && item < collection->number_of_items);
	
	widget = GTK_WIDGET(collection);

	if (item == -1)
		end = 0;
	else
	{
		collection_select_item(collection, item);
		end = 1;
	}

	if (collection->number_selected == 0)
		return;

	while (collection->number_selected > end)
	{
		while (i == item || !collection->items[i].selected)
			i++;

		collection->items[i].selected = FALSE;
		collection_draw_item(collection, i, TRUE);
		i++;
		
		collection->number_selected--;
	}

	if (end == 0)
		g_signal_emit(collection, collection_signals[LOSE_SELECTION], 0,
				current_event_time);
	EMIT_SELECTION_CHANGED(collection, current_event_time);
}

/* Unselect all items in the collection */
void collection_clear_selection(Collection *collection)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));

	collection_clear_except(collection, -1);
}

/* Force a redraw of the specified item, if it is visible */
void collection_draw_item(Collection *collection, gint item, gboolean blank)
{
	GdkRectangle	area;
	GtkWidget	*widget;
	int		row, col;

	g_return_if_fail(collection != NULL);
	/* g_return_if_fail(IS_COLLECTION(collection)); (slow) */
	g_return_if_fail(item >= 0 &&
			(item == 0 || item < collection->number_of_items));

	widget = GTK_WIDGET(collection);
	if (!GTK_WIDGET_REALIZED(widget))
		return;

	collection_item_to_rowcol(collection, item, &row, &col);

	collection_get_item_area(collection, row, col, &area);

	gdk_window_invalidate_rect(widget->window, &area, FALSE);
}

void collection_set_item_size(Collection *collection, int width, int height)
{
	GtkWidget	*widget;

	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(width > 4 && height > 4);

	if (collection->item_width == width &&
			collection->item_height == height)
		return;

	widget = GTK_WIDGET(collection);

	collection->item_width = width;
	collection->item_height = height;

	if (GTK_WIDGET_REALIZED(widget))
	{
		gint		window_width;

		gdk_drawable_get_size(widget->window, &window_width, NULL);
		collection->columns = MAX(window_width / collection->item_width,
					  1);
		if (collection->cursor_item != -1)
			scroll_to_show(collection, collection->cursor_item);
		gtk_widget_queue_draw(widget);
	}

	gtk_widget_queue_resize(GTK_WIDGET(collection));
}

static int (*cmp_callback)(const void *a, const void *b) = NULL;
static int collection_cmp(const void *a, const void *b)
{
	return cmp_callback(((CollectionItem *) a)->data,
			    ((CollectionItem *) b)->data);
}
static int collection_rcmp(const void *a, const void *b)
{
	return -cmp_callback(((CollectionItem *) a)->data,
			     ((CollectionItem *) b)->data);
}

/* Cursor is positioned on item with the same data as before the sort.
 * Same for the wink item.
 */
void collection_qsort(Collection *collection,
		      int (*compar)(const void *, const void *),
		      GtkSortType order)
{
	int	cursor, wink, items, wink_on_map;
	gpointer cursor_data = NULL;
	gpointer wink_data = NULL;
	gpointer wink_on_map_data = NULL;
	CollectionItem *array;
	int	i;
	int	mul = order == GTK_SORT_ASCENDING ? 1 : -1;
	
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(compar != NULL);
	g_return_if_fail(cmp_callback == NULL);

	/* Check to see if it needs sorting (saves redrawing) */
	if (collection->number_of_items < 2)
		return;

	array = collection->items;
	for (i = 1; i < collection->number_of_items; i++)
	{
		if (mul * compar(array[i - 1].data, array[i].data) > 0)
			break;
	}
	if (i == collection->number_of_items)
		return;		/* Already sorted */

	items = collection->number_of_items;

	wink_on_map = collection->wink_on_map;
	if (wink_on_map >= 0 && wink_on_map < items)
	{
		wink_on_map_data = collection->items[wink_on_map].data;
		collection->wink_on_map = -1;
	}
	else
		wink = -1;

	wink = collection->wink_item;
	if (wink >= 0 && wink < items)
	{
		wink_data = collection->items[wink].data;
		collection->wink_item = -1;
	}
	else
		wink = -1;

	cursor = collection->cursor_item;
	if (cursor >= 0 && cursor < items)
		cursor_data = collection->items[cursor].data;
	else
		cursor = -1;
	
	cmp_callback = compar;
	qsort(collection->items, items, sizeof(collection->items[0]),
			order == GTK_SORT_ASCENDING ? collection_cmp
						    : collection_rcmp);
	cmp_callback = NULL;

	if (cursor > -1 || wink > -1 || wink_on_map > -1)
	{
		int	item;

		for (item = 0; item < items; item++)
		{
			if (collection->items[item].data == cursor_data)
				collection_set_cursor_item(collection, item,
						TRUE);
			if (collection->items[item].data == wink_on_map_data)
				collection->wink_on_map = item;
			if (collection->items[item].data == wink_data)
			{
				collection->cursor_item_old = item;
				collection->wink_item = item;
				scroll_to_show(collection, item);
			}
		}
	}
	
	gtk_widget_queue_draw(GTK_WIDGET(collection));
}

/* Find an item in a sorted collection.
 * Returns the item number, or -1 if not found.
 */
int collection_find_item(Collection *collection, gpointer data,
		         int (*compar)(const void *, const void *),
			 GtkSortType order)
{
	int	lower, upper;
	int	mul = order == GTK_SORT_ASCENDING ? 1 : -1;

	g_return_val_if_fail(collection != NULL, -1);
	g_return_val_if_fail(IS_COLLECTION(collection), -1);
	g_return_val_if_fail(compar != NULL, -1);

	/* If item is here, then: lower <= i < upper */
	lower = 0;
	upper = collection->number_of_items;

	while (lower < upper)
	{
		int	i, cmp;

		i = (lower + upper) >> 1;

		cmp = mul * compar(collection->items[i].data, data);
		if (cmp == 0)
			return i;

		if (cmp > 0)
			upper = i;
		else
			lower = i + 1;
	}

	return -1;
}

/* Return the number of the item under the point (x,y), or -1 for none.
 * This may call your test_point callback. The point is relative to the
 * collection's origin.
 */
int collection_get_item(Collection *collection, int x, int y)
{
	int		row, col;
	int		width;
	int		item;

	g_return_val_if_fail(collection != NULL, -1);

	col = x / collection->item_width;
	row = y / collection->item_height;

	if (col >= collection->columns)
		col = collection->columns - 1;

	if (col < 0 || row < 0)
		return -1;

	if (col == collection->columns - 1)
		width = collection->item_width << 1;
	else
		width = collection->item_width;

	item = collection_rowcol_to_item(collection, row, col);
	if (item >= collection->number_of_items)
		return -1;

	x -= col * collection->item_width;
	y -= row * collection->item_height;

	if (collection->test_point(collection, x, y,
	    &collection->items[item], width, collection->item_height,
	    collection->cb_user_data))
		return item;

	return -1;
}

/* Set the cursor/highlight over the given item. Passing -1
 * hides the cursor. As a special case, you may set the cursor item
 * to zero when there are no items.
 */
void collection_set_cursor_item(Collection *collection, gint item,
				gboolean may_scroll)
{
	int	old_item;
	
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(item >= -1 &&
		(item < collection->number_of_items || item == 0));

	old_item = collection->cursor_item;

	if (old_item == item)
		return;
	
	collection->cursor_item = item;
	
	if (old_item != -1)
		collection_draw_item(collection, old_item, TRUE);

	if (item != -1)
	{
		collection_draw_item(collection, item, TRUE);
		if (may_scroll)
			scroll_to_show(collection, item);
	}
	else if (old_item != -1)
		collection->cursor_item_old = old_item;
}

/* Briefly highlight an item to draw the user's attention to it.
 * -1 cancels the effect, as does deleting items, sorting the collection
 * or starting a new wink effect.
 * Otherwise, the effect will cancel itself after a short pause.
 * */
void collection_wink_item(Collection *collection, gint item)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(item >= -1 && item < collection->number_of_items);

	if (collection->wink_item != -1)
		cancel_wink(collection);
	if (item == -1)
		return;

	if (!GTK_WIDGET_MAPPED(GTK_WIDGET(collection)))
	{
		collection->wink_on_map = item;
		return;
	}

	collection->cursor_item_old = collection->wink_item = item;
	collection->winks_left = MAX_WINKS;

	collection->wink_timeout = g_timeout_add(70,
					   (GSourceFunc) wink_timeout,
					   collection);
	scroll_to_show(collection, item);
	invert_wink(collection);

	gdk_flush();
}

/* Call test(item, data) on each item in the collection.
 * Remove all items for which it returns TRUE. test() should
 * free the data before returning TRUE. The collection is in an
 * inconsistant state during this call (ie, when test() is called).
 *
 * If test is NULL, remove all items.
 */
void collection_delete_if(Collection *collection,
			  gboolean (*test)(gpointer item, gpointer data),
			  gpointer data)
{
	int	in, out = 0;
	int	selected = 0;
	int	cursor;

	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));

	cursor = collection->cursor_item;

	for (in = 0; in < collection->number_of_items; in++)
	{
		if (test && !test(collection->items[in].data, data))
		{
			/* Keep item */
			if (collection->items[in].selected)
			{
				collection->items[out].selected = TRUE;
				selected++;
			}
			else
				collection->items[out].selected = FALSE;

			collection->items[out].data =
				collection->items[in].data;
			collection->items[out].view_data =
				collection->items[in].view_data;
			out++;
		}
		else 
		{
			/* Remove item */
			if (collection->free_item)
				collection->free_item(collection,
							&collection->items[in]);
			
			if (collection->cursor_item >= in)
				cursor--;
		}
	}

	if (in != out)
	{
		collection->cursor_item = cursor;

		if (collection->wink_item != -1)
		{
			collection->wink_item = -1;
			g_source_remove(collection->wink_timeout);
		}
		
		collection->number_of_items = out;
		if (collection->number_selected && !selected)
		{
			/* We've lost all the selected items */
			g_signal_emit(collection,
					collection_signals[LOSE_SELECTION], 0,
					current_event_time);
		}

		collection->number_selected = selected;
		resize_arrays(collection,
			MAX(collection->number_of_items, MINIMUM_ITEMS));

		if (GTK_WIDGET_REALIZED(GTK_WIDGET(collection)))
			gtk_widget_queue_draw(GTK_WIDGET(collection));

		gtk_widget_queue_resize(GTK_WIDGET(collection));
	}
}

/* Move the cursor by the given row and column offsets.
 * Moving by (0,0) can be used to simply make the cursor appear.
 */
void collection_move_cursor(Collection *collection, int drow, int dcol)
{
	int	row, col, item;
	int	first, last, total_rows, total_cols;

	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));

	if (!collection->number_of_items)
	{
		/* Show the cursor, even though there are no items */
		collection_set_cursor_item(collection, 0, TRUE);
		return;
	}

	get_visible_limits(collection, &first, &last);
	total_rows = collection_get_rows(collection);
	total_cols = collection_get_cols(collection);

	item = collection->cursor_item;
	if (item == -1)
	{
		item = MIN(collection->cursor_item_old,
			   collection->number_of_items - 1);
	}

	if (item == -1)
	{
		col = 0;
		row = first;
	}
	else
	{
		collection_item_to_rowcol(collection, item, &row, &col);
		
		col += dcol;
		if (collection->vertical_order) 
		{
			col = MAX(0,col);
			col = MIN(col, total_cols - 1);
		}

		if (row < first)
			row = first;
		else if (row > last)
			row = last;
		else {
			row += drow;
			if (collection->vertical_order) 
			{
				if (row >= total_rows)
				{
					row = 0;
					col += 1;
				}
			}
			else 
			{
				row = MAX(row, 0); 
				row = MIN(row, total_rows - 1);
			}
		}
	}

	item = collection_rowcol_to_item(collection, row, col);

	item = MAX(item, 0);
	item = MIN(item, collection->number_of_items-1);

	collection_set_cursor_item(collection, item, TRUE);
}

/* Start a lasso box drag */
void collection_lasso_box(Collection *collection, int x, int y)
{
	collection->drag_box_x[0] = x;
	collection->drag_box_y[0] = y;
	collection->drag_box_x[1] = x;
	collection->drag_box_y[1] = y;

	add_lasso_box(collection);
}

/* Remove the lasso box. Applies fn to each item inside the box.
 * fn may be GDK_INVERT, GDK_SET, GDK_NOOP or GDK_CLEAR.
 */
void collection_end_lasso(Collection *collection, GdkFunction fn)
{
	if (fn != GDK_CLEAR)
	{
		GdkRectangle	region;

		find_lasso_area(collection, &region);

		collection_process_area(collection, &region, fn,
				GDK_CURRENT_TIME);
	}

	abort_lasso(collection);
}

/* Unblock the selection_changed signal, emitting the signal if the
 * block counter reaches zero and emit is TRUE.
 */
void collection_unblock_selection_changed(Collection	*collection,
					  guint		time,
					  gboolean	emit)
{
	g_return_if_fail(collection != NULL);
	g_return_if_fail(IS_COLLECTION(collection));
	g_return_if_fail(collection->block_selection_changed > 0);

	collection->block_selection_changed--;

	if (emit && !collection->block_selection_changed)
		g_signal_emit(collection,
				collection_signals[SELECTION_CHANGED], 0, time);
}

/* Translate the item number to the (row, column) form */
void    collection_item_to_rowcol       (const Collection *collection,
					 int item, int *row, int *col)
{
	if (!collection->vertical_order) 
	{
		*row = item / collection->columns;
		*col = item % collection->columns; 
	}
	else 
	{
		int rows = collection_get_rows(collection);
		*row = item % rows;
		*col = item / rows;
	}
}



/* Translate the (row, column) form to the item number.
 * May return a number >= collection->number_of_items.
 */
int collection_rowcol_to_item(const Collection *collection, int row, int col)
{
	if (!collection->vertical_order) 
		return row * collection->columns + col;
	else 
	{
		int rows = collection_get_rows(collection);
		if (row >= rows)
			return collection->number_of_items;
		return row + col * rows;
	}
}
