/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __VIEW_IFACE_H__
#define __VIEW_IFACE_H__

#define AUTOSCROLL_STEP 20

#include <glib-object.h>
#include <gdk/gdk.h>

typedef enum {
	/* iter->next moves to selected items only */
	VIEW_ITER_SELECTED	= 1 << 0,

	/* iteration starts from cursor (first call to next() returns
	 * iter AFTER cursor). If there is no cursor, flag is ignored
	 * (will iterate over everything).
	 */
	VIEW_ITER_FROM_CURSOR	= 1 << 1,

	/* next() moves backwards */
	VIEW_ITER_BACKWARDS	= 1 << 2,

	/* next() always returns NULL and has no effect */
	VIEW_ITER_ONE_ONLY	= 1 << 3,

	/* Like FROM_CURSOR, but using the base position. The base is set
	 * from the cursor position when the path minibuffer is opened.
	 */
	VIEW_ITER_FROM_BASE	= 1 << 4,
} IterFlags;

typedef struct _ViewIfaceClass	ViewIfaceClass;

/* A viewport containing a Collection which also handles redraw.
 * This is the Collection-based implementation of the View interface.
 */
typedef struct _ViewCollection ViewCollection;

struct _ViewIter {
	/* Returns the value last returned by next() */
	DirItem	   *(*peek)(ViewIter *iter);

	DirItem	   *(*next)(ViewIter *iter);

	/* private fields */
	ViewIface   *view;
	int	   i, n_remaining;
	int	   flags;
};

struct _ViewIfaceClass {
	GTypeInterface base_iface;

	void (*sort)(ViewIface *obj);
	void (*style_changed)(ViewIface *obj, int flags);
	void (*add_items)(ViewIface *obj, GPtrArray *items);
	void (*update_items)(ViewIface *obj, GPtrArray *items);
	void (*delete_if)(ViewIface *obj,
			gboolean (*test)(gpointer item, gpointer data),
			gpointer data);
	void (*clear)(ViewIface *obj);
	void (*select_all)(ViewIface *obj);
	void (*clear_selection)(ViewIface *obj);
	int (*count_items)(ViewIface *obj);
	int (*count_selected)(ViewIface *obj);
	void (*show_cursor)(ViewIface *obj);

	void (*get_iter)(ViewIface *obj, ViewIter *iter, IterFlags flags);
	void (*get_iter_at_point)(ViewIface *obj, ViewIter *iter,
				  GdkWindow *src, int x, int y);
	void (*cursor_to_iter)(ViewIface *obj, ViewIter *iter);

	void (*set_selected)(ViewIface *obj, ViewIter *iter, gboolean selected);
	gboolean (*get_selected)(ViewIface *obj, ViewIter *iter);
	void (*set_frozen)(ViewIface *obj, gboolean frozen);
	void (*select_only)(ViewIface *obj, ViewIter *iter);
	void (*wink_item)(ViewIface *obj, ViewIter *iter);
	void (*autosize)(ViewIface *obj);
	gboolean (*cursor_visible)(ViewIface *obj);
	void (*set_base)(ViewIface *obj, ViewIter *iter);
	void (*start_lasso_box)(ViewIface *obj, GdkEventButton *event);
	void (*extend_tip)(ViewIface *obj, ViewIter *iter, GString *tip);
	gboolean (*auto_scroll_callback)(ViewIface *obj);
};

#define VIEW_TYPE_IFACE           (view_iface_get_type())

#define VIEW(obj)		  (G_TYPE_CHECK_INSTANCE_CAST((obj), \
				   VIEW_TYPE_IFACE, ViewIface))

#define VIEW_IS_IFACE(obj)	  (G_TYPE_CHECK_INSTANCE_TYPE((obj), \
				   VIEW_TYPE_IFACE))

#define VIEW_IFACE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_INTERFACE((obj), \
				   VIEW_TYPE_IFACE, ViewIfaceClass))

/* Flags for view_style_changed() */
enum {
	VIEW_UPDATE_VIEWDATA	= 1 << 0,
	VIEW_UPDATE_NAME	= 1 << 1,
	VIEW_UPDATE_HEADERS	= 1 << 2,
};

GType view_iface_get_type(void);
void view_sort(ViewIface *obj);
void view_style_changed(ViewIface *obj, int flags);
gboolean view_autoselect(ViewIface *obj, const gchar *leaf);
void view_add_items(ViewIface *obj, GPtrArray *items);
void view_update_items(ViewIface *obj, GPtrArray *items);
void view_delete_if(ViewIface *obj,
		    gboolean (*test)(gpointer item, gpointer data),
		    gpointer data);
void view_clear(ViewIface *obj);
void view_select_all(ViewIface *obj);
void view_clear_selection(ViewIface *obj);
int view_count_items(ViewIface *obj);
int view_count_selected(ViewIface *obj);
void view_show_cursor(ViewIface *obj);

void view_get_iter(ViewIface *obj, ViewIter *iter, IterFlags flags);
void view_get_iter_at_point(ViewIface *obj, ViewIter *iter,
			    GdkWindow *src, int x, int y);
void view_get_cursor(ViewIface *obj, ViewIter *iter);
void view_cursor_to_iter(ViewIface *obj, ViewIter *iter);

void view_set_selected(ViewIface *obj, ViewIter *iter, gboolean selected);
gboolean view_get_selected(ViewIface *obj, ViewIter *iter);
void view_select_only(ViewIface *obj, ViewIter *iter);
void view_freeze(ViewIface *obj);
void view_thaw(ViewIface *obj);
void view_select_if(ViewIface *obj,
		    gboolean (*test)(ViewIter *iter, gpointer data),
		    gpointer data);

void view_wink_item(ViewIface *obj, ViewIter *iter);
void view_autosize(ViewIface *obj);
gboolean view_cursor_visible(ViewIface *obj);
void view_set_base(ViewIface *obj, ViewIter *iter);
void view_start_lasso_box(ViewIface *obj, GdkEventButton *event);
void view_extend_tip(ViewIface *obj, ViewIter *iter, GString *tip);
gboolean view_auto_scroll_callback(ViewIface *obj);

#endif /* __VIEW_IFACE_H__ */
