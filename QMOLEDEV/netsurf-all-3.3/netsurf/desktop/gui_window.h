/*
 * Copyright 2014 Vincent Sanders <vince@netsurf-browser.org>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * \file
 *
 * Interface to platform-specific graphical user interface window
 * operations.
 */

#ifndef _NETSURF_DESKTOP_GUI_WINDOW_H_
#define _NETSURF_DESKTOP_GUI_WINDOW_H_

typedef enum gui_save_type {
	GUI_SAVE_SOURCE,
	GUI_SAVE_DRAW,
	GUI_SAVE_PDF,
	GUI_SAVE_TEXT,
	GUI_SAVE_COMPLETE,
	GUI_SAVE_OBJECT_ORIG,
	GUI_SAVE_OBJECT_NATIVE,
	GUI_SAVE_LINK_URI,
	GUI_SAVE_LINK_URL,
	GUI_SAVE_LINK_TEXT,
	GUI_SAVE_HOTLIST_EXPORT_HTML,
	GUI_SAVE_HISTORY_EXPORT_HTML,
	GUI_SAVE_TEXT_SELECTION,
	GUI_SAVE_CLIPBOARD_CONTENTS
} gui_save_type;

typedef enum {
	GDRAGGING_NONE,
	GDRAGGING_SCROLLBAR,
	GDRAGGING_SELECTION,
	GDRAGGING_OTHER
} gui_drag_type;

typedef enum {
	GW_CREATE_NONE		= 0,		/* New window */
	GW_CREATE_CLONE		= (1 << 0),	/* Clone existing window */
	GW_CREATE_TAB		= (1 << 1)	/* In same window as existing */
} gui_window_create_flags;

struct browser_window;
struct form_control;
struct rect;
struct hlcache_handle;
struct nsurl;

enum gui_pointer_shape;

/**
 * Graphical user interface window function table.
 *
 * function table implementing window operations
 */
struct gui_window_table {

	/* Mandantory entries */

	/**
	 * Create and open a gui window for a browsing context.
	 *
	 * \param bw		bw to create gui_window for
	 * \param existing	an existing gui_window, may be NULL
	 * \param flags		flags for gui window creation
	 * \return gui window, or NULL on error
	 *
	 * If GW_CREATE_CLONE flag is set existing is non-NULL.
	 *
	 * Front end's gui_window must include a reference to the
	 * browser window passed in the bw param.
	 */
	struct gui_window *(*create)(struct browser_window *bw,
			struct gui_window *existing,
			gui_window_create_flags flags);

	/**
	 * Destroy previously created gui window
	 *
	 * \param gw The gui window to destroy.
	 */
	void (*destroy)(struct gui_window *gw);

	/**
	 * Force a redraw of the entire contents of a window.
	 *
	 * @todo this API should be merged with update.
	 *
	 * \param g gui_window to redraw
	 */
	void (*redraw)(struct gui_window *g);

	/**
	 * Redraw an area of a window.
	 *
	 * \param g gui_window
	 * \param rect area to redraw
	 */
	void (*update)(struct gui_window *g, const struct rect *rect);

	/**
	 * Get the scroll position of a browser window.
	 *
	 * \param  g   gui_window
	 * \param  sx  receives x ordinate of point at top-left of window
	 * \param  sy  receives y ordinate of point at top-left of window
	 * \return true iff successful
	 */
	bool (*get_scroll)(struct gui_window *g, int *sx, int *sy);

	/**
	 * Set the scroll position of a browser window.
	 *
	 * \param  g   gui_window to scroll
	 * \param  sx  point to place at top-left of window
	 * \param  sy  point to place at top-left of window
	 */
	void (*set_scroll)(struct gui_window *g, int sx, int sy);

	/**
	 * Find the current dimensions of a browser window's content area.
	 *
	 * @todo The implementations of this are buggy and its only
	 * used from frames code.
	 *
	 * \param g	 gui_window to measure
	 * \param width	 receives width of window
	 * \param height receives height of window
	 * \param scaled whether to return scaled values
	 */
	void (*get_dimensions)(struct gui_window *g, int *width, int *height, bool scaled);

	/**
	 * Update the extent of the inside of a browser window to that of the
	 * current content.
	 *
	 * @todo this is used to update scroll bars does it need
	 * renaming? some frontends (windows) do not even implement it.
	 *
	 * \param  g gui_window to update the extent of
	 */
	void (*update_extent)(struct gui_window *g);

	/**
	 * Reformat a window.
	 *
	 * This is used to perform reformats when the page contents
	 * require reformating. The reformat is requested using
	 * browser_window_schedule_reformat and occours via a scheduled
	 * callback hence from top level context.
	 *
	 * \param g gui_window to reformat.
	 */
	void (*reformat)(struct gui_window *g);


	/* Optional entries */

	/**
	 * Set the title of a window.
	 *
	 * \param  g	  window to update
	 * \param  title  new window title
	 */
	void (*set_title)(struct gui_window *g, const char *title);

	/**
	 * Set the navigation url.
	 *
	 * \param gw window to update.
	 * \param url The url to use as icon.
	 */
	nserror (*set_url)(struct gui_window *gw, struct nsurl *url);

	/**
	 * Set a favicon for a gui window.
	 *
	 * \param gw window to update.
	 * \param icon handle to object to use as icon.
	 */
	void (*set_icon)(struct gui_window *gw, struct hlcache_handle *icon);

	/**
	 * Set the status bar of a browser window.
	 *
	 * \param  g	 gui_window to update
	 * \param  text  new status text
	 */
	void (*set_status)(struct gui_window *g, const char *text);

	/**
	 * Change mouse pointer shape
	 */
	void (*set_pointer)(struct gui_window *g, enum gui_pointer_shape shape);

	/**
	 * Place the caret in a browser window.
	 *
	 * \param  g	   window with caret
	 * \param  x	   coordinates of caret
	 * \param  y	   coordinates of caret
	 * \param  height  height of caret
	 * \param  clip	   clip rectangle, or NULL if none
	 */
	void (*place_caret)(struct gui_window *g, int x, int y, int height, const struct rect *clip);

	/**
	 * Remove the caret, if present.
	 *
	 * \param g window with caret
	 */
	void (*remove_caret)(struct gui_window *g);

	/** start the navigation throbber. */
	void (*start_throbber)(struct gui_window *g);

	/** stop the navigation throbber. */
	void (*stop_throbber)(struct gui_window *g);

	/** start a drag operation within a window */
	bool (*drag_start)(struct gui_window *g, gui_drag_type type, const struct rect *rect);

	/** save link operation */
	nserror (*save_link)(struct gui_window *g, struct nsurl *url, const char *title);

	/**
	 * Scrolls the specified area of a browser window into view.
	 *
	 * @todo investigate if this can be merged with set_scroll
	 * which is what the default implementation used by most
	 * toolkits uses.
	 *
	 * \param  g   gui_window to scroll
	 * \param  x0  left point to ensure visible
	 * \param  y0  bottom point to ensure visible
	 * \param  x1  right point to ensure visible
	 * \param  y1  top point to ensure visible
	 */
	void (*scroll_visible)(struct gui_window *g, int x0, int y0, int x1, int y1);

	/**
	 * Starts drag scrolling of a browser window
	 *
	 * \param g the window to scroll
	 */
	bool (*scroll_start)(struct gui_window *g);

	/**
	 * Called when the gui_window has new content.
	 *
	 * \param  g  the gui_window that has new content
	 */
	void (*new_content)(struct gui_window *g);

	/**
	 * create a form select menu
	 */
	void (*create_form_select_menu)(struct gui_window *gw, struct form_control *control);

	/**
	 * Called when file chooser gadget is activated
	 */
	void (*file_gadget_open)(struct gui_window *g, struct hlcache_handle *hl, struct form_control *gadget);

	/** object dragged to window*/
	void (*drag_save_object)(struct gui_window *g, struct hlcache_handle *c, gui_save_type type);

	/** drag selection save */
	void (*drag_save_selection)(struct gui_window *g, const char *selection);

	/** selection started */
	void (*start_selection)(struct gui_window *g);
};

#endif
