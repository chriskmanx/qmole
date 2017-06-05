/*
 * Copyright 2003 Phil Mellor <monkeyson@users.sourceforge.net>
 * Copyright 2006 James Bursa <bursa@users.sourceforge.net>
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

/** \file
 * Browser window creation and manipulation (interface).
 */

#ifndef _NETSURF_DESKTOP_BROWSER_H_
#define _NETSURF_DESKTOP_BROWSER_H_

#include <stdbool.h>
#include <stdio.h>

#include "utils/errors.h"
#include "desktop/plot_style.h"
#include "desktop/frame_types.h"
#include "desktop/mouse.h"

struct browser_window;
struct hlcache_handle;
struct gui_window;
struct history;
struct selection;
struct fetch_multipart_data;
struct form_control;
struct nsurl;
struct rect;
struct redraw_context;
enum content_debug;

typedef enum {
	DRAGGING_NONE,
	DRAGGING_SELECTION,
	DRAGGING_PAGE_SCROLL,
	DRAGGING_FRAME,
	DRAGGING_SCR_X,
	DRAGGING_SCR_Y,
	DRAGGING_CONTENT_SCROLLBAR,
	DRAGGING_OTHER
} browser_drag_type;

typedef enum {
	BW_EDITOR_NONE		=  0,		/**< No selection, no editing */
	BW_EDITOR_CAN_COPY	= (1 << 0),	/**< Have selection */
	BW_EDITOR_CAN_CUT  	= (1 << 1),	/**< Selection not read-only */
	BW_EDITOR_CAN_PASTE	= (1 << 2)	/**< Can paste, input */
} browser_editor_flags;

typedef enum {
	BW_SCROLLING_AUTO,
	BW_SCROLLING_YES,
	BW_SCROLLING_NO
} browser_scrolling;

/** flags to browser_window_create */
enum browser_window_create_flags {
	/** No flags set */
	BW_CREATE_NONE			= 0,

	/** this will form a new history node (don't set for back/reload/etc) */
	BW_CREATE_HISTORY		= (1 << 0),

	/** New gui_window to be tab in same window as "existing" gui_window */
	BW_CREATE_TAB			= (1 << 1),

	/** New gui_window to be clone of "existing" gui_window */
	BW_CREATE_CLONE			= (1 << 2),

	/** Window not opened by user interaction (e.g. JS popup)
	 *
	 * rfc2965:
	 *    A transaction is verifiable if the user, or a
	 *    user-designated agent, has the option to review
	 *    the request-URI prior to its use in the transaction.
	 *    A transaction is unverifiable if the user does not
	 *    have that option.
	 */
	BW_CREATE_UNVERIFIABLE		= (1 << 3),
};

/** flags to browser_window_navigate  */
enum browser_window_nav_flags {
	/** No flags set */
	BW_NAVIGATE_NONE		= 0,

	/** this will form a new history node (don't set for back/reload/etc) */
	BW_NAVIGATE_HISTORY		= (1 << 0),

	/** download rather than render the uri */
	BW_NAVIGATE_DOWNLOAD		= (1 << 1),

	/** Transation not caused by user interaction (e.g. JS-caused)
	 *
	 * rfc2965:
	 *    A transaction is verifiable if the user, or a
	 *    user-designated agent, has the option to review
	 *    the request-URI prior to its use in the transaction.
	 *    A transaction is unverifiable if the user does not
	 *    have that option.
	 */
	BW_NAVIGATE_UNVERIFIABLE	= (1 << 2)
};

/**
 * Page features at a specific spatial location.
 */
struct browser_window_features {
	/** URL of a link or NULL. */
	struct nsurl *link;

	/** Object at position or NULL. */
	struct hlcache_handle *object;

	/** handle of top level content. */
	struct hlcache_handle *main;

	/** type of form feature. */
	enum {
		CTX_FORM_NONE,
		CTX_FORM_TEXT,
		CTX_FORM_FILE
	} form_features;
};

/**
 * Create and open a new root browser window with the given page.
 *
 * \param flags		Flags to control operation
 * \param url		URL to fetch in the new window or NULL for blank
 * \param referrer	The referring uri or NULL if none
 * \param existing	The an existing bw or NULL, required for some flags.
 * \param bw		Updated to created browser window or untouched on error.
 * \return NSERROR_OK, or appropriate error otherwise.
 */
nserror browser_window_create(enum browser_window_create_flags flags,
		struct nsurl *url, struct nsurl *referrer,
		struct browser_window *existing,
		struct browser_window **bw);

/**
 * Start fetching a page in a browser window.
 *
 * \param bw		  browser window
 * \param url		  URL to start fetching
 * \param flags           Flags to control operation
 * \param referrer	  The referring uri or NULL if none
 * \param post_urlenc	  url encoded post data or NULL if none
 * \param post_multipart  multipart post data or NULL if none
 * \param parent	  Parent content or NULL if none
 *
 * Any existing fetches in the window are aborted.
 *
 * If post_urlenc and post_multipart are NULL the url is fetched using
 * GET rather than POST.
 *
 */
nserror browser_window_navigate(struct browser_window *bw,
			     struct nsurl *url,
			     struct nsurl *referrer,
			     enum browser_window_nav_flags flags,
			     char *post_urlenc,
			     struct fetch_multipart_data *post_multipart,
			     struct hlcache_handle *parent);

/**
 * Return true if a browser window can navigate upwards.
 *
 * \param bw the browser window to test.
 * \return true if navigation up is possible otherwise false.
 */
bool browser_window_up_available(struct browser_window *bw);

/**
 * Navigate to a browser_window's parent URL.
 *
 * \param bw		browser window
 * \param new_window	whether to open parent in a new window, or existing
 */
nserror browser_window_navigate_up(struct browser_window *bw, bool new_window);

/**
 * Get a browser window's URL.
 *
 * \param bw browser window
 * \return pointer to nsurl. Doesn't create a ref for caller.
 *
 * \note guaranteed to return a valid nsurl ptr, never returns NULL.
 */
struct nsurl* browser_window_get_url(struct browser_window *bw);

/**
 * Get the title of a browser_window.
 *
 * \param bw The browser window.
 */
const char* browser_window_get_title(struct browser_window *bw);

/**
 * Get a browser window's history object.
 *
 * \param bw	  browser window
 * \return pointer browser window's history object
 *
 * Clients need history object to make use of the history_* functions.
 */
struct history * browser_window_get_history(struct browser_window *bw);

/**
 * Get a browser window's content extents.
 *
 * \param bw	  browser window
 * \param scaled  whether to apply current browser window scale
 * \param width   updated to content width extent in px
 * \param height  updated to content height extent in px
 * \return NSERROR_OK, or appropriate error otherwise.
 */
nserror browser_window_get_extents(struct browser_window *bw, bool scaled,
		int *width, int *height);

/**
 * Find out if a browser window is currently showing a content.
 *
 * \param bw	  browser window
 * \return true iff browser window is showing a content, else false.
 */
bool browser_window_has_content(struct browser_window *bw);

/**
 * Get a cache handle for the content within a browser window.
 */
struct hlcache_handle *browser_window_get_content(struct browser_window *bw);

/**
 * Set the dimensions of the area a browser window occupies
 *
 * \param  bw      The browser window to set dimensions of
 * \param  width   Width in pixels
 * \param  height  Height in pixels
 */
void browser_window_set_dimensions(struct browser_window *bw,
		int width, int height);

/**
 * Redraw browser window, set extent to content, and update title.
 *
 * \param  bw		  browser_window
 * \param  scroll_to_top  move view to top of page
 */
void browser_window_update(struct browser_window *bw, bool scroll_to_top);

/**
 * update an area of a browser window.
 *
 * \param bw The browser window to update.
 * \param rect The area to redraw
 */
void browser_window_update_box(struct browser_window *bw, struct rect *rect);

/**
 * Stop all fetching activity in a browser window.
 *
 * \param bw The browser window to stop activity in.
 */
void browser_window_stop(struct browser_window *bw);

/**
 * Reload the page in a browser window.
 *
 * \param  bw  browser window
 * \param  all whether to reload all objects associated with the page
 */
void browser_window_reload(struct browser_window *bw, bool all);

/**
 * Close and destroy a browser window.
 *
 * \param  bw  browser window
 */
void browser_window_destroy(struct browser_window *bw);

/**
 * Reformat a browser window contents to a new width or height.
 *
 * \param bw         The browser window to reformat.
 * \param background Reformat in the background.
 * \param width      new width
 * \param height     new height
 */
void browser_window_reformat(struct browser_window *bw, bool background,
		int width, int height);


/**
 * Sets the scale of a browser window.
 *
 * \param bw The browser window to scale.
 * \param scale The new scale.
 * \param all Scale all windows in the tree (ie work up as well as down)
 */
void browser_window_set_scale(struct browser_window *bw, float scale, bool all);


/**
 * Gets the scale of a browser window
 *
 * \param bw The browser window to get the scale of.
 * \return The scale of the window.
 */
float browser_window_get_scale(struct browser_window *bw);

/**
 * Get access to any page features at the given coordinates.
 *
 * Fetches page features like content, link URLs and objects (images)
 * at the specified co-ordinates within the browsing context.
 *
 * Fields within the supplied features structure are updated with
 * pointers to any relevent content, or set to NULL if none.
 *
 * \param[in] bw browser window to examine.
 * \param[in] x x-coordinate of point of interest
 * \param[in] y y-coordinate of point of interest
 * \param[out] data Feature structure to update.
 * \return NSERROR_OK or appropriate error code on faliure.
 */
nserror browser_window_get_features(struct browser_window *bw,
		int x, int y, struct browser_window_features *data);

/**
 * Send a scroll request to a browser window at a particular point.  The
 * 'deepest' scrollable object which can be scrolled in the requested
 * direction at the given point will consume the scroll.
 *
 * \param bw	browser window to look inside
 * \param x	x-coordinate of point of interest
 * \param y	y-coordinate of point of interest
 * \param scrx	number of px try to scroll something in x direction
 * \param scry	number of px try to scroll something in y direction
 * \return true iff scroll request has been consumed
 */
bool browser_window_scroll_at_point(struct browser_window *bw,
		int x, int y, int scrx, int scry);

/**
 * Drop a file onto a browser window at a particular point, or determine if a
 * file may be dropped onto the content at given point.
 *
 * \param bw	browser window to look inside
 * \param x	x-coordinate of point of interest
 * \param y	y-coordinate of point of interest
 * \param file	path to file to be dropped, or NULL to know if drop allowed
 * \return true iff file drop has been handled, or if drop possible (NULL file)
 */
bool browser_window_drop_file_at_point(struct browser_window *bw,
		int x, int y, char *file);

/**
 * set filename on form control.
 *
 * \param bw browser window to look inside.
 * \param gadget form control.
 * \param fn filename to set.
 */
void browser_window_set_gadget_filename(struct browser_window *bw,
		struct form_control *gadget, const char *fn);

/**
 * Update URL bar for a given browser window to bw's content's URL
 *
 * \param bw Browser window to update URL bar for.
 */
nserror browser_window_refresh_url_bar(struct browser_window *bw);

/**
 * Handle mouse clicks in a browser window.
 *
 * \param  bw	  browser window
 * \param  mouse  state of mouse buttons and modifier keys
 * \param  x	  coordinate of mouse
 * \param  y	  coordinate of mouse
 */
void browser_window_mouse_click(struct browser_window *bw,
		browser_mouse_state mouse, int x, int y);

/**
 * Handle non-click mouse action in a browser window. (drag ends, movements)
 *
 * \param  bw	  browser window
 * \param  mouse  state of mouse buttons and modifier keys
 * \param  x	  coordinate of mouse
 * \param  y	  coordinate of mouse
 */
void browser_window_mouse_track(struct browser_window *bw,
		browser_mouse_state mouse, int x, int y);

/**
 * Locate a browser window in the specified stack according.
 *
 * \param bw  the browser_window to search all relatives of
 * \param target  the target to locate
 * \param mouse The current mouse state
 * \return The browser window the mouse is in
 */
struct browser_window *browser_window_find_target(
		struct browser_window *bw, const char *target,
		browser_mouse_state mouse);

/**
 * Cause the frontends reformat entry to be called in safe context.
 *
 * The browser_window_reformat call cannot safely be called from some
 * contexts, this call allows for the reformat to happen from a safe
 * top level context.
 *
 * The callback is frontend provided as the context information (size
 * etc.) about the windowing toolkit is only available to the
 * frontend.
 */
nserror browser_window_schedule_reformat(struct browser_window *bw);



void browser_select_menu_callback(void *client_data,
		int x, int y, int width, int height);

/**
 * Redraw a rectangular region of a browser window.
 *
 * \param  bw	  browser window to be redrawn
 * \param  x	  x co-ord of top-left
 * \param  y	  y co-ord of top-left
 * \param  width  width of rectangle
 * \param  height height of rectangle
 */
void browser_window_redraw_rect(struct browser_window *bw, int x, int y,
		int width, int height);

/**
 * Change the shape of the mouse pointer
 *
 * \param bw Browser window to set shape in
 * \param shape The pointer shape to use
 */
void browser_window_set_pointer(struct browser_window *bw,
		browser_pointer_shape shape);

/**
 * Start drag scrolling the contents of the browser window
 *
 * \param bw  browser window
 * \param x   x ordinate of initial mouse position
 * \param y   y ordinate
 */
void browser_window_page_drag_start(struct browser_window *bw, int x, int y);

/**
 * Check availability of Back action for a given browser window
 *
 * \param bw  browser window
 * \return true if Back action is available
 */
bool browser_window_back_available(struct browser_window *bw);

/**
 * Check availability of Forward action for a given browser window
 *
 * \param bw  browser window
 * \return true if Forward action is available
 */
bool browser_window_forward_available(struct browser_window *bw);

/**
 * Check availability of Reload action for a given browser window
 *
 * \param bw  browser window
 * \return true if Reload action is available
 */
bool browser_window_reload_available(struct browser_window *bw);

/**
 * Check availability of Stop action for a given browser window
 *
 * \param bw  browser window
 * \return true if Stop action is available
 */
bool browser_window_stop_available(struct browser_window *bw);

/**
 * Redraw an area of a window.
 *
 * Calls the redraw function for the content.
 *
 * \param  bw    The window to redraw
 * \param  x     coordinate for top-left of redraw
 * \param  y     coordinate for top-left of redraw
 * \param  clip  clip rectangle coordinates
 * \param  ctx   redraw context
 * \return true if successful, false otherwise
 *
 * The clip rectangle is guaranteed to be filled to its extents, so there is
 * no need to render a solid background first.
 *
 * x, y and clip are coordinates from the top left of the canvas area.
 *
 * The top left corner of the clip rectangle is (x0, y0) and
 * the bottom right corner of the clip rectangle is (x1, y1).
 * Units for x, y and clip are pixels.
 */
bool browser_window_redraw(struct browser_window *bw, int x, int y,
		const struct rect *clip, const struct redraw_context *ctx);

/**
 * Check whether browser window is ready for redraw
 *
 * \param  bw    The window to redraw
 * \return true if browser window is ready for redraw
 */
bool browser_window_redraw_ready(struct browser_window *bw);

/**
 * Get the position of the current browser window with respect to the root or
 * parent browser window
 *
 * \param  bw     browser window to get the position of
 * \param  root   true if we want position wrt root bw, false if wrt parent bw
 * \param  pos_x  updated to x position of bw
 * \param  pos_y  updated to y position of bw
 */
void browser_window_get_position(struct browser_window *bw, bool root,
		int *pos_x, int *pos_y);

/**
 * Set the position of the current browser window with respect to the parent
 * browser window
 *
 * \param  bw     browser window to set the position of
 * \param  x      x position of bw
 * \param  y      y position of bw
 */
void browser_window_set_position(struct browser_window *bw, int x, int y);

/**
 * Scroll the browser window to display the passed area
 *
 * \param  bw		browser window to scroll
 * \param  rect		area to display
 */
void browser_window_scroll_visible(struct browser_window *bw,
		const struct rect *rect);

/**
 * Set scroll offsets for a browser window.
 *
 * \param  bw	    The browser window
 * \param  x	    The x scroll offset to set
 * \param  y	    The y scroll offset to set
 *
 * \todo Do we really need this and browser_window_scroll_visible?
 *         Ditto for gui_window_* variants.
 */
void browser_window_set_scroll(struct browser_window *bw, int x, int y);

/**
 * Set drag type for a browser window, and inform front end
 *
 * \param  bw     browser window to set the type of the current drag for
 * \param  type   drag type
 * \param  rect   area pointer may be confined to, during drag, or NULL
 */
void browser_window_set_drag_type(struct browser_window *bw,
		browser_drag_type type, const struct rect *rect);

/**
 * Get type of any current drag for a browser window
 *
 * \param  bw     browser window to set the type of the current drag for
 * \return  drag type
 */
browser_drag_type browser_window_get_drag_type(struct browser_window *bw);

/**
 * Check whether browser window can accept a cut/copy/paste, or has a selection
 * that could be saved.
 *
 * \param  bw    The browser window
 * \return flags indicating editor flags
 */
browser_editor_flags browser_window_get_editor_flags(struct browser_window *bw);

/**
 * Find out if given browser window content is selectable
 *
 * \param bw	browser window to look at
 * \return true iff browser window is selectable
 */
bool browser_window_can_select(struct browser_window *bw);

/**
 * Get the current selection from a root browser window, ownership passed to
 * caller, who must free() it.
 *
 * \param  bw    The browser window
 * \return the selected text string, or NULL
 */
char * browser_window_get_selection(struct browser_window *bw);

/**
 * Find out if given browser window can be searched
 *
 * \param bw	browser window to look at
 * \return true iff browser window is searchable
 */
bool browser_window_can_search(struct browser_window *bw);

/**
 * Find out if a browser window contains a frameset
 *
 * \param bw	browser window to look at
 * \return true iff browser window contains a frameset
 */
bool browser_window_is_frameset(struct browser_window *bw);

/**
 * Get the browser window's scrollbar details.
 *
 * Vertical and horizontal scrollbars may be {YES|NO|AUTO}, although
 * it is entirely up to the front end whether this is implemented.
 * e.g. if the gui toolkit style-guide says all windows must have
 * scrollbars then this API can be ignored.
 *
 * \param bw  browser window to look at
 * \param h   Updated to indicate horizontal scrollbar type
 * \param v   Updated to indicate vertical scrollbar type
 * \return NSERROR_OK, or appropriate error otherwise
 */
nserror browser_window_get_scrollbar_type(struct browser_window *bw,
		browser_scrolling *h, browser_scrolling *v);

/**
 * Set the DPI of the browser.
 *
 * \param dpi The DPI to set.
 */
nserror browser_set_dpi(int dpi);

/**
 * Get the browser DPI.
 *
 * \return The DPI in use.
 */
int browser_get_dpi(void);

/**
 * Dump debug info concerning the browser window's contents to file
 *
 * \param bw The browser window.
 * \param f The file to dump to.
 * \param op The debug operation type to dump.
 * \return NSERROR_OK on success or error code on faliure.
 */
nserror browser_window_debug_dump(struct browser_window *bw, FILE *f, enum content_debug op);

/**
 * set debug options on a window
 * \param bw The browser window.
 * \param op The debug operation type.
 * \return NSERROR_OK on success or error code on faliure.
 */
nserror browser_window_debug(struct browser_window *bw, enum content_debug op);

#endif
