/*
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
 * Browser history tree (interface).
 */

#ifndef _NETSURF_DESKTOP_BROWSER_HISTORY_H_
#define _NETSURF_DESKTOP_BROWSER_HISTORY_H_

#include <stdbool.h>
#include <libwapcaplet/libwapcaplet.h>

#include "utils/errors.h"

struct hlcache_handle;
struct browser_window;
struct history_entry;
struct redraw_context;

/**
 * Create a new history tree for a browser window window.
 *
 * \param bw browser window to create history for.
 *
 * \return NSERROR_OK or appropriate error otherwise
 */
nserror browser_window_history_create(struct browser_window *bw);

/**
 * Clone a bw's history tree for new bw
 *
 * \param  existing	browser window with history to clone.
 * \param  clone	browser window to make cloned history for.
 *
 * \return  NSERROR_OK or appropriate error otherwise
 */
nserror browser_window_history_clone(const struct browser_window *existing,
		struct browser_window *clone);
/**
 * Insert a url into the history tree.
 *
 * \param  bw       browser window with history object
 * \param  content  content to add to history
 * \param  frag_id  fragment identifier, or NULL.
 * \return NSERROR_OK or error code on faliure.
 *
 * The page is added after the current entry and becomes current.
 */
nserror browser_window_history_add(struct browser_window *bw,
		struct hlcache_handle *content, lwc_string *frag_id);

/**
 * Update the thumbnail for the current entry.
 *
 * \param bw The browser window to update the history within.
 * \param content content for current entry
 * \return NSERROR_OK or error code on faliure.
 */
nserror browser_window_history_update(struct browser_window *bw,
		struct hlcache_handle *content);

/**
 * Free a history structure.
 *
 * \param bw The browser window to destroy the history within.
 */
void browser_window_history_destroy(struct browser_window *bw);

/**
 * Go back in the history.
 *
 * \param bw A browser window to navigate the history in.
 * \param new_window whether to open in new window.
 * \return NSERROR_OK or error code on faliure.
 */
nserror browser_window_history_back(struct browser_window *bw, bool new_window);

/**
 * Go forward in the history.
 *
 * \param bw A browser window to navigate the history in.
 * \param new_window whether to open in new window.
 * \return NSERROR_OK or error code on faliure.
 */
nserror browser_window_history_forward(struct browser_window *bw, bool new_window);

/**
 * Check whether it is pssible to go back in the history.
 *
 * \param bw A browser window to check the history navigation in.
 * \return true if the history can go back, false otherwise
 */
bool browser_window_history_back_available(struct browser_window *bw);

/**
 * Check whether it is pssible to go forwards in the history.
 *
 * \param bw A browser window to check the history navigation in.
 * \return true if the history can go forwards, false otherwise
 */
bool browser_window_history_forward_available(struct browser_window *bw);

/**
 * Get the dimensions of a history.
 *
 * \param bw browser window with history object.
 * \param width updated to width
 * \param height updated to height
 */
void browser_window_history_size(struct browser_window *bw,
		int *width, int *height);

/**
 * Redraw all of a history area.
 *
 * \param bw browser window with history object.
 * \param ctx current redraw context
 */
bool browser_window_history_redraw(struct browser_window *bw,
		const struct redraw_context *ctx);

/**
 * Redraw part of a history area.
 *
 * \param bw browser window with history object.
 * \param x0 left X co-ordinate of redraw area
 * \param y0 top Y co-ordinate of redraw area
 * \param x1 right X co-ordinate of redraw area
 * \param y1 lower Y co-ordinate of redraw area
 * \param x start X co-ordinate on plot canvas
 * \param y start Y co-ordinate on plot canvas
 * \param ctx current redraw context
 */
bool browser_window_history_redraw_rectangle(struct browser_window *bw,
		int x0, int y0, int x1, int y1, int x, int y,
		const struct redraw_context *ctx);

/**
 * Handle a mouse click in a history.
 *
 * \param bw browser window containing history
 * \param x click coordinate
 * \param y click coordinate
 * \param new_window  open a new window instead of using bw
 * \return true if action was taken, false if click was not on an entry
 */
bool browser_window_history_click(struct browser_window *bw,
		int x, int y, bool new_window);

/**
 * Determine the URL of the entry at a position.
 *
 * \param bw browser window containing history
 * \param x x coordinate.
 * \param y y coordinate.
 * \return URL, or 0 if no entry at (x, y)
 */
const char *browser_window_history_position_url(struct browser_window *bw,
		int x, int y);

/**
 * Callback function type for history enumeration
 *
 * \param	bw		The browser window with history being enumerated
 * \param	x0, y0, x1, y1	Coordinates of entry in history tree view
 * \param	entry		Current history entry
 * \return	true to continue enumeration, false to cancel enumeration
 */
typedef bool (*browser_window_history_enumerate_cb)(
		const struct browser_window *bw,
		int x0, int y0, int x1, int y1, 
		const struct history_entry *entry, void *user_data);

/**
 * Enumerate all entries in the history.
 * Do not change the history while it is being enumerated.
 *
 * \param	bw		The browser window to enumerate history of
 * \param	cb		callback function
 * \param	user_data	context pointer passed to cb
 */
void browser_window_history_enumerate(const struct browser_window *bw,
		browser_window_history_enumerate_cb cb, void *user_data);

/**
 * Enumerate all entries that will be reached by the 'forward' button
 *
 * \param	bw		The browser window to enumerate history of
 * \param	cb		The callback function
 * \param	user_data	Data passed to the callback
 */
void browser_window_history_enumerate_forward(const struct browser_window *bw, 
		browser_window_history_enumerate_cb cb, void *user_data);

/**
 * Enumerate all entries that will be reached by the 'back' button
 *
 * \param	bw		The browser window to enumerate history of
 * \param	cb		The callback function
 * \param	user_data	Data passed to the callback
 */
void browser_window_history_enumerate_back(const struct browser_window *bw, 
		browser_window_history_enumerate_cb cb, void *user_data);

/**
 * Returns the URL to a history entry
 *
 * \param	entry		the history entry to retrieve the URL from
 * \return	the URL
 */
const char *browser_window_history_entry_get_url(
		const struct history_entry *entry);

/**
 * Returns the URL to a history entry
 *
 * \param	entry		the history entry to retrieve the fragment id from
 * \return	the fragment id
 */
const char *browser_window_history_entry_get_fragment_id(
		const struct history_entry *entry);

/**
 * Returns the title of a history entry
 *
 * \param	entry		the history entry to retrieve the title from
 * \return	the title
 */
const char *browser_window_history_entry_get_title(
		const struct history_entry *entry);

/**
 * Navigate to specified history entry, optionally in new window
 *
 * \param  bw          browser window
 * \param  entry       entry to open
 * \param  new_window  open entry in new window
 * \return NSERROR_OK or error code on faliure.
 */
nserror browser_window_history_go(struct browser_window *bw,
		struct history_entry *entry, bool new_window);

#endif
