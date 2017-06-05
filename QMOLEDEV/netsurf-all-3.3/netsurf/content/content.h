/*
 * Copyright 2005-2007 James Bursa <bursa@users.sourceforge.net>
 * Copyright 2003 Philip Pemberton <philpem@users.sourceforge.net>
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
 * Content handling (interface).
 *
 * The content functions manipulate struct contents, which correspond to URLs.
 */

#ifndef _NETSURF_CONTENT_CONTENT_H_
#define _NETSURF_CONTENT_CONTENT_H_

#include <stdbool.h>
#include <stdio.h>

#include <libwapcaplet/libwapcaplet.h>

#include "utils/config.h"
#include "utils/errors.h"
#include "utils/http.h"
#include "content/content_factory.h"
#include "content/content_type.h"
#include "desktop/search.h"
#include "desktop/mouse.h"
#include "desktop/plot_style.h"

struct browser_window;
struct browser_window_features;
struct content;
struct llcache_handle;
struct hlcache_handle;
struct object_params;
struct rect;
struct redraw_context;

/** Status of a content */
typedef enum {
	CONTENT_STATUS_LOADING,	/**< Content is being fetched or
				  converted and is not safe to display. */
	CONTENT_STATUS_READY,	/**< Some parts of content still being
				  loaded, but can be displayed. */
	CONTENT_STATUS_DONE,	/**< All finished. */
	CONTENT_STATUS_ERROR	/**< Error occurred, content will be
				  destroyed imminently. */
} content_status;

/** Used in callbacks to indicate what has occurred. */
typedef enum {
	CONTENT_MSG_LOADING,   /**< fetching or converting */
	CONTENT_MSG_READY,     /**< may be displayed */
	CONTENT_MSG_DONE,      /**< finished */
	CONTENT_MSG_ERROR,     /**< error occurred */
	CONTENT_MSG_ERRORCODE, /**< error occurred return nserror */
	CONTENT_MSG_REDIRECT,  /**< fetch url redirect occured */
	CONTENT_MSG_STATUS,    /**< new status string */
	CONTENT_MSG_REFORMAT,  /**< content_reformat done */
	CONTENT_MSG_REDRAW,    /**< needs redraw (eg. new animation frame) */
	CONTENT_MSG_REFRESH,   /**< wants refresh */
	CONTENT_MSG_DOWNLOAD,  /**< download, not for display */
	CONTENT_MSG_LINK,      /**< RFC5988 link */
	CONTENT_MSG_GETCTX,    /**< Javascript context */
	CONTENT_MSG_SCROLL,    /**< Request to scroll content */
	CONTENT_MSG_DRAGSAVE,  /**< Allow drag saving of content */
	CONTENT_MSG_SAVELINK,  /**< Allow URL to be saved */
	CONTENT_MSG_POINTER,   /**< Wants a specific mouse pointer set */
	CONTENT_MSG_SELECTION, /**< A selection made or cleared */
	CONTENT_MSG_CARET,     /**< Caret movement / hiding */
	CONTENT_MSG_DRAG,      /**< A drag started or ended */
	CONTENT_MSG_SELECTMENU,/**< Create a select menu */
	CONTENT_MSG_GADGETCLICK/**< A gadget has been clicked on (mainly for file) */
} content_msg;

/** Debugging dump operations */
enum content_debug {
	CONTENT_DEBUG_RENDER, /** Debug the contents rendering. */
	CONTENT_DEBUG_DOM,    /** Debug the contents Document Object. */
	CONTENT_DEBUG_REDRAW  /** Debug redraw operations. */
};

/** Content encoding informstion types */
enum content_encoding_type {
	CONTENT_ENCODING_NORMAL, /** The content encoding */
	CONTENT_ENCODING_SOURCE  /** The content encoding source */
};

/** RFC5988 metadata link */
struct content_rfc5988_link {
	struct content_rfc5988_link *next; /**< next rfc5988_link in list */

	lwc_string *rel; /**< the link relationship - must be present */
	struct nsurl *href; /**< the link href - must be present */
	lwc_string *hreflang;
	lwc_string *type;
	lwc_string *media;
	lwc_string *sizes;
};

/** Extra data for some content_msg messages. */
union content_msg_data {
	/** CONTENT_MSG_ERROR - Error message */
	const char *error;
        /** CONTENT_MSG_ERRORCODE - Error code */
	nserror errorcode;
        /** CONTENT_MSG_REDIRECT - Redirect info */
	struct {
		struct nsurl *from;	/**< Redirect origin */
		struct nsurl *to;	/**< Redirect target */
	} redirect;		/**< Fetch URL redirect occured */
	/** CONTENT_MSG_REDRAW - Area of content which needs redrawing */
	struct {
		int x, y, width, height;
		/** Redraw the area fully. If false, object must be set,
		 * and only the object will be redrawn. */
		bool full_redraw;
		/** Object to redraw if full_redraw is false. */
		struct content *object;
		/** Coordinates to plot object at. */
		int object_x, object_y;
		/** Dimensions to plot object with. */
		int object_width, object_height;
	} redraw;
	/** CONTENT_MSG_REFRESH - Minimum delay  */
	int delay;
	/** CONTENT_MSG_REFORMAT - Reformat should not cause a redraw */
	bool background;
	/** CONTENT_MSG_STATUS - Status message update.  If NULL, the content's
	 * internal status text has been updated, and listener should use
	 * content_get_status_message() */
	const char *explicit_status_text;
	/** CONTENT_MSG_DOWNLOAD - Low-level cache handle */
	struct llcache_handle *download;
	/** CONTENT_MSG_RFC5988_LINK - rfc5988 link data   */
	struct content_rfc5988_link *rfc5988_link;
	/** CONTENT_MSG_GETCTX - Javascript context */
	struct jscontext **jscontext;
	/** CONTENT_MSG_SCROLL - Part of content to scroll to show */
	struct {
		/** if true, scroll to show area given by (x0, y0) and (x1,y1).
		 * if false, scroll point (x0, y0) to top left of viewport */
		bool area;
		int x0, y0;
		int x1, y1;
	} scroll;
	/** CONTENT_MSG_DRAGSAVE - Drag save a content */
	struct {
		enum {
			CONTENT_SAVE_ORIG,
			CONTENT_SAVE_NATIVE,
			CONTENT_SAVE_COMPLETE,
			CONTENT_SAVE_SOURCE
		} type;
		 /** if NULL, save the content generating the message */
		struct hlcache_handle *content;
	} dragsave;
	/** CONTENT_MSG_SAVELINK - Save a URL */
	struct {
		struct nsurl *url;
		const char *title;
	} savelink;
	/** CONTENT_MSG_POINTER - Mouse pointer to set */
	browser_pointer_shape pointer;
	/** CONTENT_MSG_SELECTION - Selection made or cleared */
	struct {
		bool selection; /**< false for selection cleared */
		bool read_only;
	} selection;
	/** CONTENT_MSG_CARET - set caret position or, hide caret */
	struct {
		enum {
			CONTENT_CARET_SET_POS,
			CONTENT_CARET_HIDE,
			CONTENT_CARET_REMOVE
		} type;
		struct {
			int x;				/**< Carret x-coord */
			int y;				/**< Carret y-coord */
			int height;			/**< Carret height */
			const struct rect *clip;	/**< Carret clip rect */
		} pos;			/**< With CONTENT_CARET_SET_POS */
	} caret;
	/** CONTENT_MSG_DRAG - Drag start or end */
	struct {
		enum {
			CONTENT_DRAG_NONE,
			CONTENT_DRAG_SCROLL,
			CONTENT_DRAG_SELECTION
		} type;
		const struct rect *rect;
	} drag;
	/** CONTENT_MSG_SELECTMENU - Create select menu at pointer */
	struct {
		struct form_control *gadget;
	} select_menu;
	/** CONTENT_MSG_GADGETCLICK - User clicked on a form gadget */
	struct {
		struct form_control *gadget;
	} gadget_click;
};

/** parameters to content redraw */
struct content_redraw_data {
	int x; /**< coordinate for top-left of redraw */
	int y; /**< coordinate for top-left of redraw */

	/** dimensions to render content at
	 *  (for scaling contents with intrinsic dimensions) */
	int width; /**< horizontal dimension */
	int height; /**< vertical dimension */

	/** The background colour */
	colour background_colour;

	/** Scale for redraw
	 *  (for scaling contents without intrinsic dimensions) */
	float scale; /**< Scale factor for redraw */

	bool repeat_x; /**< whether content is tiled in x direction */
	bool repeat_y; /**< whether content is tiled in y direction */
};

/* The following are for hlcache */
void content_destroy(struct content *c);

bool content_add_user(struct content *h,
		void (*callback)(struct content *c, content_msg msg,
			union content_msg_data data, void *pw),
		void *pw);
void content_remove_user(struct content *c,
		void (*callback)(struct content *c, content_msg msg,
			union content_msg_data data, void *pw),
		void *pw);

uint32_t content_count_users(struct content *c);
bool content_matches_quirks(struct content *c, bool quirks);
bool content_is_shareable(struct content *c);

const struct llcache_handle *content_get_llcache_handle(struct content *c);
struct nsurl *content_get_url(struct content *c);

struct content *content_clone(struct content *c);

nserror content_abort(struct content *c);

/* Client functions */
bool content_can_reformat(struct hlcache_handle *h);
void content_reformat(struct hlcache_handle *h, bool background,
		int width, int height);
void content_request_redraw(struct hlcache_handle *h,
		int x, int y, int width, int height);
void content_mouse_track(struct hlcache_handle *h, struct browser_window *bw,
		browser_mouse_state mouse, int x, int y);
void content_mouse_action(struct hlcache_handle *h, struct browser_window *bw,
		browser_mouse_state mouse, int x, int y);
bool content_keypress(struct hlcache_handle *h, uint32_t key);
bool content_redraw(struct hlcache_handle *h, struct content_redraw_data *data,
		const struct rect *clip, const struct redraw_context *ctx);
void content_open(struct hlcache_handle *h, struct browser_window *bw,
		struct content *page, struct object_params *params);
void content_close(struct hlcache_handle *h);
void content_clear_selection(struct hlcache_handle *h);
char * content_get_selection(struct hlcache_handle *h);

/**
 * Get positional contextural information for a content.
 *
 * \param[in] h Handle to content to examine.
 * \param[in] x The x coordinate to examine.
 * \param[in] y The y coordinate to examine.
 * \param[out] data The context structure to fill in.
 */
nserror content_get_contextual_content(struct hlcache_handle *h,
		int x, int y, struct browser_window_features *data);

bool content_scroll_at_point(struct hlcache_handle *h,
		int x, int y, int scrx, int scry);
bool content_drop_file_at_point(struct hlcache_handle *h,
		int x, int y, char *file);

void content_search(struct hlcache_handle *h, void *context,
		search_flags_t flags, const char *string);
void content_search_clear(struct hlcache_handle *h);

/**
 * Dump debug information to file.
 *
 * \param h content handle to debug.
 * \param f File to write output to.
 * \param op Debug operation type.
 */
nserror content_debug_dump(struct hlcache_handle *h, FILE *f, enum content_debug op);

/**
 * Control debug con a content.
 *
 * \param h content handle to debug.
 * \param op Debug operation type.
 */
nserror content_debug(struct hlcache_handle *h, enum content_debug op);

/**
 * find link in content that matches the rel string.
 *
 * \param h handle to the content to retrieve tyoe of.
 * \param rel The string to match.
 * \return A matching rfc5988 link or NULL if none is found.
 *
 */
struct content_rfc5988_link *content_find_rfc5988_link(struct hlcache_handle *h,
		lwc_string *rel);

/* Member accessors */

/**
 * Retrieve computed type of content
 *
 * \param h handle to the content to retrieve tyoe of.
 * \return Computed content type
 */
content_type content_get_type(struct hlcache_handle *h);

/**
 * Retrieve mime-type of content
 *
 * \param h handle to the content to retrieve mime type from
 * \return Pointer to referenced mime type, or NULL if not found.
 */
lwc_string *content_get_mime_type(struct hlcache_handle *h);

/**
 * Retrieve title associated with content
 *
 * \param h handle to the content to retrieve title from
 * \return Pointer to title, or NULL if not found.
 */
const char *content_get_title(struct hlcache_handle *h);

/**
 * Retrieve status of content
 *
 * \param h handle to the content to retrieve status from
 * \return Content status
 */
content_status content_get_status(struct hlcache_handle *h);

/**
 * Retrieve status of content
 *
 * \param c Content to retrieve status from.
 * \return Content status
 */
content_status content__get_status(struct content *c);

/**
 * Retrieve status message associated with content
 *
 * \param h handle to the content to retrieve status message from
 * \return Pointer to status message, or NULL if not found.
 */
const char *content_get_status_message(struct hlcache_handle *h);

/**
 * Retrieve width of content
 *
 * \param h handle to the content to get width of.
 * \return Content width
 */
int content_get_width(struct hlcache_handle *h);

/**
 * Retrieve height of content
 *
 * \param h handle to the content to get height of.
 * \return Content height
 */
int content_get_height(struct hlcache_handle *h);

/**
 * Retrieve available width of content
 *
 * \param h handle to the content to get available width of.
 * \return Available width of content.
 */
int content_get_available_width(struct hlcache_handle *h);

/**
 * Retrieve source of content
 *
 * \param h Content handle to retrieve source of
 * \param size Pointer to location to receive byte size of source
 * \return Pointer to source data
 */
const char *content_get_source_data(struct hlcache_handle *h,
		unsigned long *size);

/**
 * Invalidate content reuse data.
 *
 * causes subsequent requests for content URL to query server to
 * determine if content can be reused. This is required behaviour for
 * forced reloads etc.
 *
 * \param h Content handle to invalidate.
 */
void content_invalidate_reuse_data(struct hlcache_handle *h);

/**
 * Retrieve the refresh URL for a content
 *
 * \param h Content to retrieve refresh URL from
 * \return Pointer to URL, or NULL if none
 */
struct nsurl *content_get_refresh_url(struct hlcache_handle *h);

/**
 * Retrieve the bitmap contained in an image content
 *
 * \param h handle to the content.
 * \return Pointer to bitmap, or NULL if none.
 */
struct bitmap *content_get_bitmap(struct hlcache_handle *h);

/**
 * Determine if a content is opaque from handle
 *
 * \param h high level cache handle to retrieve opacity from.
 * \return false if the content is not opaque or information is not
 *         known else true.
 */
bool content_get_opaque(struct hlcache_handle *h);

/**
 * Retrieve quirkiness of a content
 *
 * \param h Content to examine
 * \return True if content is quirky, false otherwise
 */
bool content_get_quirks(struct hlcache_handle *h);

/**
 * Retrieve the encoding of a content
 *
 * \param h handle to the content.
 * \param op encoding operation.
 * \return Pointer to content info or NULL if none.
 */
const char *content_get_encoding(struct hlcache_handle *h, enum content_encoding_type op);

/**
 * Return whether a content is currently locked
 *
 * \param h handle to the content.
 * \return true iff locked, else false
 */
bool content_is_locked(struct hlcache_handle *h);

#endif
