/*
 * Copyright 2003 James Bursa <bursa@users.sourceforge.net>
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
 * Fetching of data from a URL (interface).
 */

#ifndef _NETSURF_DESKTOP_FETCH_H_
#define _NETSURF_DESKTOP_FETCH_H_

#include <stdbool.h>

#include "utils/config.h"
#include "utils/nsurl.h"

struct content;
struct fetch;
struct ssl_cert_info;

typedef enum {
	FETCH_PROGRESS,
	FETCH_HEADER,
	FETCH_DATA,
	FETCH_FINISHED,
	FETCH_ERROR,
	FETCH_REDIRECT,
	FETCH_NOTMODIFIED,
	FETCH_AUTH,
	FETCH_CERT_ERR,
	FETCH_SSL_ERR
} fetch_msg_type;

typedef struct fetch_msg {
	fetch_msg_type type;

	union {
		const char *progress;

		struct {
			const uint8_t *buf;
			size_t len;
		} header_or_data;

		const char *error;

		/** \todo Use nsurl */
		const char *redirect;

		struct {
			const char *realm;
		} auth;

		struct {
			const struct ssl_cert_info *certs;
			size_t num_certs;
		} cert_err;
	} data;
} fetch_msg;

/** Fetch POST multipart data */
struct fetch_multipart_data {
	bool file;			/**< Item is a file */
	char *name;			/**< Name of item */
	char *value;			/**< Item value */
	char *rawfile;			/**< Raw filename if file is true */

	struct fetch_multipart_data *next;	/**< Next in linked list */
};

struct ssl_cert_info {
	long version;		/**< Certificate version */
	char not_before[32];	/**< Valid from date */
	char not_after[32];	/**< Valid to date */
	int sig_type;		/**< Signature type */
	long serial;		/**< Serial number */
	char issuer[256];	/**< Issuer details */
	char subject[256];	/**< Subject details */
	int cert_type;		/**< Certificate type */
};

typedef void (*fetch_callback)(const fetch_msg *msg, void *p);

/**
 * Start fetching data for the given URL.
 *
 * The function returns immediately. The fetch may be queued for later
 * processing.
 *
 * A pointer to an opaque struct fetch is returned, which can be passed to
 * fetch_abort() to abort the fetch at any time. Returns NULL if memory is
 * exhausted (or some other fatal error occurred).
 *
 * The caller must supply a callback function which is called when anything
 * interesting happens. The callback function is first called with msg
 * FETCH_HEADER, with the header in data, then one or more times
 * with FETCH_DATA with some data for the url, and finally with
 * FETCH_FINISHED. Alternatively, FETCH_ERROR indicates an error occurred:
 * data contains an error message. FETCH_REDIRECT may replace the FETCH_HEADER,
 * FETCH_DATA, FETCH_FINISHED sequence if the server sends a replacement URL.
 *
 */
struct fetch *fetch_start(nsurl *url, nsurl *referer,
			  fetch_callback callback,
			  void *p, bool only_2xx, const char *post_urlenc,
			  const struct fetch_multipart_data *post_multipart,
			  bool verifiable, bool downgrade_tls,
			  const char *headers[]);

/**
 * Abort a fetch.
 */
void fetch_abort(struct fetch *f);


/**
 * Check if a URL's scheme can be fetched.
 *
 * \param  url  URL to check
 * \return  true if the scheme is supported
 */
bool fetch_can_fetch(const nsurl *url);

/**
 * Change the callback function for a fetch.
 */
void fetch_change_callback(struct fetch *fetch, fetch_callback callback, void *p);

/**
 * Get the HTTP response code.
 */
long fetch_http_code(struct fetch *fetch);

/**
 * Determine if a fetch was verifiable
 *
 * \param fetch  Fetch to consider
 * \return Verifiable status of fetch
 */
bool fetch_get_verifiable(struct fetch *fetch);

/**
 * Free a linked list of fetch_multipart_data.
 *
 * \param list Pointer to head of list to free
 */
void fetch_multipart_data_destroy(struct fetch_multipart_data *list);

/**
 * Clone a linked list of fetch_multipart_data.
 *
 * \param list  List to clone
 * \return Pointer to head of cloned list, or NULL on failure
 */
struct fetch_multipart_data *fetch_multipart_data_clone(const struct fetch_multipart_data *list);

/**
 * send message to fetch
 */
void fetch_send_callback(const fetch_msg *msg, struct fetch *fetch);

/**
 * remove a queued fetch
 */
void fetch_remove_from_queues(struct fetch *fetch);

/**
 * Free a fetch structure and associated resources.
 */
void fetch_free(struct fetch *f);

/**
 * set the http code of a fetch
 */
void fetch_set_http_code(struct fetch *fetch, long http_code);

/**
 * get the referer from the fetch
 */
const char *fetch_get_referer_to_send(struct fetch *fetch);

/**
 * set cookie data on a fetch
 */
void fetch_set_cookie(struct fetch *fetch, const char *data);


#endif
