/*
 * Copyright 2006 John M Bell <jmb202@ecs.soton.ac.uk>
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
 * Unified URL information database (interface)
 */

#ifndef _NETSURF_CONTENT_URLDB_H_
#define _NETSURF_CONTENT_URLDB_H_

#include <stdbool.h>
#include <time.h>
#include "utils/nsurl.h"
#include "content/content_type.h"

typedef enum {
	COOKIE_NETSCAPE = 0,
	COOKIE_RFC2109 = 1,
	COOKIE_RFC2965 = 2
} cookie_version;

struct url_data {
	const char *title;		/**< Resource title */
	unsigned int visits;		/**< Visit count */
	time_t last_visit;		/**< Last visit time */
	content_type type;		/**< Type of resource */
};

struct cookie_data {
	const char *name;		/**< Cookie name */
	const char *value;		/**< Cookie value */
	const bool value_was_quoted;	/**< Value was quoted in Set-Cookie: */
	const char *comment;		/**< Cookie comment */
	const bool domain_from_set;	/**< Domain came from Set-Cookie: header */
	const char *domain;		/**< Domain */
	const bool path_from_set;	/**< Path came from Set-Cookie: header */
	const char *path;		/**< Path */
	const time_t expires;		/**< Expiry timestamp, or 1 for session */
	const time_t last_used;		/**< Last used time */
	const bool secure;		/**< Only send for HTTPS requests */
	const bool http_only;		/**< Only expose to HTTP(S) requests */
	cookie_version version;		/**< Specification compliance */
	const bool no_destroy;		/**< Never destroy this cookie,
				 	* unless it's expired */

	const struct cookie_data *prev;	/**< Previous in list */
	const struct cookie_data *next;	/**< Next in list */
};

struct bitmap;

/**
 * Destroy urldb
 */
void urldb_destroy(void);


/* Persistence support */

/**
 * Import an URL database from file, replacing any existing database
 *
 * \param filename Name of file containing data
 */
nserror urldb_load(const char *filename);

/**
 * Export the current database to file
 *
 * \param filename Name of file to export to
 */
nserror urldb_save(const char *filename);

/**
 * Set the cross-session persistence of the entry for an URL
 *
 * \param url Absolute URL to persist
 * \param persist True to persist, false otherwise
 */
void urldb_set_url_persistence(nsurl *url, bool persist);


/* URL insertion */

/**
 * Insert an URL into the database
 *
 * \param url Absolute URL to insert
 * \return true on success, false otherwise
 */
bool urldb_add_url(nsurl *url);


/* URL data modification / lookup */

/**
 * Set an URL's title string, replacing any existing one
 *
 * \param url The URL to look for
 * \param title The title string to use (copied)
 */
void urldb_set_url_title(nsurl *url, const char *title);

/**
 * Set an URL's content type
 *
 * \param url The URL to look for
 * \param type The type to set
 */
void urldb_set_url_content_type(nsurl *url, content_type type);

/**
 * Update an URL's visit data
 *
 * \param url The URL to update
 */
void urldb_update_url_visit_data(nsurl *url);

/**
 * Reset an URL's visit statistics
 *
 * \param url The URL to reset
 */
void urldb_reset_url_visit_data(nsurl *url);

/**
 * Find data for an URL.
 *
 * \param url Absolute URL to look for
 * \return Pointer to result struct, or NULL
 */
const struct url_data *urldb_get_url_data(nsurl *url);

/**
 * Extract an URL from the db
 *
 * \param url URL to extract
 * \return Pointer to database's copy of URL or NULL if not found
 */
nsurl *urldb_get_url(nsurl *url);


/* Authentication modification / lookup */

/**
 * Set authentication data for an URL
 *
 * \param url The URL to consider
 * \param realm The authentication realm
 * \param auth The authentication details (in form username:password)
 */
void urldb_set_auth_details(nsurl *url, const char *realm, const char *auth);

/**
 * Look up authentication details in database
 *
 * \param url Absolute URL to search for
 * \param realm When non-NULL, it is realm which can be used to determine
 *        the protection space when that's not been done before for given URL.
 * \return Pointer to authentication details, or NULL if not found
 */
const char *urldb_get_auth_details(nsurl *url, const char *realm);


/* SSL certificate permissions */

/**
 * Set certificate verification permissions
 *
 * \param url URL to consider
 * \param permit Set to true to allow invalid certificates
 */
void urldb_set_cert_permissions(nsurl *url, bool permit);

/**
 * Retrieve certificate verification permissions from database
 *
 * \param url Absolute URL to search for
 * \return true to permit connections to hosts with invalid certificates,
 * false otherwise.
 */
bool urldb_get_cert_permissions(nsurl *url);


/* Thumbnail handling */

/**
 * Set thumbnail for url, replacing any existing thumbnail
 *
 * \param url Absolute URL to consider
 * \param bitmap Opaque pointer to thumbnail data, or NULL to invalidate
 */
void urldb_set_thumbnail(nsurl *url, struct bitmap *bitmap);

/**
 * Retrieve thumbnail data for given URL
 *
 * \param url Absolute URL to search for
 * \return Pointer to thumbnail data, or NULL if not found.
 */
struct bitmap *urldb_get_thumbnail(nsurl *url);


/* URL completion */

/**
 * Iterate over entries in the database which match the given prefix
 *
 * \param prefix Prefix to match
 * \param callback Callback function
 */
void urldb_iterate_partial(const char *prefix,
		bool (*callback)(nsurl *url, const struct url_data *data));


/* Iteration */

/**
 * Iterate over all entries in database
 *
 * \param callback Function to callback for each entry
 */
void urldb_iterate_entries(bool (*callback)(nsurl *url,
		const struct url_data *data));

/**
 * Iterate over all cookies in database
 *
 * \param callback Function to callback for each entry
 */
void urldb_iterate_cookies(bool (*callback)(const struct cookie_data *cookie));


/* Cookies */

/**
 * Parse Set-Cookie header and insert cookie(s) into database
 *
 * \param header Header to parse, with Set-Cookie: stripped
 * \param url URL being fetched
 * \param referer Referring resource, or 0 for verifiable transaction
 * \return true on success, false otherwise
 */
bool urldb_set_cookie(const char *header, nsurl *url, nsurl *referer);

/**
 * Retrieve cookies for an URL
 *
 * \param url URL being fetched
 * \param include_http_only Whether to include HTTP(S) only cookies.
 * \return Cookies string for libcurl (on heap), or NULL on error/no cookies
 */
char *urldb_get_cookie(nsurl *url, bool include_http_only);

/**
 * Delete a cookie
 *
 * \param domain The cookie's domain
 * \param path The cookie's path
 * \param name The cookie's name
 */
void urldb_delete_cookie(const char *domain, const char *path, const char *name);

/**
 * Load a cookie file into the database
 *
 * \param filename File to load
 */
void urldb_load_cookies(const char *filename);

/**
 * Save persistent cookies to file
 *
 * \param filename Path to save to
 */
void urldb_save_cookies(const char *filename);


/* Debug */

/**
 * Dump URL database to stderr
 */
void urldb_dump(void);


/* test harness only */

/**
 * Add a host to the database, creating any intermediate entries
 *
 * \param host Hostname to add
 * \return Pointer to leaf node, or NULL on memory exhaustion
 */
struct host_part *urldb_add_host(const char *host);

/**
 * Add a path to the database, creating any intermediate entries
 *
 * \param scheme URL scheme associated with path
 * \param port Port number on host associated with path
 * \param host Host tree node to attach to
 * \param path_query Absolute path plus query to add (freed)
 * \param fragment URL fragment, or NULL
 * \param url URL (fragment ignored)
 * \return Pointer to leaf node, or NULL on memory exhaustion
 */
struct path_data *urldb_add_path(lwc_string *scheme, unsigned int port,
		const struct host_part *host, char *path_query,
		lwc_string *fragment, nsurl *url);

#endif
