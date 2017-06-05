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
 * Interface to platform-specific fetcher operations.
 */

#ifndef _NETSURF_DESKTOP_GUI_FETCH_H_
#define _NETSURF_DESKTOP_GUI_FETCH_H_

struct nsurl;

/**
 * function table for fetcher operations.
 */
struct gui_fetch_table {
	/* Mandantory entries */

	/**
	 * Determine the MIME type of a local file.
	 *
	 * @note used in file fetcher
	 *
	 * \param unix_path Unix style path to file on disk
	 * \return Pointer to MIME type string (should not be freed) -
	 *	   invalidated on next call to fetch_filetype.
	 */
	const char *(*filetype)(const char *unix_path);

	/* Optional entries */

	/**
	 * Callback to translate resource to full url.
	 *
	 * @note used in resource fetcher
	 *
	 * Transforms a resource: path into a full URL. The returned URL
	 * is used as the target for a redirect. The caller takes ownership of
	 * the returned nsurl including unrefing it when finished with it.
	 *
	 * \param path The path of the resource to locate.
	 * \return A string containing the full URL of the target object or
	 *         NULL if no suitable resource can be found.
	 */
	struct nsurl* (*get_resource_url)(const char *path);

	/**
	 * Find a MIME type for a local file
	 *
	 * @note only used in curl fetcher on RISC OS otherwise its a
	 * strdup of filetype.
	 *
	 * \param ro_path RISC OS style path to file on disk
	 * \return MIME type string (on heap, caller should free), or NULL
	 */
	char *(*mimetype)(const char *ro_path);

};

#endif
