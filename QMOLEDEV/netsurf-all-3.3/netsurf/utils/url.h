/*
 * Copyright 2005 James Bursa <bursa@users.sourceforge.net>
 * Copyright 2005 John M Bell <jmb202@ecs.soton.ac.uk>
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
 * \file utils/url.h
 * \brief Interface to URL parsing and joining operations.
 */

#ifndef _NETSURF_UTILS_URL_H_
#define _NETSURF_UTILS_URL_H_

#include "utils/errors.h"


/**
 * Escape a string suitable for inclusion in an URL.
 *
 * \param  unescaped      the unescaped string
 * \param  toskip         number of bytes to skip in unescaped string
 * \param  sptoplus       true iff spaces should be converted to +
 * \param  escexceptions  NULL or a string of characters excluded to be escaped
 * \param  result         pointer to pointer to buffer to hold escaped string
 * \return  NSERROR_OK on success
 */
nserror url_escape(const char *unescaped, size_t toskip, bool sptoplus,
		const char *escexceptions, char **result);


/**
 * Convert an escaped string to plain.
 *
 * \param[in] str String to unescape.
 * \param[out] result unescaped string owned by caller must be freed with free()
 * \return  NSERROR_OK on success
 */
nserror url_unescape(const char *str, char **result);

#endif
