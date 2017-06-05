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
 * \file utils/time.h
 * \brief Interface to time operations.
 */

#ifndef _NETSURF_UTILS_TIME_H_
#define _NETSURF_UTILS_TIME_H_

#include <time.h>

/**
 * Write the time in seconds since epoch to a buffer.
 *
 * This is provided as strftime is not generally portable.
 *
 * @param str The destination buffer.
 * @param size The length of the destination buffer.
 * @param timep The pointer to the time to write.
 * @return The length of the string written.
 */
int nsc_sntimet(char *str, size_t size, time_t *timep);

/**
 * Parse time in seconds since epoc.
 *
 * This is provided as strptime is not generally portable.
 *
 * @param str The source buffer.
 * @param size The length of the source buffer.
 * @param timep Pointer to result.
 * @return NSERROR_OK on success or error code on faliure.
 */
nserror nsc_snptimet(char *str, size_t size, time_t *timep);

#endif
