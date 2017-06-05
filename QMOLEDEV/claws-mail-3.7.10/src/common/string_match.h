/*
 * Sylpheed -- regexp pattern matching utilities
 * Copyright (C) 2001 Thomas Link, Hiroyuki Yamamoto
 *                    Modified by Melvin Hadasht.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */


#ifndef STRING_MATCH_H__
#define STRING_MATCH_H__

#include <sys/types.h>
#include <regex.h>
#include <glib.h>

/* Precompile the preg buffer for the rexp regexp string. See regex man for the
 * meaning of cflags.  
 */

int string_match_precompile (gchar *rexp, regex_t *preg, int cflags);

/* remove from txt the substrings matching the regexp in the precompiled preg buffer.  
 * The result is stored in the preallocated buf buffer which maximal length
 * is buflen.
 */
gchar *string_remove_match(gchar *buf, gint buflen, gchar * txt, regex_t *preg);

#endif /* STRING_MATCH_H__ */
