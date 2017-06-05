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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <glib.h>
#ifdef ENABLE_NLS
#include <glib/gi18n.h>
#else
#define _(a) (a)
#define N_(a) (a)
#endif
#include <string.h>

#include "string_match.h"
#include "utils.h"

int string_match_precompile (gchar *rexp, regex_t *preg, int cflags)
{
	int problem = 0;

	cm_return_val_if_fail(rexp, -1);
	cm_return_val_if_fail(*rexp, -1);

	problem = regcomp(preg, rexp, cflags);  
	
	return problem;
}


gchar *string_remove_match(gchar *buf, gint buflen, gchar * txt, regex_t *preg)
{
	regmatch_t match;
	int notfound;
	gint i, j ,k;
#ifdef G_OS_WIN32
	return txt;
#else
	if (!preg)
		return txt;
	if (*txt != 0x00) {
		i = 0;
		j = 0;
		do {
			notfound = regexec(preg, txt+j, 1, &match, (j ? REG_NOTBOL : 0));
			if (notfound) {
				while (txt[j] && i < buflen -1)
					buf[i++] = txt[j++];
			} else {
				if ( match.rm_so == match.rm_eo)
					buf[i++] = txt[j++];
				else {
					k = j;
					while (txt[j] &&  j != k + match.rm_so)	
						buf[i++] = txt[j++];
					if (txt[j])
						j = k + match.rm_eo;
				}
			}
		} while (txt[j] && i < buflen - 1);
		buf[i] = 0x00;
		if (buf[0] == 0x00) {
			strncpy(buf, _("(Subject cleared by RegExp)"),
					buflen - 1);
			buf[buflen - 1] = 0x00;
		}
		return buf;		
	}
	return txt;
#endif
}

