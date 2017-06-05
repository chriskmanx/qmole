/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include "wv.h"
#include "utf.h"


char *
wvWideStrToMB (U16 * str)
{
    int len, len2 = 0, j;
    char *utf8 = NULL;
    char target[5];		/*
				   no wide char becomes longer than about 3 or 4 chars, 
				   but you never know :-)
				 */
    if (str == NULL)
	return (NULL);

    while (*str != 0)
      {
	  len = our_wctomb (target, *str);
	  utf8 = (char *) realloc (utf8, len2 + len + 1);
	  for (j = 0; j < len; j++)
	      utf8[len2 + j] = target[j];
	  len2 += len;
	  str++;
      }
    if (utf8 != NULL)
	utf8[len2] = '\0';
    return (utf8);
}


char *
wvWideCharToMB (U16 char16)
{
    int len, len2 = 0, j;
    char *utf8 = NULL;
    char target[5];		/*
				   no wide char becomes longer than about 3 or 4 chars, 
				   but you never know :-)
				 */
    len = our_wctomb (target, char16);
    utf8 = (char *) realloc (utf8, len2 + len + 1);
    for (j = 0; j < len; j++)
	utf8[len2 + j] = target[j];
    len2 += len;

    if (utf8 != NULL)
	utf8[len2] = '\0';
    return (utf8);
}
