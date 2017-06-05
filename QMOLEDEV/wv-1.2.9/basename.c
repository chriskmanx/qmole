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
#include <string.h>
#include "wv.h"

/* basename -- strip directory and suffix from filenames
   Copyright (C) 1990-1997, 1999 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


/* Remove SUFFIX from the end of NAME if it is there, unless NAME
   consists entirely of SUFFIX. */

void
remove_suffix (char *name, const char *suffix)
{
    char *np;
    const char *sp;

/*lvm007@aha.ru fix for filename case insencetivity in Win*/
#ifdef _WIN32
	_strlwr(name);
#endif
    np = name + strlen (name);
    sp = suffix + strlen (suffix);

    while (np > name && sp > suffix)
	if (*--np != *--sp)
	    return;
    if (np > name)
	*np = '\0';
}

#ifndef FILESYSTEM_PREFIX_LEN
# define FILESYSTEM_PREFIX_LEN(Filename) 0
#endif

/*lvm007@aha.ru fix for filename back splashes*/
#ifndef ISSLASH
#ifndef _WIN32
# define ISSLASH(C) ((C) == '/')
#else
# define ISSLASH(C) ((C) == '/'||(C) == '\\')
#endif
#endif

/* In general, we can't use the builtin `basename' function if available,
   since it has different meanings in different environments.
   In some environments the builtin `basename' modifies its argument.
   If NAME is all slashes, be sure to return `/'.  */

char *
base_name (char const *name)
{
    char const *base = name += FILESYSTEM_PREFIX_LEN (name);
    int all_slashes = 1;
    char const *p;

    for (p = name; *p; p++)
      {
	  if (ISSLASH (*p))
	      base = p + 1;
	  else
	      all_slashes = 0;
      }

    /* If NAME is all slashes, arrange to return `/'.  */
    if (*base == '\0' && ISSLASH (*name) && all_slashes)
	--base;

    return (char *) base;
}
