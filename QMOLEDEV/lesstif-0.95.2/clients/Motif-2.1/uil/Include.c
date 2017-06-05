/**
 *
 * $Id: Include.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/


#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "Include.h"
#include "misc.h"

#include "main.h"


#define MAX_PATHS 20

#ifdef PATH_MAX
# define MAX_PATH_LEN PATH_MAX
#else
# define MAX_PATH_LEN	2048 /* this is OS dependent, but this should catch most */
#endif

static char *Paths[MAX_PATHS];
static int nDirs = 0;

void 
IncludeAddDirectory(char *d)
{
    if (nDirs >= MAX_PATHS)
    {
	__MrmExit(LOC, "too many '-I' on command line\n");
    }

    Paths[nDirs++] = __MrmStore(d);
}


FILE *
IncludeOpenFile(char *f)
{
    int i;
    FILE *r = NULL;
    char b[MAX_PATH_LEN];

    if (f)
    {
#ifdef __EMX__
	if (('/' == f[0]) || (isalpha(f[0]) && '/' == f[1]))
#else
	if ('/' == f[0])
#endif
	{
	    return fopen(f, "r");
	}

	for (strcpy(b, f), i = 0;
	     (i <= nDirs) && (NULL == (r = fopen(b, "r")));
	     sprintf(b, "%s/%s", Paths[i++], f));
    }

    return r;
}
