/*
 * $Id: LTmisc.h,v 1.1 2004/08/28 19:23:30 dannybackx Exp $
 * 
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2001 LessTif Development Team
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

#ifndef _LTMISC_H
#define _LTMISC_H

#ifndef	HAVE_STRCASECMP
int _Lt_strcasecmp(const char *str1, const char *str2);
#define strcasecmp _Lt_strcasecmp
#endif

#ifndef	HAVE_STRNCASECMP
int _Lt_strncasecmp(const char *str1, const char *str2, size_t count);
#define strncasecmp _Lt_strncasecmp
#endif

#ifndef HAVE_BASENAME
char *_Lt_basename(char *path);
#define basename _Lt_basename
#endif

#ifndef HAVE_DIRNAME
char *_Lt_dirname(char *dirname);
#define dirname _Lt_dirname
#endif

#endif /* _LTMISC_H */
