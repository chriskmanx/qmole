/* $Id: debug.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2003-2009 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __DEBUG_H__
#define __DEBUG_H__

#include "build.h"
#include <errno.h>

//debug-level names
enum
{
	WARN = 1,
    INFO,
	ERROR,
    NOTICE,
	DEBUG
};

#define IS_A(wid) g_type_name(G_OBJECT_TYPE(G_OBJECT(wid)))

#ifdef DEBUG_MESSAGES_ALWAYS
# ifndef DEBUG_MESSAGES
#  define DEBUG_MESSAGES
# endif
#else
//the Makefile sets E2_DEBUG_LEVEL to 2, or 5 if debugging is requested
#if E2_DEBUG_LEVEL > 2
# define DEBUG_MESSAGES
#else
# undef DEBUG_MESSAGES
#endif
#endif

#ifdef DEBUG_MESSAGES
void printd_raw (gint level, gchar *file, gint line, const gchar *format, ...);
#define printd(level, str, args...) printd_raw (level, __FILE__, __LINE__, str, ## args)
#define ERR_STR strerror (errno)
#else
//don't include any code when user is not really interested
#define printd(level, str, args...)
#endif

#endif //ndef __DEBUG_H__
