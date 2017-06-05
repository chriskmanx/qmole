/* $Id: debug.c 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2003-2010 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"
#ifdef DEBUG_MESSAGES
#include <time.h>
#include <sys/time.h>
#include "e2_cl_option.h"

void printd_raw (gint level, gchar *file, gint line, const gchar *format, ...)
{
	if (level > e2_cl_options.debug_level)
		return;
	gchar *lvl;
	va_list args;
	if (e2_cl_options.verbose)
	{
		static glong sec = 0;
		static glong usec = 0;
		struct timeval t;

		gettimeofday (&t, NULL);
		if (!sec) sec = t.tv_sec;
		if (!usec) usec = t.tv_usec;

		glong dsec = t.tv_sec - sec;
		glong dusec = t.tv_usec - usec;

		sec = t.tv_sec;
		usec = t.tv_usec;

		if ((dsec > 0) && (dsec < 10))
			dusec += dsec * 1000000;

		if ((dusec < 10000) && (dsec == 0))
			printf ("%6ld us %s/%d ", dusec, file, line);
		else if ((dusec < (1000000)) && (dsec == 0))
			printf ("%6ld ms %s/%d ", (dusec / 1000), file, line);
		else
			printf ("%6ld s %s/%d ", (dusec / 1000000) + dsec, file, line);
	}
	switch (level)
	{
		case ERROR:  lvl = "ERROR "; break;
		case WARN:   lvl = "WARN  "; break;
		case INFO:   lvl = "INFO  "; break;
		case NOTICE: lvl = "NOTICE"; break;
		default:     lvl = "DEBUG "; break;
	}
	printf ("[%s] ", lvl);

	va_start (args, format);
	vprintf (format, args);
	va_end (args);
	printf ("\n");
}
#endif //def DEBUG_MESSAGES
