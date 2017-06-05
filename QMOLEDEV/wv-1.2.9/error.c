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
#include <stdarg.h>
#include "wv.h"


#ifdef DEBUG
#define wvwarn  NULL /* stderr */
#define wvtrace NULL /* stderr */
#define wverror NULL /* stderr */
#else
#define wvwarn  NULL
#define wvtrace NULL
#define wverror NULL
#endif

void
wvInitError (void)
{
  wvError (("EXTREME WARNING: using deprecated API\n"));
}

char *
wvFmtMsg (char *fmt, ...)
{
  static char mybuf[1024];
#if 0
  mybuf[0] = 0;
#endif  

  va_list argp;
  va_start (argp, fmt);
  vsprintf (mybuf, fmt, argp);
  va_end (argp);

  return mybuf;
}

void
wvRealError (char *file, int line, char *msg)
{
    if (wverror == NULL)
	return;
    fprintf (wverror, "Diagnostic: (%s:%d) %s ", file, line, msg);
    fflush (wverror);
}

void
wvWarning (char *fmt, ...)
{
    va_list argp;
    if (wvwarn == NULL)
	return;
    fprintf (wvwarn, "Trace: ");
    va_start (argp, fmt);
    vfprintf (wvwarn, fmt, argp);
    va_end (argp);
    fflush (wvwarn);
}

void
wvRealTrace (char *file, int line, char *msg)
{
    if (wvtrace == NULL)
	return;
    fprintf (wvtrace, "Trace: (%s:%d) %s ", file, line, msg);
    fflush (wvtrace);
}

