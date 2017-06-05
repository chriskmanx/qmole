/**
 *
 * $Id: gethostname.c,v 1.1 2004/08/28 19:25:45 dannybackx Exp $
 *
 * Based on gethostname.c from GNU sh-utils 1.12.
 * 
 * gethostname emulation for SysV and POSIX.1.
 *
 * Copyright (C) 1992, 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

 /* David MacKenzie <djm@gnu.ai.mit.edu> */


#include <LTconfig.h>

#ifndef HAVE_GETHOSTNAME

#include <stddef.h>
#include <string.h>

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#else
#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif
#endif

/* Put up to LEN chars of the host name into NAME.
   Null terminate it if the name is shorter than LEN.
   Return 0 if ok, -1 if error.  */

extern int
gethostname (char *name, size_t len)
{
#ifdef HAVE_UNAME
  struct utsname uts;

  if (uname (&uts) == -1)
    return -1;
  if (len > sizeof (uts.nodename))
    {
      /* More space than we need is available.  */
      name[sizeof (uts.nodename)] = '\0';
      len = sizeof (uts.nodename);
    }
  strncpy (name, uts.nodename, len);
#else
#ifdef HAVE_SYS_SYSTEMINFO_H
	sysinfo(SI_HOSTNAME, name, len);
#else
#error You lose.
#endif /* HAVE_SYS_SYSTEMINFO_H */
#endif /* HAVE_UNAME */
  return 0;
}

#endif /* !HAVE_GETHOSTNAME */
