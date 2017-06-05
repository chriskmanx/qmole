/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/


/* System dependent functions for GNU cpio.

   Copyright (C) 2007, 2010 Free Software Foundation, Inc.

   GNU cpio is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GNU cpio is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU cpiio.  If not, see <http://www.gnu.org/licenses/>. */


#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <unistd.h>
#include <errno.h>




#ifndef HAVE_GETPWUID
# warning "Providing stub placeholder for getpwuid function"
struct passwd *
getpwuid ( uid_t uid __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return NULL;
}
#endif

#ifndef HAVE_GETPWNAM
# warning "Providing stub placeholder for getpwnam function"
struct passwd *
getpwnam ( const char *name __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return NULL;
}
#endif

#ifndef HAVE_GETGRGID
# warning "Providing stub placeholder for getgrgid function"
struct group *
getgrgid ( gid_t gid __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return NULL;
}
#endif

#ifndef HAVE_GETGRNAM
# warning "Providing stub placeholder for getgrnam function"
struct group *
getgrnam ( const char *name __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return NULL;
}
#endif

#ifndef HAVE_PIPE
# warning "Providing stub placeholder for pipe function"
int
pipe ( int filedes[2] __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_FORK
# warning "Providing stub placeholder for fork function"
int
fork (void)
{
  errno = ENOSYS;
  return -1;
}
#endif



#ifndef HAVE_GETUID
# warning "Providing stub placeholder for getuid function"
int
getuid (void)
{
  errno = ENOSYS;
  return -1;
}
#endif



#ifndef HAVE_GETEUID
# warning "Providing stub placeholder for geteuid function"
int
geteuid (void)
{
  errno = ENOSYS;
  return -1;
}
#endif



#ifndef HAVE_GETGID
# warning "Providing stub placeholder for getgid function"
int
getgid (void)
{
  errno = ENOSYS;
  return -1;
}
#endif


#ifndef HAVE_SETUID
# warning "Providing stub placeholder for setuid function"
int
setuid ( int newuid __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_SETGID
# warning "Providing stub placeholder for setgid function"
int
setgid ( int newgid __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_MKNOD
# warning "Providing stub placeholder for mknod function"
int
mknod ( const char *filename __attribute__ ((unused)) ,  int mode __attribute__ ((unused)) ,  int dev __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_SYMLINK
# warning "Providing stub placeholder for symlink function"
int
symlink ( const char *oldname __attribute__ ((unused)) ,  const char *newname __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_LINK
# warning "Providing stub placeholder for link function"
int
link ( const char *oldname __attribute__ ((unused)) ,  const char *newname __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_CHOWN
# warning "Providing stub placeholder for chown function"
int
chown ( const char *filename __attribute__ ((unused)) ,  int owner __attribute__ ((unused)) ,  int group __attribute__ ((unused)) )
{
  errno = ENOSYS;
  return -1;
}
#endif

