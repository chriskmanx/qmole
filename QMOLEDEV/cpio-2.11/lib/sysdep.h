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


#ifdef HAVE_PROCESS_H
# include <process.h>
#endif

#ifndef HAVE_PWD_H
/* Borrowed from GNU libc */
/* The passwd structure.  */
struct passwd
{
  char *pw_name;		/* Username.  */
  char *pw_passwd;		/* Password.  */
  int pw_uid;			/* User ID.  */
  int pw_gid;			/* Group ID.  */
  char *pw_gecos;		/* Real name.  */
  char *pw_dir;			/* Home directory.  */
  char *pw_shell;		/* Shell program.  */
};
#endif
#ifndef HAVE_GRP_H
/* Borrowed from GNU libc */
/* The group structure.	 */
struct group
  {
    char *gr_name;		/* Group name.	*/
    char *gr_passwd;		/* Password.	*/
    int gr_gid;			/* Group ID.	*/
    char **gr_mem;		/* Member list.	*/
  };
#endif

#include <signal.h>
#ifndef SIGPIPE
# define SIGPIPE -1
#endif





#ifndef HAVE_GETPWUID
struct passwd * getpwuid (uid_t uid);
#endif


#ifndef HAVE_GETPWNAM
struct passwd * getpwnam (const char *name);
#endif


#ifndef HAVE_GETGRGID
struct group * getgrgid (gid_t gid);
#endif


#ifndef HAVE_GETGRNAM
struct group * getgrnam (const char *name);
#endif


#ifndef HAVE_PIPE
int pipe (int filedes[2]);
#endif


#ifndef HAVE_FORK
int fork (void);
#endif




#ifndef HAVE_GETUID
int getuid (void);
#endif




#ifndef HAVE_GETEUID
int geteuid (void);
#endif




#ifndef HAVE_GETGID
int getgid (void);
#endif



#ifndef HAVE_SETUID
int setuid (int newuid);
#endif


#ifndef HAVE_SETGID
int setgid (int newgid);
#endif


#ifndef HAVE_MKNOD
int mknod (const char *filename,int mode,int dev);
#endif


#ifndef HAVE_SYMLINK
int symlink (const char *oldname,const char *newname);
#endif


#ifndef HAVE_LINK
int link (const char *oldname,const char *newname);
#endif


#ifndef HAVE_CHOWN
int chown (const char *filename,int owner,int group);
#endif


