/* w32lib.h  - Posix emulation layer for Sylpheed (Claws)
 *
 * This file is part of w32lib.
 *
 * w32lib is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * w32lib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * The code has been taken from the package
 *    http://claws.sylpheed.org/win32/sylpheed/w32lib-dev-2004.2.3.0.zip
 * on 2005-11-17 by Werner Koch <wk@gnupg.org>. There are no regular
 * copyright notices but the file version.rc from the ZIP archive
 * claims:
 *
 *     #define FILEVER "2004.2.3.0\0"
 *     #define PRODVER "2004.2.3\0"
 *
 *     [...]
 *      VALUE "FileDescription", "Posix emulation layer for Sylpheed (Claws)\0"
 *      VALUE "FileVersion", FILEVER
 *      VALUE "ProductVersion", PRODVER
 *      VALUE "LegalCopyright", "GPL\0"
 *      VALUE "CompanyName", "GNU / Free Software Foundation\0"
 *      VALUE "ProductName", "w32lib\0"
 *
 * Along with the fact that Sylpheed is under the GPL we can assume
 * that this code is under the GPL.  No author information or
 * changelogs have been found.
 * Files taken form the package are:
 *    w32_dirent.c w32_reg.c w32_stat.c w32_stdlib.c w32_time.c w32_wait.c
 *    w32_gettext.c w32_signal.c w32_stdio.c w32_string.c w32_unistd.c
 */

/* Changes are:

2007-05-21  Werner Koch  <wk@g10code.com>

	* src/common/w32_account.c: New.

	* src/common/w32lib.h: Undef "interface".

2005-11-17  Werner Koch  <wk@g10code.com>

	Add boilerplate text to all files and explain legal status.

	* w32_reg.c: Replaced g_free and g_strdup by regular C functions.
	(get_content_type_from_registry_with_ext): Ditto.
	* w32_dirent.c (readdir): Ditto. 
	(opendir): Ditto.
	(closedir): Reformatted.
	(readdir): Reformatted, replaced use of g_strdup_printf and other
	g-style malloc function by regular ones.  Use DIR structure from mingw.
        * w32lib.h: Don't define finddata_t for mingw. Replaced replacement
        DIR structure by the one form mingw.  Allocate filename in dirent
        statically to match the defintion ussed by mingw.
	* w32_reg.c (read_w32_registry_string): Return error for invalid root
        key.

  */


#ifndef _W32LIB_H_
#define _W32LIB_H_

#include <windows.h>
#include <io.h>
#include <stdio.h>

#ifdef __MINGW32__
#include <_mingw.h>
#define MINGW32_VERSION (__MINGW32_MAJOR_VERSION * 100 \
			 + __MINGW32_MINOR_VERSION)
#include <wchar.h>
#include <dirent.h>
#include <sys/time.h>
#endif

/* Mingw32 3.4.4 defines interface to struct and thus breaks our own
   use of that symbol.  Undef it here. */
#if defined(_BASETYPS_H) && defined(interface) 
#undef interface
#endif


/* types */
/*** ??? ***/
#ifndef __MINGW32__
typedef long int off_t;
typedef int pid_t;
typedef unsigned char u_char;
#endif /* __MINGW32__ */
typedef unsigned int uid_t;

#ifndef __MINGW32__
/*** stat ***/
#define S_IRUSR	_S_IREAD
#define S_IWUSR	_S_IWRITE
#define S_IXUSR	_S_IEXEC
#define S_IRWXU	(_S_IREAD|_S_IWRITE|_S_IEXEC)
#endif /* __MINGW32__ */


/* (signal?) */
#define SIGPIPE	_S_IFIFO


/* (directory) */
#define __S_ISTYPE(mode, mask)  (((mode) & _S_IFMT) == (mask))
#ifndef __MINGW32__
#define S_ISFIFO(mode)	__S_ISTYPE((mode), _S_IFIFO)
#define S_ISDIR(mode)	__S_ISTYPE((mode), _S_IFDIR)
#define S_ISREG(mode)	__S_ISTYPE((mode), _S_IFREG)
#endif /* __MINGW32__ */

/* functions */
/*** str ***/
int strcasecmp( const char *s1, const char *s2 );

int strncasecmp( const char *s1, const char *s2, size_t n );

/*** dir ***/
#ifndef __MINGW32__
typedef void * HANDLE;

#ifndef _FINDDATA_T_DEFINED
typedef unsigned long _fsize_t; /* Could be 64 bits for Win32 */
struct _finddata_t {
    unsigned    attrib;
    time_t      time_create;    /* -1 for FAT file systems */
    time_t      time_access;    /* -1 for FAT file systems */
    time_t      time_write;
    _fsize_t    size;
    char        name[260];
};
#endif /* !_FINDDATA_T_DEFINED */

struct dirent {
	long d_ino;
	unsigned short d_reclen;
	unsigned short d_namlen;
	char d_name[FILENAME_MAX];
};

typedef struct
{
	struct _finddata_t	dd_dta;
	struct dirent		dd_dir;
	long			dd_handle;
	int			dd_stat;
	char			dd_name[1];
} DIR;

#endif /* !__MINGW32__ */


DIR *opendir( const char *name );
int closedir( DIR *dir );
struct dirent *readdir( DIR *dir );

#if defined (__MINGW32__) && MINGW32_VERSION < 312
struct timezone {
  int tz_minuteswest;
  int tz_dsttime;
};
#endif

/*** stat ***/
int lstat( const char *file_name, struct stat *buf );

/*** sys/wait ***/
pid_t waitpid( pid_t pid, int *status, int options );

/*** sys/time ***/
#if ! defined (__MINGW32__) || MINGW32_VERSION < 312
int gettimeofday( struct timeval *tv, struct timezone *tz );
#endif

/*** unistd ***/
int setpgid( pid_t pid, pid_t pgid );
pid_t getppid( void );
pid_t fork( void );
unsigned int sleep( unsigned int seconds );

/*** stdlib ***/
long int random( void );
void srandom( unsigned int seed );
int truncate( const char *path, off_t length );

/*** signal ***/
int kill( pid_t pid, int sig );

/*** stdio ***/
FILE *popen( const char *command, const char *type );
int pclose( FILE *stream );

/*** w32_account.c ***/
int w32_is_administrator (void);

/*** misc ***/
int write_w32_registry_string( char *parent, char *section, char *value, char *data );
int write_w32_registry_dword( char *parent, char *section, char *value, int data );
char *read_w32_registry_string( char *parent, char *section, char *key );
char *get_content_type_from_registry_with_ext( char *ext );

#endif
