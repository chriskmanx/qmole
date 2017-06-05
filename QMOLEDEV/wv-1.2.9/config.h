/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */


#ifndef HAVE_WV_CONFIG_H
#define HAVE_WV_CONFIG_H


/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define this if strcasecmp must be defined */
/* #undef DEFINE_STRCASECMP */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <io.h> header file. */
/* #undef HAVE_IO_H */

/* Define if you have libwmf(2) and want it to convert wmf to png files */
/* #undef HAVE_LIBWMF */

/* Define if libwmf(2) has support for reading 'foreign' image formats */
/* #undef HAVE_LIBWMF_FOREIGN_H */

/* Define if you have libxml2 */
#define HAVE_LIBXML2 1

/* Define to 1 if you have the <malloc.h> header file. */
/* #undef HAVE_MALLOC_H */

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have a working `mmap' system call. */
#define HAVE_MMAP 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/file.h> header file. */
#define HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define if you want zlib to uncompress wmf files */
#define HAVE_ZLIB 1

/* define if you have libpng */
#define HasPNG 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Define if sizeof({char,short,int}) != {1,2,4} */
#define MATCHED_TYPE 1

/* Name of package */
#define PACKAGE "wv"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "wvware-devel@sourceforge.net"

/* Define to the full name of this package. */
#define PACKAGE_NAME "wv"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "wv 1.2.9"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "wv"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.2.9"

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "1.2.9"

/* Define if byte order is big-endian */
/* #undef WORDS_BIGENDIAN */

/* Define as 12 for little-endian, 21 for big-endian */
#define XML_BYTE_ORDER 12

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */


#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if defined(HAVE_ERRNO_H)
#include <errno.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdarg.h>

#if defined(__GNUC__) && !defined(WORDS_BIGENDIAN) && defined(MATCHED_TYPE)
#define NO_HOLES
#endif

#ifndef HAVE_MEMCPY
#define memcpy(d, s, n) bcopy ((s), (d), (n))
#endif /* not HAVE_MEMCPY */

/* redefs of things that are either in glibc or we have to include them ourselves */
#if defined(WIN32) && !defined(__MWERKS__) && !defined(__WINE__)
#define strcasecmp(s1,s2) stricmp(s1,s2)
#else
#if !defined(__GLIBC__) || (__GLIBC__ < 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 2)
#ifdef DEFINE_STRCASECMP
int strcasecmp (const char *s1, const char *s2);
#endif
#endif
#endif

#endif /* ! HAVE_WV_CONFIG_H */

