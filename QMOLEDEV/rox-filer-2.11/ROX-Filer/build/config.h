/* config.h.  Generated from config.h.in by configure.  */
/* The configure script will auto-generate config.h from config.h.in */

#define PROJECT "ROX-Filer"
#define VERSION "2.11"
#define GTK_VERSION "2.24.17"

/* #undef STDC_HEADERS */
#define HAVE_SYS_UCRED_H 1
/* #undef HAVE_MNTENT_H */
/* #undef HAVE_SYS_MNTENT_H */
#define HAVE_FCNTL_H 1
#define HAVE_GETOPT_LONG 1
#define HAVE_UNSETENV 1
#define FILE_B_FLAG 1
#define USE_PANGO_WRAP_WORD_CHAR 1
/* #undef HAVE_APSYMBOLS_H */
/* #undef HAVE_APBUILD_APSYMBOLS_H */
#define HAVE_STATFS 1
#define HAVE_STATVFS 1
/* #undef HAVE_SYS_VFS_H */
#define HAVE_SYS_STATVFS_H 1
#define HAVE_LIBINTL_H 1
/* #undef HAVE_SYS_INOTIFY_H */

#define HAVE_MBRTOWC 1
#define HAVE_WCTYPE_H 1

/* #undef LARGE_FILE_SUPPORT */

/* #undef HAVE_REGEX_H */

#define HAVE_GETXATTR 1
/* #undef HAVE_ATTROPEN */
#define HAVE_SYS_XATTR_H 1
/* #undef HAVE_ATTR_XATTR_H */

/* Enable extensions - used for dnotify support */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE
#endif

#if defined(HAVE_APSYMBOLS_H)
# include <apsymbols.h>
#elif defined(HAVE_APBUILD_APSYMBOLS_H)
# include <apbuild/apsymbols.h>
#endif

#include "my_vfs.h"

#ifdef HAVE_LIBINTL_H
# include <libintl.h>
# define _(String) dgettext("ROX-Filer", String)
#else
# define _(String) (String)
#endif
/* Short for gettext_noop() - marks a string as translatable without
 * actually translating it at that point. Used by xgettext.
 */
#define N_(String) (String)

/* printf format string to print file sizes */
#ifdef LARGE_FILE_SUPPORT
# define SIZE_FMT G_GINT64_MODIFIER "d"
#else
# define SIZE_FMT G_GINT32_MODIFIER "d"
#endif

#if 1
/* For releases... */
# define GTK_ENABLE_DEPRECATED
# define GDK_ENABLE_DEPRECATED
# define G_ENABLE_DEPRECATED
#else
/* Only for testing... */
# define GTK_DISABLE_DEPRECATED 
# define GDK_DISABLE_DEPRECATED
# define G_DISABLE_DEPRECATED
#endif

#ifndef S_ISDOOR
# define S_ISDOOR(mode) (FALSE)
#endif
