/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */

/* major.minor */
#define ABIWORD_SERIES "2.7"

/* Define to enable debugging functionality */
/* #undef DEBUG */

/* Define to prevent symbols from being exported dynamically */
/* #undef DISABLE_EXPORTS */

/* Whether we are building for embedded device */
/* #undef EMBEDDED_TARGET */

/* Generic embedded platform */
#define EMBEDDED_TARGET_GENERIC 1

/* Hildon embedded platform */
#define EMBEDDED_TARGET_HILDON 2

/* Poky embedded platform */
#define EMBEDDED_TARGET_POKY 3

/* Define to enable include emacs-compatible keyboard commands */
#define ENABLE_EMACS_KEYBINDING 1

/* Define if building menu button support */
/* #undef ENABLE_MENUBUTTON */

/* Define if building printing support */
/* #undef ENABLE_PRINT */

/* Define if building spell checking support */
#define ENABLE_SPELL 1

/* Define if building status bar */
#define ENABLE_STATUSBAR 1

/* Define to enable include vi-compatible keyboard commands */
#define ENABLE_VI_KEYBINDING 1

/* Define to 1 if you have the <asio.hpp> header file. */
/* #undef HAVE_ASIO_HPP */

/* define if the Boost library is available */
/* #undef HAVE_BOOST */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <eps/eps.h> header file. */
/* #undef HAVE_EPS_EPS_H */

/* GSF has GIO support */
#define HAVE_GSF_GIO 1

/* GTK is at least 2.14 */
#define HAVE_GTK214 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <tidy/tidy.h> header file. */
/* #undef HAVE_TIDY_TIDY_H */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to disable debugging functionality */
#define NDEBUG 1

/* Name of package */
#define PACKAGE "abiword"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "http://www.abisource.com/"

/* Define to the full name of this package. */
#define PACKAGE_NAME "abiword"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "abiword 2.7.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "abiword"

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.7.0"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
/* #undef TIME_WITH_SYS_TIME */

/* Build cocoa user interface */
/* #undef TOOLKIT_COCOA */

/* Build gtk+ user interface */
#define TOOLKIT_GTK 1

/* Build win32 user interface */
/* #undef TOOLKIT_WIN */

/* Version number of package */
#define VERSION "2.7.0"

/* Using Cairo */
#define WITH_CAIRO 1

/* use Dom's enchanting spell checker abstraction library */
#define WITH_ENCHANT 1

/* Define if using GIO */
#define WITH_GIO 1

/* Define if using gnome-vfs */
/* #undef WITH_GNOMEVFS */

/* Define if using goffice */
/* #undef WITH_GOFFICE */

/* Define if using gucharmap */
/* #undef WITH_GUCHARMAP */

/* do not use XOR drawing functions */
/* #undef XAP_DONTUSE_XOR */

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#define YYTEXT_POINTER 1

/* Define for Solaris 2.5.1 so the uint32_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef were allowed, the
   #define below would cause a syntax error. */
/* #undef _UINT32_T */

/* minimal comctl.dll v4.70 for toolbars */
/* #undef _WIN32_IE */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to the type of an unsigned integer type of width exactly 32 bits if
   such a type exists and the standard includes do not define it. */
/* #undef uint32_t */
