/* src/config.h.  Generated from config.h.in by configure.  */
/* src/config.h.in.  Generated from configure.ac by autoheader.  */

/* Define to 1 if `struct sockaddr_in' has a `sin_len' member */
#define BSD44SOCKETS 1

/* Include compose table cache support */
/* #undef COMPOSECACHE */

/* Has getresuid() & getresgid() functions */
/* #undef HASGETRESUID */

/* Has issetugid() function */
#define HASSETUGID 1

/* Has shm*() functions */
#define HAS_SHM 1

/* Define to 1 if you have the `authdes_create' function. */
/* #undef HAVE_AUTHDES_CREATE */

/* Define to 1 if you have the `authdes_seccreate' function. */
/* #undef HAVE_AUTHDES_SECCREATE */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Use dlopen to load shared libraries */
#define HAVE_DLOPEN 1

/* Define to 1 if you have the <dl.h> header file. */
/* #undef HAVE_DL_H */

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* launchd support available */
#define HAVE_LAUNCHD 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have a working `mmap' system call. */
/* #undef HAVE_MMAP */

/* Use shl_load to load shared libraries */
/* #undef HAVE_SHL_LOAD */

/* Define to 1 if the system has the type `socklen_t'. */
#define HAVE_SOCKLEN_T 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Support IPv6 for TCP connections */
#define IPv6 1

/* Support os-specific local connections */
/* #undef LOCALCONN */

/* Disable XLOCALEDIR environment variable */
/* #undef NO_XLOCALEDIR */

/* Name of package */
#define PACKAGE "libX11"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "https://bugs.freedesktop.org/enter_bug.cgi?product=xorg"

/* Define to the full name of this package. */
#define PACKAGE_NAME "libX11"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "libX11 1.1.5"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "libX11"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.1.5"

/* Major version of this package */
#define PACKAGE_VERSION_MAJOR 1

/* Minor version of this package */
#define PACKAGE_VERSION_MINOR 1

/* Patch version of this package */
#define PACKAGE_VERSION_PATCHLEVEL 5

/* Define as the return type of signal handlers (`int' or `void'). */
/* #undef RETSIGTYPE */

/* Support Secure RPC ("SUN-DES-1") authentication for X11 clients */
/* #undef SECURE_RPC */

/* Define to 1 if you have the ANSI C header files. */
/* #undef STDC_HEADERS */

/* Support TCP socket connections */
#define TCPCONN 1

/* launchd support available */
#define TRANS_REOPEN 1

/* Support UNIX socket connections */
#define UNIXCONN 1

/* Split some i18n functions into loadable modules */
/* #undef USE_DYNAMIC_LC */

/* Use the X cursor library to load cursors */
#define USE_DYNAMIC_XCURSOR 1

/* poll() function is available */
#define USE_POLL 1

/* Use XCB for low-level protocol implementation */
#define USE_XCB 1

/* Version number of package */
#define VERSION "1.1.5"

/* Location of libX11 data */
#define X11_DATADIR "/usr/local/share/X11"

/* Location of libX11 library data */
#define X11_LIBDIR "/usr/local/lib/X11"

/* Include support for XCMS */
#define XCMS 1

/* Location of error message database */
#define XERRORDB "/usr/local/share/X11/XErrorDB"

/* Enable XF86BIGFONT extension */
#define XF86BIGFONT 1

/* Use XKB */
#define XKB 1

/* Location of keysym database */
#define XKEYSYMDB "/usr/local/share/X11/XKeysymDB"

/* support for X Locales */
#define XLOCALE 1

/* Location of libX11 locale data */
#define XLOCALEDATADIR "/usr/local/share/X11/locale"

/* Location of libX11 locale data */
#define XLOCALEDIR "/usr/local/share/X11/locale"

/* Location of libX11 locale libraries */
#define XLOCALELIBDIR "/usr/local/lib/X11/locale"

/* Whether libX11 is compiled with thread support */
#define XTHREADS 

/* Whether libX11 needs to use MT safe API's */
#define XUSE_MTSAFE_API 

/* Enable GNU and other extensions to the C environment for glibc */
/* #undef _GNU_SOURCE */
