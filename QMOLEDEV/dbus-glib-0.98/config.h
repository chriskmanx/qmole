/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Enable bash completion */
#define DBUS_BASH_COMPLETION 1

/* Build test code */
/* #undef DBUS_BUILD_TESTS */

/* Disable assertion checking */
#define DBUS_DISABLE_ASSERT 1

/* Disable public API sanity checking */
/* #undef DBUS_DISABLE_CHECKS */

/* Support a verbose mode */
/* #undef DBUS_ENABLE_VERBOSE_MODE */

/* Defined to the gcc version if gcov is enabled, to force a rebuild due to
   config.h changing */
/* #undef DBUS_GCOV_ENABLED */

/* Where to put test sockets */
#define DBUS_TEST_SOCKET_DIR ""

/* Disable GLib assertion macros */
#define G_DISABLE_ASSERT 1

/* Disable GLib public API sanity checking */
/* #undef G_DISABLE_CHECKS */

/* Have abstract socket namespace */
/* #undef HAVE_ABSTRACT_SOCKETS */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <expat.h> header file. */
#define HAVE_EXPAT_H 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Have socklen_t type */
#define HAVE_SOCKLEN_T 1

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

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Name of package */
#define PACKAGE "dbus-glib"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "https://bugs.freedesktop.org/enter_bug.cgi?product=dbus"

/* Define to the full name of this package. */
#define PACKAGE_NAME "dbus-glib"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "dbus-glib 0.98"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "dbus-glib"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "0.98"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Full path to test file test/core/test-service-glib in builddir */
#define TEST_CORE_SERVICE_BINARY "/home/chris/dbus-glib-0.98/test/core/test-service-glib"

/* Full path to test file test/test-exit in builddir */
#define TEST_EXIT_BINARY "/home/chris/dbus-glib-0.98/test/test-exit"

/* Full path to test file test/interfaces/test-service in builddir */
#define TEST_INTERFACES_SERVICE_BINARY "/home/chris/dbus-glib-0.98/test/interfaces/test-service"

/* Full path to test file test/test-segfault in builddir */
#define TEST_SEGFAULT_BINARY "/home/chris/dbus-glib-0.98/test/test-segfault"

/* Full path to test file test/test-service in builddir */
#define TEST_SERVICE_BINARY "/home/chris/dbus-glib-0.98/test/test-service"

/* Full path to test file test/data/valid-service-files in builddir */
#define TEST_SERVICE_DIR "/home/chris/dbus-glib-0.98/test/data/valid-service-files"

/* Full path to test file test/test-shell-service in builddir */
#define TEST_SHELL_SERVICE_BINARY "/home/chris/dbus-glib-0.98/test/test-shell-service"

/* Full path to test file test/test-sleep-forever in builddir */
#define TEST_SLEEP_FOREVER_BINARY "/home/chris/dbus-glib-0.98/test/test-sleep-forever"

/* Version number of package */
#define VERSION "0.98"
