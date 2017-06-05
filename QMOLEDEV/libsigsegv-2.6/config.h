/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* The name of the include file describing the fault handler. */
#define CFG_FAULT "fault-none.h"

/* The name of the file implementing the handler functionality. */
#define CFG_HANDLER "handler-none.c"

/* The name of the file implementing sigsegv_reset_onstack_flag. */
#define CFG_LEAVE "leave-nop.c"

/* The name of the include file describing the Mach fault handler. */
#define CFG_MACHFAULT "fault-none.h"

/* The name of the include file describing the fault signals. */
#define CFG_SIGNALS "signals-macos.h"

/* The name of the file determining the stack virtual memory area. */
#define CFG_STACKVMA "stackvma-mach.c"

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define if getpagesize() is available as a function or a macro. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the `getrlimit' function. */
#define HAVE_GETRLIMIT 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mincore' function. */
/* #undef HAVE_MINCORE */

/* Define if <sys/mman.h> defines MAP_ANON and mmaping with MAP_ANON works. */
#define HAVE_MMAP_ANON 1

/* Define if <sys/mman.h> defines MAP_ANONYMOUS and mmaping with MAP_ANONYMOUS
   works. */
/* #undef HAVE_MMAP_ANONYMOUS */

/* Define if mmaping of the special device /dev/zero works. */
/* #undef HAVE_MMAP_DEVZERO */

/* Define if PAGESIZE is available as a macro. */
/* #undef HAVE_PAGESIZE */

/* Define to 1 if you have the `setrlimit' function. */
#define HAVE_SETRLIMIT 1

/* Define to 1 if you have the `sigaltstack' function. */
#define HAVE_SIGALTSTACK 1

/* Define if CFG_STACKVMA is set to a nontrivial source file. */
#define HAVE_STACKVMA 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define if sysconf(_SC_PAGESIZE) is available as a function or a macro. */
#define HAVE_SYSCONF_PAGESIZE 1

/* Define to 1 if you have the <sys/signal.h> header file. */
#define HAVE_SYS_SIGNAL_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define if you have the sigaltstack() function and it works. */
#define HAVE_WORKING_SIGALTSTACK 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Name of package */
#define PACKAGE "libsigsegv"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#define PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME ""

/* Define to the version of this package. */
#define PACKAGE_VERSION ""

/* Define as the direction of stack growth for your system. STACK_DIRECTION >
   0 => grows toward higher addresses STACK_DIRECTION < 0 => grows toward
   lower addresses STACK_DIRECTION = 0 => spaghetti stack. */
#define STACK_DIRECTION -1

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "2.6"

/* Define to 'struct sigaltstack' if that's the type of the argument to
   sigaltstack */
/* #undef stack_t */
