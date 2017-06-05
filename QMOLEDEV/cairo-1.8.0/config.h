/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* whether memory barriers are needed around atomic operations */
/* #undef ATOMIC_OP_NEEDS_MEMORY_BARRIER */

/* define if glitz backend can be tested against agl */
/* #undef CAIRO_CAN_TEST_GLITZ_AGL_SURFACE */

/* define if glitz backend can be tested against egl */
/* #undef CAIRO_CAN_TEST_GLITZ_EGL_SURFACE */

/* define if glitz backend can be tested against glx */
/* #undef CAIRO_CAN_TEST_GLITZ_GLX_SURFACE */

/* define if glitz backend can be tested against wgl */
/* #undef CAIRO_CAN_TEST_GLITZ_WGL_SURFACE */

/* Define to 1 if the PDF backend can be tested (need poppler and other
   dependencies for pdf2png) */
/* #undef CAIRO_CAN_TEST_PDF_SURFACE */

/* Define to 1 if the PS backend can be tested (needs ghostscript) */
/* #undef CAIRO_CAN_TEST_PS_SURFACE */

/* Define to 1 if the SVG backend can be tested */
#define CAIRO_CAN_TEST_SVG_SURFACE 1

/* Define to 1 if the Win32 Printing backend can be tested (needs ghostscript)
   */
/* #undef CAIRO_CAN_TEST_WIN32_PRINTING_SURFACE */

/* Define to 1 to enable cairo's test surfaces feature */
/* #undef CAIRO_HAS_TEST_SURFACES */

/* Define to 1 to disable certain code paths that rely heavily on double
   precision floating-point calculation */
/* #undef DISABLE_SOME_FLOATING_POINT */

/* Define to 1 if your system stores words within floats with the most
   significant word first */
/* #undef FLOAT_WORDS_BIGENDIAN */

/* Define to 1 if you have the <byteswap.h> header file. */
/* #undef HAVE_BYTESWAP_H */

/* Define to 1 if you have the `ctime_r' function. */
#define HAVE_CTIME_R 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the `drand48' function. */
#define HAVE_DRAND48 1

/* Define to 1 if you have the `FcFini' function. */
#define HAVE_FCFINI 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fedisableexcept' function. */
/* #undef HAVE_FEDISABLEEXCEPT */

/* Define to 1 if you have the `feenableexcept' function. */
/* #undef HAVE_FEENABLEEXCEPT */

/* Define to 1 if you have the `flockfile' function. */
#define HAVE_FLOCKFILE 1

/* FT_Bitmap_Size structure includes y_ppem field */
#define HAVE_FT_BITMAP_SIZE_Y_PPEM 1

/* Define to 1 if you have the `FT_GlyphSlot_Embolden' function. */
#define HAVE_FT_GLYPHSLOT_EMBOLDEN 1

/* Define to 1 if you have the `FT_Library_SetLcdFilter' function. */
#define HAVE_FT_LIBRARY_SETLCDFILTER 1

/* Define to 1 if you have the `FT_Load_Sfnt_Table' function. */
#define HAVE_FT_LOAD_SFNT_TABLE 1

/* Whether you have gcov */
/* #undef HAVE_GCOV */

/* Enable if your compiler supports the Intel __sync_* atomic primitives */
#define HAVE_INTEL_ATOMIC_PRIMITIVES 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <libgen.h> header file. */
#define HAVE_LIBGEN_H 1

/* Define to 1 if you have the `link' function. */
#define HAVE_LINK 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `poppler_page_render' function. */
/* #undef HAVE_POPPLER_PAGE_RENDER */

/* Define to 1 if you have the <pthread.h> header file. */
#define HAVE_PTHREAD_H 1

/* Define to 1 if you have the `rsvg_pixbuf_from_file' function. */
#define HAVE_RSVG_PIXBUF_FROM_FILE 1

/* Define to 1 if you have the `sched_getaffinity' function. */
/* #undef HAVE_SCHED_GETAFFINITY */

/* Define to 1 if you have the <sched.h> header file. */
#define HAVE_SCHED_H 1

/* Define to 1 if you have the <setjmp.h> header file. */
#define HAVE_SETJMP_H 1

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/int_types.h> header file. */
/* #undef HAVE_SYS_INT_TYPES_H */

/* Define to 1 if you have the <sys/poll.h> header file. */
#define HAVE_SYS_POLL_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/un.h> header file. */
#define HAVE_SYS_UN_H 1

/* Define to 1 if the system has the type `uint128_t'. */
/* #undef HAVE_UINT128_T */

/* Define to 1 if the system has the type `uint64_t'. */
#define HAVE_UINT64_T 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `vasnprintf' function. */
/* #undef HAVE_VASNPRINTF */

/* Define to 1 if you have the <windows.h> header file. */
/* #undef HAVE_WINDOWS_H */

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "http://bugs.freedesktop.org/enter_bug.cgi?product=cairo"

/* Define to the full name of this package. */
#define PACKAGE_NAME USE_cairo_INSTEAD

/* Define to the full name and version of this package. */
#define PACKAGE_STRING USE_cairo_version_OR_cairo_version_string_INSTEAD

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME USE_cairo_INSTEAD

/* Define to the version of this package. */
#define PACKAGE_VERSION USE_cairo_version_OR_cairo_version_string_INSTEAD

/* Define to 1 if you have the ANSI C header files. */
/* #undef STDC_HEADERS */

/* Define to the value your compiler uses to support the warn-unused-result
   attribute */
#define WARN_UNUSED_RESULT __attribute__((__warn_unused_result__))

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */


/* Deal with multiple architecture compiles on Mac OS X */
#ifdef __APPLE_CC__
#ifdef __BIG_ENDIAN__
#define WORDS_BIGENDIAN 1
#define FLOAT_WORDS_BIGENDIAN 1
#else
/* #undef WORDS_BIGENDIAN */
/* #undef FLOAT_WORDS_BIGENDIAN */
#endif
#endif


/* Define to 1 if the X Window System is missing or not being used. */
/* #undef X_DISPLAY_MISSING */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif
