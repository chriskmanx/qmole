/* magick/config.h.  Generated automatically by configure.  */
/* magick/config.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define if you don't have vprintf but do have _doprnt.  */
/* #undef HAVE_DOPRNT */

/* Define if you have a working `mmap' system call.  */
/* #undef HAVE_MMAP */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1

/* Define if you have the vprintf function.  */
#define HAVE_VPRINTF 1

/* Define if you have the wait3 system call.  */
/* #undef HAVE_WAIT3 */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `long' if <sys/types.h> doesn't define.  */
/* #undef off_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if the `S_IS*' macros in <sys/stat.h> do not work properly.  */
/* #undef STAT_MACROS_BROKEN */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define on System V Release 4.  */
/* #undef SVR4 */

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define if the X Window System is missing or not being used.  */
/* #undef X_DISPLAY_MISSING */

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define if you don't have vprintf but do have _doprnt.  */
/* #undef HAVE_DOPRNT */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1

/* Define if you have the vprintf function.  */
#define HAVE_VPRINTF 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define on System V Release 4.  */
/* #undef SVR4 */

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define if the X Window System is missing or not being used.  */
/* #undef X_DISPLAY_MISSING */

/* Define if you have X11 library */
#define HasX11 1

/* Include patented LZW compression */
/* #undef HasLZW */

/* X11 server supports shape extension */
#define HasShape 1

/* X11 server supports shared memory extension */
#define HasSharedMemory 1

/* Increase max color value from 255 to 65535 */
/* #undef QuantumLeap */

/* Location of X11 RGB database */
#define RGBColorDatabase "/usr/X11R6/lib/X11/rgb.txt"

/* Define if you have the bzip2 library */
/* #undef HasBZLIB */

/* Define if you have Display Postscript */
/* #undef HasDPS */

/* Define if you have FlashPIX library */
/* #undef HasFPX */

/* Define if you have HDF library */
/* #undef HasHDF */

/* Define if you have JBIG library */
/* #undef HasJBIG */

/* Define if you have JPEG library */
#define HasJPEG 1

/* Define if you have PNG library */
#define HasPNG 1

/* Define if you have TIFF library */
#define HasTIFF 1

/* Define if you have FreeType (TrueType font) library */
/* #undef HasTTF */

/* Define to specify default TrueType font path. */
/* #undef TT_FONT_PATH */

/* Define if you have the SFIO mmap-based stdio library emulation */
/* #undef HasSFIO */

/* Define if you have zlib compression library */
#define HasZLIB 1

/* Define if you have sys_errlist[] in libc */
#define HAVE_SYS_ERRLIST 1

/* Define if you have HP style pty allocation (/dev/ptym/ptyp*) */
/* #undef HAVE_PTYM */

/* Define if you have HP style pty trapping (struct.*request_info in sys/ptyio.h) */
/* #undef HAVE_PTYTRAP */

/* Define if you have AIX new-style pty allocation (/dev/ptc && /dev/pts) */
/* #undef HAVE_PTC_PTS */

/* Define if you have SGI old-style pty allocation (/dev/ptc && ! /dev/pts) */
/* #undef HAVE_PTC */

/* Define if you have SCO style pty allocation */
/* #undef HAVE_SCO_CLIST_PTYS */

/* Define if you have SVR4 style pty allocation (/dev/ptmx) */
#define HAVE_PTMX 1

/* Define if you have OSF/1 style pty allocation (/dev/ptmx_bsd) */
/* #undef HAVE_PTMX_BSD */

/* Define directory where ImageMagick/delegates.h lives. (default /usr/local/share) */
#define DelegatePath "/usr/local/share/ImageMagick/"

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getpagesize function.  */
#define HAVE_GETPAGESIZE 1

/* Define if you have the mkdir function.  */
#define HAVE_MKDIR 1

/* Define if you have the select function.  */
#define HAVE_SELECT 1

/* Define if you have the snprintf function.  */
#define HAVE_SNPRINTF 1

/* Define if you have the strchr function.  */
#define HAVE_STRCHR 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define if you have the strtol function.  */
#define HAVE_STRTOL 1

/* Define if you have the tempnam function.  */
#define HAVE_TEMPNAM 1

/* Define if you have the vsnprintf function.  */
#define HAVE_VSNPRINTF 1

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <errno.h> header file.  */
#define HAVE_ERRNO_H 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <malloc.h> header file.  */
/* #undef HAVE_MALLOC_H */

/* Define if you have the <math.h> header file.  */
#define HAVE_MATH_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <pwd.h> header file.  */
#define HAVE_PWD_H 1

/* Define if you have the <stdarg.h> header file.  */
#define HAVE_STDARG_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/stat.h> header file.  */
#define HAVE_SYS_STAT_H 1

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <sys/types.h> header file.  */
#define HAVE_SYS_TYPES_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if using the dmalloc debugging malloc package */
/* #undef WITH_DMALLOC */

