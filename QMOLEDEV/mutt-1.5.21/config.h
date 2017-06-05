/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define if you want classic PGP support. */
#define CRYPT_BACKEND_CLASSIC_PGP 1

/* Define if you want clasic S/MIME support. */
#define CRYPT_BACKEND_CLASSIC_SMIME 1

/* Define if you use GPGME to support OpenPGP */
/* #undef CRYPT_BACKEND_GPGME */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to enable debugging info. */
/* #undef DEBUG */

/* Define if you want to use an external dotlocking program. */
/* #undef DL_STANDALONE */

/* Define your domain name. */
/* #undef DOMAIN */

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#define ENABLE_NLS 1

/* Enable exact regeneration of email addresses as parsed? NOTE: this requires
   significant more memory when defined. */
/* #undef EXACT_ADDRESS */

/* program to use for shell commands */
#define EXECSHELL "/bin/sh"

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <argz.h> header file. */
/* #undef HAVE_ARGZ_H */

/* Define to 1 if you have the `bind_textdomain_codeset' function. */
/* #undef HAVE_BIND_TEXTDOMAIN_CODESET */

/* Define if you have bkgdset, as a function or macro. */
#define HAVE_BKGDSET 1

/* Define if you have the C99 integer types */
#define HAVE_C99_INTTYPES 1

/* Define if your curses library supports color. */
#define HAVE_COLOR 1

/* Define if you have curs_set, as a function or macro. */
#define HAVE_CURS_SET 1

/* Berkeley DB4 Support */
/* #undef HAVE_DB4 */

/* Define to 1 if you have the `dcgettext' function. */
#define HAVE_DCGETTEXT 1

/* Define to 1 if you have the declaration of
   `GNUTLS_VERIFY_DISABLE_TIME_CHECKS', and to 0 if you don't. */
/* #undef HAVE_DECL_GNUTLS_VERIFY_DISABLE_TIME_CHECKS */

/* Define to 1 if you have the declaration of `sys_siglist', and to 0 if you
   don't. */
#define HAVE_DECL_SYS_SIGLIST 1

/* Define to 1 if your system has the dirent::d_ino member */
#define HAVE_DIRENT_D_INO 1

/* Define to 1 if you have the `fchdir' function. */
#define HAVE_FCHDIR 1

/* Define to 1 if you have the `feof_unlocked' function. */
#define HAVE_FEOF_UNLOCKED 1

/* Define to 1 if you have the `fgetpos' function. */
#define HAVE_FGETPOS 1

/* Define to 1 if you have the `fgets_unlocked' function. */
/* #undef HAVE_FGETS_UNLOCKED */

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
#define HAVE_FSEEKO 1

/* Define to 1 if you have the `ftruncate' function. */
#define HAVE_FTRUNCATE 1

/* GDBM Support */
/* #undef HAVE_GDBM */

/* Define to 1 if you have the `getaddrinfo' function. */
#define HAVE_GETADDRINFO 1

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the `getegid' function. */
#define HAVE_GETEGID 1

/* Define to 1 if you have the `geteuid' function. */
#define HAVE_GETEUID 1

/* Define to 1 if you have the `getgid' function. */
#define HAVE_GETGID 1

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the `getsid' function. */
/* #undef HAVE_GETSID */

/* Define if the GNU gettext() function is already present or preinstalled. */
#define HAVE_GETTEXT 1

/* Define to 1 if you have the `getuid' function. */
#define HAVE_GETUID 1

/* Define if GPGME supports PKA */
/* #undef HAVE_GPGME_PKA_TRUST */

/* Define if your GSSAPI implementation is Heimdal */
/* #undef HAVE_HEIMDAL */

/* Define if you have the iconv() function. */
/* #undef HAVE_ICONV */

/* Define to 1 if you have the <iconv.h> header file. */
/* #undef HAVE_ICONV_H */

/* Define if <iconv.h> defines iconv_t. */
/* #undef HAVE_ICONV_T_DEF */

/* Define to 1 if you have the `idna_to_ascii_8z' function. */
/* #undef HAVE_IDNA_TO_ASCII_8Z */

/* Define to 1 if you have the `idna_to_ascii_from_locale' function. */
/* #undef HAVE_IDNA_TO_ASCII_FROM_LOCALE */

/* Define to 1 if you have the `idna_to_ascii_from_utf8' function. */
/* #undef HAVE_IDNA_TO_ASCII_FROM_UTF8 */

/* Define to 1 if you have the `idna_to_ascii_lz' function. */
/* #undef HAVE_IDNA_TO_ASCII_LZ */

/* Define to 1 if you have the `idna_to_unicode_8z8z' function. */
/* #undef HAVE_IDNA_TO_UNICODE_8Z8Z */

/* Define to 1 if you have the `idna_to_unicode_utf8_from_utf8' function. */
/* #undef HAVE_IDNA_TO_UNICODE_UTF8_FROM_UTF8 */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <ioctl.h> header file. */
/* #undef HAVE_IOCTL_H */

/* Define to 1 if you have the `iswalnum' function. */
#define HAVE_ISWALNUM 1

/* Define to 1 if you have the `iswalpha' function. */
#define HAVE_ISWALPHA 1

/* Define to 1 if you have the `iswblank' function. */
#define HAVE_ISWBLANK 1

/* Define to 1 if you have the `iswcntrl' function. */
#define HAVE_ISWCNTRL 1

/* Define to 1 if you have the `iswdigit' function. */
#define HAVE_ISWDIGIT 1

/* Define to 1 if you have the `iswgraph' function. */
#define HAVE_ISWGRAPH 1

/* Define to 1 if you have the `iswlower' function. */
#define HAVE_ISWLOWER 1

/* Define to 1 if you have the `iswprint' function. */
#define HAVE_ISWPRINT 1

/* Define to 1 if you have the `iswpunct' function. */
#define HAVE_ISWPUNCT 1

/* Define to 1 if you have the `iswspace' function. */
#define HAVE_ISWSPACE 1

/* Define to 1 if you have the `iswupper' function. */
#define HAVE_ISWUPPER 1

/* Define to 1 if you have the `iswxdigit' function. */
#define HAVE_ISWXDIGIT 1

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
#define HAVE_LANGINFO_CODESET 1

/* Define if you have <langinfo.h> and nl_langinfo(YESEXPR). */
#define HAVE_LANGINFO_YESEXPR 1

/* Define if your <locale.h> file defines LC_MESSAGES. */
#define HAVE_LC_MESSAGES 1

/* Define to 1 if you have the `idn' library */
/* #undef HAVE_LIBIDN */

/* Define to 1 if you have the `intl' library (-lintl). */
/* #undef HAVE_LIBINTL */

/* Define to 1 if you have the `nsl' library (-lnsl). */
/* #undef HAVE_LIBNSL */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef HAVE_LIBSOCKET */

/* Define to 1 if you have the `ssl' library (-lssl). */
/* #undef HAVE_LIBSSL */

/* Define to 1 if you have the `termlib' library (-ltermlib). */
/* #undef HAVE_LIBTERMLIB */

/* Define to 1 if you have the `x' library (-lx). */
/* #undef HAVE_LIBX */

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if the system has the type `long long int'. */
#define HAVE_LONG_LONG_INT 1

/* Define to 1 if you have the <malloc.h> header file. */
/* #undef HAVE_MALLOC_H */

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mempcpy' function. */
/* #undef HAVE_MEMPCPY */

/* Define if you have meta, as a function or macro. */
#define HAVE_META 1

/* Define to 1 if you have the `mkdtemp' function. */
#define HAVE_MKDTEMP 1

/* Define to 1 if you have a working `mmap' system call. */
#define HAVE_MMAP 1

/* Define to 1 if you have the `munmap' function. */
#define HAVE_MUNMAP 1

/* Define to 1 if you have the <ncursesw/ncurses.h> header file. */
/* #undef HAVE_NCURSESW_NCURSES_H */

/* Define to 1 if you have the <ncurses.h> header file. */
/* #undef HAVE_NCURSES_H */

/* Define to 1 if you have the <ncurses/ncurses.h> header file. */
#define HAVE_NCURSES_NCURSES_H 1

/* Define to 1 if you have the <nl_types.h> header file. */
#define HAVE_NL_TYPES_H 1

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* QDBM Support */
/* #undef HAVE_QDBM */

/* Define to 1 if you have the `RAND_egd' function. */
/* #undef HAVE_RAND_EGD */

/* Define to 1 if you have the `RAND_status' function. */
/* #undef HAVE_RAND_STATUS */

/* Define to 1 if you have the `regcomp' function. */
#define HAVE_REGCOMP 1

/* Define if you have resizeterm, as a function or macro. */
#define HAVE_RESIZETERM 1

/* Define to 1 if you have the `setegid' function. */
#define HAVE_SETEGID 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the `setrlimit' function. */
#define HAVE_SETRLIMIT 1

/* Define to 1 if you have a C99 compliant snprintf function. */
#define HAVE_SNPRINTF 1

/* Define to 1 if you have the `srand48' function. */
#define HAVE_SRAND48 1

/* Define if you have start_color, as a function or macro. */
#define HAVE_START_COLOR 1

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `stpcpy' function. */
#define HAVE_STPCPY 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strcasestr' function. */
#define HAVE_STRCASESTR 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the `strftime' function. */
#define HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strsep' function. */
#define HAVE_STRSEP 1

/* Define to 1 if you have the `strtok_r' function. */
#define HAVE_STRTOK_R 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the <sysexits.h> header file. */
#define HAVE_SYSEXITS_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Tokyo Cabinet Support */
/* #undef HAVE_TC */

/* Define to 1 if you have the `towlower' function. */
#define HAVE_TOWLOWER 1

/* Define to 1 if you have the `towupper' function. */
#define HAVE_TOWUPPER 1

/* Define to 1 if you have the `tsearch' function. */
#define HAVE_TSEARCH 1

/* Define if you have typeahead, as a function or macro. */
#define HAVE_TYPEAHEAD 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the <unix.h> header file. */
/* #undef HAVE_UNIX_H */

/* Define if you have use_default_colors, as a function or macro. */
#define HAVE_USE_DEFAULT_COLORS 1

/* Define to 1 if you have the <villa.h> header file. */
/* #undef HAVE_VILLA_H */

/* Define to 1 if you have a C99 compliant vsnprintf function. */
#define HAVE_VSNPRINTF 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if you have the `wcscasecmp' function. */
/* #undef HAVE_WCSCASECMP */

/* Define to 1 if you have the <wctype.h> header file. */
#define HAVE_WCTYPE_H 1

/* Define if you are using the system's wchar_t functions. */
#define HAVE_WC_FUNCS 1

/* Define to 1 if you have the `__argz_count' function. */
/* #undef HAVE___ARGZ_COUNT */

/* Define to 1 if you have the `__argz_next' function. */
/* #undef HAVE___ARGZ_NEXT */

/* Define to 1 if you have the `__argz_stringify' function. */
/* #undef HAVE___ARGZ_STRINGIFY */

/* Is mail spooled to the user's home directory? If defined, MAILPATH should
   be set to the filename of the spool mailbox relative the the home
   directory. use: configure --with-homespool=FILE */
#define HOMESPOOL 1

/* Define as const if the declaration of iconv() needs const. */
/* #undef ICONV_CONST */

/* Define as 1 if iconv() only converts exactly and we should treat all return
   values other than (size_t)(-1) as equivalent. */
/* #undef ICONV_NONTRANS */

/* Where to find ispell on your system. */
/* #undef ISPELL */

/* Define if the result of isprint() is unreliable. */
/* #undef LOCALES_HACK */

/* Where new mail is spooled. */
#define MAILPATH "/home/chris/mail"

/* Define if you want complete documentation. */
#define MAKEDOC_FULL 1

/* Where to find mixmaster on your system. */
/* #undef MIXMASTER */

/* Define if you have problems with mutt not detecting new/old mailboxes over
   NFS. Some NFS implementations incorrectly cache the attributes of small
   files. */
/* #undef NFS_ATTRIBUTE_HACK */

/* Name of package */
#define PACKAGE "mutt"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#define PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME ""

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION ""

/* Define to 1 if the C compiler supports function prototypes. */
#define PROTOTYPES 1

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Where to find sendmail on your system. */
#define SENDMAIL "/usr/sbin/sendmail"

/* Some systems declare sig_atomic_t as volatile, some others -- no. This
   define will have value `sig_atomic_t' or `volatile sig_atomic_t'
   accordingly. */
#define SIG_ATOMIC_VOLATILE_T volatile sig_atomic_t

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `off_t', as computed by sizeof. */
#define SIZEOF_OFF_T 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to enable Sun mailtool attachments support. */
/* #undef SUN_ATTACHMENT */

/* Define to use dotlocking for mailboxes. */
#define USE_DOTLOCK 1

/* Define to use fcntl() to lock folders. */
#define USE_FCNTL 1

/* Define to use flock() to lock mailboxes. */
/* #undef USE_FLOCK */

/* Define if you want to use the included regex.c. */
/* #undef USE_GNU_REGEX */

/* Define if you have GSSAPI libraries available */
/* #undef USE_GSS */

/* Enable header caching */
/* #undef USE_HCACHE */

/* Define if you want support for the IMAP protocol. */
#define USE_IMAP 1

/* Define if you want support for the POP3 protocol. */
#define USE_POP 1

/* Define if want to use the SASL library for POP/IMAP authentication. */
/* #undef USE_SASL */

/* Define if mutt should run setgid "mail". */
/* #undef USE_SETGID */

/* Define if you compile with SLang instead of curses/ncurses. */
/* #undef USE_SLANG_CURSES */

/* Include internal SMTP relay support */
/* #undef USE_SMTP */

/* Include code for socket support. Set automatically if you enable POP3 or
   IMAP */
#define USE_SOCKET 1

/* Define if you want support for SSL. */
/* #undef USE_SSL */

/* Define if you want support for SSL via GNUTLS. */
/* #undef USE_SSL_GNUTLS */

/* Define if you want support for SSL via OpenSSL. */
/* #undef USE_SSL_OPENSSL */

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Version number of package */
#define VERSION "1.5.21"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

#ifndef HAVE_C99_INTTYPES
#  if SIZEOF_SHORT == 4
typedef unsigned short uint32_t;
#  elif SIZEOF_INT == 4
typedef unsigned int uint32_t;
#  elif SIZEOF_LONG == 4
typedef unsigned long uint32_t;
#  endif
#  if SIZEOF_INT == 8
typedef unsigned int uint64_t;
#  elif SIZEOF_LONG == 8
typedef unsigned long uint64_t;
#  elif SIZEOF_LONG_LONG == 8
typedef unsigned long long uint64_t;
#  endif
#endif
  

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
/* #undef _LARGEFILE_SOURCE */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define like PROTOTYPES; this can be used by system headers. */
#define __PROTOTYPES 1

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to 'int' if system headers don't define. */
/* #undef mbstate_t */

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef sig_atomic_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to 'int' if <sys/socket.h> doesn't have it. */
/* #undef socklen_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef ssize_t */

/* define if va_copy is not available */
/* #undef va_copy */

/* Define to 'int' if system headers don't define. */
/* #undef wchar_t */

/* Define to 'int' if system headers don't define. */
/* #undef wint_t */

/* fseeko portability defines */
#ifdef HAVE_FSEEKO
# define LOFF_T off_t
# if HAVE_C99_INTTYPES && HAVE_INTTYPES_H
#  if SIZEOF_OFF_T == 8
#   define OFF_T_FMT "%" PRId64
#  else
#   define OFF_T_FMT "%" PRId32
#  endif
# else
#  if (SIZEOF_OFF_T == 8) && (SIZEOF_LONG == 4)
#   define OFF_T_FMT "%lld"
#  else
#   define OFF_T_FMT "%ld"
#  endif
# endif
#else
# define LOFF_T long
# define fseeko fseek
# define ftello ftell
# define OFF_T_FMT "%ld"
#endif

