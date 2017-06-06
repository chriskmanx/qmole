/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define if AIX */
/* #undef AIX */

/* Define if broken SIOCGIFMTU */
#define BROKEN_SIOCGIFMTU 1

/* Define if BSDi */
/* #undef BSDI */

/* Don't close opened shared objects for valgrind leak testing of dynamic
   libraries */
/* #undef DISABLE_DLCLOSE_FOR_VALGRIND_TESTING */

/* Define if errlist is predefined */
#define ERRLIST_PREDEFINED 1

/* Build with extended file inspection features. (Experimental) */
/* #undef FEAT_FILE_INSPECT */

/* Build with application id support. (Experimental) */
/* #undef FEAT_OPEN_APPID */

/* Define if FreeBSD */
/* #undef FREEBSD */

/* Define to 1 if the system has the type `boolean'. */
/* #undef HAVE_BOOLEAN */

/* Define to 1 if you have the `daq_acquire_with_meta' function. */
#define HAVE_DAQ_ACQUIRE_WITH_META 1

/* DAQ version supports address space ID in header. */
#define HAVE_DAQ_ADDRESS_SPACE_ID 1

/* Define to 1 if you have the `daq_dp_add_dc' function. */
#define HAVE_DAQ_DP_ADD_DC 1

/* DAQ version supports flow ID in header. */
#define HAVE_DAQ_FLOW_ID 1

/* Define to 1 if you have the `daq_hup_apply' function. */
#define HAVE_DAQ_HUP_APPLY 1

/* DAQ version supports DAQ_VERDICT_RETRY in DAQ_Verdict. */
#define HAVE_DAQ_VERDICT_RETRY 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <dnet.h> header file. */
#define HAVE_DNET_H 1

/* Define to 1 if you have the <dumbnet.h> header file. */
/* #undef HAVE_DUMBNET_H */

/* Define to 1 if you have the `inet_ntop' function. */
#define HAVE_INET_NTOP 1

/* Define to 1 if the system has the type `int16_t'. */
#define HAVE_INT16_T 1

/* Define to 1 if the system has the type `int32_t'. */
#define HAVE_INT32_T 1

/* Define to 1 if the system has the type `int64_t'. */
#define HAVE_INT64_T 1

/* Define to 1 if the system has the type `int8_t'. */
#define HAVE_INT8_T 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `dnet' library (-ldnet). */
#define HAVE_LIBDNET 1

/* Define to 1 if you have the `dumbnet' library (-ldumbnet). */
/* #undef HAVE_LIBDUMBNET */

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `nsl' library (-lnsl). */
/* #undef HAVE_LIBNSL */

/* Define to 1 if you have the `pcap' library (-lpcap). */
#define HAVE_LIBPCAP 1

/* Define to 1 if you have the `pcre' library (-lpcre). */
#define HAVE_LIBPCRE 1

/* Define to 1 if you have the `pfring' library (-lpfring). */
/* #undef HAVE_LIBPFRING */

/* Define to 1 if you have the `rt' library (-lrt). */
/* #undef HAVE_LIBRT */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef HAVE_LIBSOCKET */

/* Define to 1 if you have the `uuid' library (-luuid). */
#define HAVE_LIBUUID 1

/* Define to 1 if you have the `z' library (-lz). */
#define HAVE_LIBZ 1

/* Define whether linuxthreads is being used */
/* #undef HAVE_LINUXTHREADS */

/* Define to 1 if you have the <lzma.h> header file. */
#define HAVE_LZMA_H 1

/* Define to 1 if you have the `mallinfo' function. */
/* #undef HAVE_MALLINFO */

/* Define to 1 if you have the `malloc_trim' function. */
/* #undef HAVE_MALLOC_TRIM */

/* Define to 1 if you have the <math.h> header file. */
#define HAVE_MATH_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `memrchr' function. */
/* #undef HAVE_MEMRCHR */

/* openssl MD5 available */
#define HAVE_OPENSSL_MD5 1

/* openssl SHA available */
#define HAVE_OPENSSL_SHA 1

/* Define to 1 if you have the <paths.h> header file. */
#define HAVE_PATHS_H 1

/* Can cleanup lex buffer stack created by pcap bpf filter */
#define HAVE_PCAP_LEX_DESTROY 1

/* Can output the library version. */
#define HAVE_PCAP_LIB_VERSION 1

/* Define to 1 if you have the <pcre.h> header file. */
#define HAVE_PCRE_H 1

/* Define to 1 if you have the <pfring.h> header file. */
/* #undef HAVE_PFRING_H */

/* Define to 1 if you have the `sigaction' function. */
#define HAVE_SIGACTION 1

/* snprintf function is available */
#define HAVE_SNPRINTF /**/

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strlcat' function. */
#define HAVE_STRLCAT 1

/* Define to 1 if you have the `strlcpy' function. */
#define HAVE_STRLCPY 1

/* Define to 1 if you have the `strtoul' function. */
/* #undef HAVE_STRTOUL */

/* Define to 1 if you have the <sys/sockio.h> header file. */
#define HAVE_SYS_SOCKIO_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if the system has the type `uint16_t'. */
#define HAVE_UINT16_T 1

/* Define to 1 if the system has the type `uint32_t'. */
#define HAVE_UINT32_T 1

/* Define to 1 if the system has the type `uint64_t'. */
#define HAVE_UINT64_T 1

/* Define to 1 if the system has the type `uint8_t'. */
#define HAVE_UINT8_T 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the <uuid/uuid.h> header file. */
#define HAVE_UUID_UUID_H 1

/* Define to 1 if the system has the type `u_int16_t'. */
#define HAVE_U_INT16_T 1

/* Define to 1 if the system has the type `u_int32_t'. */
#define HAVE_U_INT32_T 1

/* Define to 1 if the system has the type `u_int64_t'. */
#define HAVE_U_INT64_T 1

/* Define to 1 if the system has the type `u_int8_t'. */
#define HAVE_U_INT8_T 1

/* Define if the compiler supports visibility declarations. */
#define HAVE_VISIBILITY 1

/* Define to 1 if you have the `vsnprintf' function. */
/* #undef HAVE_VSNPRINTF */

/* Define to 1 if you have the `vswprintf' function. */
#define HAVE_VSWPRINTF 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if you have the `wprintf' function. */
#define HAVE_WPRINTF 1

/* Define whether yylex_destroy is supported in flex version */
#define HAVE_YYLEX_DESTROY 1

/* Define to 1 if you have the <zlib.h> header file. */
#define HAVE_ZLIB_H 1

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Define if the compiler understands __FUNCTION__. */
#define HAVE___FUNCTION__ 1

/* Define if the compiler understands __func__. */
/* #undef HAVE___func__ */

/* Define if HP-UX 10 or 11 */
/* #undef HPUX */

/* For INADDR_NONE definition */
/* #undef INADDR_NONE */

/* Define if Irix 6 */
/* #undef IRIX */

/* Define if Linux */
/* #undef LINUX */

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Define if MacOS */
#define MACOS 1

/* Define if OpenBSD < 2.3 */
/* #undef OPENBSD */

/* Define if Tru64 */
/* #undef OSF1 */

/* Name of package */
#define PACKAGE "snort"

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

/* Set by user */
/* #undef SIGNAL_SNORT_DUMP_STATS */

/* Set by user */
/* #undef SIGNAL_SNORT_READ_ATTR_TBL */

/* Set by user */
/* #undef SIGNAL_SNORT_RELOAD */

/* Set by user */
/* #undef SIGNAL_SNORT_ROTATE_STATS */

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long int', as computed by sizeof. */
#define SIZEOF_LONG_INT 4

/* The size of `long long int', as computed by sizeof. */
#define SIZEOF_LONG_LONG_INT 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of `unsigned long int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_INT 4

/* The size of `unsigned long long int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG_INT 8

/* Define if Solaris */
/* #undef SOLARIS */

/* For sparc v9 with %time register */
/* #undef SPARCV9 */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define if SunOS */
/* #undef SUNOS */

/* Version number of package */
#define VERSION "2.9.7.6"

/* Define if words are big endian */
/* #undef WORDS_BIGENDIAN */

/* Define if words must align */
/* #undef WORDS_MUSTALIGN */

/* Define __FUNCTION__ as required. */
/* #undef __FUNCTION__ */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif
