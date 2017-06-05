/* UNARJ.H, UNARJ, R JUNG, 07/29/96
 * Include file
 * Copyright (c) 1990-2002 ARJ Software, Inc.  All rights reserved.
 *
 *   This code may be freely used in programs that are NOT ARJ archivers
 *   (both compress and extract ARJ archives).
 *
 *   If you wish to distribute a modified version of this program, you
 *   MUST indicate that it is a modified version both in the program and
 *   source code.
 *
 *   If you modify this program, we would appreciate a copy of the new
 *   source code.  we are holding the copyright on the source code, so
 *   please do not delete our name from the program files or from the
 *   documentation.
 *
 * Modification history:
 * Date      Programmer  Description of modification.
 * 04/05/91  R. Jung     Rewrote code.
 * 04/23/91  M. Adler    Portabilized.
 * 04/29/91  R. Jung     Added volume label support.
 * 05/30/91  R. Jung     Added SEEK_END definition.
 * 06/03/91  R. Jung     Changed arguments in get_mode_str() and
 *                       set_ftime_mode().
 * 06/28/91  R. Jung     Added new HOST OS numbers.
 * 07/08/91  R. Jung     Added default_case_path() and strlower().
 * 07/21/91  R. Jung     Fixed #endif _QC comment.
 * 08/27/91  R. Jung     Added #ifdef for COHERENT.
 * 09/01/91  R. Jung     Added new host names.
 * 12/03/91  R. Jung     Added BACKUP_FLAG.
 * 04/06/92  R. Jung     Added ARCHIMEDES.
 * 02/17/93  R. Jung     Improved ARJ header information.  Added ARJ_M_VERSION.
 * 01/22/94  R. Jung     Changed copyright message.
 * 07/29/96  R. Jung     Added "/" to list of path separators.
 *
 */

#ifndef _ARH_DEF_
#define _ARH_DEF_

#define MODERN

// Use prototypes and ANSI libraries if __STDC__ */
#ifdef __STDC__
#  define MODERN
#endif // __STDC__ */

// Do not use prototypes for COHERENT */
#ifdef COHERENT
#  undef  MODERN
#  define KEEP_WINDOW
#endif // COHERENT */

// Use prototypes and ANSI libraries if __TURBOC__ */
#ifdef __TURBOC__
#  define MODERN
#endif // __TURBOC__ */

// Use prototypes and ANSI libraries if _QC */
#ifdef _QC
#  define MODERN
#endif // _QC */

// Use prototypes and ANSI libraries if _OS2 */
#ifdef _OS2
#  define MODERN
#endif // _OS2 */

// Used to remove arguments in function prototypes for non-ANSI C */
#ifdef MODERN
#  define OF(a) a
#else // !MODERN */
#  define OF(a) ()
#endif // ?MODERN */

#ifdef MODERN
   typedef void voidp;
#else // !MODERN */
#  define void int
   typedef char voidp;
#endif // ?MODERN */

#include <stdio.h>
#ifdef MODERN
#  include <limits.h>
#else // !MODERN */
#  ifndef UCHAR_MAX
#    define UCHAR_MAX (255)
#  endif
#  ifndef CHAR_BIT
#    define CHAR_BIT  (8)
#  endif
#  ifndef LONG_MAX
#    define LONG_MAX  (0x7FFFFFFFL)
#  endif
#endif // ?MODERN */

#ifndef SEEK_SET
#  define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#  define SEEK_CUR 1
#endif

#ifndef SEEK_END
#  define SEEK_END 2
#endif

typedef unsigned char  uchar;   //  8 bits or more */
typedef unsigned int   uint;    // 16 - 32 bits or more */
typedef unsigned short ushort;  // 16 bits or more */
typedef unsigned long  ulong;   // 32 bits or more */

#define USHRT_BIT   (CHAR_BIT * sizeof(ushort))

// ********************************************************* */
// Environment definitions (implementation dependent)        */
// ********************************************************* */

#ifdef _QC
#define __MSDOS__
#endif

#ifdef __MSDOS__
#define OS                  0
#define PATH_SEPARATORS     "\\:/"
#define PATH_CHAR           '\\'
#define MAXSFX              25000L
#define ARJ_SUFFIX          ".ARJ"
#endif

#ifdef _OS2
#define OS                  0           // ??? */
#define PATH_SEPARATORS     "\\:"
#define PATH_CHAR           '\\'
#define SWITCH_CHARS        "-/"
#define MAXSFX              25000L
#define ARJ_SUFFIX          ".ARJ"
#endif

#ifdef __CI
#define PRIME               1
#define OS                  1
#define PATH_SEPARATORS     ">"
#define PATH_CHAR           '>'
#define FIX_PARITY(c)       c |= ~ASCII_MASK
#define DEFAULT_DIR         "*>"
#define ARJ_SUFFIX          ".ARJ"
#endif

// Error levels */

#ifndef ERROR_DEFINES

#define ERR_OK        0       // success */
#define ERR_WARN      1       // minor problem (file not found) */
#define ERR_FAIL      2       // fatal error */
#define ERR_CRC       3       // CRC error */
#define ERR_SECURE    4       // ARJ security invalid or not found */
#define ERR_WRITE     5       // disk full */
#define ERR_OPEN      6       // can't open file */
#define ERR_USER      7       // user specified bad parameters */
#define ERR_MEMORY    8       // not enough memory */

#endif

#ifndef MAXSFX              // size of self-extracting prefix */
#define MAXSFX              500000L
#endif
#ifndef FNAME_MAX
#define FNAME_MAX           512
#endif
#ifndef SWITCH_CHARS
#define SWITCH_CHARS        "-"
#endif
#ifndef FIX_PARITY
#define FIX_PARITY(c)       c &= ASCII_MASK
#endif
#ifndef ARJ_SUFFIX
#define ARJ_SUFFIX          ".arj"
#endif
#ifndef ARJ_DOT
#define ARJ_DOT             '.'
#endif
#ifndef DEFAULT_DIR
#define DEFAULT_DIR         ""
#endif
#ifndef OS
#define OS                  2
#endif
#ifndef PATH_SEPARATORS
#define PATH_SEPARATORS     "/"
#endif
#ifndef PATH_CHAR
#define PATH_CHAR           '/'
#endif

// ********************************************************* */
// end of environmental defines                              */
// ********************************************************* */

// ********************************************************* */
//
// Structure of archive main header (low order byte first):
//
//  2  header id (comment and local file) = 0x60, 0xEA
//  2  basic header size (from 'first_hdr_size' thru 'comment' below)
//	     = first_hdr_size + strlen(filename) + 1 + strlen(comment) + 1
//	     = 0 if end of archive
//
//  1  first_hdr_size (size up to 'extra data')
//  1  archiver version number
//  1  minimum archiver version to extract
//  1  host OS	 (0 = MSDOS, 1 = PRIMOS, 2 = UNIX, 3 = AMIGA, 4 = MACDOS)
//               (5 = OS/2, 6 = APPLE GS, 7 = ATARI ST, 8 = NEXT)
//               (9 = VAX VMS)
//  1  arj flags (0x01 = GARBLED_FLAG, 0x02 = OLD_SECURED_FLAG)
//               (0x04 = VOLUME_FLAG,  0x08 = EXTFILE_FLAG)
//               (0x10 = PATHSYM_FLAG, 0x20 = BACKUP_FLAG)
//               (0x40 = SECURED_FLAG)
//  1  arj security version (2 = current)
//  1  file type            (2 = comment header)
//  1  ?                   ]
//  4  date time stamp created
//  4  date time stamp modified
//  4  archive size up to the end of archive marker
//  4  file position of security envelope data
//  2  entryname position in filename
//  2  length in bytes of trailing security data
//  2  host data
//  ?  extra data
//
//  ?  archive filename (null-terminated)
//  ?  archive comment  (null-terminated)
//
//  4  basic header CRC
//
//  2  1st extended header size (0 if none)
//  ?  1st extended header
//  4  1st extended header's CRC
//  ...
//
//
// Structure of archive file header (low order byte first):
//
//  2  header id (comment and local file) = 0x60, 0xEA
//  2  basic header size (from 'first_hdr_size' thru 'comment' below)
//	     = first_hdr_size + strlen(filename) + 1 + strlen(comment) + 1
//	     = 0 if end of archive
//
//  1  first_hdr_size (size up to 'extra data')
//  1  archiver version number
//  1  minimum archiver version to extract
//  1  host OS	 (0 = MSDOS, 1 = PRIMOS, 2 = UNIX, 3 = AMIGA, 4 = MACDOS)
//               (5 = OS/2, 6 = APPLE GS, 7 = ATARI ST, 8 = NEXT)
//               (9 = VAX VMS)
//  1  arj flags (0x01 = GARBLED_FLAG, 0x02 = NOT USED)
//               (0x04 = VOLUME_FLAG,  0x08 = EXTFILE_FLAG)
//               (0x10 = PATHSYM_FLAG, 0x20 = BACKUP_FLAG)
//               (0x40 = NOT USED)
//  1  method    (0 = stored, 1 = compressed most ... 4 compressed fastest)
//  1  file type (0 = binary, 1 = text, 2 = comment header, 3 = directory)
//               (4 = label)
//  1  garble password modifier
//  4  date time stamp modified
//  4  compressed size
//  4  original size
//  4  original file's CRC
//  2  entryname position in filename
//  2  file access mode
//  2  host data
//  ?  extra data
//     4 bytes for extended file position
//
//  ?  filename (null-terminated)
//  ?  comment	(null-terminated)
//
//  4  basic header CRC
//
//  2  1st extended header size (0 if none)
//  ?  1st extended header
//  4  1st extended header's CRC
//  ...
//  ?  compressed file
//
// ********************************************************* */
// ********************************************************* */
//                                                           */
//     Time stamp format:                                    */
//                                                           */
//      31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16      */
//     |<---- year-1980 --->|<- month ->|<--- day ---->|     */
//                                                           */
//      15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0      */
//     |<--- hour --->|<---- minute --->|<- second/2 ->|     */
//                                                           */
// ********************************************************* */

#define CODE_BIT          16

#define NULL_CHAR       '\0'
#define MAXMETHOD          4

#define ARJ_VERSION        3
#define ARJ_M_VERSION      6    // ARJ version that supports modified date. */
#define ARJ_X_VERSION      3    // decoder version */
#define ARJ_X1_VERSION     1
#define DEFAULT_METHOD     1
#define DEFAULT_TYPE       0    // if type_sw is selected */
#define HEADER_ID     0xEA60
#define HEADER_ID_HI    0xEA
#define HEADER_ID_LO    0x60
#define FIRST_HDR_SIZE    30
#define FIRST_HDR_SIZE_V  34
#define COMMENT_MAX     2048
#define HEADERSIZE_MAX   (FIRST_HDR_SIZE + 10 + FNAME_MAX + COMMENT_MAX)
#define BINARY_TYPE        0    // This must line up with binary/text strings */
#define TEXT_TYPE          1
#define COMMENT_TYPE       2
#define DIR_TYPE           3
#define LABEL_TYPE         4

#define GARBLE_FLAG     0x01
#define VOLUME_FLAG     0x04
#define EXTFILE_FLAG    0x08
#define PATHSYM_FLAG    0x10
#define BACKUP_FLAG     0x20

typedef ulong UCRC;     // CRC-32 */

#define CRC_MASK        0xFFFFFFFFL

#define ARJ_PATH_CHAR   '/'

#define FA_RDONLY       0x01            // Read only attribute */
#define FA_HIDDEN       0x02            // Hidden file */
#define FA_SYSTEM       0x04            // System file */
#define FA_LABEL        0x08            // Volume label */
#define FA_DIREC        0x10            // Directory */
#define FA_ARCH         0x20            // Archive */

#define HOST_OS_NAMES1 "MS-DOS","PRIMOS","UNIX","AMIGA","MAC-OS","OS/2"
#define HOST_OS_NAMES2 "APPLE GS","ATARI ST","NEXT","VAX VMS"
#define HOST_OS_NAMES  { HOST_OS_NAMES1, HOST_OS_NAMES2, NULL }

// Timestamp macros */

#define get_tx(m,d,h,n) (((ulong)m<<21)+((ulong)d<<16)+((ulong)h<<11)+(n<<5))
#define get_tstamp(y,m,d,h,n,s) ((((ulong)(y-1980))<<25)+get_tx(m,d,h,n)+(s/2))

#define ts_year(ts)  ((uint)((ts >> 25) & 0x7f) + 1980)
#define ts_month(ts) ((uint)(ts >> 21) & 0x0f)      // 1..12 means Jan..Dec */
#define ts_day(ts)   ((uint)(ts >> 16) & 0x1f)      // 1..31 means 1st..31st */
#define ts_hour(ts)  ((uint)(ts >> 11) & 0x1f)
#define ts_min(ts)   ((uint)(ts >> 5) & 0x3f)
#define ts_sec(ts)   ((uint)((ts & 0x1f) * 2))

// Global functions */

// unarj.c */

// environ.c */

FILE   *file_open OF((char *name, char *mode));
int    file_read OF((char *buf, int size, int nitems, FILE *stream));
int    file_seek OF((FILE *stream, long offset, int mode));
long   file_tell OF((FILE *stream));
int    file_write OF((char *buf, int size, int nitems, FILE *stream));
voidp  *xmalloc OF((int size));
void   case_path OF((char *name));
void   default_case_path OF((char *name));
int    file_exists OF((char *name));
void   get_mode_str OF((char *str, uint fmode));
int    set_ftime_mode OF((char *name, ulong timestamp, uint fmode, uint host));

// decode.c */

#define THRESHOLD    3
#define DDICSIZ      26624
#define MAXDICBIT   16
#define MATCHBIT     8
#define MAXMATCH   256
#define NC          (UCHAR_MAX + MAXMATCH + 2 - THRESHOLD)
#define NP          (MAXDICBIT + 1)
#define CBIT         9
#define NT          (CODE_BIT + 3)
#define PBIT         5
#define TBIT         5

#if NT > NP
#define NPT NT
#else
#define NPT NP
#endif

#define CTABLESIZE  4096
#define PTABLESIZE   256

#define STRTP          9
#define STOPP         13

#define STRTL          0
#define STOPL          7


// Message strings */

extern char M_VERSION [];

extern char M_ARCDATE [];
extern char M_ARCDATEM[];
extern char M_BADCOMND[];
extern char M_BADCOMNT[];
extern char M_BADHEADR[];
extern char M_BADTABLE[];
extern char M_CANTOPEN[];
extern char M_CANTREAD[];
extern char M_CANTWRIT[];
extern char M_CRCERROR[];
extern char M_CRCOK   [];
extern char M_DIFFHOST[];
extern char M_ENCRYPT [];
extern char M_ERRORCNT[];
extern char M_EXTRACT [];
extern char M_FEXISTS [];
extern char M_HEADRCRC[];
extern char M_NBRFILES[];
extern char M_NOMEMORY[];
extern char M_NOTARJ  [];
extern char M_PROCARC [];
extern char M_SKIPPED [];
extern char M_SUFFIX  [];
extern char M_TESTING [];
extern char M_UNKNMETH[];
extern char M_UNKNTYPE[];
extern char M_UNKNVERS[];

#endif

// end UNARJ.H */
