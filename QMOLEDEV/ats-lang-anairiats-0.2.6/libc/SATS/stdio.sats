(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)
//
// This is essentially the first example of its kind:
// building API for C functions
//
(* ****** ****** *)
//
// HX-2010-07-13:
// There are really two versions here: version-0 and version-1; the former
// is unsafe and prone to resource-leaks; it should be avoided once the programmer
// becomes comfortable with linear types in ATS.
//
// HX-2010-10-03:
// This design is overly complicated mostly for historic reasons; 
// it is _not_ suggested that this one be used as an example for learning how to
// set up interfaces for C functions
//
(* ****** ****** *)

%{#
#include "libc/CATS/stdio.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"
typedef whence_t = $TYPES.whence_t
macdef SEEK_SET = $TYPES.SEEK_SET
macdef SEEK_CUR = $TYPES.SEEK_CUR
macdef SEEK_END = $TYPES.SEEK_END

(* ****** ****** *)

sortdef fm = file_mode
typedef bytes (n:int) = @[byte][n]
typedef b0ytes (n:int) = @[byte?][n]
viewdef FILE_v (m:fm, l:addr) = FILE m @ l
viewdef FILEopt_v (m:fm, l:addr) = option_v (FILE m @ l, l > null)

(* ****** ****** *)

praxi stdin_isnot_null : [stdin_addr > null] void
praxi stdout_isnot_null : [stdout_addr > null] void
praxi stderr_isnot_null : [stderr_addr > null] void

// ------------------------------------------------

macdef EOF = $extval (int, "EOF") // HX: it must be -1

// ------------------------------------------------

(*

// void clearerr (FILE *stream);

The function [clearerr] clears the end-of-file and error indicators for
the stream pointed to by stream.

*)
symintr clearerr
fun clearerr0 (f: FILEref):<> void = "mac#atslib_clearerr"
overload clearerr with clearerr0
fun clearerr1 {m:fm} (f: &FILE m):<> void = "mac#atslib_clearerr"
overload clearerr with clearerr1

// ------------------------------------------------

(*

// int fclose (FILE *stream);

The [fclose] function will flush the stream pointed to by fp (writing any
buffered output data using [fflush] and close the underlying file
descriptor. The behaviour of [fclose] is undefined if the stream parameter
is an illegal pointer, or is a descriptor already passed to a previous
invocation of [fclose].

Upon successful completion 0 is returned.  Otherwise, EOF is returned and
the global variable errno is set to indicate the error.  In either case any
further access (including another call to fclose()) to the stream results
in undefined behaviour.

*)

symintr fclose_err

fun fclose0_err
  (r: FILEref):<> int = "mac#atslib_fclose_err"
overload fclose_err with fclose0_err
fun fclose1_err
  {m:fm} {l:addr} (
  pf: !FILE_v (m, l) >> option_v (FILE_v (m, l), i < 0) | p: ptr l
) :<> #[i:int | i <= 0] int i
  = "mac#atslib_fclose_err"
overload fclose_err with fclose1_err

symintr fclose_exn

fun fclose0_exn
  (r: FILEref):<!exn> void = "atslib_fclose_exn"
overload fclose_exn with fclose0_exn
fun fclose1_exn
  {m:fm} {l:addr}
  (pf: FILE m @ l | p: ptr l):<!exn> void
  = "atslib_fclose_exn"
overload fclose_exn with fclose1_exn

// ------------------------------------------------

//
// HX-2010-10-02:
// This one ignores all errors except EINTR, which forces
// a retry.
//
fun fclose1_loop // implemented in $ATSHOME/libc/DATS/stdio.dats
  {m:fm} {l:addr} (pf: FILE m @ l | p: ptr l):<> int // 0/neg : succ/fail
// end of [fclose1_loop]

// ------------------------------------------------

fun fclose_stdin ():<!exn> void = "atslib_fclose_stdin"
fun fclose_stdout ():<!exn> void = "atslib_fclose_stdout"
fun fclose_stderr ():<!exn> void = "atslib_fclose_stderr"

// ------------------------------------------------

(*  
//
// int feof (FILE *stream);
//
The function feof() returns a nonzero value if the end of the given file
stream has been reached.
//
*)

symintr feof
fun feof0 (f: FILEref):<> int = "atslib_feof"
overload feof with feof0
fun feof1 {m:fm} (f: &FILE m):<> int = "atslib_feof"
overload feof with feof1

// ------------------------------------------------

(*
//
// int ferror (FILE *stream);
//
The function [ferror] tests the error indicator for the stream pointed to by
stream, returning non-zero if it is set.  The error indicator can only be
reset by the [clearerr] function.
//
*)

symintr ferror
fun ferror0 (f: FILEref):<> int = "atslib_ferror"
overload ferror with ferror0
fun ferror1 {m:fm} (f: &FILE m):<> int = "atslib_ferror"
overload ferror with ferror1

// ------------------------------------------------

(*
//
// int fflush (FILE *stream);
//
The function fflush forces a write of all user-space buffered data for the
given output or update stream via the streams underlying write function.
The open status of the stream is unaffected.
//
Upon successful completion 0 is returned.  Otherwise, EOF is returned and
the global variable errno is set to indicate the error.
//
*)

symintr fflush_err
fun fflush0_err (f: FILEref):<> int = "mac#atslib_fflush_err"
overload fflush_err with fflush0_err
fun fflush1_err {m:fm} (
  pf: file_mode_lte (m, w) | f: &FILE m
) :<> [i:int | i <= 0] int (i) = "mac#atslib_fflush_err"
overload fflush_err with fflush1_err

//

symintr fflush_exn
fun fflush0_exn
  (f: FILEref):<!exn> void = "atslib_fflush_exn"
overload fflush_exn with fflush0_exn
fun fflush1_exn {m:fm}
  (pf: file_mode_lte (m, w) | f: &FILE m):<!exn> void
  = "atslib_fflush_exn"
overload fflush_exn with fflush1_exn

fun fflush_stdout ():<!exn> void = "atslib_fflush_stdout"

// ------------------------------------------------

(*
//
// int fgetc (FILE *stream)
//
[fgetc] reads the next character from stream and returns it as an
unsigned char cast to an int, or EOF on end of file or error.
//
*)

symintr fgetc_err
fun fgetc0_err
  (f: FILEref):<> int = "mac#atslib_fgetc_err"
overload fgetc_err with fgetc0_err
fun fgetc1_err // [EOF] must be a negative number!
  {m:fm} (
  pf: file_mode_lte (m, r) | f: &FILE m
) :<> [i:int | i <= UCHAR_MAX] int i
  = "mac#atslib_fgetc_err"
overload fgetc_err with fgetc1_err

// ------------------------------------------------

(*
//
// char *fgets (char *str, int size, FILE *stream);
//
[fgets] reads in at most one less than [size] characters from stream and
stores them into the buffer pointed to by s.  Reading stops after an EOF or
a newline.  If a newline is read, it is stored into the buffer.  A '\0' is
stored after the last character in the buffer.

*)

dataview
fgets_v (sz:int, n0: int, addr, addr) =
  | {l_buf:addr}
    fgets_v_fail (sz, n0, l_buf, null) of b0ytes (sz) @ l_buf
  | {n:nat | n < n0} {l_buf:addr | l_buf > null}
    fgets_v_succ (sz, n0, l_buf, l_buf) of strbuf (sz, n) @ l_buf
// end of [fgets_v]

fun fgets_err
  {sz,n0:int | 0 < n0; n0 <= sz}
  {m:fm} {l_buf:addr} (
    pf_mod: file_mode_lte (m, r)
  , pf_buf: b0ytes (sz) @ l_buf
  | p: ptr l_buf, n0: int n0, f: &FILE m
  ) :<> [l:addr] (fgets_v (sz, n0, l_buf, l) | ptr l)
  = "mac#atslib_fgets_err"
// end of [fgets_err]

//
// HX:
// this function returns an empty strbuf
// if EOF is reached but no character is read
//
fun fgets_exn
  {sz,n0:int | 0 < n0; n0 <= sz}
  {m:fm} {l_buf:addr} (
    pf_mod: file_mode_lte (m, r),
    pf_buf: !b0ytes (sz) @ l_buf >>
     [n:nat | n < n0] strbuf (sz, n) @ l_buf
  | p: ptr l_buf, n0: int n0, f: &FILE m
  ) :<!exn> void = "atslib_fgets_exn"
// end of [fgets_exn]

// ------------------------------------------------

(*
//
// int fgetpos(FILE *stream, fpos_t *pos);
//
The [fgetpos] function stores the file position indicator of the given file
stream in the given position variable. The position variable is of type
fpos_t (which is defined in stdio.h) and is an object that can hold every
possible position in a FILE. [fgetpos] returns zero upon success, and a
non-zero value upon failure.
//
*)

abst@ype
fpos_t = $extype"ats_fpos_type"

fun fgetpos0_err (
  fil: FILEref, pos: &fpos_t? >> fpos_t
) :<> [i:int | i <= 0] int (i) = "mac#atslib_fgetpos"
// end of [fgetpos0_err]
fun fgetpos1_err {m:fm} (
  fil: &FILE m, pos: &fpos_t? >> opt (fpos_t, i==0)
) :<> #[i:int | i <= 0] int (i) = "mac#atslib_fgetpos"
// end of [fgetpos1_err]

// ------------------------------------------------

(*
//
// int fileno (FILE* filp) ;
// 
The function fileno examines the argument stream and returns its integer
descriptor. In case fileno detects that its argument is not a valid stream,
it must return -1 and set errno to EBADF.
*)

symintr fileno
fun fileno0_err (f: FILEref):<> int = "mac#atslib_fileno"
overload fileno with fileno0_err
fun fileno1_err {m:fm} (f: &FILE m):<> int = "mac#atslib_fileno"
overload fileno with fileno1_err

// ------------------------------------------------

(*
//
// FILE *fopen (const char *path, const char *mode);
//
The fopen function opens the file whose name is the string pointed to by
path and associates a stream with it.

The argument mode points to a string beginning with one of the follow
ing sequences (Additional characters may follow these sequences.):

  r      Open  text  file  for  reading.  The stream is positioned at the
         beginning of the file.

  r+     Open for reading and writing.  The stream is positioned  at  the
         beginning of the file.

  w      Truncate  file  to  zero length or create text file for writing.
         The stream is positioned at the beginning of the file.

  w+     Open for reading and writing.  The file is created  if  it  does
         not  exist, otherwise it is truncated.  The stream is positioned
         at the beginning of the file.


  a      Open for appending (writing at end of file).  The file is created
         if it does not exist.  The stream is positioned at the end of the
         file.

  a+     Open for reading and appending (writing at end  of  file).   The
         file  is created if it does not exist.  The stream is positioned
         at the end of the file.

*)

fun fopen_err {m:fm}
  (path: !READ(string), m: file_mode m)
  :<> [l:addr] (FILEopt_v (m, l) | ptr l) = "mac#atslib_fopen_err"
// end of [fopen_err]

fun fopen_exn {m:fm}
  (path: !READ(string), m: file_mode m)
  :<!exn> [l:addr] (FILE m @ l | ptr l) = "atslib_fopen_exn"
// end of [fopen_exn]

fun fopen_ref_exn {m:fm}
  (path: !READ(string), m: file_mode m):<!exn> FILEref = "atslib_fopen_exn"
// end of [fopen_ref_exn]

// ------------------------------------------------

(*

// int fputc (int c, FILE *stream)

The function [fputc] writes the given character [c] to the given output
stream. The return value is the character, unless there is an error, in
which case the return value is EOF.

*)

symintr fputc_err
fun fputc0_err (
  c: char, f: FILEref
) :<> int = "mac#atslib_fputc_err"
overload fputc_err with fputc0_err
fun fputc1_err {m:fm}
  (pf: file_mode_lte (m, w) | c: char, f: &FILE m)
  :<> [i:int | i <= UCHAR_MAX] int i
  = "mac#atslib_fputc_err"
overload fputc_err with fputc1_err

symintr fputc_exn
fun fputc0_exn
  (c: char, f: FILEref):<!exn> void = "atslib_fputc_exn"
overload fputc_exn with fputc0_exn
fun fputc1_exn {m:fm}
  (pf: file_mode_lte (m, w) | c: char, f: &FILE m):<!exn> void
  = "atslib_fputc_exn"
overload fputc_exn with fputc1_exn

// ------------------------------------------------

(*

// int fputs (const char* s, FILE *stream)

The function [fputs] writes a string to a file. it returns a non-negative
number on success, or EOF on error.

*)

symintr fputs_err
fun fputs0_err (
  str: !READ(string), fil: FILEref
) :<> int = "mac#atslib_fputs_err"
overload fputs_err with fputs0_err
fun fputs1_err {m:fm}
  (pf: file_mode_lte (m, w) | str: !READ(string), f: &FILE m):<> int
  = "mac#atslib_fputs_err"
overload fputs_err with fputs1_err

symintr fputs_exn
fun fputs0_exn
  (str: !READ(string), fil: FILEref):<!exn> void = "atslib_fputs_exn"
overload fputs_exn with fputs0_exn
fun fputs1_exn {m:fm}
  (pf: file_mode_lte (m, w) | str: !READ(string), f: &FILE m):<!exn> void
  = "atslib_fputs_exn"
overload fputs_exn with fputs1_exn

// ------------------------------------------------

(*
//
// size_t fread (void *ptr, size_t size, size_t nmemb, FILE *stream);
//
The function [fread] reads [nmemb] elements of data, each [size] bytes
long, from the stream pointed to by stream, storing them at the location
given by ptr. The return value is the number of items that are actually
read.
//
[fread] does not distinguish between end-of-file and error, and callers
must use [feof] and [ferror] to determine which occurred.
//
*)

fun fread
  {sz:pos} {n_buf:int}
  {n,nsz:nat | nsz <= n_buf} {m:fm} (
    pf_mod: file_mode_lte (m, r)
  , pf_mul: MUL (n, sz, nsz)
  | buf: &bytes (n_buf)
  , sz: size_t sz, n: size_t n
  , f: &FILE m
  ) :<> sizeLte n = "mac#atslib_fread"
// end of [fread]

fun fread_byte
  {n_buf:int}
  {n:nat | n <= n_buf}
  {m:fm} (
  pf_mod: file_mode_lte (m, r) | buf: &bytes (n_buf), n: size_t n, f: &FILE m
) :<> sizeLte n = "atslib_fread_byte"
// end of [fread_byte]

fun fread_byte_exn
  {n_buf:int}
  {n:nat | n <= n_buf}
  {m:fm} (
  pf_mod: file_mode_lte (m, r) | buf: &bytes (n_buf), n: size_t n, f: &FILE m
) :<!exn> void = "atslib_fread_byte_exn"
// end of [fread_byte_exn]

// ------------------------------------------------

(*
//
// FILE *freopen (const char *path, const char *mode, FILE *stream);
//
The [freopen] function opens the file whose name is the string pointed to by
path and associates the stream pointed to by stream with it.  The original
stream (if it exists) is closed.  The mode argument is used just as in the
fopen function.  The primary use of the freopen function is to change the
file associated with a standard text stream (stderr, stdin, or stdout).
//
*)

symintr freopen_err

fun freopen0_err {m_new:fm} (
  path: !READ(string), m_new: file_mode m_new, f: FILEref
) :<> void = "mac#atslib_freopen_err"
overload freopen_err with freopen0_err

fun freopen1_err
  {m_old,m_new:fm} {l0:addr} (
  pf: FILE m_old @ l0
| path: !READ(string), m: file_mode m_new, p: ptr l0
) :<> [l:addr | l==null || l == l0] (FILEopt_v (m_new, l) | ptr l)
  = "mac#atslib_freopen_err"
overload freopen_err with freopen1_err

symintr freopen_exn
fun freopen0_exn {m_new:fm} (
  path: !READ(string), m_new: file_mode m_new, f: FILEref
) :<!exn> void = "atslib_freopen_exn"
overload freopen_exn with freopen0_exn
fun freopen1_exn {m_old,m_new:fm} {l0:addr} (
  pf: FILE m_old @ l0
| path: !READ(string), m: file_mode m_new, p: ptr l0
) :<!exn> (FILE m_new @ l0 | void) = "atslib_freopen_exn"
overload freopen_exn with freopen1_exn

fun freopen_stdin
  (s: !READ(string)):<!exn> void = "atslib_freopen_stdin"
// end of [freopen_stdin]
fun freopen_stdout
  (s: !READ(string)):<!exn> void = "atslib_freopen_stdout"
// end of [freopen_stdout]
fun freopen_stderr
  (s: !READ(string)):<!exn> void = "atslib_freopen_stderr"
// end of [freopen_stderr]

// ------------------------------------------------

(*

// int fseek (FILE *stream, long offset, int whence)

The [fseek] function sets the file position indicator for the stream
pointed to by stream.  The new position, measured in bytes, is obtained by
adding offset bytes to the position specified by whence.  If whence is set
to [SEEK_SET], [SEEK_CUR], or [SEEK_END], the offset is relative to the
start of the file, the current position indicator, or end-of-file,
respectively.  A successful call to the [fseek] function clears the end-
of-file indicator for the stream and undoes any effects of the [ungetc]
function on the same stream. Upon success, [fseek] returns 0. Otherwise,
it returns -1.

*)

symintr fseek_err
fun fseek0_err (
  f: FILEref, offset: lint, whence: whence_t
) :<> int = "mac#atslib_fseek_err"
overload fseek_err with fseek0_err
fun fseek1_err {m:fm} (
  f: &FILE m, offset: lint, whence: whence_t
) :<> int = "mac#atslib_fseek_err"
overload fseek_err with fseek1_err

symintr fseek_exn
fun fseek0_exn
  (f: FILEref, offset: lint, whence: whence_t):<!exn> void = "atslib_fseek_exn"
overload fseek_exn with fseek0_exn
fun fseek1_exn {m:fm}
  (f: &FILE m, offset: lint, whence: whence_t):<!exn> void = "atslib_fseek_exn"
overload fseek_exn with fseek1_exn

// ------------------------------------------------

(*

// void fsetpos(FILE *stream, const fpos_t *pos);

The [fsetpos] function moves the file position indicator for the given
stream to a location specified by the position object. The type fpos_t is
defined in stdio.h.  The return value for fsetpos() is zero upon success,
non-zero on failure.

*)

fun fsetpos {m:fm}
  (f: &FILE m, pos: &fpos_t): int = "mac#atslib_fsetpos"
// end of [fsetpos]

// ------------------------------------------------

(*
//
// long ftell (FILE *stream)
//
[ftell] returns the current offset of the given file stream upon on
success. Otherwise, -1 is returned and the global variable errno is set to
indicate the error.
//
*)

symintr ftell_err
fun ftell0_err (f: FILEref):<> lint = "mac#atslib_ftell_err"
overload ftell_err with ftell0_err
fun ftell1_err {m:fm} (f: &FILE m):<> lint = "mac#atslib_ftell_err"
overload ftell_err with ftell1_err

symintr ftell_exn
fun ftell0_exn (f: FILEref):<!exn> lint = "atslib_ftell_exn"
overload ftell_exn with ftell0_exn
fun ftell1_exn {m:fm} (f: &FILE m):<!exn> lint = "atslib_ftell_exn"
overload ftell_exn with ftell1_exn

// ------------------------------------------------

(*
//
// size_t fwrite (const void *ptr,  size_t size,  size_t nmemb, FILE *stream);
//
The function [fwrite] writes [nmemb] elements of data, each [size] bytes
long, to the stream pointed to by stream, obtaining them from the location
given by [ptr]. The return value is the number of items that are actually
written.
//
*)

fun fwrite // [sz]: the size of each item
  {sz:pos} {bsz:int} {n,nsz:nat | nsz <= bsz} {m:fm} (
  pf_mod: file_mode_lte (m, w), pf_mul: MUL (n, sz, nsz)
| buf: &bytes (bsz), sz: size_t sz, n: size_t n, fil: &FILE m
) :<> natLte n
  = "mac#atslib_fwrite"
//
// HX: [fwrite_byte] is a special case of [fwrite]
//
fun fwrite_byte // [fwrite_byte] only writes once
  {bsz:int} {n:nat | n <= bsz} {m:fm} (
  pf_mod: file_mode_lte (m, w) | buf: &bytes (bsz), n: size_t n, fil: &FILE m
) :<> sizeLte n
  = "atslib_fwrite_byte"
//
// HX: an uncatchable exception is thrown if not all bytes are written
//
fun fwrite_byte_exn
  {bsz:int} {n:nat | n <= bsz} {m:fm} (
  pf_mod: file_mode_lte (m, w) | buf: &bytes (bsz), n: size_t n, fil: &FILE m
) :<!exn> void
  = "atslib_fwrite_byte_exn"

// ------------------------------------------------

(*
//
// perror - print a system error message
//
The routine [perror(s)] produces a message on the standard error output,
describing the last error encountered during a call to a system or library
function.  First (if s is not NULL and *s is not NULL) the argument string
s is printed, followed by a colon and a blank.  Then the message and a
newline.
//
*)

fun perror
  (msg: !READ(string)):<> void = "atslib_perror"
// end of [perror]

// ------------------------------------------------

macdef getc = fgetc_err
macdef putc = fputc_err

// ------------------------------------------------

fun getchar ():<> int = "atslib_getchar"
fun getchar1
  () :<> [i:int | i <= UCHAR_MAX] int i = "atslib_getchar"
// end of [getchar1]

fun putchar (c: char):<> int = "atslib_putchar"
fun putchar1
  (c: char):<> [i:int | i <= UCHAR_MAX] int i = "atslib_putchar"
// end of [putchar1]

// ------------------------------------------------

// [puts] puts a newline at the end
fun puts_err
  (inp: !READ(string)):<> int = "atslib_puts_err"
fun puts_exn
  (inp: !READ(string)):<!exn> void = "atslib_puts_exn"

// ------------------------------------------------

fun remove_err
  (inp: !READ(string)):<> int = "mac#atslib_remove_err"
fun remove_exn
  (inp: !READ(string)):<!exn> void = "atslib_remove_exn"

// ------------------------------------------------

fun rename_err (
  oldpath: !READ(string), newpath: !READ(string)
) :<> int = "mac#atslib_rename_err"

fun rename_exn (
  oldpath: !READ(string), newpath: !READ(string)
) :<!exn> void = "atslib_rename_exn"

// ------------------------------------------------

//
// HX: [rewind] generates no error
//
symintr rewind
fun rewind0 {m:fm}
  (fil: FILEref):<> void = "mac#atslib_rewind"
overload rewind with rewind0
fun rewind1 {m:fm} (fil: &FILE m):<> void = "atslib_rewind"
overload rewind with rewind1

// ------------------------------------------------

fun tmpfile_err (
) :<> [l:agez] (FILEopt_v (rw, l) | ptr l)
  = "mac#atslib_tmpfile_err"
fun tmpfile_exn (
) :<!exn> [l:addr] (FILE_v (rw, l) | ptr l)
  = "atslib_tmpfile_exn"
fun tmpfile_ref_exn ():<!exn> FILEref = "atslib_tmpfile_exn"

// ------------------------------------------------

(*
//
// int ungetc(int c, FILE *stream);
//
[ungetc] pushes [c] back to stream, cast to unsigned char, where it is
available for subsequent read operations.  Pushed-back characters will be
returned in reverse order; only one pushback is guaranteed.
//
*)

symintr ungetc_err
fun ungetc0_err
  (c: char, f: FILEref):<> int = "mac#atslib_ungetc_err"
overload ungetc_err with ungetc0_err
fun ungetc1_err {m:fm}
  (c: char, f: &FILE m):<> [i:int | i <= UCHAR_MAX] int i
  = "mac#atslib_ungetc_err"
overload ungetc_err with ungetc1_err

symintr ungetc_exn
fun ungetc0_exn (c: char, f: FILEref):<!exn> void
  = "atslib_ungetc_exn"
overload ungetc_exn with ungetc0_exn
fun ungetc1_exn {m:fm} (c: char, f: &FILE m):<!exn> void
  = "atslib_ungetc_exn"
overload ungetc_exn with ungetc1_exn

// ------------------------------------------------

sta BUFSIZ : int
praxi BUFSIZ_gtez (): [BUFSIZ >= 0] void
macdef BUFSIZ = $extval (int(BUFSIZ), "BUFSIZ")

abst@ype bufmode_t = int
macdef _IOFBF = $extval (bufmode_t, "_IOFBF") // fully buffered
macdef _IOLBF = $extval (bufmode_t, "_IOLBF") // line buffered
macdef _IONBF = $extval (bufmode_t, "_IONBF") // no buffering

symintr setbuf_null
fun setbuf0_null (f: FILEref): void = "mac#atslib_setbuf_null"
overload setbuf_null with setbuf0_null
fun setbuf1_null {m:fm} (f: &FILE m): void = "mac#atslib_setbuf_null"
overload setbuf_null with setbuf1_null

//
// HX-2010-10-03:
// the buffer can be freed only after it is no longer used by
// the stream to which it is attached!!!
//
symintr setbuffer
fun setbuffer0 {n1,n2:nat | n2 <= n1} {l:addr}
  (pf_buf: !b0ytes n1 @ l | f: FILEref, p_buf: ptr l, n2: size_t n2): void
  = "mac#atslib_setbuffer"
overload setbuffer with setbuffer0
fun setbuffer1 {m:fm} {n1,n2:nat | n2 <= n1} {l:addr}
  (pf_buf: !b0ytes n1 @ l | f: &FILE m, p_buf: ptr l, n2: size_t n2): void
  = "mac#atslib_setbuffer"
overload setbuffer with setbuffer1

symintr setlinebuf
fun setlinebuf0 (f: FILEref): void = "mac#atslib_setlinebuf"
overload setlinebuf with setlinebuf0
fun setlinebuf1 {m:fm} (f: &FILE m): void = "mac#atslib_setlinebuf"
overload setlinebuf with setlinebuf1

symintr setvbuf_null
fun setvbuf0_null
  (f: FILEref, mode: bufmode_t): int = "mac#atslib_setvbuf_null"
overload setvbuf_null with setvbuf0_null
fun setvbuf1_null {m:fm}
  (f: &FILE m, mode: bufmode_t): int = "mac#atslib_setvbuf_null"
overload setvbuf_null with setvbuf1_null

symintr setvbuf
fun setvbuf0 {n1,n2:nat | n2 <= n1} {l:addr}
  (pf_buf: !b0ytes(n1) @ l | fil: FILEref, mode: bufmode_t, n2: size_t n2): int
  = "mac#ats_setvbuf"
overload setvbuf with setvbuf0
fun setvbuf1 {m:fm} {n1,n2:nat | n2 <= n1} {l:addr}
  (pf_buf: !b0ytes(n1) @ l | fil: &FILE m, mode: bufmode_t, n2: size_t n2): int
  = "mac#ats_setvbuf"
overload setvbuf with setvbuf1

// ------------------------------------------------

(* end of [stdio.sats] *)
