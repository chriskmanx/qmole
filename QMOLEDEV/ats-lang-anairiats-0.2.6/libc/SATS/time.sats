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

%{#
#include "libc/CATS/time.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"

(* ****** ****** *)

typedef tm_struct =
  $extype_struct "ats_tm_struct_type" of {
  tm_sec= int (* seconds *)
, tm_min= int (* minutes *)
, tm_hour= int (* hours *)
, tm_mday= int (* day of the month *)
, tm_mon= int (* month *)
, tm_year= int (* year *)
, tm_wday= int (* day of the week *)
, tm_yday= int (* day in the year *)
, tm_isdst= int (* daylight saving time *)
} // end of [tm_struct]

(* ****** ****** *)

typedef time_t = $TYPES.time_t
//
// HX: these are implemented in libc/sys/CATS/types.cats
//
fun lint_of_time (t: time_t):<> lint = "atslib_lint_of_time"
overload lint_of with lint_of_time
fun double_of_time (t: time_t):<> double = "atslib_double_of_time"
overload double_of with double_of_time

(* ****** ****** *)

fun difftime (
  finish: time_t, start: time_t
) :<> double
  = "mac#atslib_difftime"
// end of [difftime]

(* ****** ****** *)

(*
** HX (2010-01-15):
** These functions are now kept for backward compatibility
*)
fun tm_get_sec
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_sec"
fun tm_get_min
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_min"
fun tm_get_hour
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_hour"
fun tm_get_mday
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_mday"
fun tm_get_mon
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_mon"
fun tm_get_year
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_year"
fun tm_get_wday
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_wday"
fun tm_get_yday
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_yday"
fun tm_get_isdst
  (tm: &READ(tm_struct)):<> int = "atslib_tm_get_isdst"

(* ****** ****** *)
//
symintr time
//
// HX: error-checking is not forced
//
fun time_get (): time_t = "atslib_time_get"
overload time with time_get
//
fun time_get_and_set // HX: error must be checked!
  (p: &time_t? >> opt (time_t, b)): #[b:bool] bool (b)
  = "atslib_time_get_and_set" // function!
overload time with time_get_and_set
//
(* ****** ****** *)
//
// HX: [ctime] is non-reentrant
// HX: the returned string ends with a newline.
//
fun ctime (
  t: &READ(time_t)
) :<!ref> [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "mac#atslib_ctime"
// end of [ctime]

//
// HX: reentrant version of [ctime]
//
 #define CTIME_BUFLEN 26
dataview ctime_v (m:int, addr, addr) =
  | {l:addr | l > null}
    ctime_v_succ (m, l, l) of strbuf (m, CTIME_BUFLEN - 1) @ l
  | {l:addr} ctime_v_fail (m, l, null) of b0ytes (m) @ l
fun ctime_r
  {m:int | m >= CTIME_BUFLEN}
  {l:addr} (
  pf: ! b0ytes (m) @ l >> ctime_v (m, l, l1)
| t: &READ(time_t), p_buf: ptr l
) :<> #[l1:addr] ptr l1
  = "mac#atslib_ctime_r"
// end of [ctime_r]

(* ****** ****** *)
//
// HX: [localtime] is non-reentrant
//
fun localtime (
  time: &READ(time_t)
) :<!ref>
  [l:addr] (
  vptroutopt (tm_struct, l)
| ptr l
) = "mac#atslib_localtime"
// end of [localtime]
//
// HX: [localtime_r] is reentrant
//
fun localtime_r (
  time: &READ(time_t)
, tm: &tm_struct? >> opt (tm_struct, l > null)
) :<> #[l:addr] ptr l
  = "mac#atslib_localtime_r"
// end of [localtime_r]

(* ****** ****** *)
//
// HX: [gmtime] is non-reentrant
//
fun gmtime (
  time: &READ(time_t)
) :<!ref>
  [l:addr] (
  vptroutopt (tm_struct, l)
| ptr l
) = "mac#atslib_gmtime"
// end of [gmtime]
//
// HX: [gmtime_r] is reentrant
//
fun gmtime_r (
  time: &READ(time_t), tm: &tm_struct? >> opt (tm_struct, l > null)
) :<> #[l:addr] ptr l = "mac#atslib_gmtime_r"
// end of [gmtime_r]

(* ****** ****** *)

fun mktime (
  tm: &READ(tm_struct)
) : time_t
  = "mac#atslib_mktime" // returns -1 on error
// end of [mktime]

(* ****** ****** *)

fun asctime (
  tm: &READ(tm_struct)
) :<!ref>
  [l:addr] (
  strptr l -<lin,prf> void
| strptr l
) = "mac#atslib_asctime"
// end of [asctime]

(* ****** ****** *)

fun strftime
  {m:pos} {l:addr} (
  pf: !b0ytes m @ l >> strbuf (m, n) @ l
| p: ptr l
, m: size_t m
, fmt: !READ(string)
, tm: &READ(tm_struct)
) :<> #[n:nat | n < m] size_t n
  = "mac#atslib_strftime" // this a macro!
// end of [strftime]

(* ****** ****** *)

(*
//
// HX-2010-09-26:
// the function is not in FreeBSD or Darwin;
// [getdate] sets [getdate_err] if an error occurs
//
fun getdate_err_get ():<> int = "atslib_getdate_err_get"
fun getdate_err_set (x: int):<> void = "atslib_getdate_err_set"
fun getdate (str: !READ(string)):<!ref>
  [l:addr] (ptroutopt (tm_struct, l) | ptr l) = "mac#atslib_getdate"
// end of [getdate]
*)

//
// -D_XOPEN_SOURCE
//
fun strptime (
  str: !READ(string), fmt: !READ(string)
, tm: &tm_struct? >> opt (tm_struct, l > null)
) : #[l:addr] ptr l
  = "mac#atslib_strptime" // HX: it returns NULL on error
// end of [strptime]

(* ****** ****** *)

(*
extern int daylight ; // not in FreeBSD or Darwin
extern long int timezone ; // not in FreeBSD or Darwin
extern char *tzname[2] ; // not in FreeBSD or Darwin
*)
fun tzsset ():<!ref> void = "mac#atslib_tzset"

(* ****** ****** *)

typedef clock_t = $TYPES.clock_t
macdef CLOCKS_PER_SEC = $extval (clock_t, "CLOCKS_PER_SEC")
//
// HX: these are implemented in libc/sys/CATS/types.cats
//
fun lint_of_clock (c: clock_t):<> lint = "atslib_lint_of_clock"
overload lint_of with lint_of_clock
fun double_of_clock (c: clock_t):<> double = "atslib_double_of_clock"
overload double_of with double_of_clock
//
fun clock (): clock_t = "mac#atslib_clock" // HX: it returns -1 on error

(* ****** ****** *)

typedef
timespec_struct =
$extype_struct
"ats_timespec_type" of {
  tv_sec= time_t // seconds
, tv_nsec= lint  // nanoseconds
} // end of [timespec_struct]
typedef timespec = timespec_struct

(* ****** ****** *)
//
// HX: 0/-1 : succ/fail // errno set to EINTR
//
fun nanosleep (
  nsec: &READ(timespec)
, rem: &timespec? >> opt (timespec, i==0)
) : #[i:int | i <= 0] int(i) = "mac#atslib_nanosleep"
// end of [nanosleep]

fun nanosleep_null
  (nsec: &READ(timespec)): int = "mac#atslib_nanosleep_null"
// end of [nanosleep]

(* ****** ****** *)

typedef clockid_t = $TYPES.clockid_t
macdef CLOCK_REALTIME = $extval (clockid_t, "CLOCK_REALTIME")
macdef CLOCK_MONOTONIC = $extval (clockid_t, "CLOCK_MONOTONIC")
(*
macdef CLOCK_THREAD_CPUTIME_ID = $extval (clockid_t, "CLOCK_THREAD_CPUTIME_ID")
macdef CLOCK_PROCESS_CPUTIME_ID = $extval (clockid_t, "CLOCK_PROCESS_CPUTIME_ID")
*)

(* ****** ****** *)
//
// HX: 0/-1 : succ/fail // errno set
//
fun clock_gettime (
  id: clockid_t
, tp: &timespec? >> opt (timespec, i==0)
) : #[i:int | i <= 0] int(i) = "mac#atslib_clock_gettime"
// end of [clock_gettime]
//
// HX: 0/-1 : succ/fail // errno set
//
fun clock_getres (
  id: clockid_t
, tp: &timespec? >> opt (timespec, i==0)
) : #[i:int | i <= 0] int(i) = "mac#atslib_clock_getres"
// end of [clock_getres]

// HX: superuser privilege is needed for this one
fun clock_settime ( // HX: 0/-1 : succ/fail // errno set
  id: clockid_t, tp: &READ(timespec)
) : int = "mac#atslib_clock_settime"
// end of [clock_settime]

(* ****** ****** *)

absview timer_v (i:int)
stadef timer_t = $TYPES.timer_t

(* ****** ****** *)

typedef
itimerspec_struct =
$extype_struct
"ats_itimerspec_type" of {
  it_interval= timespec (* reset value *)
, it_value= timespec    (* current value *)
} // end of [itimerspec_struct]
typedef itimerspec = itimerspec_struct

(* ****** ****** *)
//
// HX: 0/-1 : succ/fail // errno set
//
fun timer_create_null (
  cid: clockid_t
, tid: &timer_t? >> opt (timer_t(id), i==0)
) : #[i,id:int | i <= 0] (
  option_v (timer_v(id), i==0)
| int (i)
) = "mac#atslib_timer_create_null"
// end of [timer_create_null]

//
// HX: 0/-1 : succ/fail // errno set
//
fun timer_delete {id:int} (
  pf: !timer_v(id) >> option_v (timer_v(id), i < 0)
| tid: timer_t (id)
) : #[i:int | i <= 0] int (i)
  = "mac#atslib_timer_delete"
// end of [timer_delete]

(* ****** ****** *)
//
// HX: 0/-1 : succ/fail // errno set
//
fun timer_gettime {id:int} (
  pf: !timer_v (id)
| tid: timer_t (id)
, itp: &itimerspec? >> opt (itimerspec, i==0)
) : #[i: int | i <= 0] int (i)
  = "mac#atslib_timer_gettime"
// end of [timer_gettime]

fun timer_settime {id:int} (
  pf: !timer_v (id)
| tid: timer_t (id)
, newitp: &READ(itimerspec)
, olditp: &itimerspec? >> opt (itimerspec, i==0)
) : #[i: int | i <= 0] int (i)
  = "mac#atslib_timer_settime"
// end of [timer_settime]

(* ****** ****** *)
//
// HX: 0/-1 : succ/fail // errno set
//
fun timer_getoverrun
  {id:int} (
  pf: !timer_v (id) | tid: timer_t (id)
) : intGte (~1)
  = "mac#atslib_timer_getoverrun"
// end of [timer_getoverrun]

(* ****** ****** *)

(* end of [time.sats] *)
