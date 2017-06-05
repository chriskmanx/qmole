//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staloading at run-time

(* ****** ****** *)

%{#
#include "errinfo.cats"
%} // end of [%{#]

(* ****** ****** *)

staload ERRNO = "libc/SATS/errno.sats"
typedef errno_t = $ERRNO.errno_t

(* ****** ****** *)

absviewt@ype errinfo_t = $extype "ats_errinfo_type"
absviewt@ype
opterr (i:int, l:addr) = int(i)
//
castfn
opterr_none {l:addr} (pf: errinfo_t? @ l | (*none*)): opterr (0, l)
castfn
opterr_some {i:int | i < 0} {l:addr} (pf: errinfo_t @ l | i: int i): opterr (i, l)
//
prfun
opterr_unnone {l:addr} (err: opterr (0, l)): errinfo_t? @ l
prfun
opterr_unsome {i:int | i < 0} {l:addr} (err: opterr (i, l)): errinfo_t @ l
//
fun opterr_clear {i:int} {l:addr} (err: !opterr (i, l) >> opterr (0, l)): void
//
fun opterr_set
  {i:int} {l:addr} (err: opterr (0, l), i: int i): opterr (i, l)
// end of [opterr_set]
castfn opterr2int {i:int} {l:addr} (err: !opterr (i, l)):<> int (i)
fun lt_opterr_int {i1,i2:int} {l:addr}
  (err: !opterr (i1, l), i2: int i2):<> bool (i1 < i2) = "#atspre_lt_int_int"
fun lte_opterr_int {i1,i2:int} {l:addr}
  (err: !opterr (i1, l), i2: int i2):<> bool (i1 <= i2) = "#atspre_lte_int_int"
fun gt_opterr_int {i1,i2:int} {l:addr}
  (err: !opterr (i1, l), i2: int i2):<> bool (i1 > i2) = "#atspre_gt_int_int"
fun gte_opterr_int {i1,i2:int} {l:addr}
  (err: !opterr (i1, l), i2: int i2):<> bool (i1 >= i2) = "#atspre_gte_int_int"
//
fun fprint_opterr {i:int} {l:addr} (err: !opterr (i, l)): void
//
(* ****** ****** *)

fun fprint_errinfo (out: FILEref, ei: &errinfo_t): void

(* ****** ****** *)

fun errinfo_set_wloc
  (ei: &errinfo_t? >> errinfo_t, loc: string) : void
// end of [errinfo_set_wloc]

macdef errinfo_set (ei) = errinfo_set_wloc (,(ei), #LOCATION)

(* ****** ****** *)

fun errinfo_clear (ei: &errinfo_t >> errinfo_t?): void

(* ****** ****** *)

(* end of [errinfo.sats] *)
