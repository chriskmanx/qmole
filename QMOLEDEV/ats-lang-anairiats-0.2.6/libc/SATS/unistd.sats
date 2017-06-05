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
#include "libc/CATS/unistd.cats"
%} // end of [%{#]

(* ****** ****** *)
//
staload TYPES = "libc/sys/SATS/types.sats"
//
typedef off_t = $TYPES.off_t
typedef pid_t = $TYPES.pid_t
typedef uid_t = $TYPES.uid_t
typedef gid_t = $TYPES.gid_t
//
typedef mode_t = $TYPES.mode_t
//
typedef whence_t = $TYPES.whence_t
//
(* ****** ****** *)

staload FCNTL = "libc/SATS/fcntl.sats"
stadef fildes_v = $FCNTL.fildes_v

(* ****** ****** *)

sta STDIN_FILENO : int
sta STDOUT_FILENO : int
sta STDERR_FILENO : int
//
praxi STDIN_FILENO_gtez (): [STDIN_FILENO >= 0] void
praxi STDOUT_FILENO_gtez (): [STDOUT_FILENO >= 0] void
praxi STDERR_FILENO_gtez (): [STDERR_FILENO >= 0] void
//
macdef STDIN_FILENO = $extval (int STDIN_FILENO, "STDIN_FILENO")
macdef STDOUT_FILENO = $extval (int STDOUT_FILENO, "STDOUT_FILENO")
macdef STDERR_FILENO = $extval (int STDERR_FILENO, "STDERR_FILENO")

(* ****** ****** *)
//
// HX: implemented in [$ATSHOME/prelude/CATS/basics.cats]
//
fun stdin_fildes_view_get
  (): (fildes_v (STDIN_FILENO) | void) = "atspre_stdin_view_get"
fun stdin_fildes_view_set
  (pf: fildes_v (STDIN_FILENO) | (*none*)): void = "atspre_stdin_view_set"
// end of [stdin_fildes_view_set]

fun stdout_fildes_view_get
  (): (fildes_v (STDOUT_FILENO) | void) = "atspre_stdout_view_get"
fun stdout_fildes_view_set
  (pf: fildes_v (STDOUT_FILENO) | (*none*)): void = "atspre_stdout_view_set"
// end of [stdout_fildes_view_set]

fun stderr_fildes_view_get
  (): (fildes_v (STDERR_FILENO) | void) = "atspre_stderr_view_get"
fun stderr_fildes_view_set
  (pf: fildes_v (STDERR_FILENO) | (*none*)): void = "atspre_stderr_view_set"
// end of [stderr_fildes_view_get]

(* ****** ****** *)

fun dup {fd:int} (
  pf: !fildes_v fd | fd: int fd
) : [fd1: int] (
  option_v (fildes_v fd1, fd1 >= 0) | int fd1
) = "mac#atslib_dup"
// end of [dup]

symintr dup2

fun dup2_exi {fd:int;fd2:nat} (
  pf: !fildes_v fd
, pf2: fildes_v fd2
| fd: int fd, fd2: int fd2
) : [i:int] (
  option_v (fildes_v fd2, i >= 0) | int i // i == fd2 if i >= 0
) = "mac#atslib_dup2"
overload dup2 with dup2_exi

fun dup2_noexi {fd:int;fd2:nat} (
  pf: !fildes_v fd
| fd: int fd, fd2: int fd2
) : [i:int] (
  option_v (fildes_v fd2, i >= 0) | int i // i == fd2 if i >= 0
) = "mac#atslib_dup2"
overload dup2 with dup2_noexi

(* ****** ****** *)

fun _exit (status: int): void = "mac#atslib__exit" // !macro

(* ****** ****** *)

fun execv {n:pos}
  (path: !READ(string), argv: &ptrarr(n)): int = "mac#atslib_execv"
fun execvp {n:pos}
  (path: !READ(string), argv: &ptrarr(n)): int = "mac#atslib_execvp"

(* ****** ****** *)

fun fork_err (): pid_t = "atslib_fork_err" // = fork

(* ****** ****** *)

//
// HX-2010-10-08:
// these functions, which are implemented in [$ATSHOME/libc/DATS/unistd.dats],
// are now kept for historic reasons.
//
fun fork_exn (): pid_t = "atslib_fork_exn" // function!

//
// HX: the parent returns immediately
//
fun fork_exec_cloptr_exn
  {v:view} (pf: !v | f: (v | (*none*)) -<cloptr1> void): void
 = "atslib_fork_exec_cloptr_exn"
// end of [fork_exec_cloptr_exn]

//
// HX: the parent waits until the (only) child finishes
//
fun fork_exec_and_wait_cloptr_exn
  (proc: () -<cloptr1> void): int = "atslib_fork_exec_and_wait_cloptr_exn"
// end of [fork_exec_and_wait_cloptr_exn]

(* ****** ****** *)

dataview
getcwd_v (m:int, l:addr, addr) =
  | {l>null} {n:nat}
    getcwd_v_succ (m, l, l) of strbuf_v (m, n, l)
  | getcwd_v_fail (m, l, null) of b0ytes (m) @ l
// end of [getcwd_v]

fun getcwd {m:nat} {l:addr} (
  pf: !b0ytes (m) @ l >> getcwd_v (m, l, l1)
| p: ptr l, m: size_t m
) : #[l1:addr] ptr l1
  = "mac#atslib_getcwd"
// end of [getcwd]

(* ****** ****** *)
//
// HX: implemented in [$ATSHOME/libc/DATS/unistd.dats]
//
fun getcwd0 (): strptr1 = "atslib_getcwd0"
//
// HX: [get_current_dir_name] is available if _GNU_SOURCE is on
//
(* ****** ****** *)

absview alarm_v (int)
praxi alarm_v_elim (pf: alarm_v (0)): void
fun alarm_set {i:nat}
  (t: uint i): (alarm_v (i) | uInt) = "mac#atslib_alarm_set"
// end of [alarm_set]
fun alarm_cancel {i:int}
  (pf: alarm_v (i) | (*none*)): uInt = "mac#atslib_alarm_cancel"
// end of [alarm_cancel]

(* ****** ****** *)

// [sleep] may be implemented using SIGARM
fun sleep {i:nat}
  (t: int i): [j:nat | j <= i] int j = "mac#atslib_sleep"
// end of [sleep]

(* ****** ****** *)

#define MILLION 1000000
// some systems require that the argument of usleep <= 1 million
fun usleep
  (n: natLte MILLION (*microseconds*)): void = "atslib_usleep" // !fun
// end of [usleep]

(* ****** ****** *)

fun getpagesize ():<> int = "mac#atslib_getpagesize" // macro

(* ****** ****** *)

fun getuid ():<> uid_t = "mac#atslib_getuid" // user
fun geteuid ():<> uid_t = "mac#atslib_geteuid" // effective user

(* ****** ****** *)
// 
// HX: for superuser // 0/-1 : succ/fail
//
fun setuid (
  uid: uid_t
) :<> int
  = "mac#atslib_setuid"
// end of [setuid]

fun seteuid (
  uid: uid_t
) :<> int
  = "mac#atslib_seteuid" // 0/-1 : succ/fail
// end of [seteuid]

(* ****** ****** *)

fun getgid ():<> gid_t = "mac#atslib_getgid" // group
fun getegid ():<> gid_t = "mac#atslib_getegid" // effective group

(* ****** ****** *)
// 
// HX: for superuser // 0/-1 : succ/fail
//
fun setgid (
  gid: gid_t
) :<> int 
  = "mac#atslib_setgid" // !macro
// end of [setgid]

fun setegid (
  gid: gid_t
) :<> int
  = "mac#atslib_setegid" // 0/-1 : succ/fail
// end of [setegid]

(* ****** ****** *)

fun getpid (): pid_t = "mac#atslib_getpid" // process ID
fun getppid (): pid_t = "mac#atslib_getppid" // parent process ID

(* ****** ****** *)
//
// HX: session IDs
//
fun setsid (): pid_t = "mac#atslib_setsid" // -1 is returned on error
fun getsid (pid: pid_t): pid_t = "mac#atslib_getsid" // -1 is returned on error

(* ****** ****** *)
//
// HX: process group IDs
//
fun getpgid (
  pid: pid_t
) :<> pid_t
  = "mac#atslib_getpgid" // -1 is returned on error
// end of [fun]

fun setpgid (
  pid: pid_t, pgid: pid_t
) : int
  = "mac#atslib_setpgid" // 0/-1 : succ/fail
// end of [fun]

fun getpgrp (
// there is no argument
) : pid_t
  = "mac#atslib_getpgrp" // = getpgid (0) // no error
// end of [getpgrp]

fun setpgrp (
// there is no argument
) : int
  = "mac#atslib_setpgrp" // = setpgid (0, 0)
// end of [setpgrp]

(* ****** ****** *)
//
// HX: non-reentrant version
//
fun getlogin (
// there is no argument
) :<!ref> [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "mac#atslib_getlogin" // macro
// end of [getlogin]

dataview
getlogin_v (m:int, l:addr, int) =
  | {n:nat}
    getlogin_v_succ (m, l, 0) of strbuf_v (m, n, l)
  | {i:int | i <> 0}
    getlogin_v_fail (m, l, i) of b0ytes (m) @ l
// end of [getlogin_v]

fun getlogin_r
  {m:int} {l:addr} (
  pf: !b0ytes (m) @ l >> getlogin_v (m, l, i) | p: ptr l, n: size_t
) : #[i:int] int i
  = "mac#atslib_getlogin_r" // 0/!0: succ/fail
// end of [fun]

(* ****** ****** *)
//
macdef R_OK = $extval (uint, "R_OK") // test for read permission
macdef W_OK = $extval (uint, "W_OK") // test for write permission
macdef X_OK = $extval (uint, "X_OK") // test for execute permission
macdef F_OK = $extval (uint, "F_OK") // test for existence
//
fun access
  (path: !READ(string), mode: uint): int = "mac#atslib_access"
// end of [access]

(* ****** ****** *)

fun chroot
  (path: !READ(string)): int = "mac#atslib_chroot" // 0/-1 : succ/fail
// end of [chroot]

(* ****** ****** *)

fun chdir
  (path: !READ(string)): int(*err*) = "mac#atslib_chdir"
fun fchdir {fd:int}
  (pf: !fildes_v (fd) | fd: int): int(*err*) = "mac#atslib_fchdir"
// end of [fchdir]

(* ****** ****** *)

fun nice
  (incr: int): int = "mac#atslib_nice" // NZERO/-1 : succ/fail // errno set
// end of [nice]

(* ****** ****** *)
//
// HX: succ/fail: 0/-1
//
fun rmdir (path: !READ(string)): int = "mac#atslib_rmdir"

(* ****** ****** *)

fun link (
  src: !READ(string), dst: !READ(string)
) : int = "mac#atslib_link"
fun unlink (path: !READ(string)): int = "mac#atslib_unlink"

(* ****** ****** *)

fun lseek_err {fd:int}
  (pf: !fildes_v (fd) | fd: int fd, ofs: off_t, whence: whence_t): off_t
  = "atslib_fildes_lseek_err"
// end of [lseek_err]
fun lseek_exn {fd:int}
  (pf: !fildes_v (fd) | fd: int fd, ofs: off_t, whence: whence_t): off_t
  = "atslib_fildes_lseek_exn"
// end of [lseek_exn]

(* ****** ****** *)

fun pread
  {fd:int} {n,sz:nat | n <= sz} (
    pf: !fildes_v (fd)
  | fd: int fd, buf: &bytes sz, ntotal: size_t n, ofs: off_t
  ) : ssizeBtw(~1, n+1)
  = "atslib_fildes_pread"
// end of [fildes_pread]

fun pwrite
  {fd:int} {n,sz:nat | n <= sz} (
    pf: !fildes_v (fd)
  | fd: int fd, buf: &bytes sz, ntotal: size_t n, ofs: off_t
  ) : ssizeBtw(~1, n+1)
  = "atslib_fildes_pwrite"
// end of [fildes_pwrite]

(* ****** ****** *)

fun sync (): void = "mac#atslib_sync"

// [fsync] returns 0 on success or -1 on error
fun fsync {fd:int} // (sets errno)
  (pf: !fildes_v (fd) | fd: int fd): int = "mac#atslib_fsync"
// end of [fsync]

// [fdatasync] returns 0 on success or -1 on error
fun fdatasync {fd:int} // (sets errno)
  (pf: !fildes_v (fd) | fd: int fd): int = "mac#atslib_fdatasync"
// end of [fdatasync]

(* ****** ****** *)

fun readlink
  {n:nat} {l:addr} (
  pf: !b0ytes(n) @ l >> bytes(n) @ l
| path: !READ(string), p: ptr l, n: size_t n
) : [n1:int | n1 <= n] ssize_t (n1) = "mac#atslib_readlink"
// end of [readlink]

(* ****** ****** *)

fun pipe (
  fd1: &int? >> int fd1, fd2: &int? >> int fd2
) : #[fd1,fd2:int] [i:int | i <= 0]
  (option_v ((fildes_v fd1, fildes_v fd2), i==0) | int i)
  = "atslib_pipe" // function!
// end of [pipe]

(* ****** ****** *)

fun tcsetpgrp {fd:nat} (
  fd: int fd, pgid: pid_t
) : int
  = "mac#atslib_tcsetpgrp" // 0/-1 : succ/fail
// end of [tcsetpgrp]
fun tcgetpgrp {fd:nat}
  (fd: int fd): pid_t = "mac#atslib_tcgetpgrp" // -1 is returned on error
// end of [tcgetpgrp]

(* ****** ****** *)

fun ttyname {fd:nat} (
  fd: int fd
) :<!ref> [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "mac#atslib_ttyname"
// end of [ttyname]

dataview
ttyname_v (m:int, l:addr, int) =
  | {n:nat | m > n}
    ttyname_v_succ (m, l, 0) of strbuf_v (m, n, l)
  | {i:int | i > 0} ttyname_v_fail (m, l, i) of b0ytes m @ l
fun ttyname_r
  {fd:nat} {m:nat} {l:addr} (
    pf: b0ytes m @ l
  | fd: int fd, p: ptr l, m: size_t m
  ) :<> [i:int | i >= 0] (ttyname_v (m, l, i) | int i)
  = "mac#atslib_ttyname_r" // if it fails, errno is returned
// end of [ttyname_r]

(* ****** ****** *)

fun isatty {fd:nat}
  (fd: int fd): int = "mac#atslib_isatty" // 1/0 : yes/no
// end of [isatty]

(* ****** ****** *)

fun environ_get_arrsz
  (n: &size_t? >> size_t n):<!ref> #[n:nat] [l:addr] (
  array_v (string, n, l), array_v (string, n, l) -<lin,prf> void | ptr l
) = "atslib_environ_get_arrsz"
// end of [environ_get_arrsz]

(* ****** ****** *)

dataview
gethostname_v (m:int, l:addr, int) =
  | gethostname_v_fail (m, l, ~1) of (b0ytes m @ l)
  | {n:nat | n < m}
    gethostname_v_succ (m, l,  0) of strbuf_v (m, n, l)
// end of [gethostname_v]

fun gethostname
  {m:pos} {l:addr} (
  pf: b0ytes(m) @ l | p: ptr l, m: size_t m
) : [i:nat] (
  gethostname_v (m, l, i) | int i
) = "atslib_gethostname" // function!
// end of [gethostname]

//
// HX: [m] should most likely be [n+1].
//
fun sethostname {m,n:nat | n < m}
  (name: !READ(string n), m: size_t m): int = "mac#atslib_sethostname"
// end of [sethostname]

(* ****** ****** *)

dataview
getdomainname_v (m:int, l:addr, int) =
  | getdomainname_v_fail (m, l, ~1) of (b0ytes m @ l)
  | {n:nat | n < m}
    getdomainname_v_succ (m, l,  0) of strbuf_v (m, n, l)
// end of [getdomainname_vt]

fun getdomainname
  {m:pos} {l:addr} (
  pf: b0ytes(m) @ l | p: ptr l, m: size_t m
) : [i:nat] (
  getdomainname_v (m, l, i) | int i
) = "atslib_getdomainname" // function!
// end of [getdomainname]
//
// HX: [m] should most likely be [n+1].
//
fun setdomainname {m,n:nat | n < m}
  (name: !READ(string n), m: size_t m): int = "mac#atslib_setdomainname"
// end of [setdomainname]

(* ****** ****** *)

fun pause (): int = "mac#atslib_pause" // if it returns, the return value is -1

(* ****** ****** *)

(* end of [unistd.sats] *)
