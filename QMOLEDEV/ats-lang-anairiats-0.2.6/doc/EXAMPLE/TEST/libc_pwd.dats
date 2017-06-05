(*
** some testing code for functions declared in
** libc/SATS/pwd.sats
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/pwd.sats"
staload "libc/SATS/stdlib.sats" // for [getenv]
staload "libc/SATS/unistd_sysconf.sats" // for [_SC_GETPW_R_SIZE_MAX]

(* ****** ****** *)

fun show_passwd (pw: &passwd): void = let
//
  val (fpf | p) = passwd_get_pw_name (pw)
  val () = printf ("NAME = %s\n", @($UNSAFE.castvwtp1{string}(p)))
  prval () = fpf (p)
//
  val (fpf | p) = passwd_get_pw_passwd (pw)
  val () = printf ("PASSWD = %s\n", @($UNSAFE.castvwtp1{string}(p)))
  prval () = fpf (p)
//
  val (fpf | p) = passwd_get_pw_gecos (pw)
  val () = printf ("RNAME = %s\n", @($UNSAFE.castvwtp1{string}(p)))
  prval () = fpf (p)
//
  val (fpf | p) = passwd_get_pw_dir (pw)
  val () = printf ("HOME = %s\n", @($UNSAFE.castvwtp1{string}(p)))
  prval () = fpf (p)
//
  val (fpf | p) = passwd_get_pw_shell (pw)
  val () = printf ("SHELL = %s\n", @($UNSAFE.castvwtp1{string}(p)))
  prval () = fpf (p)
//
in
  // nothing
end // end of [show_passwd]

(* ****** ****** *)

implement
main () = () where {
  val (fpf_logname | logname) = getenv ("LOGNAME")
//
(*
fun getpwnam (nam: !READ(string)):<!ref>
  [l:addr] (ptroutopt (passwd, l) | ptr l) = "#atslib_getpwnam"
// end of [getpwnam]
*)
  val (pfopt | p_pw) = getpwnam ($UNSAFE.castvwtp1{string}(logname))
  val () = assertloc (p_pw > null)
  prval Some_v @(pf_pw, fpf_pw) = pfopt
  val () = show_passwd (!p_pw)
  prval () = fpf_pw (pf_pw)
//
(*
fun getpwnam_r {n:nat} (
    nam: !READ(string)
  , pwbuf: &passwd? >> opt (passwd, i==0)
  , buf: &b0ytes(n) >> bytes(n), n: size_t (n)
  ) :<> #[i:int | i >= 0] int (0)
  = "#atslib_getpwnam_r"
// end of [getpwnam_r]
*)
  val n = sysconf (_SC_GETPW_R_SIZE_MAX)
  val () = assertloc (n >= 0L)
  val n = size_of_lint (n)
  val n = size1_of_size (n)
  var !p_buf with pf_buf = @[byte][n]()
  var pwbuf: passwd
  var ppwbuf: ptr
  val err = getpwnam_r ($UNSAFE.castvwtp1{string}(logname), pwbuf, !p_buf, n, ppwbuf)
  val () = assertloc (err = 0)
  val () = assertloc (ppwbuf = &pwbuf)
  prval () = opt_unsome {passwd} (pwbuf)
  val () = show_passwd (pwbuf)  
//
  prval () = fpf_logname (logname)
//
} // end of [main]

(* ****** ****** *)

(* end of [libc_pwd.dats] *)
