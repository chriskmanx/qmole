//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 315 - 316
// section 5.11: Getting User and Group IDs
//
(* ****** ****** *)

staload "libc/sys/SATS/types.sats"
staload "libc/SATS/grp.sats"
staload "libc/SATS/pwd.sats"
staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

implement
main () = () where {
//
  extern castfn __castuid (x: uid_t): lint
  extern castfn __castgid (x: gid_t): lint
  extern castfn __cast2 {l:agz} (x: !strptr l): string
//
  val uid = getuid ()
  val (pfopt | p) = getpwuid (uid)
  val () = if p > null then let
    prval Some_v @(pf, fpf) = pfopt
    val (fpf_name | name) = passwd_get_pw_name (!p)
    val () = assert (strptr_isnot_null name)
    val () = printf (
      "Real user = %ld (%s)\n", @((__castuid)uid, (__cast2)name)
    ) // end of [val]
    prval () = fpf_name (name)
    prval () = fpf (pf)
  in
    // nothing
  end else let
    prval None_v () = pfopt in exit (EXIT_FAILURE)
  end // end of [val]
//
  val uid = geteuid ()
  val (pfopt | p) = getpwuid (uid)
  val () = if p > null then let
    prval Some_v @(pf, fpf) = pfopt
    val (fpf_name | name) = passwd_get_pw_name (!p)
    val () = assert (strptr_isnot_null name)
    val () = printf (
      "Effective user = %ld (%s)\n", @((__castuid)uid, (__cast2)name)
    ) // end of [val]
    prval () = fpf_name (name)
    prval () = fpf (pf)
  in
    // nothing
  end else let
    prval None_v () = pfopt in exit (EXIT_FAILURE)
  end // end of [val]
//
  val gid = getgid ()
  val (pfopt | p) = getgrgid (gid)
  val () = if p > null then let
    prval Some_v @(pf, fpf) = pfopt
    val (fpf_name | name) = group_get_gr_name (!p)
    val () = assert (strptr_isnot_null name)
    val () = printf (
      "Real group = %ld (%s)\n", @((__castgid)gid, (__cast2)name)
    ) // end of [val]
    prval () = fpf_name (name)
    prval () = fpf (pf)
  in
    // nothing
  end else let
    prval None_v () = pfopt in exit (EXIT_FAILURE)
  end // end of [val]  
//
  val gid = getegid ()
  val (pfopt | p) = getgrgid (gid)
  val () = if p > null then let
    prval Some_v @(pf, fpf) = pfopt
    val (fpf_name | name) = group_get_gr_name (!p)
    val () = assert (strptr_isnot_null name)
    val () = printf (
      "Effective group = %ld (%s)\n", @((__castgid)gid, (__cast2)name)
    ) // end of [val]
    prval () = fpf_name (name)
    prval () = fpf (pf)
  in
    // nothing
  end else let
    prval None_v () = pfopt in exit (EXIT_FAILURE)
  end // end of [val]  
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_5_11.dats] *)
