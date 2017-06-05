(*
** some testing code for functions declared in
** libc/SATS/stdlib.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//

(* ****** ****** *)

staload "libc/sys/SATS/utsname.sats"

(* ****** ****** *)

implement
main () = () where {
//
  var name: utsname?
  val err = uname (name)
  val () = if err = 0 then let
    prval () = opt_unsome {utsname} (name)
//
    val (fpf_x | x) = utsname_get_sysname (name)
    val () = (print "name.sysname = "; print x; print_newline ())
    prval () = fpf_x (x)
    val (fpf_x | x) = utsname_get_nodename (name)
    val () = (print "name.nodename = "; print x; print_newline ())
    prval () = fpf_x (x)
    val (fpf_x | x) = utsname_get_release (name)
    val () = (print "name.release = "; print x; print_newline ())
    prval () = fpf_x (x)
    val (fpf_x | x) = utsname_get_version (name)
    val () = (print "name.version = "; print x; print_newline ())
    prval () = fpf_x (x)
    val (fpf_x | x) = utsname_get_machine (name)
    val () = (print "name.machine = "; print x; print_newline ())
    prval () = fpf_x (x)
(*
    val (fpf_x | x) = utsname_get_domainname (name)
    val () = (print "name.domainname = "; print x; print_newline ())
    prval () = fpf_x (x)
*)
//
  in
    // nothing
  end else let
    prval () = opt_unnone {utsname} (name)
  in
    // nothing
  end // end of [val]
//
} // end of [main]

(* ****** ****** *)

(* end of [libc_sys_utsname.dats] *)
