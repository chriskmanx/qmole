//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynloading at run-time

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/string.sats"

(* ****** ****** *)

staload "errinfo.sats"

viewtypedef errinfo_struct =
$extype_struct "ats_errinfo_type" of {
  errinfo_loc= string
, errinfo_errno= errno_t
, errinfo_errstr= strptr0
} // end of [errinfo_struct]
assume errinfo_t = errinfo_struct

(* ****** ****** *)

implement
fprint_errinfo (out, ei) = let
  val en = int_of_errno (ei.errinfo_errno)
  val () = fprintf (out, "ERROR(%i): ", @(en))
  val () = fprint_string (out, ei.errinfo_loc)
  val () = fprintf (out, ": ", @())
  val () = fprint_strptr (out, ei.errinfo_errstr)
  val () = fprint_newline (out)
in
  // nothing
end // end of [fprint_errinfo]

(* ****** ****** *)

implement
errinfo_set_wloc
  (ei, loc) = let
  val errno = errno_get ()
  val (fpf_errstr | errstr) = strerror (errno)
  val () = ei.errinfo_loc := loc
  val () = () where {
    val () = ei.errinfo_errstr := strptr_dup (errstr)
  } // end of [val]
  prval () = fpf_errstr (errstr)
  val () = ei.errinfo_errno := errno
in
  // nothing
end // end of [errinfo_set_wloc]

(* ****** ****** *)

implement
errinfo_clear (ei) = () where {
  val () = strptr_free (ei.errinfo_errstr)
} // end of [errinfo_clear]

(* ****** ****** *)

(* end of [errinfo.dats] *)
