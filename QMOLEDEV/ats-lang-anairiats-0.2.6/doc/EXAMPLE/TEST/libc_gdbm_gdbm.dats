(*
** some testing code for functions declared in
** libc/SATS/printf.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/fcntl.sats"
staload "libc/sys/SATS/stat.sats"
staload "libc/sys/SATS/types.sats"
staload "libc/gdbm/SATS/gdbm.sats"

(* ****** ****** *)

implement
main () = () where {
//
  val mode = lor_mode_mode (S_IRUSR, S_IWUSR)
  val [lf:addr] dbf = gdbm_open ("gdbmtest.gdbm", 512(*blksz*), GDBM_NEWDB, mode, null)
(*
  val () = printf ("errstr = %s\n", @(gdbm_strerror (gdbm_errno_get ())))
*)
  val () = assertloc (ptr_of (dbf) > null)
  val (fpf_k | k) = datum_make0_string ("a")
  val v = datum_make1_string ("A")
  val _err = gdbm_store (dbf, k, v, GDBM_INSERT)
  val () = datum_free (v)
  val isexi = gdbm_exists (dbf, k)
  val () = assertloc (isexi > 0)
  val v = gdbm_fetch (dbf, k)
//
  val () = println! ("k(a) = ", $UN.cast {string} (ptr_of(k.dptr)))
  val () = println! ("v(A) = ", $UN.cast {string} (ptr_of(v.dptr)))
//
  val () = datum_free (v)
  val err = gdbm_delete (dbf, k)
  val () = assertloc (err = 0)
  val isexi = gdbm_exists (dbf, k)
  val () = assertloc (isexi = 0)
  prval () = fpf_k (k.dptr)
  prval () = cleanup_top {datum(null,0)} (k)
(*
  prval () = fpf_k (datum_takeout_ptr (k)) // HX: an alternative to do it
*)
//
  val () = gdbm_close (dbf)
//
} // end of [main]

(* ****** ****** *)

(* end of [libc_gdbm_gdbm.dats] *)


