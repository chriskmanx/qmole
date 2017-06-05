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
staload "libc/gdbm/SATS/ndbm.sats"

(* ****** ****** *)

implement
main () = () where {
//
  val flag = O_RDWR lor O_CREAT
  val mode = lor_mode_mode (S_IRUSR, S_IWUSR)
  val [lf:addr] dbf = dbm_open ("ndbmtest", flag, mode)
  val () = assertloc (ptr_of (dbf) > null)
  val (fpf_k | k) = datum_make0_string ("a")
  val v = datum_make1_string ("A")
  val _err = dbm_store (dbf, k, v, DBM_INSERT)
  val () = datum_free (v)
  val (fpf_v | v) = dbm_fetch (dbf, k)
//
  val () = println! ("k(a) = ", $UN.cast {string} (ptr_of(k.dptr)))
  val () = println! ("v(A) = ", $UN.cast {string} (ptr_of(v.dptr)))
//
  prval () = fpf_v (datum_takeout_ptr (v))
  val _err = dbm_delete (dbf, k)
  val () = assertloc (_err = 0)
  prval () = fpf_k (datum_takeout_ptr (k))
//
  val () = dbm_close (dbf)
//
} // end of [main]

(* ****** ****** *)

(* end of [libc_gdbm_ndbm.dats] *)


