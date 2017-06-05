//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//

(* ****** ****** *)
//
// book: AUP (2nd edition), pages 144 - 146
// section: 3.4: Pathnames
//
(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/unistd.sats"
staload "libc/SATS/unistd_pathconf.sats"

(* ****** ****** *)

fun get_max_pathname
  (path: string): lint = let
  var maxlen: lint = pathconf (path, _PC_PATH_MAX)
  val () = if (maxlen < 0L) then
    if errno_get () = (errno_of_int)0 then maxlen := 4096L else ()
  // end of [val]
in
  if maxlen >= 0L then maxlen + 1L else ~1L
end // end of [get_max_pathname]

(* ****** ****** *)

fun mygetcwd (): strptr0 = let
  val maxlen = get_max_pathname (".")
in
  case+ 0 of
  | _ when maxlen >= 0L => let
      val maxlen = ulint_of_lint (maxlen)
      val nsz = size_of_ulint (maxlen)
      val [n:int] nsz = size1_of_size (nsz)
      val [l:addr] (pfgc, pf | p) = malloc_gc (nsz)
      val p1 = getcwd (pf | p, nsz)
      val () = assert_errmsg (p1 > null, #LOCATION)
      prval getcwd_v_succ (pf) = pf
    in
      strptr_of_strbuf @(pfgc, pf | p)
    end // end of [_ when ...]
  | _ (*maxlen = -1L*) => strptr_null ()
end // end of [mygetcwd]

(* ****** ****** *)

implement main () = () where {
//
  val path = "/"
  val maxlen = get_max_pathname (path)
  val () = (printf ("get_max_pathname (\"%s\") = ", @(path)); print maxlen; print_newline ())
//
  val cwd = mygetcwd ()
  val () = (print "cwd = "; print cwd; print_newline ())
  val () = strptr_free (cwd)
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_3_4.dats] *)
