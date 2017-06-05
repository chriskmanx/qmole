(*
** My own [find] function for traversing file trees.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (* hwxi AT cs DOT bu DOT edu *)
// 14 February 2011:
//
(* ****** ****** *)

staload "libc/SATS/dirent.sats"

(*
fun dirent_stream_vt_make_DIR {l_dir:addr}
  (pf: DIR @ l_dir | p: ptr l_dir):<!laz> stream_vt (dirent)
// end of [dirent_strean_vt_make_DIR]
*)

(* ****** ****** *)

extern
fun doit {l:agz}
  (dirname: string, name: !strptr l): void
// end of [doit]

fun myfind (dirname: string): void = let
  val (pf_dir | p_dir) = opendir_exn (dirname)
  val xs = dirent_stream_vt_make_DIR (pf_dir | p_dir)
  fun loop (
    xs: stream_vt (dirent)
  ) :<cloref1> void = let
    val xs_con = !xs
  in
    case+ xs_con of
    | stream_vt_cons (!p_x, xs) => let
        val (fpf_name | name) = dirent_get_d_name (!p_x)
        val () = doit (dirname, name)
        prval () = fpf_name (name)
        val () = free@ {dirent} (xs_con)
      in
        loop (xs)
      end // end of [stream_vt_cons]
    | ~stream_vt_nil () => ()
  end // end of [loop]
  val () = loop (xs)
in
  // nothing
end // end of [myfind]

(* ****** ****** *)

staload S = "libc/sys/SATS/stat.sats"
staload T = "libc/sys/SATS/types.sats"
staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

fun isexec (x: uint): bool = let
  val m = $T.uint_of_mode ($S.S_IXUSR) land (x)
in
  if m > 0U then true else false
end // end of [isexec]

(* ****** ****** *)

implement
doit (dirname, basename) = let
  val basename = $UN.castvwtp1 {string} (basename)
  val () = case+ basename of
    | "." => ()
    | ".." => ()
    | name when
        test_file_isdir (basename) > 0 => let
        val fullname = sprintf ("%s/%s", @(dirname, basename))
        val () = myfind ($UN.castvwtp1 {string} (fullname))
        val () = strptr_free (fullname)
      in
        // nothing
      end // end of [_ when ...]
    | name => println! (dirname, "/", basename)
  // end of [val]
in
  // nothing
end // end of [doit]

(* ****** ****** *)

(*
implement
doit (dirname, basename) = let
  val fullname1 = sprintf
    ("%s/%s", @(dirname, basename)) where {
    val basename = $UN.castvwtp1 {string} (basename)
  } // end of [val]
  val fullname = $UN.castvwtp1 {string} (fullname1)
  val () = println! ("doit: fullname = ", fullname)
  val () = case+ $UN.castvwtp1 {string} (basename) of
    | "." => ()
    | ".." => ()
    | _ when
        test_file_isdir (fullname) > 0 => let
        val () = $S.chmod_exn (fullname, $T.mode_of_int (0755))
        val () = myfind (fullname)
      in
        // nothing
      end // end of [_ when ...]
    | _ when test_file_islnk (fullname) > 0 => ()
    | _ when test_file_isreg (fullname) > 0 => let
        val m = (
          if test_file_mode (fullname, isexec) > 0 then 0755 else 0644
        ) : int // end of [val]
        val () = $S.chmod_exn (fullname, $T.mode_of_int (m))
      in
        // nothing
      end // end of [_ when ...]
    | _ => () // end of [_]
  // end of [val]
  val () = strptr_free (fullname1)
in
  // nothing
end // end of [doit]
*)

(* ****** ****** *)

implement
main (argc, argv) = let
  var dirname: string = "."
  val () = if argc >= 2 then dirname := argv.[1]
in
  myfind (dirname)
end // end of [main]

(* ****** ****** *)

(* end of [myfind.dats] *)
