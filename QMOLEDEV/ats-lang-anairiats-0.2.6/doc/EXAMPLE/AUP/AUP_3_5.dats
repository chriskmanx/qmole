//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 147 - 158
// section 3.5: Accessing and Displaying File Metadata
//
(* ****** ****** *)
//
// implementing a command [longls], which is kind of like [ls -l]
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/grp.sats"
staload "libc/SATS/pwd.sats"
staload "libc/SATS/time.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/stat.sats"
staload "libc/sys/SATS/types.sats"

(* ****** ****** *)

fun print_mode .<>.
  (st: &stat): void = let
  val mode = st.st_mode
//
  macdef TYPE(b) = ,(b) = (mode land S_IFMT)
  macdef MODE(b) = ,(b) = (mode land ,(b))
//
  val () = case+ 0 of
    | _ when TYPE(S_IFBLK) => print 'b'
    | _ when TYPE(S_IFCHR) => print 'c'
    | _ when TYPE(S_IFDIR) => print 'd'
    | _ when TYPE(S_IFIFO) => print 'p'
    | _ when TYPE(S_IFLNK) => print 'l'
    | _ when TYPE(S_IFREG) => print '-'
    | _ when TYPE(S_IFSOCK) => print 's'
    | _ => print '?'
  // end of [val]
//
  val () = if MODE(S_IRUSR) then print 'r' else print '-'
  val () = if MODE(S_IWUSR) then print 'w' else print '-'
  val () = case+ 0 of
    | _ when MODE(S_ISUID) =>
       if MODE(S_IXUSR) then print 's' else print 'S'
    | _ => 
       if MODE(S_IXUSR) then print 'x' else print '-'
  // end of [val]
//
  val () = if MODE(S_IRGRP) then print 'r' else print '-'
  val () = if MODE(S_IWGRP) then print 'w' else print '-'
  val () = case+ 0 of
    | _ when MODE(S_ISGID) =>
       if MODE(S_IXGRP) then print 's' else print 'S'
    | _ => 
       if MODE(S_IXGRP) then print 'x' else print '-'
   // end of [val]
//
  val () = if MODE(S_IROTH) then print 'r' else print '-'
  val () = if MODE(S_IWOTH) then print 'w' else print '-'
  val () = case+ 0 of
    | _ when MODE(S_IFDIR) andalso MODE(S_ISVTX) =>
       if MODE(S_IXOTH) then print 't' else print 'T'
    | _ => 
       if MODE(S_IXOTH) then print 'x' else print '-'
  // end of [val]
//
in
  // nothing
end // end of [print_mode]

(* ****** ****** *)

fun print_nlink .<>.
  (st: &stat): void = let
  val nlink = st.st_nlink
  val nlink = lint_of_nlink (nlink)
in
  printf ("%5ld", @(nlink))
end // end of [print_nlink]

(* ****** ****** *)

fun print_owner .<>.
  (st: &stat): void = let
  val uid = st.st_uid
  val (pfopt| p) = getpwuid (uid)
in
  if p > null then let
    prval Some_v @(pf, fpf) = pfopt
    val (fpf_x | x) = passwd_get_pw_name (!p)
    prval () = fpf (pf)
    val () = assert_errmsg (strptr_isnot_null x, #LOCATION)
    val () = printf (" %-8s", @(__cast x)) where {
      extern castfn __cast {l:addr} (x: !strptr l):<> string
    }
    prval () = fpf_x (x)
  in
    // nothing
  end else let
    prval None_v () = pfopt
    val uid = lint_of_uid (uid)
  in
    printf (" %-8ld", @(uid))
  end // end of [if]
end (* end of [print_owner] *)

(* ****** ****** *)

fun print_group .<>.
  (st: &stat): void = let
  val gid = st.st_gid
  val (pfopt| p) = getgrgid (gid)
in
  if p > null then let
    prval Some_v @(pf, fpf) = pfopt
    val (fpf_x | x) = group_get_gr_name (!p)
    prval () = fpf (pf)
    val () = assert_errmsg (strptr_isnot_null x, #LOCATION)
    val () = printf (" %-8s", @(__cast x)) where {
      extern castfn __cast {l:addr} (x: !strptr l):<> string
    }
    prval () = fpf_x (x)
  in
    // nothing
  end else let
    prval None_v () = pfopt
    val gid = lint_of_gid (gid)
  in
    printf (" %-8ld", @(gid))
  end // end of [if]
end (* end of [print_group] *)

(* ****** ****** *)

fun print_size .<>.
  (st: &stat): void = let
  val mode = st.st_mode
//
  macdef TYPE(b) = ,(b) = (mode land S_IFMT)
  macdef MODE(b) = ,(b) = (mode land ,(b))
//
in
  case+ 0 of
  | _ when (TYPE(S_IFCHR) orelse TYPE(S_IFBLK)) => let
      val rdev = st.st_rdev
      val rdev = uint_of_dev (rdev)
      val u1 = rdev >> 8
      val u2 = rdev land 0xFFU
    in
      printf ("%4u,%4u", @(u1, u2))
    end // end of [_ when ...]
  | _ => let
      val off = st.st_size
      val off = lint_of_off (off)
      val off = ulint_of (off)
    in
      printf ("%9lu", @(off))
    end (* end of [_] *)
end // end of [print_size]

(* ****** ****** *)

fun print_date .<>.
  (st: &stat): void = let
  val now = time_get ()
in
//
if (lint_of_time)now >= 0L then let
  val diff = difftime (now, st.st_mtime)
  val (pfopt | p) = localtime (st.st_mtime)
  val () = assert_errmsg (p > null, #LOCATION)
  prval Some_v @(pf, fpf) = pfopt
  var !p_buf with pf_buf = @[byte][64]()
  val fmt = (
    if (diff < 0.0 orelse diff > 60*60*24*182.5 (*6months*)) then "%b %e  %Y" else "%b %e %H:%M"
  ) : string
  val _n = strftime (pf_buf | p_buf, 64, fmt, !p)
  prval () = fpf (pf)
  val str = __cast (p_buf) where {
    extern castfn __cast (x: ptr): string // cutting a corner
  } // end of [val]
  prval () = pf_buf := bytes_v_of_strbuf_v (pf_buf)
in
  printf (" %s", @(str))
end else begin
  printf (" ????????????", @())
end // end of [if]
//
end // end of [print_date]

(* ****** ****** *)

fun print_name .<>.
  (st: &stat, name: !READ(string)): void = let
  // nothing
in
  case+ 0 of
  | _ when S_ISLNK
      (st.st_mode) => let
      val n = lint_of_off (st.st_size)
      val n = n + 1L
      val n = size_of_lint (n)
      val n = size1_of_size (n)
      val (pfgc, pf | p) = malloc_gc (n)
      val n1 = readlink (pf | name, p, n)
      val () = if (n1 >= 0) then let
        val n1 = size1_of_ssize1 (n1)
        val () = assert_errmsg (n1 < n, #LOCATION)
        val () = bytes_strbuf_trans (pf | p, n1)
        val () = printf (" %s -> %s", @($UNSAFE.castvwtp1{string}(name), __cast p)) where {
          extern castfn __cast (x: ptr): string // cutting a corner
        } // end of [val]
        prval () = pf := bytes_v_of_strbuf_v (pf)
      in
        // nothing
      end else
        printf (" %s -> [can't read link]", @($UNSAFE.castvwtp1{string}(name)))
      // end of [val]
    in
      free_gc (pfgc, pf | p)
    end // end of [_ when ...]
  | _ => printf (" %s", @($UNSAFE.castvwtp1{string}(name)))
end // end of [print_name]

(* ****** ****** *)

fun longls .<>.
  (st: &stat, path: string): void = let
  val () = print_mode (st)
  val () = print_nlink (st)
  val () = print_owner (st)
  val () = print_group (st)
  val () = print_size (st)
  val () = print_date (st)
  val () = print_name (st, path)
  val () = print_newline ()
in
  // nothing
end // end of [longls]

(* ****** ****** *)

implement
main {n} (argc, argv) = () where {
//
  var i: natLte n
  var st: stat? // uninitialized
//
  val () = for
    (i := 1; i < argc; i := i+1) let
    val path = argv.[i]
    val _err = lstat_err (path, st)
    val () = if _err >= 0 then let
      prval () = opt_unsome {stat} (st)
      val () = longls (st, path)
    in
      // nothing
    end else let
      prval () = opt_unnone {stat} (st)
      val () = printf ("longls: cannot access [%s]: No such file or directory\n", @(path))
    in
      // nothing
    end // end of [if]
  in
    // nothing
  end // end of [val]
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_3_5.dats] *)
