(*
** some testing code for functions declared in
** libc/SATS/dirent.sats
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2009

(* ****** ****** *)

staload "libc/SATS/stdlib.sats"
staload "libc/SATS/dirent.sats"

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"
staload _(*anon*) = "prelude/DATS/lazy_vt.dats"

(* ****** ****** *)

extern
fun print_direntptr
  (p: &direntptr_gc): void = "print_direntptr"
// end of [print_direntptr]

implement
print_direntptr (p) = let
  prval pf_ent = p.1 // p = (pf_gc, pf | p_ent)
  val (fpf_x | x) = dirent_get_d_name !(p.2)
  val () = print_strptr (x)
  prval () = fpf_x (x)
in
  p.1 := pf_ent
end // end of [print_direntptr]

(* ****** ****** *)

fun compare_direntptr_direntptr .<>.
  (p1: &direntptr_gc, p2: &direntptr_gc):<> int = let
  prval pf1_ent = p1.1
  val (fpf_x1 | x1) = dirent_get_d_name !(p1.2)
  prval pf2_ent = p2.1
  val (fpf_x2 | x2) = dirent_get_d_name !(p2.2)
  val sgn = compare ($UN.castvwtp1 {string} (x1), $UN.castvwtp1 {string} (x2))
  prval () = fpf_x1 (x1) and () = fpf_x2 (x2)
  prval () = p1.1 := pf1_ent and () = p2.1 := pf2_ent
in
  sgn
end // end of [compare_direntptr_direntptr]
  
(* ****** ****** *)

fn prerr_usage (cmd: string): void =
  prerrf ("Usage: %s [dirname]\n", @(cmd))
// end of [prerr_usage]

fn ls (dirname: string): void = let
  val (pfopt_dir | p_dir) = opendir_err (dirname)
in
  if (p_dir > null) then let
    prval Some_v (pf_dir) = pfopt_dir
    val ents = direntptr_stream_vt_make_DIR (pf_dir | p_dir)
    val [n:int] (nent, ents) = list_vt_of_stream_vt<direntptr_gc> (ents)
    val nent_sz = size1_of_int1 (nent)
    var !p_arr with pf_arr = @[direntptr_gc][nent]()
    val () = loop_init
      {direntptr_gc} (pf_arr | p_arr, ents) where {
      fun loop_init {a:viewtype} {n:nat} {l:addr} .<n>. (
          pf_arr: !array_v (a?, n, l) >> array_v (a, n, l)
        | p: ptr l, xs: list_vt (a, n)
        ) : void = case+ xs of
        | ~list_vt_cons (x, xs) => let
            prval (pf1_at, pf2_arr) = array_v_uncons {a?} (pf_arr)
            val () = !p := x
            val () = loop_init {a} (pf2_arr | p+sizeof<a>, xs)
          in
            pf_arr := array_v_cons {a} (pf1_at, pf2_arr)
          end // end of [list_vt_cons]
        | ~list_vt_nil () => let
            prval () = array_v_unnil {a?} (pf_arr)
          in
            pf_arr := array_v_nil {a} ()
          end // end of [list_vt_nil]
      // end of [loop]
    } // end of [val]
//
    val () = qsort {direntptr_gc}
      (!p_arr, nent_sz, sizeof<direntptr_gc>, compare_direntptr_direntptr)
    // end of [qsort]
//
    val () = printf (
      "The entries in the directory [%s] are listed as follows:\n", @(dirname)
    ) // end of [val]
//
    prval pfu = unit_v ()
    val () = let
      var !p_f = @lam (
        pfu: !unit_v | p_ent: &direntptr_gc
      ) : void =<clo>
        $effmask_all (print_direntptr (p_ent); print_newline ())
      // end of [@lam]
    in
      array_ptr_foreach_vclo<direntptr_gc> (pfu | !p_arr, !p_f, nent_sz)
    end // end of [val]
    prval unit_v () = pfu
//    
    val () = array_ptr_clear_fun<direntptr_gc> (
      !p_arr, nent_sz, lam p =<fun> ptr_free {dirent} (p.0, p.1 | p.2)
    ) // end of [val]
//
  in
    printf ("There are %i entries in the directory [%s]\n", @(nent, dirname))
  end else let
    prval None_v () = pfopt_dir
  in
    printf ("*** ERROR ***: the directory [%s] cannot be opened\n", @(dirname))
  end // end of [if]
end // end of [ls]

(* ****** ****** *)

fn ls2 (dirname: string): void = let
  val (pfopt_dir | p_dir) = opendir_err (dirname)
in
  if p_dir > null then let
    prval Some_v (pf_dir) = pfopt_dir    
    val ents = direntptr_stream_vt_make_DIR (pf_dir | p_dir)
    val nams = stream_vt_map_fun (ents, f) where {
      fun f (
        p: &direntptr_gc >> direntptr_gc?
      ) :<!laz> strptr1 = x1 where {
        prval pfent = p.1
        val (fpf_x | x) = dirent_get_d_name !(p.2)
        prval () = p.1 := pfent
        val x1 = strptr_dup (x)
        prval () = fpf_x (x)
        val () = ptr_free {dirent} (p.0, p.1 | p.2)
      } // end of [fun f]
    } // end of [nams]
    val (n, nams) = list_vt_of_stream_vt<strptr1> (nams)
    val nams = list_vt_mergesort (nams, !p_cmp) where {
      var !p_cmp = @lam (x1: &strptr1, x2: &strptr1): Sgn =<clo>
        compare ($UN.castvwtp1 {string} (x1), $UN.castvwtp1 {string} (x2))
      // end of [p_cmp]
    } // end of [val]
    val () = loop (nams) where {
      fun loop (nams: List_vt (strptr1)): void =
        case+ nams of
        | ~list_vt_cons (nam, nams) => loop (nams) where {
            val () = print_strptr (nam); val () = print_newline (); val () = strptr_free (nam)
          }  // end of [string]
        | ~list_vt_nil () => ()
    } // end of [val]
    val () = printf ("There is [%i] entries in total.\n", @(n))
  in
    // nothing
  end else let
    prval None_v () = pfopt_dir
  in
    printf ("*** ERROR ***: the directory [%s] cannot be opened\n", @(dirname))
  end // end of [if]
end // end of [ls2]

(* ****** ****** *)

// listing all of the files in a given directory
implement main (argc, argv) = let
  val () = if (argc < 2) then prerr_usage (argv.[0])
  val () = assert (argc >= 2); val dirname = argv.[1]
  var i: Nat // uninitialized
  val () = for (i := 1; i < argc; i := i+1) ls (argv.[i])
  val () = for (i := 1; i < argc; i := i+1) ls2 (argv.[i])
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [libc_dirent.dats] *)
