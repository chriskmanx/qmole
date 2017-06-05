(*
** some testing code for functions declared in
** libc/SATS/dlfcn.sats
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//

(* ****** ****** *)

staload "libc/SATS/dlfcn.sats"

(* ****** ****** *)

typedef ftrig_t = (double) -> double
fun ftrig_get {l:addr} .<>. (
    pf_lib: !dlopen_v l | p_lib: ptr l, name: string
  ) : ftrig_t = let
  val (fpf_msg | msg) = dlerror () // clearing any existing error
  prval () = fpf_msg (msg)
  val ftrig = dlsym (pf_lib | p_lib, name)
  val (fpf_msg | msg) = dlerror () // see if there is any error
  val p_msg = ptr_of_strptr (msg)
  prval () = fpf_msg (msg)
  val () = assert_errmsg (p_msg = null, #LOCATION)
in
  __cast (ftrig) where { extern castfn __cast (x: ptr):<> ftrig_t }
end // end of [ftrig_get]

(* ****** ****** *)

#define PI 3.1415926535898

implement main () = () where {
  val (pf_lib | p_lib) = dlopen_exn ("libm.so", RTLD_LAZY)
//
  val fsin = ftrig_get (pf_lib | p_lib, "sin")
  val sin30 = fsin (PI / 6)
  val () = (print "sin30 = "; print sin30; print_newline ())
  val sin45 = fsin (PI / 4)
  val () = (print "sin45 = "; print sin45; print_newline ())
  val sin60 = fsin (PI / 3)
  val () = (print "sin60 = "; print sin60; print_newline ())
//
  val fcos = ftrig_get (pf_lib | p_lib, "cos")
  val cos30 = fcos (PI / 6)
  val () = (print "cos30 = "; print cos30; print_newline ())
  val cos45 = fcos (PI / 4)
  val () = (print "cos45 = "; print cos45; print_newline ())
  val cos60 = fcos (PI / 3)
  val () = (print "cos60 = "; print cos60; print_newline ())
//
(*
  val () = dlclose_exn (pf_lib | p_lib)
*)
  val err = dlclose (pf_lib | p_lib)
  val () = assert_errmsg (err = 0, #LOCATION)
  prval None_v () = pf_lib
} // end of [main]

(* ****** ****** *)

(* end of [libc_dlfcn.dats] *)
