(*
** some testing code for functions declared in
** libc/SATS/sched.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//

(* ****** ****** *)

staload "libc/SATS/sched.sats"
staload TYPES = "libc/sys/SATS/types.sats"
macdef pid_t = $TYPES.pid_of_int

extern fun ncore_get (): int
implement ncore_get () = let
  var cs: cpu_set0_t // uninitialized
  prval () = cpusetinit (cs) // not a real initialization
  stavar nset: int
  val nset = cpusetsize_get (cs)
  val () = (print "nset = "; print nset; print_newline ())
  val err = sched_getaffinity ((pid_t)0, nset, cs)
  val () = assert_errmsg (nset >= 2, #LOCATION)
  var count: Nat = 0
  var i: Nat // uninitialized
  val () = for* (cs: cpu_set_t nset) =>
    (i := 0; i < 16; i := i + 1)
    if (CPU_ISSET (i, cs) > 0) then (count := count + 1)
  // end of [val]
in
  count
end // end of [ncore_get]

(* ****** ****** *)

implement main () = () where {
  val ncore = ncore_get ()
  val () = (print "ncore = "; print ncore; print_newline ())
} // end of [main]

(* ****** ****** *)

(* end of [libc_sched.dats] *)
