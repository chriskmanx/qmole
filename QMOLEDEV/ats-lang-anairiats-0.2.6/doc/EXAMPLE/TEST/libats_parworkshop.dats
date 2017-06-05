(*
** some testing code for functions declared in
** libats/SATS/parworkshop.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March, 2010
//

(* ****** ****** *)

staload UNISTD = "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "libc/SATS/pthread.sats"
staload "libats/SATS/parworkshop.sats"
staload _(*anon*) = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

#define QSZ 1 // extreme testing condition!

(* ****** ****** *)

dynload "libats/DATS/parworkshop.dats"

(* ****** ****** *)

implement main () = () where {
  typedef work = int
  val fwork = lam {l:addr} (
    ws: !WORKSHOPptr (work, l), x: &work
  ) : int =<fun> $effmask_all let
    val tid = pthread_self ()
    val tid = int_of_pthread (tid)
    val () = (print "tid = "; print tid)
    val () = (print ": x = "; print x; print_newline ())
    // val () = $UNISTD.usleep (1)
  in
    x + 1
  end // end of [val]
//
  val ws = workshop_make<work> (QSZ, fwork)
//
  val status = workshop_add_worker (ws)
  val () = (print "status = "; print status; print_newline ())
  val nworker = workshop_get_nworker (ws)
  val () = (print "nworker = "; print nworker; print_newline ())
//
  val status = workshop_add_worker (ws)
  val () = (print "status = "; print status; print_newline ())
  val nworker = workshop_get_nworker (ws)
  val () = (print "nworker = "; print nworker; print_newline ())
//
  val status = workshop_add_worker (ws)
  val () = (print "status = "; print status; print_newline ())
  val nworker = workshop_get_nworker (ws)
  val () = (print "nworker = "; print nworker; print_newline ())
//
  var i: Nat = 0
  val () = while (i < 10) let
    val () = workshop_insert_work (ws, i)
  in
    i := i + 1
  end // end of [val]
//
  val () = workshop_wait_blocked_all (ws)
  val nblocked = workshop_get_nblocked (ws)
  val () = (print "nblocked = "; print nblocked; print_newline ())
//
  val () = workshop_insert_work (ws, ~2)
  val () = workshop_insert_work (ws, ~2)
  val () = workshop_insert_work (ws, ~2)
//
  val () = begin
    print "workshop_wait_paused_all(beg)"; print_newline ()
  end // end of [val]
  val () = workshop_wait_paused_all (ws)
  val () = begin
    print "workshop_wait_paused_all(aft)"; print_newline ()
  end // end of [val]
//
  val npaused = workshop_get_npaused (ws)
  val () = (print "npaused = "; print npaused; print_newline ())
//
  val () = workshop_resume_paused_all (ws)
//
  val () = workshop_insert_work (ws, ~1)
  val () = workshop_insert_work (ws, ~1)
  val () = workshop_insert_work (ws, ~1)
//
  val () = workshop_free (ws)
} // end of [main]

(* ****** ****** *)

(* end of [libats_parworkshop.dats] *)
 