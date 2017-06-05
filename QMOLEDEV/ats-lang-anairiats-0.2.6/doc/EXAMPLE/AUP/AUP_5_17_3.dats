//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 332 - 338
// section 5.17.3: Thread Synchronization (Mutexes)
//
(* ****** ****** *)

staload "libc/SATS/stdlib.sats" // EXIT_FAILURE
staload "libc/SATS/unistd.sats"
staload "libc/SATS/pthread.sats"

(* ****** ****** *)

%{^
static pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER ;
#define lock_theX() pthread_mutex_lock(&mtx)
#define unlock_theX() pthread_mutex_unlock(&mtx)
%} // end of [%{^]
var theX: lint = 0L
val p_theX = &theX
extern
fun lock_theX
  () : (lint @ theX | void) = "mac#lock_theX"
extern
fun unlock_theX
  (pf: lint @ theX | (*none*)): void = "mac#unlock_theX"

(* ****** ****** *)

fun theX_inc_and_get
  (delta: lint): lint = let
  val (pf | ()) = lock_theX ()
  val x = !p_theX + delta
  val () = !p_theX := x
  val () = unlock_theX (pf | (*none*))
in
  x // return value
end // end of [theX_inc_and_get]

(* ****** ****** *)

fun thread_func
  (arg: ptr): ptr = let
  val arg = (intptr_of)(arg)
  val arg = lint_of_intptr(arg)
  val () = while (true) let
    val x = theX_inc_and_get (0L)
  in
    if x < arg then let
      val () = printf ("Thread 2 says %ld\n", @(theX_inc_and_get(1L)))
      val _leftover = sleep (1)
    in
      // nothing
    end else break
  end // end of [val]
  val x = theX_inc_and_get (0L)
in
  (ptr_of_intptr)(intptr_of_lint(x))
end // end of [thread_func]

(* ****** ****** *)

implement main () = () where {
  var tid: pthread_t
  val () = assert_errmsg (sizeof<lint> <= sizeof<ptr>, #LOCATION)
  val (pf, fpf | p) = __assert (null) where {
    extern castfn __assert (x: ptr null)
      :<> (pthread_attr_t @ null, pthread_attr_t @ null -<lin,prf> void | ptr null)
  } // end of [prval]
  val _err = pthread_create (tid, !p, thread_func, (ptr_of_intptr)(intptr_of(6)))
  prval () = fpf (pf)
  val () = while (theX_inc_and_get(0L) < 10L) let
    val () = printf ("Thread 1 says %ld\n", @(theX_inc_and_get(1L)))
    val _leftover = sleep (1)
  in
    // nothing
  end // end of [val]
  var status: ptr = null
  val () = if (pthread_join (tid, status) > 0) then exit (EXIT_FAILURE)
  val rtn = lint_of_intptr(intptr_of(status))
  val () = printf ("Thread 2's exit status is %ld\n", @(rtn))
  val () = exit (EXIT_SUCCESS)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_5_17_3.dats] *)
