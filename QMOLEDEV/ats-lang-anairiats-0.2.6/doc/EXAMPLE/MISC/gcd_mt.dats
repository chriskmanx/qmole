//
// A multithreaded implementation of GCD in coroutine-style
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March 10, 2008
//

(* ****** ****** *)

staload "libc/SATS/pthread.sats"
staload _(*anonymous*) = "libc/DATS/pthread.dats"
staload "libc/SATS/pthread_uplock.sats"

(* ****** ****** *)

abstype pthread_mutexref_viewt0ype_type (a: viewt@ype)

stadef mutexref_t = pthread_mutexref_viewt0ype_type

absview pthread_mutexref_unlock_ticket_viewt0ype_addr_view
  (viewt@ype, addr)

stadef mutexref_unlock_ticket = 
  pthread_mutexref_unlock_ticket_viewt0ype_addr_view

extern
fun{a:viewt@ype} pthread_mutexref_create (x: a): mutexref_t a

extern
fun pthread_mutexref_create_tsz {a:viewt@ype} {l:addr}
  (pf: !a @ l >> a? @ l | p: ptr l, tsz: sizeof_t a): mutexref_t a
  = "atslib_pthread_mutexref_create_tsz"

extern fun pthread_mutexref_lock
  {a:viewt@ype} (mtxrf: mutexref_t a)
  : [l:addr] (mutexref_unlock_ticket (a, l), a @ l | ptr l)
  = "atslib_pthread_mutexref_lock"

extern fun pthread_mutexref_unlock {a:viewt@ype} {l:addr}
  (_tick: mutexref_unlock_ticket (a, l), _at: a @ l | p: ptr l): void
  = "atslib_pthread_mutexref_unlock"

(* ****** ****** *)

implement{a}
pthread_mutexref_create (x) = let
  var x = x
in
  pthread_mutexref_create_tsz {a} (view@ (x) | &x, sizeof<a>)
end // end of [pthread_mutexref_create]

%{^

//
// HX-2010-03-28: this style cannot safely support GC
//
typedef struct {
  pthread_mutex_t mutex ; void* value[0] ;
} pthread_mutexref_struct ;

/* ****** ****** */

static inline
ats_ptr_type
atslib_pthread_mutexref_create_tsz
  (ats_ptr_type p_x, ats_size_type tsz) {
  pthread_mutexref_struct *p ;
  p = ATS_MALLOC(sizeof (pthread_mutexref_struct) + tsz) ;
  pthread_mutex_init (&(p->mutex), NULL) ;
  memcpy (&(p->value), p_x, (size_t)tsz) ;
  return p ;
} // end of [atslib_pthread_mutexref_create_tsz]

static inline
ats_ptr_type
atslib_pthread_mutexref_lock
  (ats_ptr_type p0) {
  pthread_mutexref_struct *p = p0 ;
  pthread_mutex_lock (&(p->mutex)) ;
  return &(p->value) ;
} // end of [atslib_pthread_mutexref_lock]

static inline
ats_void_type
atslib_pthread_mutexref_unlock
  (ats_ptr_type p_value) {
  pthread_mutex_unlock (
    (pthread_mutex_t*)((char*)p_value - sizeof(pthread_mutexref_struct))
  ) ;
  return ;
} // end of [atslib_pthread_mutexref_unlock]

%} // end of [%{^]

(* ****** ****** *)

extern prfun gcd_lemma0 {x,y:int} ():<prf> [z:nat] GCD (x, y, z)
extern prfun gcd_lemma1 {x,y,z:int} (pf: GCD (x, y, z)):<prf> GCD (x - y, y, z)
extern prfun gcd_lemma2 {x,y,z:int} (pf: GCD (x, y, z)):<prf> GCD (x, y - x, z)
extern prfun gcd_lemma3 {x:nat;z:int} (pf: GCD (x, x, z)):<prf> [x == z] void

//

viewdef gcd_v (a:addr, b:addr, z:int) =
  [x,y:pos] @(int x @ a, int y @ b, GCD (x, y, z))

//

viewtypedef upticket0 = upticket (void)

fun gcd_flag {a,b:addr} {z:int}
  (pf: !gcd_v (a, b, z) | flag: int, a: ptr a, b: ptr b): bool = let
  prval @(pfa, pfb, pfgcd) = pf
  val x = !a and y = !b
in
  if x > y then begin
    if flag > 0 then let
      val () = (!a := x - y)
      prval () = (pf := @(pfa, pfb, gcd_lemma1 pfgcd))
    in
      gcd_flag (pf | flag, a, b)
    end else begin
      pf := @(pfa, pfb, pfgcd); false // not done
    end
  end else if x < y then begin
    if flag < 0 then let
      val () = (!b := y - x)
      prval () = (pf := @(pfa, pfb, gcd_lemma2 pfgcd))
    in
      gcd_flag (pf | flag, a, b)
    end else begin
      pf := @(pfa, pfb, pfgcd); false // not done
    end
  end else begin
    pf := @(pfa, pfb, pfgcd); true // is done!
  end
end // end of [gcd_flag]

fun gcd_mt {x0,y0:pos} {z:int}
  (pfgcd: GCD (x0, y0, z) | x0: int x0, y0: int y0): int z = let
  var x = x0 and y = y0
  viewtypedef VT = @(gcd_v (x, y, z) | int)
  val ini = @((view@ x, view@ y, pfgcd) | 0)
  val mut = pthread_mutexref_create<VT> (ini)
  fun gcd_worker (ticket: upticket0, flag: int):<cloptr1> void = let
    val (pf_ticket, pf_at | ptr) = pthread_mutexref_lock (mut)
    val done = gcd_flag (ptr->0 | flag, &x, &y)
    val () = pthread_mutexref_unlock (pf_ticket, pf_at | ptr)
  in
    if done then begin
      pthread_upticket_upload_and_destroy (() | ticket)
    end else begin
      gcd_worker (ticket, flag)
    end
  end // end of [gcd_worker]
  val uplock1 = pthread_uplock_create ()
  val upticket1 = pthread_upticket_create {void} (uplock1)
  val () = pthread_create_detached_cloptr (llam () => gcd_worker (upticket1,  1))

  val uplock2 = pthread_uplock_create ()
  val upticket2 = pthread_upticket_create {void} (uplock2)
  val () = pthread_create_detached_cloptr (llam () => gcd_worker (upticket2, ~1))
//
  val (_(*void*) | ()) = pthread_uplock_download {void} (uplock1)
  val () = pthread_uplock_destroy (uplock1)
  val (_(*void*) | ()) = pthread_uplock_download {void} (uplock2)
  val () = pthread_uplock_destroy (uplock2)
//
  val (pf_ticket, pf_at | ptr) = pthread_mutexref_lock (mut)
  prval @(pfa, pfb, pfgcd) = ptr->0
  prval () = view@ x := pfa
  prval () = view@ y := pfb
  val z1 = x and z2 = y
  prval () = begin // this is a serious problem with persistent locks!
    discard (pf_ticket); discard (pf_at)
  end where {
    extern prval discard {v:view} (pf: v):<prf> void
  } // end of [where]
  val () = assert (z1 = z2)
  prval () = gcd_lemma3 (pfgcd)
in
  z1
end // end of [gcd_mt]

fun gcd_main {x,y: pos}
  (x: int x, y: int y): [z:nat] @(GCD (x, y, z) | int z) = let
  prval pf = gcd_lemma0 {x, y} ()
in
  @(pf | gcd_mt (pf | x, y))
end // end of [gcd_main]

//

// This example shows clearly the superiority of multicore over
// singlecore in case of busy waiting.

fn usage (cmd: string): void =
  prerrf ("Usage: %s [positive integer] [positive integer]\n", @(cmd))

implement main (argc, argv) = let
  val () = if argc < 3 then (usage (argv.[0]); exit (1))
  val () = assert (argc >= 3)
  val x = int1_of (argv.[1])
  val () = assert_errmsg_bool1 (x > 0, "The 1st integer argument is not positive.\n")
  val y = int1_of (argv.[2])
  val () = assert_errmsg_bool1 (y > 0, "The 2nd integer argument is not positive.\n")
  val @(pf | z) = gcd_main (x, y)
in
  printf (
    "The greatest common divisor of (%i, %i) is %i.\n", @(x, y, z)
  ) // end of [printf]
end // end of [main]

(* ****** ****** *)

(* end of [gcd_mt.dats] *)
