(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010
**
*)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload LQ = "libats/SATS/linqueue_arr.sats"
staload _(*anon*) = "libats/DATS/linqueue_arr.dats"
staload _(*anon*) =
  "libats/ngc/DATS/deque_arr.dats" // implements [queue]
// end of [staload]

(* ****** ****** *)

staload PT = "libc/SATS/pthread.sats"
stadef mutex = $PT.mutex_vt
stadef cond = $PT.cond_vt

(* ****** ****** *)

staload "libats/SATS/parworkshop.sats"

(* ****** ****** *)

%{^

ats_ptr_type
atslib_parworkshop_workshop_make_tsz (
  ats_size_type qsz, ats_fun_ptr_type fwork, ats_size_type tsz
) {
  atslib_parworkshop_WORKSHOP *p ;
//
  p = ATS_MALLOC (sizeof(atslib_parworkshop_WORKSHOP)) ;
  pthread_mutex_init (&p->WSmut, (pthread_mutexattr_t*)0) ;
//
  atslib_linqueue_arr_queue_initialize_tsz (&p->WQ, qsz, tsz) ;
//
  pthread_cond_init (&p->WQemp, (pthread_condattr_t*)0) ;
  pthread_cond_init (&p->WQful, (pthread_condattr_t*)0) ;
  p->nworker = 0 ;
  pthread_cond_init (&p->WSisz, (pthread_condattr_t*)0) ;
  p->npaused = 0 ;
  pthread_cond_init (&p->WSpaused, (pthread_condattr_t*)0) ;
  pthread_cond_init (&p->WSequ1, (pthread_condattr_t*)0) ;
  p->nblocked = 0 ;
  pthread_cond_init (&p->WSequ2, (pthread_condattr_t*)0) ;
  p->fwork = fwork ;
  p->refcount = 0 ;
//
  return (p) ;
//
} // end of [atslib_parworkshop_workshop_make_tsz]

%} // end of [%{^]

(* ****** ****** *)

%{^

ats_void_type
atslib_parworkshop_workshop_free_lin
  (ats_ptr_type p0, ats_int_type lin) {
//
  atslib_parworkshop_WORKSHOP *p = p0 ;
  atslib_linqueue_arr_QUEUE *p_WQ = &(p->WQ) ;
//
  pthread_mutex_lock (&p->WSmut);
//
  while (1) {
    if (p->nworker != 0) {
      pthread_cond_wait (&p->WSisz, &p->WSmut) ;
    } else {
      break ;
    } // end of [if]
  } // end of [while]
//
  // [p->WSmut] is held at this point
  if (lin && p_WQ->nitm != 0) {
    fprintf (stderr
    , "exit(ATS): [atslib_parworkshop_workshop_free_lin]: work queue is not empty\n"
    ) ; exit (1) ;
  } // end of [if]
//
  atslib_linqueue_arr_queue_uninitialize (p_WQ) ;
//
  pthread_mutex_destroy (&p->WSmut) ;
  pthread_cond_destroy (&p->WQemp) ;
  pthread_cond_destroy (&p->WQful) ;
  pthread_cond_destroy (&p->WSisz) ;
  pthread_cond_destroy (&p->WSpaused) ;
  pthread_cond_destroy (&p->WSequ1) ;
  pthread_cond_destroy (&p->WSequ2) ;
//
  ATS_FREE(p) ;
//
  return ;
} // end of [atslib_parworkshop_workshop_free_lin]

ats_void_type
atslib_parworkshop_workshop_free
  (ats_ptr_type p0) { atslib_parworkshop_workshop_free_lin (p0, 0) ; return ; }
// end of [atslib_parworkshop_workshop_free]

ats_void_type
atslib_parworkshop_workshop_free_vt_exn
  (ats_ptr_type p0) { atslib_parworkshop_workshop_free_lin (p0, 1) ; return ; }
// end of [atslib_parworkshop_workshop_free]

%} // end of [%{^]

(* ****** ****** *)

implement{a}
workshop_make (qsz, f) =
  workshop_make_tsz (qsz, f, sizeof<a>)
// end of [workshop_make]

(* ****** ****** *)

extern
fun workshop_get_fwork
  {a:viewt@ype} {l:addr} (
  ws: !WORKSHOPptr (a, l)
) :<> (
  !WORKSHOPptr (a, l), &a >> a?
) -<fun> int // HX: a function is returned!
  = "mac#atslib_parworkshop_workshop_get_fwork"
// end of [workshop_get_fwork]

(* ****** ****** *)

extern
fun workshop_nworker_inc
  {a:viewt@ype} {l:addr} (
  ws: !WORKSHOPptr (a, l)
) :<> void
  = "mac#atslib_parworkshop_workshop_nworker_inc"
// end of [workshop_nworker_inc]

(* ****** ****** *)

viewtypedef
WORKSHOP (a:viewt@ype) =
  $extype_struct "atslib_parworkshop_WORKSHOP" of {
//
// WSmut = $PT.mutex_vt
//
  WQ = $LQ.QUEUE0 (a)
, WQemp = $PT.cond_vt
, WQful = $PT.cond_vt
, nworker = int // number of workers affiliated with the workshop
, WSisz = $PT.cond_vt // nworker = 0
, npaused = int // number of workers paused
, WSpaused = $PT.cond_vt
, WSequ1 = $PT.cond_vt // npaused = nworker
, nblocked = int // number of workers blocked
, WSequ2 = $PT.cond_vt // nblocked = nworker
, fwork = {l:addr} (!WORKSHOPptr (a, l), &a >> a?) -<cloref> int
, refcount = int
} // end of [WORKSHOP]

(* ****** ****** *)

extern
fun workshop_acquire
  {a:viewt@ype} {l:addr} (
  ws: !WORKSHOPptr (a, l)
) :<> (
  WORKSHOP (a) @ l | ptr l
) = "mac#atslib_parworkshop_workshop_acquire"

extern
fun workshop_release
  {a:viewt@ype} {l:addr} (
  pf: WORKSHOP (a) @ l | p_ws: ptr l
) :<> void
  = "mac#atslib_parworkshop_workshop_release"
// end of [workshop_release]

(* ****** ****** *)

extern
fun workshop_get_WSmut
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_mut:addr] (
  mutex (WORKSHOP a @ l) @ l_mut
, minus (ptr l, mutex (WORKSHOP a @ l) @ l_mut)
| ptr l_mut
) = "mac#atslib_parworkshop_workshop_get_WSmut"

extern
fun workshop_get_WQemp
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_emp:addr] (
  cond @ l_emp
, minus (ptr l, cond @ l_emp)
| ptr l_emp
) = "mac#atslib_parworkshop_workshop_get_WQemp"

extern
fun workshop_get_WQful
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_ful:addr] (
  cond @ l_ful
, minus (ptr l, cond @ l_ful)
| ptr l_ful
) = "mac#atslib_parworkshop_workshop_get_WQful"

(* ****** ****** *)

extern
fun workshop_get_WSisz
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_isz:addr] (
  cond @ l_isz
, minus (ptr l, cond @ l_isz)
| ptr l_isz
) = "mac#atslib_parworkshop_workshop_get_WSisz"

extern
fun workshop_get_WSpaused
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_pau:addr] (
  cond @ l_pau
, minus (ptr l, cond @ l_pau)
| ptr l_pau
) = "mac#atslib_parworkshop_workshop_get_WSpaused"

extern fun
workshop_get_WSequ1
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_equ:addr] (
  cond @ l_equ
, minus (ptr l, cond @ l_equ)
| ptr l_equ
) = "mac#atslib_parworkshop_workshop_get_WSequ1"

extern fun
workshop_get_WSequ2
  {a:viewt@ype}
  {l:addr} (
  pf: !WORKSHOP (a) @ l | p: ptr l
) :<> [l_equ:addr] (
  cond @ l_equ
, minus (ptr l, cond @ l_equ)
| ptr l_equ
) = "mac#atslib_parworkshop_workshop_get_WSequ2"

(* ****** ****** *)

implement
workshop_get_nworker
  (ws) = nworker where {
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val nworker = p_ws->nworker
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_get_nworker]

implement
workshop_get_npaused
  (ws) = npaused where {
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val npaused = p_ws->npaused
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_get_npaused]

implement
workshop_get_nblocked
  (ws) = nblocked where {
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val nblocked = p_ws->nblocked
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_get_nblocked]

(* ****** ****** *)

extern
fun workshop_nworker_inc
  {a:viewt@ype} {l:addr} (ws: !WORKSHOPptr (a, l)):<> void
// end of [workshop_nworker_inc]

implement
workshop_nworker_inc (ws) = () where {
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val () = p_ws->nworker := p_ws->nworker + 1
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_nworker_inc]

(* ****** ****** *)

extern
fun workshop_ref
  {a:viewt@ype} {l:addr}
  (ws: !WORKSHOPptr (a, l)):<!ref> WORKSHOPptr (a, l)
// end of [workshop_ref]

implement
workshop_ref {a} {l} (ws) = ws where {
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val () = p_ws->refcount := p_ws->refcount + 1
  val () = workshop_release (pf_ws | p_ws)
  val ws = __cast (ws) where {
    extern castfn __cast (ws: !WORKSHOPptr (a, l)):<> WORKSHOPptr (a, l)
  } // end of [val]
} // end of [workshop_ref]

(* ****** ****** *)

extern
fun workshop_unref
  {a:viewt@ype} {l:addr} (ws: WORKSHOPptr (a, l)):<!ref> void
// end of [workshop_unref]

implement
workshop_unref {a} {l} (ws) = () where {
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val () = p_ws->refcount := p_ws->refcount - 1
  val () = workshop_release (pf_ws | p_ws)
  val _ptr = __cast (ws) where {
    extern castfn __cast (ws: WORKSHOPptr (a, l)):<> ptr l
  } // end of [val]
} // end of [workshop_unref]

(* ****** ****** *)

//
// HX:
// return status >  0 : continue
// return status =  0 : quit
// return status = ~1 : pause
//
implement{a}
workshop_add_worker
  {l} (ws) = err where {
  viewtypedef env = WORKSHOPptr (a, l)
  fun worker (ws: env): void = let
    var wk = workshop_remove_work<a> (ws)
    val fwork = workshop_get_fwork (ws)
    val status = fwork (ws, wk)
  in
    case+ 0 of
    | _ when status > 0 => worker (ws)
    | _ when (status = 0) => let // status = 0
        val (pf_ws | p_ws) = workshop_acquire (ws)
        val nworker1 = p_ws->nworker - 1
        val () = p_ws->nworker := nworker1
        val () = if (nworker1 = 0) then let
          val _err = $PT.pthread_cond_broadcast (p_ws->WSisz) in (*none*)
        end // end of [val]
        val () = if (nworker1 = p_ws->npaused) then let
          val _err = $PT.pthread_cond_broadcast (p_ws->WSequ1) in (*none*)
        end // end of [val]
        val () = if (nworker1 = p_ws->nblocked) then let
          val _err = $PT.pthread_cond_broadcast (p_ws->WSequ2) in (*none*)
        end // end of [val]
        val () = workshop_release (pf_ws | p_ws)
        val () = workshop_unref (ws)
      in
        // the pthread exits normally
      end // end of [_]
    | _ => let // for handling uncommon requests
        viewdef V_ws = WORKSHOP a @ l
        val (pf_ws | p_ws) = workshop_acquire (ws)
        val npaused1 = p_ws->npaused + 1
        val () = p_ws->npaused := npaused1
        val nworker = p_ws->nworker
        val () = if (npaused1 = nworker) then let
          val _err = $PT.pthread_cond_broadcast (p_ws->WSequ1) in (*none*)
        end // end of [val]
        val [l_mut:addr] (pf_mut, fpf_mut | p_mut) =
          workshop_get_WSmut {a} {l} (pf_ws | p_ws)
        val [l_pau:addr] (pf_pau, fpf_pau | p_pau) = 
          workshop_get_WSpaused {a} {l} (pf_ws | p_ws)
        val _err = $PT.pthread_cond_wait {V_ws} (pf_ws | !p_pau, !p_mut)
        val p1_ws = p_ws
        prval () = minus_addback (fpf_mut, pf_mut | p1_ws)
        prval () = minus_addback (fpf_pau, pf_pau | p1_ws)
        val npaused = p_ws->npaused
        val () = p_ws->npaused := npaused - 1
        val () = workshop_release (pf_ws | p_ws)
      in
        worker (ws)
      end // end of [status < 0]
  end // end of [val]
  val ws_new = workshop_ref (ws)
  val err = $PT.pthread_create_detached {env} (worker, ws_new)
  val () = if err > 0 then let
    // no new worker is added
    prval () = opt_unsome {env} (ws_new); val () = workshop_unref (ws_new)
  in
    // (*nothing*)
  end else let
    // a new worker is added successully
    val () = workshop_nworker_inc (ws)
    prval () = opt_unnone {env} (ws_new)
    prval () = cleanup_top {env} (ws_new)
  in
    // (*nothing*)
  end // end of [val]
} // end of [workshop_add_worker]

(* ****** ****** *)

implement{a}
workshop_add_nworker
  {l} {n} (ws, n) =
  loop (ws, n, 0, 0(*err*)) where {
  fun loop {i:nat | i <= n} .<n-i>. (
    ws: !WORKSHOPptr (a, l), n: int n, i: int i, err0: int
  ) : int =
    if i < n then let
      val err = workshop_add_worker (ws)
    in
      loop (ws, n, i+1, err0 + err)
    end else err0 // end of [if]
  // end of [loop]
} // end of [workshop_add_nworker]

(* ****** ****** *)

implement{a}
workshop_insert_work {l} (ws, wk) = let
(*
  val () = begin
    print "workshop_insert_work: start"; print_newline ()
  end // end of [val]
*)
  val (pf_ws | p_ws) = workshop_acquire (ws)
  viewdef V_ws = WORKSHOP a @ l
  fun loop (
    pf_ws: V_ws | p_ws: ptr l, wk: a
  ) : void = let
    val p1_ws = p_ws
    val isful = $LQ.queue_is_full {a} (p_ws->WQ)
  in    
    if isful then let
      val [l_mut:addr] (pf_mut, fpf_mut | p_mut) =
        workshop_get_WSmut {a} {l} (pf_ws | p_ws)
      val [l_ful:addr] (pf_ful, fpf_ful | p_ful) = 
        workshop_get_WQful {a} {l} (pf_ws | p_ws)
      viewdef V_mut = mutex (WORKSHOP a @ l) @ l_mut
      val _err = $PT.pthread_cond_wait {V_ws} (pf_ws | !p_ful, !p_mut)
      prval () = minus_addback (fpf_mut, pf_mut | p1_ws)
      prval () = minus_addback (fpf_ful, pf_ful | p1_ws)
    in
      loop (pf_ws | p_ws, wk)
    end else let
      val isemp =
        $LQ.queue_is_empty {a} (p_ws->WQ)
      // end of [val]
      val () = $LQ.queue_insert<a> (p_ws->WQ, wk)
      val () = if isemp then let
//
        val () = p_ws->nblocked := 0
//
        val [l_emp:addr] (pf_emp, fpf_emp | p_emp) = 
          workshop_get_WQemp {a} {l} (pf_ws | p_ws)
        val _err = $PT.pthread_cond_broadcast (!p_emp)
        prval () = minus_addback (fpf_emp, pf_emp | p_ws)
      in
        // nothing
      end // end of [val]
      val () = workshop_release (pf_ws | p_ws)
    in
      // nothing
    end // end of [if]
  end (* end of [loop] *)
in
  loop (pf_ws | p_ws, wk)
end // end of [workshop_insert_work]

(* ****** ****** *)

implement{a}
workshop_remove_work {l} (ws) = let
  val (pf_ws | p_ws) = workshop_acquire (ws)
  viewdef V_ws = WORKSHOP a @ l
  fun loop
    (pf_ws: V_ws | p_ws: ptr l): a = let
    val p1_ws = p_ws
    val isemp = $LQ.queue_is_empty {a} (p_ws->WQ)
  in    
    if isemp then let
//
      val nblock1 = p_ws->nblocked + 1
      val () = p_ws->nblocked := nblock1
      val () = if (nblock1 = p_ws->nworker) then let
        val _err = $PT.pthread_cond_broadcast (p_ws->WSequ2) in (*none*)
      end // end of [val]
//
      val [l_mut:addr] (
        pf_mut, fpf_mut | p_mut
      ) =
        workshop_get_WSmut {a} {l} (pf_ws | p_ws)
      val [l_emp:addr] (
        pf_emp, fpf_emp | p_emp
      ) = 
        workshop_get_WQemp {a} {l} (pf_ws | p_ws)
      viewdef V_mut = mutex (WORKSHOP a @ l) @ l_mut
      val _err = $PT.pthread_cond_wait {V_ws} (pf_ws | !p_emp, !p_mut)
      prval () = minus_addback (fpf_mut, pf_mut | p1_ws)
      prval () = minus_addback (fpf_emp, pf_emp | p1_ws)
    in
      loop (pf_ws | p_ws)
    end else let
      val isful =
        $LQ.queue_is_full {a} (p_ws->WQ)
      // end of [val]
      val wk = $LQ.queue_remove<a> (p_ws->WQ)
      val () = if isful then let
        val [l_ful:addr] (pf_ful, fpf_ful | p_ful) = 
          workshop_get_WQful {a} {l} (pf_ws | p_ws)
        val _err = $PT.pthread_cond_broadcast (!p_ful)
        prval () = minus_addback (fpf_ful, pf_ful | p_ws)
      in
        // nothing
      end // end of [val]
      val () = workshop_release (pf_ws | p_ws)
    in
      wk (* return value *)
    end // end of [if]
  end (* end of [loop] *)
in
  loop (pf_ws | p_ws)
end // end of [workshop_remove_work]

(* ****** ****** *)

implement
workshop_wait_quit_all
  {a} {l} (ws) = () where {
  viewdef V_ws = WORKSHOP a @ l
  val (pf_ws | p_ws) = workshop_acquire (ws)
  fun loop (
    pf_ws: !V_ws | p_ws: ptr l
  ) : void = let
    val nworker = p_ws->nworker
  in
    if nworker > 0 then let
      val (pf_mut, fpf_mut | p_mut) = workshop_get_WSmut (pf_ws | p_ws)
      val (pf_isz, fpf_isz | p_isz) = workshop_get_WSisz (pf_ws | p_ws)
      val _err = $PT.pthread_cond_wait {V_ws} (pf_ws | !p_isz, !p_mut)
      prval () = minus_addback (fpf_mut, pf_mut | p_ws)
      prval () = minus_addback (fpf_isz, pf_isz | p_ws)
    in
      loop (pf_ws | p_ws)
    end else () // end of [if]
  end // end of [loop]
  val () = loop (pf_ws | p_ws)
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_wait_quit_all]

implement
workshop_wait_paused_all
  {a} {l} (ws) = () where {
  viewdef V_ws = WORKSHOP a @ l
  val (pf_ws | p_ws) = workshop_acquire (ws)
  fun loop (
    pf_ws: !V_ws | p_ws: ptr l
  ) : void = let
    val nworker = p_ws->nworker
    val npaused = p_ws->npaused
  in
    if nworker > npaused then let
      val (pf_mut, fpf_mut | p_mut) = workshop_get_WSmut (pf_ws | p_ws)
      val (pf_equ, fpf_equ | p_equ) = workshop_get_WSequ1 (pf_ws | p_ws)
      val _err = $PT.pthread_cond_wait {V_ws} (pf_ws | !p_equ, !p_mut)
      prval () = minus_addback (fpf_mut, pf_mut | p_ws)
      prval () = minus_addback (fpf_equ, pf_equ | p_ws)
    in
      loop (pf_ws | p_ws)
    end else () // end of [if]
  end // end of [loop]
  val () = loop (pf_ws | p_ws)
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_wait_paused_all]

implement
workshop_resume_paused_all
  {a} {l} (ws) = () where {
  viewdef V_ws = WORKSHOP a @ l
  val (pf_ws | p_ws) = workshop_acquire (ws)
  val _err = $PT.pthread_cond_broadcast (p_ws->WSpaused)
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_wait_paused_all]

(* ****** ****** *)

implement
workshop_wait_blocked_all
  {a} {l} (ws) = () where {
  viewdef V_ws = WORKSHOP a @ l
  val (pf_ws | p_ws) = workshop_acquire (ws)
  fun loop (
    pf_ws: !V_ws | p_ws: ptr l
  ) : void = let
    val nworker = p_ws->nworker
    val nblocked = p_ws->nblocked
  in
    if nworker > nblocked then let
      val (pf_mut, fpf_mut | p_mut) = workshop_get_WSmut (pf_ws | p_ws)
      val (pf_equ, fpf_equ | p_equ) = workshop_get_WSequ2 (pf_ws | p_ws)
      val _err = $PT.pthread_cond_wait {V_ws} (pf_ws | !p_equ, !p_mut)
      prval () = minus_addback (fpf_mut, pf_mut | p_ws)
      prval () = minus_addback (fpf_equ, pf_equ | p_ws)
    in
      loop (pf_ws | p_ws)
    end else () // end of [if]
  end // end of [loop]
  val () = loop (pf_ws | p_ws)
  val () = workshop_release (pf_ws | p_ws)
} // end of [workshop_wait_blocked_all]

(* ****** ****** *)

(* end of [parworkshop.dats] *)
