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
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

(*
// HX: this is declared in $ATSHOME/prelude/basic_dyn.sats:
// prfun verify_constraint {p:bool | p} (): [p] void // verify and add
*)
implement verify_constraint () = ()

(* ****** ****** *)
//
// HX:
// file_mode_lte_r_r and file_mode_lte_w_w
// are declared in $ATSHOME/prelude/basic_dyn.ats
//
implement file_mode_lte_r_r = file_mode_lte_refl {r} ()
implement file_mode_lte_w_w = file_mode_lte_refl {w} ()
implement file_mode_lte_rw_rw = file_mode_lte_refl {rw} ()
//
(* ****** ****** *)

implement option_v_unnone (pfopt) =
  let prval None_v () = pfopt in (*nothing *) end
implement option_v_unsome (pfopt) = let prval Some_v (pf) = pfopt in pf end

(* ****** ****** *)

implement unit_v_elim (pf) = let prval unit_v () = pf in (*nothing*) end

(* ****** ****** *)

%{^
ats_int_type
ats_crash_int () {
  return *((int*)0) ; // HX: for the purpose of debugging
} // end of [ats_crash_int]
%} // end of [%{^]
implement crash () = let
  val status = crash_int () where {
    extern fun crash_int (): int = "ats_crash_int"
  } // end of [val]
  val () = exit (status)
in
  // nothing
end // end of [crash]

(* ****** ****** *)

%{^

/*
** HX: various functions for exits
*/

ats_void_type // external
ats_exit
  (ats_int_type status) { exit(status) ; return ; }
// end of [ats_exit]

ats_void_type // external
ats_exit_errmsg (
  ats_int_type status
, ats_ptr_type errmsg
) {
  fprintf(stderr, "%s", (char*)errmsg) ; exit(status) ;
  return ; // deadcode
} /* end of [ats_exit_errmsg] */

%} // end of [%{^]

(* ****** ****** *)

%{^

/*
** HX: various functions for asserts
*/

#ifndef EXIT_SUCCESS
#define	EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

ats_void_type
atspre_assertfalse () {
//
extern void ats_crash() ;
//
  fprintf(stderr, "[assertfalse] executed\n") ;
  ats_crash() ; // HX: this is likely to cause a core dump!
  return ;
} // end of [atspre_asertfasle]

ats_void_type
atspre_assert (
  ats_bool_type assertion
) {
  if (!assertion) {
    fprintf (stderr, "exit(ATS): [assert] failed\n") ;
    exit(EXIT_FAILURE) ;
  } // end of [if]
  return ;
} /* end of [atspre_assert] */

ats_void_type
atspre_assert_errmsg (
  ats_bool_type assertion, ats_ptr_type errmsg
) {
  if (!assertion) {
    fprintf (stderr, "exit(ATS): [assert] failed: %s\n", (char*)errmsg) ;
    exit(EXIT_FAILURE) ;
  } // end of [if]
  return ;
} /* end of [atspre_assert_errmsg] */

%} // end of [%{^]

(* ****** ****** *)

(* end of [basics.dats] *)
