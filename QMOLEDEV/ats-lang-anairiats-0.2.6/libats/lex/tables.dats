(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Anairiats - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (* hwxi AT cs DOT bu DOT edu *)
// Time: July 2007
//

(* ****** ****** *)

staload "libats/lex/lexing.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

dataviewtype tblopt =
  | {n:nat} {l:addr}
    tblopt_some of (array_v (int16, n, l) | ptr l, int n)
  | tblopt_none

extern fun new_tbloptref_some {n:nat} {l:addr}
  (pf: array_v (int16, n, l) | p: ptr l, n: int n): ref tblopt =
  "new_tbloptref_some"

implement new_tbloptref_some (pf | p, n) = let
  val tblopt = tblopt_some (pf | p, n) in ref<tblopt> (tblopt)
end // end of [new_tbloptref_some]

(* ****** ****** *)

extern
fun table_ptr_free
  {a:viewt@ype} {n:nat} {l:addr}
  (pf: array_v (a, n, l) | p: ptr l):<> void = "table_ptr_free"
// end of [table_ptr_free]

(* ****** ****** *)

%{^
//
extern void free (void*) ;
//
ATSinline()
ats_void_type
table_ptr_free (ats_ptr_type p) { free (p) ; return ; }
//
%} // end of [%{^]

fn tbloptref_free
  (r_tblopt: ref tblopt): void = let
  val (vbox pf_tblopt | p_tblopt) = ref_get_view_ptr r_tblopt
in
  case+ !p_tblopt of
  | ~tblopt_some (pf | p, n) => begin
      table_ptr_free {int16} (pf | p); !p_tblopt := tblopt_none ()
    end // end of [tblopt_some]
  | tblopt_none () => fold@ (!p_tblopt)
end // end of [tbloptref_free]
  
(* ****** ****** *)

assume accept_table_t = ref (tblopt)
assume transition_table_t = ref (tblopt)

(* ****** ****** *)

extern fun __accept_table_make_fun
  (ntot: int, nfin: int, s: string): accept_table_t
  = "__accept_table_make_fun"

implement
__accept_table_make (ntot) =
  lam nfin => lam s => __accept_table_make_fun (ntot, nfin, s)
// end of [__accept_table_make ntot]

implement
__accept_table_free (r_tblopt): void = tbloptref_free r_tblopt
// end of [__accept_table_free]

(* ****** ****** *)

extern fun int_of_int16 (x: int16):<> int = "ats_int_of_int16"

%{^
//
ATSinline()
ats_int_type
ats_int_of_int16 (ats_int16_type i) { return i ; }
//
%} // end of [%{^]

(* ****** ****** *)

implement
accept_table_get
  (r_tblopt, nstate) = let
//
  var ans: int = (0: int)
  var err: int = (0: int)
//
  val () = let
    val (vbox pf | p_tblopt) =
      ref_get_view_ptr (r_tblopt)
    // end of [val]
  in
    case+ !p_tblopt of
    | tblopt_none () => let
        prval () = fold@ (!p_tblopt) in err := (1: int)
      end // end of [tblopt_none]
    | tblopt_some (!pf | p, n) => let
        val nstate = int1_of_int nstate // no-op cast
      in
        if nstate < 0 then begin
          err := (2: int); fold@ (!p_tblopt)
        end else if nstate >= n then begin
          err := (3: int); fold@ (!p_tblopt)
        end else let
          prval pf_v = !pf in
          ans := int_of_int16 (!p.[nstate]); !pf := pf_v; fold@ (!p_tblopt)
        end (* end of [if] *)
      end // end of [tblopt_some]
  end // end of [val]
//
in
  case+ err of
  | 1 => exit_errmsg (1, "lexing: accept_table_get: table is not available\n")
  | 2 => exit_errmsg (1, "lexing: accept_table_get: state number is illegal\n")
  | 3 => exit_errmsg (1, "lexing: accept_table_get: state number is illegal\n")
  | _ => ans
end // end of [accept_table_get]

(* ****** ****** *)

#define NBITS_PER_BYTE 8
//
// the characters with ascii from 0 to 127 and the special character -1
//
macdef CHAR_MAX_PLUS1 = 1 << 7 // 128
macdef NUMBER_OF_CHARS = CHAR_MAX_PLUS1 + 1 // 129

extern fun
__transition_table_make_fun
  (n: int, s: string): transition_table_t = "__transition_table_make_fun"
// end of [__transition_table_make_fun]

implement
__transition_table_make (n) =
  lam s => __transition_table_make_fun (n, s)

implement
__transition_table_free (r_tblopt) = tbloptref_free (r_tblopt)

implement
transition_table_get (r_tblopt, nstate, c) = let
(*
  val () = printf (
    "transition_table_get: nstate = %i and c = %i\n", @(nstate, int_of_char c)
  ) // end of [val]
*)
//
  var ans: int = (0: int)
  var err: int = (0: int)
//
  val () = let
    val (vbox pf | p_tblopt) =
      ref_get_view_ptr (r_tblopt)
    // end of [val]
  in
    case+ !p_tblopt of
    | tblopt_none () => begin
        err := (1: int); fold@ (!p_tblopt)
      end // end of [tblopt_none]
    | tblopt_some (!pf | p, n) => let
(*
        Note that [int_of_schar] rather than [int_of_char] is used.
        This change was made after Eckehard Berns (ecki@ecki.to) reported a bug
        due to [char] being treated as [unsigned char].
*)
        // [c] is treated as the null character if [c] > CHAR_MAX holds
        val c = (if c < CHAR_MAX_PLUS1 then c else 0(*null*)): int
        val c1 = c + 1
        val i = int1_of_int ((nstate - 1) * NUMBER_OF_CHARS + c1)
(*
        val () = $effmask_all begin
          printf ("transition_table_get: nstate = %i\n", @(nstate))
          printf ("transition_table_get: n = %i and i = %i\n", @(n,i))
        end // end of [val]
*)
      in
        if i < 0 then begin
          err := (2: int); fold@ (!p_tblopt)
        end else if i >= n then begin
          err := (3: int); fold@ (!p_tblopt)
        end else let
          prval pf_v = !pf
        in
          ans := int_of_int16 (!p.[i]); !pf := pf_v; fold@ (!p_tblopt)
        end (* end of [if] *)
      end // end of [tblopt_some]
  end // end of [val]
//
(*
  val () = begin
    prerr "transition_table_get: ans = "; prerr ans; prerr_newline ()
  end // end of [val]
*)
//
in
  case+ err of
  | 1 => exit_errmsg (1, "lexing: transition_table_get: table is not available\n")
  | 2 => exit_errmsg (1, "lexing: transition_table_get: state number is illegal\n")
  | 3 => exit_errmsg (1, "lexing: transition_table_get: state number is illegal\n")
  | _ => ans
end // end of [transition_table_get]

(* ****** ****** *)

%{

#define NBITS_PER_BYTE 8
#define NUMBER_OF_CHARS ((1 << NBITS_PER_BYTE - 1) + 1)

extern void *malloc (size_t bsz) ;
extern void *calloc (size_t n, size_t tsz) ;

ats_ptr_type
__accept_table_make_fun (
  ats_int_type ntot, ats_int_type nfin, ats_ptr_type s0
) {
  int i, nstate, irule, sz ;
  ats_int16_type *p0 ; ats_uchar_type *s ; ats_ptr_type res ;
//
  s = (ats_uchar_type*)s0;
/*
// [calloc] is used as only integers are to be stored; thus,
// there is no need to scan the allocated memory during GC;
// the allocated memory is freed by a call to [free]
*/
  sz = ntot + 1 ; p0 = calloc(sz, sizeof(ats_int16_type)) ;  
//
  for (i = 0 ; i < nfin ; ++i) {
    nstate = (s[0] << NBITS_PER_BYTE) + s[1] ;
    s += 2 ;
    p0[nstate] = (s[0] << NBITS_PER_BYTE) + s[1] ;
    s += 2 ; 
/*
    fprintf (stdout, "%i -> %i\n", nstate, p0[nstate]) ;
*/
  } /* end of [for] */
//
  res = new_tbloptref_some (p0, sz) ;
//
/*
  fprintf (stdout, "__accept_table_make_fun: sz = %i\n", sz);
  fprintf (stdout, "__accept_table_make_fun: ptr = %p\n", p0);
  fprintf (stdout, "__accept_table_make_fun: res = %p\n", res);
*/
  return res ;
} /* end of [__accept_table_make_fun] */

ats_ptr_type
__transition_table_make_fun (ats_int_type n, ats_ptr_type s0) {
  int i, j, c, sz;
  ats_int16_type *p0, *p ; ats_uchar_type *s ;
  ats_ptr_type res ;
//
  sz = n * NUMBER_OF_CHARS ;
/*
// [malloc] is used to allocate memory for storing integers;
// thus, there is no need to scan the allocated memory during GC;
// the allocated memory is freed by a call to [free]
*/
  p0 = malloc (sz*sizeof(ats_int16_type)) ; p = p0 ;
//
  s = (ats_uchar_type*)s0;
//
  for (i = 0 ; i < n ; ++i) {
    for (j = 0 ; j < NUMBER_OF_CHARS ; ++j) {
      *p = (s[0] << NBITS_PER_BYTE) + s[1] ;
/*
      fprintf (stdout, "__transition_table_make_fun: %i: *p = %i\n", j, *p);
*/
      s += 2 ; ++p ;
    } /* end of [for] */
  } /* end of [for] */

  res = new_tbloptref_some (p0, sz) ;
/*
  fprintf (stdout, "__transition_table_make_fun: sz = %i\n", sz);
  fprintf (stdout, "__transition_table_make_fun: ptr = %p\n", p0);
  fprintf (stdout, "__transition_table_make_fun: res = %p\n", res);
*/
  return res ;
} /* end of [__transition_table_make_fun] */

%} // end of [%{]

(* ****** ****** *)

(* end of [tables.dats] *)
