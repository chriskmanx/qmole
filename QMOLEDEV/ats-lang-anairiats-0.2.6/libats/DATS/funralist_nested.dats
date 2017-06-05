(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** A functional random-access list implementation based on nested datatypes
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: April, 2010 // based on a version done in DML (circa. 2000)
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "libats/SATS/funralist_nested.sats"

(* ****** ****** *)

typedef P (a1:t@ype) (a2:t@ype) = '(a1, a2)

datatype ralist (a:t@ype+, int) =
  | {n:pos} RAevn (a, n+n) of ralist (P a a, n)
  | {n:nat} RAodd (a, n+n+1) of (a, ralist (P a a, n))
  | RAnil (a, 0)
// end of [ralist]

(* ****** ****** *)

macdef P x0 x1 = '( ,(x0), ,(x1) )

(* ****** ****** *)

assume list_t0ype_int_type (a:t@ype, n:int) = ralist (a, n)

(* ****** ****** *)

implement{} funralist_make_nil {elt} () = RAnil ()

(* ****** ****** *)

implement{elt}
funralist_length
  (xs) = length<elt> (xs) where {
  fun{elt:t@ype}
  length {n:nat} .<n>.
    (xs: ralist (elt, n)):<> int n = let
    typedef elt2 = P elt elt
  in
    case+ xs of
    | RAevn xxs => 2 * length<elt2> (xxs)
    | RAodd (_, xxs) => 2 * length<elt2> (xxs) + 1
    | RAnil () => 0
  end // end of [length]
} // end of [funralist_length]

(* ****** ****** *)

implement{elt}
funralist_cons
  (x0, xs) = cons<elt> (x0, xs) where {
  fun{elt:t@ype} cons {n:nat} .<n>.
    (x0: elt, xs: ralist (elt, n)):<> ralist (elt, n+1) = let
    typedef elt2 = P elt elt
  in
    case+ xs of
    | RAevn xxs => RAodd (x0, xxs)
    | RAodd (x, xxs) => RAevn (cons<elt2> (P x0 x, xxs))
    | RAnil _ => RAodd (x0, RAnil ())
  end // end of [cons]
} // end of [funralist_cons]

(* ****** ****** *)

implement{elt}
funralist_uncons
  (xs, x0) = uncons<elt> (xs, x0) where {
  fun{elt:t@ype} uncons {n:pos} .<n>.
    (xs: ralist (elt, n), x0: &elt? >> elt)
    :<> ralist (elt, n-1) = let
    typedef elt2 = P elt elt
  in
    case+ xs of
    | RAevn xxs => let
        var xx0: elt2 // uninitialized
        val xxs = uncons<elt2> (xxs, xx0); val () = x0 := xx0.0
      in
        RAodd {elt} (xx0.1, xxs)
      end // end of [RAevn]
    | RAodd (x, xxs) => let
        val () = x0 := x in
        case+ xxs of RAnil _ => RAnil () | _ =>> RAevn xxs
      end // end of [RAodd]
  end // end of [uncons]
} // end of [funralist_uncons]

(* ****** ****** *)

implement{elt}
funralist_head (xs) = head (xs) where {
  fun{elt:t@ype}
  head {n:pos} .<n>. (xs: ralist (elt, n)):<> elt = let
    typedef elt2 = P elt elt
  in
    case+ xs of
    | RAevn xxs => let val xx = funralist_head<elt2> xxs in xx.0 end
    | RAodd (x, _) => x
  end // end of [head]
} // end of [funralist_head]

(* ****** ****** *)

implement{elt}
funralist_tail
  (xs) = tail<elt> (xs) where {
  fun{elt:t@ype} tail {n:pos} .<n>.
    (xs: ralist (elt, n)):<> ralist (elt, n-1) = let
    typedef elt2 = P elt elt
  in
    case+ xs of
    | RAevn xxs => let
        var xx: elt2 // uninitialized
        val xxs = funralist_uncons<elt2> (xxs, xx)
      in
        RAodd (xx.1, xxs)
      end // end of [RAevn]
    | RAodd (_, xxs) => begin
        case+ xxs of RAnil () => RAnil () | _ =>> RAevn xxs
      end // end of [RAodd]
   end // end of [tail]
} // end of [funralist_tail]

(* ****** ****** *)

implement{elt}
funralist_lookup
  (xs, i) = lookup<elt> (xs, i) where {
  fun{elt:t@ype}
  lookup {n,i:nat | i < n} .<n>.
    (xs: ralist (elt, n), i: int i):<> elt = let
    typedef elt2 = P elt elt
  in
    case+ xs of
    | RAevn xxs => let
        val x01 = lookup<elt2> (xxs, nhalf i)
      in
        if i nmod 2 = 0 then x01.0 else x01.1
      end // end of [RAevn]
    | RAodd (x, xxs) => begin
        if i = 0 then x else let
          val x01 = lookup<elt2> (xxs, nhalf (i-1))
        in
          if i nmod 2 = 0 then x01.1 else x01.0
        end // end of [if]
      end // end of [RAodd]
  end // end of [lookup]
} // end of [funralist_lookup]

(* ****** ****** *)

//
// Here is an example of constructing linear closures explicitly
//

dataviewtype closure_ (a:t@ype) =
  {param: viewtype} CLOSURE_ (a) of (param, (param, a) -<fun> a)
// end of [closure_]

fn{a:t@ype}
cloapp (c: closure_ a, x: a):<> a = let
  val+ ~CLOSURE_ {..} {param} (p, f) = c; val f = f: (param, a) -<fun> a
in
  f (p, x)
end // end of [cloapp]
  
fun{a:t@ype}
fupdate {n,i:nat | i < n} .<n>.
  (xs: ralist (a, n), i: int i, c: closure_ a):<> ralist (a, n) = let
  fn f0 (c: closure_ a, xx: P a a):<> P a a = '(cloapp<a> (c, xx.0), xx.1)
  fn f1 (c: closure_ a, xx: P a a):<> P a a = '(xx.0, cloapp<a> (c, xx.1))
in
  case+ xs of
  | RAevn xxs => let
      val i2 = i / 2; val parity = i - (i2 + i2)
    in
      if parity = 0 then begin
        RAevn (fupdate<P a a> (xxs, i2, CLOSURE_ {P a a} (c, f0)))
      end else begin
        RAevn (fupdate<P a a> (xxs, i2, CLOSURE_ {P a a} (c, f1)))
      end // end of [if]
    end // end of [RAevn]
  | RAodd (x, xxs) => begin
      if i = 0 then RAodd (cloapp<a> (c, x), xxs) else let
        val i1 = i - 1; val i2 = i1 / 2; val parity = i1 - (i2 + i2)
      in
        if parity = 0 then begin
          RAodd (x, fupdate<P a a> (xxs, i2, CLOSURE_ {P a a} (c, f0)))
        end else begin
          RAodd (x, fupdate<P a a> (xxs, i2, CLOSURE_ {P a a} (c, f1)))
        end // end of [if]
      end // end of [if]
    end // end of [RAodd]
end // end of [fupdate]

implement{elt}
funralist_update (xs, i, x) = let
  val f0 = lam (x_box: box_vt elt, _: elt): elt =<fun> let val+ ~box_vt (x) = x_box in x end
in
  fupdate<elt> (xs, i, CLOSURE_ (box_vt x, f0))
end // end of [funralist_update]

(* ****** ****** *)

local

fun{a:t@ype}
foreach
  {v:view}
  {n:nat}
  {f:eff} .<n>. (
  pf0: !v
| xs: ralist (a, n), f: (!v | a) -<cloref,f> void
) :<f> void = let
  extern fun donothing ():<> void = "atspre_donothing"
in
  case+ xs of
  | RAnil () => ()
  | RAevn xxs => let
      var !p_f2 with pf_f2 = @lam
        (pf0: !v | xx: P a a): void =<clo,f> (f (pf0 | xx.0); f (pf0 | xx.1))
      typedef clo_type = (!v | P a a) -<clo,f> void
      typedef cloref_type = (!v | P a a) -<cloref,f> void
      val f2 = __encode (pf_f2 | p_f2) where { // cutting a corner here!
        extern castfn __encode (pf: !clo_type @ p_f2 | p: ptr p_f2):<> cloref_type
      } // end of [val]
      val () = foreach<P a a> (pf0 | xxs, f2)
    in
      donothing () // HX-2011-02-26: prevent tail-recursion optimization
    end // end of [RAevn]
  | RAodd (x, xxs) => let
      val () = f (pf0 | x) in case+ xxs of
      | RAnil () => () | _ =>> let
          var !p_f2 with pf_f2 = @lam
            (pf0: !v | xx: P a a): void =<clo,f> (f (pf0 | xx.0); f (pf0 | xx.1))
          typedef clo_type = (!v | P a a) -<clo,f> void
          typedef cloref_type = (!v | P a a) -<cloref,f> void
          val f2 = __encode (pf_f2 | p_f2) where { // cutting a corner here!
            extern castfn __encode (pf: !clo_type @ p_f2 | p: ptr p_f2):<> cloref_type
          } // end of [val]
          val () = foreach<P a a> (pf0 | xxs, f2)
        in
           donothing () // HX-2011-02-26: prevent tail-recursion optimization
        end // end of [_]
    end // end of [RAodd]
end // end of [foreach]

in // in of [local]

implement{elt}
funralist_foreach_vcloptr
  {v} {n} {f:eff} (pf0 | xs, f) = let
  typedef cloref_type = (!v | elt) -<cloref,f> void
  viewtypedef cloptr_type = (!v | elt) -<cloptr,f> void
  val f = __cast (f) where {
    extern castfn __cast (f: !cloptr_type):<> cloref_type
  } // end of [val]
in
  foreach<elt> (pf0 | xs, f)
end // end of [funralist_foreach_vcloptr]

end // end of [local]

(* ****** ****** *)

implement{elt}
funralist_foreach_vclo
  {v} {n} {f:eff} (pf0 | xs, f) = let
  typedef clo_type = (!v | elt) -<clo,f> void
  viewtypedef cloptr_type = (!v | elt) -<cloptr,f> void
  val f = __encode (view@ f | &f) where {
    extern castfn __encode
      {l:addr} (pf: !clo_type @ l | p: ptr l):<> cloptr_type
  } // end of [where]
  val () = funralist_foreach_vcloptr<elt> {v} (pf0 | xs, f)
  val _ptr = __decode (f) where {
    extern castfn __decode
      (f: cloptr_type):<> ptr // HX: this matches [__encode]
  } // end of [val]
in
  // empty
end // end of [funralist_foreach_vclo]

implement{elt}
funralist_foreach_cloptr
  {n} {f:eff} (xs, f) = () where {
//
  viewtypedef cloptr0_t = (elt) -<cloptr, f> void
  viewtypedef cloptr1_t = (!unit_v | elt) -<cloptr, f> void
//
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr0_t >> cloptr1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val () = funralist_foreach_vcloptr<elt> {unit_v} (pfu | xs, f)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr1_t >> cloptr0_t): void
  } // end of [val]
} // end of [funralist_foreach_cloptr]

implement{elt}
funralist_foreach_cloref
  {n} {f:eff} (xs, f) = let
  viewtypedef cloptr_type = (!unit_v | elt) -<cloptr,f> void  
  val f = __encode (f) where {
    extern castfn __encode (f: (elt) -<cloref,f> void):<> cloptr_type
  } // end of [val]
  prval pfu = unit_v ()
  val () = funralist_foreach_vcloptr<elt> (pfu | xs, f)
  prval unit_v () = pfu
  val _ptr = __decode (f) where {
    extern castfn __decode (f: cloptr_type):<> ptr
  } // end of [val]
in
  // nothing
end // end of [funralist_foreach_cloref]

(* ****** ****** *)

(* end of [funralist_nested.dats] *)
