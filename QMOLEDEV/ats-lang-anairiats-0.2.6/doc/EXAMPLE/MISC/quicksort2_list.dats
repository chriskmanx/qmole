(*
//
// A verified implementation of quicksort on lists:
// the returned output list is guaranteed to be a permutation
// of the original input
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Tuesday, October 5, 2010
//
//
// How to compile:
//   atscc -o quicksort2_list quicksort2_list.dats
// How to test:
//   ./quicksort2_list
*)

(* ****** ****** *)

staload "libc/SATS/random.sats"
staload "libats/SATS/ilistp.sats"

(* ****** ****** *)

stadef nil = ilist_nil
stadef cons = ilist_cons

(* ****** ****** *)

abst@ype E (a: t@ype, x:int) = a
extern castfn encode {a:t@ype} (x: a):<> [x:pos] E (a, x)
extern castfn decode {a:t@ype} {x:int} (x: E (a, x)):<> a

(* ****** ****** *)

extern
fun{a:t@ype}
lte_elt_elt {x,y:nat} (x: E(a, x), y: E (a, y)):<> bool (x <= y)
overload <= with lte_elt_elt

datatype list (a:t@ype, ilist) =
  | nil (a, nil) of ()
  | {x:pos} {xs: ilist} cons (a, cons (x, xs)) of (E (a, x), list (a, xs))
// end of [list]

typedef list (a:t@ype) = [xs:ilist] list (a, xs)

(* ****** ****** *)

(*
//
// HX-2010-10-08:
// if you want to show that the output of quicksort is always a ordered list,
// please be my guest :)
//
dataprop LB (x0:int, ilist) =
  | LBnil (x0, ilist_nil) of ()
  | {x:int | x0 <= x} {xs:ilist} LBcons (x0, ilist_cons (x, xs)) of LB (x0, xs)
dataprop UB (x0:int, ilist) =
  | UBnil (x0, ilist_nil) of ()
  | {x:int | x0 >= x} {xs:ilist} UBcons (x0, ilist_cons (x, xs)) of UB (x0, xs)
dataprop ISORD (ilist) =
  | ISORDnil (ilist_nil) of ()
  | {x:int} {xs:ilist}
    ISORDcons (ilist_cons (x, xs)) of (LB (x, xs), ISORD (xs))
// end of [ISORD]
*)

(* ****** ****** *)

extern
fun{a:t@ype}
quicksort {xs:ilist}
  (xs: list (a, xs)): [ys:ilist] (PERMUTE (xs, ys) | list (a, ys))
// end of [quicksort]

(* ****** ****** *)

extern
fun{a:t@ype}
append {xs1,xs2:ilist}
  (xs1: list (a, xs1), xs2: list (a, xs2))
  : [xs3: ilist] (APPEND (xs1, xs2, xs3) | list (a, xs3))
// end of [append]

(* ****** ****** *)

implement{a}
append {xs1,xs2}
  (xs1, xs2) = case+ xs1 of
  | cons {x1} (x1, xs11) => let
      val [xs31:ilist] (pf1 | xs31) = append (xs11, xs2)
    in
      (APPENDcons (pf1) | cons (x1, xs31))
    end // end of [cons]
  | nil () => (APPENDnil () | xs2)
// end of [append]  

(* ****** ****** *)

propdef
MUNION (xs1:ilist, xs2:ilist, xs3:ilist) =
  {x0:int} {n1,n2:nat}
  (MSETCNT (x0, xs1, n1), MSETCNT (x0, xs2, n2)) -<prf> MSETCNT (x0, xs3, n1+n2)
// end of [MUNION]

extern
prfun append_munion_lemma
  {xs,ys,zs:ilist} (pf: APPEND (xs,ys,zs)): MUNION (xs, ys, zs)
// end of [append_munion_lemma]

implement
append_munion_lemma
  (pf) = lemma (pf) where {
  prfun lemma {xs,ys,zs:ilist} .<xs>.
    (pf: APPEND (xs,ys,zs)): MUNION (xs, ys, zs) =
    case+ pf of
    | APPENDcons (pf) => let
        prval fpf = lemma (pf) in
        lam (pf1, pf2) => let
          prval MSETCNTcons pf1 = pf1 in MSETCNTcons (fpf (pf1, pf2))
        end // end of [lam]
      end (* end of [APPENDcons] *)
    | APPENDnil () =>
        lam (pf1, pf2) => let
          prval MSETCNTnil () = pf1 in pf2
        end // end of [lam]
      (* end of [APPENDnil] *)
  // end of [lemma]
} // end of [append_munion_lemma]

(* ****** ****** *)

propdef PART (
  x: int, xs0: ilist, xs1: ilist, xs2: ilist, xs: ilist
) = {x0:int} {n0,n1,n2:nat} (
  MSETCNT (x0, xs0, n0), MSETCNT (x0, xs1, n1), MSETCNT (x0, xs2, n2)
) -<prf> MSETCNT (x0, xs, n0+n1+n2+b2i(x0==x))
// end of [PART]

fun{a:t@ype}
qsrt {xs:ilist} (
  xs: list (a, xs)
) : [ys:ilist] (PERMUTE (xs, ys) | list (a, ys)) =
  case+ xs of
  | cons {x} {xs1} (x, xs1) => let
      val [ys:ilist] (fpf | ys) = part (x, xs1, nil (), nil ())
      prval fpf = lam {x0:int} {n:nat}
        (pf: MSETCNT (x0, xs, n)): MSETCNT (x0, ys, n) =<prf> let
        prval MSETCNTcons pf = pf
      in
         fpf (pf, MSETCNTnil, MSETCNTnil)
      end // end of [prval]
    in
      (fpf | ys)
    end // end of [cons]
  | nil () => (permute_refl {ilist_nil} () | nil ())
// end of [qsrt]

and
part {x:pos}
  {xs,xs1,xs2:ilist} (
  x: E (a, x)
, xs: list (a, xs)
, xs1: list (a, xs1)
, xs2: list (a, xs2)
) : [ys:ilist] (PART (x, xs, xs1, xs2, ys) | list (a, ys)) =
  case xs of
  | cons (x_, xs_) => (
      if (x_ <= x) then let
        val [ys:ilist] (fpf | ys) = part (x, xs_, cons (x_, xs1), xs2)
        prval fpf = lam {x0:int} {n0,n1,n2:nat}
          (pf0: MSETCNT (x0, xs, n0), pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2))
          : MSETCNT (x0, ys, n0+n1+n2+b2i(x0==x)) =<prf> let
          prval MSETCNTcons pf0 = pf0
          prval pf1 = MSETCNTcons (pf1)
        in
          fpf (pf0, pf1, pf2)
        end // end of [prval]
      in
        (fpf | ys)
      end else let
        val [ys:ilist] (fpf | ys) = part (x, xs_, xs1, cons (x_, xs2))
        prval fpf = lam {x0:int} {n0,n1,n2:nat}
          (pf0: MSETCNT (x0, xs, n0), pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2))
          : MSETCNT (x0, ys, n0+n1+n2+b2i(x0==x)) =<prf> let
          prval MSETCNTcons pf0 = pf0
          prval pf2 = MSETCNTcons (pf2)
        in
          fpf (pf0, pf1, pf2)
        end // end of [prval]
      in
        (fpf | ys)
      end // end of [let]
    ) // end of [cons]
  | nil () => let
      val [ys1:ilist] (fpf1 | ys1) = qsrt (xs1)
      val [ys2:ilist] (fpf2 | ys2) = qsrt (xs2)
      val [ys:ilist] (pf3 | ys) = append (ys1, cons (x, ys2))
      prval fpf3 = append_munion_lemma (pf3)
      prval fpf = lam {x0:int} {n0,n1,n2:nat}
        (pf0: MSETCNT (x0, xs, n0), pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2))
        : MSETCNT (x0, ys, n0+n1+n2+b2i(x0==x)) =<prf> let
        prval MSETCNTnil () = pf0
        prval pf1 = fpf1 (pf1) // pf1 : MSETCNT (x0, ys1, n1)
        prval pf2 = fpf2 (pf2) // pf2 : MSETCNT (x0, ys2, n2)
      in
        fpf3 (pf1, MSETCNTcons (pf2))
      end // end of [prval]
    in
      (fpf | ys)
    end // end of [nil]
// end of [part]

(* ****** ****** *)

implement{a} quicksort (xs) = qsrt<a> (xs)

(* ****** ****** *)

implement
lte_elt_elt<double>
  {x,y} (x, y) = let
  val x = decode (x) and y = decode(y)
  extern castfn __cast (_: bool):<> bool (x <= y)
in
  __cast (lte_double_double (x, y))
end // end of [lte_elt_elt]

fn print_list (
  xs: list (double)
) : void = let
  fun aux (
    xs: list (double), i: int
  ) : void = begin
    case+ xs of
    | cons (x, xs) => let
        val x = decode (x)
      in
        if i > 0 then print ", "; printf ("%.2f", @(x)); aux (xs, i+1)
      end // end of [cons]
    | nil () => ()
  end // end of [aux]
in
  aux (xs, 0)
end // end of [print_list]

fun randgen_list
  {n:nat} .<n>.
  (n: int n): list (double) =
  if n > 0 then let
    val x = drand48 ()
    val x = encode (x)
  in
    cons {double} (x, randgen_list (n-1))
  end else nil ()
// end of [randgen_list]

(* ****** ****** *)

implement main () = let
//
  val () = srand48_with_time ()
//
  val xs = randgen_list (20)
  val () = (print "xs = "; print_list xs; print_newline ())
  val (_fpf | ys) = quicksort (xs)
  val () = (print "ys = "; print_list ys; print_newline ())
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [quicksort2_list.dats] *)
