(*
//
// A verified implementation of quicksort on lists
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Saturday, September 27, 2008
//

//
// How to compile:
//   atscc -o quicksort_list quicksort_list.dats
// How to test:
//   ./quicksort_list
//
*)

(*

This is a milestone example in the development of ATS.

A list quicksort implementation given below is proven to be terminating
and its return is guaranteed to be a sorted permutation of its input list.

An implementation of list quicksort in DML was given in 1998 that can
guarantee based on its type that it always returns a list of the same
length as its input. Since then, a question that has been asked frequently
by many people is whether it can be done in DML to give an implementation
of list quicksort that can guarantee based on its type that it always
returns a list that is a sorted permutation of its input.

This is finally done now in ATS, which succeeds DML as well as extends it.

*)

(* ****** ****** *)

staload "libats/SATS/ilistp.sats"
stadef nil = ilist_nil
stadef cons = ilist_cons

(* ****** ****** *)

sortdef nats = nat
dataprop MSET (ilist, int(*nats*)) = 
  | {x:nat} {xs:ilist} {n:nats}
    MSETcons (cons (x, xs), x + n) of MSET (xs, n)
  | MSETnil (nil, 0)
// end of [MSET]

extern praxi MSET_istot {xs:ilist} (): [n:nats] MSET (xs, n)

(* ****** ****** *)

dataprop LB (int, ilist) =
  | {l:nat} {x:nat | l <= x} {xs:ilist} LBcons (l, cons (x, xs)) of LB (l, xs)
  | {l:nat} LBnil (l, nil)
// end of [LB]

dataprop UB (ilist, int) =
  | {u:nat} {x:nat | x <= u} {xs:ilist} UBcons (cons (x, xs), u) of UB (xs, u)
  | {u:nat} UBnil (nil, u)
// end of [UB]

(* ****** ****** *)

extern praxi LB_MSET_lemma {x:nat} {xs1,xs2:ilist} {n:nats}
  (_: MSET (xs1, n), _: MSET (xs2, n), _lb: LB (x, xs1)):<prf> LB (x, xs2)
extern praxi UB_MSET_lemma {x:nat} {xs1,xs2:ilist} {n:nats}
  (_: MSET (xs1, n), _: MSET (xs2, n), _ub: UB (xs1, x)):<prf> UB (xs2, x)

(* ****** ****** *)

extern prfun LB_lemma_monotone
  {l1,l2:nat | l1 <= l2} {xs: ilist} (pf: LB (l2, xs)):<prf> LB (l1, xs)
extern prfun UB_lemma_monotone
  {u1,u2:nat | u1 >= u2} {xs: ilist} (pf: UB (xs, u2)):<prf> UB (xs, u1)

(* ****** ****** *)

absprop ISORD (ilist)
extern prfun isord_nil (): ISORD (nil)
extern prfun isord_cons {x:int} {xs:ilist}
  (pf1: LB (x, xs), pf2: ISORD (xs)): ISORD (cons (x, xs))
// end of [isord_cons]

(* ****** ****** *)

extern
prfun APPEND_MSET_lemma {xs,ys,zs:ilist} {n1,n2:nats}
  (pf1: MSET (xs, n1), pf2: MSET (ys, n2), pf3: APPEND (xs, ys, zs))
  :<prf> MSET (zs, n1 + n2)
// end of [APPEND_MSET_lemma]

extern
prfun APPEND_ISORD_lemma
  {xs1,xs2,xs:ilist} {x:nat} (
    pf1: ISORD xs1
  , pf2: ISORD xs2
  , pf3: UB (xs1, x)
  , pf4: LB (x, xs2)
  , pf5: APPEND (xs1, xs2, xs)
) :<prf> ISORD (xs) // end of [APPEND_ISORD_lemma]

(* ****** ****** *)

abst@ype E (a: t@ype, x:int) = a
extern castfn eltencode {a:t@ype} (x: a):<> [x:pos] E (a, x)
extern castfn eltdecode {a:t@ype} {x:int} (x: E (a, x)):<> a

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

extern fun{a:t@ype} append {xs,ys:ilist}
  (xs: list (a, xs), ys: list (a, ys)):<> [zs:ilist] (APPEND (xs, ys, zs) | list (a, zs))
implement{a}
append (xs, ys) = let
  fun aux {xs,ys:ilist} .<xs>.
    (xs: list (a, xs), ys: list (a, ys))
    :<> [zs:ilist] (APPEND (xs, ys, zs) | list (a, zs)) = begin
    case+ xs of
    | cons (x, xs) => let
        val (pf | zs) = aux (xs, ys) in (APPENDcons pf | cons (x, zs))
      end
    | nil () => (APPENDnil () | ys)
  end // end of [aux]
in
  aux (xs, ys)
end // end of [append]

(* ****** ****** *)

fun{a:t@ype}
qsrt {xs:ilist} {n:nats} .<n,0>.
  (pf: MSET (xs, n) | xs: list (a, xs))
  :<> [xs: ilist] (MSET (xs, n), ISORD (xs) | list (a, xs)) = begin
  case+ xs of
  | cons (x, xs) => let
      prval MSETcons pf = pf in part (
      pf, MSETnil (), MSETnil (), UBnil (), LBnil () | x, xs, nil (), nil ()
    ) end
  | nil () => let
      prval MSETnil () = pf in (MSETnil (), isord_nil () | nil ())
    end
end // end of [qsrt]

and part {x:pos} {xs0,xs1,xs2:ilist} {n0,n1,n2:nats} .<n0+n1+n2,n0+1>. (
    pf0: MSET (xs0, n0)
  , pf1: MSET (xs1, n1)
  , pf2: MSET (xs2, n2)
  , pf_ub: UB (xs1, x)
  , pf_lb: LB (x, xs2)
  | x: E (a, x), xs0: list (a, xs0), xs1: list (a, xs1), xs2: list (a, xs2)
  ) :<> [xs: ilist] (MSET (xs, x+n0+n1+n2), ISORD (xs) | list (a, xs)) = begin
  case+ xs0 of
  | cons (x0, xs0) => let
      prval MSETcons (pf0) = pf0
    in
      if x0 <= x then part (
        pf0, MSETcons pf1, pf2, UBcons (pf_ub), pf_lb
      | x, xs0, cons (x0, xs1), xs2
      ) else part (
        pf0, pf1, MSETcons pf2, pf_ub, LBcons (pf_lb)
      | x, xs0, xs1, cons (x0, xs2)
      )
    end // end of [cons]
  | nil () => let
      prval MSETnil () = pf0
      val (pf1_set, pf1_ord | xs1) = qsrt (pf1 | xs1)
      val (pf2_set, pf2_ord | xs2) = qsrt (pf2 | xs2)
      prval pf_ub = UB_MSET_lemma (pf1, pf1_set, pf_ub)
      prval pf_lb = LB_MSET_lemma (pf2, pf2_set, pf_lb)
      prval pf2_ord1 = isord_cons (pf_lb, pf2_ord)
      val (pf_app | xs) = append (xs1, cons (x, xs2))
      prval pf_set = APPEND_MSET_lemma (pf1_set, MSETcons pf2_set, pf_app)
      prval pf_ord = APPEND_ISORD_lemma (
        pf1_ord, pf2_ord1, pf_ub, LBcons {x} (pf_lb), pf_app
      )
    in
      (pf_set, pf_ord | xs)      
    end
end // end of [part]

(* ****** ****** *)

extern fun{a:t@ype}
quicksort {xs:ilist} {n:nats}
  (pf: MSET (xs, n) | xs: list (a, xs))
  :<> [xs: ilist] (MSET (xs, n), ISORD (xs) | list (a, xs))
implement{a} quicksort (pf | xs) = qsrt<a> (pf | xs)

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

implement
lte_elt_elt<double>
  {x,y} (x, y) = let
  val x = eltdecode (x) and y = eltdecode(y)
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
        val x = eltdecode (x)
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
    val x = eltencode (x)
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
  prval pfmset = MSET_istot ()
  val (pford, pfmset | ys) = quicksort (pfmset | xs)
  val () = (print "ys = "; print_list ys; print_newline ())
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [quicksort_list.dats] *)
