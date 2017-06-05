(*
//
// A verified implementation of quicksort on lists
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Saturday, September 27, 2008
//

//
// How to compile:
//   atscc -o quicksort1_list quicksort1_list.dats
// How to test:
//   ./quicksort1_list
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
  | {l:nat} {x:nat | l <= x} {xs: ilist}
    LBcons (l, cons (x, xs)) of LB (l, xs)
  | {l:nat} LBnil (l, nil)
// end of [LB]

dataprop UB (ilist, int) =
  | {u:nat} {x:nat | x <= u} {xs: ilist}
    UBcons (cons (x, xs), u) of UB (xs, u)
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

implement LB_lemma_monotone {l1,l2} (xs) = let
  prfun aux {xs:ilist} .<xs>.
    (pf: LB (l2, xs)): LB (l1, xs) = begin
    case+ pf of LBcons (pf) => LBcons (aux pf) | LBnil () => LBnil ()
  end
in
  aux (xs)
end // end of [LB_lemma_monotone]

(* ****** ****** *)

extern prfun UB_lemma_monotone
  {u1,u2:nat | u1 >= u2} {xs: ilist} (pf: UB (xs, u2)):<prf> UB (xs, u1)
implement UB_lemma_monotone {u1,u2} (xs) = let
  prfun aux {xs:ilist} .<xs>.
    (pf: UB (xs, u2)): UB (xs, u1) = begin
    case+ pf of UBcons (pf) => UBcons (aux pf) | UBnil () => UBnil ()
  end
in
  aux (xs)
end // end of [UB_lemma_monotone]

(* ****** ****** *)

dataprop ISORD1 (ilist) =
  | ISORD1nil (nil)
  | {x:nat} {xs:ilist} ISORD1cons (cons (x, xs)) of (LB (x, xs), ISORD1 xs)
// end of [ISORD1]

(* ****** ****** *)

extern prfun APPEND_MSET_lemma {xs,ys,zs:ilist} {n1,n2:nats}
  (pf1: MSET (xs, n1), pf2: MSET (ys, n2), pf3: APPEND (xs, ys, zs))
  :<prf> MSET (zs, n1 + n2)

implement APPEND_MSET_lemma (pf1, pf2, pf3) = let
  prfun aux {xs,ys,zs:ilist} {n1,n2:nats} .<xs>.
    (pf1: MSET (xs, n1), pf2: MSET (ys, n2), pf3: APPEND (xs, ys, zs))
    : MSET (zs, n1 + n2) = begin case+ pf3 of
    | APPENDcons (pf3) => let
        val+ MSETcons (pf1) = pf1 in MSETcons (aux (pf1, pf2, pf3))
      end
    | APPENDnil () => let val+ MSETnil () = pf1 in pf2 end
  end // end of [aux]
in
  aux (pf1, pf2, pf3)
end // end of [APPEND_MSET_lemma]

(* ****** ****** *)

extern prfun APPEND_ISORD1_lemma {xs1,xs2,xs:ilist} {x:nat} (
    pf1: ISORD1 xs1
  , pf2: ISORD1 xs2
  , pf3: UB (xs1, x)
  , pf4: LB (x, xs2)
  , pf5: APPEND (xs1, xs2, xs)
) :<prf> ISORD1 (xs)

implement APPEND_ISORD1_lemma (pf1, pf2, pf3, pf4, pf5) = let
  prfun aux {x0:nat} {xs1,xs2,xs:ilist} {x:nat | x0 <= x} .<xs1>. (
      pf1: ISORD1 xs1
    , pf2: ISORD1 xs2
    , pf3: UB (xs1, x)
    , pf4: LB (x, xs2)
    , pf5: APPEND (xs1, xs2, xs)
    , pf6: LB (x0, xs1)
  ) : @(ISORD1 xs, LB (x0, xs)) =
  case+ pf5 of
  | APPENDcons {x1} (pf5) => let
      val ISORD1cons (pf1_lb1, pf1) = pf1
      val UBcons pf3 = pf3
      val (pf_ord, pf_lb1) = aux {x1} (pf1, pf2, pf3, pf4, pf5, pf1_lb1)
      val LBcons pf6 = pf6
      val pf_lb0 = LB_lemma_monotone {x0,x1} (pf_lb1)
    in
      (ISORD1cons (pf_lb1, pf_ord), LBcons pf_lb0)
    end
  | APPENDnil () => (pf2, LB_lemma_monotone {x0, x} pf4)
in
  case+ pf5 of
  | APPENDcons {x1} _ => let
      val UBcons _ = pf3
      val ISORD1cons (pf_lb, _) = pf1
      val pf_lb = LBcons {x1} {x1} (pf_lb)
      val (pf_ord, pf_lb) = aux {x1} (pf1, pf2, pf3, pf4, pf5, pf_lb)
    in
      pf_ord
    end
  | APPENDnil () => pf2
end // end of [APPEND_ISORD1_lemma]

(* ****** ****** *)

abst@ype T (int) = double

extern fun lte_elt_elt {x,y:nat} (x: T x, y: T y):<> bool (x <= y)
overload <= with lte_elt_elt

datatype list (ilist) =
  | {x:pos} {xs:ilist} cons (cons (x, xs)) of (T (x), list (xs))
  | nil (nil)
// end of [list]  

typedef list = [xs:ilist] list (xs)

(* ****** ****** *)

extern fun append {xs,ys:ilist}
  (xs: list (xs), ys: list (ys)):<> [zs:ilist] (APPEND (xs, ys, zs) | list zs)
implement append (xs, ys) = let
  fun aux {xs,ys:ilist} .<xs>.
    (xs: list xs, ys: list ys)
    :<> [zs:ilist] (APPEND (xs, ys, zs) | list zs) = begin
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

fun qsrt {xs:ilist} {n:nats} .<n,0>.
  (pf: MSET (xs, n) | xs: list xs)
  :<> [xs: ilist] (MSET (xs, n), ISORD1 (xs) | list xs) = begin
  case+ xs of
  | cons (x, xs) => let
      prval MSETcons pf = pf in part (
      pf, MSETnil (), MSETnil (), UBnil (), LBnil () | x, xs, nil (), nil ()
    ) end
  | nil () => let
      prval MSETnil () = pf in (MSETnil (), ISORD1nil () | nil ())
    end
end // end of [qsrt]

and part {x:pos} {xs0,xs1,xs2:ilist} {n0,n1,n2:nats} .<n0+n1+n2,n0+1>. (
    pf0: MSET (xs0, n0)
  , pf1: MSET (xs1, n1)
  , pf2: MSET (xs2, n2)
  , pf_ub: UB (xs1, x)
  , pf_lb: LB (x, xs2)
  | x: T x, xs0: list xs0, xs1: list xs1, xs2: list xs2
  ) :<> [xs: ilist] (MSET (xs, x+n0+n1+n2), ISORD1 (xs) | list xs) = begin
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
      prval pf2_ord1 = ISORD1cons (pf_lb, pf2_ord)
      val (pf_app | xs) = append (xs1, cons (x, xs2))
      prval pf_set = APPEND_MSET_lemma (pf1_set, MSETcons pf2_set, pf_app)
      prval pf_ord = APPEND_ISORD1_lemma (
        pf1_ord, pf2_ord1, pf_ub, LBcons {x} (pf_lb), pf_app
      )
    in
      (pf_set, pf_ord | xs)      
    end
end // end of [part]

(* ****** ****** *)

extern fun quicksort
  {xs:ilist} {n:nats} (pf: MSET (xs, n) | xs: list xs)
  :<> [xs: ilist] (MSET (xs, n), ISORD1 (xs) | list xs)
implement quicksort (pf | xs) = qsrt (pf | xs)

(* ****** ****** *)

local

assume T (n:int) = double

in

implement lte_elt_elt {x,y} (x, y) = let
  extern castfn __cast (_: bool):<> bool (x <= y)
in
  __cast (lte_double_double (x, y))
end // end of [lte_elt_elt]

fn print_list (xs: list): void = let
  fun aux (xs: list, i: int): void = begin case+ xs of
    | cons (x, xs) => begin
        if i > 0 then print ", "; printf ("%.1f", @(x)); aux (xs, i+1)
      end // end of [cons]
    | nil () => ()
  end // end of [aux]
in
  aux (xs, 0)
end // end of [print_list]

castfn T .<>. (f: double):<> [x:pos] T (x) = #[1 | f]

end // end of [local]

(* ****** ****** *)

#define :: cons

implement main () = let
  val xs: list =
     T 2.0 :: T 1.0 :: T 4.0 :: T 3.0 :: T 6.0 :: T 5.0
  :: T 2.0 :: T 1.0 :: T 4.0 :: T 3.0 :: T 6.0 :: T 5.0
  :: T 2.0 :: T 1.0 :: T 4.0 :: T 3.0 :: T 6.0 :: T 5.0
  :: T 2.0 :: T 1.0 :: T 4.0 :: T 3.0 :: T 6.0 :: T 5.0
  :: nil ()
  val (_(*pf_set*), _(*pf_ord*) | ys) = quicksort (MSET_istot () | xs)
in
  print "xs = "; print_list xs; print_newline ();
  // is there any doubt :)
  print "ys = "; print_list ys; print_newline ();
end // end of [main]

(* ****** ****** *)

(* end of [quicksort1_list.dats] *)
