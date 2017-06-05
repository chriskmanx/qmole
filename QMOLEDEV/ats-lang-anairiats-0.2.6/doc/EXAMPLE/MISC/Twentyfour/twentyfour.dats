//
// An implemention of the so-called game-of-24
// Some typical uses of macros are presented here.
//
// The code was first translated to ATS/Proto by Hongwei Xi in
// the summer of 2004 from an earlier version in Standard ML
//
// Translated to ATS/Geizella by Hongwei Xi
// Time: June 2007
//

(*

Given four natural numbers n1, n2, n3 and n4, one chooses two of them and
generates a rational number r1 using either addition, subtraction,
multiplication or division; one mixes r1 with the remaining two numbers and
chooses two of them to generate a rational number r2 using either addition,
subtraction, multiplication or division; one then takes r2 and the last
remaining number to get a rational number r3 using addition, subtraction,
multiplication, or division; if there is a way to make r3 equal 24, then
we say that (n1, n2, n3, n4) is a good quad.  For instance, (10,10,4,4) is
a good quad since we have

(10 * 10 - 4) / 4 = 24

Similarly, (5, 7, 7, 11) is a good quad since we have

(5 - 11 / 7) * 7 = 24

Game of 24 is a game that determines whether four given natural numbers are
a good quad.

*)

(*

typedef int_t = int
staload "rational.sats" with { int_t = int_t }
dynload "rational.dats" with { int_t = int_t }

*)

staload "rational.sats"
dynload "rational.dats"

(* ****** ****** *)

// macro definition
macdef ignore (x) = let val _ = ,(x) in () end

datatype expr =
  | Num of rat_t
  | Add of (expr, expr)
  | Sub of (expr, expr)
  | Mul of (expr, expr)
  | Div of (expr, expr)
// end of [expr]

// macro definition
macdef priority_mac e = (case+ ,(e) of
  | Num _ => 0 | Add _ => 2 | Sub _ => 2 | Mul _ => 1 | Div _ => 1
) : Nat // end of [priority_mac]

// fun priority (e: expr): Nat = ,(priority_mac `(e))

fun print_expr (e: expr): void = begin case+ e of
  | Num r => print r
  | Add (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
    in
      print_expr_ (2, p1, e1); print '+'; print_expr_ (2, p2, e2)
    end // end of [Add]
  | Sub (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
    in
      print_expr_ (2, p1, e1); print '-'; print_expr_ (2, p2, e2)
    end // end of [Sub]
  | Mul (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
    in
      print_expr_ (1, p1, e1); print '*'; print_expr_ (1, p2, e2)
    end // end of [Mul]
  | Div (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
    in
      print_expr_ (1, p1, e1); print '/'; print_expr_ (1, p2, e2)
    end // end of [Div]
end // end of [print_expr]

and print_expr_ (p0: Nat, p: Nat, e: expr): void =
  if p < p0 then print_expr e else (print "("; print_expr e; print ")")
// end of [print_expr_]

typedef card = (expr, rat_t)

macdef val_of_card_mac c = let val (_, v) = ,(c) in v end

// fun val_of_card (c: card): rat_t = val_of_card_mac `(c)

#define i2r rat_make_int

fun card_of_int0 (i: int): card =
  let val i = int_make_int0 i in (Num (i2r i), i2r i) end

macdef print_card_mac c =
  let val (e, _): card = ,(c) in print_expr e end

// fun print_card (c: card): void = print_card_mac c

macrodef cardAdd_mac c1 c2 = `(
  let val (e1, v1) = ,(c1) and (e2, v2) = ,(c2) in
     (Add (e1, e2), v1 + v2)
  end
) // end of [cardAdd_mac]

macrodef cardSub_mac c1 c2 = `(
  let val (e1, v1) = ,(c1) and (e2, v2) = ,(c2) in
     (Sub (e1, e2), v1 - v2)
  end
) // end of [cardSub_mac]

macrodef cardMul_mac c1 c2 = `(
  let val (e1, v1) = ,(c1) and (e2, v2) = ,(c2) in
     (Mul (e1, e2), v1 * v2)
  end
) // end of [cardMul_mac]

macrodef cardDiv_mac c1 c2 = `(
  let val (e1, v1) = ,(c1) and (e2, v2) = ,(c2) in
     (Div (e1, e2), v1 / v2)
  end
) // end of [cardDiv_mac]

(*
// fun cardAdd (c1: card, c2: card) = ,(cardAdd_mac `(c1) `(c2))
// fun cardSub (c1: card, c2: card) = ,(cardSub_mac `(c1) `(c2))
// fun cardMul (c1: card, c2: card) = ,(cardMul_mac `(c1) `(c2))
// fun cardDiv (c1: card, c2: card) = ,(cardDiv_mac `(c1) `(c2))
*)

datatype cards = cards_nil | cards_cons of (card, cards)

#define nil cards_nil
#define :: cards_cons

fn combine (x: card, y: card): cards = let
  val c1 = ,(cardAdd_mac `(x) `(y))
  val c2 = ,(cardSub_mac `(x) `(y)) and c3 = ,(cardSub_mac `(y) `(x))
  val c4 = ,(cardMul_mac `(x) `(y))
in
  if is_zero (val_of_card_mac x) then
    c1 :: c2 :: c3 :: c4 :: nil ()
  else if is_zero (val_of_card_mac y) then
    c1 :: c2 :: c3 :: c4 :: nil ()
  else let
    val c5 = ,(cardDiv_mac `(x) `(y)) and c6 = ,(cardDiv_mac `(y) `(x))
  in
    c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: nil ()
  end
end // end of [combine]

fun cards_append_r
  (xs: cards, ys: cards): cards = begin case+ xs of
  | x :: xs => cards_append_r (xs, x :: ys) | nil () => ys
end // end of [cards_append_r]

(* ****** ****** *)

// this code does not make use of templates
datatype ratpairlst =
  | ratpairlst_nil | ratpairlst_cons of (rat_t, rat_t, ratpairlst)
// end of [ratpairlst]

#define rp_nil ratpairlst_nil
#define rp_cons ratpairlst_cons

fn ismem
  (x0: rat_t, y0: rat_t, xys: ratpairlst): Bool = let
  fun aux (xys: ratpairlst):<cloptr1> Bool = case+ xys of
    | rp_nil () => false
    | rp_cons (x, y, xys) => begin
        if x0 = x then (if y0 = y then true else aux xys) else aux xys
      end // end of [rp_cons]
  // end of [aux]
in
  aux xys
end // end of [ismem]

(* ****** ****** *)

exception Fatal
fn fatal{a:viewtype} (msg: String): a = (print msg; $raise Fatal)

(* ****** ****** *)

fun playGame (cs: cards, res: rat_t): Bool = let
  // [fn*]: mutual tail-call optimization
  fn* aux_main (zs: cards, xys: ratpairlst):<cloptr1> Bool =
    case- zs of
    | x :: nil () => 
       if res = val_of_card_mac x then begin
         (print_card_mac x; print " = "; print res; true)
       end else false
    | x :: y :: zs => aux1 (x, nil (), y, nil (), zs, xys)
  // end of [aux_main]

  and aux1
    (x: card, xs: cards, y: card, ys: cards, zs: cards, xys: ratpairlst)
    :<cloptr1> Bool =
    if ismem (val_of_card_mac x, val_of_card_mac y, xys) then
      aux2 (x, xs, y, ys, zs, xys) else aux1_ (combine (x, y), x, xs, y, ys, zs, xys)
    // end of [if]
  (* end of [aux1] *)

  and aux1_
    (rs: cards, x: card, xs: cards, y: card, ys: cards, zs: cards, xys: ratpairlst)
    :<cloptr1> Bool =
    case+ rs of
    | r :: rs =>
        if aux_main (cards_append_r (xs, cards_append_r (ys, r :: zs)), rp_nil ()) then true
        else  aux1_ (rs, x, xs, y, ys, zs, xys)
      // end of [::]
    | nil () =>
        aux2 (x, xs, y, ys, zs, rp_cons (val_of_card_mac x, val_of_card_mac y, xys))
      // end of [nil]
  (* end of [aux1_] *)

  and aux2 (x: card, xs: cards, y: card, ys: cards, zs: cards, xys: ratpairlst)
    :<cloptr1> Bool =
    case+ zs of
    | z :: zs => aux1 (x, xs, z, y :: ys, zs, xys) | nil () => (
        case+ cards_append_r (ys, y :: nil ()) of
        | x1 :: y1 :: zs1 => aux1 (x1, x :: xs, y1, nil (), zs1, xys)
        | _ => false
      ) // end of [nil]
  // end of [aux2]
in
  aux_main (cs, rp_nil ())
end // end of [playGame]

val answer = i2r (int_make_int0 24)

fun play
  (n1: int, n2: int, n3: int, n4: int): Bool = let
  val c1 = card_of_int0 n1 and c2 = card_of_int0 n2
  and c3 = card_of_int0 n3 and c4 = card_of_int0 n4
  val cs = c1 :: c2 :: c3 :: c4 :: nil ()
in
  playGame (cs, answer)
end // end of [play]

#define BOUND 13
macdef BOUND_f = double_of BOUND

staload "libc/SATS/math.sats"
staload "libc/SATS/random.sats"

fn int_gen (): int =
  int_of (floor (1.0 + drand48 () * BOUND_f))
// end of [int_gen]

(* ****** ****** *)

implement main (argc, argv) = let

(*

// some interesting cases

val _ = play (1, 7, 13, 13)
val _ = play (2, 3, 5, 12)
val _ = play (3, 3, 7, 7)
val _ = play (3, 3, 8, 8)
val _ = play (4, 4, 10, 10)
val _ = play (5, 5, 7, 11)
val _ = play (5, 7, 7, 11)

*)

//
// mutual tail recursion
//
fn* loop1 (i1: int): void = begin
(*
  prerr "loop1: i1 = "; prerr i1; prerr_newline ();
*)
  if i1 <= BOUND then loop2 (i1, i1) else ()
end // end of [loop1]

and loop2 (i1: int, i2: int): void =
  if i2 <= BOUND then loop3 (i1, i2, i2) else loop1 (i1+1)
// end of [loop2]

and loop3 (i1: int, i2: int, i3: int): void =
  if i3 <= BOUND then loop4 (i1, i2, i3, i3) else loop2 (i1, i2+1)
// end of [loop3]

and loop4 (i1: int, i2: int, i3: int, i4: int): void =
  if i4 <= BOUND then let
    val () = printf ("(%i, %i, %i, %i): ", @(i1, i2, i3, i4))
    val _ = play (i1, i2, i3, i4)
    val () = print_newline ()
  in
    loop4 (i1, i2, i3, i4+1)
  end else begin
    loop3 (i1, i2, i3+1)
  end // end of [if]
// end of [loop4]

(*

val () = srand48_with_time ()
val n1 = int_gen () and n2 = int_gen () and n3 = int_gen () and n4 = int_gen ()
val () = printf ("(%i, %i, %i, %i): ", @(n1, n2, n3, n4))
val res = play (n1, n2, n3, n4)
val () = if ~res then print ("no solution.\n") else ()

*)

in

loop1 1

end // end of [main]

(* ****** ****** *)

(* end of [twentyfour.dats] *)
