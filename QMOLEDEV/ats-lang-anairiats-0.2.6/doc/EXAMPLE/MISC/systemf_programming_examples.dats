//
// BU CAS CS 520: Principles of Programing Languages
// Semester: Fall 2005
//

(* ****** ****** *)

//
// Some programming examples in System F
// By Hongwei Xi (November 2, 2005)

//
// HX-2010-08-12:
// This code is updated to compile and run under ATS-0.2.1. Voila!
//

(* ****** ****** *)

//
// Implementing Church numerals in System F
//

typedef nat_f =
  {X:type} (X -<cloref> X) -<cloref> X -<cloref> X
// end of [nat_f]

(* ****** ****** *)

fn pair_get_fst {X,Y:type} '(x: X, _: Y):<> X = x
fn pair_get_snd {X,Y:type} '(_: X, y: Y):<> Y = y

(* ****** ****** *)

typedef int = uintptr
val _0 = uintptr_of_uint (0U)
val _1 = uintptr_of_uint (1U)
#define isucc succ_uintptr
fun print_nat_f (n: nat_f): void = print (n {int} (lam x => isucc x) (_0))

(* ****** ****** *)

val Z = (lam s => lam z => z): nat_f
val S = lam (n: nat_f): nat_f =<cloref> lam s => lam z => n(s)(s(z))

(* ****** ****** *)

val zero = Z
val one = S(zero)
val two = S(one)
val three = S(two)
val four = S(three)
val five = S(four)

(* ****** ****** *)

fn add (m: nat_f, n: nat_f):<> nat_f = m (S) (n)
fn mul (m: nat_f, n: nat_f):<> nat_f = m {nat_f} (n S) (Z)
fn pow (m: nat_f, n: nat_f):<> nat_f = n {nat_f-<cloref>nat_f} (m {nat_f}) (S) (Z)

(* ****** ****** *)

val () = begin
  print_string "pow (3, 5) = ";
  print_nat_f (pow (three, five));
  print_newline ()
end // end of [val]

(* ****** ****** *)

fn pred (n: nat_f):<> nat_f = let
  val z = '(zero, zero)
  val s = lam '(n1: nat_f, n2: nat_f): '(nat_f, nat_f) =<cloref> '(S n1, n1)
  val '(_, res) = n {'(nat_f, nat_f)} s z
in
   res
end // end of [pred]

(* ****** ****** *)

fn fact (n: nat_f):<> nat_f = let
  typedef X = '(nat_f, nat_f)
  val z: X = '(one, one)
  val s = lam ('(x, y): X): X =<cloref> '(S x, mul (x, y))
in
  pair_get_snd (n {X} (s) (z))
end // end of [fact]

val () = begin
  print_string "fact (5) = ";
  print_nat_f (fact five);
  print_newline ()
end // end of [val]

fun fib (n: nat_f): nat_f =
  let
     typedef X = '(nat_f, nat_f)
     val z: X = '(zero, one)
     val s = lam ('(x, y): X): X =<cloref> '(y, add (x, y))
  in
     pair_get_fst (n {X} (s) (z))
  end

val _ = begin
  print_string "fib (10) = ";
  print_nat_f (fib (add (five, five)));
  print_newline ()
end

fn ack
  (m: nat_f):<> nat_f -<cloref> nat_f =
let
  val helper =
    lam (f: nat_f -<cloref> nat_f) =<cloref>
    lam (n: nat_f): nat_f =<cloref> f (n {nat_f} (f) (one))
in
  m {nat_f -<cloref> nat_f} helper (S)
end // end of [ack]

val () = begin
  print_string "ack (3, 5) = ";
  print_nat_f (ack three five);
  print_newline ()
end // end of [val]

(* ****** ****** *)
//
// Implementing Lists in System F
//

typedef list_f (A: type) =
  {X:type} (X, (A, X) -<cloref1> X) -<cloref1> X
// endof [[list_f]

val Nil = lam {A:type}: list_f (A) => lam (n, c) => n
val Cons =
  lam {A:type} (x: A, xs: list_f A): list_f A =<cloref> lam (n, c) => c (x, xs (n, c))
// end of [Cons]

val list_length =
  lam {A:type} (xs: list_f A): int => let
    val nil = _0
    val cons = lam (_: A, i: int): int =<cloref> succ i
  in
    xs {int} (nil, cons)
  end
// end of [list_length]

val list_append =
  lam {A:type} (xs: list_f A, ys: list_f A): list_f A => xs {list_f A} (ys, Cons{A})
// end of [list_append]

val list_reverse =
  lam {A:type} (xs: list_f A): list_f A => let
     val nil = Nil {A}
     val cons =
       lam (x: A, xs: list_f A): list_f A =<cloref1> list_append (xs, Cons (x, Nil))
  in
     xs {list_f A} (nil, cons)
  end
// end of [list_reverse]

(* ****** ****** *)

(*
// Please note that the above code make use of no recursion!
*)

(* ****** ****** *)

//
// The following is some test code:
//

// condef :: = cons // HX: 2010-08-12: this is what it was
#define :: cons
#define ipred pred_uintptr

fun gen_list_f (i: Nat): list_f (int) =
  if i = 0 then Nil else Cons (uintptr_of_int1 i, gen_list_f (i - 1))
// end of [gen_list_f]

typedef intlst = list0 int

fun print_intlist (xs: intlst): void =
  case xs of
  | list0_nil () => print_newline ()
  | list0_cons (x, xs) => print_intlist_aux (x, xs)
// end of [print_intlist]

and print_intlist_aux (x: int, xs: intlst): void =
  case xs of
  | list0_nil () => (print x; print_newline ())
  | list0_cons (x', xs') => (print x; print_string ", "; print_intlist_aux (x', xs'))
// end of [print_intlist_aux]

fn print_list
  (xs: list_f int): void = let
  val nil = list0_nil ()
  val cons = lam (x: int, xs: intlst): intlst =<cloref> list0_cons (x, xs)
in
   print_intlist (xs {intlst} (nil, cons))
end // end of [print_list]

(* ****** ****** *)

val () = let
  val xs = gen_list_f (5)
  val xs' = list_reverse xs
  val xsxs = list_append (xs, xs)
  val xsxs' = list_reverse xsxs
in
//
  print_string "xs = ";
  print_list xs;
  print_string "length (xs) = ";
  print (list_length xs);
  print_newline ();
//
  print_string "xs' = ";
  print_list xs';
  print_string "list_length (xs') = ";
  print (list_length xs');
  print_newline ();
//
  print_string "xsxs = ";
  print_list xsxs;
  print_string "length (xsxs) = ";
  print (list_length xsxs);
  print_newline ();
//
  print_string "xsxs' = ";
  print_list xsxs';
  print_string "length (xsxs') = ";
  print (list_length xsxs');
  print_newline ()
end // end of [val]

(* ****** ****** *)

//
// Implementing generic trees in System F
//

(* ****** ****** *)

typedef gtree (A: type) =
  {X:type} (X, (A -<cloref> X) -<cloref> X) -<cloref> X

// E: {A:type} gtree (A)
val E = lam {A:type}: gtree (A) => lam (e, b) => e
val B = lam {A:type}
  (f: A -<cloref> gtree A): gtree (A) =<cloref> lam (e, b) => b (lam x => f (x) (e, b))
// end of [B]

(* ****** ****** *)

typedef A = int
typedef btree = gtree (A)

(* ****** ****** *)

fn print_btree
  (t: btree): void = let
  typedef X = () -<cloref1> void
  val e = (lam () => print_string "E"): X
  val b = lam (f: (A -<cloref> X)): X =<cloref>
    lam () => begin // 0/1: left/right
      print_string "B("; f (_0) (); print_string ", "; f (_1) (); print_string ")"
    end (* end of [lam] *)
  // end of [b]
in
   t {X} (e, b) ()
end // end of [print_btree]

(* ****** ****** *)

fn btree_size (t: btree):<> int = let
  typedef X = int
  val e = _0
  val b = lam (f: (A -<cloref> X)): X =<cloref> isucc (f _1 + f _0)
in
  t {X} (e, b)
end // end of [btree_size]

fn btree_height (t: btree):<> int = let
  typedef X = int
  val e = _0
  val b = lam (f: (A -<cloref> X)): X =<cloref> max (f _1, f _0) + _1
in
  t {X} (e, b)
end // end of [btree_height]

fn btree_isperfect (t: btree):<> bool = let
  typedef X = Option int
  val e = (Some _0): X
  val b = lam (f: A -<cloref> X): X =<cloref>
    case+ (f _0, f _1) of
    | (Some h1, Some h2) =>
        if h1 = h2 then Some (isucc h1) else None
    | (_, _) => None
  // end of [val]
in
   case+ t {X} (e, b) of None () => false | Some _ => true
end // end of [btree_isperfect]

(* ****** ****** *)

fn btree_left
  (t: btree):<> btree = let
  typedef X = '(btree, btree)
  val e = '(E, E): X
  val b = lam
    (f: (A -<cloref> X)): X =<cloref> let
    val '(t1, _) = f (_1); val '(t2, _) = f (_0)
    val f1 = lam (x: A): btree =<cloref> if x > _0 then t1 else t2
  in
    '(B {A} (f1), t2)
  end // end of [val]
in
  pair_get_snd (t {X} (e, b))
end // end of [left_child_tree]

fn btree_right
  (t: btree):<> btree = let
  typedef X = '(btree, btree)
  val e = '(E, E): X
  val b = lam
    (f: (A -<cloref> X)): X =<cloref> let
    val '(t1, _) = f (_1); val '(t2, _) = f (_0)
    val f1 = lam (x: A): btree =<cloref> if x > _0 then t1 else t2
  in
    '(B {A} (f1), t1)
  end // end of [val]
in
  pair_get_snd (t {X} (e, b))
end // end of [right_child_tree]

(* ****** ****** *)

val t0 = E: btree
val t1: btree = let
  val f = lam (x: int): btree =<cloref> if x <= _0 then t0 else t0
in
  B {A} (f)
end // end of [val]
val t2: btree = let
  val f = lam (x: int): btree =<cloref> if x <= _0 then t1 else t0
in
  B {A} (f)
end // end of [val]

val () = begin
  print_string "The tree t2 = "; print_btree t2; print_newline ()
end // end of [val]

(* ****** ****** *)

val t3: btree = let
  val f = lam (x: int): btree =<cloref> if x <= _0 then t1 else t2
in
   B {A} (f)
end // end of [val]

val () = begin
  print_string "The tree t3 = "; print_btree t3; print_newline ()
end // end of [val]

(* ****** ****** *)

val isperfect = btree_isperfect (t1)
val () = (print "isperfect(t1) = "; print isperfect; print_newline ())
val isperfect = btree_isperfect (t3)
val () = (print "isperfect(t3) = "; print isperfect; print_newline ())

(* ****** ****** *)

val t31: btree = btree_left (t3)
val t32: btree = btree_right (t3)

val () = begin
  print_string "The left subtree of t3 = "; print_btree t31; print_newline ();
  print_string "The right subtree of t3 = "; print_btree t32; print_newline ();
end // end of [val]

(* ****** ****** *)

val s: int = btree_size (t3)
val h: int = btree_height (t3)

val () = begin
  print_string "The size of t3 = "; print s; print_newline ();
  print_string "The height of t3 = "; print h; print_newline ();
end // end of [val]

(* ****** ****** *)

implement main () = ()

(* ****** ****** *)

(* end of [systemf_programming_examples.dats] *)
