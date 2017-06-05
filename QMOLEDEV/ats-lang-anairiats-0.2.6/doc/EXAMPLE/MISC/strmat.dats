//
//
// Author: Hongwei Xi (May 2007)
//
//

// An implementation of string matching in continuation-passing style.
// The code is translated by Hongwei Xi from an earlier version in DML,
// which was originally adopted by him from a version by Frank Pfenning
// (fp+ AT cs DOT cmu DOT edu)

datatype regexp (int) =
  | Any_char(1) (* any character *)
  | Empty(1) (* empty string matches Empty *)
  | Char(1) of Char (* "c" matches Char (c) *)
  // every char other than "c" matches Char (c)
  | Char_not(1) of Char
  // every char in [c1, c2] matches Chars (c1, c2)
  | Chars(1) of (Char, Char)
  // every char not in [c1, c2] matches Chars (c1, c2)
  | Chars_not(1) of (Char, Char)
  // cs matches Alt(p1, p2) if cs matches either p1 or p2
  | {i,j:nat} Alt(i+j+1) of (regexp i, regexp j)
  // cs matches Seq(p1, p2) if a prefix of cs matches p1 and the rest matches p2
  | {i,j:nat} Seq(i+j+1) of (regexp i, regexp j)
  // cs matches Star(p) if cs matches some, possibly 0, copies of p
  | {i:nat} Star(i+1) of regexp i

typedef Regexp = [i:nat] regexp i

// Note that [acc] is verified to be terminating!

fun acc {i0,n,i:nat} {b:two | i+b <= i0} .<n,i>. // metric for termination verification
   (cs0: string i0, i0: int i0, p: regexp n, i: int i, b: int b,
    k: {i':nat; b':two | i'+b' <= i+b} (int i', int b') -<cloref1> Bool): Bool =
  case+ p of
    | Any_char () => if i > 0 then k (i-1, 1) else false

    | Empty () => k (i, b)

    | Char c =>
      if i > 0 then (if c = cs0[i0-i] then k (i-1, 1) else false) else false

    | Char_not c =>
      if i > 0 then (if c <> cs0[i0-i] then k (i-1, 1) else false) else false

    | Chars (c1, c2) =>
      if i > 0 then let
        val c = cs0[i0-i]
      in
        if c1 <= c then (if c <= c2 then k (i-1, 1) else false) else false
      end else false

    | Chars_not (c1, c2) =>
      if i > 0 then let
        val c = cs0[i0-i]
      in 
        if c < c1 then k (i-1, 1) else if c > c2 then k (i-1, 1) else false
      end else false

    | Alt (p1, p2) =>
      if acc (cs0, i0, p1, i, b, k) then true else acc (cs0, i0, p2, i, b, k)

    | Seq (p1, p2) =>
      acc (cs0, i0, p1, i, b, lam (i', b') => acc (cs0, i0, p2, i', b', k))

    | Star p0 =>
      if k (i, b) then true else begin
        acc (cs0, i0, p0, i, 0,
          lam (i', b') => if b' = 0 then false else acc (cs0, i0, p, i', 1, k)
        ) // end of [acc]
      end // end of [if]

extern fun accept (cs0: String, p: Regexp): Bool

implement accept (cs0, p) = let
  val i0 = string_length cs0; val i0 = int1_of_size1 i0
in
  acc (cs0, i0, p, i0, 0, lam (i, _) => i = 0)
end // end of [accept]

// some tests

val regexp_digit = Chars ('0', '9') 
val regexp_digits = Star (Chars ('0', '9'))

val regexp_uint =
  Alt (Char '0', Seq (Chars ('1', '9'), regexp_digits))

val regexp_int =
  Alt (regexp_uint, Seq (Alt (Char '-', Char '+'), regexp_uint))

val regexp_dot_sats =
  Seq (Star Any_char, Seq (Char '.', Seq (Char 's', Seq (Char 'a', Seq (Char 't', Char 's')))))

val regexp_dot_dats =
  Seq (Star Any_char, Seq (Char '.', Seq (Char 'd', Seq (Char 'a', Seq (Char 't', Char 's')))))

implement main (argc, argv) = let
  val ans10 = accept ("123456789", regexp_int)
  val ans11 = accept ("+123456789", regexp_int)
  val ans12 = accept ("-123456789", regexp_int)
  val ans20 = accept ("abcde", regexp_int)
  val ans31 = accept ("abcde.sats", regexp_dot_sats)
  val ans32 = accept ("abcde.sats", regexp_dot_dats)
  val ans41 = accept ("abcde.dats", regexp_dot_sats)
  val ans42 = accept ("abcde.dats", regexp_dot_dats)
in
  print ("ans10(true) = "); print ans10; print_newline ();
  print ("ans11(true) = "); print ans11; print_newline ();
  print ("ans12(true) = "); print ans12; print_newline ();
  print ("ans20(false) = "); print ans20; print_newline ();
  print ("ans31(true) = "); print ans31; print_newline ();
  print ("ans32(false) = "); print ans32; print_newline ();
  print ("ans41(false) = "); print ans41; print_newline ();
  print ("ans42(true) = "); print ans42; print_newline ();
end // end of [main]

(* ****** ****** *)

(* end of [strmat.dats] *)
