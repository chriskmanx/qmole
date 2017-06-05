(*
**
** An example of parsing based on parsing combinators
**
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: December 2008

(* ****** ****** *)

%{^
#include "libc/CATS/stdio.cats"
%} // end of [%{^]

(* ****** ****** *)

staload "contrib/parcomb/SATS/parcomb.sats" ;

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/list_vt.dats"
staload _(*anonymous*) = "contrib/parcomb/DATS/parcomb.dats" ;

(* ****** ****** *)

(*
** expr = term expr'
** expr' = (PLUS | MINUS) term expr' | /* empty */
** term = term term'
** term' = (TIMES | DIVIDES) factor term' | /* empty */
** factor = integer | LPAREN expr RPAREN | UMINUS factor
*)

infix (|| + 1) wth
infixl (&& + 2) <<; infixr (&& + 1) >>
postfix ^* ^+

(* ****** ****** *)

typedef charpar (a:t@ype) = parser_t (a, char)
typedef lcharpar (a:t@ype) = lazy (charpar (a))

val anychar = any_parser<char> ()

fn litchar (c0): charpar (char) =
  anychar \sat (lam (c:char): bool =<cloref> (c0 = c))

(* ****** ****** *)

val LPAREN = litchar '\('; val RPAREN = litchar ')'
val PLUS = litchar '+'; val MINUS = litchar '-'
val TIMES = litchar '*'; val DIVIDES = litchar '/'
val UMINUS = litchar '~'

// datatype unit = unit of ()

val SPACE = (anychar \sat char_isspace): charpar (char)

val p_spaces = discard_many_parser (SPACE): charpar (unit)

val
rec
lp_digit: lcharpar int = $delay (
  (anychar \sat char_isdigit) wth digit_val_get
) where {
  fn digit_val_get (c: char):<> int = (c - '0')
} // end of [lp_digit]

and lp_integer: lcharpar int = $delay (!lp_digit^+ wth f) where {
  fn f (ds: List1 int):<0> int = loop (ds, d) where {
    val+ list_cons (d, ds) = ds
    fun loop {n:nat} .<n>.
      (ds: list (int, n), i: int):<0> int = begin case+ ds of
      | list_cons (d, ds) => loop (ds, d + 10 * i) | list_nil () => i
    end // end of [loop]
  } // end of [f]
} // end of [lp_integer]

typedef intc = int -<cloref> int
typedef intintc = @(int, intc)

val
rec lp_expr : lcharpar int = $delay (
  seq2wth_parser_fun (lzeta lp_term,  lzeta lp_expr', f)
) where {
  val f = lam (t: int, k: intc): int =<fun> k (t)
} // end of [lp_expr]

and lp_expr' : lcharpar intc = $delay (
  p_spaces >> (
    seq3wth_parser_fun
      (PLUS || MINUS, lzeta lp_term, lzeta lp_expr', f) ||
    return (lam (t: int) =<cloref> t)
  )
) where {
  fn f (c: char, t: int, k: intc):<> intc = begin
    if c = '+' then lam t0 => k (t0 + t) else lam t0 => k (t0 - t)
  end // end of [f]
} // end of [lp_expr']

and lp_term : lcharpar int = $delay (
  seq2wth_parser_fun (lzeta lp_factor, lzeta lp_term', f)
) where {
  val f = lam (t: int, k: intc): int =<fun> k (t)
} // end of [lp_term]

and lp_term' : lcharpar intc = $delay (
  p_spaces >> (
    seq3wth_parser_fun
      (TIMES || DIVIDES, lzeta lp_factor, lzeta lp_term', f) ||
    return (lam (t: int) =<cloref> t)
  )
) where {
  fn f (c: char, t: int, k: intc):<> intc = begin
    if c = '*' then lam t0 => k (t0 * t) else lam t0 => k (t0 / t)
  end // end of [f]
} // end of [lp_term']

and lp_factor : lcharpar int = $delay (
  p_spaces >> (
    !lp_integer ||
    (LPAREN >> !lp_expr << RPAREN) ||
    (UMINUS >> lzeta lp_factor wth (lam (t: int): int =<fun> ~t))
  )
) // end of [lp_factor]

(* ****** ****** *)

dynload "contrib/parcomb/dynloadall.dats"

(* ****** ****** *)

implement main () = let
  var tks = char_stream_make_file stdin_ref
  var ncur: int = 0 and nmax: int = 0
  val r = apply_parser (!lp_expr, tks, ncur, nmax)
  val i = (
    case+ r of ~Some_vt i => i | ~None_vt _ => 0
  ) : int
in
  print "The value of the expression = "; print i; print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [calc1_example.dats] *)
