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

staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload "contrib/parcomb/SATS/posloc.sats"
staload "contrib/parcomb/SATS/tokenize.sats"

(* ****** ****** *)

staload "contrib/parcomb/SATS/parcomb.sats" ;
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

typedef P (a: t@ype) = parser_t (a, token)
typedef LP (a: t@ype) = lazy (parser_t (a, token))

val anytoken = any_parser<token> ()
val anyopttoken = anyopt_parser<token> ()

val LPAREN = anytoken \sat (lam (tok: token): bool =<fun>
  case+ tok.token_node of TOKsingleton ('\(') => true | _ => false
)

val RPAREN = anytoken \sat (lam (tok: token): bool =<fun>
  case+ tok.token_node of TOKsingleton (')') => true | _ => false
)

fn litident (name0: string): P token =
  anytoken \sat (lam (tok: token): bool =<cloref>
    case+ tok.token_node of TOKide name => name0 = name | _ => false
  )
// end of [litident]

val UMINUS  = litident "~"
val PLUS    = litident "+"
val MINUS   = litident "-"
val TIMES   = litident "*"
val DIVIDES = litident "/"

(* ****** ****** *)

datatype exp_node =
  | EXPint of int | EXPuminus of exp
  | EXPadd of (exp, exp) | EXPsub of (exp, exp)
  | EXPmul of (exp, exp) | EXPdiv of (exp, exp)

where exp = '{ exp_loc= loc_t, exp_node= exp_node }
// end of [exp]

(* ****** ****** *)

fn exp_int (loc: loc_t, int: int):<> exp =
  '{ exp_loc= loc, exp_node= EXPint int }

fn exp_uminus (loc: loc_t, e: exp):<> exp =
  '{ exp_loc= loc, exp_node= EXPuminus e }

fn exp_add (loc: loc_t, e1: exp, e2: exp):<> exp =
  '{ exp_loc= loc, exp_node= EXPadd (e1, e2) }

fn exp_sub (loc: loc_t, e1: exp, e2: exp):<> exp =
  '{ exp_loc= loc, exp_node= EXPsub (e1, e2) }

fn exp_mul (loc: loc_t, e1: exp, e2: exp):<> exp =
  '{ exp_loc= loc, exp_node= EXPmul (e1, e2) }

fn exp_div (loc: loc_t, e1: exp, e2: exp):<> exp =
  '{ exp_loc= loc, exp_node= EXPdiv (e1, e2) }

(* ****** ****** *)

fun exp_eval (e: exp): int = case+ e.exp_node of
  | EXPint int => int
  | EXPuminus e => ~(exp_eval e)
  | EXPadd (e1, e2) => exp_eval e1 + exp_eval e2
  | EXPsub (e1, e2) => exp_eval e1 - exp_eval e2
  | EXPmul (e1, e2) => exp_eval e1 * exp_eval e2
  | EXPdiv (e1, e2) => exp_eval e1 / exp_eval e2
// end of [exp_eval]

(* ****** ****** *)

typedef expc = exp -<cloref> exp

val p_int =
  (anytoken \sat pred) : P token where {
  fn pred (tok: token):<> bool =
    case+ tok.token_node of TOKint _ => true | _ => false
} // end of [p_int]

val
rec lp_expr : LP exp = $delay (
  seq2wth_parser_fun (lzeta lp_term,  lzeta lp_expr', f)
) where {
  fn f (e: exp, k: expc):<> exp = k (e)
} // end of [lp_expr]

and lp_expr' : LP expc = $delay (
  seq3wth_parser_fun
    (PLUS || MINUS, lzeta lp_term, lzeta lp_expr', f) ||
  return (lam (e: exp) =<cloref> e)
) where {
  fn f (opr: token, e: exp, k: expc):<> expc =
    case+ opr.token_node of
    | TOKide "+" => lam e0 => let
        val loc = location_combine (e0.exp_loc, e.exp_loc)
      in
        k (exp_add (loc, e0, e))
      end
    | _ => lam e0 => let
        val loc = location_combine (e0.exp_loc, e.exp_loc)
      in
        k (exp_sub (loc, e0, e))
      end // end of [_]
  // end of [f]
} // end of [p_expr']

and lp_term : LP exp = $delay (
  seq2wth_parser_fun (lzeta lp_factor, lzeta lp_term', f)
) where {
  fn f (e: exp, k: expc):<> exp = k (e)
} // end of [lp_term]

and lp_term' : LP expc = $delay (
  seq3wth_parser_fun (
    TIMES || DIVIDES, lzeta lp_factor, lzeta lp_term', f
  ) ||
  return (lam (e: exp) =<cloref> e)
) where {
  fn f (opr: token, e: exp, k: expc):<> expc =
    case+ opr.token_node of
    | TOKide "*" => lam e0 => let
        val loc = location_combine (e0.exp_loc, e.exp_loc)
      in
        k (exp_mul (loc, e0, e))
      end
    | _ => lam e0 => let
        val loc = location_combine (e0.exp_loc, e.exp_loc)
      in
        k (exp_div (loc, e0, e))
      end // end of [_]
  // end of [f]
} // end of [p_term']

and lp_factor : LP exp = $delay (
  p_int wth f1 ||
  seq2wth_parser_fun (UMINUS, lzeta lp_factor, f2) ||
  (LPAREN >> !lp_expr << RPAREN)
) where {
  fn f1 (tok: token):<> exp = let
    val- TOKint int = tok.token_node in exp_int (tok.token_loc, int)
  end // end of [f1]
  fn f2 (opr: token, e: exp):<> exp = let
    val loc = location_combine (opr.token_loc, e.exp_loc)
  in
    exp_uminus (loc, e)
  end // end of [f2]
} // end of [p_fac]

(* ****** ****** *)

dynload "contrib/parcomb/dynloadall.dats"

(* ****** ****** *)

implement main () = let
  fun errck (tks: &stream token): void =
    case+ stream_get_item<token> (tks) of
    | ~Some_vt tk => begin
        prerr tk.token_loc; prerr ": error(0)";
        prerr ": there are tokens left unconsumed.";
        prerr_newline (); exit {void} (1)
      end // end of [Some]
    | ~None_vt () => ()
  // end of [errck]
  val cs = char_stream_make_file stdin_ref
  val tks0 = tokenstream_make_charstream (cs)
  var tks = tks0
  var ncur: int = 0 and nmax: int = 0
  val r = apply_parser (!lp_expr, tks, ncur, nmax)
  val i = (case+ r of
    | ~Some_vt e => begin
        let val () = errck (tks) in exp_eval e end
      end // end of [Some_vt]
    | ~None_vt _ => let
        val () = (tks := tks0; ncur := 0; errck tks) in 0
      end // end of [None_vt]
  ) : int // end of [val]
in
  print "the value of the expression = "; print i; print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [calc2_example.dats] *)
