(*
**
** An example of parsing based on parsing combinators
**
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: January 2009

(* ****** ****** *)

%{^
#include "libc/CATS/stdio.cats"
%} // end of [%{^]

(* ****** ****** *)

staload "contrib/parcomb/SATS/posloc.sats"
staload "contrib/parcomb/SATS/tokenize.sats"

(* ****** ****** *)

staload "contrib/parcomb/SATS/parcomb.sats" ;
staload _(*anonymous*) = "contrib/parcomb/DATS/parcomb.dats"

(* ****** ****** *)

(*
** term_atm = var | ( term )
** term     = \ vars . term | term_atm term_rst
** term_rst = (*empty*) | term_atm term_rst
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

val BACKSLASH  = litident "\\"
val DOT  = litident "."

(* ****** ****** *)

datatype term_node =
  | Var of string | Lam of (string, term) | App of (term, term)

where term = '{
  term_loc= location_t, term_node= term_node
} // end of [term]

fun print_term (t: term): void = case+ t.term_node of
  | Var name => (print "Var("; print name; print ")")
  | Lam (name, t) => begin
      print "Lam("; print name; print "; "; print_term t; print ")"
    end // end of [Lam]
  | App (t1, t2) => begin
      print "App("; print_term t1; print ", "; print_term t2; print ")"
    end // end of [App]
// end of [print_term]

overload print with print_term

(* ****** ****** *)

fn term_var (loc: loc_t, name: string):<> term =
  '{ term_loc= loc, term_node= Var (name) }
// end of [term_var]

fn term_lam (loc: loc_t, x: string, t_body: term):<> term =
  '{ term_loc= loc, term_node= Lam (x, t_body) }
// end of [term_lam]

fn term_app (loc: loc_t, t1_fun: term, t2_arg: term):<> term =
  '{ term_loc= loc, term_node= App (t1_fun, t2_arg) }
// end of [term_app]

(* ****** ****** *)

typedef termc = term -<cloref> term

fn isIdSymFst (c: char):<> bool = begin
  if char_isalpha c then true else string_contains ("_'", c)
end // end of [isIdSymFst]

fn isIdSymRst (c: char):<> bool = begin
  if isIdSymFst c then true else char_isdigit (c)
end // end of [isIdSymRst]

fn isKeyword (name: string):<> bool = false
fn isVar (name: string):<> bool = let
  fun loop {n,i:nat | i <= n} .<n-i>.
    (name: string n, i: size_t i):<> bool =
    if string_is_at_end (name, i) then true else let
      val c = $effmask_ref (name[i])
    in
      if isIdSymRst (c) then loop (name, i+1) else false
    end // end of [if]
  // end of [loop]
in
  if isKeyword name then false else let
    val [n:int] name = string1_of_string (name)
  in
    if string_is_at_end (name, 0) then false else let
      val c0 = $effmask_ref (name[0])
    in
      if isIdSymFst c0 then loop (name, 1) else false
    end // end of [if]
  end // end of [isKeyword]
end // end of [isVar]

(* ****** ****** *)

val p_var =
  (anytoken \sat pred) : P token where {
  fn pred (tok: token):<> bool =
    case+ tok.token_node of TOKide name => isVar name | _ => false
} // end of [p_int]

val p_vars = (p_var)^* : P (List token)

val
rec lp_term : LP term = $delay (
  seq2wth_parser_fun (BACKSLASH >> p_vars, DOT >> lzeta lp_term, f_lams) ||
  seq2wth_parser_fun (lzeta lp_term_atm, lzeta lp_term_rst, f_app)
) where {
  fn f_lams
    (toks: List token, t_body: term):<> term = aux (toks, t_body) where {
    fun aux {n:nat} .<n>.
      (toks: list (token, n), t_body: term):<> term =
      case+ toks of
      | list_cons (tok, toks) => let
          val- TOKide name = tok.token_node
          val t_body = aux (toks, t_body)
          val loc = location_combine (tok.token_loc, t_body.term_loc)
        in
          term_lam (loc, name, t_body)
        end // end of [list_cons]
      | list_nil () => t_body
  } // end of [f_lams]
  fn f_app (t: term, tc: termc):<> term = tc (t)
} // end of [lp_term]

and lp_term_atm : LP term = $delay (
  p_var wth f_var || LPAREN >> !lp_term << RPAREN
) where {
  fn f_var (tok: token):<> term = let
    val- TOKide name = tok.token_node
  in
    term_var (tok.token_loc, name)
  end // end of [f_var]
} // end of [lp_term_atm]

and lp_term_rst : LP termc = $delay (
  seq2wth_parser_fun (!lp_term_atm, lzeta lp_term_rst, f) ||
  return (lam (t: term): term =<cloref> t)
) where {
  fn f (t: term, tc: termc):<> termc =
    lam (t0) => let
      val loc = location_combine (t0.term_loc, t.term_loc)
    in
      tc (term_app (loc, t0, t))
    end // end of [lam]
  // end of [f]
} // end of [lp_term_rst]

(* ****** ****** *)

dynload "contrib/parcomb/dynloadall.dats"

(* ****** ****** *)

extern fun parse_failure
  (tks: stream token, ncur: int, nmax: int): void

implement parse_failure (tks, ncur, nmax) = let
  fun loop
    (tks: stream token, n: int): Option_vt (token) =
    case+ !tks of
    | stream_cons (tk, tks) =>
        if n > 0 then loop (tks, n-1) else Some_vt (tk)
    | stream_nil () => None_vt ()
  // end of [loop]
  val otk = loop (tks, nmax - ncur)
in
  case+ otk of
  | ~Some_vt tk => begin
      prerr_location tk.token_loc;
      prerr ": exit";
      prerr ": parsing failure";
      prerr_newline ()
    end // end of [Some_vt]
  | ~None_vt () => begin
      prerr ": exit";
      prerr ": parsing failure at the end of the token stream.";
      prerr_newline ()
    end // end of [None_vt]
end // end of [parse_failure]

(* ****** ****** *)

implement main () = let
  val cs = char_stream_make_file stdin_ref
  val tks0 = tokenstream_make_charstream (cs)
  var tks: stream token = tks0
  var ncur: int = 0 and nmax: int = 0
  val otrm = apply_parser (!lp_term, tks, ncur, nmax)
  val trm = case+ otrm of
    | ~Some_vt trm => trm
    | ~None_vt () => let
        val () = parse_failure (tks, ncur, nmax) in exit {term} (1)
      end // end of [None_vt]
  // end of [val]
  val otk = stream_get_item<token> (tks)  
  val () = (case+ otk of
    | ~Some_vt tk => begin
        prerr_location tk.token_loc;
        prerr ": exit";
        prerr ": parsing failure: unconsumed token";
        prerr_newline ();
        exit {void} (1)
      end // end of [Some]
    // there are no unconsumed tokens
    | ~None_vt () => ()
  ) : void // end of [token]
  val () = begin
    print "trm = "; print trm; print_newline ()
  end // end of [val]
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [lambda_example.dats] *)
