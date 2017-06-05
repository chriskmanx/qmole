(*
**
** An example of parsing based on parsing combinators
** The grammar used in this example is adopted from the
** straightline calculator example in Appel's compiler
** book (ML version): pages 10, 11, 12
**
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: January 2009

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
staload _(*anonymous*) = "contrib/parcomb/DATS/parcomb.dats"

(* ****** ****** *)

(*
** Stm := id := Exp
** Stm := print (ExpLst)
** Stm := Stm ; Stm
** Exp := id | num | Exp Binop Exp | (Stm, Exp)
** ExpLst := Exp, ExpLst
** ExpLst := Exp
** Binop := + | - | * | /
*)

infix (|| + 1) wth
infixl (&& + 2) <<; infixr (&& + 1) >>
postfix ^* ^+

(* ****** ****** *)

typedef P (a: t@ype) = parser_t (a, token)
typedef LP (a: t@ype) = lazy (parser_t (a, token))

val anytoken = any_parser<token> ()
val anyopttoken = anyopt_parser<token> ()

//

fn litchar (c0: char): P token =
  anytoken \sat (lam (tok: token): bool =<cloref>
    case+ tok.token_node of TOKsingleton c => c0 = c | _ => false
  )
// end of [litchar]

val LPAREN = litchar '\('
val RPAREN = litchar ')'

val COMMA = litchar ','
val SEMICOLON = litchar ';'

//

fn litident (name0: string): P token =
  anytoken \sat (lam (tok: token): bool =<cloref>
    case+ tok.token_node of TOKide name => name0 = name | _ => false
  )
// end of [litident]

val PLUS    = litident "+"
val MINUS   = litident "-"
val TIMES   = litident "*"
val DIV     = litident "/"

val ASSGN   = litident ":="
val PRINT   = litident "print"

(* ****** ****** *)

abstype ident_t

extern fun ident_make (name: string):<> ident_t
extern fun fprint_ident (out: FILEref, id: ident_t): void
extern fun eq_ident_ident (_: ident_t, _: ident_t): bool
overload = with eq_ident_ident

local

assume ident_t = string

in

implement ident_make (name) = name
implement fprint_ident (out, id) = fprint_string (out, id)
implement eq_ident_ident (id1, id2) = eq_string_string (id1, id2)

end // end of [local]

(* ****** ****** *)

datatype binop = Plus | Minus | Times | Div

datatype stm_node =
  | CompoundStm of (stm, stm)
  | AssignStm of (ident_t, exp)
  | PrintStm of explst

and exp_node =
  | IdExp of ident_t
  | NumExp of int
  | OpExp of (exp, binop, exp)
  | EseqExp of (stm, exp)

where stm = '{ stm_loc= location_t, stm_node= stm_node }
  and exp = '{ exp_loc= location_t, exp_node= exp_node }
  and explst = List exp

(* ****** ****** *)

extern fun fprint_binop (out: FILEref, opr: binop): void

implement fprint_binop (out, opr) = case+ opr of
  | Plus () => fprint_string (out, "+")
  | Minus () => fprint_string (out, "-")
  | Times () => fprint_string (out, "*")
  | Div () => fprint_string (out, "/")
// end of [fprint_binop]

extern fun fprint_stm (out: FILEref, stm: stm): void
extern fun fprint_exp (out: FILEref, exp: exp): void
extern fun fprint_explst (out: FILEref, exps: explst): void

implement fprint_stm (out, stm) = case+ stm.stm_node of
  | CompoundStm (stm1, stm2) => begin
      fprint_string (out, "(");
      fprint_stm (out, stm1); fprint_string (out, "; "); fprint_stm (out, stm2);
      fprint_string (out, ")");
    end
  | AssignStm (id1, exp2) => begin
      fprint_ident (out, id1); fprint_string (out, " := "); fprint_exp (out, exp2)
    end
  | PrintStm (exps) => begin
      fprint_string (out, "print ("); fprint_explst (out, exps); fprint_string (out, ")")
    end
// end of [fprint_stm]

implement fprint_exp (out, exp) = case+ exp.exp_node of
  | IdExp (id) => fprint_ident (out, id)
  | NumExp (int) => fprint_int (out, int)
  | OpExp (exp1, opr, exp2) => begin
      fprint_string (out, "(");
      fprint_exp (out, exp1);
      fprint_string (out, " "); fprint_binop (out, opr); fprint_string (out, " ");
      fprint_exp (out, exp2);
      fprint_string (out, ")")
    end
  | EseqExp (stm1, exp2) => begin
      fprint_string (out, "(");
      fprint_stm (out, stm1); fprint_string (out, "; "); fprint_exp (out, exp2);
      fprint_string (out, ")")
    end
// end of [fprint_exp]

implement fprint_explst (out, exps) = loop (out, exps, 0) where {
  fun loop (out: FILEref, exps: explst, i: int): void = case+ exps of
    | list_cons (exp, exps) => begin
        if i > 0 then fprint_string (out, ", ");
        fprint_exp (out, exp);
        loop (out, exps, i+1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]    
} // end of [fprint_explst]

(* ****** ****** *)

fn fCompoundStm (stm1: stm, stm2: stm):<> stm = let
  val loc = location_combine (stm1.stm_loc, stm2.stm_loc)
in
  '{ stm_loc= loc, stm_node= CompoundStm (stm1, stm2) }
end // end of [fCompoundStm]

fn fAssignStm (tok1: token, exp2: exp):<> stm = let
  val- TOKide name = tok1.token_node
  val loc1 = tok1.token_loc; val id1 = ident_make name
  val loc = location_combine (loc1, exp2.exp_loc)
in
  '{ stm_loc= loc, stm_node= AssignStm (id1, exp2) }
end // end of [fAssignStm]

fn fPrintStm
  (tok1: token, exps2: explst, tok3: token):<> stm = let
  val loc = location_combine (tok1.token_loc, tok3.token_loc)
in
  '{ stm_loc= loc, stm_node= PrintStm (exps2) }
end // end of [fPrintStm]

(* ****** ****** *)

fn fIdExp (tok: token):<> exp = let
  val loc = tok.token_loc
  val- TOKide (name) = tok.token_node
  val id = ident_make (name)
in '{
  exp_loc= loc, exp_node= IdExp (id)
} end // end of [fIdExp]

fn fNumExp (tok: token):<> exp = let
  val- TOKint int = tok.token_node in '{
  exp_loc= tok.token_loc, exp_node= NumExp (int)
} end // end of [fNumExp]

fn fOpExp (exp1: exp, opr: binop, exp2: exp):<> exp = let
  val loc = location_combine (exp1.exp_loc, exp2.exp_loc)
in
  '{ exp_loc= loc, exp_node= OpExp (exp1, opr, exp2) }
end // end of [fOpExp]

fn fEseqExp
  (tok1: token, stm2: stm, exp3: exp, tok4: token):<> exp = let
  val loc = location_combine (tok1.token_loc, tok4.token_loc)
in
  '{ exp_loc= loc, exp_node= EseqExp (stm2, exp3) }
end // end of [fEseqExp]

(* ****** ****** *)

typedef stmc = stm -<cloref> stm
typedef expc = exp -<cloref> exp

val p_ide =
  (anytoken \sat pred) : P token where {
  fn pred (tok: token):<> bool =
    case+ tok.token_node of TOKide _ => true | _ => false
} // end of [p_ide]

val p_num =
  (anytoken \sat pred) : P token where {
  fn pred (tok: token):<> bool =
    case+ tok.token_node of TOKint _ => true | _ => false
} // end of [p_num]

//

val
rec lp_stm0 : LP stm = $delay (
  seq2wth_parser_fun (p_ide, ASSGN >> lzeta lp_exp, fAssignStm) ||
  seq3wth_parser_fun (PRINT, LPAREN >> lzeta lp_explst, RPAREN, fPrintStm)
) // end of [lp_stm0]

and lp_stm0_c : LP stmc = $delay (
  seq2wth_parser_fun<stm,stmc,stmc> (SEMICOLON >> !lp_stm0, lzeta lp_stm0_c, f) ||
  return (lam (stm: stm) =<cloref> stm)
) where {
  fn f (stm1: stm, stmc: stmc):<> stmc = lam (stm0: stm) => stmc (fCompoundStm (stm0, stm1))
} // end of [lp_stm_cont]

and lp_stm : LP stm = $delay (
  seq2wth_parser_fun<stm,stmc,stm>
    (!lp_stm0, !lp_stm0_c, lam (stm, stmc) => stmc (stm))
) // end of [lp_stm1]

and lp_exp0 : LP exp = $delay (
  p_ide wth fIdExp || p_num wth fNumExp ||
  seq4wth_parser_fun
    (LPAREN, !lp_stm, COMMA >> lzeta lp_exp, RPAREN, fEseqExp)
) // end of [lp_exp0]

and lp_exp0_c : LP expc = $delay (
  seq3wth_parser_fun (TIMES || DIV, !lp_exp0, lzeta lp_exp0_c, f) ||
  return (lam (exp: exp) =<cloref> exp)
) where {
  fn f (tok: token, exp1: exp, expc: expc):<> expc = let
    val opr = let
      val- TOKide name = tok.token_node
    in
      case+ name of "*" => Times () | _ => Div ()
    end : binop
  in
    lam exp0 => expc (fOpExp (exp0, opr, exp1))
  end // end of [f]
} // end of [lp_exp0_c]

and lp_exp1 : LP exp = $delay (
  seq2wth_parser_fun<exp,expc,exp>
    (!lp_exp0, !lp_exp0_c, lam (exp, expc) => expc (exp))
) // end of [lp_exp1]

and lp_exp1_c : LP expc = $delay (
  seq3wth_parser_fun (PLUS || MINUS, !lp_exp1, lzeta lp_exp1_c, f) ||
  return (lam (exp: exp) =<cloref> exp)
) where {
  fn f (tok: token, exp1: exp, expc: expc):<> expc = let
    val opr = let
      val- TOKide name = tok.token_node
    in
      case+ name of "+" => Plus () | _ => Minus ()
    end : binop
  in
    lam exp0 => expc (fOpExp (exp0, opr, exp1))
  end // end of [f]
} // end of [lp_exp0_c]

and lp_exp : LP exp = $delay (
  seq2wth_parser_fun<exp,expc,exp>
    (!lp_exp1, !lp_exp1_c, lam (exp, expc) => expc (exp))
) // end of [lp_exp]

(*

and lp_explst_c : LP explst = $delay (
  seq2wth_parser_fun<exp, explst, explst>
    (COMMA >> !lp_exp, lzeta lp_explst_c, lam (e, es) => list_cons (e, es)) ||
  return (list_nil ())
)

and lp_explst : LP (explst) = $delay (
  seq2wth_parser_fun<exp, explst, explst>
    (!lp_exp, !lp_explst_c, lam (e, es) => list_cons (e, es))
) // end of [lp_explst]

*)

and lp_explst : LP (explst) = $delay (repeat0_sep_parser (!lp_exp, COMMA))

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

extern fun parse_from_stdin (): stm

implement parse_from_stdin () = let
  val cs = char_stream_make_file stdin_ref
  val tks0 = tokenstream_make_charstream (cs)
  var tks: stream token = tks0
  var ncur: int = 0 and nmax: int = 0
  val r = apply_parser (!lp_stm, tks, ncur, nmax)
  val stm = (case+ r of
    | ~Some_vt stm => stm | ~None_vt _ => let
        val () = parse_failure (tks, ncur, nmax) in exit {stm} (1)
      end // end of [Fail_vt]
  ) : stm // end of [val]
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
in
  stm
end // end of [parse_from_stdin]


(* ****** ****** *)

abstype table_t

extern fun table_make (): table_t
extern fun lookup (tbl: table_t, x: ident_t): int
extern fun update (tbl: table_t, x: ident_t, v: int): table_t

local

assume table_t = List @(ident_t, int)

in

implement table_make () = list_nil ()

implement lookup (tbl, x0) = case+ tbl of
  | list_cons (xv, tbl) => if (x0 = xv.0) then xv.1 else lookup (tbl, x0)
  | list_nil () => begin
      0 // if a variable is not defined; this is convenient but not such a good style
    end // end of [list_nil]
// end of [lookup]

implement update (tbl, x0, v_new) = case+ tbl of
  | list_cons (xv, tbl) => begin
      if x0 = xv.0 then
        list_cons (@(x0, v_new), tbl)
      else
        list_cons (xv, update (tbl, x0, v_new))
      // end of [if]
    end // end of [list_cons]
  | list_nil () => list_cons (@(x0, v_new), list_nil ())
// end of [update]

end // end of [local]

(* ****** ****** *)

extern fun interpExp (tbl: &table_t, _: exp): int
extern fun interpStm (tbl: &table_t, _: stm): void

(* ****** ****** *)

implement interpExp (tbl, exp) =
  case+ exp.exp_node of
  | IdExp id => let val v = lookup (tbl, id) in v end
  | NumExp v => v
  | OpExp (exp1, binop, exp2) => let
      val v1 = interpExp (tbl, exp1)
      val v2 = interpExp (tbl, exp2)
    in
      case+ binop of
      | Plus () => v1 + v2 | Minus () => v1 - v2 | Times () => v1 * v2 | Div () => v1 / v2
    end // end of [OpExp]
  | EseqExp (stm, exp) => let
      val () = interpStm (tbl, stm) in interpExp (tbl, exp)
    end // end of [EseqExp]
// end of [interpExp]

implement interpStm (tbl, stm) = case+ stm.stm_node of
  | CompoundStm (stm1, stm2) => let
      val () = interpStm (tbl, stm1) in interpStm (tbl, stm2)
    end // end of [CompoundStm]
  | AssignStm (id, exp) => let
      val v = interpExp (tbl, exp)
      val tbl_new = update (tbl, id, v)
    in
      tbl := tbl_new
    end // end of [AssignStm]
  | PrintStm (exps) => loop (tbl, exps, 0) where {
      fun loop (tbl: &table_t, exps: explst, i: int): void =
        case+ exps of
        | list_cons (exp, exps) => let
            val v = interpExp (tbl, exp)
          in
            if i > 0 then print ' '; print v; loop (tbl, exps, i+1)
          end // end of [list_cons]
        | list_nil () => print_newline ()
    } // end of [PrintStm]
// end of [interpStm]
    
(* ****** ****** *)

extern fun interp (_: stm): void

implement interp (stm) = let
  var tbl0 = table_make (); val () = interpStm (tbl0, stm)
in
  // empty
end // end of [interp]

(* ****** ****** *)

dynload "contrib/parcomb/dynloadall.dats"

(* ****** ****** *)

(*
// HX; heere is an example:
a := 5 + 3; b := (print (a, a-1), 10 * a); print (b)
*)

(* ****** ****** *)

implement main () = let
  val () = begin
    print "Please input a statement on the next line:\n"
  end // end of [val]
  val stm = parse_from_stdin ()
in
  interp (stm)
end // end of [main]

(* ****** ****** *)

(* end of [calc3_example.dats] *)
