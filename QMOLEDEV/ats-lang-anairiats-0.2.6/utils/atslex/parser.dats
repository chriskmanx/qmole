(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

fun prerr_range (): void = let
  val pos_prev = position_prev_get ()
  val pos = position_get ()
in
  prerr_pos pos_prev; prerr "-"; prerr_pos pos
end // end of [prerr_range]

fun errmsg_illegal
  {a:viewt@ype} (msg: string): a = begin
  prerr msg;
  prerr (": The token at [");
  prerr_range ();
  prerr ("] is illegal.");
  prerr_newline ();
  exit {a} (1)
end // end of [errmsg_illegal]

(* ****** ****** *)

fun errmsg_literal (c: char): void = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not ["); prerr_char (c); prerr ("].");
  prerr_newline ();
  exit {void} (1)
end // end of [errmsg_literal]

fun literal (c0: char): void = let
  val tok = token_get () in case+ tok of
  | TOKlit c when c0 = c => token_update ()
  | _ => errmsg_literal (c0)
end // end of [literal]

(* ****** ****** *)

fun errmsg_litword (s: string): void = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not ["); prerr (s); prerr ("].");
  prerr_newline ();
  exit {void} (1)
end // end of [errmsg_litword]

fun litword (s0: string): void = let
  val tok = token_get () in case+ tok of
  | TOKword s when s0 = s => token_update ()
  | _ => errmsg_litword (s0)
  // end of [case]
end // end of [litword]

(* ****** ****** *)

fun errmsg_char (): char = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not a char.");
  prerr_newline ();
  exit {char} (1)
end // end of [errmsg_char]

fun char (): char = let
  val tok = token_get () in case+ tok of
  | TOKchar c => (token_update (); c) | _ => errmsg_char ()
end // end of [char ()]

(* ****** ****** *)

fun errmsg_string (): string = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not a string.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_string]

fun string (): string = let
  val tok = token_get () in case+ tok of
  | TOKstring s => (token_update (); s) | _ => errmsg_string ()
end // end of [string ()]

(* ****** ****** *)

fun errmsg_ident (): string = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not an identifier.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_ident]

fun ident (): string = let
  val tok = token_get () in case+ tok of
  | TOKword s => (token_update (); s) | _ => errmsg_ident ()
end // end of [ident]

(* ****** ****** *)

fun errmsg_code (): string = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not code.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_code]

fun code (): string = let
  val tok = token_get () in case+ tok of
  | TOKcode s => (token_update (); s) | _ => errmsg_code ()
end // end of [code]

(* ****** ****** *)

fun charset_atm_r
  (c0: char): charset_t = let
  val tok = token_get () in case+ tok of
  | TOKword "-" => let
      val () = token_update (); val c1 = char ()
    in
      charset_interval (c0, c1)
    end // end of [TOKword]
  | _ => charset_singleton c0
  // end of [case]
end // end of [charset_atm_r]

fun charset_seq_r
  (cs0: charset_t): charset_t = let
  val tok = token_get () in case+ tok of
  | TOKchar c => let
      val () = token_update (); val cs1 = charset_atm_r c
    in
      charset_seq_r (charset_union (cs0, cs1))
    end //end of [TOKchar]
  | _ => cs0
  // end of [case]
end // end of [charset_seq_r]

(* ****** ****** *)

fun charset_r (): charset_t = let
  val tok = token_get () in case+ tok of
  | TOKlit ']' => let
      val () = token_update () in charset_nil
    end // end of [TOKlit ']']
  | TOKlit '^' => let
      val () = token_update (); val c = charset_r ()
    in
      charset_complement (c)
    end // end of [TOKlit '^']
  | TOKchar c => let
      val () = token_update ()
      val cs = charset_seq_r (charset_atm_r c)
      val () = literal ']'
    in
      cs
    end // end of [TOKchar]
  | _ => begin
      errmsg_illegal {charset_t} ("charset_r")
    end // end of [_]
  // end of [case]
end // end of [charset_seq_r]

(* ****** ****** *)

extern fun fprint_regex {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, reg: regex): void =
  "fprint_regex"

implement fprint_regex (pf_mod | fil, reg): void = begin
  case+ reg of
  | REGalt (reg1, reg2) => begin
      fprint_string (pf_mod | fil, "REGalt(");
      fprint_regex (pf_mod | fil, reg1);
      fprint_string (pf_mod | fil, ", ");
      fprint_regex (pf_mod | fil, reg2);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGalt]
  | REGchars cs => begin
      fprint_string (pf_mod | fil, "REGchars(");
      fprint_charset (pf_mod | fil, cs);
      fprint_string (pf_mod | fil, ")");
    end // end [REGchars]
  | REGid id => begin
      fprint_string (pf_mod | fil, "REGid(");
      fprint_string (pf_mod | fil, id);
      fprint_string (pf_mod | fil, ")");
    end // end of [id]
  | REGnil () => begin
      fprint_string (pf_mod | fil, "REGnil()");
    end // end of [REGnil]
  | REGopt reg => begin
      fprint_string (pf_mod | fil, "REGopt(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGopt]
  | REGplus reg => begin
      fprint_string (pf_mod | fil, "REGplus(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGplus]
  | REGrep (reg, i) => begin
      fprint_string (pf_mod | fil, "REGrep(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ", ");
      fprint_int (pf_mod | fil, i);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGrep]
  | REGseq (reg1, reg2) => begin
      fprint_string (pf_mod | fil, "REGseq(");
      fprint_regex (pf_mod | fil, reg1);
      fprint_string (pf_mod | fil, ", ");
      fprint_regex (pf_mod | fil, reg2);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGseq]
  | REGstar reg => begin
      fprint_string (pf_mod | fil, "REGstar(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGstar]
  | REGstr s => begin
      fprint_string (pf_mod | fil, "REGstr(\"");
      fprint_string (pf_mod | fil, s);
      fprint_string (pf_mod | fil, "\")");
    end // end of [REGstr]
end // end of [fprint_regex]

implement print_regex (reg) = let
   val (pf_stdout | ptr_stdout) = stdout_get ()
in
   fprint_regex (file_mode_lte_w_w | !ptr_stdout, reg);
   stdout_view_set (pf_stdout | (*none*))
end // end of [print_regex]

implement prerr_regex (reg) = let
  val (pf_stderr | ptr_stderr) = stderr_get ()
in
  fprint_regex (file_mode_lte_w_w | !ptr_stderr, reg);
  stderr_view_set (pf_stderr | (*none*))
end // end of [prerr_regex]

(* ****** ****** *)

fun is_regex_0 (): bool = begin
  case+ token_get () of
  | TOKword "_" => true
  | TOKchar c => true
  | TOKlit '$' => true
  | TOKstring s => true
  | TOKlit '\[' => true
  | TOKlit '\(' => true
  | _ => false
end // end of [is_regex_0]

fun regex_0 (): regex = let
  val tok = token_get () in case+ tok of
  | TOKword "_" => begin
      token_update (); REGchars (charset_all)
    end // end of [TOKword]
  | TOKchar c => begin
      token_update (); REGchars (charset_singleton c)
    end // end of [TOKchar]
  | TOKlit '$' => (token_update (); REGid (ident ()))
  | TOKstring s => (token_update (); REGstr s)
  | TOKlit '\[' => let
      val () = token_update (); val cs = charset_r ()
    in
      REGchars cs
    end // end of [TOKlit]
  | TOKlit '\(' => let
      val () = token_update (); val re = regex_3 (); val () = literal ')'
    in
      re
    end // end of [TOKlit]
  | _ => errmsg_illegal {regex} ("regex_0")
end // end of [regex_0]

and regex_1 (): regex = begin
  let val reg = regex_0 () in regex_1_r (reg) end
end // end of [regex_1]

and regex_1_r (reg0: regex): regex = let
  val tok = token_get () in case+ tok of
  | TOKword "*" =>
      let val () = token_update () in regex_1_r (REGstar reg0) end
  | TOKword "+" =>
      let val () = token_update () in regex_1_r (REGplus reg0) end
  | TOKlit '?' =>
      let val () = token_update () in regex_1_r (REGopt reg0) end
  | _ => reg0
  // end of [case]
end // end of [regex_1_r]

and regex_2 (): regex = regex_2_r (regex_1 ())

and regex_2_r (reg0: regex): regex = begin
  if is_regex_0 () then let
    val reg1 = regex_1 () in regex_2_r (REGseq (reg0, reg1))
  end else reg0
end // end of [regex_2_r]

and regex_3 (): regex = regex_3_r (regex_2 ())

and regex_3_r (reg0: regex): regex = let
  val tok = token_get () in case+ tok of
  | TOKword "|" => let
      val () = token_update (); val reg1 = regex_2 ()
    in
      regex_3_r (REGalt (reg0, reg1))
    end // end of [TOKword]
  | _ => reg0
  // end of [case]
end // end of [regex_3_r]

val regex = regex_3

(* ****** ****** *)

fun redef_reverse (rds: redef): redef = let
  fun loop (rds1: redef, rds2: redef): redef =
    case+ rds1 of
    | redef_cons (id, reg, rds1) =>
        loop (rds1, redef_cons (id, reg, rds2))
    | redef_nil () => rds2
  // end of [loop]
in
  loop (rds, redef_nil ())
end // end of [redef_reverse]

fun redef (rds: redef): redef = let
  val tok = token_get () in case+ tok of
  | TOKword id when id <> "%%" => let
(*
      val () = (prerr "redef: id = "; prerr id; prerr_newline ())
*)
      val () = token_update ()
      val () = litword ("=")
      val reg = regex ()
(*
      val () = (prerr "redef: reg = "; prerr_regex reg; prerr_newline ())
*)
    in
      redef (redef_cons (id, reg, rds))
    end // end of [TOKword]
  | _ => redef_reverse rds
  // end of [case]
end // end of [redef]

(* ****** ****** *)

fun rules_reverse (rls: rules): rules = let
  fun loop (rls1: rules, rls2: rules): rules =
    case+ rls1 of
    | rules_cons (r, s, rls1) =>
        loop (rls1, rules_cons (r, s, rls2))
    | rules_nil () => rls2
  // end of [loop]
in
  loop (rls, rules_nil ())
end // end of [rules_reverse]

fun barrules (rls: rules): rules = let
  val tok = token_get () in case+ tok of
  | TOKword "|" => let
      val () = token_update ()
      val reg = regex ()
      val cstr = code ()
(*
      val () = (prerr "rules: reg = "; prerr_regex reg; prerr_newline ())
      val () = (prerr "rules: cstr = "; prerr cstr; prerr_newline ())
*)
    in
      barrules (rules_cons (reg, cstr, rls))
    end // end of [TOKword "|"]
  | _ => rules_reverse rls
end // end of [barrules]

fun rules (): rules = let
  val tok = token_get () in case+ tok of
  | TOKword "|" => barrules (rules_nil ())
  | _ => let
      val reg = regex ()
(*
      val () = (prerr "rules: reg = "; prerr_regex reg; prerr_newline ())
*)
      val cstr = code ()
(*
      val () = (prerr "rules: cstr = "; prerr cstr; prerr_newline ())
*)
    in
      barrules (rules_cons (reg, cstr, rules_nil ()))
    end // end of [_]
  // end of [case]
end // end of [rules]

(* ****** ****** *)

fun lexfn_funarg (): string = case+ token_get () of
  | TOKlit '\(' => begin
      let val arg = tokenize_funarg () in token_update (); arg end
    end
  | _ => ""
// end of [lexfn_funarg]

fun lexfns (): lexfns = case+ token_get () of
  | TOKword id when id <> "%%" => let
(*
      val () = (prerr "lexfns: id = "; prerr id; prerr_newline ())
*)
      val () = token_update ()
      val arg = lexfn_funarg ()
      val () = litword "="
      val rls = rules ()
    in
      lexfns_cons (id, arg, rls, lexfns ())
    end // end of [TOKword id when ...]
  | _ => lexfns_nil ()
// end of [lexfns]

(* ****** ****** *)

fun preamble (): string = let
(*
  val () = begin
    prerr "preamble: enter"; prerr_newline ()
  end
*)
  val result = case+ token_get () of
    | TOKmark "%{" => begin
        let val s = tokenize_logue () in (token_update (); s) end
      end
    | _ => ""
  // end of [result]
(*
  val () = begin
    prerr "preamble: leave"; prerr_newline ()
  end // end of [val]
*)
in
  result
end // end of [preamble]

fun postamble (): string = case+ token_get () of
  | TOKword "%%" => let
      val s = tokenize_rest_text () in (token_update (); s)
    end // end of [TOKword "%%"]
  | _ => ""
// end of [postamble]

(* ****** ****** *)

fun done (): void = let
  val tok = token_get () in case+ tok of
  | TOKeof () => () | _ => errmsg_illegal {void} "done"
end // end of [done()]

(* ****** ****** *)

implement lexer_parse () = let
  val str1 = preamble ()
(*
  val () = (prerr "preamble =\n"; prerr str1; prerr_newline ())
*)
  val rds = redef (redef_nil ())
  val () = litword "%%"
  val lfs = lexfns ()
  val str2 = postamble ()
(*
  val () = (prerr "postamble =\n"; prerr str2; prerr_newline ())
*)
  // val () = done () // no need for this because of [postamble]
in '{
  preamble= str1, redef= rds, lexfns= lfs, postamble= str2
} end // end of [lexer_parse]

(* ****** ****** *)

(* end of [parser.dats] *)
