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
**
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: July 2007
//
(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

extern fun errmsg {a:viewt@ype} (msg: string): a
implement errmsg (msg) = (prerr msg; prerr_newline (); exit 1)

(* ****** ****** *)

#define BASE 8
#define c2i int_of_char
#define i2c char_of_int1

(* ****** ****** *)

typedef intch = intBtw (~1, UCHAR_MAX+1)

extern fun char_get (): intch = "char_get"
extern fun char_update (): void = "char_update"
extern fun char_get_update (): intch = "char_get_update"
extern fun char_update_get (): intch = "char_update_get"

extern fun pos_prev_reset (): void = "pos_prev_reset"

%{^

static int the_line_cnt = 1 ;
static int the_line_cnt_prev = 1 ;
static int the_char_cnt = 0 ;
static int the_char_cnt_prev = 0 ;

ats_int_type
pos_get_line () { return the_line_cnt ; }
ats_int_type
pos_get_char () { return the_char_cnt ; }

ats_int_type
pos_get_line_prev () { return the_line_cnt_prev ; }
ats_int_type
pos_get_char_prev () { return the_char_cnt_prev ; }

ATSinline()
ats_void_type
pos_prev_reset (
// there is no argument for this fun
) {
  the_line_cnt_prev = the_line_cnt ;
  the_char_cnt_prev = the_char_cnt ;
  return ;  
} // end of [pos_prev_reset]

/* ****** ****** */

ATSinline()
ats_void_type
pos_advance (ats_char_type c) {
  switch (c) {
    case '\n':
      ++the_line_cnt ; the_char_cnt = 0 ; break ;
    default: ++the_char_cnt ; break ;
  } /* end of [switch] */
  return ;
}

static ats_int_type the_char ;

ATSinline()
ats_int_type char_get() { return the_char ; }

ATSinline()
ats_void_type
char_update() { 
  the_char = atslex_getchar () ;
  pos_advance (the_char) ;
  return ;
} // end of [char_update]

ATSinline()
ats_int_type
char_get_update() {
  int c = the_char ;
  the_char = atslex_getchar () ;
  pos_advance (the_char) ;
  return c ;
} // end of [char_get_update]

ATSinline()
ats_int_type
char_update_get() {
  the_char = atslex_getchar () ;
  pos_advance (the_char) ;
  return the_char ;
} // end of [char_update_get]

%} // end of [%{^]

(* ****** ****** *)

dataviewtype chars (int) =
  | chars_nil (0)
  | {n:nat} chars_cons (n+1) of (char, chars n)
// end of [chars]

#define nil chars_nil
#define :: chars_cons

extern
fun chars_is_nil
  {n:nat} (cs: !chars n): bool (n == 0) = "chars_is_nil"
// end of [chars_is_nil]

implement
chars_is_nil (cs) = case+ cs of
  | nil () => (fold@ cs; true) | _ :: _ => (fold@ cs; false)
// end of [chars_is_nil]

extern
fun chars_uncons {n:pos}
  (cs: &chars n >> chars (n-1)): char = "chars_uncons"
// end of [chars_uncons]

implement
chars_uncons (cs) =
  let val+ ~(c :: cs1) = cs in cs := cs1; c end
// end of [chars_uncons]

fun chars_free
  {n:nat} (cs: chars n): void =
  case+ cs of ~(c :: cs) => chars_free cs | ~nil () => ()
// end of [chars_free]

(* ****** ****** *)

extern // [cs] must not contain null bytes
fun string_make_charlst_rev_int
  {n:nat} (
  cs: chars n, n: int n
) : string n =
  "string_make_charlst_rev_int"
// end of [fun]

%{$

ats_ptr_type
string_make_charlst_rev_int
  (ats_ptr_type cs, const ats_int_type n) {
  char *s0, *s;
  s0 = ats_malloc_gc(n+1) ; s = s0 + n ; *s = '\0' ; --s ;
  while (!chars_is_nil(cs)) { *s = chars_uncons(&cs) ; --s ; }
  return s0 ;
} /* string_make_charlst_rev_int */

%} // end of [%{$]

(* ****** ****** *)

implement
tokenize_line_comment () = loop () where {
  fun loop (): void = let
    val c = char_get () in
    if c >= 0 then let
      val c = i2c c in char_update (); if c <> '\n' then loop () else ()
    end else begin // end of [if]
      // loop returns
    end // end of [if]
  end // end of [let]
} // end of [tokenize_line_comment]

(* ****** ****** *)

implement
tokenize_rest_text () = loop (nil (), 0) where {
  fun loop {n:nat} (cs: chars n, n: int n): string = let
    val c = char_get () in
    if c >= 0 then let
      val c = i2c c in char_update (); loop (c :: cs, n+1)
    end else begin // c = EOF
      string_make_charlst_rev_int (cs, n) // the end of file is reached
    end // end of [if]
  end // end of [let]
} // end of [tokenize_rest_text]

(* ****** ****** *)

fn errmsg_unclosed_logue
  (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The logue starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_logue]

implement
tokenize_logue () =
  loop (nil (), 0, 0) where {
  fun loop {n,level:nat}
    (cs: chars n, n: int n, level: int level): string = let
    val c = char_get ()
  in
    if c >= 0 then let
      val c = i2c c in case+ c of
      | '%' => let
          val c1 = char_update_get () in
          if c1 >= 0 then let
            val c1 = i2c c1 in case+ c1 of
            | '}' => if level > 0 then begin
                char_update (); loop (c1 :: c :: cs, n+2, level-1)
              end else begin
                char_update (); string_make_charlst_rev_int (cs, n)
              end // end of ['}']
            | '\{' => begin
                char_update (); loop (c1 :: c :: cs, n+2, level+1)
              end // end of ['\{']
            | _ => loop (c :: cs, n+1, level)
          end else begin // c1 = EOF
            chars_free cs; errmsg_unclosed_logue ()
          end // end of [if]
        end (* end of ['%'] *)
      | _ (* c <> '%' *) => begin
          char_update (); loop (c :: cs, n+1, level)
        end // end of [_]
    end else begin // c = EOF
      chars_free cs; errmsg_unclosed_logue ()
    end // end of [if]
  end // end of [loop]
} // end of [tokenize_logue]

(* ****** ****** *)

fn errmsg_unclosed_funarg
  (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The function argument starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_funarg]

implement
tokenize_funarg () =
  loop (nil (), 0) where {
  fun loop {n:nat} (cs: chars n, n: int n): string = let
    val c = char_get ()
  in
    if c >= 0 then let
      val c = i2c c in case+ c of
      | ')' => begin
          char_update (); string_make_charlst_rev_int (cs, n)
        end // end of [')']
      | _ => (char_update (); loop (c :: cs, n+1))
    end else begin
      chars_free cs; errmsg_unclosed_funarg ()
    end // end of [if]
  end // end of [loop]
} // end of [tokenize_funarg]

(* ****** ****** *)

fn errmsg_char_esc
  (): char = let
  val pos = position_prev_get ()
in
  prerr_string ("The escaped char at [");
  prerr_pos pos;
  prerr_string ("] is not supported.");
  prerr_newline ();
  exit {char} (1)
end // end of [errmsg_char_esc]

fun tokenize_char_esc_code (ci: int, i: int): int =
  if i < 2 then let
    val c = char_get () in
    if c >= 0 then let
      val c = i2c c in case+ 0 of 
      | _ when char_isdigit c => begin
          char_update (); tokenize_char_esc_code (BASE * ci + (c - '0'), i+1)
        end // end of [_ when ...]
      | _ => ci // end of [_]
    end else ci
  end else begin
    ci // function returns: an error is to be reported later
  end // end of [if]
// end of [tokenize_char_esc_code]

fun tokenize_char_esc (): char = let
  val c = char_get_update () in case+ 0 of
  | _ when c >= 0 => let
      val c = i2c c in
      if char_isdigit c then
        char_of_int (tokenize_char_esc_code (c - '0', 0))
      else begin case+ c of
        | 'a' => '\007' (* alert *)
        | 'b' => '\010' (* backspace *)
        | 'f' => '\014' (* line feed *)
        | 'n' => '\012' (* newline *)
        | 'r' => '\015' (* carriage return *)
        | 't' => '\011' (* horizonal tab *)
        | 'v' => '\013' (* vertical tab *)
        | _ => c (* no effect on other chars *)
      end // end of [if]
    end // end of [_ when ...]
  | _ (* c = EOF *) => '\000' // an error is to be reported later
end // end of [tokenize_char_esc]

fn errmsg_unclosed_char (): char = let
  val pos = position_prev_get ()
in
  prerr_string ("The char starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {char} (1)
end // end of [errmsg_unclose_char]

fun tokenize_char (): char = let
  val c0 = char_get (); val c = case+ 0 of
  | _ when c0 >= 0 => let
      val c0 = i2c c0 in case+ c0 of
      | '\\' => (char_update (); tokenize_char_esc ())
      | '\'' => '\0' (* '' stands for '\0' *)
      | _ => (char_update (); c0)
    end // end of [_ when ...]
  | _ (* c0 < 0 *) => '\000'
  val c1 = char_get_update ()
in
  case+ 0 of
  | _ when c1 >= 0 => let val c1 = i2c c1 in
      if c1 <> '\'' then errmsg_unclosed_char () else c
    end // end of [_ when ...]
  | _ (* c1 < 0 *) => errmsg_unclosed_char ()
end // end of [tokenize_char]

(* ****** ****** *)

fn errmsg_unclosed_code
  (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The code starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_code]

fun tokenize_code
  {n,level:nat} (
  cs: chars n, n: int n, level: int level
) : string = let
  val c = char_get ()
in
  if c >= 0 then let
    val c = i2c c in case+ c of
    | '\{' => begin
        char_update (); tokenize_code (c :: cs, n+1, level+1)
      end // end of ['\{']
    | '}' when level > 0 => begin
        char_update (); tokenize_code (c :: cs, n+1, level-1)
      end // end of ['}' when ...]
    | '}' (* level = 0 *) => begin
        char_update (); string_make_charlst_rev_int (cs, n)
      end // end of ['}']
    | _ => begin
        char_update (); tokenize_code (c :: cs, n+1, level)
      end // end of [_]
  end else begin
    chars_free cs; errmsg_unclosed_code ()
  end // end of [if]
end // end of [tokenize_code]

(* ****** ****** *)

fn errmsg_unclosed_comment
  (): void = let
  val pos = position_prev_get ()
in
  prerr_string ("The comment starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {void} (1)
end // end of [errmsg_unclosed_comment]

fun tokenize_comment
  {level:nat} (level: int level): void = let
  val c = char_get ()
in
  if c >= 0 then let
    val c = i2c c in case+ c of
    | '\(' => let
        val c1 = char_update_get () in case+ 0 of
        | _ when c1 >= 0 => let
            val c1 = i2c c1 in case+ c1 of
            | '*' => tokenize_comment (level+1) | _ => tokenize_comment level
          end // end of [_ when ...]
        | _ (* c1 < 0 *) => tokenize_comment level
      end // end of ['\(']
    | '*' => let
        val c1 = char_update_get () in case+ 0 of
        | _ when c1 >= 0 => let
            val c1 = i2c c1 in case+ c1 of 
            | ')' => begin
                if level > 0 then tokenize_comment (level-1) else char_update ()
              end // end of [')']
            | _ => tokenize_comment level
          end // end of [_ when ...]
        | _ (* c1 < 0 *) => tokenize_comment level
      end // end of ['*']
    | _ => (char_update (); tokenize_comment level)
  end else begin
    errmsg_unclosed_comment ()
  end // end of [if]
end // end of [tokenize_comment]

(* ****** ****** *)

fun tokenize_int
  (i: int): int = let
  val c = char_get () in
  case+ 0 of
  | _ when c >= 0 => let val c = i2c c in
      if char_isdigit c then
        (char_update (); tokenize_int (BASE * i + (c - '0')))
      else i
    end // end of [_ when ...]
  | _ (* c = EOF *) => i
end // end of [tokenize_int]

(* ****** ****** *)

fun tokenize_string_char () = let
  val c = char_get_update () in case+ 0 of
  | _ when c >= 0 => let
      val c = i2c c in if c = '\\' then tokenize_char_esc () else c
    end // end of [_ when ...]
  | _ (* c = EOF *) => '\000' // an error is to be reported later
end // end of [tokenize_string_char]

fn errmsg_unclosed_string (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The string starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_string]

fun tokenize_string {n:nat}
  (cs: chars n, n: int n): string = let
  val c0 = char_get ()
in
  case+ 0 of
  | _ when (c0 >= 0) => let
      val c0 = i2c c0 in if c0 = '"' then begin
        char_update (); string_make_charlst_rev_int (cs, n)
      end else let
        val c = tokenize_string_char ()
      in
        tokenize_string (c :: cs, n+1)
      end // end of [if]
    end // end of [_ when ...]
  | _ (* c0 < 0 *) => begin
      chars_free cs; errmsg_unclosed_string ()
    end // end of [_]
end // end of [tokenize_string]

(* ****** ****** *)

fn char_iseof (c: int): bool =
  if c >= 0 then false else true

fn char_issymbl (c: char): bool =
  string_contains ("!%&#+-/:<=>@\\~`|*", c)

fun tokenize_word_sym {n:nat}
  (cs: chars n, n: int n): string = let
  val c = char_get () in case+ 0 of
  | _ when c >= 0 => let
      val c = i2c c in if char_issymbl c then
        (char_update (); tokenize_word_sym (c :: cs, n+1))
      else string_make_charlst_rev_int (cs, n)
    end // end of [_ when ...]
  | _ (* c = EOF *) => string_make_charlst_rev_int (cs, n)
end // end of [tokenize_word_sym]

fun tokenize_word_ide
  {n:nat} (
  cs: chars n, n: int n
) : string = let
  val c = char_get () in case+ 0 of
  | _ when c >= 0 => let
      val c = i2c c in if char_islttr c then
        (char_update (); tokenize_word_ide (c :: cs, n+1))
      else string_make_charlst_rev_int (cs, n)
    end // end of [_ when ...]
  | _ (* c = EOF *) => string_make_charlst_rev_int (cs, n)
end where {
  fn char_islttr (c: char): bool =
    if char_isalnum c then true else c = '_'
} // end of [tokenize_word_ide]

(* ****** ****** *)

extern fun
fprint_token {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, tok: token): void =
  "fprint_token"

implement
fprint_token (
  pf_mod | fil, tok
) = begin case+ tok of
  | TOKchar c => fprintf (pf_mod | fil, "char(%c)", @(c))
  | TOKcode s => fprintf (pf_mod | fil, "code(%s)", @(s))
  | TOKint i => fprintf (pf_mod | fil, "int(%i)", @(i))
  | TOKstring s => fprintf (pf_mod | fil, "string(%s)", @(s))
  | TOKword s => fprintf (pf_mod | fil, "word(%s)", @(s))
  | TOKlit c => fprintf (pf_mod | fil, "lit(%c)", @(c))
  | TOKmark s => fprintf (pf_mod | fil, "mark(%s)", @(s))
  | TOKeof () => fprint_string (pf_mod | fil, "EOF")
end // end of [fprint_token]

implement
print_token (tok) = () where {
  val (pf_stdout | ptr_stdout) = stdout_get ()
  val () = fprint_token (file_mode_lte_w_w | !ptr_stdout, tok)
  val () = stdout_view_set (pf_stdout | (*none*))
} // end of [print_token]

implement
prerr_token (tok) = () where {
  val (pf_stderr | ptr_stderr) = stderr_get ()
  val () = fprint_token (file_mode_lte_w_w | !ptr_stderr, tok)
  val () = stderr_view_set (pf_stderr | (*none*))
} // end of [prerr_token]

(* ****** ****** *)

fun pos_prev_reset_and_char_update
  (): void = (pos_prev_reset (); char_update ())
// end of [fun]

extern fun tokenize (): token = "tokenize"

implement
tokenize () = let
  val c = char_get () in case+ 0 of
  | _ when c >= 0 => tokenize_main (i2c c)
  | _ (* c = EOF *) => TOKeof ()
end where {
//
fun tokenize_main (c: char) = case+ c of
  | '\'' => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKchar (tokenize_char ())
    end // end of ['\'']
  | '\(' => let
      val () = pos_prev_reset_and_char_update ()
      val c1 = char_get (); val isstar = (
        if c1 >= 0 then let
          val c1 = i2c c1 in case+ c1 of '*' => true | _ => false
        end else false
      ) : bool // end of [isstar]
    in
      if isstar then (tokenize_comment (0); tokenize ())
      else TOKlit '\('
    end // end of ['\(']
  | '/' => let
      val () = pos_prev_reset_and_char_update ()
      val c1 = char_get (); val isslash = (
        if c1 >= 0 then let
          val c1 = i2c c1 in case+ c1 of '/' => true | _ => false
        end else false
      ) : bool // end of [isslash]
    in
      if isslash then begin
        tokenize_line_comment (); tokenize ()
      end else begin
        TOKword (tokenize_word_sym ('/' :: nil (), 1))
      end // end of [if]
    end // end of ['/']
  | '\{' => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKcode (tokenize_code (nil (), 0, 0))
    end // end of ['\{']
  | '"' => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKstring (tokenize_string (nil (), 0))
    end // end of ['"']
  | '_' => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKword (tokenize_word_ide (c :: nil (), 1))
    end // end of ['_']
  | '%' => let
      val () = pos_prev_reset_and_char_update ()
      val c1 = char_get (); val islbrace = (
        if c1 >= 0 then let
          val c1 = i2c c1 in case+ c1 of '\{' => true | _ => false
        end else false
      ) : bool // end of [islbrace]
    in
      if islbrace then begin
        char_update (); TOKmark "%{"
      end else begin
        TOKword (tokenize_word_sym (c :: nil (), 1))
      end // end of [if]
    end // end of ['%']
  | _ when char_isdigit c => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKint (tokenize_int (c - '0'))
    end // end of [digit]
  | _ when char_isalpha c => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKword (tokenize_word_ide (c :: nil (), 1))
    end // end of [alpha]
  | _ when char_issymbl c => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKword (tokenize_word_sym (c :: nil (), 1))
    end // end of [symbl]
  | _ when char_isspace c => let
      val () = char_update () in tokenize ()
    end // end of [space]
  | _ => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKlit c
    end // end of [_]
(* end of [tokenize_main] *)
//
} // end of [tokenize ... where ...]

(* ****** ****** *)

%{$

static ats_ptr_type the_token ;

ats_ptr_type token_get () { return the_token ; }

ats_void_type
token_update () {
  the_token = tokenize () ; return ;
}

ats_ptr_type
token_get_update () {
  ats_ptr_type tok = the_token ; the_token = tokenize () ; return tok;
} // end of [token_get_update]

%} // end of [%{$]

(* ****** ****** *)
//
// HX: initialization
//
implement
token_initialization () = let
  val () = char_update () // flush out a junk value
  val () = token_update () // flush out a junk value
in
  // empty
end // end of [let]

(* ****** ****** *)

(* end of [token.dats] *)
