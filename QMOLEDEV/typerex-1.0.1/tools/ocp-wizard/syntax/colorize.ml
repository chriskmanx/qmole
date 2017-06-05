(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open OcpLang
open Util
open GapBuffer
open OcamlBuffer
open Parser
open OcamlTokenize
open OCamlTokenBuffer

include Debug.Tag(struct let tag = "colorize" end)

let drop_until keep =
  List.drop_while (function t -> not (List.mem t keep))

let rec search pattern l =
  try pattern l
  with Not_found ->
    match l with
      | [] -> raise Not_found
      | t :: l ->
        search pattern (List.tl l)

let rec search_remember pattern ?(stack=[]) l =
  try pattern (l, stack)
  with Not_found ->
    match l with
      | [] -> raise Not_found
      | t :: l ->
        search_remember pattern ~stack:(t :: stack) l

let drop_until_checking keep ~check l =
  let found = ref false in
  let l =
    List.drop_while
      (function t ->
        if List.mem t check then found := true;
        not (List.mem t keep))
      l
  in
  l, !found

let drop_any tokens =
  List.drop_while (function t -> List.mem t tokens)

let introducing = [
  LET;
  VAL;
  EXTERNAL;
  TYPE;
  EXCEPTION;
  MODULE;
  CLASS;
  OPEN;
  INCLUDE
]

let t_introducing = function
  | LET -> `LET
  | VAL -> `VAL
  | EXTERNAL -> `EXTERNAL
  | TYPE -> `TYPE
  | EXCEPTION -> `EXCEPTION
  | MODULE -> `MODULE
  | CLASS -> `CLASS
  | OPEN -> `OPEN
  | INCLUDE -> `INCLUDE
  | _ -> raise Not_found

let p_introducing = function
  | t :: _ -> t_introducing t
  | [] -> raise Not_found

let enclosing_item ~before =
  let keywords =
    [ END ; SEMISEMI ; STRUCT ; SIG ; OBJECT ; IN ; (*WITH ;*) CONSTRAINT ] @
    introducing
  in
  let after check =
    snd (drop_until_checking (AND :: keywords) ~check before)
  and start = drop_until keywords before in
  match start with
(*
    | TYPE :: WITH :: _ -> `with_type
*)
    | TYPE :: MODULE :: _ -> `modules (after [EQUAL])
    | TYPE :: _ -> `types (after [EQUAL])
(*
    | MODULE :: WITH :: _ -> `with_module
*)
    | MODULE :: _ -> `modules (after [EQUAL])
    | (OPEN | INCLUDE) :: _ -> `modules true
    | EXCEPTION :: _ ->
      if after [OF] then `type_expr else `exn
    | CONSTRAINT :: _ -> `type_expr
    | _ -> (* values *)
      let after_equal =
        match start with
          | (STRUCT | SIG | SEMISEMI | OBJECT | IN) :: _
            -> true
          | (VAL | EXTERNAL) :: _ | TYPE :: CLASS :: _
            ->  after [EQUAL]
          | [] -> true
          | (LET | CLASS) :: _
          | _ ->  after [EQUAL]
      in (* check if we are in a type_expr *)
      match drop_until ([COLON ; LPAREN ; EQUAL ; AND] @ keywords) before with
        | COLON :: _ -> `type_expr
        | _ -> `values after_equal

let probably_type ~stack =
  match drop_until [COLON ; LPAREN ; EQUAL ; AND] (List.rev stack) with
    | COLON :: _ -> true
    | _ -> false

let check_after_and t ~stack =
  List.mem t
    (List.take_while (( <> ) AND) (List.rev stack))

let rec enclosing_item ~before =
  try
    search_remember (function
      | TYPE :: MODULE :: _, stack -> `modules (check_after_and EQUAL ~stack)
      | TYPE :: _, stack -> `types (check_after_and EQUAL ~stack)
      | MODULE :: _, stack -> `modules (check_after_and EQUAL ~stack)
      | (OPEN | INCLUDE) :: _, stack -> `modules true
      | OF :: TYPE :: MODULE :: _, stack -> `modules true
      | EXCEPTION :: _, stack -> `exn
      | OF :: _, stack when not (List.mem BAR stack)
          && not (List.mem AND stack) -> `type_expr
      | CONSTRAINT :: _, stack -> `type_expr
      | COLON :: LIDENT _ :: (VAL | EXTERNAL) :: _, stack ->
        if check_after_and EQUAL ~stack then `values true else `type_expr
(*
      | END
*)
      | (SEMISEMI | STRUCT | SIG | OBJECT | IN) :: _, stack ->
        if probably_type ~stack then `type_expr else `values true
      | AND :: before, stack ->
        (match enclosing_item ~before with
          | `values _ -> `values (List.mem EQUAL stack)
          | `types _ | `type_expr -> `types (List.mem EQUAL stack)
          | `modules _ -> `modules (List.mem EQUAL stack)
          | i -> i
        )
      | (LET | METHOD | CLASS | VAL | EXTERNAL) :: _, stack
      | (VIRTUAL | PRIVATE) :: METHOD :: _, stack
      | (VIRTUAL | PRIVATE) ::(VIRTUAL | PRIVATE) :: METHOD :: _, stack ->
        if probably_type ~stack then `type_expr
        else `values (check_after_and EQUAL ~stack)
      (* TODO: handle optional labels with default value *)
      | (FUN | FUNCTION) :: _, stack ->
        `values (check_after_and MINUSGREATER ~stack)
      | _ -> raise Not_found
    ) before
  with Not_found ->
    `values true

(* We should use a stack *)
let classify_let ~after =
  match after with
    | (TYPE | MODULE) :: _ -> `inner
    | _ ->
      match drop_until [LET ; IN] after with
        | IN :: _ -> `inner
        | _ -> `top

let rec classify_let ~max level = function
  | IN :: _ when level = 0 -> `inner
  | LET :: _ when level = max -> `top
  | []
  | (SEMISEMI) :: _ -> `top
  | IN :: after -> classify_let ~max (level-1) after
  | LET :: after -> classify_let ~max (level+1) after
  | _ :: after -> classify_let ~max level after

let classify_let ?(max=max_int) ~after () = classify_let ~max 0 after

let classify_module_type_val token ~before ~after =
  match token :: before, token :: after with
    | (TYPE | MODULE) :: (WITH | AND) :: _, _ -> `with_constraint
    | _, (TYPE :: OF :: _ | MODULE :: TYPE :: OF :: _) -> `modtype_of
    | TYPE :: LPAREN :: _, _ -> `tvar
    | (MODULE | VAL) :: LPAREN :: _, _ -> `package
    | _ -> `governing

let classify_bar ~before =
  match before with
    | (WITH | FUN | FUNCTION) :: _ -> `matching
    | _ ->
      match enclosing_item ~before,
        drop_until
        [BAR ; MINUSGREATER ; WITH ; FUN ; FUNCTION;
         TYPE (*; LBRACKET ; LBRACKETLESS ; LBRACKETGREATER*)] before with
    | `values _, MINUSGREATER :: _ -> `matching
    | _ -> `other (* either or-pattern or type *)

let classify_uid ~before ~after =
  match before, after with
    | EXCEPTION :: _, _ -> `def, `exn

    | _, DOT :: _ -> `occ, `modname `arg (* not exact *)

    | (BAR | EQUAL) :: _, _ when enclosing_item ~before = `types true ->
      `def, `cstr

    | MODULE :: LET :: _, _ -> `def, `modname `local

    | ((MODULE | AND) :: _ | (TYPE | REC) :: MODULE :: _), _
      -> `def, `modname `top
    | _ ->
      match enclosing_item ~before with
        | `modules equal ->
          (match equal, before, after with
            | true, _, _ | _, (COLON | DOT) :: _, _ | _, _, LPAREN :: _ ->
              `occ, `modname `arg
            | _ -> `def, `modname `arg)
        | _ ->
          match before, after with
            | _ -> `occ, `cstr

let rec value_ref_is_applied ~before ~after =
  match before, after with
      (END
          | SEMISEMI
          | BEGIN
          | IN
          | NEW
          | MATCH | TRY | WHEN
          | IF | THEN | ELSE
          | TO | DOWNTO | WHILE | DO
          | MINUSGREATER | SHARP
          | LPAREN | LBRACKET | COLONCOLON | LBRACKETBAR
          | LBRACELESS
          | SEMI | COMMA | INFIXOP1 _
          | AMPERAMPER | BARBAR | AMPERSAND | OR
          | PLUS | PLUSDOT | MINUS | MINUSDOT | STAR
          | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
          | LESS | GREATER | EQUAL | INFIXOP0 _ | PREFIXOP _
          | COLONEQUAL | LESSMINUS
          | LAZY) :: _,
  (OBJECT
      | BEGIN
      | NEW
      | FUN | FUNCTION | MATCH | TRY
      | IF
      | FOR | WHILE
      | LPAREN | LBRACKET | LBRACKETBAR
      | LBRACELESS
      | QUOTE | BACKQUOTE | TILDE | QUESTION
      | ASSERT | FALSE | TRUE | LAZY
      | UIDENT _
      | LIDENT _ | LABEL _ | OPTLABEL _
      | STRING _
      | CHAR _ | FLOAT _ | INT _ | INT32 _ | INT64 _ | NATIVEINT _) :: _
  -> true
    | DOT :: UIDENT _ :: before, after -> value_ref_is_applied ~before ~after
    | _ -> false



let classify_lid ~before ~after =
  match before, after with
    | QUOTE :: _, _ -> `def, `tvar
    | BACKQUOTE :: _, _ -> `occ, `variant

    | (METHOD :: _ |
       (VIRTUAL | PRIVATE) :: METHOD :: _ |
       (VIRTUAL | PRIVATE) :: (VIRTUAL | PRIVATE) :: METHOD :: _), _ ->
      `def, `methodname
    | SHARP :: before, _ ->
      (match before with
        | (COLON | BAR | LPAREN | UIDENT _ | COMMA | EQUAL | SEMI |
            COLONCOLON | LAZY) :: _ -> `occ, `tconstr
        | _ -> `occ, `methodname)

    | (MUTABLE | SEMI | LBRACE) :: _, COLON :: _ -> `def, `field

    (* problem with '; x = y' (boolean) *)
    | ((WITH | SEMI | LBRACE) :: _
          (* quick hack *)
          | DOT :: UIDENT _ :: (WITH | SEMI | LBRACE) :: _
          | DOT :: UIDENT _ :: DOT :: UIDENT _ :: (WITH | SEMI | LBRACE) :: _),
      (EQUAL | SEMI | RBRACE) :: _ when
          match drop_until ([LBRACE ; RBRACE] (*@ introducing*)) before with
            | LBRACE :: _ -> true | _ -> false -> `occ, `field
    | _, LESSMINUS :: _ -> `occ, `field
    | DOT :: before, _ when
        let first = ref `dot in
        ignore
          (List.drop_while
             (function
               | DOT -> first := `dot ; true
               | UIDENT _ -> first := `uident ; true
               | _ -> false)
             before);
        !first = `dot -> `occ, `field

    |  _ ->
      match enclosing_item ~before with
        | `types equal -> (if equal then `occ else `def), `tconstr
        | `type_expr | `exn ->
          `occ,
          (match after with COLON :: _ -> `arglabel | _ -> `tconstr)
        | `values equal ->
          let arity =
            if equal then
              if value_ref_is_applied ~before ~after then `func else `value
            else
              match before with
                | (LET | REC | AND | CLASS) :: _ | VIRTUAL :: CLASS :: _ ->
                  (match after with
                    | EQUAL :: (FUN | FUNCTION) :: _ -> `func
                    | (EQUAL | COLON | COMMA | SEMI | RBRACE) :: _ -> `value
                    | _ :: _ -> `func
                    | _ -> `value)
                | _ -> `value
          in
          (if equal then `occ else `def), `value arity
        | `modules _ -> `occ, `value `value (* should not happen *)

let any_token2faces = ref []

let lookahead = 300
let lookbehind = 300

let colors buffer start end_ =

  (* First, widen the area: *)
  goto buffer start;
  let t_start0 = buffer.t_pre in
  let start = ref start in
  while buffer.t_pre > max 0 (t_start0 - lookahead)
    || buffer.offset > 0 do
    backward buffer;
    decr start
  done;
  let t_start = buffer.t_pre
  and start = !start in
  goto buffer end_;
  let t_end0 = buffer.t_pre in
  while buffer.t_pre
    < min (t_length buffer) (t_end0 + lookbehind) do
    forward buffer
  done;
  let t_end = buffer.t_pre in

  (* Also record that the faces for the modified area must be re-sent anyway *)
  for i = t_start0 to min t_end0 (t_length buffer - 1) do
    let t = buffer.t_buf.(pos2pointer buffer i) in
    set_faces t []
  done;

  (* Second, compute the initial forward and backward stacks *)
  let modified = sub buffer t_start t_end in
  let modified_tokens =
    Array.fold_left
      (function n -> function
        | {token = Token _} -> n + 1
        | _ -> n)
      0 modified
  in
  let before = ref (rev_tokens_before ~max:lookbehind buffer t_start)
  and after = ref (tokens_from ~max:(modified_tokens + lookahead) buffer t_start) in

  (* Finally, traverse the tokens and compute the faces *)
  let pos = ref start
  and t = Hashtbl.create 10
  and helps = ref [] in
  let find key =
    try Hashtbl.find t key
    with Not_found ->
      let r = ref [] in
      Hashtbl.add t key r;
      r
  in
  List.iteri
    (fun i t ->
      let t_pos = t_start + i
      and len = length t in
      let b = !pos in
      let e = b + len in
      pos := e;
      try
        let next, help =
          match t.token with
            | Token tok ->
              (match !after with
                | tok' :: tokens when tok' = tok ->
                  after := tokens
                | _ -> assert false);
              (function () -> before := tok :: !before), None
            | Error e ->
              Lexer.report_error Format.str_formatter e;
              (function () -> ()), Some (Format.flush_str_formatter ())
            | White_space | Comment -> (function () -> ()), None
            | Pad -> assert false
        in
        (try
           (* positions are now relative to the beginning of the token, so that
              we can compare them after shifting. *)
           let faces =
             snd (List.hd !any_token2faces) t ~before:!before ~after:!after 0 len in
           (* only consider faces if they have changed, or we are inside the
              replaced window (since we must recolorize this text) *)
           if faces <> t.faces ||
             t_start0 <= t_pos && t_pos <= t_end0 then (
             set_faces t faces;
             List.iter
               (function b', e', face ->
                 let b' = b + b' and e' = b + e' in
                 if b <= b' && b' <= e' && e' <= e then
                   let r = find face in
                   r := (b', e') :: !r)
               faces
             ) else
             debugln "colors for %d: [%d, %d[ unchanged" t_pos b e
         with _ -> ());
        next ();
        match help with
          | Some help -> helps := ((b, e), help) :: !helps
          | None -> ()
      with Not_found -> ())
    (Array.to_list modified);
  let l =
    Hashtbl.fold
      (fun k l props -> (k, !l) :: props)
      t
      []
  in
  (start, !pos), l, !helps
  (*@ [start, !pos, [`fontified]]*)

let register_token2faces name f =
  any_token2faces := (name, f) :: !any_token2faces

let register_token2face name f =
  register_token2faces name
    (fun t ~before ~after b e -> [b, e, f t ~before ~after])

let set_theme name =
  try
    let theme = List.assoc name !any_token2faces in
    any_token2faces :=
      (name, theme) :: List.remove_assoc name !any_token2faces
  with Not_found ->
    () (* Don't crash the server just because of missing theme *)

let list_themes () = List.map fst !any_token2faces
