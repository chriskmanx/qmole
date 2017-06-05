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

open Location
open Util
open OcpLang

open OcpLang
open Util
open GapBuffer
include Debug.Tag(struct let tag = "ocamlTokenize" end)

open Lexing

module OCamlToken = struct

type extended_token =
  | Pad
  | Token of Parser.token
  | Error of Lexer.error
  | White_space
  | Comment

type token = {
  token : extended_token;
  string : string;
  length : int; (* temporary *)
  mutable faces : (int * int * Face.face) list
}

(* We record the position in the gap buffer of the start of every
   unterminated comment or string, as well as the (only) unmatched
   double quote, if any. The former is used to make sure that we
   restart lexing at the first unterminated token, and the latter to
   avoid swapping all following code and strings every time we type a
   double-quote. *)
type state = {
  mutable unterminated : mark list;
  mutable unmatched_double_quote : mark option
}

let set_unterminated state gb pos =
  state.unterminated <- (mark gb pos) :: state.unterminated

let set_unmatched_double_quote state gb pos =
  state.unmatched_double_quote <- Some (mark gb pos)

let t_pad = {
  token = Pad;
  string = "<pad>";
  length = 0;
  faces = []
}

let string t =
  match t.token with
    | Error _ -> Printf.sprintf "error:%S" t.string
    | _ -> t.string

let length t = String.length t.string
let length t = t.length (* temporary *)

let set_faces t faces = t.faces <- faces

let equals t t' =
  t.token = t'.token && t.string = t'.string && t.length = t'.length

(* Prepend a given list of tokens with the preceeding white space and
   comments, determined as the segment between start and token_start,
   relative to the lexbuf starting character [from]. *)
let add_white_space gb start ~from token_start t =
  let comments = Lexer.comments () in
  let add_white length ts =
    if length = 0 then
      ts
    else
      { token = White_space;
        string = String.make length ' ';
        length = length;
        faces = [] } ::
        ts
  in
  let tokens_start, tokens =
    List.fold_right
      (fun (c, loc) (ts_start, ts) ->
        let b, e = loc.loc_start.pos_cnum, loc.loc_end.pos_cnum in
        let ts = add_white (ts_start - e) ts in
        let ts =
          let length = e - b
          and string = substring gb (from+b) (from+e) in
(*
          debugln "string=%S" string;
          debugln "c=%S" c;
 faux:
          assert (String.sub string 2 (String.length string - 4) = c);
*)
          { token = Comment;
            string = string;
            length = length;
            faces = [] } ::
            ts
        in
        b, ts)
      comments
      (token_start, t)
  in
  add_white (tokens_start - start) tokens

let lexbuf ~from ~modified_end chars state =
  let unterminated_string_ahead =
    match state.unmatched_double_quote with
      | Some m ->
        let pos = mark2pos chars m in
        if pos2pointer chars pos >= modified_end then
          Some pos
        else (* This is a just-created mark *)
          None
      | None -> None
  in
  let _ =
    match unterminated_string_ahead with
      | Some pos -> debugln "unterminated string ahead at %d" pos
      | _ -> ()
  in
  let prefix_start = pos2pointer chars from in
  let i = ref prefix_start in
  let stops = [
          (* stop here to jump over the gap *)
          chars.pre, (function () -> i := chars.post ; max_int);
          (* at modified_end, if we are in a string, and unless we
             have an unmatched double-quote ahead or we are between
             '\' and '"', we stop here to avoid switching strings and
             code. The eof is not a problem since the lexer will be
             reinitialized. *)
          modified_end,
          (function () ->
            if
              Lexer.in_string () &&
              unterminated_string_ahead = None &&
              (chars.buf.[if !i = chars.post then chars.pre-1 else !i-1] <> '\\' ||
              !i < String.length chars.buf && chars.buf.[!i] <> '"')
            then
              !i
            else
              max_int)
        ] @
        (* also stop just after the unmatched double-quote itself,
           and stop if we are still in a string. *)
        (match unterminated_string_ahead with
           | Some pos ->
               [pos2pointer chars pos,
                (function () ->
                   state.unmatched_double_quote <- None ; max_int);
                pos2pointer chars pos + 1,
                (function () ->
                   if Lexer.in_string () then !i else max_int)]
           | _ -> []) @
        (* stop at each unterminated string or comment, and undo it *)
        List.map
          (function mark ->
             let pos = mark2pos chars mark in
             pos2pointer chars pos, function () ->
               delete_mark chars mark;
               state.unterminated <- List.removeq mark state.unterminated;
               max_int)
          state.unterminated
        in
  let lexbuf =
    from_function
      (fun buf n ->
        Profile.time_push "refill";
        debug "refill %d" n;
        let bound =
          List.fold_left
            (fun m (stop, bound) ->
               if !i = stop then min (bound ()) m
               else if !i < stop then min stop m
               else m)
            max_int
            stops
        in
      let bound = min bound (String.length chars.buf) in
      let len = min n (bound - !i) in
      debugln "blit \"%d\" %d \"%d\" %d %d" (String.length chars.buf) !i (String.length buf) 0 len;
      String.blit chars.buf !i buf 0 len;
      i := !i + len;
      debugln "-> %d" len;
      Profile.time_pop ();
      len)
  in
  let inputfile = "" in
  Location.input_name := inputfile;
  Location.init lexbuf inputfile;
  lexbuf

let lexer state ~prefix_start ~modified_end ~find_ahead chars =
  (* First clear all marks on the definitely relexed portion. *)
  state.unterminated <-
    List.filter
      (function m ->
         try
           let p = pos2pointer chars (mark2pos chars m) in
           assert (p >= prefix_start);
           p >= modified_end
         with MarkDeleted -> false)
      state.unterminated;
  (match state.unmatched_double_quote with
    | Some m ->
        (try
          let p = pos2pointer chars (mark2pos chars m) in
          assert (p >= prefix_start);
          if p < modified_end then (
            delete_mark chars m;
            state.unmatched_double_quote <- None
          )
        with MarkDeleted ->
          state.unmatched_double_quote <- None)
      | None -> ());
  let from = ref prefix_start in
    (* [from] is the character position from which the current lexbuf starts. *)
  let buf = ref (lexbuf ~from:!from chars state ~modified_end) in
  function () ->
(*
    Profile.time_push "lexe";
*)
    Lexer.init (); (* reinit the comment stack and in_string *)
  (* All positions are relative to the lexbuf start, but it doesn't
     matter since we only keep the length of the tokens. *)
  let start = lexeme_end !buf in
  let current_from = !from in (* to get the comments in case of error *)
  let t, token_start, token_end, string =
    try
(*
      try
*)
        let t = Lexer.token !buf in
        Token t, lexeme_start !buf, lexeme_end !buf, lexeme !buf
(* Does not work because comment_start_loc is flushed
      with
        | Lexer.Error (Lexer.Unterminated_string_in_comment, loc) ->
          Lexer.comment !buf;
          White_space, lexeme_end !buf, lexeme_end !buf, ""
*)
    with Lexer.Error (e, loc) ->
      debugln "start=%d, token_start=%d, lexeme_end=%d, loc_start=%d, loc_end=%d"
        start (lexeme_start !buf) (lexeme_end !buf)
        loc.Location.loc_start.pos_cnum loc.Location.loc_end.pos_cnum;
      match e with
        | Lexer.Unterminated_comment start
        | Lexer.Unterminated_string_in_comment start ->
(*
          let start =
            match Lexer.last_outermost_comment_start () with
              | Some start -> start.Location.loc_start.pos_cnum
              | None -> assert false (* loc.Location.loc_start.pos_cnum *)
          in
*)
          let start = start.Location.loc_start.pos_cnum in
          set_unterminated state chars (!from + start);
          from := !from + start + 2;
          buf := lexbuf ~from:!from ~modified_end chars state;
          Error e,
          start,
          start + 2,
          "(*"
        | Lexer.Unterminated_string ->
          let unmatched_pos = !from + loc.Location.loc_start.pos_cnum in
          set_unterminated state chars unmatched_pos;
          set_unmatched_double_quote state chars unmatched_pos;
          from := unmatched_pos + 1;
          buf := lexbuf ~from:!from ~modified_end chars state;
          Error e,
          loc.Location.loc_start.pos_cnum,
          loc.Location.loc_start.pos_cnum + 1,
          "\""
        | _ ->
          Error e, loc.Location.loc_start.pos_cnum, lexeme_end !buf, lexeme !buf
  in
  let length = token_end - token_start in
(*
  Profile.time_switch_to "make token list";
           *)
  let t =
    if length = 0 then
      []
    else [
      { token = t;
        string = string;
        length = length;
        faces = [] }
    ]
  in
  let res = add_white_space chars start ~from:current_from token_start t in
(*
  Profile.time_pop ();
           *)
  res

let min_undo_start gb state =
  List.fold_left
    (fun m mark -> try min m (mark2pos gb mark) with MarkDeleted -> m)
    max_int
    state.unterminated

end

module OCamlTokenBuffer = TokenBuffer.Make(OCamlToken)

include OCamlToken
open OCamlTokenBuffer

let empty () =
  let state = {
    unterminated = [];
    unmatched_double_quote = None
  } in
  create 0 0 state

let reset buffer =
  clear buffer;
  buffer.state.unterminated <- [];
  buffer.state.unmatched_double_quote <- None

let update_buffer buffer ~start ~old_length first_time s =
  let old_length =
    if first_time then (
      reset buffer;
      0
    ) else
      old_length
  in
  goto buffer start;
  let start', _, modified = replace buffer old_length s in
  let end' = Array.fold_left (fun n t -> n + length t) start' modified in
  let start' = pos_bot buffer (min start start') in
  let t_start = buffer.t_pre in (* depend on the above side effect! *)
  let end' = pos_bont buffer (max (start + String.length s) end') in
  let t_end = goto buffer end' ; buffer.t_pre in
  debugln "after widening: fontify [%d, %d[ ([%d, %d[)"
    start' end' t_start t_end;
  let modified = sub buffer t_start t_end in
  debugln "|%t"
    (function c ->
      Array.iter (function t -> Printf.fprintf c "%s|" (string t)) modified);
  start', end', t_start, t_end

let classify_comment c =
  if c = "(**/**)" then
    `ocamldoc_stop
  else if
    String.length c >= 4 &&
    String.sub c 0 3 = "(**" &&
    c.[3] <> '*'
  then
    `ocamldoc
  else
    `normal

let parse_comment s =
  debugln "parsing comment %S" s;
  let lexbuf = Lexing.from_string s in
  Odoc_text_lexer.init ();
(*
  let elts = ref [] in
  while lexbuf.lex_curr_pos < String.length s do
    try
      let b, e, elt =
        Odoc_text_parser.located_text_element Odoc_text_lexer.main lexbuf in
      debugln "element at [%d, %d[" b e;
      elts := (b, e, elt) :: !elts
    with _ ->
      debugln "parse error";
      ()
  done;
  List.rev !elts
           *)
  Odoc_text_parser.located_element_list Odoc_text_lexer.main lexbuf

let tokens_before ?(max=max_int) buffer pos =
  let i = ref pos in
  let l = ref [] and len = ref 0 in
  while !i > 0 && !len < max do
    decr i;
    match buffer.t_buf.(pos2pointer buffer !i) with
      | {token = Token tok} -> l := tok :: !l ; incr len
      | _ -> ()
  done;
  !l

let rev_tokens_from ?(max=max_int) buffer pos =
  let i = ref pos in
  let l = ref [] and len = ref 0 in
  while !i < OCamlTokenBuffer.t_length buffer && !len < max do
    (match buffer.t_buf.(pos2pointer buffer !i) with
      | {token = Token tok} -> l := tok :: !l ; incr len
      | _ -> ());
    incr i
  done;
  !l

let rev_tokens_before ?max buffer pos =
  List.rev (tokens_before ?max buffer pos)

let tokens_from ?max buffer pos =
  List.rev (rev_tokens_from ?max buffer pos)
