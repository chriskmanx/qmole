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
include Debug.Tag(struct let tag = "tokenBuffer" end)

module type TOKEN = sig

  type token
  type state
  val t_pad : token
  val length : token -> int
  val string : token -> string
  val equals : token -> token -> bool
  val lexer :
    state ->
    prefix_start:int ->
    modified_end:int ->
    find_ahead:((token -> bool) -> int option) ->
    gap_buffer ->
    unit -> token list

  val min_undo_start : gap_buffer -> state -> int

end

module Make(Token : TOKEN) = struct

  open Token

(* We assume that all chars belong to a token (including whitespace)
   but token must have at least one char *)
type tokenized_buffer = {
  chars : gap_buffer;
  mutable t_buf : token array;
  (* Gap buffer of tokens, with their length. *)
  mutable t_pre : int;
  mutable t_post : int;
  mutable offset : int;
    (* position with respect to begining of the token t_buf.(t_post),
       in the interval [0, len-1] if len = |t_buf.(t_post)| *)
  state: state
}

let create size t_size state =
  let t_size = max 1 t_size in {
    chars = create size;
    t_buf = Array.create t_size t_pad;
    t_pre = 0;
    t_post = t_size;
    offset = 0;
    state = state;
  }

let t_dump { t_buf = t_buf;
             t_pre = t_pre ;
             t_post = t_post ;
             offset = offset } c =
  Printf.fprintf c "|t_buf|=%d, t_pre=%d, t_post=%d, offset=%d"
    (Array.length t_buf) t_pre t_post offset

let t_snapshot { t_buf = t_buf ; t_pre = t_pre ;
                 t_post = t_post ; offset = offset } c =
  let print i = Printf.fprintf c "%s|" (string t_buf.(i)) in
  Printf.fprintf c "...|";
  for i = max 0 (t_pre - 11) to t_pre - 1 do print i done;
  Printf.fprintf c "offset=%d|" offset;
  for i = t_post to min (t_post + 10) (Array.length t_buf - 1) do print i done;
  Printf.fprintf c "..."

let clear tb =
  clear tb.chars;
  tb.t_pre <- 0;
  tb.t_post <- Array.length tb.t_buf;
  tb.offset <- 0

let forward tb =
  forward tb.chars;
  let l = length tb.t_buf.(tb.t_post) in
  let offset = tb.offset + 1 in
  if offset < l then
    tb.offset <- tb.offset + 1
  else (
    tb.offset <- 0;
    tb.t_buf.(tb.t_pre) <- tb.t_buf.(tb.t_post);
    tb.t_pre <- tb.t_pre + 1;
    tb.t_post <- tb.t_post + 1
  )

let backward tb =
  backward tb.chars;
  if tb.offset > 0 then
    tb.offset <- tb.offset - 1
  else (
    tb.t_pre <- tb.t_pre - 1;
    tb.t_post <- tb.t_post - 1;
    tb.t_buf.(tb.t_post) <- tb.t_buf.(tb.t_pre);
    tb.offset <- length tb.t_buf.(tb.t_post) - 1
  )

let goto tb n =
  while tb.chars.pre < n do forward tb done;
  while tb.chars.pre > n do backward tb done

let resize gb =
  let len = Array.length gb.t_buf in
  let buf = Array.create (2 * len) t_pad in
  Array.blit gb.t_buf 0 buf 0 gb.t_pre;
  Array.blit gb.t_buf gb.t_post buf (len + gb.t_post) (len - gb.t_post);
  gb.t_buf <- buf;
  gb.t_post <- len + gb.t_post

let insert pos gb c =
  if gb.t_post = gb.t_pre then
    resize gb;
  let set pos c =
    if equals gb.t_buf.(pos) c then
      debugln "unchanged token, no replacement required"
    else
      gb.t_buf.(pos) <- c
  in
  match pos with
    | `before ->
      set gb.t_pre c;
      gb.t_pre <- gb.t_pre + 1;
    | `after ->
      gb.t_post <- gb.t_post - 1;
      set gb.t_post c

let t_length tb = Array.length tb.t_buf + tb.t_pre - tb.t_post

let pos2pointer tb pos =
  if pos < tb.t_pre then
    pos
  else
    pos + tb.t_post - tb.t_pre

let pos_bot buffer pos =
  goto buffer pos;
  pos - buffer.offset

let pos_bont buffer pos =
  goto buffer pos;
  if buffer.offset = 0 then
    pos
  else
    pos + length buffer.t_buf.(buffer.t_post) - buffer.offset

let sub ({t_buf = t_buf ; t_pre = t_pre ; t_post = t_post} as tb) b e =
  let len = e - b in
  let e = pos2pointer tb e in
  let s = Array.create len t_pad in
  let before_gap = min len (max 0 (t_pre - b))
  and after_gap = min len (max 0 (e - t_post)) in
  assert (before_gap + after_gap = len);
  Array.blit t_buf b s 0 before_gap;
  Array.blit t_buf (e - after_gap) s (len - after_gap) after_gap;
  s

(* Discard a number of tokens whose total length (ignoring offset) is
   at least count, and return the excess discarded length. *)
let discard_tokens_for_length tb count =
  debug "%t -discard %d chars-> " (t_dump tb) count;
  let removed_chars = ref 0 in
  while !removed_chars < count do
    let l = length tb.t_buf.(tb.t_post) in
    tb.t_post <- tb.t_post + 1;
    removed_chars := !removed_chars + l;
  done;
  debugln "%t" (t_dump tb);
  !removed_chars - count

(* Delete at least n tokens backward (or up to the first one), for a
   total length of at least [n_chars] characters, and return this
   total length, and the deleted tokens. Offset is ignored! *)
let delete_backward tb n n_chars =
  debugln "delete at least %d chars backward" n_chars;
  let _suffix_length = Array.length tb.t_buf - tb.t_post in
  let len = ref 0 in
  let pre = max 0 (tb.t_pre - n) in
  while tb.t_pre > 0 && (tb.t_pre > pre || !len < n_chars) do
    tb.t_pre <- tb.t_pre - 1;
    len := !len + length tb.t_buf.(tb.t_pre)
  done;
  !len

(* Precondition:
   t_buf[0, t_pre[ = buf[0, prefix_start[
   t_buf[t_post, |t_buf|[ = buf[suffix_end, |buf|[
   roughly, insert the string
   buf[prefx_start, pre[ ^ buf[post, suffix_end[
   (except that we may have to re-lexe additional chars after suffix_end,
   and that we could also decide to re-lexe before prefix_start).
   Return
   - the first modified character
   - the number of excess chars re-lexed after suffix_end.
   - the array of modified tokens. *)
let update_tokens tb prefix_start suffix_end chars_undo =
  let deleted_chars = delete_backward tb 10 chars_undo in
  let prefix_start = prefix_start - deleted_chars in
  debugln "prefix_start = %d" prefix_start;
  let chars = tb.chars in
  let find_ahead pred =
    let found = ref None
    and i = ref tb.t_post
    and c = ref suffix_end in
    while !found = None && !i < Array.length tb.t_buf do
      if pred tb.t_buf.(!i) then
        found := Some !c;
      c := !c + length tb.t_buf.(!i);
      incr i
    done;
    !found
  in
  let lexe = lexer tb.state ~prefix_start ~modified_end:suffix_end ~find_ahead chars in
  let initial = chars.pre - prefix_start + suffix_end - chars.post in
  let unlexed = ref initial
  and relexed = ref 0 in
  let rev_lexed = ref [] in
  while !relexed <> !unlexed do
    let ts = lexe () in
    rev_lexed := List.rev_append ts !rev_lexed;
    List.iter (function t -> relexed := !relexed + length t) ts;
    if !relexed > !unlexed then
      unlexed := !relexed +
        discard_tokens_for_length tb (!relexed - !unlexed)
  done;
  let excess = !relexed - initial in
  prefix_start, excess, (List.rev !rev_lexed)

(* Replace the count next character by s, and return, for the token buffer,
   - the first modified char (which is necessarily the start of a token)
   - the two arrays of deleted tokens (after and before the gap)
   - the new tokens which have been written to replace removed ones.
   The char and token position are both at the end of the modification
   (with coorect offset). *)
  let replace tb count s =
    debugln "BEFORE REPLACE:\n%t\n%t" (snapshot tb.chars) (t_snapshot tb);
    debugln "replace %d chars" count;
    debugln "%t" (dump tb.chars);
    debugln "%t" (t_dump tb);
  (* These chars are removed from the token in addition to count *)
    let prefix_start = tb.chars.pre - tb.offset in
    debugln "prefix_start = %d" prefix_start;
    let min_undo_start = min_undo_start tb.chars tb.state in
    Profile.time_push "replace chars";
    replace tb.chars count s;
    debugln "replace -> %t" (dump tb.chars);
    let prefix_length = tb.t_pre
    and initial_post = tb.t_post in
    let suffix_end =
      tb.chars.post + discard_tokens_for_length tb (tb.offset + count) in
    debugln "suffix_end = %d" suffix_end;
  (* These chars are removed from the token in addition to count *)
    tb.offset <- 0;
    Profile.time_switch_to "update tokens";
    let start, excess, written =
      update_tokens tb prefix_start suffix_end (prefix_start - min_undo_start) in
    let deleted_prefix =
      Array.sub tb.t_buf tb.t_pre (prefix_length - tb.t_pre)
    and deleted_suffix =
      Array.sub tb.t_buf initial_post (tb.t_post - initial_post) in
    List.iter (insert `before tb) written;
    debugln "updated, excess = %d" excess;
    let len = tb.chars.pre + suffix_end - tb.chars.post in
    debug "goto %d" (len + excess);
    GapBuffer.goto tb.chars (len + excess);
    Profile.time_pop ();
    debugln " OK";
    debugln "%t" (dump tb.chars);
    debugln "%t" (t_dump tb);
    debugln "AFTER REPLACE:\n%t\n%t" (snapshot tb.chars) (t_snapshot tb);
    start, deleted_prefix, deleted_suffix, written

(*
  let replace tb count = Profile.time_call "replace" (replace tb count)
*)

  (* Remove the longest common prefix, and return it. *)
  let rec remove_common prefix = function
    | x :: u, y :: v when equals x y -> remove_common (x :: prefix) (u, v)
    | u, v -> List.rev prefix, u, v

  let replace tb count s =
    let start, deleted_prefix, deleted_suffix, written =
      replace tb count s in
(*
    debug "deleted prefix: |";
    Array.iter (function t -> debug "%s|" (string t)) deleted_prefix;
    debug "\ndeleted suffix: |";
    Array.iter (function t -> debug "%s|" (string t)) deleted_suffix;
    debug "\nwritten: |";
    List.iter (function t -> debug "%s|" (string t)) written;
    debugln "";
*)
    let deleted = Array.to_list (Array.append deleted_prefix deleted_suffix) in
    (* Remove the longest common prefix *)
    let common_prefix, written, deleted = remove_common [] (written, deleted) in
    let rev_common_suffix, rev_written, rev_deleted =
      remove_common [] (List.rev written, List.rev deleted) in
    let start =
      List.fold_left
        (fun start t -> start + length t)
        start
        common_prefix
    and old_len =
      List.fold_left
        (fun len t -> len + length t)
        0
        rev_deleted
    and modified = List.rev rev_written in
(*
    debug "smallest modif: start = %d, len = %d ->\n|" start old_len;
    List.iter (function t -> debug "%s|" (string t)) modified;
*)
    start, old_len, Array.of_list modified

  let replace tb count = (*Profile.time_call "replace (outer)"*) (replace tb count)

  (* 3.11 compatibility *)
  module type T = sig
    type tokenized_buffer = private {
      chars : GapBuffer.gap_buffer;
      mutable t_buf : Token.token array;
      mutable t_pre : int;
      mutable t_post : int;
      mutable offset : int;
      state: Token.state
    }
    val create : int -> int -> Token.state -> tokenized_buffer
    val clear : tokenized_buffer -> unit
    val t_length : tokenized_buffer -> int
    val t_snapshot : tokenized_buffer -> out_channel -> unit
    val goto : tokenized_buffer -> int -> unit
    val forward : tokenized_buffer -> unit
    val backward : tokenized_buffer -> unit
    val pos_bot : tokenized_buffer -> int -> int
    val pos_bont : tokenized_buffer -> int -> int
    val sub : tokenized_buffer -> int -> int -> Token.token array
    val pos2pointer : tokenized_buffer -> int -> int
    val replace : tokenized_buffer -> int -> string -> int * int * Token.token array
  end

end
