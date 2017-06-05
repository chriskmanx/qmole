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
include Debug.Tag(struct let tag = "gapBuffer" end)

type mark = int ref

type gap_buffer = {
  mutable buf : string;
    (* gap buffer of characters. *)
  mutable pre : int;
    (* start of the gap : pre <= post *)
  mutable post : int; (* end of the gap *)
    (* the string is [0, pre[ @ [post, String.length buf[, point is at pre *)
  mutable line : int;
    (* 1 + number of eols in [0, pre[ = line number of buf.[pre] counting from 1 *)
  mutable marks_before : mark list;
  (* A list of markers < pre, in decreasing order *)
  mutable marks_after : mark list
  (* A list of markers >= post, in increasing order *)
}

let create size =
  let size = max 1 size in {
    buf = String.create size;
    pre = 0;
    post = size;
    line = 1;
    marks_before = [];
    marks_after = []
  }

let dump { buf = buf ; pre = pre ; post = post ; line = line } c =
  Printf.fprintf c "|buf|=%d, pre=%d, post=%d, line=%d" (String.length buf) pre post line

let snapshot { buf = buf ; pre = pre ; post = post } c =
  let print i = Printf.fprintf c "%c" buf.[i] in
  Printf.fprintf c "...";
  for i = max 0 (pre - 20) to pre - 1 do print i done;
  Printf.fprintf c "$";
  for i = post to min (post + 19) (String.length buf - 1) do print i done;
  Printf.fprintf c "..."

let clear gb =
  gb.pre <- 0;
  gb.post <- String.length gb.buf;
  gb.line <- 1;
  gb.marks_before <- [];
  gb.marks_after <- []

let contents {buf = buf ; pre = pre ; post = post} =
  let s = String.create (pre + String.length buf - post) in
  String.blit buf 0 s 0 pre;
  String.blit buf post s pre (String.length buf - post);
  s

let pos2pointer gb pos =
  if pos < gb.pre then
    pos
  else
    pos + gb.post - gb.pre

let pointer2pos gb pos =
  if pos < gb.pre then
    pos
  else if pos >= gb.post then
    pos - gb.post + gb.pre
  else
    invalid_arg "pointer2pos"

exception MarkDeleted

let mark2pos gb mark =
  if !mark = -1 then
    raise MarkDeleted
  else
    pointer2pos gb !mark

let rec insert ( < ) x = function
  | y :: l when y < x -> y :: insert ( < ) x l
  | l -> x :: l

let mark gb pos =
  if pos < gb.pre then
    let m = ref pos in
    gb.marks_before <- insert ( > ) m gb.marks_before;
    m
  else
    let m = ref (pos + gb.post - gb.pre) in
    gb.marks_after <- insert ( < ) m gb.marks_after;
    m

let delete_mark gb m =
  if !m = -1 then
    ()
  else if !m < gb.pre then
    (* We could be slightly more efficient. *)
    gb.marks_before <- List.filter (( != ) m) gb.marks_before
  else if !m >= gb.post then
    gb.marks_after <- List.filter (( != ) m) gb.marks_after
  else
    invalid_arg "delete_mark";
  m := -1

let substring ({buf = buf; pre = pre ; post = post} as gb) b e =
  if b < 0 || e > pre + String.length buf - post then
    invalid_arg "substring";
  debugln "substring";
  let len = e - b in
  let e = pos2pointer gb e in
  let s = String.create len in
  let before_gap = min len (max 0 (pre - b))
  and after_gap = min len (max 0 (e - post)) in
  assert (before_gap + after_gap = len);
  debugln "blit \"%d\" %d \"%d\" %d %d" (String.length buf) b (String.length s) 0 before_gap;
  String.blit buf b s 0 before_gap;
  debugln "blit \"%d\" %d \"%d\" %d %d" (String.length buf) (e - after_gap) (String.length s) (len - after_gap) after_gap;
  String.blit buf (e - after_gap) s (len - after_gap) after_gap;
  debugln "OK";
  s

let resize gb =
  let len = String.length gb.buf in
  let buf = String.create (2 * len) in
  String.blit gb.buf 0 buf 0 gb.pre;
  String.blit gb.buf gb.post buf (len + gb.post) (len - gb.post);
  gb.buf <- buf;
  gb.post <- len + gb.post;
  List.iter (function m -> m := !m + len) gb.marks_after

let forward ({buf = buf ; pre = pre ; post = post} as gb) =
  if buf.[post] = '\n' then
    gb.line <- gb.line + 1;
  buf.[pre] <- buf.[post];
  let gap_size = gb.post - gb.pre in
  while (
    match gb.marks_after with
      | m :: after when !m = gb.post ->
        gb.marks_after <- after ;
        m := !m - gap_size;
        gb.marks_before <- m :: gb.marks_before;
        true
      | _ -> false
  ) do () done;
  gb.pre <- gb.pre + 1;
  gb.post <- gb.post + 1

let backward ({buf = buf} as gb) =
  gb.pre <- gb.pre - 1;
  gb.post <- gb.post - 1;
  buf.[gb.post] <- buf.[gb.pre];
  if buf.[gb.post] = '\n' then
    gb.line <- gb.line - 1;
  let gap_size = gb.post - gb.pre in
  while (
    match gb.marks_before with
      | m :: before when !m = gb.pre ->
        gb.marks_before <- before ;
        m := !m + gap_size;
        gb.marks_after <- m :: gb.marks_after;
        true
      | _ -> false
  ) do () done

let goto gb n =
  while gb.pre < n do forward gb done;
  while gb.pre > n do backward gb done

let insert pos gb c =
  if gb.post = gb.pre then
    resize gb;
  match pos with
    | `before ->
      gb.buf.[gb.pre] <- c;
      gb.pre <- gb.pre + 1;
      if c = '\n' then
        gb.line <- gb.line + 1
    | `after ->
      gb.post <- gb.post - 1;
      gb.buf.[gb.post] <- c

(* does not count newlines:
let insert_string gb s =
  let len = String.length s in
  while gb.post - gb.pre < len do
    resize gb
  done;
  String.blit s 0 gb.buf gb.pre len;
  gb.pre <- gb.pre + len
  *)

let insert_string pos gb s =
  match pos with
    | `before -> String.iter (insert pos gb) s
    | `after ->
      for i = String.length s - 1 downto 0 do
        insert pos gb s.[i]
      done

let delete pos gb n =
  match pos with
    | `before ->
      gb.pre <- gb.pre - n;
      gb.marks_before <-
        List.drop_while
        (function m -> !m >= gb.pre && (m := -1 ; true))
        gb.marks_before
    | `after ->
      gb.post <- gb.post + n;
      gb.marks_after <-
        List.drop_while
        (function m -> !m < gb.post && (m := -1 ; true))
        gb.marks_after

let replace gb l s =
  delete `after gb l;
  insert_string `before gb s
