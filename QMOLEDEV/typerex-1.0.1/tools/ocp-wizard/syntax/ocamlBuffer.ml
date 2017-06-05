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
include Debug.Tag(struct let tag = "ocamlBuffer" end)

open OcamlTokenize
open OCamlToken
open OCamlTokenBuffer

type buffer_state = {
  filename : string;
  contents : tokenized_buffer;
  program : (Program.program * Program.source_file_id) Lazy.t;
  local_envs : Completion.local_envs;
  mutable needs_refontifying : (mark * mark) option;
  mutable last_completion : (Env_untyped.path_sort * string * string Lazy.t) list
}

(* maps absolute file names to buffers *)
let buffers = Hashtbl.create 100

let find_or_create ?(init= function () -> raise Not_found) buffername =
  try Hashtbl.find buffers buffername
  with Not_found ->
    let filename, program = Profile.time_call "init buffer" init () in
    let state = {
      filename = filename;
      program = program;
      local_envs = Completion.initial_env program;
      contents = empty ();
      needs_refontifying = None;
      last_completion = []
    } in
    Hashtbl.add buffers buffername state;
    state

let update_buffer buff ~start ~old_length first_time s =
  let start', end', t_start, t_end =
    update_buffer buff.contents ~start ~old_length first_time s in
  let updated =
    let {chars = chars} = buff.contents in
    let start, end_ =
      match buff.needs_refontifying with
        | Some (start, end_) ->
           (try Some (start, mark2pos chars start)
            with MarkDeleted -> None),
           (try Some (end_, mark2pos chars end_)
            with MarkDeleted -> None)
        | None -> None, None
    in
    (match start with
       | Some (start, p) ->
           if p <= start' then
             start
           else (delete_mark chars start ; mark chars start')
       | None -> mark chars start'),
    (match end_ with
       | Some (end_, p) ->
           if p >= end' then
             end_
           else (delete_mark chars end_ ; mark chars end')
       | None -> mark chars end')
  in
  buff.needs_refontifying <- Some updated;
  Completion.invalidate_env_after buff.local_envs t_start
