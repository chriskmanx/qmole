(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
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

let cmd = Sys.argv.(0)

let usage = Printf.sprintf "Usage:

    %s [-emacs] -o <file>

" cmd

type t =
  | Emacs

let gen  = ref None
let file = ref None

let gen_emacs = Arg.Unit   (fun () -> gen  := Some Emacs)
let set_file  = Arg.String (fun s  -> file := Some s)

let args = Arg.align [
  "-emacs", gen_emacs, "        generate emacs bindings";
  "-o"    , set_file , " <file> set the output filename";
]

let nice_exit () =
  Arg.usage args usage;
  exit 2

let no_ano _ = nice_exit ()

let _ =
  Arg.parse args no_ano usage

let gen = match !gen with
  | None
  | Some Emacs -> Emacs

let filename = match !file with
  | None   -> nice_exit ()
  | Some f -> f

module M = Plugin.Make(Emacs)

let _ =
  match gen with
    | Emacs -> Emacs.(
      let code_of_keymap = code_of_keymap ~add_hook:true in
      IDE.save ~lang ~code_of_command ~code_of_keymap ~filename
    )
