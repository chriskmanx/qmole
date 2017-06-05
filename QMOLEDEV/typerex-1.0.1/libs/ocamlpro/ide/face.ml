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

(** Colors *)
type color = [
  `rgb of int * int * int (** 3*8bit rgb *)
| `x11 of string
]

type face_attr = [
  `weight of [`ultra_bold | `bold | `normal | `light]
| `slant of [`italic | `oblique | `normal | `reverse_italic | `reverse_oblique]
| `foreground of color
| `background of color
| `underline of color option
]

(** Typerex-mode customizable faces *)
type tface = [
  `governing
| `multistage
| `operator
| `error
| `interactive_output
| `interactive_error
]

(** Caml-mode customizable faces *)
type cface = [ `doccomment | `stop ]

(** Emacs Font Lock predefined faces *)
type fface = [
  `comment
| `comment_delimiter
| `doc
| `string
| `keyword
| `builtin
| `function_name
| `variable_name
| `ftype
| `constant
| `preprocessor
| `negation_char
| `warning
]

(** Faces can be described either by a "portable" definition or by an
    editor-specific customizable face name. *)
type face = [
  `desc of face_attr list
| `typerex of tface (** tuareg-mode faces *)
| `caml of cface (** caml-mode faces *)
| `font_lock of fface (** font-lock predefined *)
| `ocp of string (* named OCP faces *)
| `none (** used to clear the face *)
]

let highlight_definition = `ocp "highlight-def"
let highlight_reference = `ocp "highlight-occ"

let read_colors file =
  let colors = ref [] in
  File.iter_lines
    (function l ->
      try
        Scanf.sscanf l " %d %d %d %[^\n]"
          (fun r g b color ->
            colors := (String.strip color, (r, g, b)) :: !colors)
      with _ -> Printf.eprintf "ommit %s\n%!" l)
    file;
  List.rev !colors

(** X11 named colors and their 3*16bit rgb values. *)

(* This would require proper configuration of the X11 path
let x11_colors = read_colors "/usr/share/X11/rgb.txt"
*)
(* Instead, we include the colors by generating a file x11_colors.ml.
let _ =
  let c = open_out "/home/tifn/x11_colors.ml" in
  List.iter
    (function (color, (r, g, b)) ->
      Printf.fprintf c "  %S, (%d, %d, %d);\n" color r g b)
    (read_colors "/usr/share/X11/rgb.txt");
  close_out c
*)

let color_emacs_name = function
  | `rgb (r, g, b) ->
(*
    let r = r * 256
    and g = g * 256
    and b = b * 256 in
*)
    Printf.sprintf "#%02X%02X%02X" r g b
  | `x11 color -> color

let color_rgb = function
  | `rgb (r, g, b) -> r, g, b
  | `x11 color ->
    try
      List.assoc color X11_colors.x11_colors
    with Not_found ->
      failwith ("color " ^ color ^ " not found")

let color_eclipse_name c =
  let r, g, b = color_rgb c in
  Printf.sprintf "#%03d%03d%03d" r g b

let tface2string = function
  | `governing -> "governing"
  | `operator -> "operator"
  | `multistage -> "multistage"
  | `error -> "error"
  | `interactive_output -> "interactive-output"
  | `interactive_error -> "interactive-error"

let cface2string = function
  | `doccomment -> "doccomment"
  | `stop -> "stop"

let fface2string = function
  | `comment -> "comment"
  | `comment_delimiter -> "comment-delimiter"
  | `doc -> "doc"
  | `string -> "string"
  | `keyword -> "keyword"
  | `builtin -> "builtin"
  | `function_name -> "function-name"
  | `variable_name -> "variable-name"
  | `ftype -> "type"
  | `constant -> "constant"
  | `preprocessor -> "preprocessor"
  | `negation_char -> "negation-char"
  | `warning -> "warning"

let weight2string = function
  | `ultra_bold -> "ultra-bold"
  | `bold -> "bold"
  | `normal -> "normal"
  | `light -> "light"

let slant2string = function
  | `italic -> "italic"
  | `oblique -> "oblique"
  | `normal -> "normal"
  | `reverse_italic -> "reverse_italic"
  | `reverse_oblique -> "reverse_oblique"

let face_emacs_name = function
  | `none -> "nil"
  | `typerex tface ->
    Printf.sprintf "'typerex-font-lock-%s-face" (tface2string tface)
  | `caml cface ->
    Printf.sprintf "caml-font-%s-face" (cface2string cface)
  | `font_lock fface ->
    Printf.sprintf "font-lock-%s-face" (fface2string fface)
  | `ocp name -> Printf.sprintf "'ocp-face-%s" name
  | `desc attributes ->
    Printf.sprintf "`(%s)"
      (String.concat " "
         (List.map
            (function
              | `weight w -> Printf.sprintf ":weight %s" (weight2string w)
              | `slant w -> Printf.sprintf ":slant %s" (slant2string w)
              | `foreground color ->
                Printf.sprintf ":foreground %S" (color_emacs_name color)
              | `background color ->
                Printf.sprintf ":background %S" (color_emacs_name color)
              | `underline (Some color) ->
                Printf.sprintf ":underline %S" (color_emacs_name color)
              | `underline None -> Printf.sprintf ":underline ,t")
            attributes))

let face_eclipse_name = function
  | `none -> "none"
  | `typerex tface -> (tface2string tface)
  | `caml cface -> (cface2string cface)
  | `font_lock fface -> (fface2string fface)
  | `ocp name -> Printf.sprintf "ocp-%s" name
  | `desc attributes ->
    Printf.sprintf "face:%s"
      (String.concat ":"
         (List.map
            (function
              | `weight w -> Printf.sprintf "weight:%s" (weight2string w)
              | `slant w -> Printf.sprintf "slant:%s" (slant2string w)
              | `foreground color ->
                Printf.sprintf "foreground:%s" (color_eclipse_name color)
              | `background color ->
                Printf.sprintf "background:%s" (color_eclipse_name color)
              | `underline (Some color) ->
                Printf.sprintf "underline:%s" (color_eclipse_name color)
              | `underline None -> Printf.sprintf "underline:none")
            attributes))



