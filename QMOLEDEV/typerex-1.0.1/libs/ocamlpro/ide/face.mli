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

(** Colors and faces, usable for Emacs and Eclipse. *)

(** {4 Colors} *)

(** Colors *)
type color = [
  `rgb of int * int * int (** 3*8bit rgb *)
| `x11 of string (** An X11 color name *)
]

(** Return the 3*8bit rgb values of a color. *)
val color_rgb : color -> int * int * int

(** Represent a color either by its name, or as #RRRGGGBBB (in decimal). *)
val color_emacs_name : color -> string

(** Represent a color only as #RRRGGGBBB (in decimal). *)
val color_eclipse_name : color -> string

(** {4 Faces} *)

(** Portable "extensive" description of faces *)
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

(** Return an Emacs Lisp form representing the specified face. *)
val face_emacs_name : face -> string

(** Represent a face by either:
    - the name of the Emacs face, without the heading "font-lock-"
      or "typerex-font-lock-" and trailing "-face"
    - a string of the form face:<colon-separated list of attributes>
      e.g., face:weight:ultra-bold:foreground:#160082045
    - "none" *)
val face_eclipse_name : face -> string

(** Two faces for highlighting *)

val highlight_definition : face
val highlight_reference : face
