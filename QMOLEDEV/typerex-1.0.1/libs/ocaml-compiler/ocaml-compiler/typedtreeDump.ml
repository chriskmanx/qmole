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

(* We should not re-implement printtype *)

open Types
open Format

let rec path_name = function
  | Path.Pident id -> Ident.name_with_ctx id
  | Path.Pdot(p, s, pos) -> path_name p ^ "." ^ s
  | Path.Papply(p1, p2) -> path_name p1 ^ "(" ^ path_name p2 ^ ")"

let ident id fmt = fprintf fmt "%s" (Ident.name_with_ctx id)
let path p fmt = fprintf fmt "%s" (path_name p)

let rec_status = function
  | Trec_not -> "rec_not"
  | Trec_first -> "rec_first"
  | Trec_next -> "rec_next"

(*
let rec type_expr t fmt =
  match t.type_desc with
    | Tvar (Some v) -> fprintf fmt "'%s" v
    | Tvar None -> fprintf fmt "'%s" v
  | Tarrow of label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * string list * type_expr list
*)

let value_description vd fmt = ()
(*
  Printtyp.value_description vd
  type_expr vd.val_type
*)

let type_declaration vd fmt = ()
let exception_declaration vd fmt = ()
let class_declaration ctd fmt = ()
let class_type_declaration ctd fmt = ()

let rec signature sg fmt =
  fprintf fmt "sig@;@[<v 2>%t@]@,end@."
    (function fmt ->
      List.iter
	(function item ->
	  fprintf fmt "@,%t" (sig_item item))
	sg)

and sig_item i fmt =
  let item kind id = fprintf fmt "%s %t%s%t" kind (ident id)
  and rec_item kind id r =
    fprintf fmt "%s %t (%s)%s%t" kind (ident id) (rec_status r)
  in
  match i with
    | Sig_value (id, vd) ->
(*
      Printtyp.value_description id fmt vd
*)
      item "val" id " : " (value_description vd)
    | Sig_type (id, td, r) -> rec_item "type" id r "" (type_declaration td)
    | Sig_exception (id, ed) -> item "exception" id "" (exception_declaration ed)
    | Sig_module (id, mt, r) -> rec_item "module" id r " : " (module_type mt)
    | Sig_modtype (id, md) -> item "module type" id "" (modtype_declaration md)
    | Sig_class (id, cd, r) -> rec_item "class" id r "" (class_declaration cd)
    | Sig_class_type (id, ctd, r) ->
      rec_item "class type" id r "" (class_type_declaration ctd)

and module_type = function
  | Mty_ident p -> path p
  | Mty_signature sg -> signature sg
  | Mty_functor (id, mt, mt') ->
    (function fmt ->
      fprintf fmt "functor (%t : %t) -> %t"
	(ident id) (module_type mt) (module_type mt'))

and modtype_declaration d fmt =
  match d with
    | Modtype_abstract -> ()
    | Modtype_manifest mt -> fprintf fmt " : %t" (module_type mt)
