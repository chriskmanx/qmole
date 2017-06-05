(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Bincompat

let ghost_loc loc =
  { loc with Location.loc_ghost = true }

  let string s = s

module AST = struct


module Asttypes = struct

    open Asttypes
    module T = V3112_types.Asttypes


    let constant c =
      match c with
        T.Const_int int ->  Const_int int
      | T.Const_char char -> Const_char char
      | T.Const_string  string -> Const_string string
      | T.Const_float string -> Const_float string
      | T.Const_int32 int32 -> Const_int32 int32
      | T.Const_int64 int64 -> Const_int64 int64
      | T.Const_nativeint nativeint -> Const_nativeint nativeint


    let virtual_flag vf =
      match vf with
        T.Virtual -> Virtual
      | T.Concrete -> Concrete

    let private_flag pf =
      match pf with
        T.Private -> Private
      | T.Public -> Public

    let rec_flag r =
      match r with
        T.Nonrecursive -> Nonrecursive
      | T.Recursive -> Recursive
      | T.Default -> Default

    let mutable_flag mf =
      match mf with
        T.Immutable -> Immutable
      | T.Mutable -> Mutable

    let direction_flag d =
      match d with
        T.Upto -> Upto
      | T.Downto -> Downto

    let closed_flag = Open

    let override_flag = Override



    let label s = s

end

module Lexing = struct

    open Lexing
    module T = V3112_types.Lexing

    let position p =
      { pos_fname = p.T.pos_fname;
        pos_lnum = p.T.pos_lnum;
        pos_bol = p.T.pos_bol;
        pos_cnum = p.T.pos_cnum;
      }

  end

module Location = struct

    open Location
    module T = V3112_types.Location

    let t loc =
      { loc_start = Lexing.position loc.T.loc_start;
        loc_end = Lexing.position loc.T.loc_end;
        loc_ghost = loc.T.loc_ghost;
      }

  end

module Longident = struct

    open Longident
    module T = V3112_types.Longident

  let rec t l =
    match l with
	T.Lident s -> Lident s
      | T.Ldot (ll, s) -> Ldot (t ll, s)
      | T.Lapply (l1, l2) -> Lapply (t l1, t l2)

end


module Parsetree : sig

  val signature :
      V3112_types.Parsetree.signature -> Parsetree.signature

  val structure :
    V3112_types.Parsetree.structure -> Parsetree.structure

end = struct

  open Asttypes

  open Parsetree
  module T = V3112_types.Parsetree

  let rec core_type c =
    { ptyp_desc = core_type_desc c.T.ptyp_desc;
      ptyp_loc = Location.t c.T.ptyp_loc }

  and core_type_desc c =
    match c with
    T.Ptyp_any -> Ptyp_any
  | T.Ptyp_var string -> Ptyp_var string
  | T.Ptyp_arrow (l, c1, c2) ->
      Ptyp_arrow (label l, core_type c1, core_type c2)
  | T.Ptyp_tuple list -> Ptyp_tuple (List.map core_type list)
  | T.Ptyp_constr (l, list) ->
      Ptyp_constr (Longident.t l, List.map core_type list)
  | T.Ptyp_object list -> Ptyp_object (List.map core_field_type list)
  | T.Ptyp_class (l, clist, llist) ->
      Ptyp_class (Longident.t l, List.map core_type clist, List.map label llist)
  | T.Ptyp_alias (c1, string) -> Ptyp_alias (core_type c1, string)
  | T.Ptyp_variant (list, bool, option) ->
      Ptyp_variant (List.map row_field list, bool,
		    match option with
			None -> None
		      | Some list -> Some (List.map label list))
  | T.Ptyp_poly (list, c1) -> Ptyp_poly (list, core_type c1)

and package_type (l, list) =
    (Longident.t l,List.map (fun (s,c) -> (s, core_type c)) list)

and core_field_type c =
  { pfield_desc = core_field_desc c.T.pfield_desc;
    pfield_loc = Location.t c.T.pfield_loc;
  }

and core_field_desc c =
    match c with
	T.Pfield (s, c1) -> Pfield (s, core_type c1)
      | T.Pfield_var -> Pfield_var

and row_field r =
    match r with
    T.Rtag (l, bool, list) ->
      Rtag (label l, bool, List.map core_type list)
  | T.Rinherit c -> Rinherit (core_type c)

let class_infos a c =
  { pci_virt = virtual_flag c.T.pci_virt;
    pci_params = (let (list, l) = c.T.pci_params in
      (list, Location.t l));
    pci_name = c.T.pci_name;
    pci_expr = a c.T.pci_expr;
    pci_variance = c.T.pci_variance;
    pci_loc = Location.t c.T.pci_loc;
  }


let option f x =
  match x with
    None -> None
  | Some s -> Some (f s)

let rec pattern p =
  { ppat_desc =  pattern_desc p.T.ppat_desc;
    ppat_loc = Location.t p.T.ppat_loc }

and pattern_desc p =
  match p with
    T.Ppat_any -> Ppat_any
  | T.Ppat_var s -> Ppat_var s
  | T.Ppat_alias (p1, s) -> Ppat_alias (pattern p1, s)
  | T.Ppat_constant c -> Ppat_constant (constant c)
  | T.Ppat_tuple list -> Ppat_tuple (List.map pattern list)
  | T.Ppat_construct (l, o, bool) ->
      Ppat_construct (Longident.t l, option pattern o, bool)
  | T.Ppat_variant (l, o) ->
      Ppat_variant (label l, option pattern o)
  | T.Ppat_record list ->
      Ppat_record (List.map (fun (l, p) ->
            (Longident.t l, pattern p)) list, closed_flag)
  | T.Ppat_array list ->  Ppat_array (List.map pattern list)
  | T.Ppat_or (p1, p2) -> Ppat_or (pattern p1, pattern p2)
  | T.Ppat_constraint (p1, c) -> Ppat_constraint (pattern p1, core_type c)
  | T.Ppat_type l -> Ppat_type (Longident.t l)
  | T.Ppat_lazy p1 -> Ppat_lazy (pattern p1)

let ghexp d loc = { pexp_desc = d; pexp_loc = ghost_loc loc; }
let ghtyp d loc = { ptyp_desc = d; ptyp_loc = ghost_loc loc; }
let mkexp exp loc = { pexp_desc = exp; pexp_loc = loc }

let rec expression e =
  let pexp_loc = Location.t e.T.pexp_loc in
  let pexp_desc = expression_desc e.T.pexp_desc
  in
  { pexp_desc; pexp_loc  }

and expression_desc e =
  match e with
    T.Pexp_ident l -> Pexp_ident (Longident.t l)
  | T.Pexp_constant c -> Pexp_constant (constant c)
  | T.Pexp_let (r, list, e) ->
      Pexp_let (rec_flag r,
        List.map (fun (p,e) ->
            (pattern p, expression e)) list, expression e)
  | T.Pexp_function (l, o, list) ->
      Pexp_function (label l, option expression o,
        List.map (fun (p,e) -> (pattern p, expression e)) list)
  | T.Pexp_apply (e, list) ->
      Pexp_apply (expression e, List.map (fun (l, e) ->
            (label l,  expression e)) list)
  | T.Pexp_match (e, list) ->
      Pexp_match (expression e,
        List.map (fun (p,e) -> (pattern p, expression e)) list)
  | T.Pexp_try (e, list) ->
      Pexp_try (expression e,
        List.map (fun (p,e) -> (pattern p, expression e)) list)
  | T.Pexp_tuple list ->
      Pexp_tuple (List.map expression list)
  | T.Pexp_construct (l, o, bool) ->
      Pexp_construct (Longident.t l, option expression o, bool)
  | T.Pexp_variant (l, o) ->
      Pexp_variant (label l, option expression o)
  | T.Pexp_record (list, o) ->
      Pexp_record (List.map (fun (l, e) ->
            (Longident.t l, expression e)) list, option expression o)
  | T.Pexp_field (e, l) ->
      Pexp_field (expression e, Longident.t l)
  | T.Pexp_setfield (e1, l, e2) ->
      Pexp_setfield (expression e1, Longident.t l, expression e2)
  | T.Pexp_array list -> Pexp_array (List.map expression list)
  | T.Pexp_ifthenelse (e1, e2, o) ->
      Pexp_ifthenelse (expression e1, expression e2, option expression o)
  | T.Pexp_sequence (e1, e2) ->
      Pexp_sequence (expression e1, expression e2)
  | T.Pexp_while (e1, e2) ->
      Pexp_while (expression e1, expression e2)
  | T.Pexp_for (s, e1, e2, d, e3) ->
      Pexp_for (s, expression e1, expression e2,
        direction_flag d, expression e3)
  | T.Pexp_constraint (e1, o1, o2) ->
      Pexp_constraint (expression e1, option core_type o1, option core_type o2)
  | T.Pexp_when (e1, e2) -> Pexp_when (expression e1, expression e2)
  | T.Pexp_send (e1, s) -> Pexp_send (expression e1, s)
  | T.Pexp_new l -> Pexp_new (Longident.t l)
  | T.Pexp_setinstvar (s, e) -> Pexp_setinstvar (s, expression e)
  | T.Pexp_override list ->
      Pexp_override (List.map (fun (s, e) -> (s, expression e)) list)
  | T.Pexp_letmodule (s, m, e) ->
      Pexp_letmodule (s, module_expr m, expression e)
  | T.Pexp_assert e -> Pexp_assert (expression e)
  | T.Pexp_assertfalse -> Pexp_assertfalse
  | T.Pexp_lazy e -> Pexp_lazy (expression e)
  | T.Pexp_poly (e, o) -> Pexp_poly (expression e, option core_type o)
  | T.Pexp_object cl -> Pexp_object (class_structure cl)

and value_description v =
  {
    pval_type = core_type v.T.pval_type;
    pval_prim = v.T.pval_prim;
  }


and type_declaration t =
  { ptype_params = t.T.ptype_params;
    ptype_cstrs = List.map (fun (c1, c2, l) ->
        (core_type c1, core_type c2, Location.t l)) t.T.ptype_cstrs;
    ptype_kind = type_kind t.T.ptype_kind;
    ptype_private = private_flag t.T.ptype_private;
    ptype_manifest =  option core_type t.T.ptype_manifest;
    ptype_variance = t.T.ptype_variance;
    ptype_loc =  Location.t t.T.ptype_loc;
  }

and type_kind t =
  match t with
    T.Ptype_abstract -> Ptype_abstract
  | T.Ptype_variant list ->
      Ptype_variant (List.map (fun (s, list, l) ->
            (s, List.map core_type list, Location.t l)) list)
  | T.Ptype_record list ->
      Ptype_record (List.map (fun (s,m,c,l) ->
            (s, mutable_flag m, core_type c, Location.t l)) list)

and exception_declaration list = List.map core_type list

and class_type c =
  { pcty_desc = class_type_desc c.T.pcty_desc;
    pcty_loc = Location.t c.T.pcty_loc; }

and class_type_desc c =
  match c with
    T.Pcty_constr (l, list) ->
      Pcty_constr (Longident.t l, List.map core_type list)
  | T.Pcty_signature c -> Pcty_signature (class_signature c)
  | T.Pcty_fun (l, ct, cl) ->
      Pcty_fun (label l, core_type ct, class_type cl)

and class_signature (c, list) =
  (core_type c, List.map class_type_field list)

and class_type_field c =
  match c with
    T.Pctf_inher c -> Pctf_inher (class_type c)
  | T.Pctf_val (s, m, v, c, l) ->
      Pctf_val (s, mutable_flag m, virtual_flag v, core_type c, Location.t l)
  | T.Pctf_virt (s, p, c, l) ->
      Pctf_virt (s, private_flag p, core_type c, Location.t l)
  | T.Pctf_meth (s, p, c, l) ->
      Pctf_meth (s, private_flag p, core_type c, Location.t l)
  | T.Pctf_cstr (c1, c2, l) ->
      Pctf_cstr (core_type c1, core_type c2, Location.t l)

and class_description c = class_infos class_type c

and class_type_declaration c = class_infos class_type c

and class_expr c =
  { pcl_desc = class_expr_desc c.T.pcl_desc;
    pcl_loc = Location.t c.T.pcl_loc; }

and class_expr_desc c =
  match c with
    T.Pcl_constr (l, list) ->
      Pcl_constr (Longident.t l, List.map core_type list)
  | T.Pcl_structure c ->
      Pcl_structure (class_structure c)
  | T.Pcl_fun (l, o, p, c) ->
      Pcl_fun (label l, option expression o, pattern p, class_expr c)
  | T.Pcl_apply (c, list) ->
      Pcl_apply (class_expr c, List.map (fun (l, e) ->
            (label l, expression e)) list)
  | T.Pcl_let (r, list, c) ->
      Pcl_let (rec_flag r,
        List.map (fun (p,e) -> (pattern p, expression e)) list, class_expr c)
  | T.Pcl_constraint (c1, c2) ->
      Pcl_constraint (class_expr c1, class_type c2)

and class_structure (p, list) =
  (pattern p, List.map class_field list)

and class_field c =
  match c with
    T.Pcf_inher (e, op) ->
      Pcf_inher (override_flag, class_expr e, option string op)
  | T.Pcf_valvirt (s, mf, c, l) ->
      Pcf_valvirt (s, mutable_flag mf, core_type c, Location.t l)
  | T.Pcf_val (s, mf, e, loc) ->
      Pcf_val (s, mutable_flag mf, override_flag, expression e,  Location.t loc)
  | T.Pcf_virt  (s, pf, c, loc) ->
      Pcf_virt (s, private_flag pf, core_type c, Location.t loc)
  | T.Pcf_meth (s, pf, e, loc) ->
      Pcf_meth (s , private_flag pf, override_flag, expression e, Location.t loc)
  | T.Pcf_cstr (c1, c2, loc) ->
      Pcf_cstr (core_type c1, core_type c2, Location.t loc)
  | T.Pcf_let  (r, list, loc) ->
      Pcf_let ( rec_flag r, List.map (fun (pat, e)  ->
            (pattern pat, expression e)) list, Location.t loc)
  | T.Pcf_init e -> Pcf_init (expression e)

and class_declaration list = class_infos class_expr list

and module_type m =
  { pmty_desc = module_type_desc m.T.pmty_desc;
    pmty_loc = Location.t m.T.pmty_loc }

and module_type_desc m =
  match m with
    T.Pmty_ident l -> Pmty_ident (Longident.t l)
  | T.Pmty_signature sg -> Pmty_signature (signature sg)
  | T.Pmty_functor (s, mt1, mt2) ->
      Pmty_functor (s, module_type mt1, module_type mt2)
  | T.Pmty_with (mt, list) ->
      Pmty_with (module_type mt, List.map (fun (l, w) ->
            (Longident.t l, with_constraint w)) list)

and signature list = List.map signature_item list

and signature_item sg =
  { psig_desc = signature_item_desc sg.T.psig_desc;
    psig_loc = Location.t sg.T.psig_loc }

and signature_item_desc si =
  match si with
    T.Psig_value (s, v) ->
      Psig_value (s, value_description v)
  | T.Psig_type list ->
      Psig_type (List.map (fun (s, t) -> (string s, type_declaration t)) list)
  | T.Psig_exception (s, e) ->
      Psig_exception (s,  exception_declaration e)
  | T.Psig_module (s, mt) ->
      Psig_module (s, module_type mt)
  | T.Psig_recmodule list ->
      Psig_recmodule (List.map (fun (s, mt) -> (s, module_type mt)) list)
  | T.Psig_modtype (s, md) ->
      Psig_modtype (s, modtype_declaration md)
  | T.Psig_open l -> Psig_open (Longident.t l)
  | T.Psig_include mt -> Psig_include (module_type mt)
  | T.Psig_class list ->
      Psig_class (List.map class_description list)
  | T.Psig_class_type list -> Psig_class_type (List.map class_type_declaration list)

and modtype_declaration m =
  match m with
    T.Pmodtype_abstract -> Pmodtype_abstract
  | T.Pmodtype_manifest mt -> Pmodtype_manifest (module_type mt)

and with_constraint w =
  match w with
    T.Pwith_type t -> Pwith_type (type_declaration t)
  | T.Pwith_module l -> Pwith_module (Longident.t l)

(* value expressions for the module language *)

and module_expr me =
  let pmod_loc = Location.t me.T.pmod_loc in
  let pmod_desc = module_expr_desc me.T.pmod_desc in
  { pmod_desc; pmod_loc }

and module_expr_desc me =
  match me with
    T.Pmod_ident l -> Pmod_ident (Longident.t l)
  | T.Pmod_structure (s) -> Pmod_structure (structure s)
  | T.Pmod_functor (s, mt, me) ->
      Pmod_functor (s, module_type mt, module_expr me)
  | T.Pmod_apply (me1, me2) -> Pmod_apply (module_expr me1, module_expr me2)
  | T.Pmod_constraint (me, mt) -> Pmod_constraint (module_expr me, module_type mt)

and structure list = List.map structure_item list

and structure_item si =
  { pstr_desc = structure_item_desc si.T.pstr_desc;
    pstr_loc = Location.t  si.T.pstr_loc }

and structure_item_desc si =
  match si with
    T.Pstr_eval e -> Pstr_eval (expression e)
  | T.Pstr_value (r, list) -> Pstr_value (rec_flag r,
        List.map (fun (pat, e) -> (pattern pat, expression e)) list)
  | T.Pstr_primitive (s, v) -> Pstr_primitive (s, value_description v)
  | T.Pstr_type (list) -> Pstr_type (List.map
        (fun (s, t) -> (s, type_declaration t)) list)
  | T.Pstr_exception (s, e) -> Pstr_exception (s, exception_declaration e)
  | T.Pstr_exn_rebind (s, l) -> Pstr_exn_rebind (s, Longident.t l)
  | T.Pstr_module (s, me) -> Pstr_module (s, module_expr me)
  | T.Pstr_recmodule list -> Pstr_recmodule (List.map (
          fun (s, mt, me) -> (s, module_type mt, module_expr me)) list)
  | T.Pstr_modtype (s, mt) -> Pstr_modtype (s, module_type mt)
  | T.Pstr_open (l) -> Pstr_open (Longident.t l)
  | T.Pstr_class (list) -> Pstr_class (List.map class_declaration list)
  | T.Pstr_class_type (list) -> Pstr_class_type (List.map class_type_declaration list)
  | T.Pstr_include (me) -> Pstr_include (module_expr me)

    (*
(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | T.Ptop_dir (a) -> Ptop_dir (string, directive_argument

and directive_argument =
    Pdir_none
  | T.Pdir_string (a) -> Pdir_string (string
  | T.Pdir_int (a) -> Pdir_int (int
  | T.Pdir_ident (a) -> Pdir_ident (Longident.t
  | T.Pdir_bool (a) -> Pdir_bool (bool
*)

end

end

let input_intf_file ic magic =
  if magic <> V3112_types.ast_intf_magic_number then
    raise (Error (No_Such_Magic ("intf", magic)))
  else begin
    let v = (input_value ic : V3112_types.Parsetree.signature) in
      AST.Parsetree.signature v
    end


let input_impl_file ic magic =
  if magic <> V3112_types.ast_impl_magic_number then
    raise (Error (No_Such_Magic ("impl", magic)))
  else begin
      let v = (input_value ic : V3112_types.Parsetree.structure) in
      AST.Parsetree.structure v
    end
