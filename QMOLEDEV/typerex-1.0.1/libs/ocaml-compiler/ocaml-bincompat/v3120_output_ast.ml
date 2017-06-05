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

let this_version = "3.12.0"

let ghost_loc loc =
  { loc with V3120_types.Location.loc_ghost = true }

let string s = s

module AST = struct


module Asttypes = struct

    open Asttypes
    module T = V3120_types.Asttypes


    let constant c =
      match c with
        Const_int int -> T. Const_int int
      | Const_char char -> T.Const_char char
      | Const_string  string -> T.Const_string string
      | Const_float string -> T.Const_float string
      | Const_int32 int32 -> T.Const_int32 int32
      | Const_int64 int64 -> T.Const_int64 int64
      | Const_nativeint nativeint -> T.Const_nativeint nativeint


    let virtual_flag vf =
      match vf with
        Virtual -> T.Virtual
      | Concrete -> T.Concrete

    let private_flag pf =
      match pf with
        Private -> T.Private
      | Public -> T.Public

    let rec_flag r =
      match r with
        Nonrecursive -> T.Nonrecursive
      | Recursive -> T.Recursive
      | Default -> T.Default

    let mutable_flag mf =
      match mf with
        Immutable -> T.Immutable
      | Mutable -> T.Mutable

    let direction_flag d =
      match d with
        Upto -> T.Upto
      | Downto -> T.Downto

    let closed_flag cf = match cf with
        Closed -> T.Closed | Open -> T.Open

    let override_flag ovf =
      match ovf with
        Override -> T.Override
      | Fresh -> T.Fresh

    let label s = s

end

module Lexing = struct

    open Lexing
    module T = V3120_types.Lexing

    let position p =
      { T.pos_fname = p.pos_fname;
        T.pos_lnum = p.pos_lnum;
        T.pos_bol = p.pos_bol;
        T.pos_cnum = p.pos_cnum;
      }

  end

module Location = struct

    open Location
    module T = V3120_types.Location

    let t loc =
      { T.loc_start = Lexing.position loc.loc_start;
        T.loc_end = Lexing.position loc.loc_end;
        T.loc_ghost = loc.loc_ghost;
      }

  end

module Longident = struct

    open Longident
    module T = V3120_types.Longident

  let rec t l =
    match l with
        Lident s -> T.Lident s
      | Ldot (ll, s) -> T.Ldot (t ll, s)
      | Lapply (l1, l2) -> T.Lapply (t l1, t l2)

end


module Parsetree : sig
  val signature :
    Parsetree.signature -> V3120_types.Parsetree.signature
  val structure :
    Parsetree.structure -> V3120_types.Parsetree.structure

end = struct

  open Asttypes

  open Parsetree
  module T = V3120_types.Parsetree

  let rec core_type c =
    { ptyp_desc = core_type_desc c.ptyp_desc;
      T.ptyp_loc = Location.t c.ptyp_loc }

  and core_type_desc c =
    match c with
    Ptyp_any -> T.Ptyp_any
  | Ptyp_var string -> T.Ptyp_var string
  | Ptyp_arrow (l, c1, c2) ->
      T.Ptyp_arrow (label l, core_type c1, core_type c2)
  | Ptyp_tuple list -> T.Ptyp_tuple (List.map core_type list)
  | Ptyp_constr (l, list) ->
      T.Ptyp_constr (Longident.t l, List.map core_type list)
  | Ptyp_object list -> T.Ptyp_object (List.map core_field_type list)
  | Ptyp_class (l, clist, llist) ->
      T.Ptyp_class (Longident.t l, List.map core_type clist, List.map label llist)
  | Ptyp_alias (c1, string) -> T.Ptyp_alias (core_type c1, string)
  | Ptyp_variant (list, bool, option) ->
      T.Ptyp_variant (List.map row_field list, bool,
		    match option with
              None -> None
		      | Some list -> Some (List.map label list))
  | Ptyp_poly (list, c1) -> T.Ptyp_poly (list, core_type c1)
  | Ptyp_package p -> T.Ptyp_package (package_type p)

and package_type (l, list) =
    (Longident.t l,List.map (fun (s,c) -> T.(s, core_type c)) list)

and core_field_type c =
  { T.pfield_desc = core_field_desc c.pfield_desc;
    T.pfield_loc = Location.t c.pfield_loc;
  }

and core_field_desc c =
    match c with
        Pfield (s, c1) -> T.Pfield (s, core_type c1)
      | Pfield_var -> T.Pfield_var

and row_field r =
    match r with
    Rtag (l, bool, list) ->
      T.Rtag (label l, bool, List.map core_type list)
  | Rinherit c -> T.Rinherit (core_type c)

let class_infos a c =
  { T.pci_virt = virtual_flag c.pci_virt;
    pci_params = (let (list, l) = c.pci_params in
      (list, Location.t l));
    pci_name = c.pci_name;
    pci_expr = a c.pci_expr;
    pci_variance = c.pci_variance;
    pci_loc = Location.t c.pci_loc;
  }


let option f x =
  match x with
    None -> None
  | Some s -> Some (f s)

let rec pattern p =
  { T.ppat_desc =  pattern_desc p.ppat_desc;
    ppat_loc = Location.t p.ppat_loc }

and pattern_desc p =
  match p with
    Ppat_any -> T.Ppat_any
  | Ppat_var s -> T.Ppat_var s
  | Ppat_alias (p1, s) -> T.Ppat_alias (pattern p1, s)
  | Ppat_constant c -> T.Ppat_constant (constant c)
  | Ppat_tuple list -> T.Ppat_tuple (List.map pattern list)
  | Ppat_construct (l, o, bool) ->
      T.Ppat_construct (Longident.t l, option pattern o, bool)
  | Ppat_variant (l, o) ->
      T.Ppat_variant (label l, option pattern o)
  | Ppat_record (list, cl) ->
      T.Ppat_record (List.map (fun (l, p) ->
            (Longident.t l, pattern p)) list, closed_flag cl)
  | Ppat_array list -> T. Ppat_array (List.map pattern list)
  | Ppat_or (p1, p2) -> T.Ppat_or (pattern p1, pattern p2)
  | Ppat_constraint (p1, c) -> T.Ppat_constraint (pattern p1, core_type c)
  | Ppat_type l -> T.Ppat_type (Longident.t l)
  | Ppat_lazy p1 -> T.Ppat_lazy (pattern p1)
(*  After 3.12.0
    | Ppat_unpack _ -> raise (No_Such_Feature (this_version, "Unpack pattern")) *)

let ghmod d loc = { T.pmod_desc = d; pmod_loc = ghost_loc loc; }
let ghmty d loc = { T.pmty_desc = d; pmty_loc = ghost_loc loc; }
let ghexp d loc = { T.pexp_desc = d; pexp_loc = ghost_loc loc; }
let ghtyp d loc = { T.ptyp_desc = d; ptyp_loc = ghost_loc loc; }
let mkexp exp loc = { T.pexp_desc = exp; pexp_loc = loc }

let rec expression e =
  let pexp_loc = Location.t e.pexp_loc in
  let pexp_desc = expression_desc e.pexp_desc in
  { T.pexp_desc; pexp_loc  }

and expression_desc e =
  match e with
    Pexp_ident l -> T.Pexp_ident (Longident.t l)
  | Pexp_constant c -> T.Pexp_constant (constant c)
  | Pexp_let (r, list, e) ->
      T.Pexp_let (rec_flag r,
        List.map (fun (p,e) ->
            (pattern p, expression e)) list, expression e)
  | Pexp_function (l, o, list) ->
      T.Pexp_function (label l, option expression o,
        List.map (fun (p,e) -> T.(pattern p, expression e)) list)
  | Pexp_apply (e, list) ->
      T.Pexp_apply (expression e, List.map (fun (l, e) ->
            (label l,  expression e)) list)
  | Pexp_match (e, list) ->
      T.Pexp_match (expression e,
        List.map (fun (p,e) -> T.(pattern p, expression e)) list)
  | Pexp_try (e, list) ->
      T.Pexp_try (expression e,
        List.map (fun (p,e) -> T.(pattern p, expression e)) list)
  | Pexp_tuple list ->
      T.Pexp_tuple (List.map expression list)
  | Pexp_construct (l, o, bool) ->
      T.Pexp_construct (Longident.t l, option expression o, bool)
  | Pexp_variant (l, o) ->
      T.Pexp_variant (label l, option expression o)
  | Pexp_record (list, o) ->
      T.Pexp_record (List.map (fun (l, e) ->
            (Longident.t l, expression e)) list, option expression o)
  | Pexp_field (e, l) ->
      T.Pexp_field (expression e, Longident.t l)
  | Pexp_setfield (e1, l, e2) ->
      T.Pexp_setfield (expression e1, Longident.t l, expression e2)
  | Pexp_array list -> T.Pexp_array (List.map expression list)
  | Pexp_ifthenelse (e1, e2, o) ->
      T.Pexp_ifthenelse (expression e1, expression e2, option expression o)
  | Pexp_sequence (e1, e2) ->
      T.Pexp_sequence (expression e1, expression e2)
  | Pexp_while (e1, e2) ->
      T.Pexp_while (expression e1, expression e2)
  | Pexp_for (s, e1, e2, d, e3) ->
      T.Pexp_for (s, expression e1, expression e2,
        direction_flag d, expression e3)
(* After 3.12.0
  | Pexp_constraint (
      { pexp_desc = Pexp_pack m; pexp_loc = loc },
      Some { ptyp_desc = Ptyp_package p1 },
      op) ->
      let m = module_expr m in
      let p1 = package_type p1 in
      begin
        match op with
          None ->
            T.Pexp_pack (m, p1)
        | Some c ->
            let loc = Location.t loc in
            T.Pexp_constraint ( ghexp (T.Pexp_pack (m,p1)) loc,
              None, Some (core_type c))
      end *)
  | Pexp_constraint (e1, o1, o2) ->
      T.Pexp_constraint (expression e1, option core_type o1, option core_type o2)
  | Pexp_when (e1, e2) -> T.Pexp_when (expression e1, expression e2)
  | Pexp_send (e1, s) -> T.Pexp_send (expression e1, s)
  | Pexp_new l -> T.Pexp_new (Longident.t l)
  | Pexp_setinstvar (s, e) -> T.Pexp_setinstvar (s, expression e)
  | Pexp_override list ->
      T.Pexp_override (List.map (fun (s, e) -> T.(s, expression e)) list)
  | Pexp_letmodule (s, m, e) ->
      T.Pexp_letmodule (s, module_expr m, expression e)
  | Pexp_assert e -> T.Pexp_assert (expression e)
  | Pexp_assertfalse -> T.Pexp_assertfalse
  | Pexp_lazy e -> T.Pexp_lazy (expression e)
  | Pexp_poly (e, o) -> T.Pexp_poly (expression e, option core_type o)
  | Pexp_object cl -> T.Pexp_object (class_structure cl)
  | Pexp_newtype (s, e) -> T.Pexp_newtype (s, expression e)
  | Pexp_pack (me, pt) -> T.Pexp_pack (module_expr me, package_type pt)
(* After 3.12.0
 | Pexp_pack m ->
      raise (No_Such_Feature (this_version, "Pack without constraints")) *)
  | Pexp_open (l, e) -> T.Pexp_open (Longident.t l, expression e)

and value_description v =
  {
    T.pval_type = core_type v.pval_type;
    pval_prim = v.pval_prim;
  }


and type_declaration t =
  { T.ptype_params = t.ptype_params;
    ptype_cstrs = List.map (fun (c1, c2, l) ->
        (core_type c1, core_type c2, Location.t l)) t.ptype_cstrs;
    ptype_kind = type_kind t.ptype_kind;
    ptype_private = private_flag t.ptype_private;
    ptype_manifest =  option core_type t.ptype_manifest;
    ptype_variance = t.ptype_variance;
    ptype_loc =  Location.t t.ptype_loc;
  }

and type_kind t =
  match t with
    Ptype_abstract -> T.Ptype_abstract
  | Ptype_variant list ->
      T.Ptype_variant (List.map (fun (s, list, l) ->
            (s, List.map core_type list, Location.t l)) list)
  | Ptype_record list ->
      T.Ptype_record (List.map (fun (s,m,c,l) ->
            (s, mutable_flag m, core_type c, Location.t l)) list)

and exception_declaration list = List.map core_type list

and class_type c =
  { T.pcty_desc = class_type_desc c.pcty_desc;
    pcty_loc = Location.t c.pcty_loc; }

and class_type_desc c =
  match c with
    Pcty_constr (l, list) ->
      T.Pcty_constr (Longident.t l, List.map core_type list)
  | Pcty_signature c -> T.Pcty_signature (class_signature c)
  | Pcty_fun (l, ct, cl) ->
      T.Pcty_fun (label l, core_type ct, class_type cl)

and class_signature (c, list) =
  (core_type c, List.map class_type_field list)

and class_type_field c =
  match c with
    Pctf_inher c -> T.Pctf_inher (class_type c)
  | Pctf_val (s, m, v, c, l) ->
      T.Pctf_val (s, mutable_flag m, virtual_flag v, core_type c, Location.t l)
  | Pctf_virt (s, p, c, l) ->
      T.Pctf_virt (s, private_flag p, core_type c, Location.t l)
  | Pctf_meth (s, p, c, l) ->
      T.Pctf_meth (s, private_flag p, core_type c, Location.t l)
  | Pctf_cstr (c1, c2, l) ->
      T.Pctf_cstr (core_type c1, core_type c2, Location.t l)

and class_description c = class_infos class_type c

and class_type_declaration c = class_infos class_type c

and class_expr c =
  { T.pcl_desc = class_expr_desc c.pcl_desc;
    pcl_loc = Location.t c.pcl_loc; }

and class_expr_desc c =
  match c with
    Pcl_constr (l, list) ->
      T.Pcl_constr (Longident.t l, List.map core_type list)
  | Pcl_structure c ->
      T.Pcl_structure (class_structure c)
  | Pcl_fun (l, o, p, c) ->
      T.Pcl_fun (label l, option expression o, pattern p, class_expr c)
  | Pcl_apply (c, list) ->
      T.Pcl_apply (class_expr c, List.map (fun (l, e) ->
            (label l, expression e)) list)
  | Pcl_let (r, list, c) ->
      T.Pcl_let (rec_flag r,
        List.map (fun (p,e) -> T.(pattern p, expression e)) list, class_expr c)
  | Pcl_constraint (c1, c2) ->
      T.Pcl_constraint (class_expr c1, class_type c2)

and class_structure (p, list) =
  (pattern p, List.map class_field list)

and class_field c =
  match c with
    Pcf_inher (ovf, e, op) ->
      T.Pcf_inher (override_flag ovf, class_expr e, option string op)
  | Pcf_valvirt (s, mf, c, l) ->
      T.Pcf_valvirt (s, mutable_flag mf, core_type c, Location.t l)
  | Pcf_val (s, mf, ovf, e, loc) ->
      T.Pcf_val (s, mutable_flag mf, override_flag ovf, expression e,  Location.t loc)
  | Pcf_virt  (s, pf, c, loc) ->
      T.Pcf_virt (s, private_flag pf, core_type c, Location.t loc)
  | Pcf_meth (s, pf, ovf, e, loc) ->
      T.Pcf_meth (s , private_flag pf, override_flag ovf, expression e, Location.t loc)
  | Pcf_cstr (c1, c2, loc) ->
      T.Pcf_cstr (core_type c1, core_type c2, Location.t loc)
  | Pcf_let  (r, list, loc) ->
      T.Pcf_let ( rec_flag r, List.map (fun (pat, e)  ->
            (pattern pat, expression e)) list, Location.t loc)
  | Pcf_init e -> T.Pcf_init (expression e)

and class_declaration list = class_infos class_expr list

and module_type m =
  { T.pmty_desc = module_type_desc m.pmty_desc;
    pmty_loc = Location.t m.pmty_loc }

and module_type_desc m =
  match m with
    Pmty_ident l -> T.Pmty_ident (Longident.t l)
  | Pmty_signature sg -> T.Pmty_signature (signature sg)
  | Pmty_functor (s, mt1, mt2) ->
      T.Pmty_functor (s, module_type mt1, module_type mt2)
  | Pmty_with (mt, list) ->
      T.Pmty_with (module_type mt, List.map (fun (l, w) ->
            (Longident.t l, with_constraint w)) list)
  | Pmty_typeof me -> T.Pmty_typeof (module_expr me)

and signature list = List.map signature_item list

and signature_item sg =
  { T.psig_desc = signature_item_desc sg.psig_desc;
    psig_loc = Location.t sg.psig_loc }

and signature_item_desc si =
  match si with
    Psig_value (s, v) ->
      T.Psig_value (s, value_description v)
  | Psig_type list ->
      T.Psig_type (List.map (fun (s, t) -> T.(string s, type_declaration t)) list)
  | Psig_exception (s, e) ->
      T.Psig_exception (s,  exception_declaration e)
  | Psig_module (s, mt) ->
      T.Psig_module (s, module_type mt)
  | Psig_recmodule list ->
      T.Psig_recmodule (List.map (fun (s, mt) -> T.(s, module_type mt)) list)
  | Psig_modtype (s, md) ->
      T.Psig_modtype (s, modtype_declaration md)
  | Psig_open l -> T.Psig_open (Longident.t l)
  | Psig_include mt -> T.Psig_include (module_type mt)
  | Psig_class list ->
      T.Psig_class (List.map class_description list)
  | Psig_class_type list -> T.Psig_class_type (List.map class_type_declaration list)

and modtype_declaration m =
  match m with
    Pmodtype_abstract -> T.Pmodtype_abstract
  | Pmodtype_manifest mt -> T.Pmodtype_manifest (module_type mt)

and with_constraint w =
  match w with
    Pwith_type t -> T.Pwith_type (type_declaration t)
  | Pwith_module l -> T.Pwith_module (Longident.t l)
  | Pwith_typesubst t -> T.Pwith_typesubst (type_declaration t)
  | Pwith_modsubst l -> T. Pwith_modsubst (Longident.t l)

(* value expressions for the module language *)

and module_expr me =
  let pmod_loc = Location.t me.pmod_loc in
  let pmod_desc = module_expr_desc me.pmod_desc in
  { T.pmod_desc; pmod_loc }

and module_expr_desc me =
  match me with
    Pmod_ident l -> T.Pmod_ident (Longident.t l)
  | Pmod_structure (s) -> T.Pmod_structure (structure s)
  | Pmod_functor (s, mt, me) -> T.
      Pmod_functor (s, module_type mt, module_expr me)
  | Pmod_apply (me1, me2) -> T.Pmod_apply (module_expr me1, module_expr me2)
  | Pmod_constraint (me, mt) -> T.Pmod_constraint (module_expr me, module_type mt)
(* After 3.12.0
  | Pmod_unpack {
      pexp_desc = Pexp_constraint (e,
        Some { ptyp_desc = Ptyp_package p }, None);
    } ->
      let e = expression e in
      let p = package_type p in
      T.Pmod_unpack (e, p)
  | Pmod_unpack {
      pexp_desc = Pexp_constraint (e,
        Some { ptyp_desc = Ptyp_package p1 },
        Some { ptyp_desc = Ptyp_package (l, wlist) });
      pexp_loc = loc;
    } ->
      let loc = Location.t loc in
      let e = expression e in
      let p1 = package_type p1 in
      let l = Longident.t l in
      T.Pmod_constraint (
        ghmod (T.Pmod_unpack (e, p1)) loc,
        ghmty (T.Pmty_with (
            ghmty (T.Pmty_ident l) loc,
            List.map (fun (l, c) ->
                (V3120_types.Longident.Lident l,
                  let c = core_type c in
                  T.Pwith_type { T.ptype_params = [];
                    ptype_cstrs = [];
                    ptype_kind = T.Ptype_abstract;
                    ptype_private = V3120_types.Asttypes.Public;
                    ptype_manifest = Some c;
                    ptype_variance = [];
                    ptype_loc = ghost_loc loc;
                  })) wlist
          )) loc
      ) *)
(* After 3.12.0
  | Pmod_unpack e ->
      raise (No_Such_Feature (this_version, "Unpack without constraints"))
*)
  | Pmod_unpack (e, pt) -> T.Pmod_unpack (expression e, package_type pt)

and structure list = List.map structure_item list

and structure_item si =
  { T.pstr_desc = structure_item_desc si.pstr_desc;
    pstr_loc = Location.t  si.pstr_loc }

and structure_item_desc si =
  match si with
    Pstr_eval e -> T.Pstr_eval (expression e)
  | Pstr_value (r, list) -> T.Pstr_value (rec_flag r,
        List.map (fun (pat, e) -> T.(pattern pat, expression e)) list)
  | Pstr_primitive (s, v) -> T.Pstr_primitive (s, value_description v)
  | Pstr_type (list) -> T.Pstr_type (List.map
        (fun (s, t) -> T.(s, type_declaration t)) list)
  | Pstr_exception (s, e) -> T.Pstr_exception (s, exception_declaration e)
  | Pstr_exn_rebind (s, l) -> T.Pstr_exn_rebind (s, Longident.t l)
  | Pstr_module (s, me) -> T.Pstr_module (s, module_expr me)
  | Pstr_recmodule list -> T.Pstr_recmodule (List.map (
          fun (s, mt, me) -> T.(s, module_type mt, module_expr me)) list)
  | Pstr_modtype (s, mt) -> T.Pstr_modtype (s, module_type mt)
  | Pstr_open (l) -> T.Pstr_open (Longident.t l)
  | Pstr_class (list) -> T.Pstr_class (List.map class_declaration list)
  | Pstr_class_type (list) -> T.Pstr_class_type (List.map class_type_declaration list)
  | Pstr_include (me) -> T.Pstr_include (module_expr me)

    (*
(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir (a) -> T.Ptop_dir (string, directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string (a) -> T.Pdir_string (string
  | Pdir_int (a) -> T.Pdir_int (int
  | Pdir_ident (a) -> T.Pdir_ident (Longident.t
  | Pdir_bool (a) -> T.Pdir_bool (bool
*)

end

end




let output_intf_file version =
  if version = this_version then
    (V3120_types.ast_intf_magic_number,
      (fun oc ast ->
          output_value oc (AST.Parsetree.signature ast)))
  else
    V3112_output_ast.output_intf_file version

let output_impl_file version =
  if version = this_version then
    (V3120_types.ast_impl_magic_number,
      (fun oc ast ->
          output_value oc (AST.Parsetree.structure ast)))
  else
    V3112_output_ast.output_impl_file version
