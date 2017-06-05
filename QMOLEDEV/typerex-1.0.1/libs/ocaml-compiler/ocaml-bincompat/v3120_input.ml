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

let ctx = ref Ident.no_ctx
let timestamp = ref 1000000

let ident_create s =
  (* Increase the stamp above all existing idents in the unit. *)
  let current_time = Ident.current_time () in
  Ident.set_current_time !timestamp;
  let id = Ident.create_with_ctx !ctx s in
  incr timestamp;
  Ident.currentstamp := current_time;
  id

open Bincompat

module ASTTYPES = struct
  let virtual_flag s = s
  let mutable_flag s = s
  let private_flag s = s
  let rec_flag s = s
  let override_flag s = s
  let string s = s
end


module PARSETREE : sig
  val signature :
    V3120_types.Parsetree.signature -> Parsetree.signature
  val structure :
    V3120_types.Parsetree.structure -> Parsetree.structure

end = struct

  open ASTTYPES
  open Asttypes

  let mknoloc = Location.mknoloc

  open Parsetree
  module T = V3120_types.Parsetree

  let rec core_type c =
    { ptyp_desc = core_type_desc c.T.ptyp_desc;
      ptyp_loc = c.T.ptyp_loc }

  and core_type_desc c =
    match c with
        T.Ptyp_any -> Ptyp_any
      | T.Ptyp_var string -> Ptyp_var string
      | T.Ptyp_arrow (l, c1, c2) ->
        Ptyp_arrow (l, core_type c1, core_type c2)
      | T.Ptyp_tuple list -> Ptyp_tuple (List.map core_type list)
      | T.Ptyp_constr (l, list) ->
        Ptyp_constr (mknoloc l, List.map core_type list)
      | T.Ptyp_object list -> Ptyp_object (List.map core_field_type list)
      | T.Ptyp_class (l, clist, llist) ->
        Ptyp_class (mknoloc l, List.map core_type clist, llist)
      | T.Ptyp_alias (c1, string) -> Ptyp_alias (core_type c1, string)
      | T.Ptyp_variant (list, bool, option) ->
        Ptyp_variant (List.map row_field list, bool,
		      match option with
			  None -> None
		        | Some list -> Some list)
      | T.Ptyp_poly (list, c1) -> Ptyp_poly (list, core_type c1)
      | T.Ptyp_package p -> Ptyp_package (package_type p)

  and package_type (l, list) =
    (mknoloc l,List.map (fun (s,c) -> (mknoloc s, core_type c)) list)

  and core_field_type c =
    { pfield_desc = core_field_desc c.T.pfield_desc;
      pfield_loc =  c.T.pfield_loc;
    }

  and core_field_desc c =
    match c with
	T.Pfield (s, c1) -> Pfield (s, core_type c1)
      | T.Pfield_var -> Pfield_var

  and row_field r =
    match r with
        T.Rtag (l, bool, list) ->
          Rtag (l, bool, List.map core_type list)
      | T.Rinherit c -> Rinherit (core_type c)

  let class_infos a c =
    { pci_virt =  c.T.pci_virt;
      pci_params = (let (list, l) = c.T.pci_params in
                    (List.map mknoloc list, l));
      pci_name = mknoloc c.T.pci_name;
      pci_expr = a c.T.pci_expr;
      pci_variance = c.T.pci_variance;
      pci_loc =  c.T.pci_loc;
    }


  let option f x =
    match x with
        None -> None
      | Some s -> Some (f s)

  let rec pattern p =
    { ppat_desc =  pattern_desc p.T.ppat_desc;
      ppat_loc = p.T.ppat_loc }

  and pattern_desc p =
    match p with
        T.Ppat_any -> Ppat_any
      | T.Ppat_var s -> Ppat_var (mknoloc s)
      | T.Ppat_alias (p1, s) -> Ppat_alias (pattern p1, mknoloc s)
      | T.Ppat_constant c -> Ppat_constant c
      | T.Ppat_tuple list -> Ppat_tuple (List.map pattern list)
      | T.Ppat_construct (l, o, bool) ->
        Ppat_construct (mknoloc l, option pattern o, bool)
      | T.Ppat_variant (l, o) ->
        Ppat_variant ( l, option pattern o)
      | T.Ppat_record (list, cl) ->
        Ppat_record (List.map (fun (l, p) ->
          (mknoloc l, pattern p)) list,  cl)
      | T.Ppat_array list ->  Ppat_array (List.map pattern list)
      | T.Ppat_or (p1, p2) -> Ppat_or (pattern p1, pattern p2)
      | T.Ppat_constraint (p1, c) -> Ppat_constraint (pattern p1, core_type c)
      | T.Ppat_type l -> Ppat_type (mknoloc l)
      | T.Ppat_lazy p1 -> Ppat_lazy (pattern p1)

(*
  let ghexp d loc = { pexp_desc = d; pexp_loc = ghost_loc loc; }
  let ghtyp d loc = { ptyp_desc = d; ptyp_loc = ghost_loc loc; }
  let mkexp exp loc = { pexp_desc = exp; pexp_loc = loc }
*)

  let rec expression e =
    let pexp_desc =
      match e.T.pexp_desc with
    (* after 3.12.0
       T.Pexp_pack (m, p) ->
       Pexp_constraint (
       ghexp (Pexp_pack (module_expr m)) pexp_loc,
       Some (ghtyp (Ptyp_package (package_type p)) pexp_loc), None)
    *)
        | _ ->   expression_desc e.T.pexp_desc
    in
    { pexp_desc = pexp_desc; pexp_loc = e.T.pexp_loc  }

  and expression_desc e =
    match e with
        T.Pexp_ident l -> Pexp_ident (mknoloc l)
      | T.Pexp_constant c -> Pexp_constant c
      | T.Pexp_let (r, list, e) ->
        Pexp_let (r,
                  List.map (fun (p,e) ->
                    (pattern p, expression e)) list, expression e)
      | T.Pexp_function (l, o, list) ->
        Pexp_function (l, option expression o,
                       List.map (fun (p,e) -> (pattern p, expression e)) list)
      | T.Pexp_apply (e, list) ->
        Pexp_apply (expression e, List.map (fun (l, e) ->
          (l,  expression e)) list)
      | T.Pexp_match (e, list) ->
        Pexp_match (expression e,
                    List.map (fun (p,e) -> (pattern p, expression e)) list)
      | T.Pexp_try (e, list) ->
        Pexp_try (expression e,
                  List.map (fun (p,e) -> (pattern p, expression e)) list)
      | T.Pexp_tuple list ->
        Pexp_tuple (List.map expression list)
      | T.Pexp_construct (l, o, bool) ->
        Pexp_construct (mknoloc l, option expression o, bool)
      | T.Pexp_variant (l, o) ->
        Pexp_variant (l, option expression o)
      | T.Pexp_record (list, o) ->
        Pexp_record (List.map (fun (l, e) ->
          (mknoloc l, expression e)) list, option expression o)
      | T.Pexp_field (e, l) ->
        Pexp_field (expression e, mknoloc l)
      | T.Pexp_setfield (e1, l, e2) ->
        Pexp_setfield (expression e1, mknoloc l, expression e2)
      | T.Pexp_array list -> Pexp_array (List.map expression list)
      | T.Pexp_ifthenelse (e1, e2, o) ->
        Pexp_ifthenelse (expression e1, expression e2, option expression o)
      | T.Pexp_sequence (e1, e2) ->
        Pexp_sequence (expression e1, expression e2)
      | T.Pexp_while (e1, e2) ->
        Pexp_while (expression e1, expression e2)
      | T.Pexp_for (s, e1, e2, d, e3) ->
        Pexp_for (mknoloc s, expression e1, expression e2,
                  d, expression e3)
      | T.Pexp_constraint (e1, o1, o2) ->
        Pexp_constraint (expression e1, option core_type o1, option core_type o2)
      | T.Pexp_when (e1, e2) -> Pexp_when (expression e1, expression e2)
      | T.Pexp_send (e1, s) -> Pexp_send (expression e1, s)
      | T.Pexp_new l -> Pexp_new (mknoloc l)
      | T.Pexp_setinstvar (s, e) -> Pexp_setinstvar (mknoloc s, expression e)
      | T.Pexp_override list ->
        Pexp_override (List.map (fun (s, e) -> (mknoloc s, expression e)) list)
      | T.Pexp_letmodule (s, m, e) ->
        Pexp_letmodule (mknoloc s, module_expr m, expression e)
      | T.Pexp_assert e -> Pexp_assert (expression e)
      | T.Pexp_assertfalse -> Pexp_assertfalse
      | T.Pexp_lazy e -> Pexp_lazy (expression e)
      | T.Pexp_poly (e, o) -> Pexp_poly (expression e, option core_type o)
      | T.Pexp_object cl -> Pexp_object (class_structure cl)
      | T.Pexp_newtype (s, e) -> Pexp_newtype (s, expression e)
      | T.Pexp_pack (m, p) ->
        Pexp_constraint (
          { pexp_desc = Pexp_pack (module_expr m);
            pexp_loc = m.T.pmod_loc},
          Some {
            ptyp_desc = Ptyp_package (package_type p);
            ptyp_loc = Location.none},
          None
        )
      | T.Pexp_open (l, e) -> Pexp_open (mknoloc l, expression e)

  and value_description v =
    {
      pval_type = core_type v.T.pval_type;
      pval_prim = v.T.pval_prim;
      pval_loc = Location.none;
    }


  and type_declaration t =
    { ptype_params = List.map (fun s -> Some (mknoloc s)) t.T.ptype_params;
      ptype_cstrs = List.map (fun (c1, c2, l) ->
        (core_type c1, core_type c2, l)) t.T.ptype_cstrs;
      ptype_kind = type_kind t.T.ptype_kind;
      ptype_private =  t.T.ptype_private;
      ptype_manifest =  option core_type t.T.ptype_manifest;
      ptype_variance = t.T.ptype_variance;
      ptype_loc =  t.T.ptype_loc;
    }

  and type_kind t =
    match t with
        T.Ptype_abstract -> Ptype_abstract
      | T.Ptype_variant list ->
        Ptype_variant (List.map (fun (s, list, l) ->
          (mknoloc s, List.map core_type list, None, l)) list)
      | T.Ptype_record list ->
        Ptype_record (List.map (fun (s,m,c,l) ->
          (mknoloc s, m, core_type c, l)) list)

  and exception_declaration list = List.map core_type list

  and class_type c =
    { pcty_desc = class_type_desc c.T.pcty_desc;
      pcty_loc = c.T.pcty_loc; }

  and class_type_desc c =
    match c with
        T.Pcty_constr (l, list) ->
          Pcty_constr (mknoloc l, List.map core_type list)
      | T.Pcty_signature c -> Pcty_signature (class_signature c)
      | T.Pcty_fun (l, ct, cl) ->
        Pcty_fun (l, core_type ct, class_type cl)

  and class_signature (c, list) =
    { pcsig_self = core_type c;
      pcsig_fields = List.map class_type_field list;
      pcsig_loc = Location.none;
    }

  and class_type_field c =
    let pctf_desc, pctf_loc =
      match c with
        T.Pctf_inher c -> Pctf_inher (class_type c), Location.none
        | T.Pctf_val (s, m, v, c, l) ->
          Pctf_val (s, mutable_flag m, virtual_flag v, core_type c), l
      | T.Pctf_virt (s, p, c, l) ->
        Pctf_virt (s, private_flag p, core_type c), l
      | T.Pctf_meth (s, p, c, l) ->
        Pctf_meth (s, private_flag p, core_type c), l
      | T.Pctf_cstr (c1, c2, l) ->
        Pctf_cstr (core_type c1, core_type c2), l
    in
    { pctf_desc = pctf_desc; pctf_loc = pctf_loc }

and class_description c = class_infos class_type c

and class_type_declaration c = class_infos class_type c

and class_expr c =
  { pcl_desc = class_expr_desc c.T.pcl_desc;
    pcl_loc = c.T.pcl_loc; }

and class_expr_desc c =
  match c with
    T.Pcl_constr (l, list) ->
      Pcl_constr (mknoloc l, List.map core_type list)
  | T.Pcl_structure c ->
      Pcl_structure (class_structure c)
  | T.Pcl_fun (l, o, p, c) ->
      Pcl_fun (l, option expression o, pattern p, class_expr c)
  | T.Pcl_apply (c, list) ->
      Pcl_apply (class_expr c, List.map (fun (l, e) ->
            (l, expression e)) list)
  | T.Pcl_let (r, list, c) ->
      Pcl_let (rec_flag r,
        List.map (fun (p,e) -> (pattern p, expression e)) list, class_expr c)
  | T.Pcl_constraint (c1, c2) ->
      Pcl_constraint (class_expr c1, class_type c2)

and class_structure (p, list) = {
  pcstr_pat = pattern p;
  pcstr_fields =  List.map class_field list;
}

and class_field c =
    let pcf_desc, pcf_loc =
  match c with
      T.Pcf_inher (ovf, e, op) ->
        Pcf_inher (override_flag ovf, class_expr e, option string op), Location.none
  | T.Pcf_valvirt (s, mf, c, l) ->
      Pcf_valvirt (mknoloc s, mutable_flag mf, core_type c), l
  | T.Pcf_val (s, mf, ovf, e, loc) ->
      Pcf_val (mknoloc s, mutable_flag mf, override_flag ovf, expression e), loc
  | T.Pcf_virt  (s, pf, c, loc) ->
      Pcf_virt (mknoloc s, private_flag pf, core_type c), loc
  | T.Pcf_meth (s, pf, ovf, e, loc) ->
      Pcf_meth (mknoloc s , private_flag pf, override_flag ovf, expression e), loc
  | T.Pcf_cstr (c1, c2, loc) ->
      Pcf_constr (core_type c1, core_type c2), loc
  | T.Pcf_let  (r, list, loc) ->
      Pcf_let ( rec_flag r, List.map (fun (pat, e)  ->
            (pattern pat, expression e)) list), loc
  | T.Pcf_init e -> Pcf_init (expression e), Location.none
  in
  { pcf_desc = pcf_desc; pcf_loc = pcf_loc }

and class_declaration list = class_infos class_expr list

and module_type m =
  { pmty_desc = module_type_desc m.T.pmty_desc;
    pmty_loc = m.T.pmty_loc }

and module_type_desc m =
  match m with
    T.Pmty_ident l -> Pmty_ident (mknoloc l)
  | T.Pmty_signature sg -> Pmty_signature (signature sg)
  | T.Pmty_functor (s, mt1, mt2) ->
      Pmty_functor (mknoloc s, module_type mt1, module_type mt2)
  | T.Pmty_with (mt, list) ->
      Pmty_with (module_type mt, List.map (fun (l, w) ->
            (mknoloc l, with_constraint w)) list)
  | T.Pmty_typeof me -> Pmty_typeof (module_expr me)

and signature list = List.map signature_item list

and signature_item sg =
  { psig_desc = signature_item_desc sg.T.psig_desc;
    psig_loc = sg.T.psig_loc }

and signature_item_desc si =
  match si with
    T.Psig_value (s, v) ->
      Psig_value (mknoloc s, value_description v)
  | T.Psig_type list ->
      Psig_type (List.map (fun (s, t) -> (mknoloc s, type_declaration t)) list)
  | T.Psig_exception (s, e) ->
      Psig_exception (mknoloc s,  exception_declaration e)
  | T.Psig_module (s, mt) ->
      Psig_module (mknoloc s, module_type mt)
  | T.Psig_recmodule list ->
      Psig_recmodule (List.map (fun (s, mt) -> (mknoloc s, module_type mt)) list)
  | T.Psig_modtype (s, md) ->
      Psig_modtype (mknoloc s, modtype_declaration md)
  | T.Psig_open l -> Psig_open (mknoloc l)
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
  | T.Pwith_module l -> Pwith_module (mknoloc l)
  | T.Pwith_typesubst t -> Pwith_typesubst (type_declaration t)
  | T.Pwith_modsubst l ->  Pwith_modsubst (mknoloc l)

(* value expressions for the module language *)

and module_expr me =
  let pmod_loc = me.T.pmod_loc in
  let pmod_desc = match me.T.pmod_desc with
(* After 3.12.0
    | T.Pmod_unpack (e, p) ->
        Pmod_unpack (ghexp
          (Pexp_constraint (expression e,
              Some (ghtyp (Ptyp_package (package_type p)) pmod_loc),
              None
              ))
          pmod_loc) *)
    | me ->  module_expr_desc me in
  { pmod_desc = pmod_desc; pmod_loc = pmod_loc }

and module_expr_desc me =
  match me with
    T.Pmod_ident l -> Pmod_ident (mknoloc l)
  | T.Pmod_structure (s) -> Pmod_structure (structure s)
  | T.Pmod_functor (s, mt, me) ->
      Pmod_functor (mknoloc s, module_type mt, module_expr me)
  | T.Pmod_apply (me1, me2) -> Pmod_apply (module_expr me1, module_expr me2)
  | T.Pmod_constraint (me, mt) -> Pmod_constraint (module_expr me, module_type mt)
  | T.Pmod_unpack (e, p) ->
    Pmod_unpack {
      pexp_loc = e.T.pexp_loc;
      pexp_desc =
        Pexp_constraint
          (expression e,
           Some {
             ptyp_loc = Location.none;
             ptyp_desc = (Ptyp_package (package_type p))
           },
           None)
    }

and structure list = List.map structure_item list

and structure_item si =
  { pstr_desc = structure_item_desc si.T.pstr_desc;
    pstr_loc = si.T.pstr_loc }

and structure_item_desc si =
  match si with
    T.Pstr_eval e -> Pstr_eval (expression e)
  | T.Pstr_value (r, list) -> Pstr_value (rec_flag r,
        List.map (fun (pat, e) -> (pattern pat, expression e)) list)
  | T.Pstr_primitive (s, v) -> Pstr_primitive (mknoloc s, value_description v)
  | T.Pstr_type (list) -> Pstr_type (List.map
        (fun (s, t) -> (mknoloc s, type_declaration t)) list)
  | T.Pstr_exception (s, e) -> Pstr_exception (mknoloc s, exception_declaration e)
  | T.Pstr_exn_rebind (s, l) -> Pstr_exn_rebind (mknoloc s, mknoloc l)
  | T.Pstr_module (s, me) -> Pstr_module (mknoloc s, module_expr me)
  | T.Pstr_recmodule list -> Pstr_recmodule (List.map (
          fun (s, mt, me) -> (mknoloc s, module_type mt, module_expr me)) list)
  | T.Pstr_modtype (s, mt) -> Pstr_modtype (mknoloc s, module_type mt)
  | T.Pstr_open (l) -> Pstr_open (mknoloc l)
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
  | T.Pdir_ident (a) -> Pdir_ident (mknoloc
  | T.Pdir_bool (a) -> Pdir_bool (bool
*)

end


module IDENT : sig

(*
  val reset : unit -> unit
*)
  val t : V3120_types.Ident.t -> Ident.t

end = struct

  module T = V3120_types.Ident
  open Ident

(*
  let tbl = Hashtbl.create 13
  let reset () = Hashtbl.clear tbl
*)

  let t id =
    let predef = 0 < id.T.stamp && id.T.stamp < 1000 in
    if predef then
      (* This works provided older versions only have a subset of the
         builtin idents of the current versions. *)
      List.assoc id.T.name Predef.builtin_idents
    else {
      stamp = id.T.stamp;
      name = id.T.name;
      flags = id.T.flags;
      ctx =
        if id.T.stamp = 0 then
          Ident.persistent_ctx
        else
          !ctx
    }

(*
  let key =  (id.T.name, id.T.stamp) in
  try
  Hashtbl.find tbl key
  with Not_found ->
  let t = Ident.magic id.T.stamp id.T.name id.T.flags in
  Hashtbl.add tbl key t;
  t
*)

end

module PATH : sig

  val t : V3120_types.Path.t -> Path.t

end = struct

  module T = V3120_types.Path
  open Path

  let rec t p =
    match p with
	T.Pident id -> Pident (IDENT.t id)
      | T.Pdot(p, s, pos) -> Pdot (t p, s, pos)
      | T.Papply (p1, p2) -> Papply (t p1, t p2)

end

(*


module CMI = struct

  open V3120_input_ast.AST

module Primitive : sig

  val description : V3120_types.Primitive.description -> Primitive.description

end = struct

    module T = V3120_types.Primitive
    open Primitive

    let description p =
      { prim_name = p.T.prim_name;
        prim_arity = p.T.prim_arity;
        prim_alloc = p.T.prim_alloc;
        prim_native_name = p.T.prim_native_name;
        prim_native_float = p.T.prim_native_float;
      }

end
*)

module TYPES : sig

    val reset : unit -> unit

    val signature :
      V3120_types.Types.signature_item list -> Types.signature_item list

    val record_representation :
      V3120_types.Types.record_representation ->
      Types.record_representation

  end = struct

    open Asttypes

    module T = V3120_types.Types
    open Types

    let tbl = Hashtbl.create 113
    let reset () =
      Hashtbl.clear tbl

    let label l = l

    let rec commutable c =
      match c with
        T.Cok -> Cok
      | T.Cunknown -> Cunknown
      | T.Clink r -> Clink (ref (commutable !r))


    let rec type_expr ty =
      let list =
        try
          Hashtbl.find tbl ty.T.id
        with Not_found ->
            let list = ref [] in
            Hashtbl.add tbl ty.T.id list;
            list
      in
      try
        List.assq ty !list
      with Not_found ->
          let t = {
              desc = Tvar None;
              level = ty.T.level;
              id = ty.T.id;
            } in
          list := (ty, t) :: !list;
          t.desc <- type_desc ty.T.desc;
          t

    and type_desc desc =
      match desc with
        T.Tvar -> Tvar None
      | T.Tarrow (l, t1, t2, c) ->
          Tarrow (l, type_expr t1, type_expr t2, commutable c)
      | T.Ttuple list -> Ttuple (List.map type_expr list)
      | T.Tconstr (p, list, ab) ->
          Tconstr (PATH.t p, List.map type_expr list, ref (abbrev_memo !ab))
      | T.Tobject (t, { contents = None }) ->
          Tobject (type_expr t, ref None)
      | T.Tobject (t, { contents = Some (p, list) }) ->
          Tobject (type_expr t, ref (Some (PATH.t p, List.map type_expr list)))
      | T.Tfield (s, f, t1, t2) ->
          Tfield (s, field_kind f, type_expr t1, type_expr t2)
      | T.Tnil -> Tnil
      | T.Tlink t -> Tlink (type_expr t)
      | T.Tsubst t ->  Tsubst (type_expr t)
      | T.Tvariant r -> Tvariant (row_desc r)
      | T.Tunivar -> Tunivar None
      | T.Tpoly (t, list) -> Tpoly (type_expr t, List.map type_expr list)
      | T.Tpackage (p, sl, tl) ->
          Tpackage (PATH.t p, sl, List.map type_expr tl)


    and abbrev_memo ab =
      match ab with
        T.Mnil -> Mnil
      | T.Mcons (pf, p, t1, t2, ab) ->
          Mcons(pf, PATH.t p, type_expr t1, type_expr t2,
            abbrev_memo ab)
      | T.Mlink ab -> Mlink (ref (abbrev_memo !ab))

    and field_kind fk =
      match fk with
        T.Fvar r -> begin
            match !r with
              None -> Fvar (ref None)
            | Some fk -> Fvar (ref (Some (field_kind fk)))
          end
      | T.Fpresent -> Fpresent
      | T.Fabsent -> Fabsent

    and row_desc r =
      { row_fields = List.map (fun (l, rf) ->
            (l, row_field rf)) r.T.row_fields;
        row_more = type_expr r.T.row_more;
        row_bound = r.T.row_bound;
        row_closed = r.T.row_closed;
        row_fixed = r.T.row_fixed;
        row_name = (match r.T.row_name with
            None -> None
          | Some (p, list) ->
              Some ( PATH.t p, List.map type_expr list));
      }

    and row_field rf =
      match rf with
        T.Rabsent -> Rabsent
      | T.Rpresent None -> Rpresent None
      | T.Rpresent (Some t) -> Rpresent (Some (type_expr t))
      | T.Reither (b1, list, b2, rbf) ->
          Reither (b1, List.map type_expr list, b2,
            (match !rbf with
                None -> ref None
              | Some rf -> ref (Some (row_field rf))))




    let rec signature list = List.map signature_item list

    and signature_item item =
      match item with
        T.Tsig_value (id, v) ->
          Sig_value (IDENT.t id, value_description v)
      | T.Tsig_type (id, t, r) ->
          Sig_type (IDENT.t id, type_declaration t, rec_status r)
      | T.Tsig_exception (id, decl) ->
          Sig_exception (IDENT.t id, exception_declaration decl)
      | T.Tsig_module (id, m, r) ->
          Sig_module (IDENT.t id,  module_type m, rec_status r)
      | T.Tsig_modtype (id, m) ->
          Sig_modtype (IDENT.t id, modtype_declaration m)
      | T.Tsig_class (id, cl, r) ->
          Sig_class (IDENT.t id, class_declaration cl, rec_status r)
      | T.Tsig_cltype (id, cl, r) ->
        Sig_class_type (IDENT.t id, cltype_declaration cl, rec_status r)

    and value_description v =
      { val_type = type_expr v.T.val_type;
        val_kind = value_kind v.T.val_kind; }

    and value_kind v =
      match v with
        T.Val_reg -> Val_reg
      | T.Val_prim prim -> Val_prim (prim)
      | T.Val_ivar (m,s) ->
          Val_ivar (m, s)
      | T.Val_self (meths2, vars2, s, t) ->
          let meths = ref Meths.empty in
          let vars = ref Vars.empty in
          T.Meths.iter (fun s (id,t) ->
              meths := Meths.add s (IDENT.t id, type_expr t) !meths) !meths2;
          T.Vars.iter (fun s (id, mf, vf, t) ->
              vars := Vars.add s (IDENT.t id,
                mf, vf, type_expr t) !vars) !vars2;
          Val_self(meths, vars, s, type_expr t)
      | T.Val_anc (list, s) ->
          Val_anc (List.map (fun (s,id) -> (s, IDENT.t id)) list, s)
          | T.Val_unbound -> Val_unbound

    and rec_status r = match r with
        T.Trec_next -> Trec_next
      | T.Trec_first -> Trec_first
      | T.Trec_not -> Trec_not

    and type_declaration decl =
      { type_params = List.map type_expr decl.T.type_params;
        type_arity = decl.T.type_arity;
        type_kind = type_kind decl.T.type_kind;
        type_private = decl.T.type_private;
        type_manifest = (match decl.T.type_manifest with
            None -> None | Some t -> Some (type_expr t));
        type_variance = decl.T.type_variance;
        type_newtype_level = None;
      }

    and type_kind t =
      match t with
        T.Type_abstract -> Type_abstract
      | T.Type_variant list -> Type_variant (List.map (fun (s, tlist) ->
                (ident_create s, List.map type_expr tlist, None)) list)
      | T.Type_record (list, rr) ->
          Type_record (List.map (fun (s, mf, t) ->
                (ident_create s, mf, type_expr t)) list,
            record_representation rr)

    and record_representation rr =
      match rr with
        T.Record_regular -> Record_regular
      | T.Record_float -> Record_float

    and exception_declaration list =
      List.map type_expr list

    and module_type decl =
      match decl with
        T.Tmty_ident p -> Mty_ident (PATH.t p)
      | T.Tmty_signature s ->
          Mty_signature (signature s)
      | T.Tmty_functor (id, m1, m2) ->
          Mty_functor (IDENT.t id, module_type m1, module_type m2)

    and modtype_declaration decl =
      match decl with
        T.Tmodtype_abstract -> Modtype_abstract
      | T.Tmodtype_manifest mt ->
          Modtype_manifest (module_type mt)

    and class_declaration d =
      { cty_params = List.map type_expr d.T.cty_params;
        cty_type = class_type d.T.cty_type;
        cty_path = PATH.t d.T.cty_path;
        cty_new = (match d.T.cty_new with
            None -> None | Some t -> Some (type_expr t));
        cty_variance = d.T.cty_variance;
      }

    and class_type cl =
      match cl with
        T.Tcty_constr (p, list, ct) ->
          Cty_constr (PATH.t p, List.map type_expr list, class_type ct)
      | T.Tcty_signature cl ->
          Cty_signature (class_signature cl)
      | T.Tcty_fun (l, t, ct) ->
          Cty_fun (l, type_expr t, class_type ct)

    and class_signature c =
      let cty_vars = ref Vars.empty in
      T.Vars.iter (fun s (mf, vf, ty) ->
          cty_vars := Vars.add s (mf, vf,
            type_expr ty) !cty_vars
      ) c.T.cty_vars;
      let cty_concr = ref Concr.empty in
      T.Concr.iter (fun s ->
          cty_concr := Concr.add s !cty_concr) c.T.cty_concr;
      { cty_self = type_expr c.T.cty_self;
        cty_vars = !cty_vars;
        cty_concr = !cty_concr;
        cty_inher = List.map (fun (p, l) ->
            (PATH.t p, List.map type_expr l)) c.T.cty_inher;
      }

  and cltype_declaration c =
      { clty_params = List.map type_expr c.T.clty_params;
        clty_type = class_type c.T.clty_type;
        clty_path = PATH.t c.T.clty_path;
        clty_variance = c.T.clty_variance;
      }


end
;;



(*
module Cmi_format : sig

    val pers_flags :
      V3120_types.Cmi_format.pers_flags -> Cmi_format.pers_flags
    ;;

  end = struct

    module T = V3120_types.Cmi_format
    open Cmi_format

    let pers_flags flag =
      match flag with
        T.Rectypes -> Rectypes
  end

end

let input_cmi_file ic magic =
  if magic <> V3120_types.cmi_magic_number then
    raise (Error (No_Such_Magic ("cmi", magic)));

  CMI.Ident.reset ();
  CMI.Types.reset ();

  let (cmi_name, cmi_sign) = (input_value ic : string *  V3120_types.Types.signature_item list) in
  let cmi_crcs = (input_value ic : (string * Digest.t) list) in
  let cmi_flags = (input_value ic : V3120_types.Cmi_format.pers_flags list) in

  let cmi_sign = List.map CMI.Types.signature_item cmi_sign in
  let cmi_flags = List.map CMI.Cmi_format.pers_flags cmi_flags in
  { Cmi_format.cmi_name ; cmi_sign; cmi_crcs; cmi_flags }

*)

let input_cmi ic =
  let (cmi_name, cmi_sign) =
    (input_value ic : string *  V3120_types.Types.signature_item list) in
  ctx := {
    Ident.modname = cmi_name;
    Ident.kind = `interface;
    Ident.source_digest = "" (* stub *)
  };
  TYPES.reset ();
  let cmi_sign = TYPES.signature cmi_sign in
  let cmi_crcs = (input_value ic : (string * Digest.t) list) in
  let cmi_flags = (input_value ic : V3120_types.Env.pers_flags list) in
  { Env.cmi_name = cmi_name ; cmi_sign = cmi_sign;
    cmi_crcs = cmi_crcs; cmi_flags = cmi_flags }

let input_ast_intf ic =
  let input_name = (input_value ic : string) in
  let intf = (input_value ic : V3120_types.Parsetree.signature) in
  let intf = PARSETREE.signature intf in
  (input_name, intf)

let input_ast_impl ic =
  let input_name = (input_value ic : string) in
  let impl = (input_value ic : V3120_types.Parsetree.structure) in
  let impl = PARSETREE.structure impl in
  (input_name, impl)
