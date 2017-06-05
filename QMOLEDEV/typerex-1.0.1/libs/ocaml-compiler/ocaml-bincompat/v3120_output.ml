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

open OCamlTypes
open Bincompat

let this_version = "3.12.0"

(*

module CMI = struct

    open V3120_output_ast.AST

      module Ident : sig

    val reset : unit -> unit
    val t : Ident.t -> V3120_types.Ident.t

end = struct

    open Ident

  module T = V3120_types.Ident
  let tbl = Hashtbl.create 13

  let reset () = Hashtbl.clear tbl

  let t id =
    let key =  (Ident.name id, Ident.stamp id) in
    try
      Hashtbl.find tbl key
  with Not_found ->
              let t = {
                  T.stamp = Ident.stamp id;
                  T.name = Ident.name id;
                  T.flags = Ident.flags id
                } in
      Hashtbl.add tbl key t;
      t

end
*)

module ASTTYPES = V3112_output.ASTTYPES
module PATH = V3112_output.PATH
module IDENT = V3112_output.IDENT
module PRIMITIVE = V3112_output.PRIMITIVE

(*
module Primitive : sig

  val description : PRIMITIVE.description -> V3120_types.PRIMITIVE.description

end = struct

    module T = V3120_types.Primitive
    open Primitive

    let description p =
      { T.prim_name = p.prim_name;
        prim_arity = p.prim_arity;
        prim_alloc = p.prim_alloc;
        prim_native_name = p.prim_native_name;
        prim_native_float = p.prim_native_float;
      }

end
*)

module TYPES : sig

    val reset : unit -> unit

    val signature :
      Types.signature -> V3120_types.Types.signature

    val record_representation :
      Types.record_representation ->
      V3120_types.Types.record_representation

  end = struct

    open Asttypes
    open ASTTYPES

    module T = V3120_types.Types
    open Types

    let tbl = Hashtbl.create 113
    let reset () =
      Hashtbl.clear tbl

    let label l = l

    let rec commutable c =
      match c with
        Cok -> T.Cok
      | Cunknown -> T.Cunknown
      | Clink r -> T.Clink (ref (commutable !r))

    let rec type_expr ty =
      let list =
        try
          Hashtbl.find tbl ty.id
        with Not_found ->
            let list = ref [] in
            Hashtbl.add tbl ty.id list;
            list
      in
      try
        List.assq ty !list
      with Not_found ->
          let t = {
              T.desc = T.Tvar;
              level = ty.level;
              id = ty.id;
            } in
          list := (ty, t) :: !list;
          t.T.desc <- type_desc ty.desc;
          t

    and type_desc desc =
      match desc with
        Tvar _ -> T.Tvar
      | Tarrow (l, t1, t2, c) ->
          T.Tarrow (label l, type_expr t1, type_expr t2, commutable c)
      | Ttuple list -> T.Ttuple (List.map type_expr list)
      | Tconstr (p, list, ab) ->
          T.Tconstr (PATH.t p, List.map type_expr list, ref (abbrev_memo !ab))
      | Tobject (t, { contents = None }) ->
          T.Tobject (type_expr t, ref None)
      | Tobject (t, { contents = Some (p, list) }) ->
          T.Tobject (type_expr t, ref (Some (PATH.t p, List.map type_expr list)))
      | Tfield (s, f, t1, t2) ->
          T.Tfield (s, field_kind f, type_expr t1, type_expr t2)
      | Tnil -> T.Tnil
      | Tlink t -> T.Tlink (type_expr t)
      | Tsubst t -> T. Tsubst (type_expr t)
      | Tvariant r -> T.Tvariant (row_desc r)
      | Tunivar _ -> T.Tunivar
      | Tpoly (t, list) -> T.Tpoly (type_expr t, List.map type_expr list)
      | Tpackage (p, sl, tl) ->
          T.Tpackage (PATH.t p, sl, List.map type_expr tl)

    and abbrev_memo ab =
      match ab with
        Mnil -> T.Mnil
      | Mcons (pf, p, t1, t2, ab) ->
          T.Mcons(private_flag pf, PATH.t p, type_expr t1, type_expr t2,
            abbrev_memo ab)
      | Mlink ab -> T.Mlink (ref (abbrev_memo !ab))

    and field_kind fk =
      match fk with
        Fvar r -> begin
            match !r with
              None -> T.Fvar (ref None)
            | Some fk -> T.Fvar (ref (Some (field_kind fk)))
          end
      | Fpresent -> T.Fpresent
      | Fabsent -> T.Fabsent

    and row_desc r =
      { T.row_fields = List.map (fun (l, rf) ->
            (label l, row_field rf)) r.row_fields;
        row_more = type_expr r.row_more;
        row_bound = r.row_bound;
        row_closed = r.row_closed;
        row_fixed = r.row_fixed;
        row_name = (match r.row_name with
            None -> None
          | Some (p, list) ->
              Some (PATH.t p, List.map type_expr list));
      }

    and row_field rf =
      match rf with
        Rabsent -> T.Rabsent
      | Rpresent None -> T.Rpresent None
      | Rpresent (Some t) -> T.Rpresent (Some (type_expr t))
      | Reither (b1, list, b2, rbf) ->
          T.Reither (b1, List.map type_expr list, b2,
            (match !rbf with
                None -> ref None
              | Some rf -> ref (Some (row_field rf))))




    let rec signature list = List.map signature_item list

    and signature_item item =
      match item with
        Tsig_value (id, v) ->
          T.Tsig_value (IDENT.t id, value_description v)
      | Tsig_type (id, t, r) ->
          T.Tsig_type (IDENT.t id, type_declaration t, rec_status r)
      | Tsig_exception (id, decl) ->
          T.Tsig_exception (IDENT.t id, exception_declaration decl)
      | Tsig_module (id, m, r) ->
          T.Tsig_module (IDENT.t id,  module_type m, rec_status r)
      | Tsig_modtype (id, m) ->
          T.Tsig_modtype (IDENT.t id, modtype_declaration m)
      | Tsig_class (id, cl, r) ->
          T.Tsig_class (IDENT.t id, class_declaration cl, rec_status r)
      | Tsig_cltype (id, cl, r) ->
          T.Tsig_cltype (IDENT.t id, cltype_declaration cl, rec_status r)

    and value_description v =
      { T.val_type = type_expr v.val_type;
        val_kind = value_kind v.val_kind; }

    and value_kind v =
      match v with
        Val_reg -> T.Val_reg
      | Val_prim prim -> T.Val_prim (PRIMITIVE.description prim)
      | Val_ivar (m,s) ->
          T.Val_ivar (mutable_flag m, s)
      | Val_self (meths2, vars2, s, t) ->
          let meths = ref T.Meths.empty in
          let vars = ref T.Vars.empty in
          Meths.iter (fun s (id,t) ->
              meths := T.Meths.add s (IDENT.t id, type_expr t) !meths) !meths2;
          Vars.iter (fun s (id, mf, vf, t) ->
              vars := T.Vars.add s (IDENT.t id,
                mutable_flag mf, virtual_flag vf, type_expr t) !vars) !vars2;
          T.Val_self(meths, vars, s, type_expr t)
      | Val_anc (list, s) ->
          T.Val_anc (List.map (fun (s,id) -> T.(s, IDENT.t id)) list, s)
          | Val_unbound -> T.Val_unbound

    and rec_status r = match r with
        Trec_next -> T.Trec_next
      | Trec_first -> T.Trec_first
      | Trec_not -> T.Trec_not

    and type_declaration decl =
      { T.type_params = List.map type_expr decl.type_params;
        type_arity = decl.type_arity;
        type_kind = type_kind decl.type_kind;
        type_private = private_flag decl.type_private;
        type_manifest = (match decl.type_manifest with
            None -> None | Some t -> Some (type_expr t));
        type_variance = decl.type_variance;
      }

    and type_kind t =
      match t with
        Type_abstract -> T.Type_abstract
      | Type_variant list -> T.Type_variant (List.map (fun (s, tlist, gadt) ->
        match gadt with
            None -> (s, List.map type_expr tlist)
          | Some gadt ->
	    raise (Error (No_Such_Feature (this_version, "gadt")))
      ) list)
      | Type_record (list, rr) ->
          T.Type_record (List.map (fun (s, mf, t) ->
                (s, mutable_flag mf, type_expr t)) list,
            record_representation rr)

    and record_representation rr =
      match rr with
        Record_regular -> T.Record_regular
      | Record_float -> T.Record_float

    and exception_declaration list =
      List.map type_expr list

    and module_type decl =
      match decl with
        Tmty_ident p -> T.Tmty_ident (PATH.t p)
      | Tmty_signature s ->
          T.Tmty_signature (signature s)
      | Tmty_functor (id, m1, m2) ->
          T.Tmty_functor (IDENT.t id, module_type m1, module_type m2)

    and modtype_declaration decl =
      match decl with
        Tmodtype_abstract -> T.Tmodtype_abstract
      | Tmodtype_manifest mt ->
          T.Tmodtype_manifest (module_type mt)

    and class_declaration d =
      { T.cty_params = List.map type_expr d.cty_params;
        cty_type = class_type d.cty_type;
        cty_path = PATH.t d.cty_path;
        cty_new = (match d.cty_new with
            None -> None | Some t -> Some (type_expr t));
        cty_variance = d.cty_variance;
      }

    and class_type cl =
      match cl with
        Tcty_constr (p, list, ct) ->
          T.Tcty_constr (PATH.t p, List.map type_expr list, class_type ct)
      | Tcty_signature cl ->
          T.Tcty_signature (class_signature cl)
      | Tcty_fun (l, t, ct) ->
          T.Tcty_fun (label l, type_expr t, class_type ct)

    and class_signature c =
      let cty_vars = ref T.Vars.empty in
      Vars.iter (fun s (mf, vf, ty) ->
          cty_vars := T.Vars.add s (mutable_flag mf, virtual_flag vf,
            type_expr ty) !cty_vars
      ) c.cty_vars;
      let cty_concr = ref T.Concr.empty in
      Concr.iter (fun s ->
          cty_concr := T.Concr.add s !cty_concr) c.cty_concr;
      { T.cty_self = type_expr c.cty_self;
        cty_vars = !cty_vars;
        cty_concr = !cty_concr;
        cty_inher = List.map (fun (p, l) ->
            (PATH.t p, List.map type_expr l)) c.cty_inher;
      }

  and cltype_declaration c =
      { T.clty_params = List.map type_expr c.clty_params;
        clty_type = class_type c.clty_type;
        clty_path = PATH.t c.clty_path;
        clty_variance = c.clty_variance;
      }


end

(*
;;

module Cmi_format : sig

    val pers_flags :
      Cmi_format.pers_flags -> V3120_types.Cmi_format.pers_flags
    ;;

  end = struct

    module T = V3120_types.Cmi_format
    open Cmi_format

    let pers_flags flag =
      match flag with
        Rectypes -> T.Rectypes
  end

end

open Cmi_format

(*
let output_cmi_file version =
  if version <> this_version then
    V3112_output_cmi.output_cmi_file version
  else
  (V3120_types.cmi_magic_number,
    fun oc cmi ->
      CMI.Ident.reset ();
      CMI.Types.reset ();

        output_value oc (cmi.cmi_name, cmi.cmi_sign);
            flush oc;
    let crc = Digest.file filename in
    let crcs = (modname, crc) :: imports in

      output_value oc cmi.cmi_crcs;
      output_value oc
  )
    *)

let output_cmi_file oc filename version cmi =
  if version <> this_version then
    V3112_output_cmi.output_cmi_file oc filename version cmi
  else begin
      output_string oc V3120_types.cmi_magic_number;
      output_value oc (cmi.cmi_name, CMI.Types.signature cmi.cmi_sign);
      flush oc;
      let crc = Digest.file filename in
      cmi.cmi_crcs <- (cmi.cmi_name, crc) :: cmi.cmi_crcs;
      output_value oc cmi.cmi_crcs;
      output_value oc (List.map CMI.Cmi_format.pers_flags cmi.cmi_flags);
      crc
    end

*)


let signature sg =
  TYPES.reset ();
  TYPES.signature sg
