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

(*


module CMI = struct

  open V3112_input_ast.AST

module Ident : sig

  val reset : unit -> unit
  val t : V3112_types.Ident.t -> Ident.t

end = struct

  module T = V3112_types.Ident
  let tbl = Hashtbl.create 13

  let reset () = Hashtbl.clear tbl

  let t id =
    let key =  (id.T.name, id.T.stamp) in
    try
      Hashtbl.find tbl key
  with Not_found ->
    let t = Ident.magic id.T.stamp id.T.name id.T.flags in
      Hashtbl.add tbl key t;
      t

end

module Path : sig

  val t : V3112_types.Path.t -> Path.t

end = struct

  module T = V3112_types.Path
  open Path

  let rec t p =
    match p with
	T.Pident id -> Pident (Ident.t id)
      | T.Pdot(p, s, pos) -> Pdot (t p, s, pos)
      | T.Papply (p1, p2) -> Papply (t p1, t p2)

end

module Primitive : sig

  val description : V3112_types.Primitive.description -> Primitive.description

end = struct

    module T = V3112_types.Primitive
    open Primitive

    let description p =
      { prim_name = p.T.prim_name;
        prim_arity = p.T.prim_arity;
        prim_alloc = p.T.prim_alloc;
        prim_native_name = p.T.prim_native_name;
        prim_native_float = p.T.prim_native_float;
      }

end

module Types : sig

    val reset : unit -> unit

    val signature_item :
      V3112_types.Types.signature_item -> Types.signature_item

    val record_representation :
      V3112_types.Types.record_representation ->
      Types.record_representation

  end = struct

    open Asttypes

    module T = V3112_types.Types
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
              desc = Tvar;
              level = ty.T.level;
              id = ty.T.id;
            } in
          list := (ty, t) :: !list;
          t.desc <- type_desc ty.T.desc;
          t

    and type_desc desc =
      match desc with
        T.Tvar -> Tvar
      | T.Tarrow (l, t1, t2, c) ->
          Tarrow (label l, type_expr t1, type_expr t2, commutable c)
      | T.Ttuple list -> Ttuple (List.map type_expr list)
      | T.Tconstr (p, list, ab) ->
          Tconstr (Path.t p, List.map type_expr list, ref (abbrev_memo !ab))
      | T.Tobject (t, { contents = None }) ->
          Tobject (type_expr t, ref None)
      | T.Tobject (t, { contents = Some (p, list) }) ->
          Tobject (type_expr t, ref (Some (Path.t p, List.map type_expr list)))
      | T.Tfield (s, f, t1, t2) ->
          Tfield (s, field_kind f, type_expr t1, type_expr t2)
      | T.Tnil -> Tnil
      | T.Tlink t -> Tlink (type_expr t)
      | T.Tsubst t ->  Tsubst (type_expr t)
      | T.Tvariant r -> Tvariant (row_desc r)
      | T.Tunivar -> Tunivar
      | T.Tpoly (t, list) -> Tpoly (type_expr t, List.map type_expr list)


    and abbrev_memo ab =
      match ab with
        T.Mnil -> Mnil
      | T.Mcons (pf, p, t1, t2, ab) ->
          Mcons(private_flag pf, Path.t p, type_expr t1, type_expr t2,
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
            (label l, row_field rf)) r.T.row_fields;
        row_more = type_expr r.T.row_more;
        row_bound = r.T.row_bound;
        row_closed = r.T.row_closed;
        row_fixed = r.T.row_fixed;
        row_name = (match r.T.row_name with
            None -> None
          | Some (p, list) ->
              Some (Path.t p, List.map type_expr list));
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
          Tsig_value (Ident.t id, value_description v)
      | T.Tsig_type (id, t, r) ->
          Tsig_type (Ident.t id, type_declaration t, rec_status r)
      | T.Tsig_exception (id, decl) ->
          Tsig_exception (Ident.t id, exception_declaration decl)
      | T.Tsig_module (id, m, r) ->
          Tsig_module (Ident.t id,  module_type m, rec_status r)
      | T.Tsig_modtype (id, m) ->
          Tsig_modtype (Ident.t id, modtype_declaration m)
      | T.Tsig_class (id, cl, r) ->
          Tsig_class (Ident.t id, class_declaration cl, rec_status r)
      | T.Tsig_cltype (id, cl, r) ->
          Tsig_cltype (Ident.t id, cltype_declaration cl, rec_status r)

    and value_description v =
      { val_type = type_expr v.T.val_type;
        val_kind = value_kind v.T.val_kind; }

    and value_kind v =
      match v with
        T.Val_reg -> Val_reg
      | T.Val_prim prim -> Val_prim (Primitive.description prim)
      | T.Val_ivar (m,s) ->
          Val_ivar (mutable_flag m, s)
      | T.Val_self (meths2, vars2, s, t) ->
          let meths = ref Meths.empty in
          let vars = ref Vars.empty in
          T.Meths.iter (fun s (id,t) ->
              meths := Meths.add s (Ident.t id, type_expr t) !meths) !meths2;
          T.Vars.iter (fun s (id, mf, vf, t) ->
              vars := Vars.add s (Ident.t id,
                mutable_flag mf, virtual_flag vf, type_expr t) !vars) !vars2;
          Val_self(meths, vars, s, type_expr t)
      | T.Val_anc (list, s) ->
          Val_anc (List.map (fun (s,id) -> (s, Ident.t id)) list, s)
          | T.Val_unbound -> Val_unbound

    and rec_status r = match r with
        T.Trec_next -> Trec_next
      | T.Trec_first -> Trec_first
      | T.Trec_not -> Trec_not

    and type_declaration decl =
      { type_params = List.map type_expr decl.T.type_params;
        type_arity = decl.T.type_arity;
        type_kind = type_kind decl.T.type_kind;
        type_private = private_flag decl.T.type_private;
        type_manifest = (match decl.T.type_manifest with
            None -> None | Some t -> Some (type_expr t));
        type_variance = decl.T.type_variance;
      }

    and type_kind t =
      match t with
        T.Type_abstract -> Type_abstract
      | T.Type_variant list -> Type_variant (List.map (fun (s, tlist) ->
                (s, List.map type_expr tlist)) list)
      | T.Type_record (list, rr) ->
          Type_record (List.map (fun (s, mf, t) ->
                (s, mutable_flag mf, type_expr t)) list,
            record_representation rr)

    and record_representation rr =
      match rr with
        T.Record_regular -> Record_regular
      | T.Record_float -> Record_float

    and exception_declaration list =
      List.map type_expr list

    and module_type decl =
      match decl with
        T.Tmty_ident p -> Tmty_ident (Path.t p)
      | T.Tmty_signature s ->
          Tmty_signature (signature s)
      | T.Tmty_functor (id, m1, m2) ->
          Tmty_functor (Ident.t id, module_type m1, module_type m2)

    and modtype_declaration decl =
      match decl with
        T.Tmodtype_abstract -> Tmodtype_abstract
      | T.Tmodtype_manifest mt ->
          Tmodtype_manifest (module_type mt)

    and class_declaration d =
      { cty_params = List.map type_expr d.T.cty_params;
        cty_type = class_type d.T.cty_type;
        cty_path = Path.t d.T.cty_path;
        cty_new = (match d.T.cty_new with
            None -> None | Some t -> Some (type_expr t));
        cty_variance = d.T.cty_variance;
      }

    and class_type cl =
      match cl with
        T.Tcty_constr (p, list, ct) ->
          Tcty_constr (Path.t p, List.map type_expr list, class_type ct)
      | T.Tcty_signature cl ->
          Tcty_signature (class_signature cl)
      | T.Tcty_fun (l, t, ct) ->
          Tcty_fun (label l, type_expr t, class_type ct)

    and class_signature c =
      let cty_vars = ref Vars.empty in
      T.Vars.iter (fun s (mf, vf, ty) ->
          cty_vars := Vars.add s (mutable_flag mf, virtual_flag vf,
            type_expr ty) !cty_vars
      ) c.T.cty_vars;
      let cty_concr = ref Concr.empty in
      T.Concr.iter (fun s ->
          cty_concr := Concr.add s !cty_concr) c.T.cty_concr;
      { cty_self = type_expr c.T.cty_self;
        cty_vars = !cty_vars;
        cty_concr = !cty_concr;
        cty_inher = List.map (fun (p, l) ->
            (Path.t p, List.map type_expr l)) c.T.cty_inher;
      }

  and cltype_declaration c =
      { clty_params = List.map type_expr c.T.clty_params;
        clty_type = class_type c.T.clty_type;
        clty_path = Path.t c.T.clty_path;
        clty_variance = c.T.clty_variance;
      }


end
;;

module Cmi_format : sig

    val pers_flags :
      V3112_types.Cmi_format.pers_flags -> Cmi_format.pers_flags
    ;;

  end = struct

    module T = V3112_types.Cmi_format
    open Cmi_format

    let pers_flags flag =
      match flag with
        T.Rectypes -> Rectypes
  end

end

let input_cmi_file ic magic =
  if magic <> V3112_types.cmi_magic_number then
    raise (Error (No_Such_Magic ("cmi", magic)));

  CMI.Ident.reset ();
  CMI.Types.reset ();

  let (cmi_name, cmi_sign) = (input_value ic : string *  V3112_types.Types.signature_item list) in
  let cmi_crcs = (input_value ic : (string * Digest.t) list) in
  let cmi_flags = (input_value ic : V3112_types.Cmi_format.pers_flags list) in

  let cmi_sign = List.map CMI.Types.signature_item cmi_sign in
  let cmi_flags = List.map CMI.Cmi_format.pers_flags cmi_flags in
  { Cmi_format.cmi_name ; cmi_sign; cmi_crcs; cmi_flags }

*)

