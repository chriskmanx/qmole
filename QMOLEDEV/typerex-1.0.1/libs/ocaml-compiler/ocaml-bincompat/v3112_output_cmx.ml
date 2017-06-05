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

module CMX = struct

    module Debuginfo = struct

        open Debuginfo
        module T =V3112_types.Debuginfo

        let  kind k = match k with
            Dinfo_call -> T.Dinfo_call
          | Dinfo_raise -> T.Dinfo_raise

        let t d = {
            T.dinfo_kind = kind d.dinfo_kind;
            dinfo_file = d.dinfo_file;
            dinfo_line = d.dinfo_line;
            dinfo_char_start = d.dinfo_char_start;
            dinfo_char_end = d.dinfo_char_end
          }

      end

    module Clambda : sig

        val value_approximation :
          Clambda.value_approximation ->
          V3112_types.Clambda.value_approximation

      end = struct

        open V3112_output_ast.AST
        open V3112_output_cmi.CMI
        open V3112_output_cmo.CMO

        open Asttypes

        open Clambda
        module T = V3112_types.Clambda


        let function_label string = string

        let rec ulambda u =
          match u with
            Uvar id -> T.Uvar (Ident.t id)
          | Uconst sc -> T.Uconst (Lambda.structured_constant sc)
          | Udirect_apply (l, list, d) ->
              T.Udirect_apply (function_label l, List.map ulambda list,
                Debuginfo.t d)
          | Ugeneric_apply (u, list, d) ->
              T.Ugeneric_apply (ulambda u, List.map ulambda list, Debuginfo.t d)
          | Uclosure (fs, env) ->
              T.Uclosure (List.map (fun (l, int, list, u) ->
                    (function_label l, int, List.map Ident.t list, ulambda u))
                fs,
                List.map ulambda env)
          | Uoffset (u, int) -> T.Uoffset (ulambda u, int)
          | Ulet (id, u1, u2) -> T.Ulet (Ident.t id, ulambda u1, ulambda u2)
          | Uletrec (list,u) ->
              T.Uletrec (List.map (fun (id, u) ->
                    (Ident.t id, ulambda u)) list, ulambda u)
          | Uprim (p, list, d) ->
              T.Uprim (Lambda.primitive p, List.map ulambda list, Debuginfo.t d)
          | Uswitch (u, sw) ->
              T.Uswitch (ulambda u, ulambda_switch sw)
          | Ustaticfail (int, list) ->
              T.Ustaticfail (int, List.map ulambda list)
          | Ucatch (int, list, u1, u2) ->
              T.Ucatch (int, List.map Ident.t list, ulambda u1, ulambda u2)
          | Utrywith (u1, id, u2) ->
              T.Utrywith (ulambda u1,Ident.t id, ulambda u2)
          | Uifthenelse (u1, u2, u3) ->
              T.Uifthenelse (ulambda u1, ulambda u2,  ulambda u3)
          | Usequence (u1, u2) ->
              T.Usequence (ulambda u1, ulambda u2)
          | Uwhile (u1, u2) ->
              T.Uwhile (ulambda u1, ulambda u2)
          | Ufor (id, u1, u2, d, u3) ->
              T.Ufor (Ident.t id, ulambda u1, ulambda u2,
                direction_flag d, ulambda u3)
          | Uassign (id, u1) ->
              T.Uassign (Ident.t id, ulambda u1)
          | Usend (kind, u1, u2, list, d) ->
              T.Usend (Lambda.meth_kind kind, ulambda u1, ulambda u2, List.map ulambda list, Debuginfo.t d)

        and ulambda_switch s =
          { T.us_index_consts = s.us_index_consts;
            us_actions_consts = Array.map ulambda s.us_actions_consts;
            us_index_blocks = s.us_index_blocks;
            us_actions_blocks = Array.map ulambda s.us_actions_blocks;
          }


        let function_description fd =
          { T.fun_label = function_label fd.fun_label;
            fun_arity = fd.fun_arity;
            fun_closed = fd.fun_closed;
            fun_inline = (match fd.fun_inline with
                None -> None
              | Some (list, ul) ->
                  Some (List.map Ident.t list, ulambda ul));
          }

        let rec value_approximation v =
          match v with
            Value_closure (fd, va) ->
              T.Value_closure (function_description fd, value_approximation va)
          | Value_tuple array ->
              T.Value_tuple (Array.map value_approximation array)
          | Value_unknown -> T.Value_unknown
          | Value_integer int -> T.Value_integer int
          | Value_constptr int -> T.Value_constptr int

      end

    module Cmx_format : sig

        val unit_infos :
          Cmx_format.unit_infos ->
          V3112_types.Cmx_format.unit_infos

        val library_infos :
          Cmx_format.library_infos ->
          V3112_types.Cmx_format.library_infos

      end = struct

        open Cmx_format
        module T = V3112_types.Cmx_format

        let unit_infos ui =
          { T.ui_name = ui.ui_name;
            ui_symbol = ui.ui_symbol;
            ui_defines = ui.ui_defines;
            ui_imports_cmi = ui.ui_imports_cmi;
            ui_imports_cmx = ui.ui_imports_cmx;
            ui_approx = Clambda.value_approximation ui.ui_approx;
            ui_curry_fun = ui.ui_curry_fun;
            ui_apply_fun = ui.ui_apply_fun;
            ui_send_fun = ui.ui_send_fun;
            ui_force_link = ui.ui_force_link;
          }

        let library_infos l =
          { T.
            lib_units = List.map (fun (ui, crc) ->
                unit_infos ui, crc) l.lib_units;
            lib_ccobjs = l.lib_ccobjs;
            lib_ccopts = l.lib_ccopts;
          }
      end

  end

let this_version = "3.11.2"

let output_cmx_file version =
  if version = this_version then
    (V3112_types.cmx_magic_number,
      (fun oc v ->
          let v = CMX.Cmx_format.unit_infos v in
          output_value oc (v : V3112_types.Cmx_format.unit_infos);
          ))
  else
    raise (Error (No_Such_Version version))

let output_cmxa_file version =
  if version = this_version then
    (V3112_types.cmxa_magic_number,
      (fun oc v ->
          let v = CMX.Cmx_format.library_infos v in
          output_value oc (v : V3112_types.Cmx_format.library_infos);
          ))
  else
    raise (Error (No_Such_Version version))

