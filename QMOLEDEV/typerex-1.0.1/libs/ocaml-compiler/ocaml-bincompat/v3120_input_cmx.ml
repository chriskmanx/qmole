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
        module T =V3120_types.Debuginfo
        
        let  kind k = match k with
            T.Dinfo_call -> Dinfo_call
          | T.Dinfo_raise -> Dinfo_raise
              
        let t d = {
            dinfo_kind = kind d.T.dinfo_kind;
            dinfo_file = d.T.dinfo_file;
            dinfo_line = d.T.dinfo_line;
            dinfo_char_start = d.T.dinfo_char_start;
            dinfo_char_end = d.T.dinfo_char_end
          }
          
      end
    
    module Clambda : sig
        
        val value_approximation : 
          V3120_types.Clambda.value_approximation ->
          Clambda.value_approximation
        
      end = struct

        open V3120_input_ast.AST
        open V3120_input_cmi.CMI
        open V3120_input_cmo.CMO        

        open Asttypes
          
        open Clambda
        module T = V3120_types.Clambda

          
        let function_label string = string
          
        let rec ulambda u =
          match u with
            T.Uvar id -> Uvar (Ident.t id)
          | T.Uconst sc -> Uconst (Lambda.structured_constant sc)
          | T.Udirect_apply (l, list, d) ->
              Udirect_apply (function_label l, List.map ulambda list, 
                Debuginfo.t d)
          | T.Ugeneric_apply (u, list, d) ->
              Ugeneric_apply (ulambda u, List.map ulambda list, Debuginfo.t d)
          | T.Uclosure (fs, env) ->
              Uclosure (List.map (fun (l, int, list, u) ->
                    (function_label l, int, List.map Ident.t list, ulambda u))
                fs, 
                List.map ulambda env)
          | T.Uoffset (u, int) -> Uoffset (ulambda u, int)
          | T.Ulet (id, u1, u2) -> Ulet (Ident.t id, ulambda u1, ulambda u2)
          | T.Uletrec (list,u) ->
              Uletrec (List.map (fun (id, u) ->
                    (Ident.t id, ulambda u)) list, ulambda u)
          | T.Uprim (p, list, d) ->
              Uprim (Lambda.primitive p, List.map ulambda list, Debuginfo.t d)
          | T.Uswitch (u, sw) ->
              Uswitch (ulambda u, ulambda_switch sw)
          | T.Ustaticfail (int, list) ->
              Ustaticfail (int, List.map ulambda list)
          | T.Ucatch (int, list, u1, u2) ->
              Ucatch (int, List.map Ident.t list, ulambda u1, ulambda u2)
          | T.Utrywith (u1, id, u2) ->
              Utrywith (ulambda u1,Ident.t id, ulambda u2)
          | T.Uifthenelse (u1, u2, u3) ->
              Uifthenelse (ulambda u1, ulambda u2,  ulambda u3)
          | T.Usequence (u1, u2) ->
              Usequence (ulambda u1, ulambda u2)
          | T.Uwhile (u1, u2) ->
              Uwhile (ulambda u1, ulambda u2)
          | T.Ufor (id, u1, u2, d, u3) ->
              Ufor (Ident.t id, ulambda u1, ulambda u2,
                direction_flag d, ulambda u3)
          | T.Uassign (id, u1) ->
              Uassign (Ident.t id, ulambda u1)
          | T.Usend (kind, u1, u2, list, d) ->
              Usend (Lambda.meth_kind kind, ulambda u1, ulambda u2, List.map ulambda list, Debuginfo.t d)

        and ulambda_switch s =
          { us_index_consts = s.T.us_index_consts;
            us_actions_consts = Array.map ulambda s.T.us_actions_consts;
            us_index_blocks = s.T.us_index_blocks;
            us_actions_blocks = Array.map ulambda s.T.us_actions_blocks;
          }

              
        let function_description fd =
          { fun_label = function_label fd.T.fun_label;
            fun_arity = fd.T.fun_arity;
            fun_closed = fd.T.fun_closed;
            fun_inline = (match fd.T.fun_inline with
                None -> None
              | Some (list, ul) ->
                  Some (List.map Ident.t list, ulambda ul));
          }

        let rec value_approximation v = 
          match v with
            T.Value_closure (fd, va) ->
              Value_closure (function_description fd, value_approximation va)
          | T.Value_tuple array ->
              Value_tuple (Array.map value_approximation array)
          | T.Value_unknown -> Value_unknown
          | T.Value_integer int -> Value_integer int
          | T.Value_constptr int -> Value_constptr int

      end
    
    module Cmx_format : sig
        
        val unit_infos : 
          V3120_types.Cmx_format.unit_infos ->
          Cmx_format.unit_infos
        
        val library_infos : 
          V3120_types.Cmx_format.library_infos ->
          Cmx_format.library_infos
        
      end = struct

        open Cmx_format
        module T = V3120_types.Cmx_format
        
        let unit_infos ui = 
          { ui_name = ui.T.ui_name;
            ui_symbol = ui.T.ui_symbol;
            ui_defines = ui.T.ui_defines;
            ui_imports_cmi = ui.T.ui_imports_cmi;
            ui_imports_cmx = ui.T.ui_imports_cmx;
            ui_approx = Clambda.value_approximation ui.T.ui_approx;
            ui_curry_fun = ui.T.ui_curry_fun;
            ui_apply_fun = ui.T.ui_apply_fun;
            ui_send_fun = ui.T.ui_send_fun;
            ui_force_link = ui.T.ui_force_link;
          }

        let library_infos l = 
          { 
            lib_units = List.map (fun (ui, crc) ->
                unit_infos ui, crc) l.T.lib_units;
            lib_ccobjs = l.T.lib_ccobjs;
            lib_ccopts = l.T.lib_ccopts;
          }
      end
    
  end

      
let input_cmx_file ic magic =
  if magic = V3120_types.cmx_magic_number then
    let ui = (input_value ic : V3120_types.Cmx_format.unit_infos) in
    CMX.Cmx_format.unit_infos ui
  else
    V3112_input_cmx.input_cmx_file ic magic

let input_cmxa_file ic magic = 
  if magic = V3120_types.cmxa_magic_number then
    let infos = (input_value ic : V3120_types.Cmx_format.library_infos) in
    CMX.Cmx_format.library_infos infos
  else
    V3112_input_cmx.input_cmxa_file ic magic

