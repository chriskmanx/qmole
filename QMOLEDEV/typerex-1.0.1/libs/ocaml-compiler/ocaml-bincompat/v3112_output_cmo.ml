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

module CMO = struct

  open V3112_output_ast.AST
  open V3112_output_cmi.CMI

    module Lambda : sig

        val structured_constant :
          Lambda.structured_constant ->
          V3112_types.Lambda.structured_constant

        val primitive :
          Lambda.primitive ->
          V3112_types.Lambda.primitive

        val meth_kind :
          Lambda.meth_kind ->
          V3112_types.Lambda.meth_kind

      end = struct

        open Asttypes

        open Lambda
        module T = V3112_types.Lambda

        let rec structured_constant sc =
          match sc with
            Const_base c -> T.Const_base (Asttypes.constant c)
          | Const_pointer int -> T.Const_pointer int
          | Const_block (int, list) ->
              T.Const_block (int, List.map structured_constant list)
          | Const_float_array slist -> T.Const_float_array slist
          | Const_immstring string -> T.Const_immstring string

        let rec primitive p =
          match p with
            Pidentity -> T.Pidentity
          | Pignore -> T.Pignore

          | Pgetglobal id -> T.Pgetglobal (Ident.t id)
          | Psetglobal id -> T. Psetglobal (Ident.t id)

          | Pmakeblock (int, m) -> T.Pmakeblock (int, mutable_flag m)
          | Pfield int -> T.Pfield int
          | Psetfield (int, bool) -> T.Psetfield (int, bool)
          | Pfloatfield int -> T.Pfloatfield int
          | Psetfloatfield int -> T.Psetfloatfield int
          | Pduprecord (r, int) ->
              T.Pduprecord (Types.record_representation r, int)

          | Plazyforce -> T.Plazyforce

          | Pccall d -> T.Pccall (Primitive.description d)

          | Praise -> T.Praise

          | Psequand -> T.Psequand
          | Psequor -> T.Psequor
          | Pnot -> T.Pnot

          | Pnegint -> T.Pnegint
          | Paddint -> T.Paddint
          | Psubint -> T.Psubint
          | Pmulint -> T.Pmulint
          | Pdivint -> T.Pdivint
          | Pmodint -> T.Pmodint
          | Pandint -> T.Pandint
          | Porint -> T.Porint
          | Pxorint -> T.Pxorint
          | Plslint -> T.Plslint
          | Plsrint -> T.Plsrint
          | Pasrint -> T.Pasrint
          | Pintcomp c -> T.Pintcomp (comparison c)
          | Poffsetint int -> T.Poffsetint int
          | Poffsetref int -> T.Poffsetref int

          | Pintoffloat -> T.Pintoffloat
          | Pfloatofint -> T.Pfloatofint

          | Pnegfloat -> T.Pnegfloat
          | Pabsfloat -> T.Pabsfloat

          | Paddfloat -> T.Paddfloat
          | Psubfloat -> T.Psubfloat
          | Pmulfloat -> T.Pmulfloat
          | Pdivfloat -> T.Pdivfloat

          | Pfloatcomp c -> T.Pfloatcomp (comparison c)

          | Pstringlength -> T.Pstringlength
          | Pstringrefu -> T.Pstringrefu
          | Pstringsetu -> T.Pstringsetu
          | Pstringrefs -> T.Pstringrefs
          | Pstringsets -> T.Pstringsets

          | Pmakearray k -> T.Pmakearray (array_kind k)
          | Parraylength k -> T.Parraylength (array_kind k)
          | Parrayrefu k -> T.Parrayrefu (array_kind k)
          | Parraysetu k -> T.Parraysetu (array_kind k)
          | Parrayrefs k -> T.Parrayrefs (array_kind k)
          | Parraysets k -> T.Parraysets (array_kind k)

          | Pisint -> T.Pisint

          | Pisout -> T.Pisout

          | Pbittest -> T.Pbittest

          | Pbintofint b -> T.Pbintofint (boxed_integer b)
          | Pintofbint b -> T.Pintofbint (boxed_integer b)
          | Pcvtbint (s, d) ->
              T.Pcvtbint (boxed_integer s, boxed_integer d)
          | Pnegbint b -> T.Pnegbint (boxed_integer b)
          | Paddbint b -> T.Paddbint (boxed_integer b)
          | Psubbint b -> T.Psubbint (boxed_integer b)
          | Pmulbint b -> T.Pmulbint (boxed_integer b)
          | Pdivbint b -> T.Pdivbint (boxed_integer b)
          | Pmodbint b -> T.Pmodbint (boxed_integer b)
          | Pandbint b -> T.Pandbint (boxed_integer b)
          | Porbint b -> T.Porbint (boxed_integer b)
          | Pxorbint b -> T.Pxorbint (boxed_integer b)
          | Plslbint b -> T.Plslbint (boxed_integer b)
          | Plsrbint b -> T.Plsrbint (boxed_integer b)
          | Pasrbint b -> T.Pasrbint (boxed_integer b)
          | Pbintcomp (b,c) -> T.Pbintcomp (boxed_integer b, comparison c)

          | Pbigarrayref (bool, int, kind, layout) ->
              T.Pbigarrayref (bool, int,
                bigarray_kind kind, bigarray_layout layout)
          | Pbigarrayset (bool, int, kind, layout) ->
              T.Pbigarrayset (bool, int,
                bigarray_kind kind, bigarray_layout layout)

        and comparison c =
          match c with
            Ceq -> T.Ceq
          | Cneq -> T.Cneq
          | Clt -> T.Clt
          | Cgt -> T.Cgt
          | Cle -> T.Cle
          | Cge -> T.Cge

        and array_kind k =
          match k with
            Pgenarray -> T.Pgenarray
          | Paddrarray -> T.Paddrarray
          | Pintarray -> T.Pintarray
          | Pfloatarray -> T.Pfloatarray

        and boxed_integer b =
          match b with
            Pnativeint -> T.Pnativeint
          | Pint32 -> T.Pint32
          | Pint64 -> T.Pint64

        and bigarray_kind kind =
          match kind with
            Pbigarray_unknown -> T.Pbigarray_unknown
          | Pbigarray_float32 -> T.Pbigarray_float32
          | Pbigarray_float64 -> T.Pbigarray_float64
          | Pbigarray_sint8 -> T.Pbigarray_sint8
          | Pbigarray_uint8 -> T.Pbigarray_uint8
          | Pbigarray_sint16 -> T.Pbigarray_sint16
          | Pbigarray_uint16 -> T.Pbigarray_uint16
          | Pbigarray_int32 -> T.Pbigarray_int32
          | Pbigarray_int64 -> T.Pbigarray_int64
          | Pbigarray_caml_int -> T.Pbigarray_caml_int
          | Pbigarray_native_int -> T.Pbigarray_native_int
          | Pbigarray_complex32 -> T.Pbigarray_complex32
          | Pbigarray_complex64 -> T.Pbigarray_complex64

        and bigarray_layout layout =
          match layout with
            Pbigarray_unknown_layout -> T.Pbigarray_unknown_layout
          | Pbigarray_c_layout -> T.Pbigarray_c_layout
          | Pbigarray_fortran_layout -> T.Pbigarray_fortran_layout


        let meth_kind k =
          match k with
            Self -> T.Self
          | Public -> T.Public
          | Cached -> T.Cached
      end


module Cmo_format : sig


    val compilation_unit :
      Cmo_format.compilation_unit ->
      V3112_types.Cmo_format.compilation_unit
    val library : Cmo_format.library -> V3112_types.Cmo_format.library

  end = struct

    open V3112_output_cmi.CMI

    open Cmo_format
    module T = V3112_types.Cmo_format

    let reloc_info r =
      match r with
        Reloc_literal sc ->
          T.Reloc_literal (Lambda.structured_constant sc)
      | Reloc_getglobal id ->
          T.Reloc_getglobal (Ident.t id)
      | Reloc_setglobal id -> T.Reloc_setglobal (Ident.t id)
      | Reloc_primitive s -> T.Reloc_primitive s


    let compilation_unit v =
      {
        T.cu_name = v.cu_name;
        cu_pos = v.cu_pos;
        cu_codesize = v.cu_codesize;
        cu_reloc = List.map (fun (r,i) ->
            (reloc_info r,i)) v.cu_reloc;
        cu_imports = v.cu_imports;
        cu_primitives = v.cu_primitives;
        cu_force_link = v.cu_force_link;
        cu_debug = v.cu_debug;
        cu_debugsize = v.cu_debugsize;
      }

    let library v =
      {
        T.lib_units = List.map compilation_unit v.lib_units;
        lib_custom = v.lib_custom;
        lib_ccobjs = v.lib_ccobjs;
        lib_ccopts = v.lib_ccopts;
        lib_dllibs = v.lib_dllibs;
      }


  end

end

let this_version = "3.11.2"

let output_cmo_file version =
  if version = this_version then
    V3112_types.cmo_magic_number,
    (fun oc cmo ->
        output_value oc (CMO.Cmo_format.compilation_unit cmo))
  else
    raise (Error (No_Such_Version version))

let output_cma_file version =
  if version = this_version then
    V3112_types.cma_magic_number,
    (fun oc cma ->
        output_value oc (CMO.Cmo_format.library cma))
  else
    raise (Error (No_Such_Version version))


