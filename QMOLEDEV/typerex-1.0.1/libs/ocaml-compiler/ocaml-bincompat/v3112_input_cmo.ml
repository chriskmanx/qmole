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


  open V3112_input_ast.AST
  open V3112_input_cmi.CMI

    module Lambda : sig

        val structured_constant :
          V3112_types.Lambda.structured_constant ->
          Lambda.structured_constant

        val primitive :
          V3112_types.Lambda.primitive ->
          Lambda.primitive

        val meth_kind :
          V3112_types.Lambda.meth_kind ->
          Lambda.meth_kind

      end = struct

        open Asttypes

        open Lambda
        module T = V3112_types.Lambda

        let rec structured_constant sc =
          match sc with
            T.Const_base c -> Const_base (Asttypes.constant c)
          | T.Const_pointer int -> Const_pointer int
          | T.Const_block (int, list) ->
              Const_block (int, List.map structured_constant list)
          | T.Const_float_array slist -> Const_float_array slist
          | T.Const_immstring string -> Const_immstring string

        let rec primitive p =
          match p with
            T.Pidentity -> Pidentity
          | T.Pignore -> Pignore

          | T.Pgetglobal id -> Pgetglobal (Ident.t id)
          | T.Psetglobal id ->  Psetglobal (Ident.t id)

          | T.Pmakeblock (int, m) -> Pmakeblock (int, mutable_flag m)
          | T.Pfield int -> Pfield int
          | T.Psetfield (int, bool) -> Psetfield (int, bool)
          | T.Pfloatfield int -> Pfloatfield int
          | T.Psetfloatfield int -> Psetfloatfield int
          | T.Pduprecord (r, int) ->
              Pduprecord (Types.record_representation r, int)

          | T.Plazyforce -> Plazyforce

          | T.Pccall d -> Pccall (Primitive.description d)

          | T.Praise -> Praise

          | T.Psequand -> Psequand
          | T.Psequor -> Psequor
          | T.Pnot -> Pnot

          | T.Pnegint -> Pnegint
          | T.Paddint -> Paddint
          | T.Psubint -> Psubint
          | T.Pmulint -> Pmulint
          | T.Pdivint -> Pdivint
          | T.Pmodint -> Pmodint
          | T.Pandint -> Pandint
          | T.Porint -> Porint
          | T.Pxorint -> Pxorint
          | T.Plslint -> Plslint
          | T.Plsrint -> Plsrint
          | T.Pasrint -> Pasrint
          | T.Pintcomp c -> Pintcomp (comparison c)
          | T.Poffsetint int -> Poffsetint int
          | T.Poffsetref int -> Poffsetref int

          | T.Pintoffloat -> Pintoffloat
          | T.Pfloatofint -> Pfloatofint

          | T.Pnegfloat -> Pnegfloat
          | T.Pabsfloat -> Pabsfloat

          | T.Paddfloat -> Paddfloat
          | T.Psubfloat -> Psubfloat
          | T.Pmulfloat -> Pmulfloat
          | T.Pdivfloat -> Pdivfloat

          | T.Pfloatcomp c -> Pfloatcomp (comparison c)

          | T.Pstringlength -> Pstringlength
          | T.Pstringrefu -> Pstringrefu
          | T.Pstringsetu -> Pstringsetu
          | T.Pstringrefs -> Pstringrefs
          | T.Pstringsets -> Pstringsets

          | T.Pmakearray k -> Pmakearray (array_kind k)
          | T.Parraylength k -> Parraylength (array_kind k)
          | T.Parrayrefu k -> Parrayrefu (array_kind k)
          | T.Parraysetu k -> Parraysetu (array_kind k)
          | T.Parrayrefs k -> Parrayrefs (array_kind k)
          | T.Parraysets k -> Parraysets (array_kind k)

          | T.Pisint -> Pisint

          | T.Pisout -> Pisout

          | T.Pbittest -> Pbittest

          | T.Pbintofint b -> Pbintofint (boxed_integer b)
          | T.Pintofbint b -> Pintofbint (boxed_integer b)
          | T.Pcvtbint (s, d) ->
              Pcvtbint (boxed_integer s, boxed_integer d)
          | T.Pnegbint b -> Pnegbint (boxed_integer b)
          | T.Paddbint b -> Paddbint (boxed_integer b)
          | T.Psubbint b -> Psubbint (boxed_integer b)
          | T.Pmulbint b -> Pmulbint (boxed_integer b)
          | T.Pdivbint b -> Pdivbint (boxed_integer b)
          | T.Pmodbint b -> Pmodbint (boxed_integer b)
          | T.Pandbint b -> Pandbint (boxed_integer b)
          | T.Porbint b -> Porbint (boxed_integer b)
          | T.Pxorbint b -> Pxorbint (boxed_integer b)
          | T.Plslbint b -> Plslbint (boxed_integer b)
          | T.Plsrbint b -> Plsrbint (boxed_integer b)
          | T.Pasrbint b -> Pasrbint (boxed_integer b)
          | T.Pbintcomp (b,c) -> Pbintcomp (boxed_integer b, comparison c)

          | T.Pbigarrayref (bool, int, kind, layout) ->
              Pbigarrayref (bool, int,
                bigarray_kind kind, bigarray_layout layout)
          | T.Pbigarrayset (bool, int, kind, layout) ->
              Pbigarrayset (bool, int,
                bigarray_kind kind, bigarray_layout layout)

        and comparison c =
          match c with
            T.Ceq -> Ceq
          | T.Cneq -> Cneq
          | T.Clt -> Clt
          | T.Cgt -> Cgt
          | T.Cle -> Cle
          | T.Cge -> Cge

        and array_kind k =
          match k with
            T.Pgenarray -> Pgenarray
          | T.Paddrarray -> Paddrarray
          | T.Pintarray -> Pintarray
          | T.Pfloatarray -> Pfloatarray

        and boxed_integer b =
          match b with
            T.Pnativeint -> Pnativeint
          | T.Pint32 -> Pint32
          | T.Pint64 -> Pint64

        and bigarray_kind kind =
          match kind with
            T.Pbigarray_unknown -> Pbigarray_unknown
          | T.Pbigarray_float32 -> Pbigarray_float32
          | T.Pbigarray_float64 -> Pbigarray_float64
          | T.Pbigarray_sint8 -> Pbigarray_sint8
          | T.Pbigarray_uint8 -> Pbigarray_uint8
          | T.Pbigarray_sint16 -> Pbigarray_sint16
          | T.Pbigarray_uint16 -> Pbigarray_uint16
          | T.Pbigarray_int32 -> Pbigarray_int32
          | T.Pbigarray_int64 -> Pbigarray_int64
          | T.Pbigarray_caml_int -> Pbigarray_caml_int
          | T.Pbigarray_native_int -> Pbigarray_native_int
          | T.Pbigarray_complex32 -> Pbigarray_complex32
          | T.Pbigarray_complex64 -> Pbigarray_complex64

        and bigarray_layout layout =
          match layout with
            T.Pbigarray_unknown_layout -> Pbigarray_unknown_layout
          | T.Pbigarray_c_layout -> Pbigarray_c_layout
          | T.Pbigarray_fortran_layout -> Pbigarray_fortran_layout


        let meth_kind k =
          match k with
            T.Self -> Self
          | T.Public -> Public
          | T.Cached -> Cached
      end


module Cmo_format : sig


    val compilation_unit :
      V3112_types.Cmo_format.compilation_unit ->
      Cmo_format.compilation_unit
    val library : V3112_types.Cmo_format.library -> Cmo_format.library

  end = struct

    open V3112_input_cmi.CMI

    open Cmo_format
    module T = V3112_types.Cmo_format

    let reloc_info r =
      match r with
        T.Reloc_literal sc ->
          Reloc_literal (Lambda.structured_constant sc)
      | T.Reloc_getglobal id ->
          Reloc_getglobal (Ident.t id)
      | T.Reloc_setglobal id -> Reloc_setglobal (Ident.t id)
      | T.Reloc_primitive s -> Reloc_primitive s


    let compilation_unit v =
      {
        cu_name = v.T.cu_name;
        cu_pos = v.T.cu_pos;
        cu_codesize = v.T.cu_codesize;
        cu_reloc = List.map (fun (r,i) ->
            (reloc_info r,i)) v.T.cu_reloc;
        cu_imports = v.T.cu_imports;
        cu_primitives = v.T.cu_primitives;
        cu_force_link = v.T.cu_force_link;
        cu_debug = v.T.cu_debug;
        cu_debugsize = v.T.cu_debugsize;
      }

    let library v =
      {
        lib_units = List.map compilation_unit v.T.lib_units;
        lib_custom = v.T.lib_custom;
        lib_ccobjs = v.T.lib_ccobjs;
        lib_ccopts = v.T.lib_ccopts;
        lib_dllibs = v.T.lib_dllibs;
      }


  end

end

open Cmo_format

let input_cmo_file ic magic =
  if magic = V3112_types.cmo_magic_number then
    let v = (input_value ic : V3112_types.Cmo_format.compilation_unit) in
    Compunit (CMO.Cmo_format.compilation_unit v)
  else
  if magic = V3112_types.cma_magic_number then
    let v = (input_value ic : V3112_types.Cmo_format.library) in
    Library (CMO.Cmo_format.library v)
  else
    raise (Error (No_Such_Magic ("cmo", magic)))
