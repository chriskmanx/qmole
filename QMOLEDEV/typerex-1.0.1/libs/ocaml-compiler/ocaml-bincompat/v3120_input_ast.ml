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
    module T = V3120_types.Asttypes


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

    let closed_flag cf = match cf with
        T.Closed -> Closed | T.Open -> Open

    let override_flag ovf =
      match ovf with
        T.Override -> Override
      | T.Fresh -> Fresh

    let label s = s

end

module Lexing = struct

    open Lexing
    module T = V3120_types.Lexing

    let position p =
      { pos_fname = p.T.pos_fname;
        pos_lnum = p.T.pos_lnum;
        pos_bol = p.T.pos_bol;
        pos_cnum = p.T.pos_cnum;
      }

  end

module Location = struct

    open Location
    module T = V3120_types.Location

    let t loc =
      { loc_start = Lexing.position loc.T.loc_start;
        loc_end = Lexing.position loc.T.loc_end;
        loc_ghost = loc.T.loc_ghost;
      }

  end

module Longident = struct

    open Longident
    module T = V3120_types.Longident

  let rec t l =
    match l with
	T.Lident s -> Lident s
      | T.Ldot (ll, s) -> Ldot (t ll, s)
      | T.Lapply (l1, l2) -> Lapply (t l1, t l2)

end


end




let input_intf_file ic magic =
  if magic <> V3120_types.ast_intf_magic_number then
    V3112_input_ast.input_intf_file ic magic
  else begin
    let v = (input_value ic : V3120_types.Parsetree.signature) in
      AST.Parsetree.signature v
    end

let input_impl_file ic magic =
  if magic <> V3120_types.ast_impl_magic_number then
    V3112_input_ast.input_impl_file ic magic
  else begin
      let v = (input_value ic : V3120_types.Parsetree.structure) in
      AST.Parsetree.structure v
    end
