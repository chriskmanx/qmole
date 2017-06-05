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

module AST : sig
    
    module Asttypes : sig 
           val constant : V3120_types.Asttypes.constant -> Asttypes.constant
           val virtual_flag : V3120_types.Asttypes.virtual_flag -> Asttypes.virtual_flag
           val private_flag : V3120_types.Asttypes.private_flag -> Asttypes.private_flag
           val rec_flag : V3120_types.Asttypes.rec_flag -> Asttypes.rec_flag
           val mutable_flag : V3120_types.Asttypes.mutable_flag -> Asttypes.mutable_flag
           val direction_flag : V3120_types.Asttypes.direction_flag -> Asttypes.direction_flag
      end
      
(*      functor(S: sig end) -> sig end *)
  end

val input_intf_file : in_channel -> string -> Parsetree.signature
val input_impl_file : in_channel -> string -> Parsetree.structure