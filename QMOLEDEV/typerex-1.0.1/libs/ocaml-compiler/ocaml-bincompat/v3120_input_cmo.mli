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

module CMO :  sig
           module Lambda :
             sig
               val structured_constant :
                 V3120_types.Lambda.structured_constant ->
                 Lambda.structured_constant
               val primitive :
                 V3120_types.Lambda.primitive -> Lambda.primitive
               val meth_kind :
                 V3120_types.Lambda.meth_kind -> Lambda.meth_kind
             end
           module Cmo_format :
             sig
               val compilation_unit :
                 V3120_types.Cmo_format.compilation_unit ->
                 Cmo_format.compilation_unit
               val library :
                 V3120_types.Cmo_format.library -> Cmo_format.library
             end
         end

  (* functor(M : sig end) -> sig end *)
  
val input_cmo_file : in_channel -> string -> Cmo_format.cmo_unit
