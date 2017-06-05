(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* Pervasives extension *)

include OcpPervasives

module List = struct
  include List
  include OcpList
end

module String = struct
  include String
  include OcpString
end

module Stream = struct
  include Stream
  include OcpStream
end

module Genlex = struct
  include Genlex
  include OcpGenlex
end

module Hashtbl = struct
  include Hashtbl
  include OcpHashtbl
end

module Digest = struct
  include Digest
  include OcpDigest
end

