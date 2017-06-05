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

let ocamllib = "/usr/local/lib/ocaml"

let runner = "@OCAMLRUN@"
(*
OCPBUILD=/home/chris/typerex-1.0.1/_obuild/unixrun /home/chris/typerex-1.0.1/boot/ocp-build.boot

OCAMLDEP=ocamldep

OCP_BUILD_OCAMLC=$(patsubst %,"%",ocamlc)
OCP_BUILD_OCAMLOPT="ocamlopt"
OCP_BUILD_OCAMLDEP="ocamldep"
OCP_BUILD_OCAMLLEX="ocamllex"
OCP_BUILD_OCAMLYACC="ocamlyacc"
#OCP_BUILD_CCOPT=$(patsubst %,"%",@CPPFLAGS@) $(patsubst %,"%",@LDFLAGS@)
OCP_BUILD_CCOPT=

# Used for menhirLib ?
#OCAMLC=ocamlc.opt
#OCAMLOPT=ocamlopt.opt

EMACS      = emacs
OCAMLVERSION=3.12.0

# These are needed because configure does not expand them.
prefix=/usr/local
exec_prefix=/usr/local
datarootdir = /usr/local/share

# Where to install ocp executables and lisp code
BINDIR=/usr/local/bin
EMACSDIR=/usr/local/share/emacs/site-lisp
*)

let typerex_version = "1.0.1"

let version = (
  "--version",

  Arg.Unit (fun () ->
    Printf.printf "\
%s is part of TypeRex-%s

Copyright (C) 2012 OCamlPro - INRIA

This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
      Sys.argv.(0) typerex_version;
  exit 0),

    " Display version information"
)
