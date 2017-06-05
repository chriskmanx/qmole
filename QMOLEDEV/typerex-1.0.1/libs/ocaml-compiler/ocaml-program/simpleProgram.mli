(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Reading program information from a .ocp-wizard file *)

(** The following program builder ensures flexibility by providing a
    very simple file format for describing programs independently of
    the build process. With this solution, some features of programs
    are not avaiable:
    - the load path is the same for all compilation units (which prevents
      a correct handling of multiple modules with the same name)
    - the -nopervasives and -nostdlib options are not available
    - only the ocamlyacc and ocamllex pre-processor can be used.

    Programs built by this module have the following property: all
    load paths only contains directories which are in the
    project. This allows to search only in the project when resolving
    a module, avoiding [find_in_path_uncap], which reduces the number
    of special cases to consider. *)

(** Raised in case of incorrect project file. The first string is the
    absolute project file name; the second is an explanation. *)
exception ProjectFileError of (string * string)

(** Raised by program if the specified file is not in the program
    description file whose directory contains the file. First argument
    is the absolute path to the project file ; second is the relative
    path from the root to the directory containing the file. *)
exception FileNotInProgram of string * string list

(** Raised by program if the specified file is explicitely excluded
    from the program description file whose directory contains the
    file. First argument is the absolute path to the project file ;
    second is the excluded string (prefix or filename). *)
exception FileExcluded of string * string

(** Raised by program if the specified file is not known to be an
    OCaml source file. *)
exception NotASourceFile of string

(** Return the program containing a given filename, as well as the id
    of the source file itself. If cwd is provided, it overwrites the
    current working directory when interpreting the provided filename
    (if it is relative). If [ignore_project_file] is set, use "." as
    the project's root and ignore any .ocamlwizard file. If
    [default_cwd] is not set, and if there is no project file (or if
    it is ignored), then the project consists only of the provided
    source file, ignoring the contents of "."

    If [ignore_absent] is set, force the inclusion of the given
    filename, even if it is not in the project, or masked, or does not
    exist.

    @raise [FileNotInProgram] as described above.

    @raise [Invalid_argument] if the file is not reduced to a
    basename, or does not end in .ml, .mli, .mly or .mll, unless
    [ignore_extension] is set. *)
val program :
  ?ignore_absent:bool -> (** default false *)
  ?ignore_extension:bool -> (** default false *)
  ?ignore_project_file:bool -> (** default false *)
  ?default_cwd:bool -> (** default true *)
  ?cwd:string  -> string -> Program.source_file_id * Program.program

(** List of extensions that are treated as implementations. Default is
    [\[".ml"\]]. *)
val plain_impl_extensions : string list ref

(** List of extensions that are treated as interfaces. Default is
    [\[".mli"\]]. *)
val plain_intf_extensions : string list ref

(** List of source extensions that should be preferred to plain
    implementation, when both exist. Default is [\[".mll"; ".mly";
    ".mlp"\]]. *)
val impl_generating_extensions : string list ref

(** List of source extensions that should be preferred to plain
    interfaces, when both exist. Default is [\[".mly"\]]. *)
val intf_generating_extensions : string list ref

(** List of extensions that are considered when collecting OCaml
    implementation files. With defaults values, this evaluates to
    [\[".mll"; ".mly"; ".mlp"; ".ml"\]] *)
val ocaml_impl_extensions : unit -> string list

(** List of extensions that are considered when collecting OCaml
    interface files. With defaults values, this evaluates to
    [\[".mly"; ".mli"\]] *)
val ocaml_impl_extensions : unit -> string list

(** List of extensions that are considered when collecting all kinds
    of OCaml source files. With defaults values, this evaluates to
    [\[".ml"; ".mli"; ".mll"; ".mly"; ".mlp"\]] *)
val ocaml_source_extensions : unit -> string list
