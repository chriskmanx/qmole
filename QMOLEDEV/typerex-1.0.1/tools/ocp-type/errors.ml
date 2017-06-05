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

(* $Id$ *)

(* WARNING: if you change something in this file, you must look at
   opterrors.ml and ocamldoc/odoc_analyse.ml
   to see if you need to make the same changes there.
*)

open Format

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print_error ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Pparse.Error ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Preprocessor error"
  | Env.Error err ->
      Location.print_error_cur_file ppf;
      Env.report_error ppf err
  | Ctype.Tags(l, l') ->
      Location.print_error_cur_file ppf;
      fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value.@ Change one of them." l l'
  | Typecore.Error(loc, err) ->
      Location.print_error ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print_error ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print_error ppf loc; Typedecl.report_error ppf err
  | Typeclass.Error(loc, err) ->
      Location.print_error ppf loc; Typeclass.report_error ppf err
  | Includemod.Error err ->
      Location.print_error_cur_file ppf;
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print_error ppf loc; Typemod.report_error ppf err
  | Sys_error msg ->
      Location.print_error_cur_file ppf;
      fprintf ppf "I/O error: %s" msg
  | Warnings.Errors (n) ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Error-enabled warnings (%d occurrences)" n
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn
