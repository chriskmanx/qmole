open Typedtree

let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  let magic_number = String.create len_magic_number in
  really_input ic magic_number 0 len_magic_number;
  magic_number

type binary_annots =
  | Packed of Types.signature * string list
  | Implementation of structure
  | Interface of signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array

and binary_part =
| Partial_structure of structure
| Partial_structure_item of structure_item
| Partial_signature of signature
| Partial_signature_item of signature_item
| Partial_expression of expression
| Partial_module_type of module_type
| Partial_pattern of pattern
| Partial_class_expr of class_expr

type cmt_infos = {
  cmt_modname : string;
  cmt_annots : binary_annots;
  cmt_comments : (string * Location.t) list;
  cmt_args : string array;
  cmt_sourcefile : string;
  cmt_builddir : string;
  cmt_loadpath : string list;
  cmt_packed : string list;
  cmt_source_digest : string;
(* TODO
  cmt_crcs : (string * Digest.t) list;
  cmt_flags : Env.pers_flags list;
*)
}

type snapshot = {
  filename : string;
  contents : string
}

type generator =
  | Ocamlyacc
  | Ocamllex

type source_info = {
  source_file : snapshot;
  generated : (generator * snapshot) option
}

type error =
    Not_a_typedtree of string

exception Error of error

(*

open Typedtree



type t = Typedtree.cmt

let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  let magic_number = String.create len_magic_number in
  really_input ic magic_number 0 len_magic_number;
  magic_number

let is_magic_number magic =
  (magic = Config.cmt_magic_number)

let read ic =
  let cmt = (input_value ic : t) in
  cmt

let write_magic_number oc =
  output_string oc Config.cmt_magic_number

let write oc t =
  output_value oc t


let read_signature modname filename =
  if not (Filename.check_suffix filename ".cmti"
          || Filename.check_suffix filename ".cmt")
  then
    raise (Not_a_typedtree filename);
  let ic = open_in_bin filename in
  let sign =
    try
      let buffer = read_magic_number ic in
      if buffer <> Config.cmt_magic_number then begin
        close_in ic;
        raise (Not_a_typedtree filename)
      end;
      let cmt = input_value ic in
      close_in ic;
      match cmt.cmt_annots with
        | Typedtree.Interface tsg -> tsg.Typedtree.sig_type
        | Typedtree.Implementation str -> str.Typedtree.str_type
        | _ -> raise (Corrupted_typedtree filename)
    with End_of_file | Failure _ ->
      close_in ic;
      raise (Corrupted_typedtree filename)
  in
  sign, [], []

*)

let input_cmt ic = (input_value ic : cmt_infos)

let read filename =
(*  Printf.fprintf stderr "Cmt_format.read %s\n%!" filename; *)
  let ic = open_in filename in
  try
    let magic_number = read_magic_number ic in
    let cmi, cmt =
      if magic_number = Config.cmt_magic_number then
        None, Some (input_cmt ic)
      else if magic_number = Config.cmi_magic_number then
        let cmi = Env.input_cmi ic in
        let cmt = try
                    let magic_number = read_magic_number ic in
                    if magic_number = Config.cmt_magic_number then
                      let cmt = input_cmt ic in
                      Some cmt
                    else None
          with _ -> None
        in
        Some cmi, cmt
      else
        raise(Env.Error(Env.Not_an_interface filename))
    in
    let source_info =
      try Some (input_value ic : source_info)
      with _ -> None in
    close_in ic;
(*    Printf.fprintf stderr "Cmt_format.read done\n%!"; *)
    cmi, cmt, source_info
  with e ->
    close_in ic;
    raise e

let read_cmt filename =
  match read filename with
      _, None, _ -> raise (Error (Not_a_typedtree filename))
    | _, Some cmt, _ -> cmt

let read_cmi filename =
  match read filename with
      None, _, _ -> raise (Env.Error (Env.Not_an_interface filename))
    | Some cmi, _, _ -> cmi

let rec first_existing prefix = function
  | [] -> raise Not_found
  | (suffix, kind) :: suffixes ->
    let f = prefix ^ suffix in
    if Sys.file_exists f then
      kind, {filename = f; contents = File.string_of_file f}
    else
      first_existing prefix suffixes

let generated sourcefile cmtfile =
  if Filename.check_suffix cmtfile ".cmti" then
    try
      Some (first_existing (Misc.chop_extension_if_any sourcefile) [".mly", Ocamlyacc])
    with Not_found -> None
  else
    try
      Some
        (first_existing (Misc.chop_extension_if_any sourcefile)
           [".mly", Ocamlyacc ; ".mll", Ocamllex])
    with Not_found -> None

(*
val get_saved_types : unit -> saved_type list
val set_saved_types : saved_type list -> unit
val add_saved_type : saved_type -> unit
*)

let saved_types = ref []

let add_saved_type b = saved_types := b :: !saved_types
let get_saved_types () = !saved_types
let set_saved_types l = saved_types := l

(* same as env.output_cmi, but allow to not add self crc. *)
let output_cmi ?(no_self_crc=false) filename oc cmi =
  output_value oc (cmi.Env.cmi_name, cmi.Env.cmi_sign);
  flush oc;
  let crcs =
    if no_self_crc then
      cmi.Env.cmi_crcs
    else
      (cmi.Env.cmi_name, Digest.file filename) :: cmi.Env.cmi_crcs
  in
  output_value oc crcs;
  output_value oc cmi.Env.cmi_flags

let use_existing_cmi = ref false

let save_cmt modname filename binary_annots sourcefile packed_modules sg =
  if !Clflags.binary_annotations && not !Clflags.print_types then begin
    let oc = open_out filename in
    begin
      match sg with
          None -> ()
        | Some (sg, imports) ->
          let cmi =
            if !use_existing_cmi then
              match sourcefile with
                | Some sourcefile ->
                  let cmi = Misc.chop_extension_if_any sourcefile ^ ".cmi" in
                  {(!Env.read_cmi_fun cmi) with Env.cmi_sign = sg}
                | None -> assert false
            else
                {
                  Env.cmi_name = modname;
                  cmi_sign = sg;
                  cmi_flags =
                    if !Clflags.recursive_types then [Env.Rectypes] else [];
                  cmi_crcs = imports;
                }
          in
          output_string oc Config.cmi_magic_number;
          output_cmi ~no_self_crc:!use_existing_cmi filename oc cmi
    end;
    output_string oc Config.cmt_magic_number;
    let cmt = {
      cmt_modname = modname;
      cmt_annots = binary_annots;
      cmt_comments = Lexer.comments ();
      cmt_args = Sys.argv;
      cmt_sourcefile = (match sourcefile with Some f -> f | None -> filename);
      cmt_builddir =  Sys.getcwd ();
      cmt_loadpath = !Config.load_path;
      cmt_packed = packed_modules;
      cmt_source_digest =
        (match sourcefile with Some f -> Digest.file f | None -> "");
(* TODO
      cmt_crcs = crcs;
      cmt_flags = [];
*)
    } in
    output_value oc cmt;
    let source_info =
      match sourcefile with
        | Some f -> {
          source_file = {
            filename = f;
            contents = File.string_of_file f
          };
          generated = generated f filename
        }
        | None -> { source_file = {filename = filename; contents = ""} ; generated = None }
    in
    output_value oc source_info;
    close_out oc;
    set_saved_types [];
  end;
  set_saved_types  []
