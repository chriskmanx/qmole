/**************************************************************************/
/*                                                                        */
/*                        TypeRex OCaml Studio                            */
/*                                                                        */
/*                 Thomas Gazagnaire, Fabrice Le Fessant                  */
/*                                                                        */
/*  Copyright 2011-2012 OCamlPro                                          */
/*  All rights reserved.  This file is distributed under the terms of     */
/*  the GNU Public License version 3.0.                                   */
/*                                                                        */
/*  TypeRex is distributed in the hope that it will be useful,            */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU General Public License for more details.                          */
/*                                                                        */
/**************************************************************************/

%{

  open BuildOCPTypes

  (* TODO: location of the type in ocamlyacc is erroneous, for example here token "main"
   type is located in the .mli/.ml file instead of the .mly file. *)

%}

%token <string> STRING
%token <int> INT
%token EOF
%token <float> FLOAT
%token <char> CHAR
%token SEMI
%token BEGIN
%token END
%token <string> IDENT
%token LBRACKET
%token RBRACKET
%token PLUSEQUAL
%token MINUSEQUAL
%token TRUE
%token FALSE
%token INCLUDE
%token OBJECTS
%token LIBRARY
%token PROGRAM
%token CONFIG
%token EQUAL
%token LPAREN
%token RPAREN
%token FILES
%token REQUIRES
%token TYPE
%token FILE
%token USE
%token PACK

%start main
%type <BuildOCPTypes.statement list> main

%%

main:
statements EOF { $1 }
;

statements:
  { [] }
| statement statements { $1 :: $2 }
| SEMI statements { $2 }
;

package_type:
  PROGRAM { ProjectProgram }
| LIBRARY { ProjectLibrary }
| OBJECTS { ProjectObjects }
;

statement:
| BEGIN CONFIG STRING options END { StmtDefineConfig ($3, $4) }
| BEGIN package_type STRING simple_statements END { StmtDefinePackage ($2, $3, $4) }
| BEGIN statements END { StmtBlock $2 }
| INCLUDE STRING { StmtInclude $2 }
| simple_statement { $1 }

/* for backward compatibility */
| BEGIN STRING TYPE EQUAL package_type simple_statements END { StmtDefinePackage ($5, $2, $6) }
;

simple_statements:
  { [] }
| simple_statement simple_statements { $1 :: $2 }
| SEMI simple_statements { $2 }
;

simple_statement:
| option { StmtOption $1 }
| FILES EQUAL list_of_files { StmtFilesSet $3 }
| FILE EQUAL STRING { StmtFilesSet [$3, []] }
| FILES PLUSEQUAL list_of_files { StmtFilesAppend $3 }
| REQUIRES list_of_strings { StmtRequiresAppend $2 }
/* The two next ones only for backward compatibility */
| REQUIRES EQUAL list_of_strings { StmtRequiresAppend $3 }
| REQUIRES PLUSEQUAL list_of_strings { StmtRequiresAppend $3 }
;

option:
| USE STRING { OptionConfigSet $2 }
| IDENT EQUAL STRING { OptionListSet ($1,[$3]) }
| IDENT PLUSEQUAL STRING { OptionListAppend ($1,[$3]) }
| IDENT MINUSEQUAL STRING { OptionListRemove ($1,[$3]) }
| IDENT EQUAL list_of_strings { OptionListSet ($1,$3) }
| IDENT PLUSEQUAL list_of_strings { OptionListAppend ($1, $3) }
| IDENT MINUSEQUAL list_of_strings { OptionListRemove ($1, $3) }
| IDENT EQUAL TRUE { OptionBoolSet ($1, true) }
| IDENT EQUAL FALSE { OptionBoolSet ($1, false) }
;

options:
| { [] }
| option options { $1 :: $2 }
| SEMI options { $2 }
;

list_of_options:
|                        { [] }
|  LPAREN options RPAREN { $2 }
;

list_of_strings:
| LBRACKET strings RBRACKET { $2 }
;

strings:
 { [] }
| STRING strings { $1 :: $2 }
| IDENT strings { $1 :: $2 }
| SEMI strings { $2 }
;

list_of_files:
| LBRACKET files RBRACKET { $2 }
;

packer:
| PACK STRING { $2 }
| PACK IDENT  { let s = $2 in s.[0] <- Char.lowercase s.[0]; s ^ ".ml" }
;

files:
 { [] }
| STRING list_of_options files { ($1, $2) :: $3 }
| SEMI files { $2 }
| BEGIN list_of_options files END files
   { let shared_options = $2 in
     let inner_files = $3 in
     let outter_files = $5 in
     let inner_files =
       List.map (fun (file, file_options) ->
	 (file, shared_options @ file_options)
       ) inner_files in
     inner_files @ outter_files
   }
| packer list_of_options list_of_files list_of_options files {
  let packname = $1 in
  let pack_options1 = $2 in
  let files = $3 in
  let pack_options2 = $4 in
  let other_files = $5 in
  let pack_options = pack_options1 @ pack_options2 in

  let packmodname = BuildOCPTypes.modname_of_fullname packname in

  let modnames = ref [] in
  let packed_files =
    List.map (fun (file, file_options) ->
      begin
        match file_options with
            OptionListAppend ( "packed", _ ) :: _ -> ()
          | _ ->
            modnames := Filename.basename file :: !modnames
      end;
      (file, OptionListAppend ("packed", [packmodname]) :: pack_options @ file_options)
  ) files;
  in
  packed_files @
    [ packname, OptionListSet ("pack", List.rev !modnames) :: pack_options] @
    other_files
}
;

%%
