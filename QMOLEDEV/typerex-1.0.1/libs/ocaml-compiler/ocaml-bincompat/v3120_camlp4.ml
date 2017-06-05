(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant, Tiphaine Turpin            *)
(*                                                                        *)
(*    OCaml - Camlp4                                                      *)
(*      Daniel de Rauglaudre, Nicolas Pouillard, INRIA Rocquencourt       *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 2006-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU LGPL, with the special exception on linking described in      *)
(*  file LIDENSE                                                          *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Library General Public License for more details.                  *)
(**************************************************************************)

(* This file is the concatenation of the parts of boot/Camlp4.ml that
   are needed to input parsetrees from camlp4 ASTs, namely:
   - the concrete Loc implementation (from Camlp4/Struct)
   - the concrete Ast implem (from Camlp4/Camlp4Ast.partial +
   Camlp4/Struct/Camlp4Ast)
   - the types camlp4_token and quotation
   - the concrete Token implem (from Camlp4/Token)
   - Camlp4Ast2OCamlAst (from Camlp4/Camlp4Ast2OCamlAst).

   The necessay changes are made to output a "current version"
   Parsetree instead of a 3.12 one. *)

(* About the differences between 3.12.0 and 3.12.1:

   The only difference between the two 3.12 versions is the additional
   constructor MtOf (for the syntax 'module type of') present in
   3.12.1 only. Since the only constructor which appears after MtOf in
   the type definition is MtAnt (for anti-quotations), and
   antiquotations cannot appear in the output AST, it should be safe
   to unmarshal both versions as if they were 3.12.1. *)


(* These are the same since version 3.10 ! *)
let camlp4_ast_impl_magic_number = "Camlp42006M001"
let camlp4_ast_intf_magic_number = "Camlp42006N001"

module Camlp4_config = struct

  let constructors_arity = ref false
  let unsafe = ref false
end

module Loc = struct

        open Format
          
        type pos = { line : int; bol : int; off : int }
        
        type t =
          { file_name : string; start : pos; stop : pos; ghost : bool
          }
        
        let dump_sel f x =
          let s =
            match x with
            | `start -> "`start"
            | `stop -> "`stop"
            | `both -> "`both"
            | _ -> "<not-printable>"
          in pp_print_string f s
          
        let dump_pos f x =
          fprintf f "@[<hov 2>{ line = %d ;@ bol = %d ;@ off = %d } : pos@]"
            x.line x.bol x.off
          
        let dump_long f x =
          fprintf f
            "@[<hov 2>{ file_name = %s ;@ start = %a (%d-%d);@ stop = %a (%d);@ ghost = %b@ } : Loc.t@]"
            x.file_name dump_pos x.start (x.start.off - x.start.bol)
            (x.stop.off - x.start.bol) dump_pos x.stop
            (x.stop.off - x.stop.bol) x.ghost
          
        let dump f x =
          fprintf f "[%S: %d:%d-%d %d:%d%t]" x.file_name x.start.line
            (x.start.off - x.start.bol) (x.stop.off - x.start.bol)
            x.stop.line (x.stop.off - x.stop.bol)
            (fun o -> if x.ghost then fprintf o " (ghost)" else ())
          
        let start_pos = { line = 1; bol = 0; off = 0; }
          
        let ghost =
          {
            file_name = "ghost-location";
            start = start_pos;
            stop = start_pos;
            ghost = true;
          }
          
        let mk file_name =
          {
            file_name = file_name;
            start = start_pos;
            stop = start_pos;
            ghost = false;
          }
          
        let of_tuple (file_name, start_line, start_bol, start_off, stop_line,
                      stop_bol, stop_off, ghost)
                     =
          {
            file_name = file_name;
            start = { line = start_line; bol = start_bol; off = start_off; };
            stop = { line = stop_line; bol = stop_bol; off = stop_off; };
            ghost = ghost;
          }
          
        let to_tuple {
                       file_name = file_name;
                       start =
                         {
                           line = start_line;
                           bol = start_bol;
                           off = start_off
                         };
                       stop =
                         { line = stop_line; bol = stop_bol; off = stop_off };
                       ghost = ghost
                     } =
          (file_name, start_line, start_bol, start_off, stop_line, stop_bol,
           stop_off, ghost)
          
        let pos_of_lexing_position p =
          let pos =
            {
              line = p.Lexing.pos_lnum;
              bol = p.Lexing.pos_bol;
              off = p.Lexing.pos_cnum;
            }
          in pos
          
        let pos_to_lexing_position p file_name =
          {
            Lexing.pos_fname = file_name;
            pos_lnum = p.line;
            pos_bol = p.bol;
            pos_cnum = p.off;
          }
          
        let better_file_name a b =
          match (a, b) with
          | ("", "") -> a
          | ("", x) -> x
          | (x, "") -> x
          | ("-", x) -> x
          | (x, "-") -> x
          | (x, _) -> x
          
        let of_lexbuf lb =
          let start = Lexing.lexeme_start_p lb
          and stop = Lexing.lexeme_end_p lb in
          let loc =
            {
              file_name =
                better_file_name start.Lexing.pos_fname stop.Lexing.pos_fname;
              start = pos_of_lexing_position start;
              stop = pos_of_lexing_position stop;
              ghost = false;
            }
          in loc
          
        let of_lexing_position pos =
          let loc =
            {
              file_name = pos.Lexing.pos_fname;
              start = pos_of_lexing_position pos;
              stop = pos_of_lexing_position pos;
              ghost = false;
            }
          in loc
          
        let to_ocaml_location x =
          {
            Location.loc_start =
              pos_to_lexing_position x.start x.file_name;
            loc_end = pos_to_lexing_position x.stop x.file_name;
            loc_ghost = x.ghost;
          }
          
        let of_ocaml_location {
                                Location.loc_start = a;
                                loc_end = b;
                                loc_ghost = g
                              } =
          let res =
            {
              file_name =
                better_file_name a.Lexing.pos_fname b.Lexing.pos_fname;
              start = pos_of_lexing_position a;
              stop = pos_of_lexing_position b;
              ghost = g;
            }
          in res
          
        let start_pos x = pos_to_lexing_position x.start x.file_name
          
        let stop_pos x = pos_to_lexing_position x.stop x.file_name
          
        let merge a b =
          if a == b
          then a
          else
            (let r =
               match ((a.ghost), (b.ghost)) with
               | (false, false) -> { (a) with stop = b.stop; }
               | (true, true) -> { (a) with stop = b.stop; }
               | (true, _) -> { (a) with stop = b.stop; }
               | (_, true) -> { (b) with start = a.start; }
             in r)
          
        let join x = { (x) with stop = x.start; }
          
        let map f start_stop_both x =
          match start_stop_both with
          | `start -> { (x) with start = f x.start; }
          | `stop -> { (x) with stop = f x.stop; }
          | `both -> { (x) with start = f x.start; stop = f x.stop; }
          
        let move_pos chars x = { (x) with off = x.off + chars; }
          
        let move s chars x = map (move_pos chars) s x
          
        let move_line lines x =
          let move_line_pos x =
            { (x) with line = x.line + lines; bol = x.off; }
          in map move_line_pos `both x
          
        let shift width x =
          { (x) with start = x.stop; stop = move_pos width x.stop; }
          
        let file_name x = x.file_name
          
        let start_line x = x.start.line
          
        let stop_line x = x.stop.line
          
        let start_bol x = x.start.bol
          
        let stop_bol x = x.stop.bol
          
        let start_off x = x.start.off
          
        let stop_off x = x.stop.off
          
        let is_ghost x = x.ghost
          
        let set_file_name s x = { (x) with file_name = s; }
          
        let ghostify x = { (x) with ghost = true; }
          
        let make_absolute x =
          let pwd = Sys.getcwd ()
          in
            if Filename.is_relative x.file_name
            then { (x) with file_name = Filename.concat pwd x.file_name; }
            else x
          
        let strictly_before x y =
          let b = (x.stop.off < y.start.off) && (x.file_name = y.file_name)
          in b
          
        let to_string x =
          let (a, b) = ((x.start), (x.stop)) in
          let res =
            sprintf "File \"%s\", line %d, characters %d-%d" x.file_name
              a.line (a.off - a.bol) (b.off - a.bol)
          in
            if x.start.line <> x.stop.line
            then
              sprintf "%s (end at line %d, character %d)" res x.stop.line
                (b.off - b.bol)
            else res
          
        let print out x = pp_print_string out (to_string x)
          
        let check x msg =
          if
            ((start_line x) > (stop_line x)) ||
              (((start_bol x) > (stop_bol x)) ||
                 (((start_off x) > (stop_off x)) ||
                    (((start_line x) < 0) ||
                       (((stop_line x) < 0) ||
                          (((start_bol x) < 0) ||
                             (((stop_bol x) < 0) ||
                                (((start_off x) < 0) || ((stop_off x) < 0))))))))
          then
            (eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg
               print x;
             false)
          else true
          
        exception Exc_located of t * exn
          
(* Tifn: removing this
        let _ =
          ErrorHandler.register
            (fun ppf ->
               function
               | Exc_located (loc, exn) ->
                   fprintf ppf "%a:@\n%a" print loc ErrorHandler.print exn
               | exn -> raise exn)
*)
          
        let name = ref "_loc"
          
        let raise loc exc =
          match exc with
          | Exc_located (_, _) -> raise exc
          | _ -> raise (Exc_located (loc, exc))

end

module Ast = struct
module Ast = struct
        type loc =
          Loc.
          t
          and meta_bool =
          | BTrue | BFalse | BAnt of string
          and rec_flag =
          | ReRecursive | ReNil | ReAnt of string
          and direction_flag =
          | DiTo | DiDownto | DiAnt of string
          and mutable_flag =
          | MuMutable | MuNil | MuAnt of string
          and private_flag =
          | PrPrivate | PrNil | PrAnt of string
          and virtual_flag =
          | ViVirtual | ViNil | ViAnt of string
          and override_flag =
          | OvOverride | OvNil | OvAnt of string
          and row_var_flag =
          | RvRowVar | RvNil | RvAnt of string
          and 'a meta_option =
          | ONone | OSome of 'a | OAnt of string
          and 'a meta_list =
          | LNil | LCons of 'a * 'a meta_list | LAnt of string
          and ident =
          | IdAcc of loc * ident * ident
          | (* i . i *)
          IdApp of loc * ident * ident
          | (* i i *)
          IdLid of loc * string
          | (* foo *)
          IdUid of loc * string
          | (* Bar *)
          IdAnt of loc * string
          and (* $s$ *)
          ctyp =
          | TyNil of loc
          | TyAli of loc * ctyp * ctyp
          | (* t as t *)
          (* list 'a as 'a *)
          TyAny of loc
          | (* _ *)
          TyApp of loc * ctyp * ctyp
          | (* t t *)
          (* list 'a *)
          TyArr of loc * ctyp * ctyp
          | (* t -> t *)
          (* int -> string *)
          TyCls of loc * ident
          | (* #i *)
          (* #point *)
          TyLab of loc * string * ctyp
          | (* ~s:t *)
          TyId of loc * ident
          | (* i *)
          (* Lazy.t *)
          TyMan of loc * ctyp * ctyp
          | (* t == t *)
          (* type t = [ A | B ] == Foo.t *)
          (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
          TyDcl of loc * string * ctyp list * ctyp * (ctyp * ctyp) list
          | (* < (t)? (..)? > *)
          (* < move : int -> 'a .. > as 'a  *)
          TyObj of loc * ctyp * row_var_flag
          | TyOlb of loc * string * ctyp
          | (* ?s:t *)
          TyPol of loc * ctyp * ctyp
          | (* ! t . t *)
          (* ! 'a . list 'a -> 'a *)
          TyQuo of loc * string
          | (* 's *)
          TyQuP of loc * string
          | (* +'s *)
          TyQuM of loc * string
          | (* -'s *)
          TyVrn of loc * string
          | (* `s *)
          TyRec of loc * ctyp
          | (* { t } *)
          (* { foo : int ; bar : mutable string } *)
          TyCol of loc * ctyp * ctyp
          | (* t : t *)
          TySem of loc * ctyp * ctyp
          | (* t; t *)
          TyCom of loc * ctyp * ctyp
          | (* t, t *)
          TySum of loc * ctyp
          | (* [ t ] *)
          (* [ A of int and string | B ] *)
          TyOf of loc * ctyp * ctyp
          | (* t of t *)
          (* A of int *)
          TyAnd of loc * ctyp * ctyp
          | (* t and t *)
          TyOr of loc * ctyp * ctyp
          | (* t | t *)
          TyPrv of loc * ctyp
          | (* private t *)
          TyMut of loc * ctyp
          | (* mutable t *)
          TyTup of loc * ctyp
          | (* ( t ) *)
          (* (int * string) *)
          TySta of loc * ctyp * ctyp
          | (* t * t *)
          TyVrnEq of loc * ctyp
          | (* [ = t ] *)
          TyVrnSup of loc * ctyp
          | (* [ > t ] *)
          TyVrnInf of loc * ctyp
          | (* [ < t ] *)
          TyVrnInfSup of loc * ctyp * ctyp
          | (* [ < t > t ] *)
          TyAmp of loc * ctyp * ctyp
          | (* t & t *)
          TyOfAmp of loc * ctyp * ctyp
          | (* t of & t *)
          TyPkg of loc * module_type
          | (* (module S) *)
          TyAnt of loc * string
          and (* $s$ *)
          patt =
          | PaNil of loc
          | PaId of loc * ident
          | (* i *)
          PaAli of loc * patt * patt
          | (* p as p *)
          (* (Node x y as n) *)
          PaAnt of loc * string
          | (* $s$ *)
          PaAny of loc
          | (* _ *)
          PaApp of loc * patt * patt
          | (* p p *)
          (* fun x y -> *)
          PaArr of loc * patt
          | (* [| p |] *)
          PaCom of loc * patt * patt
          | (* p, p *)
          PaSem of loc * patt * patt
          | (* p; p *)
          PaChr of loc * string
          | (* c *)
          (* 'x' *)
          PaInt of loc * string
          | PaInt32 of loc * string
          | PaInt64 of loc * string
          | PaNativeInt of loc * string
          | PaFlo of loc * string
          | PaLab of loc * string * patt
          | (* ~s or ~s:(p) *)
          (* ?s or ?s:(p) *)
          PaOlb of loc * string * patt
          | (* ?s:(p = e) or ?(p = e) *)
          PaOlbi of loc * string * patt * expr
          | PaOrp of loc * patt * patt
          | (* p | p *)
          PaRng of loc * patt * patt
          | (* p .. p *)
          PaRec of loc * patt
          | (* { p } *)
          PaEq of loc * ident * patt
          | (* i = p *)
          PaStr of loc * string
          | (* s *)
          PaTup of loc * patt
          | (* ( p ) *)
          PaTyc of loc * patt * ctyp
          | (* (p : t) *)
          PaTyp of loc * ident
          | (* #i *)
          PaVrn of loc * string
          | (* `s *)
          PaLaz of loc * patt
          and (* lazy p *)
          expr =
          | ExNil of loc
          | ExId of loc * ident
          | (* i *)
          ExAcc of loc * expr * expr
          | (* e.e *)
          ExAnt of loc * string
          | (* $s$ *)
          ExApp of loc * expr * expr
          | (* e e *)
          ExAre of loc * expr * expr
          | (* e.(e) *)
          ExArr of loc * expr
          | (* [| e |] *)
          ExSem of loc * expr * expr
          | (* e; e *)
          ExAsf of loc
          | (* assert False *)
          ExAsr of loc * expr
          | (* assert e *)
          ExAss of loc * expr * expr
          | (* e := e *)
          ExChr of loc * string
          | (* 'c' *)
          ExCoe of loc * expr * ctyp * ctyp
          | (* (e : t) or (e : t :> t) *)
          ExFlo of loc * string
          | (* 3.14 *)
          (* for s = e to/downto e do { e } *)
          ExFor of loc * string * expr * expr * direction_flag * expr
          | ExFun of loc * match_case
          | (* fun [ mc ] *)
          ExIfe of loc * expr * expr * expr
          | (* if e then e else e *)
          ExInt of loc * string
          | (* 42 *)
          ExInt32 of loc * string
          | ExInt64 of loc * string
          | ExNativeInt of loc * string
          | ExLab of loc * string * expr
          | (* ~s or ~s:e *)
          ExLaz of loc * expr
          | (* lazy e *)
          (* let b in e or let rec b in e *)
          ExLet of loc * rec_flag * binding * expr
          | (* let module s = me in e *)
          ExLmd of loc * string * module_expr * expr
          | (* match e with [ mc ] *)
          ExMat of loc * expr * match_case
          | (* new i *)
          ExNew of loc * ident
          | (* object ((p))? (cst)? end *)
          ExObj of loc * patt * class_str_item
          | (* ?s or ?s:e *)
          ExOlb of loc * string * expr
          | (* {< rb >} *)
          ExOvr of loc * rec_binding
          | (* { rb } or { (e) with rb } *)
          ExRec of loc * rec_binding * expr
          | (* do { e } *)
          ExSeq of loc * expr
          | (* e#s *)
          ExSnd of loc * expr * string
          | (* e.[e] *)
          ExSte of loc * expr * expr
          | (* s *)
          (* "foo" *)
          ExStr of loc * string
          | (* try e with [ mc ] *)
          ExTry of loc * expr * match_case
          | (* (e) *)
          ExTup of loc * expr
          | (* e, e *)
          ExCom of loc * expr * expr
          | (* (e : t) *)
          ExTyc of loc * expr * ctyp
          | (* `s *)
          ExVrn of loc * string
          | (* while e do { e } *)
          ExWhi of loc * expr * expr
          | (* let open i in e *)
          ExOpI of loc * ident * expr
          | (* fun (type t) -> e *)
          (* let f x (type t) y z = e *)
          ExFUN of loc * string * expr
          | (* (module ME : S) which is represented as (module (ME : S)) *)
          ExPkg of loc * module_expr
          and module_type =
          | MtNil of loc
          | (* i *)
          (* A.B.C *)
          MtId of loc * ident
          | (* functor (s : mt) -> mt *)
          MtFun of loc * string * module_type * module_type
          | (* 's *)
          MtQuo of loc * string
          | (* sig sg end *)
          MtSig of loc * sig_item
          | (* mt with wc *)
          MtWit of loc * module_type * with_constr
          | (* module type of m *)
          MtOf of loc * module_expr
          | MtAnt of loc * string
          and (* $s$ *)
          sig_item =
          | SgNil of loc
          | (* class cict *)
          SgCls of loc * class_type
          | (* class type cict *)
          SgClt of loc * class_type
          | (* sg ; sg *)
          SgSem of loc * sig_item * sig_item
          | (* # s or # s e *)
          SgDir of loc * string * expr
          | (* exception t *)
          SgExc of loc * ctyp
          | (* external s : t = s ... s *)
          SgExt of loc * string * ctyp * string meta_list
          | (* include mt *)
          SgInc of loc * module_type
          | (* module s : mt *)
          SgMod of loc * string * module_type
          | (* module rec mb *)
          SgRecMod of loc * module_binding
          | (* module type s = mt *)
          SgMty of loc * string * module_type
          | (* open i *)
          SgOpn of loc * ident
          | (* type t *)
          SgTyp of loc * ctyp
          | (* value s : t *)
          SgVal of loc * string * ctyp
          | SgAnt of loc * string
          and (* $s$ *)
          with_constr =
          | WcNil of loc
          | (* type t = t *)
          WcTyp of loc * ctyp * ctyp
          | (* module i = i *)
          WcMod of loc * ident * ident
          | (* type t := t *)
          WcTyS of loc * ctyp * ctyp
          | (* module i := i *)
          WcMoS of loc * ident * ident
          | (* wc and wc *)
          WcAnd of loc * with_constr * with_constr
          | WcAnt of loc * string
          and (* $s$ *)
          binding =
          | BiNil of loc
          | (* bi and bi *)
          (* let a = 42 and c = 43 *)
          BiAnd of loc * binding * binding
          | (* p = e *)
          (* let patt = expr *)
          BiEq of loc * patt * expr
          | BiAnt of loc * string
          and (* $s$ *)
          rec_binding =
          | RbNil of loc
          | (* rb ; rb *)
          RbSem of loc * rec_binding * rec_binding
          | (* i = e *)
          RbEq of loc * ident * expr
          | RbAnt of loc * string
          and (* $s$ *)
          module_binding =
          | MbNil of loc
          | (* mb and mb *)
          (* module rec (s : mt) = me and (s : mt) = me *)
          MbAnd of loc * module_binding * module_binding
          | (* s : mt = me *)
          MbColEq of loc * string * module_type * module_expr
          | (* s : mt *)
          MbCol of loc * string * module_type
          | MbAnt of loc * string
          and (* $s$ *)
          match_case =
          | McNil of loc
          | (* a | a *)
          McOr of loc * match_case * match_case
          | (* p (when e)? -> e *)
          McArr of loc * patt * expr * expr
          | McAnt of loc * string
          and (* $s$ *)
          module_expr =
          | MeNil of loc
          | (* i *)
          MeId of loc * ident
          | (* me me *)
          MeApp of loc * module_expr * module_expr
          | (* functor (s : mt) -> me *)
          MeFun of loc * string * module_type * module_expr
          | (* struct st end *)
          MeStr of loc * str_item
          | (* (me : mt) *)
          MeTyc of loc * module_expr * module_type
          | (* (value e) *)
          (* (value e : S) which is represented as (value (e : S)) *)
          MePkg of loc * expr
          | MeAnt of loc * string
          and (* $s$ *)
          str_item =
          | StNil of loc
          | (* class cice *)
          StCls of loc * class_expr
          | (* class type cict *)
          StClt of loc * class_type
          | (* st ; st *)
          StSem of loc * str_item * str_item
          | (* # s or # s e *)
          StDir of loc * string * expr
          | (* exception t or exception t = i *)
          StExc of loc * ctyp * (*FIXME*) ident meta_option
          | (* e *)
          StExp of loc * expr
          | (* external s : t = s ... s *)
          StExt of loc * string * ctyp * string meta_list
          | (* include me *)
          StInc of loc * module_expr
          | (* module s = me *)
          StMod of loc * string * module_expr
          | (* module rec mb *)
          StRecMod of loc * module_binding
          | (* module type s = mt *)
          StMty of loc * string * module_type
          | (* open i *)
          StOpn of loc * ident
          | (* type t *)
          StTyp of loc * ctyp
          | (* value (rec)? bi *)
          StVal of loc * rec_flag * binding
          | StAnt of loc * string
          and (* $s$ *)
          class_type =
          | CtNil of loc
          | (* (virtual)? i ([ t ])? *)
          CtCon of loc * virtual_flag * ident * ctyp
          | (* [t] -> ct *)
          CtFun of loc * ctyp * class_type
          | (* object ((t))? (csg)? end *)
          CtSig of loc * ctyp * class_sig_item
          | (* ct and ct *)
          CtAnd of loc * class_type * class_type
          | (* ct : ct *)
          CtCol of loc * class_type * class_type
          | (* ct = ct *)
          CtEq of loc * class_type * class_type
          | (* $s$ *)
          CtAnt of loc * string
          and class_sig_item =
          | CgNil of loc
          | (* type t = t *)
          CgCtr of loc * ctyp * ctyp
          | (* csg ; csg *)
          CgSem of loc * class_sig_item * class_sig_item
          | (* inherit ct *)
          CgInh of loc * class_type
          | (* method s : t or method private s : t *)
          CgMth of loc * string * private_flag * ctyp
          | (* value (virtual)? (mutable)? s : t *)
          CgVal of loc * string * mutable_flag * virtual_flag * ctyp
          | (* method virtual (private)? s : t *)
          CgVir of loc * string * private_flag * ctyp
          | CgAnt of loc * string
          and (* $s$ *)
          class_expr =
          | CeNil of loc
          | (* ce e *)
          CeApp of loc * class_expr * expr
          | (* (virtual)? i ([ t ])? *)
          CeCon of loc * virtual_flag * ident * ctyp
          | (* fun p -> ce *)
          CeFun of loc * patt * class_expr
          | (* let (rec)? bi in ce *)
          CeLet of loc * rec_flag * binding * class_expr
          | (* object ((p))? (cst)? end *)
          CeStr of loc * patt * class_str_item
          | (* ce : ct *)
          CeTyc of loc * class_expr * class_type
          | (* ce and ce *)
          CeAnd of loc * class_expr * class_expr
          | (* ce = ce *)
          CeEq of loc * class_expr * class_expr
          | (* $s$ *)
          CeAnt of loc * string
          and class_str_item =
          | CrNil of loc
          | (* cst ; cst *)
          CrSem of loc * class_str_item * class_str_item
          | (* type t = t *)
          CrCtr of loc * ctyp * ctyp
          | (* inherit(!)? ce (as s)? *)
          CrInh of loc * override_flag * class_expr * string
          | (* initializer e *)
          CrIni of loc * expr
          | (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
          CrMth of loc * string * override_flag * private_flag * expr * ctyp
          | (* value(!)? (mutable)? s = e *)
          CrVal of loc * string * override_flag * mutable_flag * expr
          | (* method virtual (private)? s : t *)
          CrVir of loc * string * private_flag * ctyp
          | (* value virtual (mutable)? s : t *)
          CrVvr of loc * string * mutable_flag * ctyp
          | CrAnt of loc * string

end
  include Ast              

            external loc_of_ctyp : ctyp -> Loc.t = "%field0"
              
            external loc_of_patt : patt -> Loc.t = "%field0"
              
            external loc_of_expr : expr -> Loc.t = "%field0"
              
            external loc_of_module_type : module_type -> Loc.t = "%field0"
              
            external loc_of_module_expr : module_expr -> Loc.t = "%field0"
              
            external loc_of_sig_item : sig_item -> Loc.t = "%field0"
              
            external loc_of_str_item : str_item -> Loc.t = "%field0"
              
            external loc_of_class_type : class_type -> Loc.t = "%field0"
              
            external loc_of_class_sig_item : class_sig_item -> Loc.t =
              "%field0"
              
            external loc_of_class_expr : class_expr -> Loc.t = "%field0"
              
            external loc_of_class_str_item : class_str_item -> Loc.t =
              "%field0"
              
            external loc_of_with_constr : with_constr -> Loc.t = "%field0"
              
            external loc_of_binding : binding -> Loc.t = "%field0"
              
            external loc_of_rec_binding : rec_binding -> Loc.t = "%field0"
              
            external loc_of_module_binding : module_binding -> Loc.t =
              "%field0"
              
            external loc_of_match_case : match_case -> Loc.t = "%field0"
              
            external loc_of_ident : ident -> Loc.t = "%field0"
              
            let ghost = Loc.ghost

            let rec is_module_longident =
              function
              | Ast.IdAcc (_, _, i) -> is_module_longident i
              | Ast.IdApp (_, i1, i2) ->
                  (is_module_longident i1) && (is_module_longident i2)
              | Ast.IdUid (_, _) -> true
              | _ -> false
              
            let ident_of_expr =
              let error () =
                invalid_arg
                  "ident_of_expr: this expression is not an identifier" in
              let rec self =
                function
                | Ast.ExApp (_loc, e1, e2) ->
                    Ast.IdApp (_loc, (self e1), (self e2))
                | Ast.ExAcc (_loc, e1, e2) ->
                    Ast.IdAcc (_loc, (self e1), (self e2))
                | Ast.ExId (_, (Ast.IdLid (_, _))) -> error ()
                | Ast.ExId (_, i) ->
                    if is_module_longident i then i else error ()
                | _ -> error ()
              in
                function
                | Ast.ExId (_, i) -> i
                | Ast.ExApp (_, _, _) -> error ()
                | t -> self t
              
            let ident_of_ctyp =
              let error () =
                invalid_arg "ident_of_ctyp: this type is not an identifier" in
              let rec self =
                function
                | Ast.TyApp (_loc, t1, t2) ->
                    Ast.IdApp (_loc, (self t1), (self t2))
                | Ast.TyId (_, (Ast.IdLid (_, _))) -> error ()
                | Ast.TyId (_, i) ->
                    if is_module_longident i then i else error ()
                | _ -> error ()
              in function | Ast.TyId (_, i) -> i | t -> self t
              
            let ident_of_patt =
              let error () =
                invalid_arg
                  "ident_of_patt: this pattern is not an identifier" in
              let rec self =
                function
                | Ast.PaApp (_loc, p1, p2) ->
                    Ast.IdApp (_loc, (self p1), (self p2))
                | Ast.PaId (_, (Ast.IdLid (_, _))) -> error ()
                | Ast.PaId (_, i) ->
                    if is_module_longident i then i else error ()
                | _ -> error ()
              in function | Ast.PaId (_, i) -> i | p -> self p
              
            let rec is_irrefut_patt =
              function
              | Ast.PaId (_, (Ast.IdLid (_, _))) -> true
              | Ast.PaId (_, (Ast.IdUid (_, "()"))) -> true
              | Ast.PaAny _ -> true
              | Ast.PaNil _ -> true
              | Ast.PaAli (_, x, y) ->
                  (is_irrefut_patt x) && (is_irrefut_patt y)
              | Ast.PaRec (_, p) -> is_irrefut_patt p
              | Ast.PaEq (_, _, p) -> is_irrefut_patt p
              | Ast.PaSem (_, p1, p2) ->
                  (is_irrefut_patt p1) && (is_irrefut_patt p2)
              | Ast.PaCom (_, p1, p2) ->
                  (is_irrefut_patt p1) && (is_irrefut_patt p2)
              | Ast.PaOrp (_, p1, p2) ->
                  (is_irrefut_patt p1) && (is_irrefut_patt p2)
              | Ast.PaApp (_, p1, p2) ->
                  (is_irrefut_patt p1) && (is_irrefut_patt p2)
              | Ast.PaTyc (_, p, _) -> is_irrefut_patt p
              | Ast.PaTup (_, pl) -> is_irrefut_patt pl
              | Ast.PaOlb (_, _, (Ast.PaNil _)) -> true
              | Ast.PaOlb (_, _, p) -> is_irrefut_patt p
              | Ast.PaOlbi (_, _, p, _) -> is_irrefut_patt p
              | Ast.PaLab (_, _, (Ast.PaNil _)) -> true
              | Ast.PaLab (_, _, p) -> is_irrefut_patt p
              | Ast.PaLaz (_, p) -> is_irrefut_patt p
              | Ast.PaId (_, _) -> false
              | Ast.PaVrn (_, _) | Ast.PaStr (_, _) | Ast.PaRng (_, _, _) |
                  Ast.PaFlo (_, _) | Ast.PaNativeInt (_, _) |
                  Ast.PaInt64 (_, _) | Ast.PaInt32 (_, _) | Ast.PaInt (_, _)
                  | Ast.PaChr (_, _) | Ast.PaTyp (_, _) | Ast.PaArr (_, _) |
                  Ast.PaAnt (_, _) -> false
              
            let rec is_constructor =
              function
              | Ast.IdAcc (_, _, i) -> is_constructor i
              | Ast.IdUid (_, _) -> true
              | Ast.IdLid (_, _) | Ast.IdApp (_, _, _) -> false
              | Ast.IdAnt (_, _) -> assert false
              
            let is_patt_constructor =
              function
              | Ast.PaId (_, i) -> is_constructor i
              | Ast.PaVrn (_, _) -> true
              | _ -> false
              
            let rec is_expr_constructor =
              function
              | Ast.ExId (_, i) -> is_constructor i
              | Ast.ExAcc (_, e1, e2) ->
                  (is_expr_constructor e1) && (is_expr_constructor e2)
              | Ast.ExVrn (_, _) -> true
              | _ -> false
              
            let rec tyOr_of_list =
              function
              | [] -> Ast.TyNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_ctyp t
                  in Ast.TyOr (_loc, t, (tyOr_of_list ts))
              
            let rec tyAnd_of_list =
              function
              | [] -> Ast.TyNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_ctyp t
                  in Ast.TyAnd (_loc, t, (tyAnd_of_list ts))
              
            let rec tySem_of_list =
              function
              | [] -> Ast.TyNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_ctyp t
                  in Ast.TySem (_loc, t, (tySem_of_list ts))
              
            let rec tyCom_of_list =
              function
              | [] -> Ast.TyNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_ctyp t
                  in Ast.TyCom (_loc, t, (tyCom_of_list ts))
              
            let rec tyAmp_of_list =
              function
              | [] -> Ast.TyNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_ctyp t
                  in Ast.TyAmp (_loc, t, (tyAmp_of_list ts))
              
            let rec tySta_of_list =
              function
              | [] -> Ast.TyNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_ctyp t
                  in Ast.TySta (_loc, t, (tySta_of_list ts))
              
            let rec stSem_of_list =
              function
              | [] -> Ast.StNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_str_item t
                  in Ast.StSem (_loc, t, (stSem_of_list ts))
              
            let rec sgSem_of_list =
              function
              | [] -> Ast.SgNil ghost
              | [ t ] -> t
              | t :: ts ->
                  let _loc = loc_of_sig_item t
                  in Ast.SgSem (_loc, t, (sgSem_of_list ts))
              
            let rec biAnd_of_list =
              function
              | [] -> Ast.BiNil ghost
              | [ b ] -> b
              | b :: bs ->
                  let _loc = loc_of_binding b
                  in Ast.BiAnd (_loc, b, (biAnd_of_list bs))
              
            let rec rbSem_of_list =
              function
              | [] -> Ast.RbNil ghost
              | [ b ] -> b
              | b :: bs ->
                  let _loc = loc_of_rec_binding b
                  in Ast.RbSem (_loc, b, (rbSem_of_list bs))
              
            let rec wcAnd_of_list =
              function
              | [] -> Ast.WcNil ghost
              | [ w ] -> w
              | w :: ws ->
                  let _loc = loc_of_with_constr w
                  in Ast.WcAnd (_loc, w, (wcAnd_of_list ws))
              
            let rec idAcc_of_list =
              function
              | [] -> assert false
              | [ i ] -> i
              | i :: is ->
                  let _loc = loc_of_ident i
                  in Ast.IdAcc (_loc, i, (idAcc_of_list is))
              
            let rec idApp_of_list =
              function
              | [] -> assert false
              | [ i ] -> i
              | i :: is ->
                  let _loc = loc_of_ident i
                  in Ast.IdApp (_loc, i, (idApp_of_list is))
              
            let rec mcOr_of_list =
              function
              | [] -> Ast.McNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_match_case x
                  in Ast.McOr (_loc, x, (mcOr_of_list xs))
              
            let rec mbAnd_of_list =
              function
              | [] -> Ast.MbNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_module_binding x
                  in Ast.MbAnd (_loc, x, (mbAnd_of_list xs))
              
            let rec meApp_of_list =
              function
              | [] -> assert false
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_module_expr x
                  in Ast.MeApp (_loc, x, (meApp_of_list xs))
              
            let rec ceAnd_of_list =
              function
              | [] -> Ast.CeNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_class_expr x
                  in Ast.CeAnd (_loc, x, (ceAnd_of_list xs))
              
            let rec ctAnd_of_list =
              function
              | [] -> Ast.CtNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_class_type x
                  in Ast.CtAnd (_loc, x, (ctAnd_of_list xs))
              
            let rec cgSem_of_list =
              function
              | [] -> Ast.CgNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_class_sig_item x
                  in Ast.CgSem (_loc, x, (cgSem_of_list xs))
              
            let rec crSem_of_list =
              function
              | [] -> Ast.CrNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_class_str_item x
                  in Ast.CrSem (_loc, x, (crSem_of_list xs))
              
            let rec paSem_of_list =
              function
              | [] -> Ast.PaNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_patt x
                  in Ast.PaSem (_loc, x, (paSem_of_list xs))
              
            let rec paCom_of_list =
              function
              | [] -> Ast.PaNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_patt x
                  in Ast.PaCom (_loc, x, (paCom_of_list xs))
              
            let rec exSem_of_list =
              function
              | [] -> Ast.ExNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_expr x
                  in Ast.ExSem (_loc, x, (exSem_of_list xs))
              
            let rec exCom_of_list =
              function
              | [] -> Ast.ExNil ghost
              | [ x ] -> x
              | x :: xs ->
                  let _loc = loc_of_expr x
                  in Ast.ExCom (_loc, x, (exCom_of_list xs))
              
            let ty_of_stl =
              function
              | (_loc, s, []) -> Ast.TyId (_loc, (Ast.IdUid (_loc, s)))
              | (_loc, s, tl) ->
                  Ast.TyOf (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))),
                    (tyAnd_of_list tl))
              
            let ty_of_sbt =
              function
              | (_loc, s, true, t) ->
                  Ast.TyCol (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))),
                    (Ast.TyMut (_loc, t)))
              | (_loc, s, false, t) ->
                  Ast.TyCol (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))),
                    t)
              
            let bi_of_pe (p, e) =
              let _loc = loc_of_patt p in Ast.BiEq (_loc, p, e)
              
            let sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l)
              
            let record_type_of_list l = tySem_of_list (List.map ty_of_sbt l)
              
            let binding_of_pel l = biAnd_of_list (List.map bi_of_pe l)
              
            let rec pel_of_binding =
              function
              | Ast.BiAnd (_, b1, b2) ->
                  (pel_of_binding b1) @ (pel_of_binding b2)
              | Ast.BiEq (_, p, e) -> [ (p, e) ]
              | _ -> assert false
              
            let rec list_of_binding x acc =
              match x with
              | Ast.BiAnd (_, b1, b2) ->
                  list_of_binding b1 (list_of_binding b2 acc)
              | t -> t :: acc
              
            let rec list_of_rec_binding x acc =
              match x with
              | Ast.RbSem (_, b1, b2) ->
                  list_of_rec_binding b1 (list_of_rec_binding b2 acc)
              | t -> t :: acc
              
            let rec list_of_with_constr x acc =
              match x with
              | Ast.WcAnd (_, w1, w2) ->
                  list_of_with_constr w1 (list_of_with_constr w2 acc)
              | t -> t :: acc
              
            let rec list_of_ctyp x acc =
              match x with
              | Ast.TyNil _ -> acc
              | Ast.TyAmp (_, x, y) | Ast.TyCom (_, x, y) |
                  Ast.TySta (_, x, y) | Ast.TySem (_, x, y) |
                  Ast.TyAnd (_, x, y) | Ast.TyOr (_, x, y) ->
                  list_of_ctyp x (list_of_ctyp y acc)
              | x -> x :: acc
              
            let rec list_of_patt x acc =
              match x with
              | Ast.PaNil _ -> acc
              | Ast.PaCom (_, x, y) | Ast.PaSem (_, x, y) ->
                  list_of_patt x (list_of_patt y acc)
              | x -> x :: acc
              
            let rec list_of_expr x acc =
              match x with
              | Ast.ExNil _ -> acc
              | Ast.ExCom (_, x, y) | Ast.ExSem (_, x, y) ->
                  list_of_expr x (list_of_expr y acc)
              | x -> x :: acc
              
            let rec list_of_str_item x acc =
              match x with
              | Ast.StNil _ -> acc
              | Ast.StSem (_, x, y) ->
                  list_of_str_item x (list_of_str_item y acc)
              | x -> x :: acc
              
            let rec list_of_sig_item x acc =
              match x with
              | Ast.SgNil _ -> acc
              | Ast.SgSem (_, x, y) ->
                  list_of_sig_item x (list_of_sig_item y acc)
              | x -> x :: acc
              
            let rec list_of_class_sig_item x acc =
              match x with
              | Ast.CgNil _ -> acc
              | Ast.CgSem (_, x, y) ->
                  list_of_class_sig_item x (list_of_class_sig_item y acc)
              | x -> x :: acc
              
            let rec list_of_class_str_item x acc =
              match x with
              | Ast.CrNil _ -> acc
              | Ast.CrSem (_, x, y) ->
                  list_of_class_str_item x (list_of_class_str_item y acc)
              | x -> x :: acc
              
            let rec list_of_class_type x acc =
              match x with
              | Ast.CtAnd (_, x, y) ->
                  list_of_class_type x (list_of_class_type y acc)
              | x -> x :: acc
              
            let rec list_of_class_expr x acc =
              match x with
              | Ast.CeAnd (_, x, y) ->
                  list_of_class_expr x (list_of_class_expr y acc)
              | x -> x :: acc
              
            let rec list_of_module_expr x acc =
              match x with
              | Ast.MeApp (_, x, y) ->
                  list_of_module_expr x (list_of_module_expr y acc)
              | x -> x :: acc
              
            let rec list_of_match_case x acc =
              match x with
              | Ast.McNil _ -> acc
              | Ast.McOr (_, x, y) ->
                  list_of_match_case x (list_of_match_case y acc)
              | x -> x :: acc
              
            let rec list_of_ident x acc =
              match x with
              | Ast.IdAcc (_, x, y) | Ast.IdApp (_, x, y) ->
                  list_of_ident x (list_of_ident y acc)
              | x -> x :: acc
              
            let rec list_of_module_binding x acc =
              match x with
              | Ast.MbAnd (_, x, y) ->
                  list_of_module_binding x (list_of_module_binding y acc)
              | x -> x :: acc
  
 end

    type quotation =
      { q_name : string; q_loc : string; q_shift : int; q_contents : string
      }

    type camlp4_token =
      | KEYWORD of string
      | SYMBOL of string
      | LIDENT of string
      | UIDENT of string
      | ESCAPED_IDENT of string
      | INT of int * string
      | INT32 of int32 * string
      | INT64 of int64 * string
      | NATIVEINT of nativeint * string
      | FLOAT of float * string
      | CHAR of char * string
      | STRING of string * string
      | LABEL of string
      | OPTLABEL of string
      | QUOTATION of quotation
      | ANTIQUOT of string * string
      | COMMENT of string
      | BLANKS of string
      | NEWLINE
      | LINE_DIRECTIVE of int * string option
      | EOI

(*    
    module type Camlp4Token = Token with type t = camlp4_token
*)
    module Token = struct
        open Format
            type t = camlp4_token
            
            type token = t
            
            let to_string =
              function
              | KEYWORD s -> sprintf "KEYWORD %S" s
              | SYMBOL s -> sprintf "SYMBOL %S" s
              | LIDENT s -> sprintf "LIDENT %S" s
              | UIDENT s -> sprintf "UIDENT %S" s
              | INT (_, s) -> sprintf "INT %s" s
              | INT32 (_, s) -> sprintf "INT32 %sd" s
              | INT64 (_, s) -> sprintf "INT64 %sd" s
              | NATIVEINT (_, s) -> sprintf "NATIVEINT %sd" s
              | FLOAT (_, s) -> sprintf "FLOAT %s" s
              | CHAR (_, s) -> sprintf "CHAR '%s'" s
              | STRING (_, s) -> sprintf "STRING \"%s\"" s
              | LABEL s -> sprintf "LABEL %S" s
              | OPTLABEL s -> sprintf "OPTLABEL %S" s
              | ANTIQUOT (n, s) -> sprintf "ANTIQUOT %s: %S" n s
              | QUOTATION x ->
                  sprintf
                    "QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
                    x.q_name x.q_loc x.q_shift x.q_contents
              | COMMENT s -> sprintf "COMMENT %S" s
              | BLANKS s -> sprintf "BLANKS %S" s
              | NEWLINE -> sprintf "NEWLINE"
              | EOI -> sprintf "EOI"
              | ESCAPED_IDENT s -> sprintf "ESCAPED_IDENT %S" s
              | LINE_DIRECTIVE (i, None) -> sprintf "LINE_DIRECTIVE %d" i
              | LINE_DIRECTIVE (i, (Some s)) ->
                  sprintf "LINE_DIRECTIVE %d %S" i s
              
            let print ppf x = pp_print_string ppf (to_string x)
              
            let match_keyword kwd =
              function | KEYWORD kwd' when kwd = kwd' -> true | _ -> false
              
            let extract_string =
              function
              | KEYWORD s | SYMBOL s | LIDENT s | UIDENT s | INT (_, s) |
                  INT32 (_, s) | INT64 (_, s) | NATIVEINT (_, s) |
                  FLOAT (_, s) | CHAR (_, s) | STRING (_, s) | LABEL s |
                  OPTLABEL s | COMMENT s | BLANKS s | ESCAPED_IDENT s -> s
              | tok ->
                  invalid_arg
                    ("Cannot extract a string from this token: " ^
                       (to_string tok))
              
            module Error =
              struct
                type t =
                  | Illegal_token of string
                  | Keyword_as_label of string
                  | Illegal_token_pattern of string * string
                  | Illegal_constructor of string
                
                exception E of t
                  
                let print ppf =
                  function
                  | Illegal_token s -> fprintf ppf "Illegal token (%s)" s
                  | Keyword_as_label kwd ->
                      fprintf ppf
                        "`%s' is a keyword, it cannot be used as label name"
                        kwd
                  | Illegal_token_pattern (p_con, p_prm) ->
                      fprintf ppf "Illegal token pattern: %s %S" p_con p_prm
                  | Illegal_constructor con ->
                      fprintf ppf "Illegal constructor %S" con
                  
                let to_string x =
                  let b = Buffer.create 50 in
                  let () = bprintf b "%a" print x in Buffer.contents b
                  
              end

(*              
            let _ = let module M = ErrorHandler.Register(Error) in ()
*)

(*              
            module Filter =
              struct
                type token_filter = (t, Loc.t) stream_filter
                
                type t =
                  { is_kwd : string -> bool; mutable filter : token_filter
                  }
                
                let err error loc =
                  raise (Loc.Exc_located (loc, (Error.E error)))
                  
                let keyword_conversion tok is_kwd =
                  match tok with
                  | SYMBOL s | LIDENT s | UIDENT s when is_kwd s -> KEYWORD s
                  | ESCAPED_IDENT s -> LIDENT s
                  | _ -> tok
                  
                let check_keyword_as_label tok loc is_kwd =
                  let s =
                    match tok with | LABEL s -> s | OPTLABEL s -> s | _ -> ""
                  in
                    if (s <> "") && (is_kwd s)
                    then err (Error.Keyword_as_label s) loc
                    else ()
                  
                let check_unknown_keywords tok loc =
                  match tok with
                  | SYMBOL s -> err (Error.Illegal_token s) loc
                  | _ -> ()
                  
                let error_no_respect_rules p_con p_prm =
                  raise
                    (Error.E (Error.Illegal_token_pattern (p_con, p_prm)))
                  
                let check_keyword _ = true
                  
                let error_on_unknown_keywords = ref false
                  
                let rec ignore_layout (__strm : _ Stream.t) =
                  match Stream.peek __strm with
                  | Some
                      (((COMMENT _ | BLANKS _ | NEWLINE |
                           LINE_DIRECTIVE (_, _)),
                        _))
                      -> (Stream.junk __strm; ignore_layout __strm)
                  | Some x ->
                      (Stream.junk __strm;
                       let s = __strm
                       in
                         Stream.icons x
                           (Stream.slazy (fun _ -> ignore_layout s)))
                  | _ -> Stream.sempty
                  
                let mk is_kwd = { is_kwd = is_kwd; filter = ignore_layout; }
                  
                let filter x =
                  let f tok loc =
                    let tok = keyword_conversion tok x.is_kwd
                    in
                      (check_keyword_as_label tok loc x.is_kwd;
                       if !error_on_unknown_keywords
                       then check_unknown_keywords tok loc
                       else ();
                       (tok, loc)) in
                  let rec filter (__strm : _ Stream.t) =
                    match Stream.peek __strm with
                    | Some ((tok, loc)) ->
                        (Stream.junk __strm;
                         let s = __strm
                         in
                           Stream.lcons (fun _ -> f tok loc)
                             (Stream.slazy (fun _ -> filter s)))
                    | _ -> Stream.sempty in
                  let rec tracer (__strm : _ Stream.t) =
                    match Stream.peek __strm with
                    | Some (((_tok, _loc) as x)) ->
                        (Stream.junk __strm;
                         let xs = __strm
                         in
                           Stream.icons x (Stream.slazy (fun _ -> tracer xs)))
                    | _ -> Stream.sempty
                  in fun strm -> tracer (x.filter (filter strm))
                  
                let define_filter x f = x.filter <- f x.filter
                  
                let keyword_added _ _ _ = ()
                  
                let keyword_removed _ _ = ()
                  
              end
*)
        module Eval =
          struct
            let valch x = (Char.code x) - (Char.code '0')
              
            let valch_hex x =
              let d = Char.code x
              in
                if d >= 97
                then d - 87
                else if d >= 65 then d - 55 else d - 48
              
            let rec skip_indent (__strm : _ Stream.t) =
              match Stream.peek __strm with
              | Some (' ' | '\t') -> (Stream.junk __strm; skip_indent __strm)
              | _ -> ()
              
            let skip_opt_linefeed (__strm : _ Stream.t) =
              match Stream.peek __strm with
              | Some '\010' -> (Stream.junk __strm; ())
              | _ -> ()
              
            let chr c =
              if (c < 0) || (c > 255)
              then failwith "invalid char token"
              else Char.chr c
              
            let rec backslash (__strm : _ Stream.t) =
              match Stream.peek __strm with
              | Some '\010' -> (Stream.junk __strm; '\010')
              | Some '\013' -> (Stream.junk __strm; '\013')
              | Some 'n' -> (Stream.junk __strm; '\n')
              | Some 'r' -> (Stream.junk __strm; '\r')
              | Some 't' -> (Stream.junk __strm; '\t')
              | Some 'b' -> (Stream.junk __strm; '\b')
              | Some '\\' -> (Stream.junk __strm; '\\')
              | Some '"' -> (Stream.junk __strm; '"')
              | Some '\'' -> (Stream.junk __strm; '\'')
              | Some ' ' -> (Stream.junk __strm; ' ')
              | Some (('0' .. '9' as c1)) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (('0' .. '9' as c2)) ->
                        (Stream.junk __strm;
                         (match Stream.peek __strm with
                          | Some (('0' .. '9' as c3)) ->
                              (Stream.junk __strm;
                               chr
                                 (((100 * (valch c1)) + (10 * (valch c2))) +
                                    (valch c3)))
                          | _ -> raise (Stream.Error "")))
                    | _ -> raise (Stream.Error "")))
              | Some 'x' ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c1)) ->
                        (Stream.junk __strm;
                         (match Stream.peek __strm with
                          | Some
                              (('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c2))
                              ->
                              (Stream.junk __strm;
                               chr ((16 * (valch_hex c1)) + (valch_hex c2)))
                          | _ -> raise (Stream.Error "")))
                    | _ -> raise (Stream.Error "")))
              | _ -> raise Stream.Failure
              
            let rec backslash_in_string strict store (__strm : _ Stream.t) =
              match Stream.peek __strm with
              | Some '\010' -> (Stream.junk __strm; skip_indent __strm)
              | Some '\013' ->
                  (Stream.junk __strm;
                   let s = __strm in (skip_opt_linefeed s; skip_indent s))
              | _ ->
                  (match try Some (backslash __strm)
                         with | Stream.Failure -> None
                   with
                   | Some x -> store x
                   | _ ->
                       (match Stream.peek __strm with
                        | Some c when not strict ->
                            (Stream.junk __strm; store '\\'; store c)
                        | _ -> failwith "invalid string token"))
              
            let char s =
              if (String.length s) = 1
              then s.[0]
              else
                if (String.length s) = 0
                then failwith "invalid char token"
                else
                  (let (__strm : _ Stream.t) = Stream.of_string s
                   in
                     match Stream.peek __strm with
                     | Some '\\' ->
                         (Stream.junk __strm;
                          (try backslash __strm
                           with | Stream.Failure -> raise (Stream.Error "")))
                     | _ -> failwith "invalid char token")
              
            let string ?strict s =
              let buf = Buffer.create 23 in
              let store = Buffer.add_char buf in
              let rec parse (__strm : _ Stream.t) =
                match Stream.peek __strm with
                | Some '\\' ->
                    (Stream.junk __strm;
                     let _ =
                       (try backslash_in_string (strict <> None) store __strm
                        with | Stream.Failure -> raise (Stream.Error ""))
                     in parse __strm)
                | Some c ->
                    (Stream.junk __strm;
                     let s = __strm in (store c; parse s))
                | _ -> Buffer.contents buf
              in parse (Stream.of_string s)
              
          end

end              

module Camlp4Ast2OCamlAst : sig
  open Ast

            val sig_item : sig_item -> Parsetree.signature

            val str_item : str_item -> Parsetree.structure

            val phrase : str_item -> Parsetree.toplevel_phrase

end = struct

            open Format
              
            open Parsetree
              
            open Longident
              
            open Asttypes
              
            open Ast
              
            let constructors_arity () = !Camlp4_config.constructors_arity
              
            let error loc str = Loc.raise loc (Failure str)
              
            let char_of_char_token loc s =
              try Token.Eval.char s
              with | (Failure _ as exn) -> Loc.raise loc exn
              
            let string_of_string_token loc s =
              try Token.Eval.string s
              with | (Failure _ as exn) -> Loc.raise loc exn
              
            let remove_underscores s =
              let l = String.length s in
              let rec remove src dst =
                if src >= l
                then if dst >= l then s else String.sub s 0 dst
                else
                  (match s.[src] with
                   | '_' -> remove (src + 1) dst
                   | c -> (s.[dst] <- c; remove (src + 1) (dst + 1)))
              in remove 0 0
              
            let mkloc = Loc.to_ocaml_location

            let with_loc txt loc = Location.mkloc txt (mkloc loc)
              
            let mkghloc loc = Loc.to_ocaml_location (Loc.ghostify loc)
              
            let mktyp loc d = { ptyp_desc = d; ptyp_loc = mkloc loc; }
              
            let mkpat loc d = { ppat_desc = d; ppat_loc = mkloc loc; }
              
            let mkghpat loc d = { ppat_desc = d; ppat_loc = mkghloc loc; }
              
            let mkexp loc d = { pexp_desc = d; pexp_loc = mkloc loc; }
              
            let mkmty loc d = { pmty_desc = d; pmty_loc = mkloc loc; }
              
            let mksig loc d = { psig_desc = d; psig_loc = mkloc loc; }
              
            let mkmod loc d = { pmod_desc = d; pmod_loc = mkloc loc; }
              
            let mkstr loc d = { pstr_desc = d; pstr_loc = mkloc loc; }
              
            let mkfield loc d = { pfield_desc = d; pfield_loc = mkloc loc; }
              
            let mkcty loc d = { pcty_desc = d; pcty_loc = mkloc loc; }
              
            let mkpcl loc d = { pcl_desc = d; pcl_loc = mkloc loc; }
              
            let mkpolytype t =
              match t.ptyp_desc with
              | Ptyp_poly (_, _) -> t
              | _ -> { (t) with ptyp_desc = Ptyp_poly ([], t); }
              
            let mkvirtual =
              function
              | Ast.ViVirtual -> Virtual
              | Ast.ViNil -> Concrete
              | _ -> assert false
              
            let mkdirection =
              function
              | Ast.DiTo -> Upto
              | Ast.DiDownto -> Downto
              | _ -> assert false
              
            let lident loc s = with_loc (Lident s) loc
              
            let ldot l s = Ldot (l, s)
              
            let lapply l s = Lapply (l, s)
              
            let conv_con =
              let t = Hashtbl.create 73
              in
                (List.iter (fun (s, s') -> Hashtbl.add t s s')
                   [ ("True", "true"); ("False", "false"); (" True", "True");
                     (" False", "False") ];
                 fun s -> try Hashtbl.find t s with | Not_found -> s)
              
            let conv_lab =
              let t = Hashtbl.create 73
              in
                (List.iter (fun (s, s') -> Hashtbl.add t s s')
                   [ ("val", "contents") ];
                 fun s -> try Hashtbl.find t s with | Not_found -> s)
              
            let array_function_noloc str name =
              ldot (Lident str)
                (if !Camlp4_config.unsafe then "unsafe_" ^ name else name)

            let array_function loc str name =
              with_loc (array_function_noloc str name) loc
                
            let mkrf =
              function
                | Ast.ReRecursive -> Recursive
                | Ast.ReNil -> Nonrecursive
                | _ -> assert false
                  
            let mkli loc s x =
              let rec loop f =
                function | i :: il -> loop (ldot (f i)) il | [] -> f s
              in with_loc (loop (function x -> Lident x) x) loc
              
            let rec ctyp_fa al =
              function
                | TyApp (_, f, a) -> ctyp_fa (a :: al) f
                | f -> (f, al)
                  
            let ident_tag ?(conv_lid = fun x -> x) i =
              let rec self i acc =
                match i with
                  | Ast.IdAcc (_, i1, i2) -> self i2 (Some (self i1 acc))
                  | Ast.IdApp (_, i1, i2) ->
                    let i' =
                      Lapply ((fst (self i1 None)), (fst (self i2 None))) in
                    let x =
                      (match acc with
                        | None -> i'
                        | _ ->
                          error (loc_of_ident i) "invalid long identifier")
                    in (x, `app)
                  | Ast.IdUid (_, s) ->
                    let x =
                      (match acc with
                        | None -> Lident s
                        | Some ((acc, (`uident | `app))) -> ldot acc s
                        | _ ->
                          error (loc_of_ident i) "invalid long identifier")
                    in (x, `uident)
                  | Ast.IdLid (_, s) ->
                    let x =
                      (match acc with
                        | None -> Lident (conv_lid s)
                        | Some ((acc, (`uident | `app))) ->
                          ldot acc (conv_lid s)
                        | _ ->
                          error (loc_of_ident i) "invalid long identifier")
                    in (x, `lident)
                  | _ -> error (loc_of_ident i) "invalid long identifier"
              in self i None
              
            let ident_noloc ?conv_lid i = fst (ident_tag ?conv_lid i)

            let ident ?conv_lid loc i =
              with_loc (ident_noloc ?conv_lid i) (loc_of_ident i)

            let long_lident msg loc id =
              match ident_tag id with
                | (i, `lident) -> with_loc i (loc_of_ident id)
                | _ -> error (loc_of_ident id) msg
                  
            let long_type_ident = long_lident "invalid long identifier type"
              
            let long_class_ident = long_lident "invalid class name"
              
            let long_uident_noloc ?(conv_con = fun x -> x) i =
              match ident_tag i with
                | (Ldot (i, s), `uident) -> ldot i (conv_con s)
                | (Lident s, `uident) -> Lident (conv_con s)
                | (i, `app) -> i
                | _ -> error (loc_of_ident i) "uppercase identifier expected"

            let long_uident ?conv_con loc i =
              with_loc (long_uident_noloc ?conv_con i) (loc_of_ident i)
                
            let rec ctyp_long_id_prefix t =
              match t with
                | Ast.TyId (_, i) -> ident_noloc i
                | Ast.TyApp (_, m1, m2) ->
                  let li1 = ctyp_long_id_prefix m1 in
                  let li2 = ctyp_long_id_prefix m2 in Lapply (li1, li2)
                | t -> error (loc_of_ctyp t) "invalid module expression"
                  
            let ctyp_long_id t =
              match t with
                | Ast.TyId (loc, i) -> (false, (long_type_ident loc i))
                | TyApp (loc, _, _) -> error loc "invalid type name"
                | TyCls (loc, i) -> (true, (ident loc i))
                | t -> error (loc_of_ctyp t) "invalid type"
                  
            let rec ty_var_list_of_ctyp =
              function
                | Ast.TyApp (_, t1, t2) ->
                  (ty_var_list_of_ctyp t1) @ (ty_var_list_of_ctyp t2)
                | Ast.TyQuo (_, s) -> [ s ]
                | _ -> assert false
                  
            let rec ctyp =
              function
                | TyId (loc, i) ->
                  let li = long_type_ident loc i
                  in mktyp loc (Ptyp_constr (li, []))
                | TyAli (loc, t1, t2) ->
                  let (t, i) =
                    (match (t1, t2) with
                      | (t, TyQuo (_, s)) -> (t, s)
                      | (TyQuo (_, s), t) -> (t, s)
                      | _ -> error loc "invalid alias type")
                  in mktyp loc (Ptyp_alias ((ctyp t), i))
                | TyAny loc -> mktyp loc Ptyp_any
                | (TyApp (loc, _, _) as f) ->
                  let (f, al) = ctyp_fa [] f in
                  let (is_cls, li) = ctyp_long_id f
                  in
                  if is_cls
                  then mktyp loc (Ptyp_class (li, (List.map ctyp al), []))
                  else mktyp loc (Ptyp_constr (li, (List.map ctyp al)))
                | TyArr (loc, (TyLab (_, lab, t1)), t2) ->
                  mktyp loc (Ptyp_arrow (lab, (ctyp t1), (ctyp t2)))
                | TyArr (loc, (TyOlb (loc1, lab, t1)), t2) ->
                  let t1 =
                    TyApp (loc1,
                           (Ast.TyId (loc1, (Ast.IdLid (loc1, "option")))), t1)
                  in
                  mktyp loc
                    (Ptyp_arrow (("?" ^ lab), (ctyp t1), (ctyp t2)))
                | TyArr (loc, t1, t2) ->
                  mktyp loc (Ptyp_arrow ("", (ctyp t1), (ctyp t2)))
                | Ast.TyObj (loc, fl, Ast.RvNil) ->
                  mktyp loc (Ptyp_object (meth_list fl []))
                | Ast.TyObj (loc, fl, Ast.RvRowVar) ->
                  mktyp loc
                    (Ptyp_object (meth_list fl [ mkfield loc Pfield_var ]))
                | TyCls (loc, id) ->
                  mktyp loc (Ptyp_class ((ident loc id), [], []))
                | Ast.TyPkg (loc, pt) ->
                  let (i, cs) = package_type pt
                  in mktyp loc (Ptyp_package (i, cs))
                | TyLab (loc, _, _) ->
                  error loc "labelled type not allowed here"
                | TyMan (loc, _, _) ->
                  error loc "manifest type not allowed here"
                | TyOlb (loc, _, _) ->
                  error loc "labelled type not allowed here"
                | TyPol (loc, t1, t2) ->
                  mktyp loc (Ptyp_poly ((ty_var_list_of_ctyp t1), (ctyp t2)))
                | TyQuo (loc, s) -> mktyp loc (Ptyp_var s)
                | TyRec (loc, _) -> error loc "record type not allowed here"
                | TySum (loc, _) -> error loc "sum type not allowed here"
                | TyPrv (loc, _) -> error loc "private type not allowed here"
                | TyMut (loc, _) -> error loc "mutable type not allowed here"
                | TyOr (loc, _, _) ->
                  error loc "type1 | type2 not allowed here"
                | TyAnd (loc, _, _) ->
                  error loc "type1 and type2 not allowed here"
                | TyOf (loc, _, _) ->
                  error loc "type1 of type2 not allowed here"
                | TyCol (loc, _, _) ->
                  error loc "type1 : type2 not allowed here"
                | TySem (loc, _, _) ->
                  error loc "type1 ; type2 not allowed here"
                | Ast.TyTup (loc, (Ast.TySta (_, t1, t2))) ->
                  mktyp loc
                    (Ptyp_tuple
                       (List.map ctyp (list_of_ctyp t1 (list_of_ctyp t2 []))))
                | Ast.TyVrnEq (loc, t) ->
                  mktyp loc (Ptyp_variant ((row_field t), true, None))
                | Ast.TyVrnSup (loc, t) ->
                  mktyp loc (Ptyp_variant ((row_field t), false, None))
              | Ast.TyVrnInf (loc, t) ->
                  mktyp loc (Ptyp_variant ((row_field t), true, (Some [])))
              | Ast.TyVrnInfSup (loc, t, t') ->
                  mktyp loc
                    (Ptyp_variant ((row_field t), true,
                       (Some (name_tags t'))))
              | TyAnt (loc, _) -> error loc "antiquotation not allowed here"
              | TyOfAmp (_, _, _) | TyAmp (_, _, _) | TySta (_, _, _) |
                  TyCom (_, _, _) | TyVrn (_, _) | TyQuM (_, _) |
                  TyQuP (_, _) | TyDcl (_, _, _, _, _) |
                  TyObj (_, _, (RvAnt _)) | TyNil _ | TyTup (_, _) ->
                  assert false
            and row_field =
              function
              | Ast.TyNil _ -> []
              | Ast.TyVrn (_, i) -> [ Rtag (i, true, []) ]
              | Ast.TyOfAmp (_, (Ast.TyVrn (_, i)), t) ->
                  [ Rtag (i, true, (List.map ctyp (list_of_ctyp t []))) ]
              | Ast.TyOf (_, (Ast.TyVrn (_, i)), t) ->
                  [ Rtag (i, false, (List.map ctyp (list_of_ctyp t []))) ]
              | Ast.TyOr (_, t1, t2) -> (row_field t1) @ (row_field t2)
              | t -> [ Rinherit (ctyp t) ]
            and name_tags =
              function
              | Ast.TyApp (_, t1, t2) -> (name_tags t1) @ (name_tags t2)
              | Ast.TyVrn (_, s) -> [ s ]
              | _ -> assert false
            and meth_list fl acc =
              match fl with
              | Ast.TyNil _ -> acc
              | Ast.TySem (_, t1, t2) -> meth_list t1 (meth_list t2 acc)
              | Ast.TyCol (loc, (Ast.TyId (_, (Ast.IdLid (_, lab)))), t) ->
                  (mkfield loc (Pfield (lab, (mkpolytype (ctyp t))))) :: acc
              | _ -> assert false
            and package_type_constraints wc acc =
              match wc with
              | Ast.WcNil _ -> acc
              | Ast.WcTyp (_, (Ast.TyId (_, (Ast.IdLid (loc, id)))), ct) ->
                  (with_loc id loc, (ctyp ct)) :: acc
              | Ast.WcAnd (_, wc1, wc2) ->
                  package_type_constraints wc1
                    (package_type_constraints wc2 acc)
              | _ ->
                  error (loc_of_with_constr wc)
                    "unexpected `with constraint' for a package type"
            and package_type : module_type -> package_type =
              function
              | Ast.MtWit (_, (Ast.MtId (loc, i)), wc) ->
                  ((long_uident loc i), (package_type_constraints wc []))
              | Ast.MtId (loc, i) -> ((long_uident loc i), [])
              | mt -> error (loc_of_module_type mt) "unexpected package type"
              
            let mktype loc tl cl tk tp tm =
              let (params, variance) = List.split tl
              in
                {
                  ptype_params = params;
                  ptype_cstrs = cl;
                  ptype_kind = tk;
                  ptype_private = tp;
                  ptype_manifest = tm;
                  ptype_loc = mkloc loc;
                  ptype_variance = variance;
                }
              
            let mkprivate' m = if m then Private else Public
              
            let mkprivate =
              function
              | Ast.PrPrivate -> Private
              | Ast.PrNil -> Public
              | _ -> assert false
              
            let mktrecord =
              function
              | Ast.TyCol (loc, (Ast.TyId (_, (Ast.IdLid (s_loc, s)))),
                  (Ast.TyMut (_, t))) ->
                  (with_loc s s_loc, Mutable, (mkpolytype (ctyp t)), (mkloc loc))
              | Ast.TyCol (loc, (Ast.TyId (_, (Ast.IdLid (s_loc, s)))), t) ->
                  (with_loc s s_loc, Immutable, (mkpolytype (ctyp t)),
                   (mkloc loc))
              | _ -> assert false
              
            let mkvariant =
              function
              | Ast.TyId (loc, (Ast.IdUid (s_loc, s))) ->
                  (with_loc (conv_con s) s_loc, [], None, (mkloc loc))
              | Ast.TyOf (loc, (Ast.TyId (_, (Ast.IdUid (s_loc, s)))), t) ->
                  (with_loc (conv_con s) s_loc,
                   (List.map ctyp (list_of_ctyp t [])),
                   None,
                   (mkloc loc))
              | _ -> assert false
              
            let rec type_decl tl cl loc m pflag =
              function
              | Ast.TyMan (_, t1, t2) ->
                  type_decl tl cl loc (Some (ctyp t1)) pflag t2
              | Ast.TyPrv (_, t) -> type_decl tl cl loc m true t
              | Ast.TyRec (_, t) ->
                  mktype loc tl cl
                    (Ptype_record (List.map mktrecord (list_of_ctyp t [])))
                    (mkprivate' pflag) m
              | Ast.TySum (_, t) ->
                  mktype loc tl cl
                    (Ptype_variant (List.map mkvariant (list_of_ctyp t [])))
                    (mkprivate' pflag) m
              | t ->
                  if m <> None
                  then
                    error loc "only one manifest type allowed by definition"
                  else
                    (let m =
                       match t with
                       | Ast.TyNil _ -> None
                       | _ -> Some (ctyp t)
                     in mktype loc tl cl Ptype_abstract (mkprivate' pflag) m)
              
            let type_decl tl cl t =
              type_decl tl cl (loc_of_ctyp t) None false t
              
            let mkvalue_desc loc t p =
              { pval_type = ctyp t; pval_prim = p; pval_loc = mkloc loc}
              
            let rec list_of_meta_list =
              function
              | Ast.LNil -> []
              | Ast.LCons (x, xs) -> x :: (list_of_meta_list xs)
              | Ast.LAnt _ -> assert false
              
            let mkmutable =
              function
              | Ast.MuMutable -> Mutable
              | Ast.MuNil -> Immutable
              | _ -> assert false
              
            let paolab lab p =
              match (lab, p) with
              | ("",
                 (Ast.PaId (_, (Ast.IdLid (_, i))) |
                    Ast.PaTyc (_, (Ast.PaId (_, (Ast.IdLid (_, i)))), _)))
                  -> i
              | ("", p) -> error (loc_of_patt p) "bad ast in label"
              | _ -> lab
              
            let opt_private_ctyp =
              function
              | Ast.TyPrv (_, t) -> (Ptype_abstract, Private, (ctyp t))
              | t -> (Ptype_abstract, Public, (ctyp t))
              
            let rec type_parameters t acc =
              match t with
              | Ast.TyApp (_, t1, t2) ->
                  type_parameters t1 (type_parameters t2 acc)
              | Ast.TyQuP (loc, s) ->
                (Some (with_loc s loc), (true, false)) :: acc
              | Ast.TyQuM (loc, s) ->
                (Some (with_loc s loc), (false, true)) :: acc
              | Ast.TyQuo (loc, s) ->
                (Some (with_loc s loc), (false, false)) :: acc
              | _ -> assert false
              
            let rec class_parameters t acc =
              match t with
              | Ast.TyCom (_, t1, t2) ->
                  class_parameters t1 (class_parameters t2 acc)
              | Ast.TyQuP (loc, s) -> (with_loc s loc, (true, false)) :: acc
              | Ast.TyQuM (loc, s) -> (with_loc s loc, (false, true)) :: acc
              | Ast.TyQuo (loc, s) -> (with_loc s loc, (false, false)) :: acc
              | _ -> assert false
              
            let rec type_parameters_and_type_name t acc =
              match t with
              | Ast.TyApp (_, t1, t2) ->
                  type_parameters_and_type_name t1 (type_parameters t2 acc)
              | Ast.TyId (loc, i) -> ((ident loc i), acc)
              | _ -> assert false
              
            let mkwithtyp pwith_type loc id_tpl ct =
              let (id, tpl) = type_parameters_and_type_name id_tpl [] in
              let (params, variance) = List.split tpl in
              let (kind, priv, ct) = opt_private_ctyp ct
              in
                (id,
                 (pwith_type
                    {
                      ptype_params = params;
                      ptype_cstrs = [];
                      ptype_kind = kind;
                      ptype_private = priv;
                      ptype_manifest = Some ct;
                      ptype_loc = mkloc loc;
                      ptype_variance = variance;
                    }))
              
            let rec mkwithc wc acc =
              match wc with
              | Ast.WcNil _ -> acc
              | Ast.WcTyp (loc, id_tpl, ct) ->
                  (mkwithtyp (fun x -> Pwith_type x) loc id_tpl ct) :: acc
              | Ast.WcMod (loc, i1, i2) ->
                  ((long_uident loc i1),
                   (Pwith_module (long_uident loc i2))) :: acc
              | Ast.WcTyS (loc, id_tpl, ct) ->
                  (mkwithtyp (fun x -> Pwith_typesubst x) loc id_tpl ct) ::
                    acc
              | Ast.WcMoS (loc, i1, i2) ->
                  ((long_uident loc i1),
                   (Pwith_modsubst (long_uident loc i2))) ::
                    acc
              | Ast.WcAnd (_, wc1, wc2) -> mkwithc wc1 (mkwithc wc2 acc)
              | Ast.WcAnt (loc, _) ->
                  error loc "bad with constraint (antiquotation)"
              
            let rec patt_fa al =
              function
              | PaApp (_, f, a) -> patt_fa (a :: al) f
              | f -> (f, al)
              
            let rec deep_mkrangepat loc c1 c2 =
              if c1 = c2
              then mkghpat loc (Ppat_constant (Const_char c1))
              else
                mkghpat loc
                  (Ppat_or ((mkghpat loc (Ppat_constant (Const_char c1))),
                     (deep_mkrangepat loc (Char.chr ((Char.code c1) + 1)) c2)))
              
            let rec mkrangepat loc c1 c2 =
              if c1 > c2
              then mkrangepat loc c2 c1
              else
                if c1 = c2
                then mkpat loc (Ppat_constant (Const_char c1))
                else
                  mkpat loc
                    (Ppat_or ((mkghpat loc (Ppat_constant (Const_char c1))),
                       (deep_mkrangepat loc (Char.chr ((Char.code c1) + 1))
                          c2)))
              
            let rec patt =
              function
              | Ast.PaId (loc, (Ast.IdLid (s_loc, s))) ->
                mkpat loc (Ppat_var (with_loc s s_loc))
              | Ast.PaId (loc, i) ->
                  let p =
                    Ppat_construct ((long_uident ~conv_con loc i), None,
                      (constructors_arity ()))
                  in mkpat loc p
              | PaAli (loc, p1, p2) ->
                  let (p, i) =
                    (match (p1, p2) with
                     | (p, Ast.PaId (_, (Ast.IdLid (s_loc, s)))) ->
                       (p, with_loc s s_loc)
                     | (Ast.PaId (_, (Ast.IdLid (s_loc, s))), p) ->
                       (p, with_loc s s_loc)
                     | _ -> error loc "invalid alias pattern")
                  in mkpat loc (Ppat_alias ((patt p), i))
              | PaAnt (loc, _) -> error loc "antiquotation not allowed here"
              | PaAny loc -> mkpat loc Ppat_any
              | Ast.PaApp (loc, (Ast.PaId (_, (Ast.IdUid (s_loc, s)))),
                  (Ast.PaTup (_, (Ast.PaAny loc_any)))) ->
                  mkpat loc
                    (Ppat_construct (lident s_loc (conv_con s),
                       (Some (mkpat loc_any Ppat_any)), false))
              | (PaApp (loc, _, _) as f) ->
                  let (f, al) = patt_fa [] f in
                  let al = List.map patt al
                  in
                    (match (patt f).ppat_desc with
                     | Ppat_construct (li, None, _) ->
                         if constructors_arity ()
                         then
                           mkpat loc
                             (Ppat_construct (li,
                                (Some (mkpat loc (Ppat_tuple al))), true))
                         else
                           (let a =
                              match al with
                              | [ a ] -> a
                              | _ -> mkpat loc (Ppat_tuple al)
                            in
                              mkpat loc
                                (Ppat_construct (li, (Some a), false)))
                     | Ppat_variant (s, None) ->
                         let a =
                           if constructors_arity ()
                           then mkpat loc (Ppat_tuple al)
                           else
                             (match al with
                              | [ a ] -> a
                              | _ -> mkpat loc (Ppat_tuple al))
                         in mkpat loc (Ppat_variant (s, (Some a)))
                     | _ ->
                         error (loc_of_patt f)
                           "this is not a constructor, it cannot be applied in a pattern")
              | PaArr (loc, p) ->
                  mkpat loc (Ppat_array (List.map patt (list_of_patt p [])))
              | PaChr (loc, s) ->
                  mkpat loc
                    (Ppat_constant (Const_char (char_of_char_token loc s)))
              | PaInt (loc, s) ->
                  let i =
                    (try int_of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type int")
                  in mkpat loc (Ppat_constant (Const_int i))
              | PaInt32 (loc, s) ->
                  let i32 =
                    (try Int32.of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type int32")
                  in mkpat loc (Ppat_constant (Const_int32 i32))
              | PaInt64 (loc, s) ->
                  let i64 =
                    (try Int64.of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type int64")
                  in mkpat loc (Ppat_constant (Const_int64 i64))
              | PaNativeInt (loc, s) ->
                  let nati =
                    (try Nativeint.of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type nativeint")
                  in mkpat loc (Ppat_constant (Const_nativeint nati))
              | PaFlo (loc, s) ->
                  mkpat loc
                    (Ppat_constant (Const_float (remove_underscores s)))
              | PaLab (loc, _, _) ->
                  error loc "labeled pattern not allowed here"
              | PaOlb (loc, _, _) | PaOlbi (loc, _, _, _) ->
                  error loc "labeled pattern not allowed here"
              | PaOrp (loc, p1, p2) ->
                  mkpat loc (Ppat_or ((patt p1), (patt p2)))
              | PaRng (loc, p1, p2) ->
                  (match (p1, p2) with
                   | (PaChr (loc1, c1), PaChr (loc2, c2)) ->
                       let c1 = char_of_char_token loc1 c1 in
                       let c2 = char_of_char_token loc2 c2
                       in mkrangepat loc c1 c2
                   | _ ->
                       error loc "range pattern allowed only for characters")
              | PaRec (loc, p) ->
                  let ps = list_of_patt p [] in
                  let is_wildcard =
                    (function | Ast.PaAny _ -> true | _ -> false) in
                  let (wildcards, ps) = List.partition is_wildcard ps in
                  let is_closed = if wildcards = [] then Closed else Open
                  in
                    mkpat loc
                      (Ppat_record (((List.map mklabpat ps), is_closed)))
              | PaStr (loc, s) ->
                  mkpat loc
                    (Ppat_constant
                       (Const_string (string_of_string_token loc s)))
              | Ast.PaTup (loc, (Ast.PaCom (_, p1, p2))) ->
                  mkpat loc
                    (Ppat_tuple
                       (List.map patt (list_of_patt p1 (list_of_patt p2 []))))
              | Ast.PaTup (loc, _) -> error loc "singleton tuple pattern"
              | PaTyc (loc, p, t) ->
                  mkpat loc (Ppat_constraint ((patt p), (ctyp t)))
              | PaTyp (loc, i) -> mkpat loc (Ppat_type (long_type_ident loc i))
              | PaVrn (loc, s) -> mkpat loc (Ppat_variant (s, None))
              | PaLaz (loc, p) -> mkpat loc (Ppat_lazy (patt p))
              | (PaEq (_, _, _) | PaSem (_, _, _) | PaCom (_, _, _) | PaNil _
                 as p) -> error (loc_of_patt p) "invalid pattern"
            and mklabpat =
              function
              | Ast.PaEq (loc, i, p) ->
                  ((ident ~conv_lid: conv_lab loc i), (patt p))
              | p -> error (loc_of_patt p) "invalid pattern"
              
            let rec expr_fa al =
              function
              | ExApp (_, f, a) -> expr_fa (a :: al) f
              | f -> (f, al)
              
            let rec class_expr_fa al =
              function
              | CeApp (_, ce, a) -> class_expr_fa (a :: al) ce
              | ce -> (ce, al)
              
            let rec sep_expr_acc l =
              function
              | ExAcc (_, e1, e2) -> sep_expr_acc (sep_expr_acc l e2) e1
              | (Ast.ExId (loc, (Ast.IdUid (_, s))) as e) ->
                  (match l with
                   | [] -> [ (loc, [], e) ]
                   | (loc', sl, e) :: l ->
                       ((Loc.merge loc loc'), (s :: sl), e) :: l)
              | Ast.ExId (_, ((Ast.IdAcc (_, _, _) as i))) ->
                  let rec normalize_acc =
                    (function
                     | Ast.IdAcc (_loc, i1, i2) ->
                         Ast.ExAcc (_loc, (normalize_acc i1),
                           (normalize_acc i2))
                     | Ast.IdApp (_loc, i1, i2) ->
                         Ast.ExApp (_loc, (normalize_acc i1),
                           (normalize_acc i2))
                     | (Ast.IdAnt (_loc, _) | Ast.IdUid (_loc, _) |
                          Ast.IdLid (_loc, _)
                        as i) -> Ast.ExId (_loc, i))
                  in sep_expr_acc l (normalize_acc i)
              | e -> ((loc_of_expr e), [], e) :: l
              
            let override_flag loc =
              function
              | Ast.OvOverride -> Override
              | Ast.OvNil -> Fresh
              | _ -> error loc "antiquotation not allowed here"
              
            let list_of_opt_ctyp ot acc =
              match ot with | Ast.TyNil _ -> acc | t -> list_of_ctyp t acc
              
            let rec expr =
              function
              | Ast.ExAcc (loc, x, (Ast.ExId (_, (Ast.IdLid (_, "val"))))) ->
                  mkexp loc
                    (Pexp_apply
                       ((mkexp loc (Pexp_ident (lident loc "!"))),
                       [ ("", (expr x)) ]))
              | (ExAcc (loc, _, _) | Ast.ExId (loc, (Ast.IdAcc (_, _, _))) as
                 e) ->
                  let (e, l) =
                    (match sep_expr_acc [] e with
                     | (loc, ml, Ast.ExId (_, (Ast.IdUid (_, s)))) :: l ->
                         let ca = constructors_arity ()
                         in
                           ((mkexp loc
                               (Pexp_construct ((mkli loc (conv_con s) ml), None,
                                  ca))),
                            l)
                     | (loc, ml, Ast.ExId (_, (Ast.IdLid (s_loc, s)))) :: l ->
                         ((mkexp loc (Pexp_ident (mkli s_loc s ml))), l)
                     | (_, [], e) :: l -> ((expr e), l)
                     | _ -> error loc "bad ast in expression") in
                  let (_, e) =
                    List.fold_left
                      (fun (loc_bp, e1) (loc_ep, ml, e2) ->
                         match e2 with
                         | Ast.ExId (_, (Ast.IdLid (s_loc, s))) ->
                             let loc = Loc.merge loc_bp loc_ep
                             in
                               (loc,
                                (mkexp loc
                                   (Pexp_field (e1, (mkli s_loc (conv_lab s) ml)))))
                         | _ ->
                             error (loc_of_expr e2)
                               "lowercase identifier expected")
                      (loc, e) l
                  in e
              | ExAnt (loc, _) -> error loc "antiquotation not allowed here"
              | (ExApp (loc, _, _) as f) ->
                  let (f, al) = expr_fa [] f in
                  let al = List.map label_expr al
                  in
                    (match (expr f).pexp_desc with
                     | Pexp_construct (li, None, _) ->
                         let al = List.map snd al
                         in
                           if constructors_arity ()
                           then
                             mkexp loc
                               (Pexp_construct (li,
                                  (Some (mkexp loc (Pexp_tuple al))), true))
                           else
                             (let a =
                                match al with
                                | [ a ] -> a
                                | _ -> mkexp loc (Pexp_tuple al)
                              in
                                mkexp loc
                                  (Pexp_construct (li, (Some a), false)))
                     | Pexp_variant (s, None) ->
                         let al = List.map snd al in
                         let a =
                           if constructors_arity ()
                           then mkexp loc (Pexp_tuple al)
                           else
                             (match al with
                              | [ a ] -> a
                              | _ -> mkexp loc (Pexp_tuple al))
                         in mkexp loc (Pexp_variant (s, (Some a)))
                     | _ -> mkexp loc (Pexp_apply ((expr f), al)))
              | ExAre (loc, e1, e2) ->
                  mkexp loc
                    (Pexp_apply
                       ((mkexp loc
                           (Pexp_ident (array_function loc "Array" "get"))),
                       [ ("", (expr e1)); ("", (expr e2)) ]))
              | ExArr (loc, e) ->
                  mkexp loc (Pexp_array (List.map expr (list_of_expr e [])))
              | ExAsf loc -> mkexp loc Pexp_assertfalse
              | ExAss (loc, e, v) ->
                  let e =
                    (match e with
                     | Ast.ExAcc (loc, x,
                         (Ast.ExId (_, (Ast.IdLid (_, "val"))))) ->
                         Pexp_apply ((mkexp loc (Pexp_ident (lident loc ":="))),
                           [ ("", (expr x)); ("", (expr v)) ])
                     | ExAcc (loc, _, _) ->
                         (match (expr e).pexp_desc with
                          | Pexp_field (e, lab) ->
                              Pexp_setfield (e, lab, (expr v))
                          | _ -> error loc "bad record access")
                     | ExAre (_, e1, e2) ->
                         Pexp_apply
                           ((mkexp loc
                               (Pexp_ident (array_function loc "Array" "set"))),
                           [ ("", (expr e1)); ("", (expr e2)); ("", (expr v)) ])
                     | Ast.ExId (_, (Ast.IdLid (loc, lab))) ->
                         Pexp_setinstvar (with_loc lab loc, (expr v))
                     | ExSte (_, e1, e2) ->
                         Pexp_apply
                           ((mkexp loc
                               (Pexp_ident (array_function loc "String" "set"))),
                           [ ("", (expr e1)); ("", (expr e2)); ("", (expr v)) ])
                     | _ -> error loc "bad left part of assignment")
                  in mkexp loc e
              | ExAsr (loc, e) -> mkexp loc (Pexp_assert (expr e))
              | ExChr (loc, s) ->
                  mkexp loc
                    (Pexp_constant (Const_char (char_of_char_token loc s)))
              | ExCoe (loc, e, t1, t2) ->
                  let t1 =
                    (match t1 with | Ast.TyNil _ -> None | t -> Some (ctyp t))
                  in
                    mkexp loc
                      (Pexp_constraint ((expr e), t1, (Some (ctyp t2))))
              | ExFlo (loc, s) ->
                  mkexp loc
                    (Pexp_constant (Const_float (remove_underscores s)))
              | ExFor (loc, i, e1, e2, df, el) ->
                  let e3 = ExSeq (loc, el)
                  in
                    mkexp loc
                      (Pexp_for (with_loc i loc, (expr e1), (expr e2), (mkdirection df),
                         (expr e3)))
              | Ast.ExFun (loc, (Ast.McArr (_, (PaLab (_, lab, po)), w, e)))
                  ->
                  mkexp loc
                    (Pexp_function (lab, None,
                       [ ((patt_of_lab loc lab po), (when_expr e w)) ]))
              | Ast.ExFun (loc,
                  (Ast.McArr (_, (PaOlbi (_, lab, p, e1)), w, e2))) ->
                  let lab = paolab lab p
                  in
                    mkexp loc
                      (Pexp_function (("?" ^ lab), (Some (expr e1)),
                         [ ((patt p), (when_expr e2 w)) ]))
              | Ast.ExFun (loc, (Ast.McArr (_, (PaOlb (_, lab, p)), w, e)))
                  ->
                  let lab = paolab lab p
                  in
                    mkexp loc
                      (Pexp_function (("?" ^ lab), None,
                         [ ((patt_of_lab loc lab p), (when_expr e w)) ]))
              | ExFun (loc, a) ->
                  mkexp loc (Pexp_function ("", None, (match_case a [])))
              | ExIfe (loc, e1, e2, e3) ->
                  mkexp loc
                    (Pexp_ifthenelse ((expr e1), (expr e2), (Some (expr e3))))
              | ExInt (loc, s) ->
                  let i =
                    (try int_of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type int")
                  in mkexp loc (Pexp_constant (Const_int i))
              | ExInt32 (loc, s) ->
                  let i32 =
                    (try Int32.of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type int32")
                  in mkexp loc (Pexp_constant (Const_int32 i32))
              | ExInt64 (loc, s) ->
                  let i64 =
                    (try Int64.of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type int64.1")
                  in mkexp loc (Pexp_constant (Const_int64 i64))
              | ExNativeInt (loc, s) ->
                  let nati =
                    (try Nativeint.of_string s
                     with
                     | Failure _ ->
                         error loc
                           "Integer literal exceeds the range of representable integers of type nativeint")
                  in mkexp loc (Pexp_constant (Const_nativeint nati))
              | ExLab (loc, _, _) ->
                  error loc "labeled expression not allowed here"
              | ExLaz (loc, e) -> mkexp loc (Pexp_lazy (expr e))
              | ExLet (loc, rf, bi, e) ->
                  mkexp loc (Pexp_let ((mkrf rf), (binding bi []), (expr e)))
              | ExLmd (loc, i, me, e) ->
                  mkexp loc (Pexp_letmodule
                               (with_loc i loc, (module_expr me), (expr e)))
              | ExMat (loc, e, a) ->
                  mkexp loc (Pexp_match ((expr e), (match_case a [])))
              | ExNew (loc, id) -> mkexp loc (Pexp_new (long_type_ident loc id))
              | ExObj (loc, po, cfl) ->
                  let p =
                    (match po with | Ast.PaNil _ -> Ast.PaAny loc | p -> p) in
                  let cil = class_str_item cfl []
                  in mkexp loc (Pexp_object {
                    pcstr_pat = patt p;
                    pcstr_fields = cil
                  })
              | ExOlb (loc, _, _) ->
                  error loc "labeled expression not allowed here"
              | ExOvr (loc, iel) ->
                  mkexp loc (Pexp_override (mkideexp iel []))
              | ExRec (loc, lel, eo) ->
                  (match lel with
                   | Ast.RbNil _ -> error loc "empty record"
                   | _ ->
                       let eo =
                         (match eo with
                          | Ast.ExNil _ -> None
                          | e -> Some (expr e))
                       in mkexp loc (Pexp_record ((mklabexp lel []), eo)))
              | ExSeq (_loc, e) ->
                  let rec loop =
                    (function
                     | [] -> expr (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))))
                     | [ e ] -> expr e
                     | e :: el ->
                         let _loc = Loc.merge (loc_of_expr e) _loc
                         in mkexp _loc (Pexp_sequence ((expr e), (loop el))))
                  in loop (list_of_expr e [])
              | ExSnd (loc, e, s) -> mkexp loc (Pexp_send ((expr e), s))
              | ExSte (loc, e1, e2) ->
                  mkexp loc
                    (Pexp_apply
                       ((mkexp loc
                           (Pexp_ident (array_function loc "String" "get"))),
                       [ ("", (expr e1)); ("", (expr e2)) ]))
              | ExStr (loc, s) ->
                  mkexp loc
                    (Pexp_constant
                       (Const_string (string_of_string_token loc s)))
              | ExTry (loc, e, a) ->
                  mkexp loc (Pexp_try ((expr e), (match_case a [])))
              | Ast.ExTup (loc, (Ast.ExCom (_, e1, e2))) ->
                  mkexp loc
                    (Pexp_tuple
                       (List.map expr (list_of_expr e1 (list_of_expr e2 []))))
              | Ast.ExTup (loc, _) -> error loc "singleton tuple"
              | ExTyc (loc, e, t) ->
                  mkexp loc
                    (Pexp_constraint ((expr e), (Some (ctyp t)), None))
              | Ast.ExId (loc, (Ast.IdUid (_, "()"))) ->
                  mkexp loc (Pexp_construct ((lident loc "()"), None, true))
              | Ast.ExId (loc, (Ast.IdLid (_, s))) ->
                  mkexp loc (Pexp_ident (lident loc s))
              | Ast.ExId (loc, (Ast.IdUid (_, s))) ->
                  mkexp loc
                    (Pexp_construct ((lident loc (conv_con s)), None, true))
              | ExVrn (loc, s) -> mkexp loc (Pexp_variant (s, None))
              | ExWhi (loc, e1, el) ->
                  let e2 = ExSeq (loc, el)
                  in mkexp loc (Pexp_while ((expr e1), (expr e2)))
              | Ast.ExOpI (loc, i, e) ->
                  mkexp loc (Pexp_open ((long_uident loc i), (expr e)))
              | Ast.ExPkg (loc, (Ast.MeTyc (_, me, pt))) ->
                  mkexp loc
                    (Pexp_constraint
                       (mkexp loc (Pexp_pack (module_expr me)),
                        Some (mktyp loc (Ptyp_package (package_type pt))),
                        None))
              | Ast.ExPkg (loc, _) ->
                  error loc "(module_expr : package_type) expected here"
              | ExFUN (loc, i, e) -> mkexp loc (Pexp_newtype (i, (expr e)))
              | Ast.ExCom (loc, _, _) ->
                  error loc "expr, expr: not allowed here"
              | Ast.ExSem (loc, _, _) ->
                  error loc
                    "expr; expr: not allowed here, use do {...} or [|...|] to surround them"
              | (ExId (_, _) | ExNil _ as e) ->
                  error (loc_of_expr e) "invalid expr"
            and patt_of_lab _loc lab =
              function
              | Ast.PaNil _ ->
                  patt (Ast.PaId (_loc, (Ast.IdLid (_loc, lab))))
              | p -> patt p
            and expr_of_lab _loc lab =
              function
              | Ast.ExNil _ ->
                  expr (Ast.ExId (_loc, (Ast.IdLid (_loc, lab))))
              | e -> expr e
            and label_expr =
              function
              | ExLab (loc, lab, eo) -> (lab, (expr_of_lab loc lab eo))
              | ExOlb (loc, lab, eo) ->
                  (("?" ^ lab), (expr_of_lab loc lab eo))
              | e -> ("", (expr e))
            and binding x acc =
              match x with
              | Ast.BiAnd (_, x, y) -> binding x (binding y acc)
              | Ast.BiEq (_loc, p,
                  (Ast.ExTyc (_, e, (Ast.TyPol (_, vs, ty))))) ->
                  ((patt (Ast.PaTyc (_loc, p, (Ast.TyPol (_loc, vs, ty))))),
                   (expr e)) :: acc
              | Ast.BiEq (_, p, e) -> ((patt p), (expr e)) :: acc
              | Ast.BiNil _ -> acc
              | _ -> assert false
            and match_case x acc =
              match x with
              | Ast.McOr (_, x, y) -> match_case x (match_case y acc)
              | Ast.McArr (_, p, w, e) -> ((patt p), (when_expr e w)) :: acc
              | Ast.McNil _ -> acc
              | _ -> assert false
            and when_expr e w =
              match w with
              | Ast.ExNil _ -> expr e
              | w -> mkexp (loc_of_expr w) (Pexp_when ((expr w), (expr e)))
            and mklabexp x acc =
              match x with
              | Ast.RbSem (_, x, y) -> mklabexp x (mklabexp y acc)
              | Ast.RbEq (loc, i, e) ->
                  ((ident ~conv_lid: conv_lab loc i), (expr e)) :: acc
              | _ -> assert false
            and mkideexp x acc =
              match x with
              | Ast.RbNil _ -> acc
              | Ast.RbSem (_, x, y) -> mkideexp x (mkideexp y acc)
              | Ast.RbEq (_, (Ast.IdLid (s_loc, s)), e) ->
                (with_loc s s_loc, (expr e)) :: acc
              | _ -> assert false
            and mktype_decl x acc =
              match x with
              | Ast.TyAnd (_, x, y) -> mktype_decl x (mktype_decl y acc)
              | Ast.TyDcl (c_loc, c, tl, td, cl) ->
                  let cl =
                    List.map
                      (fun (t1, t2) ->
                         let loc =
                           Loc.merge (loc_of_ctyp t1) (loc_of_ctyp t2)
                         in ((ctyp t1), (ctyp t2), (mkloc loc)))
                      cl
                  in
                    (with_loc c c_loc,
                     (type_decl (List.fold_right type_parameters tl []) cl td)) ::
                      acc
              | _ -> assert false
            and module_type =
              function
              | Ast.MtNil loc ->
                  error loc "abstract/nil module type not allowed here"
              | Ast.MtId (loc, i) -> mkmty loc (Pmty_ident (long_uident loc i))
              | Ast.MtFun (loc, n, nt, mt) ->
                  mkmty loc
                    (Pmty_functor (with_loc n loc, (module_type nt), (module_type mt)))
              | Ast.MtQuo (loc, _) ->
                  error loc "module type variable not allowed here"
              | Ast.MtSig (loc, sl) ->
                  mkmty loc (Pmty_signature (sig_item sl []))
              | Ast.MtWit (loc, mt, wc) ->
                  mkmty loc (Pmty_with ((module_type mt), (mkwithc wc [])))
              | Ast.MtOf (loc, me) ->
                  mkmty loc (Pmty_typeof (module_expr me))
              | Ast.MtAnt (_, _) -> assert false
            and sig_item s l =
              match s with
              | Ast.SgNil _ -> l
              | SgCls (loc, cd) ->
                  (mksig loc
                     (Psig_class
                        (List.map class_info_class_type
                           (list_of_class_type cd [])))) ::
                    l
              | SgClt (loc, ctd) ->
                  (mksig loc
                     (Psig_class_type
                        (List.map class_info_class_type
                           (list_of_class_type ctd [])))) ::
                    l
              | Ast.SgSem (_, sg1, sg2) -> sig_item sg1 (sig_item sg2 l)
              | SgDir (_, _, _) -> l
              | Ast.SgExc (loc, (Ast.TyId (_, (Ast.IdUid (_, s))))) ->
                  (mksig loc (Psig_exception (with_loc (conv_con s) loc, [])))
                :: l
              | Ast.SgExc (loc,
                  (Ast.TyOf (_, (Ast.TyId (_, (Ast.IdUid (_, s)))), t))) ->
                  (mksig loc
                     (Psig_exception (with_loc (conv_con s) loc,
                        (List.map ctyp (list_of_ctyp t []))))) ::
                    l
              | SgExc (_, _) -> assert false
              | SgExt (loc, n, t, sl) ->
                  (mksig loc
                     (Psig_value
                        (with_loc n loc,
                         (mkvalue_desc loc t (list_of_meta_list sl))))) ::
                    l
              | SgInc (loc, mt) ->
                  (mksig loc (Psig_include (module_type mt))) :: l
              | SgMod (loc, n, mt) ->
                  (mksig loc
                     (Psig_module (with_loc n loc, (module_type mt)))) :: l
              | SgRecMod (loc, mb) ->
                  (mksig loc (Psig_recmodule (module_sig_binding mb []))) ::
                    l
              | SgMty (loc, n, mt) ->
                  let si =
                    (match mt with
                     | MtQuo (_, _) -> Pmodtype_abstract
                     | _ -> Pmodtype_manifest (module_type mt))
                  in (mksig loc (Psig_modtype (with_loc n loc, si))) :: l
              | SgOpn (loc, id) ->
                  (mksig loc (Psig_open (long_uident loc id))) :: l
              | SgTyp (loc, tdl) ->
                  (mksig loc (Psig_type (mktype_decl tdl []))) :: l
              | SgVal (loc, n, t) ->
                (mksig loc
                   (Psig_value (with_loc n loc, (mkvalue_desc loc t [])))) :: l
              | Ast.SgAnt (loc, _) -> error loc "antiquotation in sig_item"
            and module_sig_binding x acc =
              match x with
              | Ast.MbAnd (_, x, y) ->
                  module_sig_binding x (module_sig_binding y acc)
              | Ast.MbCol (loc, s, mt) -> (with_loc s loc, (module_type mt)) :: acc
              | _ -> assert false
            and module_str_binding x acc =
              match x with
              | Ast.MbAnd (_, x, y) ->
                  module_str_binding x (module_str_binding y acc)
              | Ast.MbColEq (loc, s, mt, me) ->
                  (with_loc s loc, (module_type mt), (module_expr me)) :: acc
              | _ -> assert false
            and module_expr =
              function
              | Ast.MeNil loc -> error loc "nil module expression"
              | Ast.MeId (loc, i) -> mkmod loc (Pmod_ident (long_uident loc i))
              | Ast.MeApp (loc, me1, me2) ->
                  mkmod loc
                    (Pmod_apply ((module_expr me1), (module_expr me2)))
              | Ast.MeFun (loc, n, mt, me) ->
                  mkmod loc
                    (Pmod_functor (with_loc n loc, (module_type mt), (module_expr me)))
              | Ast.MeStr (loc, sl) ->
                  mkmod loc (Pmod_structure (str_item sl []))
              | Ast.MeTyc (loc, me, mt) ->
                  mkmod loc
                    (Pmod_constraint ((module_expr me), (module_type mt)))
              | Ast.MePkg (loc, (Ast.ExTyc (c_loc, e, (Ast.TyPkg (pt_loc, pt))))) ->
                mkmod loc
                  (Pmod_unpack
                     (mkexp c_loc
                        (Pexp_constraint
                           (expr e,
                            Some(mktyp pt_loc (Ptyp_package (package_type pt))),
                            None))))
              | Ast.MePkg (loc, _) ->
                  error loc "(value expr) not supported yet"
              | Ast.MeAnt (loc, _) ->
                  error loc "antiquotation in module_expr"
            and str_item s l =
              match s with
              | Ast.StNil _ -> l
              | StCls (loc, cd) ->
                  (mkstr loc
                     (Pstr_class
                        (List.map class_info_class_expr
                           (list_of_class_expr cd [])))) ::
                    l
              | StClt (loc, ctd) ->
                  (mkstr loc
                     (Pstr_class_type
                        (List.map class_info_class_type
                           (list_of_class_type ctd [])))) ::
                    l
              | Ast.StSem (_, st1, st2) -> str_item st1 (str_item st2 l)
              | StDir (_, _, _) -> l
              | Ast.StExc (loc, (Ast.TyId (_, (Ast.IdUid (s_loc, s)))), Ast.
                  ONone) ->
                  (mkstr loc (Pstr_exception (with_loc (conv_con s) s_loc, []))) :: l
              | Ast.StExc (loc,
                  (Ast.TyOf (_, (Ast.TyId (_, (Ast.IdUid (s_loc, s)))), t)), Ast.
                  ONone) ->
                  (mkstr loc
                     (Pstr_exception (with_loc (conv_con s) s_loc,
                        (List.map ctyp (list_of_ctyp t []))))) ::
                    l
              | Ast.StExc (loc, (Ast.TyId (_, (Ast.IdUid (s_loc, s)))),
                  (Ast.OSome i)) ->
                  (mkstr loc
                     (Pstr_exn_rebind
                        (with_loc (conv_con s) s_loc, (ident loc i)))) ::
                    l
              | StExc (_, _, _) -> assert false
              | StExp (loc, e) -> (mkstr loc (Pstr_eval (expr e))) :: l
              | StExt (loc, n, t, sl) ->
                  (mkstr loc
                     (Pstr_primitive (with_loc n loc,
                        (mkvalue_desc loc t (list_of_meta_list sl))))) ::
                    l
              | StInc (loc, me) ->
                  (mkstr loc (Pstr_include (module_expr me))) :: l
              | StMod (loc, n, me) ->
                  (mkstr loc (Pstr_module (with_loc n loc, (module_expr me)))) :: l
              | StRecMod (loc, mb) ->
                  (mkstr loc (Pstr_recmodule (module_str_binding mb []))) ::
                    l
              | StMty (loc, n, mt) ->
                  (mkstr loc (Pstr_modtype (with_loc n loc, (module_type mt)))) :: l
              | StOpn (loc, id) ->
                  (mkstr loc (Pstr_open (long_uident loc id))) :: l
              | StTyp (loc, tdl) ->
                  (mkstr loc (Pstr_type (mktype_decl tdl []))) :: l
              | StVal (loc, rf, bi) ->
                  (mkstr loc (Pstr_value ((mkrf rf), (binding bi [])))) :: l
              | Ast.StAnt (loc, _) -> error loc "antiquotation in str_item"
            and class_type =
              function
              | CtCon (loc, ViNil, id, tl) ->
                  mkcty loc
                    (Pcty_constr ((long_class_ident loc id),
                       (List.map ctyp (list_of_opt_ctyp tl []))))
              | CtFun (loc, (TyLab (_, lab, t)), ct) ->
                  mkcty loc (Pcty_fun (lab, (ctyp t), (class_type ct)))
              | CtFun (loc, (TyOlb (loc1, lab, t)), ct) ->
                  let t =
                    TyApp (loc1,
                      (Ast.TyId (loc1, (Ast.IdLid (loc1, "option")))), t)
                  in
                    mkcty loc
                      (Pcty_fun (("?" ^ lab), (ctyp t), (class_type ct)))
              | CtFun (loc, t, ct) ->
                  mkcty loc (Pcty_fun ("", (ctyp t), (class_type ct)))
              | CtSig (loc, t_o, ctfl) ->
                  let t =
                    (match t_o with | Ast.TyNil _ -> Ast.TyAny loc | t -> t) in
                  let cil = class_sig_item ctfl []
                  in mkcty loc
                  (Pcty_signature {
                    pcsig_self = ctyp t;
                    pcsig_fields = cil;
                    pcsig_loc = mkloc loc
                  })
              | CtCon (loc, _, _, _) ->
                  error loc "invalid virtual class inside a class type"
              | CtAnt (_, _) | CtEq (_, _, _) | CtCol (_, _, _) |
                  CtAnd (_, _, _) | CtNil _ -> assert false
            and class_info_class_expr ci =
              match ci with
              | CeEq (_, (CeCon (loc, vir, (IdLid (name_loc, name)), params)), ce)
                  ->
                  let (loc_params, (params, variance)) =
                    (match params with
                     | Ast.TyNil _ -> (loc, ([], []))
                     | t ->
                         ((loc_of_ctyp t),
                          (List.split (class_parameters t []))))
                  in
                    {
                      pci_virt = mkvirtual vir;
                      pci_params = (params, (mkloc loc_params));
                      pci_name = with_loc name name_loc;
                      pci_expr = class_expr ce;
                      pci_loc = mkloc loc;
                      pci_variance = variance;
                    }
              | ce -> error (loc_of_class_expr ce) "bad class definition"
            and class_info_class_type ci =
              match ci with
              | CtEq (_, (CtCon (loc, vir, (IdLid (name_loc, name)), params)), ct) |
                  CtCol (_, (CtCon (loc, vir, (IdLid (name_loc, name)), params)),
                    ct)
                  ->
                  let (loc_params, (params, variance)) =
                    (match params with
                     | Ast.TyNil _ -> (loc, ([], []))
                     | t ->
                         ((loc_of_ctyp t),
                          (List.split (class_parameters t []))))
                  in
                    {
                      pci_virt = mkvirtual vir;
                      pci_params = (params, (mkloc loc_params));
                      pci_name = with_loc name name_loc;
                      pci_expr = class_type ct;
                      pci_loc = mkloc loc;
                      pci_variance = variance;
                    }
              | ct ->
                  error (loc_of_class_type ct)
                    "bad class/class type declaration/definition"
            and class_sig_item c l =
              match c with
              | Ast.CgNil _ -> l
              | CgCtr (loc, t1, t2) ->
                { pctf_desc =
                    Pctf_cstr (((ctyp t1), (ctyp t2)));
                  pctf_loc = (mkloc loc)} :: l
              | Ast.CgSem (_, csg1, csg2) ->
                  class_sig_item csg1 (class_sig_item csg2 l)
              | CgInh (loc, ct) ->
                { pctf_desc = Pctf_inher (class_type ct);
                  pctf_loc = mkloc loc} :: l
              | CgMth (loc, s, pf, t) ->
                { pctf_desc = Pctf_meth
                    ((s, (mkprivate pf), (mkpolytype (ctyp t))));
                  pctf_loc = mkloc loc} ::
                    l
              | CgVal (loc, s, b, v, t) ->
                { pctf_desc = Pctf_val
                     ((s, (mkmutable b), (mkvirtual v), (ctyp t)));
                  pctf_loc = (mkloc loc)} ::
                    l
              | CgVir (loc, s, b, t) ->
                  {pctf_desc = Pctf_virt
                      ((s, (mkprivate b), (mkpolytype (ctyp t))));
                   pctf_loc = (mkloc loc)} ::
                    l
              | CgAnt (_, _) -> assert false
            and class_expr =
              function
              | (CeApp (loc, _, _) as c) ->
                  let (ce, el) = class_expr_fa [] c in
                  let el = List.map label_expr el
                  in mkpcl loc (Pcl_apply ((class_expr ce), el))
              | CeCon (loc, ViNil, id, tl) ->
                  mkpcl loc
                    (Pcl_constr ((long_class_ident loc id),
                       (List.map ctyp (list_of_opt_ctyp tl []))))
              | CeFun (loc, (PaLab (_, lab, po)), ce) ->
                  mkpcl loc
                    (Pcl_fun (lab, None, (patt_of_lab loc lab po),
                       (class_expr ce)))
              | CeFun (loc, (PaOlbi (_, lab, p, e)), ce) ->
                  let lab = paolab lab p
                  in
                    mkpcl loc
                      (Pcl_fun (("?" ^ lab), (Some (expr e)), (patt p),
                         (class_expr ce)))
              | CeFun (loc, (PaOlb (_, lab, p)), ce) ->
                  let lab = paolab lab p
                  in
                    mkpcl loc
                      (Pcl_fun (("?" ^ lab), None, (patt_of_lab loc lab p),
                         (class_expr ce)))
              | CeFun (loc, p, ce) ->
                  mkpcl loc (Pcl_fun ("", None, (patt p), (class_expr ce)))
              | CeLet (loc, rf, bi, ce) ->
                  mkpcl loc
                    (Pcl_let ((mkrf rf), (binding bi []), (class_expr ce)))
              | CeStr (loc, po, cfl) ->
                  let p =
                    (match po with | Ast.PaNil _ -> Ast.PaAny loc | p -> p) in
                  let cil = class_str_item cfl []
                  in mkpcl loc
                  (Pcl_structure {
                    pcstr_pat =((patt p));
                    pcstr_fields = cil})
              | CeTyc (loc, ce, ct) ->
                  mkpcl loc
                    (Pcl_constraint ((class_expr ce), (class_type ct)))
              | CeCon (loc, _, _, _) ->
                  error loc "invalid virtual class inside a class expression"
              | CeAnt (_, _) | CeEq (_, _, _) | CeAnd (_, _, _) | CeNil _ ->
                  assert false
            and class_str_item c l =
              match c with
              | CrNil _ -> l
              | CrCtr (loc, t1, t2) ->
                { pcf_desc = Pcf_constr ((ctyp t1), (ctyp t2));
                  pcf_loc = (mkloc loc)} :: l
              | Ast.CrSem (_, cst1, cst2) ->
                  class_str_item cst1 (class_str_item cst2 l)
              | CrInh (loc, ov, ce, pb) ->
                  let opb = if pb = "" then None else Some pb
                  in
                    { pcf_desc = Pcf_inher
                        ((override_flag loc ov), (class_expr ce), opb);
                      pcf_loc = mkloc loc } ::
                      l
              | CrIni (loc, e) ->
                { pcf_desc = Pcf_init (expr e) ; pcf_loc = mkloc loc } :: l
              | CrMth (loc, s, ov, pf, e, t) ->
                  let t =
                    (match t with
                     | Ast.TyNil _ -> None
                     | t -> Some (mkpolytype (ctyp t))) in
                  let e = mkexp loc (Pexp_poly ((expr e), t))
                  in
                    {pcf_desc = Pcf_meth
                        ((with_loc s loc, (mkprivate pf), (override_flag loc ov), e));
                     pcf_loc = (mkloc loc)} ::
                      l
              | CrVal (loc, s, ov, mf, e) ->
                  {pcf_desc = Pcf_val
                     ((with_loc s loc, (mkmutable mf), (override_flag loc ov), (expr e)));
                   pcf_loc = (mkloc loc)} ::
                    l
              | CrVir (loc, s, pf, t) ->
                  {pcf_desc = Pcf_virt
                     ((with_loc s loc, (mkprivate pf), (mkpolytype (ctyp t))));
                   pcf_loc = (mkloc loc)} ::
                    l
              | CrVvr (loc, s, mf, t) ->
                  {pcf_desc = Pcf_valvirt
                      ((with_loc s loc, (mkmutable mf), (ctyp t)));
                   pcf_loc =  (mkloc loc)} ::
                    l
              | CrAnt (_, _) -> assert false
              
            let sig_item ast = sig_item ast []
              
            let str_item ast = str_item ast []
              
            let directive =
              function
              | Ast.ExNil _ -> Pdir_none
              | ExStr (_, s) -> Pdir_string s
              | ExInt (_, i) -> Pdir_int (int_of_string i)
              | Ast.ExId (_, (Ast.IdUid (_, "True"))) -> Pdir_bool true
              | Ast.ExId (_, (Ast.IdUid (_, "False"))) -> Pdir_bool false
              | e -> Pdir_ident (ident_noloc (ident_of_expr e))
              
            let phrase =
              function
              | StDir (_, d, dp) -> Ptop_dir (d, (directive dp))
              | si -> Ptop_def (str_item si)

end

let input_camlp4_ast_impl ic =
  let camlp4_ast = (input_value ic : Ast.str_item) in
  let ast = Camlp4Ast2OCamlAst.str_item camlp4_ast in
  !Location.input_name, ast

let input_camlp4_ast_intf ic =
  let camlp4_ast = (input_value ic : Ast.sig_item) in
  let ast = Camlp4Ast2OCamlAst.sig_item camlp4_ast in
  !Location.input_name, ast
