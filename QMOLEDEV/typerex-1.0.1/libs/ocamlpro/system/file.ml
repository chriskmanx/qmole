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

open OcpLang


(****************************)
(* Filename management *)
(****************************)


type t = {
  file_basename : string;
  file_dir : t;
  file_string : string;           (* The system filename, i.e. with system-specific separators *)
  file_partition : string;
}

let to_string t = t.file_string

let win32 = (Sys.os_type = "Win32")
let dir_separator = if win32 then '\\' else '/'
let path_separator = if win32 then
    ';'
  else
    ':'

let dir_separator_s = String.make 1 dir_separator

(*
    let rec output t =
      if t == t.file_dir then
        Printf.sprintf " { |%s| %s [%s]} " t.file_partition t.file_basename t.file_string
      else
        Printf.sprintf "%s -> %s [%s]" (output t.file_dir) t.file_basename t.file_string
*)

    let dirname t =
      if t.file_dir == t then
        match t.file_basename with
          "" | "." -> t
        | _ ->
            let rec root = {
                file_dir = root;
                file_basename = ".";
                file_partition = t.file_partition;
                file_string = ".";
              } in
            root
      else t.file_dir


    let rec normalize_path path =
      match path with
        [] -> []
      | dir :: tail ->
          let dir = dir :: normalize_path tail in
          match dir with
          | "" :: path -> path
          | "." :: path -> path
          | ".." :: _ -> dir
          | _ :: ".." :: path -> path
          | _ -> dir

    let rec remove_leading_dotdots path =
      match path with
        ".." :: path -> remove_leading_dotdots path
      | _ -> path

    let add_basename_s dir basename =
      if dir.file_string = dir_separator_s then
        dir_separator_s ^ basename
      else
        dir.file_string ^ dir_separator_s ^ basename

    let rec make dir path =
      match path with
        [] -> dir
      | basename :: tail ->
          let t = {
              file_basename = basename;
              file_dir = dir;
              file_partition =  dir.file_partition;
              file_string = add_basename_s dir basename;
            } in
          make t tail

    let of_path part path =
      let absolute = match path with
          "" :: _ :: _ -> true
        | _ -> false
      in
      let path = normalize_path path in
      let path = if absolute then remove_leading_dotdots path else path in

      if absolute then
        let rec root = {
            file_basename = "";
            file_dir = root;
            file_string = part ^ dir_separator_s;
            file_partition = part;
          } in
        make root path
      else
      match path with
        [] ->
          let rec current_dir = {
              file_basename = ".";
              file_dir = current_dir;
              file_string = part ^ ".";
              file_partition = part;
            } in
          current_dir
      | dir :: tail ->
          let rec base_dir = {
              file_basename = dir;
              file_dir = base_dir;
              file_partition = part;
              file_string = part ^ dir;
            } in
          make base_dir tail

    let of_unix_string s =
      let path = OcpString.split s '/' in
      let part = "" in
      of_path part path


    let of_win32_string s =
      let s1, s2  = OcpString.cut_at s ':' in
      let ss = if s1 == s then s else s2 in
      let part = if s1 == s then "" else (String.lowercase s1) ^ ":" in
      let path = OcpString.split ss '\\' in
      of_path part path

    let of_string s =
      if win32 then of_win32_string s else of_unix_string s

    let rec is_absolute t =
      if t.file_dir != t then is_absolute t.file_dir
      else
        t.file_basename = ""

    let is_implicit t = not (is_absolute t)

    let add_basename dir basename =
      if dir.file_basename = "." then
        let rec base_dir = {
            file_basename =  basename;
            file_dir = base_dir;
            file_partition = dir.file_partition;
            file_string = basename;
          } in
        base_dir
      else
      if basename = ".." && (dir.file_basename <> "..") then dir.file_dir else
        {
          file_basename =  basename;
          file_dir = dir;
          file_partition = dir.file_partition;
          file_string = add_basename_s dir basename;
        }

    let rec add_basenames dir list =
      match list with
        [] -> dir
      | "" :: tail ->
          add_basenames dir tail
      | basename :: tail ->
          add_basenames (add_basename dir basename) tail


    let check_suffix file suffix =
      Filename.check_suffix file.file_basename suffix

    let add_suffix t suffix =
      match t.file_basename with
        "." | ".." | "" -> failwith "Filename2.add_extension: symbolic file"
      | _ ->
          if t.file_dir == t then
            let rec root = {
                file_basename = t.file_basename ^ suffix;
                file_partition = t.file_partition;
                file_dir = root;
                file_string = t.file_string ^ suffix;
              }
            in
            root
          else
            {
              file_basename = t.file_basename ^ suffix;
              file_partition = t.file_partition;
              file_dir = t.file_dir;
              file_string = t.file_string ^ suffix;
            }

    let concat t1 t2 =
      if t2.file_partition <> "" && t1.file_partition <> t2.file_partition then
        failwith "Filename2.concat: filenames have different partitions";
      if is_absolute t2 then
        failwith "Filename2.concat: second filename is absolute";

      let rec iter dir t =
        let dir =
          if t.file_dir != t then
            iter dir t.file_dir
          else dir in
        add_basename dir t.file_basename
      in
      iter t1 t2

    let basename t = t.file_basename

    let safe_basename s =
      basename (of_string s)

(* could be usefull too : *)
    let safe_basenames s =
      let path = OcpString.split s dir_separator in
      let path = normalize_path path in
      remove_leading_dotdots path

    let extensions t =
      match OcpString.split_simplify t.file_basename '.' with
        [] | [_] -> []
      | filename :: xx -> xx

    let last_extension t =
      let rec iter list =
        match list with
          [] -> ""
        | [x] -> x
        | _ :: tail -> iter tail
      in
      iter (extensions t)

    let temp_file t ext =
      of_string (Filename.temp_file (to_string t) ext)

    let current_dir_name = of_string "."
    let getcwd () = of_string (Sys.getcwd ())

    let to_rooted_string t =
      if is_absolute t then
        t.file_string
      else
        Printf.sprintf ".%c%s" dir_separator t.file_string

    let equal t1 t2 =
      t1.file_string = t2.file_string &&
      t1.file_partition = t2.file_partition

    let chop_extension f =
      let (basename, ext) = OcpString.cut_at f.file_basename '.' in
      let ext_len = String.length f.file_basename - String.length basename in
      if ext_len = 0 then f else
      let len = String.length f.file_string in
      { f with
        file_basename = basename;
        file_string = String.sub f.file_string 0 (len-ext_len);
      }


(* IMPORTANT OS specificities (from the JDK):

Each filename is composed of:
- an optional prefix:
   nothing
  / root on Unix for absolute filenames
  \ root on Windows for absolute filenames without drive
  \\ UNC filename
  c: drive on Windows for relative filenames
  c:\ root and drive on windows for absolute filenames
  (nothing on Unix or c: or C: on Windows or \)
- a list of basenames (possibly empty for the root )

- there is an official separator like \ or /
- there is an official path-separator like : (unix) or ; (windows)

- listRoots() returns the list of available drives
- getAbsolutePath() -> absolute path
- getCanonicalPath() -> absolute path simplified and without symlinks

*)

































(****************************)
(* File management *)
(****************************)

let cut_last_extension basename =
  try
    let pos = String.rindex basename '.' in
    String.before basename pos,
    String.lowercase (String.after basename pos)
  with Not_found -> (basename, "")

(* We use ReentrantBuffer to allow sharing this buffer with other
functions that would need such buffers. *)
let string_of_channel ic =
  let s = ReentrantBuffers.get 32768 in
  let b = Buffer.create 1000 in
  let rec iter ic b s =
    let nread = input ic s 0 32768 in
    if nread > 0 then begin
      Buffer.add_substring b s 0 nread;
      iter ic b s
    end
  in
  iter ic b s;
  ReentrantBuffers.free s;
  Buffer.contents b

let string_of_file filename =
  let ic = open_in filename in
  try
    let s = string_of_channel ic in
    close_in ic;
    s
  with e ->
      close_in ic;
      raise e



let string_of_subfile filename pos len =
  let ic = open_in filename in
  seek_in ic pos;
  if len = 0 then begin
    close_in ic;
    ""
  end else  try
              let s = String.create len in
              let rec iter pos len =
                if len > 0 then
                  let nread = input ic s pos len in
                  if nread > 0 then
                    iter (pos+nread) (len-nread)
                  else raise End_of_file
              in
              iter 0 len;
              close_in ic;
              s
    with e ->
      close_in ic;
      raise e

let output_line chan string =
  output_string chan (string ^ "\n")

(* [line_break] tells whether or not the '\n' characters need to be kept. *)

(* NOT TAIL RECURSIVE !!!
let lines_of_file file =
  let chan = open_in file in
  let x = ref [] in
  let rec aux () =
    try
      let line = input_line chan in
      if not (discard line) then begin
        let l = if line_break then line ^ "\n" else line in
        x := l :: !x
      end;
      aux ()
    with End_of_file -> () in
  aux ();
  close_in chan;
  List.rev !x
*)

let lines_of_file file =
  let ic = open_in file in
  let lines = ref [] in
  begin try
	  while true do
	    lines := input_line ic :: !lines
	  done
    with End_of_file -> ()
  end;
  close_in ic;
  List.rev !lines


let file_of_lines filename lines =
  let oc = open_out filename in
  List.iter (fun l -> output_line oc l) lines;
  close_out oc

(* NOT CORRECT: append a newline at the end
let file_of_string filename str =
  file_of_lines filename [str]
*)

let file_of_string filename str =
  let oc = open_out filename in
  output_string oc str;
  close_out oc


let iter_lines f name =
  let ic = open_in name in
  try
    while true do
      let line = input_line ic in
      f line
    done
  with
    End_of_file -> close_in ic
  | e -> close_in ic; raise e

let iteri_lines f name =
  let ic = open_in name in
  let n = ref 0 in
  try
    while true do
      let line = input_line ic in
      f !n line;
      incr n;
    done
  with
    | End_of_file -> close_in ic
    | e -> close_in ic; raise e


let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let sub_lines file off len =
  let l = ref [] in
  let aux i elt =
    if i >= off && i <= off + len then
      l := elt :: !l in
  iteri_lines aux file;
  List.rev !l

module X = struct

  let sub_lines file off len = sub_lines (to_string file) off len
  let iteri_lines f file = iteri_lines f (to_string file)
  let iter_lines f file = iter_lines f (to_string file)
  let write_of_string file s = file_of_string (to_string file) s
  let read_to_string file = string_of_file (to_string file)
  let write_lines file lines = file_of_lines (to_string file) lines
  let read_lines file = lines_of_file (to_string file)

  let rename t1 t2 = Sys.rename (to_string t1) (to_string t2)

  let read_part_to_string file pos len =
    string_of_subfile (to_string file) pos len

  let exists file = Sys.file_exists (to_string file)
  let is_directory filename =
    try let s = Unix.stat (to_string filename) in
        s.Unix.st_kind = Unix.S_DIR with _ -> false

  let is_link filename =
    try let s = Unix.lstat (to_string filename) in
        s.Unix.st_kind = Unix.S_LNK with _ -> false


  let size filename =
    let s = Unix.stat (to_string filename) in
    s.Unix.st_size

  let stat filename = Unix.stat (to_string filename)
  let lstat filename = Unix.lstat (to_string filename)

  let size64 filename =
    let s = Unix.LargeFile.stat (to_string filename) in
    s.Unix.LargeFile.st_size

  let getcwd () = of_string (Sys.getcwd ())

  let open_in filename = open_in (to_string filename)
  let open_out filename = open_out (to_string filename)

  let open_in_bin filename = open_in_bin (to_string filename)
  let open_out_bin filename = open_out_bin (to_string filename)



  let copy_file f1 f2 =
  let ic = open_in_bin f1 in
  let oc = open_out_bin f2 in
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy();
  close_in ic;
  close_out oc

  let open_fd file mode perm = Unix.openfile (to_string file) mode perm

  let remove file = Sys.remove (to_string file)

  let iter f file =
    let s = ReentrantBuffers.get 32768 in
    let ic = open_in file in
    let rec iter f ic s =
      let nread = input ic s 0 32768 in
      if nread > 0 then begin
          f s 0 nread;
          iter f ic s
        end
    in
    iter f ic s;
    ReentrantBuffers.free s

end















module Dir = struct

  open X

  let mkdir dir perm = Unix.mkdir (to_string dir) perm
  let make dir = mkdir dir 0o755

  let rec make_all dir =
      if exists dir then begin
          if not (is_directory dir) then
            failwith (Printf.sprintf "File.Dir.make_all: %s not a directory" (to_string dir))
        end
      else
      if is_link dir then
        failwith (Printf.sprintf "File.Dir.make_all: %s is an orphan symbolic link" (to_string dir))
      else begin
          let predir = dirname dir in
          if predir != dir then make_all predir;
          if not (exists dir) then
            try
              mkdir dir 0o775
            with e ->
                failwith (Printf.sprintf "File.Dir.make_all: mkdir [%s] raised %s" (to_string dir) (Printexc.to_string e))
      end

  let list filename =
    let dir = Unix.opendir (to_string filename) in
    let list = ref [] in
    try
      while true do
        let file = Unix.readdir dir in
        if file <> "." && file <> ".." then begin
          list := file :: !list
        end;
      done;
      assert false
    with End_of_file -> Unix.closedir dir; !list
      | e ->
        Unix.closedir dir;
        raise e


    let list_files filename =
      let dir = Unix.opendir (to_string filename) in
      let list = ref [] in
      try
        while true do
          let file = Unix.readdir dir in
          if file <> "." && file <> ".." then begin
              list := (add_basename filename file) :: !list
            end;
        done;
        assert false
      with End_of_file -> Unix.closedir dir; !list
        | e ->
          Unix.closedir dir;
          raise e

    let iter f dirname =
      let dir = Unix.opendir (to_string dirname) in
      try
        while true do
          let file = Unix.readdir dir in
          if file <> "." && file <> ".." then begin
              f file
            end;
        done;
        assert false
      with End_of_file ->
          Unix.closedir dir
        | e ->
          Unix.closedir dir;
          raise e

    let iter_files f dirname =
      let dir = Unix.opendir (to_string dirname) in
      try
        while true do
          let file = Unix.readdir dir in
          if file <> "." && file <> ".." then begin
              f (add_basename dirname file)
            end;
        done;
        assert false
      with End_of_file ->
          Unix.closedir dir
        | e ->
          Unix.closedir dir;
          raise e

    let remove dir = Unix.rmdir (to_string dir)

    let rec remove_all (dir : t) =
      iter_files (fun filename ->
        if not (X.is_link filename) && X.is_directory filename then
          remove_all filename
        else
          X.remove filename
      ) dir;
      remove dir

end
