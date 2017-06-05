(**************************************************************************)
(*                                                                        *)
(*                          Profile                                       *)
(*  Tiphaine Turpin                                                       *)
(*  Copyright 2009 INRIA Rennes - Bretagne Atlantique                     *)
(*                                                                        *)
(*  Profile is free software: you can redistribute it and/or modify       *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.                       *)
(*                                                                        *)
(*  Profile is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with Profile (see LICENSE.Profile under the root        *)
(*  directory of TypeRex).  If not, see <http://www.gnu.org/licenses/>.   *)
(*                                                                        *)
(**************************************************************************)

open Profile
open Arg

let style = ref `percentage
let filename = ref default_dump_filename

let _ =
  parse (align [
    "-seconds", Unit (function () -> style := `seconds),
    " Display times in seconds";

    "-percentage", Unit (function () -> style := `percentage),
    " Display times in percentage (default)"
  ])
  (( := ) filename)
  (Printf.sprintf "Usage: %s [options] {<filename>}" Sys.argv.(0));
  let dotfile = Filename.chop_extension !filename ^ ".dot" in
  write_graph ~print_time:!style (read_stats !filename) dotfile;
  Printf.eprintf "wrote %s\n%!" dotfile
