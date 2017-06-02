(*
  Copyright 2015 Christoph Kohlhepp 
  All Rights Reserved

  Licenced under the
  GNU GENERAL PUBLIC LICENSE
  Version 3, 29 June 2007

 *)

open Unix;;
open Str;;

putenv "PATH" "/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin";;

sleep 1;;

let trim s =
  let s' = Str.replace_first (Str.regexp "^[ \t\n]+") "" s in
  Str.replace_first (Str.regexp "[ \t\n]+$") "" s';;

let _ = print_endline "HERBSTLUFT AUTOSTART START";;

(* Check what we are running on *)
let read_process command =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer;;

let split = Str.split (Str.regexp_string " ");;

let output = read_process "qmoledevice  2>&1 | /bin/grep Device";;

let devid = List.hd (List.rev (split output));;

(* http://theiphonewiki.com/wiki/Models *)
(* http://en.wikipedia.org/wiki/List_of_iOS_devices#Display *)
let bottomgap id =
  match id with
     "iPad1,1" -> "145" (* Ipad 1 145 *)
    |"iPad2,1" -> "145" (* Ipad 2 *)
    |"iPad2,2" -> "145" 
    |"iPad2,3" -> "145"
    |"iPad2,4" -> "145"
    |"iPad3,1" -> "145" (* Ipad 3 *)
    |"iPad3,2" -> "145"
    |"iPad3,3" -> "145"
    |"iPad3,4" -> "145" (* Ipad 4 *) 
    |"iPad3,5" -> "145"
    |"iPad3,6" -> "145"
    |"iPad4,1" -> "100" (* Ipad Air *)
    |"iPad4,2" -> "100"
    |"iPad4,3" -> "100"
    |"iPad5,3" -> "145" (* Ipad Air2 *)
    |"iPad5,4" -> "145"
    |"iPad2,5" -> "145" (* Ipad Mini 1G *)
    |"iPad2,6" -> "145"
    |"iPad2,7" -> "145"
    |"iPad4,4" -> "145" (* Ipad Mini 2 *)
    |"iPad4,5" -> "145"
    |"iPad4,6" -> "145"
    |"iPad4,7" -> "145" (* Ipad Mini 3 *)
    |"iPad4,8" -> "145"
    |"iPad4,9" -> "145"
    |"iPhone1,1" -> "145" (* IPhone 1*)
    |"iPhone1,2" -> "145" (* IPhone 3G *)
    |"iPhone2,1" -> "145" (* IPhone 3GS*)
    |"iPhone3,1" -> "145" (* IPhone 4*)
    |"iPhone3,2" -> "145"
    |"iPhone3,3" -> "145"
    |"iPhone4,1" -> "145" (* IPhone 4s*)
    |"iPhone5,1" -> "145" (* IPhone 5*)
    |"iPhone5,2" -> "145" 
    |"iPhone5,3" -> "145" (* IPhone 5c*)
    |"iPhone5,4" -> "145"
    |"iPhone6,1" -> "104" (* IPhone 5s*)
    |"iPhone6,2" -> "104"
    |"iPhone7,2" -> "104" (* IPhone 6*)
    |"iPhone7,1" -> "104" (* IPhone 6Plus*)
    | _ -> "145";;

let bmpanely id =
  match id with
     "iPad1,1" -> "880" (* Ipad 1 145 *)
    |"iPad2,1" -> "880" (* Ipad 2 *)
    |"iPad2,2" -> "880" 
    |"iPad2,3" -> "880"
    |"iPad2,4" -> "880"
    |"iPad3,1" -> "880" (* Ipad 3 *)
    |"iPad3,2" -> "880"
    |"iPad3,3" -> "880"
    |"iPad3,4" -> "880" (* Ipad 4 *) 
    |"iPad3,5" -> "880"
    |"iPad3,6" -> "880"
    |"iPad4,1" -> "0" (* Ipad Air *)
    |"iPad4,2" -> "0"
    |"iPad4,3" -> "0"
    |"iPad5,3" -> "880" (* Ipad Air2 *)
    |"iPad5,4" -> "880"
    |"iPad2,5" -> "880" (* Ipad Mini 1G *)
    |"iPad2,6" -> "880"
    |"iPad2,7" -> "880"
    |"iPad4,4" -> "880" (* Ipad Mini 2 *)
    |"iPad4,5" -> "880"
    |"iPad4,6" -> "880"
    |"iPad4,7" -> "880" (* Ipad Mini 3 *)
    |"iPad4,8" -> "880"
    |"iPad4,9" -> "880"
    |"iPhone1,1" -> "880" (* IPhone 1*)
    |"iPhone1,2" -> "880" (* IPhone 3G *)
    |"iPhone2,1" -> "880" (* IPhone 3GS*)
    |"iPhone3,1" -> "880" (* IPhone 4*)
    |"iPhone3,2" -> "880"
    |"iPhone3,3" -> "880"
    |"iPhone4,1" -> "880" (* IPhone 4s*)
    |"iPhone5,1" -> "880" (* IPhone 5*)
    |"iPhone5,2" -> "880" 
    |"iPhone5,3" -> "880" (* IPhone 5c*)
    |"iPhone5,4" -> "880"
    |"iPhone6,1" -> "0" (* IPhone 5s*)
    |"iPhone6,2" -> "0"
    |"iPhone7,2" -> "0" (* IPhone 6*)
    |"iPhone7,1" -> "0" (* IPhone 6Plus*)
    | _ -> "0";;

(* Set up environment *)

putenv "DISPLAY" "localhost:0.0";;
putenv "Mod" "Mod1";;  (* alt key *)

(* Basic Setup *)
let frameTransparency = Sys.command("herbstclient set frame_bg_transparent 1");;
let frameOpacity = Sys.command("herbstclient set frame_normal_opacity 1");;
let padCommand = "herbstclient pad 0 64 0 " ^ bottomgap (trim devid) ^ " 0";;
let gaps = Sys.command(padCommand);;

(* Key Bindings *)
let bindReload = Sys.command("herbstclient keybind $Mod-i reload");;
let bindClose = Sys.command("herbstclient keybind $Mod-x close");;

(*
let bindClose = Sys.command("herbstclient keybind $Mod-u spawn xdotool key Up");;
let bindClose = Sys.command("herbstclient keybind $Mod-d spawn xdotool key Down");;
let bindClose = Sys.command("herbstclient keybind $Mod-o spawn xdotool type '('");;
let bindClose = Sys.command("herbstclient keybind $Mod-c spawn xdotool type ')'");;
*)

let bindIvte = Sys.command("herbstclient keybind $Mod-Return spawn ivte");;
(*
let bindEmacs = Sys.command("herbstclient keybind $Mod-e spawn emacs");;
*)

(* Define window classes not managed *)
let noManage = Sys.command("herbstclient rule class=bmpanel manage=off");;

(* Enable key autorepeat - synonymous with 'xset -r on' *)
let noManage = Sys.command("xset -r");;

(* Start workspace-router *)
let workSpace  = Sys.command("workspace-router &");;

(* Start bmpanel2 *)
let panel  = Sys.command("bmpanel2 &");;

(* Set gap *)
let gap  = Sys.command("/var/mobile/.config/herbstluftwm/mindthegap.sh 0");;

(* Go to terminal screen and launch console font size 14*)
let gotohome  = Sys.command("herbstclient use Term");; 
let terminal = Sys.command("ivte -fn \"Monospace 14\" &")

(* Move bmpanel after giving it time to show *)
let panelMoved = 
  if (bmpanely (trim devid) <> "0") then 
    Sys.command("xdotool search --sync --onlyvisible --class \"bmpanel\" windowmove 0 880 &")
  else
    0;;

(* Set terminal again *)
let gotohome2  = Sys.command("herbstclient use Term");; 

let _ = print_endline "HERBSTLUFT AUTOSTART END";;




