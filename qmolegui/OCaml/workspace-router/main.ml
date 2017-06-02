(*
  Copyright 2015 Christoph Kohlhepp 
  All Rights Reserved

  Licenced under the
  GNU GENERAL PUBLIC LICENSE
  Version 3, 29 June 2007

 *)


open Unix;;
open Str;;

let _ = print_endline "QMOLE-WSBAR START";;

(* Set up environment *)

putenv "FG" "black";;
putenv "BG" "grey";;
putenv "DISPLAY" "localhost:0.0";;
putenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin";;

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

let trim s =
  let s' = Str.replace_first (Str.regexp "^[ \t\n]+") "" s in
  Str.replace_first (Str.regexp "[ \t\n]+$") "" s';;

let devid = List.hd (List.rev (split output));;

let dzenmode id =
  match id with
     "iPad1,1" -> "extended" (* Ipad 1 145 *)
    |"iPad2,1" -> "extended" (* Ipad 2 *)
    |"iPad2,2" -> "extended" 
    |"iPad2,3" -> "extended"
    |"iPad2,4" -> "extended"
    |"iPad3,1" -> "extended" (* Ipad 3 *)
    |"iPad3,2" -> "extended"
    |"iPad3,3" -> "extended"
    |"iPad3,4" -> "extended" (* Ipad 4 *) 
    |"iPad3,5" -> "extended"
    |"iPad3,6" -> "extended"
    |"iPad4,1" -> "extended" (* Ipad Air *)
    |"iPad4,2" -> "extended"
    |"iPad4,3" -> "extended"
    |"iPad5,3" -> "extended" (* Ipad Air2 *)
    |"iPad5,4" -> "extended"
    |"iPad2,5" -> "extended" (* Ipad Mini 1G *)
    |"iPad2,6" -> "extended"
    |"iPad2,7" -> "extended"
    |"iPad4,4" -> "extended" (* Ipad Mini 2 *)
    |"iPad4,5" -> "extended"
    |"iPad4,6" -> "extended"
    |"iPad4,7" -> "extended" (* Ipad Mini 3 *)
    |"iPad4,8" -> "extended"
    |"iPad4,9" -> "extended"
    |"iPhone1,1" -> "abridged" (* IPhone 1*)
    |"iPhone1,2" -> "abridged" (* IPhone 3G *)
    |"iPhone2,1" -> "abridged" (* IPhone 3GS*)
    |"iPhone3,1" -> "abridged" (* IPhone 4*)
    |"iPhone3,2" -> "abridged"
    |"iPhone3,3" -> "abridged"
    |"iPhone4,1" -> "abridged" (* IPhone 4s*)
    |"iPhone5,1" -> "abridged" (* IPhone 5*)
    |"iPhone5,2" -> "abridged" 
    |"iPhone5,3" -> "abridged" (* IPhone 5c*)
    |"iPhone5,4" -> "abridged"
    |"iPhone6,1" -> "abridged" (* IPhone 5s*)
    |"iPhone6,2" -> "abridged"
    |"iPhone7,2" -> "abridged" (* IPhone 6*)
    |"iPhone7,1" -> "abridged" (* IPhone 6Plus*)
    | _ -> "abridged";;


(* Make us seven workspaces *)

let statusDesk = Sys.command("herbstclient rename default Desk");;
let statusTerminal = Sys.command("herbstclient add Term");;
let statusWeb = Sys.command("herbstclient add Web");;
let statusPdf = Sys.command("herbstclient add Pdf");;
let statusEmail = Sys.command("herbstclient add Mail");;
let statusEmacs = Sys.command("herbstclient add Edit");;
let statusFiles = Sys.command("herbstclient add File");;

(* Set up routing *)

let propEmacs = Sys.command("herbstclient rule class~'[Ee]macs' tag=Edit");;
let propTerminal = Sys.command("herbstclient rule class~'XTerm' tag=Term");;
let propTerminal2 = Sys.command("herbstclient rule class~'[Ii]vte' tag=Term");;
let propFiles = Sys.command("herbstclient rule class~'ROX-Filer' tag=File");;
let propWeb = Sys.command("herbstclient rule class~'Netsurf' tag=Web");;
let propEmail = Sys.command("herbstclient rule class~'claws-mail' tag=Mail");;
let propPdf = Sys.command("herbstclient rule class~'Xournal' tag=Pdf");;

(* Set terminal as desktop *)
let gotohome  = Sys.command("herbstclient use Term");; 

let monitorCmd = 
  if (dzenmode (trim devid) = "extended") then 
"
{ 
herbstclient --idle 
} | {
   TAGS=( $(herbstclient tag_status $monitor) ) 
      separator=\"^fg(#1793D0)^ro(1x16)^fg()\"
      while true; do
         for i in \"${TAGS[@]}\" ; do
            echo -n \"^ca(1,herbstclient use ${i:1}) \"
            case ${i:0:1} in
            '#')
               echo -n \"^fg(#1793D0)[^fg(#FFFFFF)${i:1}^fg(#1793D0)]\"
               ;;
            ':')
               echo -n \"^fg(#FFFFFF) ${i:1} \"
               ;;
            *)
               echo -n \"^fg(#123456) ${i:1} \"
               ;;
            esac
            echo -n \"^ca()\"
        done
        echo -n \" $separator\"
        echo -n \" ^ca(1,xdotool key Up) [Up] ^ca()\"
        echo -n \" ^ca(1,xdotool key Down) [Down] ^ca()\"
        echo -n \" ^ca(1,xdoopenparen) ((( ^ca()\"
        echo -n \" ^ca(1,xdocloseparen) ))) ^ca()\"
        echo
        read line || break
        TAGS=( $(herbstclient tag_status $monitor) )                   
      done
} 2> /dev/null | dzen2 -fn \"Sans:size=14\" -ta l -y 0 -x 0 -h 60 -fg $FG -bg $BG"
  else 
"
{ 
herbstclient --idle 
} | {
   TAGS=( $(herbstclient tag_status $monitor) ) 
      separator=\"^fg(#1793D0)^ro(1x16)^fg()\"
      while true; do
         for i in \"${TAGS[@]}\" ; do
            echo -n \"^ca(1,herbstclient use ${i:1}) \"
            case ${i:0:1} in
            '#')
               echo -n \"^fg(#1793D0)[^fg(#FFFFFF)${i:1}^fg(#1793D0)]\"
               ;;
            ':')
               echo -n \"^fg(#FFFFFF) ${i:1} \"
               ;;
            *)
               echo -n \"^fg(#123456) ${i:1} \"
               ;;
            esac
            echo -n \"^ca()\"
        done
        echo -n \" $separator\"
        echo -n \" ^ca(1,xdotool key Up) [U] ^ca()\"
        echo -n \" ^ca(1,xdotool key Down) [D] ^ca()\"
        echo -n \" ^ca(1,xdoopenparen) ((( ^ca()\"
        echo -n \" ^ca(1,xdocloseparen) ))) ^ca()\"
        echo
        read line || break
        TAGS=( $(herbstclient tag_status $monitor) )                   
      done
} 2> /dev/null | dzen2 -fn \"Sans:size=14\" -ta l -y 0 -x 0 -h 60 -fg $FG -bg $BG";;


let _ = print_endline monitorCmd;;

let status = Sys.command(monitorCmd);;

let _ = print_endline "QMOLE-WSBAR END";;



