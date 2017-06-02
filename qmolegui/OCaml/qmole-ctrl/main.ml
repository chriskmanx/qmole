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

let _ = print_endline ("Running on " ^ devid);;

let trim s =
  let s' = Str.replace_first (Str.regexp "^[ \t\n]+") "" s in
  Str.replace_first (Str.regexp "[ \t\n]+$") "" s';;

(* http://theiphonewiki.com/wiki/Models *)
(* http://en.wikipedia.org/wiki/List_of_iOS_devices#Display *)
let idtores id =
  match id with
     "iPad1,1" -> "768x1024" (* Ipad 1 768x1024 *)
    |"iPad2,1" -> "768x1024" (* Ipad 2 *)
    |"iPad2,2" -> "768x1024" 
    |"iPad2,3" -> "768x1024"
    |"iPad2,4" -> "768x1024"
    |"iPad3,1" -> "768x1024" (* Ipad 3 *)
    |"iPad3,2" -> "768x1024"
    |"iPad3,3" -> "768x1024"
    |"iPad3,4" -> "768x1024" (* Ipad 4 *) 
    |"iPad3,5" -> "768x1024"
    |"iPad3,6" -> "768x1024"
    |"iPad4,1" -> "1536x2048" (* Ipad Air *)
    |"iPad4,2" -> "1536x2048"
    |"iPad4,3" -> "1536x2048"
    |"iPad5,3" -> "768x1024" (* Ipad Air2 *)
    |"iPad5,4" -> "768x1024"
    |"iPad2,5" -> "768x1024" (* Ipad Mini 1G *)
    |"iPad2,6" -> "768x1024"
    |"iPad2,7" -> "768x1024"
    |"iPad4,4" -> "768x1024" (* Ipad Mini 2 *)
    |"iPad4,5" -> "768x1024"
    |"iPad4,6" -> "768x1024"
    |"iPad4,7" -> "768x1024" (* Ipad Mini 3 *)
    |"iPad4,8" -> "768x1024"
    |"iPad4,9" -> "768x1024"
    |"iPhone1,1" -> "768x1024" (* IPhone 1*)
    |"iPhone1,2" -> "768x1024" (* IPhone 3G *)
    |"iPhone2,1" -> "768x1024" (* IPhone 3GS*)
    |"iPhone3,1" -> "768x1024" (* IPhone 4*)
    |"iPhone3,2" -> "768x1024"
    |"iPhone3,3" -> "768x1024"
    |"iPhone4,1" -> "768x1024" (* IPhone 4s*)
    |"iPhone5,1" -> "768x1024" (* IPhone 5*)
    |"iPhone5,2" -> "768x1024" 
    |"iPhone5,3" -> "768x1024" (* IPhone 5c*)
    |"iPhone5,4" -> "768x1024"
    |"iPhone6,1" -> "768x1136" (* IPhone 5s*)
    |"iPhone6,2" -> "640x1136"
    |"iPhone7,2" -> "750x1334" (* IPhone 6*)
    |"iPhone7,1" -> "1242x2208" (* IPhone 6Plus*)
    | _ -> "1024x768";;


let vncCleanStatus = Sys.command("killall Xvnc");;

(* Set DISPLAY variable *)
putenv "DISPLAY" "localhost:0.0";;

  
let _ = print_endline "QMOLE-CONTROLLER";;

let passdir  = Sys.command("mkdir /var/mobile/.vnc 2>/dev/null");;

(* Start X11VNC subsystem *)

let _ = Printf.printf "Start display";; 
let _ = print_endline "";;

let hostname = Unix.gethostname ();;

let _ = print_endline ("Hostname: " ^ hostname);;

let vncpassword = (String.sub (Digest.to_hex (Digest.string hostname)) 0 8);;

let _ = print_endline ("Digest: " ^ vncpassword);;

let vncpassword_generator = "{ echo \"" ^ vncpassword ^ "\" ; echo \"" ^ vncpassword ^ "\" ; } | /usr/local/bin/vncpasswd";;
let vncPasswordStatus = Sys.command(vncpassword_generator);;

let geometry = " -geometry " ^ idtores (trim devid);;
let depth = " -depth 16";;
let access = " -localhost";;
let passwdfile = " -PasswordFile /var/mobile/.vnc/passwd";;
let security = " -SecurityTypes VncAuth";;
let vnclog = " > /tmp/qmoleaux.log 2>&1";;       (* Overwrite log *)
let background = " &";;
let vncStatus = Sys.command("Xvnc" ^ geometry ^ depth ^ access ^ security ^ passwdfile ^ vnclog ^ background);;

let _ = Printf.printf "Display status %d\n" vncStatus
let _ = print_endline "";

if vncStatus != 0 then exit 1;
sleep 3

(* Disable auto-repeat *)
let _ = Printf.printf "Disable auto repeat" 
let _ = print_endline ""
let xsetpid = Sys.command("xset -r")

(* Esetroot *)
let _ = Printf.printf "Esetroot" 
let _ = print_endline ""
let wallpaper = 
  if (idtores (trim devid) = "768x1024") then
    " /var/mobile/.config/herbstluftwm/wallpaper_768x1024.jpg"
  else if (idtores (trim devid) = "640x1136") then
    " /var/mobile/.config/herbstluftwm/wallpaper_640x1136.jpg"
  else if (idtores (trim devid) = "750x1334") then
    " /var/mobile/.config/herbstluftwm/wallpaper_750x1334.jpg"
  else if (idtores (trim devid) = "1242x2208") then
    " /var/mobile/.config/herbstluftwm/wallpaper_1242x2208.jpg"
  else 
    " /var/mobile/.config/herbstluftwm/wallpaper_768x1024.jpg";;

let _ = print_endline wallpaper;;


let esetroot = "/usr/local/bin/Esetroot"
let esetpid = Sys.command(esetroot ^ wallpaper)
let _ = Printf.printf "Esetroot status %d\n" xsetpid;

sleep 3

(* Herbstluft *)
let _ = Printf.printf "Herbstluft"
let _ = print_endline ""
let herbstluft = "herbstluftwm"
let wmlog = " > /tmp/herbstluft.log 2>&1"
(* let wmpid = Sys.command("/bin/bash -c \"" ^ herbstluft ^ wmlog ^  background ^ "\"") *)
let wmpid = Sys.command(herbstluft ^ wmlog ^ background)

let _ = Printf.printf "wm status %d\n" wmpid
