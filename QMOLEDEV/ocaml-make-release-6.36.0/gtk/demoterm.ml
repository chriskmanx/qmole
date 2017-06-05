(*
  A little bit of a terminal emulator written in Objective CAML.

  Copyright (C) 2002 Tim Freeman <tim@fungible.com>
  Minor changes by Markus Mottl  <markus.mottl@gmail.com>

  This software is distributed under the terms of the GNU general
  public license.
*)

module String = StringLabels

(* To start with, just create a subprocess and copy text back and forth
   to it. *)

(* Make a subprocess.  Hmm, this is imperfect because many programs
   decide to buffer their input in this case because its standard
   output isn't a terminal.  "cat -u" and ocaml are exceptions. *)

let _ =
  let inch, outch = Unix.open_process "ocaml" in
  let buffer_size = 10240 in
  let exitNoError () = exit 0 in
  let window = GWindow.window ~title:"DemoTerm" () in
  let text =
    GText.view
      ~editable:false ~wrap_mode:`WORD
      ~width:80 ~height:24 ~show:true ~packing:window#add () in

  text#buffer#insert
    "DemoTerm version 0, Copyright (C) 2002 Tim Freeman\n\
     DemoTerm comes with ABSOLUTELY NO WARRANTY.\n\
     This is free software, and you are welcome to redistribute it \
     under certain conditions; for details go find a copy of the \
     GNU lesser general public license somewhere.\n";

  (* Use this flag to make sure we don't send any more data to the
     subprocess after we've closed its input. *)
  let closed = ref false in

  (* If the event is a key press, return true, otherwise return false.
     Returning false is the signal to gtk to try to find another handler
     for the event. *)
  let receive_key ev =
    let s = GdkEvent.Key.string ev in
    if GdkEvent.get_type ev = `KEY_PRESS then
      let sendit ch =
        (* To send a character, insert it into the buffer and copy it to
           outch. *)
        let tosend = String.make 1 ch in
        text#buffer#insert tosend;
        output outch tosend 0 1;
        flush outch in
      let rec loop pos =
        if not !closed && pos < String.length s then
          let ch = s.[pos] in
            (* If it's a control-d, then close the output stream. *)
            if ch = '\004' then (
              close_out outch;
              closed := true;
            ) else (
              if ch = '\r' then
                (* If the user pressed enter, then send a newline. *)
                sendit '\n'
              else
                (* Otherwise send what the user pressed. *)
                sendit ch;
              loop (pos + 1)) in
      let _ = loop 0 in true
    else false in

  ignore (text#event#connect#key_press ~callback:receive_key);

  (* Register a callback to exit if they close the window. *)
  (* FIXME Kill the subprocess if it's still running. *)
  ignore (window#connect#destroy ~callback:exitNoError);
  window#show ();

  let copyFromSubprocess () =
    let buf = String.make buffer_size 'x' in
    let rec copyLoop () =
      let len = input inch buf 0 (String.length buf) in
      if len > 0 then (
        text#buffer#insert (String.sub buf ~pos:0 ~len);
        copyLoop ())
      else close_in inch in
    copyLoop () in

  let maingtk = Thread.create GtkThread.main () in
  let copyout = Thread.create copyFromSubprocess () in
  Thread.join copyout;
  GMain.Main.quit ();
  Thread.join maingtk
