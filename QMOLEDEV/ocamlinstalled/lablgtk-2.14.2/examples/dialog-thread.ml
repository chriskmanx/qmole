(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

(* lablgtk2 -thread -nothinit dialog-thread.ml *)

let window = GWindow.window ~border_width: 10 ()

let button = GButton.button ~label:"Open Dialog" ~packing: window#add ()

let mythread =
  Thread.create
    (fun () -> while true do Thread.delay 2.0; prerr_endline "running." done)
    ()

let main () =
  Glib.Timeout.add ~ms:100 ~callback:GtkThread.do_jobs;
  window#connect#destroy ~callback:GMain.quit;
  button#connect#clicked ~callback:(fun () ->
    let dialog = 
      GWindow.message_dialog ~title:"Quit ?"
        ~message_type:`QUESTION ~message:"Quit the application ?"
        ~buttons:GWindow.Buttons.yes_no ()
    in match dialog#run () with
      `YES -> GMain.quit ()
    | `NO | `DELETE_EVENT -> dialog#destroy ());
  window#show ();
  GtkThread.main ()

let _ = Printexc.print main ()
