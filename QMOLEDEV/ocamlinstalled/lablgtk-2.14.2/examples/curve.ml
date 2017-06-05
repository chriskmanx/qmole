(* $Id$ *)

let w = GWindow.window ~width:200 ~height:150 ();;
let curve = GMisc.curve ~min_y:0. ~max_y:10. ~packing:w#add ();;
let show_vector _ =
  let vect = curve#get_vector 5 in
  Printf.printf "%g %g %g %g %g\n%!"
    vect.(0) vect.(1) vect.(2) vect.(3) vect.(4)
let () =
  curve#set_vector [|0.; 5.; 4.; 6.; 9.|];
  (* curve#reset (); *) (* works *)
  (* curve#set_curve_type `SPLINE; *) (* doesn't work with quartz... *)
  show_vector ();
  curve#event#connect#after_any ~callback:show_vector;
  w#connect#destroy ~callback:GMain.quit;
  w#show ();
  GMain.main ()
  
    
