(* The function executed by some thread *)
let a_thread () = print_endline "A thread has been started!"

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      print_int result; print_newline();   (*  Prints calculator result  *)

      Test.f ();                           (* Tests the C-function *)

      let t = Thread.create a_thread () in (* Creates a thread *)
      Thread.join t;                       (* Waites for thread termination *)
      print_endline "Thread terminated!";

      flush stdout
    done
  with Lexer.Eof -> exit 0
