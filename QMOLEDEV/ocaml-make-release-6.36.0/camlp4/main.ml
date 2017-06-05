(*pp camlp4o -I . repeat_syn.cmo *)

let _ =
  let i = ref 0 in
  repeat print_int !i; incr i until !i = 10 done;
  print_newline ()
