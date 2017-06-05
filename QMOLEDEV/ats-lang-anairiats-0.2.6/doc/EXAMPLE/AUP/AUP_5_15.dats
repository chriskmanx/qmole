//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 320 - 321
// section 5.15: Getting and Setting the Priority
//
(* ****** ****** *)

staload "libc/sys/SATS/types.sats"
staload "libc/SATS/stdlib.sats" // atoi
staload "libc/SATS/string.sats" // strncmp
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

%{^
#undef atslib_execvp
#define atslib_execvp(arg0, argv) execvp((char*)arg0, (char**)argv)
%} // end of [%{^]

fun print_usage () =
  printf ("usage: aupnice [-num] command\n", @())
// end of [print_usage]

implement
main {n} (argc, argv) = () where {
  val () = (
    if (argc >= 2) then () else
      (print_usage () ; exit (EXIT_FAILURE); assertfalse ())
  ) : [n >= 2] void
  val arg1 = argv.[1]
  var incr: int = 10 // default
  var cmdarg: intGte 1 = 1
  val () = if strncmp (arg1, "-", 1) = 0 then (incr := atoi(arg1); cmdarg := 2)
  stavar cmdarg: int
  val cmdarg = cmdarg : int (cmdarg)
  val () = (
    if (argc > cmdarg) then () else
      (print_usage () ;  exit (EXIT_FAILURE); assertfalse ())
  ) : [n > cmdarg ] void
  val _err = nice (incr) // this [nice] value is to be inherited
  val cmdpath = argv.[cmdarg]
  val cmdpath = string1_of_string (cmdpath)
  val n = strlen (cmdpath)
  val ind = string_index_of_char_from_left (cmdpath, '/')
  val cmdname = (
    if ind >= 0 then let
      val ind = size1_of_ssize1 (ind)
    in
      string_of_strbuf (string_make_substring (cmdpath, ind + 1, n-ind-1))
    end else cmdpath 
  ) : string
  val () = argv.[cmdarg] := cmdname
  val _err = execvp (cmdpath, argv.[cmdarg]) where {
    extern fun execvp (arg0: string, argv: &string): int = "mac#atslib_execvp"
  } // end of [val]
  val () = exit (EXIT_FAILURE)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_5_15.dats] *)
