//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 248 - 249
// section 4.8: Full-Screen Applications
//
(* ****** ****** *)

staload "libc/SATS/curses.sats"
staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

%{^
#define atslib_read1(c) read(STDIN_FILENO, c, 1)
%} // end of [%{^]
extern fun read1 (
  pf: !fildes_v (STDIN_FILENO) | c: &char
) : ssize_t = "mac#atslib_read1"
// end of [read1]

fun getch
  (pf: !fildes_v (STDIN_FILENO) | c: &char): int = let
  val nread = read1 (pf | c)
  val nread = ssize1_of_ssize (nread)
  val nread = int1_of_ssize1 (nread)
in
  case+ 0 of
  | _ when nread <= 0 => let
      val () = if nread = 0 then errno_set (errno_of_int(0))
    in
      ERR
    end // end of [_ when ...]
  | _ (*nread = 1*) => OK
end // end of [getch]

(* ****** ****** *)

fun mainloop (
    pf: !fildes_v (STDIN_FILENO)
  | (*none*)
  ) : int(*0/1:succ/fail*) = let
  var c: char = '\000'
  var nerr: int = 0
  val () = while (true) let
    val _err = clear ()
    val () = if _err = ERR then nerr := nerr+1
(*
    val () = printf ("[clear] is done\n", @())
*)
//
    val _err = mvaddstr (2, 9, "What do you want to do?")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = mvaddstr (3, 9, "1. Check out tape/DVD")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = mvaddstr (4, 9, "2. Reserve tape/DVD")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = mvaddstr (5, 9, "3. Register new member")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = mvaddstr (6, 9, "4. Search for title/actor")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = mvaddstr (7, 9, "5. Quit")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = mvaddstr (9, 9, "(Type item number to continue)")
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
    val _err = refresh () // HX: otherwise, nothing is shown
    val () = if _err = ERR then (nerr := nerr+1; break; assertfalse ())
//
    val _err = getch (pf | c)
    val () = if _err = ERR then nerr := nerr+1
    val () = (case+ 0 of
      | _ when ('1' <= c andalso c <= '4') => let
          val _err = clear ()
          val () = if _err = ERR then nerr := nerr+1
          val [l:addr] str = sprintf ("You typed %c", @(c))
          val _err = mvaddstr (4, 9, __cast str) where {
            extern castfn __cast (x: !strptr l):<> string
          } // end of [val]
          val () = strptr_free (str)
          val () = if _err = ERR then nerr := nerr+1
          val _err = mvaddstr (9, 9, "(Press any key to continue)")
          val () = if _err = ERR then nerr := nerr+1
          val _err = refresh () // HX: otherwise, change is not shown
          val () = if _err = ERR then nerr := nerr+1
          val _err = getch (pf | c)
          val () = if _err = ERR then nerr := nerr+1
        in
          break
        end // end of [_ when ...]
      | _ when c = '5' => break
      | _ => let
          val _err = beep ()
          val () = if _err = ERR then nerr := nerr+1
        in
          // nothing
        end // end of [_]
    ) : void // end of [val]
  in
    // nothing
  end // end of [val]
(*
  val () = (print "_err = "; print _err; print_newline ())
*)
in
  if nerr > 0 then 1 else 0
end // end of [mainloop]

(* ****** ****** *)

implement
main () = () where {
  val _ptr = initscr ()
  val () = assert_errmsg (_ptr > null, #LOCATION)
  val _err = raw ()
  val () = assert_errmsg (_err <> ERR, #LOCATION)
  val (pf_fd | ()) = stdin_fildes_view_get ()
  val status = mainloop (pf_fd | (*none*))
  val () = stdin_fildes_view_set (pf_fd | (*none*))
  val _err = clear ()
  val _err = refresh ()
  val _err = endwin ()
  val () = exit (status)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_4_8.dats] *)
