//
// K&R, 2nd edition, page 97
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

extern fun getint {l:addr}
  (pf: !int? @ l >> int @ l | pn: ptr l): void

implement getint {l} (pf | pn) = let
  var c: int // unintialized
  val () = loop (c) where {
    fun loop (c: &int? >> int): void = let
      val () = c := getchar1 ()
    in
      if c >= 0 then // c <> EOF
        (if char_isspace (char_of_int1 c) then loop (c) else ())
      else ()
    end // end of [loop]
  } // end of [val]
  var isnum: bool = false
  val () =
    if c <> EOF then begin
      case+ char_of_int (c) of
      | c when char_isdigit c => (isnum := true)
      | '+' => (isnum := true)
      | '-' => (isnum := true)
      | c => ungetc_exn (c, stdin_ref)
    end // end of [if]
in
  if :(pf: int @ l) => isnum then let
    var sgn: int = 1
    val () = case+ char_of_int (c) of '-' => (sgn := ~1) | _ => ()
    val () = case+ char_of_int (c) of
      | '+' => (c := getchar ()) | '-' => (c := getchar ()) | _ => ()
    val () = !pn := 0
    val () = loop (pf | pn, c) where {
      fun loop (pf: !int @ l >> int @ l | pn: ptr l, c: &int): void =
        if c <> EOF then let
          val c1 = char_of_int c
        in
          if char_isdigit (c1) then begin
            !pn := 10 * !pn + (c1 - '0'); c := getchar (); loop (pf | pn, c)
          end else begin
            // loop exists
          end // end of [if]
        end // end of [if]
    } // end of [val]
    val () = if c <> EOF then ungetc_exn (char_of_int c, stdin_ref)
  in
    !pn := sgn * !pn
  end else begin
    !pn := 0 // it is not a number
  end // end of [if]
end // end of [getint]

(* ****** ****** *)

implement main (argc, argv) = let
  var int1: int // uninitialized
  val () = getint (view@ int1 | &int1)
  val () = (print int1; print_newline ())
  var int2: int // uninitialized
  val () = getint (view@ int2 | &int2)
  val () = (print int2; print_newline ())
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [getint.dats] *)
