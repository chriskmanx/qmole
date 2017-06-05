//
// K&R, 2nd edition, page 71
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

extern fun atof (s: string):<> double

implement atof (s) = let
  val [n:int] s = string1_of_string s
  var i: sizeLte n // uninitialized
  val () = i := loop (s, 0) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (s: string n, i: size_t i):<> sizeLte n =
      if string_is_at_end (s, i) then i else let
        val c = s[i]
      in
        if char_isspace c then loop (s, i+1) else i
      end // end of [if]
  } // end of [val]
  var sgn: int = 1; val () =
    if string_is_at_end (s, i) then () else let
      val c = s[i] in case+ c of
        | '-' => (i := i + 1; sgn := ~1) | '+' => (i := i + 1)
        | _ (* no sign *) => ()
    end // end of [if]
  var d: double = 0.0
  val () = i := loop (s, i, d) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (s: string n, i: size_t i, d: &double):<> sizeLte n =
      if string_is_at_end (s, i) then i else let
        val c = s[i]
      in
        if char_isdigit c then begin
          d := 10.0 * d + double_of_int (c - '0'); loop (s, i+1, d)
        end else begin
          i // loop exists
        end // end of [if]
      end // end of [if]
  } // end of [val]
  val () =
    if string_is_at_end (s, i) then () else (
      if (s[i] = '.') then (i := i + 1) else ()
    ) // end of [if]
  var pow: double = 1.0
  val () = i := loop (s, i, pow, d) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (s: string n, i: size_t i, pow: &double, d: &double):<> sizeLte n =
      if string_is_at_end (s, i) then i (* loop exists *) else let
        val c = s[i]
      in
        if char_isdigit c then begin
          d := 10.0 * d + double_of_int (c - '0'); pow := 10.0 * pow;
          loop (s, i+1, pow, d)
        end else begin
          i // loop exists
        end // end of [if]
      end // end of [if]
  } // end of [val]
  val () = d := d / pow
in
  if sgn >= 0 then d else ~d
end // end of [atof]

implement main (argc, argv) = let
  val () = assert (argc >= 2)
in
  print (atof argv.[1]); print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [atof.dats] *)
