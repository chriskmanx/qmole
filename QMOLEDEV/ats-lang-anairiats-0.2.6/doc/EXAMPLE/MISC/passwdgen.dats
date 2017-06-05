(*
**
** random password generation
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: August, 2008
**
*)

(* ****** ****** *)

staload Rand = "libc/SATS/random.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

implement main (argc, argv) = let
  var n: int = 8
  val () = begin
    if argc >= 2 then n := int_of_string (argv.[1])
  end
  val [n:int] n = int1_of n
  val () = assert (n >= 0)
  val () = $Rand.srand48_with_time ()
  val passwd = array_make_elt<char> (size1_of_int1 n, '\000')
  val () = loop (n, 0) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (n: int n, i: int i):<cloref1> void =
      if (i < n) then let
        val c = char_of_int ($Rand.randint (94) + 33)
      in
        passwd[i] := c; loop (n, i+1)
      end
  } // end of [where]
  val () = loop (n, 0) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (n: int n, i: int i):<cloref1> void =
      if (i < n) then (print passwd[i]; loop (n, i+1))
  } // end of [where]
  val () = print_newline ()
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [passwdgen.dats] *)
