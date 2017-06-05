(*
** some testing code for functions declared in
** prelude/SATS/string.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009
//

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

#define sbp2str string1_of_strbuf

(* ****** ****** *)

fn test__lt_string_string () = () where {
  val ans = lt_string_string ("abcde", "abcde")
  val () = assert_errmsg (~ans, #LOCATION)
  val ans = lt_string_string ("abcde", "abcdef")
  val () = assert_errmsg (ans, #LOCATION)
  val ans = lt_string_string ("abcde", "bcdef")
  val () = assert_errmsg (ans, #LOCATION)
} // end of [test__lt_string_string]

(* ****** ****** *)

fn test__lte_string_string () = () where {
  val ans = lte_string_string ("abcde", "abcde")
  val () = assert_errmsg (ans, #LOCATION)
  val ans = lte_string_string ("abcde", "abcdef")
  val () = assert_errmsg (ans, #LOCATION)
  val ans = lte_string_string ("abcde", "bcdef")
  val () = assert_errmsg (ans, #LOCATION)
  val ans = lte_string_string ("bcdef", "abcde")
  val () = assert_errmsg (~ans, #LOCATION)
} // end of [test__lte_string_string]

(* ****** ****** *)

fn test__compare_string_string () = () where {
  val sgn = compare ("abcde", "abcde")
  val () = assert_errmsg (sgn = 0, #LOCATION)
  val sgn = compare_string_string ("abcde", "abcdef")
  val () = assert_errmsg (sgn = ~1, #LOCATION)
  val sgn = compare_string_string ("abcde", "bcdef")
  val () = assert_errmsg (sgn = ~1, #LOCATION)
  val sgn = compare_string_string ("bcdef", "abcde")
  val () = assert_errmsg (sgn = 1, #LOCATION)
} // end of [test__compare_string_string]

(* ****** ****** *)

fn test__string_make_substring () = () where {
  val s1 = "abcdefghijklmnopqrstuvwxyz"
  val s2 = string_make_substring (s1, 11, 3)
  val s2 = sbp2str (s2)
  val () = assert_errmsg (s2 = "lmn", #LOCATION)
} // end of [test__string_make_substring]

(* ****** ****** *)

//
// HX-2010-03-24:
// Note that string0_append and string1_append are the same
// at run-time
//
fn test__string_append () = () where {
  val s1 = "Hello, " and s2 = "world!"
  val s12 = sbp2str (s1 + s2)
  val () = assert_errmsg (s12 = "Hello, world!", #LOCATION)
} // end of [test__string_append]

(* ****** ****** *)

fn test__stringlst_concat () = () where {
  val abcdef = $lst {string} ("a", "bc", "def")
  val s = stringlst_concat (abcdef)
  val s = string_of_strptr (s)
  val () = assert_errmsg (s = "abcdef", #LOCATION)
} // end of [test__stringlst_concat]

(* ****** ****** *)

implement main (argc, argv) = let
//
  val () = test__lt_string_string ()
  val () = test__lte_string_string ()
  val () = test__compare_string_string ()
//
  val () = test__string_make_substring ()
  val () = test__string_append ()
  val () = test__stringlst_concat ()
//
  val s1 = "Hello" and s2 = ", world!"
  val s12 = sbp2str (s1 + s2)
//
  val () = begin
    print "s12 (Hello, world!) = "; print s12; print_newline ()
  end // end of [val]
  val cs = string_explode (s12)
  val () = list_vt_free (cs)
//
  val cs = string_explode (s12)
  val s12' = sbp2str (sbp) where {
    val sbp = string_implode (__cast cs) where {
      extern castfn __cast {n:nat} (cs: !list_vt (char, n)): list (char, n)
    } // end of [val]
  } // end of [s12']
  val () = list_vt_free (cs)
  val () = begin
    print "s12' (Hello, world!) = "; print s12'; print_newline ()
  end // end of [val]
  val () = assert_errmsg (s12' = "Hello, world!", #LOCATION)
//
  val s12_upper = sbp2str (string_toupper (s12))
  val () = begin
    print "s12_upper (HELLO, WORLD!) = "; print s12_upper; print_newline ()
  end // end of [val]
  val () = assert_errmsg (s12_upper = "HELLO, WORLD!", #LOCATION)
  val s12_lower = sbp2str (string_tolower (s12))
  val () = begin
    print "s12_lower (hello, world!) = "; print s12_lower; print_newline ()
  end // end of [val]
  val () = assert_errmsg (s12_lower = "hello, world!", #LOCATION)
//
  val ind = string_index_of_string ("abcdefghijklmnopqrstuvwsyz", "def")
  val ind = int1_of_ssize1 (ind)
  val () = begin
    print "ind (3) = "; print ind; print_newline ()
  end // end of [val]
  val () = assert_errmsg (ind = 3, #LOCATION)
//
  val ind = string_index_of_string ("abcdefghijklmnopqrstuvwsyz", "defhij")
  val ind = int1_of_ssize1 (ind)
  val () = begin
    print "ind (-1) = "; print ind; print_newline ()
  end // end of [val]
  val () = assert_errmsg (ind = ~1, #LOCATION)
//
  prval pf = unit_v ()
  val () = string_foreach__main {..} {ptr}
    (pf | "Hello, world!", f, null) where {
    fn f (pf: !unit_v | c: char, _p: !ptr):<1> void = print (c)
  } // end of [val]
  val () = print_newline ()
  prval unit_v () = pf
in
  print "[prelude_string.dats] testing passes!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_string.dats] *)
