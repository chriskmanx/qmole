//
// K&R, 2nd edition, page 29
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(*
** Handling C strings (byte sequences ending with the null byte)
** in ATS is a constant challenge. This example makes use of several
** advanced features in ATS that are probably difficult for a beginner
** to grasp. So skip it if you find it to be the case.
*)

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(****** ****** *)
 
#define MAXLINE 1000
typedef b0ytes (n: int) = @[byte?][n]

(* ****** ****** *)

// implemented in C
extern
fun getline {m:pos} {l:addr}
  (pf_buf: b0ytes m @ l | p_buf: ptr l, m: int m)
  : [n:nat | n < m] (strbuf (m, n) @ l  | int n)
  = "__getline"
// end of [getline]

(* ****** ****** *)

// implemented in C
extern
fun copy {m,n:nat | n < m} {l_to,l_from:addr} (
    pf_to: !b0ytes m @ l_to >> strbuf (m, n) @ l_to
  , pf_from: !strbuf (m, n) @ l_from
  | p_to: ptr l_to, p_from: ptr l_from
  ) : void
  = "__copy"
// end of [copy]

(* ****** ****** *)

#define s2b bytes_v_of_strbuf_v // from [prelude/SATS/string.sats]

(*
** It is formally verified in the type system of ATS that there is
** *no* possibility of buffer overlow in this implementation under the
** assumption that both [getline] and [copy] are implemented correctly
** in C.
*)

implement main () = let
  #define M MAXLINE
  var !p_line with pf_line = @[byte][M]()
  var !p_longest with pf_longest = @[byte][M]()
  val () = bytes_strbuf_trans (pf_longest | p_longest, 0)
  val max = loop (pf_line, pf_longest | p_line, p_longest, 0) where {
    fun loop {max:nat | max < M} (
        pf_line: !b0ytes M @ p_line
      , pf_longest: !strbuf (M, max) @ p_longest >> strbuf (M, max) @ p_longest
      | p_line: ptr p_line, p_longest: ptr p_longest, max: int (max)
      ) : #[max: nat | max < M] int max = let
      val (pf_line_new | n) = getline (pf_line | p_line, M)
    in
      if n = 0 then let
        prval () = pf_line := s2b (pf_line_new)
      in
        max // loop exits
      end else begin
        if max < n then let
          prval () = pf_longest := s2b (pf_longest)
          val () = copy (pf_longest, pf_line_new | p_longest, p_line)
          prval () = pf_line := s2b (pf_line_new)
        in
          loop (pf_line, pf_longest | p_line, p_longest, n)
        end else let
          prval () = pf_line := s2b (pf_line_new)
        in
          loop (pf_line, pf_longest | p_line, p_longest, max)
        end (* endif *)
      end // end of [if]
    end (* end of [loop] *)
  }
in
  if (max > 0) then let
    val () = print_string (str) where {
      extern castfn string_of_ptr (p: ptr): string
      val str = string_of_ptr (p_longest)
    } // end of [val]
    prval () = pf_longest := s2b (pf_longest)
  in
    // empty
  end else let
    prval () = pf_longest := s2b (pf_longest)
  in
    // empty
  end // end of [if]
end // end of [main]

(* ****** ****** *)

%{$

ats_int_type
__getline (
  ats_ptr_type s0, ats_int_type lim
) {
  int c, i; char *s = (char*)s0 ;
  for (i = 0; i < lim-1 && (c=getchar()) != EOF && c!='\n'; ++i)
    s[i] = c;
  if (c == '\n') { s[i] = c; ++i; }
  s[i] = '\0';
  return i;
} // end of [__getline]

ats_void_type
__copy (
  ats_ptr_type to, ats_ptr_type from
) {
  int i;
  i = 0;
  while ((((char*)to)[i] = ((char*)from)[i]) != '\0') ++i;
  return ;
} // end of [__copy]

%} // end of [%{$]

(* ****** ****** *)

(* end of [longest_line.dats] *)
