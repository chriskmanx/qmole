//
// K&R, 2nd edition, page 62
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

extern fun strbuf_swap {m,n:nat | n < m}
  (s: &strbuf (m, n), i: natLte n, j: natLte n):<> void
  = "strbuf_swap"

extern fun reverse {m,n:nat | n < m} (s: &strbuf (m, n)): void
  = "strbuf_reverse"

implement reverse {m,n} (s) =
  loop {0,n} (s, 0, n - 1) where {
  val n = strbuf_length s; val n = int1_of_size1 n
  fun loop {i,j:nat | i+j==n} .<j>.
    (s: &strbuf (m, n), i: int i, j1: int (j-1)): void =
    if i < j1 then begin
      strbuf_swap (s, i, j1); loop {i+1,j-1} (s, i+1, j1-1)
    end // end of [if]
} // end of [reverse]

%{^

ats_void_type strbuf_swap
   (ats_ref_type s0, ats_int_type i, ats_int_type j)
{
  char *s = s0; int c ;
  c = s[i]; s[i] = s[j]; s[j] = c ;
  return ;
}

%}

(* ****** ****** *)

#define BUFLEN 1024
val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

implement main () = let
  var !p_buf with pf_buf = @[byte][BUFLEN]()
  val () = strbuf_initialize_substring (pf_buf | p_buf, alphabet, 0, 26)
  val () = print "reverse bef: "
  val () = print_string (__cast p_buf) where {
    extern castfn __cast (p: ptr): string 
  }
  val () = print_newline ()
  val () = print "reverse aft: " 
  val () = reverse (!p_buf)
  val () = print_string (__cast p_buf) where {
    extern castfn __cast (p: ptr): string 
  }
  val () = print_newline ()
  prval () = pf_buf := bytes_v_of_strbuf_v (pf_buf)
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [reverse.dats] *)
