//
// K&R, 2nd edition, pages 165
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(*

/* fgets: get at most n chars from iop */

char *fgets (char *s, int n, FILE *iop) {
  register int c;
  register char *cs ;
  
  cs = s;
  while (--n > 0 && (c = fgetc(iop)) != EOF)
    if (( *cs++ = c) == '\n') break;
  *cs = '\0';
  return (c == EOF && cs == s) ? (char* )0 ; s ;
} /* end of [fgets] */

*)

(* ****** ****** *)

staload STDIO = "libc/SATS/stdio.sats"

(* ****** ****** *)

dataview fgets_v (sz:int, addr, addr) =
  | {l_buf:addr}
    fgets_v_fail (sz, l_buf, null) of b0ytes (sz) @ l_buf
  | {n:nat | n < sz} {l_buf:addr | l_buf <> null}
    fgets_v_succ (sz, l_buf, l_buf) of strbuf (sz, n) @ l_buf

extern fun fgets
  {m:file_mode} {sz,n:nat | n < sz} {l_buf:addr} (
    _mod: file_mode_lte (m, r), _buf: b0ytes sz @ l_buf
  | p_buf: ptr l_buf, n: int n, iop: &FILE m
) : [l:addr] (fgets_v (sz, l_buf, l) | ptr l)

(* ****** ****** *)

extern praxi lemma_addr_isnot_null
  {n:pos} {l:addr} (pf: !b0ytes n @ l): [l <> null] void

#define i2sz size1_of_int1

implement fgets {m}
  (pf_mod, pf_buf | p_buf, n, iop) = let
  var eof: int = 0
  prval () = lemma_addr_isnot_null (pf_buf)
  prval pf_buf = bytes_v_of_b0ytes_v (pf_buf)
  val nleft = loop (pf_buf | p_buf, n, iop, eof) where {
    fun loop {sz,n:nat | n < sz} {l:addr} .<n>. (
        pf: !bytes sz @ l
      | p: ptr l, n: int n, iop: &FILE m, eof: &int
      ) : natLte n =
      if n > 0 then let
        val c = $STDIO.fgetc1_err (pf_mod | iop)
      in
        if c >= 0 then let // c <> EOF
          prval @(pf1, pf2) = array_v_uncons {byte} (pf)
          val () = !p := byte_of_int1 (c)
          val nleft = begin
            if char_of_int1 c <> '\n' then
              loop (pf2 | p+sizeof<byte>, n-1, iop, eof)
            else n-1
          end : natLt n // end of [val]
          prval () = pf := array_v_cons {byte} (pf1, pf2)
        in
          nleft
        end else begin // c = EOF
          eof := 1; n // loop exits
        end // end of [if]
      end else begin
        0 // loop exists
      end // end of [if]
  } // end of [val]
in
  if eof > 0 then begin
    if nleft < n then let
      val () = bytes_strbuf_trans (pf_buf | p_buf, i2sz (n - nleft))
    in
      (fgets_v_succ pf_buf | p_buf)
    end else begin
      (fgets_v_fail pf_buf | null)
    end // end of [if]
  end else let
    val () = bytes_strbuf_trans (pf_buf | p_buf, i2sz (n - nleft))
  in
    (fgets_v_succ pf_buf | p_buf)
  end // end of [if]
end // end of [fgets]

(* ****** ****** *)

#define BUFSZ 1024

implement main (argc, argv) = let
  #define BUFSZ1 (BUFSZ+1)
  var !p_buf with pf_buf = @[byte][BUFSZ1]()
  stadef l_buf = p_buf
  val (pf_stdin | p_stdin) = stdin_get ()
  val () = loop (pf_buf | p_buf, !p_stdin) where {
    fun loop (
        pf_buf: !b0ytes BUFSZ1 @ l_buf
      | p_buf: ptr l_buf, iop: &FILE r
      ) : void = let
      val (pf1 | p1) =
        fgets (file_mode_lte_r_r, pf_buf | p_buf, BUFSZ, iop)
    in
      if :(pf_buf: b0ytes (BUFSZ+1) @ p_buf) =>
        p1 <> null then let
        prval fgets_v_succ (pf1) = pf1
        val () =
          print_string (__cast p_buf) where {
          extern castfn __cast (p: ptr): string 
        } // end of [val]
        prval () = pf_buf := bytes_v_of_strbuf_v (pf1)
      in
        loop (pf_buf | p_buf, iop)
      end else let
        prval fgets_v_fail (pf1) = pf1; prval () = pf_buf := pf1
      in
        // no more bytes
      end // end of [if]
    end // end of [loop]
  } // end of [val]
  val () = stdin_view_set (pf_stdin | (*none*))
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [fgets.dats] *)
