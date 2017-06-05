//
// November, 2005:
// A naive implementation of the wc program
// This is one of the first examples written in ATS
//
// author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
// February, 2007:
// ported to ATS/Geizella by Hongwei Xi

staload "libc/SATS/stdio.sats"

typedef two = [b:two] int b
viewdef bytes_v (n:int, l:addr) = bytes n @ l

extern
fun array_byte_ptr_alloc {n:nat}
  (n: int n):<!ref> [l:addr] (array_v (byte, n, l) | ptr l)
  = "ats_array_byte_ptr_make"

and array_byte_ptr_free {n:nat} {l:addr}
  (pf: array_v (byte, n, l) | p: ptr l): void
  = "ats_array_byte_ptr_free"

(* ****** ****** *)

fn* wc_aux_1 {n:nat} {l_buf,l1,l2,l3:addr}
   (pf1: !Nat @ l1, pf2: !Nat @ l2, pf3: !Nat @ l3 |
    fil: &FILE r, buf: &bytes n,
    n: int n, inword: two, lc: ptr l1, wc: ptr l2, cc: ptr l3)
  : void = let
  val [m:int] m =
    fread_byte (file_mode_lte_r_r | buf, size1_of_int1 n, fil)
  val m = int1_of_size1 (m)
(*
  val () = printf ("wc_aux_1: m = %i\n", @(m))
*)
in
  if m > 0 then begin
    !cc := !cc + m;
    wc_aux_2 {n,m,m}
      (pf1, pf2, pf3 | fil, buf, n, m, m, inword, lc, wc, cc)
  end
end // end of [wc_aux_1]

and wc_aux_2
  {n,m,i:nat | i <= m; m <= n} {l_buf,l1,l2,l3:addr}
  (pf1: !Nat @ l1, pf2: !Nat @ l2, pf3: !Nat @ l3 |
   fil: &FILE r, buf: &bytes n,
   n: int n, m: int m, i: int i, inword: two,
   lc: ptr l1, wc: ptr l2, cc: ptr l3): void = begin
  if i > 0 then let
    val c = char_of_byte buf.[m-i]
  in
    if c = '\n' then begin
      !lc := !lc + 1; !wc := !wc + inword;
      wc_aux_2 {n,m,i-1}
        (pf1, pf2, pf3 | fil, buf, n, m, i-1, 0, lc, wc, cc)
    end else if char_isalnum (c) then
      wc_aux_2 {n,m,i-1}
        (pf1, pf2, pf3 | fil, buf, n, m, i-1, 1, lc, wc, cc)
    else begin
      !wc := !wc + inword;
      wc_aux_2 {n,m,i-1}
        (pf1, pf2, pf3 | fil, buf, n, m, i-1, 0, lc, wc, cc)
    end
  end else begin wc_aux_1 {n}
    (pf1, pf2, pf3 | fil, buf, n, inword, lc, wc, cc)
  end
end // end of [wc_aux_2]

//

#define BUFSZ 1024

fun wc (filename: string): (Nat, Nat, Nat) = let
  val (pf_fil | ptr_fil) = fopen_exn (filename, file_mode_r)
  val (pfopt | p_buf) = malloc_ngc (BUFSZ)
  val () = assert_errmsg (p_buf > null, #LOCATION)
  prval malloc_v_succ (pf_ngc, pf_buf) = pfopt
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  var lc: Nat = 0
  and wc: Nat = 0
  and cc: Nat = 0
in
  wc_aux_1 (
    view@ lc, view@ wc, view@ cc |
    !ptr_fil, !p_buf, BUFSZ, 0, &lc, &wc, &cc
  );
  free_ngc (pf_ngc, pf_buf | p_buf);
  fclose_exn (pf_fil | ptr_fil);
  @(lc, wc, cc)
end // end of [wc]

//

implement main (argc, argv) = let
   var i: Nat = 1
   var lc_total: Nat = 0
   var wc_total: Nat = 0
   var cc_total: Nat = 0
in
   while (i < argc) let
     val filename = argv.[i]
     val (lc, wc, cc) = wc (filename)
   in
     lc_total := lc_total + lc;
     wc_total := wc_total + wc;
     cc_total := cc_total + cc;
     printf ("%i\t%i\t%i\t%s\n", @(lc, wc, cc, filename));
     i := i + 1;
  end ;

  if argc > 2 then begin
    printf ("%i\t%i\t%i\ttotal\n", @(lc_total, wc_total, cc_total))
  end ;
end // end of [main]

(* ****** ****** *)

(* end of [wc.dats] *)
