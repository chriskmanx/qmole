(*
**
** An interface for ATS to interact with BLAS
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"
staload "contrib/cblas/SATS/cblas.sats"

(* ****** ****** *)

implement
  CBLAS_ORDER_of_ORDER (x) = 
  case+ x of
  | ORDERrow () => CblasRowMajor
  | ORDERcol () => CblasColMajor
// end of [CBLAS_ORDER_of_ORDER]

(*

// HX: as of now, [ORDER_of_CBLAS_ORDER] is not needed

prfun ORDER_istot
  {ord:order} .<>. (): ORDER (ord) =
  scase ord of
  | row () => ORDERrow () | col () => ORDERcol ()
// end of [ORDER_istot]

implement
  ORDER_of_CBLAS_ORDER
  {ord} (cbord) = let
  prval ord = ORDER_istot {ord} ()
in
  if cbord = CblasRowMajor then let
    prval ORDERrow () = ord
  in
    ORDERrow
  end else if cbord = CblasColMajor then let
    prval ORDERcol () = ord
  in
    ORDERcol ()
  end else let
    val () = prerr "exit(ATS/CBLAS): ORDER_of_CBLAS_ORDER: out-of-range"
    val () = prerr_newline ()
  in
    exit (1)
  end // end of [if]
end (* end of [ORDER_of_CBLAS_ORDER] *)

*)

(* ****** ****** *)

implement
CBLAS_UPLO_of_UPLO (x) = case+ x of
  | UPLOupper () => CblasUpper | UPLOlower () => CblasLower
// end of [CBLAS_UPLO_of_UPLO]

(* ****** ****** *)

implement
CBLAS_DIAG_of_DIAG (x) = case+ x of
  | DIAGunit () => CblasUnit | DIAGnonunit () => CblasNonUnit
// end of [CBLAS_DIAG_of_DIAG]

(* ****** ****** *)

implement
CBLAS_TRANSPOSE_of_TRANSPOSE (x) =
  case+ x of
  | TRANSPOSE_N () => CblasNoTrans
  | TRANSPOSE_T () => CblasTrans
  | TRANSPOSE_C () => CblasConjTrans
// end of [CBLAS_TRANSPOSE_of_TRANSPOSE]

(* ****** ****** *)

implement
CBLAS_SIDE_of_SIDE (x) = case+ x of
  | SIDEleft () => CblasLeft | SIDEright () => CblasRight
// end of [CBLAS_SIDE_of_SIDE]

(* ****** ****** *)

(* end of [cblas.dats] *)
