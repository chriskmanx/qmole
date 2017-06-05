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

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_CLAPACK_DEBUG 1 // for controlling some debugging code

(* ****** ****** *)

staload
F2C = "contrib/clapack/SATS/f2c.sats"
typedef integer (i:int) =  $F2C.integer (i)
typedef integer =  $F2C.integer
typedef real = $F2C.real
typedef doublereal = $F2C.doublereal
typedef complex = $F2C.complex
typedef doublecomplex = $F2C.doublecomplex

staload "contrib/clapack/SATS/clapack.sats"

(* ****** ****** *)

local

typedef float = real
typedef double = doublereal

typedef ccmplx = complex
typedef zcmplx = doublecomplex

in // in of [local]

(* ****** ****** *)

// lamch: S, D

implement lamch<float> (cmach) = slamch (cmach)
implement lamch<double> (cmach) = dlamch (cmach)

implement lamch_eps<float>   () = slamch ('E')
implement lamch_sfmin<float> () = slamch ('S')
implement lamch_base<float>  () = slamch ('B')
implement lamch_prec<float>  () = slamch ('P')
implement lamch_t<float>     () = slamch ('N')
implement lamch_rnd<float>   () = slamch ('R')
implement lamch_emin<float>  () = slamch ('M')
implement lamch_rmin<float>  () = slamch ('U')
implement lamch_emax<float>  () = slamch ('L')
implement lamch_rmax<float>  () = slamch ('O')

implement lamch_eps<double>   () = dlamch ('E')
implement lamch_sfmin<double> () = dlamch ('S')
implement lamch_base<double>  () = dlamch ('B')
implement lamch_prec<double>  () = dlamch ('P')
implement lamch_t<double>     () = dlamch ('N')
implement lamch_rnd<double>   () = dlamch ('R')
implement lamch_emin<double>  () = dlamch ('M')
implement lamch_rmin<double>  () = dlamch ('U')
implement lamch_emax<double>  () = dlamch ('L')
implement lamch_rmax<double>  () = dlamch ('O')

(* ****** ****** *)

// lange: S, D, C, Z

implement lange<float,float>
  (pf | norm, m, n, a, lda, work) =
  slange (pf | norm, m, n, a, lda, work)
// end of [lange<float,float>]

implement lange<double,double>
  (pf | norm, m, n, a, lda, work) =
  dlange (pf | norm, m, n, a, lda, work)
// end of [lange<double,double>]

implement lange<ccmplx,float>
  (pf | norm, m, n, a, lda, work) =
  clange (pf | norm, m, n, a, lda, work)
// end of [lange<ccmplx,float>]

implement lange<zcmplx,double>
  (pf | norm, m, n, a, lda, work) =
  zlange (pf | norm, m, n, a, lda, work)
// end of [lange<zcmplx,double>]

(* ****** ****** *)

// lange_inf: S, D, C, Z

implement{t1,t2} lange_inf
  {m,n} {lda} {m1} (m, n, a, lda, work) = let
  stavar l_work: addr
  val p_work: ptr l_work = &work
  prval pf_work = view@ work
  prval pf_lange = langework_v_some {t2,'I',m1,l_work} (pf_work)
  val res = lange<t1,t2> (pf_lange | ClapackNormInf, m, n, a, lda, p_work)
  prval langework_v_some (pf_work) = pf_lange
  prval () = view@ work := pf_work
in
  res
end // end of [lange_inf]

implement{t1,t2} lange_one
  {m,n} {lda} (m, n, a, lda) = let
  prval pf_lange = langework_v_none {t2, 'O', m+1, null} ()
  val res = lange<t1,t2> (pf_lange | ClapackNormOne, m, n, a, lda, null)
  prval langework_v_none () = pf_lange
in
  res
end // end of [lange_one]

implement{t1,t2} lange_max
  {m,n} {lda} (m, n, a, lda) = let
  prval pf_lange = langework_v_none {t2, 'M', m+1, null} ()
  val res = lange<t1,t2> (pf_lange | ClapackNormMax, m, n, a, lda, null)
  prval langework_v_none () = pf_lange
in
  res
end // end of [lange_max]

implement{t1,t2} lange_frob
  {m,n} {lda} (m, n, a, lda) = let
  prval pf_lange = langework_v_none {t2, 'F', m+1, null} ()
  val res = lange<t1,t2> (pf_lange | ClapackNormFrob, m, n, a, lda, null)
  prval langework_v_none () = pf_lange
in
  res
end // end of [lange_frob]

(* ****** ****** *)

//
// lacpy: S, D, C, Z
//

implement lacpy<float>
  (uln, m, n, a, lda, b, ldb) = slacpy (uln, m, n, a, lda, b, ldb)
// end of [lacpy<float>]

implement lacpy<double>
  (uln, m, n, a, lda, b, ldb) = dlacpy (uln, m, n, a, lda, b, ldb)
// end of [lacpy<double>]

implement lacpy<ccmplx>
  (uln, m, n, a, lda, b, ldb) = clacpy (uln, m, n, a, lda, b, ldb)
// end of [lacpy<ccmplx>]

implement lacpy<zcmplx>
  (uln, m, n, a, lda, b, ldb) = zlacpy (uln, m, n, a, lda, b, ldb)
// end of [lacpy<zcmplx>]

(* ****** ****** *)

implement gelqf<float>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  sgelqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gelqf<float>]

implement gelqf<double>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  dgelqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gelqf<double>]

implement gelqf<ccmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  cgelqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gelqf<ccmplx>]

implement gelqf<zcmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  zgelqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gelqf<zcmplx>]

implement{t} gelqf_exn
  (pfa, pftau | m, n, a, lda, tau, work, lwork) = let
  val [err:int] (pflq | info) =
    gelqf<t> (pfa, pftau | m, n, a, lda, tau, work, lwork)
  // end of [val]
//
  val () = $effmask_all (if (info <> 0) then begin
    prerr "exit(ATS/CLAPACK): [gelqf] failed."; prerr_newline (); exit (1)
  end) : [err==0] void
//
in
  (pflq | ())
end // end of [gelqf_exn]

(* ****** ****** *)

typedef gelqf_dummy_type (t:t@ype) = (
    (*m:*) integer, (*n:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

extern fun{t:t@ype} gelqf_dummy: gelqf_dummy_type (t)
  = "atsctrb_clapack_gelqf"
  
extern fun sgelqf_dummy: gelqf_dummy_type (float)
  = "atsctrb_clapack_sgelqf"

extern fun dgelqf_dummy: gelqf_dummy_type (double)
  = "atsctrb_clapack_dgelqf"

extern fun cgelqf_dummy: gelqf_dummy_type (ccmplx)
  = "atsctrb_clapack_cgelqf"

extern fun zgelqf_dummy: gelqf_dummy_type (zcmplx)
  = "atsctrb_clapack_zgelqf"
  
implement gelqf_dummy<float> (m, n, a, lda, tau, work, lwork) =
  sgelqf_dummy (m, n, a, lda, tau, work, lwork)

implement gelqf_dummy<double> (m, n, a, lda, tau, work, lwork) =
  dgelqf_dummy (m, n, a, lda, tau, work, lwork)

implement gelqf_dummy<ccmplx> (m, n, a, lda, tau, work, lwork) =
  cgelqf_dummy (m, n, a, lda, tau, work, lwork)

implement gelqf_dummy<zcmplx> (m, n, a, lda, tau, work, lwork) =
  zgelqf_dummy (m, n, a, lda, tau, work, lwork)

(* ****** ****** *)

// ormlq: S, D (and unmlq: C, Z)

implement ormlq<float>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormlq<float>]

implement ormlq<double>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormlq<double>]

implement ormlq<ccmplx>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormlq<ccmplx>]

implement ormlq<zcmplx>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormlq<zcmplx>]

// (ormlq: S, D) and unmlq: C, Z

implement unmlq<float>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmlq<float>]

implement unmlq<double>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmlq<double>]

implement unmlq<ccmplx>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmlq<ccmplx>]

implement unmlq<zcmplx>
  (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmlq (pfsd, pflq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmlq<zcmplx>]

(* ****** ****** *)

typedef unmlq_dummy_type (t:t@ype) = {lr:side} {tr:transpose} (
    (*side:*) CLAPACK_SIDE_t lr
  , (*trans:*) CLAPACK_TRANSPOSE_t tr
  , (*m:*) integer, (*n:*) integer, (*k:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*c:*) ptr, (*ldc:*) integer
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

stadef ormlq_dummy_type = unmlq_dummy_type

extern fun{t:t@ype} ormlq_dummy: ormlq_dummy_type (t)
  = "atsctrb_clapack_ormlq"
  
extern fun{t:t@ype} unmlq_dummy: unmlq_dummy_type (t)
  = "atsctrb_clapack_unmlq"
  
extern fun sormlq_dummy: ormlq_dummy_type (float)
  = "atsctrb_clapack_sormlq"

extern fun dormlq_dummy: ormlq_dummy_type (double)
  = "atsctrb_clapack_dormlq"
  
extern fun cunmlq_dummy: unmlq_dummy_type (ccmplx)
  = "atsctrb_clapack_cunmlq"

extern fun zunmlq_dummy: unmlq_dummy_type (zcmplx)
  = "atsctrb_clapack_zunmlq"

// ormlq_dummy

implement ormlq_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormlq_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormlq_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormlq_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

// unmlq_dummy

implement unmlq_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmlq_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmlq_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmlq_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormlq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

(* ****** ****** *)

implement geqlf<float>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  sgeqlf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqlf<float>]

implement geqlf<double>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  dgeqlf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqlf<double>]

implement geqlf<ccmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  cgeqlf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqlf<ccmplx>]

implement geqlf<zcmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  zgeqlf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqlf<zcmplx>]

implement{t} geqlf_exn
  (pfa, pftau | m, n, a, lda, tau, work, lwork) = let
  val [err:int] (pfql | info) =
    geqlf<t> (pfa, pftau | m, n, a, lda, tau, work, lwork)
  // end of [val]
//
  val () = $effmask_all (if (info <> 0) then begin
    prerr "exit(ATS/CLAPACK): [geqlf] failed."; prerr_newline (); exit (1)
  end) : [err==0] void
//
in
  (pfql | ())
end // end of [geqlf_exn]

(* ****** ****** *)

typedef geqlf_dummy_type (t:t@ype) = (
    (*m:*) integer, (*n:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

extern fun{t:t@ype} geqlf_dummy: geqlf_dummy_type (t)
  = "atsctrb_clapack_geqlf"
  
extern fun sgeqlf_dummy: geqlf_dummy_type (float)
  = "atsctrb_clapack_sgeqlf"

extern fun dgeqlf_dummy: geqlf_dummy_type (double)
  = "atsctrb_clapack_dgeqlf"

extern fun cgeqlf_dummy: geqlf_dummy_type (ccmplx)
  = "atsctrb_clapack_cgeqlf"

extern fun zgeqlf_dummy: geqlf_dummy_type (zcmplx)
  = "atsctrb_clapack_zgeqlf"
  
implement geqlf_dummy<float> (m, n, a, lda, tau, work, lwork) =
  sgeqlf_dummy (m, n, a, lda, tau, work, lwork)

implement geqlf_dummy<double> (m, n, a, lda, tau, work, lwork) =
  dgeqlf_dummy (m, n, a, lda, tau, work, lwork)

implement geqlf_dummy<ccmplx> (m, n, a, lda, tau, work, lwork) =
  cgeqlf_dummy (m, n, a, lda, tau, work, lwork)

implement geqlf_dummy<zcmplx> (m, n, a, lda, tau, work, lwork) =
  zgeqlf_dummy (m, n, a, lda, tau, work, lwork)

(* ****** ****** *)

// ormql: S, D (and unmql: C, Z)

implement ormql<float>
  (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormql (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormql<float>]

implement ormql<double>
  (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormql (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormql<double>]

implement ormql<ccmplx>
  (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmql (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormql<ccmplx>]

implement ormql<zcmplx>
  (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmql (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormql<zcmplx>]

// (ormql: S, D) and unmql: C, Z

implement unmql<float>
  (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormql (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmql<float>]

implement unmql<double>
  (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormql (pfsd, pfql | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmql<double>]

implement unmql<ccmplx>
  (pf, pfql | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork) =
  cunmql (pf, pfql | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork)
// end of [unmql<ccmplx>]

implement unmql<zcmplx>
  (pf, pfql | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork) =
  zunmql (pf, pfql | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork)
// end of [unmql<zcmplx>]

(* ****** ****** *)

(* ****** ****** *)

typedef unmql_dummy_type (t:t@ype) = {lr:side} {tr:transpose} (
    (*side:*) CLAPACK_SIDE_t lr
  , (*trans:*) CLAPACK_TRANSPOSE_t tr
  , (*m:*) integer, (*n:*) integer, (*k:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*c:*) ptr, (*ldc:*) integer
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

stadef ormql_dummy_type = unmql_dummy_type

extern fun{t:t@ype} ormql_dummy: ormql_dummy_type (t)
  = "atsctrb_clapack_ormql"
  
extern fun{t:t@ype} unmql_dummy: unmql_dummy_type (t)
  = "atsctrb_clapack_unmql"
  
extern fun sormql_dummy: ormql_dummy_type (float)
  = "atsctrb_clapack_sormql"

extern fun dormql_dummy: ormql_dummy_type (double)
  = "atsctrb_clapack_dormql"
  
extern fun cunmql_dummy: unmql_dummy_type (ccmplx)
  = "atsctrb_clapack_cunmql"

extern fun zunmql_dummy: unmql_dummy_type (zcmplx)
  = "atsctrb_clapack_zunmql"

// ormql_dummy

implement ormql_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormql_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormql_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormql_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

// unmql_dummy

implement unmql_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmql_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmql_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmql_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormql_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

(* ****** ****** *)

implement geqrf<float>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  sgeqrf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqrf<float>]

implement geqrf<double>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  dgeqrf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqrf<double>]

implement geqrf<ccmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  cgeqrf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqrf<ccmplx>]

implement geqrf<zcmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  zgeqrf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [geqrf<zcmplx>]

implement{t} geqrf_exn
  (pfa, pftau | m, n, a, lda, tau, work, lwork) = let
  val [err:int] (pfqr | info) =
    geqrf<t> (pfa, pftau | m, n, a, lda, tau, work, lwork)
  // end of [val]
//
  val () = $effmask_all (if (info <> 0) then begin
    prerr "exit(ATS/CLAPACK): [geqrf] failed."; prerr_newline (); exit (1)
  end) : [err==0] void
//
in
  (pfqr | ())
end // end of [geqrf_exn]

(* ****** ****** *)

typedef geqrf_dummy_type (t:t@ype) = (
    (*m:*) integer, (*n:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

extern fun{t:t@ype} geqrf_dummy: geqrf_dummy_type (t)
  = "atsctrb_clapack_geqrf"
  
extern fun sgeqrf_dummy: geqrf_dummy_type (float)
  = "atsctrb_clapack_sgeqrf"

extern fun dgeqrf_dummy: geqrf_dummy_type (double)
  = "atsctrb_clapack_dgeqrf"

extern fun cgeqrf_dummy: geqrf_dummy_type (ccmplx)
  = "atsctrb_clapack_cgeqrf"

extern fun zgeqrf_dummy: geqrf_dummy_type (zcmplx)
  = "atsctrb_clapack_zgeqrf"
  
implement geqrf_dummy<float> (m, n, a, lda, tau, work, lwork) =
  sgeqrf_dummy (m, n, a, lda, tau, work, lwork)

implement geqrf_dummy<double> (m, n, a, lda, tau, work, lwork) =
  dgeqrf_dummy (m, n, a, lda, tau, work, lwork)

implement geqrf_dummy<ccmplx> (m, n, a, lda, tau, work, lwork) =
  cgeqrf_dummy (m, n, a, lda, tau, work, lwork)

implement geqrf_dummy<zcmplx> (m, n, a, lda, tau, work, lwork) =
  zgeqrf_dummy (m, n, a, lda, tau, work, lwork)

(* ****** ****** *)

// ormqr: S, D (and unmqr: C, Z)

implement ormqr<float>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormqr<float>]

implement ormqr<double>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormqr<double>]

implement ormqr<ccmplx>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormqr<ccmplx>]

implement ormqr<zcmplx>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormqr<zcmplx>]

// unmqr: C, Z and (ormqr: S, D)

implement unmqr<float>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmqr<float>]

implement unmqr<double>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmqr<double>]

implement unmqr<ccmplx>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork) =
  cunmqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork)
// end of [unmqr<ccmplx>]

implement unmqr<zcmplx>
  (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork) =
  zunmqr (pfsd, pfqr | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork)
// end of [unmqr<zcmplx>]

(* ****** ****** *)

typedef unmqr_dummy_type (t:t@ype) = {lr:side} {tr:transpose} (
    (*side:*) CLAPACK_SIDE_t lr
  , (*trans:*) CLAPACK_TRANSPOSE_t tr
  , (*m:*) integer, (*n:*) integer, (*k:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*c:*) ptr, (*ldc:*) integer
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

stadef ormqr_dummy_type = unmqr_dummy_type

extern fun{t:t@ype} ormqr_dummy: ormqr_dummy_type (t)
  = "atsctrb_clapack_ormqr"
  
extern fun{t:t@ype} unmqr_dummy: unmqr_dummy_type (t)
  = "atsctrb_clapack_unmqr"
  
extern fun sormqr_dummy: ormqr_dummy_type (float)
  = "atsctrb_clapack_sormqr"

extern fun dormqr_dummy: ormqr_dummy_type (double)
  = "atsctrb_clapack_dormqr"
  
extern fun cunmqr_dummy: unmqr_dummy_type (ccmplx)
  = "atsctrb_clapack_cunmqr"

extern fun zunmqr_dummy: unmqr_dummy_type (zcmplx)
  = "atsctrb_clapack_zunmqr"

// ormqr_dummy

implement ormqr_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormqr_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormqr_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormqr_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

// unmqr_dummy

implement unmqr_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmqr_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmqr_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmqr_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormqr_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

(* ****** ****** *)

implement gerqf<float>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  sgerqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gerqf<float>]

implement gerqf<double>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  dgerqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gerqf<double>]

implement gerqf<ccmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  cgerqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gerqf<ccmplx>]

implement gerqf<zcmplx>
  (pfa, pftau | m, n, a, lda, tau, work, lwork) =
  zgerqf (pfa, pftau | m, n, a, lda, tau, work, lwork)
// end of [gerqf<zcmplx>]

implement{t} gerqf_exn
  (pfa, pftau | m, n, a, lda, tau, work, lwork) = let
  val [err:int] (pfrq | info) =
    gerqf<t> (pfa, pftau | m, n, a, lda, tau, work, lwork)
  // end of [val]
//
  val () = $effmask_all (if (info <> 0) then begin
    prerr "exit(ATS/CLAPACK): [gerqf] failed."; prerr_newline (); exit (1)
  end) : [err==0] void
//
in
  (pfrq | ())
end // end of [gerqf_exn]

(* ****** ****** *)

typedef gerqf_dummy_type (t:t@ype) = (
    (*m:*) integer, (*n:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

extern fun{t:t@ype} gerqf_dummy: gerqf_dummy_type (t)
  = "atsctrb_clapack_gerqf"
  
extern fun sgerqf_dummy: gerqf_dummy_type (float)
  = "atsctrb_clapack_sgerqf"

extern fun dgerqf_dummy: gerqf_dummy_type (double)
  = "atsctrb_clapack_dgerqf"

extern fun cgerqf_dummy: gerqf_dummy_type (ccmplx)
  = "atsctrb_clapack_cgerqf"

extern fun zgerqf_dummy: gerqf_dummy_type (zcmplx)
  = "atsctrb_clapack_zgerqf"
  
implement gerqf_dummy<float> (m, n, a, lda, tau, work, lwork) =
  sgerqf_dummy (m, n, a, lda, tau, work, lwork)

implement gerqf_dummy<double> (m, n, a, lda, tau, work, lwork) =
  dgerqf_dummy (m, n, a, lda, tau, work, lwork)

implement gerqf_dummy<ccmplx> (m, n, a, lda, tau, work, lwork) =
  cgerqf_dummy (m, n, a, lda, tau, work, lwork)

implement gerqf_dummy<zcmplx> (m, n, a, lda, tau, work, lwork) =
  zgerqf_dummy (m, n, a, lda, tau, work, lwork)

(* ****** ****** *)

implement ormrq<float>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormrq<float>]

implement ormrq<double>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormrq<double>]

implement ormrq<ccmplx>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormrq<ccmplx>]

implement ormrq<zcmplx>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [ormrq<zcmplx>]

implement unmrq<float>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmrq<float>]

implement unmrq<double>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)
// end of [unmrq<double>]

implement unmrq<ccmplx>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork) =
  cunmrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork)
// end of [unmrq<ccmplx>]

implement unmrq<zcmplx>
  (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork) =
  zunmrq (pfsd, pfrq | side, trans, m, n, k, a, lda, tau, c__, ldc, work, lwork)
// end of [unmrq<zcmplx>]

(* ****** ****** *)
(* ****** ****** *)

typedef unmrq_dummy_type (t:t@ype) = {lr:side} {tr:transpose} (
    (*side:*) CLAPACK_SIDE_t lr
  , (*trans:*) CLAPACK_TRANSPOSE_t tr
  , (*m:*) integer, (*n:*) integer, (*k:*) integer
  , (*a:*) ptr, (*lda:*) integer, (*tau:*) ptr
  , (*c:*) ptr, (*ldc:*) integer
  , (*work:*) ptr, (*lwork:*) integer
  ) -<fun> int

stadef ormrq_dummy_type = unmrq_dummy_type

extern fun{t:t@ype} ormrq_dummy: ormrq_dummy_type (t)
  = "atsctrb_clapack_ormrq"
  
extern fun{t:t@ype} unmrq_dummy: unmrq_dummy_type (t)
  = "atsctrb_clapack_unmrq"
  
extern fun sormrq_dummy: ormrq_dummy_type (float)
  = "atsctrb_clapack_sormrq"

extern fun dormrq_dummy: ormrq_dummy_type (double)
  = "atsctrb_clapack_dormrq"
  
extern fun cunmrq_dummy: unmrq_dummy_type (ccmplx)
  = "atsctrb_clapack_cunmrq"

extern fun zunmrq_dummy: unmrq_dummy_type (zcmplx)
  = "atsctrb_clapack_zunmrq"

// ormrq_dummy

implement ormrq_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormrq_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormrq_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement ormrq_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

// unmrq_dummy

implement unmrq_dummy<ccmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  cunmrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmrq_dummy<zcmplx>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  zunmrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmrq_dummy<float>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  sormrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

implement unmrq_dummy<double>
  (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork) =
  dormrq_dummy (side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork)

(* ****** ****** *)

(*

/* Subroutine */
int dgels_(
  char *trans
, integer *m, integer *n
, integer *nrhs
, doublereal *a, integer *lda
, doublereal *b, integer *ldb
, doublereal *work, integer *lwork
, integer *info
) ;

*)

// gels: S, D, C, Z

implement gels<float> (
    pf_lwork
  | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) = sgels (
  pf_lwork | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) // end of [gels<float>]

implement gels<double> (
    pf_lwork
  | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) = dgels (
  pf_lwork | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) // end of [gels<double>]

implement gels<ccmplx> (
    pf_lwork
  | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) = cgels (
  pf_lwork | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) // end of [gels<ccmplx>]

implement gels<zcmplx> (
    pf_lwork
  | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) = zgels (
  pf_lwork | trans, m, n, nrhs, a, lda, b, ldb, work, lwork
) // end of [gels<zcmplx>]

(* ****** ****** *)

(*

/* Subroutine */
int dtrtrs_(
  char *uplo
, char *trans
, char *diag
, integer *n
, integer *nrhs
, doublereal *a, integer *lda
, doublereal *b, integer *ldb
, integer *info
) ;

*)

implement trtrs<float>
  (uplo, trans, diag, n, nhrs, a, lda, b, ldb) =
  strtrs (uplo, trans, diag, n, nhrs, a, lda, b, ldb)
// end of [trtrs<float>]

implement trtrs<double>
  (uplo, trans, diag, n, nhrs, a, lda, b, ldb) =
  dtrtrs (uplo, trans, diag, n, nhrs, a, lda, b, ldb)
// end of [trtrs<double>]

implement trtrs<ccmplx>
  (uplo, trans, diag, n, nhrs, a, lda, b, ldb) =
  ctrtrs (uplo, trans, diag, n, nhrs, a, lda, b, ldb)
// end of [trtrs<ccmplx>]

implement trtrs<zcmplx>
  (uplo, trans, diag, n, nhrs, a, lda, b, ldb) =
  ztrtrs (uplo, trans, diag, n, nhrs, a, lda, b, ldb)
// end of [trtrs<zcmplx>]


(* ****** ****** *)

// getrf: S, D, C, Z

(*

/* Subroutine */
int dgetrf_(
  integer *m, integer *n
, doublereal *a, integer *lda
, integer *ipiv
, integer *info
) ;

*)

implement getrf<float>
  (pfa | m, n, a, lda, ipiv) = sgetrf (pfa | m, n, a, lda, ipiv)
// end of [getrf<float>]

implement getrf<double>
  (pfa | m, n, a, lda, ipiv) = dgetrf (pfa | m, n, a, lda, ipiv)
// end of [getrf<double>]

implement getrf<ccmplx>
  (pfa | m, n, a, lda, ipiv) = cgetrf (pfa | m, n, a, lda, ipiv)
// end of [getrf<ccmplx>]

implement getrf<zcmplx>
  (pfa | m, n, a, lda, ipiv) = zgetrf (pfa | m, n, a, lda, ipiv)
// end of [getrf<zcmplx>]

implement{t}
getrf_exn (pfa | m, n, a, lda, ipiv) = let
  val [info:int] (pflu | info) = getrf<t> (pfa | m, n, a, lda, ipiv)
  val () = $effmask_all (if (info < 0) then begin
    prerr "exit(ATS/CLAPACK): [getrf] failed."; prerr_newline (); exit (1)
  end) (* end of [val] *)
  val () = assert (info >= 0)
in
  (pflu | info)
end // end of [getrf_exn]

(* ****** ****** *)

// laswp: S, D, C, Z

(*

/* Subroutine */
int dlaswp_(
  integer *n
, doublereal *a, integer *lda
, integer *k1, integer *k2
, integer *ipiv
, integer *incx
) ;

*)

implement laswp<float>
  (n, a, lda, k1, k2, ipiv, incx) = slaswp (n, a, lda, k1, k2, ipiv, incx)

implement laswp<double>
  (n, a, lda, k1, k2, ipiv, incx) = dlaswp (n, a, lda, k1, k2, ipiv, incx)

implement laswp<ccmplx>
  (n, a, lda, k1, k2, ipiv, incx) = claswp (n, a, lda, k1, k2, ipiv, incx)

implement laswp<zcmplx>
  (n, a, lda, k1, k2, ipiv, incx) = zlaswp (n, a, lda, k1, k2, ipiv, incx)

(* ****** ****** *)

// gesv: S, D, C, Z

(*

/* Subroutine */
int dgesv_ (
  integer *n, integer *nrhs
, doublereal *a, integer *lda
, integer *ipiv
, doublereal *b, integer *ldb
, integer *info
) ;

*)

implement gesv<float>
  (pf | n, nrhs, a, lda, ipiv, b, ldb) = sgesv (pf | n, nrhs, a, lda, ipiv, b, ldb)
// end of [gesv<float>]

implement gesv<double>
  (pf | n, nrhs, a, lda, ipiv, b, ldb) = dgesv (pf | n, nrhs, a, lda, ipiv, b, ldb)
// end of [gesv<double>]

implement gesv<ccmplx>
  (pf | n, nrhs, a, lda, ipiv, b, ldb) = cgesv (pf | n, nrhs, a, lda, ipiv, b, ldb)
// end of [gesv<ccmplx>]

implement gesv<zcmplx>
  (pf | n, nrhs, a, lda, ipiv, b, ldb) = zgesv (pf | n, nrhs, a, lda, ipiv, b, ldb)
// end of [gesv<zcmplx>]

(* ****** ****** *)

typedef
gesvd_dummy_type
  (t1:t@ype,t2:t@ype) = {m,n:nat} (
  (*jobu:*) char, (*jobvt:*) char
, (*m:*) integer m, (*n:*) integer n
, (*a:*) ptr, (*lda:*) integer, (*s:*) ptr
, (*u:*) ptr, (*ldu:*) integer, (*vt:*) ptr, (*ldvt:*) integer
, (*work:*) ptr, (*lwork:*) integer
) -<fun> int

extern
fun{t1,t2:t@ype}
gesvd_dummy: gesvd_dummy_type (t1, t2) = "atsctrb_clapack_gesvd"

typedef
gesvd_c_dummy_type
  (t1:t@ype,t2:t@ype) = {m,n:nat} (
  (*jobu:*) char, (*jobvt:*) char
, (*m:*) integer m, (*n:*) integer n
, (*a:*) ptr, (*lda:*) integer, (*s:*) ptr
, (*u:*) ptr, (*ldu:*) integer, (*vt:*) ptr, (*ldvt:*) integer
, (*work:*) ptr, (*lwork:*) integer, (*rwork*) ptr
) -<fun> int

extern fun{t1,t2:t@ype}
gesvd_c_dummy: gesvd_c_dummy_type (t1, t2) = "atsctrb_clapack_gesvd_c"

extern fun
sgesvd_dummy: gesvd_dummy_type (real, real) = "atsctrb_clapack_sgesvd"
extern fun
dgesvd_dummy: gesvd_dummy_type (double, double) = "atsctrb_clapack_dgesvd"
extern fun
cgesvd_dummy: gesvd_c_dummy_type (ccmplx, float) = "atsctrb_clapack_cgesvd"
extern fun
zgesvd_dummy: gesvd_c_dummy_type (zcmplx, double) = "atsctrb_clapack_zgesvd"

implement gesvd_dummy<float,float>
  (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork) =
  sgesvd_dummy (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork)
// end of [gesvd_dummy<float,float>]

implement gesvd_dummy<double,double>
  (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork) =
  dgesvd_dummy (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork)
// end of [gesvd_dummy<double,double>]

implement gesvd_dummy<ccmplx,float>
  (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork) = let
  val mn = $F2C.min_integer_integer (m, n)
  val mn = $F2C.size1_of_integer mn // no-op casting
  var !p_rwork = @[float][5*mn]() // could this be dangerous?
in
  cgesvd_dummy (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, p_rwork)
end // end of [gesvd_dummy<ccmplx,float>]

implement gesvd_dummy<zcmplx,double>
  (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork) = let
  val mn = $F2C.min_integer_integer (m, n)
  val mn = $F2C.size1_of_integer mn // no-op casting
  var !p_rwork = @[double][5*mn]() // could this be dangerous?
in
  zgesvd_dummy (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, p_rwork)
end // end of [gesvd_dummy<zcmplx,double>]

implement gesvd_c_dummy<ccmplx,float>
  (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork) =
  cgesvd_dummy (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork)
// end of [gesvd_c_dummy<ccmplx,float>]

implement gesvd_c_dummy<zcmplx,double>
  (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork) =
  zgesvd_dummy (jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork)
// end of [gesvd_c_dummy<zcmplx,double>]

(* ****** ****** *)

end // end of [local]

(* end of [clapack.hats] *)
