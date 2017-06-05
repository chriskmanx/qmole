/*
**
** An interface for ATS to interact with BLAS
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*/

/* ****** ****** */
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
/* ****** ****** */

#ifndef ATS_CONTRIB_CBLAS_CATS
#define ATS_CONTRIB_CBLAS_CATS

/* ****** ****** */

#include "ats_types.h"
#include "ats_memory.h"

/* ****** ****** */

#include "cblas.h"

/* ****** ****** */

// #include <complex.h>
// typedef float complex ats_fcomplex_type ;
// typedef double complex ats_dcomplex_type ;
// typedef long double complex ats_lcomplex_type ;

/* ****** ****** */

#if (0)

ATSinline()
ats_bool_type
atsctrb_eq_CBLAS_ORDER_ORDER (
  ats_int_type x1, ats_int_type x2
) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atsctrb_eq_CBLAS_ORDER_ORDER] */

ATSinline()
ats_bool_type
atsctrb_eq_CBLAS_UPLO_UPLO (
  ats_int_type x1, ats_int_type x2
) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atsctrb_eq_CBLAS_UPLO_UPLO] */

ATSinline()
ats_bool_type
atsctrb_eq_CBLAS_DIAG_DIAG (
  ats_int_type x1, ats_int_type x2
) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atsctrb_eq_CBLAS_DIAG_DIAG] */

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_eq_CBLAS_TRANSPOSE_TRANSPOSE (
  ats_int_type x1, ats_int_type x2
) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atsctrb_eq_CBLAS_TRANSPOSE_TRANSPOSE] */

ATSinline()
ats_bool_type
atsctrb_eq_CBLAS_SIDE_SIDE (
  ats_int_type x1, ats_int_type x2
) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atsctrb_eq_CBLAS_SIDE_SIDE] */

#endif // end of [#if(0)]

/* ****** ****** */

//
// BLAS level 1
//

/* ****** ****** */

#define atsctrb_cblas_srotg cblas_srotg
#define atsctrb_cblas_drotg cblas_drotg

#if (0)
#define atsctrb_cblas_crotg cblas_crotg
#define atsctrb_cblas_zrotg cblas_zrotg
#endif // end of [0]

/* ****** ****** */

#define atsctrb_cblas_srotmg cblas_srotmg
#define atsctrb_cblas_drotmg cblas_drotmg

/* ****** ****** */

#define atsctrb_cblas_srot cblas_srot
#define atsctrb_cblas_drot cblas_drot

#if (0)
// not in CBLAS proper
#define atsctrb_cblas_csrot cblas_csrot
#define atsctrb_cblas_zdrot cblas_zdrot
#endif // end of [0]

/* ****** ****** */

#define atsctrb_cblas_srotm cblas_srotm
#define atsctrb_cblas_drotm cblas_drotm

/* ****** ****** */

#define atsctrb_cblas_sdsdot cblas_sdsdot
#define atsctrb_cblas_dsdot cblas_dsdot
#define atsctrb_cblas_sdot cblas_sdot
#define atsctrb_cblas_ddot cblas_ddot

/* ****** ****** */

#define atsctrb_cblas_cdotu_sub cblas_cdotu_sub
#define atsctrb_cblas_cdotc_sub cblas_cdotc_sub
#define atsctrb_cblas_zdotu_sub cblas_zdotu_sub
#define atsctrb_cblas_zdotc_sub cblas_zdotc_sub

/* ****** ****** */

#define atsctrb_cblas_snrm2 cblas_snrm2
#define atsctrb_cblas_dnrm2 cblas_dnrm2
#define atsctrb_cblas_scnrm2 cblas_scnrm2 
#define atsctrb_cblas_dznrm2 cblas_dznrm2

/* ****** ****** */

#define atsctrb_cblas_sasum cblas_sasum
#define atsctrb_cblas_dasum cblas_dasum
#define atsctrb_cblas_scasum cblas_scasum
#define atsctrb_cblas_dzasum cblas_dzasum

/* ****** ****** */

#define atsctrb_cblas_isamax cblas_isamax
#define atsctrb_cblas_idamax cblas_idamax
#define atsctrb_cblas_icamax cblas_icamax
#define atsctrb_cblas_izamax cblas_izamax

/* ****** ****** */

#define atsctrb_cblas_sswap cblas_sswap
#define atsctrb_cblas_dswap cblas_dswap
#define atsctrb_cblas_cswap cblas_cswap
#define atsctrb_cblas_zswap cblas_zswap

/* ****** ****** */

#define atsctrb_cblas_scopy cblas_scopy
#define atsctrb_cblas_dcopy cblas_dcopy
#define atsctrb_cblas_ccopy cblas_ccopy
#define atsctrb_cblas_zcopy cblas_zcopy

/* ****** ****** */

#define atsctrb_cblas_saxpy cblas_saxpy
#define atsctrb_cblas_daxpy cblas_daxpy
#define atsctrb_cblas_caxpy cblas_caxpy
#define atsctrb_cblas_zaxpy cblas_zaxpy

/* ****** ****** */

#define atsctrb_cblas_sscal cblas_sscal
#define atsctrb_cblas_dscal cblas_dscal
#define atsctrb_cblas_cscal cblas_cscal
#define atsctrb_cblas_zscal cblas_zscal

#define atsctrb_cblas_csscal cblas_csscal
#define atsctrb_cblas_zdscal cblas_zdscal

/* ****** ****** */

//
// BLAS level 2
//

/* ****** ****** */

/*
** GEMV: S, D, C, Z
*/

#define atsctrb_cblas_sgemv cblas_sgemv
#define atsctrb_cblas_dgemv cblas_dgemv
#define atsctrb_cblas_cgemv cblas_cgemv
#define atsctrb_cblas_zgemv cblas_zgemv

/* ****** ****** */

/*
** GBMV: S, D, C, Z
*/

#define atsctrb_cblas_sgbmv cblas_sgbmv
#define atsctrb_cblas_dgbmv cblas_dgbmv
#define atsctrb_cblas_cgbmv cblas_cgbmv
#define atsctrb_cblas_zgbmv cblas_zgbmv

/* ****** ****** */

/*
** TRMV: S, D, C, Z
*/

#define atsctrb_cblas_strmv cblas_strmv
#define atsctrb_cblas_dtrmv cblas_dtrmv
#define atsctrb_cblas_ctrmv cblas_ctrmv
#define atsctrb_cblas_ztrmv cblas_ztrmv

/* ****** ****** */

/*
** TBMV: S, D, C, Z
*/

#define atsctrb_cblas_stbmv cblas_stbmv
#define atsctrb_cblas_dtbmv cblas_dtbmv
#define atsctrb_cblas_ctbmv cblas_ctbmv
#define atsctrb_cblas_ztbmv cblas_ztbmv

/* ****** ****** */

/*
** TPMV: S, D, C, Z
*/

#define atsctrb_cblas_stpmv cblas_stpmv
#define atsctrb_cblas_dtpmv cblas_dtpmv
#define atsctrb_cblas_ctpmv cblas_ctpmv
#define atsctrb_cblas_ztpmv cblas_ztpmv

/* ****** ****** */

/*
** TRSV: S, D, C, Z
*/

#define atsctrb_cblas_strsv cblas_strsv
#define atsctrb_cblas_dtrsv cblas_dtrsv
#define atsctrb_cblas_ctrsv cblas_ctrsv
#define atsctrb_cblas_ztrsv cblas_ztrsv

/* ****** ****** */

/*
** TBSV: S, D, C, Z
*/

#define atsctrb_cblas_stbsv cblas_stbsv
#define atsctrb_cblas_dtbsv cblas_dtbsv
#define atsctrb_cblas_ctbsv cblas_ctbsv
#define atsctrb_cblas_ztbsv cblas_ztbsv

/* ****** ****** */

/*
** TPSV: S, D, C, Z
*/

#define atsctrb_cblas_stpsv cblas_stpsv
#define atsctrb_cblas_dtpsv cblas_dtpsv
#define atsctrb_cblas_ctpsv cblas_ctpsv
#define atsctrb_cblas_ztpsv cblas_ztpsv

/* ****** ****** */

/*
** SYMV: S, D
*/

#define atsctrb_cblas_ssymv cblas_ssymv
#define atsctrb_cblas_dsymv cblas_dsymv

/* ****** ****** */

/*
** SBMV: S, D
*/

#define atsctrb_cblas_ssbmv cblas_ssbmv
#define atsctrb_cblas_dsbmv cblas_dsbmv

/* ****** ****** */

/*
** SPMV: S, D
*/

#define atsctrb_cblas_sspmv cblas_sspmv
#define atsctrb_cblas_dspmv cblas_dspmv

/* ****** ****** */

/*
** GER: S, D
*/

#define atsctrb_cblas_sger cblas_sger
#define atsctrb_cblas_dger cblas_dger

/* ****** ****** */

/*
** SYR: S, D
*/

#define atsctrb_cblas_ssyr cblas_ssyr
#define atsctrb_cblas_dsyr cblas_dsyr

/* ****** ****** */

/*
** SPR: S, D
*/

#define atsctrb_cblas_sspr cblas_sspr
#define atsctrb_cblas_dspr cblas_dspr

/* ****** ****** */

/*
** SYR2: S, D
*/

#define atsctrb_cblas_ssyr2 cblas_ssyr2
#define atsctrb_cblas_dsyr2 cblas_dsyr2

/* ****** ****** */

/*
** SPR2: S, D
*/

#define atsctrb_cblas_sspr2 cblas_sspr2
#define atsctrb_cblas_dspr2 cblas_dspr2

/* ****** ****** */

/*
** HEMV: C, Z // extended with S, D
*/

#define atsctrb_cblas_chemv cblas_chemv
#define atsctrb_cblas_zhemv cblas_zhemv

/* ****** ****** */

/*
** HBMV; C, Z // extended with S, D
*/

#define atsctrb_cblas_chbmv cblas_chbmv
#define atsctrb_cblas_zhbmv cblas_zhbmv

/* ****** ****** */

/*
** HPMV; C, Z // extended with S, D
*/

#define atsctrb_cblas_chpmv cblas_chpmv
#define atsctrb_cblas_zhpmv cblas_zhpmv

/* ****** ****** */

/*
** GERU: C, Z // extended with S, D
*/

#define atsctrb_cblas_cgeru cblas_cgeru
#define atsctrb_cblas_zgeru cblas_zgeru

/* ****** ****** */

/*
** GERC: C, Z // extended with S, D
*/

#define atsctrb_cblas_cgerc cblas_cgerc
#define atsctrb_cblas_zgerc cblas_zgerc

/* ****** ****** */

/*
** HER: C, D // extended with S, D
*/

#define atsctrb_cblas_cher cblas_cher
#define atsctrb_cblas_zher cblas_zher

/* ****** ****** */

/*
** HPR: C, Z // extended with S, D
*/

#define atsctrb_cblas_chpr cblas_chpr
#define atsctrb_cblas_zhpr cblas_zhpr

/* ****** ****** */

/*
** HER2: C, Z // extended with S, D
*/

#define atsctrb_cblas_cher2 cblas_cher2
#define atsctrb_cblas_zher2 cblas_zher2

/* ****** ****** */

#define atsctrb_cblas_chpr2 cblas_chpr2
#define atsctrb_cblas_zhpr2 cblas_zhpr2

/* ****** ****** */

//
// BLAS level 3
//

/* ****** ****** */

#define atsctrb_cblas_sgemm cblas_sgemm
#define atsctrb_cblas_dgemm cblas_dgemm
#define atsctrb_cblas_cgemm cblas_cgemm
#define atsctrb_cblas_zgemm cblas_zgemm

/* ****** ****** */

#define atsctrb_cblas_ssyrk cblas_ssyrk
#define atsctrb_cblas_dsyrk cblas_dsyrk
#define atsctrb_cblas_csyrk cblas_csyrk
#define atsctrb_cblas_zsyrk cblas_zsyrk

/* ****** ****** */

#define atsctrb_cblas_ssyr2k cblas_ssyr2k
#define atsctrb_cblas_dsyr2k cblas_dsyr2k
#define atsctrb_cblas_csyr2k cblas_csyr2k
#define atsctrb_cblas_zsyr2k cblas_zsyr2k

/* ****** ****** */

#define atsctrb_cblas_ssymm cblas_ssymm
#define atsctrb_cblas_dsymm cblas_dsymm
#define atsctrb_cblas_csymm cblas_csymm
#define atsctrb_cblas_zsymm cblas_zsymm

/* ****** ****** */

#define atsctrb_cblas_strmm cblas_strmm
#define atsctrb_cblas_dtrmm cblas_dtrmm
#define atsctrb_cblas_ctrmm cblas_ctrmm
#define atsctrb_cblas_ztrmm cblas_ztrmm

/* ****** ****** */

#define atsctrb_cblas_strsm cblas_strsm
#define atsctrb_cblas_dtrsm cblas_dtrsm
#define atsctrb_cblas_ctrsm cblas_ctrsm
#define atsctrb_cblas_ztrsm cblas_ztrsm

/* ****** ****** */

#define atsctrb_cblas_chemm cblas_chemm
#define atsctrb_cblas_zhemm cblas_zhemm

/* ****** ****** */

#define atsctrb_cblas_cherk cblas_cherk
#define atsctrb_cblas_zherk cblas_zherk

/* ****** ****** */

#define atsctrb_cblas_cher2k cblas_cher2k
#define atsctrb_cblas_zher2k cblas_zher2k

/* ****** ****** */

#endif /* [ATS_CONTRIB_CBLAS_CATS] */

/* end of [cblas.cats] */
