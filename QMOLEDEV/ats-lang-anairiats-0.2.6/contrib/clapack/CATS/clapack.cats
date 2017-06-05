/*
**
** An interface for ATS to interact with LAPACK
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*/

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

/* ****** ****** */

#ifndef ATS_CONTRIB_CLAPACK_CATS
#define ATS_CONTRIB_CLAPACK_CATS

/* ****** ****** */

#include "f2c.cats"

/* ****** ****** */

/*
** lamch: S, D
*/

#if (0)
doublereal dlamch_(char *cmach) ;
#endif

extern
// F2Creal slamch_ (char *cmach) ;
F2Cdoublereal slamch_t (char *cmach) ; // CLAPACK says it!

ATSinline()
F2Creal atsctrb_clapack_slamch
  (ats_char_type cmach) { return slamch_(&cmach) ; }
// end of [atsctrb_clapack_slamch]

extern
F2Cdoublereal dlamch_ (char *cmach) ;

ATSinline()
F2Cdoublereal atsctrb_clapack_dlamch
  (ats_char_type cmach) { return dlamch_(&cmach) ; }
// end of [atsctrb_clapack_dlamch]

/* ****** ****** */

/*
** lange: S, D, C, Z
*/

#if (0)
/* Subroutine */
doublereal dlange_(
  char *norm
, integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *work
) ;
#endif

extern
F2Creal slange_(
  char *norm
, F2Cinteger *m
, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *work
) ;

ATSinline()
F2Creal atsctrb_clapack_slange (
  ats_char_type norm
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ptr_type work
) {
  F2Creal res ;
  res = slange_(&norm, &m, &n, a, &lda, work) ;
  return res ;
} /* end of [atsctrb_clapack_slange] */

//

extern
F2Cdoublereal dlange_(
  char *norm
, F2Cinteger *m
, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *work
) ;

ATSinline()
F2Cdoublereal atsctrb_clapack_dlange (
  ats_char_type norm
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ptr_type work
) {
  F2Cdoublereal res ;
  res = dlange_(&norm, &m, &n, a, &lda, work) ;
  return res ;
} /* end of [atsctrb_clapack_dlange] */

//

extern
F2Creal clange_(
  char *norm
, F2Cinteger *m
, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *work
) ;

ATSinline()
F2Creal atsctrb_clapack_clange (
  ats_char_type norm
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ptr_type work
) {
  F2Creal res ;
  res = clange_(&norm, &m, &n, a, &lda, work) ;
  return res ;
} /* end of [atsctrb_clapack_clange] */

//

extern
F2Cdoublereal zlange_(
  char *norm
, F2Cinteger *m
, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *work
) ;

ATSinline()
F2Cdoublereal atsctrb_clapack_zlange (
  ats_char_type norm
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ptr_type work
) {
  F2Cdoublereal res ;
  res = zlange_(&norm, &m, &n, a, &lda, work) ;
  return res ;
} /* end of [atsctrb_clapack_zlange] */

/* ****** ****** */

/*
** lacpy: S, D, C, Z
*/

extern int slacpy_(
  char *uplo
, F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *b, F2Cinteger *ldb
) ;

ATSinline()
ats_void_type
atsctrb_clapack_slacpy (
  ats_char_type uln
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  slacpy_(&uln, &m, &n, a, &lda, b, &ldb) ; return ;
} /* end of [atsctrb_clapack_slacpy] */

//

extern int dlacpy_(
  char *uplo
, F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *b, F2Cinteger *ldb
) ;

ATSinline()
ats_void_type
atsctrb_clapack_dlacpy (
  ats_char_type uln
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  dlacpy_(&uln, &m, &n, a, &lda, b, &ldb) ; return ;
} /* end of [atsctrb_clapack_dlacpy] */

//

extern int clacpy_(
  char *uplo
, F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *b, F2Cinteger *ldb
) ;

ATSinline()
ats_void_type
atsctrb_clapack_clacpy (
  ats_char_type uln
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  clacpy_(&uln, &m, &n, a, &lda, b, &ldb) ; return ;
} /* end of [atsctrb_clapack_clacpy] */

//

extern int zlacpy_(
  char *uplo
, F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *b, F2Cinteger *ldb
) ;

ATSinline()
ats_void_type
atsctrb_clapack_zlacpy (
  ats_char_type uln
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  zlacpy_(&uln, &m, &n, a, &lda, b, &ldb) ; return ;
} /* end of [atsctrb_clapack_zlacpy] */

/* ****** ****** */

/*
** gelqf: S, D, C, Z
*/

extern int sgelqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau, F2Creal *work
, F2Cinteger *lwork, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgelqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sgelqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgelqf] */

//

extern int dgelqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgelqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dgelqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgelqf] */

//

int cgelqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgelqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cgelqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgelqf] */

//

extern int zgelqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgelqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zgelqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgelqf] */

/* ****** ****** */

/*
** ormlq: S, D & unmlq: C, Z
*/

/* Subroutine */
extern int sormlq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau
, F2Creal *c, F2Cinteger *ldc
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sormlq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sormlq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_sormlq] */

//
extern int dormlq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *c, F2Cinteger *ldc
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dormlq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dormlq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_dormlq] */

//

/* Subroutine */
extern int cunmlq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *c__, F2Cinteger *ldc
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cunmlq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cunmlq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_cunmlq] */

//

extern int zunmlq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n,	F2Cinteger *k
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *c__, F2Cinteger *ldc
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zunmlq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zunmlq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_zunmlq] */

/* ****** ****** */

/*
** geqlf: S, D, C, Z
*/

extern int sgeqlf_(
  F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgeqlf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sgeqlf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgeqlf] */

//

extern int dgeqlf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgeqlf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dgeqlf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgeqlf] */

//

extern int cgeqlf_(
  F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgeqlf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cgeqlf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgeqlf] */

//

extern int zgeqlf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgeqlf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zgeqlf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgeqlf] */

/* ****** ****** */

/*
** ormql: S, D and unmql: C, Z
*/

/* Subroutine */
extern int sormql_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau
, F2Creal *c, F2Cinteger *ldc
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sormql (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sormql_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_sormql] */

//

extern int dormql_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *c, F2Cinteger *ldc
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dormql (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dormql_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_dormql] */

/* Subroutine */
extern int cunmql_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *c__, F2Cinteger *ldc
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cunmql (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cunmql_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_cunmql] */

//

extern int zunmql_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *c__, F2Cinteger *ldc
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zunmql (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zunmql_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_zunmql] */

/* ****** ****** */

/*
** geqrf: S, D, C, Z
*/

extern int sgeqrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgeqrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sgeqrf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgeqrf] */

//

extern int dgeqrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgeqrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dgeqrf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgeqrf] */

//

extern int cgeqrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgeqrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cgeqrf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgeqrf] */

//

extern int zgeqrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgeqrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zgeqrf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgeqrf] */

/* ****** ****** */

/*
** ormqr: S, D and unmqr: C, Z
*/

/* Subroutine */
extern int sormqr_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau
, F2Creal *c, F2Cinteger *ldc
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sormqr (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  char transp = trans;
  if (transp == 'C' || transp == 'c') transp = 'T';
  sormqr_(
    &side, &transp, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_sormqr] */

//

extern int dormqr_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *c, F2Cinteger *ldc
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dormqr (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  char transp = trans;
  if (transp == 'C' || transp == 'c') transp = 'T';
  dormqr_(
    &side, &transp, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_dormqr] */

//

/* Subroutine */
extern int cunmqr_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *c__, F2Cinteger *ldc
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cunmqr (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cunmqr_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_cunmqr] */

//

extern int zunmqr_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *c__, F2Cinteger *ldc
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zunmqr (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zunmqr_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_zunmqr] */

/* ****** ****** */

/*
** gerqf: S, D, C, Z
*/

extern int sgerqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau, F2Creal *work
, F2Cinteger *lwork, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgerqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sgerqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgerqf] */

//

extern int dgerqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgerqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dgerqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgerqf] */

//

int cgerqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgerqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cgerqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgerqf] */

//

extern int zgerqf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgerqf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zgerqf_(&m, &n, a, &lda, tau, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgerqf] */

/* ****** ****** */

/*
** unmrq: C, Z
*/

/* Subroutine */
extern int cunmrq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *tau
, F2Ccomplex *c__, F2Cinteger *ldc
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cunmrq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cunmrq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_cunmrq] */

//

extern int zunmrq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n,	F2Cinteger *k
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *tau
, F2Cdoublecomplex *c__, F2Cinteger *ldc
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zunmrq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c__, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zunmrq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c__, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_zunmrq] */

/* Subroutine */
extern int sormrq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Creal *a, F2Cinteger *lda
, F2Creal *tau
, F2Creal *c, F2Cinteger *ldc
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sormrq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sormrq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_sormrq] */

//

extern int dormrq_(
  char *side, char *trans
, F2Cinteger *m, F2Cinteger *n, F2Cinteger *k
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *tau
, F2Cdoublereal *c, F2Cinteger *ldc
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dormrq (
  ats_char_type side
, ats_char_type trans
, F2Cinteger m, F2Cinteger n, F2Cinteger k
, ats_ref_type a, F2Cinteger lda
, ats_ref_type tau
, ats_ref_type c, F2Cinteger ldc
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dormrq_(
    &side, &trans, &m, &n, &k, a, &lda, tau, c, &ldc, work, &lwork, &info
  ) ; return info ;
} /* end of [atsctrb_clapack_dormrq] */

/* ****** ****** */

// gels: S, D, C, Z

extern int sgels_(
  char *trans
, F2Cinteger *m, F2Cinteger *n
, F2Cinteger *nrhs
, F2Creal *a, F2Cinteger *lda
, F2Creal *b, F2Cinteger *ldb
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgels (
  ats_char_type trans
, F2Cinteger m, F2Cinteger n
, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sgels_(&trans, &m, &n, &nrhs, a, &lda, b, &ldb, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgels] */

//

/* Subroutine */
extern int dgels_(
  char *trans
, F2Cinteger *m, F2Cinteger *n
, F2Cinteger *nrhs
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *b, F2Cinteger *ldb
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgels (
  ats_char_type trans
, F2Cinteger m, F2Cinteger n
, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dgels_(&trans, &m, &n, &nrhs, a, &lda, b, &ldb, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgels] */

//

extern int cgels_(
  char *trans
, F2Cinteger *m, F2Cinteger *n
, F2Cinteger *nrhs
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *b, F2Cinteger *ldb
, F2Ccomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgels (
  ats_char_type trans
, F2Cinteger m, F2Cinteger n
, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  cgels_(&trans, &m, &n, &nrhs, a, &lda, b, &ldb, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgels] */

//

extern int zgels_(
  char *trans
, F2Cinteger *m, F2Cinteger *n
, F2Cinteger *nrhs
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *b, F2Cinteger *ldb
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgels (
  ats_char_type trans
, F2Cinteger m, F2Cinteger n
, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  zgels_(&trans, &m, &n, &nrhs, a, &lda, b, &ldb, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgels] */

/* ****** ****** */

// trtrs: S, D, C, Z

extern int strtrs_(
  char *uplo
, char *trans
, char *diag
, F2Cinteger *n
, F2Cinteger *nrhs
, F2Creal *a, F2Cinteger *lda
, F2Creal *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_strtrs (
  ats_char_type uplo
, ats_char_type trans
, ats_char_type diag
, F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  strtrs_(&uplo, &trans, &diag, &n, &nrhs, a, &lda, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_strtrs] */

//

extern int dtrtrs_(
  char *uplo
, char *trans
, char *diag
, F2Cinteger *n
, F2Cinteger *nrhs
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dtrtrs (
  ats_char_type uplo
, ats_char_type trans
, ats_char_type diag
, F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  dtrtrs_(&uplo, &trans, &diag, &n, &nrhs, a, &lda, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dtrtrs] */

//

extern int ctrtrs_(
  char *uplo
, char *trans
, char *diag
, F2Cinteger *n
, F2Cinteger *nrhs
, F2Ccomplex *a, F2Cinteger *lda
, F2Ccomplex *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_ctrtrs (
  ats_char_type uplo
, ats_char_type trans
, ats_char_type diag
, F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  ctrtrs_(&uplo, &trans, &diag, &n, &nrhs, a, &lda, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_ctrtrs] */

//

extern int ztrtrs_(
  char *uplo
, char *trans
, char *diag
, F2Cinteger *n
, F2Cinteger *nrhs
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublecomplex *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_ztrtrs (
  ats_char_type uplo
, ats_char_type trans
, ats_char_type diag
, F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  ztrtrs_(&uplo, &trans, &diag, &n, &nrhs, a, &lda, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_ztrtrs] */

/* ****** ****** */

// getrf: S, D, C, Z

extern int sgetrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgetrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
) {
  F2Cinteger info ;
  sgetrf_(&m, &n, a, &lda, ipiv, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgetrf] */

//

extern int dgetrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgetrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
) {
  F2Cinteger info ;
  dgetrf_(&m, &n, a, &lda, ipiv, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgetrf] */

//

extern int cgetrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgetrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
) {
  F2Cinteger info ;
  cgetrf_(&m, &n, a, &lda, ipiv, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgetrf] */

//

extern int zgetrf_(
  F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgetrf (
  F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
) {
  F2Cinteger info ;
  zgetrf_(&m, &n, a, &lda, ipiv, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgetrf] */

/* ****** ****** */

// gesv: S, D, C, Z

/* Subroutine */
extern int dgesv_ (
  F2Cinteger *n, F2Cinteger *nrhs
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Cdoublereal *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgesv (
  F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  dgesv_ (&n, &nrhs, a, &lda, ipiv, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgesv] */

/* Subroutine */
extern int sgesv_ (
  F2Cinteger *n, F2Cinteger *nrhs
, F2Creal *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Creal *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgesv (
  F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  sgesv_ (&n, &nrhs, a, &lda, ipiv, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgesv] */

/* Subroutine */
extern int cgesv_ (
  F2Cinteger *n, F2Cinteger *nrhs
, F2Ccomplex *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Ccomplex *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgesv (
  F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  cgesv_ (&n, &nrhs, a, &lda, ipiv, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgesv] */

/* Subroutine */
extern int zgesv_ (
  F2Cinteger *n, F2Cinteger *nrhs
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cinteger *ipiv
, F2Cdoublecomplex *b, F2Cinteger *ldb
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgesv (
  F2Cinteger n, F2Cinteger nrhs
, ats_ref_type a, F2Cinteger lda
, ats_ref_type ipiv
, ats_ref_type b, F2Cinteger ldb
) {
  F2Cinteger info ;
  zgesv_ (&n, &nrhs, a, &lda, ipiv, b, &ldb, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgesv] */

/* ****** ****** */

// gesvd: S, D, C, Z

/* Subroutine */
extern int sgesvd_(
  char *jobu, char *jobvt
, F2Cinteger *m, F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Creal *s
, F2Creal *u, F2Cinteger *ldu
, F2Creal *vt, F2Cinteger *ldvt
, F2Creal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_sgesvd (
  char jobu, char jobvt
, F2Cinteger m, F2Cinteger n
, ats_ptr_type a, F2Cinteger lda
, ats_ptr_type s
, ats_ptr_type u, F2Cinteger ldu
, ats_ptr_type vt, F2Cinteger ldvt
, ats_ptr_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  sgesvd_ (&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_sgesvd] */

/* Subroutine */
extern int dgesvd_(
  char *jobu, char *jobvt
, F2Cinteger *m, F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cdoublereal *s
, F2Cdoublereal *u, F2Cinteger *ldu
, F2Cdoublereal *vt, F2Cinteger *ldvt
, F2Cdoublereal *work, F2Cinteger *lwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_dgesvd (
  char jobu, char jobvt
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type s
, ats_ref_type u, F2Cinteger ldu
, ats_ref_type vt, F2Cinteger ldvt
, ats_ref_type work, F2Cinteger lwork
) {
  F2Cinteger info ;
  dgesvd_ (&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_dgesvd] */

/* Subroutine */
extern int cgesvd_(
  char *jobu, char *jobvt
, F2Cinteger *m, F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Creal *s
, F2Ccomplex *u, F2Cinteger *ldu
, F2Ccomplex *vt, F2Cinteger *ldvt
, F2Ccomplex *work, F2Cinteger *lwork
, F2Creal *rwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_cgesvd (
  char jobu, char jobvt
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type s
, ats_ref_type u, F2Cinteger ldu
, ats_ref_type vt, F2Cinteger ldvt
, ats_ref_type work, F2Cinteger lwork
, ats_ref_type rwork
) {
  F2Cinteger info ;
  cgesvd_ (&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, rwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_cgesvd] */

/* Subroutine */
extern int zgesvd_(
  char *jobu, char *jobvt
, F2Cinteger *m, F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cdoublereal *s
, F2Cdoublecomplex *u, F2Cinteger *ldu
, F2Cdoublecomplex *vt, F2Cinteger *ldvt
, F2Cdoublecomplex *work, F2Cinteger *lwork
, F2Cdoublereal *rwork
, F2Cinteger *info
) ;

ATSinline()
ats_int_type
atsctrb_clapack_zgesvd (
  char jobu, char jobvt
, F2Cinteger m, F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, ats_ref_type s
, ats_ref_type u, F2Cinteger ldu
, ats_ref_type vt, F2Cinteger ldvt
, ats_ref_type work, F2Cinteger lwork
, ats_ref_type rwork
) {
  F2Cinteger info ;
  zgesvd_ (&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, rwork, &info) ;
  return info ;
} /* end of [atsctrb_clapack_zgesvd] */

/* ****** ****** */

/* Subroutine */
extern int slaswp_ (
  F2Cinteger *n
, F2Creal *a, F2Cinteger *lda
, F2Cinteger *k1, F2Cinteger *k2
, F2Cinteger *ipiv
, F2Cinteger *incx
) ;

ATSinline()
ats_void_type
atsctrb_clapack_slaswp (
  F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, F2Cinteger k1, F2Cinteger k2
, ats_ref_type ipiv
, F2Cinteger incx
) {
  slaswp_ (&n, a, &lda, &k1, &k2, ipiv, &incx) ;
  return ;
}

/* Subroutine */
extern int dlaswp_ (
  F2Cinteger *n
, F2Cdoublereal *a, F2Cinteger *lda
, F2Cinteger *k1, F2Cinteger *k2
, F2Cinteger *ipiv
, F2Cinteger *incx
) ;

ATSinline()
ats_void_type
atsctrb_clapack_dlaswp (
  F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, F2Cinteger k1, F2Cinteger k2
, ats_ref_type ipiv
, F2Cinteger incx
) {
  dlaswp_ (&n, a, &lda, &k1, &k2, ipiv, &incx) ;
  return ;
}

/* Subroutine */
extern int claswp_ (
  F2Cinteger *n
, F2Ccomplex *a, F2Cinteger *lda
, F2Cinteger *k1, F2Cinteger *k2
, F2Cinteger *ipiv
, F2Cinteger *incx
) ;

ATSinline()
ats_void_type
atsctrb_clapack_claswp (
  F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, F2Cinteger k1, F2Cinteger k2
, ats_ref_type ipiv
, F2Cinteger incx
) {
  claswp_ (&n, a, &lda, &k1, &k2, ipiv, &incx) ;
  return ;
}

/* Subroutine */
extern int zlaswp_ (
  F2Cinteger *n
, F2Cdoublecomplex *a, F2Cinteger *lda
, F2Cinteger *k1, F2Cinteger *k2
, F2Cinteger *ipiv
, F2Cinteger *incx
) ;

ATSinline()
ats_void_type
atsctrb_clapack_zlaswp (
  F2Cinteger n
, ats_ref_type a, F2Cinteger lda
, F2Cinteger k1, F2Cinteger k2
, ats_ref_type ipiv
, F2Cinteger incx
) {
  zlaswp_ (&n, a, &lda, &k1, &k2, ipiv, &incx) ;
  return ;
}

/* ****** ****** */

#endif /* [ATS_CONTRIB_CLAPACK_CATS] */

/* end of [clapack.cats] */
