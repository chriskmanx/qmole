(*
**
** An interface for ATS to interact with LAPACK
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)

local #include "prelude/HATS/number.hats" in (*empty*) end
local #include "contrib/clapack/HATS/f2c.hats" in (*empty*) end
local #include "contrib/cblas/HATS/cblas.hats" in (*empty*) end
local #include "contrib/clapack/HATS/clapack.hats" in (*empty*) end

(* ****** ****** *)

staload NUM = "prelude/SATS/number.sats"
fn{a:t@ype} of_int (i:int):<> a = $NUM.of_int (i)

(* ****** ****** *)

staload F2C = "contrib/clapack/SATS/f2c.sats"
typedef integer = $F2C.integer
macdef lint_of_integer = $F2C.lint_of_integer
macdef integer_of_int1 = $F2C.integer_of_int1
macdef size1_of_integer = $F2C.size1_of_integer
fn{a:t@ype} print_typ (): void = $F2C.print_typ<a> ()
fn{a:t@ype} print_elt (x: a): void = $F2C.print_elt<a> (x)

(* ****** ****** *)

staload M = "libc/SATS/math.sats"
staload C = "libc/SATS/complex.sats"

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

typedef real = $F2C.real
typedef doublereal = $F2C.doublereal
typedef complex = $F2C.complex
typedef doublecomplex = $F2C.doublecomplex

(* ****** ****** *)

#define i2sz size1_of_int1
#define sz2i int1_of_size1

(* ****** ****** *)

fn srandgen_elt ():<!ref> real = let
  val x = drand48 () in $F2C.real_of_float (float_of_double x)
end // end of [srandgen_elt]

fn drandgen_elt ():<!ref> doublereal =
  $F2C.doublereal_of_double (drand48 ())

fn crandgen_elt ():<!ref> complex = let
  val x = drand48 () and y = drand48 ()
  val x = float_of_double x and y = float_of_double y
in
  $F2C.complex_of_ccmplx ($C.ccmplx_make_cart (x, y))
end // end of [crandgen_elt]

fn zrandgen_elt
  ():<!ref> doublecomplex = let
  val x = drand48 () and y = drand48 () in
  $F2C.doublecomplex_of_zcmplx ($C.zcmplx_make_cart (x, y))
end // end of [zrandgen_elt]

(* ****** ****** *)

extern fun{t:t@ype} randgen_elt ():<!ref> t
implement randgen_elt<real>          () = srandgen_elt ()
implement randgen_elt<doublereal>    () = drandgen_elt ()
implement randgen_elt<complex>       () = crandgen_elt ()
implement randgen_elt<doublecomplex> () = zrandgen_elt ()

(* ****** ****** *)

staload "libats/SATS/fmatrix.sats"
staload _(*anonymous*) = "libats/DATS/fmatrix.dats"

staload "libats/SATS/genarrays.sats"
staload _(*anonymous*) = "libats/DATS/genarrays.dats"

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"
staload "contrib/cblas/SATS/cblas_extra.sats"
staload _(*anonymous*) = "contrib/cblas/DATS/cblas_extra.dats"

(* ****** ****** *)

fun{t:t@ype}
randgen_arr {n:nat} .<>. (n: int n)
  :<!ref> [l:addr] (
    free_gc_v (t, n, l), array_v (t, n, l)
  | ptr l
  ) = let
  val tsz = sizeof<t>
  val n_sz = size1_of_int1 (n)
  val (pf_gc, pf_arr | p_arr) =
    array_ptr_alloc_tsz {t} (n_sz, tsz)
  // end of [val]
//
  val () = array_ptr_initialize_fun<t> (!p_arr, n_sz, f) where {
    val f = lam (
      _: sizeLt n, x: &(t?) >> t
    ) : void =<fun,!ref>
      x :=  randgen_elt<t> ()
    // end of [val]
  } // end of [val]
//
in
  (pf_gc, pf_arr | p_arr)
end // end of [rangen_arr]

(* ****** ****** *)

fun{t:t@ype}
randgen_fmat
  {m,n:nat} (m: int m, n: int n)
  : [mn:int] [l:addr] (
    free_gc_v (t, mn, l)
  , MUL (m, n, mn)
  , fmatrix_v (t, m, n, l)
  | ptr l
  ) = let
  val (pf_mn | mn) = m imul2 n
  prval () = mul_nat_nat_nat (pf_mn)
  val (pf_gc, pf_arr | p_arr) = randgen_arr<t> (mn)
  prval pf_fmat = fmatrix_v_of_array_v (pf_mn, pf_arr)
in
  (pf_gc, pf_mn, pf_fmat | p_arr)
end // end of [randgen_fmat]

fn{a:t@ype}
print_fmat {m,n:nat} (
    M: &fmatrix (a, m, n), m: size_t m, n: size_t n
  ) : void =
  fmatrix_ptr_iforeach_fun<a>
    (M, pr, ORDERrow, m, n) where {
    val pr = lam (
      i: size_t, j: size_t, x: &a
    ) : void =<fun> $effmask_all let
      val () = if j > 0 then print ", "
      val () = if (i > 0 andalso j = 0) then print "\n" 
      val () = print_elt<a> (x)
    in
      // nothing
    end // end of [let]
  } // end of [fmatrix_ptr_iforeach_fun]
// end of [print_fmat]

(* ****** ****** *)

fun{t:t@ype}
randgen_gemat
  {m,n:nat} (m: int m, n: int n)
  : [mn:int] [l:addr] (
    free_gc_v (t, mn, l)
  , MUL (m, n, mn)
  , GEMAT_v (t, m, n, col, m, l)
  | ptr l
  ) = let
  val (pf_gc, pf_mn, pf_fmat | p_arr) = randgen_fmat<t> (m, n)
  prval (pf_gmat, _) = GEMAT_v_of_fmatrix_v (pf_fmat)
in
  (pf_gc, pf_mn, pf_gmat | p_arr)
end // end of [randgen_gemat]

fn{a:t@ype}
print_gemat
  {ord:order} {m,n:nat} {lda:pos} (
  ord: ORDER ord, 
  M: &GEMAT (a, m, n, ord, lda)
, m: size_t m, n: size_t n, lda: size_t lda
) : void = let
  val () = GEMAT_ptr_iforeach_fun<a>
    (ord, M, pr, ORDERrow, m, n, lda) where {
    val pr = lam (
      i: size_t, j: size_t, x: &a
    ) : void =<fun> $effmask_all let
      val () = if j > 0 then print ", "
      val () = if (i > 0 andalso j = 0) then print "\n" 
      val () = print_elt<a> (x)
    in
      // empty
    end // end of [val]
  } // end of [val]
in
  // nothing
end // end of [print_gemat]

(* ****** ****** *)
//
// SC: Fill matrix with random FP numbers
//
fn{t:t@ype}
GEMAT_rand
  {ord:order} {m,n:nat} {lda:pos} (
  ord: ORDER ord, m: size_t m, n: size_t n
, A: &GEMAT(t?, m, n, ord, lda) >> GEMAT (t, m, n, ord, lda)
, lda: size_t lda
) : void = let
//
  val f = lam (
    i: sizeLt m, j: sizeLt n, x: &t? >> t
  ) : void =<>
    x := $effmask_ref (randgen_elt<t> ())
  // end of [val]
//
  val () = GEMAT_ptr_initialize_fun<t> (ord, A, m, n, lda, f)
//
in
  // nothing
end // end of [GEMAT_rand]

(* ****** ****** *)

extern
fun{a:t@ype} trcpy {ord:order}
  {ul:uplo} {dg:diag} {m:nat} {lda,ldb:inc} (
    ord: ORDER ord
  , ul: UPLO ul, dg: DIAG dg, m: size_t m
  , A: &TRMAT (a, m, ord, ul, dg, lda), lda: size_t lda
  , B: &GEMAT (a, m, m, ord, ldb), ldb: size_t ldb
  ) :<> void
// end of [trcpy]

implement{a} trcpy
  {ord} {ul} {dg} {m} {lda,ldb}
  (ord, ul, dg, m, A, lda, B, ldb) = let
  prval (pfb_trm, fpfb) = TRMAT_v_of_GEMAT_v (view@ B, ul, dg)
  val () = TRMAT_ptr_copy<a> (ord, ul, dg, A, B, m, lda, ldb)
  prval () = view@ B := fpfb (pfb_trm)
  fun loop {i:nat | i <= m} .<m-i>.
    (B: &GEMAT (a, m, m, ord, ldb), i: size_t i, _1: a):<cloref> void =
    if i < m then begin
      GEMAT_ptr_set_elt_at<a> (ord, B, ldb, i, i, _1); loop (B, i+1, _1)
    end // end of [if]
  // end of [loop]
  val () = case+ dg of
    | DIAGunit () => loop (B, 0, of_int<a> 1) | DIAGnonunit () => ()
  // end of [val]
in
  // nothing
end // end of [trcpy]

(* ****** ****** *)

staload "contrib/clapack/SATS/clapack.sats"
staload _(*anonymous*) = "contrib/clapack/DATS/clapack.dats"

(* ****** ****** *)

//
// Part I
//

(* ****** ****** *)

extern fun{t,tr:t@ype} lange_lacpy_test (): void

implement{t,tr}
lange_lacpy_test () = () where {
  #define M 20
  #define M2 %(M/2)
  #define N 13
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<t> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {t} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf11_gem, pf12_gem, fpf1_gem | p11, p12) =
    GEMAT_ptr_split2x1 (pf1_gem | ORDERcol, p1_arr, M, M2)
  (* end of [val] *)
//
  val m = integer_of_int1 (M)
  val n = integer_of_int1 (N)
  val m2 = integer_of_int1 (M2)
  var !p_work with pf_work = @[tr?][M2]()
//
  val NMinf11 = lange_inf<t,tr> (m2, n, !p11, m, !p_work)
  // val () = (print "NMinf11 = "; print_elt NMinf11; print_newline ())
  val NMfro11 = lange_frob<t,tr> (m2, n, !p11, m)
  // val () = (print "NMfro11 = "; print_elt NMfro11; print_newline ())
//
  val () = lacpy<t> {M2,N} (ClapackULN_U, m2, n, !p11, m, !p12, m)
  val () = lacpy<t> {M2,N} (ClapackULN_L, m2, n, !p11, m, !p12, m)
//
  val NMinf12 = lange_inf<t,tr> (m2, n, !p12, m, !p_work)
  // val () = (print "NMinf12 = "; print_elt NMinf12; print_newline ())
  val NMfro12 = lange_frob<t,tr> (m2, n, !p12, m)
  // val () = (print "NMfro12 = "; print_elt NMfro12; print_newline ())
//
  val sfmin = lamch<tr> ('S')
  val () = assert_errmsg_bool
    ((NMinf11 \$F2C.sub NMinf12) \$F2C.lte sfmin, #LOCATION)
  val () = assert_errmsg_bool
    ((NMfro11 \$F2C.sub NMfro12) \$F2C.lte sfmin, #LOCATION)
//
  prval () = pf1_gem := fpf1_gem (pf11_gem, pf12_gem)
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
//
(*
  val () = print "M =\n"
  val () = print_fmat<t> (!p1_arr, M, N)
  val () = print_newline ()
*)
//
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
} // end of [lange_lacpy_test]

(* ****** ****** *)

fn{a:t@ype}
  macheps_get ():<> a = let
  val _1 = $NUM.of_double<a> 1.0
  val _2 = $NUM.of_double<a> 2.0
  fun guess (x: a):<cloref,!ntm> a =
    if $F2C.eq<a> (_1, x \$F2C.add _1) then x else guess (x \$F2C.div _2)
  (* end of [guess] *)
in
  $effmask_ntm (guess _1)
end // end of [machine_eps]

val macheps_float = macheps_get<float> ()
val macheps_double = macheps_get<double> ()

(* ****** ****** *)

// gels & gels_work_query test

extern fun{t,tr:t@ype} gels_test (): void

implement{t,tr}
gels_test () = () where {
  #define M 30
  #define N 10
  #define NRHS 2
  val () = assert_prerrf_bool1 (
    M >= N, "%s: test works only for LS problems\n", @(#LOCATION)
  ) // end of [val]
// Make A
  val (pf_MN | MN) = M imul2 N
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf_gc_A, pf_arr_A | p_A) = randgen_arr<t> (MN)
  prval pf_fmat_A = fmatrix_v_of_array_v (pf_MN, pf_arr_A)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make X
  val (pf_NNRHS | NNRHS) = N imul2 NRHS
  prval () = mul_nat_nat_nat (pf_NNRHS)
  val (pf_gc_X, pf_arr_X | p_X) = randgen_arr<t> (NNRHS)
  prval pf_fmat_X = fmatrix_v_of_array_v (pf_NNRHS, pf_arr_X)
  prval (pf_gmat_X, fpf_fmat_X) = GEMAT_v_of_fmatrix_v (pf_fmat_X)
// Make B
  val (pf_MNRHS | MNRHS) = M imul2 NRHS
  prval () = mul_nat_nat_nat (pf_MNRHS)
  val MNRHS_sz = size1_of_int1 MNRHS
  val (pf_gc_B, pf_arr_B | p_B) = array_ptr_alloc<t> (MNRHS_sz)
  val () = array_ptr_initialize_elt<t> (!p_B, MNRHS_sz, of_int 1)
  prval pf_fmat_B = fmatrix_v_of_array_v (pf_MNRHS, pf_arr_B)
  prval (pf_gmat_B, fpf_fmat_B) = GEMAT_v_of_fmatrix_v (pf_fmat_B)
// B <- A * X
  val () = cblas_gemm<t> {col} {TPN, TPN} (
      TRANDIM_N (), TRANDIM_N () // match dimensions after transposition
    | CblasColMajor
    , CblasNoTrans // For A
    , CblasNoTrans // For X
    , M, NRHS, N
    , of_int 1
    , !p_A, M
    , !p_X, N
    , of_int 0
    , !p_B, M
  ) // [cblas_gemm]
  val M_ = integer_of_int1 (M)
  val N_ = integer_of_int1 (N)
  val NRHS_ = integer_of_int1 NRHS
// Setup gels
  val (pf_lwork | lwork) = gels_work_query<t> (ClapackNoTrans, M_, N_, NRHS_)
(*
  val lwork1 = lint_of_integer lwork
  val lwork1 = ulint_of_lint (lwork1)
  val () = begin
    printf ("lwork = %lu\n", @(lwork1)); printf ("lwork = 0x%lx\n", @(lwork1));
  end // end of [val]
*)
  val (pf_gc_work, pf_arr_work | p_work) =
    array_ptr_alloc<t> (size1_of_integer lwork)
// Solve for X (inside B after gels)
  val info = gels<t> (
      pf_lwork
    | ClapackNoTrans
    , M_, N_, NRHS_, !p_A, M_, !p_B, M_
    , !p_work, lwork
    )
  val () = assert_prerrf_bool
    (info = 0, "%s: gels failed: info = %i\n", @(#LOCATION, info))
// Free work
  val () = array_ptr_free (pf_gc_work, pf_arr_work | p_work)
// Split B = [B1; B2] = [X; res]
  val (pf_gmat_B1, pf_gmat_B2, fpf_gmat_B | p_B1, p_B2) =
    GEMAT_ptr_split2x1<t> (pf_gmat_B | ORDERcol, p_B, M, N)
// X <- X - B
  val () = GEMAT_axpy (ORDERcol, N, NRHS, of_int (~1), !p_B1, M, !p_X, N)
// norm (X - B)
  val err = lange_one<t,tr> (N_, NRHS_, !p_X, N_)
// check
  val mach_eps = lamch_eps<tr> ()
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (mach_eps, of_int<tr> (M * (N + NRHS))))
  val () = assert_prerrf_bool (isbig, "%s: gels LS solve failed", @(#LOCATION))
// Unsplit B
  prval pf_gmat_B = fpf_gmat_B (pf_gmat_B1, pf_gmat_B2)
// Free B
  prval pf_fmat_B = fpf_fmat_B (pf_gmat_B)
  prval (pf2_MNRHS, pf_arr_B) = array_v_of_fmatrix_v (pf_fmat_B)
  prval () = mul_isfun (pf_MNRHS, pf2_MNRHS)
  val () = array_ptr_free (pf_gc_B, pf_arr_B | p_B)
// Free X
  prval pf_fmat_X = fpf_fmat_X (pf_gmat_X)
  prval (pf2_NNRHS, pf_arr_X) = array_v_of_fmatrix_v (pf_fmat_X)
  prval () = mul_isfun (pf_NNRHS, pf2_NNRHS)
  val () = array_ptr_free (pf_gc_X, pf_arr_X | p_X)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  prval (pf2_MN, pf_arr_A) = array_v_of_fmatrix_v (pf_fmat_A)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf_gc_A, pf_arr_A | p_A)
} // end of [gels_test]

//
// Separate test for fat systems
//

extern fun{t,tr:t@ype} gels_test_minnorm (): void

implement{t,tr}
gels_test_minnorm () = () where {
  #define M 10
  #define N 30
  #define NRHS 2
  val () = assert_prerrf_bool1 (
    M <= N, "%s: test works only for MN problems\n", @(#LOCATION)
  ) // end of [val]
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Z
  val (pf_gc_Z, pf_MNRHS, pf_fmat_Z | p_Z) = randgen_fmat<t> (M, NRHS)
  prval () = mul_nat_nat_nat (pf_MNRHS)
  prval (pf_gmat_Z, fpf_fmat_Z) = GEMAT_v_of_fmatrix_v (pf_fmat_Z)
// Make X = A^T * Z
  val (pf_gc_X, pf_NNRHS, pf_fmat_X | p_X) = fmatrix_ptr_alloc<t> (N, NRHS)
  val () = fmatrix_ptr_initialize_elt<t> (!p_X, N, NRHS, of_int<t> 0)
    // cannot be skipped: 0 * NAN = NAN; 0 * INF = NAN
  prval (pf_gmat_X, fpf_fmat_X) = GEMAT_v_of_fmatrix_v (pf_fmat_X)
  val () = cblas_gemm<t> (
        TRANDIM_C ()
      , TRANDIM_N ()
      | CblasColMajor
      , CblasConjTrans
      , CblasNoTrans
      , N, NRHS, M
      , of_int<t> 1
      , !p_A, M
      , !p_Z, M
      , of_int<t> 0
      , !p_X, N
      )
// Free Z
  prval pf_fmat_Z = fpf_fmat_Z (pf_gmat_Z)
  val () = fmatrix_ptr_free (pf_gc_Z, pf_MNRHS, pf_fmat_Z | p_Z)
// Make B = [B1; B2] (larger than needed to hold X; B1 = B)
  val (pf_gc_B, pf2_NNRHS, pf_fmat_B | p_B) = fmatrix_ptr_alloc<t> (N, NRHS)
  val () = fmatrix_ptr_initialize_elt<t> (!p_B, N, NRHS, of_int<t> 0)
  prval (pf_gmat_B, fpf_fmat_B) = GEMAT_v_of_fmatrix_v (pf_fmat_B)
  val (pf_gmat_B1, pf_gmat_B2, fpf_gmat_B | p_B1, p_B2) =
    GEMAT_ptr_split2x1<t> (pf_gmat_B | ORDERcol, p_B, N, M)
// B1 <- A * X
  val () = cblas_gemm<t> {col} {TPN, TPN} (
      TRANDIM_N (), TRANDIM_N () // match dimensions after transposition
    | CblasColMajor
    , CblasNoTrans // For A
    , CblasNoTrans // For X
    , M, NRHS, N
    , of_int 1
    , !p_A, M
    , !p_X, N
    , of_int 0
    , !p_B1, N
  ) // [cblas_gemm]
// Unsplit B to pass to gels
  prval pf_gmat_B = fpf_gmat_B (pf_gmat_B1, pf_gmat_B2)
// Setup gels
  val M_ = integer_of_int1 (M)
  val N_ = integer_of_int1 (N)
  val NRHS_ = integer_of_int1 NRHS
  val (pf_lwork | lwork) = gels_work_query<t> (ClapackNoTrans, M_, N_, NRHS_)
(*
  val lwork1 = lint_of_integer lwork
  val lwork1 = ulint_of_lint (lwork1)
  val () = begin
    printf ("lwork = %lu\n", @(lwork1)); printf ("lwork = 0x%lx\n", @(lwork1));
  end // end of [val]
*)
  val (pf_gc_work, pf_arr_work | p_work) =
    array_ptr_alloc<t> (size1_of_integer lwork)
// Solve for X (inside B after gels)
  val info = gels<t> (
    pf_lwork
  | ClapackNoTrans
  , M_, N_, NRHS_, !p_A, M_, !p_B, N_
  , !p_work, lwork
  ) // end of [val]
  val () = assert_prerrf_bool
    (info = 0, "%s: gels failed: info = %i\n", @(#LOCATION, info))
// Free work
  val () = array_ptr_free (pf_gc_work, pf_arr_work | p_work)
// X <- X - B
  val () = GEMAT_axpy (ORDERcol, N, NRHS, of_int (~1), !p_B, N, !p_X, N)
// norm (X - B)
  val err = lange_one<t,tr> (N_, NRHS_, !p_X, N_)
// check
  val mach_eps = macheps_get<tr> () // lamch_eps<tr> ()
(*
  val () = begin
    print "mach_eps = ";
    print_elt<tr> mach_eps;
    print " gels_MN err = ";
    print_elt<tr> ($F2C.div<tr> (err, mach_eps));
    print_newline ()
  end // end of [val]
*)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (mach_eps, of_int<tr> (20 * M * (N + NRHS))))
  val () = assert_prerrf_bool (isbig, "%s: gels MN solve failed", @(#LOCATION))
// Free B
  prval pf_fmat_B = fpf_fmat_B (pf_gmat_B)
  val () = fmatrix_ptr_free (pf_gc_B, pf2_NNRHS, pf_fmat_B | p_B)
// Free X
  prval pf_fmat_X = fpf_fmat_X (pf_gmat_X)
  val () = fmatrix_ptr_free (pf_gc_X, pf_NNRHS, pf_fmat_X | p_X)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gels_test_minnorm]

(* ****** ****** *)

// gesv: S, D, C, Z

extern fun{t,tr:t@ype} gesv_test (): void

implement{t,tr}
gesv_test () = () where {
  #define M 300
  #define NRHS 2
(*
  val () = begin
    print "gesv_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
*)
// Make A
  val (pf_MM | MM) = M imul2 M
  prval () = mul_nat_nat_nat (pf_MM)
  val (pf_gc_A, pf_arr_A | p_A) = randgen_arr<t> (MM)
  prval pf_fmat_A = fmatrix_v_of_array_v (pf_MM, pf_arr_A)
  // Add M to diagonal to improve condition number
//
  val () =
    fmatrix_ptr_iforeach_fun<t>
      (!p_A, f, ORDERcol, M, M) where {
    val f = lam (
      i: size_t, j: size_t, x: &t
    ) : void =<fun>
      if i = j then x := $F2C.add<t> (x, of_int<t> M)
    } // end of [val]
//
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make X
  val (pf_MNRHS | MNRHS) = M imul2 NRHS
  prval () = mul_nat_nat_nat (pf_MNRHS)
  val (pf_gc_X, pf_arr_X | p_X) = randgen_arr<t> (MNRHS)
  prval pf_fmat_X = fmatrix_v_of_array_v (pf_MNRHS, pf_arr_X)
  prval (pf_gmat_X, fpf_fmat_X) = GEMAT_v_of_fmatrix_v (pf_fmat_X)
// Make B
  val MNRHS_sz = size1_of_int1 MNRHS
  val (pf_gc_B, pf_arr_B | p_B) = array_ptr_alloc<t> (MNRHS_sz)
  val () = array_ptr_initialize_elt<t> (!p_B, MNRHS_sz, of_int 1)
  prval pf_fmat_B = fmatrix_v_of_array_v (pf_MNRHS, pf_arr_B)
  prval (pf_gmat_B, fpf_fmat_B) = GEMAT_v_of_fmatrix_v (pf_fmat_B)
// B <- A * X
  val () = cblas_gemm<t> {col} {TPN, TPN} (
      TRANDIM_N (), TRANDIM_N () // match dimensions after transposition
    | CblasColMajor
    , CblasNoTrans // For A
    , CblasNoTrans // For X
    , M, NRHS, M
    , of_int 1
    , !p_A, M
    , !p_X, M
    , of_int 0
    , !p_B, M
  ) // [cblas_gemm]
// Set-up for gesv
  val (pf_gc_ipiv, pf_arr_ipiv | p_ipiv) =
    array_ptr_alloc<integer> (size1_of_int1 M)
// Solve for X (inside B after gesv)
  val M_ = integer_of_int1 (M)
  val NRHS_ = integer_of_int1 NRHS
  val (pferr | info) = gesv<t> (pf_gmat_A  | M_, NRHS_, p_A, M_, !p_ipiv, !p_B, M_)
  val () = assert_prerrf_bool
    (info = 0, "%s: gesv failed: info = %i", @(#LOCATION, info))
  val () = pf_gmat_A := LUMAT_err_v_elim (pferr)
// Free ipiv
  val () = array_ptr_free (pf_gc_ipiv, pf_arr_ipiv | p_ipiv)
// X <- X - B
  val () = GEMAT_axpy (ORDERcol, M, NRHS, of_int (~1), !p_B, M, !p_X, M)
// norm (X - B)
  val err = lange_one<t,tr> (M_, NRHS_, !p_X, M_)
  // val () = (print ("error = "); print_elt<tr> (err); print_newline ())
// check
  val mach_eps = macheps_get<tr> () // lamch_eps<tr> ()
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (mach_eps, of_int<tr> (2*NRHS*M*M)))
  val () = assert_prerrf_bool (isbig, "%s: [gesv] solve failed", @(#LOCATION))
// Free B
  prval pf_fmat_B = fpf_fmat_B (pf_gmat_B)
  prval (pf2_MNRHS, pf_arr_B) = array_v_of_fmatrix_v (pf_fmat_B)
  prval () = mul_isfun (pf_MNRHS, pf2_MNRHS)
  val () = array_ptr_free (pf_gc_B, pf_arr_B | p_B)
// Free X
  prval pf_fmat_X = fpf_fmat_X (pf_gmat_X)
  prval (pf2_NNRHS, pf_arr_X) = array_v_of_fmatrix_v (pf_fmat_X)
  prval () = mul_isfun (pf_MNRHS, pf2_NNRHS)
  val () = array_ptr_free (pf_gc_X, pf_arr_X | p_X)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  prval (pf2_MM, pf_arr_A) = array_v_of_fmatrix_v (pf_fmat_A)
  prval () = mul_isfun (pf_MM, pf2_MM)
  val () = array_ptr_free (pf_gc_A, pf_arr_A | p_A)
} // end of [gesv_test]

(* ****** ****** *)

// gesvd tests: S, D, C, Z with many variants

extern fun{t,tr:t@ype} gesvd_test (): void

implement{t,tr}
gesvd_test () = () where {
  #define M 55
  #define N 30
// (*
  val () = begin
    print "gesvd_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate U
  val (pf_gc_U, pf_MM, pf_fmat_U | p_U) = fmatrix_ptr_alloc<t> (M, M)
  prval () = mul_nat_nat_nat (pf_MM)
  prval (pf_gmat_U, fpf_fmat_U) = GEMAT_v_of_fmatrix_v (pf_fmat_U)
// Allocate S
  stavar mn : int
  val MN = min_size1_size1 (M, N) : size_t mn
  var !p_S = @[tr][MN]()
// Allocate VT
  val (pf_gc_VT, pf_NN, pf_fmat_VT | p_VT) = fmatrix_ptr_alloc<t> (N, N)
  prval () = mul_nat_nat_nat (pf_NN)
  prval (pf_gmat_VT, fpf_fmat_VT) = GEMAT_v_of_fmatrix_v (pf_fmat_VT)
// Set-up workspace for SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Full SVD of A
  val info = gesvd<t,tr> (
    pf_lwork
  | M_, N_, !p_A, M_, !p_S, !p_U, M_, !p_VT, N_, !p_work, lwork
  ) // end of [val]
  val () = (print "info = "; print info; print_newline ())
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd failed: info = %i\n", @(#LOCATION, info))
// A <- U * S * VT
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_A, M, N, M, of_int<t> 0)
  val () =
    if M <= N then let
      // U <- U * diag (S)
      val () =
        GEMAT_scal_col<t,tr> {col} {mn,mn} (ORDERcol, M, M, !p_U, M, !p_S)
      val (pf_gmat_VT1, pf_gmat_VT2, fpf_gmat_VT | p_VT1, p_VT2) =
        GEMAT_ptr_split1x2<t> (pf_gmat_VT | ORDERcol, p_VT, N, M(*min(M,N)*))
      val () = cblas_gemm<t> (
          TRANDIM_N {M,M} (), TRANDIM_N ()
        | CblasColMajor
        , CblasNoTrans
        , CblasNoTrans
        , M, N, min(M,N)
        , of_int<t> 1
        , !p_U, M
        , !p_VT1, N
        , of_int<t> 0
        , !p_A, M
      ) // end of [val]
      prval () = pf_gmat_VT := fpf_gmat_VT (pf_gmat_VT1, pf_gmat_VT2)
    in
      // nothing
    end else let
      // VT <- diag(S) * VT
      val () =
        GEMAT_scal_row<t,tr> {col} {mn,mn} (ORDERcol, N, N, !p_S, !p_VT, N)
      val (pf_gmat_U1, pf_gmat_U2, fpf_gmat_U | p_U1, p_U2) =
        GEMAT_ptr_split1x2<t> (pf_gmat_U | ORDERcol, p_U, M, N(*min(M,N)*))
      val () = cblas_gemm<t> (
          TRANDIM_N {M,mn} (), TRANDIM_N ()
        | CblasColMajor
        , CblasNoTrans
        , CblasNoTrans
        , M, N, min(M,N)
        , of_int<t> 1
        , !p_U1, M
        , !p_VT, N
        , of_int<t> 0
        , !p_A, M
      ) // end of [val]
      prval () = pf_gmat_U := fpf_gmat_U (pf_gmat_U1, pf_gmat_U2)
    in
      // nothing
    end (* end of [if] *)
// Check ||Ac - A||
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int<t> (~1), !p_A, M, !p_Ac, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac, M_)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M*N*min(M,N)), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_test failed\n", @(#LOCATION))
// Free VT
  prval pf_fmat_VT = fpf_fmat_VT (pf_gmat_VT)
  val () = fmatrix_ptr_free (pf_gc_VT, pf_NN, pf_fmat_VT | p_VT)
// Free U
  prval pf_fmat_U = fpf_fmat_U (pf_gmat_U)
  val () = fmatrix_ptr_free (pf_gc_U, pf_MM, pf_fmat_U | p_U)
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_test]

(* ****** ****** *)

// gesvd_econ test: S, D, C, Z

extern fun{t,tr:t@ype} gesvd_econ_test (): void

implement{t,tr}
gesvd_econ_test () = () where {
  #define M 55
  #define N 30
// (*
  val () = begin
    print "gesvd_econ_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
  val MN = min (M, N)
  val MN_sz = i2sz MN
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate U
  val (pf_gc_U, pf_MMN, pf_fmat_U | p_U) = fmatrix_ptr_alloc<t> (M, MN_sz)
  prval () = mul_nat_nat_nat (pf_MMN)
  prval (pf_gmat_U, fpf_fmat_U) = GEMAT_v_of_fmatrix_v (pf_fmat_U)
// Allocate S
  var !p_S = @[tr][MN]()
// Allocate VT
  val (pf_gc_VT, pf_MNN, pf_fmat_VT | p_VT) = fmatrix_ptr_alloc<t> (MN_sz, N)
  prval () = mul_nat_nat_nat (pf_MNN)
  prval (pf_gmat_VT, fpf_fmat_VT) = GEMAT_v_of_fmatrix_v (pf_fmat_VT)
// Set-up workspace for SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_econ_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Economy SVD of A
  val info = gesvd_econ<t,tr> (
    pf_lwork
  | M_, N_, !p_A, M_, !p_S, !p_U, M_, !p_VT, N_, !p_work, lwork
  ) // end of [val]
  val () = (print "info = "; print info; print_newline ())
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_econ failed: info = %i\n", @(#LOCATION, info))
// Prepare A
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_A, M, N, M, of_int<t> 0)
// U <- U * diag (S)
  val () = GEMAT_scal_col<t,tr> (ORDERcol, M, MN_sz, !p_U, M, !p_S)
// A <- (U * S) * VT
  val () = cblas_gemm<t> (
     TRANDIM_N (), TRANDIM_N ()
   | CblasColMajor
   , CblasNoTrans
   , CblasNoTrans
   , M, N, MN
   , of_int<t> 1
   , !p_U, M
   , !p_VT, N
   , of_int<t> 0
   , !p_A, M
   ) // end of [val]
// Check ||Ac - A||
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int<t> (~1), !p_A, M, !p_Ac, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac, M_)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M*N*min(M,N)), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_econ_test failed\n", @(#LOCATION))
// Free VT
  prval pf_fmat_VT = fpf_fmat_VT (pf_gmat_VT)
  val () = fmatrix_ptr_free (pf_gc_VT, pf_MNN, pf_fmat_VT | p_VT)
// Free U
  prval pf_fmat_U = fpf_fmat_U (pf_gmat_U)
  val () = fmatrix_ptr_free (pf_gc_U, pf_MMN, pf_fmat_U | p_U)
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_econ_test]

(* ****** ****** *)

// gesvd_sing test: S, D, C, Z

extern fun{t,tr:t@ype} gesvd_sing_test (): void

implement{t,tr}
gesvd_sing_test () = () where {
  #define M 55
  #define N 30
// (*
  val () = begin
    print "gesvd_sing_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
  val MN = min (M, N)
  val MN_sz = i2sz (MN)
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate U
  val (pf_gc_U, pf_MMN, pf_fmat_U | p_U) = fmatrix_ptr_alloc<t> (M, MN_sz)
  prval () = mul_nat_nat_nat (pf_MMN)
  prval (pf_gmat_U, fpf_fmat_U) = GEMAT_v_of_fmatrix_v (pf_fmat_U)
// Allocate S and Sc
  var !p_S with pfS_arr = @[tr][MN_sz]()
  var !p_Sc with pfSc_arr = @[tr][MN_sz]()
// Allocate VT
  val (pf_gc_VT, pf_MNN, pf_fmat_VT | p_VT) = fmatrix_ptr_alloc<t> (MN_sz, N)
  prval () = mul_nat_nat_nat (pf_MNN)
  prval (pf_gmat_VT, fpf_fmat_VT) = GEMAT_v_of_fmatrix_v (pf_fmat_VT)
// Set-up workspace for singular values SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_sing_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Singular values of A
  val info = gesvd_sing<t,tr> (
    pf_lwork
  | M_, N_, !p_A, M_, !p_S, !p_work, lwork
  ) // end of [val]
  val () = begin
    print "info = "; print info; print_newline ();
  end // end of [va]
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_sing failed: info = %i\n", @(#LOCATION, info))
// Set-up workspace for economy SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_econ_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Economy SVD of Ac
  val info = gesvd_econ<t,tr> (
    pf_lwork
  | M_, N_, !p_Ac, M_, !p_Sc, !p_U, M_, !p_VT, N_, !p_work, lwork
  ) // end of [val]
  val () = (print "info = "; print info; print_newline ())
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_econ failed: info = %i\n", @(#LOCATION, info))
// Check ||Sc - S||
  prval pfS_gvec = GEVEC_v_of_array_v (pfS_arr)
  prval pfSc_gvec = GEVEC_v_of_array_v (pfSc_arr)
  val () = cblas_axpy<tr> (MN, of_int<tr> (~1), !p_S, 1, !p_Sc, 1)
  val err = cblas_asum<tr,tr> (MN, !p_Sc, 1)
  prval () = pfS_arr := array_v_of_GEVEC_v (pfS_gvec)
  prval () = pfSc_arr := array_v_of_GEVEC_v (pfSc_gvec)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N * min(M,N)), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_sing_test failed\n", @(#LOCATION))
// Free VT
  prval pf_fmat_VT = fpf_fmat_VT (pf_gmat_VT)
  val () = fmatrix_ptr_free (pf_gc_VT, pf_MNN, pf_fmat_VT | p_VT)
// Free U
  prval pf_fmat_U = fpf_fmat_U (pf_gmat_U)
  val () = fmatrix_ptr_free (pf_gc_U, pf_MMN, pf_fmat_U | p_U)
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_sing_test]

(* ****** ****** *)

// gesvd_left test: S, D, C, Z

extern fun{t,tr:t@ype} gesvd_left_test (): void

implement{t,tr}
gesvd_left_test () = () where {
  #define M 55
  #define N 30
  val () = assert_prerrf_bool (M >= N
    , "%s: gesvd_left only for skinny matrices", @(#LOCATION)
  ) // end of [val]
// (*
  val () = begin
    print "gesvd_left_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate S and Sc
  stavar n : int
  val Nz = min_size1_size1 (M, N) : size_t n // cannot remove
  var !p_S with pfS_arr = @[tr][Nz]()
  var !p_Sc with pfSc_arr = @[tr][Nz]()
// Set-up workspace for left SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_left_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Left SVD of A
  val info = gesvd_left<t,tr> {M,n} (
        pf_lwork
      | M_, N_, !p_A, M_, !p_S, !p_work, lwork
      )
  val () = begin
    print "info = "; print info; print_newline ();
  end
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_left failed: info = %i\n", @(#LOCATION, info))
// Set-up workspace for singular values SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_sing_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Singular values of Ac
  val info = gesvd_sing<t,tr> (
        pf_lwork
      | M_, N_, !p_Ac, M_, !p_Sc, !p_work, lwork
      )
  val () = begin
    print "info = "; print info; print_newline ();
  end
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_sing failed: info = %i\n", @(#LOCATION, info))
// Check ||Sc - S||
  prval pfS_gvec = GEVEC_v_of_array_v (pfS_arr)
  prval pfSc_gvec = GEVEC_v_of_array_v (pfSc_arr)
  val () = cblas_axpy<tr> (N, of_int<tr> (~1), !p_S, 1, !p_Sc, 1)
  val err = cblas_asum<tr,tr> (N, !p_Sc, 1)
  prval () = pfS_arr := array_v_of_GEVEC_v (pfS_gvec)
  prval () = pfSc_arr := array_v_of_GEVEC_v (pfSc_gvec)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N * min(M,N)), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_left_test failed\n", @(#LOCATION))
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_left_test]

(* ****** ****** *)

// gesvd_right test: S, D, C, Z

extern fun{t,tr:t@ype} gesvd_right_test (): void

implement{t,tr}
gesvd_right_test () = () where {
  #define M 30
  #define N 65
  val () = assert_prerrf_bool
    (M <= N, "%s: gesvd_right only for fat matrices", @(#LOCATION))
// (*
  val () = begin
    print "gesvd_right_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate S and Sc
  stavar m : int
  val Mz = min_size1_size1 (M, N) : size_t m // cannot remove
  var !p_S with pfS_arr = @[tr][Mz]()
  var !p_Sc with pfSc_arr = @[tr][Mz]()
// Set-up workspace for right SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_right_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Right SVD of A
  val info = gesvd_right<t,tr> {m,N} (
        pf_lwork
      | M_, N_, !p_A, M_, !p_S, !p_work, lwork
      )
  val () = begin
    print "info = "; print info; print_newline ();
  end
  val () = assert_prerrf_bool (
    info = 0, "%s: gesvd_right failed: info = %i\n", @(#LOCATION, info)
  )
// Set-up workspace for singular values SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_sing_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Singular values of Ac
  val info = gesvd_sing<t,tr> (
        pf_lwork
      | M_, N_, !p_Ac, M_, !p_Sc, !p_work, lwork
      )
  val () = begin
    print "info = "; print info; print_newline ();
  end
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_sing failed: info = %i", @(#LOCATION, info))
// Check ||Sc - S||
  prval pfS_gvec = GEVEC_v_of_array_v (pfS_arr)
  prval pfSc_gvec = GEVEC_v_of_array_v (pfSc_arr)
  val () = cblas_axpy<tr> (M, of_int<tr> (~1), !p_S, 1, !p_Sc, 1)
  val err = cblas_asum<tr,tr> (M, !p_Sc, 1)
  prval () = pfS_arr := array_v_of_GEVEC_v (pfS_gvec)
  prval () = pfSc_arr := array_v_of_GEVEC_v (pfSc_gvec)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N * min(M,N)), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_right_test failed\n", @(#LOCATION))
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_right_test]

(* ****** ****** *)

// gesvd_skinny test: S, D, C, Z

extern fun{t,tr:t@ype} gesvd_skinny_test (): void

implement{t,tr}
gesvd_skinny_test () = () where {
  #define M 55
  #define N 30
  val () = assert_prerrf_bool1 (
    M >= N, "%s: gesvd_skinny only works for skinny", @(#LOCATION)
  )
// (*
  val () = begin
    print "gesvd_skinny_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
  stavar n : int
  val Nsz = min_size1_size1 (M,N) : size_t n
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate S
  var !p_S = @[tr][Nsz]()
// Allocate VT
  val (pf_gc_VT, pf_NN, pf_fmat_VT | p_VT) = fmatrix_ptr_alloc<t> (N, N)
  prval () = mul_nat_nat_nat (pf_NN)
  prval (pf_gmat_VT, fpf_fmat_VT) = GEMAT_v_of_fmatrix_v (pf_fmat_VT)
// Set-up workspace for SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_skinny_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Skinny SVD of A
  val info = gesvd_skinny<t,tr> {M,n} (
        pf_lwork
      | M_, N_, !p_A, M_, !p_S, !p_VT, N_, !p_work, lwork
      )
  val () = begin
    print "info = "; print info; print_newline ();
  end
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_skinny failed: info = %i\n", @(#LOCATION, info))
// A = U <- U * diag (S)
  val () = GEMAT_scal_col<t,tr> {col} {M,n} (ORDERcol, M, N, !p_A, M, !p_S)
// Ac <- (U * S) * VT - Ac
  val () = cblas_gemm<t> (
          TRANDIM_N (), TRANDIM_N ()
        | CblasColMajor
        , CblasNoTrans
        , CblasNoTrans
        , M, N, N
        , of_int<t> 1
        , !p_A, M
        , !p_VT, N
        , of_int<t> (~1)
        , !p_Ac, M
      )
// Check ||Ac - A||
  val err = lange_one<t,tr> (M_, N_, !p_Ac, M_)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N * min(M,N)), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_skinny_test failed\n", @(#LOCATION))
// Free VT
  prval pf_fmat_VT = fpf_fmat_VT (pf_gmat_VT)
  val () = fmatrix_ptr_free (pf_gc_VT, pf_NN, pf_fmat_VT | p_VT)
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_skinny_test]

(* ****** ****** *)

// gesvd_fat test: S, D, C, Z

extern fun{t,tr:t@ype} gesvd_fat_test (): void

implement{t,tr}
gesvd_fat_test () = () where {
  #define M 30
  #define N 56
  val () = assert_prerrf_bool (
    M <= N, "%s: gesvd_fat only for fat", @(#LOCATION)
  )
// (*
  val () = begin
    print "gesvd_fat_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
  stavar m : int
  val Msz = min_size1_size1 (M,N) : size_t m
// Make A
  val (pf_gc_A, pf_MN, pf_fmat_A | p_A) = randgen_fmat<t> (M, N)
  prval () = mul_nat_nat_nat (pf_MN)
  prval (pf_gmat_A, fpf_fmat_A) = GEMAT_v_of_fmatrix_v (pf_fmat_A)
// Make Ac = copy of A
  val (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac) = fmatrix_ptr_alloc<t> (M, N)
  prval () = mul_nat_nat_nat (pfc_MN)
  val () = fmatrix_ptr_initialize_elt<t> (!p_Ac, M, N, of_int 0)
  prval (pf_gmat_Ac, fpf_fmat_Ac) = GEMAT_v_of_fmatrix_v (pf_fmat_Ac)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate U
  val (pf_gc_U, pf_MM, pf_fmat_U | p_U) = fmatrix_ptr_alloc<t> (M, M)
  prval () = mul_nat_nat_nat (pf_MM)
  prval (pf_gmat_U, fpf_fmat_U) = GEMAT_v_of_fmatrix_v (pf_fmat_U)
// Allocate S
  var !p_S = @[tr][Msz]()
// Set-up workspace for SVD
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val [lwork:int] (pf_lwork | lwork) = gesvd_fat_work_query<t,tr> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// Economy fat SVD of A
  val info = gesvd_fat<t,tr> {m,N} (
        pf_lwork
      | M_, N_, !p_A, M_, !p_S, !p_U, M_, !p_work, lwork
      )
  val () = assert_prerrf_bool
    (info = 0, "%s: gesvd_fat failed: info = %i\n", @(#FILENAME, info))
// U <- U * diag (S)
  val () = GEMAT_scal_col<t,tr> {col} {m,m} (ORDERcol, M, M, !p_U, M, !p_S)
// Ac <- (U * S) * VT - Ac (VT = A)
  val () = cblas_gemm<t> (
          TRANDIM_N (), TRANDIM_N ()
        | CblasColMajor
        , CblasNoTrans
        , CblasNoTrans
        , M, N, M
        , of_int<t> 1
        , !p_U, M
        , !p_A, M
        , of_int<t> (~1)
        , !p_Ac, M
      )
// Check ||Ac - A||
//  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int<t> (~1), !p_A, M, !p_Ac, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac, M_)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N * M), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gesvd_fat_test failed\n", @(#LOCATION))
// Free U
  prval pf_fmat_U = fpf_fmat_U (pf_gmat_U)
  val () = fmatrix_ptr_free (pf_gc_U, pf_MM, pf_fmat_U | p_U)
// Free Ac
  prval pf_fmat_Ac = fpf_fmat_Ac (pf_gmat_Ac)
  val () = fmatrix_ptr_free (pf_gc_Ac, pfc_MN, pf_fmat_Ac | p_Ac)
// Free A
  prval pf_fmat_A = fpf_fmat_A (pf_gmat_A)
  val () = fmatrix_ptr_free (pf_gc_A, pf_MN, pf_fmat_A | p_A)
} // end of [gesvd_fat_test]

(* ****** ****** *)

extern fun{t,tr:t@ype} gelqf_unmlq_test (): void

implement{t,tr}
gelqf_unmlq_test () = () where {
  #define M 2
  #define N 3 
  val () = assert_prerrf_bool
    (M <= N, "%s: only fat matrices\n", @(#LOCATION))
// (*
  val () = begin
    print "gelqf_unmlq_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gmat_A | p_A, free_A) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_rand<t> (ORDERcol, M, N, !p_A, M)
// Copy A
  val (pf_gmat_Ac | p_Ac, free_Ac) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_Ac, M, N, M, of_int 0)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)  
// Allocate tau of Q
  val MN = min (M, N)
  var !p_tau with pf_arr_tau = @[t][MN]()
// Query for optimal work size and allocate work
  val lwork = gelqf_work_query<t> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// A <- LQ = A
  val (pf_lqmat_A | info) = gelqf<t> (
      pf_gmat_A, pf_arr_tau | M_, N_, p_A, M_, p_tau, !p_work, lwork)
  val () = assert_prerrf_bool1
    (info = 0, "%s: gelqf failed: info = %i\n", @(#LOCATION, info))
// Allocate Ac2 <- (L 0)
  val (pf_gmat_Ac2 | p_Ac2, free_Ac2) = GEMAT_col_ptr_allocfree<t> (M, N)
  val (pf1_gmat_Ac2, pf2_gmat_Ac2, fpf_gmat_Ac2 | p1_Ac2, p2_Ac2) =
    GEMAT_ptr_split1x2 (pf_gmat_Ac2 | ORDERcol, p_Ac2, M, M)
  val () = GEMAT_ptr_initialize_eye<t> (ORDERcol, M, !p1_Ac2, M)
  prval (pf_trmat_A, fpf_lqmat_A) = TRMAT_v_of_LQMAT_v (pf_lqmat_A)
  val () = cblas_trmm<t> (
      SIDEDIM_L ()
    | CblasColMajor
    , CblasLeft, CblasLower, CblasNoTrans, CblasNonUnit
    , M, M, of_int 1, !p_A, M, !p1_Ac2, M)
  prval pf_lqmat_A = fpf_lqmat_A (pf_trmat_A)
  val NM_sz = i2sz (N-M)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p2_Ac2, M, NM_sz, M, of_int 0)
  prval pf_gmat_Ac2 = fpf_gmat_Ac2 (pf1_gmat_Ac2, pf2_gmat_Ac2)
// Ac2 <- (L 0) * Q
  val lwork = unmlq_work_query<t>
    (SIDEDIM_R () | ClapackRight, ClapackNoTrans, M_, N_, M_)
  var !p_work with pf_arr_work = @[t][size1_of_integer lwork]()
  val info = unmlq<t> (
      SIDEDIM_R (), pf_lqmat_A
    | ClapackRight, ClapackNoTrans
    , M_, N_, M_, p_A, M_, p_tau, !p_Ac2, M_, !p_work, lwork
    )
// Check ||Ac - Ac2||
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int (~1), !p_Ac, M, !p_Ac2, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac2, M_)
  val isbig = // error cannot be ignored
    $F2C.lte<tr> (err, $F2C.mul<tr> (of_int<tr> (M * N * M), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gelqf_unmlq_test failed\n", @(#LOCATION))
// Free Ac2
  val () = free_Ac2 (pf_gmat_Ac2 | p_Ac2)
// Free LQMAT
  prval (pf_gmat_A, pf_arr_tau1) = LQMAT_err_v_elim (pf_lqmat_A)
  prval () = pf_arr_tau := pf_arr_tau1
// Free Ac
  val () = free_Ac (pf_gmat_Ac | p_Ac)
// Free A
  val () = free_A (pf_gmat_A | p_A)
} // end of [gelqf_unmlq_test]

(* ****** ****** *)

extern fun{t,tr:t@ype} geqlf_unmql_test (): void

implement{t,tr}
geqlf_unmql_test () = () where {
  #define M 30
  #define N 20 
  val () = assert_prerrf_bool
    (N <= M, "%s: only skinny matrices\n", @(#LOCATION))
// (*
  val () = begin
    print "geqlf_unmql_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gmat_A | p_A, free_A) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_rand<t> (ORDERcol, M, N, !p_A, M)
// Copy A
  val (pf_gmat_Ac | p_Ac, free_Ac) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_Ac, M, N, M, of_int 0)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)  
// Allocate tau of Q
  stavar n : int
  val Nsz = min_size1_size1 (M, N) : size_t n
  var !p_tau with pf_arr_tau = @[t][Nsz]()
// Query lwork and allocate work
  val lwork = geqlf_work_query<t> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// A <- LQ = A
  val (pf_qlmat_A | info) = geqlf<t> (
      pf_gmat_A, pf_arr_tau | M_, N_, p_A, M_, p_tau, !p_work, lwork)
  val () = assert_prerrf_bool1
    (info = 0, "%s: geqlf failed: info = %i\n", @(#LOCATION, info))
// Allocate Ac2 <- (0 L')'
  val (pf_gmat_Ac2 | p_Ac2, free_Ac2) = GEMAT_col_ptr_allocfree<t> (M, N)
  val (pf1_gmat_Ac2, pf2_gmat_Ac2, fpf_gmat_Ac2 | p1_Ac2, p2_Ac2) =
    GEMAT_ptr_split2x1 (pf_gmat_Ac2 | ORDERcol, p_Ac2, M, i2sz (M-N))
  val () = GEMAT_ptr_initialize_eye<t> (ORDERcol, N, !p2_Ac2, M)
  val (pf_trmat_A, fpf_qlmat_A | p_L) =
    TRMAT_of_QLMAT<t> (pf_qlmat_A | M_, N_, p_A)
  val () = cblas_trmm<t> (
      SIDEDIM_L ()
    | CblasColMajor
    , CblasLeft, CblasLower, CblasNoTrans, CblasNonUnit
    , N, N, of_int 1, !p_L, M, !p2_Ac2, M)
  prval pf_qlmat_A = fpf_qlmat_A (pf_trmat_A)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p1_Ac2, i2sz (M-N), N, M, of_int 0)
  prval pf_gmat_Ac2 = fpf_gmat_Ac2 (pf1_gmat_Ac2, pf2_gmat_Ac2)
// Ac2 <- Q * (0 L')'
  val lwork = unmql_work_query<t>
    (SIDEDIM_L () | ClapackLeft, ClapackNoTrans, M_, N_, N_)
  var !p_work with pf_arr_work = @[t][size1_of_integer lwork]()
  val info = unmql<t> (
      SIDEDIM_L (), pf_qlmat_A
    | ClapackLeft, ClapackNoTrans
    , M_, N_, N_, p_A, M_, p_tau, !p_Ac2, M_, !p_work, lwork
    )
// Check ||Ac - Ac2||
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int (~1), !p_Ac, M, !p_Ac2, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac2, M_)
  val isbig = // error cannot be ignored
    $F2C.lte<tr> (err, $F2C.mul<tr> (of_int<tr> (M * N * N), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: geqlf_unmql_test failed\n", @(#LOCATION))
// Free Ac2
  val () = free_Ac2 (pf_gmat_Ac2 | p_Ac2)
// Free LQMAT
  prval (pf_gmat_A, pf_arr_tau1) = QLMAT_err_v_elim (pf_qlmat_A)
  prval () = pf_arr_tau := pf_arr_tau1
// Free Ac
  val () = free_Ac (pf_gmat_Ac | p_Ac)
// Free A
  val () = free_A (pf_gmat_A | p_A)
} // end of [geqlf_unmql_test]

(* ****** ****** *)

extern fun{t,tr:t@ype} geqrf_unmqr_test (): void

implement{t,tr}
geqrf_unmqr_test () = () where {
  #define M 30
  #define N 20 
  val () = assert_prerrf_bool1
    (N <= M, "%s: Only skinny matrices.", @(#LOCATION))
// (*
  val () = begin
    print "geqrf_unmqr_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gmat_A | p_A, free_A) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_rand<t> (ORDERcol, M, N, !p_A, M)
// Copy A
  val (pf_gmat_Ac | p_Ac, free_Ac) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_Ac, M, N, M, of_int 0)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)  
// Allocate tau of Q
  stavar n : int
  val Nsz = min_size1_size1 (M, N) : size_t n
  var !p_tau with pf_arr_tau = @[t][Nsz]()
// Work query and allocate
  val lwork = geqrf_work_query<t> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// A <- QR = A
  val (pf_qrmat_A | info) = geqrf<t> (
      pf_gmat_A, pf_arr_tau | M_, N_, p_A, M_, p_tau, !p_work, lwork)
  val () = assert_prerrf_bool1
    (info = 0, "%s: geqrf failed!", @(#LOCATION))
// Allocate Ac2 <- (R' 0)'
  val (pf_gmat_Ac2 | p_Ac2, free_Ac2) = GEMAT_col_ptr_allocfree<t> (M, N)
  val (pf1_gmat_Ac2, pf2_gmat_Ac2, fpf_gmat_Ac2 | p1_Ac2, p2_Ac2) =
    GEMAT_ptr_split2x1 (pf_gmat_Ac2 | ORDERcol, p_Ac2, M, N)
  val () = GEMAT_ptr_initialize_eye<t> (ORDERcol, N, !p1_Ac2, M)
  prval (pf_trmat_A, fpf_qrmat_A) = TRMAT_v_of_QRMAT_v (pf_qrmat_A)
  val () = cblas_trmm<t> (
      SIDEDIM_L ()
    | CblasColMajor
    , CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit
    , N, N, of_int 1, !p_A, M, !p1_Ac2, M)
  prval pf_qrmat_A = fpf_qrmat_A (pf_trmat_A)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p2_Ac2, i2sz (M-N), N, M, of_int 0)
  prval pf_gmat_Ac2 = fpf_gmat_Ac2 (pf1_gmat_Ac2, pf2_gmat_Ac2)
// Ac2 <- Q * (R' 0)'
  val lwork = unmqr_work_query<t>
    (SIDEDIM_L () | ClapackLeft, ClapackNoTrans, M_, N_, N_)
  var !p_work with pf_arr_work = @[t][size1_of_integer lwork]()
  val info = unmqr<t> (
      SIDEDIM_L (), pf_qrmat_A
    | ClapackLeft, ClapackNoTrans
    , M_, N_, N_, p_A, M_, p_tau, !p_Ac2, M_, !p_work, lwork
    )
// Check ||Ac - Ac2||
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int (~1), !p_Ac, M, !p_Ac2, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac2, M_)
  val isbig = // error cannot be ignored
    $F2C.lte<tr> (err,  $F2C.mul<tr> (of_int<tr> (M * N * N), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: geqrf_unmql_test failed\n", @(#LOCATION))
// Free Ac2
  val () = free_Ac2 (pf_gmat_Ac2 | p_Ac2)
// Free QRMAT
  prval (pf_gmat_A, pf_arr_tau1) = QRMAT_err_v_elim (pf_qrmat_A)
  prval () = pf_arr_tau := pf_arr_tau1
// Free Ac
  val () = free_Ac (pf_gmat_Ac | p_Ac)
// Free A
  val () = free_A (pf_gmat_A | p_A)
} // end of [geqrf_unmqr_test]

(* ****** ****** *)

extern fun{t,tr:t@ype} gerqf_unmrq_test (): void

implement{t,tr}
gerqf_unmrq_test () = () where {
  #define M 20
  #define N 30 
  val () = assert_prerrf_bool
    (N >= M, "%s: only fat matrices\n", @(#LOCATION))
// (*
  val () = begin
    print "gerqf_unmrq_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gmat_A | p_A, free_A) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_rand<t> (ORDERcol, M, N, !p_A, M)
// Copy A
  val (pf_gmat_Ac | p_Ac, free_Ac) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_Ac, M, N, M, of_int 0)
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)  
// Allocate tau of Q
  val MN = min (M, N)
  var !p_tau with pf_arr_tau = @[t][MN]()
// Work query and allocate
  val lwork = gerqf_work_query<t> (M_, N_)
  var !p_work = @[t][size1_of_integer lwork]()
// A <- RQ = A
  val (pf_rqmat_A | info) = gerqf<t> (
      pf_gmat_A, pf_arr_tau | M_, N_, p_A, M_, p_tau, !p_work, lwork)
  val () = assert_prerrf_bool1
    (info = 0, "%s: gerqf failed: info = %i\n", @(#LOCATION, info))
// Allocate Ac2 <- (0 R)
  val (pf_gmat_Ac2 | p_Ac2, free_Ac2) = GEMAT_col_ptr_allocfree<t> (M, N)
  val (pf1_gmat_Ac2, pf2_gmat_Ac2, fpf_gmat_Ac2 | p1_Ac2, p2_Ac2) =
    GEMAT_ptr_split1x2 (pf_gmat_Ac2 | ORDERcol, p_Ac2, M, i2sz (N-M))
  val () = GEMAT_ptr_initialize_eye<t> (ORDERcol, M, !p2_Ac2, M)
  val (pf_trmat_A, fpf_rqmat_A | p_R) =
    TRMAT_of_RQMAT<t> (pf_rqmat_A | M_, N_, M_, p_A)
  val () = cblas_trmm<t> (
      SIDEDIM_L ()
    | CblasColMajor
    , CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit
    , M, M, of_int 1, !p_R, M, !p2_Ac2, M)
  prval pf_rqmat_A = fpf_rqmat_A (pf_trmat_A)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p1_Ac2, M, i2sz (N-M), M, of_int 0)
  prval pf_gmat_Ac2 = fpf_gmat_Ac2 (pf1_gmat_Ac2, pf2_gmat_Ac2)
// Ac2 <- (0 R) * Q
  val lwork = unmrq_work_query<t>
    (SIDEDIM_R () | ClapackRight, ClapackNoTrans, M_, N_, M_)
  var !p_work with pf_arr_work = @[t][size1_of_integer lwork]()
  val info = unmrq<t> (
      SIDEDIM_R (), pf_rqmat_A
    | ClapackRight, ClapackNoTrans
    , M_, N_, M_, p_A, M_, p_tau, !p_Ac2, M_, !p_work, lwork
    )
// Check ||Ac - Ac2||
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int (~1), !p_Ac, M, !p_Ac2, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac2, M_)
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N * M), macheps_get<tr> ()))
  val () = assert_prerrf_bool (isbig, "%s: gerqf_unmrq_test failed\n", @(#LOCATION))
// Free Ac2
  val () = free_Ac2 (pf_gmat_Ac2 | p_Ac2)
// Free RQMAT
  prval (pf_gmat_A, pf_arr_tau1) = RQMAT_err_v_elim (pf_rqmat_A)
  prval () = pf_arr_tau := pf_arr_tau1
// Free Ac
  val () = free_Ac (pf_gmat_Ac | p_Ac)
// Free A
  val () = free_A (pf_gmat_A | p_A)
} // end of [gerqf_unmrq_test]

(* ****** ****** *)

extern prfun anonymitize_at
  {a:viewt@ype} {l:addr} (pf: !a @ l >> v): #[v:view] (v -<prf> a @ l)
// end of [vhide]

implement
anonymitize_at {a} {l} (pf) = let
  viewdef V = a @ l; prval id = lam (pf: V): V =<prf> pf in #[V | id]
end // end of [anonymitize]

extern fun{t,tr:t@ype} getrf_laswp_test (): void

implement{t,tr}
getrf_laswp_test () = () where {
  #define M 30
  #define N 20
  val () = assert_prerrf_bool
    (M >= N, "%s: only skinny matrices\n", @(#LOCATION))
  val M_ = integer_of_int1 M
  val N_ = integer_of_int1 N
// (*
  val () = begin
    print "getrf_laswp_test<"; print_typ<t> (); print ">: "; print_newline ()
  end // end of [val]
// *)
// Make A
  val (pf_gmat_A | p_A, free_A) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_rand<t> (ORDERcol, M, N, !p_A, M)
// Copy A
  val (pf_gmat_Ac | p_Ac, free_Ac) = GEMAT_col_ptr_allocfree<t> (M, N)
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_Ac, M, N, M, of_int 0)
  val () = lacpy<t> (ClapackULN_N, M_, N_, !p_A, M_, !p_Ac, M_)
// Allocate ipiv
  var !p_ipiv = @[integer][min_size1_size1(M,N)]()
// A -> PLU
  val (pf_lumat | info) = getrf<t> (pf_gmat_A | M_, N_, p_A, M_, !p_ipiv)
  val () = assert_prerrf_bool1
    (info >= 0, "%s: getrf failed with info = %i\n", @(#LOCATION, info))
// Get factors
  val (pf_L, pf_U, pf_mat, fpf_lumat | p_mat) =
    LUMAT_ptr_split_skinny<t> (pf_lumat | p_A, M, N, M)
// Multiply factors
// Allocate space
  val (pf_gmat_Ac2 | p_Ac2, free_Ac2) = GEMAT_col_ptr_allocfree<t> (M, N)
// Split into top & bottom
  val (pf_gmat_Ac2_1, pf_gmat_Ac2_2, fpf_gmat_Ac2 | p_Ac2_1, p_Ac2_2) =
    GEMAT_ptr_split2x1<t?> (pf_gmat_Ac2 | ORDERcol, p_Ac2, M, N)
//   Ac2 <- [L1 L2] * U
  val () = GEMAT_ptr_initialize_elt<t> (ORDERcol, !p_Ac2_1, N, N, M, of_int 0)
//
  prval fpf_U = anonymitize_at (pf_U)
  val () = trcpy<t>
    (ORDERcol, UPLOlower, DIAGunit, N, !p_A, M, !p_Ac2_1, M)
  prval () = pf_U := fpf_U (pf_U)
//
  val () = GEMAT_ptr_initialize_elt<t>
    (ORDERcol, !p_Ac2_2, i2sz (M-N), N, M, of_int 0)
  val () = lacpy<t> (ClapackULN_N, M_-N_, N_, !p_mat, M_, !p_Ac2_2, M_)
  prval pf_gmat_Ac2 = fpf_gmat_Ac2 (pf_gmat_Ac2_1, pf_gmat_Ac2_2)
//
  prval fpf_L = anonymitize_at (pf_L)
  val () = cblas_trmm<t> (
    SIDEDIM_R ()
  | CblasColMajor, CblasRight, CblasUpper, CblasNoTrans, CblasNonUnit
  , M, N, of_int 1, !p_A, M, !p_Ac2, M
  ) // end of [val]
  prval () = pf_L := fpf_L (pf_L)
// Ac2 <- ipiv * Ac2
  val () = laswp<t> (N_, !p_Ac2, M_,
                     integer_of_int1 1, N_ , !p_ipiv, integer_of_int1 (~1))
// norm (Ac - Ac2)
  val () = GEMAT_axpy<t> (ORDERcol, M, N, of_int (~1), !p_Ac, M, !p_Ac2, M)
  val err = lange_one<t,tr> (M_, N_, !p_Ac2, M_)
// Error check
  val isbig = $F2C.lte<tr> // error cannot be ignored
    (err, $F2C.mul<tr> (of_int<tr> (M * N), macheps_get<tr> ()))
  val () = assert_prerrf_bool
    (isbig, "%s: getrf_laswp_test failed \n", @(#LOCATION))
// Free A
  prval pf_lumat = fpf_lumat (pf_L, pf_U, pf_mat)
  prval pf_gmat_A = LUMAT_err_v_elim (pf_lumat)
  val () = free_A (pf_gmat_A | p_A)
// Free Ac
  val () = free_Ac (pf_gmat_Ac | p_Ac)
// Free Ac2
  val () = free_Ac2 (pf_gmat_Ac2 | p_Ac2)
} // end of [getrf_laswp_test]

(* ****** ****** *)

fn part1_test () = () where {
  val () = print "lange_lacpy_test: begins ... "
  val () = lange_lacpy_test<real,real> ()
  val () = lange_lacpy_test<doublereal,doublereal> ()
  val () = lange_lacpy_test<complex,real> ()
  val () = lange_lacpy_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gels_test: begins ... "
  val () = gels_test<real,real> ()
  val () = gels_test<doublereal,doublereal> ()
  val () = gels_test<complex,real> ()
  val () = gels_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gels_test_minnorm: begins ... "
  val () = gels_test_minnorm<real,real> ()
  val () = gels_test_minnorm<doublereal,doublereal> ()
  val () = gels_test_minnorm<complex,real> ()
  val () = gels_test_minnorm<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesv_test: begins ... "
  val () = gesv_test<real,real> ()
  val () = gesv_test<doublereal,doublereal> ()
  val () = gesv_test<complex,real> ()
  val () = gesv_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_test: begins ...\n"
  val () = gesvd_test<real,real> ()
  val () = gesvd_test<doublereal,doublereal> ()
  val () = gesvd_test<complex,real> ()
  val () = gesvd_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_econ_test: begins ...\n"
  val () = gesvd_econ_test<real,real> ()
  val () = gesvd_econ_test<doublereal,doublereal> ()
  val () = gesvd_econ_test<complex,real> ()
  val () = gesvd_econ_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_sing_test: begins ...\n"
  val () = gesvd_sing_test<real,real> ()
  val () = gesvd_sing_test<doublereal,doublereal> ()
  val () = gesvd_sing_test<complex,real> ()
  val () = gesvd_sing_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_left_test: begins ...\n"
  val () = gesvd_left_test<real,real> ()
  val () = gesvd_left_test<doublereal,doublereal> ()
  val () = gesvd_left_test<complex,real> ()
  val () = gesvd_left_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_right_test: begins ...\n"
  val () = gesvd_right_test<real,real> ()
  val () = gesvd_right_test<doublereal,doublereal> ()
  val () = gesvd_right_test<complex,real> ()
  val () = gesvd_right_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_skinny_test: begins ...\n"
  val () = gesvd_skinny_test<real,real> ()
  val () = gesvd_skinny_test<doublereal,doublereal> ()
  val () = gesvd_skinny_test<complex,real> ()
  val () = gesvd_skinny_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gesvd_fat_test: begins ...\n"
  val () = gesvd_fat_test<real,real> ()
  val () = gesvd_fat_test<doublereal,doublereal> ()
  val () = gesvd_fat_test<complex,real> ()
  val () = gesvd_fat_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gelqf_unmlq_test: begins ...\n"
  val () = gelqf_unmlq_test<real,real> ()
  val () = gelqf_unmlq_test<doublereal,doublereal> ()
  val () = gelqf_unmlq_test<complex,real> ()
  val () = gelqf_unmlq_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "geqlf_unmql_test: begins ...\n"
  val () = geqlf_unmql_test<real,real> ()
  val () = geqlf_unmql_test<doublereal,doublereal> ()
  val () = geqlf_unmql_test<complex,real> ()
  val () = geqlf_unmql_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "geqrf_unmqr_test: begins ...\n"
  val () = geqrf_unmqr_test<real,real> ()
  val () = geqrf_unmqr_test<doublereal,doublereal> ()
  val () = geqrf_unmqr_test<complex,real> ()
  val () = geqrf_unmqr_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "gerqf_unmrq_test: begins ...\n"
  val () = gerqf_unmrq_test<real,real> ()
  val () = gerqf_unmrq_test<doublereal,doublereal> ()
  val () = gerqf_unmrq_test<complex,real> ()
  val () = gerqf_unmrq_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
//
  val () = print "getrf_laswp_test: begins ...\n"
  val () = getrf_laswp_test<real,real> ()
  val () = getrf_laswp_test<doublereal,doublereal> ()
  val () = getrf_laswp_test<complex,real> ()
  val () = getrf_laswp_test<doublecomplex,doublereal> ()
  val () = (print "finishes"; print_newline ())
} // end of [part1_test]

(* ****** ****** *)

dynload "contrib/cblas/DATS/cblas.dats"
dynload "contrib/cblas/DATS/cblas_extra.dats"
dynload "contrib/clapack/DATS/clapack.dats"

(* ****** ****** *)

implement main () = () where {
// (*
  val () = srand48_with_time () // this makes it really hard for debugging
// *)
  val () = begin
    print "clapack_test: starts"; print_newline ()
  end // end of [val]
  val () = part1_test ()
  val () = begin
    print "clapack_test: finishes"; print_newline ()
  end // end of [val]
} // end of [main]

(* ****** ****** *)

(* end of [clapack_test.dats] *)
