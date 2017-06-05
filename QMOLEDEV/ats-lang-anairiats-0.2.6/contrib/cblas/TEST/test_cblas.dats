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

local #include "prelude/HATS/number.hats" in (*empty*) end
local #include "contrib/cblas/HATS/cblas.hats" in (*empty*) end

(* ****** ****** *)

staload M = "libc/SATS/math.sats"
macdef M_PI = $M.M_PI

staload C = "libc/SATS/complex.sats"
typedef ccmplx = $C.ccmplx
typedef zcmplx = $C.zcmplx

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload NUM = "prelude/SATS/number.sats"
macdef sin = $NUM.sin
macdef cos = $NUM.cos

(* ****** ****** *)

fn{a:t@ype} of_int (x):<> a = $NUM.of_int<a> (x)
fn{a:t@ype} of_double (x):<> a = $NUM.of_double<a> (x)

fn{a:t@ype} print_typ (): void = $NUM.print_typ<a> ()
fn{a:t@ype} print_elt (x: a): void = $NUM.print_elt<a> (x)

(* ****** ****** *)

#define sz2i int1_of_size1
#define i2sz size1_of_int1

(* ****** ****** *)

local

fn{a:t@ype}
macheps_get ():<> a = let
  val one = of_double<a> 1.0
  val two = of_double<a> 2.0
  fun guess (x: a):<cloref,!ntm> a =
    if $NUM.eq<a> (one, one \$NUM.add x)
      then x
      else guess (x \$NUM.div two)
    // end of [if]
  (* end of [guess] *)
in
  $effmask_ntm (guess one)
end // end of [machine_eps]

in // in of [local]

val macheps_float = macheps_get<float> ()
val macheps_double = macheps_get<double> ()

end // end of [local]

(* ****** ****** *)

extern fun{a:t@ype} isEpsilon (x: a): bool

val epsilon_double = 0.001
val epsilon_float = float_of (0.001)

implement isEpsilon<float> (x) = abs x <= epsilon_float
implement isEpsilon<double> (x) = abs x <= epsilon_double
implement isEpsilon<ccmplx> (x) = $C.abs_ccmplx x <= epsilon_float
implement isEpsilon<zcmplx> (x) = $C.abs_zcmplx x <= epsilon_double

(*

//
// this is too strict for some of the tests to pass
//
implement isEpsilon<float> (x) = abs x <= macheps_float
implement isEpsilon<double> (x) = abs x <= macheps_double
implement isEpsilon<ccmplx> (x) = abs_ccmplx x <= macheps_float
implement isEpsilon<zcmplx> (x) = abs_zcmplx x <= macheps_double

*)

(* ****** ****** *)

fn srandgen_elt ():<!ref> float = let
  val x = drand48 () in float_of_double (x)
end // end of [srandgen_elt]

fn drandgen_elt ():<!ref> double = drand48 ()

fn crandgen_elt ():<!ref> ccmplx =
  $C.ccmplx_make_cart (x, y) where {
  val x = drand48 () and y = drand48 ()
  val x = float_of_double x and y = float_of_double y
} // end of [crandgen_elt]

fn zrandgen_elt ():<!ref> zcmplx = let
  val x = drand48 () and y = drand48 () in
  $C.zcmplx_make_cart (x, y)
end // end of [zrandgen_elt]

extern fun{a:t@ype} randgen_elt ():<!ref> a

implement randgen_elt<float> () = srandgen_elt ()
implement randgen_elt<double> () = drandgen_elt ()
implement randgen_elt<ccmplx> () = crandgen_elt ()
implement randgen_elt<zcmplx> () = zrandgen_elt ()

(* ****** ****** *)

fun{a:t@ype}
randgen_arr
  {n:nat} .<>.
  (n: int n)
  :<!ref> [l:addr] (
    free_gc_v (a, n, l), array_v (a, n, l)
  | ptr l
  ) = let
  val tsz = sizeof<a>
  val n_sz = size1_of_int1 (n)
  val (pf_gc, pf_arr | p_arr) =
    array_ptr_alloc_tsz {a} (n_sz, tsz)
  // end of [val]
  val () = array_ptr_initialize_fun<a> (!p_arr, n_sz, f) where {
    val f = lam (
      _: sizeLt n
    , x: &(a?) >> a
    ) : void =<fun,!ref>
      x :=  randgen_elt<a> ()
    // end of [val]
  } // end of [val]
in
  (pf_gc, pf_arr | p_arr)
end // end of [rangen_arr]

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"
staload _(*anonymous*) = "libats/DATS/genarrays.dats"

(* ****** ****** *)

staload "libats/SATS/fmatrix.sats"
staload _(*anonymous*) = "libats/DATS/fmatrix.dats"

fun{a:t@ype}
randgen_fmat
  {m,n:nat} (m: int m, n: int n)
  : [mn:int] [l:addr] (
    free_gc_v (a, mn, l)
  , MUL (m, n, mn)
  , fmatrix_v (a, m, n, l)
  | ptr l
  ) = let
  val (pf_mn | mn) = m imul2 n
  prval () = mul_nat_nat_nat (pf_mn)
  val (pf_gc, pf_arr | p_arr) = randgen_arr<a> (mn)
  prval pf_fmat = fmatrix_v_of_array_v (pf_mn, pf_arr)
in
  (pf_gc, pf_mn, pf_fmat | p_arr)
end // end of [randgen_fmat]

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"

(* ****** ****** *)

// BLAS level 1

(* ****** ****** *)

extern fun{a:t@ype} rotg_test () : void

implement{t}
rotg_test (): void = () where {
  var a = of_double<t> 1.0
  var b = of_double<t> 1.0
  var c : t
  var s : t
  val () = cblas_rotg<t> (a, b, c, s)
  val () = begin
    print "rotg_test<"; print_typ<t> (); print ">";
    print ": a = "; print_elt<t> a;
    print ", b = "; print_elt<t> b;
    print ", c = "; print_elt<t> c;
    print ", s = "; print_elt<t> s;
    print_newline ()
  end // end of [val]
} // end of [rotg_test]

(* ****** ****** *)

extern fun{a:t@ype} rot_test () : void

implement{a}
rot_test () : void = () where {
  #define M 17
  #define N 19
  val (pf_MN | MN) = M imul2 N
  prval () = mul_nat_nat_nat (pf_MN)
//
  val tsz = sizeof<a>
//
  val (pfA_gc, pfA_arr | pA_arr) = randgen_arr<a> (MN)
  prval pfA_fmat = fmatrix_v_of_array_v {a} (pf_MN, pfA_arr)
  prval (pfA_gem, fpfA_fmat) = GEMAT_v_of_fmatrix_v {a} (pfA_fmat)
  val (pfA11_gem, pfA21_gem, fpfA_gem| pA11_arr, pA21_arr) =
    GEMAT_ptr_split2x1<a> (pfA_gem | ORDERcol, pA_arr, M, 1)
  // end of [val]
  prval (pfX_incX, pfX_gev, fpfA11_gem) = GEVEC_v_of_GEMAT_v_row (pfA11_gem)
  // end of [val]
  val incX = MATVECINC_get (pfX_incX | ORDERrow, ORDERcol, M)
  val incX = sz2i incX

  prval (pfY_incY, pfY_gev, fpfA21_gem) = GEMAT_v_uncons_row {a} (pfA21_gem)
  val incY = MATVECINC_get (pfY_incY | ORDERrow, ORDERcol, M)
  val incY = sz2i incY
  
  val N_sz = size1_of_int1 (N)
  val (pfX2_gc, pfX2_arr | pX2_arr) = array_ptr_alloc_tsz {a} (N_sz, tsz)
  var zero: a = of_double<a> (0.0)
  val () = array_ptr_initialize_elt_tsz {a} (!pX2_arr, N_sz, zero, tsz)
  prval pfX2_gev = GEVEC_v_of_array_v {a} (pfX2_arr)
  val () = cblas_copy<a> (N, !pA11_arr, incX, !pX2_arr, 1)
  val (pfY2_gc, pfY2_arr | pY2_arr) = array_ptr_alloc_tsz {a} (N_sz, tsz)
  val () = array_ptr_initialize_elt_tsz {a} (!pY2_arr, N_sz, zero, tsz)
  prval pfY2_gev = GEVEC_v_of_array_v {a} (pfY2_arr)
  val () = cblas_copy<a> (N, !pA21_arr, incY, !pY2_arr, 1)
//
  val theta: double = cos (M_PI / 6.0)
  val c = of_double<a> (cos theta)
  val s = of_double<a> (sin theta)
  val () = cblas_rot<a> (N, !pA11_arr, incX, !pA21_arr, incY, c, s)
  val ci = of_double<a> (cos (~theta))
  val si = of_double<a> (sin (~theta))
  val () = cblas_rot<a> (N, !pA11_arr, incX, !pA21_arr, incY, ci, si)
  val neg1 = of_double<a> (~1.0)
  val () = cblas_axpy<a> (N, neg1, !pA11_arr, incX, !pX2_arr, 1)
  val () = cblas_axpy<a> (N, neg1, !pA21_arr, incY, !pY2_arr, 1)
  val Xnrm2 = cblas_nrm2<a,a> (N, !pX2_arr, 1)
  val Ynrm2 = cblas_nrm2<a,a> (N, !pY2_arr, 1)
//
  val () = begin
    print "rot_test<"; print_typ<a> (); print ">"; print ": starts ... "
  end // end of [val]
(*
  val () = begin
    print_newline ();
    print ": Xnrm2 = "; print_elt<a> Xnrm2;
    print ", Ynrm2 = "; print_elt<a> Ynrm2;
    print_newline ()
  end // end of [val]
*)
  val () = assert (isEpsilon<a> (Xnrm2 \$NUM.div (of_int<a> N)))
  val () = assert (isEpsilon<a> (Ynrm2 \$NUM.div (of_int<a> N)))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pfY2_arr := array_v_of_GEVEC_v (pfY2_gev)
  val () = array_ptr_free (pfY2_gc, pfY2_arr | pY2_arr)
  prval () = pfX2_arr := array_v_of_GEVEC_v (pfX2_gev)
  val () = array_ptr_free (pfX2_gc, pfX2_arr | pX2_arr)
  prval () = pfA21_gem := fpfA21_gem (pfY_gev)
  prval () = pfA11_gem := fpfA11_gem (pfX_gev)
  prval () = pfA_gem := fpfA_gem (pfA11_gem, pfA21_gem)
  prval () = pfA_fmat := fpfA_fmat (pfA_gem)
  prval (pfA_MN, pfA_arr) = array_v_of_fmatrix_v (pfA_fmat)
  prval () = mul_isfun (pf_MN, pfA_MN)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [rot_test]

(* ****** ****** *)

extern fun{a:t@ype} rotm_test (): void

implement{a}
rotm_test (): void = () where {
  #define M 7
  #define N 100
  val (pf_MN | MN) = M imul2 N
  prval () = mul_nat_nat_nat (pf_MN)
//
  val tsz = sizeof<a>
//
  val (pfA_gc, pfA_arr | pA_arr) = randgen_arr<a> (MN)
  prval pfA_fmat = fmatrix_v_of_array_v {a} (pf_MN, pfA_arr)
  prval (pfA_gem, fpfA_fmat) = GEMAT_v_of_fmatrix_v (pfA_fmat)
  prval (pfX_inc, pfX_gev, fpfA_gem) = GEMAT_v_uncons_row (pfA_gem)
  val incX = MATVECINC_get (pfX_inc | ORDERrow, ORDERcol, M)
//
  val (pfB_gc, pfB_arr | pB_arr) = randgen_arr<a> (MN)
  prval pfB_fmat = fmatrix_v_of_array_v {a} (pf_MN, pfB_arr)
  prval (pfB_gem, fpfB_fmat) = GEMAT_v_of_fmatrix_v (pfB_fmat)
  prval (pfY_inc, pfY_gev, fpfB_gem) = GEMAT_v_uncons_row (pfB_gem)
  val incY = MATVECINC_get (pfY_inc | ORDERrow, ORDERcol, M)
//
  var neg1: a = of_double (~1.0)
  val (pfP_gc, pfP_arr | pP_arr) = array_ptr_alloc_tsz {a} (5, tsz)
  val () = array_ptr_initialize_elt_tsz {a} (!pP_arr, 5, neg1, tsz)
  prval pfP_gev = GEVEC_v_of_array_v {a} (pfP_arr)
//
  val _0 = size1_of_int1 0
  var d1 = of_double<a> (1.0)
  var d2 = of_double<a> (1.0)
  var b1 = GEVEC_ptr_get_elt_at<a> (!pA_arr, incX, _0)
  var b2 = GEVEC_ptr_get_elt_at<a> (!pB_arr, incY, _0)
  val () = cblas_rotmg (d1, d2, b1, b2, !pP_arr)
  val () = cblas_rotm (N, !pA_arr, sz2i incX, !pB_arr, sz2i incY, !pP_arr)
//
  val () = assert (
    isEpsilon<a> (GEVEC_ptr_get_elt_at (!pB_arr, incY, _0) \$NUM.div (of_int N))
  ) // end of [val]
  val () = begin
    print "rotm_test<"; print_typ<a> (); print ">";
    print ": Y[0] = "; print_elt<a> (GEVEC_ptr_get_elt_at (!pB_arr, incY, _0));
    print_newline ()
  end // end of [val]
//
  prval () = pfP_arr := array_v_of_GEVEC_v (pfP_gev)
  val () = array_ptr_free (pfP_gc, pfP_arr | pP_arr)
//
  prval () = pfB_gem := fpfB_gem (pfY_gev)
  prval () = pfB_fmat := fpfB_fmat (pfB_gem)
  prval (pfB_MN, pfB_arr) = array_v_of_fmatrix_v (pfB_fmat)
  prval () = mul_isfun (pf_MN, pfB_MN)
  val () = array_ptr_free (pfB_gc, pfB_arr | pB_arr)
//
  prval () = pfA_gem := fpfA_gem (pfX_gev)
  prval () = pfA_fmat := fpfA_fmat (pfA_gem)
  prval (pfA_MN, pfA_arr) = array_v_of_fmatrix_v (pfA_fmat)
  prval () = mul_isfun (pf_MN, pfA_MN)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [rotm_test]

(* ****** ****** *)

extern fun{a1,a2:t@ype} asum_test (): void

implement{a,ar} // ar = |a|
asum_test (): void = () where {
  #define M 7
  #define N 100
  val (pf_MN | MN) = M imul2 N
  prval () = mul_nat_nat_nat (pf_MN)
//
  val tsz = sizeof<a>
//
  val MN_sz = size1_of_int1 MN
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (MN_sz, tsz)
  var one: a = of_int<a> (1)
  val () = array_ptr_initialize_elt_tsz {a} (!p_arr, MN_sz, one, tsz)
  prval (pf_fmat) = fmatrix_v_of_array_v {a} (pf_MN, pf_arr)
  prval (pf_gmat, fpf_fmat) = GEMAT_v_of_fmatrix_v {a} (pf_fmat)
  prval (pf_inc, pf_gev, fpf_gmat) = GEMAT_v_uncons_col {a} (pf_gmat)
  val inc = MATVECINC_get (pf_inc | ORDERcol, ORDERcol, M)
//
  val nrm1 = cblas_asum<a,ar> (M, !p_arr, sz2i inc)
  val () = begin
    print "asum_test<"; print_typ<a> (); print ">"; print ": starts ... ";
  end // end of [val]
(*  
  val () = begin
    print ": nrm1 = "; print_elt<ar> nrm1;
    print_newline ()
  end // end of [val]
*)
  val () = assert (isEpsilon<ar> (nrm1 \$NUM.sub (of_int M)))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pf_gmat := fpf_gmat (pf_gev)
  prval () = pf_fmat := fpf_fmat (pf_gmat)
  prval (pf1_MN, pf_arr) = array_v_of_fmatrix_v (pf_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf_gc, pf_arr | p_arr)
} // end of [asum_test]

(* ****** ****** *)

extern fun{a:t@ype} iamax_test (): void

implement{a}
iamax_test (): void = () where {
  #define M 7
  #define N 10
  val (pf_MN | MN) = M imul2 N
  prval () = mul_nat_nat_nat (pf_MN)
//
  val tsz = sizeof<a>
//
  val MN_sz = size1_of_int1 (MN)
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (MN_sz, tsz)
  prval pf_fmat = fmatrix_v_of_array_v {a?} (pf_MN, pf_arr)
  prval pf = unit_v
  val () = fmatrix_ptr_initialize_vclo<a>
    {unit_v} (pf | !p_arr, M, N, !p_f) where {
    var !p_f = @lam (
        pf_v : !unit_v
      | x : &(a?) >> a, i : sizeLt M, j : sizeLt N
      ) : void =<clo>
      x := of_int<a> (sz2i j - sz2i i)
    // end of [var !p_f]
  } // end of [val]
  prval unit_v () = pf
  prval (pf_gem, fpf_fmat) = GEMAT_v_of_fmatrix_v (pf_fmat)
  #define M1 2
  val (pf_gem1, pf_gem2, fpf_gem | p_arr1, p_arr2) =
    GEMAT_ptr_split2x1<a> (pf_gem | ORDERcol, p_arr, M, M1)
  prval (pf_inc1, pf_gev1, fpf_gem2) = GEMAT_v_uncons_row {a} (pf_gem2)
  val inc1 = MATVECINC_get (pf_inc1 | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val indx = cblas_iamax<a> (N, !p_arr2, inc1)
  val () = begin
    print "iamax_test<"; print_typ<a> (); print">"; print ": starts ... ";
  end // end of [val]
(*
  val () = begin
    print_newline ();
    print ": max_index = "; print indx;
    print_newline ()
  end // end of [val]
*)
  val () = assert (indx = (if M1 < N/2 then N-1 else 0))
  val () = begin
   print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pf_gem2 := fpf_gem2 (pf_gev1)
  prval () = pf_gem := fpf_gem (pf_gem1, pf_gem2)
  prval () = pf_fmat := fpf_fmat (pf_gem)
  prval (pf1_MN, pf_arr) = array_v_of_fmatrix_v (pf_fmat)
  prval () = mul_isfun (pf1_MN, pf_MN)
  val () = array_ptr_free (pf_gc, pf_arr | p_arr)
} // end of [iamax_test]

(* ****** ****** *)

extern fun{a:t@ype} dot_test (): void

implement{a}
dot_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val res_X_Y = cblas_dot<a> (N, !p1_arr, inc1, !p2_arr, inc2)
  val () = begin
    print "dot_test<"; print_typ<a> (); print ">"; print (": starts ... ")
  end // end of [val]
  val res_Y_X = cblas_dot<a> (N, !p1_arr, inc1, !p2_arr, inc2)  
  val () = assert
    (isEpsilon<a> ((res_X_Y \$NUM.sub res_Y_X) \$NUM.div (of_int N)))
  (* end pf [val] *)
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [dot_test]

(* ****** ****** *)

extern fun{a:t@ype} dotu_test (): void

implement{a}
dotu_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val () = begin
    print "dotu_test<"; print_typ<a> (); print ">"; print (": starts ... ")
  end // end of [val]
  val res_X_Y = cblas_dotu<a> (N, !p1_arr, inc1, !p2_arr, inc2)
  val res_Y_X = cblas_dotu<a> (N, !p1_arr, inc1, !p2_arr, inc2)  
  val () = assert
    (isEpsilon<a> ((res_X_Y \$NUM.sub res_Y_X) \$NUM.div (of_int N)))
  (* end pf [val] *)
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [dotu_test]

(* ****** ****** *)

extern fun{a:t@ype} dotc_test (): void

implement{a}
dotc_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val () = begin
    print "dotc_test<"; print_typ<a> (); print ">"; print (": starts ... ")
  end // end of [val]
  val res_X_Y = cblas_dotc<a> (N, !p1_arr, inc1, !p2_arr, inc2)
  val res_Y_X = cblas_dotc<a> (N, !p1_arr, inc1, !p2_arr, inc2)  
  val () = assert
    (isEpsilon<a> ((res_X_Y \$NUM.sub res_Y_X) \$NUM.div (of_int N)))
  (* end pf [val] *)
  val () = begin
    print ("finishes"); print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [dotc_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} swap_test (): void

implement{a,ar} // ar = |a|
swap_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
  prval (pf3_inc, pf3_gev, fpf3_gem) = GEMAT_v_uncons_row (pf3_gem)
  // inc3 = M
  val inc3 = MATVECINC_get (pf3_inc | ORDERrow, ORDERcol, M)
  val inc3 = sz2i inc3
//
  val () = begin
    print "swap_test<"; print_typ<a> (); print ">"; print ": starts ... "
  end // end of [val]
  val neg1 = of_double<a> (~1.0)
  val () = cblas_copy<a> (N, !p1_arr, inc1, !p2_arr, inc2)
  val () = cblas_swap<a> (N, !p1_arr, inc1, !p3_arr, inc3)
  val () = cblas_axpy<a> (N, neg1, !p2_arr, inc2, !p3_arr, inc3)
  val diff = cblas_nrm2<ar,a> (N, !p3_arr, inc3)
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int N)))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_gem := fpf3_gem (pf3_gev)
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [swap_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} copy_test (): void

implement{a,ar} // ar = |a|
copy_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val () = begin
    print "copy_test<"; print_typ<a> (); print ">"; print (": starts ... ")
  end // end of [val]
  val X_nrm2 = cblas_nrm2<ar,a> (N, !p1_arr, inc1)
  val () = cblas_copy<a> (N, !p1_arr, inc1, !p2_arr, inc2)
  val Y_nrm2 = cblas_nrm2<ar,a> (N, !p2_arr, inc2)
  val () = assert
    (isEpsilon<ar> ((X_nrm2 \$NUM.sub Y_nrm2) \$NUM.div (of_int N)))
  (* end of [val] *)
//
  val () = begin
    print ("finishes"); print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [copy_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} axpy_test (): void

implement{a,ar} // ar = |a|
axpy_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val () = begin
    print "axpy_test<"; print_typ<a> (); print ">"; print (": starts ... ")
  end // end of [val]
  val X1_nrm2 = cblas_nrm2<ar,a> (N, !p1_arr, inc1)
  val Y1_nrm2 = cblas_nrm2<ar,a> (N, !p2_arr, inc2)
  val zero = of_double<a> ( 0.0)
  val pos1 = of_double<a> ( 1.0)
  val neg1 = of_double<a> (~1.0)
  val () = cblas_axpy<a>
    (N, neg1, !p1_arr, inc1, !p2_arr, inc2) // Y <- Y - X // Y = Y0 - X0
  val () = cblas_axpy<a>
    (N, pos1, !p2_arr, inc2, !p1_arr, inc1) // X <- X + Y // X = Y0
  val () = cblas_axpy<a>
    (N, neg1, !p1_arr, inc1, !p2_arr, inc2) // Y <-  Y - X // Y = -X0
  val X2_nrm2 = cblas_nrm2<ar,a> (N, !p1_arr, inc1)
  val Y2_nrm2 = cblas_nrm2<ar,a> (N, !p2_arr, inc2)
  val () = assert
    (isEpsilon<ar> ((X1_nrm2 \$NUM.sub Y2_nrm2) \$NUM.div (of_int N)))
  val () = assert
    (isEpsilon<ar> ((Y1_nrm2 \$NUM.sub X2_nrm2) \$NUM.div (of_int N)))
  val () = begin
    print ("finishes"); print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [axpy_test]

(* ****** ****** *)

extern fun{a,a2,ar:t@ype} scal_test (): void

implement{a,a2,ar} // ar = |a|
scal_test (): void = () where {
  #define M 7 // needs to be a positive number
  #define N 100 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
//
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_inc, pf1_gev, fpf1_gem) = GEMAT_v_uncons_row (pf1_gem)
  // inc1 = M
  val inc1 = MATVECINC_get (pf1_inc | ORDERrow, ORDERcol, M)
  val inc1 = sz2i inc1
//
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (MN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_row (pf2_gem)
  // inc2 = M
  val inc2 = MATVECINC_get (pf2_inc | ORDERrow, ORDERcol, M)
  val inc2 = sz2i inc2
//
  val () = begin
    print "scal_test<";
    print_typ<a> ();
    print ",";
    print_typ<a2> ();
    print ">";
    print (": starts ... ")
  end // end of [val]
  val X1_nrm2 = cblas_nrm2<ar,a> (N, !p1_arr, inc1)
(*
  val () = begin
    print "X_nrm2 = "; print_elt<ar> (X1_nrm2); print_newline ()
  end // end of [val]
*)
  val neg1 = of_double<a2> (~1.0)
  val () = cblas_scal<a,a2> (N, neg1, !p1_arr, inc1) // X <- ~X
  val X2_nrm2 = cblas_nrm2<ar,a> (N, !p1_arr, inc1)
(*
  val () = begin
    print "X_nrm2 = "; print_elt<ar> (X2_nrm2);
    print_newline ()
  end // end of [val]
*)
  val () = assert
    (isEpsilon<ar> ((X1_nrm2 \$NUM.sub X2_nrm2) \$NUM.div (of_int N)))
  // end of [val]
  val () = begin
    print ("finishes"); print_newline ()
  end // end of [val]
//
  prval () = pf1_gem := fpf1_gem (pf1_gev)  
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_MN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_MN, pf2_MN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [scal_test]

(* ****** ****** *)

// BLAS level 2

(* ****** ****** *)

extern fun{a,ar:t@ype} gemv_test (): void

//
// relate gemv to gemm
//

implement{a,ar} // ar = |a|
gemv_test (): void = () where {
  #define M 19 // needs to be a positive number
  #define K 100 // needs to be a positive number
  #define N 1 // needs to be a positive number
//
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_gemm<a> (
    TRANDIM_N
  , TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, N, K
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , zero (* beta *)
  , !p3_arr, M
  ) // end of [val]
//
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_col (pf2_gem)
  // inc2 = 1
  val inc2 = MATVECINC_get (pf2_inc | ORDERcol, ORDERcol, K)
  val inc2 = sz2i inc2
  prval (pf3_inc, pf3_gev, fpf3_gem) = GEMAT_v_uncons_col (pf3_gem)
  // inc3 = 1
  val inc3 = MATVECINC_get (pf3_inc | ORDERcol, ORDERcol, M)
  val inc3 = sz2i inc3
//
  val () = cblas_gemv<a> (
    TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , M, K
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, inc2
  , neg1 (* beta *)
  , !p3_arr, inc3
  ) // end of [val]
//
  val diff = cblas_nrm2<ar,a> (M, !p3_arr, inc3)
  val () = begin
    print "gemv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = assert (isEpsilon<ar> diff)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_gem := fpf3_gem (pf3_gev)  
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [gemv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} gbmv_test (): void

implement{a,ar} // ar = |a|
gbmv_test (): void = () where {
  #define M 100
  #define K 70
  #define KU 5
  #define KL 7
  #define R (1+KL+KU)
  val K_sz = i2sz K
  val R_sz = i2sz R
  val (pf_RK | RK_sz) = mul2_size1_size1 (R_sz, K_sz)
  prval () = mul_nat_nat_nat (pf_RK)
  val (pfB_gc, pfB_arr | pB_arr) =
    array_ptr_alloc_tsz {a} (RK_sz, sizeof<a>)
  // end of [val]
  prval pfB_fmat =
    fmatrix_v_of_array_v {a?} {R,K} (pf_RK, pfB_arr)
  // end of [prval]
  prval pf_unit = unit_v
  val () = fmatrix_ptr_initialize_vclo<a>
    {unit_v} (pf_unit | !pB_arr, R_sz, K_sz, !p_f) where {
    val klpku = of_int<a> (KL + KU)
    var !p_f = @lam (
        pf : !unit_v | x : &(a?) >> a, k : sizeLt R, j : sizeLt K
      ) : void =<clo>
      x := // k = diagonal (ku = main), j = column
        (if k <> KU then of_int<a> (~1) else klpku)
  } // end of [val]
  prval unit_v () = pf_unit
  prval (pfB_gbmat, fpfB_fmat) =
    GBMAT_v_of_fmatrix_v {a} (pfB_fmat, M, KL, KU)
  // end of [prval]
  val (pfX_gc, pfX_arr | pX_arr) =
    array_ptr_alloc_tsz {a} (K, sizeof<a>)
  // end of [val]
  val () = array_ptr_initialize_elt<a> (!pX_arr, K, of_int 1)
  prval pfX_gev = GEVEC_v_of_array_v {a} (pfX_arr)
  val incX = 1
  val (pfY_gc, pfY_arr | pY_arr) =
    array_ptr_alloc_tsz {a} (M, sizeof<a>)
  // end of [val]
//
  val () = array_ptr_initialize_fun<a> (!pY_arr, M, f) where {
    fun f .<>. (
      i : sizeLt M, x : &(a?) >> a
    ) :<> void = let
      val i = sz2i i
      val x0 = (
        if i < KL then
          KL - i
        else if i < K-KU then
          0
        else if i < K then
          KU - (K - i - 1)
        else if i < K + KL then
          i - K - KL
        else
          0
        // end of [if]
      ) : int
    in
      x := of_int<a> x0
    end // end of [f]
  } // end of [val]
//
  prval pfY_gev = GEVEC_v_of_array_v {a} (pfY_arr)
  val incY = 1
//
  val () = cblas_gbmv<a> (
    TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , M, K
  , KL, KU
  , of_int 1 (* alpha *)
  , !pB_arr, R
  , !pX_arr, incX
  , of_int (~1) (* beta *)
  , !pY_arr, incY
  )
  val diff = cblas_asum<a,ar> (M, !pY_arr, incY)
(*
  val () = begin
    print "gbmv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
*)
  val () = begin
    print "gbmv_test<"; print_typ<a> (); print ">: starts ... ";
  end
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (M*(KL+KU)))))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pfX_arr := array_v_of_GEVEC_v (pfX_gev)
  val () = array_ptr_free (pfX_gc, pfX_arr | pX_arr)
  prval () = pfY_arr := array_v_of_GEVEC_v (pfY_gev)
  val () = array_ptr_free (pfY_gc, pfY_arr | pY_arr)
  prval () = pfB_fmat := fpfB_fmat (pfB_gbmat)
  prval (pf2_RK, pfB_arr) = array_v_of_fmatrix_v (pfB_fmat)
  prval () = mul_isfun (pf_RK, pf2_RK)
  val () = array_ptr_free (pfB_gc, pfB_arr | pB_arr)
} // end of [gbmv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} trmv_trsv_test (): void

implement{a,ar} // ar = |a|
trmv_trsv_test (): void = () where {
  #define M 100
//
  val (pf_MM | MM) = M imul2 M
  prval () = mul_nat_nat_nat (pf_MM)
  val (pfA_gc, pfA_arr | pA_arr) = array_ptr_alloc<a> (size1_of_int1 MM)
  prval pfA_fmat = fmatrix_v_of_array_v (pf_MM, pfA_arr)
  prval pf_unit = unit_v
  val () = fmatrix_ptr_initialize_vclo<a>
    {unit_v} (pf_unit | !pA_arr, M, M, !p_f) where {
    var !p_f = @lam (pf : !unit_v | A : &(a?) >> a, i : sizeLt M, j : sizeLt M)
      : void =<clo>
      A := of_int<a> (sz2i j - sz2i i)
  }
  prval unit_v () = pf_unit
  prval (pfA_gmat, fpfA_fmat) = GEMAT_v_of_fmatrix_v (pfA_fmat)
  prval (pfA_trmat, fpfA_gmat) =
    TRMAT_v_of_GEMAT_v (pfA_gmat, UPLOupper, DIAGunit)
//
  val (pfX_gc, pfX_arr | pX_arr) = array_ptr_alloc<a> (M)
  val () = array_ptr_initialize_elt<a> (!pX_arr, M, of_int 1)
  prval pfX_gev = GEVEC_v_of_array_v {a} (pfX_arr)
  val incX = 1
  val (pfY_gc, pfY_arr | pY_arr) = array_ptr_alloc<a> (M)
  val () = array_ptr_initialize_elt<a> (!pY_arr, M, of_int 1)
  prval pfY_gev = GEVEC_v_of_array_v (pfY_arr)
  val incY = 1
//
  val () = cblas_trmv<a> (
    CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , M
  , !pA_arr, M
  , !pX_arr, incX
  )
//
  val () = cblas_trsv<a> (
    CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , M
  , !pA_arr, M
  , !pX_arr, incX
  )
//
  val () = cblas_axpy<a> (
    M
  , of_int (~1)
  , !pX_arr, incX
  , !pY_arr, incY
  )
  val diff = cblas_asum<a,ar> (M, !pY_arr, incY)
  val () = begin
    print "trmv_trsv_test<"; print_typ<a> (); print ">: starts ... "
  end
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (M * M))))
  val () = begin
    print "finishes"; print_newline ()
  end
(*
  val () = begin
    print "trmv_trsv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end
*)
//
  prval () = pfY_arr := array_v_of_GEVEC_v {a} (pfY_gev)
  val () = array_ptr_free (pfY_gc, pfY_arr | pY_arr)
  prval () = pfX_arr := array_v_of_GEVEC_v {a} (pfX_gev)
  val () = array_ptr_free (pfX_gc, pfX_arr | pX_arr)
  prval () = pfA_gmat := fpfA_gmat (pfA_trmat)
  prval () = pfA_fmat := fpfA_fmat (pfA_gmat)
  prval (pf2_MM, pfA_arr) = array_v_of_fmatrix_v (pfA_fmat)
  prval () = mul_isfun (pf_MM, pf2_MM)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [trmv_trsv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} tbmv_tbsv_test (): void

implement{a,ar} // ar = |a|
tbmv_tbsv_test (): void = () where {
  #define N 100
  #define K 7
//
  val (pf_KN | KN) = (1+K) imul2 N
  prval () = mul_nat_nat_nat (pf_KN)
  val KN_sz = size1_of_int1 KN
  val (pfA_gc, pfA_arr | pA_arr) = array_ptr_alloc<a> (KN_sz)
  prval pfA_fmat = fmatrix_v_of_array_v (pf_KN, pfA_arr)
  prval pf_unit = unit_v
  val () = fmatrix_ptr_initialize_vclo<a>
    {unit_v} (pf_unit | !pA_arr, i2sz (K+1), N, !p_f) where {
    var !p_f = @lam (
        pf: !unit_v
      | A: &(a?) >> a, i: sizeLt (K+1), j: sizeLt N
      ) : void =<clo>
      A := of_double<a> (
        if i < K then 1.0 / double_of_int K else 1.0
      ) // end of [of_double]
  } // end of [val]
  prval unit_v () = pf_unit
  prval (pfA_gmat, fpfA_fmat) = GEMAT_v_of_fmatrix_v (pfA_fmat)
  prval (pfA_tpmat, fpfA_gmat) =
    TBMAT_v_of_GEMAT_v {a} {N,K} (pfA_gmat, UPLOupper, DIAGunit, K)
  (* end of [prval] *)
//
  val (pfX_gc, pfX_arr | pX_arr) = array_ptr_alloc<a> (N)
  val () = array_ptr_initialize_elt<a> (!pX_arr, N, of_int 1)
  prval pfX_gev = GEVEC_v_of_array_v {a} (pfX_arr)
  val incX = 1
  val (pfY_gc, pfY_arr | pY_arr) = array_ptr_alloc<a> (N)
  val () = array_ptr_initialize_elt<a> (!pY_arr, N, of_int 1)
  prval pfY_gev = GEVEC_v_of_array_v (pfY_arr)
  val incY = 1
//
  val () = cblas_tbmv<a> (
    CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , N, K
  , !pA_arr, (1+K)
  , !pX_arr, incX
  )
//
  val () = cblas_tbsv<a> (
    CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , N, K
  , !pA_arr, (1+K)
  , !pX_arr, incX
  )
//
  val () = cblas_axpy<a> (
    N
  , of_int (~1)
  , !pX_arr, incX
  , !pY_arr, incY
  )
  val diff = cblas_asum<a,ar> (N, !pY_arr, incY)
(*
  val () = begin
    print "tbmv_tbsv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end
*)
  val () = begin
    print "tbmv_tbsv_test<"; print_typ<a> (); print ">: starts ... "
  end
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (N * K))))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval () = pfY_arr := array_v_of_GEVEC_v {a} (pfY_gev)
  val () = array_ptr_free (pfY_gc, pfY_arr | pY_arr)
  prval () = pfX_arr := array_v_of_GEVEC_v {a} (pfX_gev)
  val () = array_ptr_free (pfX_gc, pfX_arr | pX_arr)
  prval () = pfA_gmat := fpfA_gmat (pfA_tpmat)
  prval () = pfA_fmat := fpfA_fmat (pfA_gmat)
  prval (pf2_KN, pfA_arr) = array_v_of_fmatrix_v (pfA_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [tbmv_tbsv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} tpmv_tpsv_test (): void

implement{a,ar} // ar = |a|
tpmv_tpsv_test (): void = () where {
  #define M 100
  #define L ((M * (M+1))/2)
//
  prval pf_mml2 = mul_make {M,M+1} ()
  prval () = mul_elim {M,M+1} {2*L} (pf_mml2)
  val L_sz = size1_of_int1 L
  val (pfA_gc, pfA_arr | pA_arr) = array_ptr_alloc<a> (L_sz)
  val () = array_ptr_initialize_elt<a>
    (!pA_arr, L_sz, of_double (1.0 / double_of_int M))
  prval pfA_gvec = GEVEC_v_of_array_v {a} (pfA_arr)
  prval (pfA_tpmat, fpfA_gvec) = TPMAT_v_of_GEVEC_v {a}
    (pf_mml2, pfA_gvec,  ORDERcol, UPLOupper, DIAGunit)
//
  val (pfX_gc, pfX_arr | pX_arr) = array_ptr_alloc<a> (M)
  val () = array_ptr_initialize_elt<a> (!pX_arr, M, of_int 1)
  prval pfX_gev = GEVEC_v_of_array_v {a} (pfX_arr)
  val incX = 1
  val (pfY_gc, pfY_arr | pY_arr) = array_ptr_alloc<a> (M)
  val () = array_ptr_initialize_elt<a> (!pY_arr, M, of_int 1)
  prval pfY_gev = GEVEC_v_of_array_v (pfY_arr)
  val incY = 1
//
  val () = cblas_tpmv<a> (
    CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , M
  , !pA_arr
  , !pX_arr, incX
  )
//
  val () = cblas_tpsv<a> (
    CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , M
  , !pA_arr
  , !pX_arr, incX
  )
//
  val () = cblas_axpy<a> (
    M
  , of_int (~1)
  , !pX_arr, incX
  , !pY_arr, incY
  )
  val diff = cblas_asum<a,ar> (M, !pY_arr, incY)
(*
  val () = begin
    print "tpmv_tpsv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end
*)
  val () = begin
    print "tpmv_tpsv_test<"; print_typ<a> (); print ">: starts ... "
  end
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (M * M))))
  val () = begin
    print "finishes"; print_newline ()
  end
//
  prval () = pfY_arr := array_v_of_GEVEC_v {a} (pfY_gev)
  val () = array_ptr_free (pfY_gc, pfY_arr | pY_arr)
  prval () = pfX_arr := array_v_of_GEVEC_v {a} (pfX_gev)
  val () = array_ptr_free (pfX_gc, pfX_arr | pX_arr)
  prval () = pfA_gvec := fpfA_gvec (pfA_tpmat)
  prval () = pfA_arr := array_v_of_GEVEC_v (pfA_gvec)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [tpmv_tpsv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} symv_test (): void

//
// relate symv to symm
//

implement{a,ar} // ar = |a|
symv_test (): void = () where {
  #define M 100 // needs to be a positive number
  #define K 100 // needs to be a positive number
  #define N 1 // needs to be a positive number
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_sym, fpf1_gem) = SYMAT_v_of_GEMAT_v (pf1_gem, UPLOupper)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_symm<a> (
    SIDEDIM_L
  | CblasColMajor
  , CblasLeft
  , CblasUpper
  , M, N
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , zero (* beta *)
  , !p3_arr, M
  ) // end of [val]
//
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_col (pf2_gem)
  // inc2 = 1
  val inc2 = MATVECINC_get (pf2_inc | ORDERcol, ORDERcol, K)
  val inc2 = sz2i inc2
  prval (pf3_inc, pf3_gev, fpf3_gem) = GEMAT_v_uncons_col (pf3_gem)
  // inc3 = 1
  val inc3 = MATVECINC_get (pf3_inc | ORDERcol, ORDERcol, M)
  val inc3 = sz2i inc3
//
  val () = cblas_symv<a> (
    CblasColMajor
  , CblasUpper
  , M
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, inc2
  , neg1 (* beta *)
  , !p3_arr, inc3
  ) // end of [val]
//
  val diff = cblas_nrm2<ar,a> (M, !p3_arr, inc3)
  val () = begin
    print "symv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = assert (isEpsilon<ar> diff)
//
  prval () = pf1_gem := fpf1_gem (pf1_sym)
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_gem := fpf3_gem (pf3_gev)  
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [symv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} ger_test (): void

//
// relate ger to gemm
//

implement{a,ar} // ar = |a|
ger_test (): void = () where {
  #define M 19 // needs to be a positive number
  #define N 57 // needs to be a positive number
  #define K 01 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pfx_gc, pfx_arr | px_arr) = randgen_arr<a> (MK)
  prval pfx_fmat = fmatrix_v_of_array_v {a} (pf_MK, pfx_arr)
  prval (pfx_gem, fpfx_fmat) = GEMAT_v_of_fmatrix_v (pfx_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pfy_gc, pfy_arr | py_arr) = randgen_arr<a> (KN)
  prval pfy_fmat = fmatrix_v_of_array_v {a} (pf_KN, pfy_arr)
  prval (pfy_gem, fpfy_fmat) = GEMAT_v_of_fmatrix_v (pfy_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_gemm<a> (
    TRANDIM_N
  , TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, N, K
  , pos1 (* alpha *)
  , !px_arr, M
  , !py_arr, K
  , zero (* beta *)
  , !p1_arr, M
  ) // end of [val]
//
  prval (pfx_inc, pfx_gev, fpfx_gem) = GEMAT_v_uncons_col (pfx_gem)
  // incx = 1
  val incx = MATVECINC_get (pfx_inc | ORDERcol, ORDERcol, M)
  val incx = sz2i incx
  prval (pfy_inc, pfy_gev, fpfy_gem) = GEMAT_v_uncons_row (pfy_gem)
  // incy = 1
  val incy = MATVECINC_get (pfy_inc | ORDERrow, ORDERcol, K)
  val incy = sz2i incy
//
 val () = cblas_ger<a> (
   CblasColMajor, M, N, neg1, !px_arr, incx, !py_arr, incy, !p1_arr, M 
 )
//
  prval () = pfx_gem := fpfx_gem (pfx_gev)
  prval () = pfy_gem := fpfy_gem (pfy_gev)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  prval pf1_vec = GEVEC_v_of_array_v {a} (pf1_arr)
  val diff = cblas_nrm2<ar,a> (MN, !p1_arr, 1)
  prval pf1_arr = array_v_of_GEVEC_v {a} (pf1_vec)
  val () = begin
    print "ger_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = assert (isEpsilon<ar> diff)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pfx_fmat := fpfx_fmat (pfx_gem)
  prval (pfx_MK, pfx_arr) = array_v_of_fmatrix_v (pfx_fmat)
  prval () = mul_isfun (pf_MK, pfx_MK)
  val () = array_ptr_free (pfx_gc, pfx_arr | px_arr)
//
  prval () = pfy_fmat := fpfy_fmat (pfy_gem)
  prval (pfy_KN, pfy_arr) = array_v_of_fmatrix_v (pfy_fmat)
  prval () = mul_isfun (pf_KN, pfy_KN)
  val () = array_ptr_free (pfy_gc, pfy_arr | py_arr)
} // end of [ger_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} geru_test (): void

//
// relate geru to gemm
//

implement{a,ar} // ar = |a|
geru_test (): void = () where {
  #define M 19 // needs to be a positive number
  #define N 57 // needs to be a positive number
  #define K 01 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pfx_gc, pfx_arr | px_arr) = randgen_arr<a> (MK)
  prval pfx_fmat = fmatrix_v_of_array_v {a} (pf_MK, pfx_arr)
  prval (pfx_gem, fpfx_fmat) = GEMAT_v_of_fmatrix_v (pfx_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pfy_gc, pfy_arr | py_arr) = randgen_arr<a> (KN)
  prval pfy_fmat = fmatrix_v_of_array_v {a} (pf_KN, pfy_arr)
  prval (pfy_gem, fpfy_fmat) = GEMAT_v_of_fmatrix_v (pfy_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_gemm<a> (
    TRANDIM_N
  , TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, N, K
  , pos1 (* alpha *)
  , !px_arr, M
  , !py_arr, K
  , zero (* beta *)
  , !p1_arr, M
  ) // end of [val]
//
  prval (pfx_inc, pfx_gev, fpfx_gem) = GEMAT_v_uncons_col (pfx_gem)
  // incx = 1
  val incx = MATVECINC_get (pfx_inc | ORDERcol, ORDERcol, M)
  val incx = sz2i incx
  prval (pfy_inc, pfy_gev, fpfy_gem) = GEMAT_v_uncons_row (pfy_gem)
  // incy = 1
  val incy = MATVECINC_get (pfy_inc | ORDERrow, ORDERcol, K)
  val incy = sz2i incy
//
 val () = cblas_geru<a> (
   CblasColMajor, M, N, neg1, !px_arr, incx, !py_arr, incy, !p1_arr, M 
 )
//
  prval () = pfx_gem := fpfx_gem (pfx_gev)
  prval () = pfy_gem := fpfy_gem (pfy_gev)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  prval pf1_vec = GEVEC_v_of_array_v {a} (pf1_arr)
  val diff = cblas_nrm2<ar,a> (MN, !p1_arr, 1)
  prval pf1_arr = array_v_of_GEVEC_v {a} (pf1_vec)
  val () = begin
    print "geru_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = assert (isEpsilon<ar> diff)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pfx_fmat := fpfx_fmat (pfx_gem)
  prval (pfx_MK, pfx_arr) = array_v_of_fmatrix_v (pfx_fmat)
  prval () = mul_isfun (pf_MK, pfx_MK)
  val () = array_ptr_free (pfx_gc, pfx_arr | px_arr)
//
  prval () = pfy_fmat := fpfy_fmat (pfy_gem)
  prval (pfy_KN, pfy_arr) = array_v_of_fmatrix_v (pfy_fmat)
  prval () = mul_isfun (pf_KN, pfy_KN)
  val () = array_ptr_free (pfy_gc, pfy_arr | py_arr)
} // end of [geru_test]

(* ****** ****** *)

extern fun{a:t@ype} conj (x: a):<> a

implement conj<float> (x) = x
implement conj<double> (x) = x
implement conj<ccmplx> (x) = $C.conj_ccmplx (x)
implement conj<zcmplx> (x) = $C.conj_zcmplx (x)

extern fun{a,ar:t@ype} gerc_test (): void

//
// relate gerc to gemm
//

implement{a,ar} // ar = |a|
gerc_test (): void = () where {
  #define M 19 // needs to be a positive number
  #define N 57 // needs to be a positive number
  #define K 01 // needs to be a positive number
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MN)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pfx_gc, pfx_arr | px_arr) = randgen_arr<a> (MK)
  prval pfx_fmat = fmatrix_v_of_array_v {a} (pf_MK, pfx_arr)
  prval (pfx_gem, fpfx_fmat) = GEMAT_v_of_fmatrix_v (pfx_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pfy_gc, pfy_arr | py_arr) = randgen_arr<a> (KN)
  prval pfy_fmat = fmatrix_v_of_array_v {a} (pf_KN, pfy_arr)
  prval (pfy_gem, fpfy_fmat) = GEMAT_v_of_fmatrix_v (pfy_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_gemm<a> (
    TRANDIM_N
  , TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, N, K
  , pos1 (* alpha *)
  , !px_arr, M
  , !py_arr, K
  , zero (* beta *)
  , !p1_arr, M
  ) // end of [val]
//
  prval (pfx_inc, pfx_gev, fpfx_gem) = GEMAT_v_uncons_col (pfx_gem)
  // incx = 1
  val incx = MATVECINC_get (pfx_inc | ORDERcol, ORDERcol, M)
  val incx = sz2i incx
  prval (pfy_inc, pfy_gev, fpfy_gem) = GEMAT_v_uncons_row (pfy_gem)
  // incy = 1
  val incy = MATVECINC_get (pfy_inc | ORDERrow, ORDERcol, K)
  val incy_i = sz2i incy
//
  val () = loop (pfy_gev | N, py_arr, incy) where {
    fun loop {n:nat} {d:inc} {l:addr} .<n>. (
        pf: !GEVEC_v (a, n, d, l) | n: size_t n, p: ptr l, inc: size_t d
      ) : void =
      if n > 0 then let
        prval pf_mul = mul_istot {d, sizeof a} ()
        prval (pf_at, pf1) = GEVEC_v_uncons {a} (pf_mul, pf)
        val () = !p := conj<a> (!p)
        prval () = pf := GEVEC_v_cons {a} (pf_mul, pf_at, pf1)
        val (pf1, pf2, fpf | p1) = GEVEC_ptr_split<a> (pf | p, inc, 1)
        val () = loop (pf2 | n-1, p1, inc)
        prval () = pf := fpf (pf1, pf2)
      in
        // nothing
      end (* end of [if] *)
    // end of [loop]
  } // end of [val]
//
// HX-2010-07-15: if removed, a gcc optimization bug occurs!!!
//
  val incy_i = sz2i incy
  val () = cblas_gerc<a> (
    CblasColMajor, M, N, neg1, !px_arr, incx, !py_arr, incy_i, !p1_arr, M 
  ) // end of [cblas_gerc]
//
  prval () = pfx_gem := fpfx_gem (pfx_gev)
  prval () = pfy_gem := fpfy_gem (pfy_gev)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MN, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MN, pf1_MN)
  prval pf1_vec = GEVEC_v_of_array_v {a} (pf1_arr)
  val diff = cblas_nrm2<ar,a> (MN, !p1_arr, 1)
  prval pf1_arr = array_v_of_GEVEC_v {a} (pf1_vec)
  val () = begin
    print "gerc_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = assert (isEpsilon<ar> diff)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pfx_fmat := fpfx_fmat (pfx_gem)
  prval (pfx_MK, pfx_arr) = array_v_of_fmatrix_v (pfx_fmat)
  prval () = mul_isfun (pf_MK, pfx_MK)
  val () = array_ptr_free (pfx_gc, pfx_arr | px_arr)
//
  prval () = pfy_fmat := fpfy_fmat (pfy_gem)
  prval (pfy_KN, pfy_arr) = array_v_of_fmatrix_v (pfy_fmat)
  prval () = mul_isfun (pf_KN, pfy_KN)
  val () = array_ptr_free (pfy_gc, pfy_arr | py_arr)
} // end of [gerc_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} hemv_test (): void

//
// relate hemv to hemm
//

implement{a,ar} // ar = |a|
hemv_test (): void = () where {
  #define M 100 // needs to be a positive number
  #define K 100 // needs to be a positive number
  #define N 1 // needs to be a positive number
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
  prval (pf1_hem, fpf1_gem) = HEMAT_v_of_GEMAT_v (pf1_gem, UPLOupper)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_hemm<a> (
    SIDEDIM_L
  | CblasColMajor
  , CblasLeft
  , CblasUpper
  , M, N
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , zero (* beta *)
  , !p3_arr, M
  ) // end of [val]
//
  prval (pf2_inc, pf2_gev, fpf2_gem) = GEMAT_v_uncons_col (pf2_gem)
  // inc2 = 1
  val inc2 = MATVECINC_get (pf2_inc | ORDERcol, ORDERcol, K)
  val inc2 = sz2i inc2
  prval (pf3_inc, pf3_gev, fpf3_gem) = GEMAT_v_uncons_col (pf3_gem)
  // inc3 = 1
  val inc3 = MATVECINC_get (pf3_inc | ORDERcol, ORDERcol, M)
  val inc3 = sz2i inc3
//
  val () = cblas_hemv<a> (
    CblasColMajor
  , CblasUpper
  , M
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, inc2
  , neg1 (* beta *)
  , !p3_arr, inc3
  ) // end of [val]
//
  val diff = cblas_nrm2<ar,a> (M, !p3_arr, inc3)
  val () = begin
    print "hemv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = assert (isEpsilon<ar> diff)
//
  prval () = pf1_gem := fpf1_gem (pf1_hem)
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_gem := fpf2_gem (pf2_gev)  
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_gem := fpf3_gem (pf3_gev)  
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [hemv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} hbmv_test (): void

extern prfun HBMAT_v_of_fmatrix_v
  {a:viewt@ype} {k,m:nat} {l:addr} {ul : uplo} (
    pf : fmatrix_v (a, k, m, l), ul: UPLO (ul)
  ) :<prf> (
    HBMAT_v (a, m, col, ul, k-1, k, l)
  , HBMAT_v (a, m, col, ul, k-1, k, l) -<prf> fmatrix_v (a, k, m, l)
  )
// end of [HBMAT_v_of_fmatrix_v]

implement{a,ar} // ar = |a|
hbmv_test (): void = () where {
  #define M 100
  #define K 5
  #define R (1+K)
//
  val R_sz = i2sz R
  val M_sz = i2sz M
  val (pf_RM | RM_sz) = mul2_size1_size1 (R_sz, M_sz)
  prval () = mul_nat_nat_nat (pf_RM)
  val (pfB_gc, pfB_arr | pB_arr) = array_ptr_alloc<a> (RM_sz)
  prval (pfB_fmat) = fmatrix_v_of_array_v {a?} {R,M} (pf_RM, pfB_arr)
  prval pf_unit = unit_v
  val () = fmatrix_ptr_initialize_vclo<a>
    {unit_v} (pf_unit | !pB_arr, R_sz, M_sz, !p_f) where {
    val kpk1 = of_int<a> (2*K)
    var !p_f = @lam (
        pf : !unit_v | x : &(a?) >> a, k : sizeLt R, j : sizeLt M
      ) : void =<clo>
      x := // k = diagonal (K = main/upper), j = column
        (if k < K then of_int<a> (~1) else kpk1)
  }
  prval unit_v () = pf_unit
  prval (pfB_hbmat, fpfB_fmat) =
    HBMAT_v_of_fmatrix_v {a} (pfB_fmat, UPLOupper)
//
  val (pfX_gc, pfX_arr | pX_arr) = array_ptr_alloc<a> (M)
  val () = array_ptr_initialize_elt<a> (!pX_arr, M, of_int 1)
  prval pfX_gev = GEVEC_v_of_array_v {a} (pfX_arr)
  val incX = 1
  val (pfY_gc, pfY_arr | pY_arr) = array_ptr_alloc<a> (M)
//
  val () = array_ptr_initialize_fun<a> (!pY_arr, M, f) where {
    fn f (
      i : sizeLt M
    , y : &(a?) >> a
    ) :<> void = let
      val i = sz2i i
      val y0 = (
        if i < K then
          K - i
        else if i < M-K then
          0
        else // i < M
          K - (M - i - 1)
        // end of [if]
      ) : int
    in
      y := of_int<a> y0
    end // end of [f]
  } // end of [val]
//
  prval pfY_gev = GEVEC_v_of_array_v {a} (pfY_arr)
  val incY = 1
//
  val () = cblas_hbmv<a> (
    CblasColMajor
  , CblasUpper
  , M, K
  , of_int 1 (* alpha *)
  , !pB_arr, R
  , !pX_arr, incX
  , of_int (~1) (* beta *)
  , !pY_arr, incY
  )
  val diff = cblas_asum<a,ar> (M, !pY_arr, incY)
(*
  val () = begin
    print "hbmv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end
*)
  val () = begin
    print "hbmv_test<"; print_typ<a> (); print ">: starts ... ";
  end
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (M*K))))
  val () = begin
    print "finishes"; print_newline ()
  end
//
  prval () = pfX_arr := array_v_of_GEVEC_v (pfX_gev)
  val () = array_ptr_free (pfX_gc, pfX_arr | pX_arr)
  prval () = pfY_arr := array_v_of_GEVEC_v (pfY_gev)
  val () = array_ptr_free (pfY_gc, pfY_arr | pY_arr)
  prval () = pfB_fmat := fpfB_fmat (pfB_hbmat)
  prval (pf2_RM, pfB_arr) = array_v_of_fmatrix_v (pfB_fmat)
  prval () = mul_isfun (pf_RM, pf2_RM)
  val () = array_ptr_free (pfB_gc, pfB_arr | pB_arr)
} // end of [hbmv_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} hpmv_test (): void

//
// [HPMAT_v_of_array_v] can be proven by using [HPMAT_v_of_GEVEC_v]
//
extern prfun HPMAT_v_of_array_v
  {a:viewt@ype} {n,m:nat} {l:addr} {ord:order} {ul:uplo} (
    pf_mmn: MUL (m, m+1, 2*n)
  , pf: array_v (a, n, l), m: int m, ord: ORDER ord, ul: UPLO ul
  ) :<prf> (
    HPMAT_v (a, m, ord, ul, l)
  , HPMAT_v (a, m, ord, ul, l) -<prf> array_v (a, n, l)
  )
// end of [HPMAT_v_of_array_v]

implement{a,ar} // ar = |a|
hpmv_test (): void = () where {
  #define M 100
  #define L ((M * (M+1)) / 2)
//
  prval pf_mml2 = mul_make {M,M+1} ()
  prval () = mul_elim {M,M+1} {2*L} (pf_mml2)
  val L_sz = size1_of_int1 L
  val (pfA_gc, pfA_arr | pA_arr) = array_ptr_alloc<a> (L_sz)
//
  val () = array_ptr_initialize_fun<a> (!pA_arr, L_sz, f) where {
    fn f (i : sizeLt L, A : &(a?) >> a) :<> void = let
      val i = sz2i i
      val c = int_of_double (($M.sqrt (1.0 + 8.0 * (double_of_int i)) - 1.0) / 2.0)
      val r = i - (c * (c + 1)) / 2
    in
      A := of_int<a> (c - r)
    end // end of [f]
  } // end of [val]
//
  prval (pfA_hpmat, fpfA_arr) = HPMAT_v_of_array_v (pf_mml2,
    pfA_arr, M, ORDERcol, UPLOupper)
//
  val (pfX_gc, pfX_arr | pX_arr) = array_ptr_alloc<a> (M)
  val () = array_ptr_initialize_elt<a> (!pX_arr, M, of_int 1)
  prval pfX_gev = GEVEC_v_of_array_v {a} (pfX_arr)
  val incX = 1
  val (pfY_gc, pfY_arr | pY_arr) = array_ptr_alloc<a> (M)
//
  val () = array_ptr_initialize_fun<a> (!pY_arr, M, f) where {
    fn f (
       i : sizeLt M, y : &(a?) >> a
    ) :<> void =
      y := of_int<a> let
        val i = sz2i i in (i * (i + 1) + (M - i - 1) * (M - i)) / 2
      end // end of [let]
  } // end of [val]
//
  prval pfY_gev = GEVEC_v_of_array_v {a} (pfY_arr)
  val incY = 1
//
  val () = cblas_hpmv<a> (
    CblasColMajor
  , CblasUpper
  , M
  , of_int 1 (* alpha *)
  , !pA_arr
  , !pX_arr, incX
  , of_int (~1) (* beta *)
  , !pY_arr, incY
  )
  val diff = cblas_asum<a,ar> (M, !pY_arr, incY)
  val () = begin
    print "hpmv_test<"; print_typ<a> (); print ">: starts ... "
  end
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int M)))
  val () = begin
    print "finishes"; print_newline ()
  end
(*
  val () = begin
    print "hpmv_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end
*)
//
  prval () = pfX_arr := array_v_of_GEVEC_v (pfX_gev)
  val () = array_ptr_free (pfX_gc, pfX_arr | pX_arr)
  prval () = pfY_arr := array_v_of_GEVEC_v (pfY_gev)
  val () = array_ptr_free (pfY_gc, pfY_arr | pY_arr)
  prval () = pfA_arr := fpfA_arr (pfA_hpmat)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [hpmv_test]

(* ****** ****** *)

// Sum of 2-norms of columns
fun{a,ar:t@ype} GEMAT_nrmF
  {m,n:nat} {ld:pos} {ord:order} (
    Order: ORDER ord
  , A: &GEMAT (a, m, n, ord, ld), m: size_t m, n: size_t n, ld: size_t ld
  ): ar = let
  val m_i = sz2i m
  fun loop {k:nat} {l0:addr} (
    pf_gmat: !GEMAT_v (a, m, k, ord, ld, l0)
  | pA: ptr l0, acc: ar, k: size_t k):<cloref1> ar =
    if k = 0 then
      acc
    else let
      prval (pf_inc, pf_gev, fpf_gmat) =
        GEMAT_v_uncons_col {a} {ord} {m,k} (pf_gmat)
      val inc = MATVECINC_get (pf_inc | ORDERcol, Order, ld)
      val inc = sz2i inc
      val cnrm = cblas_nrm2<ar,a> (m_i, !pA, inc)
      prval () = pf_gmat := fpf_gmat (pf_gev)
      val (pf1_gmat, pf2_gmat, fpf_gmat | pA1, pA2) =
        GEMAT_ptr_split1x2<a> (pf_gmat | Order, pA, ld, 1)
      val acc = loop (pf2_gmat | pA2, $NUM.add<ar> (acc, cnrm), k-1)
      prval () = pf_gmat := fpf_gmat (pf1_gmat, pf2_gmat)
    in
      acc
    end // end of [loop]
in
  loop (view@ A | &A, of_int<ar> 0, n)
end // end of [GEMAT_nrmF]

// Y <- alpha X + Y
fun{a:t@ype} GEMAT_axpy
  {m,n:nat} {ldx,ldy:pos} {ord:order} (
    Order: ORDER ord
  , m: size_t m, n: size_t n
  , alpha: a
  , X: &GEMAT (a, m, n, ord, ldx), ldx: size_t ldx
  , Y: &GEMAT (a, m, n, ord, ldy), ldy: size_t ldy
  ): void = let
  val m_i = sz2i m
  fun loop {lx,ly:addr} {k:nat} (
      pfX_gmat: !GEMAT_v (a, m, k, ord, ldx, lx)
    , pfY_gmat: !GEMAT_v (a, m, k, ord, ldy, ly)
    | pX: ptr lx, pY: ptr ly, k: size_t k):<cloref1> void =
    if k > 0 then let
      prval (pfX_inc, pfX_gvec, fpfX_gmat) = GEMAT_v_uncons_col (pfX_gmat)
      val incX = MATVECINC_get (pfX_inc | ORDERcol, Order, ldx)
      val incX = sz2i incX
      prval (pfY_inc, pfY_gvec, fpfY_gmat) = GEMAT_v_uncons_col (pfY_gmat)
      val incY = MATVECINC_get (pfY_inc | ORDERcol, Order, ldy)
      val incY = sz2i incY
      val () = cblas_axpy<a> (m_i, alpha, !pX, incX, !pY, incY)
      prval () = pfX_gmat := fpfX_gmat (pfX_gvec)
      prval () = pfY_gmat := fpfY_gmat (pfY_gvec)
      val (pfX1_gmat, pfX2_gmat, fpfX_gmat | pX1, pX2) =
        GEMAT_ptr_split1x2<a> (pfX_gmat | Order, pX, ldx, 1)
      val (pfY1_gmat, pfY2_gmat, fpfY_gmat | pY1, pY2) =
        GEMAT_ptr_split1x2<a> (pfY_gmat | Order, pY, ldy, 1)
      val () = loop (pfX2_gmat, pfY2_gmat | pX2, pY2, k-1)
      prval () = pfX_gmat := fpfX_gmat (pfX1_gmat, pfX2_gmat)
      prval () = pfY_gmat := fpfY_gmat (pfY1_gmat, pfY2_gmat)
    in
      // nothing
    end // end of [if]
in
  loop (view@ X, view@ Y | &X, &Y, n)
end // end of [GEMAT_axpy]

(* ****** ****** *)

extern fun{a,ar:t@ype} her_her2_test (): void

(*
  [ c s] [-c^2    ] [ c s]^T = 1/2 [-2c2 s2] = ([-c2][1 0] + [1][-c2 s2]) 1/2
  [-s c] [     s^2] [-s c]         [  s2   ]   ([ s2]        [0]        )
where
  c = cos theta
  s = sin theta
  c2 = cos 2theta
  s2 = sin 2theta
*)

implement{a,ar} // ar = |a|
her_her2_test (): void = () where {
  #define M 100
  val (pfA_gc, pf_MM, pfA_fmat | pA_arr) = randgen_fmat<a> (M, M)
  val () = loop_diag (!pA_arr, 0) where { // real diag for HEMAT
    fun loop_diag
      {i:nat | i <= M} .<M-i>.
      (A: &fmatrix(a, M, M), i: size_t i)
      :<cloref1> void =
      if i < M then let
        val x = fmatrix_ptr_get_elt_at (A, M, i, i)
        val x_re = x \$NUM.add (conj<a> x)
        val () = fmatrix_ptr_set_elt_at (A, M, i, i, x_re)
      in
        loop_diag (A, i+1)
      end // end of [if]
    // end of [loop_diag]
  } // end of [val]
  val (pfC_gc, pfC_MM, pfC_fmat | pC_arr) = fmatrix_ptr_alloc<a> (M, M)
  prval () = mul_isfun (pf_MM, pfC_MM)
  val () = fmatrix_ptr_copy<a> (!pA_arr, !pC_arr, M, M)
  prval (pfC_gmat, fpfC_fmat) = GEMAT_v_of_fmatrix_v (pfC_fmat)
  prval (pfA_gmat, fpfA_fmat) = GEMAT_v_of_fmatrix_v (pfA_fmat)
  prval (pfA_hmat, fpfA_gmat) = HEMAT_v_of_GEMAT_v (pfA_gmat, UPLOupper)
//
  val [_:int] [lb:addr] (pfB_gc, pfB_M2, pfB_fmat | pB_arr) =
    randgen_fmat<a> (M, 2)
  // end of [val]
  prval (pfB_gmat, fpfB_fmat) = GEMAT_v_of_fmatrix_v (pfB_fmat)
  prval () = mul_nat_nat_nat (pfB_M2)
//
  val (pfBV_gc, pfBV_M2, pfBV_fmat | pBV_arr) = randgen_fmat<a> (M, 2)
  prval (pfBV_gmat, fpfBV_fmat) = GEMAT_v_of_fmatrix_v (pfBV_fmat)
  prval () = mul_isfun (pfB_M2, pfBV_M2)
//
  #define theta (M_PI / 8.0)
  val cr = cos theta
  val sr = sin theta
  var c = of_double<a> cr
  val s = of_double<a> sr
  val s_m = $NUM.neg<a> s
  val l1 = of_double<ar> (~(cr * cr))
  val l2 = of_double<ar> (sr * sr)
  val [_:int] [lv:addr]
    (pfV_gc, pfV_22, pfV_fmat | pV_arr) = fmatrix_ptr_alloc<a> (2, 2)
  // end of [val]
  val () = fmatrix_ptr_initialize_elt<a> (!pV_arr, 2, 2, c)
//
  macdef fmset (A, i, j, c) = 
    fmatrix_ptr_set_elt_at<a> {2,2} (,(A), 2, ,(i), ,(j), ,(c))
  (* end of [macdef] *)
//
  val () = fmset (!pV_arr, 0, 0, c)
  val () = fmset (!pV_arr, 1, 0, s_m)
  val () = fmset (!pV_arr, 0, 1, s)
  val () = fmset (!pV_arr, 1, 1, c)
  prval (pfV_gmat, fpfV_fmat) = GEMAT_v_of_fmatrix_v (pfV_fmat)
//
  viewdef vB = GEMAT_v (a, M, 2, col, M, lb)
  viewdef vV = GEMAT_v (a, 2, 2, col, 2, lv)
  viewdef vBV = @(vB, vV)
  prval pf_BV_gmat = @(pfB_gmat, pfV_gmat)
  val () = cblas_gemm__main<a> {vBV} ( // BV <- B * V
    pf_BV_gmat
  , TRANDIM_N
  , vsubr_tup_2_0 {vB,vV} ()
  , TRANDIM_N
  , vsubr_tup_2_1 {vB,vV} ()
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, 2, 2
  , of_int<a> 1
  , pB_arr, M
  , pV_arr, 2
  , of_int<a> 0
  , !pBV_arr, M
  )
  prval (pfB_gmat, pfV_gmat) = pf_BV_gmat
//
  val (pfBV1_gmat, pfBV2_gmat, fpfBV_gmat | pBV1_arr, pBV2_arr) =
    GEMAT_ptr_split1x2<a> (pfBV_gmat | ORDERcol, pBV_arr, M, 1)
  // end of [val]
  prval (pfBV1_inc, pfBV1_gev, fpfBV1_gmat) = GEVEC_v_of_GEMAT_v_col (pfBV1_gmat)
  // end of [prval]
  val incBV1 = MATVECINC_get (pfBV1_inc | ORDERcol, ORDERcol, M)
  val incBV1 = sz2i incBV1
  prval (pfBV2_inc, pfBV2_gev, fpfBV2_gmat) = GEVEC_v_of_GEMAT_v_col (pfBV2_gmat)
  // end of [prval]
  val incBV2 = MATVECINC_get (pfBV2_inc | ORDERcol, ORDERcol, M)
  val incBV2 = sz2i incBV2
// A <- A + l1 BV1 * BV1' + l2 BV2 * BV2 '
  val () = cblas_her<ar,a> (
    CblasColMajor
  , CblasUpper
  , M
  , l1
  , !pBV1_arr, incBV1
  , !pA_arr, M
  )
  val () = cblas_her<ar,a> (
    CblasColMajor
  , CblasUpper
  , M
  , l2
  , !pBV2_arr, incBV2
  , !pA_arr, M
  )
//
  val (pfB1_gmat, pfB2_gmat, fpfB_gmat | pB1_arr, pB2_arr) =
    GEMAT_ptr_split1x2<a> (pfB_gmat | ORDERcol, pB_arr, M, 1)
  prval (pfB1_inc, pfB1_gev, fpfB1_gmat) = GEVEC_v_of_GEMAT_v_col (pfB1_gmat)
  val incB1 = MATVECINC_get (pfB1_inc | ORDERcol, ORDERcol, M)
  val incB1 = sz2i incB1
  prval (pfB2_inc, pfB2_gev, fpfB2_gmat) = GEVEC_v_of_GEMAT_v_col (pfB2_gmat)
  val incB2 = MATVECINC_get (pfB2_inc | ORDERcol, ORDERcol, M)
  val incB2 = sz2i incB2
  val c2r = cos (2.0 * theta)
  val s2r = sin (2.0 * theta)
//
  val () = cblas_scal<a, ar> ( // B2 <- sin(2 theta) B2
    M, of_double<ar> s2r, !pB2_arr, incB2
  )
  val () = cblas_axpy<a> ( // B2 <- - cos(2 theta) B1 + [sin(2 theta) B2] 
    M
  , of_double<a> (~c2r)
  , !pB1_arr, incB1
  , !pB2_arr, incB2
  )
//
  val () = cblas_her2<a> ( // A <- A + 0.5 (B2 B1' + B1 * B2')
    CblasColMajor
  , CblasUpper
  , M
  , of_double<a> (~0.5)
  , !pB2_arr, incB2
  , !pB1_arr, incB1
  , !pA_arr, M
  )
//
  prval pfA_gmat = fpfA_gmat (pfA_hmat)
  val () = GEMAT_axpy<a> (
    ORDERcol, M, M, of_int<a> (~1), !pA_arr, M, !pC_arr, M)
  val diff = GEMAT_nrmF<a,ar> (ORDERcol, !pC_arr, M, M, M)
(*
  val () = begin
    print "her_her2_test<"; print_typ<ar> (); print ","; print_typ<a> ();
    print ">: diff = "; print_elt<ar> diff; print_newline ()
  end // end of [val]
*)
  val () = begin
    print "her_her2_test<"; print_typ<a> (); print ","; print_typ<ar> ();
    print ">: starts ... ";
  end // end of [val]
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (M * M))))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval pfBV1_gmat = fpfBV1_gmat (pfBV1_gev)
  prval pfBV2_gmat = fpfBV2_gmat (pfBV2_gev)
  prval pfBV_gmat = fpfBV_gmat (pfBV1_gmat, pfBV2_gmat)
  prval pfBV_fmat = fpfBV_fmat (pfBV_gmat)
  val () = fmatrix_ptr_free (pfBV_gc, pfBV_M2, pfBV_fmat| pBV_arr)
//
  prval pfV_fmat = fpfV_fmat (pfV_gmat)
  val () = fmatrix_ptr_free (pfV_gc, pfV_22, pfV_fmat | pV_arr)
//
  prval pfC_fmat = fpfC_fmat (pfC_gmat)
  val () = fmatrix_ptr_free (pfC_gc, pfC_MM, pfC_fmat| pC_arr)
//
  prval pfB1_gmat = fpfB1_gmat (pfB1_gev)
  prval pfB2_gmat = fpfB2_gmat (pfB2_gev)
  prval pfB_gmat = fpfB_gmat (pfB1_gmat, pfB2_gmat)
  prval pfB_fmat = fpfB_fmat (pfB_gmat)
  val () = fmatrix_ptr_free (pfB_gc, pfB_M2, pfB_fmat | pB_arr)
//
  prval pfA_fmat = fpfA_fmat (pfA_gmat)
  val () = fmatrix_ptr_free (pfA_gc, pf_MM, pfA_fmat | pA_arr)
} // end of [her_her2_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} hpr_hpr2_test (): void

implement{a,ar} // ar = |a|
hpr_hpr2_test (): void = () where {
  #define M 100
  #define L ((M * (M+1))/2)
  val (pfA_gc, pfA_arr | pA_arr) = randgen_arr<a> (L)
  val () = loop_diag (!pA_arr, 0, 2) where { // real diag for HPMAT
    fun loop_diag {i,k:nat | i < L && k <= M+1}
      (A: &(@[a][L]), i: int i, k: int k):<cloptr1> void = let
      val () = A[i] := $NUM.add<a> (A[i], conj A[i])
    in
      if i < L-M && k <= M then loop_diag (A, i+k, k+1)
    end // end of [loop_diag]
  }
  val L_sz = size1_of_int1 L
  val (pfC_gc, pfC_arr | pC_arr) = array_ptr_alloc<a> (L_sz)
  val () = array_ptr_copy_tsz (!pA_arr, !pC_arr, L_sz, sizeof<a>)
  prval pf_mml2 = mul_make {M,M+1} ()
  prval () = mul_elim {M,M+1} {2*L} (pf_mml2)
  prval pfC_gev = GEVEC_v_of_array_v (pfC_arr)
  prval pfA_gev = GEVEC_v_of_array_v (pfA_arr)
  prval (pfA_hpmat, fpfA_gev) =
    HPMAT_v_of_GEVEC_v (pf_mml2, pfA_gev, ORDERcol, UPLOupper)
  (* end of [prval] *)
//
  val [_:int] [lb:addr] (pfB_gc, pfB_M2, pfB_fmat | pB_arr) =
    randgen_fmat<a> (M, 2)
  // end of [val]
  prval (pfB_gmat, fpfB_fmat) = GEMAT_v_of_fmatrix_v (pfB_fmat)
  prval () = mul_nat_nat_nat (pfB_M2)
//
  val (pfBV_gc, pfBV_M2, pfBV_fmat | pBV_arr) =
    randgen_fmat<a> (M, 2)
  // end of [val]
  prval (pfBV_gmat, fpfBV_fmat) = GEMAT_v_of_fmatrix_v (pfBV_fmat)
  prval () = mul_isfun (pfB_M2, pfBV_M2)
//
  #define theta (M_PI / 8.0)
  val cr = cos theta
  val sr = sin theta
  var c = of_double<a> cr
  val s = of_double<a> sr
  val s_m = $NUM.neg<a> s
  val l1 = of_double<ar> (~(cr * cr))
  val l2 = of_double<ar> (sr * sr)
  val [_:int] [lv:addr]
    (pfV_gc, pfV_22, pfV_fmat | pV_arr) = fmatrix_ptr_alloc<a> (2, 2)
  val () = fmatrix_ptr_initialize_elt<a> (!pV_arr, 2, 2, c)
//
  macdef fmset (A, i, j, c) = 
    fmatrix_ptr_set_elt_at<a> {2,2} (,(A), 2, ,(i), ,(j), ,(c))
//
  val () = fmset (!pV_arr, 0, 0, c)
  val () = fmset (!pV_arr, 1, 0, s_m)
  val () = fmset (!pV_arr, 0, 1, s)
  val () = fmset (!pV_arr, 1, 1, c)
  prval (pfV_gmat, fpfV_fmat) = GEMAT_v_of_fmatrix_v (pfV_fmat)
//
  viewdef vB = GEMAT_v (a, M, 2, col, M, lb)
  viewdef vV = GEMAT_v (a, 2, 2, col, 2, lv)
  viewdef vBV = @(vB, vV)
  prval pf_BV_gmat = @(pfB_gmat, pfV_gmat)
  val () = cblas_gemm__main<a> {vBV} ( // BV <- B * V
    pf_BV_gmat
  , TRANDIM_N
  , vsubr_tup_2_0 {vB,vV} ()
  , TRANDIM_N
  , vsubr_tup_2_1 {vB,vV} ()
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, 2, 2
  , of_int<a> 1
  , pB_arr, M
  , pV_arr, 2
  , of_int<a> 0
  , !pBV_arr, M
  )
  prval (pfB_gmat, pfV_gmat) = pf_BV_gmat
//
  val (pfBV1_gmat, pfBV2_gmat, fpfBV_gmat | pBV1_arr, pBV2_arr) =
    GEMAT_ptr_split1x2<a> (pfBV_gmat | ORDERcol, pBV_arr, M, 1)
  prval (pfBV1_inc, pfBV1_gev, fpfBV1_gmat) = GEVEC_v_of_GEMAT_v_col (pfBV1_gmat)
  val incBV1 = MATVECINC_get (pfBV1_inc | ORDERcol, ORDERcol, M)
  val incBV1 = sz2i incBV1
  prval (pfBV2_inc, pfBV2_gev, fpfBV2_gmat) = GEVEC_v_of_GEMAT_v_col (pfBV2_gmat)
  val incBV2 = MATVECINC_get (pfBV2_inc | ORDERcol, ORDERcol, M)
  val incBV2 = sz2i incBV2
// A <- A + l1 BV1 * BV1' + l2 BV2 * BV2 '
  val () = cblas_hpr<ar,a> (
    CblasColMajor
  , CblasUpper
  , M
  , l1
  , !pBV1_arr, incBV1
  , !pA_arr
  )
  val () = cblas_hpr<ar,a> (
    CblasColMajor
  , CblasUpper
  , M
  , l2
  , !pBV2_arr, incBV2
  , !pA_arr
  )
//
  val (pfB1_gmat, pfB2_gmat, fpfB_gmat | pB1_arr, pB2_arr) =
    GEMAT_ptr_split1x2<a> (pfB_gmat | ORDERcol, pB_arr, M, 1)
  prval (pfB1_inc, pfB1_gev, fpfB1_gmat) = GEVEC_v_of_GEMAT_v_col (pfB1_gmat)
  val incB1 = MATVECINC_get (pfB1_inc | ORDERcol, ORDERcol, M)
  val incB1 = sz2i incB1
  prval (pfB2_inc, pfB2_gev, fpfB2_gmat) = GEVEC_v_of_GEMAT_v_col (pfB2_gmat)
  val incB2 = MATVECINC_get (pfB2_inc | ORDERcol, ORDERcol, M)
  val incB2 = sz2i incB2
  val c2r = cos (2.0 * theta)
  val s2r = sin (2.0 * theta)
//
  val () = cblas_scal<a, ar> ( // B2 <- sin(2 theta) B2
    M, of_double<ar> s2r, !pB2_arr, incB2
  )
  val () = cblas_axpy<a> ( // B2 <- - cos(2 theta) B1 + [sin(2 theta) B2] 
    M
  , of_double<a> (~c2r)
  , !pB1_arr, incB1
  , !pB2_arr, incB2
  )
//
  val () = cblas_hpr2<a> ( // A <- A + 0.5 (B2 B1' + B1 * B2')
    CblasColMajor
  , CblasUpper
  , M
  , of_double<a> (~0.5)
  , !pB2_arr, incB2
  , !pB1_arr, incB1
  , !pA_arr
  )
//
  prval pfA_gev = fpfA_gev (pfA_hpmat)
  val () = cblas_axpy<a> (
    L, of_int<a> (~1), !pA_arr, 1, !pC_arr, 1)
  val diff = cblas_asum<a,ar> (L, !pC_arr, 1)
(*
  val () = begin
    print "hpr_hpr2_test<"; print_typ<a> (); print ","; print_typ<ar> ();
    print ">: diff = "; print_elt<ar> diff; print_newline ()
  end // end of [val]
*)
  val () = begin
    print "hpr_hpr2_test<"; print_typ<a> (); print ","; print_typ<ar> ();
    print ">: starts ... ";
  end // end of [val]
  val () = assert (isEpsilon<ar> (diff \$NUM.div (of_int (M * M))))
  val () = begin
    print "finishes"; print_newline ()
  end // end of [val]
//
  prval pfBV1_gmat = fpfBV1_gmat (pfBV1_gev)
  prval pfBV2_gmat = fpfBV2_gmat (pfBV2_gev)
  prval pfBV_gmat = fpfBV_gmat (pfBV1_gmat, pfBV2_gmat)
  prval pfBV_fmat = fpfBV_fmat (pfBV_gmat)
  val () = fmatrix_ptr_free (pfBV_gc, pfBV_M2, pfBV_fmat| pBV_arr)
//
  prval pfV_fmat = fpfV_fmat (pfV_gmat)
  val () = fmatrix_ptr_free (pfV_gc, pfV_22, pfV_fmat | pV_arr)
//
  prval pfC_arr = array_v_of_GEVEC_v (pfC_gev)
  val () = array_ptr_free (pfC_gc, pfC_arr| pC_arr)
//
  prval pfB1_gmat = fpfB1_gmat (pfB1_gev)
  prval pfB2_gmat = fpfB2_gmat (pfB2_gev)
  prval pfB_gmat = fpfB_gmat (pfB1_gmat, pfB2_gmat)
  prval pfB_fmat = fpfB_fmat (pfB_gmat)
  val () = fmatrix_ptr_free (pfB_gc, pfB_M2, pfB_fmat | pB_arr)
//
  prval pfA_arr = array_v_of_GEVEC_v (pfA_gev)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
} // end of [hpr_hpr2_test]

(* ****** ****** *)

// BLAS level 3

(* ****** ****** *)

extern fun{a,ar:t@ype} gemm_test (): void

//
// checking the identity A * B = (B^T * A^T)^T
//
implement{a,ar} // ar = |a|
gemm_test (): void = () where {
  #define M 13 // needs to be a positive number
  #define K 29 // needs to be a positive number
  #define N 17 // needs to be a positive number
//
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  val () = cblas_gemm<a> (
    TRANDIM_N
  , TRANDIM_N
  | CblasColMajor
  , CblasNoTrans
  , CblasNoTrans
  , M, N, K
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , zero (* beta *)
  , !p3_arr, M
  ) // end of [val]
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf3_gem)
  val () = cblas_gemm<a> (
    TRANDIM_N
  , TRANDIM_N
  | CblasRowMajor
  , CblasNoTrans
  , CblasNoTrans
  , N, M, K
  , pos1 (* alpha *)
  , !p2_arr, K
  , !p1_arr, M
  , neg1 (* beta *)
  , !p3_arr, M
  ) // end of [val]
  prval TRANORDrowcol () = GEMAT_v_trans (pf1_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf3_gem)      
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  prval pf3_vec = GEVEC_v_of_array_v {a} (pf3_arr)
  val diff = cblas_nrm2<ar,a> (MN, !p3_arr, 1)
  val () = begin
    print "gemm_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf3_arr = array_v_of_GEVEC_v {a} (pf3_vec)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [gemm_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} syrk_test (): void

//
// checking the identity A * A' = (A')' * A'
//
implement{a,ar} // ar = |a|
syrk_test (): void = () where {
  #define N 17 // needs to be a positive number
  #define K 29 // needs to be a positive number
//
  val (pf_NK | NK) = op imul2 (N, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (NK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_NK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  var zero: a = of_double<a> (0.0)
  val (pf_NN | NN) = op imul2 (N, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NN)
  val NN_sz = size1_of_int1 NN
  val (pf2_gc, pf2_arr | p2_arr) =
    array_ptr_alloc_tsz {a} (NN_sz, sizeof<a>)
  // end of [val]
  val () =
    array_ptr_initialize_elt_tsz {a} (!p2_arr, NN_sz, zero, sizeof<a>)
  // end of [val]
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_NN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  prval (pf2_sym, fpf2_gem) = SYMAT_v_of_GEMAT_v (pf2_gem, UPLOupper)
  val () = cblas_syrk<a> (
    TRANDIM_N
  | CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , N, K
  , pos1 (* alpha *)
  , !p1_arr, N
  , zero (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval (pf2_sym, fpf2_gem) = SYMAT_v_of_GEMAT_v (pf2_gem, UPLOlower)
  val () = cblas_syrk<a> (
    TRANDIM_T
  | CblasRowMajor
  , CblasLower
  , CblasTrans
  , N, K
  , pos1 (* alpha *)
  , !p1_arr, N
  , neg1 (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
  prval TRANORDrowcol () = GEMAT_v_trans (pf1_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_NK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_NK, pf1_NK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_NN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_NN, pf2_NN)
  prval pf2_vec = GEVEC_v_of_array_v {a} (pf2_arr)
  val diff = cblas_nrm2<ar,a> (NN, !p2_arr, 1)
  val () = begin
    print "syrk_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf2_arr = array_v_of_GEVEC_v {a} (pf2_vec)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [syrk_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} syr2k_test (): void

//
// checking the identity A * B' + B * A' = A' * B'' + B' * A''
//
implement{a,ar} // ar = |a|
syr2k_test (): void = () where {
  #define N 17 // needs to be a positive number
  #define K 29 // needs to be a positive number
//
  val (pf_NK | NK) = op imul2 (N, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NK)
  val (pf1A_gc, pf1A_arr | p1A_arr) = randgen_arr<a> (NK)
  prval pf1A_fmat = fmatrix_v_of_array_v {a} (pf_NK, pf1A_arr)
  prval (pf1A_gem, fpf1A_fmat) = GEMAT_v_of_fmatrix_v (pf1A_fmat)
  val (pf1B_gc, pf1B_arr | p1B_arr) = randgen_arr<a> (NK)
  prval pf1B_fmat = fmatrix_v_of_array_v {a} (pf_NK, pf1B_arr)
  prval (pf1B_gem, fpf1B_fmat) = GEMAT_v_of_fmatrix_v (pf1B_fmat)
//
  var zero: a = of_double<a> (0.0)
  val (pf_NN | NN) = op imul2 (N, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NN)
  val NN_sz = size1_of_int1 NN
  val (pf2_gc, pf2_arr | p2_arr) =
    array_ptr_alloc_tsz {a} (NN_sz, sizeof<a>)
  // end of [val]
  val () =
    array_ptr_initialize_elt_tsz {a} (!p2_arr, NN_sz, zero, sizeof<a>)
  // end of [val]
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_NN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  prval (pf2_sym, fpf2_gem) = SYMAT_v_of_GEMAT_v (pf2_gem, UPLOupper)
  val () = cblas_syr2k<a> (
    TRANDIM_N
  | CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , N, K
  , pos1 (* alpha *)
  , !p1A_arr, N
  , !p1B_arr, N
  , zero (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1A_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf1B_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval (pf2_sym, fpf2_gem) = SYMAT_v_of_GEMAT_v (pf2_gem, UPLOlower)
  val () = cblas_syr2k<a> (
    TRANDIM_T
  | CblasRowMajor
  , CblasLower
  , CblasTrans
  , N, K
  , pos1 (* alpha *)
  , !p1A_arr, N
  , !p1B_arr, N
  , neg1 (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
  prval TRANORDrowcol () = GEMAT_v_trans (pf1A_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf1B_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
//
  prval () = pf1A_fmat := fpf1A_fmat (pf1A_gem)
  prval (pf1A_NK, pf1A_arr) = array_v_of_fmatrix_v (pf1A_fmat)
  prval () = mul_isfun (pf_NK, pf1A_NK)
  val () = array_ptr_free (pf1A_gc, pf1A_arr | p1A_arr)
  prval () = pf1B_fmat := fpf1B_fmat (pf1B_gem)
  prval (pf1B_NK, pf1B_arr) = array_v_of_fmatrix_v (pf1B_fmat)
  prval () = mul_isfun (pf_NK, pf1B_NK)
  val () = array_ptr_free (pf1B_gc, pf1B_arr | p1B_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_NN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_NN, pf2_NN)
  prval pf2_vec = GEVEC_v_of_array_v {a} (pf2_arr)
  val diff = cblas_nrm2<ar,a> (NN, !p2_arr, 1)
  val () = begin
    print "syr2k_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf2_arr = array_v_of_GEVEC_v {a} (pf2_vec)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [syr2k_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} symm_test (): void

//
// checking the identity A * B = (B^T * A^T)^T
//
implement{a,ar} // ar = |a|
symm_test (): void = () where {
  #define M 13 // needs to be a positive number
  #define K 13 // needs to be a positive number
  #define N 17 // needs to be a positive number
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  prval (pf1_sym, fpf1_gem) = SYMAT_v_of_GEMAT_v (pf1_gem, UPLOupper)
  val () = cblas_symm<a> (
    SIDEDIM_L
  | CblasColMajor
  , CblasLeft
  , CblasUpper
  , M, N
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , zero (* beta *)
  , !p3_arr, M
  ) // end of [val]
  prval pf1_gem = fpf1_gem (pf1_sym)
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf3_gem)
//
  prval (pf1_sym, fpf1_gem) = SYMAT_v_of_GEMAT_v (pf1_gem, UPLOlower)
  val () = cblas_symm<a> (
    SIDEDIM_R
  | CblasRowMajor
  , CblasRight
  , CblasLower
  , N, M
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , neg1 (* beta *)
  , !p3_arr, M
  ) // end of [val]
  prval pf1_gem = fpf1_gem (pf1_sym)
//
  prval TRANORDrowcol () = GEMAT_v_trans (pf1_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf3_gem)      
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  prval pf3_vec = GEVEC_v_of_array_v {a} (pf3_arr)
//
  val diff = cblas_nrm2<ar,a> (MN, !p3_arr, 1)
  val () = begin
    print "symm_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf3_arr = array_v_of_GEVEC_v {a} (pf3_vec)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [symm_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} trmm_trsm_test (): void

//
// checking the identity: B = A^{-1} A B
//
implement{a,ar} // ar = |a|
trmm_trsm_test (): void = () where {
  #define M 13 // needs to be a positive number
  #define K 13 // needs to be a positive number
  #define N 17 // needs to be a positive number
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_vec = GEVEC_v_of_array_v {a} (pf2_arr)
  val nrm1 = cblas_nrm2<ar,a> (KN, !p2_arr, 1)
  prval pf2_arr = array_v_of_GEVEC_v {a} (pf2_vec)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val pos1 = of_double<a> (1.0)
  prval (pf1_trm, fpf1_gem) = TRMAT_v_of_GEMAT_v (pf1_gem, UPLOupper, DIAGunit)
  val () = cblas_trmm<a> (
    SIDEDIM_L
  | CblasColMajor
  , CblasLeft
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , M, N
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  ) // end of [val]
  val () = cblas_trsm<a> (
    SIDEDIM_L
  | CblasColMajor
  , CblasLeft
  , CblasUpper
  , CblasNoTrans
  , CblasUnit
  , M, N
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  ) // end of [val]
  prval pf1_gem = fpf1_gem (pf1_trm)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  prval pf2_vec = GEVEC_v_of_array_v {a} (pf2_arr)
  val nrm2 = cblas_nrm2<ar,a> (KN, !p2_arr, 1)
  val diff = $NUM.sub<ar> (nrm1, nrm2)
  prval pf2_arr = array_v_of_GEVEC_v {a} (pf2_vec)
  val () = begin
    print "trmm_trsm_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
} // end of [trmm_trsm_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} hemm_test (): void

//
// checking the identity A * B = (B^T * A^T)^T
//
implement{a,ar} // ar = |a|
hemm_test (): void = () where {
  #define M 13 // needs to be a positive number
  #define K 13 // needs to be a positive number
  #define N 17 // needs to be a positive number
  val (pf_MK | MK) = op imul2 (M, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (MK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_MK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  val (pf_KN | KN) = op imul2 (K, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_KN)
  val (pf2_gc, pf2_arr | p2_arr) = randgen_arr<a> (KN)
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_KN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val (pf_MN | MN) = op imul2 (M, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_MN)
  val (pf3_gc, pf3_arr | p3_arr) = randgen_arr<a> (MN)
  prval pf3_fmat = fmatrix_v_of_array_v {a} (pf_MN, pf3_arr)
  prval (pf3_gem, fpf3_fmat) = GEMAT_v_of_fmatrix_v (pf3_fmat)
//
  val zero = of_double<a> (0.0)
  val pos1 = of_double<a> (1.0)
  val neg1 = of_double<a> (~1.0)
//
  prval (pf1_sym, fpf1_gem) = HEMAT_v_of_GEMAT_v (pf1_gem, UPLOupper)
  val () = cblas_hemm<a> (
    SIDEDIM_L
  | CblasColMajor
  , CblasLeft
  , CblasUpper
  , M, N
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , zero (* beta *)
  , !p3_arr, M
  ) // end of [val]
  prval pf1_gem = fpf1_gem (pf1_sym)
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf3_gem)
//
  prval (pf1_sym, fpf1_gem) = HEMAT_v_of_GEMAT_v (pf1_gem, UPLOlower)
  val () = cblas_hemm<a> (
    SIDEDIM_R
  | CblasRowMajor
  , CblasRight
  , CblasLower
  , N, M
  , pos1 (* alpha *)
  , !p1_arr, M
  , !p2_arr, K
  , neg1 (* beta *)
  , !p3_arr, M
  ) // end of [val]
  prval pf1_gem = fpf1_gem (pf1_sym)
//
  prval TRANORDrowcol () = GEMAT_v_trans (pf1_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf3_gem)      
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_MK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_MK, pf1_MK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_KN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_KN, pf2_KN)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
//
  prval () = pf3_fmat := fpf3_fmat (pf3_gem)
  prval (pf3_MN, pf3_arr) = array_v_of_fmatrix_v (pf3_fmat)
  prval () = mul_isfun (pf_MN, pf3_MN)
  prval pf3_vec = GEVEC_v_of_array_v {a} (pf3_arr)
//
  val diff = cblas_nrm2<ar,a> (MN, !p3_arr, 1)
  val () = begin
    print "hemm_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf3_arr = array_v_of_GEVEC_v {a} (pf3_vec)
  val () = array_ptr_free (pf3_gc, pf3_arr | p3_arr)
} // end of [hemm_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} herk_test (): void

//
// checking the identity A * A' = (A')' * A'
//
implement{a,ar} // ar = |a|
herk_test (): void = () where {
  #define N 17 // needs to be a positive number
  #define K 29 // needs to be a positive number
//
  val (pf_NK | NK) = op imul2 (N, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NK)
  val (pf1_gc, pf1_arr | p1_arr) = randgen_arr<a> (NK)
  prval pf1_fmat = fmatrix_v_of_array_v {a} (pf_NK, pf1_arr)
  prval (pf1_gem, fpf1_fmat) = GEMAT_v_of_fmatrix_v (pf1_fmat)
//
  var zero: a = of_double<a> (0.0)
  val (pf_NN | NN) = op imul2 (N, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NN)
  val NN_sz = size1_of_int1 NN
  val (pf2_gc, pf2_arr | p2_arr) =
    array_ptr_alloc_tsz {a} (NN_sz, sizeof<a>)
  // end of [val]
  val () =
    array_ptr_initialize_elt_tsz {a} (!p2_arr, NN_sz, zero, sizeof<a>)
  // end of [val]
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_NN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val zero = of_double<ar> (0.0)
  val pos1 = of_double<ar> (1.0)
  val neg1 = of_double<ar> (~1.0)
//
  prval (pf2_sym, fpf2_gem) = HEMAT_v_of_GEMAT_v (pf2_gem, UPLOupper)
  val () = cblas_herk<a,ar> (
    TRANDIM_N
  | CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , N, K
  , pos1 (* alpha *)
  , !p1_arr, N
  , zero (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval (pf2_sym, fpf2_gem) = HEMAT_v_of_GEMAT_v (pf2_gem, UPLOlower)
  val () = cblas_herk<a,ar> (
    TRANDIM_C
  | CblasRowMajor
  , CblasLower
  , CblasConjTrans
  , N, K
  , pos1 (* alpha *)
  , !p1_arr, N
  , neg1 (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
  prval TRANORDrowcol () = GEMAT_v_trans (pf1_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
//
  prval () = pf1_fmat := fpf1_fmat (pf1_gem)
  prval (pf1_NK, pf1_arr) = array_v_of_fmatrix_v (pf1_fmat)
  prval () = mul_isfun (pf_NK, pf1_NK)
  val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_NN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_NN, pf2_NN)
  prval pf2_vec = GEVEC_v_of_array_v {a} (pf2_arr)
  val diff = cblas_nrm2<ar,a> (NN, !p2_arr, 1)
  val () = begin
    print "herk_test<";
    print_typ<a> ();
    print ",";
    print_typ<ar> ();
    print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf2_arr = array_v_of_GEVEC_v {a} (pf2_vec)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [herk_test]

(* ****** ****** *)

extern fun{a,ar:t@ype} her2k_test (): void

//
// checking the identity A * B' + B * A' = A' * B'' + B' * A''
//
implement{a,ar} // ar = |a|
her2k_test (): void = () where {
  #define N 17 // needs to be a positive number
  #define K 29 // needs to be a positive number
//
  val (pf_NK | NK) = op imul2 (N, K) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NK)
  val (pf1A_gc, pf1A_arr | p1A_arr) = randgen_arr<a> (NK)
  prval pf1A_fmat = fmatrix_v_of_array_v {a} (pf_NK, pf1A_arr)
  prval (pf1A_gem, fpf1A_fmat) = GEMAT_v_of_fmatrix_v (pf1A_fmat)
  val (pf1B_gc, pf1B_arr | p1B_arr) = randgen_arr<a> (NK)
  prval pf1B_fmat = fmatrix_v_of_array_v {a} (pf_NK, pf1B_arr)
  prval (pf1B_gem, fpf1B_fmat) = GEMAT_v_of_fmatrix_v (pf1B_fmat)
//
  var zero: a = of_double<a> (0.0)
  val (pf_NN | NN) = op imul2 (N, N) // this should not be moved out!
  prval () = mul_nat_nat_nat (pf_NN)
  val NN_sz = size1_of_int1 NN
  val (pf2_gc, pf2_arr | p2_arr) =
    array_ptr_alloc_tsz {a} (NN_sz, sizeof<a>)
  // end of [val]
  val () =
    array_ptr_initialize_elt_tsz {a} (!p2_arr, NN_sz, zero, sizeof<a>)
  // end of [val]
  prval pf2_fmat = fmatrix_v_of_array_v {a} (pf_NN, pf2_arr)
  prval (pf2_gem, fpf2_fmat) = GEMAT_v_of_fmatrix_v (pf2_fmat)
//
  val pos1 = of_double<a> (1.0)
  val zero = of_double<ar> (0.0)
  val neg1 = of_double<ar> (~1.0)
//
  prval (pf2_sym, fpf2_gem) = HEMAT_v_of_GEMAT_v (pf2_gem, UPLOupper)
  val () = cblas_her2k<a,ar> (
    TRANDIM_N
  | CblasColMajor
  , CblasUpper
  , CblasNoTrans
  , N, K
  , pos1 (* alpha *)
  , !p1A_arr, N
  , !p1B_arr, N
  , zero (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
//
  prval TRANORDcolrow () = GEMAT_v_trans (pf1A_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf1B_gem)
  prval TRANORDcolrow () = GEMAT_v_trans (pf2_gem)
  prval (pf2_sym, fpf2_gem) = HEMAT_v_of_GEMAT_v (pf2_gem, UPLOlower)
  val () = cblas_her2k<a,ar> (
    TRANDIM_C
  | CblasRowMajor
  , CblasLower
  , CblasConjTrans
  , N, K
  , pos1 (* alpha *)
  , !p1A_arr, N
  , !p1B_arr, N
  , neg1 (* beta *)
  , !p2_arr, N
  ) // end of [val]
  prval () = pf2_gem := fpf2_gem (pf2_sym)
  prval TRANORDrowcol () = GEMAT_v_trans (pf1A_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf1B_gem)
  prval TRANORDrowcol () = GEMAT_v_trans (pf2_gem)
//
  prval () = pf1A_fmat := fpf1A_fmat (pf1A_gem)
  prval (pf1A_NK, pf1A_arr) = array_v_of_fmatrix_v (pf1A_fmat)
  prval () = mul_isfun (pf_NK, pf1A_NK)
  val () = array_ptr_free (pf1A_gc, pf1A_arr | p1A_arr)
  prval () = pf1B_fmat := fpf1B_fmat (pf1B_gem)
  prval (pf1B_NK, pf1B_arr) = array_v_of_fmatrix_v (pf1B_fmat)
  prval () = mul_isfun (pf_NK, pf1B_NK)
  val () = array_ptr_free (pf1B_gc, pf1B_arr | p1B_arr)
//
  prval () = pf2_fmat := fpf2_fmat (pf2_gem)
  prval (pf2_NN, pf2_arr) = array_v_of_fmatrix_v (pf2_fmat)
  prval () = mul_isfun (pf_NN, pf2_NN)
  prval pf2_vec = GEVEC_v_of_array_v {a} (pf2_arr)
  val diff = cblas_nrm2<ar,a> (NN, !p2_arr, 1)
  val () = begin
    print "her2k_test<"; print_typ<a> (); print ">";
    print ": diff = "; print_elt<ar> diff;
    print_newline ()
  end // end of [val]
  prval pf2_arr = array_v_of_GEVEC_v {a} (pf2_vec)
  val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)
} // end of [her2k_test]

(* ****** ****** *)

(*
// no longer needed
dynload "libats/DATS/genarrays.dats"
dynload "libats/DATS/fmatrix.dats"
*)

(* ****** ****** *)

fn level1_test (): void = () where {
//
  val () = rotg_test<float> ()
  val () = rotg_test<double> ()
(*
  val () = rotg_test<ccmplx> () // ATLAS extension
  val () = rotg_test<zcmplx> () // ATLAS extension
*)
//
  val () = rot_test<float> ()
  val () = rot_test<double> ()
//
  val () = rotm_test<float> ()
  val () = rotm_test<double> ()
//
  val () = asum_test<float,float> ()
  val () = asum_test<double,double> ()
  val () = asum_test<ccmplx,float> ()
  val () = asum_test<zcmplx,double> ()
//
  val () = iamax_test<float> ()
  val () = iamax_test<double> ()
  val () = iamax_test<ccmplx> ()
  val () = iamax_test<zcmplx> ()
//
  val () = dot_test<float> ()
  val () = dot_test<double> ()
//
  val () = dotu_test<float> ()
  val () = dotu_test<double> ()
  val () = dotu_test<ccmplx> ()
  val () = dotu_test<zcmplx> ()
//
  val () = dotc_test<float> ()
  val () = dotc_test<double> ()
  val () = dotc_test<ccmplx> ()
  val () = dotc_test<zcmplx> ()
//
  val () = swap_test<float,float> ()
  val () = swap_test<double,double> ()
  val () = swap_test<ccmplx,float> ()
  val () = swap_test<zcmplx,double> ()
//
  val () = copy_test<float,float> ()
  val () = copy_test<double,double> ()
  val () = copy_test<ccmplx,float> ()
  val () = copy_test<zcmplx,double> ()
//
  val () = axpy_test<float,float> ()
  val () = axpy_test<double,double> ()
  val () = axpy_test<ccmplx,float> ()
  val () = axpy_test<zcmplx,double> ()
//
  val () = scal_test<float,float,float> ()
  val () = scal_test<double,double,double> ()
  val () = scal_test<ccmplx,float,float> ()
  val () = scal_test<ccmplx,ccmplx,float> ()
  val () = scal_test<zcmplx,double,double> ()
  val () = scal_test<zcmplx,zcmplx,double> ()
//
  val () = begin
    print "Testing for BLAS level 1 functions is done."; print_newline ()
  end // end of [val]
} // end of [level1]

(* ****** ****** *)

fn level2_test (): void = () where {
  val () = gemv_test<float,float> ()
  val () = gemv_test<double,double> ()
  val () = gemv_test<ccmplx,float> ()
  val () = gemv_test<zcmplx,double> ()
//
  val () = gbmv_test<float,float> ()
  val () = gbmv_test<double,double> ()
  val () = gbmv_test<ccmplx,float> ()
  val () = gbmv_test<zcmplx,double> ()
//
  val () = trmv_trsv_test<float,float> ()
  val () = trmv_trsv_test<double,double> ()
  val () = trmv_trsv_test<ccmplx,float> ()
  val () = trmv_trsv_test<zcmplx,double> ()
//
  val () = tbmv_tbsv_test<float,float> ()
  val () = tbmv_tbsv_test<double,double> ()
  val () = tbmv_tbsv_test<ccmplx,float> ()
  val () = tbmv_tbsv_test<zcmplx,double> ()
//
  val () = tpmv_tpsv_test<float,float> ()
  val () = tpmv_tpsv_test<double,double> ()
  val () = tpmv_tpsv_test<ccmplx,float> ()
  val () = tpmv_tpsv_test<zcmplx,double> ()
//
  val () = symv_test<float,float> ()
  val () = symv_test<double,double> ()
//
  val () = ger_test<float,float> ()
  val () = ger_test<double,double> ()
//
  val () = geru_test<float,float> ()
  val () = geru_test<double,double> ()
  val () = geru_test<ccmplx,float> ()
  val () = geru_test<zcmplx,double> ()
//
  val () = gerc_test<float,float> ()
  val () = gerc_test<double,double> ()
  val () = gerc_test<ccmplx,float> ()
  val () = gerc_test<zcmplx,double> ()
//
  val () = hemv_test<float,float> ()
  val () = hemv_test<double,double> ()
  val () = hemv_test<ccmplx,float> ()
  val () = hemv_test<zcmplx,double> ()
//
  val () = hbmv_test<float,float> ()
  val () = hbmv_test<double,double> ()
  val () = hbmv_test<ccmplx,float> ()
  val () = hbmv_test<zcmplx,double> ()
//
  val () = hpmv_test<float,float> ()
  val () = hpmv_test<double,double> ()
  val () = hpmv_test<ccmplx,float> ()
  val () = hpmv_test<zcmplx,double> ()
//
  val () = her_her2_test<float,float> ()
  val () = her_her2_test<double,double> ()
  val () = her_her2_test<ccmplx,float> ()
  val () = her_her2_test<zcmplx,double> ()
//
  val () = hpr_hpr2_test<float,float> ()
  val () = hpr_hpr2_test<double,double> ()
  val () = hpr_hpr2_test<ccmplx,float> ()
  val () = hpr_hpr2_test<zcmplx,double> ()
//
  val () = begin
    print "Testing for BLAS level 2 functions is done."; print_newline ()
  end // end of [val]
} // end of [level2]

(* ****** ****** *)

fn level3_test (): void = () where {
//
  val () = gemm_test<float,float> ()
  val () = gemm_test<double,double> ()
  val () = gemm_test<ccmplx,float> ()
  val () = gemm_test<zcmplx,double> ()
//
  val () = syrk_test<float,float> ()
  val () = syrk_test<double,double> ()
  val () = syrk_test<ccmplx,float> ()
  val () = syrk_test<zcmplx,double> ()
//
  val () = syr2k_test<float,float> ()
  val () = syr2k_test<double,double> ()
  val () = syr2k_test<ccmplx,float> ()
  val () = syr2k_test<zcmplx,double> ()
//
  val () = symm_test<float,float> ()
  val () = symm_test<double,double> ()
  val () = symm_test<ccmplx,float> ()
  val () = symm_test<zcmplx,double> ()
//
  val () = trmm_trsm_test<float,float> ()
  val () = trmm_trsm_test<double,double> ()
  val () = trmm_trsm_test<ccmplx,float> ()
  val () = trmm_trsm_test<zcmplx,double> ()
//
  val () = hemm_test<float,float> ()
  val () = hemm_test<double,double> ()
  val () = hemm_test<ccmplx,float> ()
  val () = hemm_test<zcmplx,double> ()
//
  val () = herk_test<float,float> ()
  val () = herk_test<double,double> ()
  val () = herk_test<ccmplx,float> ()
  val () = herk_test<zcmplx,double> ()
//
  val () = her2k_test<float,float> ()
  val () = her2k_test<double,double> ()
  val () = her2k_test<ccmplx,float> ()
  val () = her2k_test<zcmplx,double> ()
//
  val () = begin
    print "Testing for BLAS level 3 functions is done."; print_newline ()
  end // end of [val]
} // end of [level3]

(* ****** ****** *)

implement main () = () where {
// (*
  val () = srand48_with_time () // this makes it really hard for debugging
// *)
  val () = level1_test ()
  val () = level2_test ()
  val () = level3_test ()
} // end of [main]

(* ****** ****** *)

(* end of [cblas_test.dats] *)
