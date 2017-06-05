
(*
**
** This is a primitive irrational-base discrete weighted transform taken
** from pseudocode found in: "Prime Numbers: A Computational Perspective"
**
** The code was originally written by Rick Lavoie and translated to ATS
** by Hongwei Xi (November, 2005)
**
** Absolutely no optimization has gone into this code; written for clarity,
** not speed.
**
*)

(*
**
** The code is ported to ATS/Geizella by Hongwei Xi (July 2007)
**
** test: 2 ^ 44497 - 1 is a prime! (BITSIZE = 16)
** time: 357.479u 0.234s 5:59.57 99.4%	0+0k 0+0io 0pf+0w
**
** test: 2 ^ 110503 - 1 is a prime! (BITSIZE = 16) 
** time: 2171.782u 2.094s 36:18.67 99.7% 0+0k 0+0io 0pf+0w
**
*)

(* ****** ****** *)

%{^
static inline
ats_double_type
double_of_double (ats_double_type x) { return x ; }
%} // end of [%{^]

(* ****** ****** *)

staload "libc/SATS/math.sats"
staload "libc/SATS/stdio.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

typedef real = double

macdef PI = 3.1415926535897932384626

extern fun real_of_double (d: double):<> real = "double_of_double"
#define d2r real_of_double

// This is an experimental effort to differentiate imaginary from real

abst@ype imag = $extype "ats_double_type"

extern fun imag_of_double (d: double):<> imag
  = "double_of_double"

#define d2i imag_of_double

extern fun add_imag_imag (x: imag, y: imag): imag
  = "atspre_add_double_double"
overload + with add_imag_imag

extern fun sub_imag_imag (x: imag, y: imag): imag
  = "atspre_sub_double_double"
overload - with sub_imag_imag

extern fun mul_imag_imag (x: imag, y: imag): real
  = "atspre_mul_double_double"

extern fun mul_real_imag (x: real, y: imag): imag
  = "atspre_mul_double_double"

extern fun mul_imag_real (x: imag, y: real): imag
  = "atspre_mul_double_double"

overload * with mul_imag_imag
overload * with mul_real_imag
overload * with mul_imag_real

//

#define i2d double_of_int

//

fn compute_signal_sum {n:nat} {l:addr}
  (pf: !array_v (real, n, l) | A: ptr l, n: int n): real =
  let
     fun loop {i:nat | i <= n} .<n-i>. (
         pf: !array_v (real, n, l) | i: int i, res: real
       ) :<cloptr1> real =
       if i < n then loop (pf | i+1, res + A[i]) else res
  in
     loop (pf | 0, d2r 0.0)
  end // end of [compute_signal_sum]

fn convolve_signal {n:nat} {l_r,l_i:addr}
  (pf_r: !array_v (real, n, l_r), pf_i: !array_v (imag, n, l_i) |
   R: ptr l_r, I: ptr l_i, n: int n): void = let
  fun loop {i:nat | i <= n} .<n-i>. (
      pf_r: !array_v (real, n, l_r)
    , pf_i: !array_v (imag, n, l_i)
    | i: int i
    ) :<cloptr1> void =
    if i < n then let
      val t_r = R[i] and t_i = I[i]
      val t_rr = t_r * t_r and t_ii = t_i * t_i and t_ri = t_r * t_i
    in
      R[i] := t_rr - t_ii; I[i] := t_ri + t_ri; loop (pf_r, pf_i | i+1)
    end else begin
      // this is empty
    end 
in
  loop (pf_r, pf_i | 0)
end // end of [convolve_signal]

fn round_signal {n:nat} {l_r,l_i:addr}
  (pf_r: !array_v (real, n, l_r), pf_i: !array_v (imag, n, l_i) |
   R: ptr l_r, I: ptr l_i, n: int n): void = let
  fun loop {i:nat | i <= n} .<n-i>. (
      pf_r: !array_v (real, n, l_r)
    , pf_i: !array_v (imag, n, l_i)
    | i: int i
    ) :<cloptr1> void =
    if i < n then begin
      R[i] := floor (R[i] + d2r 0.5); I[i] := d2i (0.0); loop (pf_r, pf_i | i+1)
    end
in
  loop (pf_r, pf_i | 0)
end // end of [round_signal]

//

fun forward_fft {n:nat} {l_r, l_i, l_w:addr}
   (pf_r: !array_v (real, n, l_r) ,
    pf_i: !array_v (imag, n, l_i) ,
    pf_w: !array_v (real, n, l_w) |
    R: ptr l_r, I: ptr l_i, W: ptr l_w, n: int n): real = let

  fun aux0 {i:nat | i <= n} .<n-i>. (
      pf_r: !array_v (real, n, l_r)
    , pf_w: !array_v (real, n, l_w)
    | i: int i
    ) :<cloptr1> void = begin
    if i < n then (R[i] := R[i] * W[i]; aux0 (pf_r, pf_w | i+1)) else ()
  end

  fun aux1 {i,m:nat} (
      pf_r: !array_v (real, n, l_r)
    , pf_i: !array_v (imag, n, l_i)
    | i: int i, m: int m, a_r: real, a_i: imag
    ) :<cloptr1> void = let
    val im = i + m
  in
    if im < n then let
      val t1_r = R[im] and t1_i = I[im]
      val t2_r = R[i] and t2_i = I[i]
      val () = R[i] := t2_r + t1_r
      and () = I[i] := t2_i + t1_i
      val t1_r = t2_r - t1_r and t1_i = t2_i - t1_i
      val () = R[im] := a_r * t1_r - a_i * t1_i
      val () = I[im] := a_r * t1_i + a_i * t1_r
    in
      aux1 (pf_r, pf_i | im + m, m, a_r, a_i)
    end else begin
      // this is empty
    end
  end // end of [aux1]

  fun aux2 {j,m:nat} (
      pf_r: !array_v (real, n, l_r)
    , pf_i: !array_v (imag, n, l_i)
    | j: int j, m: int m
    ) :<cloptr1> void =
    if j < m then let
      val theta = d2r (PI * (i2d j) / (i2d m))
    in
      aux1 (pf_r, pf_i | j, m, d2r (cos theta), d2i ~(sin theta));
      aux2 (pf_r, pf_i | j+1, m)
    end

  fun aux3 {m:nat}
    (pf_r: !array_v (real, n, l_r), pf_i: !array_v (imag, n, l_i) | m: int m)
    :<cloptr1> void =
    if m >= 1 then (aux2 (pf_r, pf_i | 0, m); aux3 (pf_r, pf_i | nhalf m))

  val () = aux0 (pf_r, pf_w | 0)
  val sum = compute_signal_sum (pf_r | R, n)
in
  aux3 (pf_r, pf_i | nhalf n); sum
end // end of [forward_fft]

//

fun inverse_fft {n:nat} {l_r, l_i, l_w:addr}
  (pf_r: !array_v (real, n, l_r) ,
   pf_i: !array_v (imag, n, l_i) ,
   pf_w: !array_v (real, n, l_w) |
   R: ptr l_r, I: ptr l_i, W: ptr l_w, n: int n): real = let
  fun aux1 {i,m:nat} (
      pf_r: !array_v (real, n, l_r)
    , pf_i: !array_v (imag, n, l_i)
    | i: int i, m: int m, a_r: real, a_i: imag
    ) :<cloptr1> void = let
    val im = i + m
  in
    if im < n then let
      val c_r = R[im] and c_i = I[im]
      val t1_r = c_r * a_r - c_i * a_i
      and t1_i = c_r * a_i + c_i * a_r
      val t2_r = R[i] and t2_i = I[i]
      val () = R[i] := t2_r + t1_r and () = I[i] := t2_i + t1_i
      val () = R[im] := t2_r - t1_r and () = I[im] := t2_i - t1_i
    in
      aux1 (pf_r, pf_i | im + m, m, a_r, a_i)
    end
  end

  fun aux2 {j,m:nat}
    (pf_r: !array_v (real, n, l_r), pf_i: !array_v (imag, n, l_i) |
     j: int j, m: int m):<cloptr1> void =
    if j < m then let
      val theta = d2r (PI * (i2d j) / (i2d m))
    in
      aux1 (pf_r, pf_i | j, m, d2r (cos theta), d2i (sin theta));
      aux2 (pf_r, pf_i | j+1, m)
    end

  fun aux3 {m:nat} (
      pf_r: !array_v (real, n, l_r)
    , pf_i: !array_v (imag, n, l_i)
    | m: int m
    ) :<cloptr1> void =
    if m < n then (aux2 (pf_r, pf_i | 0, m); aux3 (pf_r, pf_i | m+m))

  fun aux4 {i:nat | i <= n} (
      pf_r: !array_v (real, n, l_r) | i: int i, nf: real
    ) :<cloptr1> void =
    if i < n then (R[i] := R[i] / nf; aux4 (pf_r | i+1, nf))

  fun aux5 {i:nat | i <= n} (
      pf_r: !array_v (real, n, l_r)
    , pf_w: !array_v (real, n, l_w)
    | i: int i
    ) :<cloptr1> void = begin
    if i < n then (R[i] := R[i] / W[i]; aux5 (pf_r, pf_w | i+1)) else ()
  end

  val () = aux3 (pf_r, pf_i | 1)
  val () = aux4 (pf_r | 0, d2r (i2d n))
  val sum = compute_signal_sum (pf_r | R, n)
  val () = aux5 (pf_r, pf_w | 0)
in
  sum
end // end of [inverse_fft]

//

fun modular_reduce {n:nat} {l_r,l_b:addr}
  (pf_r: !array_v (real, n, l_r), pf_b: !array_v (real, n, l_b) |
   R: ptr l_r, B: ptr l_b, n: int n): void = let
  fun aux1 {i:nat | i <= n} (
      pf_r: !array_v (real, n, l_r)
    , pf_b: !array_v (real, n, l_b)
    | i: int i, carry: real
    ) :<cloptr1> real =
    if i < n then let
      val temp = R[i] + carry
      val bi = B[i]
      val () = R[i] := fmod (temp, bi)
    in
      aux1 (pf_r, pf_b | i+1, floor (temp / bi))
    end else begin
      carry // return value
    end // end of [if]

  fun aux2 {i:nat | i <= n} (
      pf_r: !array_v (real, n, l_r)
    , pf_b: !array_v (real, n, l_b)
    | i: int i, carry: real
    ) :<cloptr1> void =
    if i < n then
      if carry = d2r 0.0 then () else let
        val temp = R[i] + carry
        val bi = B[i]
        val () = R[i] := fmod (temp, bi)
      in
        aux2 (pf_r, pf_b | i+1, floor (temp / bi))
      end
    else begin
      aux2 (pf_r, pf_b | 0, carry)
    end // end of [if]

  val carry = aux1 (pf_r, pf_b | 0, d2r 0.0)
in
  aux2 (pf_r, pf_b | 0, carry)
end // end of [modular_reduce]

//

fun subtract2 {n:nat} {l_r,l_b:addr}
  (pf_r: !array_v (real, n, l_r), pf_b: !array_v (real, n, l_b) |
   R: ptr l_r, B: ptr l_b, n: int n): void = let
  fun aux {i:nat | i <= n} (
      pf_r: !array_v (real, n, l_r)
    , pf_b: !array_v (real, n, l_b)
    | i: int i, carry: real
    ) :<cloptr1> void =
    if i < n then
      if carry = d2r 0.0 then () else let
        val ri = R[i] and bi = B[i]
      in
        if carry > ri then
          (R[i] := bi - carry + ri; aux (pf_r, pf_b | i+1, d2r 1.0))
        else let
          val t = ri - carry
        in
          R[i] := fmod (t, bi);
          aux (pf_r, pf_b | i+1, floor (t / bi))
        end
      end else begin
        // this is empty
      end // end of [if]
   // end of [if]
in
  aux (pf_r, pf_b | 0, d2r 2.0)
end // end of [substract2]

//

#define BITSIZE 16

fun compute_optimal_signal_size (exponent: Nat): intGt 0 =
  let
     fun aux (i: Nat, p: intGt 0): intGt 0 =
       if i < BITSIZE then p else aux (nhalf i, p + p)
  in
     aux (exponent, 1)
  end

//

fun compute_base_signal {n:nat} {l_b:addr} (
    pf_b: !array_v (real, n, l_b) | B: ptr l_b, exponent: Nat, n: int n
  ): void = let
  fun aux {i:nat | i <= n}
    (pf_b: !array_v (real, n, l_b) | i: int i):<cloptr1> void =
    if i < n then let
      val e = i2d exponent
      val en = e / (i2d n)
      val eni = en * (i2d i)
    in
      B[i] := pow (d2r 2.0, d2r (ceil (eni + en) - ceil (eni)));
      aux (pf_b | i+1)
    end // end of [if]
in
  aux (pf_b | 0)
end // end of [compute_base_signal]

//

fun compute_weight_signal {n:nat} {l_w:addr} (
    pf_w: !array_v (real, n, l_w)
  | W: ptr l_w, exponent: Nat, n: int n
  ) : void = let
  fun aux {i:nat | i <= n} .<n-i>.
    (pf_w: !array_v (real, n, l_w) | i: int i):<cloptr1> void =
    if i < n then let
      val e = i2d exponent
      val en = e / (i2d n)
      val eni = en * (i2d i)
    in
      W[i] := pow (d2r 2.0, d2r (ceil eni - eni));
      aux (pf_w | i+1)
    end
in
  aux (pf_w | 0)
end // end of [compute_weight_signal]

//

fun check_zero_signal {n:nat} {l:addr}
  (pf: !array_v (real, n, l) | A: ptr l, n: int n): bool = let
  fun aux {i:nat | i <= n} .<n-i>.
    (pf: !array_v (real, n, l) | i: int i):<cloptr1> bool =
    if i < n then (if A[i] = d2r 0.0 then aux (pf | i+1) else false)
    else true
in
  aux (pf | 0)
end // end of [check_zero_signal]

//

fun do_iteration {n:nat} {l_r,l_i,l_b,l_w:addr}
   (pf_r: !array_v (real, n, l_r), pf_i: !array_v (imag, n, l_i) , 
    pf_b: !array_v (real, n, l_b), pf_w: !array_v (real, n, l_w) |
    R: ptr l_r, I: ptr l_i, B: ptr l_b, W: ptr l_w, n: int n): void = let
  val sum_in = forward_fft (pf_r, pf_i, pf_w | R, I, W, n)
  val () = convolve_signal (pf_r, pf_i | R, I, n)
  val sum_out = inverse_fft (pf_r, pf_i, pf_w | R, I, W, n)
  val diff = abs (sum_in * sum_in - sum_out)
in
  if diff > d2r 1.0 then begin
    print "Possible error: difference = "; print diff; print_newline ()
   end;
   round_signal (pf_r, pf_i | R, I, n);
   modular_reduce (pf_r, pf_b | R, B, n);
   subtract2 (pf_r, pf_b | R, B, n)
end // end of [do_iteration]

#define STRIDE 100

fun main_loop {n:nat} {l_r,l_i,l_b,l_w:addr}
   (pf_r: !array_v (real, n, l_r), pf_i: !array_v (imag, n, l_i) , 
    pf_b: !array_v (real, n, l_b), pf_w: !array_v (real, n, l_w) |
    R: ptr l_r, I: ptr l_i, B: ptr l_b, W: ptr l_w, n: int n,
    i: Nat, limit: Nat): void =
  if i < limit then begin
    if (i mod STRIDE = 0) then (print '.'; fflush_stdout ());
    do_iteration (pf_r, pf_i, pf_b, pf_w | R, I, B, W, n);
    main_loop (pf_r, pf_i, pf_b, pf_w | R, I, B, W, n, i+1, limit)
  end // end of [if]

// Here are the first few Mersenne primes:
// 2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607,
// 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213,
// 19937, 21701, 23209, 44497, 86243, 110503, 132049, 216091,
// 756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593,
// 13466917, ...

(* ****** ****** *)

(*

implement{a} array_ptr_alloc (n) =
  array_ptr_alloc_tsz {a} (n, sizeof<a>)

*)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

// The following function tests whether 2^exponent-1 is a prime:
fun is_mersenne_prime (exponent: intGte 2): bool = let
  val n = compute_optimal_signal_size (exponent)
  val n_sz = size1_of_int1 n
  val (pf_gc_r, pf_r | R) = array_ptr_alloc<real> (n_sz)
  val () = array_ptr_initialize_elt<real> (!R, n_sz, d2r 0.0)

  val (pf_gc_i, pf_i | I) = array_ptr_alloc<imag> (n_sz)
  val () = array_ptr_initialize_elt<imag> (!I, n_sz, d2i 0.0)

  val (pf_gc_b, pf_b | B) = array_ptr_alloc<real> (n_sz)
  val () = array_ptr_initialize_elt<real> (!B, n_sz, d2r 0.0)

  val (pf_gc_w, pf_w | W) = array_ptr_alloc<real> (n_sz)
  val () = array_ptr_initialize_elt<real> (!W, n_sz, d2r 0.0)

  val () = compute_base_signal (pf_b | B, exponent, n)
  val () = compute_weight_signal (pf_w | W, exponent, n)
  val () = R[0] := d2r 4.0
  val () = main_loop (pf_r, pf_i, pf_b, pf_w | R, I, B, W, n, 0, exponent - 2)
  val () = print_newline ()
  val ans = check_zero_signal (pf_r | R, n)
in
  array_ptr_free {real} (pf_gc_r, pf_r | R);
  array_ptr_free {imag} (pf_gc_i, pf_i | I);
  array_ptr_free {real} (pf_gc_b, pf_b | B);
  array_ptr_free {real} (pf_gc_w, pf_w | W);
  ans
end // end of [is_mersenne_prime]

//

fn usage (): void = begin
  print "A natural number argument >= 2 is needed.";
  print_newline ()
end

implement main (argc, argv) = let
  val () = if argc <= 1 then (usage (); exit {void} (1))
  val () = assert (argc > 1)
  val exp = int1_of (argv.[1])
  val () = if exp <= 1 then (usage (); exit {void} (1))
  val () =  assert (exp > 1)
in
  if is_mersenne_prime exp then begin
    printf ("2 ^ %i - 1 is a prime!\n", @(exp))
  end else begin
    printf ("2 ^ %i - 1 is a composite.\n", @(exp))
  end // end of [if]
end // end of [main]

////

/* FFT.c
Very primitive implementation of the irrational-base discrete weighted transform.
Written by Rick Lavoie
Taken from pseudocode found in "Prime Numbers: A Computational Perspective"

Absolutely no optimization has gone into this code. It's written for clarity, not speed.
*/
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include <stdlib.h>

#define PI 3.1415926535897932384626

// Displays the contents of a signal
int display_signal(double* real, double* imag, unsigned int signal_size)
{
	unsigned int i;

	if (imag == NULL) { // Pass NULL as the second parameter if we just want the real part
		for (i = 0; i < signal_size; i++)
			printf("%.2f ", real[i]);
	} else {
		for (i = 0; i < signal_size; i++) 
			printf("%.2f+%.2fi ", real[i], imag[i]);
	}

	printf("\n");
	return 0;
}

// Adds together the values of a signal. Used for error-checking
double compute_signal_sum(double* real, unsigned int signal_size)
{
	unsigned int i;
	double running_total = 0.0;

	for (i = 0; i < signal_size; i++) 
		running_total += real[i]; 
	
	return running_total;
}

// Computes the forward fast fourier transform of a complex signal. Uses Gentleman-Sande decimation
// in frequency variant. weight is a weight signal, and signal_size is the size of the signal. real
// and imag are the signal to be transformed. Bitscrambling is not done.
double forward_fast_fourier(double* real, double* imag, double* weight, unsigned int signal_size)
{
	unsigned int m, j, i;
	double a_real, a_imag, temp1_real, temp1_imag, temp2_real, temp2_imag;
	double sum;
	
	for (m = 0; m < signal_size; m++) // Initially multiply the input by the weights
		real[m] *= weight[m];
	
	sum = compute_signal_sum(real, signal_size);

	// Taken straight from "Prime Numbers"
	for (m = signal_size/2; m >= 1; m = m/2) {
		for (j = 0; j < m; j++) {
			a_real = cos((2.0*PI*j*signal_size/(2*m))/signal_size);
			a_imag = -sin((2.0*PI*j*signal_size/(2*m))/signal_size);
			for (i = j; i < signal_size; i += 2*m) {
				temp1_real = real[i+m];
				temp1_imag = imag[i+m];
				temp2_real = real[i];
				temp2_imag = imag[i];
				real[i] = temp2_real + temp1_real;
				imag[i] = temp2_imag + temp1_imag;
				temp1_real = temp2_real - temp1_real;
				temp1_imag = temp2_imag - temp1_imag;
				real[i+m] = (a_real * temp1_real) - (a_imag * temp1_imag);
				imag[i+m] = (a_real * temp1_imag) + (a_imag * temp1_real);
			}
		}
	}

	return sum;
}

// Computes the inverse fast fourier transform. Uses Cooley-Turkey decimation in time. Parameters
// are the same as the forward transform. Bitscrambling is not done, because the use of the two
// different decimations cancels them out.
double inverse_fast_fourier(double* real, double* imag, double* weight, unsigned int signal_size)
{
	unsigned int m, j, i;
	double a_real, a_imag, temp1_real, temp1_imag, temp2_real, temp2_imag;
	double sum;
		
	for (m = 1; m < signal_size; m = 2*m) {
		for (j = 0; j < m; j++) {
			a_real = cos((2.0*PI*j*signal_size/(2*m))/signal_size);
			a_imag = sin((2.0*PI*j*signal_size/(2*m))/signal_size);
			for (i = j; i < signal_size; i += 2*m) {
				temp1_real = (real[i+m] * a_real) - (imag[i+m] * a_imag);
				temp1_imag = (real[i+m] * a_imag) + (imag[i+m] * a_real);
				temp2_real = real[i];
				temp2_imag = imag[i];
				real[i] = temp2_real + temp1_real;
				imag[i] = temp2_imag + temp1_imag;
				real[i+m] = temp2_real - temp1_real;
				imag[i+m] = temp2_imag - temp1_imag;
			}
		}
	}

	for (m = 0; m < signal_size; m++) // Divide by the signal_size to re-normalize the signal
		real[m] = real[m] / signal_size;
	
	sum = compute_signal_sum(real, signal_size);
	
	for (m = 0; m < signal_size; m++)  // Adjust by the weights
		real[m] = real[m] / weight[m];
	
	return sum;
}

// Convulves a transformed signal. IE, square the signal dyadically.
int convulve_signal(double* real, double* imag, unsigned int signal_size)
{
	unsigned int i;
	double temp_real, temp_imag;

	for (i = 0; i < signal_size; i++) {
		temp_real = real[i];
		temp_imag = imag[i];
		real[i] = (temp_real * temp_real) - (temp_imag * temp_imag); // Complex multiplication
		imag[i] = (temp_real * temp_imag) + (temp_real * temp_imag);
	}

	return 0;
}

// Rounds the floating point signal to closest integers.
int round_signal(double* real, double* imag, unsigned int signal_size)
{
	unsigned int i;

	// I'll eventually add a check to see if we're not rounding too far
	for (i = 0; i < signal_size; i++) 
		real[i] = floor(real[i] + 0.5);
	
	return 0;
}

// Reduces the signal into their appropriate bases. base is the base signal. 
int modular_reduce(double* real, double* base, unsigned int signal_size)
{
	unsigned int i;
	double carry = 0;
	double temp;

	for (i = 0; i < signal_size; i++) {
		temp = real[i] + carry;
		real[i] = fmod(temp, base[i]);
		carry = floor(temp/base[i]);
	}
	
	while (carry != 0) {
		for (i = 0; i < signal_size; i++) {
			if (carry == 0)
				break;
			temp = real[i] + carry;
			real[i] = fmod(temp, base[i]);
			carry = floor(temp/base[i]);
		}
	}
	
	return 0;
}

// Subtracts 2 from the signal. This is part of the Lucas-Lehmer test. This code won't work
// if the base is less than 2.
int subtract_2(double* real, double* base, unsigned int signal_size)
{
	unsigned int i;
	double carry = 2; // Subtract 2
	double temp;

	for (i = 0; i < signal_size; i++) {
		if (carry == 0) // No more carry, so we can exit early
			break;
		else if (carry > real[i]) { // If real[i] is less than the carry
			real[i] = base[i] - (carry - real[i]);
			carry = 1;
		} else {
			temp = real[i] - carry; // Subtract the carry
			real[i] = fmod(temp, base[i]); // Modular adjust
			carry = floor(temp/base[i]); // Get the carry
		}
	}

	return 0;
}

// Computes the optimal size of a signal, given an exponent
unsigned int compute_optimal_signal_size(unsigned int exponent)
{
	unsigned int temp = 1;
	// "Prime Numbers" says to find a signal_length such that Floor[2^(exponent/temp)] is
	// an "appropriate" size;
	while (1) { 
		if ((double)exponent/temp < 16) // I have no clue how to decide what's an ideal bitsize
			break; // so I'm just using 16 bits and hope it's adequate
		temp *= 2;
	}

	return temp;
}

// Computes the base signal for an exponent
int compute_base_signal(double* base, unsigned int exponent, unsigned int signal_size)
{
	unsigned int i;
	// "Prime Numbers" says base[i-1] = 2^(Ceil[exponent*i/signal_size] - Ceil[exponent*(i-1)/signal_size]);
	// Running i from 1 to signal_size;
	for (i = 1; i <= signal_size; i++) 
		base[i-1] = pow(2, ceil((double)exponent*i/signal_size)-ceil((double)exponent*(i-1)/signal_size));

	return 0;
}

// Computes the weight signal for an exponent
int compute_weight_signal(double* weight, unsigned int exponent, unsigned int signal_size)
{
	unsigned int i;
	double temp;
	// "Prime Numbers" says weight[i] = 2^(Ceil[exponent*i/signal_size]-(exponent*i/signal_size))
	for (i = 0; i < signal_size; i++) {
		temp = ((double)exponent*i)/signal_size;
		weight[i] = pow(2, ceil(temp) - temp);
	}

	return 0;
}

int check_zero_signal(double* signal, unsigned int signal_size)
{
	int is_zero = 1;
	unsigned int i;

	for (i = 0; i < signal_size; i++) {
		if (signal[i] != 0) {
			is_zero = 0;
			break;
		}
	}

	return is_zero;
}

// Performs one Lucas-Lehmer iteration using the IBDWT algorithm
int do_iteration(double* real, double* imag, double* weight, double* base, unsigned int signal_size)
{
	double sum_in, sum_out, difference;
		
	// I know that you can "embed" the real signal into the complex, but for clarity I'm avoiding that
	sum_in = forward_fast_fourier(real, imag, weight, signal_size);

	// Square the elements
	convulve_signal(real, imag, signal_size);

	// Invert the signal
	sum_out = inverse_fast_fourier(real, imag, weight, signal_size);

	// What's the difference between sum_in^2 and sum_out
	difference = fabs((sum_in * sum_in) - sum_out);
	if (difference > 10)  // Uh oh, an error, they're not matching
		printf("**POSSIBLE ERROR**: SUMIN and SUMOUT differ by %.2f\n", difference);
		
	// Round the signal to integers
	round_signal(real, imag, signal_size);
	memset(imag, 0, signal_size*sizeof(double));

	// Modular reduce them to their variable base
	modular_reduce(real, base, signal_size); 
	// Subtract 2
	subtract_2(real, base, signal_size);

	return 0;
}

int main(int argc, char* argv[])
{

  double* real; // Real part of the signal
  double* imag; // Imaginary part of the signal
  unsigned int signal_size; // Signal length
  unsigned int exponent; // Exponent for the LL test (Change as needed)
  double* base; // Variable base of the signal
  double* weight; // Weight signal
  unsigned int i;

  exponent = atoi(argv[1]);

  // Find out the appropriate size for this exponent
  signal_size = compute_optimal_signal_size(exponent);

  // Allocate memory
  base = malloc(sizeof(double)*signal_size);
  weight = malloc(sizeof(double)*signal_size);
  real = malloc(sizeof(double)*signal_size);
  imag = malloc(sizeof(double)*signal_size);

  // Clear memory and compute the signals
  memset(real, 0, sizeof(double) * signal_size);
  memset(imag, 0, sizeof(double) * signal_size);
  compute_base_signal(base, exponent, signal_size);
  compute_weight_signal(weight, exponent, signal_size);

  real[0] = 4; // LL test starts with 4
  for (i = 0; i < exponent-2; i++) {
    do_iteration(real, imag, weight, base, signal_size);
    if (i % 100 == 0)
      printf("%i\r\n", i);
  }

  if (check_zero_signal(real, signal_size) == 1)
    printf("PRIME!!!\n");
  else
    printf("Not Prime\n");

  // Clear memory
  free(base);
  free(weight);
  free(real);
  free(imag);
  return 0;
}

(* ****** ****** *)

(* end of [fft.dats] *)
