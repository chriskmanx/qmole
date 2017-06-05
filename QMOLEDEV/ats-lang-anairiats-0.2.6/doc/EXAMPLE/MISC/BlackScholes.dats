//
//
// An implementation of the Black-Scholes formula. The ATS code is
// largely _translated_ from the C++ version by Espen Gaarder Haug
// attached at the end of this file.
//
// Hongwei Xi (November 17 2007)
//
//

staload "libc/SATS/math.sats"

(* ****** ****** *)

#define Pi 3.1415926535897932384626

// The cumulative normal distribution function 
fn CND (X: double): double = let

val a1 = 0.31938153
val a2 = ~0.356563782
val a3 = 1.781477937
val a4 = ~1.821255978
val a5 = 1.330274429
val L = abs(X)
val K = 1.0 / (1.0 + 0.2316419 * L)
var w: double = a5
val () = w := (w + a4) * K
val () = w := (w + a3) * K
val () = w := (w + a2) * K
val () = w := (w + a1) * K
val () = w := 1.0 - 1.0 / sqrt (Pi + Pi) * exp (~(L * L / 2.0)) * w

in

if X < 0.0 then 1.0 - w else w

end

fn BlackScholes
  (CallPutFlag: char, S: double, X: double, T: double, r: double, v: double)
  : double = let

val sqrt_T = sqrt T
val d1= (log (S/X) + (r+v*v/2.0) * T) / (v * sqrt_T)
val d2 = d1 - v * sqrt_T

in

case+ CallPutFlag of
  | 'c' => S * CND(d1) - X * exp(~r*T) * CND(d2)
  | 'p' => X * exp(~r * T) * CND(~d2) - S * CND (~d1)
  | _ => begin
      prerr "The value of [CallPutFlag] is illegal: ";
      prerr CallPutFlag;
      prerr_newline ();
      exit (1)
   end

end // end of [BlackScholes]

////

Black-Scholes in C++

By Espen Gaarder Haug

C++: a bit harder than most other languages but very fast and
powerful. After my opinion the Rolls Royce computer language for
mathematical models where you need speed (for closed form solutions like
Blacks-Scholes you are naturally doing fine in almost any language, but
when it comes to large scale Monte Carlo C++ is really a plus).

#ifndef Pi 
#define Pi 3.141592653589793238462643 
#endif 


// The Black and Scholes (1973) Stock option formula
double BlackScholes(char CallPutFlag, double S, double X, double T, double r, double v)
{
double d1, d2;


d1=(log(S/X)+(r+v*v/2)*T)/(v*sqrt(T));
d2=d1-v*sqrt(T);

if(CallPutFlag == 'c')
return S *CND(d1)-X * exp(-r*T)*CND(d2);
else if(CallPutFlag == 'p')
return X * exp(-r * T) * CND(-d2) - S * CND(-d1);
}


// The cumulative normal distribution function 
double CND( double X )
{

double L, K, w ;

double const a1 = 0.31938153, a2 = -0.356563782, a3 = 1.781477937;
double const a4 = -1.821255978, a5 = 1.330274429;

L = fabs(X);
K = 1.0 / (1.0 + 0.2316419 * L);
w = 1.0 - 1.0 / sqrt(2 * Pi) * exp(-L *L / 2) * (a1 * K + a2 * K *K + a3 * pow(K,3) + a4 * pow(K,4) + a5 * pow(K,5));

if (X < 0 ){
w= 1.0 - w;
}
return w;
} 

(* ****** ****** *)

(* end of [BlackScholes.dats] *)
