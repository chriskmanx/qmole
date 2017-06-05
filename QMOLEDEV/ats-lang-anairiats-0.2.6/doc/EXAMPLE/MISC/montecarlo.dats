//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: June 8, 2009
//

(* ****** ****** *)

staload ARRAY2 = "libats/smlbas/SATS/array2.sats"
typedef array2 (a:t@ype) = $ARRAY2.array (a)

(* ****** ****** *)

staload Math = "libc/SATS/math.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "libats/smlbas/DATS/array2.dats"

(* ****** ****** *)

typedef point = @(double, double) // flat representation

fun square (x: double): double = x * x

fun dist_pt_pt (pt1: point, pt2: point) = let
  val dx = pt1.0 - pt2.0 and dy = pt1.1 - pt2.1
in
  $Math.sqrt (dx * dx + dy * dy) 
end // end of [dist_pt_pt]

(* ****** ****** *)

// some params for this program

#define N 1000

(* ****** ****** *)

val NN: int = N * N
val Nr: double = double_of_int (N) 
val epsilon: double = 1.0 / Nr

(* ****** ****** *)

typedef pointlst = list0 (point)
val thePointlstArray = $ARRAY2.array<pointlst> (N, N, list0_nil)

(* ****** ****** *)

staload Rand = "libc/SATS/random.sats"

extern fun rand_double (): double (* btw 0 and 1 *)

// without the adjustment, [rand_double] may return 1.000000
implement rand_double () = $Rand.drand48 () * (1.0 - 1.0 / NN)

(* ****** ****** *)

extern fun do_one_square
  (pt: point, i: int, j: int): int
  
implement do_one_square (pt, i, j) = let
  val i = int1_of_int (i); val () = assert (i >= 0); val i = size1_of_int1 (i)
  val j = int1_of_int (j); val () = assert (j >= 0); val j = size1_of_int1 (j)
  val pts = $ARRAY2.sub (thePointlstArray, i, j)
  fun loop (pt0: point, pts: pointlst, res: int): int =
    case+ pts of
    | list0_cons (pt, pts) => let
        val dist = dist_pt_pt (pt, pt0) in
        if dist >= epsilon then loop (pt0, pts, res)
                           else loop (pt0, pts, res+1)
      end // end of [list0_cons]                     
    | list0_nil () => res
  // end of [loop]
in
  loop (pt, pts, 0(*res*)) 
end (* end of [do_one_square] *)

(* ****** ****** *)

fun do_one_round (): int = res where {
  val px = rand_double () and py = rand_double ()
  val pt = @(px, py)
  val Nx = int_of_double (N * px)
  val Ny = int_of_double (N * py)
  fun do_all_squares .<>. (
      pt: point, Nx: int, Ny: int
    ) : int = res where {
    var i: int = 0 and j: int = 0; var res: int = 0
    val () = for (i := Nx-1; i <= Nx+1; i := i+1) let
      val () = if i < 0 then continue else (if i >= N then break else ())
      val () = for (j := Ny-1; j <= Ny+1; j := j+1) let
        val () = if j < 0 then continue else (if j >= N then break else ())
      in
        res := res + do_one_square (pt, i, j)
      end (* end of [val] *)
    in
      // empty
    end (* end of [val] *)
  } (* end of [do_all_squares] *)
  val res = do_all_squares (pt, Nx, Ny)
  val Nx = int1_of_int Nx; val () = assert (Nx >= 0); val Nx = size1_of_int1 Nx
  val Ny = int1_of_int Ny; val () = assert (Ny >= 0); val Ny = size1_of_int1 Ny
  val pts = $ARRAY2.sub (thePointlstArray, Nx, Ny)
  val () = $ARRAY2.update (thePointlstArray, Nx, Ny, list0_cons (pt, pts))
} (* end of [do_one_round] *)

fun do_all (): int = loop (0, 0(*res*)) where {
  fun loop (i: int, res: int): int =
    if i < NN then loop (i+1, res + do_one_round ()) else res
  // end of [loop]  
} // end of [do_all]

(* ****** ****** *)

dynload "libats/smlbas/DATS/array2.dats"

(* ****** ****** *)

implement main () = let
  val () = $Rand.srand48_with_time ()
  val PI = 2.0 * do_all () / (N * (N - 1))
in
  printf ("PI = %.6f\n", @(PI))
end (* end of [main] *)

(* ****** ****** *)
