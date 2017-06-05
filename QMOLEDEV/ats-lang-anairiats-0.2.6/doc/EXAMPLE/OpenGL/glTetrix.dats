(*
**
** A simple implementation of the tetrix game
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: January, 2009
**
*)

(* ****** ****** *)

//
// Usage:
//
//   'i' : counterclockwise rotation
//   'k' : clockwise rotation
//   'j' : horizontal move to the left
//   'l' : horizontal move to the right
//   ' ' : free fall
//
//   'n' : show/hide the next piece

//   'a' : acceleration
//   'd' : deceleration

//
//   ESC : exit
//

(* ****** ****** *)

staload "libc/SATS/random.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"
staload "contrib/GL/SATS/glut.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/matrix.dats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

%{^

static ats_void_type
  mainats (ats_int_type argc, ats_ptr_type argv) ;

%}

(* ****** ****** *)

// flag = 1/0: show/hide the current shape
extern fun glTetrix_display_main (flag: int): void
extern fun glTetrix_display (): void = "glTetrix_display"

extern fun glTetrix_finalize (): void
extern fun glTetrix_initialize (): void = "glTetrix_initialize"

(* ****** ****** *)

fn drawRectangle
  (x0: double, y0: double, wd: double, ht: double) : void = let
  val (pf_begin | ()) = glBegin (GL_POLYGON)
  val () = glVertex3d (x0     , y0     , 0.0)
  val () = glVertex3d (x0 + wd, y0     , 0.0)
  val () = glVertex3d (x0 + wd, y0 + ht, 0.0)
  val () = glVertex3d (x0     , y0 + ht, 0.0)
  val () = glEnd (pf_begin | (*none*))
in
  // empty
end // end of [drawRectangle]

(* ****** ****** *)

#define FRAME_X 12
#define FRAME_Y 24
#define FRAME_MARGIN 4

val FRAME_MAX = let
  var max: int = FRAME_X
  val () = if (max < FRAME_Y) then (max := FRAME_Y)
in
  max + FRAME_MARGIN
end // end of [FRAME_MAX]

val FRAME_unit = 1.0 / double_of_int (FRAME_MAX)

val FRAME_wd = FRAME_X * FRAME_unit
val FRAME_ht = FRAME_Y * FRAME_unit
val FRAME_xbase = (1.0 - FRAME_wd) / 2
and FRAME_ybase=  (1.0 - FRAME_ht) / 2

(* ****** ****** *)

// a color is really just an index
abst@ype color_t = $extype "ats_int_type"
extern typedef "color_t" = color_t

extern fun color_index_get (c: color_t): int
  = "color_index_get"

extern fun color_is_none (c: color_t): bool // there is no color
  = "color_is_none"

extern fun color_is_some (c: color_t): bool // there is some color
  = "color_is_some"

macdef NONE_color = $extval (color_t, "-1")
macdef FRAME_background_color = $extval (color_t, "0")
macdef SHAPE0_color = $extval (color_t, "1")
macdef SHAPE1_color = $extval (color_t, "2")
macdef SHAPE2_color = $extval (color_t, "3")
macdef SHAPE3_color = $extval (color_t, "4")
macdef SHAPE4_color = $extval (color_t, "5")
macdef SHAPE5_color = $extval (color_t, "6")
macdef SHAPE6_color = $extval (color_t, "7")

extern fun eq_color_color (c1: color_t, c2: color_t): bool
  = "eq_color_color"
overload = with eq_color_color

extern fun neq_color_color (c1: color_t, c2: color_t): bool
  = "neq_color_color"
overload <> with neq_color_color

(* ****** ****** *)

abstype shape_t (m: int, n: int)

typedef shape0 = [m,n:nat] shape_t (m, n)

extern fun shape_make {m,n:nat}
  (xlen: int m, ylen: int n, mat: matrix (color_t, m, n), pts: int): shape_t (m, n)

extern fun shape_xlen_get {m,n:nat} (S: shape_t (m, n)): int m
extern fun shape_ylen_get {m,n:nat} (S: shape_t (m, n)): int n
extern fun shape_matrix_get {m,n:nat} (S: shape_t (m, n)): matrix (color_t, m, n)
extern fun shape_points_get (S: shape0): int 

(* ****** ****** *)

typedef rotkind = natLt 4
extern typedef "rotkind_t" = rotkind

#define ROTKIND_000 0
#define ROTKIND_090 1
#define ROTKIND_180 2
#define ROTKIND_270 3

extern fun the_rotkind_get (): rotkind = "the_rotkind_get"
extern fun the_rotkind_set (rk: rotkind): void = "the_rotkind_set"

// flag = 1: draw; flag = 0: undraw
extern fun shape_draw_atrot
  (flag: int, S: shape0, ix_center: int, iy_center: int, rk: rotkind): void

(* ****** ****** *)

#define NSHAPE 7

extern val theShapeArray : array (shape0, NSHAPE)
extern val theShapeColorArray : array (color_t, NSHAPE)

(* ****** ****** *)

extern val theNextShapeRef : ref (shape0)
extern val theCurrentShapeRef : ref (shape0)

(* ****** ****** *)

extern fun theNextShapeShow_get (): int
extern fun theNextShapeShow_toggle (): void

local

val toggle = ref_make_elt<int> (1) // it is on by default

in

implement theNextShapeShow_get () = !toggle
implement theNextShapeShow_toggle () = (!toggle := 1 - !toggle)

end // end of [local]

(* ****** ****** *)

extern fun theGameOver_get (): bool
extern fun theGameOver_set (): void

extern fun theGameLevel_get (): int

extern fun theTimerTimeInterval_get (): uint
extern fun theTimerTimeInterval_inc (): void
extern fun theTimerTimeInterval_dec (): void

local

val theGameOver = ref_make_elt<bool> (false)
val level = ref_make_elt<int> (1)
val interval = ref_make_elt<double> (1000.0)

in

implement theGameOver_get () = !theGameOver
implement theGameOver_set () = !theGameOver := true

implement theGameLevel_get () = !level

implement theTimerTimeInterval_get () =
 let val ti = !interval in uint_of_double ti end
// end of [theTimerTimeInterval_get]

implement theTimerTimeInterval_inc () = let
  val ti: double = 1.25 * !interval
in
  if ti <= 1000.0 then begin
    !interval := ti; !level := !level - 1
  end
end // end of [theTimerTimeInterval_inc]

implement theTimerTimeInterval_dec () = let
  val ti: double = !interval / 1.25
in
  if ti >= 20.0 then begin
    !interval := ti; !level := !level + 1
  end
end // end of [theTimerTimeInterval_dec]

end // end of [local]

(* ****** ****** *)

extern fun theTotalScore_get (): int
extern fun theTotalScore_incby (pts: int): void

local

#define ACCTHRESHOLD 1000

val theTotalScoreRef = ref_make_elt<int> (0)

in

implement theTotalScore_get () = !theTotalScoreRef

implement theTotalScore_incby (pts) = let
  val score = !theTotalScoreRef; val score_new = score + pts
  val () = let
    val level = theGameLevel_get ()
  in
    if score_new > ACCTHRESHOLD * level then begin
      theTimerTimeInterval_dec () // acceleration by decreasing the time interval
    end // end of [if]
  end // end of [val]
in
  !theTotalScoreRef := score_new
end // end of [theTotalScore_incby]

end // end of [local]

(* ****** ****** *)

fn FRAME_glClear (): void =
  glClear (GL_COLOR_BUFFER_BIT)

fn FRAME_glColor3d (): void =
  glColor3d (0.0, 0.0, 0.0) // black

val FRAME_matrix: matrix (color_t, FRAME_X, FRAME_Y) =
  matrix_make_elt (FRAME_X, FRAME_Y, NONE_color)
  
extern fun FRAME_row_flash (j0: natLt FRAME_Y): void

implement FRAME_row_flash (j0: natLt FRAME_Y) = let
  #define _X FRAME_X; #define _Y FRAME_Y
  // array allocation in the stack frame
  var !p_arr with pf_arr = @[color_t][_X](NONE_color)
  fn restore (arr: &(@[color_t][_X])):<cloref1> void = let
    var i: natLte _X // uninitialized
  in
    for (i := 0; i < _X; i := i+1) FRAME_matrix[i,_Y,j0] := arr.[i]
  end // end of [restore]
  fn disappear (arr: &(@[color_t][_X])):<cloref1> void = let
    var i: natLte _X // uninitialized
  in
    for (i := 0; i < _X; i := i+1) FRAME_matrix[i,_Y,j0] := NONE_color
  end // end of [disappear]
  val () = save (!p_arr) where {
    fn save (arr: &(@[color_t][_X])):<cloref1> void = let
      var i: natLte _X // uninitialized
    in
      for (i := 0; i < _X; i := i+1) arr.[i] := FRAME_matrix[i,_Y,j0]
    end // end of [save]
  } // end of [val]
  var n: int = 6 // flash six times
in
  while (n > 0) let
    val () = n := n - 1
    val () = disappear (!p_arr)
    val () = glTetrix_display_main (0) // theCurrentShape is aborbed!
    val () = usleep (25000)
    val () = restore (!p_arr)
    val () = glTetrix_display_main (0) // theCurrentShape is aborbed!
    val () = usleep (25000)
  in
    // empty
  end // end of [while]
end // end of [FRAME_row_flash]

extern fun FRAME_row_remove (j0: natLt FRAME_Y): void
  = "FRAME_row_remove"

implement FRAME_row_remove (j0) = let
  fn remove_one (j: natLt (FRAME_Y - 1)): void = let
    var i: natLte (FRAME_X) // unintialized
  in
    for (i := 0; i < FRAME_X; i := i+1)
      FRAME_matrix[i,FRAME_Y,j] := FRAME_matrix[i,FRAME_Y,j+1]
  end // end of [remove_one] 
  fn remove_last () = let
    var i: natLte (FRAME_X) // unintialized
  in
    for (i := 0; i < FRAME_X; i := i+1)
      FRAME_matrix[i,FRAME_Y,FRAME_Y-1] := NONE_color
  end // end of [remove_last]
  var j: natLte (FRAME_Y)
  val () = for (j := j0; j < FRAME_Y - 1; j := j + 1) remove_one (j)
  val () = remove_last ()
in
  // empty
end // end of [FRAME_row_remove]

(* ****** ****** *)

#define POINTS_FOR_FILLED_ROW 100

extern fun FRAME_rows_remove_if (): void = "FRAME_rows_remove_if"

implement FRAME_rows_remove_if () = let
  fn test (j: natLt FRAME_Y): bool = let
    var ans: bool = true
    var i: Nat // uninitialized
    val () = for (i := 0; i < FRAME_X; i := i+1) let
      val c = FRAME_matrix[i, FRAME_Y, j]
    in
      if color_is_none c then (ans := false; break) else ()
    end // end of [val]
  in
    ans
  end // end of [test]
  var j: Nat = 0
in
  while (j < FRAME_Y) (
    if test (j) then let
      val () = theTotalScore_incby (POINTS_FOR_FILLED_ROW)
    in
      FRAME_row_flash (j); FRAME_row_remove (j)
    end else (j := j+1)
  ) // end of [while]
end // end of [FRAME_rows_remove]

(* ****** ****** *)

extern fun the_ix_center_get (): int = "the_ix_center_get"
extern fun the_ix_center_set (x: int): void = "the_ix_center_set"

extern fun the_iy_center_get (): int = "the_iy_center_get"
extern fun the_iy_center_set (y: int): void = "the_iy_center_set"

(* ****** ****** *)

staload "libc/SATS/math.sats"

fn drawArc {n:int | n >= 1} (
    pf: !glBegin_v
  | x0: double, y0: double
  , radius: double, ang_init: double, ang_delta: double
  , n: int n
  ) : void = let
  val theta = (ang_delta / n)
  fun loop {i:nat | i <= n}
    (i: int i, theta_i: double):<cloref1> void = let
    val () = glVertex3d
      (x0 + radius * cos (theta_i), y0 + radius * sin (theta_i), 0.0)
  in
    if i < n then loop (i + 1, theta_i + theta)
  end // end of [loop]
  val () = loop (0, ang_init)
in
  // empty
end // end of [drawArc]

#define PI 3.1415926535898

// a plain vanilla version
fn drawBlock (ix: int, iy: int): void = let
  #define NARC 8
  #define NCORNER 3
  val u = FRAME_unit
  val u_N0 = u / NCORNER
  val u_N1 = u - u_N0
  val x0 = ix * u and y0 = iy * u
  val (pf_begin | ()) = glBegin (GL_POLYGON)
(*
  // kind of funky looking
  val () = drawArc (pf_begin | x0 + u_N0, y0 + u_N0, u_N0, ~PI  , ~PI/2, NARC)
  val () = drawArc (pf_begin | x0 + u_N1, y0 + u_N0, u_N0, ~PI/2,  0.0 , NARC)
  val () = drawArc (pf_begin | x0 + u_N1, y0 + u_N1, u_N0,  0.0 ,  PI/2, NARC)
  val () = drawArc (pf_begin | x0 + u_N0, y0 + u_N1, u_N0,  PI/2,  PI  , NARC)
*)
  val () = drawArc (pf_begin | x0 + u_N0, y0 + u_N0, u_N0, ~PI  , PI/2, NARC)
  val () = drawArc (pf_begin | x0 + u_N1, y0 + u_N0, u_N0, ~PI/2, PI/2, NARC)
  val () = drawArc (pf_begin | x0 + u_N1, y0 + u_N1, u_N0,  0.0 , PI/2, NARC)
  val () = drawArc (pf_begin | x0 + u_N0, y0 + u_N1, u_N0,  PI/2, PI/2, NARC)

  val () = glEnd (pf_begin | (*none*))
in
  // empty
end // end of [drawBlock]

// a placeholder
fn glColor3d_color (c: color_t): void = let
  val index = color_index_get (c)
in
  case+ index of
  | 1(*SHAPE0*) => glColor3d (1.0, 0.0, 0.0) // red
  | 2(*SHAPE1*) => glColor3d (0.0, 1.0, 0.0) // green
  | 3(*SHAPE2*) => glColor3d (0.5, 0.5, 1.0) // light blue
  | 4(*SHAPE3*) => glColor3d (1.0, 1.0, 0.0) // yellow
  | 5(*SHAPE4*) => glColor3d (1.0, 1.0, 0.0) // yellow
  | 6(*SHAPE3*) => glColor3d (1.0, 0.475, 0.475) // pink?
  | 7(*SHAPE4*) => glColor3d (1.0, 0.475, 0.475) // pink?
  | _ => glColor3d (1.0, 1.0, 1.0)
end // end of [glColor3d_color]

// flag = 1: draw; flag = 0: undraw
fn shape_draw (flag: int, S: shape0): void = let
  fn loop_row {m,n:nat} (
    flag: int
  , mat: matrix (color_t, m, n), m: int m, n: int n, j: natLt n
  ) : void = let
    var i: Nat // uninitialize
  in
    for (i := 0; i < m; i := i + 1) let
      val c = mat[i, n, j]
    in
      if color_is_some (c) then let
        val () = begin
          if flag > 0 then glColor3d_color (c) else FRAME_glColor3d ()
        end // end of [val]
        val () = drawBlock (i, j)
      in
        // empty
      end // end of [if]
    end // end of [for]
  end // end of [loop_row]
  stavar m: int and n: int
  val xlen: int m = shape_xlen_get (S)
  val ylen: int n = shape_ylen_get (S)
  val mat = shape_matrix_get (S)
  var j: Nat // uninitialized
in
  for (j := 0; j < ylen; j := j + 1) loop_row (flag, mat, xlen, ylen, j)
end // end of [shape_draw]

(* ****** ****** *)

implement shape_draw_atrot
  (flag, S, ix_center, iy_center, rk) = let
  val xlen = shape_xlen_get S
  val ylen = shape_ylen_get S
  val (pf_matrix | ()) = glPushMatrix ()
  val u = FRAME_unit
  val () = glTranslated (FRAME_xbase, FRAME_ybase, 0.0)
  val () = glTranslated (ix_center * u, iy_center * u, 0.0)
  val () = case+ rk of
    | ROTKIND_000 => ()
    | ROTKIND_090 => glRotated (90.0, 0.0, 0.0, 1.0)
    | ROTKIND_180 => glRotated (180.0, 0.0, 0.0, 1.0)
    | ROTKIND_270 => glRotated (270.0, 0.0, 0.0, 1.0)
  // end of [val]
  val xlen2 = xlen / 2 and ylen2 = ylen / 2
  val () = glTranslated (~xlen2 * u, ~ylen2 * u, 0.0)
  val () = shape_draw (flag, S)
  val () = glPopMatrix (pf_matrix | (*none*))
in
  // empty
end // end of [shape_draw_atrot]

(* ****** ****** *)

extern
fun shape_absorb_atrot (
  flag: int, S: shape0, ix_center: int, iy_center: int, rk: rotkind
) : int // end of [shape_absorb_atrot]

implement shape_absorb_atrot
  (flag, S, ix_center, iy_center, rk) = let
  stavar m: int and n: int
  val xlen: int m = shape_xlen_get (S)
  and ylen: int n = shape_ylen_get (S)
  val xlen2 = xlen / 2 and ylen2 = ylen / 2
  val mat = shape_matrix_get (S)
  fn action (
      c: color_t
    , frame_i: natLt FRAME_X, frame_j: natLt FRAME_Y
    , ans: &int
    ) :<cloref1> void =
    if flag > 0 then begin
      FRAME_matrix[frame_i, FRAME_Y, frame_j] := c
    end else let
      val c0 = FRAME_matrix[frame_i, FRAME_Y, frame_j]
    in
      if color_is_some (c0) then (ans := 1) else ()
    end // end of [if]
  // end of [action]
in
  case+ rk of
  | ROTKIND_000 => loop1 (0) where {
      val ix_base: int = ix_center - xlen2
      and iy_base: int = iy_center - ylen2
      fn* loop1 (i: natLte m):<cloref1> int =
        if i < xlen then loop2 (i, 0) else 0
      and loop2 (i: natLt m, j: natLte n):<cloref1> int = let
        var ans: int = 0
      in
        if j < ylen then let
          val c = mat[i, ylen, j]
          val () = if color_is_some c then let
            val frame_i = int1_of_int (ix_base + i)
            val () = assert (0 <= frame_i)
            val () = assert (frame_i < FRAME_X)
            val frame_j = int1_of_int (iy_base + j)
            val () = assert (0 <= frame_j)
            val () = assert (frame_j < FRAME_Y)
          in
            action (c, frame_i, frame_j, ans)
          end // end of [if]
        in
          if ans = 0 then loop2 (i, j+1) else ans
        end else begin
          loop1 (i + 1)
        end // end of [if]
      end // end of [loop2]
    } // end of [ROTKIND_000]
  | ROTKIND_090 => loop1 (0) where {
      val ix_base: int = ix_center + ylen2
      and iy_base: int = iy_center - xlen2
      fn* loop1 (i: natLte m):<cloref1> int =
        if i < xlen then loop2 (i, 0) else 0
      and loop2 (i: natLt m, j: natLte n):<cloref1> int = let
        var ans: int = 0
      in
        if j < ylen then let
          val c = mat[i, ylen, j]
          val () = if color_is_some c then let
            val frame_i = int1_of_int (ix_base - j - 1)
            val () = assert (0 <= frame_i)
            val () = assert (frame_i < FRAME_X)
            val frame_j = int1_of_int (iy_base + i)
            val () = assert (0 <= frame_j)
            val () = assert (frame_j < FRAME_Y)
          in
            action (c, frame_i, frame_j, ans)
          end // end of [if]
        in
          if ans = 0 then loop2 (i, j+1) else ans
        end else begin
          loop1 (i + 1)
        end // end of [if]
      end // end of [loop2]
    } // end of [ROTKIND_090]
  | ROTKIND_180 => loop1 (0) where {
      val ix_base: int = ix_center + xlen2
      and iy_base: int = iy_center + ylen2
      fn* loop1 (i: natLte m):<cloref1> int =
        if i < xlen then loop2 (i, 0) else 0
      and loop2 (i: natLt m, j: natLte n):<cloref1> int = let
        var ans: int = 0
      in
        if j < ylen then let
          val c = mat[i, ylen, j]
          val () = if color_is_some c then let
            val frame_i = int1_of_int (ix_base - i - 1)
            val () = assert (0 <= frame_i)
            val () = assert (frame_i < FRAME_X)
            val frame_j = int1_of_int (iy_base - j - 1)
            val () = assert (0 <= frame_j)
            val () = assert (frame_j < FRAME_Y)
          in
            action (c, frame_i, frame_j, ans)
          end // end of [if]
        in
          if ans = 0 then loop2 (i, j+1) else ans
        end else begin
          loop1 (i + 1)
        end // end of [if]
      end // end of [loop2]
    } // end of [ROTKIND_180]
  | ROTKIND_270 => loop1 (0) where {
      val ix_base: int = ix_center - ylen2
      and iy_base: int = iy_center + xlen2
      fn* loop1 (i: natLte m):<cloref1> int =
        if i < xlen then loop2 (i, 0) else 0
      and loop2 (i: natLt m, j: natLte n):<cloref1> int = let
        var ans: int = 0
      in
        if j < ylen then let
          val c = mat[i, ylen, j]
          val () = if color_is_some c then let
            val frame_i = int1_of_int (ix_base + j)
            val () = assert (0 <= frame_i)
            val () = assert (frame_i < FRAME_X)
            val frame_j = int1_of_int (iy_base - i - 1)
            val () = assert (0 <= frame_j)
            val () = assert (frame_j < FRAME_Y)
          in
            action (c, frame_i, frame_j, ans)
          end // end of [if]
        in
          if ans = 0 then loop2 (i, j+1) else ans
        end else begin
          loop1 (i + 1)
        end // end of [if]
      end // end of [loop2]
    } // end of [ROTKIND_270]
end // end of [shape_absorb_atrot]

(* ****** ****** *)

extern
fun shape_position_test (
  S: shape0, ix_center: int, iy_center: int, rk: rotkind
) : bool // end of [shape_position_test]

implement
shape_position_test (
  S, ix_center, iy_center, rk
) = let
  val xlen = shape_xlen_get S and ylen = shape_ylen_get S
  val xlen20 = xlen / 2 and ylen20 = ylen / 2
  val xlen21 = xlen - xlen20 and ylen21 = ylen - ylen20
  var ix_ll: int = 0 and iy_ll: int = 0
  var ix_ur: int = 0 and iy_ur: int = 0
  val () = case+ rk of
    | ROTKIND_000 => begin
        ix_ll := ix_center - xlen20; iy_ll := iy_center - ylen20;
        ix_ur := ix_center + xlen21; iy_ur := iy_center + ylen21;
      end // end of [ROTKIND_000]
    | ROTKIND_090 => begin
        ix_ll := ix_center - ylen21; iy_ll := iy_center - xlen20;
        ix_ur := ix_center + ylen20; iy_ur := iy_center + xlen21;
      end // end of [ROTKIND_000]
    | ROTKIND_180 => begin
        ix_ll := ix_center - xlen21; iy_ll := iy_center - ylen21;
        ix_ur := ix_center + xlen20; iy_ur := iy_center + ylen20;
      end // end of [ROTKIND_000]
    | ROTKIND_270 => begin
        ix_ll := ix_center - ylen20; iy_ll := iy_center - xlen21;
        ix_ur := ix_center + ylen21; iy_ur := iy_center + xlen20;
      end // end of [ROTKIND_000]
  // end of [val]
in
  if ix_ll < 0 then false
  else if iy_ll < 0 then false
  else if ix_ur > FRAME_X then false
  else if iy_ur > FRAME_Y then false
  else let
    val ans = shape_absorb_atrot (0(*flag*), S, ix_center, iy_center, rk)
  in
    if ans > 0 then false (*collision*) else true (*no collision*)
  end // end of [if]
end // end of [shape_position_test]

(* ****** ****** *)

extern fun theCurrentShape_absorb (): void

implement theCurrentShape_absorb () = let
  val S = !theCurrentShapeRef
  val ix_center = the_ix_center_get ()
  and iy_center = the_iy_center_get ()
  val rk = the_rotkind_get ()
  val _(*0*) = shape_absorb_atrot (1(*flag*), S, ix_center, iy_center, rk)
  val () = theTotalScore_incby (shape_points_get S)
  val () = FRAME_rows_remove_if ()
in
  // empty
end // end of [theCurrentShape_absorb]

(* ****** ****** *)

extern fun theCurrentShape_update (): void

implement theCurrentShape_update () = let
  val S = !theNextShapeRef
  val () = !theCurrentShapeRef := S
  val ylen = shape_ylen_get S
  val rk_init = ROTKIND_000
  val ix_center = (FRAME_X / 2)
  val iy_center = FRAME_Y - (ylen + 1) / 2
  val () = let // collision test
    val ans = shape_absorb_atrot
      (0(*flag*), S, ix_center, iy_center, rk_init)
  in
    if ans > 0 then // there is collision
      glTetrix_finalize () // the game is over
    // end of [if]
  end // end of [val]
  val () = the_ix_center_set (ix_center)
  val () = the_iy_center_set (iy_center)
  val () = the_rotkind_set (rk_init)
  val n = randint (NSHAPE)
  val S = array_get_elt_at (theShapeArray, size1_of_int1 n)
  val () = !theNextShapeRef := S
in
  // empty
end // end of [theCurrentShape_update ()]

(* ****** ****** *)

extern fun theCurrentShape_xmove_if (delta: int): int
  = "theCurrentShape_xmove_if"

implement theCurrentShape_xmove_if (delta) = let
  val S = !theCurrentShapeRef
  val ix_center = the_ix_center_get () and iy_center = the_iy_center_get ()
  val rk = the_rotkind_get ()
  val ix_center_new = ix_center + delta
  val test = shape_position_test (S, ix_center_new, iy_center, rk)
in
  if test then (the_ix_center_set (ix_center_new); delta) else 0
end // end of [theCurrentShape_xmove_if]

(* ****** ****** *)

extern fun theCurrentShape_ymove_if (delta: int): int
  = "theCurrentShape_ymove_if"

implement theCurrentShape_ymove_if (delta) = let
  val S = !theCurrentShapeRef
  val ix_center = the_ix_center_get () and iy_center = the_iy_center_get ()
  val rk = the_rotkind_get ()
  val iy_center_new = iy_center + delta
  val test = shape_position_test (S, ix_center, iy_center_new, rk)
in
  if test then begin
    the_iy_center_set (iy_center_new); delta
  end else begin
    0 // [0] indicates that no move is made
  end // end of [if]
end // end of [theCurrentShape_ymove_if]

extern fun theCurrentShape_freefall (): void
  = "theCurrentShape_freefall"

implement theCurrentShape_freefall (): void = let
  #define FreeFallTimeInterval 2048
  fun loop (S: shape0): void = let
    val () = usleep (FreeFallTimeInterval)
    val res = theCurrentShape_ymove_if (~1)
  in
    if res <> 0 then let
      val () = glTetrix_display_main (1)
    in
      loop (S)
    end else let
      val () = theCurrentShape_absorb ()
      val () = theCurrentShape_update ()
    in
      // empty
    end // end of [if]
  end // end of [loop]
in
  loop (!theCurrentShapeRef)
end // end of [theCurrentShape_freefall]

(* ****** ****** *)

extern fun theCurrentShape_rotate_if (delta: Nat): int
  = "theCurrentShape_rotate_if"

implement theCurrentShape_rotate_if (delta) = let
  val S = !theCurrentShapeRef
  val ix_center = the_ix_center_get () and iy_center = the_iy_center_get ()
  val rk = the_rotkind_get ()
  val rk_new = (rk + delta) nmod1 4
  val test = shape_position_test (S, ix_center, iy_center, rk_new)
in
  if test then (the_rotkind_set (rk_new); delta) else 0
end // end of [theCurrentShape_xmove_if]

(* ****** ****** *)

fn FRAME_draw () = begin
  drawRectangle (FRAME_xbase, FRAME_ybase, FRAME_wd, FRAME_ht)
end // end of [FRAME_draw]

fn FRAME_matrix_draw () = let
  var i: Nat and j: Nat
  val (pf_matrix | ()) = glPushMatrix ()
  val () = glTranslated (FRAME_xbase, FRAME_ybase, 0.0)
  val () = for* (j: Nat?) =>
    (i := 0; i < FRAME_X; i := i + 1) begin
    for* (i: natLt FRAME_X) =>
      (j := 0; j < FRAME_Y; j := j + 1) let
      val c = FRAME_matrix[i, FRAME_Y, j]
    in
      if :(i: natLt FRAME_X) => color_is_some (c) then let
        val () = glColor3d_color (c); val () = drawBlock (i, j)
      in
        // empty
      end // end of [if]
    end // end of [for]
  end // end of [for]
  val () = glPopMatrix (pf_matrix | (*none*))
in
  // empty
end // end of [FRAME_matrix_draw]

implement glTetrix_display_main (flag) = let
  val () = FRAME_glClear ()
  val () = FRAME_glColor3d ()
  val () = FRAME_draw ()
  val () = FRAME_matrix_draw ()
  val () = if theNextShapeShow_get () > 0 then let
    val S = !theNextShapeRef
      val xlen = shape_xlen_get (S)
      val X = ~2 * ((xlen+1) / 2)
      val ylen = shape_ylen_get (S)
      val Y = FRAME_Y - (ylen+1) / 2
    in
      shape_draw_atrot (1(*flag*), S, X, Y, ROTKIND_000)
    end // end of [if]
  // end of [val]
  val () =  if flag > 0 then let
    val S = !theCurrentShapeRef
    val rotkind = the_rotkind_get ()
    val ix_center = the_ix_center_get ()
    val iy_center = the_iy_center_get ()
  in
    shape_draw_atrot (1, S, ix_center, iy_center, rotkind)
  end // end of [val]
(*
  val () = begin
    prerr "glTetrix_display: ix_center = "; prerr ix_center; prerr_newline ()
  end // end of [val]
  val () = begin
    prerr "glTetrix_display: iy_center = "; prerr iy_center; prerr_newline ()
  end // end of [val]
*)
  val () = glFlush ()
  val () = glutSwapBuffers ()
in
  // empty
end // end of [display]

implement glTetrix_display () = glTetrix_display_main (1) // show the current shape

(* ****** ****** *)

fn FRAME_glClearColor (): void =
  glClearColor (1.0, 1.0, 1.0, 0.0) // white background

implement glTetrix_initialize () = let
  val () = srand48_with_time ()
  val () = theCurrentShape_update ()
  val () = theCurrentShape_update () // yes, twice!
  val () = FRAME_glClearColor ()
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val () = glOrtho (0.0, 1.0, 0.0, 1.0, ~1.0, 1.0)
in
  // empty
end // end of [glTetrix_initialize]

(* ****** ****** *)

(*

implement glTetrix_finalize () = let
  val level = theGameLevel_get ()
  val score = theTotalScore_get ()
  val () = begin
    print "Game Over!\n";
    printf ("The final level of the game is %i.\n", @(level));
    printf ("The final score of the game is %i.\n", @(score));
  end // end of [val]
in
  exit (0)
end // end of [glTetrix_finalize]

*)

%{^

static inline
ats_void_type
glTetrix_glutBitmapCharacter
  (ats_ref_type ft, ats_char_type c) {
  glutBitmapCharacter ((void*)ft, (int)c) ; return ;
} /* end of [glTetrix_glutBitmapCharacter] */

%}

implement glTetrix_finalize () = let
  val () = theGameOver_set ()

  val () = glutDisplayFunc (lam () => ())
  val () = glutKeyboardFunc (f) where {
    fn f (key: uchar, ix: int, iy: int): void = exit (0)
  }

  val str_gameover = "Game Over!"
  val level = theGameLevel_get ()
  val score = theTotalScore_get ()
//
  #define p2s string1_of_strptr
  val str_level = p2s (sprintf ("The final level of the game is %i.", @(level)))
  val str_score = p2s (sprintf ("The final score of the game is %i.", @(score)))
//
  abstype FONTref // a reference
  fun show_string {n:nat}
    (ft: FONTref, str: string n)
    : void = loop (ft, str, 0) where {
    extern fun glutBitmapCharacter (ft: FONTref, c: char): void
      = "glTetrix_glutBitmapCharacter"
    fun loop {i:nat | i <= n} .<n-i>.
      (ft: FONTref, str: string n, i: size_t i): void =
      if string_isnot_at_end (str, i) then let
        val () = glutBitmapCharacter (ft, str[i]) in loop (ft, str, i+1)
      end // end of [if]
  } // end of [show_string]

  macdef TIMES_ROMAN_24 = $extval (FONTref, "GLUT_BITMAP_TIMES_ROMAN_24")
  macdef HELVETICA_18 = $extval (FONTref, "GLUT_BITMAP_HELVETICA_18")

  val () = glClear(GL_COLOR_BUFFER_BIT)
  val () = glColor3d (0.0, 0.0, 0.0)
  val () = glMatrixMode(GL_MODELVIEW)
  val () = glLoadIdentity ()
  val () = glRasterPos2d (0.25, 0.75)
  val () = show_string (TIMES_ROMAN_24, str_gameover)
  val () = glRasterPos2d (0.25, 0.50)
  val () = show_string (HELVETICA_18, str_level)
  val () = glRasterPos2d (0.25, 0.45)
  val () = show_string (HELVETICA_18, str_score)
  val () = glFlush ()
  val () = glutSwapBuffers ()
in
  // exit (0)
end // end of [glTetrix_finalize]

(* ****** ****** *)

extern castfn char_of_uchar (c: uchar): char = "char_of_uchar"

extern fun glTetrix_keyboard (key: uchar, ix: int, iy: int): void
  = "glTetrix_keyboard"

implement glTetrix_keyboard (key, _(*ix*), _(*iy*)) = let
  val key = char_of_uchar (key)
in
  case+ key of
  | 'i' => let // counterclockwise rotation
      val _ = theCurrentShape_rotate_if (1) in glutPostRedisplay ()
    end
  | 'k' => let // clockwise rotation
      val _ = theCurrentShape_rotate_if (3) in glutPostRedisplay ()
    end
  | 'j' => let // left horizontal move
      val _ = theCurrentShape_xmove_if (~1) in glutPostRedisplay ()
    end
  | 'l' => let // right horizontal move
      val _ = theCurrentShape_xmove_if ( 1) in glutPostRedisplay ()
    end
  | ' ' => let
      val () = theCurrentShape_freefall () in glutPostRedisplay ()
    end
  | 'n' => let
      val () = theNextShapeShow_toggle () in glutPostRedisplay ()
    end
  | 'a' => theTimerTimeInterval_dec () // acceleration
  | 'd' => theTimerTimeInterval_inc () // deceleration
  | '\033' => exit (0)
  | _ => ()
end // end of [glTetrix_keyboard]

(* ****** ****** *)

extern fun glTetrix_timer (flag: int): void = "glTetrix_timer"

implement glTetrix_timer
  (flag) = if ~theGameOver_get () then let
(*
  val () = begin
    prerr "glTetrix_timer: flag = "; prerr flag; prerr_newline ()
  end // end of [va]
*)
  val res = theCurrentShape_ymove_if (~1)
(*
  val () = begin
    prerr "glTetrix_timer: res = "; prerr res; prerr_newline ()
  end // end of [va]
*)
  val () = if res = 0 then begin
    theCurrentShape_absorb (); theCurrentShape_update ()
  end // end of [val]
  val () = glutPostRedisplay ()
in
  glutTimerFunc (theTimerTimeInterval_get (), glTetrix_timer, 0) ;
end // end of [glTetrix_timer]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

typedef shape_t (m: int, n: int) = '{
  xlen= int m
, ylen= int n
, matrix= matrix (color_t, m, n)
, points= int
}

(* ****** ****** *)

local

assume shape_t (m: int, n: int) = shape_t (m, n)

in

implement shape_make (xlen, ylen, mat, pts) =
  '{ xlen= xlen, ylen= ylen, matrix= mat, points= pts }
// end of [shape_make]

implement shape_xlen_get (S) = S.xlen
implement shape_ylen_get (S) = S.ylen
implement shape_matrix_get (S) = S.matrix
implement shape_points_get (S) = S.points

end // end of [local]

(* ****** ****** *)

(*

XXXX
XXXX
XXXX
XXXX

*)

#define SHAPE0_X 2
#define SHAPE0_Y 2

val SHAPE0_matrix
  : matrix (color_t, SHAPE0_X, SHAPE0_Y) =
  matrix_make_elt (SHAPE0_X, SHAPE0_Y, SHAPE0_color)
// end of [val]

#define SHAPE0_points 40

(* ****** ****** *)

(*

XX
XX
XX
XX
XX
XX
XX
XX

*)

#define SHAPE1_X 1
#define SHAPE1_Y 4

val SHAPE1_matrix
  : matrix (color_t, SHAPE1_X, SHAPE1_Y) =
  matrix_make_elt (SHAPE1_X, SHAPE1_Y, SHAPE1_color)
// end of [val]

#define SHAPE1_points 40

(* ****** ****** *)

(*

XX
XX
XXXX
XXXX
XX
XX

*)

#define SHAPE2_X 2
#define SHAPE2_Y 3

val SHAPE2_matrix
  : matrix (color_t, SHAPE2_X, SHAPE2_Y) = M where {
  val M = matrix_make_elt (SHAPE2_X, SHAPE2_Y, SHAPE2_color)
  val () = M[1, SHAPE2_Y, 0] := NONE_color
  val () = M[1, SHAPE2_Y, 2] := NONE_color
} // end of [val]

#define SHAPE2_points 50

(* ****** ****** *)

(*

XX
XX
XX
XX
XXXX
XXXX

*)

#define SHAPE3_X 2
#define SHAPE3_Y 3

val SHAPE3_matrix
  : matrix (color_t, SHAPE3_X, SHAPE3_Y) = M where {
  val M = matrix_make_elt (SHAPE3_X, SHAPE3_Y, SHAPE3_color)
  val () = M[1, SHAPE3_Y, 1] := NONE_color
  val () = M[1, SHAPE3_Y, 2] := NONE_color
} // end of [val]

#define SHAPE3_points 60

(* ****** ****** *)

(*

  XX
  XX
  XX
  XX
XXXX
XXXX

*)

#define SHAPE4_X 2
#define SHAPE4_Y 3

val SHAPE4_matrix
  : matrix (color_t, SHAPE4_X, SHAPE4_Y) = M where {
  val M = matrix_make_elt (SHAPE4_X, SHAPE4_Y, SHAPE4_color)
  val () = M[0, SHAPE4_Y, 1] := NONE_color
  val () = M[0, SHAPE4_Y, 2] := NONE_color
} // end of [val]

#define SHAPE4_points 60

(* ****** ****** *)

(*

  XX
  XX
XXXX
XXXX
XX
XX

*)

#define SHAPE5_X 2
#define SHAPE5_Y 3

val SHAPE5_matrix
  : matrix (color_t, SHAPE5_X, SHAPE5_Y) = M where {
  val M = matrix_make_elt (SHAPE5_X, SHAPE5_Y, SHAPE5_color)
  val () = M[0, SHAPE5_Y, 2] := NONE_color
  val () = M[1, SHAPE5_Y, 0] := NONE_color
} // end of [val]

#define SHAPE5_points 60

(* ****** ****** *)

(*

XX
XX
XXXX
XXXX
  XX
  XX

*)

#define SHAPE6_X 2
#define SHAPE6_Y 3

val SHAPE6_matrix
  : matrix (color_t, SHAPE6_X, SHAPE6_Y) = M where {
  val M = matrix_make_elt (SHAPE6_X, SHAPE6_Y, SHAPE6_color)
  val () = M[0, SHAPE6_Y, 0] := NONE_color
  val () = M[1, SHAPE6_Y, 2] := NONE_color
} // end of [val]

#define SHAPE6_points 60

(* ****** ****** *)
  
implement theShapeArray =
  array_make_arrsz{shape0} $arrsz(
  SHAPE0, SHAPE1, SHAPE2, SHAPE3, SHAPE4, SHAPE5, SHAPE6
) where {
  val SHAPE0 = shape_make (SHAPE0_X, SHAPE0_Y, SHAPE0_matrix, SHAPE0_points)
  val SHAPE1 = shape_make (SHAPE1_X, SHAPE1_Y, SHAPE1_matrix, SHAPE1_points)
  val SHAPE2 = shape_make (SHAPE2_X, SHAPE2_Y, SHAPE2_matrix, SHAPE2_points)
  val SHAPE3 = shape_make (SHAPE3_X, SHAPE3_Y, SHAPE3_matrix, SHAPE3_points)
  val SHAPE4 = shape_make (SHAPE4_X, SHAPE4_Y, SHAPE4_matrix, SHAPE4_points)
  val SHAPE5 = shape_make (SHAPE5_X, SHAPE5_Y, SHAPE5_matrix, SHAPE5_points)
  val SHAPE6 = shape_make (SHAPE6_X, SHAPE6_Y, SHAPE6_matrix, SHAPE6_points)
} // end of [theShapeArray]

implement theNextShapeRef = let
  val S = array_get_elt_at (theShapeArray, 0)
in
  ref_make_elt<shape0> (S)
end // end of [theNextShapeRef]

implement theCurrentShapeRef = let
  val S = array_get_elt_at (theShapeArray, 0)
in
  ref_make_elt<shape0> (S)
end // end of [theCurrentShapeRef]

(* ****** ****** *)

%{$

ats_int_type color_index_get (color_t c) {
  return c ;
}

ats_bool_type color_is_none (color_t c) {
  return (c < 0) ? ats_true_bool : ats_false_bool ;
}

ats_bool_type color_is_some (color_t c) {
  return (c >= 0) ? ats_true_bool : ats_false_bool ;
}

ats_bool_type eq_color_color (color_t c1, color_t c2) {
  return (c1 == c2) ? ats_true_bool : ats_false_bool ;
}

ats_bool_type neq_color_color (color_t c1, color_t c2) {
  return (c1 != c2) ? ats_true_bool : ats_false_bool ;
}

%}

(* ****** ****** *)

%{$

static int the_rotkind = 0 ;

rotkind_t the_rotkind_get () { return the_rotkind ; }
ats_void_type
the_rotkind_set (rotkind_t rk) { the_rotkind = rk ; return ; }

static int the_ix_center = 0 ;
static int the_iy_center = 0 ;

ats_int_type the_ix_center_get () { return the_ix_center ; }
ats_void_type
the_ix_center_set (ats_int_type x) { the_ix_center = x ; return ; }

ats_int_type the_iy_center_get () { return the_iy_center ; }
ats_void_type
the_iy_center_set (ats_int_type y) { the_iy_center = y; return  ; }

%} // end of [%{$]

(* ****** ****** *)

%{$

ats_void_type
mainats (
  ats_int_type argc
, ats_ptr_type argv
) {
  glutInit ((int*)&argc, (char**)argv) ;
  glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB) ;
  glutInitWindowSize (600, 600) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow("Tetrix Game") ;
  glTetrix_initialize () ;
  glutDisplayFunc (glTetrix_display) ;
  // glutReshapeFunc (reshape) ;
  glutKeyboardFunc (glTetrix_keyboard) ;
  glTetrix_timer (0) ;
  glutMainLoop () ;
  return ; /* deadcode */
} /* end of [mainats] */

%} // end of [%{$]

(* ****** ****** *)

(* end of [glTetrix.dats] *)
