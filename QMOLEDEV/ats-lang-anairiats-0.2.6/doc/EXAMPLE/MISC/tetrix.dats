(*
**
** A poor man's game of tetrix
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: September 2007
**
*)

(*

The code was first written in ATS/Proto in October 2006 and ported to
ATS/Geizella in September 2007. The use of linear objects here should
serve as an informative example for future reference.

*)

(* ****** ****** *)

staload "libc/SATS/random.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "prelude/DATS/array.dats"
staload "prelude/DATS/matrix.dats"
staload "prelude/DATS/reference.dats"

(* ****** ****** *)

extern fun save_set_keyboard (): void = "save_set_keyboard"

extern fun restore_keyboard (): void = "restore_keyboard"

extern fun kbhit (): bool = "kbhit"

extern fun readch (): int = "readch"

(* ****** ****** *)

#define clear "[H[2J" // clear the screen
#define home "[H" // moving the the home position (upper left corner )
#define civis "[?25l" // invisible cursor
#define cnorm "[?25h" // normal cursor

#define blink "[5m"
#define bold "[1m"
#define normal "[0m"

#define cuu "[1A" // moving up
#define cud "[1B" // moving down
#define cuf "[1C" // moving forward
#define cub "[1D" // moving backward

#define BSZ 2
#define FD 28 // 14 * BSZ 
#define FW 20 // 10 * BSZ

#define XBASE 3
#define YBASE 1

macdef XSCORE = 2
macdef YSCORE = FW + 4

macdef SPEED_LIM = 100

(* ****** ****** *)

macdef matrix_make_elt (m, n, c) =
  matrix_make_elt (size1_of_int1 ,(m), size1_of_int1 ,(n), ,(c))

val frame: matrix (char, FD+1, FW) =
  matrix_make_elt (FD+1, FW, ' ')

fn frame_get_at (i: Int, j: Int): char =
  if i < 0 then '\0' else
  if i > FD then '\0' else
  if j < 0 then '\0' else
  if j >= FW then '\0' else
    frame[i, FW, j]
// end of [frame_get_at]

fn frame_set_at (i: Int, j: Int, c: char): void =
  if i < 0 then () else
  if i > FD then () else
  if j < 0 then () else
  if j >= FW then () else
    frame[i, FW, j] := c
// end of [frame_set_at]

//

macdef ignore (x) = let val _ = ,(x) in () end

//

exception GameIsOverException

fn curs_clear (): void = print_string clear
fn curs_home (): void = print_string home

fn curs_invisible (): void = print_string civis
fn curs_normal (): void = print_string cnorm

fun curs_upward (n: Nat): void =
  if n > 0 then (print_string cuu; curs_upward (n-1))

fun curs_downward (n: Nat): void =
  if n > 0 then (print_string cud; curs_downward (n-1))

fun curs_forward (n: Nat): void =
  if n > 0 then (print_string cuf; curs_forward (n-1))

fun curs_backward (n: Nat): void =
  if n > 0 then (print_string cub; curs_backward (n-1))

fun curs_goto (m:Int, n:Int): void =
  let val m = m + XBASE and n = n + YBASE in
    curs_home ();
    if m >= 0 then curs_downward m else curs_upward (~m);
    if n >= 0 then curs_forward n else curs_backward (~n);
  end

fun print_vertical_line_up (n: Nat): void =
  if n > 0 then begin
    print '|'; print_string cub; print_string cuu;
    print_vertical_line_up (n - 1)
  end

fun print_vertical_line_dn (n: Nat): void =
  if n > 0 then begin
    print '|'; print_string cub; print_string cud;
    print_vertical_line_dn (n - 1)
  end

fun print_horizontal_line (n: Nat): void =
  if n > 0 then begin
    print '_'; print_horizontal_line (n - 1)
  end

fn print_frame (): void = begin
  print_vertical_line_dn (FD isub 1);
  print '|'; 
  print_horizontal_line FW;
  print_vertical_line_up FD;
end

fn print_block (c: char, b: bool): void =
  if c <> ' ' then
    (if b then print c else print ' ')
  else curs_forward 1

//

typedef rotate_t = natLt 4

typedef shape (m:int, n:int) = '{
  row= int m
, col= int n
, mat= matrix (char, m, n)
}

typedef shape = [m,n:pos] shape (m, n)

//

fn move_test {m,n:pos}
  (S: shape (m, n), r: rotate_t, xcen: Int, ycen: Int): bool = let
  val m = S.row and n = S.col and M = S.mat
  val m2 = nhalf m and n2 = nhalf n
  fun aux (i: natLte m, j: natLte n):<cloref1> bool =
    if i < m then
      if j < n then let
        val (x, y): (Int, Int) = case+ r of
          | 0 => (xcen - m2 + i, ycen - n2 + j)
          | 1 => (xcen - n2 + j, ycen + m2 - i - 1)
          | 2 => (xcen + m2 - i - 1, ycen + n2 - j - 1)
          | 3 => (xcen + n2 - j - 1, ycen - m2 + i)
        val c = frame_get_at (x, y)
      in
        if c = '\0' then false else begin
          if c <> ' ' then begin
            if M[i,n,j] = ' ' then aux (i, j+1) else false
          end else begin
            aux (i, j+1)
          end // end of [if]
        end
      end else begin
        aux (i+1, 0)
      end // end of [if]
    else true
  // end of [aux]
in
  aux (0, 0)
end // end of [move_test]

//

fn absorb {m,n:pos}
  (S: shape (m, n), r: rotate_t, xcen: Int, ycen: Int): void = let
  val m = S.row and n = S.col and M = S.mat
  val m2 = nhalf m and n2 = nhalf n
  fun aux (i: natLte m, j: natLte n):<cloref1> void =
    if i < m then begin
      if j < n then let
        val (x, y): (Int, Int) = case+ r of
          | 0 => (xcen - m2 + i, ycen - n2 + j)
          | 1 => (xcen - n2 + j, ycen + m2 - i - 1)
          | 2 => (xcen + m2 - i - 1, ycen + n2 - j - 1)
          | 3 => (xcen + n2 - j - 1, ycen - m2 + i)
        val c = M[i, n, j]
        val () = if c <> ' ' then frame_set_at (x, y, c)
      in
        aux (i, j+1)
      end else begin
        aux (i+1, 0)
      end // end of [if]
    end // end of [if]
  // end of [aux]
in
  aux (0, 0)
end // end of [absorb]

//

fn iter {n:nat}
  (n: int n, f: !natLt n -<cloptr1> void): void = let
  fun loop {i:nat | i <= n} (i: int i, f: !natLt n -<cloptr1> void)
    :<cloptr1> void =
    if i < n then (f i; loop (i+1, f)) else ()
in
  loop (0, f)
end // end of [iter]

fn frame_line_flash (i: natLte FD): void = let
  fun aux (n: Nat):<cloptr1> void =
    if n > 0 then let
      val f = lam (j: natLt FW): void =<cloptr1> print frame[i, FW, j]
      val () = iter (FW, f)
      val () = cloptr_free (f)
    in
      fflush_stdout ();
      curs_backward (FW);
      ignore (usleep (62500));
      iter (FW, lam j => print ' ');
      fflush_stdout ();
      curs_backward (FW);
      ignore (usleep (62500));
      aux (n-1)
    end // end of [if]
in
  print_string bold;
  curs_goto (i, 0); aux (3);
  print_string normal
end // end of [frame_line_flash]

fn frame_display (): void = let
  fun aux1 (j: natLte FW):<cloref1> void =
    if j < FW then let
      val c = frame[FD, FW, j]
    in
      if c = ' ' then print '_' else print c; aux1 (j + 1)
    end else begin
      curs_backward FW; curs_upward 1
    end // end of [if]

  fun aux2 (i: intLte FD, j: natLte FW):<cloref1> void =
    if i >= 0 then
      if j < FW then begin
        print frame[i, FW, j]; aux2 (i, j + 1)
      end else begin
        curs_backward FW; curs_upward 1; aux2 (i - 1, 0)
      end // end of [if]
    else ()
in
  curs_goto (FD, 0); aux1 (0); aux2 (FD - 1, 0)
end // end of [frame_display]

fn frame_find (): intBtw (~1, FD+1) = let // find a filled-up line
  fn* aux1 {i:nat} (i: int i):<cloref1> intBtw (~1, FD+1) =
    if i <= FD then aux2 (i, 0) else (~1)

  and aux2 {i,j:nat | i <= FD; j <= FW}
    (i: int i, j: int j):<cloref1> intBtw (~1, FD+1) =
    if j < FW then begin
      if frame[i, FW, j] <> ' ' then aux2 (i, j+1) else aux1 (i+1)
    end else begin
      i
    end // end of [if]
in
  aux1 (0)
end // end of [frame_find]

(* ****** ****** *)

fn print_usage (): void = begin
  curs_goto (XSCORE + 2, YSCORE);
  print_string ("j: left move");
  curs_goto (XSCORE + 3, YSCORE);
  print_string ("l: right move");
  curs_goto (XSCORE + 4, YSCORE);
  print_string ("i: left rotation");
  curs_goto (XSCORE + 5, YSCORE);
  print_string ("k: right rotation");
  curs_goto (XSCORE + 6, YSCORE);
  print_string ("space: fall straight");
  curs_goto (XSCORE + 8, YSCORE);
  print_string "Please press the RETURN key to start the game.";
end // end of [print_usage]

//

val total_number_of_filled_lines: ref Nat = ref_make_elt 0
fn total_number_of_filled_lines_inc ():<fun1> void = begin
  !total_number_of_filled_lines := !total_number_of_filled_lines + 1
end // end of [total_number_of_filled_lines]

//

fn print_score (): void = begin
 curs_goto (XSCORE, YSCORE + 7);
 print (!total_number_of_filled_lines);
 fflush_stdout ()
end // end of [print_score]

//

fun frame_delete (): void = let
  fn aux1 (i: natLte FD, j: natLte FD):<cloref1> void = let
    val f = lam (k: natLt FW): void =<cloptr1>
      matrix_set_elt_at__intsz (frame, i, FW, k, frame[j, FW, k])
    val () = iter (FW, f)
    val () = cloptr_free (f)
  in
    // empty
  end // end of [aux1]

  fn aux2 (i: natLte FD):<cloref1> void = let
    val f = lam (k: natLt FW): void =<cloptr1> frame[i, FW, k] := ' '
    val () = iter (FW, f)
    val () = cloptr_free (f)
  in
    // empty
  end // end of [aux2]

  fun aux3 (i: intLte FD):<cloref1> void =
    if i >= 0 then let
      val j = i-1
    in
      if j < 0 then aux2 i else aux1 (i, j); aux3 (i - 1)
    end // end of [if]

  val i = frame_find ()
in 
  if i >= 0 then begin
    total_number_of_filled_lines_inc ();
    frame_line_flash (i);
    print_score ();
    aux3 i; frame_display (); frame_delete ()
  end
end // end of [frame_delete]

//

fn display_shape_0 {m,n:pos}
  (S: shape (m, n), b: bool): void = let
  val m = S.row and n = S.col and M = S.mat
  fun aux (i: natLte m, j: natLte n):<cloref1> void =
    if i < m then
      if j < n then begin
        print_block (M[i, n, j], b); aux (i, j + 1)
      end else begin
        curs_backward n; curs_downward 1; aux (i+1, 0)
      end // end of [if]
    else begin
      fflush_stdout ()
    end
in
  aux (0, 0)
end // end of [display_shape_0]

//

fn display_shape_1 {m,n:pos}
  (S: shape (m, n), b: bool): void = let
  val m = S.row and n = S.col and M = S.mat
  fun aux (i: intLt m, j: natLte n):<cloref1> void =
    if j < n then
      if i >= 0 then begin
        print_block (M[i, n, j], b); aux (i-1, j)
      end else begin
        curs_backward m; curs_downward 1; aux (m-1, j+1)
      end // end of [if]
    else begin
      fflush_stdout ()
    end // end of [if]
in
  aux (m-1, 0)
end // end of [display_shape_1]

fn display_shape_2 {m,n:pos}
  (S: shape (m, n), b: bool): void = let
  val m = S.row and n = S.col and M = S.mat
  fun aux (i: intLt m, j: intLt n):<cloref1> void =
    if i >= 0 then
      if j >= 0 then begin
        print_block (M[i, n, j], b); aux (i, j - 1)
      end else begin
        curs_backward n; curs_downward 1; aux (i-1, n-1)
      end
    else begin
      fflush_stdout ()
    end // end of [if]
in
  aux (m-1, n-1)
end // end of [display_shape_2]

fn display_shape_3 {m,n:pos}
  (S: shape (m, n), b: bool): void = let
  val m = S.row and n = S.col and M = S.mat
  fun aux (i: natLte m, j: intLt n):<cloref1> void =
    if j >= 0 then
      if i < m then begin
        print_block (M[i, n, j], b); aux (i + 1, j)
      end else begin
        curs_backward m; curs_downward 1; aux (0, j-1)
      end // end of [if]
    else begin
      fflush_stdout ()
    end // end of [if]
in
  aux (0, n-1)
end // end of [display_shape_3]

fn display_shape_at {m,n:pos}
  (S: shape (m, n), b: bool, r: rotate_t, xcen: Int, ycen: Int): void = let
  val m = S.row and n = S.col
  val m2 = nhalf m and n2 = nhalf n
in
  case+ r of
  | 0 => begin
      curs_goto (xcen - m2, ycen - n2); display_shape_0 (S, b)
    end
  | 1 => begin
      curs_goto (xcen - n2, ycen + m2 - m); display_shape_1 (S, b)
    end
  | 2 => begin
      curs_goto (xcen + m2 - m, ycen + n2 - n); display_shape_2 (S, b)
    end
  | 3 => begin
      curs_goto (xcen + n2 - n, ycen - m2); display_shape_3 (S, b)
    end
end // end of [display_shape_at]

//

viewtypedef shapeObj (m:int, n:int) = @{
  rot= rotate_t
, xlen= int m
, ylen= int n
, xpos= Int
, ypos= Int
, speed_lim= Nat
, speed_acc= Nat
, shape= shape (m, n)
}

viewtypedef shapeObj = [m,n:pos] shapeObj (m, n)
viewtypedef shapeObj0 = shapeObj(0, 0)

//

fn shapeObj_display {m,n:pos} (S: &shapeObj (m, n)): void =
  display_shape_at (S.shape, true, S.rot, S.xpos, S.ypos)

fn shapeObj_undisplay {m,n:pos} (S: &shapeObj (m, n)): void =
  display_shape_at (S.shape, false, S.rot, S.xpos, S.ypos)

// succeed if 0 is returned
fn shapeObj_downward {m,n:pos} (S: &shapeObj (m, n)): int =
  if move_test (S.shape, S.rot, S.xpos + 1, S.ypos) then
    (shapeObj_undisplay S; S.xpos := S.xpos + 1; shapeObj_display S; 0)
  else 1

#define i2c char_of_int

fn shapeObj_falling {m,n:pos} (S: &shapeObj (m, n)): void = let
  var error: int = (0: int)
in
  if ~(move_test (S.shape, 0, S.xpos, S.ypos)) then begin
    $raise GameIsOverException ()
  end ; // end of [if]

  while (error < 1) (
    ignore (usleep 1000);
    if kbhit () then let
      val c = readch ()
    in
      case+ i2c c of
      | ' ' => S.speed_lim := 0
      | 'l' => begin
          if move_test (S.shape, S.rot, S.xpos, S.ypos+1) then
            (shapeObj_undisplay S; S.ypos := S.ypos+1; shapeObj_display S)
        end
      | 'j' => begin
          if move_test (S.shape, S.rot, S.xpos, S.ypos-1) then
            (shapeObj_undisplay S; S.ypos := S.ypos-1; shapeObj_display S)
        end
      | 'i' => let
          val r = (S.rot + 3) nmod 4
        in
          if move_test (S.shape, r, S.xpos, S.ypos) then
            (shapeObj_undisplay S; S.rot := r; shapeObj_display S)
        end
      | 'k' => let
          val r = (S.rot + 1) nmod 4
        in
          if move_test (S.shape, r, S.xpos, S.ypos) then
            (shapeObj_undisplay S; S.rot := r; shapeObj_display S)
        end
      | _ => ()
    end ; // end of [if]

    if S.speed_acc >= S.speed_lim then begin
      S.speed_acc := 0; error := shapeObj_downward S
    end else begin
      S.speed_acc := S.speed_acc + 1
    end
  ) ; // end of [while]

  absorb (S.shape, S.rot, S.xpos, S.ypos);
  frame_delete ();
end // end of [shapeObj_falling]

(* ****** ****** *)

fn shape_make {m,n:pos}
  (M: matrix (char, m, n), m: int m, n: int n): shape (m, n) = '{
  row= m, col= n, mat= M
}

fn shapeObj_make {m,n:pos} (S: shape (m, n))
  : [l:addr] (free_gc_v (shapeObj0?, l), shapeObj (m, n) @ l | ptr l) = let
  val (pf_gc, pf | p) = ptr_alloc_tsz {shapeObj0} (sizeof<shapeObj>)
in
  p->rot := 0;
  p->xlen := S.row;
  p->ylen := S.col;
  p->xpos := nhalf (p->xlen);
  p->ypos := nhalf (FW);
  p->speed_lim := SPEED_LIM;
  p->speed_acc := SPEED_LIM;
  p->shape := S;
  (pf_gc, pf | p)
end // end of [shape_make]

(* ****** ****** *)

fn set_block_at {m,n:pos}
  (S: shape (m*BSZ, n*BSZ), i: natLt m, j: natLt n, c: Char)
  : void = let
  val nBSZ = S.col and M = S.mat
  fun aux (p: natLte BSZ, q: natLte BSZ):<cloref1> void =
    if p < BSZ then begin
      if q < BSZ then let
        val () = M[i imul BSZ + p, nBSZ, j imul BSZ + q] := c
      in
        aux (p, q+1)
      end else begin
        aux (p+1, 0)
      end // end of [if]
    end // end of [if]
in
  aux (0, 0)
end // end of [set_block_at]

(* ****** ****** *)

(*

XXXX
XXXX
XXXX
XXXX

*)

val shape_0: shape = let
  val m = 2 imul BSZ and n = 2 imul BSZ
  val M = matrix_make_elt (m, n, '@')
in
  shape_make (M, m, n)
end

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

val shape_1: shape = let
  val m = 4 imul BSZ and n = BSZ
  val M = matrix_make_elt (m, n, '%')
in
  shape_make (M, m, n)
end

(*

  XX
  XX
XXXXXX
XXXXXX

*)

val shape_2: shape = let
  val m = 2 imul BSZ and n = 3 imul BSZ
  val M = matrix_make_elt (m, n, 'N')
  val S = shape_make (M, m, n)
in
  set_block_at {2,3} (S, 0, 0, ' ');
  set_block_at {2,3} (S, 0, 2, ' ');
  S
end // end of [shape_2]

(*

XXXX
XXXX
  XX
  XX
  XX
  XX

*)

val shape_3: shape = let
  val m = 3 imul BSZ and n = 2 imul BSZ
  val M = matrix_make_elt (m, n, 'Z')
  val S = shape_make (M, m, n)
in
  set_block_at {3,2} (S, 1, 0, ' ');
  set_block_at {3,2} (S, 2, 0, ' ');
  S
end // end of [shape_3]

(*

XXXX
XXXX
XX
XX
XX
XX

*)

val shape_4: shape = let
  val m = 3 imul BSZ and n = 2 imul BSZ
  val M = matrix_make_elt (m, n, 'Z')
  val S = shape_make (M, m, n)
in
  set_block_at {3,2} (S, 1, 1, ' ');
  set_block_at {3,2} (S, 2, 1, ' ');
  S
end // end of [shape_4]

(*

XX
XX
XXXX
XXXX
  XX
  XX

*)

val shape_5: shape = let
  val m = 3 imul BSZ and n = 2 imul BSZ
  val M = matrix_make_elt (m, n, 'X')
  val S = shape_make (M, m, n)
in
  set_block_at {3,2} (S, 0, 1, ' ');
  set_block_at {3,2} (S, 2, 0, ' ');
  S
end // end of [shape_5]

(*

  XX
  XX
XXXX
XXXX
XX
XX

*)

val shape_6: shape = let
  val m = 3 imul BSZ and n = 2 imul BSZ
  val M = matrix_make_elt (m, n, 'X')
  val S = shape_make (M, m, n)
in
  set_block_at {3,2} (S, 0, 0, ' ');
  set_block_at {3,2} (S, 2, 1, ' ');
  S
end // end of [shape_6]

val shape_arr =
  array_make_arrsz {shape} $arrsz(
  shape_0
, shape_1
, shape_2
, shape_3
, shape_4
, shape_5
, shape_6
) // end of [shape_arr]

(* ****** ****** *)

extern fun natrand48
  {n:pos} (range: int n):<!ref> natLt n = "natrand48"

%{^
ats_int_type
natrand48 (
  ats_int_type range
) {
  return (range * atslib_drand48 ()) ;
} // end of [natrand48]
%} // end of [%{^]

fn gen_shape_obj
  (): [l:addr] (
  free_gc_v (shapeObj0?, l), shapeObj @ l | ptr l
) = let
  val i = natrand48 (7)
in
  shapeObj_make (shape_arr[i])
end // end of [gen_shape_obj]

(* ****** ****** *)

fn tetrix (): void = let
//
  val () = srand48_with_time ()
  val (pf_stdin | ptr_stdin) = stdin_get ()
//
  fun loop (): void = let
    val (pf_gc, pf | p) = gen_shape_obj ()
  in
    shapeObj_falling !p;
    ptr_free {shapeObj0} (pf_gc, pf | p) ;
    loop ()
  end // end of [loop]
//
in
  save_set_keyboard ();
  print_string clear;
  fflush_stdout ();
  print_usage ();
  curs_home ();
  curs_downward (XBASE + 1);
  print_frame ();
  curs_invisible () ;
  while (fgetc_err (file_mode_lte_r_r | !ptr_stdin) <> int_of '\n') () ;
  curs_goto (XSCORE, YSCORE) ;
  print_string "score: " ;
  print_score () ;
  try loop () with ~GameIsOverException () => begin
    curs_clear ();
    curs_normal ();
    print_string "Game Over! Your score is ";
    print !total_number_of_filled_lines;
    print_string ".";
    print_newline ();
    restore_keyboard ()
  end ;
  stdin_view_set (pf_stdin | (*none*)); // deadcode
end // end of [tetrix]

implement main (argc, argv) = begin
  try tetrix () with exn => (restore_keyboard (); $raise exn)
end // end of [main]

(* ****** ****** *)

%{$

#include <stdio.h>
#include <curses.h>

/*
* From the downloaded sources of the book
* "Beginning Linux Programming" - Wrox.
*
* Copyright Wrox Press.
* Licensed by the General Public License (GPL).
* See http://www.gnu.org/licenses/gpl.txt for details.
*/

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <term.h>
#include <curses.h>
#include <unistd.h>

static struct termios initial_settings, new_settings;
static int peek_character = -1;

ats_void_type save_set_keyboard () {
  tcgetattr(0,&initial_settings);

  new_settings = initial_settings;
  new_settings.c_lflag &= ~ICANON;
  new_settings.c_lflag &= ~ECHO;
  new_settings.c_lflag &= ~ISIG;
  new_settings.c_cc[VMIN] = 1;
  new_settings.c_cc[VTIME] = 0;

  tcsetattr(0, TCSANOW, &new_settings);
}

ats_void_type restore_keyboard () {
  tcsetattr(0, TCSANOW, &initial_settings);
}

ats_int_type kbhit () {
  char ch;
  int nread;

  if (peek_character != -1) return 1;

  new_settings.c_cc[VMIN]=0;

  tcsetattr(0, TCSANOW, &new_settings);

  nread = read(0,&ch,1);

  new_settings.c_cc[VMIN]=1;

  tcsetattr(0, TCSANOW, &new_settings);

  if (nread == 1) { peek_character = ch; return 1; }

  return 0;
}

ats_int_type readch () {
  char ch;

  if (peek_character != -1) {
    ch = peek_character;
    peek_character = -1;
    return ch;
  }

  return -1;
}

%} // end of [%{$]

(* ****** ****** *)

(* end of [tetrix.dats] *)
