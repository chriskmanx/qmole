(*
** An implementation of the QUEENS problem that
** makes use of lazy-evaluation
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: 2009
*)

(* ****** ****** *)

staload "prelude/DATS/lazy_vt.dats"

(* ****** ****** *)

fn print_row {n,i:nat | i < n}
  (n: int n, i: int i): void = let
  var k: int
  val () = for (k := 0; k < i; k := k + 1) print '.'
  val () = print 'Q'
  val () = for (k := i+1; k < n; k := k + 1) print '.'
in
  // empty
end // end of [print_row]

typedef board (n:int, k:int) = list (natLt n, k)
fn print_board {n:nat}
  (n: int n, xs: list (natLt n, n))
  : void = loop (n, xs) where {
  fun loop {k:nat | k <= n}
    (n: int n, xs: board (n, k)): void =
    case+ xs of
    | list_cons (x, xs1) => begin
        print_row (n, x); print_newline (); loop (n, xs1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
} // end of [print_board]

(* ****** ****** *)

fun test_one {n,k:nat | k < n} .<k>.
  (x0: natLt n, dist: int, xs: board (n, k)):<> bool = case+ xs of
  | list_cons (x, xs1) => begin
      if (x0 = x orelse dist = abs (x - x0)) then false else begin
        let val ans = test_one {n,k-1} (x0, dist+1, xs1) in ans end
      end // end of [if]
    end // end of [list_cons]
  | list_nil () => true
// end of [test_one]

(* ****** ****** *)

fun board_extend {n,k:nat | k < n}
  (n: int n, xss: stream_vt (board (n, k)))
  :<!laz> stream_vt_con (board (n, k+1)) =
  case+ !xss of
  | ~stream_vt_cons (xs, xss) => board_extend_aux (n, xs, xss, 0)
  | ~stream_vt_nil () => stream_vt_nil ()

and board_extend_aux {n,k:nat | k < n} {i:nat | i <= n}
  (n: int n, xs: board (n, k), xss: stream_vt (board (n, k)), x: int i)
  :<!laz> stream_vt_con (board (n, k+1)) =
  if x < n then begin
    if test_one {n,k} (x, 1, xs) then let
      val xss = $ldelay (board_extend_aux (n, xs, xss, x+1), ~xss)
    in
      stream_vt_cons (list_cons (x, xs), xss)
    end else begin
      board_extend_aux (n, xs, xss, x+1)
    end // end of [if]
  end else begin
    board_extend {n, k} (n, xss)
  end // end of [if]

(* ****** ****** *)

fun queens {n,k:nat | k <= n}
  (n: int n, k: int k):<!laz> stream_vt (board (n, k)) =
  if k = 0 then begin $ldelay (
    stream_vt_cons (list_nil (), $ldelay (stream_vt_nil ()))
  ) end else begin
    $ldelay (board_extend {n,k-1} (n, queens (n, k-1)))
  end // end of [if]
(* end of [queens] *)

(* ****** ****** *)

#define EIGHT 8

implement main (argc, argv) = loop (1, xss) where {
  val N = if argc >= 2 then int_of_string argv.[1] else EIGHT
  val [N:int] N = int1_of_int N
  val () = assert (N >= 0)
  val xss = queens (N, N)
  fun loop (i: int, xss: stream_vt (board (N, N))):<cloref1> void =
    case+ !xss of
    | ~stream_vt_cons (xs, xss) => begin
        printf ("solution no. %i:\n", @(i));
        print_board (N, xs); print_newline ();
        loop (i+1, xss)
      end // end of [stream_vt_cons]
    | ~stream_vt_nil () => ()
  // end of [loop]        
} // end of [main]

(* ****** ****** *)

(* end of [queens_lazy_vt.dats] *)
