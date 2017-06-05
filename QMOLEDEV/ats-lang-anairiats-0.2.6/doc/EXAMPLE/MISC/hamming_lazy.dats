(*

// solving hamming problem in lazy style

// author: Hongwei Xi (September, 2007)

*)

(* ****** ****** *)

staload "prelude/DATS/lazy.dats"

(* ****** ****** *)

#define nil stream_nil
#define cons stream_cons
#define :: stream_cons

(* ****** ****** *)

fn lte (x: Nat, y: Nat):<fun> bool = x <= y

(*
fn scale
  (S: stream Nat, n: Nat):<!laz> stream Nat =
  stream_map_cloref<Nat,Nat> (S, lam x => n nmul x)
// end of [scale]
*)
macdef scale (S, n) =
  stream_map_fun<Nat><Nat> (,(S), lam x => ,(n) nmul x)
// end of [scale]

val rec S: stream Nat = $delay (
  1 :: stream_ordmerge_fun (stream_ordmerge_fun (scale (S, 2), scale (S, 3), lte), scale (S, 5), lte)
) // end of [$delay]

fun remove_dup (S0: stream Nat):<!laz> stream_con Nat = let
  fun rmv (n: Nat, S0: stream Nat):<!laz> stream_con Nat = begin
    case+ !S0 of
    | x :: S =>
       if (n < x) then n :: $delay (rmv (x, S)) else rmv (n, S)
    | nil () => nil ()
  end // end of [rmv]
in
  case+ !S0 of n :: S => rmv (n, S) | nil () => nil ()
end // end of [remove_dup]

val S = $delay (remove_dup S)

implement main (argc, argv) = let

fun loop{n:nat} (S: stream Nat, n: int n): void = begin
  if n > 0 then begin case+ !S of
    | x :: S => (print (x: Nat); print_newline (); loop (S, n-1))
    | nil () => ()
  end
end // end of [loop]

in
  loop (S, 100)
end // end of [main]

(* ****** ****** *)

(* end of [hamming_lazy.dats] *)
