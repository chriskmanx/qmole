//
//
// A simple example of programming with theorem-proving
//
//

// Time: June 15, 2008
// Author: Hongwei Xi

dataprop SUMUP (int, int) =
  | SUMUPbas (0, 0)
  | {n:nat; r:int} SUMUPind (n+1, r+n+1) of SUMUP (n, r)

fn sumup {n:nat}
  (n: int n):<> [r:int] (SUMUP (n, r) | int r) = let
  fun aux {i:nat; r1,r2:int} .<i>.
    (pf: SUMUP (i, r1) | i: int i, res: int r2):<> int (r1+r2) =
    case+ i of
    | 0 => begin
        let prval SUMUPbas () = pf in res end
      end
    | _ =>> let
        prval SUMUPind pf = pf
      in
        aux (pf | i-1, res + i)
      end
  prval pf = prsumup {n} () where {
    prfun prsumup {n:nat} .<n>. ():<> [r:int] SUMUP (n, r) =
      sif (n > 0) then SUMUPind (prsumup {n-1} ()) else SUMUPbas ()
  } // end of [where]
in
  (pf | aux (pf | n, 0))
end // end of [sumup]

implement main (argc, argv) = let
  val (_ | res) = sumup (10)
in
  printf ("sumup (10) = %i", @(res)); print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [sumup.dats] *)
