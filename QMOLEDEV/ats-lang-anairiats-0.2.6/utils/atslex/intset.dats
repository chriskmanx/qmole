(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS - Unleashing the Power of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
 * Free Software Foundation; either version 2.1, or (at your option)  any
 * later version.
 * 
 * ATS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
 * for more details.
 * 
 * You  should  have  received  a  copy of the GNU General Public License
 * along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
 * Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

datatype ints =
  | ints_nil | ints_cons of (int, ints)

assume intset_t = ints

//

#define nil ints_nil; #define cons ints_cons; #define :: ints_cons

//

implement fprint_intset
  {m:file_mode} (pf_mod | fil, ns) = let
  fun aux1 (fil: &FILE m, ns: ints): void = begin
    case+ ns of n :: ns => aux2 (fil, n, ns) | nil () => ()
  end // end of [aux1]

  and aux2 (fil: &FILE m, n: int, ns: ints)
    : void = begin case+ ns of
    | cons (n1, ns1) => begin
        fprint_int (pf_mod | fil, n);
        fprint_string (pf_mod | fil, ", ");
        aux2 (fil, n1, ns1)
      end
    | nil () => fprint_int (pf_mod | fil, n)
  end // end of [aux2]
in
  aux1 (fil, ns)
end // end of [fprint_intset]

//

implement print_intset (ns) = print_mac (fprint_intset, ns)
implement prerr_intset (ns) = prerr_mac (fprint_intset, ns)

//

implement intset_nil = nil ()

implement intset_is_nil (ns) = begin
  case+ ns of nil () => true | _ :: _ => false
end

implement intset_singleton (n) = cons (n, nil ())

implement eq_intset_intset (ns1, ns2) = let
  fun loop (ns1: ints, ns2: ints): bool =
    case+ (ns1, ns2) of
    | (nil (), nil ()) => true
    | (n1 :: ns1, n2 :: ns2) =>
        if n1 = n2 then loop (ns1, ns2) else false
    | (_, _) => false
in
  loop (ns1, ns2)
end // end of [eq_intset_intset]

implement compare_intset_intset (ns1, ns2) = let
  fun loop (ns1: ints, ns2: ints): Sgn = case+ (ns1, ns2) of
    | (n1 :: ns1, n2 :: ns2) => begin
        if n1 < n2 then ~1 else (if n1 > n2 then 1 else compare (ns1, ns2))
      end
    | (_ :: _, nil ()) =>  1
    | (nil (), _ :: _) => ~1
    | (nil (), nil ()) =>  0
in
  loop (ns1, ns2)
end // end of [compare_intset_intset]

implement union_intset_intset (ns1, ns2) = let
  fun aux_union (ns1: ints, ns2: ints)
    : ints = begin case+ (ns1, ns2) of
    | (n1 :: ns1', n2 :: ns2') => let
        val sgn = compare (n1, n2)
      in
        if sgn < 0 then n1 :: aux_union (ns1', ns2)
        else if sgn > 0 then n2 :: aux_union (ns1, ns2')
        else n1 :: aux_union (ns1', ns2')
      end // end of [cons _, cons _]
    | (nil (), _) => ns2
    | (_, nil ()) => ns1
  end // end of [aux_union]
in
  aux_union (ns1, ns2)
end // end of [union_intset_intset]

implement
foreach_intset {v} (pf | f, ns): void = let
  viewtypedef cloptr_t = (!v | int) -<cloptr1> void
  fun loop (pf: !v | f: !cloptr_t, ns: intset_t)
    : void = begin case+ ns of
    | n :: ns => (f (pf | n); loop (pf | f, ns)) | nil () => ()
  end // end of [loop]
in
  loop (pf | f, ns)
end // end of [foreach_intset]

(* ****** ****** *)

(* end of [intset.dats] *)
