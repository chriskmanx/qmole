(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: Array2 (http://www.standardml.org/Basis/array2.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/general.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/array2.sats"

(* ****** ****** *)

(*
** HX: note that RowMajor representation is assumed
*)
assume
array2_t0ype_type (a: t@ype) = [m,n:nat] [l:addr] '{
  data= ptr l, view= vbox (matrix_v (a, m, n, l)), row= size_t m,  col= size_t n
} // end of [array2_t0ype_type]

(* ****** ****** *)

extern
fun vbox_make_view_ptr_matrix
  {a:viewt@ype} {m,n:int} {l:addr} (
  pf: matrix_v (a, m, n, l) | p: ptr l):<> (vbox (matrix_v (a, m, n, l)) | void
) = "atspre_vbox_make_view_ptr" // end of [vbox_make_view_ptr_matrix]

(* ****** ****** *)

implement{a}
array (row, col, ini) = let
  val [m:int] m = size1_of_size row
  and [n:int] n = size1_of_size col
  val (pf_mul | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat pf_mul
  val [l:addr] (pfgc, pfarr | p_arr) =
    array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  // end of [val]
  prval () = free_gc_elim {a?} (pfgc) // return the certificate
  val () = array_ptr_initialize_elt<a> (!p_arr, mn, ini)
  prval pf_mat = matrix_v_of_array_v (pf_mul, pfarr)
  val (pf_mat_box | ()) = vbox_make_view_ptr_matrix (pf_mat | p_arr)
in
  #[m,n | #[l | '{ data= p_arr, view= pf_mat_box, row= m, col= n} ] ]
end // end of [array]

(* ****** ****** *)

extern fun{a:t@ype}
array_ptr_initialize_lstlst
  {m,n:nat} {mn:int} (
  pf_mul: MUL (m, n, mn)
| base: &(@[a?][mn]) >> @[a][mn], m: size_t m, xss: list (list (a, m), n)
) :<> void // end of [array_ptr_initialize_lstlst]

implement{a} array_ptr_initialize_lstlst
  {m,n}{mn} (pf_mul | base, m, xss) = let
  val (pf_ofs | ofs) = mul2_size1_size1 (m, sizeof<a>)
  fun loop {n:nat} {mn:int} {l:addr} .<n>. (
      pf_mul: MUL (n, m, mn), pfarr: !array_v (a?, mn, l) >> array_v (a, mn, l)
    | p_arr: ptr l, xss: list (list (a, m), n)
    ) :<cloref> void =
    case+ xss of
    | list_nil () => () where {
        prval MULbas () = pf_mul
        prval () = array_v_unnil (pfarr); prval () = pfarr := array_v_nil {a} ()
      } // end of [list_nil]
    | list_cons (xs, xss) => let
        prval pf1_mul = mul_add_const {~1} {n,m} (pf_mul)
        prval () = mul_nat_nat_nat (pf1_mul)
        prval (pf1_arr, pf2_arr) = array_v_split {a?} {mn} {m} (pf_ofs, pfarr)
        val () = array_ptr_initialize_lst<a> (!p_arr, xs)
        val () = loop (pf1_mul, pf2_arr | p_arr + ofs, xss)
        prval () = pfarr := array_v_unsplit {a} (pf_ofs, pf1_arr, pf2_arr)
      in
        // nothing
      end (* end of [list_cons] *)
  // end of [loop]     
in
  loop (mul_commute (pf_mul), view@ base | &base, xss)
end // end of [array_ptr_initialize_lstlst]

(* ****** ****** *)

implement{a}
fromList (xss) = let
  val n = list0_length (xss)
  val [n:int] n = int1_of_int (n)
  val () = (if (n > 0) then () else $raise Size ()): [n > 0] void
  val- list0_cons (xs, xss1) = xss
  val [m:int] xs = list1_of_list0 xs
  val m = list_length (xs)
  val () = auxcheck (m, xss1) where {
    fun auxcheck (m: int, xss: list0 (list0 a)): void =
      case+ xss of
      | list0_cons (xs, xss) => begin
          if list0_length (xs) = m then auxcheck (m, xss) else $raise Size ()
        end // end of [list0_cons]
      | list0_nil () => ()
    // end of [auxcheck]      
  } // end of [val] 
//
// There should be a better way of doing this
//
  val xss = __cast (xss) where {
    extern castfn __cast (xss: list0 (list0 a)):<> list (list (a, m), n)
  } (* end of [val] *)
//
  val m = size1_of_int1 (m) and n = size1_of_int1 (n)
  val (pf_mul | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat pf_mul
  val [l:addr] (pfgc, pfarr | p_arr) =
    array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  // end of [val]
  prval () = free_gc_elim {a?} (pfgc) // return the certificate
  val () = array_ptr_initialize_lstlst<a> (pf_mul | !p_arr, m, xss)
  prval pf_mat = matrix_v_of_array_v (pf_mul, pfarr)
  val (pf_mat_box | ()) = vbox_make_view_ptr_matrix (pf_mat | p_arr)
in
  #[m,n | #[l | '{ data= p_arr, view= pf_mat_box, row= m, col= n} ] ]
end // end of [fromList]

(* ****** ****** *)

implement{a}
tabulate (trv, m, n, f) = let
  val [m:int] m = size1_of_size (m) and [n:int] n = size1_of_size (n)
  val () = (if m > 0 then () else $raise Size ()): [m > 0] void
  val () = (if n > 0 then () else $raise Size ()): [n > 0] void
  val [mn:int] (pf_mul_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mul_mn)
  val [l:addr] (pfgc, pfarr | p_arr) =
    array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  // end of [val]
  prval () = free_gc_elim {a?} (pfgc) // return the certificate
  prval pfarr = __cast pfarr where {
    extern prfun __cast (pfarr: array_v (a?, mn, l)):<> array_v (a, mn, l) 
  } (* end of [prval] *)
  val _0 = size1_of_int1 0 and _1 = size1_of_int1 1
  val () = case+ :(pfarr: array_v (a, mn, l)) => trv of
    | RowMajor () => let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (i: sizeLte m) => (i := _0; i < m; i := i + _1) 
        for* (i: sizeLt m, j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val k1 = __cast k where {
            extern castfn __cast (k: size_t):<> sizeLt (mn)
          } // end of [val]
        in   
          p_arr->[k1] := f (i, j); k := k + _1
        end // end of [for] // end of [for]
      end // end of [RowMajor]
    | ColMajor () =>  let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val () = k := j
        in  
          for* (i: sizeLte m, j: sizeLt n) => (i := _0; i < m; i := i + _1) let
            val k1 = __cast k where {
              extern castfn __cast (k: size_t):<> sizeLt (mn)
            } // end of [val]
          in   
            p_arr->[k1] := f (i, j); k := k + n
          end // end of [for]
        end // end of [for]
      end (* end of [ColMajor] *)
  // end of [val]
  prval pf_mat = matrix_v_of_array_v {a} (pf_mul_mn, pfarr)
  val (pf_mat_box | ()) = vbox_make_view_ptr_matrix (pf_mat | p_arr)
in
  #[m,n | #[l | '{ data= p_arr, view= pf_mat_box, row= m, col= n} ] ]
end // end of [tabulate] 

(* ****** ****** *)

local

prfun lemma_for_matrix_subscripting
  {m,n:nat} {i:nat | i < m} {mn,p:int} .<m>.
  (pf1: MUL (m, n, mn), pf2: MUL (i, n, p)): [p+n <= mn] void = let
  prval MULind pf11 = pf1
in
  sif i < m-1 then begin
    lemma_for_matrix_subscripting (pf11, pf2)
  end else let // i = m-1
    prval () = mul_isfun (pf11, pf2)
  in
    // empty
  end // end of [sif]
end // end of [lemma_for_matrix_subscripting]

in // in of [local]

implement{a}
sub (M, i, j) = let
  val m = M.row and n = M.col
  val i = size1_of_size (i) and j = size1_of_size (j)
in
  if i < m then
    if j < n then let
      prval vbox pf_mat = M.view
      prval (pf_mul_mn, pfarr) = array_v_of_matrix_v {a} (pf_mat)
      val (pf_mul_i_n | i_n) = mul2_size1_size1 (i, n)
      prval () = mul_nat_nat_nat pf_mul_i_n
      prval () = lemma_for_matrix_subscripting (pf_mul_mn, pf_mul_i_n)
      val M_data = M.data
      val x = !M_data.[i_n+j]
      prval () = pf_mat := matrix_v_of_array_v (pf_mul_mn, pfarr)
    in
      x // return value
    end else begin
      $raise Subscript () 
    end // end of [if]  
  else begin
    $raise Subscript ()
  end // end of [if] 
end (* end of [sub] *) 

implement{a}
update (M, i, j, x) = let
  val m = M.row and n = M.col
  val i = size1_of_size (i) and j = size1_of_size (j)
in
  if i < m then
    if j < n then let
      prval vbox pf_mat = M.view
      prval (pf_mul_mn, pfarr) = array_v_of_matrix_v {a} (pf_mat)
      val (pf_mul_i_n | i_n) = mul2_size1_size1 (i, n)
      prval () = mul_nat_nat_nat pf_mul_i_n
      prval () = lemma_for_matrix_subscripting (pf_mul_mn, pf_mul_i_n)
      val M_data = M.data
      val () = !M_data.[i_n+j] := x
      prval () = pf_mat := matrix_v_of_array_v (pf_mul_mn, pfarr)
    in
      // no return value
    end else begin
      $raise Subscript () 
    end // end of [if]  
  else begin
    $raise Subscript ()
  end // end of [if] 
end (* end of [sub] *) 

end // end of [local]

(* ****** ****** *)

implement dimensions (M) = @(M.row, M.col) 

implement nRows (M) = M.row
implement nCols (M) = M.col

(* ****** ****** *)

implement{a}
app (
  trv, f, [m,n:int] [l:addr] M
) = let
  val p_arr = M.data; prval vbox pf_mat = M.view
  val m = M.row and n = M.col
  prval [mn:int] (
    pf_mul_mn, pfarr
  ) = array_v_of_matrix_v {a} (pf_mat)
  val _0 = size1_of_int1 0 and _1 = size1_of_int1 1
  val () = case+ trv of
    | RowMajor () => let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (i: sizeLte m) => (i := _0; i < m; i := i + _1) 
        for* (i: sizeLt m, j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val k1 = __cast k where {
            extern castfn __cast (k: size_t):<> sizeLt (mn)
          } // end of [val]
        in   
          $effmask_ref (f (p_arr->[k1])); k := k + _1
        end // end of [for] // end of [for]
      end // end of [RowMajor]
    | ColMajor () =>  let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val () = k := j
        in  
          for* (i: sizeLte m, j: sizeLt n) => (i := _0; i < m; i := i + _1) let
            val k1 = __cast k where {
              extern castfn __cast (k: size_t):<> sizeLt (mn)
            } // end of [val]
          in   
            $effmask_ref (f (p_arr->[k1])); k := k + n
          end // end of [for]
        end // end of [for]
      end (* end of [ColMajor] *)
  // end of [val]
in
  pf_mat := matrix_v_of_array_v (pf_mul_mn, pfarr)
end // end of [app]
  
(* ****** ****** *)

implement{a,b}
fold (
  trv, f, ini, [m,n:int] [l:addr] M
) = let
  val p_arr = M.data; prval vbox pf_mat = M.view
  val m = M.row and n = M.col
  prval [mn:int] (
    pf_mul_mn, pfarr
  ) = array_v_of_matrix_v {a} (pf_mat)
  val _0 = size1_of_int1 0 and _1 = size1_of_int1 1
  var res: b = ini
  val () = case+ trv of
    | RowMajor () => let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (i: sizeLte m) => (i := _0; i < m; i := i + _1) 
        for* (i: sizeLt m, j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val k1 = __cast k where {
            extern castfn __cast (k: size_t):<> sizeLt (mn)
          } // end of [val]
        in   
          res := $effmask_ref (f (p_arr->[k1], res)); k := k + _1
        end // end of [for] // end of [for]
      end // end of [RowMajor]
    | ColMajor () =>  let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val () = k := j
        in  
          for* (i: sizeLte m, j: sizeLt n) => (i := _0; i < m; i := i + _1) let
            val k1 = __cast k where {
              extern castfn __cast (k: size_t):<> sizeLt (mn)
            } // end of [val]
          in   
            res := $effmask_ref (f (p_arr->[k1], res)); k := k + n
          end // end of [for]
        end // end of [for]
      end (* end of [ColMajor] *)
  // end of [val]
in
  pf_mat := matrix_v_of_array_v (pf_mul_mn, pfarr)
end // end of [fold]

(* ****** ****** *)

implement{a}
modify (
  trv, f, [m,n:int] [l:addr] M
) = let
  val p_arr = M.data; prval vbox pf_mat = M.view
  val m = M.row and n = M.col
  prval [mn:int] (
    pf_mul_mn, pfarr
  ) = array_v_of_matrix_v {a} (pf_mat)
  val _0 = size1_of_int1 0 and _1 = size1_of_int1 1
  val () = case+ trv of
    | RowMajor () => let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (i: sizeLte m) => (i := _0; i < m; i := i + _1) 
        for* (i: sizeLt m, j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val k1 = __cast k where {
            extern castfn __cast (k: size_t):<> sizeLt (mn)
          } // end of [val]
        in   
          p_arr->[k1] := $effmask_ref (f (p_arr->[k1])); k := k + _1
        end // end of [for] // end of [for]
      end // end of [RowMajor]
    | ColMajor () =>  let
        var i: size_t? and j: size_t? ; var k: size_t = _0
      in
        for* (j: sizeLte n) => (j := _0; j < n; j := j + _1) let
          val () = k := j
        in  
          for* (i: sizeLte m, j: sizeLt n) => (i := _0; i < m; i := i + _1) let
            val k1 = __cast k where {
              extern castfn __cast (k: size_t):<> sizeLt (mn)
            } // end of [val]
          in   
            p_arr->[k1] := $effmask_ref (f (p_arr->[k1])); k := k + n
          end // end of [for]
        end // end of [for]
      end (* end of [ColMajor] *)
  // end of [val]
in
  pf_mat := matrix_v_of_array_v (pf_mul_mn, pfarr)
end // end of [modify]

(* ****** ****** *)

(* end of [array2.dats] *)
