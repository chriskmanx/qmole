(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Start Time: 2007
//
(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [basic_sta.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* some integer constants *)

#define power_2_4 0x10
#define power_2_5 0x20
#define power_2_6 0x40
#define power_2_7 0x80
#define power_2_8 0x100
#define power_2_10 0x400
#define power_2_15 0x8000
#define power_2_16 0x10000
#define power_2_20 0x100000
#define power_2_31 0x80000000
#define power_2_32 0x100000000
#define power_2_63 0x8000000000000000
#define power_2_64 0x10000000000000000

(* ****** ****** *)

// some unindexed types

(* ****** ****** *)

abst@ype bool_t0ype = $extype"ats_bool_type"
abst@ype byte_t0ype = $extype"ats_byte_type" // sizeof (byte) = 1

(* ****** ****** *)

abst@ype char_t0ype = $extype"ats_char_type"
abst@ype schar_t0ype = $extype"ats_schar_type"
abst@ype uchar_t0ype = $extype"ats_uchar_type"

(* ****** ****** *)

abstype clo_t0ype // ats_clo_type // unnamed

(* ****** ****** *)

abst@ype double_t0ype = $extype"ats_double_type"

(* ****** ****** *)

absviewtype exception_viewtype // boxed type // unamed

(* ****** ****** *)
//
abst@ype int_t0ype = $extype"ats_int_type"
abst@ype uint_t0ype = $extype"ats_uint_type"
//
abst@ype int_short_t0ype = $extype"ats_sint_type"
abst@ype uint_short_t0ype = $extype"ats_usint_type"
//
abst@ype int_short_short_t0ype = $extype"ats_sint_type"
abst@ype uint_short_short_t0ype = $extype"ats_ussint_type"
//
abst@ype int_long_t0ype = $extype"ats_lint_type"
abst@ype uint_long_t0ype = $extype"ats_ulint_type"
//
abst@ype int_long_long_t0ype = $extype"ats_llint_type"
abst@ype uint_long_long_t0ype = $extype"ats_ullint_type"
//
abst@ype intmax_t0ype = $extype"ats_intmax_type"
abst@ype uintmax_t0ype = $extype"ats_uintmax_type"
//
// HX: integer types guaranteed to be of one word size
//
abstype intptr_type = $extype"ats_intptr_type" // named
abstype uintptr_type = $extype"ats_uintptr_type" // named
//
// HX: integer types with fixed size
//
abst@ype int8_t0ype = $extype"ats_int8_type"
abst@ype uint8_t0ype = $extype"ats_uint8_type"
//
abst@ype int16_t0ype = $extype"ats_int16_type"
abst@ype uint16_t0ype = $extype"ats_uint16_type"
//
abst@ype int32_t0ype = $extype"ats_int32_type"
abst@ype uint32_t0ype = $extype"ats_uint32_type"
//
abst@ype int64_t0ype = $extype"ats_int64_type"
abst@ype uint64_t0ype = $extype"ats_uint64_type"
//
// HX: integer types for sizes
//
abst@ype size_t0ype = $extype"ats_size_type"
abst@ype ssize_t0ype = $extype"ats_ssize_type"
abst@ype ptrdiff_t0ype = $extype"ats_ptrdiff_type"
//
(* ****** ****** *)
//
// HX: unindexed type for pointers
//
abstype ptr_type = $extype"ats_ptr_type" // unnamed
abstype ptrself_type = $extype"ats_ptrself_type" // named
//
(* ****** ****** *)

abstype string_type // boxed type // unnamed
abst@ype strbuf_t0ype // a type of variable size

(* ****** ****** *)
//
// HX-2010-10-23:
// sizeof (void) is undefined in the standard but GCC sets it to 1
// For instance, try to compile with the flags: '-ansi' and '-pedantic'
//
abst@ype void_t0ype = $extype"ats_void_type"
//
// HX-2010-10-23: [ats_empty_type] is a struct of no fields
//
abst@ype empty_t0ype = $extype"ats_empty_type" // sizeof(empty) = 0
//
// HX-2011-02-11: [ats_undefined_type] is undefined
//
abst@ype undefined_t0ype = $extype"ats_undefined_type"
absviewt@ype undefined_viewt0ype = $extype"ats_undefined_type"
//
(* ****** ****** *)
//
// HX: some built-in static constants for integer operations
//
stacst neg_int_int : int -> int (* integer negation *)
stadef ~ = neg_int_int
//
stacst add_int_int_int : (int, int) -> int (* addition *)
stadef + = add_int_int_int
//
stacst sub_int_int_int: (int, int) -> int (* subtraction *)
stadef - = sub_int_int_int
//
stacst nsub_int_int_int: (int, int) -> int (* subtraction on nats *)
stadef nsub = nsub_int_int_int
//
stacst mul_int_int_int : (int, int) -> int (* multiplication *)
stadef * = mul_int_int_int
//
stacst div_int_int_int : (int, int) -> int (* division *)
stadef / = div_int_int_int
//
(*
stacst mod_int_int_int : (int, int) -> int (* modulo operation *)
stadef mod = mod_int_int_int
*)
// [y] is required to be a constant
stadef mod (x:int, y:int) = x - y * (x / y)
//
stacst abs_int_int : int -> int
stadef abs = abs_int_int
//
stacst max_int_int_int : (int, int) -> int
stadef max = max_int_int_int
//
stacst min_int_int_int : (int, int) -> int
stadef min = min_int_int_int
//
stacst int_of_bool : bool -> int and bool_of_int : int -> bool
stacst int_of_char : char -> int and char_of_int : int -> char
//
(* ****** ****** *)
//
// HX: some built-in boolean constants
//
stacst true_bool : bool and false_bool : bool
stadef true = true_bool and false = false_bool
//
// HX: some built-in static constants for boolean operations
//
stacst neg_bool_bool : bool -> bool (* boolean negation *)
stadef ~ = neg_bool_bool
//
stacst mul_bool_bool_bool : (bool, bool) -> bool (* conjunction *)
stadef && = mul_bool_bool_bool
//
stacst add_bool_bool_bool : (bool, bool) -> bool (* disjunction *)
stadef || = add_bool_bool_bool
//
stacst gt_bool_bool_bool : (bool, bool) -> bool
stadef > = gt_bool_bool_bool
//
stacst gte_bool_bool_bool : (bool, bool) -> bool
stadef >= = gte_bool_bool_bool
//
stacst lt_bool_bool_bool : (bool, bool) -> bool
stadef < = lt_bool_bool_bool
//
stacst lte_bool_bool_bool : (bool, bool) -> bool
stadef <= = lte_bool_bool_bool
//
stacst eq_bool_bool_bool : (bool, bool) -> bool
stadef == = eq_bool_bool_bool
//
stacst neq_bool_bool_bool : (bool, bool) -> bool
stadef <> = neq_bool_bool_bool
//
(* ****** ****** *)
//
// HX: some built-in static constants for char comparisons
//
stacst sub_char_char_int : (char, char) -> int
stadef - = sub_char_char_int
//
stacst gt_char_char_bool : (char, char) -> bool
stadef > = gt_char_char_bool
//
stacst gte_char_char_bool : (char, char) -> bool
stadef >= = gte_char_char_bool
//
stacst lt_char_char_bool : (char, char) -> bool
stadef < = lt_char_char_bool
//
stacst lte_char_char_bool : (char, char) -> bool
stadef <= = lte_char_char_bool
//
stacst eq_char_char_bool : (char, char) -> bool
stadef == = eq_char_char_bool
//
stacst neq_char_char_bool : (char, char) -> bool
stadef <> = neq_char_char_bool
//
(* ****** ****** *)
//
// HX: some built-in static constants for integer comparisons
//
stacst gt_int_int_bool : (int, int) -> bool
stadef > = gt_int_int_bool
//
stacst gte_int_int_bool : (int, int) -> bool
stadef >= = gte_int_int_bool
//
stacst lt_int_int_bool : (int, int) -> bool
stadef < = lt_int_int_bool
//
stacst lte_int_int_bool : (int, int) -> bool
stadef <= = lte_int_int_bool
//
stacst eq_int_int_bool : (int, int) -> bool
stadef == = eq_int_int_bool
//
stacst neq_int_int_bool : (int, int) -> bool
stadef <> = neq_int_int_bool
//
// HX: some built-in static constants for pointer arithmetic
//
stacst null_addr : addr
stadef null = null_addr
//
stacst add_addr_int_addr : (addr, int) -> addr
stadef + = add_addr_int_addr
//
stacst sub_addr_int_addr : (addr, int) -> addr
stadef - = sub_addr_int_addr
//
stacst sub_addr_addr_int : (addr, addr) -> int
stadef - = sub_addr_addr_int
//
stacst gt_addr_addr_bool : (addr, addr) -> bool
stadef > = gt_addr_addr_bool
//
stacst gte_addr_addr_bool : (addr, addr) -> bool
stadef >= = gte_addr_addr_bool
//
stacst lt_addr_addr_bool : (addr, addr) -> bool
stadef < = lt_addr_addr_bool
//
stacst lte_addr_addr_bool : (addr, addr) -> bool
stadef <= = lte_addr_addr_bool
//
stacst eq_addr_addr_bool : (addr, addr) -> bool
stadef == = eq_addr_addr_bool
//
stacst neq_addr_addr_bool : (addr, addr) -> bool
stadef <> = neq_addr_addr_bool
//
(* ****** ****** *)

(*
** subclass relation
*)
stacst lte_cls_cls_bool : (cls, cls) -> bool
stadef <= = lte_cls_cls_bool

(* ****** ****** *)

(*
//
// HX:
// some built-in static constants for rationals
// not yet supported and may never be supported
//
sta ~ : rat -> rat (* rational negation *)
sta + : (rat, rat) -> rat (* addition *)
and - : (rat, rat) -> rat (* subtraction *)
and * : (rat, rat) -> rat (* multiplication *)
and / : (rat, int) -> rat (* division *)
and / : (rat, rat) -> rat (* division *)
//
sta > : (rat, rat) -> bool
sta > : (rat, int) -> bool
and >= : (rat, rat) -> bool
and < : (rat, rat) -> bool
and <= : (rat, rat) -> bool
and <> : (rat, rat) -> bool
and == : (rat, rat) -> bool
//
*)

(* ****** ****** *)
//
viewtypedef bottom_t0ype_uni = {a:t@ype} a
viewtypedef bottom_t0ype_exi = [a:t@ype | false] a
//
viewtypedef bottom_viewt0ype_uni = {a:viewt@ype} a
viewtypedef bottom_viewt0ype_exi = [a:viewt@ype | false] a
//
(* ****** ****** *)
//
// HX: some built-in type/viewtype/prop/view constructors
//
absview
at_viewt0ype_addr_view (viewt@ype+, addr)
stadef @ = at_viewt0ype_addr_view // HX: @ is infix

(* ****** ****** *)

abstype
ref_viewt0ype_type
  (viewt@ype) // boxed type // unnamed
stadef ref = ref_viewt0ype_type

abstype
refconst_t0ype_type (t@ype) // boxed type // unnamed
stadef refconst = refconst_t0ype_type

(*
//
// HX-2009: should this be added?
//
abstype
refopt_viewt0ype_bool_type (viewt@ype, bool) // unnamed
stadef refopt = refopt_viewt0ype_bool_type
//
typedef Refopt (a: viewt@ype) = [b:bool] refopt (a, b)
//
*)

(* ****** ****** *)
//
// HX: for boxing a view
//
absprop
vbox_view_prop (view)
stadef vbox = vbox_view_prop
//
// HX: for taking out a field
//
abst@ype
without_viewt0ype_t0ype (viewt@ype)
stadef without = without_viewt0ype_t0ype
//
(* ****** ****** *)

(*
//
// HX: support for union type may be removed
//
absviewt@ype
opt_viewt0ype_int_viewt0ype
  (a:viewt@ype+, i:int) = union (i) { value= a }
stadef opt = value_viewt0ype_int_viewt0ype
*)

absviewt@ype
opt_viewt0ype_bool_viewt0ype (a:viewt@ype+, opt:bool) = a
stadef opt = opt_viewt0ype_bool_viewt0ype

//
// HX-2010-03-23: resulting in incorrect erasure
// stadef Opt (a:viewt@ype) = [b:bool] opt (a, b)
//

(* ****** ****** *)

(*
// HX-2008:
// this is not yet supported and may never be
datasort stamp = (* abstract *) // be supported
// sta vfrac : (stamp, view, rat) -> view
absview vfrac (stamp, view, rat)
// sta vtfrac : (stamp, viewtype, rat) -> viewtype
absviewtype vtfrac (stamp, viewtype, rat)
*)

(* ****** ****** *)
//
// HX: built-in dependent type constructors
//
abstype
array0_viewt0ype_type (elt:viewt@ype) // unnamed
abstype
array_viewt0ype_int_type (elt:viewt@ype, sz:int) // unnamed
//
abstype
matrix0_viewt0ype_type (elt:viewt@ype) // unnamed
abstype
matrix_viewt0ype_int_int_type (elt:viewt@ype, nrow:int, ncol:int)
//
(* ****** ****** *)

abst@ype bool_bool_t0ype (bool) = bool_t0ype

abst@ype char_char_t0ype (char) = char_t0ype

abst@ype int_int_t0ype (int) = int_t0ype
abst@ype uint_int_t0ype (int) = uint_t0ype

abst@ype lint_int_t0ype (int) = int_long_t0ype
abst@ype ulint_int_t0ype (int) = uint_long_t0ype

abst@ype llint_int_t0ype (int) = int_long_long_t0ype
abst@ype ullint_int_t0ype (int) = uint_long_long_t0ype

abst@ype size_int_t0ype (i:int) = size_t0ype
abst@ype ssize_int_t0ype (i:int) = ssize_t0ype

abst@ype ptrdiff_int_t0ype (i:int) = ptrdiff_t0ype

(* ****** ****** *)

abstype ptr_addr_type (addr) = ptr_type // named

(* ****** ****** *)

abstype string_int_type (int) // unnamed
abstype stropt_int_type (int) // unnamed
abst@ype
strbuf_int_int_t0ype (bsz: int, len: int) // of variable size
absviewtype
strptr_addr_int_viewtype (addr, int) // for linear strings
absviewtype strptr_addr_viewtype (addr) // for linear strings

(* ****** ****** *)
//
// HX: The following definitions are needed in the ATS constraint solver
//
// absolute value function relation
//
stadef abs_int_int_bool (x: int, v: int): bool =
  (x >= 0 && x == v) || (x <= 0 && ~x == v)
stadef abs_r = abs_int_int_bool
//
// HX: in-between relation
//
stadef btw_int_int_int_bool (x: int, y: int, z:int): bool =
  (x <= y && y < z)
//
// HX: int_of_bool conversion
//
stadef int_of_bool_bool (b: bool, v: int): bool =
  (b && v == 1) || (~b && v == 0)
//
// HX: subtraction relation on natural numbers
//
stadef nsub_int_int_int_bool (x: int, y: int, v: int): bool =
  (x >= y && v == x - y) || (x <= y && v == 0)
stadef nsub_r = nsub_int_int_int_bool
//
// HX: maximum function relation
//
stadef max_int_int_int_bool (x: int, y: int, v: int): bool =
  (x >= y && x == v) || (x <= y && y == v)
stadef max_r = max_int_int_int_bool
//
// HX: minimum function relation
//
stadef min_int_int_int_bool (x: int, y: int, v: int): bool =
  (x >= y && y == v) || (x <= y && x == v)
stadef min_r = min_int_int_int_bool
//
// HX: sign function relation
//
stadef sgn_int_int_bool (x: int, v: int): bool =
  (x > 0 && v == 1) || (x == 0 && v == 0) || (x < 0 && v == ~1)
stadef sgn_r = sgn_int_int_bool
//
// HX: division relation (nat)
//
stadef ndiv_int_int_int_bool (x: int, y: int, q: int): bool =
  (q * y <= x && x < q * y + y)
stadef ndiv_r = ndiv_int_int_int_bool
//
// HX: division relation (int)
//
stadef div_int_int_int_bool (x: int, y: int, q: int) =
  (x >= 0 && y > 0 && ndiv_int_int_int_bool (x, y, q)) ||
  (x >= 0 && y < 0 && ndiv_int_int_int_bool (x, ~y, ~q)) ||
  (x <= 0 && y > 0 && ndiv_int_int_int_bool (~x, y, ~q)) ||
  (x <= 0 && y < 0 && ndiv_int_int_int_bool (~x, ~y, q))
stadef div_r = div_int_int_int_bool
//
// HX: modulo relation // not handled yet
//
(* ****** ****** *)

stadef
size_int_int_bool
  (sz:int, n:int) = n >= 0
stacst sizeof_viewt0ype_int : viewt@ype -> int
stadef sizeof = sizeof_viewt0ype_int

(* ****** ****** *)
//
// HX: introduce some short names
//
stadef array0 = array0_viewt0ype_type // with dynamic size
stadef array = array_viewt0ype_int_type // without dynamic size
stadef matrix0 = matrix0_viewt0ype_type // with dynamic size
stadef matrix = matrix_viewt0ype_int_int_type // without dynamic size

stadef bool = bool_t0ype
stadef bool = bool_bool_t0ype

stadef byte = byte_t0ype

(* ****** ****** *)

stadef char = char_t0ype
stadef char = char_char_t0ype

stadef uchar = uchar_t0ype
stadef schar = schar_t0ype

(* ****** ****** *)

stadef double = double_t0ype

(* ****** ****** *)

stadef exn = exception_viewtype // a boxed type

(* ****** ****** *)

stadef int = int_t0ype
stadef int = int_int_t0ype

stadef uint = uint_t0ype
stadef uint = uint_int_t0ype

stadef size_t = size_t0ype
stadef size_t = size_int_t0ype

stadef ssize_t = ssize_t0ype
stadef ssize_t = ssize_int_t0ype

stadef ptrdiff_t = ptrdiff_t0ype
stadef ptrdiff_t = ptrdiff_int_t0ype

(* ****** ****** *)

stadef ptr = ptr_type
stadef ptr = ptr_addr_type

(* ****** ****** *)

stadef strbuf = strbuf_int_int_t0ype
stadef strbuf (bsz:int) = [len:int | 0 <= len] strbuf (bsz, len)

stadef string = string_int_type
stadef string = string_type
stadef stropt = stropt_int_type
stadef strptr = strptr_addr_viewtype // for linear strings
stadef strptr0 = [l:addr] strptr (l)
stadef strptr1 = [l:addr | l > null] strptr (l)
stadef strptrlen = strptr_addr_int_viewtype // for linear strings with length

(* ****** ****** *)

stadef void = void_t0ype // sizeof(void) = 1
stadef empty = empty_t0ype // sizeof(empty) = 0
stadef undefined_t= undefined_t0ype // sizeof(undefined) = ?
stadef undefined_vt= undefined_viewt0ype // sizeof(undefined_vt) = ?

(* ****** ****** *)

datatype unit = unit of ()

typedef Bool = [b:bool] bool b

typedef Char = [c:char] char c

typedef Int = [i:int] int i
typedef intLt (n:int) = [i:int | i < n] int i
typedef intLte (n:int) = [i:int | i <= n] int i
typedef intGt (n:int) = [i:int | i > n] int i
typedef intGte (n:int) = [i:int | i >= n] int i
typedef intBtw (lb:int, ub:int) = [i: int | lb <= i; i < ub] int i
typedef intBtwe (lb:int, ub:int) = [i: int | lb <= i; i <= ub] int i

typedef Nat = [n:int | n >= 0] int n
typedef natLt (n:int) = [i:int | 0 <=i; i < n] int i
typedef natLte (n:int) = [i:int | 0 <= i; i <= n] int i

typedef Pos = intGt 0
typedef Neg = intLt 0

typedef Sgn = [i:int | ~1 <= i; i <= 1] int i

typedef Ptr = [l:addr] ptr (l)
typedef Ptr1 = [l:addr | l > null] ptr (l)

typedef String = [n:int | n >= 0] string n
typedef Stropt = [n:int] stropt n

typedef uInt = [n:int | n >=0] uint n

(* ****** ****** *)

typedef sizeof_t
  (a:viewt@ype) = size_int_t0ype (sizeof_viewt0ype_int a)
// end of [sizeof_t]

typedef Size = [i:int | i >= 0] size_t i
typedef sizeLt (n: int) = [i:int | 0 <= i; i < n] size_t (i)
typedef sizeLte (n: int) = [i:int | 0 <= i; i <= n] size_t (i)
typedef sizeGt (n: int) = [i:int | i > n] size_t (i)
typedef sizeGte (n: int) = [i:int | i >= n] size_t (i)
typedef sizeBtw (lb:int, ub:int) = [i: int | lb <= i; i < ub] size_t i
typedef sizeBtwe (lb:int, ub:int) = [i: int | lb <= i; i <= ub] size_t i

typedef SSize = [i:int] ssize_t i
typedef ssizeBtw (lb:int, ub:int) = [i: int | lb <= i; i < ub] ssize_t i
typedef ssizeBtwe (lb:int, ub:int) = [i: int | lb <= i; i <= ub] ssize_t i

(* ****** ****** *)
//
// HX: for memory deallocation (with/without GC)
//
absview
free_gc_addr_view (l:addr)
stadef free_gc_v = free_gc_addr_view
absview
free_ngc_addr_view (l:addr)
stadef free_ngc_v = free_ngc_addr_view

absview
free_gc_viewt0ype_addr_view
  (a:viewt@ype+, l:addr)
stadef free_gc_v = free_gc_viewt0ype_addr_view
absview
free_ngc_viewt0ype_addr_view
  (a:viewt@ype+, l:addr)
stadef free_ngc_v = free_ngc_viewt0ype_addr_view

absview
free_gc_viewt0ype_int_addr_view
  (a:viewt@ype+, n:int, l:addr)
stadef free_gc_v = free_gc_viewt0ype_int_addr_view
absview
free_ngc_viewt0ype_int_addr_view
  (a:viewt@ype+, n:int, l: addr)
stadef free_ngc_v = free_ngc_viewt0ype_int_addr_view

absview
freebyte_gc_int_addr_view (n:int, l:addr)
stadef freebyte_gc_v = freebyte_gc_int_addr_view
absview
freebyte_ngc_int_addr_view (n:int, l:addr)
stadef freebyte_ngc_v = freebyte_ngc_int_addr_view

(* ****** ****** *)
//
// HX:
// values of viewtype [junkptr] need to be freed by calling [free];
// note that the viewtype [junkptr] may be just defined as follows:
// [a:viewt@ype; l:addr] (free_gc_v (a?, 1, l), a? @ l | ptr l)
//
absviewtype junkptr_viewtype // unnamed
stadef junkptr = junkptr_viewtype // shorthand

(* ****** ****** *)
//
// HX: This definition should not be changed!
//
viewtypedef
arraysize_viewt0ype_int_viewt0ype
  (a:viewt@ype, n:int) =
  [l:addr] (free_gc_v (a?, n, l), @[a][n] @ l | ptr l, size_t n)
// end of [viewtypedef]

stadef arraysize = arraysize_viewt0ype_int_viewt0ype

viewtypedef Arraysize
  (a:viewt@ype) = [n:int | n >= 0] arraysize (a, n)
// end of [Arraysize]

(* ****** ****** *)
//
// HX: closure, closure pointer and closure reference
//
absviewt@ype
clo_viewt0ype_viewt0ype (_fun: viewt@ype+) // unnamed
stadef clo = clo_viewt0ype_viewt0ype

absviewtype
cloptr_viewt0ype_viewtype (_fun: viewt@ype+) // unnamed
stadef cloptr = cloptr_viewt0ype_viewtype

abstype
cloref_t0ype_type (_fun: t@ype) // unnamed
stadef cloref = cloref_t0ype_type

(* ****** ****** *)
//
// HX: for handling read-only data
//
(*
absviewt@ype
READ_viewt0ype_viewt0ype
  (a: viewt@ype+, int) = a
// end of [READ_viewt0ype_viewt0ype]
*)

(*
viewtypedef
READ_viewt0ype_int_viewt0ype
  (a: viewt@ype, stamp: int) = a
stadef READ = READ_viewt0ype_int_viewt0ype
*)

viewtypedef
READ_viewt0ype_viewt0ype (a: viewt@ype) = a
stadef READ = READ_viewt0ype_viewt0ype

(* ****** ****** *)
//
// HX: for print-format strings
//
abstype
printf_c_types_type (types) // boxed type: string
stadef printf_c = printf_c_types_type

(* ****** ****** *)
//
// HX: for handling variadic functions
//
absviewt@ype
va_list_viewt0ype = $extype"ats_va_list_viewtype"
absviewt@ype
va_list_types_viewt0ype (types) = va_list_viewt0ype

stadef va_list = va_list_viewt0ype
stadef va_list = va_list_types_viewt0ype

(* ****** ****** *)

datasort file_mode =
  | file_mode_r (* read *)
  | file_mode_w (* write *)
  | file_mode_rw (* read and write *)
// end of [file_mode]
//
stadef r = file_mode_r ()
stadef w = file_mode_w ()
stadef rw = file_mode_rw ()
//
// [ats_FILE_viewtype] is defined in [libc/CATS/stdio.cats]
//
absviewt@ype
FILE_viewt0ype (file_mode) = $extype"ats_FILE_viewtype"
stadef FILE = FILE_viewt0ype
//
// HX-2010-05-02:
// this is easy to use but unsafe and should probably be DEPRECATED?
//
// [FILEref_type] is [ref (FILE m)] for some [m]
//
abstype FILEref_type // unnamed
stadef FILEref = FILEref_type // shorthand
//
(* ****** ****** *)
//
// HX: some common datatypes
//
datatype
box_t0ype_type (a:t@ype+) = box (a) of a
stadef box = box_t0ype_type
//
dataviewtype
box_viewt0ype_viewtype (a:viewt@ype+) = box_vt (a) of a
stadef box_vt = box_viewt0ype_viewtype
//
// HX: [list0_t0ype_type] is co-variant
//
datatype list0_t0ype_type (a: t@ype+) =
  | list0_cons (a) of (a, list0_t0ype_type a) | list0_nil (a) of ()
// end of [list0_t0ype_type]
stadef list0 = list0_t0ype_type
//
datatype // t@ype+: covariant
list_t0ype_int_type (a:t@ype+, int) =
  | {n:int | n >= 0}
    list_cons (a, n+1) of (a, list_t0ype_int_type (a, n))
  | list_nil (a, 0)
// end of [datatype]
stadef list = list_t0ype_int_type
typedef List (a:t@ype) = [n:int | n >= 0] list (a, n)
//
// HX: [option0_t0ype_type] is co-variant
//
datatype option0_t0ype_type (a: t@ype) =
  | option0_some (a) of (a) | option0_none (a) of ()
// end of [datatype]
stadef option0 = option0_t0ype_type
//
datatype // t@ype+: covariant
option_t0ype_bool_type (a:t@ype+, bool) =
  | None (a, false) | Some (a, true) of a
// end of [datatype]
stadef option = option_t0ype_bool_type
typedef Option (a:t@ype) = [b:bool] option (a, b)
//
(* ****** ****** *)
//
// HX: some common dataviewtypes
//
dataviewtype // viewt@ype+: covariant
list_viewt0ype_int_viewtype (a:viewt@ype+, int) =
  | {n:int | n >= 0}
    list_vt_cons (a, n+1) of (a, list_viewt0ype_int_viewtype (a, n))
  | list_vt_nil (a, 0)
// end of [list_viewt0ype_int_viewtype]
stadef list_vt = list_viewt0ype_int_viewtype
viewtypedef List_vt (a:viewt@ype) = [n:int | n >=0] list_vt (a, n)
//
dataviewtype // viewt@ype+: covariant
option_viewt0ype_bool_viewtype (a:viewt@ype+, bool) =
  | None_vt (a, false) | Some_vt (a, true) of a
// end of [option_viewt0ype_bool_viewtype]
stadef option_vt = option_viewt0ype_bool_viewtype
viewtypedef Option_vt (a:viewt@ype) = [b:bool] option_vt (a, b)
//
(* ****** ****** *)
//
// HX: some useful props and views
//
dataprop unit_p = unit_p of ()
dataview unit_v = unit_v of ()
//
dataview
option_view_bool_view
  (a:view+, bool) = Some_v (a, true) of a | None_v (a, false)
// end of [option_view_bool_view]
stadef option_v = option_view_bool_view
viewdef optvar_v (a:viewt@ype, l:addr) = option_v (a @ l, l > null)
//
dataview
disj_view_view_int_view
  (v0: view, v1: view, int) =
  | InsLeft_v (v0, v1, 0) of v0 | InsRight_v (v0, v1, 1) of v1
// end of [dataview or_view_view_int_view]
stadef disj_v = disj_view_view_int_view
//
// HX: subview relation that only allows *reading*
//
absprop vsubr_p (v1:view+, v2: view-) // v2 -<prf> [v:iew] @(v1, v)
stadef <= (v1:view, v2:view) = vsubr_p (v1, v2)
//
// HX: subview relation that allows *reading* and *writing*
//
absprop vsubw_p (v1:view, v2: view) // v2 -<prf> @(v1, v1 -<lin,prf> v2)
//
(* ****** ****** *)
//
absviewt@ype
crypt_viewt0ype_viewt0ype (a:viewt@ype) = a
stadef crypt = crypt_viewt0ype_viewt0ype
//
(* ****** ****** *)
//
// HX:
// [lazy(T)] : suspended computation with a value of type T
//
abstype
lazy_t0ype_type (t@ype+) // boxed type // unnamed
stadef lazy = lazy_t0ype_type
//
// HX: [lazy_vt(VT)] :
// suspended computation with a linear value of viewtype VT
//
absviewtype
lazy_viewt0ype_viewtype
  (viewt@ype+) // boxed linear type // unnamed
stadef lazy_vt = lazy_viewt0ype_viewtype
//
(* ****** ****** *)
//
// HX: lazy streams
//
datatype
stream_con (a:t@ype+) =
  | stream_nil (a) | stream_cons (a) of (a, stream a)
where stream (a:t@ype) = lazy (stream_con a)
//
// HX: lazy linear streams
//
dataviewtype
stream_vt_con (a:viewt@ype+) =
  | stream_vt_nil (a) | stream_vt_cons (a) of (a, stream_vt a)
where stream_vt (a:viewt@ype) = lazy_vt (stream_vt_con a)
//
(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [basic_sta.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [basics_sta.sats] *)
