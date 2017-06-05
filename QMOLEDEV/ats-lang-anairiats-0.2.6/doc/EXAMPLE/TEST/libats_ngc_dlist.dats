(*
** testing some functions declared in
** contrib/linux/utils/SATS/slist.sats
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2011
**
*)

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libats/ngc/SATS/dlist.sats"
staload _(*anon*) = "libats/ngc/DATS/dlist.dats"

(* ****** ****** *)

%{^
typedef
struct {
  char *name ;
  int age ;
  int sex ;
  void *prev ;
  void *next ;
} person_struct ;

ats_ptr_type
person_alloc () {
  return ATS_MALLOC(sizeof(person_struct)) ;
} // end of [person_alloc]

ats_void_type
person_free (ats_ptr_type p) { return ATS_FREE(p) ; }

ats_ptr_type
person_get_prev (
  ats_ptr_type x
) {
  return ((person_struct*)x)->prev ;
} // end of [person_get_prev]

ats_void_type
person_set_prev (
  ats_ptr_type x, ats_ptr_type p
) {
  ((person_struct*)x)->prev = p ; return ;
} // end of [person_set_prev]

ats_ptr_type
person_get_next (
  ats_ptr_type x
) {
  return ((person_struct*)x)->next ;
} // end of [person_get_next]

ats_void_type
person_set_next (
  ats_ptr_type x, ats_ptr_type p
) {
  ((person_struct*)x)->next = p ; return ;
} // end of [person_set_next]

%} // end of [%{]

(* ****** ****** *)

viewtypedef person =
$extype_struct
  "person_struct" of {
  name= strptr1, age= int, sex= int
} // end of [person]

viewtypedef
personlst (nf:int, nr:int) = dlist (person, nf, nr)

(* ****** ****** *)

extern
fun person_alloc
  : dlnode_alloc_type (person) = "person_alloc"
implement dlnode_alloc<person> () = person_alloc ()

(* ****** ****** *)

extern
fun person_free
  : dlnode_free_type (person) = "person_free"
implement dlnode_free<person> (pf | x) = person_free (pf | x)

(* ****** ****** *)

extern
fun person_get_prev
  : dlnode_get_prev_type (person) = "person_get_prev"
implement dlnode_get_prev<person> (pf | x) = person_get_prev (pf | x)

extern
fun person_set_prev
  : dlnode_set_prev_type (person) = "person_set_prev"
implement dlnode_set_prev<person> (pf | x, p) = person_set_prev (pf | x, p)

extern
fun person_get_next
  : dlnode_get_next_type (person) = "person_get_next"
implement dlnode_get_next<person> (pf | x) = person_get_next (pf | x)

extern
fun person_set_next
  : dlnode_set_next_type (person) = "person_set_next"
implement dlnode_set_next<person> (pf | x, p) = person_set_next (pf | x, p)

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

fun personlst_randgen {n:nat}
  (n: int n): personlst (0, n) = let
  viewtypedef a = person
in
  if n > 0 then let
//
    val (pfopt | p) = person_alloc ()
    val () = assertloc (p > null)
    prval Some_v (pfnod) = pfopt
    prval (pfat, fpfnod) = dlnode_v_takeout_val {a?} (pfnod)
//
    val id = randint (10)
    val () = p->name := sprintf ("XYZ-%1d", @(id))
    val () = p->age := randint (100)
    val () = p->sex := randint (2)
//
    prval () = pfnod := fpfnod {a} (pfat)
//
    val xs = personlst_randgen (n-1)
  in
    dlist_cons<person> (pfnod | p, xs)
  end else dlist_nil ()
end // end of [personlst_randgen]

(* ****** ****** *)

implement
main () = () where {
//
  val () = srand48_with_time ()
//
  #define N 5
  val xs = personlst_randgen (N)
//
  val xs = loop (xs) where {
    fun loop
      {nf,nr:int | nr > 0} .<nr>.
      (xs: personlst (nf, nr)): personlst (nf+nr-1, 1) = let
      val () = dlist_appfst_fun<person> (xs
      , lam (x) => $effmask_all (
          printf ("%s(age=%i,sex=%i)\n", @($UN.castvwtp1 {string} (x.name), x.age, x.sex))
        ) (* end of [lam] *)
      ) // end of [val]
    in
      if dlist_is_at_end (xs) then xs else loop (dlist_move_forward<person> (xs))
    end // end of [loop]
  } // end of [val]
//
  val xs = loop (xs) where {
    fun loop
      {nf,nr:int | nr > 0}
      (xs: personlst (nf, nr))
      : personlst (0, nf+nr) = let
      val () = dlist_appfst_fun<person> (xs
      , lam (x) => $effmask_all (
          printf ("%s(age=%i,sex=%i)\n", @($UN.castvwtp1 {string} (x.name), x.age, x.sex))
        ) (* end of [lam] *)
      ) // end of [val]
      prval () = dlist_length_is_nonnegative (xs)
    in
      if dlist_is_at_beg (xs) then xs else loop (dlist_move_backward<person> (xs))
    end // end of [loop]
  } // end of [val]
//
  val () = dlist_free_fun<person> (xs, lam (x) => strptr_free (x.name))
//
} // end of [main]

(* ****** ****** *)

(* end of [libats_ngc_dlist.dats] *)
