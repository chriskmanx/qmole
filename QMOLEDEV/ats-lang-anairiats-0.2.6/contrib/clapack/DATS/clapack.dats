(*
**
** An interface for ATS to interact with BLAS
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"

(* ****** ****** *)

staload "contrib/clapack/SATS/f2c.sats"
staload "contrib/clapack/SATS/clapack.sats"

(* ****** ****** *)

(*
//
// implemented in [clapack.hats]
//
implement{t1,t2}
  lange_inf {m,n}
  (m, n, a, lda) = ans where {
  var norm: CLAPACK_NORM_t = ClapackNormInf
  val m_sz = size1_of_integer m
  val [l_arr:addr] (pf_gc, pf_arr | p_arr) =
    array_ptr_alloc_tsz {t2} (m_sz+1, sizeof<t2>)
  prval pf_work = langework_v_some {t2,'I',m+1,l_arr} (pf_arr)
  val ans = lange<t1,t2> (pf_work | norm, m, n, a, lda, p_arr)
  prval langework_v_some pf_arr = pf_work
  val () = array_ptr_free {t2} (pf_gc, pf_arr | p_arr)
} (* end of [lange_inf] *)
*)

(* ****** ****** *)

local

assume gels_lwork_p
  (tr:transpose, m:int, n:int, nrhs:int, lwork:int) =
  [lwork >= 1; lwork >= 2*min(m,n); lwork >= min(m,n) + nrhs] void
// end of [assume]

in // in of [local]

implement lemma_gels_lwork () = ()

end // end of [local]

// implement{t} gels (...) = ... // in [clapack.hats]

implement{t} gels_work_query
  {tr} {m,n} {nrhs} (trans, m, n, nrhs) = let
//
extern fun{t:t@ype} __gels
  {tr: transpose} {m,n:pos} {nrhs:nat} (
   trans: CLAPACK_TRANSPOSE_t tr
 , m: integer m, n: integer n, nrhs: integer nrhs
 , p_a: ptr null, lda: integer // >= m
 , p_b: ptr null, ldb: integer // >= max (m,n)
 , work: &t? >> t, lwork: integer (~1)
 ) :<> int
 = "atsctrb_clapack_gels"
//
  val lda = m
  val ldb = max_integer_integer (m,n) // a bit arbitrary
  var work: t // uninitialized
  var lwork = integer_of_int1 (~1)
//
  val info = __gels<t>
    (trans, m, n, nrhs, null(*a*), lda, null(*b*), ldb, work, lwork)
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gels_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // if [clapack_gels] is correct
    extern prfun lemma (): [lwork>0] gels_lwork_p (tr, m, n, nrhs, lwork)
  } // end of [prval]
in
  (pf_lwork | lwork)
end (* end of [gels_work_query] *)

(* ****** ****** *)

local

assume gesvd_lwork_p (m:int, n:int, lwork:int) =
  [lwork >= 1; lwork >= 3*min(m,n)+max(m,n); lwork >= 5*min(m,n)] void
// end of [assume]

in // in of [local]

implement lemma_gesvd_lwork () = ()

end // end of [local]

(* ****** ****** *)

implement{t1,t2} gesvd
  (pf_lwork | m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat} {mn:int | mn==min(m,n)}
    {lda,ldu,ldvt:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'A'
    , jobvt: char 'A'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda)) >> GEMAT(t1?, m, n, lda)
    , lda: integer lda
    , s: &(@[t2?][mn]) >> @[t2][mn]
    , u: &(GEMAT (t1?, m, m, ldu)) >> GEMAT (t1, m, m, ldu)
    , ldu: integer ldu
    , vt: &(GEMAT (t1?, n, n, ldvt)) >> GEMAT (t1, n, n, ldvt)
    , ldvt: integer ldvt
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (pf_lwork | 'A', 'A', m, n, a, lda, s, u, ldu, vt, ldvt,
                              work, lwork)
end // end of [gesvd]

//

implement{t1,t2} // |t1| = t2
gesvd_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'A', jobvt: char 'A'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= n
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = m and ldvt = n
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'A', 'A', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_work_query]

(* ****** ****** *)

implement{t1,t2} gesvd_econ
  (pf_lwork | m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {mn:int | mn==min(m,n)}
    {lda,ldu,ldvt:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'S'
    , jobvt: char 'S'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda)) >> GEMAT(t1?, m, n, lda)
    , lda: integer lda
    , s: &(@[t2?][mn]) >> @[t2][mn]
    , u: &(GEMAT (t1?, m, mn, ldu)) >> GEMAT (t1, m, mn, ldu)
    , ldu: integer ldu
    , vt: &(GEMAT (t1?, mn, n, ldvt)) >> GEMAT (t1, mn, n, ldvt)
    , ldvt: integer ldvt
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (pf_lwork | 'S', 'S', m, n, a, lda, s, u, ldu, vt, ldvt,
                              work, lwork)
end // end of [gesvd_econ]

//

implement{t1,t2} // |t1| = t2
gesvd_econ_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'S', jobvt: char 'S'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= n
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = m and ldvt = n
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'S', 'S', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_econ_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_econ_work_query]

(* ****** ****** *)

implement{t1,t2} gesvd_sing
  (pf_lwork | m, n, a, lda, s, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {mn:int | mn==min(m,n)}
    {lda:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'N'
    , jobvt: char 'N'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda)) >> GEMAT(t1?, m, n, lda)
    , lda: integer lda
    , s: &(@[t2?][mn]) >> @[t2][mn]
    , u: ptr null
    , ldu: integer
    , vt: ptr null
    , ldvt: integer
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'N', 'N', m, n, a, lda, s
      , null(*u*), integer_of_int1 1(*ldu*)
      , null(*vt*), integer_of_int1 1(*ldvt*)
      , work, lwork)
end // end of [gesvd_sing]

//

implement{t1,t2} // |t1| = t2
gesvd_sing_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'N', jobvt: char 'N'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= 1
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = integer_of_int1 1 and ldvt = integer_of_int1 1
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'N', 'N', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_sing_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_sing_work_query]

(* ****** ****** *)

implement{t1,t2} gesvd_left
  (pf_lwork | m, n, a, lda, s, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {lda:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'O'
    , jobvt: char 'N'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda))
    , lda: integer lda
    , s: &(@[t2?][n]) >> @[t2][n]
    , u: ptr null
    , ldu: integer
    , vt: ptr null
    , ldvt: integer
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'O', 'N', m, n, a, lda, s
      , null(*u*), m(*ldu*)
      , null(*vt*), integer_of_int1 1(*ldvt*)
      , work, lwork)
end // end of [gesvd_left]

//

implement{t1,t2} // |t1| = t2
gesvd_left_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'O', jobvt: char 'N'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = m and ldvt = integer_of_int1 1
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'O', 'N', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_left_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_left_work_query]

(* ****** ****** *)

implement{t1,t2} gesvd_right
  (pf_lwork | m, n, a, lda, s, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {lda:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'N'
    , jobvt: char 'O'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda))
    , lda: integer lda
    , s: &(@[t2?][m]) >> @[t2][m]
    , u: ptr null
    , ldu: integer
    , vt: ptr null
    , ldvt: integer
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'N', 'O', m, n, a, lda, s
      , null(*u*), integer_of_int1 1(*ldu*)
      , null(*vt*), integer_of_int1 1(*ldvt*)
      , work, lwork)
end // end of [gesvd_right]

//

implement{t1,t2} // |t1| = t2
gesvd_right_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'N', jobvt: char 'O'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= 1
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = integer_of_int1 1 and ldvt = integer_of_int1 1
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'N', 'O', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_right_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_right_work_query]

(* ****** ****** *)

implement{t1,t2} gesvd_skinny
  (pf_lwork | m, n, a, lda, s, vt, ldvt, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {lda,ldvt:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'O'
    , jobvt: char 'S'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda))
    , lda: integer lda
    , s: &(@[t2?][n]) >> @[t2][n]
    , u: ptr null
    , ldu: integer
    , vt: &(GEMAT (t1?, n, n, ldvt)) >> GEMAT (t1, n, n, ldvt)
    , ldvt: integer ldvt
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'O', 'S', m, n, a, lda, s
      , null(*u*), integer_of_int1 1(*ldu*)
      , vt, ldvt
      , work, lwork)
end // end of [gesvd_skinny]

//

implement{t1,t2} // |t1| = t2
gesvd_skinny_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'O', jobvt: char 'S'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = integer_of_int1 1 and ldvt = n
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'O', 'S', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_skinny_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_skinny_work_query]

(* ****** ****** *)

implement{t1,t2}
gesvd_skinny_right
  (pf_lwork | m, n, a, lda, s, vt, ldvt, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {lda,ldvt:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'N'
    , jobvt: char 'A'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda)) >> GEMAT (t1?, m, n, lda)
    , lda: integer lda
    , s: &(@[t2?][n]) >> @[t2][n]
    , u: ptr null
    , ldu: integer
    , vt: &(GEMAT (t1?, n, n, ldvt)) >> GEMAT (t1, n, n, ldvt)
    , ldvt: integer ldvt
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'N', 'A', m, n, a, lda, s
      , null(*u*), integer_of_int1 1(*ldu*)
      , vt, ldvt
      , work, lwork)
end // end of [gesvd_skinny_right]

//

implement{t1,t2} // |t1| = t2
gesvd_skinny_right_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'N', jobvt: char 'A'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = integer_of_int1 1 and ldvt = n
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'N', 'A', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_skinny_right_work_query]: failed\n";
    exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_skinny_right_work_query]

(* ****** ****** *)

implement{t1,t2} gesvd_fat
  (pf_lwork | m, n, a, lda, s, u, ldu, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {lda,ldu:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'S'
    , jobvt: char 'O'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda))
    , lda: integer lda
    , s: &(@[t2?][m]) >> @[t2][m]
    , u: &(GEMAT (t1?, m, m, ldu)) >> GEMAT (t1, m, m, ldu)
    , ldu: integer ldu
    , vt: ptr null
    , ldvt: integer
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'S', 'O', m, n, a, lda, s
      , u, ldu
      , null(*vt*), integer_of_int1 1(*ldvt*)
      , work, lwork)
end // end of [gesvd_fat]

//

implement{t1,t2} // |t1| = t2
gesvd_fat_work_query {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'S', jobvt: char 'O'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = m and ldvt = n
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'S', 'O', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_fat_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_fat_work_query]

(* ****** ****** *)

implement{t1,t2}
gesvd_fat_left
  (pf_lwork | m, n, a, lda, s, u, ldu, work, lwork) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:nat}
    {lda,ldu:pos}
    {lwork:pos} (
      pf_lwork: gesvd_lwork_p (m, n, lwork)
    | jobu: char 'A'
    , jobvt: char 'N'
    , m: integer m, n: integer n
    , a: &(GEMAT (t1, m, n, lda)) >> GEMAT (t1?, m, n, lda)
    , lda: integer lda
    , s: &(@[t2?][m]) >> @[t2][m]
    , u: &(GEMAT (t1?, m, m, ldu)) >> GEMAT (t1, m, m, ldu)
    , ldu: integer ldu
    , vt: ptr null
    , ldvt: integer
    , work: &(@[t1?][lwork]), lwork: integer lwork
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
in
  __gesvd<t1,t2> (
        pf_lwork
      | 'A', 'N', m, n, a, lda, s
      , u, ldu
      , null(*vt*), integer_of_int1 1(*ldvt*)
      , work, lwork)
end // end of [gesvd_fat_left]

//

implement{t1,t2} // |t1| = t2
gesvd_fat_left_work_query
  {m,n} (m, n) = let
//
  extern
  fun{t1,t2:t@ype} __gesvd
    {m,n:pos} (
      jobu: char 'A', jobvt: char 'N'
    , m: integer m, n: integer n
    , a: ptr null, lda: integer // >= m
    , s: ptr null
    , u: ptr null, ldu: integer // >= m
    , vt: ptr null, ldvt: integer // >= 1
    , work: &t1? >> t1, lwork: integer (~1)
    ) :<> int
    = "atsctrb_clapack_gesvd"
//
  val lda = m and ldu = m and ldvt = n
  var work: t1 // uninitialized
  val lwork = integer_of_int1 (~1)
  val info = __gesvd<t1,t2> (
    'A', 'N', m, n
  , null(*a*), lda, null(*s*), null(*u*), ldu, null(*vt*), ldvt
  , work, lwork
  )  // end of [val]
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [gesvd_fat_left_work_query]: failed\n"; exit (1)
  ) // end of [val]
//
  val [lwork:int] lwork = to_integer (work)
  prval pf_lwork = lemma () where { // assume that [clapack_gesvd] is
    extern prfun lemma (): [lwork>0] gesvd_lwork_p (m, n, lwork) // correct
  } // end of [prval]
in
  (pf_lwork | lwork)
end // end of [gesvd_fat_left_work_query]

(* ****** ****** *)

implement{t}
gelqf_work_query
  {m,n} (m, n) = let
//
  extern fun{t:t@ype} __gelqf (
    m: integer m, n: integer n
  , a: ptr null, lda: integer
  , ltau: ptr null, work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_gelqf"
//
  var work = of_double<t> 0.0
  val info = __gelqf (m, n, null, m, null,
                      work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK); [gelqf_work_query]: failed\n";
    exit (1)
  )
  val [lwork:int] lwork = to_integer<t> (work)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m] ()
  }
in
  lwork
end // of [gelqf_work_query]

(* ****** ****** *)

implement{t}
ormlq_work_query
  {m,n,k} {na} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __ormlq (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_ormlq"
//
  var work = of_double<t> 0.0
  val info = __ormlq (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [ormlq_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - na] ()
  }
in
  lwork
end // end of [ormlq_work_query]

implement{t}
unmlq_work_query
  {m,n,k} {na} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __unmlq (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_unmlq"
//
  var work = of_double<t> 0.0
  val info = __unmlq (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [unmlq_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - na] ()
  }
in
  lwork
end // end of [unmlq_work_query]

(* ****** ****** *)

implement{a}
TRMAT_of_QLMAT (pf_qlmat | m, n, pa) = let
  val mn = sub_integer_integer (m, n)
  val mn = size1_of_integer (mn)
  val [ofs:int] (pf_mul | ofs) = mul2_size1_size1 (mn, sizeof<a>)
  prval (pf_trmat, fpf_qlmat) = TRMAT_v_of_QLMAT_v {a} (pf_mul, pf_qlmat)
in
  (pf_trmat, fpf_qlmat | pa + ofs)
end // end of [TRMAT_of_QLMAT]

implement{t}
geqlf_work_query
  {m,n} (m, n) = let
//
  extern fun{t:t@ype} __geqlf (
    m: integer m, n: integer n
  , a: ptr null, lda: integer
  , ltau: ptr null, work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_geqlf"
//
  var work = of_double<t> 0.0
  val info = __geqlf (m, n, null, m, null,
                      work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK); [geqlf_work_query]: failed\n";
    exit (1)
  )
  val [lwork:int] lwork = to_integer<t> (work)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= n] ()
  }
in
  lwork
end // of [geqlf_work_query]

implement{t}
ormql_work_query
  {m,n,k} {ma} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __ormql (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_ormql"
//
  var work = of_double<t> 0.0
  val info = __ormql (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [ormql_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - ma] ()
  }
in
  lwork
end // end of [ormql_work_query]

implement{t}
unmql_work_query
  {m,n,k} {ma} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __unmql (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_unmql"
//
  var work = of_double<t> 0.0
  val info = __unmql (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [unmql_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - ma] ()
  }
in
  lwork
end // end of [unmql_work_query]

(* ****** ****** *)

implement{t}
geqrf_work_query
  {m,n} (m, n) = let
//
  extern fun{t:t@ype} __geqrf (
    m: integer m, n: integer n
  , a: ptr null, lda: integer
  , ltau: ptr null, work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_geqrf"
//
  var work = of_double<t> 0.0
  val info = __geqrf (m, n, null, m, null,
                      work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK); [geqrf_work_query]: failed\n";
    exit (1)
  )
  val [lwork:int] lwork = to_integer<t> (work)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= n] ()
  }
in
  lwork
end // of [geqrf_work_query]

implement{t}
ormqr_work_query
  {m,n,k} {ma} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __ormqr (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_ormqr"
//
  var work = of_double<t> 0.0
  val info = __ormqr (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [ormqr_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - ma] ()
  }
in
  lwork
end // end of [ormqr_work_query]

implement{t}
unmqr_work_query
  {m,n,k} {ma} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __unmqr (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_unmqr"
//
  var work = of_double<t> 0.0
  val info = __unmqr (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [unmqr_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - ma] ()
  }
in
  lwork
end // end of [unmqr_work_query]

(* ****** ****** *)

implement{a}
TRMAT_of_RQMAT
  (pf_rqmat | m, n, lda, pa) = let
  val nm = sub_integer_integer (n, m)
  val nm = size1_of_integer (nm)
  val lda_ = size1_of_integer (lda)
  val [ofs1:int] (pf_mul1 | ofs1) = mul2_size1_size1 (nm, lda_)
  prval () = mul_nat_nat_nat (pf_mul1)
  val [ofs:int] (pf_mul | ofs) = mul2_size1_size1 (ofs1, sizeof<a>)
  prval (pf_trmat, fpf_rqmat) = TRMAT_v_of_RQMAT_v {a} (
    pf_mul1, pf_mul, pf_rqmat
  )
in
  (pf_trmat, fpf_rqmat | pa + ofs)
end // end of [TRMAT_of_RQMAT]

implement{t}
gerqf_work_query
  {m,n} (m, n) = let
//
  extern fun{t:t@ype} __gerqf (
    m: integer m, n: integer n
  , a: ptr null, lda: integer
  , ltau: ptr null, work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_gerqf"
//
  var work = of_double<t> 0.0
  val info = __gerqf (m, n, null, m, null,
                      work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK); [gerqf_work_query]: failed\n";
    exit (1)
  )
  val [lwork:int] lwork = to_integer<t> (work)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m] ()
  }
in
  lwork
end // of [gerqf_work_query]

implement{t}
ormrq_work_query
  {m,n,k} {na} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __ormrq (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_ormrq"
//
  var work = of_double<t> 0.0
  val info = __ormrq (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [ormrq_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - na] ()
  }
in
  lwork
end // end of [ormrq_work_query]

implement{t}
unmrq_work_query
  {m,n,k} {na} {lr} {tr}
  (pf | side, trans, m, n, k) = let
//
  extern fun{t:t@ype} __unmrq (
    side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  , a: ptr null, lda: integer, tau: ptr null
  , c: ptr null, ldc: integer
  , work: &t, lwork: integer (~1)
  ) :<> int = "atsctrb_clapack_unmrq"
//
  var work = of_double<t> 0.0
  val info = __unmrq (side, trans, m, n, k, null, m, null,
                      null, m, work, integer_of_int1 (~1))
  val () = if info < 0 then $effmask_all (
    prerr "exit(ATS/CLAPACK): [unmrq_work_query]: failed\n"; exit (1)
  ) // end of [val]
  val [lwork:int] lwork = to_integer<t> (work)
(*
  val () = $effmask_exn (assert (lwork >= m+n-k && lwork > integer_of_int1 0))
*)
  prval pf = lemma_lwork () where {
    extern prfun lemma_lwork () : [lwork > 0 && lwork >= m + n - na] ()
  }
in
  lwork
end // end of [unmrq_work_query]

(* ****** ****** *)

implement{a}
LUMAT_ptr_split_skinny
  {m,n} {lda} {la} {err}
  (pf_lumat | la, m, n, lda) = let
  val ofs = (n * sizeof<a>): size_t
  val [ofs:int] ofs = size1_of_size ofs
  viewdef L = TRMAT (a, n, lower, unit, lda) @ la
  viewdef U = TRMAT (a, n, upper, nonunit, lda) @ la
  viewdef MAT = GEMAT (a, m-n, n, lda) @ la + ofs
  viewdef LUMAT = LUMAT_err_v (a, m, n, lda, la, err)
  prval (pf_l, pf_u, pf_mat, fpf_lumat) = __lemma (pf_lumat) where {
    extern prfun __lemma (pf: LUMAT): (L, U, MAT, (L, U, MAT) -<prf> LUMAT)
  } // end of [prval]
in
  (pf_l, pf_u, pf_mat, fpf_lumat | la + ofs)
end // of [LUMAT_ptr_split_skinny]

(* ****** ****** *)

(* end of [clapack.dats] *)
