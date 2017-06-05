(*
**
** A simple OpenGL example
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Summer, 2008
**
*)

(* ****** ****** *)
//
// staload "contrib/GL/SATS/gl.sats"
//
(* ****** ****** *)

abstype GLlistref

extern fun glListRef_make (): GLlistref = "atsctrb_glListRef_make"

extern fun glListRef_get
  (r: GLlistref): [n:nat] GLlist n = "atsctrb_glListRef_get"
extern fun glListRef_set
  {n:nat} (r: GLlistref, n: GLlist n): void = "atsctrb_glListRef_set"
extern fun glCallListRef (r: GLlistref): void = "atsctrb_glCallListRef"

%{^

static inline
ats_ptr_type atsctrb_glListRef_make () {
  uint *r ;
  r = ats_malloc_gc (sizeof(uint)) ; *r = (uint)0 ;
  return r ;
}

static inline
ats_uint_type
atsctrb_glListRef_get (ats_ptr_type r) {
  uint lst ;
  lst = *(uint*)r ;
  if (lst == 0) {
    fprintf (stderr, "Exit: [glListRef_get] failed.") ;
    exit (1) ;
  }
  *(uint*)r = (uint)0 ;
  return lst ;
}

static inline
ats_void_type
atsctrb_glListRef_set
  (ats_ptr_type r, ats_uint_type lst) {
  if (*(uint*)r != 0) {
    fprintf (stderr, "Exit: [glListRef_set] failed.") ;
    exit (1) ;
  }
  *(uint*)r = (uint)lst ;
  return ;
}

static inline
ats_void_type atsctrb_glCallListRef (ats_ptr_type r) {
  uint lst ;
  lst = *(ats_uint_type*)r ;
  if (lst == 0) {
    fprintf (stderr, "Exit: [glCallList(%u)] failed.\n", lst) ;
    exit (1) ;
  }
  glCallList (lst) ;
  return ;
}

%}
