//
//
//
// Implementing the Hanoi Tower problem
// The code was written by Hongwei Xi in the summer of 2004
//
//
//

staload _(*anonymous*) = "prelude/DATS/array.dats"

fn play {sz: pos} (sz: size_t sz): void = let
  typedef T = natLte sz
  macdef tsz = sizeof<T>
  typedef post_t = array (T, sz)

  var x: T = 0
  val leftPost = let
    val (pf_gc, pf | p) =
      array_ptr_alloc_tsz {T} (sz, sizeof<T>)
    val () = array_ptr_initialize_elt_tsz {T} (!p, sz, x, tsz)
  in
    array_make_arrsz {T} @(pf_gc, pf | p, sz)
  end // end of [val]

  val middlePost = let
    val (pf_gc, pf | p) =
      array_ptr_alloc_tsz {T} (sz, sizeof<T>)
    val () = array_ptr_initialize_elt_tsz {T} (!p, sz, x, tsz)
  in
    array_make_arrsz {T} @(pf_gc, pf | p, sz)
  end // end of [val]

  val rightPost = let
    val (pf_gc, pf | p) =
      array_ptr_alloc_tsz {T} (sz, sizeof<T>)
    val () = array_ptr_initialize_elt_tsz {T} (!p, sz, x, tsz)
  in
    array_make_arrsz {T} @(pf_gc, pf | p, sz)
  end // end of [val]

  fn initialize (post: post_t):<cloptr1> void = let
    fun aux (i: natLte sz):<cloptr1> void =
      if i < sz then (post[i] := i + 1; aux (i + 1))
  in
    aux (0)
  end // end of [initialize]

  fn showpiece (n: natLte sz):<cloptr1> void = let
    fun aux {i:nat | i <= 2 * sz} .<2*sz-i>. (i: int i):<cloptr1> void =
      if i < (sz - n) then begin
        print ' '; aux (i + 1)
      end else if i < (sz + n - 1) then begin
        print 'O'; aux (i + 1)
      end else if i < (sz + sz) then begin
        print ' '; aux (i + 1)
      end
  in
    aux (0)
  end // end of [showpiece]

  fn showposts ():<cloptr1> void = let
    fun aux (i: natLte sz):<cloptr1> void =
      if i < sz then begin
        showpiece (leftPost[i]);
        showpiece (middlePost[i]);
        showpiece (rightPost[i]);
        print_newline (); 
        aux (i + 1)
      end else begin
        print_newline ()
      end
  in
    aux (0)
  end // end of [showposts]

  val () = initialize (leftPost)

  fun move {
     n,s,p,p':nat |
     p <= sz && p' <= sz && s + p + p' == sz + sz &&
     0 < n && s + n <= sz && n <= p && n <= p' } .<n>.
    (n: int n, source: post_t, s: int s,
     post: post_t, p: int p, post': post_t, p': int p')
    :<cloptr1> void = begin
    if n = 1 then begin
      post[p-1] := source[s]; source[s] := 0; showposts()
    end else begin
      move (n - 1, source, s, post', p', post, p);
      post[p- 1] := source[s+n-1]; source[s+n-1] := 0; showposts ();
      move (n - 1, post', p' - n + 1, post, p - 1, source, s + n)
    end
  end // end of [move]
  val sz_int = int1_of_size1 sz
in
  showposts ();
  move (sz_int, leftPost, 0, rightPost, sz_int, middlePost, sz_int)
end // end of [play]

(* ****** ****** *)

implement main (argc, argv) = play 4

(* ****** ****** *)

(* end of [hanoi.dats] *)
