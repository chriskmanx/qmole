//
//
// An Implementation of the Garsia-Wachs Algorithm
//
// The code uses a doubly-linked list to support a technique that
// is often dubbed Huet's "zipper" in functional programming. See
// the attached ocaml code at the end for an application of "zipper"
//

//
// Time: July 15, 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//


%{^

typedef ats_ptr_type tree1 ;

typedef struct treelst_struct {
  int weight ; tree1 tree ;
  struct treelst_struct *prev ;
  struct treelst_struct *next ;
} *treelst ;

//

static inline
void treelst_free (treelst ts) { ATS_FREE (ts) ; return ; }

//

static inline
treelst TREELSTnil () { return (treelst)0 ; }

static inline
treelst TREELSTcons (int w, tree1 t, treelst ts) {
  treelst ts_new ;
  ts_new = ATS_MALLOC (sizeof(struct treelst_struct)) ;
  ts_new->weight = w ; ts_new->tree = t ;
  ts_new->prev = (treelst)0 ;
  ts_new->next = ts; if (ts) ts->prev = ts_new ;
  return ts_new ;
}

//

extern tree1 Node1_make (tree1 t1, tree1 t2) ;

// [ts] is required to be not null
treelst combine_and_insert (treelst ts1, treelst ts2) {
  int w ; tree1 t ; treelst ts, ts_prev, ts_next, ts2_next ;
/*
  fprintf (stderr, "combine_and_insert: ts1 = %p\n", ts1) ;
  fprintf (stderr, "combine_and_insert: ts2 = %p\n", ts2) ;
*/
  w = ts1->weight + ts2->weight ;
  t = Node1_make (ts1->tree, ts2->tree) ; treelst_free (ts2) ;
  ts1->weight = w ; ts1->tree = t ;
/*
  fprintf (stderr, "combine_and_insert: 1\n") ;
*/
  ts = ts1->prev ; ts2_next = ts2->next ;

  if (ts == (treelst)0) {
    ts1->next = ts2_next ; if (ts2_next) ts2_next->prev = ts1 ;
    return ts1 ;
  }

  ts->next = ts2_next ; if (ts2_next) ts2_next->prev = ts ;

  while (1) { // [ts] is not null at this point!

    if (ts->weight >= w) {
      ts_next = ts->next ;
      ts->next = ts1 ; ts1->prev = ts ;
      ts1->next = ts_next ; if (ts_next) ts_next->prev = ts1 ;
      return ts ;
    }

    ts_prev = ts->prev ;

    if (ts_prev == (treelst)0) {
      ts1->prev = (treelst)0 ; ts1->next = ts ; ts->prev = ts1 ;
      return ts1 ;
    }

    ts = ts_prev ;
  }
} /* end of [trans2_one] */

//

static inline
treelst trans2_one (treelst ts) {
  treelst ts1, ts2, ts3 ;

  ts1 = ts ; ts2 = ts1->next ; ts3 = ts2->next ;

  while (ts3) {
/*
    fprintf (stderr, "trans2_one: ts1 = %p\n", ts1);
    fprintf (stderr, "trans2_one: ts2 = %p\n", ts2);
    fprintf (stderr, "trans2_one: ts3 = %p\n", ts3);
*/
    if (ts1->weight <= ts3->weight) break ;
    ts1 = ts2; ts2 = ts3 ; ts3 = ts2->next ;
  }

  return combine_and_insert (ts1, ts2) ;
}

static inline
tree1 trans2_all (treelst ts, int n) {
  tree1 t ;
  while (n >= 2) { ts = trans2_one (ts) ; n -= 1 ; }
  t = ts->tree ; treelst_free (ts) ;
  return t ;
}

%}

(* ****** ****** *)

typedef depth = Nat
typedef weight = Nat
typedef refdep = ref depth

(* ****** ****** *)
//
// HX:
// The function attaches a ref cell to each element in the input list
// It also computes the length of the input list
//
fun{a:t@ype} trans0 {n:nat}
  (xws: list (@(a, weight), n), len: &int? >> int n)
  : list_vt (@(a, weight, refdep), n) = begin case+ xws of
  | list_cons (xw, xws) => let
      val r = ref_make_elt<depth> 0
      val xwrs = trans0 (xws, len); val () = len := len + 1
    in
      list_vt_cons (@(xw.0, xw.1, r), xwrs)
    end
  | list_nil () => (len := 0; list_vt_nil ())
end // end of [trans0]

(* ****** ****** *)

dataviewtype tree1 (a:t@ype) =
  | Node1 (a) of (tree1 a, tree1 a) | Leaf1 (a) of (a, refdep)
// end of [tree1]

extern fun tree1_free {a:t@ype} (t: tree1 a): void = "tree1_free"

implement tree1_free (t) = begin case+ t of
  | ~Node1 (t1, t2) => (tree1_free t1; tree1_free t2) | ~Leaf1 _ => ()
end // end of [tree1_free]

extern fun Node1_make {a:t@ype}
  (t1: tree1 a, t2: tree1 a): tree1 a = "Node1_make"
implement Node1_make (t1, t2) = Node1 (t1, t2)

(* ****** ****** *)

absviewt@ype treelst (a:t@ype, n:int) = $extype "treelst"

extern fun TREELSTnil {a:t@ype} (): treelst (a, 0) = "TREELSTnil"

extern fun TREELSTcons {a:t@ype} {n:nat}
  (w: weight, t: tree1 a, ts: treelst (a, n)): treelst (a, n+1) = "TREELSTcons"

fun{a:t@ype} trans1 {n:nat}
  (xwrs: !list_vt (@(a, weight, refdep), n)): treelst (a, n) = begin
  case+ xwrs of
  | list_vt_cons (xwr, !xwrs1) => let
      val ts = TREELSTcons (xwr.1, Leaf1 (xwr.0, xwr.2), trans1 !xwrs1)
    in
      fold@ xwrs; ts
    end
  | list_vt_nil () => (fold@ xwrs; TREELSTnil ())
end // end of [trans1]

(* ****** ****** *)

extern fun trans2_all {a:t@ype} {n:int | n >= 1}
  (ts: treelst (a, n), n: int n): tree1 a = "trans2_all"

(* ****** ****** *)

fun{a:t@ype}
mark_depth_and_free (t: tree1 a): void = let
  fun aux (t: tree1 a, d: depth): void = begin case+ t of
    | ~Node1 (t1, t2) => begin
        let val d1 = d+1 in aux (t1, d1); aux (t2, d1) end
      end
    | ~Leaf1 (x, r) => !r := d
  end // end of [aux]
in
  aux (t, 0)
end // end of [mark_depth_and_free]

(* ****** ****** *)

datatype tree (a:t@ype) =
  | Node (a) of (tree a, tree a) | Leaf (a) of (a)

fun{a:t@ype} tree_build
  (xwrs: List_vt @(a, weight, refdep)): tree a = let
  typedef T = @(a, weight, refdep)
  fun aux (xwrs: &List_vt T, d: depth): tree a = begin case+ xwrs of
    | list_vt_cons (xwr, !xwrs1) => let
        val r = xwr.2
      in
        if !r <> d then begin
          fold@ xwrs; Node (aux (xwrs, d+1), aux (xwrs, d+1))
        end else let
          val xwrs_v = !xwrs1
        in
          free@ {T} {0} (xwrs); xwrs := xwrs_v; Leaf (xwr.0)
        end // end of [if]
      end // end of [list_vt_cons]
    | list_vt_nil () => begin
        fold@ xwrs;
        prerr "Fatal Error: tree_build: aux: [xwrs] is empty!"; prerr_newline ();
        exit {tree a} (1)
      end
  end // end of [aux]
  var xwrs = xwrs; val t = aux (xwrs, 0); val () = case+ xwrs of
    | ~list_vt_nil () => () | list_vt_cons _ => let
        val () = begin
          prerr "Fatal Error: tree_build: aux: [xwrs] is not nil!"; prerr_newline ();
          exit {void} (1)
        end
      in
        fold@ xwrs; list_vt_free<T> (xwrs)
      end
in
  t // the return value
end

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

// for the purpose of debugging
fun{a:t@ype} print_list {n:nat}
  (xwrs: !list_vt (@(a, weight, refdep), n)): void = begin case+ xwrs of
  | list_vt_cons (xwr, !xwrs1) => begin
      print xwr.1; print "(w)"; print !(xwr.2); print "(r)"; print_newline ();
      print_list (!xwrs1); fold@ (xwrs)
    end
  | _ => ()
end // end of [print_list]

// for the purpose of debugging
fun{a:t@ype} print_tree (t: tree a) = let
  fun aux (t: tree a, d: depth): void = begin
    case+ t of 
    | Node (t1, t2) => (aux (t1, d+1); aux (t2, d+1))
    | Leaf _ => (printf ("Leaf(%i)", @(d)); print_newline ())
  end
in
  aux (t, 0)
end // end of [print_tree]

(* ****** ****** *)

// [GW] implements the Garsia-Wachs algorithm
extern fun {a:t@ype} GW {n:pos} (xws: list (@(a, weight), n)): tree a

implement{a} GW (xws) = let
  var len: int?; val xwrs = trans0 (xws, len)
  val ts = trans1 (xwrs); val t1 = trans2_all (ts, len)
  val () = mark_depth_and_free (t1)
(*
  val () = print_list (xwrs)
*)
  val t = tree_build (xwrs)
(*
  val () = print_tree (t)
*)
in
  t // return value
end // end of [GW]

(* ****** ****** *)

implement main (argc, argv) = let
  val xws = '[
    (' ', 186)
  , ('a',  64)
  , ('b',  13)
  , ('c',  22)
  , ('d',  32)
  , ('e', 103)
  , ('f',  21)
  , ('g',  15)
  , ('h',  47)
  , ('i',  57)
  , ('j',   1)
  , ('k',   5)
  , ('l',  32)
  , ('m',  20)
  , ('n',  57)
  , ('o',  63)
  , ('p',  15)
  , ('q',   1)
  , ('r',  48)
  , ('s',  51)
  , ('t',  80)
  , ('u',  23)
  , ('v',   8)
  , ('w',  18)
  , ('x',   1)
  , ('y',  16)
  , ('z',   1)
  ]
(*
  // for the purpose of measurement
  val xws = xws + xws // 2
  val xws = xws + xws // 4
  val xws = xws + xws // 8
  val xws = xws + xws // 16
  val xws = xws + xws // 32
  val xws = xws + xws // 64
  val xws = xws + xws // 128
  val xws = xws + xws // 256
  val xws = xws + xws // 512
  val xws = xws + xws // 1024
*)
  val () = let
    val nxws = list_length (xws)
  in
    print "nxws = "; print nxws; print_newline ()
  end // end of [val]
  val _(*tree*) = GW<char> (xws)
  val () = let
    fun loop (n: int):<cloref1> void = begin
      if n > 0 then let val _ = GW<char> (xws) in loop (n-1) end
    end
  in
    loop (100)
  end // end of [val]
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [GarsiaWachs.dats] *)

////

(*

Author: Jean-Christophe Fillatre

*)

(* Garsia-Wachs algorithm for optimum binary tree. 
   See TAOCP vol. 3 page 451. *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

(* phase 1 : build an optimum tree, with leaves in any order *)

let phase1 l =
  let rec extract before = function
    | [] | [_] ->
	assert false
    | [t1,w1; t2,w2] ->
	before, (Node (t1, t2), w1 + w2), []
    | (t1, w1) :: (t2, w2) :: ((_, w3) :: _ as after) when w1 <= w3 ->
	before, (Node (t1, t2), w1 + w2), after
    | e1 :: r ->
	extract (e1 :: before) r
  in
  let rec insert after ((_,wk) as tk) = function
    | [] -> 
	tk :: after (* insertion at the beginning *)
    | (_, wj) :: _ as before when wj >= wk ->
	List.rev_append before (tk :: after) (* inefficient *)
    | tj :: before ->
	insert (tj :: after) tk before
  in
  let rec loop = function
    | [] -> 
	assert false
    | [t,_] -> 
	t
    | l -> 
	let before, tk, after = extract [] l in
	loop (insert after tk before)
  in
  loop l

(* optimization (efficient use of the Zipper, to avoid [List.rev_append]) *)

let phase1opt l =
  let rec extract before = function
    | [] ->
	assert false
    | [t,_] ->
	t
    | [t1,w1; t2,w2] ->
	insert [] (Node (t1, t2), w1 + w2) before
    | (t1, w1) :: (t2, w2) :: ((_, w3) :: _ as after) when w1 <= w3 ->
	insert after (Node (t1, t2), w1 + w2) before
    | e1 :: r ->
	extract (e1 :: before) r
  and insert after ((_,wt) as t) = function
    | [] -> 
	extract [] (t :: after)
    | (_, wj_1) as tj_1 :: before when wj_1 >= wt ->
    	begin match before with
	  | [] -> extract [] (tj_1 :: t :: after)
	  | tj_2 :: before -> extract before (tj_2 :: tj_1 :: t :: after)
	end
    | tj :: before ->
	insert (tj :: after) t before
  in
  extract [] l

(* phase 2 : mark each leaf with its depth *)

let rec mark d = function
  | Leaf (_, dx) -> dx := d
  | Node (l, r) -> mark (d + 1) l; mark (d + 1) r

(* phase 3 : build a tree from the list of leaves/depths *)

let rec build d = function
  | [] | (Node _, _) :: _ ->
      assert false
  | (Leaf (x, dx), _) :: r when !dx = d -> 
      Leaf x, r
  | l -> 
      let left,l = build (d+1) l in
      let right,l = build (d+1) l in
      Node (left, right), l

let garsia_wachs l =
  let l = List.map (fun (x, wx) -> Leaf (x, ref 0), wx) l in
  let t = phase1opt l in
  mark 0 t;
  let t, l = build 0 l in
  assert (l = []);
  t

(* test *)

let alpha = 
  [' ', 186; 'a', 64; 'b', 13; 'c', 22; 'd', 32;
   'e', 103; 'f', 21; 'g', 15; 'h', 47; 'i', 57;
   'j', 1;   'k', 5;  'l', 32; 'm', 20; 'n', 57;
   'o', 63;  'p', 15; 'q', 1;  'r', 48; 's', 51;
   't', 80;  'u', 23; 'v', 8;  'w', 18; 'x', 1;
   'y', 16;  'z', 1
  ]

let xws = alpha
(*
// for the purpose of measurements
*)
let xws = xws @ xws (* 2 *)
let xws = xws @ xws (* 4 *)
let xws = xws @ xws (* 8 *)
let xws = xws @ xws (* 16 *)
let xws = xws @ xws (* 32 *)
let xws = xws @ xws (* 64 *)
let xws = xws @ xws (* 128 *)
let xws = xws @ xws (* 256 *)
let xws = xws @ xws (* 512 *)
let xws = xws @ xws (* 1024 *)
let nxws = List.length (xws)
let () = (print_string "nxws = "; print_int nxws; print_newline ())

let rec loop (n) = begin
  if n > 0 then let t = garsia_wachs xws in loop (n-1)
end
  
let () = loop (100)

(* end of [GarsiaWachs.ml] *)
