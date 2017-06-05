(*
**
** A simple GTK/CAIRO example: demonstrating binary search
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: July, 2010
**
*)

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"
staload "contrib/cairo/SATS/cairo_extra.sats"

(* ****** ****** *)

fun draw_intarray
  {l:agz} {n:pos} (
    cr: !cairo_ref l, W: double, H: double, A: array (int, n), n: int n
  ) : void = () where {
  val () = cairo_rectangle (cr, ~W/2, ~H/2, W, H)
  val () = cairo_set_source_rgb (cr, 1.0, 1.0, 1.0)
  val () = cairo_fill (cr)
//
  val w = W / n
  fun loop {i:nat | i <= n}
    (cr: !cairo_ref l, x: double, i: int i):<cloref1> void =
    if i < n then let
      val () = cairo_move_to (cr, x, ~H/2)
      val () = cairo_rel_line_to (cr, 0.0, H)
      val () = cairo_set_source_rgb (cr, 0.0, 0.0, 0.0)
      val () = cairo_stroke (cr)
      val (pf | ()) = cairo_save (cr)
      val () = cairo_translate (cr, x + w/2, 0.0)
      val () = cairo_set_source_rgb (cr, 0.25, 0.25, 0.25)
//
      val txt = sprintf ("%2.2i", @(A[i]))
      val () = cairo_show_text_inbox (cr, w, H, $UN.castvwtp1{string} (txt))
      val () = strptr_free (txt)
//
      val () = cairo_restore (pf | cr)
    in
      loop (cr, x+w, i+1)
    end else let
      val () = cairo_move_to (cr, x, ~H/2)
      val () = cairo_rel_line_to (cr, 0.0, H)
      val () = cairo_stroke (cr)    
    in
      // nothing
    end (* end of [if] *)
  // end of [loop]
  val () = loop (cr, ~W/2, 0)
  val () = cairo_set_source_rgb (cr, 0.0, 0.0, 0.0)
  val () = cairo_stroke (cr)
//
} // end of [draw_intarray]

(* ****** ****** *)

fun{a:t@ype}
array_copy {n:nat} (
    A: array (a, n), n: size_t n
  ) : array (a, n) = B where {
  val (vbox pf1_arr | p1) = array_get_view_ptr (A)
  val (pf2_gc, pf2_arr | p2) = array_ptr_alloc_tsz {a} (n, sizeof<a>)
  prval () = free_gc_elim {a?} (pf2_gc)
  val () = array_ptr_copy_tsz (!p1, !p2, n, sizeof<a>)
  val B = array_make_view_ptr {a} (pf2_arr | p2)
} // end of [array_copy]

(* ****** ****** *)

datatype trace =
  TRACE of (int(*lb*), int(*ub*))
typedef tracelst = List (trace)

fun bsearch {n:nat} (
    A: array (int, n), n: size_t n, x0: int
  ) : tracelst = let
  fun aux
    {i:nat;j:int | i <= j+1; j+1 <= n} .<j+1-i>.
    (lb: int i, ub: int j):<cloref1> tracelst = let
    val tr = TRACE (lb, ub)
    val trs = (if lb <= ub then let
      val m = (lb + ub) / 2
      val x = A[m]
    in
      if x0 <= x then aux (lb, m-1) else aux (m+1, ub)
    end else begin
      list_nil ()
    end) : tracelst (* end of [if] *)
  in
    list_cons (tr, trs)
  end // end of [aux]
  val n = int1_of_size1 (n)
in
  aux (0, n-1)
end // end of [bsearch]

(* ****** ****** *)

#define N 100

(* ****** ****** *)

staload "libc/SATS/random.sats"
staload "libc/SATS/stdlib.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"
staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

val () = srand48_with_time ()

#define ASZ 20
val key0 = randint (N)
val A0: array (int, ASZ) = let
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc<int> (ASZ)
  val () = array_ptr_initialize_fun<int> (
    !p_arr, ASZ, lam (_, x) =<fun> x := $effmask_ref (randint (N))
  ) // end of [val]
  val () = qsort {int} (!p_arr, ASZ, sizeof<int>, lam (x1, x2) => compare (x1, x2))
  prval () = free_gc_elim {int?} (pf_gc)
in
  array_make_view_ptr (pf_arr | p_arr)
end // end of [val]

val theTracelst = bsearch (A0, ASZ, key0)
(*
val () = (
  print "length of [theTracelst] = "; print (list_length theTracelst); print_newline ()
) // end of [val]
*)
val theTracelstRef = ref<tracelst> (list_nil)

(* ****** ****** *)

fn update_theTracelstRef (): bool = let
  val trs = !theTracelstRef
in
  case+ trs of
  | list_cons (tr, trs) => (!theTracelstRef := trs; true)
  | _ => false
end // end of [update_theTracelstRef]

(* ****** ****** *)

extern fun draw_trace {l:agz}
  (cr: !cairo_ref l, w: double, h: double, n: int, tr: trace): void
// end of [draw_trace]

implement
draw_trace
  (cr, w, h, n, tr) = let
  val TRACE (lb, ub) = tr
  val nw = w / n and nh = h / n
//
  val x = lb * nw + nw / 2 and y = nh / 2
  val () = cairo_move_to (cr, ~w/2+x, y)
  val () = cairo_rel_line_to (cr, ~nw/6, nh/3)
  val () = cairo_rel_line_to (cr,  nw/3,  0.0)
  val () = cairo_close_path (cr)
  val () = cairo_fill (cr)
//
  val x = ub * nw + nw / 2 and y = ~nh / 2
  val () = cairo_move_to (cr, ~w/2+x, y)
  val () = cairo_rel_line_to (cr, ~nw/6, ~nh/3)
  val () = cairo_rel_line_to (cr,  nw/3,  0.0)
  val () = cairo_close_path (cr)
  val () = cairo_fill (cr)
//
  val () = if lb <= ub then let
    val m = (lb+ub) / 2
    val x = ~w/2+m*nw and y = ~nh/2
    val () = cairo_rectangle (cr, x, y, nw, nh)
    val () = cairo_set_source_rgb (cr, 1.0, 1.0, 0.0) // yellow
    val () = cairo_fill (cr)
//
    val m = int1_of_int (m)
    val () = assert (m >= 0 && m < ASZ)
    val m = size1_of_int1 (m)
    val (pf | ()) = cairo_save (cr)
    val () = cairo_translate (cr, x + nw/2, 0.0)
    val () = cairo_set_source_rgb (cr, 0.25, 0.25, 0.25)
    val txt = sprintf ("%2.2i", @(A0[m]))
    val () = cairo_show_text_inbox (cr, nw, nh, $UN.castvwtp1{string} (txt))
    val () = strptr_free (txt)
    val () = cairo_restore (pf | cr)
  in
    // nothing
  end // end of [val]
//
in
  // nothing
end // end of [draw_trace]

extern fun draw_main {l:agz}
  (cr: !cairo_ref l, width: int, height: int): void
implement draw_main
  (cr, width, height) = () where {
  val n = ASZ
  val w0 = double_of (width)
  val w = n*w0/(n+2)
  val h = double_of (height)
  val (pf | ()) = cairo_save (cr)
  val () = cairo_translate (cr, w0/2, h/3)
//
  val txt = sprintf ("searching for key = %i", @(key0))
  val () = cairo_show_text_inbox (cr, w0/2, 2*h/3, $UN.castvwtp1{string} (txt))
  val () = strptr_free (txt)
//
  val () = cairo_restore (pf | cr)
  val (pf | ()) = cairo_save (cr)
  val () = cairo_translate (cr, w0/2, h/2)
  val () = draw_intarray (cr, w, h/n, A0, n)
  val trs = !theTracelstRef
  val () = (
    case+ trs of
    | list_cons (tr, _) => let
        val () = cairo_set_source_rgb (cr, 0.0, 0.0, 0.0)
      in
        draw_trace (cr, w, h, n, tr)
      end // end of [list_cons]
    | list_nil () => ()
  ) : void // end of [val]
  val () = cairo_restore (pf | cr)
} // end of [draw_main]

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"
staload "contrib/glib/SATS/glib-object.sats"

(* ****** ****** *)

staload "contrib/GTK/SATS/gdk.sats"
staload "contrib/GTK/SATS/gtk.sats"
staload "contrib/GTK/SATS/gtkclassdec.sats"

(* ****** ****** *)

%{^
GtkWidget *the_drawingarea = NULL;
ats_ptr_type
the_drawingarea_get () {
  if (the_drawingarea == NULL) {
    fprintf (stderr, "exit(the_drawingarea_get): not initialized yet\n"); exit(1);
  } // end of [if]
  return the_drawingarea ;
} // end of [the_drawingarea_get]
ats_void_type
the_drawingarea_initset (ats_ptr_type x) {
  static int the_drawingarea_initset_flag = 0 ;
  if (the_drawingarea_initset_flag) {
    fprintf (stderr, "exit(the_drawingarea_initset): already initialized\n"); exit(1);
  } // end of [if]
  the_drawingarea_initset_flag = 1 ; the_drawingarea = x ; return ;
} // end of [the_drawingarea_initset]
%} // end of [%{^] 
extern
fun the_drawingarea_get (): [l:agz]
  (gobjref (GtkDrawingArea, l) -<lin,prf> void | gobjref (GtkDrawingArea, l))
  = "the_drawingarea_get"
// end of [the_drawingarea_get]
extern
fun the_drawingarea_initset
  (x: GtkDrawingArea_ref1): void = "the_drawingarea_initset"
// end of [the_drawingarea_initset]

(* ****** ****** *)

%{^
extern
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv) ;
%} // end of [%{^]

(* ****** ****** *)

fn fnext (): void = let
  val ans = update_theTracelstRef ()
  val () = if ~ans then !theTracelstRef := theTracelst
  val (fpf_darea | darea) = the_drawingarea_get ()
  val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
  val () = gtk_widget_queue_draw_area (darea, (gint)0, (gint)0, p->width, p->height)
  prval () = minus_addback (fpf, pf | darea)
  prval () = fpf_darea (darea)
in
  // nothing
end // end of [fnext]

(* ****** ****** *)

fn fexpose
  {c:cls | c <= GtkDrawingArea} {l:agz}
  (darea: !gobjref (c, l), event: &GdkEvent): gboolean = let
  prval () = clstrans {c,GtkDrawingArea,GtkWidget} ()
  val (fpf_win | win) = gtk_widget_get_window (darea)
  val () = assert_errmsg (g_object_isnot_null (win), #LOCATION)
  val cr = gdk_cairo_create (win)
  prval () = minus_addback (fpf_win, win | darea)
  val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
  val () = draw_main (cr, (int_of)p->width, (int_of)p->height)
  prval () = minus_addback (fpf, pf | darea)
  val () = cairo_destroy (cr)
in
  GFALSE // HX: what does this mean?
end // end of [fexpose]

(* ****** ****** *)

fn ftimeout
  (_: gpointer): gboolean = let
  val (fpf_darea | darea) = the_drawingarea_get ()
  val (fpf_win | win) = gtk_widget_get_window (darea)
  val res = update_theTracelstRef ()
  val () = if res then (
    if g_object_isnot_null (win) then let
      val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
      val () = gtk_widget_queue_draw_area (darea, (gint)0, (gint)0, p->width, p->height)
      prval () = minus_addback (fpf, pf | darea)
    in
      // nothing
    end // end of [if]
  ) // end of [val]
  prval () = minus_addback (fpf_win, win | darea)
  prval () = fpf_darea (darea)
in
  GTRUE
end // end of [ftimeout]

(* ****** ****** *)

macdef gs = gstring_of_string

(* ****** ****** *)

val theTimeInterval = 1000

extern fun main1 (): void = "main1"

implement main1 () = () where {
//
  val () = srand48_with_time ()
//
  val window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  val () =
    gtk_window_set_default_size (window, (gint)400, (gint)400)
  // end of [val]
  val (fpf_x | x) = (gs)"cairo: binary search"
  val () = gtk_window_set_title (window, x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (window, (gsignal)"delete-event", G_CALLBACK (gtk_widget_destroy), (gpointer)null)
  val _sid = g_signal_connect
    (window, (gsignal)"destroy", G_CALLBACK (gtk_main_quit), (gpointer)null)
//
  val vbox0 = gtk_vbox_new (GFALSE(*homo*), (gint)10(*spacing*))
  val () = gtk_container_add (window, vbox0)
//
  val hbox1 = gtk_hbox_new (GFALSE, (gint)0)
  val () = gtk_box_pack_start (vbox0, hbox1, GFALSE, GFALSE, (guint)0)
  val () = g_object_unref (hbox1)
//
  val darea = gtk_drawing_area_new ()
  val () = gtk_box_pack_start (vbox0, darea, GTRUE, GTRUE, (guint)0)
  val _sid = g_signal_connect
    (darea, (gsignal)"expose-event", G_CALLBACK (fexpose), (gpointer)null)
  val () = the_drawingarea_initset (darea)
//
  val hsep = gtk_hseparator_new ()
  val () = gtk_box_pack_start (vbox0, hsep, GFALSE, GFALSE, (guint)0)
  val () = g_object_unref (hsep)
//
  val hbox1 = gtk_hbox_new (GFALSE(*homo*), (gint)5(*spacing*))
  val () = gtk_box_pack_start (vbox0, hbox1, GFALSE, GTRUE, (guint)10)
//
  val (fpf_x | x) = (gs)"_Close"
  val btn_close = gtk_button_new_with_mnemonic (x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (btn_close, (gsignal)"clicked", G_CALLBACK(gtk_main_quit), (gpointer_vt)window)
  // end of [val]
  val () = gtk_box_pack_end (hbox1, btn_close, GFALSE, GFALSE, (guint)4)
  val () = g_object_unref (btn_close)
//
  val (fpf_x | x) = (gs)"_Next"
  val btn_next = gtk_button_new_with_mnemonic (x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (btn_next, (gsignal)"clicked", G_CALLBACK(fnext), (gpointer)null)
  // end of [val]
  val () = gtk_box_pack_end (hbox1, btn_next, GFALSE, GFALSE, (guint)4)
  val () = g_object_unref (btn_next)
(*
  val _rid = gtk_timeout_add ((guint32)theTimeInterval, ftimeout, (gpointer)null)
*)
  val () = g_object_unref (hbox1)
  val () = g_object_unref (vbox0)
  val () = gtk_widget_show_all (window)
  val () = g_object_unref (window)
  val () = gtk_main ()
} // end of [main1]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

%{$
ats_void_type
mainats (
  ats_int_type argc, ats_ptr_type argv
) {
  gtk_init ((int*)&argc, (char***)&argv) ; main1 () ; return ;
} // end of [mainats]
%} // end of [%{$]

(* ****** ****** *)

(* end of [bsearch_demo.dats] *)
