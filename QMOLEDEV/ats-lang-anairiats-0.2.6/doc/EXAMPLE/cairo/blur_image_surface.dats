(*
**
** Performing a simple 2D Gaussian blur
** The code is directly translated from on the attached C code
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: May, 2010
**
*)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynloading

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload MATH = "libc/SATS/math.sats"

(* ****** ****** *)

%{^
//
ATSinline()
ats_ptr_type
data_get_row (
  ats_ptr_type data
, ats_int_type stride
, ats_int_type i
) {
  return &((ats_uint8_type*)data)[i*stride] ;
} // end of [data_get_row]
//
ATSinline()
ats_uint32_type
data_get_elt (
  ats_ptr_type data
, ats_int_type stride
, ats_int_type i
, ats_int_type j
) {
  ats_ptr_type p = &((ats_uint8_type*)data)[i*stride] ;
  return ((ats_uint32_type*)p)[j] ;
} // end of [data_get_elt]
//
%} // end of [%{^]

(* ****** ****** *)

typedef kernarr (n:int) = @[uint8][n]

fun kernel_initialize {n:nat} (
    kernel: &kernarr n? >> kernarr n, n: int n
  ) : void = () where {
  typedef a = uint8
  prval pf = unit_v ()
  val n2 = n / 2
  var !p_clo = @lam
    (pf: !unit_v | i: sizeLt n, x: &a? >> a): void =<clo> let
    val i = int1_of_size1 (i)
    val f = double_of (i - n2)
    val d = 80 * $MATH.exp(~f*f/30)
  in
    x := uint8_of_uint(uint_of(int_of(d)))
  end // end of [var]
  val n = size1_of_int1 (n)
  val () = array_ptr_initialize_vclo<a> (pf | kernel, n, !p_clo)
  prval unit_v () = pf
} // end of [kernel_intialize]

fun kernel_sum {n:nat}(
    kernel: &kernarr n, n: int n
  ) : uint = res where {
  var i: natLte n
  var res: uint = 0U
  val () = for
    (i := 0; i < n; i := i + 1) res := res + uint_of_uint8 (kernel.[i])
  // end of [val]
} // end of [kernel_sum]

(* ****** ****** *)

fun blur_hori_row
  {n:nat} {w,h:int}
  {i,j:nat | i < h; j <= w} .<w-j>. (
    kernel: &(@[uint8][n])
  , a: uint // sum of [kernel]
  , s: &(@[uint32][w]), d: &(@[uint32][w])
  , r1: int, r2: int, n: int n
  , w: int w, h: int h
  , i: int i, j: int j
  ) : void =
if j < w then let
  val () = case+ 0 of
  | _ when (r1 < j andalso j < r2) => d.[j] := s.[j]
  | _ => () where {
      var x0: uint = 0U and x1: uint = 0U and x2: uint = 0U and x3: uint = 0U
      var k: natLte n
      val () = for
        (k := 0; k < n; k := k+1) let
        val j1 = j - n/2 + k
      in
        if :(k: natLt n) =>
          j1 < 0 then () else
          if :(k: natLt n) =>
            j1 >= w then () else let
            val p = s.[j1]
            val p = uint_of_uint32 (p)
            val kernel_k = uint_of_uint8 (kernel.[k])
            val () = x0 := x0 + ((p >> 24) land 0XffU) * kernel_k
            val () = x1 := x1 + ((p >> 16) land 0XffU) * kernel_k
            val () = x2 := x2 + ((p >>  8) land 0XffU) * kernel_k
            val () = x3 := x3 + ((p >>  0) land 0XffU) * kernel_k
          in
            // nothing
          end // end of [if]
        // end of [if]
      end // end of [val]
      val x = (x0 / a << 24) lor (x1 / a << 16) lor (x2 / a << 8) lor x3 / a
      val () = d.[j] := (uint32_of_uint)x
    } // end of [_]
in
  blur_hori_row (kernel, a, s, d, r1, r2, n, w, h, i, j + 1)
end // end of [if]
// end of [blur_hori_row]

(* ****** ****** *)

fun blur_vert_row
  {n:nat} {w,h:int}
  {i,j:nat | i < h; j <= w}
  {l:addr} .<w-j>. (
    kernel: &(@[uint8][n])
  , a: uint // sum of [kernel]
  , data: ptr l, stride: int
  , s: &(@[uint32][w]), d: &(@[uint32][w])
  , r1: int, r2: int, n: int n
  , w: int w, h: int h
  , i: int i, j: int j
  ) : void = let
extern fun data_get_elt
  (data: ptr, stride: int, i: natLt h, j: natLt w): uint32 = "data_get_elt"
in // in of [let]
if j < w then let
  val () = case+ 0 of
  | _ when (r1 <= i andalso i < r2) => d.[j] := s.[j]
  | _ => () where {
      var x0: uint = 0U and x1: uint = 0U and x2: uint = 0U and x3: uint = 0U
      var k: natLte n
      val () = for
        (k := 0; k < n; k := k+1) let
        val i1 = i - n/2 + k
      in
        if :(k: natLt n) =>
          i1 < 0 then () else
          if :(k: natLt n) =>
            i1 >= h then () else let
            val p = data_get_elt (data, stride, i1, j)
            val p = uint_of_uint32 (p)
            val kernel_k = uint_of_uint8 (kernel.[k])
            val () = x0 := x0 + ((p >> 24) land 0XffU) * kernel_k
            val () = x1 := x1 + ((p >> 16) land 0XffU) * kernel_k
            val () = x2 := x2 + ((p >>  8) land 0XffU) * kernel_k
            val () = x3 := x3 + ((p >>  0) land 0XffU) * kernel_k
          in
            // nothing
          end // end of [if]
        // end of [if]
      end // end of [val]
      val x = (x0 / a << 24) lor (x1 / a << 16) lor (x2 / a << 8) lor x3 / a
      val () = d.[j] := (uint32_of_uint)x
    } // end of [_]
in
  blur_vert_row (kernel, a, data, stride, s, d, r1, r2, n, w, h, i, j + 1)
end // end of [if]
end // end of [blur_vert_row]

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

extern
fun blur_image_surface
  {l:agz} {r:int} (sf: !cairo_surface_ref l, radius: int r): void
  = "blur_image_surface"
// end of [blur_image_surface]

(* ****** ****** *)

fun blur_image_surface_main
  {l1,l2:agz} {r:int} {w,h:nat} (
    sf1: !cairo_surface_ref l1
  , sf2: !cairo_surface_ref l2
  , r: int r, w: int w, h: int h
  ) : void = () where {
//
  extern
  fun data_get_row {i:nat | i < h}
    (data: ptr, stride: int, i: int i)
    : [l:addr] (@[uint32][w] @ l, @[uint32][w] @ l -<lin,prf> void | ptr l)
    = "data_get_row"
  // end of [extern]
//
  val n = 17
  var !p_kernel = @[uint8][n]()
  val () = kernel_initialize (!p_kernel, n)
  val a = kernel_sum (!p_kernel, n)
//
  val data1 = cairo_image_surface_get_data (sf1)
  val stride1 = cairo_image_surface_get_stride (sf1)
  val data2 = cairo_image_surface_get_data (sf2)
  val stride2 = cairo_image_surface_get_stride (sf2)
  var i: natLte h
//
(*
  val () = (print "w = "; print w; print_newline ())
  val () = (print "h = "; print h; print_newline ())
  val () = (print "stride1 = "; print stride1; print_newline ())
  val () = (print "stride2 = "; print stride2; print_newline ())
*)
//
  val () = for (i := 0; i < h; i := i + 1) let
    // val () = (print "i = "; print i; print_newline ())
    val (pf_s, fpf_s | p_s) = data_get_row (data1, stride1, i)
    val (pf_d, fpf_d | p_d) = data_get_row (data2, stride2, i)
    val () = blur_hori_row (
      !p_kernel, a, !p_s, !p_d, r, w-r, n, w, h, i, 0
    ) // end of [val]
    prval () = fpf_s (pf_s) and () = fpf_d (pf_d)
  in
    // nothing
  end // end of [val]
//
  val () = for (i := 0; i < h; i := i + 1) let
    val (pf_s, fpf_s | p_s) = data_get_row (data2, stride2, i)
    val (pf_d, fpf_d | p_d) = data_get_row (data1, stride1, i)
    val () = blur_vert_row (
      !p_kernel, a, data2, stride2, !p_s, !p_d, r, h-r, n, w, h, i, 0
    ) // end of [val]
    prval () = fpf_s (pf_s) and () = fpf_d (pf_d)
  in
    // nothing
  end // end of [val]
//
} // end of [blur_image_surface_main]

(* ****** ****** *)

implement
blur_image_surface (sf, r) = let
  val status = cairo_surface_status (sf)
in
  case+ 0 of
  | _ when status = CAIRO_STATUS_SUCCESS =>
//
let
  val fmt = cairo_image_surface_get_format (sf)
  val w = let
    val w = cairo_image_surface_get_width (sf)
    val w = int1_of_int (w)
  in
    if fmt = CAIRO_FORMAT_A1 then 0
    else if fmt = CAIRO_FORMAT_A8 then w / 4
    else w
  end : Int // end of [val]
  val h = cairo_image_surface_get_height (sf)
  val h = int1_of_int (h)
in
if (w > 0 && h > 0) then let
  val sf_tmp =
    cairo_image_surface_create (CAIRO_FORMAT_ARGB32, w, h)
  val status = cairo_surface_status (sf)
in
  if (status = CAIRO_STATUS_SUCCESS) then let
    val () = blur_image_surface_main (sf, sf_tmp, r, w, h)
    val () = cairo_surface_destroy (sf_tmp)
    val () = cairo_surface_mark_dirty (sf)
  in
    // nothing
  end else let
    val () = cairo_surface_destroy (sf_tmp)
  in
    // nothing
  end // end of [if]
end (* end of [if] *)
end // end of [CAIRO_STATUS_SUCCESS]
  | _ (* erroneous surface *) => ()
end // end of [blur_image_surface]

(* ****** ****** *)

(* end of [blur_image_surface.dats] *)

////

/*
 * Copyright © 2008 Kristian Høgsberg
 * Copyright © 2009 Chris Wilson
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation, and
 * that the name of the copyright holders not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  The copyright holders make no representations
 * about the suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

#include <math.h>
#include <stdint.h>

#define ARRAY_LENGTH(a) (sizeof (a) / sizeof (a)[0])

/* Performs a simple 2D Gaussian blur of radius @radius on surface @surface. */
void
blur_image_surface (cairo_surface_t *surface, int radius)
{
    cairo_surface_t *tmp;
    int width, height;
    int src_stride, dst_stride;
    int x, y, z, w;
    uint8_t *src, *dst;
    uint32_t *s, *d, a, p;
    int i, j, k;
    uint8_t kernel[17];
    const int size = ARRAY_LENGTH (kernel);
    const int half = size / 2;

    if (cairo_surface_status (surface))
	return;

    width = cairo_image_surface_get_width (surface);
    height = cairo_image_surface_get_height (surface);

    switch (cairo_image_surface_get_format (surface)) {
    case CAIRO_FORMAT_A1:
    default:
	/* Don't even think about it! */
	return;

    case CAIRO_FORMAT_A8:
	/* Handle a8 surfaces by effectively unrolling the loops by a
	 * factor of 4 - this is safe since we know that stride has to be a
	 * multiple of uint32_t. */
	width /= 4;
	break;

    case CAIRO_FORMAT_RGB24:
    case CAIRO_FORMAT_ARGB32:
	break;
    }

    tmp = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    if (cairo_surface_status (tmp))
	return;

    src = cairo_image_surface_get_data (surface);
    src_stride = cairo_image_surface_get_stride (surface);

    dst = cairo_image_surface_get_data (tmp);
    dst_stride = cairo_image_surface_get_stride (tmp);

    a = 0;
    for (i = 0; i < size; i++) {
	double f = i - half;
	a += kernel[i] = exp (- f * f / 30.0) * 80;
    }

    /* Horizontally blur from surface -> tmp */
    for (i = 0; i < height; i++) {
	s = (uint32_t *) (src + i * src_stride);
	d = (uint32_t *) (dst + i * dst_stride);
	for (j = 0; j < width; j++) {
	    if (radius < j && j < width - radius) {
		d[j] = s[j];
		continue;
	    }

	    x = y = z = w = 0;
	    for (k = 0; k < size; k++) {
		if (j - half + k < 0 || j - half + k >= width)
		    continue;

		p = s[j - half + k];

		x += ((p >> 24) & 0xff) * kernel[k];
		y += ((p >> 16) & 0xff) * kernel[k];
		z += ((p >>  8) & 0xff) * kernel[k];
		w += ((p >>  0) & 0xff) * kernel[k];
	    }
	    d[j] = (x / a << 24) | (y / a << 16) | (z / a << 8) | w / a;
	}
    }

    /* Then vertically blur from tmp -> surface */
    for (i = 0; i < height; i++) {
	s = (uint32_t *) (dst + i * dst_stride);
	d = (uint32_t *) (src + i * src_stride);
	for (j = 0; j < width; j++) {
	    if (radius <= i && i < height - radius) {
		d[j] = s[j];
		continue;
	    }

	    x = y = z = w = 0;
	    for (k = 0; k < size; k++) {
		if (i - half + k < 0 || i - half + k >= height)
		    continue;

		s = (uint32_t *) (dst + (i - half + k) * dst_stride);
		p = s[j];

		x += ((p >> 24) & 0xff) * kernel[k];
		y += ((p >> 16) & 0xff) * kernel[k];
		z += ((p >>  8) & 0xff) * kernel[k];
		w += ((p >>  0) & 0xff) * kernel[k];
	    }
	    d[j] = (x / a << 24) | (y / a << 16) | (z / a << 8) | w / a;
	}
    }

    cairo_surface_destroy (tmp);
    cairo_surface_mark_dirty (surface);
}
