/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
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
*/

/* ****** ****** */

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: December, 2009

/* ****** ****** */

#ifndef ATSCTRB_CAIRO_CAIRO_CATS
#define ATSCTRB_CAIRO_CAIRO_CATS

/* ****** ****** */

#include <cairo-features.h>
#include <cairo.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>
#include <cairo-svg.h>
#include <cairo-xlib.h>

/* ****** ****** */

typedef ats_ref_type ats_cairo_ref ;
typedef ats_ref_type ats_cairo_surface_ref ;
typedef ats_ref_type ats_cairo_pattern_ref ;
typedef ats_ref_type ats_cairo_font_face_ref ;
typedef ats_ref_type ats_cairo_scaled_font_ref ;

/* ****** ****** */

//
// contexts for drawing
//

/* ****** ****** */

#define atsctrb_cairo_create cairo_create

/* ****** ****** */

#define atsctrb_cairo_status cairo_status
#define atsctrb_cairo_reference cairo_reference
#define atsctrb_cairo_destroy cairo_destroy
#define atsctrb_cairo_get_reference_count cairo_get_reference_count

/* ****** ****** */

#define atsctrb_cairo_save cairo_save
#define atsctrb_cairo_restore cairo_restore

/* ****** ****** */

#define atsctrb_cairo_get_target cairo_get_target
#define atsctrb_cairo_get_group_target cairo_get_group_target

/* ****** ****** */

#define atsctrb_cairo_push_group cairo_push_group
#define atsctrb_cairo_push_group_with_content cairo_push_group_with_content
#define atsctrb_cairo_pop_group cairo_pop_group
#define atsctrb_cairo_pop_group_to_source cairo_pop_group_to_source

/* ****** ****** */

#define atsctrb_cairo_set_source_rgb cairo_set_source_rgb
#define atsctrb_cairo_set_source_rgba cairo_set_source_rgba

/* ****** ****** */

#define atsctrb_cairo_get_source cairo_get_source
#define atsctrb_cairo_set_source cairo_set_source
#define atsctrb_cairo_set_source_surface cairo_set_source_surface

/* ****** ****** */

#define atsctrb_cairo_get_antialias cairo_get_antialias
#define atsctrb_cairo_set_antialias cairo_set_antialias

/* ****** ****** */

#if (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,4,0))

#define atsctrb_cairo_get_dash_count cairo_get_dash_count

ATSinline()
ats_int_type
atsctrb_cairo_get_dash (
  ats_cairo_ref cr
, ats_ptr_type dashes, ats_int_type n
, ats_ptr_type offset
) {
  int n1 = cairo_get_dash_count((cairo_t*)cr) ;
  if (n1 <= n) {
    cairo_get_dash((cairo_t*)cr, (double*)dashes, (double*)offset) ;
  } else {
    cairo_get_dash((cairo_t*)cr, (double*)0, (double*)offset) ;
  } // end of [if]
  return n1 ;
} // end of [atsctrb_cairo_get_dash]

#define atsctrb_cairo_set_dash cairo_set_dash

#endif // end of [#if (CAIRO_VERSION >= 1.4.0)]

/* ****** ****** */

#define atsctrb_cairo_get_fill_rule cairo_get_fill_rule
#define atsctrb_cairo_set_fill_rule cairo_set_fill_rule

/* ****** ****** */

#define atsctrb_cairo_get_line_cap cairo_get_line_cap
#define atsctrb_cairo_set_line_cap cairo_set_line_cap

/* ****** ****** */

#define atsctrb_cairo_get_line_join cairo_get_line_join
#define atsctrb_cairo_set_line_join cairo_set_line_join

/* ****** ****** */

#define atsctrb_cairo_get_line_width cairo_get_line_width
#define atsctrb_cairo_set_line_width cairo_set_line_width

/* ****** ****** */

#define atsctrb_cairo_get_miter_limit cairo_get_miter_limit
#define atsctrb_cairo_set_miter_limit cairo_set_miter_limit

/* ****** ****** */

#define atsctrb_cairo_get_operator cairo_get_operator
#define atsctrb_cairo_set_operator cairo_set_operator

/* ****** ****** */

#define atsctrb_cairo_get_tolerance cairo_get_tolerance
#define atsctrb_cairo_set_tolerance cairo_set_tolerance

/* ****** ****** */

#if (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,4,0))
#define atsctrb_cairo_rectangle_list_destroy cairo_rectangle_list_destroy
#define atsctrb_cairo_copy_clip_rectangle_list cairo_copy_clip_rectangle_list
#endif // end of [#if (CAIRO_VERSION >= 1.4.0)]

/* ****** ****** */

#define atsctrb_cairo_clip cairo_clip
#define atsctrb_cairo_clip_preserve cairo_clip_preserve
#define atsctrb_cairo_clip_extents cairo_clip_extents
#define atsctrb_cairo_reset_clip cairo_reset_clip

/* ****** ****** */

#define atsctrb_cairo_fill cairo_fill
#define atsctrb_cairo_fill_preserve cairo_fill_preserve
#define atsctrb_cairo_fill_extents cairo_fill_extents
#define atsctrb_cairo_in_fill cairo_in_fill

/* ****** ****** */

#define atsctrb_cairo_mask cairo_mask
#define atsctrb_cairo_mask_surface cairo_mask_surface

/* ****** ****** */

#define atsctrb_cairo_paint cairo_paint
#define atsctrb_cairo_paint_with_alpha cairo_paint_with_alpha

/* ****** ****** */

#define atsctrb_cairo_stroke cairo_stroke
#define atsctrb_cairo_stroke_preserve cairo_stroke_preserve
#define atsctrb_cairo_stroke_extents cairo_stroke_extents
#define atsctrb_cairo_in_stroke atsctrb_cairo_in_stroke

/* ****** ****** */

#define atsctrb_cairo_copy_page cairo_copy_page
#define atsctrb_cairo_show_page cairo_show_page

/* ****** ****** */

#if (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,4,0))
#define atsctrb_cairo_get_user_data cairo_get_user_data
#define atsctrb_cairo_set_user_data cairo_set_user_data
#endif // end of [#if (CAIRO_VERSION >= 1.4.0)]

/* ****** ****** */

//
// drawing paths
//

/* ****** ****** */

#define atsctrb_cairo_copy_path cairo_copy_path
#define atsctrb_cairo_copy_path_flat cairo_copy_path_flat
#define atsctrb_cairo_append_path cairo_append_path
#define atsctrb_cairo_path_destroy cairo_path_destroy

/* ****** ****** */

#define atsctrb_cairo_has_current_point cairo_has_current_point
#define atsctrb_cairo_get_current_point cairo_get_current_point

/* ****** ****** */

#define atsctrb_cairo_new_path cairo_new_path
#define atsctrb_cairo_new_sub_path cairo_new_sub_path
#define atsctrb_cairo_close_path cairo_close_path

/* ****** ****** */

#define atsctrb_cairo_arc cairo_arc
#define atsctrb_cairo_arc_negative cairo_arc_negative
#define atsctrb_cairo_curve_to cairo_curve_to
#define atsctrb_cairo_line_to cairo_line_to
#define atsctrb_cairo_move_to cairo_move_to
#define atsctrb_cairo_rectangle cairo_rectangle

/* ****** ****** */

#define atsctrb_cairo_rel_curve_to cairo_rel_curve_to
#define atsctrb_cairo_rel_line_to cairo_rel_line_to
#define atsctrb_cairo_rel_move_to cairo_rel_move_to

/* ****** ****** */

#define atsctrb_cairo_path_extents cairo_path_extents

/* ****** ****** */

//
// patterns for drawing
//

#define atsctrb_cairo_pattern_create_rgb cairo_pattern_create_rgb
#define atsctrb_cairo_pattern_create_rgba cairo_pattern_create_rgba
#define atsctrb_cairo_pattern_get_rgba cairo_pattern_get_rgba

#define atsctrb_cairo_pattern_create_for_surface cairo_pattern_create_for_surface
#define atsctrb_cairo_pattern_get_surface cairo_pattern_get_surface

#define atsctrb_cairo_pattern_create_linear cairo_pattern_create_linear
#define atsctrb_cairo_pattern_get_linear_points cairo_pattern_get_linear_points

#define atsctrb_cairo_pattern_create_radial cairo_pattern_create_radial
#define atsctrb_cairo_pattern_get_radial_circles cairo_pattern_get_radial_circles

#define atsctrb_cairo_pattern_status cairo_pattern_status
#define atsctrb_cairo_pattern_reference cairo_pattern_reference
#define atsctrb_cairo_pattern_destroy cairo_pattern_destroy
#define atsctrb_cairo_pattern_get_reference_count cairo_pattern_get_reference_count

#define atsctrb_cairo_pattern_get_type cairo_pattern_get_type
#define atsctrb_cairo_pattern_add_color_stop_rgb cairo_pattern_add_color_stop_rgb
#define atsctrb_cairo_pattern_add_color_stop_rgba cairo_pattern_add_color_stop_rgba
#define atsctrb_cairo_pattern_get_color_stop_count cairo_pattern_get_color_stop_count
#define atsctrb_cairo_pattern_get_color_stop_rgba cairo_pattern_get_color_stop_rgba

#define atsctrb_cairo_pattern_get_extend cairo_pattern_get_extend
#define atsctrb_cairo_pattern_set_extend cairo_pattern_set_extend
#define atsctrb_cairo_pattern_get_filter cairo_pattern_get_filter
#define atsctrb_cairo_pattern_set_filter cairo_pattern_set_filter

#define atsctrb_cairo_pattern_get_matrix cairo_pattern_get_matrix
#define atsctrb_cairo_pattern_set_matrix cairo_pattern_set_matrix

#if (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,4,0))
#define atsctrb_cairo_pattern_get_user_data cairo_pattern_get_user_data
#define atsctrb_cairo_pattern_set_user_data cairo_pattern_set_user_data
#endif // end of [#if (CAIRO_VERSION >= 1.4.0)]

/* ****** ****** */

//
// drawing texts
//

/* ****** ****** */

#define atsctrb_cairo_select_font_face cairo_select_font_face
#define atsctrb_cairo_set_font_size cairo_set_font_size

/* ****** ****** */

#define atsctrb_cairo_get_font_matrix cairo_get_font_matrix
#define atsctrb_cairo_set_font_matrix cairo_set_font_matrix

/* ****** ****** */

#define atsctrb_cairo_get_font_options cairo_get_font_options
#define atsctrb_cairo_set_font_options cairo_set_font_options

/* ****** ****** */

#define atsctrb_cairo_get_font_face cairo_get_font_face
#define atsctrb_cairo_set_font_face cairo_set_font_face

/* ****** ****** */

#define atsctrb_cairo_font_extents cairo_font_extents
#define atsctrb_cairo_text_extents cairo_text_extents
#define atsctrb_cairo_glyph_extents cairo_glyph_extents
 
/* ****** ****** */

#define atsctrb_cairo_text_path cairo_text_path
#define atsctrb_cairo_glyph_path cairo_glyph_path

/* ****** ****** */

#define atsctrb_cairo_show_text cairo_show_text
#define atsctrb_cairo_show_glyphs cairo_show_glyphs

/* ****** ****** */

#define atsctrb_cairo_toy_font_face_create cairo_toy_font_face_create
#define atsctrb_cairo_toy_font_face_get_family cairo_toy_font_face_get_family
#define atsctrb_cairo_toy_font_face_get_slant cairo_toy_font_face_get_slant
#define atsctrb_cairo_toy_font_face_get_weight cairo_toy_font_face_get_weight

/* ****** ****** */

#if (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,8,0))
#define atsctrb_cairo_glyph_allocate cairo_glyph_allocate
#define atsctrb_cairo_glyph_free cairo_glyph_free
#define atsctrb_cairo_cluster_allocate cairo_cluster_allocate
#define atsctrb_cairo_cluster_free cairo_cluster_free
#endif // end of [#if (CAIRO_VERSION >= 1.8.0)]

/* ****** ****** */

//
// transformations for drawing
//

/* ****** ****** */

#define atsctrb_cairo_translate cairo_translate
#define atsctrb_cairo_scale cairo_scale
#define atsctrb_cairo_rotate cairo_rotate
#define atsctrb_cairo_transform cairo_transform

/* ****** ****** */

#define atsctrb_cairo_get_matrix cairo_get_matrix
#define atsctrb_cairo_set_matrix cairo_set_matrix
#define atsctrb_cairo_identity_matrix cairo_identity_matrix

/* ****** ****** */

#define atsctrb_cairo_user_to_device cairo_user_to_device
#define atsctrb_cairo_user_to_device_distance cairo_user_to_device_distance
#define atsctrb_cairo_device_to_user cairo_device_to_user
#define atsctrb_cairo_device_to_user_distance cairo_device_to_user_distance

/* ****** ****** */

//
// fonts for drawing
//

#define atsctrb_cairo_font_face_status cairo_font_face_status
#define atsctrb_cairo_font_face_reference cairo_font_face_reference
#define atsctrb_cairo_font_face_destroy cairo_font_face_destroy
#define atsctrb_cairo_font_face_get_reference_count cairo_font_face_get_reference_count

typedef cairo_font_type_t ats_cairo_font_type_type ;

#define atsctrb_cairo_font_face_get_type cairo_font_face_get_type

/* ****** ****** */

//
// scaled fonts
//

#define atsctrb_cairo_scaled_font_status cairo_scaled_font_status
#define atsctrb_cairo_scaled_font_reference cairo_scaled_font_reference
#define atsctrb_cairo_scaled_font_destroy cairo_scaled_font_destroy
#define atsctrb_cairo_scaled_font_get_reference_count cairo_scaled_font_get_reference_count

#define atsctrb_cairo_scaled_font_extents cairo_scaled_font_extents
#define atsctrb_cairo_scaled_font_text_extents cairo_scaled_font_text_extents
#define atsctrb_cairo_scaled_font_get_font_face cairo_scaled_font_get_font_face
#define atsctrb_cairo_scaled_font_get_font_options cairo_scaled_font_get_font_options
#define atsctrb_cairo_scaled_font_get_font_matrix cairo_scaled_font_get_font_matrix
#define atsctrb_cairo_scaled_font_get_ctm cairo_scaled_font_get_ctm
#define atsctrb_cairo_scaled_font_get_scale_matrix cairo_scaled_font_get_scale_matrix
#define atsctrb_cairo_scaled_font_get_type cairo_scaled_font_get_type

/* ****** ****** */

//
// font options
//

#define atsctrb_cairo_font_options_create cairo_font_options_create
#define atsctrb_cairo_font_options_copy cairo_font_options_copy
#define atsctrb_cairo_font_options_destroy cairo_font_options_destroy
#define atsctrb_cairo_font_options_status cairo_font_options_status
#define atsctrb_cairo_font_options_merge cairo_font_options_merge
#define atsctrb_cairo_font_options_hash cairo_font_options_hash
#define atsctrb_cairo_font_options_equal cairo_font_options_equal

#define atsctrb_cairo_font_options_get_antialias cairo_font_options_get_antialias
#define atsctrb_cairo_font_options_set_antialias cairo_font_options_set_antialias
#define atsctrb_cairo_font_options_get_subpixel_order cairo_font_options_get_subpixel_order
#define atsctrb_cairo_font_options_set_subpixel_order cairo_font_options_set_subpixel_order
#define atsctrb_cairo_font_options_get_hint_style cairo_font_options_get_hint_style
#define atsctrb_cairo_font_options_set_hint_style cairo_font_options_set_hint_style
#define atsctrb_cairo_font_options_get_hint_metrics cairo_font_options_get_hint_metrics
#define atsctrb_cairo_font_options_set_hint_metrics cairo_font_options_set_hint_metrics

/* ****** ****** */

//
// Support for FreeType Font 
//

#define atsctrb_cairo_ft_font_face_create_for_ft_face cairo_ft_font_face_create_for_ft_face
#define atsctrb_cairo_ft_font_face_create_for_pattern cairo_ft_font_face_create_for_pattern
#define atsctrb_cairo_ft_font_options_substitute cairo_ft_font_options_substitute
#define atsctrb_cairo_ft_scaled_font_lock_face cairo_ft_scaled_font_lock_face
#define atsctrb_cairo_ft_scaled_font_unlock_face cairo_ft_scaled_font_unlock_face

/* ****** ****** */

//
// surfaces for drawing
//

#define atsctrb_cairo_surface_create_similar cairo_surface_create_similar

#define atsctrb_cairo_surface_status cairo_surface_status
#define atsctrb_cairo_surface_reference cairo_surface_reference
#define atsctrb_cairo_surface_destroy cairo_surface_destroy
#define atsctrb_cairo_surface_get_reference_count cairo_surface_get_reference_count

#define atsctrb_cairo_surface_finish cairo_surface_finish
#define atsctrb_cairo_surface_flush cairo_surface_flush
#define atsctrb_cairo_surface_get_font_options cairo_surface_get_font_options
#define atsctrb_cairo_surface_get_content cairo_surface_get_content
#define atsctrb_cairo_surface_mark_dirty cairo_surface_mark_dirty
#define atsctrb_cairo_surface_mark_dirty_rectangle cairo_surface_mark_dirty_rectangle
#define atsctrb_cairo_get_device_offset cairo_get_device_offset
#define atsctrb_cairo_set_device_offset cairo_set_device_offset

#define atsctrb_cairo_surface_get_type cairo_surface_get_type

#define atsctrb_cairo_surface_copy_page cairo_surface_copy_page
#define atsctrb_cairo_surface_show_page cairo_surface_show_page

/* ****** ****** */

// image surface

typedef cairo_format_t ats_cairo_format_type ;

#define atsctrb_cairo_format_stride_for_width cairo_format_stride_for_width
#define atsctrb_cairo_image_surface_create cairo_image_surface_create
#define atsctrb_cairo_image_surface_create_for_data cairo_image_surface_create_for_data
#define atsctrb_cairo_image_surface_get_data cairo_image_surface_get_data
#define atsctrb_cairo_image_surface_get_format cairo_image_surface_get_format
#define atsctrb_cairo_image_surface_get_width cairo_image_surface_get_width
#define atsctrb_cairo_image_surface_get_height cairo_image_surface_get_height
#define atsctrb_cairo_image_surface_get_stride cairo_image_surface_get_stride

/* ****** ****** */

// PNG support

#define atsctrb_cairo_image_surface_create_from_png cairo_image_surface_create_from_png
#define atsctrb_cairo_image_surface_create_from_png_stream cairo_image_surface_create_from_png_stream
#define atsctrb_cairo_surface_write_to_png cairo_surface_write_to_png
#define atsctrb_cairo_surface_write_to_png_stream cairo_surface_write_to_png_stream

/* ****** ****** */

// PDF surface

#if (CAIRO_HAS_PDF_SURFACE)

#define atsctrb_cairo_pdf_surface_create cairo_pdf_surface_create

static inline
ats_ref_type
atsctrb_cairo_pdf_surface_create_null (
  ats_double_type width, ats_double_type height
) {
  return cairo_pdf_surface_create((char*)0, width, height) ;
} // end of [atsctrb_cairo_pdf_surface_create_null]

#define atsctrb_cairo_pdf_surface_create_for_stream cairo_pdf_surface_create_for_stream
#define atsctrb_cairo_pdf_surface_set_size cairo_pdf_surface_set_size

#endif // end of [CAIRO_HAS_PDF_SURFACE]

/* ****** ****** */

// PS surface

#if (CAIRO_HAS_PS_SURFACE)

#define atsctrb_cairo_ps_surface_create cairo_ps_surface_create

static inline
ats_ref_type
atsctrb_cairo_ps_surface_create_null (
  ats_double_type width, ats_double_type height
) {
  return cairo_ps_surface_create((char*)0, width, height) ;
} // end of [atsctrb_cairo_ps_surface_create_null]

#define atsctrb_cairo_ps_surface_create_for_stream \
  cairo_ps_surface_create_for_stream

/*
typedef cairo_ps_level_t ats_cairo_ps_level_type ;

ats_ref_type
atsctrb_cairo_ps_get_levels
  (ats_ref_type num_levels) {
  cairo_ps_level_t **p_levels ;
  cairo_ps_get_levels (p_levels, (int*)num_levels) ;
  return (ats_ref_type)(*p_levels) ;
} // end of [cairo_ps_get_levels]
*/

#define atsctrb_cairo_ps_surface_set_size \
  cairo_ps_surface_set_size
#define atsctrb_cairo_ps_surface_dsc_begin_setup \
  cairo_ps_surface_dsc_begin_setup
#define atsctrb_cairo_ps_surface_dsc_begin_page_setup \
  cairo_ps_surface_dsc_begin_page_setup
#define atsctrb_cairo_ps_surface_dsc_comment \
  cairo_ps_surface_dsc_comment

#endif // end of [CAIRO_HAS_PS_SURFACE]

/* ****** ****** */

#if (CAIRO_HAS_SVG_SURFACE)

#define atsctrb_cairo_svg_surface_create \
  cairo_svg_surface_create
#define atsctrb_cairo_svg_surface_create_for_stream \
  cairo_svg_surface_create_for_stream
#define atsctrb_cairo_svg_surface_restrict_to_version \
  cairo_svg_surface_restrict_to_version

static inline
ats_ref_type // array of versions
atsctrb_cairo_svg_get_versions
  (ats_ref_type n) {
  cairo_svg_version_t const **versions ;
  cairo_svg_get_versions(versions, (int*)n) ; return (ats_ref_type)(*versions) ;
} // end of [atsctrb_cairo_svg_get_versions]

#define atsctrb_cairo_svg_version_to_string cairo_svg_version_to_string

#endif // end of [CAIRO_HAS_SVG_SURFACE]

/* ****** ****** */

#if (CAIRO_HAS_QUARTZ_SURFACE)

#define atsctrb_cairo_quartz_surface_create \
  cairo_quartz_surface_create
#define atsctrb_cairo_quartz_surface_create_for_cg_context \
  cairo_quartz_surface_create_for_cg_context
#define atsctrb_cairo_quartz_surface_get_cg_context \
  cairo_quartz_surface_get_cg_context

#endif // end of [CAIRO_HAS_QUARTZ_SURFACE]

/* ****** ****** */

#if (CAIRO_HAS_XLIB_SURFACE)

#define atsctrb_cairo_xlib_surface_create \
  cairo_xlib_surface_create

#define atsctrb_cairo_xlib_surface_create_for_bitmap \
  cairo_xlib_surface_create_for_bitmap

#define atsctrb_cairo_xlib_surface_set_size \
  cairo_xlib_surface_set_size

#define atsctrb_cairo_xlib_surface_get_drawable \
 cairo_xlib_surface_get_drawable

#define atsctrb_cairo_xlib_surface_set_drawable \
  cairo_xlib_surface_set_drawable

#define atsctrb_cairo_xlib_surface_get_display cairo_xlib_surface_get_display
#define atsctrb_cairo_xlib_surface_get_screen cairo_xlib_surface_get_screen
#define atsctrb_cairo_xlib_surface_get_visual cairo_xlib_surface_get_visual

#define atsctrb_cairo_xlib_surface_get_width cairo_xlib_surface_get_width
#define atsctrb_cairo_xlib_surface_get_height cairo_xlib_surface_get_height
#define atsctrb_cairo_xlib_surface_get_depth cairo_xlib_surface_get_depth

#endif // end of [CAIRO_HAS_XLIB_SURFACE]

/* ****** ****** */

//
// utilities for drawing
//

/* ****** ****** */

// generic matrix operations

#define atsctrb_cairo_matrix_init cairo_matrix_init
#define atsctrb_cairo_matrix_init_identity cairo_matrix_init_identity
#define atsctrb_cairo_matrix_init_translate cairo_matrix_init_translate
#define atsctrb_cairo_matrix_init_scale cairo_matrix_init_scale
#define atsctrb_cairo_matrix_init_rotate cairo_matrix_init_rotate

/* ****** ****** */

#define atsctrb_cairo_matrix_translate cairo_matrix_translate
#define atsctrb_cairo_matrix_scale cairo_matrix_scale
#define atsctrb_cairo_matrix_rotate cairo_matrix_rotate

/* ****** ****** */

#define atsctrb_cairo_matrix_invert cairo_matrix_invert
#define atsctrb_cairo_matrix_multiply cairo_matrix_multiply
#define atsctrb_cairo_matrix_transform_distance cairo_matrix_transform_distance
#define atsctrb_cairo_matrix_transform_point cairo_matrix_transform_point

/* ****** ****** */

#define atsctrb_cairo_status_to_string cairo_status_to_string
#define atsctrb_cairo_debug_reset_static_data cairo_debug_reset_static_data

/* ****** ****** */

//
// cairo version macros and functions
//

#define atsctrb_cairo_version cairo_version

static inline
ats_ptr_type // string
atsctrb_cairo_version_string () {
  return (ats_ptr_type)cairo_version_string() ;
} // end of [atsctrb_cairo_version_string]

/* ****** ****** */

#endif // end of [ATSCTRB_CAIRO_CAIRO_CATS]

/* end of [cairo.cats] */
