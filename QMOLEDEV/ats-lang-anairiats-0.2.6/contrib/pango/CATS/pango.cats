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
// Starting time: May, 2010

/* ****** ****** */

#ifndef ATSCTRB_PANGO_PANGO_CATS
#define ATSCTRB_PANGO_PANGO_CATS

/* ****** ****** */

#include <pango/pango.h>

/* ****** ****** */

/* ****** ****** */

//
// pango-attributes.h
//

#define atsctrb_pango_color_copy pango_color_copy
#define atsctrb_pango_color_free pango_color_free
#define atsctrb_pango_color_parser pango_color_parse

#define atsctrb_pango_attribute_copy pango_attribute_copy
#define atsctrb_pango_attribute_destroy pango_attribute_destroy
#define atsctrb_pango_attribute_equal pango_attribute_equal

#define atsctrb_pango_attr_foreground_new pango_attr_foreground_new
#define atsctrb_pango_attr_background_new pango_attr_background_new

#define atsctrb_pango_attr_list_new pango_attr_list_new
#define atsctrb_pango_attr_list_ref pango_attr_list_ref
#define atsctrb_pango_attr_list_unref pango_attr_list_unref
#define atsctrb_pango_attr_list_copy pango_attr_list_copy

#define atsctrb_pango_attr_list_insert pango_attr_list_insert
#define atsctrb_pango_attr_list_insert_before pango_attr_list_insert_before
#define atsctrb_pango_attr_list_change pango_attr_list_change

/* ****** ****** */

//
// pango-context.h
//

/* ****** ****** */

//
// pango-font.h
//

#define atsctrb_pango_font_description_from_string \
  pango_font_description_from_string
#define atsctrb_pango_font_description_to_string \
  pango_font_description_to_string
#define atsctrb_pango_font_description_to_filename \
  pango_font_description_to_filename

#define atsctrb_pango_font_description_free pango_font_description_free

/* ****** ****** */

//
// pango-layout.h
//

#define atsctrb_pango_layout_new pango_layout_new
#define atsctrb_pango_layout_copy pango_layout_copy
//
#define atsctrb_pango_layout_get_attributes pango_layout_get_attributes
#define atsctrb_pango_layout_set_attributes pango_layout_set_attributes
//
#define atsctrb_pango_layout_get_text \
  atsctrb_pango_layout_get_text
#define atsctrb_pango_layout_set_text \
  atsctrb_pango_layout_set_text
#define atsctrb_pango_layout_setall_text(layout,text) \
  pango_layout_set_text(layout, text, -1)
//
#define atsctrb_pango_layout_get_size pango_layout_get_size
#define atsctrb_pango_layout_get_pixel_size pango_layout_get_pixel_size
//
#define atsctrb_pango_layout_get_width pango_layout_get_width
#define atsctrb_pango_layout_set_width pango_layout_set_width
#define atsctrb_pango_layout_get_height pango_layout_get_height
#define atsctrb_pango_layout_set_height pango_layout_set_height
//
#define atsctrb_pango_layout_get_alignment pango_layout_get_alignment
#define atsctrb_pango_layout_set_alignment pango_layout_set_alignment
//
#define atsctrb_pango_layout_get_wrap pango_layout_get_wrap
#define atsctrb_pango_layout_set_wrap pango_layout_set_wrap
#define atsctrb_pango_layout_is_wrapped pango_layout_is_wrapped
//
#define atsctrb_pango_layout_get_ellipsize pango_layout_get_ellipsize
#define atsctrb_pango_layout_set_ellipsize pango_layout_set_ellipsize
#define atsctrb_pango_layout_is_ellipsized pango_layout_is_ellipsized
//
#define atsctrb_pango_layout_get_indent pango_layout_get_indent
#define atsctrb_pango_layout_set_indent pango_layout_set_indent
#define atsctrb_pango_layout_get_spacing pango_layout_get_spacing
#define atsctrb_pango_layout_set_spacing pango_layout_set_spacing
//
#define atsctrb_pango_layout_get_justify pango_layout_get_justify
#define atsctrb_pango_layout_set_justify pango_layout_set_justify
#define atsctrb_pango_layout_get_auto_dir pango_layout_get_auto_dir
#define atsctrb_pango_layout_set_auto_dir pango_layout_set_auto_dir
#define atsctrb_pango_layout_get_single_paragraph_mode \
  pango_layout_get_single_paragraph_mode
#define atsctrb_pango_layout_set_single_paragraph_mode \
  pango_layout_set_single_paragraph_mode

/* ****** ****** */

#endif // end of [ATSCTRB_PANGO_PANGO_CATS]

/* end of [pango.cats] */
