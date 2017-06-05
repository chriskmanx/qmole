/**
 *
 * $Id: UilSymDef.h,v 1.1 2004/08/28 19:23:37 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#ifndef _UIL_UILSYMDEF_H
#define _UIL_UILSYMDEF_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif


/*
 * constraint check access macro
 */
#define _constraint_check(arg) \
    (constraint_tab[((arg)-1)/8] & (1 << ((arg)-1)%8))

/* 
 *  Header for each symbol entry
 */
typedef struct
{
    char b_tag;
    char b_type;
    unsigned short w_node_size;
    int user_data;
    struct _src_source_record_type *az_src_rec;
    unsigned char b_src_pos;
    unsigned char b_end_pos;
}
sym_entry_header_type;


typedef struct
{
    sym_entry_header_type header;
    char b_value[1];
}
sym_entry_type;


/*
 * NAMES NAMES NAMES NAMES NAMES NAMES NAMES NAMES
 */
#define		sym_m_referenced 	(1 << 0)
#define		sym_m_cycle_checked	(1 << 1)
#define		sym_m_has_cycle		(2 << 1)

typedef struct _sym_name_entry_type
{
    sym_entry_header_type header;
    sym_entry_type *az_object;
    struct _sym_name_entry_type *az_next_name_entry;
    struct _sym_name_entry_type *az_prev_name_entry;
    int az_cycle_id;
    unsigned char b_flags;
    char c_text[1];
}
sym_name_entry_type;

#define sym_k_name_entry_size (sizeof(sym_name_entry_type) - sizeof(char[1]))


/*
 * OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS 
 */
typedef struct
{
    sym_name_entry_type *az_name;
    sym_entry_type *az_reference;
    sym_entry_type *az_next;
    char *az_comment;
    unsigned int b_flags;
}
sym_obj_header_type;

typedef struct
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
}
sym_obj_entry_type;


/*
 * COLOR TABLE COLOR TABLE COLOR TABLE COLOR TABLE 
 */
typedef struct
{
    unsigned char b_letter;
    unsigned char b_index;
    unsigned short w_desc_offset;
    struct _sym_value_entry_type *az_color;
}
sym_color_element;


/*
 * COLOR ITEM COLOR ITEM COLOR ITEM COLOR ITEM 
 */
typedef struct _sym_color_item_entry_type
{
    sym_entry_header_type header;
    struct _sym_color_item_entry_type *az_next;
    unsigned char b_letter;
    unsigned char b_index;
    unsigned char b_spare1;
    struct _sym_value_entry_type *az_color;
}
sym_color_item_entry_type;

#define sym_k_color_item_entry_size (sizeof(sym_color_item_entry_type))


/*
 * ICONS ICONS ICONS ICONS ICONS ICONS ICONS ICONS 
 */
typedef struct
{
    unsigned short w_height;
    unsigned short w_width;
    struct _sym_value_entry_type *az_color_table;
    struct _sym_value_entry_type *az_rows;
}
sym_icon_element;

/*
 * aux flags
 */
#define		sym_m_table_entry	(1 << 0)
#define		sym_m_separate		(1 << 2)
#define		sym_m_sixteen_bit	sym_m_separate
#define		sym_m_exp_eval		(1 << 3)

/*
 * color flags
 */
#define	sym_k_unspecified_color	0
#define	sym_k_background_color	1
#define	sym_k_foreground_color	2


/*
 * VALUES VALUES VALUES VALUES VALUES VALUES VALUES VALUES 
 */
typedef struct _sym_value_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    unsigned char b_type;
    unsigned short w_length;
    char output_state;
    unsigned char b_table_count;
    unsigned char b_aux_flags;
    unsigned char b_arg_type;
    unsigned char b_data_offset;
    unsigned char b_pixel_type;
    unsigned char b_charset;
    unsigned char b_direction;
    unsigned char b_max_index;
    unsigned char b_expr_opr;
    unsigned int l_circular_ref_chk;
    unsigned short int b_enumeration_value_code;
    long int resource_id;

    struct _sym_value_entry_type *az_charset_value;
    struct _sym_value_entry_type *az_next_table_value;
    struct _sym_value_entry_type *az_first_table_value;
    struct _sym_value_entry_type *az_exp_op1;
    struct _sym_value_entry_type *az_exp_op2;

    union _sym_value_entry_value_union
    {
	int l_integer;
	double d_real;
	char *c_value;
	XmString xms_value;
	float single_float;
	sym_color_element *z_color;
	sym_icon_element *z_icon;
	char *az_data;
    }
    value;
}
sym_value_entry_type;

#define sym_k_value_entry_size (sizeof(sym_value_entry_type))


/*
 * LISTS LISTS LISTS LISTS LISTS LISTS LISTS LISTS 
 */
#define 	sym_k_error_list		sym_k_error_entry
#define 	sym_k_callback_list		sym_k_callback_entry
#define 	sym_k_argument_list		sym_k_argument_entry
#define 	sym_k_control_list		sym_k_control_entry
#define		sym_k_proc_ref_list		sym_k_proc_ref_entry

typedef struct _sym_list_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    unsigned short w_count;
    unsigned short w_gadget_count;
}
sym_list_entry_type;

#define sym_k_list_entry_size (sizeof(sym_list_entry_type))

typedef struct _sym_nested_list_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_list_entry_type *az_list;
}
sym_nested_list_entry_type;

#define sym_k_nested_list_entry_size (sizeof(sym_nested_list_entry_type))


/*
 * ARGUMENTS ARGUMENTS ARGUMENTS ARGUMENTS ARGUMENTS ARGUMENTS 
 */
typedef struct _sym_argument_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_value_entry_type *az_arg_name;
    sym_value_entry_type *az_arg_value;
}
sym_argument_entry_type;

#define sym_k_argument_entry_size (sizeof(sym_argument_entry_type))


/*
 * RESOURCES RESOURCES RESOURCES RESOURCES RESOURCES RESOURCES 
 */
typedef struct
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    unsigned char v_arg_checking;
    unsigned char b_arg_count;
    unsigned char b_arg_type;
    unsigned char b_widget_type;
}
sym_proc_def_entry_type;

#define sym_k_proc_def_entry_size (sizeof(sym_proc_def_entry_type))


/*
 * PROCEDURES PROCEDURES PROCEDURES PROCEDURES PROCEDURES PROCEDURES 
 */
typedef struct
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_proc_def_entry_type *az_proc_def;
    sym_value_entry_type *az_arg_value;
}
sym_proc_ref_entry_type;

#define sym_k_proc_ref_entry_size (sizeof(sym_proc_ref_entry_type))


#define sym_k_callback_proc	1
#define sym_k_object_proc	2


/*
 * CALLBACKS CALLBACKS CALLBACKS CALLBACKS CALLBACKS CALLBACKS 
 */
typedef struct _sym_callback_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_value_entry_type *az_call_reason_name;
    sym_proc_ref_entry_type *az_call_proc_ref;
    sym_list_entry_type *az_call_proc_ref_list;
}
sym_callback_entry_type;

#define sym_k_callback_entry_size (sizeof(sym_callback_entry_type))


/*
 * PARENT LIST PARENT LIST PARENT LIST PARENT LIST PARENT LIST 
 */
typedef struct _sym_parent_list_type
{
    sym_entry_header_type header;
    struct _sym_widget_entry_type *parent;
    struct _sym_parent_list_type *next;
}
sym_parent_list_type;

#define sym_k_parent_list_size (sizeof(sym_parent_list_type))


/*
 * WIDGETS & GADGETS WIDGETS & GADGETS WIDGETS & GADGETS WIDGETS & GADGETS 
 */
typedef struct _sym_widget_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_list_entry_type *az_callbacks;
    sym_list_entry_type *az_arguments;
    sym_list_entry_type *az_controls;
    sym_proc_ref_entry_type *az_create_proc;
    sym_parent_list_type *parent_list;
    int output_state;
    long int resource_id;
}
sym_widget_entry_type;

#define sym_k_widget_entry_size (sizeof(sym_widget_entry_type))


/*
 * CONTROLS CONTROLS CONTROLS CONTROLS CONTROLS CONTROLS CONTROLS CONTROLS 
 */
#define	sym_m_def_in_progress	(1 << 6)
#define	sym_m_managed		(1 << 7)
#define sym_m_obj_is_reference  (1 << 8)
#define sym_m_forward_ref	(1 << 9)
#define sym_m_validated		(1 << 10)

typedef struct _sym_control_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_widget_entry_type *az_con_obj;
}
sym_control_entry_type;

#define sym_k_control_entry_size (sizeof(sym_control_entry_type))


/*
 * EXTERNS EXTERNS EXTERNS EXTERNS EXTERNS EXTERNS EXTERNS EXTERNS 
 */
typedef struct _sym_external_def_entry_type
{
    sym_entry_header_type header;
    struct _sym_external_def_entry_type *az_next_object;
    sym_name_entry_type *az_name;
}
sym_external_def_entry_type;

#define sym_k_external_def_entry_size (sizeof(sym_external_def_entry_type))


/*
 * FORWARD REF FORWARD REF FORWARD REF FORWARD REF FORWARD REF
 */
typedef struct _sym_forward_ref_entry_type
{
    sym_entry_header_type header;
    struct _sym_forward_ref_entry_type *az_next_ref;
    char *a_update_location;
    sym_name_entry_type *az_name;
    sym_widget_entry_type *parent;
}
sym_forward_ref_entry_type;

#define sym_k_forward_ref_entry_size (sizeof(sym_forward_ref_entry_type))

#define sym_k_patch_add		1
#define sym_k_bind_value_name	2
#define sym_k_patch_list_add	3

typedef struct _sym_val_forward_ref_entry_type
{
    sym_entry_header_type header;
    struct _sym_val_forward_ref_entry_type *az_next_ref;
    char *a_update_location;
    sym_name_entry_type *az_name;
    unsigned char fwd_ref_flags;
}
sym_val_forward_ref_entry_type;

#define sym_k_val_forward_ref_entry_size (sizeof(sym_val_forward_ref_entry_type))


/*
 * OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS OBJECTS 
 */
typedef struct _sym_def_obj_entry_type
{
    sym_entry_header_type header;
    struct _sym_def_obj_entry_type *next;
    char b_object_info;
    char b_variant_info;
}
sym_def_obj_entry_type;

#define sym_k_def_obj_entry_size (sizeof(sym_def_obj_entry_type))


/*
 * MODULE MODULE MODULE MODULE MODULE MODULE MODULE MODULE 
 */
typedef struct _sym_module_entry_type
{
    sym_entry_header_type header;
    sym_obj_header_type obj_header;
    sym_value_entry_type *az_version;
    sym_value_entry_type *az_character_set;
    sym_value_entry_type *az_case_sense;
    sym_def_obj_entry_type *az_def_obj;
}
sym_module_entry_type;

#define sym_k_module_entry_size (sizeof(sym_module_entry_type))


/*
 * SECTIONS SECTIONS SECTIONS SECTIONS SECTIONS SECTIONS SECTIONS SECTIONS 
 */
#define sym_k_list_section	1
#define sym_k_procedure_section	2
#define sym_k_value_section	3
#define sym_k_identifier_section 4
#define sym_k_object_section	5
#define sym_k_include_section	6
#define sym_k_section_tail	7

typedef struct _sym_section_entry_type
{
    sym_entry_header_type header;
    struct _sym_section_entry_type *prev_section;
    sym_entry_type *next;
    sym_entry_type *entries;
}
sym_section_entry_type;

#define sym_k_section_entry_size (sizeof (sym_section_entry_type))


/*
 * INCLUDES INCLUDES INCLUDES INCLUDES INCLUDES INCLUDES INCLUDES INCLUDES 
 */
typedef struct _sym_include_file_entry_type
{
    sym_entry_header_type header;
    sym_section_entry_type *sections;
    char file_name[255];
    char full_file_name[255];
}
sym_include_file_entry_type;

#define sym_k_include_file_entry_size (sizeof(sym_include_file_entry_type))


/*
 * SYM TABLE ROOT SYM TABLE ROOT SYM TABLE ROOT SYM TABLE ROOT 
 */
typedef struct _sym_root_entry_type
{
    sym_entry_header_type header;
    struct _src_source_record_type *src_record_list;
    char file_name[255];
    char full_file_name[255];
    sym_section_entry_type *sections;
    sym_module_entry_type *module_hdr;
}
sym_root_entry_type;

#define sym_k_root_entry_size (sizeof(sym_root_entry_type))


#define _sym_copy_entry(__dest, __src, __size) _move (__dest, __src, __size)

#ifdef __cplusplus
}
#endif


#endif /* _UIL_UILSYMDEF_H */
