/**
 *
 * $Id: UilDBDef.h,v 1.1 2004/08/28 19:23:37 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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


#ifndef _UIL_UIL_DBDEF_H
#define _UIL_UIL_DBDEF_H

#ifdef __cplusplus
extern "C" {
#endif


#define _BIT_INDEX(_type)	\
            (((unsigned int) (_type)) >> 3)

#define _BIT_MASK(_type)	\
  (1 << (((unsigned int) (_type)) & 0x7))

#define _BIT_SET(table_entry_addr, _type) \
  ( _BIT_MASK (_type) &  \
    ( ((unsigned char *) (table_entry_addr)) \
      [ _BIT_INDEX (_type) ] ) )


/*
 * hash table size
 */
#define sym_k_hash_table_limit	127


/*
 * entry types
 */
#define sym_k_error_entry		(127)
#define sym_k_value_entry		1
#define sym_k_name_entry		2
#define sym_k_widget_entry		3
#define sym_k_control_entry		4
#define sym_k_forward_ref_entry		5
#define sym_k_external_def_entry	6
#define sym_k_argument_entry		7
#define sym_k_callback_entry		8
#define sym_k_module_entry		9
#define sym_k_proc_def_entry		10
#define sym_k_proc_ref_entry		11
#define sym_k_list_entry		12
#define sym_k_child_entry		13
#define sym_k_identifier_entry		14
#define sym_k_color_item_entry		15
#define sym_k_gadget_entry		16
#define sym_k_root_entry		17
#define sym_k_parent_list_entry		18
#define sym_k_nested_list_entry		19
#define sym_k_include_file_entry	20
#define sym_k_section_entry		21
#define sym_k_def_obj_entry		22
#define sym_k_UNUSED23_entry		23
#define sym_k_val_forward_ref_entry	24
#define sym_k_max_entry			24


/*
 * entry flags
 */
#define		sym_m_private		(1 << 0)
#define		sym_m_exported		(1 << 1)
#define		sym_m_imported		(1 << 2)
#define		sym_m_reference		(1 << 3)
#define		sym_m_builtin		(1 << 4)
#define		sym_m_obj_is_gadget	(1 << 5)


/*
 * output states
 */
#define		sym_k_not_processed	0
#define		sym_k_queued		1
#define		sym_k_emitted		2


/*
 * type values
 */
#define sym_k_any_value			1
#define sym_k_argument_value		2
#define sym_k_asciz_table_value		3
#define sym_k_bool_value		4
#define sym_k_char_8_value		5
#define sym_k_class_rec_name_value	6
#define sym_k_color_value		7
#define sym_k_color_table_value		8
#define sym_k_compound_string_value	9
#define sym_k_float_value		10
#define sym_k_font_value		11
#define sym_k_font_table_value		12
#define sym_k_icon_value		13
#define sym_k_identifier_value		14
#define sym_k_integer_value		15
#define sym_k_integer_table_value	16
#define sym_k_keysym_value		17
#define sym_k_pixmap_value		18
#define sym_k_reason_value		19
#define sym_k_rgb_value			20
#define sym_k_single_float_value	21
#define sym_k_string_table_value	22
#define sym_k_trans_table_value		23
#define sym_k_widget_ref_value		24
#define sym_k_xbitmapfile_value		25
#define sym_k_localized_string_value	26
#define sym_k_wchar_string_value	27
#define sym_k_fontset_value		28
#define	sym_k_child_value		29
#define sym_k_max_value			29
#define sym_k_no_value			(sym_k_max_value+1)


/*
 * error values
 */
#define sym_k_error_value		0
#define sym_k_error_object		0
#define sym_k_error_charset		0


/*
 * character sizes
 */
#define sym_k_onebyte_charsize	1
#define sym_k_twobyte_charsize	2
#define sym_k_mixed1_2byte_charsize	3

/*
 * charset tags
 */
#define sym_k_userdefined_charset	1
#define sym_k_fontlist_default_tag 	0

/*
 * operator values
 */
#define sym_k_unspecified_op	0
#define sym_k_not_op		1
#define sym_k_unary_plus_op	2
#define sym_k_unary_minus_op	3
#define sym_k_comp_str_op	4
#define sym_k_wchar_str_op	5
#define sym_k_last_unary_op	5
#define sym_k_multiply_op	6
#define sym_k_divide_op		7
#define sym_k_add_op		8
#define sym_k_subtract_op	9
#define sym_k_left_shift_op	10
#define sym_k_right_shift_op	11
#define sym_k_and_op		12
#define sym_k_xor_op		13
#define sym_k_or_op		14
#define sym_k_cat_op		15
#define sym_k_last_binary_op	15
#define sym_k_valref_op		16
#define sym_k_coerce_op		17
#define sym_k_last_special_op	17

/*
 * enum structure
 */
typedef struct
{
    short int values_cnt;
    unsigned short int *values;
}
UilEnumSetDescDef, *UilEnumSetDescDefPtr;


/*
 * token class literals
 */
#define tkn_k_class_argument    1
#define tkn_k_class_charset     2
#define tkn_k_class_color       3
#define tkn_k_class_enumval     4
#define tkn_k_class_font        5
#define tkn_k_class_identifier  6
#define tkn_k_class_keyword     7
#define tkn_k_class_literal     8
#define tkn_k_class_reason      9
#define tkn_k_class_reserved    10
#define tkn_k_class_special     11
#define tkn_k_class_unused      12
#define tkn_k_class_class       13
#define tkn_k_class_child	14

/*
 * keyword table entry
 */
typedef struct
{
    unsigned char b_class;
    unsigned short int b_subclass;
    unsigned char b_length;
    unsigned char b_token;
    char *at_name;
}
key_keytable_entry_type;



#define Constraint_Tab			1
#define Key_Table			2
#define Key_Table_Case_Ins		3
#define Allowed_Argument_Table		4
#define Argument_Type_Table_Value	5
#define Charset_Xmstring_Names_Table	6
#define Charset_Wrdirection_Table	7
#define Charset_Parsdirection_Table	8
#define Charset_Charsize_Table		9
#define Charset_Lang_Names_Table	10
#define Charset_Lang_Codes_Table	11
#define Allowed_Control_Table		12
#define Enum_Set_Table			13
#define Argument_Enum_Set_Table		14
#define Enumval_Values_Table		15
#define Uil_Widget_Names		16
#define Uil_Argument_Names		17
#define Uil_Reason_Names		18
#define Uil_Enumval_names		19
#define Uil_Charset_Names		20
#define Related_Argument_Table		21
#define Allowed_Reason_Table		22
#define Uil_Widget_Funcs		23
#define Uil_Gadget_Funcs		24
#define Uil_Urm_Nondialog_Class		25
#define Uil_Urm_Subtree_Resource	26
#define Uil_Argument_Toolkit_Names	27
#define Uil_Reason_Toolkit_Names	28
#define Child_Class_Table		29
#define Allowed_Child_Table		30
#define Uil_Children_Names		31

#define DB_Compiled_Version 2

typedef struct _db_globals_struct
{
    int version;
    int uil_max_arg;
    int uil_max_charset;
    int charset_lang_table_max;
    int uil_max_object;
    int uil_max_reason;
    int uil_max_enumval;
    int uil_max_enumset;
    int key_k_keyword_count;
    int key_k_keyword_max_length;
    int uil_max_child;
}
_db_globals;

typedef struct _db_header_struct
{
    int table_id;
    int num_items;
    int table_size;
}
_db_header, *_db_header_ptr;


#ifdef __cplusplus
}
#endif


#endif /* _UIL_UIL_H */
