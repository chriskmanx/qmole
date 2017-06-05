/**
 *
 * $Id: UilSymGl.h,v 1.1 2004/08/28 19:23:37 dannybackx Exp $
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

#ifndef _UIL_UILSYMGL_H
#define _UIL_UILSYMGL_H

#ifdef __cplusplus
extern "C" {
#endif

extern unsigned char *constraint_tab;
extern key_keytable_entry_type *key_table;
extern int key_k_keyword_max_length;
extern int key_k_keyword_count;
extern key_keytable_entry_type *key_table_case_ins;
extern unsigned char **allowed_argument_table;
extern unsigned char *argument_type_table;
extern unsigned char **allowed_child_table;
extern unsigned char *child_class_table;
extern char **charset_xmstring_names_table;
extern unsigned char *charset_writing_direction_table;
extern unsigned char *charset_parsing_direction_table;
extern unsigned char *charset_character_size_table;
extern char **charset_lang_names_table;
extern unsigned short int *charset_lang_codes_table;
extern unsigned short int charset_lang_table_max;
extern unsigned char **allowed_control_table;
extern UilEnumSetDescDef *enum_set_table;
extern unsigned short int *argument_enumset_table;
extern int *enumval_values_table;
extern int uil_max_object;
extern char **uil_widget_names;
extern int uil_max_arg;
extern char **uil_argument_names;
extern int uil_max_child;
extern char **uil_child_names;
extern int uil_max_reason;
extern char **uil_reason_names;
extern int uil_max_enumset;
extern int uil_max_enumval;
extern char **uil_enumval_names;
extern int uil_max_charset;
extern char **uil_charset_names;
extern unsigned short int *related_argument_table;
extern unsigned char **allowed_reason_table;
extern char **tok_token_name_table;
extern int tok_num_tokens;
extern char **uil_widget_funcs;
extern unsigned short int *uil_gadget_variants;
extern unsigned short int *uil_urm_nondialog_class;
extern unsigned short int *uil_urm_subtree_resource;
extern char **uil_argument_toolkit_names;
extern char **uil_reason_toolkit_names;

extern unsigned short int uil_sym_user_defined_object;
extern unsigned short int uil_sym_default_charset;
extern unsigned short int uil_sym_isolatin1_charset;

extern int uil_max_value;
extern char *uil_datatype_names[];

#ifdef __cplusplus
}
#endif


#endif /* _UIL_UILSYMGL_H */
