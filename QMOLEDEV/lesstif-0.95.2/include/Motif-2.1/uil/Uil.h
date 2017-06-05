/**
 *
 * $Id: Uil.h,v 1.1 2004/08/28 19:23:37 dannybackx Exp $
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

#ifndef _UIL_UIL_H
#define _UIL_UIL_H

#ifdef __cplusplus
extern "C"
{
#endif

/*
 *  Uil_status_type
 */
typedef unsigned int Uil_status_type;

#define Uil_k_min_status	0
#define Uil_k_success_status	0
#define Uil_k_info_status	1
#define Uil_k_warning_status	2
#define Uil_k_error_status	3
#define Uil_k_severe_status	4
#define Uil_k_max_status	4

typedef char (*string_array)[];

/*
 *  Uil_command_type
 */
typedef struct _Uil_command_type
{
    char *source_file;
    char *resource_file;
    char *listing_file;
    unsigned int include_dir_count;
    char **include_dir;
    unsigned listing_file_flag:1;
    unsigned resource_file_flag:1;
    unsigned machine_code_flag:1;
    unsigned report_info_msg_flag:1;
    unsigned report_warn_msg_flag:1;
    unsigned parse_tree_flag:1;
    unsigned issue_summary:1;
    unsigned int status_update_delay;
    char *database;
    unsigned database_flag:1;
    unsigned use_setlocale_flag:1;
}
Uil_command_type;


/*
 * Uil_compile_desc_type
 */
typedef struct _Uil_comp_desc
{
    unsigned int compiler_version;
    unsigned int data_version;
    char *parse_tree_root;
    unsigned int message_count[Uil_k_max_status + 1];
}
Uil_compile_desc_type;


/*
 *  Uil_continue_type
 */
typedef unsigned int Uil_continue_type;

#define Uil_k_terminate		0
#define Uil_k_continue		1


/* The Motif docs specify the callbacks in an ANSI violating
   incomplete type. We define the callbacks to be of 
      Uil_continue_type(*)(void) instead of
      Uil_continue_type(*)()
 */
Uil_status_type Uil(Uil_command_type * command_desc,
                    Uil_compile_desc_type * compile_desc,
                    Uil_continue_type(*message_cb) (void),
                    char *message_data,
                    Uil_continue_type(*status_cb) (void),
                    char *status_data);


#ifdef __cplusplus
}
#endif

#endif /* _UIL_UIL_H */
