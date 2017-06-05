/* -*- Mode: C; indent-tabs-mode:nil; c-basic-offset:8 -*- */

/*
 * This file is part of The Croco Library
 *
 * Copyright (C) 2002-2003 Dodji Seketeli <dodji@seketeli.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

/*
 *$Id: cr-test-utils.h 55 2003-03-19 20:48:37Z strider $
 */
#ifndef __CR_TEST_UTILS_H__
#define __CR_TEST_UTILS_H__

#include <stdio.h>
#include <glib.h>

/**
 *The options data structure.
 *The variable of this data structure are set
 *during the  parsing the command line by the
 *parse_command_line() function.
 */
struct Options
{
        gboolean display_help ;
        gboolean display_about ;
        gchar ** files_list ;
};


void
cr_test_utils_parse_cmd_line (int a_argc, char **a_argv,
			      struct Options *a_options) ;

#endif /*__CR_TEST_UTILS_H__*/
