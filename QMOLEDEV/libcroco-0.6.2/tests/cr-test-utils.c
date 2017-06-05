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

#include <string.h>
#include "cr-test-utils.h"

/**
 *Parses the command line.
 *@param a_argc the argc parameter of the main routine.
 *@param the argv parameter of the main routine.
 *@param a_options out parameter the parsed options.
 */
void
cr_test_utils_parse_cmd_line (int a_argc, char **a_argv,
                              struct Options *a_options)
{
        int i = 0;

        g_return_if_fail (a_options);

        memset (a_options, 0, sizeof (struct Options));

        for (i = 1; i < a_argc; i++) {
                if (a_argv[i][0] != '-')
                        break;

                if (!strcmp (a_argv[i], "-h")
                    || !strcmp (a_argv[i], "--help")) {
                        a_options->display_help = TRUE;
                }
                if (!strcmp (a_argv[i], "--about")) {
                        a_options->display_about = TRUE;
                }
        }

        if (i >= a_argc) {
                /*No file parameter where given */
                a_options->files_list = NULL;
        } else {
                a_options->files_list = &a_argv[i];
        }
}
