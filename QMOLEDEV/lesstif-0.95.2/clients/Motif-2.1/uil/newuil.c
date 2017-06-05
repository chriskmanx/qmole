/**
 *
 * $Id: newuil.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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
 */

static char *rcsid = "$Id: newuil.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $";


#include <LTconfig.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Intrinsic.h>

#include <uil/Uil.h>
#include <uil/UilDef.h>

static int max_includes = 8;

static Uil_command_type command = { 0 };
static Uil_compile_desc_type desc;


int 
main(int argc, char **argv)
{
    int i;

    command.resource_file = "a.uid";
    command.resource_file_flag = 1;
    command.report_info_msg_flag = 1;
    command.report_warn_msg_flag = 1;
    command.issue_summary = 1;
    command.parse_tree_flag = 1;

    for (i = 1; i < argc; )
    {
	if (argv[i][0] == '-')
	{
	    switch (argv[i][1])
	    {
	    case 'I':
		if (command.include_dir == NULL)
		{
		    command.include_dir = (char **)XtCalloc(max_includes,
							    sizeof(char *));
		}
		else if (command.include_dir_count == max_includes)
		{
		    max_includes <<= 1;
		    command.include_dir =
			(char **)XtRealloc((char *)command.include_dir,
					   max_includes * sizeof(char *));
		}

		if (argv[i][2] == 0)
		{
		    i++;
		    if (i == argc)
		    {
			fprintf(stderr, "Missing argument to -I directive.\n");
			exit(EXIT_FAILURE);
		    }
		    command.include_dir[command.include_dir_count] = argv[i];
		}
		else
		{
		    command.include_dir[command.include_dir_count] =
			&argv[i][2];
		}
		command.include_dir_count++;
		break;

	    case 'm':
		command.machine_code_flag = 1;
		break;

	    case 'o':
		if (argv[i][2] == 0)
		{
		    i++;
		    if (i == argc)
		    {
			fprintf(stderr,
				"\n\nMissing argument to -o directive.\n");

			exit(EXIT_FAILURE);
		    }
		    command.resource_file = argv[i];
		}
		else
		{
		    command.resource_file = &argv[i][2];
		}
		break;

	    case 's':
		command.use_setlocale_flag = 1;
		break;

	    case 'V':
		puts(rcsid);
		exit(EXIT_SUCCESS);
		break;

	    case 'v':
		if (argv[i][2] == 0)
		{
		    i++;
		    if (i == argc)
		    {
			fprintf(stderr,
				"\n\nMissing argument to -v directive.\n");

			exit(EXIT_FAILURE);
		    }
		    command.listing_file = argv[i];
		}
		else
		{
		    command.listing_file = &argv[i][2];
		}
		command.listing_file_flag = 1;
		break;

	    case 'w':
		if (strcmp(&argv[i][1], "wmd") == 0)
		{
		    if (argv[i][2] == 0)
		    {
			i++;
			if (i == argc)
			{
			    fprintf(stderr,
				    "\n\nMissing argument to -wmd directive.\n");

			    exit(EXIT_FAILURE);
			}
			command.database = argv[i];
		    }
		    else
		    {
			command.database = &argv[i][2];
		    }
		    command.database_flag = 1;
		}
		else
		{
		    command.report_info_msg_flag = 0;
		    command.report_warn_msg_flag = 0;
		    command.issue_summary = 0;
		}
		break;

	    default:
		fprintf(stderr, "\n\nSevere: Invalid option specified.\n");
		exit(EXIT_FAILURE);
	    }
	}
	else
	{
	    break;
	}

	i++;
    }

    if (i >= argc)
    {
	fprintf(stderr, "\n\nSevere: no source file specified\n");

	exit(EXIT_FAILURE);
    }

    command.source_file = argv[i];

    if (Uil(&command, &desc, NULL, NULL, NULL, NULL) == Uil_k_success_status)
    {
#if 0
	UilDumpSymbolTable((sym_entry_type *)desc.parse_tree_root);
#else
;
#endif
    }
   exit(0);

}
