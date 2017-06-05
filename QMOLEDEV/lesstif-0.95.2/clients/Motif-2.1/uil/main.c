/**
 *
 * $Id: main.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/

static char *rcsid = "$Id: main.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_GETOPT
#ifdef __CYGWIN__
#include <getopt.h>
#endif
#else
#error You need getopt() to build this client!
#endif /* HAVE_GETOPT */

#include "FakeWidget.h"
#include "uil.h"
#include "glue.h"
#include "Include.h"

FILE *outFile = NULL;

static void EmitWidgetTree(FakeWidgetList *);


FakeWidgetList WidgetTable;

ExpressionList GlobalSymbolTable;
ExpressionList LocalSymbolTable;


int 
main(int argc, char **argv)
{
    int Generate_Binary_Code = False;
    int c;
    FILE *input;
    extern FILE *yyin, *yyout;
    extern FileData Files[];
    extern int yydebug;
    extern int yyparse(void);
    char OutputFileName[256];


    FakeWidgetListNew(&WidgetTable);
    ExpressionListNew(&GlobalSymbolTable);
    ExpressionListNew(&LocalSymbolTable);
    yydebug = 0;
    OutputFileName[0] = 0;

    while (-1 != (c = getopt(argc, argv, "dcI:mo:sv:w")))
    {
	switch (c)
	{
	case 'c':
	case 'd':
	    yydebug = 1;
	    break;
	case 'I':
	    IncludeAddDirectory(optarg);
	    break;
	case 'm':
	    Generate_Binary_Code = True;
	    break;
	case 'o':
	    strncpy(OutputFileName, optarg, sizeof(OutputFileName)-1);
	    OutputFileName[sizeof(OutputFileName)-1]='\0';
	    break;
	case 's':
	    fprintf(stderr, "set locale\n");
	    break;
	case 'V':
	    puts(rcsid);
	    exit(0);
	    break;
	case 'v':
	    fprintf(stderr, "generate listing to %s\n",
		    optarg);
	    break;
	case 'w':
	    fprintf(stderr, "suppress all warnings\n");
	    break;
	case 'W':
	    fprintf(stderr, "WMD\n");
	    break;
	case '?':
	    exit(1);
	    break;
	default:
	    fprintf(stderr, "?? getopt returned character code 0%o \n", c);
	    exit(1);
	    break;
	}
    }

    if (optind >= argc)
    {
	fprintf(stderr, "\n\nsevere: no source file specified\n");
	exit(1);
    }

    input = freopen(argv[optind], "r", stdin);
    if (input == NULL)
    {
	fprintf(stderr, "\n\nsevere: error opening source file: %s\n",
		argv[optind]);

	exit(1);
    }

    strncpy(Files[0].Name, argv[optind], sizeof(Files[0].Name)-1);
    Files[0].Name[sizeof(Files[0].Name)-1]='\0';
    Files[0].lineno = 1;

    if (0 == OutputFileName[0])
    {
	strcpy(OutputFileName, "a.uid");
    }

    outFile = fopen(OutputFileName, "w");
    if (outFile == NULL)
    {
	__MrmExit(LOC, "\n\nsevere: error opening outFile file: %s\n",
                  OutputFileName);
    }

    yyin = input;
    yyout = stdout;

    yyparse();

    FakeWidgetListIndex(&WidgetTable);

    EmitWidgetTree(&WidgetTable);

    exit(0);
}


static void
EmitWidgetTree(FakeWidgetList *wil)
{
    /* Emit a list of the widgets (maybe this can go) */
    FakeWidgetListEmitList(wil);

    /* Global symbols defined in this uil */
    ExpressionListEmit(&GlobalSymbolTable);

    fputc((unsigned char)0, outFile);
    /* Symbols only usable within this uil */
    ExpressionListEmit(&LocalSymbolTable);

    fputc((unsigned char)0, outFile);

    FakeWidgetListEmit(wil);
}
