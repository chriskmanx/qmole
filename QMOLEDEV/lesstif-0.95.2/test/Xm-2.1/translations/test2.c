/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/translations/test2.c,v 1.2 2001/05/08 08:58:16 amai Exp $
 */
 
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include <Xm/XmP.h>
#include <Xm/TransltnsP.h>


#if XmVersion != 2001
int
main(int argc, char *argv[])
{
 puts("This program requires Motif 2.1\n");
 exit(1);
}

#else


struct {
        const char    *n;
        const char    *t;
} list[] = {
	{ "_XmComboBox_defaultTranslations", _XmComboBox_defaultTranslations },
	{ "_XmComboBox_defaultAccelerators", _XmComboBox_defaultAccelerators },
	{ "_XmComboBox_dropDownComboBoxAccelerators", _XmComboBox_dropDownComboBoxAccelerators },
	{ "_XmComboBox_dropDownListTranslations", _XmComboBox_dropDownListTranslations }, 
	{ "_XmComboBox_textFocusTranslations", _XmComboBox_textFocusTranslations }, 
	{ "_XmContainer_defaultTranslations", _XmContainer_defaultTranslations }, 
	{ "_XmContainer_traversalTranslations", _XmContainer_traversalTranslations }, 
	{ "_XmGrabShell_translations", _XmGrabShell_translations },
	{ "_XmNotebook_manager_translations", _XmNotebook_manager_translations }, 
	{ "_XmNotebook_TabAccelerators", _XmNotebook_TabAccelerators }, 
	{ "_XmSpinB_defaultTranslations", _XmSpinB_defaultTranslations }, 
	{ "_XmSpinB_defaultAccelerators", _XmSpinB_defaultAccelerators }, 
        { NULL, NULL }
};

const char *copyright = 
"/**\n"
" *\n"
" * Copyright (C) 1997 Free Software Foundation, Inc.\n"
" *\n"
" * This file is part of the GNU LessTif Library.\n"
" *\n"
" * This library is free software; you can redistribute it and/or\n"
" * modify it under the terms of the GNU Library General Public\n"
" * License as published by the Free Software Foundation; either\n"
" * version 2 of the License, or (at your option) any later version.\n"
" *\n"
" * This library is distributed in the hope that it will be useful,\n"
" * but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
" * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
" * Library General Public License for more details.\n"
" *\n"
" * You should have received a copy of the GNU Library General Public\n"
" * License along with this library; if not, write to the Free\n"
" * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n"
" *\n"
" **/\n";

int
main(int argc, char *argv[])
{
	int	i;
	const char *p;

	printf(copyright);
	printf("\n");
	printf("static char rcsid[] = \"$Id: test2.c,v 1.2 2001/05/08 08:58:16 amai Exp $\";\n");
	printf("\n");
        printf("#include <Xm/XmP.h>\n");
        printf("#include <Xm/TransltnsP.h>\n");
        printf("\n");
	printf("/* Translation & accelerator tables */\n");
	printf("\n");
	for (i=0; list[i].n; i++) {
		printf("_XmConst char %s[] =\n    %c", list[i].n, '"');
		for (p = list[i].t; *p; p++)
			if (*p == '\n') {
				putchar('\\');
				putchar('n');
				putchar('"');
				putchar('\n');
				putchar(' ');
				putchar(' ');
				putchar(' ');
				putchar(' ');
				putchar('"');
			} else if (*p == '"') {
				putchar('\\');
				putchar('"');
			} else if (!isprint(*p)&&*p!='\t') {
				printf("\\%o", (unsigned char)*p);
			} else
				putchar(*p);
		putchar('"');
		putchar(';');
		putchar('\n');
		putchar('\n');
	}
	exit(0);
}
#endif
