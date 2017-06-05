/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/translations/test1.c,v 1.3 2001/01/03 13:35:05 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include <Xm/XmP.h>
#include <Xm/TransltnsP.h>

struct {
        char    *n;
        char    *t;
} list[] = {
	{ "_XmArrowB_defaultTranslations", _XmArrowB_defaultTranslations },
	{ "_XmBulletinB_defaultTranslations", _XmBulletinB_defaultTranslations },
	{ "_XmCascadeB_menubar_events", _XmCascadeB_menubar_events },
	{ "_XmCascadeB_p_events", _XmCascadeB_p_events },
	{ "_XmClipWindowTranslationTable", _XmClipWindowTranslationTable },
	{ "_XmDragC_defaultTranslations", _XmDragC_defaultTranslations },
	{ "_XmDrawingA_defaultTranslations", _XmDrawingA_defaultTranslations },
	{ "_XmDrawingA_traversalTranslations", _XmDrawingA_traversalTranslations }, 
	{ "_XmDrawnB_defaultTranslations", _XmDrawnB_defaultTranslations }, 
	{ "_XmFrame_defaultTranslations", _XmFrame_defaultTranslations },
	{ "_XmLabel_defaultTranslations", _XmLabel_defaultTranslations },
	{ "_XmLabel_menuTranslations", _XmLabel_menuTranslations },
	{ "_XmLabel_menu_traversal_events", _XmLabel_menu_traversal_events },
	{ "_XmList_ListXlations1", _XmList_ListXlations1 },
	{ "_XmList_ListXlations2", _XmList_ListXlations2 },
	{ "_XmManager_defaultTranslations", _XmManager_defaultTranslations }, 
	{ "_XmManager_managerTraversalTranslations", _XmManager_managerTraversalTranslations }, 
	{ "_XmMenuShell_translations", _XmMenuShell_translations }, 
	{ "_XmPrimitive_defaultTranslations", _XmPrimitive_defaultTranslations }, 
	{ "_XmPushB_defaultTranslations", _XmPushB_defaultTranslations }, 
	{ "_XmPushB_menuTranslations", _XmPushB_menuTranslations }, 
	{ "_XmRowColumn_bar_table", _XmRowColumn_bar_table }, 
	{ "_XmRowColumn_menu_table", _XmRowColumn_menu_table }, 
	{ "_XmRowColumn_menu_traversal_table", _XmRowColumn_menu_traversal_table }, 
	{ "_XmRowColumn_option_table", _XmRowColumn_option_table }, 
	{ "_XmSash_defTranslations", _XmSash_defTranslations }, 
	{ "_XmScrollBar_defaultTranslations", _XmScrollBar_defaultTranslations }, 
	{ "_XmScrolledW_ScrolledWindowXlations", _XmScrolledW_ScrolledWindowXlations }, 
	{ "_XmSelectioB_defaultTextAccelerators", _XmSelectioB_defaultTextAccelerators }, 
	{ "_XmTearOffB_overrideTranslations", _XmTearOffB_overrideTranslations }, 
	{ "_XmTextF_EventBindings1", _XmTextF_EventBindings1 }, 
	{ "_XmTextF_EventBindings2", _XmTextF_EventBindings2 }, 
	{ "_XmTextF_EventBindings3", _XmTextF_EventBindings3 }, 
	{ "_XmTextIn_XmTextEventBindings1", _XmTextIn_XmTextEventBindings1 }, 
	{ "_XmTextIn_XmTextEventBindings2", _XmTextIn_XmTextEventBindings2 }, 
	{ "_XmTextIn_XmTextEventBindings3", _XmTextIn_XmTextEventBindings3 }, 
	{ "_XmToggleB_defaultTranslations", _XmToggleB_defaultTranslations }, 
	{ "_XmToggleB_menuTranslations", _XmToggleB_menuTranslations }, 
	{ "_XmVirtKeys_acornFallbackBindingString", _XmVirtKeys_acornFallbackBindingString }, 
	{ "_XmVirtKeys_apolloFallbackBindingString", _XmVirtKeys_apolloFallbackBindingString }, 
	{ "_XmVirtKeys_dblclkFallbackBindingString", _XmVirtKeys_dblclkFallbackBindingString }, 
	{ "_XmVirtKeys_decFallbackBindingString", _XmVirtKeys_decFallbackBindingString }, 
	{ "_XmVirtKeys_dgFallbackBindingString", _XmVirtKeys_dgFallbackBindingString }, 
	{ "_XmVirtKeys_fallbackBindingString", _XmVirtKeys_fallbackBindingString }, 
	{ "_XmVirtKeys_hpFallbackBindingString", _XmVirtKeys_hpFallbackBindingString }, 
	{ "_XmVirtKeys_ibmFallbackBindingString", _XmVirtKeys_ibmFallbackBindingString }, 
	{ "_XmVirtKeys_ingrFallbackBindingString", _XmVirtKeys_ingrFallbackBindingString }, 
	{ "_XmVirtKeys_megatekFallbackBindingString", _XmVirtKeys_megatekFallbackBindingString }, 
	{ "_XmVirtKeys_motorolaFallbackBindingString", _XmVirtKeys_motorolaFallbackBindingString }, 
	{ "_XmVirtKeys_sgiFallbackBindingString", _XmVirtKeys_sgiFallbackBindingString }, 
	{ "_XmVirtKeys_siemens9733FallbackBindingString", _XmVirtKeys_siemens9733FallbackBindingString }, 
	{ "_XmVirtKeys_siemensWx200FallbackBindingString", _XmVirtKeys_siemensWx200FallbackBindingString }, 
	{ "_XmVirtKeys_sunFallbackBindingString", _XmVirtKeys_sunFallbackBindingString }, 
	{ "_XmVirtKeys_tekFallbackBindingString", _XmVirtKeys_tekFallbackBindingString }, 
        { NULL, NULL }
};

char *copyright = 
"/**\n"
" *\n"
" * Copyright (C) 1997 Peter G. Williams, and Others\n"
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
main()
{
	int	i;
	char	*p;

        printf(copyright);
        printf("\n");
        printf("static char rcsid[] = \"$Id: test1.c,v 1.3 2001/01/03 13:35:05 amai Exp $\";\n");
        printf("\n");
	printf("#include <Xm/XmP.h>\n");
	printf("#include <Xm/TransltnsP.h>\n");
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
