/**
 *
 * print out the contents of the default font lists
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/fonts/test3.c,v 1.1 2003/08/24 12:55:45 dannybackx Exp $
 *
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/AtomMgr.h>


Widget toplevel;
XmFontList fontlist;


int
main(int argc, char **argv)
{
	XtAppContext app;
	Widget label;
	int	i;


	toplevel = XtVaAppInitialize(&app, "Font", NULL, 0, &argc, argv,
		NULL, NULL);

	label = XmCreateLabel(toplevel, "label", NULL, 0);
	XtManageChild(label);

	XtRealizeWidget(toplevel);

	for (i=0; i<100; i++) {
		fontlist = _XmGetDefaultFontList(label, XmBUTTON_FONTLIST);
		XmFontListFree(fontlist);
		sleep(1);
	}

	exit(0);
}
