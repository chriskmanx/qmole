/**
 *
 * test1 of font list
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/fonts/test1.c,v 1.3 2002/04/13 12:00:36 amai Exp $
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>

int
main(int argc, char **argv)
{
    Widget toplevel;
    XtAppContext app;
    XmFontList fontlist;
    XmFontList foo;

    XtSetLanguageProc(NULL, NULL, NULL);
    
    toplevel = XtVaAppInitialize(&app, "Font", NULL, 0, &argc, argv, NULL, NULL);

    fontlist = XmFontListAppendEntry(NULL,
				     XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
							   XmFONT_IS_FONT,
							   XLoadQueryFont(XtDisplay(toplevel), 
									  "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

#if 0
    fprintf(stdout, "fontlist=%p\n", fontlist);
#endif

    foo = _XmGetDefaultFontList(toplevel, XmBUTTON_FONTLIST);
#if 0
    fprintf(stdout, "foo=%p\n", foo);
#endif

    XtRealizeWidget(toplevel);
    
    /* no geometry */
    LessTifTestMainLoop(toplevel);

    exit(0);
}
