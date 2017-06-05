/**
 *
 * print out the contents of the default font lists
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/fonts/test2.c,v 1.2 2002/04/13 12:00:36 amai Exp $
 *
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/AtomMgr.h>


Widget toplevel;
XmFontList fontlist;


void
printFontList(void) 
{
    XmFontContext context;
    XmFontListEntry entry;
   
    if (fontlist == NULL) printf ("bleh\n");
    XmFontListInitFontContext(&context,
			      fontlist);

    while((entry = XmFontListNextEntry(context)) != NULL)
    {
	XtPointer foo;
	XmFontType font_type;

	foo = XmFontListEntryGetFont(entry,
				     &font_type);
	
	printf ("Entry tag: %s\n", XmFontListEntryGetTag(entry));
	switch(font_type)
	{
	case XmFONT_IS_FONT:
	    {
		unsigned long value;

		printf ("  XFontStruct\n");
	
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_FONT_NAME,
				     &value))
		    printf ("   Name: %s\n", (char*)&value);
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_FAMILY_NAME,
				     &value))
		    printf ("   Family Name: %s\n", XmGetAtomName(XtDisplay(toplevel), (Atom)value));
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_POINT_SIZE,
				     &value))
		    printf ("   Point Size: %ld\n", value);
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_RESOLUTION,
				     &value))
		    printf ("   Resolution: %ld\n", value);
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_FULL_NAME,
				     &value))
		    printf ("   Full Name: %s\n", XmGetAtomName(XtDisplay(toplevel), (Atom)value));
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_WEIGHT,
				     &value))
		    printf ("   Weight: %ld\n", value);
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_X_HEIGHT,
				     &value))
		    printf ("   X Height: %ld\n", value);
		if (XGetFontProperty((XFontStruct*)foo,
				     XA_QUAD_WIDTH,
				     &value))
		    printf ("   Quad Width: %ld\n", value);
	    }
	    break;
	case XmFONT_IS_FONTSET:
	    printf ("  XFontSet\n");
	    break;
	}
    }
}


int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget label;


    toplevel = XtVaAppInitialize(&app, "Font", NULL, 0, &argc, argv, NULL, NULL);

    label = XmCreateLabel(toplevel, "label", NULL, 0);
    XtManageChild(label);

    XtRealizeWidget(toplevel);

    printf ("Doing XmBUTTON_FONTLIST\n");
    fontlist = _XmGetDefaultFontList(label, XmBUTTON_FONTLIST);
    printFontList();
    XmFontListFree(fontlist);

    printf ("\nDoing XmLABEL_FONTLIST\n");
    fontlist = _XmGetDefaultFontList(label, XmLABEL_FONTLIST);
    printFontList();
    XmFontListFree(fontlist);

    printf ("\nDoing XmTEXT_FONTLIST\n");
    fontlist = _XmGetDefaultFontList(label, XmTEXT_FONTLIST);
    printFontList();
    XmFontListFree(fontlist);

    printf ("\nDoing 0 (invalid)\n");
    fontlist = _XmGetDefaultFontList(label, 0);
    printFontList();
    XmFontListFree(fontlist);

    printf ("\nDoing 50 (invalid)\n");
    fontlist = _XmGetDefaultFontList(label, 4);
    printFontList();
    XmFontListFree(fontlist);

    exit(0);
}
