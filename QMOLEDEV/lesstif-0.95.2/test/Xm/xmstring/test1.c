/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test1.c,v 1.2 2002/04/17 16:32:01 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include <Xm/XmP.h>

#define	LEN	40

void PrintIt(char *p)
{
	int	i;

	for (i=0; i<LEN; i++)
	    printf("  %2X", 255 & p[i]);
	printf("\n");
	for (i=0; i<LEN; i++)
	    printf(" %3d", 255 & p[i]);
	printf("\n");
	for (i=0; i<LEN; i++)
	    if (isprint(p[i]))
		printf("   %c", 255 & p[i]);
	    else
		printf("   .");
	printf("\n\n");
}
int 
main(int argc,
     char **argv)
{
    Widget toplevel;
    XtAppContext app;
    Dimension width, height;
    XmFontList fl;
    XmString xmstr, xmstr2, xmstr3;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    fl = _XmGetDefaultFontList(toplevel, XmTEXT_FONTLIST);

    xmstr = XmStringCreate("Hello World", XmFONTLIST_DEFAULT_TAG);
    xmstr2 = XmStringCreateLtoR("Hello\nWorld", XmFONTLIST_DEFAULT_TAG);
    xmstr3 = XmStringCreateLocalized("Céline Dion a chantée à Paris");

    XmStringExtent(fl, xmstr, &width, &height);

    printf ("String 'Hello World' has dimensions %dx%d\n", width, height);

    XmStringExtent(fl, xmstr2, &width, &height);

    printf ("String 'Hello\\nWorld' has dimensions %dx%d\n", width, height);

    PrintIt((char *)xmstr);
    PrintIt((char *)xmstr2);
    PrintIt((char *)xmstr3);

    printf("Trying XmStringCreateLtoR(NULL, XmFONTLIST_DEFAULT_TAG) ...\n");
    xmstr = XmStringCreateLtoR(NULL, XmFONTLIST_DEFAULT_TAG);
    printf("Result is %p\n", xmstr);

    printf("Trying XmStringCreateLtoR('This', NULL) ...\n");
    xmstr = XmStringCreateLtoR("This", NULL);
    printf("Result is %p\n", xmstr);
    exit(0);
}
