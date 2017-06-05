/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test6.c,v 1.2 2002/04/17 16:32:01 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>

int 
main(int argc,
     char **argv)
{
    Widget toplevel;
    XtAppContext app;
    XmString xmstr;
    char *text;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0,
				 &argc, argv, NULL, NULL);

    xmstr = XmStringCreateLtoR("Héllo\nWörld", XmFONTLIST_DEFAULT_TAG);

    XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &text);

    printf("text: %s\n", text);
    exit(0);
}
