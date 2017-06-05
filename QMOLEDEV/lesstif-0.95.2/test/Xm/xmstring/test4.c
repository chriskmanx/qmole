/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test4.c,v 1.3 2002/04/17 16:32:01 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>

/*
   This tests behavior pointed out by Scott Cramer

   if the tag given XmStringCreate is invalid, Motif1.2 
   uses the font associated with XmFONTLIST_DEFAULT_TAG.
*/

int 
main(int argc,
     char **argv)
{
    Widget toplevel, label;
    XtAppContext app;
    XmString xmstr;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    label = XmCreateLabel(toplevel,
			  "label",
			  NULL, 0);

    XtManageChild(label);

    xmstr = XmStringCreate("Hello World", "INVALID_FONT_LIST_TAG");

    XtVaSetValues(label,
		  XmNlabelString, xmstr,
		  NULL);

    /* free the storage from the XmString */
    XmStringFree(xmstr);

    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   70,   17, 0,0,0, /* label */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
    exit(0);
}
