/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test2.c,v 1.3 2002/04/17 16:32:01 amai Exp $
   test for non-existant fontlist tag */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>


int 
main(int argc,
     char **argv)
{
    Widget toplevel;
    XtAppContext app;
    XmString xmstr;
    Widget label;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    xmstr = XmStringCreate("Hello World", "bleh");

    label = XtVaCreateManagedWidget("label", 
				    xmLabelWidgetClass,
				    toplevel,
				    XmNlabelString, xmstr,
				    NULL);
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
