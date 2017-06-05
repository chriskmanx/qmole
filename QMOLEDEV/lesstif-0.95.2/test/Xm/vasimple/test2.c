/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vasimple/test2.c,v 1.4 2002/03/22 00:00:07 amai Exp $ */
#include <stdlib.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>


void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget rowcol, pull;
  XtAppContext app;
  XmString s1, s2, s3;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  s1 = XmStringCreateSimple("button1");
  s2 = XmStringCreateSimple("button2");
  pull = XmVaCreateSimplePulldownMenu(toplevel, "ppane", 0, cb,
				    XmVaPUSHBUTTON, s1, 0, NULL, NULL,
				    XmVaPUSHBUTTON, s2, 0, NULL, NULL,
				    NULL);
  s3 = XmStringCreateSimple("option");
  rowcol = XmVaCreateSimpleOptionMenu(toplevel, "optionMenu",
				    s3, 0, 1, cb,
#if 0
				    XmNsubMenuId, pull,
#endif
				    NULL);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   78,   23, 0,0,0, /* optionMenu */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   40,   17, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   46,    3,   29,    4, 0,0,0, /* OptionButton */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
