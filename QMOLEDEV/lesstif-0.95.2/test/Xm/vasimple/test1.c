/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vasimple/test1.c,v 1.5 2002/03/22 00:00:07 amai Exp $ */
#include <stdlib.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>


void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel, rowcol;
  XtAppContext app;
  XmString s1, s2;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  s1 = XmStringCreateSimple("check1");
  s2 = XmStringCreateSimple("check2");
  rowcol = XmVaCreateSimpleCheckBox(toplevel, "checkBox", cb,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmVaCHECKBUTTON, s1, 0, NULL, NULL,
				    XmVaCHECKBUTTON, s2, 0, NULL, NULL,
				    NULL);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   67,   60, 0,0,0, /* checkBox */
   CWWidth | CWHeight | CWX | CWY,    3,    4,   61,   25, 0,0,0, /* button_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,   61,   25, 0,0,0, /* button_1 */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
