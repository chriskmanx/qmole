#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

void
doit(Widget w, XtPointer data, XtPointer cbs) {
    XmProcessTraversal(w, XmTRAVERSE_HOME);
}

int
main(int argc, char **argv) {
    Widget toplevel, rowcol, pb;
    XtAppContext app;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "travers", NULL, 0,
				 &argc, argv, NULL, NULL);
    rowcol = XtVaCreateManagedWidget("rowcolumn",
				xmRowColumnWidgetClass, toplevel,
				XmNorientation, XmHORIZONTAL, NULL);

    pb = XtVaCreateManagedWidget("Ok",
			xmPushButtonWidgetClass, rowcol, NULL);
    pb = XtVaCreateManagedWidget("Cancel",
			xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, doit, NULL);
    pb = XtVaCreateManagedWidget("Help",
			xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, doit, NULL);

    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  120,   31, 0,0,0, /* rowcolumn */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   24,   25, 0,0,0, /* Ok */
   CWWidth | CWHeight | CWX | CWY,   30,    3,   48,   25, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   81,    3,   36,   25, 0,0,0, /* Help */ 
    };
    PrintDetails(    toplevel ,Expected);
};
    LessTifTestMainLoop(toplevel );
    exit(0);
}
