#include <Xm/RowColumn.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget rowcol;

void
cb(Widget w, XtPointer data, XtPointer cbs)
{
   printf("cb called with %d\n", (int)data);
}

int
main(int argc, char **argv)
{
   Widget toplevel;
   XtAppContext app;
   Arg args[10];
   int n = 0;
   XmString buttons[3];

   XtSetLanguageProc(NULL, NULL, NULL);

   toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, 
				NULL, NULL);

  
   buttons[0] = XmStringCreateLocalized("button number 0");
   buttons[1] = XmStringCreateLocalized("button number 1");
   buttons[2] = XmStringCreateLocalized("button number 2");

   /* these resources are the "Simple" resources used to for
    * the XmCreateSimple* routines (but not XmVaCreateSimple*)
    */
   XtSetArg(args[n], XmNbuttons, buttons); n++; 
   XtSetArg(args[n], XmNbuttonCount, 3); n++; 
   XtSetArg(args[n], XmNsimpleCallback, cb); n++;
   XtSetArg(args[n], XmNbuttonSet, 1); n++; 
   rowcol = XmCreateSimpleRadioBox(toplevel, "radioBox", args, n);

   XmStringFree(buttons[0]);
   XmStringFree(buttons[1]);
   XmStringFree(buttons[2]);
   
   XtManageChild(rowcol);
   
   XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	121,	87,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	59,	115,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
   XtAppMainLoop(app);
   */
   
   exit(0);
}
