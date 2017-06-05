#include <Xm/RowColumn.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>

Widget rowcol;

void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  XmString button1 = XmStringCreateLocalized("button number 1");
  XmString button2 = XmStringCreateLocalized("button number 2");
  XmString button3 = XmStringCreateLocalized("button number 3");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  /* the XmVaCreateSimple routines don't use the RowColumn's "Simple"
   * resources  --  I'm suprised it's not an error to do so, instead
   * they are ignored.  This is poorly documented.
   */
  rowcol = XmVaCreateSimpleRadioBox(toplevel, "radioBox", 1, cb,
                XmVaCHECKBUTTON, button1, NULL, NULL, NULL,
                XmVaRADIOBUTTON, button1, NULL, NULL, NULL,
                XmVaRADIOBUTTON, button2, NULL, NULL, NULL, 
                XmVaRADIOBUTTON, button3, NULL, NULL, NULL,
                XmNspacing, 20,
		XmNbuttonCount, 10,       /* ignored */
                NULL);
  XmStringFree(button1);
  XmStringFree(button2);
  XmStringFree(button3);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	121,	166,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	48,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	93,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	138,	115,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop(app);
  */

  exit(0);
}
