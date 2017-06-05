/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test9.c,v 1.6 2001/05/15 14:46:10 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/RowColumn.h>
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
  XmString button1;
  XmString button2;
  XmString button3;
  unsigned char pack;
  short cols;

  XtSetLanguageProc(NULL, NULL, NULL);

  button1 = XmStringCreateLocalized("button number 1");
  button2 = XmStringCreateLocalized("button number 2");
  button3 = XmStringCreateLocalized("button number 3");

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, 
			       &argc, argv, NULL, NULL);

  rowcol = XmVaCreateSimpleCheckBox(toplevel, "checkBox", cb,
				    XmVaCHECKBUTTON, button1, NULL, NULL, NULL,
				    XmVaCHECKBUTTON, button2, NULL, NULL, NULL,
				    XmVaCHECKBUTTON, button3, NULL, NULL, NULL,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmNbuttonCount, 8,	/* unused for Va */
				    XmNbuttonSet, 2,    /* unused for Va */
				    NULL);
  XtVaGetValues(rowcol, XmNpacking, &pack, XmNnumColumns, &cols, NULL);
  printf("pack: %d numCols: %d\n", pack, cols);

  XmStringFree(button1);
  XmStringFree(button2);
  XmStringFree(button3);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	121,	87,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	4,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	115,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	58,	115,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop(app);
  */

  exit(0);
}
