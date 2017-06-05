/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test14.c,v 1.6 2001/06/18 14:12:37 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

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
  Dimension st, mh, space, ht;
  XmString button1 = XmStringCreateLocalized("button number 1");
  XmString button2 = XmStringCreateLocalized("button number 2");
  XmString button3 = XmStringCreateLocalized("button number 3");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  rowcol = XmVaCreateSimpleMenuBar(toplevel, "radioBox",
                                   XmVaCASCADEBUTTON, button1, 'b',
                                   XmVaCASCADEBUTTON, button2, 'u',
                                   XmVaCASCADEBUTTON, button3, 't',
				   XmNbuttonCount, 8,
                                   NULL);

  XmVaCreateSimplePulldownMenu(rowcol, "b_menu", 0, cb,
                               XmVaPUSHBUTTON, button1, 'b', NULL, NULL,
                               NULL);

  XmStringFree(button1);
  XmStringFree(button2);
  XmStringFree(button3);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);

  XtVaGetValues(rowcol, XmNshadowThickness, &st, XmNmarginHeight, &mh,
		XmNspacing, &space, XmNheight, &ht, NULL);
  printf("ST: %d MH: %d SPACE: %d HT: %d\n", st, mh, space, ht);
  XtVaGetValues(XtNameToWidget(rowcol, "button_0"), XmNheight, &ht,
		NULL);
  printf("BHT: %d\n", ht);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	328,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	5,	5,	106,	21,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	111,	5,	106,	21,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	217,	5,	106,	21,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop(app);
  */

  exit(0);
}
