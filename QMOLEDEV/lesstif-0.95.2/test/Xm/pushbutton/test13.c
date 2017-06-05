/* $Header: /cvsroot/lesstif/lesstif/test/Xm/pushbutton/test13.c,v 1.4 2001/06/18 09:04:00 amai Exp $
 * What does XtVaGetValues(w, XmNlabelString, &x, NULL); return ?
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"

void
cb(Widget w, XtPointer data, XtPointer cbs)
{
    XmString	xms = NULL;
    char	*s = NULL;

    XtVaGetValues(w, XmNlabelString, &xms, NULL);
    if (xms == NULL) {
	fprintf(stderr, "XmString was NULL\n");
	return;
    }
    XmStringGetLtoR(xms, XmSTRING_DEFAULT_CHARSET, &s);
    if (s == NULL) {
	fprintf(stderr, "Conversion to String failed\n");
	return;
    }
    fprintf(stderr, "Label : %s\n", s);
    XmStringFree(xms);
    XtFree(s);
}

String fallback[] = {
	"*One.labelString:	this is a labelString on a widget",
	"*Two.labelString:	this is a labelString on a gadget",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, rc, w;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);
  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, fallback, NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel, NULL);

  w = XtVaCreateManagedWidget("PushButtonWidget", xmPushButtonWidgetClass, rc, NULL);
  XtAddCallback(w, XmNactivateCallback, cb, NULL);
  w = XtVaCreateManagedWidget("PushButtonGadget", xmPushButtonGadgetClass, rc, NULL);
  XtAddCallback(w, XmNactivateCallback, cb, NULL);
  w = XtVaCreateManagedWidget("One", xmPushButtonWidgetClass, rc, NULL);
  XtAddCallback(w, XmNactivateCallback, cb, NULL);
  w = XtVaCreateManagedWidget("Two", xmPushButtonGadgetClass, rc, NULL);
  XtAddCallback(w, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	216,	115,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	210,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	210,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	59,	210,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	87,	210,	25,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
