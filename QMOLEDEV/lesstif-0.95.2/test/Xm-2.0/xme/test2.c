/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/xme/test2.c,v 1.2 2002/05/14 11:43:19 amai Exp $ */
/* test XmeCreateClassDialog() */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/DrawP.h>
#include <Xm/Label.h>

#include "../../common/Test.h"

int main(int argc, char *argv[])
{
	Widget          toplevel, w;
	XtAppContext    app;
	XmString xms;
	Arg al[10];
	int ac;

	
	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "xmetest", NULL, 0, &argc, argv, NULL, NULL);

	xms = XmStringCreateSimple("Some sample string");
	ac=0;
	XtSetArg(al[ac], XmNlabelString, xms); ac++;
  
	w=XmeCreateClassDialog(xmLabelWidgetClass, toplevel, "testw",
	                       al, ac);
	                       
	XmStringFree(xms);

	XtRealizeWidget(toplevel);
	XtManageChild(w);

	LessTifTestMainLoop(toplevel);
	
	exit(0);
}
