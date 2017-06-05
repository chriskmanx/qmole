/*
From:        Pavel Roskin <pavel_roskin@geocities.com>
To:          lesstif@hungry.com
Subject:     XmNresizeWidth is ignored by Lesstif
Date:        Mon, 2 Aug 1999 11:00:39 +0400 (EEST)
*/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget pane1;
    Widget button1, button2;
    Arg args[10];
    int n;

    toplevel = XtVaAppInitialize(&theApp, "MainWindow", NULL, 0,
				 &argc, argv, NULL, 
				 XmNwidth, 120,
				 NULL);

    n = 0;
    XtSetArg(args[n], XmNresizeWidth, False); n++;
    pane1 = XmCreatePulldownMenu(toplevel,
				 "pulldown",
				 args, n);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      pane1,
				      NULL);

    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      pane1,
				      NULL);

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pane1); n++;
    XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Option:")); n++;
    XtSetArg(args[n], XmNresizeWidth, False); n++;

    rc = XmCreateOptionMenu(toplevel,
			    "option",
			    args, n);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	96,	35,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	46,	29,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	52,	3,	41,	29,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


