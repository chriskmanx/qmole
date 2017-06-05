/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test59.c,v 1.2 2001/01/23 14:09:01 amai Exp $ */
/*
From:        Pavel Roskin <pavel_roskin@geocities.com>
To:          lesstif@hungry.com
Subject:     Modelling problems with java.awt.Choice
Date:        Mon, 2 Aug 1999 11:00:39 +0400 (EEST)
*/

#include <stdio.h>
#include <stdlib.h>

#include <Xm/XmP.h>
#include <Xm/FormP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MainWP.h>


/*
 * Test for XmCreateOptionMenu
 */

int
main(int argc, char **argv)
{
    Widget toplevel, mainw;
    Widget one;
    Widget option;

    XtAppContext theApp;
    Arg args[6];
    int ac;
    Dimension mmt, mmb, mml, mmr, mmw, mmh, st, ht;

    toplevel = XtVaAppInitialize(&theApp, "mainW", NULL, 0,
				 &argc, argv, NULL, NULL);

    mainw = XmCreateMainWindow(toplevel, "MainWindow", args, 0);
    XtManageChild(mainw);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, mainw,
				  NULL);

    ac = 0;
    XtSetArg(args[ac], XmNlabelString, XmStringCreateLocalized("Guess:"));
    ac++;

    option = XmCreateOptionMenu(one, "option", args, ac);

    XtManageChild(option);

    XtVaGetValues(XmOptionButtonGadget(option),
		  XmNmarginTop, &mmt, XmNmarginBottom, &mmb,
		  XmNmarginLeft, &mml, XmNmarginRight, &mmr,
		  XmNmarginWidth, &mmw, XmNmarginHeight, &mmh,
		  XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		  NULL);
    printf("%d %d %d %d %d %d %d %d %d %d\n",
      mmt, mmb, mml, mmr, mmw, mmh, st, ht, XtWidth(option), XtHeight(option));

    XtRealizeWidget(toplevel);

    XtVaGetValues(XmOptionButtonGadget(option),
		  XmNmarginTop, &mmt, XmNmarginBottom, &mmb,
		  XmNmarginLeft, &mml, XmNmarginRight, &mmr,
		  XmNmarginWidth, &mmw, XmNmarginHeight, &mmh,
		  XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		  NULL);
    printf("%d %d %d %d %d %d %d %d %d %d\n",
      mmt, mmb, mml, mmr, mmw, mmh, st, ht, XtWidth(option), XtHeight(option));

    {
	static XtWidgetGeometry Expected[] =
	{
	    CWWidth | CWHeight, 0, 0, 133, 31, 0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 0, 0, 133, 31, 0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 0, 0, 133, 31, 0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 3, 3, 40, 25, 0, 0, 0,
	    CWWidth | CWHeight | CWX | CWY, 46, 3, 84, 25, 0, 0, 0,
	};

	PrintDetails(toplevel, Expected);
    }
    LessTifTestMainLoop(toplevel);
    /*
       XtAppMainLoop(theApp);
     */

    exit(0);
}
