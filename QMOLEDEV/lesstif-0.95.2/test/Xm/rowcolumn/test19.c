/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test19.c,v 1.6 2001/05/15 14:46:10 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>

Arg     args[20];
Cardinal        argcount;

void VC(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "ValueChanged %s\n", XtName(w));
}

int
main(int argc, char **argv)
{
        XtAppContext    appc;
        Widget          top, rc, t1, t2, t3;

        top = XtAppInitialize(&appc, "test", NULL, 0, &argc, argv, NULL, NULL, 0);

        argcount = 0;
        XtSetArg(args[argcount], XmNradioBehavior, True); argcount++;
        rc = XmCreateRowColumn(top, "rowc", args, argcount);
        XtManageChild(rc);

        argcount = 0;
        t1 = XmCreateToggleButton(rc, "t_b_1", args, argcount);
        XtManageChild(t1);

        argcount = 0;
        t2 = XmCreateToggleButton(rc, "Toggle_button_2", args, argcount);
        XtManageChild(t2);

        argcount = 0;
        t3 = XmCreateToggleButton(rc, "Third_toggle_button", args, argcount);
        XtManageChild(t3);

	XtAddCallback(t1, XmNvalueChangedCallback, VC, NULL);
	XtAddCallback(t2, XmNvalueChangedCallback, VC, NULL);
	XtAddCallback(t3, XmNvalueChangedCallback, VC, NULL);

        XtRealizeWidget(top);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	145,	87,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	139,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	139,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	59,	139,	25,	0,0,0,
};

  PrintDetails(top, Expected);
  }
    LessTifTestMainLoop(top);
    /*
        XtAppMainLoop(appc);
        */
	exit(0);
}

