/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test65.c,v 1.1 2004/10/12 22:34:02 dannybackx Exp $
 */
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <stdio.h>

Widget	tf, b1, b2;

void SetWidth(Widget w, XtPointer client, XtPointer call)
{
	char	*s, *p;
	int	n, i;
	XmString	xms;

	s = XmTextFieldGetString(tf);
	fprintf(stderr, "TF [%s]\n", s);
	n = atoi(s);
	XtFree(s);

	p = XtMalloc(n+1);
	for (i=0; i<n; i++)
		p[i] = 'a' + i;
	p[n] = '\0';
	xms = XmStringCreateSimple(p);
	XtFree(p);

	XtVaSetValues(b1, XmNlabelString, xms, NULL);
	XtVaSetValues(b2, XmNlabelString, xms, NULL);
	XmStringFree(xms);
}

void pb_activate_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("* Widget = %s - Activated\n", XtName(w));
}

void pb_arm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("* Widget = %s - Armed\n", XtName(w));
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc, bb, pb;
    Widget pane1;
    Arg args[30];
    int n;

    toplevel = XtVaAppInitialize(&theApp, "rc-test7", NULL, 0,
				 &argc, argv, NULL, NULL);

    n = 0;
    XtSetArg(args[n], XmNwidth, 300); n++;
    XtSetArg(args[n], XmNheight, 200); n++;
    bb = XmCreateBulletinBoard(toplevel, "bb", args, n);
    XtManageChild(bb);
    pane1 = XmCreatePulldownMenu(bb, "pulldown", NULL, 0);

    b1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      pane1,
				      NULL);

    b2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      pane1,
				      NULL);

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pane1); n++;
    XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Option:")); n++;

    XtSetArg(args[n], XmNx, 0); n++;
    XtSetArg(args[n], XmNy, 0); n++;
    XtSetArg(args[n], XmNmarginHeight, 0); n++;
    XtSetArg(args[n], XmNmarginWidth, 0); n++;
    XtSetArg(args[n], XmNrecomputeSize, False); n++;
    XtSetArg(args[n], XmNresizeHeight, False); n++;

    XtSetArg(args[n], XmNresizeWidth, False); n++;
    XtSetArg(args[n], XmNspacing, False); n++;
    XtSetArg(args[n], XmNborderWidth, 0); n++;
    XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
    XtSetArg(args[n], XmNtraversalOn, True); n++;
    XtSetArg(args[n], XmNadjustMargin, False); n++;
    XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
/*
    XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;

/*
    XtSetArg(args[argc], XmNbackground, bg); argc++;
    XtSetArg(args[argc], XmNforeground, fg); argc++;
    XtSetArg(args[argc], XmNsubMenuId, odata->menu); argc++;
    XtSetArg(args[argc], XmNuserData, (XtPointer)this); argc++;
 */
    rc = XmCreateOptionMenu(bb, "option", args, n);

    XtAddCallback(b1, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(b2, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(b1, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(b2, XmNarmCallback, pb_arm_callback, NULL);

    XtManageChild(rc);

    n = 0;
    XtSetArg(args[n], XmNx, 50); n++;
    XtSetArg(args[n], XmNy, 80); n++;
    tf = XmCreateTextField(bb, "tf", args, n);
    XtManageChild(tf);

    n = 0;
    XtSetArg(args[n], XmNx, 200); n++;
    XtSetArg(args[n], XmNy, 80); n++;
    pb = XmCreatePushButtonGadget(bb, "pb", args, n);
    XtManageChild(pb);
    XtAddCallback(pb, XmNactivateCallback, SetWidth, NULL);

    XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   77,   44, 0,0,0, /* option */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   77,   17, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,    0,   17,   77,   27, 0,0,0, /* OptionButton */ 
    };
    LessTifTestSetSlop(toplevel, 2);
    PrintDetails(toplevel,Expected);
};
    LessTifTestMainLoop(toplevel);
    exit(0);
}
