/** test6 -- an option menu.
**/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <stdio.h>

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
    Widget toplevel, rc;
    Widget pane1;
    Widget button1, button2;
    Arg args[30];
    int n;

    toplevel = XtVaAppInitialize(&theApp, "rc-test7", NULL, 0,
				 &argc, argv, NULL, NULL);

    pane1 = XmCreatePulldownMenu(toplevel,
				 "pulldown",
				 NULL, 0);

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
    rc = XmCreateOptionMenu(toplevel,
			    "option",
			    args, n);

    XtAddCallback(button1, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button2, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button1, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button2, XmNarmCallback, pb_arm_callback, NULL);

    XtManageChild(rc);

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


