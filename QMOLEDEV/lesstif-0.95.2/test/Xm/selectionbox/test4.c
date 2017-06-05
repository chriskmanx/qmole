#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, rc, button;

  void pushed(Widget, XtPointer, XtPointer);

  toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
			       &argc, argv, NULL, NULL);

  rc = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass, toplevel,
			NULL);

  button = XtVaCreateManagedWidget("PushMe-1", xmPushButtonWidgetClass, rc,
				   NULL);
  XtAddCallback(button, XmNactivateCallback, pushed, NULL);

  button = XtVaCreateManagedWidget("PushMe-2", xmPushButtonWidgetClass, rc,
				   NULL);
  XtAddCallback(button, XmNactivateCallback, pushed, NULL);

  XtManageChild(rc);
  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   66,   59, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   60,   25, 0,0,0, /* PushMe-1 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,   60,   25, 0,0,0, /* PushMe-2 */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

void
pushed(Widget pb, XtPointer foo, XtPointer bar)
{
  static Widget dialog;
  XmString t = XmStringCreateSimple("Enter New Button Name:");
  extern void read_name(Widget, XtPointer, XtPointer);
  extern void toggle_callback(Widget, XtPointer, XtPointer);
  Arg args[3];

  XtSetArg(args[0], XmNselectionLabelString, t);
  XtSetArg(args[1], XmNautoUnmanage, False);
  XtSetArg(args[2], XmNuserData, 0);
  dialog = XmCreatePromptDialog(XtParent(pb), "notice", args, 3);
  XmStringFree(t);

  XtAddCallback(dialog, XmNokCallback, read_name, NULL);
  XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)XtDestroyWidget,
		NULL);

  XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

  {
    XmString btn1 = XmStringCreateSimple("Change Name");
    XmString btn2 = XmStringCreateSimple("Change Color");
    Widget toggle_box =
      XmVaCreateSimpleRadioBox(dialog, "radio_box", 0, toggle_callback,
			       XmVaRADIOBUTTON, btn1, 0, NULL, NULL,
			       XmVaRADIOBUTTON, btn2, 0, NULL, NULL,
			       NULL);
    XtManageChild(toggle_box);
  }

  XtManageChild(dialog);
  XtPopup(XtParent(dialog), XtGrabNone);
}

void
toggle_callback(Widget toggle_box, XtPointer xtpn, XtPointer xtpcbs)
{
}

void
read_name(Widget dialog, XtPointer xtpn, XtPointer xtpcbs)
{
}
