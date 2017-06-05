#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/Form.h>

void HiCB (Widget w, XtPointer client_data, XtPointer call_data);

Widget toplevel;

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget butt1;

  toplevel = XtVaAppInitialize (&theApp, "LessTifTest", NULL, 0,
				&argc, argv, NULL, NULL);

  butt1 = XtVaCreateManagedWidget ("Button1", xmPushButtonWidgetClass, toplevel,
				   NULL, 0);

  XtAddCallback (butt1, XmNactivateCallback, HiCB, NULL);

  XtRealizeWidget (toplevel);

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(butt1);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  507,  323,   54,   25, 0,0,0, /* Button1 */
   CWWidth | CWHeight            ,  490,  308,  100,  100, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   10,   15,   80,   25, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   10,   60,   80,   25, 0,0,0, /* Cancel */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit (0);
}

void 
HiCB (Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget Shell, butt1, butt2;
	Widget rc;
        XmString xmstr = XmStringCreateSimple("A Dialog");
	printf ("HiCB\n");

	Shell = XtVaCreateWidget ("Shell1", xmDialogShellWidgetClass, toplevel,
			XmNwidth, 100,
			XmNheight, 40,
		NULL);
	rc = XtVaCreateWidget ("OK", xmFormWidgetClass, Shell,
			XmNentryAlignment,	XmALIGNMENT_CENTER, 
			XmNorientation,		XmHORIZONTAL, 
			XmNpacking,		XmPACK_COLUMN, 
			XmNnumColumns,		2,
			XmNwidth,		100,
			XmNheight,		100,
		NULL);

	butt1 = XtVaCreateManagedWidget ("OK", xmPushButtonWidgetClass, rc,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		10,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		10,
			XmNbottomAttachment,	XmATTACH_POSITION,
			XmNbottomPosition,	40,
		NULL);
	butt2 = XtVaCreateManagedWidget ("Cancel", xmPushButtonWidgetClass, rc,
			XmNtopAttachment,	XmATTACH_WIDGET,
			XmNtopWidget,		butt1,
			XmNtopOffset,		20,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		10,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		10,
		NULL);
	XtManageChild (rc);


	XtVaSetValues(rc, XmNdialogTitle, xmstr, NULL);
}
