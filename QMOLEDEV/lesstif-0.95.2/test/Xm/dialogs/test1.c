#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>

void HiCB (Widget w, XtPointer client_data, XtPointer call_data);

Widget toplevel;
Widget Shell;

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget butt1;

  toplevel = XtVaAppInitialize (&theApp, "drawingArea", NULL, 0,
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
   CWWidth | CWHeight            ,  506,  310,   69,   96, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   24,   25, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   10,   60,   48,   25, 0,0,0, /* Cancel */
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
  Widget butt2;
  Widget rc;
  printf ("HiCB\n");

  Shell = XtVaCreateWidget ("Shell1", xmDialogShellWidgetClass,
			    toplevel,
#if 1
			    XmNwidth, 100,
			    XmNheight, 40,
#endif
			    NULL,0);
#ifdef	notdef
  rc = XtVaCreateManagedWidget ("OK", xmRowColumnWidgetClass, Shell,
				   NULL);
#else
  rc = XtVaCreateWidget ("OK", xmBulletinBoardWidgetClass, Shell,
                                   XmNentryAlignment, XmALIGNMENT_CENTER, 
                                   XmNorientation, XmHORIZONTAL, 
                                   XmNpacking, XmPACK_COLUMN, 
                                   XmNnumColumns, 2,
				   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
				   NULL);
#endif
  butt2 = XtVaCreateManagedWidget ("OK", xmPushButtonWidgetClass, rc,
			XmNx,	10,
			XmNy,	10,
		   NULL, 0);
  butt2 = XtVaCreateManagedWidget ("Cancel", xmPushButtonWidgetClass, rc,
			XmNx,	10,
			XmNy,	60,
		   NULL, 0);
  XtManageChild (rc);

}
