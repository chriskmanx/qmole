#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, rc;
  Widget butt1, butt2, butt3;

  toplevel = XtVaAppInitialize(&theApp, "toggle1", NULL, 0,
			       &argc, argv, NULL, NULL);

  rc= XtVaCreateManagedWidget("A Simple Toggle Button", xmRowColumnWidgetClass,
	toplevel,
		XmNentryAlignment, XmALIGNMENT_CENTER,
		XmNorientation, XmHORIZONTAL,
		XmNpacking, XmPACK_COLUMN,
		XmNnumColumns, 2,
	NULL);

  butt1= XtVaCreateManagedWidget("On", xmToggleButtonWidgetClass, rc, 
#if XmVERSION > 1
		XmNtoggleMode,	XmTOGGLE_INDETERMINATE,
		XmNset,		XmSET,
#endif
	NULL);

  butt2= XtVaCreateManagedWidget("Off", xmToggleButtonWidgetClass, rc, 
#if XmVERSION > 1
		XmNtoggleMode,	XmTOGGLE_INDETERMINATE,
		XmNset,		XmUNSET,
#endif
	NULL);

  butt3= XtVaCreateManagedWidget("Dunno", xmToggleButtonWidgetClass, rc, 
#if XmVERSION > 1
		XmNtoggleMode,	XmTOGGLE_INDETERMINATE,
		XmNset,		XmINDETERMINATE,
#endif
	NULL);

  butt1= XtVaCreateManagedWidget("On", xmToggleButtonGadgetClass, rc, 
#if XmVERSION > 1
		XmNtoggleMode,	XmTOGGLE_INDETERMINATE,
		XmNset,		XmSET,
#endif
	NULL);

  butt2= XtVaCreateManagedWidget("Off", xmToggleButtonGadgetClass, rc, 
#if XmVERSION > 1
		XmNtoggleMode,	XmTOGGLE_INDETERMINATE,
		XmNset,		XmUNSET,
#endif
	NULL);

  butt3= XtVaCreateManagedWidget("Dunno", xmToggleButtonGadgetClass, rc, 
#if XmVERSION > 1
		XmNtoggleMode,	XmTOGGLE_INDETERMINATE,
		XmNset,		XmINDETERMINATE,
#endif
	NULL);

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  177,   59, 0,0,0, /* A Simple Toggle Button */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   55,   25, 0,0,0, /* On */
   CWWidth | CWHeight | CWX | CWY,   61,    3,   55,   25, 0,0,0, /* Off */
   CWWidth | CWHeight | CWX | CWY,  119,    3,   55,   25, 0,0,0, /* Dunno */
   CWWidth | CWHeight | CWX | CWY,    3,   31,   55,   25, 0,0,0, /* On */
   CWWidth | CWHeight | CWX | CWY,   61,   31,   55,   25, 0,0,0, /* Off */
   CWWidth | CWHeight | CWX | CWY,  119,   31,   55,   25, 0,0,0, /* Dunno */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  exit(0);
}
