#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBP.h>
/*
#include <XmI/MacrosI.h>
*/

#define TEST_CBS 1

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);

Widget butt;

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;

  toplevel = XtVaAppInitialize(&theApp, "toggle1", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1Button1", xmToggleButtonWidgetClass, toplevel, 
				XmNindicatorOn, False,
				XmNshadowThickness, 5,
#if 0
				XmNfillOnSelect, True,
#endif
				NULL);

  XtAddCallback(butt,XmNvalueChangedCallback,HiCB,NULL);

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  102,   31, 0,0,0, /* Button1Button1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  exit(0);
}

void HiCB(Widget w,XtPointer client_data,XtPointer call_data)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call_data;

    printf("Toggle Me and I'm Yours: %d\n", cbs->set);

  printf("Margins: %d %d %d %d %d %d\n",
	 Lab_MarginWidth(butt), Lab_MarginHeight(butt),
	 Lab_MarginTop(butt), Lab_MarginBottom(butt),
	 Lab_MarginLeft(butt), Lab_MarginRight(butt));

#if TEST_CBS
    cbs->set = False;
#endif
}
