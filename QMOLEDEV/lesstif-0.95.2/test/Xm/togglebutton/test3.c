#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
/*
#include <XmI/MacrosI.h>
*/
#include <Xm/LabelP.h>

#define TEST_CBS 1

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt;

  toplevel = XtVaAppInitialize(&theApp, "toggle1", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1", xmToggleButtonWidgetClass, toplevel, 
				XmNshadowThickness, 5,
				NULL);

  XtAddCallback(butt,XmNvalueChangedCallback,HiCB,NULL);

  printf("Margins: %d %d %d %d %d %d\n",
	 Lab_MarginWidth(butt), Lab_MarginHeight(butt),
	 Lab_MarginTop(butt), Lab_MarginBottom(butt),
	 Lab_MarginLeft(butt), Lab_MarginRight(butt));

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   77,   45, 0,0,0, /* Button1 */
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

#if TEST_CBS
    cbs->set = False;
#endif
}
