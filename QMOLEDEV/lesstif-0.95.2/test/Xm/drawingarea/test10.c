/* test for margin width's and height's -- DEFECT 61 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>


void
InputCallback(Widget w)
{
    printf("InputCallback(%s)\n", XtName(w));
}

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button, other_button, form;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  form = XmCreateForm(toplevel, "form", NULL, 0);
  button = XmCreatePushButton(form, "button", NULL, 0);
  XtVaSetValues(button,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	NULL);
    XtManageChild(button);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       form,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, button,
	                               NULL);
  XtAddCallback(drawingArea,
		XmNinputCallback,
		(XtCallbackProc) InputCallback,
		NULL);

  button = XtVaCreateManagedWidget("button",
                                   xmPushButtonWidgetClass,
                                   drawingArea,
                                   XmNtraversalOn, False,
                                   NULL);

  other_button = XtVaCreateManagedWidget("button2",
                                         xmPushButtonWidgetClass,
                                         drawingArea,
                                         XmNx, 100,
                                         XmNy, 100,
                                   XmNtraversalOn, False,
                                         NULL);

  XtManageChild(form);
  XtRealizeWidget(toplevel);
  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  0,  0,  164,  160, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,   0,   0,   48,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,  0,  25,   164,   135, 0,0,0, /* drawingArea */
   CWWidth | CWHeight | CWX | CWY,  10,  10,   48,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,  100,  100,   54,   25, 0,0,0, /* button2 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
