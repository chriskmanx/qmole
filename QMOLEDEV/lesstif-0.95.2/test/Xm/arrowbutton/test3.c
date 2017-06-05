/* $Header: /cvsroot/lesstif/lesstif/test/Xm/arrowbutton/test3.c,v 1.4 2001/08/28 14:45:46 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/ArrowB.h>

void HiCB(Widget w,XtPointer client_data,XmArrowButtonCallbackStruct *call_data);


int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt1;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt1= XtVaCreateManagedWidget("Button1", xmArrowButtonWidgetClass, toplevel, 
				XmNwidth, 50,
				XmNheight, 50,
				XmNtranslations, XtParseTranslationTable("#augment <Key>a: ArmAndActivate()"),
				NULL);

  XtAddCallback(butt1,XmNactivateCallback,(void *)HiCB,NULL);
  XtAddCallback(butt1,XmNarmCallback,(void *)HiCB,NULL);
  XtAddCallback(butt1,XmNdisarmCallback,(void *)HiCB,NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   50,   50, 0,0,0, /* Button1 */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

void HiCB(Widget w,XtPointer client_data,XmArrowButtonCallbackStruct *call_data)
{
	printf("HiCB ");
	switch(call_data->reason)
	{
	case XmCR_ACTIVATE:
		printf("Activate");
		break;
	case XmCR_ARM:
		printf("Arm     ");
		break;
	case XmCR_DISARM:
		printf("Disarm  ");
		break;
	default:
		printf("Unknown ");
		break;
	}
	if (call_data->event)
	{
		printf(" %i",call_data->event->xany.serial);
	}
	else
	{
		printf(" no event");
	}
	printf("\n");
}
