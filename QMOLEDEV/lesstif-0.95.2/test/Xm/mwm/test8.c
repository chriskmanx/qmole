#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/AtomMgr.h>
#include <Xm/PushB.h>
#include <stdio.h>

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

Widget toplevel, one;
int state = 0;
void
cb(Widget w, XtPointer data, XtPointer cbs)
{
  switch (state) {
  case 0:
    XtVaSetValues(toplevel, XmNmwmFunctions,
		  MWM_FUNC_RESIZE,
		  NULL);
    state = 1;
    break;

  case 1:
    XtVaSetValues(toplevel, XmNmwmFunctions,
		  MWM_FUNC_RESIZE|MWM_FUNC_MOVE,
		  NULL);
    state = 2;
    break;
	
  case 2:
    XtVaSetValues(toplevel, XmNmwmFunctions,
		  MWM_FUNC_RESIZE|MWM_FUNC_MOVE|MWM_FUNC_MINIMIZE,
		  NULL);
    state = 3;
    break;

  case 3:
    XtVaSetValues(toplevel, XmNmwmFunctions,
		  MWM_FUNC_RESIZE|MWM_FUNC_MOVE|MWM_FUNC_MINIMIZE|MWM_FUNC_MAXIMIZE,
		  NULL);
    state = 4;
    break;

  case 4:
    XtVaSetValues(toplevel, XmNmwmFunctions,
		  MWM_FUNC_RESIZE|MWM_FUNC_MOVE|MWM_FUNC_MINIMIZE|MWM_FUNC_MAXIMIZE|MWM_FUNC_CLOSE,
		  NULL);
    state = 5;
    break;

  case 5:
    XtVaSetValues(toplevel, XmNmwmFunctions,
		  0,
		  NULL);
    state = 0;
    break;
  }
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nLabel", "MY_FONT");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, XmNfontList, fontlist, 
				XmNlabelString, xmstr1,
				NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtVaSetValues(toplevel, XmNmwmFunctions, 0, NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   51,   67,   57,   84, 0,0,0, /* One */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
