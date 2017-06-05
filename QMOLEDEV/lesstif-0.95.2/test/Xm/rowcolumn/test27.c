/** test6 -- an option menu.
**/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <stdio.h>

extern int GlobalErrors;

typedef struct {
	Position x;
	Position y;
	Dimension width;
	Dimension height;
	unsigned char alignment;
	Dimension marginBottom;
	Dimension marginHeight;
	Dimension marginLeft;
	Dimension marginRight;
	Dimension marginTop;
	Dimension marginWidth;
} Expected_t, *ExpectedPtr;

static void
PrintDetails(Widget W, ExpectedPtr Ex)
{
unsigned char alignment;
Position x;
Position y;
Dimension width;
Dimension height;
Dimension marginBottom;
Dimension marginHeight;
Dimension marginLeft;
Dimension marginRight;
Dimension marginTop;
Dimension marginWidth;

	printf("PrintDetails(%s)\n", XtName(W));
	XtVaGetValues(W,
		XmNx, &x,
		XmNy, &y,
		XmNwidth, &width,
		XmNheight, &height,
		XmNalignment, &alignment,
		XmNmarginBottom, &marginBottom,
		XmNmarginHeight, &marginHeight,
		XmNmarginLeft, &marginLeft,
		XmNmarginRight, &marginRight,
		XmNmarginTop, &marginTop,
		XmNmarginWidth, &marginWidth,
		NULL);
	GlobalErrors += Ex->x == x ? 0 : 1;
	GlobalErrors += Ex->y == y ? 0 : 1;
	GlobalErrors += Ex->width == width ? 0 : 1;
	GlobalErrors += Ex->height == height ? 0 : 1;
	GlobalErrors += Ex->alignment == alignment ? 0 : 1;
	GlobalErrors += Ex->marginBottom == marginBottom ? 0 : 1;
	GlobalErrors += Ex->marginHeight == marginHeight ? 0 : 1;
	GlobalErrors += Ex->marginLeft == marginLeft ? 0 : 1;
	GlobalErrors += Ex->marginRight == marginRight ? 0 : 1;
	GlobalErrors += Ex->marginTop == marginTop ? 0 : 1;
	GlobalErrors += Ex->marginWidth == marginWidth ? 0 : 1;
	printf("\tx            %d (%d) %s\n", x, Ex->x, x == Ex->x ? "Good" : "Bad");
	printf("\ty            %d (%d) %s\n", y, Ex->y, y == Ex->y ? "Good" : "Bad");
	printf("\twidth        %d (%d) %s\n", width, Ex->width, width == Ex->width ? "Good" : "Bad");
	printf("\theight       %d (%d) %s\n", height, Ex->height, height == Ex->height ? "Good" : "Bad");
	printf("\talignment    %s (%s) %s\n", alignment == XmALIGNMENT_BEGINNING ? "XmALIGNMENT_BEGINNING" : 
	                         alignment == XmALIGNMENT_CENTER ? "XmALIGNMENT_CENTER" :
	                         alignment == XmALIGNMENT_END ? "XmALIGNMENT_END" : "UNKNOWN",
	                         Ex->alignment == XmALIGNMENT_BEGINNING ? "XmALIGNMENT_BEGINNING" : 
	                         Ex->alignment == XmALIGNMENT_CENTER ? "XmALIGNMENT_CENTER" :
	                         Ex->alignment == XmALIGNMENT_END ? "XmALIGNMENT_END" : "UNKNOWN",
	                         alignment == Ex->alignment ? "Good" : "Bad");
	printf("\tmarginBottom %d (%d) %s\n", marginBottom, Ex->marginBottom, marginBottom == Ex->marginBottom ? "Good" : "Bad");
	printf("\tmarginHeight %d (%d) %s\n", marginHeight, Ex->marginHeight, marginHeight == Ex->marginHeight ? "Good" : "Bad");
	printf("\tmarginLeft   %d (%d) %s\n", marginLeft, Ex->marginLeft, marginLeft == Ex->marginLeft ? "Good" : "Bad");
	printf("\tmarginRight  %d (%d) %s\n", marginRight, Ex->marginRight, marginRight == Ex->marginRight ? "Good" : "Bad");
	printf("\tmarginTop    %d (%d) %s\n", marginTop, Ex->marginTop, marginTop == Ex->marginTop ? "Good" : "Bad");
	printf("\tmarginWidth  %d (%d) %s\n", marginWidth, Ex->marginWidth, marginWidth == Ex->marginWidth ? "Good" : "Bad");
}

static void
_DoTest(Widget rc)
{
Widget Label;
Widget Button;
Position x;
Position y;
Dimension width;
Dimension height;
Expected_t EL = {
	3,
	3,
	46,
	29,
	XmALIGNMENT_CENTER,
	0,
	2,
	0,
	0,
	0,
	2
};
Expected_t EB = {
	52,
	3,
	79,
	29,
	XmALIGNMENT_CENTER,
	2,
	2,
	0,
	21,
	2,
	2
};

	Label = XmOptionLabelGadget(rc);
	Button = XmOptionButtonGadget(rc);
	printf("_DoTests(%s)\n", XtName(rc));
	XtVaGetValues(rc,
		XmNx, &x,
		XmNy, &y,
		XmNwidth, &width,
		XmNheight, &height,
		NULL);
#if 0
	printf("\tx            %d (0) %s\n", x, x == 0 ? "Good" : "Bad");
	printf("\ty            %d (0) %s\n", y, y == 0 ? "Good" : "Bad");
#endif
	printf("\twidth        %d (134) %s\n", width, width == 134 ? "Good" : "Bad");
	printf("\theight       %d (35) %s\n", height, height == 35 ? "Good" : "Bad");
	PrintDetails(Label, &EL);
	PrintDetails(Button, &EB);
}

static void
DoTests(Widget W, Widget rc, XConfigureEvent *event)
{
	if (event->type == MapNotify)
	{
		printf("DoTests(%s, %s)\n", XtName(W), XtName(rc));
		XtRemoveEventHandler(W, 
			StructureNotifyMask, 
			False, 
			(XtEventHandler)DoTests, 
			W);
		_DoTest(rc);
	}
}
int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget pane1;
    Widget button1, button2;
    Arg args[10];
    int n;

    toplevel = XtVaAppInitialize(&theApp, "rc-test28", NULL, 0,
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

    rc = XmCreateOptionMenu(toplevel,
			    "option",
			    args, n);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);
    XtAddEventHandler(toplevel, 
	StructureNotifyMask, 
	False, 
	(XtEventHandler)DoTests, 
	rc);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	300,	85,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	201,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	44,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	44,	96,	38,	0,0,0,
};

#if 0
  PrintDetails(toplevel,Expected);
#endif
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


