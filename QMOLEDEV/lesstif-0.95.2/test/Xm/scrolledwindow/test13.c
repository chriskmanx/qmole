/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test13.c,v 1.6 2001/06/18 14:22:06 amai Exp $
 * This test is an interactive interface to control the resources
 * of a bunch of widgets. These widgets are really stolen out of
 * test12.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/CascadeB.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledWP.h>

#ifdef LESSTIF_VERSION
#include <XmI/MacrosI.h>
#endif


Widget toplevel, sw, ab;
Dimension st;

/*
 * These are the ScrolledWindow resources that can be set by sliders.
 */
struct x {
	char	*name;
	int	initial, min, max;
} list [] = {
	{ XmNwidth,		100,	0,	400 },
	{ XmNheight,		100,	0,	400 },
	{ XmNspacing,		3,	0,	20 },
	{ XmNshadowThickness,	5,	0,	20 },
};

char *fallback[] = {
	"*sw.background:	green",
	"*sw.topShadowColor:	red",
	"*sw.bottomShadowColor:	blue",
	"*ab.background:	yellow",
	"*ab.foreground:	black",
	NULL
};

void cb(Widget wid, XtPointer cld, XtPointer cad)
{
Dimension w,h,bw;
static Boolean toggle=False;
	if ((toggle = !toggle))
	{
		w=300; h=300; bw = 1;
	}
	else
    {
		w=100; h=100; bw = 10;
	}
	printf("scrolledW child: asking for %ix%i (bw %i) /",w,h,bw);
	XtVaSetValues(wid,XmNwidth,w,XmNheight,h,XmNborderWidth,bw,NULL);
	XtVaGetValues(wid,XmNwidth,&w,XmNheight,&h,XmNborderWidth,&bw,NULL);
	printf("got %ix%i (bw %i)\n",w,h,bw);
}

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

void ChangePlacement(Widget w, XtPointer client, XtPointer call)
{
	XtVaSetValues(sw, XmNscrollBarPlacement, (int)client, NULL);
}

void Change(Widget w, XtPointer client, XtPointer call)
{
	int	i = (int)client;
	XmScaleCallbackStruct	*p = (XmScaleCallbackStruct *)call;

	XtVaSetValues(sw, list[i].name, p->value, NULL);
}

int main(int argc, char **argv)
{
  XtAppContext	app;
  Widget	top, mb, form, fc, fp, quit, s, om, mp, w;
  int		i;
  Arg		arg;
  XmString	xms;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0,
	&argc, argv, fallback, NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel, 
		XmNwidth,		500,
		XmNheight,		300,
		XmNresizePolicy,	XmRESIZE_NONE,
	NULL);

  mb  = XtVaCreateManagedWidget("mb", xmRowColumnWidgetClass, form, 
		XmNrowColumnType,	XmMENU_BAR,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);

  fp = XmCreatePulldownMenu(mb, "file pulldown", NULL, 0);
  fc = XtVaCreateManagedWidget("File", xmCascadeButtonWidgetClass, mb, 
		XmNsubMenuId,	fp,
	NULL);

  quit = XtVaCreateManagedWidget("Quit", xmPushButtonWidgetClass, fp,
	NULL);
  XtAddCallback(quit, XmNactivateCallback, Quit, NULL);

/* Panel of scales */
  top = mb;
  for (i=0; i<XtNumber(list); i++) {
	xms = XmStringCreateSimple(list[i].name);
	s = XtVaCreateManagedWidget(list[i].name, xmScaleWidgetClass, form,
		XmNorientation,		XmHORIZONTAL,
		XmNminimum,		list[i].min,
		XmNmaximum,		list[i].max,
		XmNvalue,		list[i].initial,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		top,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	25,
		XmNbottomAttachment,	XmATTACH_NONE,
		XmNtitleString,		xms,
		XmNshowValue,		True,
		NULL);
	XmStringFree(xms);
	top = s;
	XtAddCallback(s, XmNvalueChangedCallback, Change, (XtPointer)i);
  }

/* Option menu for placement */
  mp = XmCreatePulldownMenu(form, "option pane", NULL, 0);

  xms = XmStringCreateSimple("Placement");
  XtSetArg(arg, XmNlabelString, xms);
  om = XmCreateOptionMenu(form, "option", &arg, 1);
  XmStringFree(xms);

  XtVaSetValues(om,
  		XmNsubMenuId,		mp,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		top,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	25,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);
  XtManageChild(om);

  w = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, mp,
		XtVaTypedArg, XmNlabelString, XmRString, "TOP_RIGHT", 10,
	NULL);
  XtAddCallback(w, XmNactivateCallback,
	ChangePlacement, (XtPointer)XmTOP_RIGHT);
  w = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, mp,
		XtVaTypedArg, XmNlabelString, XmRString, "BOTTOM_RIGHT", 12,
	NULL);
  XtAddCallback(w, XmNactivateCallback,
	ChangePlacement, (XtPointer)XmBOTTOM_RIGHT);

  XtVaSetValues(om, XmNmenuHistory, w, NULL);

  w = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, mp,
		XtVaTypedArg, XmNlabelString, XmRString, "BOTTOM_LEFT", 11,
	NULL);
  XtAddCallback(w, XmNactivateCallback,
	ChangePlacement, (XtPointer)XmBOTTOM_LEFT);
  w = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, mp,
		XtVaTypedArg, XmNlabelString, XmRString, "TOP_LEFT", 9,
	NULL);
  XtAddCallback(w, XmNactivateCallback,
	ChangePlacement, (XtPointer)XmTOP_LEFT);

/* Create the SW */
  sw  = XtVaCreateManagedWidget("sw", xmScrolledWindowWidgetClass, form, 
		XmNscrollingPolicy,	XmAUTOMATIC,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		mb,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	25,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNbottomAttachment,	XmATTACH_NONE,
		XmNwidth,		100,
		XmNheight,		100,
	NULL);

  ab = XtVaCreateManagedWidget("ab", xmArrowButtonWidgetClass, sw,
		XmNwidth,	300,
		XmNheight,	300,
	NULL);
  XtAddCallback(ab,XmNactivateCallback,cb,0);

  XtRealizeWidget(toplevel);


  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  500,  300, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  500,   31, 0,0,0, /* mb */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  125,   56, 0,0,0, /* width */
   CWWidth | CWHeight | CWX | CWY,    0,   35,   34,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  125,   19, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,   87,  125,   56, 0,0,0, /* height */
   CWWidth | CWHeight | CWX | CWY,    0,   35,   40,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  125,   19, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,  143,  125,   56, 0,0,0, /* spacing */
   CWWidth | CWHeight | CWX | CWY,    0,   35,   46,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  125,   19, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,  199,  125,   56, 0,0,0, /* shadowThickness */
   CWWidth | CWHeight | CWX | CWY,    0,   35,   94,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  125,   19, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,  255,  125,   31, 0,0,0, /* option */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   58,   25, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   64,    3,  109,   25, 0,0,0, /* OptionButton */
   CWWidth | CWHeight | CWX | CWY,  125,   31,  100,  100, 0,0,0, /* sw */
   CWWidth | CWHeight | CWX | CWY,    4,    4,   69,   69, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  300,  300, 0,0,0, /* ab */
   CWWidth | CWHeight | CWX | CWY,   81,    0,   19,   77, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,   81,   77,   19, 0,0,0, /* HorScrollBar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
