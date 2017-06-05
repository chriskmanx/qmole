/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test7.c,v 1.5 2001/05/16 13:10:19 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Label.h> 
#include <Xm/PushB.h> 
#include <Xm/Form.h> 
#include <Xm/Text.h> 
#include <Xm/TextF.h> 

int	left, right;
Widget toplevel, form, lt, rt, ll, rl, pb, txt;
XtAppContext app;

void Doit(Widget w, XtPointer client, XtPointer call)
{
	left = atoi(XmTextFieldGetString(lt));
	right = atoi(XmTextFieldGetString(rt));

	fprintf(stderr, "Selecting from %d to %d ... \n", left, right);

/*
	XmTextSetSelection(txt, left, right, XmHIGHLIGHT_SELECTED);
 */
	XmTextSetSelection(txt, left, right, XmHIGHLIGHT_SECONDARY_SELECTED);
}

char *fallback[] = {
	"*blinkRate:	0",
	"*left label.labelString:	Left",
	"*left label.marginTop:		7",
	"*right label.labelString:	Right",
	"*right label.marginTop:		7",
	"*push.labelString:		Select",
	"*geometrySlop: 1",
	NULL
};

int
main(int argc, char **argv)
{
  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv, fallback, NULL);

  form = XtVaCreateManagedWidget("one", xmFormWidgetClass, toplevel, 
		NULL); 

  ll = XtVaCreateManagedWidget("left label", xmLabelWidgetClass, form,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	33,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);

  lt = XtVaCreateManagedWidget("left", xmTextFieldWidgetClass, form, 
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		ll,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);

  rl = XtVaCreateManagedWidget("right label", xmLabelWidgetClass, form,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	33,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		lt,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);
  rt = XtVaCreateManagedWidget("right", xmTextFieldWidgetClass, form, 
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		rl,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		lt,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);

  pb = XtVaCreateManagedWidget("push", xmPushButtonWidgetClass, form,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		rt,
		XmNbottomAttachment,	XmATTACH_NONE,
	NULL);
  XtAddCallback(pb, XmNactivateCallback, Doit, NULL);

  txt = XtVaCreateManagedWidget("text", xmTextWidgetClass, form, 
		XmNvalue,		"This is a simple text\n"
					"But it's kinda hard to\n"
					"make this fucker work right\n\n",
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		pb,
		XmNbottomAttachment,	XmATTACH_FORM,
		XmNrows,		10,
		XmNeditMode,		XmMULTI_LINE_EDIT,
	NULL); 

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  172,  235, 0,0,0, /* one */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   57,   24, 0,0,0, /* left label */
   CWWidth | CWHeight | CWX | CWY,   57,    0,  115,   31, 0,0,0, /* left */
   CWWidth | CWHeight | CWX | CWY,    0,   31,   57,   24, 0,0,0, /* right label */
   CWWidth | CWHeight | CWX | CWY,   57,   31,  115,   31, 0,0,0, /* right */
   CWWidth | CWHeight | CWX | CWY,    0,   62,  172,   25, 0,0,0, /* push */
   CWWidth | CWHeight | CWX | CWY,    0,   87,  172,  148, 0,0,0, /* text */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
