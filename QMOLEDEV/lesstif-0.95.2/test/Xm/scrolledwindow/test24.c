/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test24.c,v 1.1 2005/01/16 11:03:17 dannybackx Exp $
 *
 * Add multiple children.
 * Try to figure out what's wrong with bug # 993209
 * */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmAll.h>

Widget toplevel, form;
Widget sw, hsb, vsb, da;
Widget	b1, b2, b3;

Widget	arr[5];

void Create(Widget w, XtPointer client, XtPointer call)
{
	int	i = (int)client;
	char	s[20];
	sprintf(s, "button %d", i);
	arr[i] = XtVaCreateWidget(s, xmPushButtonWidgetClass, sw,
			XmNx,	20,
			XmNy,	20 + 20 * i,
			NULL);
}

void Manage(Widget w, XtPointer client, XtPointer call)
{
	int	i = (int)client;
	XtManageChild(arr[i]);
}

void Unmanage(Widget w, XtPointer client, XtPointer call)
{
	int	i = (int)client;
	XtUnmanageChild(arr[i]);
}

int
main(int argc, char **argv)
{
	int	i;
	XtAppContext appc;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&appc, "test", NULL, 0, &argc, argv, NULL, NULL);

	form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
		  XmNwidth,	800,
		  XmNheight,	600,
		  NULL);

	sw  = XtVaCreateManagedWidget("sw", xmScrolledWindowWidgetClass, form,
		  XmNscrollingPolicy,		XmAUTOMATIC,
		  XmNleftPosition,		50,
		  XmNleftAttachment,		XmATTACH_POSITION,
		  XmNrightAttachment,		XmATTACH_FORM,
		  XmNtopAttachment,		XmATTACH_FORM,
		  XmNbottomAttachment,		XmATTACH_FORM,
		  NULL);

	for (i=0; i<5; i++) {
		b1 = XtVaCreateManagedWidget("create", xmPushButtonWidgetClass, form,
			XmNleftOffset,		10,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_SELF,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		10 + 30 * i,
			XmNbottomAttachment,	XmATTACH_SELF,
			NULL);
		XtAddCallback(b1, XmNactivateCallback, Create, (XtPointer)i);
		b2 = XtVaCreateManagedWidget("manage", xmPushButtonWidgetClass, form,
			XmNleftOffset,		10,
			XmNleftWidget,		b1,
			XmNleftAttachment,	XmATTACH_WIDGET,
			XmNrightAttachment,	XmATTACH_SELF,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		10 + 30 * i,
			XmNbottomAttachment,	XmATTACH_SELF,
			NULL);
		XtAddCallback(b2, XmNactivateCallback, Manage, (XtPointer)i);
		b3 = XtVaCreateManagedWidget("unmanage", xmPushButtonWidgetClass, form,
			XmNleftOffset,		10,
			XmNleftWidget,		b2,
			XmNleftAttachment,	XmATTACH_WIDGET,
			XmNrightAttachment,	XmATTACH_SELF,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		10 + 30 * i,
			XmNbottomAttachment,	XmATTACH_SELF,
			NULL);
		XtAddCallback(b3, XmNactivateCallback, Unmanage, (XtPointer)i);
	}

	XtRealizeWidget(toplevel);

	XtAppMainLoop(appc);
	exit(0);
}
