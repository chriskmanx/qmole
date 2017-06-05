/*
 $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test17.c,v 1.3 2001/06/15 09:30:51 amai Exp $
From:        Eric Howe <mu@clio.trends.ca>
To:          lesstif@hungry.com
Subject:     Etched Shadow Glitch
Date:        Sun, 5 Jul 1998 06:40:23 -0400 (EDT)
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>


static int shadow_thicks[] = { 1, 2, 3, 4, 5, 6, 7 };
#define	N_DEPTHS (sizeof(shadow_thicks)/sizeof(shadow_thicks[0]))

static struct {
	char *name;
	int  type;
} shadows[] = {
	{ "in",	        XmSHADOW_IN         },
	{ "out",        XmSHADOW_OUT        },
	{ "etched in",  XmSHADOW_ETCHED_IN  },
	{ "etched out", XmSHADOW_ETCHED_OUT },
};
#define	N_SHADOWS (sizeof(shadows)/sizeof(shadows[0]))

static void
dothings(Widget w, XtPointer closure, XtPointer call)
{
	Widget dlg, rc, frame;
	int    type = *((int *)closure);
	int    i;

	dlg = XmCreateDialogShell(w, "shell", NULL, 0);
	rc  = XtCreateWidget("blah", xmRowColumnWidgetClass, dlg, NULL, 0);
	for(i = 0; i < N_DEPTHS; ++i) {
		frame = XtVaCreateWidget("frame", xmFrameWidgetClass, rc,
					XmNshadowType,       type,
					XmNshadowThickness,  shadow_thicks[i],
					NULL);
		XtCreateManagedWidget("blah blah blah", xmLabelWidgetClass,
					frame, NULL, 0);
		XtManageChild(frame);
	}
	XtManageChild(rc);
}

int
main(int argc, char **argv)
{
	XtAppContext ac;
	Widget       top, rc;
	int          i;

	top = XtVaAppInitialize(&ac, "shadows", NULL, 0, &argc, argv,
				NULL, NULL);

	rc = XtVaCreateWidget("rc", xmRowColumnWidgetClass, top,
				XmNorientation, XmHORIZONTAL,
				NULL);
	for(i = 0; i < N_SHADOWS; ++i) {
		Widget w = XtVaCreateManagedWidget(shadows[i].name,
				xmPushButtonWidgetClass, rc, NULL);
		XtAddCallback(w, XmNactivateCallback,dothings,&shadows[i].type);
	}
	XtManageChild(rc);
	XtRealizeWidget(top);

	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  207,   31, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   24,   25, 0,0,0, /* in */
   CWWidth | CWHeight | CWX | CWY,   30,    3,   30,   25, 0,0,0, /* out */
   CWWidth | CWHeight | CWX | CWY,   63,    3,   66,   25, 0,0,0, /* etched in */
   CWWidth | CWHeight | CWX | CWY,  132,    3,   72,   25, 0,0,0, /* etched out */ 
    };
    PrintDetails(	top ,Expected);
};
   LessTifTestMainLoop(	top );
	return 0;
}
