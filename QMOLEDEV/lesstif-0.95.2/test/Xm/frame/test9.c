/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test9.c,v 1.5 2002/05/01 15:39:21 amai Exp $
 *
 * SF [Bug #126733] XmFRAME_TITLE_CHILD label doesn't scale:
     I'm not much of an Motif expert, but I noticed a difference in behaviour
     of a XmFRAME_TITLE_CHILD label widget between IBM's AIX ( with
     Motif ) and Linux ( with Lesstif ofcourse...)
     If the label is changed using Xt(Va)SetValues the string doesn't display
     completely if the new labelstring is longer than the initial labelstring.
     On AIX it displays correctly ( or maybe I should say 'like expected' :-) )
 *
 * Compile with 'cc framelabel.c -lXm -lXt -lX11'
 *
 */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>

#include "../../common/Test.h"


void
changelabel(Widget w, XtPointer client_data, XtPointer call_data)
{
        XmString label;

        label = XmStringCreateSimple("More blah blah...");
        XtVaSetValues(client_data,
                      XmNlabelString, label,        
                      NULL);
}


int
main(int argc, char **argv)
{
	XtAppContext app_context;
	Widget toplevel, mainw, frame, framelabel, button;

	toplevel = XtVaAppInitialize(&app_context, "FrameLabel", 
                                       NULL, 0,
                                       &argc, argv, NULL,
                                       XmNallowShellResize, TRUE,
                                       NULL);

	mainw = XtVaCreateManagedWidget("main", xmFormWidgetClass,
                                       toplevel, NULL);

	frame = XtVaCreateManagedWidget(
                                      "frame", xmFrameWidgetClass,
                                      mainw, NULL);

	framelabel = XtVaCreateManagedWidget(
                                      "Blah", xmLabelWidgetClass,
                                      frame,
                                      XmNchildType, XmFRAME_TITLE_CHILD,
                                      NULL);

	button = XtVaCreateManagedWidget("Change the framelabel...", 
		         xmPushButtonWidgetClass,
		         frame,
		         NULL);
	XtAddCallback(button, XmNactivateCallback, changelabel,                   
                          framelabel);

	XtRealizeWidget(toplevel);
	{
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  160,   44, 0,0,0, /* main */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  160,   44, 0,0,0, /* frame */},
   {CWWidth | CWHeight | CWX | CWY,   12,    0,   28,   17, 0,0,0, /* Blah */},
   {CWWidth | CWHeight | CWX | CWY,    2,   17,  156,   25, 0,0,0, /* Change the framelabel... */},

   {CWWidth | CWHeight            ,  294,  650,  160,   44, 0,0,0, /* main */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  160,   44, 0,0,0, /* frame */},
   {CWWidth | CWHeight | CWX | CWY,   12,    0,  106,   17, 0,0,0, /* Blah */},
   {CWWidth | CWHeight | CWX | CWY,    2,   17,  156,   25, 0,0,0, /* Change the framelabel... */},
};
	PrintDetails(toplevel, Expected);
	LessTifTestWaitForIt(toplevel);
	LessTifTestPushButton(button);
	PrintDetails(toplevel, Expected);
	}
        LessTifTestMainLoop(toplevel);
	/*
	XtAppMainLoop(app_context);
	*/

      exit(0);
}
