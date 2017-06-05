/* $Header: /cvsroot/lesstif/lesstif/test/Xm/mainw/test11.c,v 1.8 2002/05/03 12:03:41 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>

#include "../../common/Test.h"

#define LABEL_SIZE 100

static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

int
main(int argc, char *argv[])
{
	XtAppContext app;
	Widget top;
	Widget mainw;
	Widget buttons;
	Widget quitbutton;
	Widget nextbutton;
	Widget prevbutton;
	Widget basebutton;
	Widget spacerlabel;
	Widget timingbutton;
	Widget draw;
	Widget hscroll;
	Widget vscroll;
	Widget status;
	Widget infoframe;
	Widget csrframe;
	Widget mkrframe;
	Widget difframe;
	Widget infolabel;
	Widget csrlabel;
	Widget mkrlabel;
	Widget diflabel;

	/*initialise the toolkit */

	top = XtVaAppInitialize(&app, "VCD", NULL, 0, &argc, argv, FallBack, NULL);






	mainw = XtVaCreateManagedWidget("mainw", xmMainWindowWidgetClass, top,

				       XmNheight, 400,

				       XmNwidth, 600,


				       NULL);



	buttons = XtVaCreateManagedWidget("buttons", xmRowColumnWidgetClass, mainw,

					  XmNorientation, XmHORIZONTAL,


					  XmNheight, LABEL_SIZE / 2,

					  NULL);


	/*create the buttons to go in the 'buttons' widget       */

	quitbutton = XtVaCreateManagedWidget("Quit", xmPushButtonWidgetClass, buttons,

					     NULL);



	nextbutton = XtVaCreateManagedWidget("NextEdge", xmPushButtonWidgetClass, buttons,

					     NULL);



	prevbutton = XtVaCreateManagedWidget("Prev Edge", xmPushButtonWidgetClass, buttons,

					     NULL);



	basebutton = XtVaCreateManagedWidget("Base->Marker", xmPushButtonWidgetClass, buttons,

					     NULL);



	spacerlabel = XtVaCreateManagedWidget("spacerlabel", xmLabelWidgetClass, buttons,

					      NULL);



	timingbutton = XtVaCreateManagedWidget("Timing Spread", xmPushButtonWidgetClass, buttons,

					       NULL);


	/*work area widget */

	draw = XtVaCreateManagedWidget("drawing area", xmDrawingAreaWidgetClass, mainw,

				       NULL);



	/*create some scrollbars */

	hscroll = XtVaCreateManagedWidget("hscroll", xmScrollBarWidgetClass, mainw,

					  XmNorientation, XmHORIZONTAL,

					  XmNmaximum, 10000,

					  XmNminimum, 0,

					  XmNsliderSize, 1000,


					  XmNpageIncrement, 1,

					  XmNincrement, 1,

					  NULL);




	vscroll = XtVaCreateManagedWidget("vscroll", xmScrollBarWidgetClass, mainw,


					  XmNorientation, XmVERTICAL,

					  XmNmaximum, 10000,


					  XmNminimum, 0,

					  XmNsliderSize, 1000,

					  XmNincrement, 1,


					  XmNpageIncrement, 1,

					  NULL);


	/*container for info boxes
	  note: each label widget is contained within it's own frame
	  widget (just to look nice) */

	status = XtVaCreateManagedWidget("status",
					 xmFormWidgetClass, mainw,

					 XmNorientation, XmHORIZONTAL,


/*
   XmNheight,LABEL_SIZE,
 */

					 NULL);



	infoframe = XtVaCreateManagedWidget("infoframe", xmFrameWidgetClass, status,

					  XmNleftAttachment, XmATTACH_POSITION,

					    XmNleftPosition, 1,


					    XmNtopAttachment, XmATTACH_POSITION,

					    XmNtopPosition, 2,


					XmNbottomAttachment, XmATTACH_POSITION,

					    XmNbottomPosition, 48,


					 XmNrightAttachment, XmATTACH_POSITION,

					    XmNrightPosition, 99,


					    NULL);



	csrframe = XtVaCreateManagedWidget("csrframe", xmFrameWidgetClass, status,

					   XmNleftAttachment, XmATTACH_POSITION,


					   XmNleftPosition, 1,

					   XmNtopAttachment, XmATTACH_POSITION,


					   XmNtopPosition, 52,

					XmNbottomAttachment, XmATTACH_POSITION,


					   XmNbottomPosition, 98,

					 XmNrightAttachment, XmATTACH_POSITION,


					   XmNrightPosition, 33,

					   NULL);



	mkrframe = XtVaCreateManagedWidget("mkrframe", xmFrameWidgetClass, status,

					   XmNleftAttachment, XmATTACH_POSITION,

					   XmNleftPosition, 34,

					   XmNtopAttachment, XmATTACH_POSITION,

					   XmNtopPosition, 52,


					XmNbottomAttachment, XmATTACH_POSITION,

					   XmNbottomPosition, 98,


					 XmNrightAttachment, XmATTACH_POSITION,

					   XmNrightPosition, 66,


					   NULL);



	difframe = XtVaCreateManagedWidget("difframe", xmFrameWidgetClass, status,

					   XmNleftAttachment, XmATTACH_POSITION,


					   XmNleftPosition, 67,

					   XmNtopAttachment, XmATTACH_POSITION,


					   XmNtopPosition, 52,

					XmNbottomAttachment, XmATTACH_POSITION,


					   XmNbottomPosition, 98,

					 XmNrightAttachment, XmATTACH_POSITION,


					   XmNrightPosition, 99,

					   NULL);



	infolabel = XtVaCreateManagedWidget("infolabel", xmLabelWidgetClass, infoframe,


					    XmNalignment, XmALIGNMENT_BEGINNING,


					    XmNisAligned, TRUE,

					    NULL);



	csrlabel = XtVaCreateManagedWidget("csrlabel", xmLabelWidgetClass, csrframe,


					   XmNalignment, XmALIGNMENT_BEGINNING,


					   XmNisAligned, TRUE,

					   NULL);





	mkrlabel = XtVaCreateManagedWidget("mkrlabel", xmLabelWidgetClass, mkrframe,


					   XmNalignment, XmALIGNMENT_BEGINNING,


					   XmNisAligned, TRUE,

					   NULL);



	diflabel = XtVaCreateManagedWidget("diflabel", xmLabelWidgetClass, difframe,


					   XmNalignment, XmALIGNMENT_BEGINNING,


					   XmNisAligned, TRUE,

					   NULL);



	XmMainWindowSetAreas(mainw,

			     buttons,

			     status,	/*<--this widget appears too high,
					  underneath 'buttons' widget
					  until resized */

			     hscroll,

			     vscroll,


			     draw);


	XtRealizeWidget(top);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  600,  400, 0,0,0, /* mainw */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  600,   31, 0,0,0, /* buttons */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   36,   25, 0,0,0, /* Quit */
   CWWidth | CWHeight | CWX | CWY,   42,    3,   60,   25, 0,0,0, /* NextEdge */
   CWWidth | CWHeight | CWX | CWY,  105,    3,   66,   25, 0,0,0, /* Prev Edge */
   CWWidth | CWHeight | CWX | CWY,  174,    3,   84,   25, 0,0,0, /* Base->Marker */
   CWWidth | CWHeight | CWX | CWY,  261,    3,   70,   25, 0,0,0, /* spacerlabel */
   CWWidth | CWHeight | CWX | CWY,  334,    3,   90,   25, 0,0,0, /* Timing Spread */
   CWWidth | CWHeight | CWX | CWY,    0,   77,  581,  304, 0,0,0, /* drawing area */
   CWWidth | CWHeight | CWX | CWY,    0,  385,  581,   15, 0,0,0, /* hscroll */
   CWWidth | CWHeight | CWX | CWY,  585,   77,   15,  304, 0,0,0, /* vscroll */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  600,   46, 0,0,0, /* status */
   CWWidth | CWHeight | CWX | CWY,    6,    1,  588,   21, 0,0,0, /* infoframe */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  584,   17, 0,0,0, /* infolabel */
   CWWidth | CWHeight | CWX | CWY,    6,   24,  192,   21, 0,0,0, /* csrframe */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  188,   17, 0,0,0, /* csrlabel */
   CWWidth | CWHeight | CWX | CWY,  204,   24,  192,   21, 0,0,0, /* mkrframe */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  188,   17, 0,0,0, /* mkrlabel */
   CWWidth | CWHeight | CWX | CWY,  402,   24,  192,   21, 0,0,0, /* difframe */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  188,   17, 0,0,0, /* diflabel */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(top, Expected);
}
  LessTifTestMainLoop(top);

   exit(0);
}
