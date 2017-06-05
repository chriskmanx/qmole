/* Header$ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/SeparatoG.h>
#include <Xm/DialogS.h>

#include "../../common/Test.h"


static char *FallBack[] = {
		"*.geometrySlop: 0",
		"*label_0.background: red",
		"*label_1.background: green",
		"*text_0.background: green",
		"*text_1.background: red",
		NULL
};

Widget dialog;

Widget createTickDialog (Widget Button)
{
  Widget form, pane, row, list, button, text, option;

  Dimension width, height, border;

  int num;
  Arg args[10];

  char *labels[] = { "label_0", "label_1"};
  char *texts[] = { "text_0", "text_1"};

  dialog = XtVaCreatePopupShell( "OptionTick", 
                            xmDialogShellWidgetClass,
                            XtParent(Button),
			    XmNx, 75,
			    XmNy, 75,
                            NULL );

  pane = XtVaCreateWidget ( "StockPane", xmPanedWindowWidgetClass, 
                            dialog,
                            XmNsashWidth, 1,
                            XmNsashHeight, 1,
                            NULL );

  /* form to hold rowcolumn form, scrolled list */
  form = XtVaCreateWidget ( "TickForm", xmFormWidgetClass, pane, NULL );

  num = 0;
  XtSetArg(args[num], XmNscrollBarDisplayPolicy, XmSTATIC); num++;
  XtSetArg(args[num], XmNvisibleItemCount, 5); num++;
  XtSetArg(args[num], XmNselectionPolicy,  XmSINGLE_SELECT); num++;
  XtSetArg(args[num], XmNtopAttachment,    XmATTACH_FORM); num++;
  XtSetArg(args[num], XmNleftAttachment,   XmATTACH_FORM); num++;
  XtSetArg(args[num], XmNrightAttachment,  XmATTACH_FORM); num++;
  XtSetArg(args[num], XmNbottomAttachment,  XmATTACH_FORM); num++;
  list = XmCreateScrolledList ( form, "StockList", args, num);
  XtManageChild ( list );

  XtManageChild (form);
  XtManageChild (pane);
  XtManageChild(dialog);
  return (dialog);
}

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell, Button;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);
  Button = XmCreatePushButton(Shell, "PushMePlease", NULL, 0);
  XtVaSetValues(Button,
  	XmNrecomputeSize, False,
  	XmNwidth, 84,
  	XmNheight, 25,
  	NULL);
  XtManageChild(Button);
  XtAddCallback (Button, XmNactivateCallback, (XtCallbackProc)createTickDialog, (XtPointer)NULL);

  XtRealizeWidget(Shell);
  LessTifTestWaitForIt(Shell);
  LessTifTestPushButton(Button);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   75,   75,   73,   93, 0,0,0, /* OptionTick */
   CWWidth | CWHeight            ,   75,   75,   73,   93, 0,0,0, /* StockPane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   67,   87, 0,0,0, /* TickForm */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   67,   87, 0,0,0, /* StockListSW */
   CWWidth | CWHeight | CWX | CWY,   52,    0,   15,   87, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   48,   87, 0,0,0, /* StockList */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(dialog, Expected);
}
LessTifTestMainLoop(Shell);
  exit(0);
}
