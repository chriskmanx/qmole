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

  row = XtVaCreateWidget ( "TickRow", xmRowColumnWidgetClass, form,
                            XmNnavigationType, XmNONE,
                            XmNtopAttachment, XmATTACH_FORM,
                            XmNleftAttachment, XmATTACH_FORM,
                            XmNnumColumns, XtNumber(labels),
                            XmNorientation, XmHORIZONTAL,
                            XmNpacking, XmPACK_COLUMN,
                            NULL );
                
  for (num=0; num < XtNumber(labels); num++)
  {
    /* Label describing entry */
    XtVaCreateManagedWidget ( labels[num], 
			    xmLabelWidgetClass, row,
			    XmNwidth, 46,
			    XmNheight, 17,
			    XmNrecomputeSize, False,
			    NULL );

    /* Text widget for number entry */
    text = XtVaCreateManagedWidget ( texts[num],
                            xmLabelWidgetClass, row,
			    XmNwidth, 86,
			    XmNheight, 17,
			    XmNrecomputeSize, False,
                            NULL );
  }
  XtManageChild (row);
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
  XtAddCallback (Button, XmNactivateCallback, (void *)createTickDialog, (XtPointer)NULL);

  XtRealizeWidget(Shell);
  LessTifTestWaitForIt(Shell);
  LessTifTestPushButton(Button);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   81,   97,  187,   49, 0,0,0, /* OptionTick */
   CWWidth | CWHeight            ,   81,   97,  187,   49, 0,0,0, /* StockPane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  181,   43, 0,0,0, /* TickForm */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  181,   43, 0,0,0, /* TickRow */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   86,   17, 0,0,0, /* label_0 */
   CWWidth | CWHeight | CWX | CWY,   92,    3,   86,   17, 0,0,0, /* text_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   23,   86,   17, 0,0,0, /* label_1 */
   CWWidth | CWHeight | CWX | CWY,   92,   23,   86,   17, 0,0,0, /* text_1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(dialog, Expected);
}
LessTifTestMainLoop(Shell);
  exit(0);
}
