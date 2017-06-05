/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/notebook/test2.c,v 1.4 2002/05/03 12:03:42 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MenuShell.h>
#include <Xm/Notebook.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,   39,   90,  223,   92, 0,0,0 }, /* test2.motif */
{  CWWidth | CWHeight            ,   39,   90,  223,   92, 0,0,0 }, /* notebook */
{  CWWidth | CWHeight | CWX | CWY,   72,   34,   94,   29, 0,0,0 }, /* PageScroller */
{  CWWidth | CWHeight | CWX | CWY,   20,    2,   54,   25, 0,0,0 }, /* NBTextField */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MajorTabScrollerNext */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MajorTabScrollerPrevious */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MinorTabScrollerNext */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MinorTabScrollerPrevious */
{  CWWidth | CWHeight | CWX | CWY,   28,    3,  138,   31, 0,0,0 }, /* tf */
{  CWWidth | CWHeight | CWX | CWY,  -44,  -21,   44,   21, 0,0,0 }, /* Editor */
{  CWWidth | CWHeight | CWX | CWY,  -36,  -25,   36,   25, 0,0,0 }, /* quit */
{  CWWidth | CWHeight | CWX | CWY,  -36,  -25,   36,   25, 0,0,0 }, /* Quit */
};

#if 0
   PrintDetails2(w, NULL);
#else
  if (result_index <= 0)
  {
     PrintDetails2(w, Expected);
     fflush(stdout);
     result_index ++;
  }
#endif
}

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

int
main(int argc, char **argv)
{
	Widget		toplevel, nb, b, tf, cw;
	XtAppContext	app;
	Arg		al[10];
	int		ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Notebook", NULL, 0,
		&argc, argv, NULL, NULL);

	ac = 0;
	XtSetArg(al[ac], XmNbindingType, XmSOLID); ac++;
	nb = XmCreateNotebook(toplevel, "notebook", al, ac);

	ac = 0;
	XtSetArg(al[ac], XmNnotebookChildType, XmPAGE); ac++;
	XtSetArg(al[ac], XmNpageNumber, 1); ac++;
	tf = XmCreateTextField(nb, "tf", al, ac);
	XtManageChild(tf);

	ac = 0;
	XtSetArg(al[ac], XmNnotebookChildType, XmPAGE); ac++;
	XtSetArg(al[ac], XmNpageNumber, 2); ac++;
	b = XmCreatePushButton(nb, "quit", al, ac);
	XtManageChild(b);
	XtAddCallback(b, XmNactivateCallback, Quit, NULL);

	ac = 0;
	XtSetArg(al[ac], XmNpageNumber, 1); ac++;
	b = XmCreatePushButton(nb, "Editor", al, ac);
	XtManageChild(b);

	ac = 0;
	XtSetArg(al[ac], XmNpageNumber, 2); ac++;
	b = XmCreatePushButton(nb, "Quit", al, ac);
	XtManageChild(b);

	XtManageChild(nb);

	XtRealizeWidget(toplevel);

	check_geometry(toplevel);

	LessTifTestMainLoop(toplevel);

	exit(0);
}
