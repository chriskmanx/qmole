/*
From:        Frederick (Rick) A Niles <niles@axp745.gsfc.nasa.gov>
To:          lesstif@lesstif.org
Subject:     Re: TextField/RowColumn Packing bug. 
Date:        Fri, 28 May 99 13:21:09 -0400
cc:          niles@axp745.gsfc.nasa.gov


It's been over two weeks and no one ever responded to this bug report.
I've tried it with the latest CVS and it's still broken.  Is anyone
working on this?

	Thanks,
	Rick Niles.


> I have GIF images of Lesstif and Motif output if anyone needs them.
> 
*/
/**
 * Lesstif TextField/RowColumn packing bug.
 *
 * Why are the TextField boxes so big vertically?  They should be
 * "packed" down to one row each.
 *
 * Rick Niles 
 * May 1999
 **/

#define NOPOPUP
#define NOPANE
#define NOFRAME

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/DialogS.h>
#include <Xm/TextF.h>

static char *FallBack[] = {
		"*rowcol.background: red",
		NULL
};

void
new_table(w)
  Widget w;
{
  Widget create_pane, frame1, rowcol,
         label1, label2, label4, label5, label7;
  Widget createpopup, nametf, idtf, starttf,numtf, typetf;
  XmString xm_string;

#ifndef NOPOPUP
  createpopup = XtVaCreatePopupShell("createpopup",
                xmDialogShellWidgetClass,
                w,
                NULL);
#else
  createpopup = w;
#endif

#ifndef NOPANE
  create_pane = XtVaCreateWidget("create_pane",
                xmPanedWindowWidgetClass, createpopup,
                NULL);
#else
  create_pane = createpopup;
#endif


#ifndef NOFRAME
  frame1 = XtVaCreateManagedWidget("frame1",
        xmFrameWidgetClass, create_pane,
        XmNtopAttachment,   XmATTACH_FORM,
        XmNleftAttachment,  XmATTACH_FORM,
        NULL);
#else
  frame1 = create_pane;
#endif

  rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, frame1, 
        XmNpacking,        XmPACK_COLUMN,
        XmNnumColumns,     5,
        XmNorientation,    XmHORIZONTAL,
        XmNisAligned,      True,
        XmNadjustLast,      False,
        XmNentryAlignment, XmALIGNMENT_END,
        NULL);

  xm_string = XmStringCreateSimple("Name of Table:");
  label1 = XtVaCreateManagedWidget("label1",
             xmLabelWidgetClass, rowcol,
             XmNlabelString, xm_string,
             NULL);

  nametf = XtVaCreateManagedWidget("nametf", xmLabelWidgetClass, rowcol,
             XmNcolumns, 10,
             XmNvalue, "table1",
             NULL);

  xm_string = XmStringCreateSimple("Table ID#:");
  label2 = XtVaCreateManagedWidget("label2",
             xmLabelWidgetClass, rowcol,
             XmNlabelString, xm_string,
             NULL);

  idtf = XtVaCreateManagedWidget("idtf", xmLabelWidgetClass, rowcol,
             XmNcolumns, 10,
             XmNvalue, "0",
             NULL);

  xm_string = XmStringCreateSimple("Starting Byte in Packet:");
  label7 = XtVaCreateManagedWidget("label7",
             xmLabelWidgetClass, rowcol,
             XmNlabelString, xm_string,
             NULL);

  starttf = XtVaCreateManagedWidget("starttf", xmLabelWidgetClass, rowcol
,
             XmNcolumns, 10,
             XmNvalue, "0",
             NULL);

#if 0
  xm_string = XmStringCreateSimple("Number of Elements in Table:");
  label5 = XtVaCreateManagedWidget("label5",
             xmLabelWidgetClass, rowcol,
             XmNlabelString, xm_string,
             NULL);

  numtf = XtVaCreateManagedWidget("numtf", xmTextFieldWidgetClass, rowcol,
             XmNcolumns, 10,
             XmNvalue, "1",
             NULL);

  xm_string = XmStringCreateSimple("Data Field Data Type:");
  label4 = XtVaCreateManagedWidget("label4",
             xmLabelWidgetClass, rowcol,
             XmNlabelString, xm_string,
             NULL);

  typetf = XtVaCreateManagedWidget("typetf", xmTextFieldWidgetClass, rowcol,
             XmNcolumns, 10,
             XmNvalue, "U12",
             NULL);
#endif

  XtManageChild(rowcol);
#ifndef NOPANE
  XtManageChild(create_pane);
#else
#endif
#ifndef NOPOPUP
  XtPopup(createpopup, XtGrabNone);
#else
#endif
  XmStringFree(xm_string);

}

int
main(int argc, char **argv)
{
  Widget toplevel;

  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "test", NULL, 0, &argc, argv, FallBack, NULL
);

  new_table(toplevel);

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  305,   63, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  148,   17, 0,0,0, /* label1 */
   CWWidth | CWHeight | CWX | CWY,  154,    3,  148,   17, 0,0,0, /* nametf */
   CWWidth | CWHeight | CWX | CWY,    3,   23,  148,   17, 0,0,0, /* label2 */
   CWWidth | CWHeight | CWX | CWY,  154,   23,  148,   17, 0,0,0, /* idtf */
   CWWidth | CWHeight | CWX | CWY,    3,   43,  148,   17, 0,0,0, /* label7 */
   CWWidth | CWHeight | CWX | CWY,  154,   43,  148,   17, 0,0,0, /* starttf */
};
/* toplevel should be replaced with to correct applicationShell */
    PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit(0);
}

