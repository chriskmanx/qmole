#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>
#include <Xm/DragDrop.h>

static char *FallBack[] = {
		NULL
};

static void
DropProc(Widget w, XtPointer client_data, XtPointer call_data)
{
XmDropProcCallbackStruct *DropInfo = (XmDropProcCallbackStruct *)call_data;
XmDropTransferEntryRec transferEntries[2];
Arg args[10];
int n = 0;

    printf("DropProc %s\n", XtName(w));
    switch (DropInfo->dropAction)
    {
    case XmDROP:
    	break;
    case XmDROP_HELP:
    	break;
    default:
    	XtSetArg(args[n], XmNtransferStatus, XmTRANSFER_FAILURE); n++;
    	XtSetArg(args[n], XmNnumDropTransfers, 0); n++;
    	DropInfo->operation = XmDROP_NOOP;
    	DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
    	break;
    }
    XmDropTransferStart(DropInfo->dragContext, args, n);
}

static void
DragProc(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf("DragProc\n");
}

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel;
  Widget TopLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateRowColumn(Shell,"RowColumn",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  XtManageChild(TopLabel);
  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtManageChild(BottomLabel);

  {
  Arg args[10];
  int n = 0;

      XtSetArg(args[n], XmNdropProc, (XtCallbackProc)DropProc); n++;
      /*
      XtSetArg(args[n], XmNdragProc, (XtCallbackProc)DragProc); n++;
      */
      XmDropSiteRegister(BottomLabel, args, n);
  }

  XtManageChild(Form);

  XtRealizeWidget(Shell);

  LessTifTestMainLoop(Shell);
  exit(0);
}
