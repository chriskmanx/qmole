/* $Id: test49.c,v 1.4 2000/09/14 09:31:25 amai Exp $ */

/*
 *  This test demonstrates errors in the kbd-grab routines for menus.
 *
 *  Use Alt-F to activate the File menu.  The first error is that the
 *  active button is the tear off control, instead of button 1 as under
 *  motif.  Use the arrow keys to highlight button 1 active.  Press space
 *  to arm and activate button 1 (it should be enter, but that's a
 *  different issue entirely).  Notice that when the dialog comes up, the
 *  pointer is still the upper-right-pointing arrow indicating an active
 *  menu or kbd grab.  Under motif, when the dialog comes up, the menu has
 *  already released the pointer/kbd.  For more fun, press the right arrow
 *  key in lesstif a few times.  Notice that the kbd traversal is working
 *  like you're still in the menu, instead of the dialog.
 *
 *  This problem is more serious than this test suggests, because other
 *  apps can completely lock up the server when this sorta thing happens,
 *  even though I can't reproduce it here.  I expect fixing this bug will
 *  fix the other problem too, though.
 * */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/CascadeB.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/PushBG.h>
#include <Xm/PushBP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/DrawingA.h>
#include "../../common/Test.h"

Widget toplevel, box, push;
XtAppContext	app_context;
int DialogDone=False,DialogReturn;

static void DialogCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  DialogDone=True;
  DialogReturn=1;
}

Widget CreateDialog(Widget parent, char *msg, char *deft,
		    char **buttons, int buttcnt, 
		    XtCallbackProc dialogCB,
		    int DialType)
/*- 
  this routine merely creates a dialog, it dosn't manage it, or wait for a
  response. Note: these dialogs are NOT auto-unmanaging.

  parent:  The parent of the dialog.
  msg:     message for the dialog to display.
  deft:    the default return string for prompt dialogs.
  buttons: array of null-terminated button names.
           the first one is the default button.
  buttcnt: the number of elements in buttons.
  dialogCB: a callback that is to be called when any button is pressed.  
            the button number is passed to the routine as client_data.
	    -1 is passed for the to dialogCB if the cancelCallback of the
	    dialog is invoked.
  DialType: one of
	    XmDIALOG_ERROR,
	    XmDIALOG_WARNING,
	    XmDIALOG_MESSAGE,
	    XmDIALOG_QUESTION,
	    XmDIALOG_WORKING.
 -*/
{
  Arg args[10];
  XmString xmstr[5], xmstr2;
  Widget dial, button, w, DefaultButton;
  int i, n;

  /* make the dialog box */
  n = 0;
  XtSetArg(args[n], XmNautoUnmanage, FALSE);  n++;

  dial = XmCreateMessageDialog(parent, "GXDialog", args, n);
  xmstr[0] = XmStringCreateLtoR(msg, XmFONTLIST_DEFAULT_TAG);
  if (deft)
    xmstr[1] = XmStringCreateLtoR(deft, XmFONTLIST_DEFAULT_TAG);
  else
    xmstr[1] = XmStringCreateLtoR("\0", XmFONTLIST_DEFAULT_TAG);
  xmstr[2] = XmStringCreateLtoR("test10", XmFONTLIST_DEFAULT_TAG);
  /* create buttons */
  DefaultButton = NULL;
  for (i = 0; i < buttcnt; i++) {
    n = 0;
    xmstr2 = XmStringCreateLtoR(buttons[i], XmFONTLIST_DEFAULT_TAG);
    XtSetArg(args[n], XmNlabelString, xmstr2);
    n++;
    button = XmCreatePushButton(dial, "button", args, n);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) dialogCB, (XtPointer) i);
    XtManageChild(button);
    if (!DefaultButton)
      DefaultButton = button;
    XmStringFree(xmstr2);
  }
  XtVaSetValues(dial,
		XmNmessageString, xmstr[0],
		XmNdialogType, DialType,
		XmNdialogTitle, xmstr[2],
		XmNdefaultButton, DefaultButton,
		NULL);
    
  if (dialogCB)
    XtAddCallback(dial, XmNcancelCallback, (XtCallbackProc) dialogCB,
		  (XtPointer) - 1);
  /* unmanage default buttons */
  w=XmMessageBoxGetChild(dial, XmDIALOG_OK_BUTTON);
  if (w)
    XtUnmanageChild(w);
  w=XmMessageBoxGetChild(dial, XmDIALOG_HELP_BUTTON);
  if (w)
    XtUnmanageChild(w);
  w=XmMessageBoxGetChild(dial, XmDIALOG_CANCEL_BUTTON);
  if (w) {
    printf("Destroying cancel button %p\n",w);
    XtDestroyWidget(w);
  }
  XtManageChild(dial);

  XmStringFree(xmstr[0]);
  XmStringFree(xmstr[1]);
  XmStringFree(xmstr[2]);

  return (dial);
}

int GXDialog(Widget parent, char *msg, char *deft, char *buttons[],
	     int buttcnt, int DialType)
/*- 
  create and display a dialog, and wait for a response.  Implements its
  own event loop and does not return until there's a response or cancel.
 -*/
{
  Widget dial, DefaultButton;

  DialogDone = FALSE;

  dial = CreateDialog(parent, msg, deft, buttons, buttcnt, 
		      (XtCallbackProc)DialogCB, DialType); 
  /* display the dialog & wait for answer */
  /* moving the pointer to the window and back */
  {
    Window root, child;
    int root_x, root_y;
    int win_x, win_y;
    Dimension x, y, width, height;
    unsigned int mask;

    XQueryPointer(XtDisplay(toplevel), XtWindow(toplevel),
		  &root, &child,
		  &root_x, &root_y,
		  &win_x, &win_y,
		  &mask);
    XtVaGetValues(dial,
		  XmNdefaultButton, &DefaultButton,
		  NULL);
    if (DefaultButton) {

      XtVaGetValues(DefaultButton,
		    XmNwidth, &width,
		    XmNheight, &height,
		    XmNx, &x,
		    XmNy, &y,
		    NULL);

      XWarpPointer(XtDisplay(toplevel), 0, XtWindow(dial), 0, 0, 0, 0, x, y);
      while (DialogDone == FALSE) {
	static XEvent event;
	/*
	  XtAppProcessEvent(app_context, XtIMAll);
	  */
	XtAppNextEvent(app_context, &event);
	XtDispatchEvent(&event);
      }
      XWarpPointer(XtDisplay(toplevel), 0, XtWindow(toplevel), 0, 0,
		   0, 0, win_x, win_y); 
    }
    XtUnmanageChild(dial);
    return (DialogReturn);
  }
}


#define GXQuestion(a,b,c,d) GXDialog(a,b,NULL,c,d,XmDIALOG_QUESTION)

void MakeDialog(Widget w, XtPointer client, XtPointer call)
{
  static char *buttons[] = {"I'd rather be dead", "Are you stoned"};
  static char *buttons2[] = {""};

  GXQuestion(toplevel, "Do you want to install windows?", buttons, 2);
  return;
}



void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}



char *fallback[] = {
	"*tearOffModel:				tear_off_enabled",
	"*cascade1.labelString:			File",
	"*cascade1.mnemonic:			F",
	"*Edit.mnemonic:			E",
	"*Help.mnemonic:			H",
	"*XmPushButtonGadget.labelString:	gadget",
	"*XmDrawingArea.geometry:               200x200",
	"*XmToggleButton.indicatorOn:           True",
	NULL	/* The end */
};

int
main(int argc, char **argv)
{
    Widget rc;
    Widget cascade1;
    Widget pane1,pane2,pane3;
    Widget w;
    Arg		al[5];
    int		ac;

/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&app_context, "test1", NULL, 0, &argc, argv, fallback, NULL);
    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);


/* First Menu */
    ac = 0;
    XtSetArg(al[ac], XmNnumColumns, 2); ac++;
    pane1 = XmCreatePulldownMenu(rc, "pane1", al, ac);

    cascade1 = XtVaCreateManagedWidget("cascade1",
				       xmCascadeButtonWidgetClass, rc, 
				       XmNsubMenuId,	pane1,
				       NULL);

    w = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane1,
	NULL);
    XtAddCallback(w, XmNactivateCallback, MakeDialog, NULL);


    pane2 = XmCreatePulldownMenu(rc, "pane2", al, ac);
    cascade1 = XtVaCreateManagedWidget("Edit", xmCascadeButtonWidgetClass, rc,
		XmNsubMenuId,	pane2, NULL);
    w = XtVaCreateManagedWidget("/bin/rm -rf /", xmPushButtonWidgetClass,
				pane2, NULL);

    pane3 = XmCreatePulldownMenu(rc, "pane3", al, ac);
    cascade1 = XtVaCreateManagedWidget("Help", xmCascadeButtonWidgetClass, rc,
		XmNsubMenuId,	pane3, NULL);
    w = XtVaCreateManagedWidget("Call psychic friends network",
				xmToggleButtonWidgetClass, 
				pane3, NULL);
    w = XtVaCreateManagedWidget("Call microsoft tech support",
				xmToggleButtonWidgetClass, 
				pane3, NULL);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);

    {
    int numChildren;

    	XtVaGetValues(pane1,
    		XmNnumChildren, &numChildren,
    		NULL);
    	printf("numChildren %i %s\n", 
    		numChildren,
    		(XtClass(pane1))->core_class.get_values_hook == NULL ? "NULL" : "non-NULL");
    	if (numChildren != 1)
    	{
    		GlobalErrors++;
    		printf("numChildren should be 1!!!  How do they do that.\n");
    	}
    }
    LessTifTestMainLoop(toplevel);

    exit(0);
}
