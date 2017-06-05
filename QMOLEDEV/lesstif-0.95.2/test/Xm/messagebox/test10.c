/* $Id: test10.c,v 1.9 2001/05/15 14:08:34 amai Exp $ */
/*
 *  Demonstrate difference between motif and lesstif handling of callbacks
 *  in message box pushbuttons.
 *
 *  Note, this code was ripped out of a larger program, and as such the
 *  integer returned by GXDialog is no longer meaningful.
 *
 */

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>
#include <stdlib.h>
#include <stdio.h>

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
#if 0
      while (DialogDone == FALSE) {
	static XEvent event;
	/*
	  XtAppProcessEvent(app_context, XtIMAll);
	  */
	XtAppNextEvent(app_context, &event);
	XtDispatchEvent(&event);
      }
#endif
      XWarpPointer(XtDisplay(toplevel), 0, XtWindow(toplevel), 0, 0,
		   0, 0, win_x, win_y); 
    }
    /*
      XtDestroyWidget(dial);
      */
    {
      static XtWidgetGeometry Expected[] = {
	CWWidth | CWHeight            ,    6,   52,  294,  111, 0,0,0, /* GXDialog */
	CWWidth | CWHeight | CWX | CWY,   11,   11,   31,   26, 0,0,0, /* Symbol */
	CWWidth | CWHeight | CWX | CWY,   52,   15,  230,   17, 0,0,0, /* Message */
	CWWidth | CWHeight | CWX | CWY,    0,   47,  294,    2, 0,0,0, /* Separator */
	CWWidth | CWHeight | CWX | CWY,   11,   59,  136,   41, 0,0,0, /* button */
	CWWidth | CWHeight | CWX | CWY,  147,   59,  136,   41, 0,0,0, /* button */
      };
      /* toplevel should be replaced with to correct applicationShell */
      PrintDetails(dial, Expected);
    }
    return (DialogReturn);
  }
}


#define GXQuestion(a,b,c,d) GXDialog(a,b,NULL,c,d,XmDIALOG_QUESTION)

void Push(Widget w, XtPointer client, XtPointer call)
{
  static char *buttons[] = {"I'd rather be dead", "Are you stoned"};
  static char *buttons2[] = {""};

  GXQuestion(toplevel, "Do you want to install windows?", buttons, 2);
  return;
}

int
main(int argc, char **argv)
{
  toplevel = XtVaAppInitialize(&app_context, "listTest", NULL, 0,
			       &argc, argv, NULL, NULL);

  push = XmCreatePushButton(toplevel, "push", NULL, 0);
  XtVaSetValues(push, 
		XtVaTypedArg, XmNlabelString,
		XtRString, "Push me !", 9,
		NULL);

  XtAddCallback(push, XmNactivateCallback, Push, box);

  XtManageChild(push);

  XtRealizeWidget(toplevel);

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(push);
  LessTifTestMainLoop(toplevel);

  XtAppMainLoop(app_context);
  exit(0);
}
