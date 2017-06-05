/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/bulletinboard/test13.c,v 1.7 2001/06/18 08:10:14 amai Exp $
 * This test demonstrated a failing of bulletin board to pass along the
 * client_data to a callback (fixed).
 *
 * Now, it deomnstrates a difference in lesstif and Motif where the callback
 * routine is called 3 times in lesstif and twice in motif for the same event
 * (button press, cancel press, etc);
 */

/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warranty expressed or implied.
 * This program is -not- in the public domain.  This program is
 * taken from the Motif Programming Manual, O'Reilly Volume 6.
 */

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>


static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

static Boolean answer;

/* response() --The user made some sort of response to the
 * question posed in AskUser().  Set the answer (client_data)
 * accordingly and destroy the dialog.
 */

void
response (Widget w, int *answer, XmAnyCallbackStruct *reason)
{
    switch (reason->reason) {
        case XmCR_OK:
	    printf("got ok\n");
            *answer = True;
            break;
        case XmCR_CANCEL:
	    printf("got cancel\n");
            *answer = False;
            break;
        default:
	    printf("got default\n");
            *answer = False;
            return;
    }
}

/*
 * ask_user.c -- create a pushbutton that posts a dialog box
 * that asks the user a question that requires an immediate
 * response.  The function that asks the question actually
 * posts the dialog that displays the question, waits for and
 * returns the result.
 */

/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns the response.
 */

Boolean
AskUser (Widget parent, char *question)
{
    static Widget dialog;
    XmString text, yes, no;
    XtAppContext app = XtWidgetToApplicationContext (parent);

    answer = False;
    printf("Answer pointer is %p\n",&answer);
    if (!dialog) {
        dialog = XmCreateQuestionDialog(parent, "dialog", NULL, 0);
        yes = XmStringCreateLocalized("Yes");
        no = XmStringCreateLocalized("No");
        text = XmStringCreateLocalized ("Query?");

        XtVaSetValues(dialog,
            XmNdialogStyle,        XmDIALOG_APPLICATION_MODAL,
            XmNdialogTitle,        text,
            XmNokLabelString,      yes,
            XmNcancelLabelString,  no,
            NULL);
        XmStringFree (text);
        XtSetSensitive(
            XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
        XtAddCallback(dialog,
	    XmNokCallback,
	    (XtCallbackProc) response,
	    &answer);
        XtAddCallback(dialog,
	    XmNcancelCallback,
	    (XtCallbackProc) response,
	    &answer);
        /* if the user interacts via the system menu: */
        XtAddCallback(XtParent (dialog),
	    XmNpopdownCallback,
	    (XtCallbackProc) response,
	    &answer);
    }
    text = XmStringCreateLocalized(question);
    XtVaSetValues(dialog,
        XmNmessageString,      text,
        NULL);
    XmStringFree(text);
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);

    /* while the user hasn't provided an answer, simulate XtMainLoop.
     * The answer changes as soon as the user selects one of the
     * buttons and the callback routine changes its value.  Don't
     * break loop until XtPending() also returns False to assure
     * widget destruction.
     */
    while (answer == False || XtAppPending(app))
        XtAppProcessEvent(app, XtIMAll);

    XmStringFree(yes);
    XmStringFree(no);

    return answer;
}


#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <stdio.h>

void
cb(Widget w, int *answer, XtPointer cbs)
{
  AskUser(w,"Frobnicate warp drive?");
  printf("in cb, answer pointer is %p, val %d\n",answer,*answer);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nLabel", "MY_FONT");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, XmNfontList, fontlist, 
				XmNlabelString, xmstr1,
				XtNborderWidth, 20,
				NULL);

  XtAddCallback(one, XmNactivateCallback, (XtCallbackProc) cb, &answer);

  XtRealizeWidget(toplevel);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   55,   84, 0,0,0, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}





